# This script contains functions that will need to be modified for your pipeline
# 
# List of functions:
#   -   `build_spp_list()`
#   -   `build_buff_list()`
#   -   `write_eligible_xlsx()`
#   -   `evaluate_uncertainty()`
#   -   `summarize_occ_dat()`


#' Compile a comprehensive species list from occurrence data.
#' 
#' **Note**: The input data sets might need to be modified for your pipeline. 
#'     GBIF, SEINet, IMBCR, and Forest Service data will not change. State NHP 
#'     data might need to be updated or duplicated depending on your pipeline.
#' 
#' @param gbif_list Species list from GBIF data using [psoSppEvals::gbif_spp()].
#' @param sei_list Species list from SEINet data using [psoSppEvals::seinet_spp()].
#' @param imbcr_list Species list form IMBCR data using [psoSppEvals::imbcr_spp()].
#' @param nhp_list Species list form Utah NHP data from this pipeline. 
#' @param fs_list Species list form FS EDW data from this pipeline.
#' @param master_status_list Master status list from this pipeline.
#' @param locale Unit acronym or "Buffer"
#'
#' @return A [tibble::tibble()]
build_spp_list <- function(gbif_list, sei_list, imbcr_list, nhp_list,
                           fs_list, master_status_list, locale = unit_code){
  
  # Load these parameters to troubleshoot/modify this function
  # gbif_list = targets::tar_read(gbif_unit_spp)
  # sei_list = targets::tar_read(sei_unit_spp)
  # imbcr_list = targets::tar_read(imbcr_unit_spp)
  # nhp_list = targets::tar_read(unhp_unit_spp)
  # fs_list = targets::tar_read(fs_unit_spp)
  # targets::tar_load(master_status_list)
  # locale = "DIF"
  
  # Taxonomy
  taxa_select = c("taxon_id", "scientific_name", "kingdom", "phylum", "class", 
                  "order", "family", "genus", "species", "subspecies", 
                  "variety", "form")
  taxa = dplyr::select(gbif_list$all_spp, dplyr::any_of(taxa_select)) |> 
    dplyr::bind_rows(dplyr::select(sei_list$all_spp, dplyr::any_of(taxa_select))) |> 
    dplyr::bind_rows(dplyr::select(imbcr_list, dplyr::any_of(taxa_select))) |> 
    dplyr::bind_rows(dplyr::select(nhp_list, dplyr::any_of(taxa_select))) |> 
    dplyr::bind_rows(dplyr::select(fs_list, dplyr::any_of(taxa_select))) |> 
    dplyr::distinct() |> 
    dplyr::filter(!is.na(taxon_id))
  
  # Conservation status
  status = dplyr::filter(master_status_list, taxon_id %in% taxa$taxon_id)
  
  #-- Data
  valid_dat <- summarize_occ_dat(gbif_list$valid_spp) |> 
    dplyr::full_join(summarize_occ_dat(sei_list$valid_spp), 
                     by = c("taxon_id", "scientific_name"), 
                     relationship = "many-to-many") |> 
    dplyr::distinct(taxon_id, .keep_all = TRUE) |> 
    dplyr::select(taxon_id, scientific_name, GBIF_nObs, SEINet_nObs) |> 
    dplyr::rename("valid_GBIF_nObs" = GBIF_nObs, 
                  "valid_SEINet_nObs" = SEINet_nObs)
  
  dat = summarize_occ_dat(gbif_list$all_spp) |> 
    dplyr::full_join(summarize_occ_dat(sei_list$all_spp), 
                     by = c("taxon_id", "scientific_name"), 
                     relationship = "many-to-many") |> 
    dplyr::full_join(summarize_occ_dat(imbcr_list),
                     by = c("taxon_id", "scientific_name"), 
                     relationship = "many-to-many") |> 
    dplyr::full_join(summarize_occ_dat(nhp_list),
                     by = c("taxon_id", "scientific_name"), 
                     relationship = "many-to-many") |> 
    dplyr::full_join(summarize_occ_dat(fs_list),
                     by = c("taxon_id", "scientific_name"), 
                     relationship = "many-to-many") |> 
    dplyr::distinct(taxon_id, .keep_all = TRUE) |> 
    dplyr::left_join(valid_dat, by = c("taxon_id", "scientific_name")) |> 
    dplyr::select(taxon_id:GBIF_nObs, valid_GBIF_nObs, GBIF_minYear:SEINet_nObs, 
                  valid_SEINet_nObs, SEINet_minYear:FS_EDW_occID)
  
  #-- Source
  dat_source = rbind(
    dplyr::select(gbif_list$all_spp, taxon_id, scientific_name, source),
    dplyr::select(sei_list$all_spp, taxon_id, scientific_name, source)
    ) |> 
    rbind(dplyr::select(imbcr_list, taxon_id, scientific_name, source)) |> 
    rbind(dplyr::select(nhp_list, scientific_name, taxon_id, source)) |> 
    rbind(dplyr::select(fs_list, taxon_id, scientific_name, source)) |> 
    dplyr::group_by(taxon_id, scientific_name) |> 
    dplyr::summarise(sources = stringr::str_c(unique(source), collapse = ", "),
                     .groups = 'drop')  |> 
    dplyr::filter(!is.na(taxon_id)) |> 
    dplyr::distinct()
  
  #-- Summary
  dat_sum = dat |> 
    dplyr::select(taxon_id, scientific_name, dplyr::ends_with("nObs")) |> 
    dplyr::select(!dplyr::starts_with("valid")) |> 
    tidyr::pivot_longer(cols = dplyr::ends_with("nObs"), names_to = "data", 
                        values_to = "nObs") |> 
    dplyr::summarise(tot_nObs_all = sum(nObs, na.rm = TRUE), 
                     .by = c("taxon_id", "scientific_name")) |> 
    dplyr::left_join(
      dat |> 
        dplyr::select(taxon_id, scientific_name, dplyr::ends_with("Year")) |> 
        tidyr::pivot_longer(cols = dplyr::ends_with("Year"), names_to = "data", 
                            values_to = "years") |> 
        dplyr::filter(!is.null(years)) |> 
        dplyr::mutate(years = dplyr::na_if(years, Inf), 
                      years = dplyr::na_if(years, -Inf)) |> 
        dplyr::filter(!is.na(years)) |> 
        dplyr::summarise(minYear_all = min(years), 
                         maxYear_all = max(years),
                         .by = c("taxon_id", "scientific_name")), 
      by = c("taxon_id", "scientific_name")
    ) |> 
    dplyr::left_join(dat_source, by = c("taxon_id", "scientific_name")) |> 
    dplyr::filter(!is.na(taxon_id)) |> 
    dplyr::filter(!is.na(tot_nObs_all))

  # Compile species list
  spp_list = dplyr::left_join(
    status, 
    dplyr::left_join(dat_sum, dat, by = c("taxon_id", "scientific_name")),
    by = c("taxon_id", "scientific_name"), relationship = 'many-to-many'
    ) |>
    dplyr::left_join(taxa, 
                     by = c("taxon_id", "scientific_name"), 
                     relationship = 'many-to-many') |>
    dplyr::distinct() |> 
    dplyr::mutate(
      dup_taxon = ifelse(duplicated(taxon_id) | 
                           duplicated(taxon_id, fromLast = TRUE),
                         "Yes", "No"), 
      locale = locale
      ) |> 
    dplyr::arrange("kingdom", "phylum", "class", "order", "family", "genus", 
                   "species", "scientific_name", "taxon_id") |> 
    dplyr::select(taxon_id:fine_group, elig_scc, dup_taxon, locale, 
                  esa_status:mlsnf_scc, tot_nObs_all:FS_EDW_occID, 
                  dplyr::any_of(taxa_select)) |>
    dplyr::filter(!is.na(taxon_id)) |> 
    dplyr::filter(!is.na(tot_nObs_all))
  # Convert Inf to NA
  spp_list[sapply(spp_list, is.infinite)] = NA
  valid_spp = dplyr::filter(spp_list,
                            taxon_id %in% c(gbif_list$valid_spp$taxon_id,
                                            sei_list$valid_spp$taxon_id,
                                            imbcr_list$taxon_id,
                                            nhp_list$taxon_id,
                                            fs_list$taxon_id))
  invalid_spp = dplyr::filter(spp_list, !taxon_id %in% valid_spp$taxon_id)
  return(tibble::lst('all_spp' = spp_list,
                     'valid_spp' = valid_spp,
                     'invalid_spp' = invalid_spp))
}


#' Compile a comprehensive species list from occurrence data.
#' 
#' This function omits IMBCR data when there are no observations from IMBCR in 
#'     the 1-km buffer. use `build_spp_list()` if there are observations in the 
#'     1-km buffer from IMBCR.
#' 
#' **Note**: The input data sets might need to be modified for your pipeline. 
#'     GBIF, SEINet, and Forest Service data will not change. State NHP 
#'     data might need to be updated or duplicated depending on your pipeline.
#' 
#' @param gbif_list Species list from GBIF data using [psoSppEvals::gbif_spp()].
#' @param sei_list Species list from SEINet data using [psoSppEvals::seinet_spp()].
#' @param nhp_list Species list form Utah NHP data from this pipeline. 
#' @param fs_list Species list form FS EDW data from this pipeline.
#' @param master_status_list Master status list from this pipeline.
#' @param locale Unit acronym of "Buffer"
#'
#' @return A [tibble::tibble()]
build_buff_list <- function(gbif_list, sei_list, nhp_list, fs_list, 
                            master_status_list, locale = unit_code){
  
  # Load these parameters to troubleshoot/modify this function
  # gbif_list = targets::tar_read(gbif_buff_list)
  # sei_list = targets::tar_read(sei_buff_list)
  # nhp_list = targets::tar_read(unhp_buff_list)
  # fs_list = targets::tar_read(fs_buff_list)
  # targets::tar_load(master_status_list)
  # locale = "CODE"
  
  # Taxonomy
  taxa_select = c("taxon_id", "scientific_name", "kingdom", "phylum", "class", 
                  "order", "family", "genus", "species", "subspecies", 
                  "variety", "form")
  taxa = dplyr::select(gbif_list, dplyr::any_of(taxa_select)) |> 
    dplyr::bind_rows(dplyr::select(sei_list, dplyr::any_of(taxa_select))) |> 
    dplyr::bind_rows(dplyr::select(nhp_list, dplyr::any_of(taxa_select))) |> 
    dplyr::bind_rows(dplyr::select(fs_list, dplyr::any_of(taxa_select))) |> 
    dplyr::distinct() |> 
    dplyr::mutate(taxon_id = as.numeric(taxon_id)) |>
    dplyr::filter(!is.na(taxon_id))
  
  # Conservation status
  status = dplyr::filter(master_status_list, taxon_id %in% taxa$taxon_id)
  
  # Data
  dat = summarize_occ_dat(gbif_list) |> 
    dplyr::full_join(summarize_occ_dat(sei_list), 
                     by = c("taxon_id", "scientific_name"), 
                     relationship = "many-to-many") |> 
    dplyr::full_join(summarize_occ_dat(nhp_list),
                     by = c("taxon_id", "scientific_name"), 
                     relationship = "many-to-many") |> 
    dplyr::full_join(summarize_occ_dat(fs_list),
                     by = c("taxon_id", "scientific_name"), 
                     relationship = "many-to-many") |> 
    dplyr::distinct(taxon_id, .keep_all = TRUE)
  
  
  #-- Source
  dat_source = rbind(
    dplyr::select(gbif_list, taxon_id, scientific_name, source),
    dplyr::select(sei_list, taxon_id, scientific_name, source)
    ) |> 
    rbind(dplyr::select(nhp_list, scientific_name, taxon_id, source)) |> 
    rbind(dplyr::select(fs_list, taxon_id, scientific_name, source)) |> 
    dplyr::group_by(taxon_id, scientific_name) |> 
    dplyr::summarise(sources = stringr::str_c(unique(source), collapse = ", "),
                     .groups = 'drop')  |> 
    dplyr::mutate(taxon_id = as.numeric(taxon_id)) |>
    dplyr::filter(!is.na(taxon_id)) |> 
    dplyr::distinct()
  
  #-- Summary
  dat_sum = dat |> 
    dplyr::select(taxon_id, scientific_name, dplyr::ends_with("nObs")) |> 
    tidyr::pivot_longer(cols = dplyr::ends_with("nObs"), names_to = "data", 
                        values_to = "nObs") |> 
    dplyr::summarise(tot_nObs_all = sum(nObs, na.rm = TRUE), 
                     .by = c("taxon_id", "scientific_name")) |> 
    dplyr::left_join(
      dat |> 
        dplyr::select(taxon_id, scientific_name, dplyr::ends_with("Year")) |> 
        tidyr::pivot_longer(cols = dplyr::ends_with("Year"), names_to = "data", 
                            values_to = "years") |> 
        dplyr::filter(!is.null(years)) |> 
        dplyr::mutate(years = dplyr::na_if(years, Inf), 
                      years = dplyr::na_if(years, -Inf)) |> 
        dplyr::filter(!is.na(years)) |> 
        dplyr::summarise(minYear_all = min(years), 
                         maxYear_all = max(years),
                         .by = c("taxon_id", "scientific_name")), 
      by = c("taxon_id", "scientific_name")
    ) |> 
    dplyr::left_join(dat_source, by = c("taxon_id", "scientific_name")) |> 
    dplyr::filter(!is.na(taxon_id)) |> 
    dplyr::filter(!is.na(tot_nObs_all))
  
  # Compile species list
  spp_list = dplyr::left_join(
    status, 
    dplyr::left_join(dat_sum, dat, by = c("taxon_id", "scientific_name")),
    by = c("taxon_id", "scientific_name"), relationship = 'many-to-many'
  ) |>
    dplyr::left_join(taxa, 
                     by = c("taxon_id", "scientific_name"), 
                     relationship = 'many-to-many') |>
    dplyr::distinct() |> 
    dplyr::mutate(
      dup_taxon = ifelse(duplicated(taxon_id) | 
                           duplicated(taxon_id, fromLast = TRUE),
                         "Yes", "No"), 
      locale = locale
    ) |> 
    dplyr::arrange("kingdom", "phylum", "class", "order", "family", "genus", 
                   "species", "scientific_name", "taxon_id") |> 
    dplyr::select(taxon_id:fine_group, elig_scc, dup_taxon, locale, 
                  esa_status:mlsnf_scc, tot_nObs_all:FS_EDW_occID, 
                  dplyr::any_of(taxa_select)) |> 
    dplyr::filter(!is.na(taxon_id)) |> 
    dplyr::filter(!is.na(tot_nObs_all))
  # Convert Inf to NA
  spp_list[sapply(spp_list, is.infinite)] = NA
  return(spp_list)
}


#' Build Eligible Species List
#' 
#' This function 1) builds the eligible species list and 2) creates an Excel 
#'     workbook.     
#'
#' @param tar_elig_list Eligible species list from this pipeline.
#' @param tar_unit_list Unit species list from this pipeline.
#' @param tar_buff_list Buffer species list from this pipeline.
#' @param tar_elig_syns Synonym list from this pipeline.
#' @param tar_unit_code Unit code from this pipeline.
#' @param tar_scc_library Path to SCC library.
#' @param tar_fs_region FS region code from this pipeline.
#' 
#' @details
#' This function creates the eligible species list by filtering the `unit_list` 
#'     by `eligible_scc == "Yes"`. It then creates a working data frame, 
#'     `elig_work`, by adding relevant blank variables to the returned data 
#'     frame so biologists or specialists can document their work.
#' 
#' The buffer list is created by filtering `buffer_list` by `unit_list$taxon_id`
#'     so than it contains only species found in the 1-km buffer and not in the 
#'     Plan Area.
#' 
#' The `elig_synonyms` data frame is reduced to relevant plain language 
#'     variables.
#' 
#' Data definition data frames are created for the species lists using the 
#'     `elig_work` data frame and the reduced synonyms data frame.
#' 
#' These data frames and the original `unit_list` data frame are written to an 
#'     Excel workbook using the `writexl` package. This workbook is then copied 
#'     to the `Species List` directory in the project's SCC library. The 
#'     workbook includes the following sheets:
#'     - "Eligible Species - Working" includes the blank variables for 
#'           biologists and specialists to document their work,
#'     - "Eligible Species - Original" the eligible species list with out the 
#'           blank variables,
#'     - "Comprehensive List" the comprehensive species list from `unit_list`, 
#'     - "Buffer List" the list of species found in buffer and not in the Plan
#'           Area,
#'     - "Synonyms" the list of taxonomic synonyms,
#'     - "Data Definitions - Species Lists" data definitions for the species 
#'           lists,
#'     - "Data Definitions - Synonyms Lists" data definitions for the synonyms 
#'           list,
#'     - "Dropdown Categories" drop down categories for the Native and Known to 
#'           Occur determination on the working eligible species sheet.
#'
#' @returns Nothing
write_eligible_xlsx <- function(tar_elig_list = elig_list, 
                                tar_unit_list = unit_list, 
                                tar_buff_list = buffer_list, 
                                tar_elig_syns = elig_synonyms, 
                                tar_unit_code = unit_code, 
                                gdb_path = proj_gdb, 
                                tar_scc_library = scc_library, 
                                tar_fs_region = fs_region, 
                                tar_plan_area = plan_area, 
                                nko_xlsx = nat_known_xlsx){
  
  # Load these parameters to troubleshoot/modify this function
  # tar_elig_list = targets::tar_read(elig_list)
  # tar_unit_list = targets::tar_read(unit_list)
  # tar_buff_list = targets::tar_read(buffer_list)
  # tar_elig_syns = targets::tar_read(elig_synonyms)
  # tar_unit_code = "DIF"; tar_fs_region = "R4"
  # gdb_path = file.path("data", "DIF_spp_evals.gdb")
  # tar_plan_area = targets::tar_read(basemap_data)$plan_area
  # tar_scc_library = here::here() |> dirname() |> dirname()
  # nko_xlsx <- "20260213_DIF_EligibleSppList.xlsx"

  # Native & Known to Occur Excel Workbook
  nko <- if(!is.null(nko_xlsx)){
    readxl::read_excel(file.path(tar_scc_library, "Species List", nko_xlsx), 
                       sheet = "Eligible Species - Valid Data") |> 
      dplyr::bind_rows(
        readxl::read_excel(file.path(tar_scc_library, "Species List", nko_xlsx), 
                           sheet = "Eligible Species - Invalid Da")
      )
  } else(NULL)
  
  # Data ----
  gbif_unc <- sf::read_sf(layer = "Elig_v2_GBIF_UncertaintyBuffers", 
                          dsn = gdb_path) |> 
    evaluate_uncertainty("GBIF", aoa = tar_plan_area)
  sei_unc <- sf::read_sf(layer = "Elig_v2_SEINet_UncertaintyBuffers", 
                         dsn = gdb_path) |> 
    evaluate_uncertainty("SEINet", aoa = tar_plan_area)
  unhp_unc <- sf::read_sf(layer = "Elig_v2_UNHP_UncertaintyBuffers", 
                          dsn = gdb_path) |>
    evaluate_uncertainty("UNHP", aoa = tar_plan_area)
  elig_list <- tar_elig_list |> 
    dplyr::left_join(gbif_unc, by = 'taxon_id') |> 
    dplyr::left_join(sei_unc, by = 'taxon_id') |> 
    dplyr::left_join(unhp_unc, by = 'taxon_id') |> 
    dplyr::select(taxon_id:valid_GBIF_nObs, GBIF_spatiallyElig_nObs, 
                  GBIF_minYear:valid_SEINet_nObs, SEINet_spatiallyElig_nObs, 
                  SEINet_minYear:UNHP_nObs, UNHP_spatiallyElig_nObs, 
                  UNHP_minYear:form) |> 
    dplyr::mutate('Manual Review Specialist' = as.character(""),
                  'Status' = as.character(""),
                  'Manual Review' = as.character(""),
                  'Native & Known to Occur' = as.character(""),
                  'Rationale & BASI' = as.character("")) |> 
    dplyr::select('Manual Review Specialist', "Status", 
                  taxon_id:elig_scc, 'Manual Review', 
                  'Native & Known to Occur', 'Rationale & BASI', 
                  dup_taxon:form)
  if(!is.null(nko)){
    tid_match = match(elig_list$taxon_id, nko$taxon_id)
    elig_list$`Manual Review Specialist`[!is.na(tid_match)] = nko$`Manual Review Specialist`[!is.na(tid_match)]
    elig_list$Status[!is.na(tid_match)] = nko$Status[!is.na(tid_match)]
    elig_list$`Manual Review`[!is.na(tid_match)] = nko$`Manual Review`[!is.na(tid_match)]
    elig_list$`Native & Known to Occur`[!is.na(tid_match)] = nko$`Native & Known to Occur`[!is.na(tid_match)]
    elig_list$`Rationale & BASI`[!is.na(tid_match)] = nko$`Rationale & BASI`[!is.na(tid_match)]
  }
  
  #-- Eligible List - Valid Data
  elig_work = elig_list |> 
    dplyr::filter(taxon_id %in% tar_unit_list$valid_spp$taxon_id)
  #-- Eligible List - Invalid Data
  invalid_work = elig_list |>
    dplyr::filter(!taxon_id %in% elig_work$taxon_id)
  
  #-- Buffer list
  buff_list = dplyr::filter(tar_buff_list, 
                            !taxon_id %in% tar_unit_list$all_spp$taxon_id)
  
  #-- Synonyms
  syn_list = dplyr::select(tar_elig_syns, taxon_id, accepted, canonicalName, 
                           vernacularName, authorship, nameType, rank, 
                           taxonomicStatus) |> 
    dplyr::distinct()
  
  # Data definitions ----
  # TODO: Definitions might need to be updated depending on your pipeline.
  spp_defs = tibble::tibble(
    column_name = colnames(elig_work), 
    data_type = sapply(elig_work, class), 
    definition = c(
      "First and last name of specialist assigned to manual review.",
      "Will the species be considered for SCC? Yes/NO.",
      "Taxonomic ID from GBIF.", 
      "Scientific name from NatureServer.", 
      "Common name from NatureServe.", 
      "Broad-scale taxon group from NatureServe.", 
      "Fine-scale taxon group from NatureServe.",
      "Eligible SCC. Is the taxon eligible for SCC? Yes/No.",
      "Was the species reviewed manually. Yes/No.",
      "Is the species determined to be Native & Known to Occure. Yes/No.", 
      "Rationale and BASI statement.",
      "Duplicated taxon. Is the taxon duplicated? Yes/No.", 
      "The location of the observations. Dixie NF (DIF) or 1km Buffer.",
      "Endangered Species Act Status.", 
      "NatureServe Global Rank (G-rank).", 
      "NatureServe Rounded G-rank.", 
      "NatureServe State Rank (S-rank) for Utah.", 
      paste0("USFS Region 4 (", tar_fs_region, ") Regional Foresters Sensitive Species List (SSL). Is the taxon on the ", tar_fs_region, " SSL? Yes/No."),
      "US Fish & Wildlife Service (USFWS) Birds of Conservation Concern (BCC). Is the taxon on the BCC list? Yes/No.",
      # TODO: State SWAP and T&E definitions will need to be updated
      "Utah State Wildlife Action Plan (SWAP). Yes/No.", 
      # TODO: State BLM definition will need to be updated or omitted
      "Utah Bureau of Land Management (BLM) SSL",
      # TODO: Adjacent forest SCC definition will need to be updated or omitted
      "Manti-La Sal National Forest Species of Conservation Concern list. Yes/No",
      "Total number of observations from all data sources.", 
      "Minimum observation year from all data sources.",
      "Maximum observation year from all data sources.",
      "List of sources separated by a comma.",
      "Number of observations from GBIF.",
      "Number of GBIF observations that have valid date and coordinate uncertainty data",
      "Number of spatially eligible observaions from GBIF determied by the `coordinateUncertaintyInMeters` field.", 
      "Minimum observation year from GBIF.", 
      "Maximum observation year from GBIF.", 
      "GBIF Occurrence record ID if less than six observations, separated by a comma.",
      "Number of observations from SEINet.",
      "Number of SEINet observations that have valid date and coordinate uncertainty data",
      "Number of spatially eligible observaions from SEINet determied by the `coordinateUncertaintyInMeters` field.", 
      "Minimum observation year from SEINet.", 
      "Maximum observation year from SEINet.", 
      "SEINet Occurrence record ID if less than six observations, separated by a comma.",
      "Number of observations from IMBCR.",
      "Minimum observation year from IMBCR.", 
      "Maximum observation year from IMBCR.", 
      # TODO: State NHP definitions will need to be updated
      "Number of observations from Utah NHP (UNHP).",
      "Number of spatially eligible observaions from UNHP determied by the `locuncert` field.", 
      "Minimum observation year from UNHP.", 
      "Maximum observation year from UNHP.",
      "EO Numbers form UNHP.",
      "Number of observations from Forest Service EDW (FS EDW).",
      "Minimum observation year from FS EDW.", 
      "Maximum observation year from FS EDW.",
      "Site ID from FS EDW.",
      "Taxonomic Kingdom.",
      "Taxonomic Phylum.",
      "Taxonomic Class",
      "Taxonomic Order.",
      "Taxonomic Family.",
      "Taxonomic Genus.",
      "Taxonomic Species.",
      "Taxonomic Subspecies.",
      "Taxonomic Variety.",
      "Taxonomic Form."
    )
  )
  
  # Synonym Definitions
  syn_defs = tibble::tibble(
    column_name = colnames(syn_list), 
    data_type = sapply(syn_list, class), 
    definition = c(
      "Taxonomic ID from GBIF.",
      "Accepted taxonomic name including authorship.", 
      "Canonical name (synonyms).",
      "Vernacular or common name.",
      "Authorship.",
      "Name type. E.g., scientific or placeholder",
      "Taxon rank. E.g., species or subspecies.",
      "Taxonomic status. E.g., homotypic synonym or heterotypic synonym"
    )
  )
  
  # Drop downs
  dropdowns = tibble::tibble(
    Status = c("Done", "In Progress", "Not Done", "Not native & known to occur", 
               "Duplicate Taxon - Omit", "NA - Omit", NA, NA, NA, NA, NA), 
    'Native & Known to Occur' = c(
      "Yes", 
      "No",
      "No, the species was introduced or is adventive to the plan area", 
      "No, there is insufficient taxonomic certainty to identify the observation data to a species", 
      "No, there is insufficient temporal certainty that the species still occupies the plan area", 
      "No, there is insufficient spatial certainty that the observation occurred within the plan area", 
      "No, the species is accidental or transient to the plan area", 
      "No, the observations represent an expanding species range or irruptive population that is not established in the plan area", 
      "No, the species is not in the plan area but is in the admin unit therfore is not included in the species overview spatial layer", 
      "Questionable data - needs further review", 
      "NA, see rationale"
    )
  )
  
  # Write data ----
  f_name = paste0(gsub("-", "", Sys.Date()), "_", tar_unit_code, 
                  "_EligibleSppList.xlsx")
  writexl::write_xlsx(
    list("Eligible Species - Valid Data" = elig_work, 
         "Eligible Species - Invalid Data" = invalid_work, 
         "Comprehensive List" = tar_unit_list$all_spp, 
         "Buffer List" = buff_list,
         "Synonyms" = syn_list, 
         "Dropdown Categories" = dropdowns,
         "Data Definitions - Species Lists" = spp_defs, 
         "Data Definitions - Synonyms Lists" = syn_defs),
    file.path("output", f_name)
  )
  #-- Copy to parent species list directory
  file.copy(file.path("output", f_name), 
            file.path(tar_scc_library, "Species List", f_name))
}


#' Evaluate spatial point data
#'
#' @param sf_data Spatial data from this pipeline.
#' @param data_source Data source acronym.
#' @param aoa Spatial polygon data of the Plan Area from this pipeline.
#'
#' @returns A [tibble::tibble()]
evaluate_uncertainty <- function(sf_data, data_source, aoa = plan_area){
  
  # These parameters are derived from functions above.
  # sf_data = gbif_unc; data_source = "gbif"
  # aoa = targets::tar_read(plan_area)
  
  sf_data$area <- sf::st_area(sf_data)
  sf_data_clip <- psoSppEvals::clip_fc(sf_data, aoa)
  sf_data_clip$clip_area <- sf::st_area(sf_data_clip)
  elig_spat <- sf::st_drop_geometry(sf_data_clip) |> 
    dplyr::mutate(area_diff = area - clip_area, 
                  elig_pts = ifelse(area_diff < units::as_units(0, "m^2"), 
                                    "not_eligible", "spat_elig_pts")) |> 
    # dplyr::select(taxon_id, area, clip_area, area_diff, elig_spatial) |> 
    dplyr::group_by(taxon_id, elig_pts) |> 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') |> 
    tidyr::pivot_wider(names_from = elig_pts, values_from = n, 
                       values_fill = 0) |> 
    dplyr::select(taxon_id, spat_elig_pts)
  colnames(elig_spat)[2] = paste0(data_source, "_spatiallyElig_nObs") 
  return(elig_spat)
}


#' Summarize species occurrence data
#'
#' @param occ_dat Speceis occurrence data from this pipeline.
#'
#' @return A [tibble::tibble()]
summarize_occ_dat = function(occ_dat){
  
  # This parameter are derived from functions above.
  # occ_dat = fs_list
  
  dat_select = c("taxon_id", "scientific_name", "nObs", "minYear", "maxYear", 
                 "occID")
  d = dplyr::select(occ_dat, dplyr::any_of(dat_select)) |> 
    tibble::tibble() |> 
    dplyr::mutate(taxon_id = as.numeric(taxon_id))
  d[sapply(d, is.infinite)] = NA
  ds = gsub(" ", "_", unique(occ_dat$source))
  names(d)[3:ncol(d)] = paste(ds, colnames(d[, 3:ncol(d)]), sep = "_")
  return(unique(d[, 1:ncol(d)]))
}

