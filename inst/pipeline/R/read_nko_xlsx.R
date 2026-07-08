# This script contains functions that will need to be modified for your pipeline
# 
# List of functions:
#   -   `read_nko_xlsx()`
#   -   `read_habitat_xwalk()`


#' Read Native & Known to Occur determinations into the pipeline
#'
#' @param xlsx_file Path to Excel sheet.
#' @param sheet_name Sheet name.
#' @param tar_master_cons_list Master conservation list from this pipeline.
#'
#' @returns A [tibble::tibble()]
read_nko_xlsx <- function(xlsx_file, sheet_name = eligible_sheet, 
                          tar_master_cons_list){
  
  # Load these parameters to troubleshoot/modify this function
  # xlsx_file = targets::tar_read(nko_xlsx)
  # sheet_name = "Eligible Species - Valid Data"
  # tar_master_cons_list = targets::tar_read(cl_master_list)

  taxa_select = c("taxon_id", "kingdom", "phylum", "class", "order", "family", 
                  "genus", "species", "subspecies", "variety", "form")
  cl_select = c("esa_status", "g_rank", "rounded_g_rank", "ut_s_rank", "r4_ssl",
                "usfws_bcc", "ut_swap", "blm_ssl")
  nko_select = c("Yes", "Questionable data - needs further review")
  dat = readxl::read_excel(path = xlsx_file, sheet = sheet_name) |>
    janitor::clean_names() |> 
    dplyr::filter(native_known_to_occur %in% nko_select) |> 
    dplyr::select(!dplyr::any_of(taxa_select)) |> 
    dplyr::select(!dplyr::any_of(cl_select)) |> 
    psoSppEvals::get_taxonomies(correct = TRUE) |> 
    dplyr::arrange("kingdom", "phylum", "class", "order", "family", "genus", 
                   "species", "subspecies", "variety", "form", 
                   "scientific_name")
  cons_ranks = tar_master_cons_list |> 
    janitor::clean_names() |>
    dplyr::select(taxon_id, dplyr::any_of(cl_select)) |> 
    dplyr::filter(taxon_id %in% dat$taxon_id)
  nko = dplyr::left_join(dat, cons_ranks, by = 'taxon_id')
  return(nko)
}


#' Read the NatureServe habitat crosswalk into the pipeline
#'
#' @param xlsx_file Path to Excel sheet.
#' @param sheet_name Sheet name.
#'
#' @returns A [tibble::tibble()]
read_habitat_xwalk <- function(xlsx_file, ns_habs_target, 
                               sheet_name = "NatureServe Habitat Crosswalk"){
  
  # Load these parameters to troubleshoot/modify this function
  # xlsx_file <- targets::tar_read(nko_habitat_xwalk_xlsx )
  # ns_habs_target = targets::tar_read(nko_ns_habitats)
  
  xwalk = readxl::read_excel(path = xlsx_file, sheet = sheet_name) |>
    janitor::clean_names() |> 
    dplyr::select(habitat_category, ns_habitat_type, pso_habitat) |> 
    dplyr::filter(pso_habitat != "NA")
  
  dat = dplyr::left_join(ns_habs_target, xwalk, 
                         by = c("habitat_category", "ns_habitat_type"), 
                         relationship = "many-to-many")
  return(dat)
}

