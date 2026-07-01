#' Function in this script:
#' -   get_taxonomies()
#' -   get_synonyms()
#' -   correct_taxon_ids()


#' Get Taxonomies from GBIF
#'
#' This function adds taxonomy information from GBIF to any data frame that has
#'     valid scientific names and returns a tibble. `gbif_taxonID` is the GBIF
#'     ID for the given scientific name and full taxonomy from the GBIF backbone
#'     taxonomies database. `taxon_id` is ID number of the accepted taxonomy
#'     from the GBIF backbone.
#'
#' @param spp_list A data frame containing valid scientific species names.
#' @param query_field The name of the variable with valid scientific names.
#' @param correct Logical. If TRUE, `correct_taxon_ids()` is used to correct 
#'     known issues with taxon ID's and scientific names. Default is FALSE.
#'
#' @returns A [tibble::tibble()]
#' @seealso [correct_taxon_ids()]
#' @export
#'
#' @examples
#' library(psoSppEvals)
#' spp_list <- get_taxonomies(sp_list_ex)
get_taxonomies <- function(spp_list, query_field = "scientific_name", 
                           correct = FALSE) {
  # spp_list = readr::read_csv(file.path("data-raw/data", "spp_list.csv"),
  #                            show_col_types = FALSE)
  # query_field = "scientific_name"; correct = TRUE
  
  # Get list of distinct species.
  distinct_spp = spp_list |>
    dplyr::select(dplyr::any_of(query_field)) |>
    dplyr::distinct()
  # Clean text
  distinct_spp$clean_name = distinct_spp |>
    dplyr::pull(query_field) |>
    stringr::str_replace("[\r\n]", " ") |>
    stringr::str_replace("[\r\n]", "") |>
    stringr::str_replace("  ", " ") |>
    stringr::str_replace("[^A-Za-z0-9 ]", "") |> 
    stringr::str_to_sentence()
  # Get GBIF Taxon ID's
  distinct_spp$taxon_id <- taxize::get_gbifid(
    distinct_spp$clean_name, ask = FALSE, rows = 1, messages = FALSE
  )
  
  # Correct Scientific Names with known Errors
  if(correct) {
    # Read corrected names data frame
    cor_names = psoSppEvals::name_corrections
    # Match scientific names
    matched_names = match(distinct_spp$clean_name, cor_names$errored_name)
    # Correct scientific names
    distinct_spp$clean_name[!is.na(matched_names)] = cor_names$corrected_name[matched_names[!is.na(matched_names)]]
    # Correct taxon_ids
    distinct_spp$taxon_id[!is.na(matched_names)] = cor_names$taxon_id[matched_names[!is.na(matched_names)]]
  }
  
  
  #-- Previous method using taxize::classification()
  # Pull Taxonomy from GBIF backbone taxonomy
  taxonomy_list = taxize::classification(distinct_spp$gbif_taxonID, db = "gbif")

  # Function to convert long list to wide data frame and add taxon ID's
  convert_taxonomy = function(i, tax_list) {
    # i = 1; tax_list = taxonomy_list

    # Get GBIF IF
    gbif_taxonID = names(tax_list)[[i]]
    if(!is.na(gbif_taxonID)){
      # Get taxon ID
      final_id = tax_list[[i]] |>
        tail(1) |>
        dplyr::pull(id)
      # Get taxonomy
      named_taxonomy = tax_list[[i]] |>
        dplyr::select(rank, name) |>
        tidyr::pivot_wider(names_from = rank, values_from = name)
      # Stitch together data frame
      tibble::tibble(taxon_id = as.character(final_id)) |>
        dplyr::bind_cols(tibble::tibble(gbif_taxonID = gbif_taxonID)) |>
        dplyr::bind_cols(named_taxonomy)
    }
  }

  # Convert list to data frame
  all_taxonomies = lapply(seq_along(taxonomy_list), convert_taxonomy,
                          taxonomy_list) |>
    dplyr::bind_rows()

  # authorship = lapply(distinct_spp$taxon_id, function(x){
  #   # x = distinct_spp$taxon_id[1]
  #   vars = c("taxon_id", "gbif_taxonID", "vernacularName", "canonicalName", 
  #            "authorship", "rank", "taxonomic_status", "taxonomicStatus")
  #   # Query taxon ID in GBIF Backbone
  #   if(!is.na(x)){
  #     taxize::gbif_name_usage(key = x) |> 
  #     dplyr::bind_cols() |> 
  #     dplyr::rename("gbif_taxonID" = taxonID) |> 
  #     dplyr::select(dplyr::any_of(vars)) |> 
  #     dplyr::mutate(
  #       taxon_id = as.character(x),
  #       authorship = trimws(authorship) |> stringr::str_replace("[(]", "") |> 
  #         stringr::str_replace("[)]", ""), 
  #       rank = stringr::str_to_sentence(rank), 
  #       taxonomic_status = stringr::str_to_sentence(taxonomicStatus)
  #       ) |>
  #     dplyr::select(!taxonomicStatus) |> 
  #     dplyr::select(dplyr::any_of(vars)) |> 
  #     suppressMessages()
  #     }
  #   }) |> 
  #   dplyr::bind_rows() |> 
  #   dplyr::rename(
  #     "accepted_scientific_name" = canonicalName, 
  #     "gbif_common_name" = vernacularName
  #   )
  
  # Create final data frame
  var_order = c(query_field, "taxon_id", "gbif_taxonID", "duplicated_taxon",
                "gbif_common_name", "accepted_scientific_name", "authorship", 
                "rank", "taxonomic_status", "kingdom", "phylum", "class", 
                "order", "family", "genus", "species", "subspecies", "variety", 
                "form")
  
  spp_taxonomies = distinct_spp |>
    dplyr::mutate(taxon_id = as.character(taxon_id)) |>
    dplyr::left_join(all_taxonomies, by = "taxon_id", 
                     relationship = 'many-to-many') |>
    # dplyr::left_join(authorship, by = 'taxon_id', 
    #                  relationship = 'many-to-many') |>
    dplyr::select(dplyr::any_of(var_order)) |> 
    dplyr::distinct()
  
  returned_dat = dplyr::left_join(spp_list, spp_taxonomies, by=query_field)|> 
    dplyr::mutate(
      duplicated_taxon = ifelse(duplicated(taxon_id) |
                                  duplicated(taxon_id, fromLast = TRUE),
                                "Yes", "No")
    ) |>
    dplyr::select(dplyr::any_of(c(colnames(spp_list), var_order)))
  
  
  if(correct) {
    returned_dat = psoSppEvals::correct_taxon_ids(returned_dat, 
                                                  query_field = query_field)
  }
  
  return(returned_dat)
}


#' Get Taxonomic Synonyms
#'
#' This function queries synonyms from the GBIF Backbone taxonomy. It will only
#'     return synonyms for unique taxon ID's (i.e., duplicated taxon ID's will
#'     not be queried).
#'
#' @param spp_list Species list with taxon ID's from `get_taxonomies()`.
#'
#' @returns A [tibble::tibble()]
#' @export
#'
#' @examples
#' library(psoSppEvals)
#' spp_data <- sp_list_ex |> get_taxonomies('scientific_name', correct = TRUE)
#' get_synonyms(spp_data)
get_synonyms <- function(spp_list) {

  # eligible_list = targets::tar_read(elig_list)
  # u_code = "BRF"

  t_ids = unique(spp_list$taxon_id)

  syns = lapply(t_ids, function(x){
    rgbif::name_usage(key = x, data = "synonyms")$data
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(taxon_id = acceptedKey) |>
    tibble::tibble()

  return(syns)
}


#' Correct known issues with taxon ID's and scientific names.
#' 
#' Documentation will be updated shortly.
#'
#' @param spp_list A data frame with taxon ID's from `get_taxonomies()`.
#' @param query_field Field holding scientific names
#' @param update_scientific_names Optional. TRUE/FALSE. Update query field scientific 
#'     names with corrected scientific names. Default is FALSE.
#'
#' @returns A [tibble::tibble()] with corrected taxon ID's and scientific names.
#' @seealso [get_taxonomies()]
#' @export
#' 
#' @examples
#' library(psoSppEvals)
#' 
#' spp_list = tibble::tibble(
#'    common_name = c("Western Toad", "Northern Leopard Frog", "Mountain Plover", 
#'                    "Snowy Plover", "American Goshawk", "Ferruginous Hawk", 
#'                    "Hopi Chipmunk", "Canada Lynx", "American Pika",
#'                    "Largemouth Bass", "Westslope Cutthroat Trout", 
#'                    "Vargo's Furcula", "Western Bumblebee", "Monarch"),
#'    scientific_name = c("Anaxyrus boreas", "Lithobates pipiens", 
#'                        "Anarhynchus montanus", "Anarhynchus nivosus", 
#'                        "Accipiter atricapillus", "Buteo regalis", 
#'                        "Neotamias rufus", "Lynx canadensis", "Ochotona princeps", 
#'                        "Micropterus nigricans", "Oncorhynchus lewisi", 
#'                        "Furcula vargoi", "Bombus occidentalis", 
#'                        "Danaus plexippus")
#'    ) |> 
#'    dplyr::mutate(
#'      taxon_id = taxize::get_gbifid(scientific_name, ask = FALSE, rows = 1, 
#'                                    messages = FALSE)) |> 
#'    dplyr::distinct()
#' spp_list_fix <- correct_taxon_ids(spp_list)
correct_taxon_ids <- function(spp_list, query_field = "scientific_name", 
                              update_scientific_names = FALSE){
  # Read corrected names data frame
  dat = psoSppEvals::name_corrections
  
  # Match scientific names
  spp_list_sci_names = dplyr::pull(spp_list, query_field)
  mat_nam = match(spp_list_sci_names, dat$errored_name)
  
  # Correct taxon ID's
  spp_list$taxon_id[!is.na(mat_nam)] = dat$taxon_id[mat_nam[!is.na(mat_nam)]]
  
  if(update_scientific_names){
    # Correct scientific names
    cor_sci_names = spp_list_sci_names
    cor_sci_names[!is.na(mat_nam)] = dat$corrected_name[mat_nam[!is.na(mat_nam)]]
    spp_list[, query_field] = cor_sci_names
  }
  
  # Return data
  return(spp_list)
}

