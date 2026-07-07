# This script contains functions that will need to be modified for your pipeline
# 
# List of functions:
#   -   `get_fs_data()`
#   -   `build_fs_spp()`


#' Read Forest Service EDW data into this pipeline
#'
#' @param layer Name of UNHP feature class
#' @param dsn Path to geodatabase, default if this pipeline's geodatabase 
#'                (proj_gdb).
#'
#' @returns An `sf` object.
get_fs_data <- function(tesp_layer, inv_plant_layer, dsn = proj_gdb){
  
  # Load these parameters to troubleshoot/modify this function
  # dsn = file.path("data", "DIF_SppOcc_Data.gdb")
  # tesp_layer = "Biology_TESP_OccurrenceAll"
  # inv_plant_layer = "Biology_InvasivePlant_All"
  
  # Date formats
  date_formats = c("%m/%d/%Y", "%Y", "%Y-%m", "%Y-%m-%d %H:%M:%S")
  
  # Read feature class from geodatabase and clean field names
  tesp_data = sf::read_sf(dsn = dsn, layer = tesp_layer) |> 
    janitor::clean_names()
  ip_data = sf::read_sf(dsn = dsn, layer = inv_plant_layer) |> 
    janitor::clean_names()
  fs_data = dplyr::bind_rows(tesp_data, ip_data) |> 
    sf::st_as_sf() |> 
    dplyr::mutate(
      parsed_date_collected = lubridate::parse_date_time(
        date_collected, orders = date_formats
        ) |> as.Date(),
      parsed_date_collected_recent = lubridate::parse_date_time(
        date_collected_most_recent, orders = date_formats
        ) |> as.Date(),
    )

  # Get taxon ID's and taxonomies
  fs_tids = sf::st_drop_geometry(fs_data) |> 
    dplyr::select(scientific_name) |> 
    dplyr::distinct() |> 
    psoSppEvals::get_taxonomies("scientific_name", correct = TRUE)
  
  # Return the final datasets
  fs_data = dplyr::left_join(fs_data, fs_tids, by = "scientific_name", 
                             relationship = 'many-to-many')
  return(fs_data)
}


#' Build species list from Forest Service EDW data
#'
#' @param fs_data Utah NHP data from this pipeline.
#' @param locale_label Label used for location (i.e., unit_code or "Buffer")
#'
#' @returns A [tibble::tibble()]
build_fs_spp <- function(fs_data, locale_label){
  # Taxonomy data
  taxa_select = c("taxon_id", "scientific_name", "kingdom", "phylum", "class", 
                  "order", "family", "genus", "species", "subspecies", 
                  "variety", "form")
  
  taxa = sf::st_drop_geometry(fs_data) |>
    dplyr::select(dplyr::any_of(taxa_select)) |>
    dplyr::filter(!taxon_id == "" | !is.na(taxon_id)) |>
    dplyr::distinct() |>
    dplyr::mutate(dup_taxon = ifelse(duplicated(taxon_id) |
                                       duplicated(taxon_id, fromLast = TRUE),
                                     "Yes", "No"))
  # Summarize data
  spp_dates = sf::st_drop_geometry(fs_data) |> 
    dplyr::select(taxon_id, scientific_name, parsed_date_collected, 
                  parsed_date_collected_recent) |> 
    tidyr::pivot_longer(cols = c(parsed_date_collected, 
                                 parsed_date_collected_recent), 
                        names_to = "obs_time", values_to = "date") |> 
    dplyr::mutate(year = lubridate::year(date)) |> 
    dplyr::filter(!is.na(year) & !is.na(taxon_id)) |> 
    dplyr::group_by(taxon_id, scientific_name) |> 
    dplyr::summarise(minYear = min(year, na.rm = TRUE), 
                     maxYear = max(year, na.rm = TRUE), 
                     .groups = "drop")
  
  fs_spp = sf::st_drop_geometry(fs_data) |> 
    dplyr::select(taxon_id, scientific_name, site_id) |> 
    dplyr::group_by(taxon_id, scientific_name) |> 
    dplyr::summarise(nObs = dplyr::n(), 
                     occID = ifelse(nObs <= 6,
                                    stringr::str_c(unique(site_id), 
                                                   collapse = "; "),
                                    NA),
                     .groups = "drop") |> 
    dplyr::left_join(spp_dates, by = c('taxon_id', 'scientific_name')) |>
    dplyr::select(taxon_id, scientific_name, nObs, minYear, maxYear, occID) |>
    dplyr::mutate(locale = locale_label, source = "FS EDW")|> 
    dplyr::left_join(taxa, by = c('taxon_id', 'scientific_name'), 
                     relationship = 'many-to-many') |> 
    dplyr::arrange(kingdom, phylum, class, order, family, genus, 
                   species, scientific_name) |> 
    suppressWarnings()
  
  return(fs_spp)
}
