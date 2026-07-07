# This script contains functions that will need to be modified for your pipeline
# 
# List of functions:
#   -   `build_nhp_spp()`
#   -   `get_hnp_data()`
#   -   `read_nhp_data()`
#   -   `combine_unhp_data()`
#   -   `get_unhp_point_data()`
#   -   `get_unhp_plant_data()`


#' Build species list from Utah NHP data
#'
#' @param nhp_data_sf NHP data from this pipeline.
#' @param locale_label Label used for location (i.e., unit_code or "Buffer")
#' @param nhp_abb Abbreviation or acronym for NHP data (e.g., "CNHP" for 
#'     Colorado NHP)
#'
#' @returns A [tibble::tibble()]
build_nhp_spp <- function(nhp_data_sf, locale_label, nhp_abb){
  
  # Convert spatial data to tabular data
  dat = sf::st_drop_geometry(nhp_data_sf)
  
  # Taxonomy data
  taxa_select = c("taxon_id", "scientific_name", "kingdom", "phylum", "class", 
                  "order", "family", "genus", "species", "subspecies", 
                  "variety", "form")
  taxa = dat |>
    dplyr::select(dplyr::any_of(taxa_select)) |>
    dplyr::filter(!taxon_id == "" | !is.na(taxon_id)) |>
    dplyr::distinct() |>
    dplyr::mutate(dup_taxon = ifelse(duplicated(taxon_id) |
                                       duplicated(taxon_id, fromLast = TRUE),
                                     "Yes", "No"))
  # Summarize data
  spp_dates = dat |> 
    dplyr::select(taxon_id, scientific_name, dplyr::contains("parsed")) |> 
    tidyr::pivot_longer(cols = dplyr::contains("parsed"), 
                        names_to = "obs_var", values_to = "date") |> 
    dplyr::mutate(year = lubridate::year(date)) |>
    dplyr::filter(!is.na(year) & !is.na(taxon_id)) |> 
    dplyr::group_by(taxon_id, scientific_name) |> 
    dplyr::summarise(minYear = min(year, na.rm = TRUE), 
                     maxYear = max(year, na.rm = TRUE))
  spp_list <- dat |> 
    dplyr::select(taxon_id, scientific_name, obs_id) |> 
    dplyr::group_by(taxon_id, scientific_name) |> 
    dplyr::summarise(nObs = dplyr::n(), 
                     occID = ifelse(nObs <= 6,
                                    stringr::str_c(unique(obs_id), collapse = "; "),
                                    NA),
                     .groups = "drop") |> 
    dplyr::left_join(spp_dates, by = c('taxon_id', 'scientific_name')) |> 
    dplyr::select(taxon_id, scientific_name, nObs, minYear, maxYear, occID) |> 
    dplyr::mutate(locale = locale_label, source = nhp_abb)|> 
    dplyr::left_join(taxa, by = c('taxon_id', 'scientific_name'), 
                     relationship = 'many-to-many') |> 
    dplyr::arrange(kingdom, phylum, class, order, family, genus, 
                   species, scientific_name) |> 
    dplyr::filter(!is.na(taxon_id))
  
  return(spp_list)
}


#' Read NHP EO polygon data into this pipeline.
#' 
#' @param eo_lyr_1 Name of Level 1 EO layer
#' @param eo_lyr_2 Name of Level 2 EO layer 
#' @param gdb Path to geodatabase
#' @param target_crs Target coordinate reference system
#' 
#' @returns A spatial (`sf`) feature.
#' 
#' @examples
#' proj_gdb <- file.path("data", "SppEvals_CCNGs.gdb")
#' crs <- "EPSG:26913" # NAD83 UTM Zone 13
#' get_hnp_data("CNHP_2025_L1_EO_PlanArea", "CNHP_2025_L2_EO_PlanArea")
get_hnp_data = function(eo_lyr_1, eo_lyr_2, gdb = proj_gdb, target_crs = crs){
  eo_1 = read_nhp_data(eo_lyr_1, gdb, target_crs)
  eo_2 = read_nhp_data(eo_lyr_2, gdb, target_crs)
  dat = dplyr::bind_rows(eo_1, eo_2) |> sf::st_as_sf()
  return(dat)
}


#' Read NHP polygon data into R
#' 
#' @param lyr Feature class layer name.
#' @param gdb Path to geodatabase
#' @param target_crs Target coordinate reference system
#'
#' @returns A spatial (`sf`) feature.
#' 
#' @examples
#' proj_gdb <- file.path("data", "SppEvals_CCNGs.gdb")
#' crs <- "EPSG:26913" # NAD83 UTM Zone 13
#' l1_eo <- read_cnhp_data(lyr = "CNHP_2025_L1_EO_PlanArea")
read_nhp_data <- function(lyr, gdb_path = proj_gdb, target_crs = crs){
  
  # Modify and load these parameters to troubleshoot/modify this function
  # lyr = "L2_EO_3miBuffer"
  # gdb_path = file.path("data", "WRVR_SppEvals.gdb")
  # target_crs = "EPSG:26913"
  
  # Read feature class from geodatabase
  date_formats <- c("%y-%m-%d", "%y-%m", "%y")
  dat = sf::read_sf(layer = lyr, dsn = gdb_path) |> 
    sf::st_make_valid() |> 
    janitor::clean_names() |> 
    dplyr::distinct() |> 
    dplyr::mutate(
      parsed_first_obs = stringr::str_replace(first_obs, "-99-99", ""), 
      parsed_first_obs = stringr::str_replace(first_obs, "-99", ""), 
      parsed_first_obs = ifelse(first_obs == "9999", NA, first_obs), 
      parsed_first_year = lubridate::parse_date_time(first_obs, date_formats) |> 
        as.Date() |> lubridate::year(),
      parsed_last_obs = stringr::str_replace(last_obs, "-99-99", ""),
      parsed_last_obs = stringr::str_replace(last_obs, "-99", ""),
      parsed_last_obs = ifelse(last_obs == "9999", NA, last_obs),
      parsed_last_year = lubridate::parse_date_time(last_obs, date_formats) |> 
        as.Date() |> lubridate::year()
    )
  
  # Transform data if different from target CRS
  if(sf::st_crs(dat) != target_crs){
    dat = sf::st_transform(dat, crs = target_crs)
  }
  
  # Pull taxonomies from GBIF
  taxonomies = sf::st_drop_geometry(dat) |> 
    dplyr::mutate("scientific_name" = s_name) |> 
    dplyr::select(scientific_name, s_element_id) |> 
    dplyr::distinct() |> 
    psoSppEvals::get_taxonomies(query_field = "scientific_name", correct = TRUE)
  # Join data
  nhp_dat = dplyr::left_join(dat, taxonomies, by = "s_element_id",
                             relationship = 'many-to-many')
  return(nhp_dat)
}


#' Combine Utah HNP data from this pipeline
#'
#' @param unhp_pts_sf UNHP point data from this pipeline
#' @param unhp_plants_sf UNHP plant data from this pipeline.
#'
#' @returns An `sf` object.
combine_unhp_data <- function(unhp_pts_sf, unhp_plants_sf){
  # Point data
  pts = unhp_pts_sf |> 
    dplyr::select(feature_id, elcodebcd, eo_id, eo_num, name_cat, sname, 
                  scomname, parsed_first_obs, parsed_last_obs, 
                  parsed_first_obs_sf, parsed_last_obs_sf, parsed_survey_date, 
                  sfdesc, sfloc, locuncert) |> 
    dplyr::mutate(data_set = "unhp_pts") |> 
    dplyr::rename("obs_id" = feature_id, "taxa_group" = name_cat,
                  "scientific_name" = sname, "common_name" = scomname,
                  "elcode" = elcodebcd, "description" = sfdesc, 
                  "location" = sfloc, "loc_uncert_m" = locuncert)
  # Rare plant data
  plants = unhp_plants_sf |> 
    dplyr::select(obs_id, elcode, s_sci_name, s_com_name, pres_abs, 
                  parsed_obs_date_start, parsed_obs_date_end, 
                  parsed_obs_date_text, num_live_total, species_obs_data, 
                  habitat_obs_data, location, loc_uncert_m) |> 
    dplyr::mutate(data_set = "USFS_plant_points", 
                  taxa_group = "Vascular Plant") |> 
    dplyr::rename("scientific_name" = s_sci_name, "common_name" = s_com_name,
                  "parsed_first_obs" = parsed_obs_date_start, 
                  "parsed_last_obs" = parsed_obs_date_end)
  # Combine data
  unhp_data = dplyr::bind_rows(pts, plants)
  # Get taxon ID's and taxonomies
  unhp_tids = sf::st_drop_geometry(unhp_data) |> 
    dplyr::select(scientific_name) |> 
    dplyr::distinct() |> 
    psoSppEvals::get_taxonomies(correct = TRUE)
  
  # Return the final dataset
  unhp_data = dplyr::left_join(unhp_data, unhp_tids, by = "scientific_name", 
                               relationship = 'many-to-many')
  return(unhp_data)
}


#' Read Utah HNP data into this pipeline
#'
#' @param lyr Name of UNHP points feature class
#' @param gdb_path Path to geodatabase, default if this pipeline's geodatabase 
#'                (proj_gdb).
#'
#' @returns An `sf` object.
get_unhp_point_data <- function(lyr = "unhp_pts_3miBuffer", 
                                gdb_path = proj_gdb){
  
  # Read feature class from geodatabase, clean field names, and reduce fields
  date_formats = c("%m/%d/%Y", "%Y", "%Y-%m", "%Y-%m-%d", "%Y-%d-%m")
  bad_dates = c("-00", "-77", "-156", "-233")
  unhp_pts = sf::read_sf(dsn = gdb_path, layer = lyr) |> 
    janitor::clean_names() |> 
    dplyr::select(
      !dplyr::contains(c("visitdate", "citation", "visitedby", "gencom", 
                         "gendesc", "eodata", "visitdata"))
      ) |> 
    dplyr::mutate(
      # fix dates
      firstobs = stringr::str_replace(firstobs, "-77", ""),
      firstobs = stringr::str_replace(firstobs, "-156", ""),
      lastobs = stringr::str_replace(lastobs, "-77", ""),
      lastobs = stringr::str_replace(lastobs, "-233", ""),
      sf_firstobs = stringr::str_replace(sf_firstobs, "-00", ""),
      sf_lastobs = stringr::str_replace(sf_lastobs, "-00", ""),
      surveydate = stringr::str_replace(surveydate, "-77", ""),
      # parse dates
      parsed_first_obs = lubridate::parse_date_time(
        firstobs, orders = date_formats
        ) |> as.Date(), 
      parsed_last_obs = lubridate::parse_date_time(
        lastobs, orders = date_formats
        ) |> as.Date(), 
      parsed_first_obs_sf = lubridate::parse_date_time(
        sf_firstobs, orders = date_formats
        ) |> as.Date(), 
      parsed_last_obs_sf = lubridate::parse_date_time(
        sf_lastobs, orders = date_formats
        ) |> as.Date(), 
      parsed_survey_date = lubridate::parse_date_time(
        surveydate, orders = date_formats
        ) |> as.Date() 
    )
  
  # sf::st_drop_geometry(unhp_pts) |> 
  #   dplyr::select(firstobs, parsed_first_obs) |> 
  #   dplyr::filter(is.na(parsed_first_obs)) |> 
  #   dplyr::distinct()

  return(unhp_pts)
}


#' Read Utah HNP plant data into this pipeline
#'
#' @param lyr Name of UNHP rare plants feature class
#' @param gdb_path Path to geodatabase, default if this pipeline's geodatabase 
#'                (proj_gdb).
#'
#' @returns An `sf` object.
get_unhp_plant_data <- function(lyr = "USFS_plant_points_3miBuffer", 
                                gdb_path = proj_gdb){
  # gdb_path = file.path("data", "DIF_SppOcc_Data.gdb")
  date_formats = c("%m/%d/%Y %H:%M:%S", "%m/%d/%Y", "%Y-%m", "%Y")
  unhp_plants = sf::read_sf(dsn = gdb_path, layer = lyr) |> 
    janitor::clean_names() |> 
    dplyr::mutate(
      obs_date_text = stringr::str_replace(obs_date_text, "-77", ""),
      obs_date_text = stringr::str_replace(obs_date_text, "1983-07", "1983"),
      parsed_obs_date_start = lubridate::parse_date_time(
        obs_date_start, orders = "%Y-%m-%d %H:%M:%S"
        ) |> as.Date(),
      parsed_obs_date_end = lubridate::parse_date_time(
        obs_date_end, orders = "%m/%d/%Y"
        ) |> as.Date(), 
      parsed_obs_date_text = lubridate::parse_date_time(
        obs_date_text, orders = date_formats
        ) |> as.Date() 
    )
  
  # sf::st_drop_geometry(unhp_plants) |>
  #   dplyr::select(obs_date_text, parsed_obs_date_text) |>
  #   dplyr::filter(is.na(parsed_obs_date_text)) |>
  #   dplyr::distinct() |> View()
  
  return(unhp_plants)
}

