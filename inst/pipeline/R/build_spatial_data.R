# This script contains functions that will need to be modified for your pipeline
# 
# List of functions:
#   -   `build_all_occ_data()`
#   -   `build_basemap_data()`
#   -   `write_spatial_data()`


#' This function combines all of the occurrence point data into a single [sf] 
#'     object.
#' 
#' **Note**: The input data sets might need to be modified for your pipeline. 
#'     GBIF, SEINet, IMBCR, and Forest Service data will not change. State NHP 
#'     data might need to be updated or duplicated depending on your pipeline.
#' 
#' @param gbif_data Spatial GBIF data from this pipeline.
#' @param seinet_data Spatial SEINet data from this pipeline.
#' @param imbcr_data Spatial IMBCR data from this pipeline.
#' @param nhp_data Spatial UNHP **point** data from this pipeline.
#' @param fs_data Spatial Forest Service **point** data from this pipeline.
#' @param tar_crs Target coordinate reference system. 
#'
#' @return An [sf] object.
build_all_occ_data <- function(spp_list, gbif_data, seinet_data, imbcr_data, 
                               nhp_data, fs_data, tar_crs = crs) {
  
  # Load these parameters to troubleshoot/modify this function
  # spp_list = targets::tar_read(elig_list)
  # gbif_data = targets::tar_read(gbif_unit)
  # seinet_data = targets::tar_read(sei_unit)
  # imbcr_data = targets::tar_read(imbcr_unit)
  # nhp_data = targets::tar_read(unhp_unit)
  # fs_data = targets::tar_read(fs_unit)
  # dataset_name = "EligOccData"; data_prefix = "elig"
  # tar_crs = "EPSG:26912"; proj_gdb = file.path("data", "DIF_SppOcc_Data.gdb")
  
  library(sf)
  # Function to reduce and standardize data
  process_sf = function(sf_data, source){
    # sf_data = gbif_data; source = "GBIF"
    # Reduce data
    dat = dplyr::select(sf_data, taxon_id) |>
      dplyr::mutate(taxon_id = as.numeric(taxon_id), source = source) |> 
      sf::st_make_valid()
    # Transform data if they are not in the project CRS
    if(!sf::st_crs(dat) == tar_crs){
      dat = sf::st_transform(x = dat, crs = tar_crs)
    }
    return(dat)
  }

  # Standardize data
  g_pts = psoSppEvals::build_gbif_spatial_data(gbif_data$all_data, spp_list) |> 
    process_sf(source = "GBIF")
  s_pts = psoSppEvals::build_seinet_spatial_data(seinet_data$all_data, spp_list) |> 
    process_sf(source = "SEINet")
  i_pts = psoSppEvals::build_imbcr_spatial_data(imbcr_data, spp_list) |> 
    process_sf(source = "IMBCR")
  c_pts = dplyr::filter(nhp_data, taxon_id %in% spp_list$taxon_id) |> 
    process_sf(source = "UNHP")
  f_pts = dplyr::filter(fs_data, taxon_id %in% spp_list$taxon_id) |>
    sf::st_centroid(fs) |> 
    process_sf(source = "FS")
  # Combine data
  obs_dat = dplyr::bind_rows(g_pts, s_pts, i_pts, c_pts, f_pts) |> 
    sf::st_as_sf()
  # Return data
  return(obs_dat)
}


#' Write spatial data to geodatabase
#'
#' **Note**: The input data sets might need to be modified for your pipeline. 
#'     GBIF, SEINet, IMBCR, and Forest Service data will not change. State NHP 
#'     data might need to be updated or duplicated depending on your pipeline.
#' 
#' @param gbif_data Spatial GBIF occurrence data from this pipeline.
#' @param seinet_data Spatial SEINet occurrence data from this pipeline.
#' @param imbcr_data Spatial IMBCR occurrence data from this pipeline.
#' @param nhp_data Spatial ID NHP occurrence data from this pipeline.
#' @param fs_data Spatial MT NHP occurrence data from this pipeline.
#' @param dataset_name Feature dataset name to write data in.
#' @param data_prefix Prefix to add to data name (e.g., "elig" or "nko")
#' @param t_path_gdb File path to geodatabase from this pipeline.
#' @param tar_crs Targets coordinate reference system.
#' 
write_spatial_data <- function(spp_list, gbif_data, seinet_data, imbcr_data, 
                               nhp_data, fs_data, dataset_name, data_prefix, 
                               gdb_path = proj_gdb, tar_crs = crs){
  
  # Load these parameters to troubleshoot/modify this function
  # spp_list = targets::tar_read(elig_list)
  # gbif_data = targets::tar_read(gbif_unit)
  # seinet_data = targets::tar_read(sei_unit)
  # imbcr_data = targets::tar_read(imbcr_unit)
  # nhp_data = targets::tar_read(unhp_unit)
  # fs_data = targets::tar_read(fs_unit)
  # dataset_name = "EligOccData"; data_prefix = "elig"
  # tar_crs = "EPSG:26912"; proj_gdb = file.path("data", "DIF_SppOcc_Data.gdb")
  
  # Activate ArcGIS license
  arcgisbinding::arc.check_product()
  
  # data cleaning function
  clean_sf <- function(dat){
    # dat = gbif_data$valid_data
    sf_d <- dat |> 
      dplyr::mutate(
        dplyr::across(dplyr::where(lubridate::is.Date), as.character)
      )
    if(sf::st_crs(sf_d) != tar_crs) sf_d = sf::st_transform(sf_d, tar_crs)
    return(sf_d)
  }
  
  
  message("Writing GBIF data")
  arcgisbinding::arc.write(
    path = file.path(gdb_path, dataset_name, paste0(data_prefix, "_GBIF")),
    data = psoSppEvals::build_gbif_spatial_data(gbif_data$all_data, spp_list) |> 
      clean_sf(),
    overwrite = TRUE
  )
  # Build uncertainty buffers
  gbif_u <- psoSppEvals::build_gbif_spatial_data(gbif_data$valid_data, spp_list) |> 
    dplyr::mutate(
      coordinateUncertaintyInMeters = units::set_units(coordinateUncertaintyInMeters, "m")
    )
  gbif_b <- sf::st_buffer(gbif_u, dist = gbif_u$coordinateUncertaintyInMeters)
  arcgisbinding::arc.write(
    path = file.path(gdb_path, dataset_name, 
                     paste0(data_prefix, "_GBIF_UncertaintyBuffers")),
    data = clean_sf(gbif_b),
    overwrite = TRUE
  )
  
  message("Writing SEINet data")
  arcgisbinding::arc.write(
    path = file.path(gdb_path, dataset_name, paste0(data_prefix, "_SEINet")),
    data = psoSppEvals::build_seinet_spatial_data(seinet_data$all_data, spp_list) |> 
      clean_sf(),
    overwrite = TRUE
  )
  # Build uncertainty buffers
  sei_u <- psoSppEvals::build_seinet_spatial_data(seinet_data$valid_data, spp_list) |>
    dplyr::mutate(
      coordinateUncertaintyInMeters = units::set_units(coordinateUncertaintyInMeters, "m")
    )
  sei_b <- sf::st_buffer(sei_u, dist = sei_u$coordinateUncertaintyInMeters)
  arcgisbinding::arc.write(
    path = file.path(gdb_path, dataset_name, 
                     paste0(data_prefix, "_SEINet_UncertaintyBuffers")),
    data = clean_sf(sei_b),
    overwrite = TRUE
  )
  
  message("Writing IMBCR data")
  arcgisbinding::arc.write(
    path = file.path(gdb_path, dataset_name, paste0(data_prefix, "_IMBCR")),
    data = psoSppEvals::build_imbcr_spatial_data(imbcr_data, spp_list) |> clean_sf(),
    overwrite = TRUE
  )

  message("Writing UNHP data")
  arcgisbinding::arc.write(
    path = file.path(gdb_path, dataset_name, paste0(data_prefix, "_UNHP")),
    data = dplyr::filter(nhp_data, taxon_id %in% spp_list$taxon_id) |> 
      clean_sf(),
    overwrite = TRUE
  )
  # Build uncertainty buffers
  unhp_u <- nhp_data |>
    dplyr::mutate(locuncert = units::set_units(loc_uncert_m, "m")) |>
    dplyr::filter(!is.na(loc_uncert_m))
  unhp_b <- sf::st_buffer(unhp_u, dist = unhp_u$loc_uncert_m)
  arcgisbinding::arc.write(
    path = file.path(gdb_path, dataset_name, 
                     paste0(data_prefix, "_UNHP_UncertaintyBuffers")),
    data = clean_sf(unhp_b),
    overwrite = TRUE
  )
  
  message("Writing FS EDW data")
  arcgisbinding::arc.write(
    path = file.path(gdb_path, dataset_name, paste0(data_prefix, "_FS_EDW")),
    data = dplyr::filter(fs_data, taxon_id %in% spp_list$taxon_id) |> 
      clean_sf(),
    overwrite = TRUE
  )
  
}


