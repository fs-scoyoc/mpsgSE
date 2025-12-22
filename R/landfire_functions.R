#' Download LANDFIRE Existing Vegetation Type (250EVT)
#' 
#' This function downloads and saves LANDFIRE Existing Vegetation Type (250EVT) 
#'     raster data and then summarizes the data by EVT Name and EVT Physiognomy 
#'     for the Plan Area (FS lands).
#'
#' @param plan_area_sf Plan Area `sf` polygon object.
#' @param lf_dir Directory path to save raster to.
#' @param email_address Email address. Passesd on to [rlandfire::landfireAPIv2()].
#' @param res Raster resolution. Default is 30.
#'
#' @returns A list of two data frames summarizing area of EVT.
#' @export
#'
#' @examples
#' library(mpsgSE)
pull_landfire <- function(plan_area_sf, lf_dir, email_address, res = 30){
  # plan_area_sf = targets::tar_read(plan_area)
  # lf_dir = file.path("data", "LANDFIRE")
  # email_address = Sys.getenv("GBIF_EMAIL")

  # Create directory if it does not exist
  if(!dir.exists(lf_dir)) dir.create(lf_dir)
  # Transform AoA to WGS 84
  aoa_sf = sf::st_buffer(plan_area_sf, 1000) |> 
    sf::st_transform(crs = "epsg:4326")
  # Generate AoA wkt string
  lf_aoi = rlandfire::getAOI(aoa_sf)
  # Pull EVT data from LANDFIRE API
  resp = rlandfire::landfireAPIv2(products = "250EVT", aoi = lf_aoi, 
                                   email_address, resolution = res, 
                                   path = tempfile(fileext = ".zip"), 
                                   method = 'auto', verbose = FALSE)
  # Unzip raster and save to lf_dir
  utils::unzip(resp$path, exdir = lf_dir)
  # Read raster into R
  lf = terra::rast(
    list.files(lf_dir, pattern = ".tif$", full.names = TRUE, recursive = TRUE)
  )
  # Transform AoA to raster CRS
  plan_area_proj = terra::vect(plan_area_sf) |> terra::project(terra::crs(lf))
  # Mask raster to AoA
  lf_aoa = terra::mask(lf, plan_area_proj)
  # Save raster
  # terra::writeRaster(
  #   x = lf_aoa,
  #   filename = file.path(lf_dir, "LANDFIRE_250EVT_AdminBndry.tif"),
  #   overwrite = TRUE
  # )
  # Read attribute table into R
  attr_table = foreign::read.dbf(
    list.files(lf_dir, pattern = ".dbf$", full.names = TRUE, recursive = TRUE)
  ) |> 
    janitor::clean_names() |> 
    dplyr::select(-count, -lfrdb)
  # Summarize masked raster and join attribut table
  evt_data = terra::extract(lf_aoa, aoa_sf) |> 
    janitor::clean_names() |> 
    dplyr::group_by(evt_name) |> 
    dplyr::summarise(count = dplyr::n()) |> 
    dplyr::left_join(attr_table, by = "evt_name") |> 
    dplyr::mutate(area_m2 = count * prod(terra::res(lf_aoa)), 
                  acres = area_m2 / 4046.86)
  phys_data = evt_data |> 
    dplyr::group_by(evt_phys, evt_gp_n, evt_sbcls, evt_name) |> 
    dplyr::summarise(count = sum(count), 
                     area_m2 = sum(area_m2), 
                     acres = sum(acres), 
                     .groups = 'drop')
  # Return summarized data
  return(list("EVT" = evt_data, "PHYS" = phys_data))
}


#' Extract EVT Data to Points
#'
#' @param spp_list List of species with taxon ID's from `get_taxonomies()`.
#' @param spp_pts_sf [sf] points object of species occurrences with taxon ID's 
#'     from `get_taxonomies()`.
#' @param lf_dir Path to LANDFIRE directory.
#'
#' @returns A list.
#' @export
#'
#' @examples
#' library(mpsgSE)
extract_landfire_evt <- function(spp_list, spp_pts_sf, lf_dir){
  # spp_list = targets::tar_read(nat_known_list)
  # spp_pts_sf = targets::tar_read(all_occ_data)
  # lf_dir = file.path("data", "LANDFIRE")
  
  # Read in raster
  lf = terra::rast(
    list.files(lf_dir, pattern = ".tif$", full.names = TRUE, recursive = TRUE)
  )
  # Read in attribute table
  attr_table = foreign::read.dbf(
    list.files(lf_dir, pattern = ".dbf$", full.names = TRUE, recursive = TRUE)
  ) |> 
    janitor::clean_names() |> 
    dplyr::select(-count, -lfrdb)
  # Extract to points
  pts = terra::vect(spp_pts_sf) |> terra::project(terra::crs(lf))
  pts$evt_name = terra::extract(lf, pts)$EVT_NAME
  # Summarize by species
  spp_dat = spp_list |> 
    dplyr::select(taxon_id, common_name, scientific_name, broad_group, 
                  fine_group)
  pts_dat <- terra::as.data.frame(pts) |>
    dplyr::left_join(attr_table, by = "evt_name")
  #-- EVT Name
  evt_pts = pts_dat |> 
    dplyr::group_by(taxon_id, evt_name) |> 
    dplyr::summarize(n = dplyr::n(), .groups = 'drop') |> 
    tidyr::pivot_wider(names_from = evt_name, values_from = n, values_fill = 0)
  evt_pts <- dplyr::left_join(spp_dat, evt_pts, by = 'taxon_id')
  # Physiognomy
  phys_pts = pts_dat |> 
    dplyr::group_by(taxon_id, evt_phys) |> 
    dplyr::summarize(n = dplyr::n(), .groups = 'drop') |> 
    tidyr::pivot_wider(names_from = evt_phys, values_from = n, values_fill = 0)
  phys_pts <- dplyr::left_join(spp_dat, phys_pts, by = 'taxon_id')
  # Return summarized data
  return(list("EVT" = evt_pts, "PHYS" = phys_pts))
}