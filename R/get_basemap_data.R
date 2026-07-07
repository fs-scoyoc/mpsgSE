# Function in this script:
# -   get_basemap_data()
# -   build_basemap_data()


#' Get base map data
#' 
#' @description
#' This function pulls spatial base map data for North and South America, the 
#'     lower 48 US states, and a user specified National Forest. Continental- 
#'     and national-scale data are acquired using the `rnaturalearth` package 
#'     and Forest Service data are acquired from Forest Service ArcGIS Rest 
#'     Services (https://apps.fs.usda.gov/arcx/rest/services/EDW) using 
#'     `arcgislayers` package.
#'
#' @param forest_name Character. Name of national forest or grassland of 
#'     interest.
#' @param admin_bndry Optional. An `sf` object of the administrative boundary. 
#'     Default is TRUE. If TRUE, the *EDW_ForestSystemBoundaries_01* layer will 
#'     be read in from the Forest Service ArcGIS REST service using 
#'     [psoGIStools::read_edw_lyr()] and filtered using *forest_name*.
#' @param plan_area Optional. An `sf` object of the administrative boundary. 
#'     Default is TRUE. If TRUE, the *EDW_SurfaceOwnership_01* layer will 
#'     be read in from the Forest Service ArcGIS REST service using 
#'     [psoGIStools::read_edw_lyr()] and clipped to the admin_bndry, then 
#'     filtered for Forest Service land.
#' @param target_crs Target coordinate reference system. Default is EPSG:4326 
#'     (WGS84).
#'
#' @details
#' `get_basemap_data` returns a list of spatial features used to produce 
#'     automated species evaluations.
#' @details
#' Continental-scale data include 'americas', 'north_america', and 'l_48'. These 
#'     data are acquired using [rnaturalearth::ne_countries()] and 
#'     [rnaturalearth::ne_states()] functions. These data are in the NAD83 CONUS 
#'     Albers (EPSG:5070).
#' @details
#' National Forest-scale data include 'admin_bndry', 'plan_area',  'aoa', 
#'     and 'plan_area_doughnut'. 'admin_bndry' and 'plan_area' are acquired 
#'     using [read_edw_lyr()]. 'aoa' and 'plan_area_doughnut' are derived using 
#'     the [sf] package. 'aoa', or area of analysis, is a 4828 meter (3 mile) 
#'     buffer of 'admin_bndry' using [sf::st_buffer()]. 'plan_area_doughnut' is 
#'     a 1000 meter buffer of 'plan_area' with 'plan_area' erased using 
#'     [sf::st_difference()]. These data are returned in the coordinate 
#'     reference system provided by the `crs` parameter.
#' @note
#' Sometimes the connection to the Forest Service REST Service  fails. This will 
#'     throw the following error message: "**Error:** tar_make() Status code: 
#'     500. Error: json". In most instances, the function can simply be executed 
#'     again to retrieve the data. Check the data servers if the error is 
#'     persistent. 
#' @returns A list of [sf] objects.
#' @seealso [read_edw_lyr()], [rnaturalearth::ne_countries()], 
#'          [rnaturalearth::ne_states], [osmdata::osmdata_sf()]
#' @export
#'
#' @examples
#' \dontrun{
#' library(psoSppEvals)
#' forest_name <- "Dixie National Forest"
#' basemap_data <- get_basemap_data(forest_name)
#' }
get_basemap_data = function(forest_name, admin_bndry = TRUE, plan_area = TRUE, 
                            target_crs = "EPSG:4326"){
  
  # forest_name = "White River National Forests"
  # target_crs = "EPSG:26913"
  # admin_bndry = targets::tar_read(admin_bndry)
  # plan_area = targets::tar_read(plan_area)
  
  message("Western hemisphere")
  americas = rnaturalearth::ne_countries(scale = "medium",
                                         continent = c("North America",
                                                       "South America"),
                                         returnclass = "sf") |>
    dplyr::filter(name != "Hawaii") |>
    sf::st_transform(crs = 5070)
  
  message("North America")
  north_america_c = rnaturalearth::ne_countries(
    scale = "medium", continent = "North America", returnclass = "sf"
  ) |>
    dplyr::select(name) |>
    dplyr::filter(name != "United States of America")
  north_america_s = rnaturalearth::ne_states(
    country = c("United States of America", "Canada", "Mexico"), 
    returnclass = "sf"
  ) |>
    dplyr::filter(name != "Hawaii") |>
    dplyr::select(name = name_en)
  north_america = dplyr::bind_rows(north_america_c, north_america_s) |>
    sf::st_transform(crs = 5070)
  
  message("Lower 48 states")
  l_48 = rnaturalearth::ne_states(country = c("United States of America")) |>
    sf::st_as_sf() |>
    dplyr::filter(name != "Hawaii", name != "Alaska") |>
    sf::st_transform(crs = 5070)
  
  
  message("FS Boundaries")
  # Administrative Boundary
  if (isTRUE(admin_bndry)) {
    admin_bndry = psoGIStools::read_edw_lyr("EDW_ForestSystemBoundaries_01") |>
      dplyr::filter(forestname == unit_name) |> 
      sf::st_transform(target_crs) |>
      sf::st_make_valid()
    }
  # Plan Area (Forest Service Land)
  if (isTRUE(plan_area)) {
    plan_area = psoGIStools::read_edw_lyr("EDW_SurfaceOwnership_01") |>
      psoGIStools::clip_sf(admin_bndry) |>
      dplyr::filter(ownerclassification == "USDA FOREST SERVICE") |>
      sf::st_transform(target_crs) |>
      sf::st_make_valid()
    }
  # # Ranger Districts
  # if (isTRUE(districts)){
  #   dists = psoSppEvals::read_edw_lyr("EDW_RangerDistricts_03", layer = 1) |>
  #           dplyr::filter(forestname == unit_name) |>
  #     sf::st_transform(target_crs) |>
  #     sf::st_make_valid()
  #   } else (dists = districts)
  # Area of Analysis
  aoa = sf::st_buffer(admin_bndry, units::as_units(3,"mi")) |>
    sf::st_transform(target_crs) |>
    sf::st_make_valid()
  # Buffer
  plan_area_doughnut = sf::st_buffer(plan_area, units::as_units(1,"km")) |>
    sf::st_difference(plan_area) |>
    sf::st_transform(target_crs) |>
    sf::st_make_valid() |>
    suppressWarnings()

  #-- Roads
  # message("Roads")
  # # Area of Analysis
  # roads_aoa = sf::st_buffer(admin_bndry, 1000000) |> sf::st_bbox()
  # # Roads
  # roads = lapply(states, function(state){
  #   osm_dat = osmdata::getbb(state) |> 
  #     osmdata::opq() |>
  #     osmdata::add_osm_feature("highway", 
  #                              value = c("motorway", "trunk", "primary")) |>
  #     osmdata::osmdata_sf()
  #   return(osm_dat$osm_lines)
  # }) |> 
  #   dplyr::bind_rows() |>
  #   sf::st_transform(crs) |> 
  #   sf::st_crop(roads_aoa) |> 
  #   suppressWarnings()
  
  #-- Assemble final data set
  dat = tibble::lst(americas, north_america, l_48, aoa, admin_bndry, plan_area,
                    plan_area_doughnut)
  return(dat)
}



#' Build base map data
#' 
#' @description
#' This is an alternative function to build base map data for the automated 
#'     reports. Spatial base map data for North and South America, the lower 48 US 
#'     states, and a user specified National Forest is returned in one concise 
#'     list. 
#'     
#'     Continental and national scale data are acquired using the 
#'     `rnaturalearth` package and Forest Service data provided from the user.
#'
#' @param admin_bndry An 'sf' object of the administrative Forest Service 
#'     boundary.
#' @param plan_area An 'sf' object of the plan area (Forest Service land).
#' @param target_crs The target coordinate reference system. The default is 
#'      EPSG:4326 (WGS 84).
#'
#' @returns A list of [sf] objects.
#' @seealso [read_edw_lyr()], [rnaturalearth::ne_countries()], 
#'          [rnaturalearth::ne_states], [osmdata::osmdata_sf()]
#' @export
#'
#' @examples
#' message("Coming soon. To a theater near you...")
build_basemap_data = function(admin_bndry, plan_area, 
                              target_crs = "EPSG:4326"){
  
  # Load these parameters to troubleshoot/modify this function
  # target_crs = "EPSG:26913"
  # admin_bndry = targets::tar_read(proc_bndry)
  # plan_area = targets::tar_read(plan_area)
  # states = c("Utah", "Nevada", "Arizona")
  
  message("Western hemisphere")
  americas = rnaturalearth::ne_countries(scale = "medium",
                                         continent = c("North America",
                                                       "South America"),
                                         returnclass = "sf") |>
    dplyr::filter(name != "Hawaii") |>
    sf::st_transform(crs = 5070)
  
  message("North America")
  north_america_c = rnaturalearth::ne_countries(
    scale = "medium", continent = "North America", returnclass = "sf"
  ) |>
    dplyr::select(name) |>
    dplyr::filter(name != "United States of America")
  north_america_s = rnaturalearth::ne_states(
    country = c("United States of America", "Canada", "Mexico"), 
    returnclass = "sf"
  ) |>
    dplyr::filter(name != "Hawaii") |>
    dplyr::select(name = name_en)
  north_america = dplyr::bind_rows(north_america_c, north_america_s) |>
    sf::st_transform(crs = 5070)
  
  message("Lower 48 states")
  l_48 = rnaturalearth::ne_states(country = c("United States of America")) |>
    sf::st_as_sf() |>
    dplyr::filter(name != "Hawaii", name != "Alaska") |>
    sf::st_transform(crs = 5070)
  
  message("FS Boundaries")
  # Area of Analysis
  aoa = sf::st_buffer(admin_bndry, units::as_units(3,"mi")) |> 
    sf::st_transform(target_crs) |> 
    sf::st_make_valid()
  # Buffer
  plan_area_doughnut = sf::st_buffer(plan_area, units::as_units(1,"km")) |> 
    sf::st_difference(plan_area) |> 
    sf::st_transform(target_crs) |> 
    sf::st_make_valid() |> 
    suppressWarnings()
  
  #-- Assemble final data set
  dat = tibble::lst(americas, north_america, l_48, aoa, admin_bndry, plan_area, 
                    plan_area_doughnut)
  return(dat)
}

