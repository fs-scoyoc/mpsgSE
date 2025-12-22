#' Read FWS National Wetlands Inventory 
#'
#' @param plan_area_sf 
#'
#' @returns A list of [sf] objects: riparian and wetlands
#' @export
#'
#' @examples
#' library(mpsgSE)
pull_nwi_data <- function(plan_area_sf, gdb_path){
  # plan_area_sf = targets::tar_read(plan_area)
  # gdb_path = file.path("data", "MBF_spp_eval.gdb")
  
  nwi_rest <- "https://fwspublicservices.wim.usgs.gov/wetlandsmapservice/rest/"
  riparian = arcgislayers::arc_read(
    paste0(nwi_rest, "services/Riparian/MapServer/0")
    ) |>
    # mpsgSE::clip_fc(plan_area_sf) |> 
    janitor::clean_names() |> 
    sf::st_make_valid() |> 

  sf::write_sf(obj = riparian, dsn = gdb_path, layer = "NWI_Riparian_PlanArea")
  wetlands = arcgislayers::arc_read(
    glue::glue(nwi_rest, "services/Wetlands/MapServer/0")
    ) |>
    janitor::clean_names() |> 
    sf::st_make_valid() |> 
    mpsgSE::clip_fc(plan_area_sf)
  sf::write_sf(obj = wetlands, dsn = gdb_path, layer = "NWI_Wetlands_PlanArea")
  
  return(list(riparian, wetlands))
}




# pull_nhd_data <- function(plan_area_sf, gdb_path){
#   # plan_area_sf = targets::tar_read(plan_area)
#   # gdb_path = file.path("data", "MBF_spp_eval.gdb")
#   
#   nhd_path <- "https://hydro.nationalmap.gov/arcgis/rest/services/nhd/MapServer"
#   
#   
# }