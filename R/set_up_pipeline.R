#' Set Up Species Evaluation Pipeline
#' 
#' This function establishes a targets pipeline for species evaluation data 
#'     pulls and evaluation templates. This function copies sub-directories and
#'     files into a specified directroy.
#' 
#' @param dir_path Path to directory you'd like your pipeline.
#' @param unit_code Unit acronym.
#'
#' @returns Nothing. 
#' @export
#'
#' @examples
#' \dontrun{
#' library("psoSppEvals")
#' scc_library <- file.path(Sys.getenv("USERPROFILE"), 
#'                          "USDA/Mountain Planning Service Group - SCC Library", 
#'                          "12_Lolo NF")
#' spp_lst_dir <- file.path(scc_library, "Species List")
#' set_up_pipeline(dir_path = spp_lst_dir, unit_code = "LNF")
#' }
set_up_pipeline <- function(dir_path, unit_code){
  template_dir = system.file("pipeline", package = "psoSppEvals")
  if(!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  fs::dir_copy(template_dir, dir_path)
  pipeline_dir = file.path(dir_path, paste0(unit_code, "_SppEval_Pipeline"))
  file.rename(file.path(dir_path, "pipeline"), pipeline_dir)
  dir.create(file.path(pipeline_dir, "data"))
  }