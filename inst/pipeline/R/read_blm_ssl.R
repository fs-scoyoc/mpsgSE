# This script contains functions that will need to be modified for your pipeline

#' Read the BLM Sensitive Species List into this pipeline.
#' 
#' @param xlsx_path Path to Excel file.
#' 
#' @return A [tibble::tibble()].
read_blm_ssl <- function(xlsx_path) {
  # xlsx_path = file.path("data", "BLM MT_2009_Special Status Species List.xlsx")
  # readxl::excel_sheets(xlsx_path)
  
  dat = readxl::read_excel(xlsx_path, sheet = "SSL") |>
    janitor::clean_names() |> 
    dplyr::select(scientific_name, common_name) |> 
    psoSppEvals::get_taxonomies(correct = TRUE)
  return(dat)
}

