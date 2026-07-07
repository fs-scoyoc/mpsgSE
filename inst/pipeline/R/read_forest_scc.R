# This script contains functions that will need to be modified for your pipeline

#' Read the Bitterroot SCC list into this pipeline.
#' 
#' @param xlsx_path Path to Excel file.
#' 
#' @return A [tibble::tibble()].
read_forest_scc <- function(xlsx_path) {
  # xlsx_path = file.path("data", "LNFPR_BitLoSCC_SpToConsider.xlsx")
  # readxl::excel_sheets(xlsx_path)
  
  dat = readxl::read_excel(xlsx_path, sheet = "LoloBittSCCEval") |>
    janitor::clean_names() |> 
    dplyr::select(scientific_name, common_name, forest) |> 
    dplyr::filter(forest == "Bitterroot") |> 
    psoSppEvals::get_taxonomies(correct = TRUE)
  return(dat)
}

