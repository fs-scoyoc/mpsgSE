# This script example functions that will need to be modified for your pipeline

#' Read the South Dakota SWAP list into this pipeline
#'
#' @param xlsx_path Path to Excel workbook.
#'
#' @returns A [tibble::tibble()].
read_sd_swap <- function(xlsx_path){
  swap = readxl::read_excel(xlsx_path, sheet = "South Dakota SWAP table") |>
    janitor::clean_names() |> 
    dplyr::filter(!is.na(scientific_name)) |> 
    mpsgSE::get_taxonomies(correct = TRUE)
  return(swap)
}


#' Read the South Dakota SGCN list into this pipeline
#'
#' @param xlsx_path Path to Excel workbook.
#'
#' @returns A [tibble::tibble()].
read_wy_swap <- function(xlsx_path){
  swap = readxl::read_excel(xlsx_path, sheet = "SWAP 2027 Proposed SGCN") |>
    janitor::clean_names() |>
    dplyr::filter(!is.na(scientific_name)) |> 
    dplyr::mutate(wy_swap = paste("Tier", tier, sep = " "))
  sgcn = readxl::read_excel(xlsx_path, sheet = "Plants") |>
    janitor::clean_names() |>
    dplyr::filter(!is.na(scientific_name)) |> 
    dplyr::mutate(wy_swap = "SGCN")
  dat = rbind(dplyr::select(swap, scientific_name, common_name, wy_swap), 
              dplyr::select(sgcn, scientific_name, common_name, wy_swap)) |>
    mpsgSE::get_taxonomies(correct = TRUE)
  return(dat)
}