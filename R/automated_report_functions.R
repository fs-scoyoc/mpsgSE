# Functions in this script:
# -   build_quarto_params()
# -   set_up_directories()
# -   write_evlas()


#' Build Quarto Parameters for Automated Evaluation Templates
#' 
#' This function builds the Quarto parameters to produce the automated species
#'     evaluations.
#'      
#' @param spp_list Species list with taxon ID's and taxonomies from 
#'     `get_taxonomies()`.
#' @param fs_unit_name Character string for Forest Service unit name. 
#' @param states Character string or a concatenated vector for states the forest 
#'     or grassland overlaps.
#' @param crs Coordinate reference system used to mapping.
#' @param output_path The path to the directory where you want the reports saved.
#'     This can be 
#'
#' @returns A [tibble::tibble()].
#' 
#' @details
#' This function makes a data frame with the following variables:  
#'   *   taxon_id: taxonomic ID from 'get_taxonomies()'
#'   *   scientific_name: scientific name
#'   *   common_name: common name
#'   *   file_name: name of Microsoft Word document (.docx)
#'   *   subfolder_path: sub-folder path
#'   *   full_path: file path for output file
#'   *   unit_name: Forest Service unit name
#' 
#' @details
#' The sub-folder paths is assigned using taxonomy. 
#'   *   Taxa in the taxonomic Kingdom Plantae are placed in the Plants folder. 
#'   *   Taxa in the taxonomic Phylum Ascomycota are placed in the Lichens folder.  
#'   *   Taxa in the taxonomic Phylum Arthropoda and Mollusca are placed in the Invertebrates folder.  
#'   *   Taxa in the taxonomic Class Aves are placed in the Lichens folder.  
#'   *   Taxa in the taxonomic Classes Amphibia, Squamata, and Testudines are placed in the Herpetofauna folder.  
#'   *   Taxa in the taxonomic Class Mammalia are placed in the Mammals folder.  
#'   *   Taxa in the do not have a taxonomic class are placed in the Fishes folder. For some reason Fishes are returned with Class == NA with `get_taxonomies()`.
#' 
#' @seealso [get_taxonomies()]
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' library('psoSppEvals')
#' spp_list <- get_taxonomies(psoSppEvals::sp_list_ex, correct = TRUE)
#' qmd_params <- build_quarto_params(spp_list, file.path('output', 'spp_evlas'))
#' }
build_quarto_params <- function(spp_list, fs_unit_name, states, crs,
                                output_path = file.path("output/spp_evals")){
  # spp_list = targets::tar_read(nko_list)
  # output_path = "output/spp_evals"

  # spp_list |> dplyr::pull(kingdom) |> unique()
  # spp_list |> dplyr::filter(kingdom == "Animalia") |> dplyr::pull(phylum) |> 
  #   unique()
  # spp_list |> dplyr::filter(kingdom == "Animalia" & phylum == "Chordata") |> 
  #   dplyr::pull(class) |> unique()
  
  date_stamp = gsub("-", "", Sys.Date())
  qmd_params = spp_list |> 
    # dplyr::filter(new_spp == "Yes") |>
    dplyr::select(taxon_id, scientific_name, common_name, kingdom, phylum, 
                  class) |> 
    dplyr::group_by(taxon_id) |>
    dplyr::mutate(n = dplyr::n()) |>
    dplyr::filter(n == 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      subfolder = dplyr::case_when(
        kingdom == "Plantae" ~ "Plants",
        phylum == "Ascomycota" ~ "Lichens",
        phylum == "Arthropoda" ~ "Invertebrates",
        phylum == "Mollusca" ~ "Invertebrates",
        class == "Aves" ~ "Birds",
        class == "Amphibia" ~ "Herpetofauna",
        class == "Squamata" ~ "Herpetofauna",
        class == "Testudines" ~ "Herpetofauna",
        class == "Mammalia" ~ "Mammals",
        is.na(class) ~ "Fishes"
      ),
      sn_base = gsub(" ", "_", scientific_name),
      sn_base = gsub("'", "", sn_base),
      sn_base = gsub("\\.", "", sn_base),
      sn_base = gsub("ssp", "", sn_base),
      sn_base = gsub("var", "", sn_base),
      cn = stringr::str_replace_all(common_name, " ", "_"),
      cn = gsub("'", "", cn),
      subfolder_path = glue::glue("{output_path}/{subfolder}"),
      file_name = glue::glue("{date_stamp}_AUTO_GENERATED_{cn}_{sn_base}.docx"),
      output_file = glue::glue("{subfolder_path}/{file_name}"), 
      unit_name = fs_unit_name, states = states, crs = crs
    ) |>
    dplyr::select(taxon_id, scientific_name, common_name, unit_name, states, 
                  crs, file_name, subfolder_path, output_file)
  return(qmd_params)
}


#' Create directories for automated evaluations
#' 
#' This function creates the sub-directories for the automated evaluation 
#'     reports using the sub-directory paths from `build_quarto_params()`.
#'
#' @param quarto_params Data frame of Quarto parameters from 
#'     `build_quarto_params()`.
#' 
#' @returns Nothing.
#' 
#' @seealso [build_quarto_params()]
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' library('psoSppEvals')
#' spp_list <- get_taxonomies(psoSppEvals::sp_list_ex, correct = TRUE)
#' qmd_params <- build_quarto_params(spp_list, file.path('output', 'spp_evlas'), 
#'                                   "Smokey Bear National Forest")
#' setup_directories(qmd_params)
#' }
set_up_directories <- function(quarto_params){
  # quarto_params = targets::tar_read(rpt_qmd_params)
  
  # Function to ask the user if they want to proceed
  # ask_to_proceed <- function(message = "Do you want to proceed? (y/n): ") {
  #   repeat {
  #     # Prompt the user
  #     answer <- tolower(trimws(readline(prompt = message)))
  #     
  #     # Validate input
  #     if (answer %in% c("y", "yes")) {
  #       cat("Proceeding...\n")
  #       return(TRUE)
  #     } else if (answer %in% c("n", "no")) {
  #       cat("Operation cancelled by user.\n")
  #       return(FALSE)
  #     } else {
  #       cat("Invalid input. Please enter 'y' or 'n'.\n")
  #     }
  #   }
  # }
  
  # List directories
  new_dirs = unique(quarto_params$subfolder_path)
  
  new_dirs = tibble::tibble(dir = unique(quarto_params$subfolder_path))
  dirs = tidyr::separate_wider_delim(new_dirs, dir, "/", names_sep = "")
  sub_dir = do.call(paste, c(dirs[, -ncol(dirs)], sep = "/")) |> unique()
  out_dir = paste(getwd(), sub_dir, sep = "/")
  message(glue::glue("Evaluation folders will be created in: 
                     {out_dir}"))
  # Create directories
  lapply(new_dirs$dir, function(d){
    # d = new_dirs$dir[1]
    nd = file.path(getwd(), d)
    if(!dir.exists(nd)) dir.create(nd, recursive = TRUE)
    })
  
  # Verify creation of directories and run
  # if (ask_to_proceed()) {
  #   # Code to run if user agrees
  #   cat("Creating directories.\n")
  #   lapply(new_dirs$dir, function(d){
  #     # d = new_dirs$dir[1]
  #     nd = file.path(getwd(), d)
  #     if(!dir.exists(nd)) dir.create(nd)
  #   })
  # } else {
  #   # Code to run if user declines
  #   stop("Directories not created. Pipeline stopped.", call. = TRUE)
  # }
  
}


#' Produce Automated Evaluation Template Documents
#' 
#' This function runs the Quarto script for each species in the data frame 
#'     produced from `build_quarto_params()`.
#'
#' @param quarto_params Data frame from [build_quarto_params()].
#' @param qmd_path Path to quarto script. Default is the working directory.
#' 
#' @details
#' For each species in the data frame, this function runs 
#'     `quarto::quarto_render()`. This function produces a document in the 
#'     current working directory. This function then uses `file.copy()` to copy
#'     the document to the output file path in the *output_file* variable. The
#'     document in the current working directory is then deleted using 
#'     `file.remove()`. On occation, the file in the current working directory 
#'     is not removed and will need to be manually deleted.
#'
#' @returns Nothing.
#' 
#' @seealso [build_quarto_params()], [quarto::quarto_render()], [file.copy()], [file.remove()]
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' library('psoSppEvals')
#' spp_list <- get_taxonomies(psoSppEvals::sp_list_ex, correct = TRUE)
#' qmd_params <- build_quarto_params(spp_list, file.path('output', 'spp_evlas'), 
#'                                   "Smokey Bear National Forest")
#' setup_directories(qmd_params)
#' write_evals(qmd_params)
#' }
write_evals <- function(quarto_params, 
                        qmd_path = file.path("species_evaluation.qmd")){
  # quarto_params = qmd_params
  
  lapply(quarto_params$taxon_id, function(t_id){
    # t_id = quarto_params$taxon_id[1]
    sp = dplyr::filter(quarto_params, taxon_id == t_id)
    params = dplyr::select(sp, taxon_id, unit_name, states, crs)
    message(paste0(t_id, ": ", sp$common_name, " (", sp$scientific_name, ")"))
    quarto::quarto_render(qmd_path, 
                          output_file = sp$file_name,
                          execute_params = params)
    file.copy(sp$file_name, file.path(sp$output_file), overwrite = TRUE)
    file.remove(sp$file_name)
  })
}