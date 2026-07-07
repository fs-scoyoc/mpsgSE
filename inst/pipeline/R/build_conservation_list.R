# This script contains functions that will need to be modified for your pipeline
# 
# List of functions:
#   -   `build_conservation_list()`
#   -   `get_eligible_tids()`


#' Build Master Conservation Status List
#' 
#' This function builds a master conservation status species list. This function
#'     will need significant updating for you project. `ns_list`, `rfss_list`, 
#'     and `bcc_list` will not be changed. You will need to update the state
#'     SWAP or T&E lists and adjacent BLM and/or forest SCC lists.
#' 
#' **NOTE**: Don't forget to modify `get_eligible_tids()` below.
#'
#' @param natureserve_data NatureServe status list from this pipeline.
#' @param rfss_list Regional Foresters Sensitive Species list from this pipeline.
#' @param bcc_list USFWS Birds of Conservation Concern list from this pipeline.
#' @param ut_swap Utah SWAP list from this pipeline.
#' @param ut_blm_ssl BLM sensitive species list from this pipeline.
#'
#' @return A [tibble::tibble()]
build_conservation_list <- function(ns_list, rfss_list, bcc_list, ut_swap, 
                                    ut_blm_ssl, mlsnf_scc_list){
  
  # Load these parameters to troubleshoot/modify this function
  # targets::tar_load(ns_list); targets::tar_load(rfss_list)
  # targets::tar_load(bcc_list); targets::tar_load(ut_swap)
  # targets::tar_load(ut_blm_ssl); targets::tar_load(mlsnf_scc_list)
  
  # NatureServe
  ns = ns_list |> 
    dplyr::select(taxon_id, scientific_name, common_name, broad_group, 
                  fine_group, esa_status, gRank, rounded_gRank, 
                  dplyr::contains("sRank")) |> 
    dplyr::distinct(taxon_id, .keep_all = TRUE)
  # Regional Foresters Sensitive Species
  rfss = rfss_list |> 
    dplyr::select(taxon_id) |> 
    dplyr::distinct() |> 
    dplyr::mutate('r4_ssl' = "Yes")
  # Birds of Conservation Concern
  bcc = bcc_list |> 
    dplyr::select(taxon_id) |> 
    dplyr::distinct() |> 
    dplyr::mutate(usfws_bcc = "Yes")
  # Utah SWAP
  ut = dplyr::select(ut_swap, taxon_id) |> 
    dplyr::mutate("ut_swap" = "Yes")
  # BLM SSL
  blm = dplyr::select(ut_blm_ssl, taxon_id, blm_status) |> 
    dplyr::filter(stringr::str_detect(blm_status, "S")) |> 
    dplyr::mutate(blm_ssl = "Yes") |> 
    dplyr::select(-blm_status)
  # Manti-La Sal SCC
  mlsnf <- dplyr::select(mlsnf_scc_list, taxon_id) |> 
    dplyr::mutate("mlsnf_scc" = "Yes")
  # Compile list
  master_list = ns |> 
    dplyr::left_join(rfss, by = 'taxon_id') |> 
    dplyr::left_join(bcc, by = 'taxon_id') |> 
    dplyr::left_join(ut, by = 'taxon_id') |> 
    dplyr::left_join(blm, by = 'taxon_id') |> 
    dplyr::left_join(mlsnf, by = 'taxon_id') |> 
    dplyr::mutate(r4_ssl = ifelse(is.na(r4_ssl), "No", r4_ssl), 
                  usfws_bcc = ifelse(is.na(usfws_bcc), "No", usfws_bcc), 
                  ut_swap = ifelse(is.na(ut_swap), "No", ut_swap), 
                  blm_ssl = ifelse(is.na(blm_ssl), "No", blm_ssl), 
                  mlsnf_scc = ifelse(is.na(mlsnf_scc), "No", mlsnf_scc))  
  # Identify eligigle species
  elig_list = get_eligible_tids(master_list)
  master_list = master_list |> 
    dplyr::mutate(elig_scc = ifelse(taxon_id %in% elig_list$taxon_id, 
                                    "Yes", "No"))
  # Return data
  return(master_list)
}


#' Produce a vector of SCC Eligible taxon IDs
#' 
#' This function will need significant updating for you project. `gr`, `st`, 
#'     `rss`, and `bcc` will not be changed. You will need to update the state
#'     SWAP or T&E lists and adjacent BLM and/or forest SCC lists.
#'
#' @param master_list Master conservation status list from this pipeline.
#'
#' @return A vector
get_eligible_tids <- function(master_list){
  
  # Filter by status
  #-- G-rank
  gr = dplyr::filter(master_list,
                     stringr::str_detect(rounded_gRank, "G1|G2|G3|T1|T2|T3")) |> 
    dplyr::pull(taxon_id)
  #-- S-rank
  st = dplyr::select(master_list, taxon_id, dplyr::contains("sRank")) |> 
    dplyr::filter(dplyr::if_any(dplyr::where(is.character), 
                                function(x){
                                  stringr::str_detect(x, "S1|S2|T1|T2")
                                })) |> 
    dplyr::pull(taxon_id)
  #-- Regional Forester's SSL
  rss = dplyr::filter(master_list, r4_ssl == "Yes") |> dplyr::pull(taxon_id)
  #-- Birds of Conservation Concern
  bcc = dplyr::filter(master_list, usfws_bcc == "Yes") |> dplyr::pull(taxon_id)
  #-- UT SWAP
  sw = dplyr::filter(master_list, ut_swap == "Yes") |> dplyr::pull(taxon_id)
  #-- BLM SSL
  blm = dplyr::filter(master_list, blm_ssl == "Yes") |> dplyr::pull(taxon_id)
  #-- MLSNF SCC
  mls = dplyr::filter(master_list, mlsnf_scc == "Yes") |> dplyr::pull(taxon_id)
  #-- Listed under ESA
  threatened = master_list |> 
    dplyr::filter(stringr::str_detect(tolower(esa_status), "threatened")) |> 
    dplyr::filter(!stringr::str_detect(tolower(esa_status), 
                                       "proposed threatened")) |> 
    dplyr::pull(taxon_id)
  endangered = master_list |> 
    dplyr::filter(stringr::str_detect(tolower(esa_status), "endangered")) |> 
    dplyr::pull(taxon_id)
  
  # Combine taxon lists
  elig_tids = master_list |> 
    dplyr::filter(taxon_id %in% c(gr, st, rss, bcc, sw, blm, mls)) |> 
    dplyr::filter(!taxon_id %in% c(threatened, endangered))
  
  return(elig_tids)
}
