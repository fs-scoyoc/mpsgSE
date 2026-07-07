spp_list = psoSppEvals::sp_list_ex
query_field = "scientific_name"; correct = TRUE; authorship = TRUE

# Get list of distinct species.
distinct_spp = spp_list |>
  dplyr::select(dplyr::any_of(query_field)) |>
  dplyr::distinct()
# Clean text
distinct_spp$clean_name = distinct_spp |>
  dplyr::pull(query_field) |>
  stringr::str_replace("[\r\n]", " ") |>
  stringr::str_replace("[\r\n]", "") |>
  stringr::str_replace("  ", " ") |>
  stringr::str_replace("[^A-Za-z0-9 ]", "") |> 
  stringr::str_to_sentence()
# Get GBIF Taxon ID's
distinct_spp$taxon_id <- taxize::get_gbifid(
  distinct_spp$clean_name, ask = FALSE, rows = 1, messages = FALSE
)

# Correct Scientific Names with known Errors
if(correct) {
  # Read corrected names data frame
  cor_names = psoSppEvals::name_corrections
  # Match scientific names
  matched_names = match(distinct_spp$clean_name, cor_names$errored_name)
  # Correct scientific names
  distinct_spp$clean_name[!is.na(matched_names)] = cor_names$corrected_name[matched_names[!is.na(matched_names)]]
  # Correct taxon_ids
  distinct_spp$taxon_id[!is.na(matched_names)] = cor_names$taxon_id[matched_names[!is.na(matched_names)]]
}


#-- Previous method using taxize::classification()
# Pull Taxonomy from GBIF backbone taxonomy
taxonomy_list = taxize::classification(distinct_spp$taxon_id, db = "gbif") |> 
  suppressWarnings()

# Function to convert long list to wide data frame and add taxon ID's
convert_taxonomy = function(i, tax_list) {
  # i = 1; tax_list = taxonomy_list
  
  # Get GBIF IF
  g_id = names(tax_list)[[i]]
  if(!is.na(g_id)){
    # Get taxon ID
    t_id = tax_list[[i]]$id[nrow(tax_list[[i]])] |> as.character()
    asc = tax_list[[i]]$name[nrow(tax_list[[i]])]
    if(asc == 'unranked') asc = NA
    # Get taxonomy
    named_taxonomy = tax_list[[i]] |>
      dplyr::select(rank, name) |>
      tidyr::pivot_wider(names_from = rank, values_from = name) |> 
      dplyr::mutate(
        taxon_id = as.character(ifelse(is.na(t_id), g_id, t_id)),
        gbif_taxonID = g_id,
        accepted_scientific_name = asc
      )
    return(named_taxonomy)
  }
}

# Convert list to data frame
taxonomies = lapply(seq_along(taxonomy_list), convert_taxonomy,
                    taxonomy_list) |>
  dplyr::bind_rows() |> 
  dplyr::distinct()

if(authorship){
  authors = lapply(1:nrow(taxonomies), function(x){
    # x = 66
    vars = c("taxon_id", "authorship", "rank")
    sp = taxonomies[x, 'accepted_scientific_name'][[1]]
    t_id = taxonomies[x, 'taxon_id'][[1]]
    # Queas.vector()# Query taxon ID in GBIF Backbone
    if(!is.na(sp)){
      ls_dat = taxize::gbif_name_usage(name = sp)$results
      dat = lapply(sequence(length(ls_dat)), function(i){ 
        dplyr::bind_cols(ls_dat[[i]])
      }) |>
        dplyr::bind_rows() |> 
        dplyr::rename("taxon_id" = key) |>
        dplyr::filter(taxon_id == t_id) |> 
        dplyr::select(dplyr::contains(vars)) |>
        dplyr::mutate(
          taxon_id = as.character(taxon_id),
          rank = ifelse(is.na(rank), "Unranked", stringr::str_to_sentence(rank))
        ) |>
        dplyr::select(dplyr::any_of(vars)) |>
        suppressMessages()
    }
  }) |>
    dplyr::bind_rows() |> 
    dplyr::distinct()
  
  taxonomies <- dplyr::left_join(taxonomies, authors, by = "taxon_id", 
                                 relationship = 'many-to-many') |>
    dplyr::mutate(rank = ifelse(is.na(rank), "Unranked", rank))
}
