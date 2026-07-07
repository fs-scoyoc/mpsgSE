# Functions in this script:
# -   build_gbif_spatial_data()
# -   get_gbif_data()
# -   get_gbif_occ_data()
# -   build_gbif_spp()
# -   wkt_string()
# -   gbif_spatial()


#' Subset eligible species from GBIF data and reduce variables.
#'
#' @param gbif_data Spatial GBIF data from [get_gbif_data()].
#' @param spp_list Species list that includes taxon ID from [get_taxonomies()].
#'                     This is the list that is used to subset the spatial data.
#'
#' @return An [sf] object.
#'
#' @seealso [get_gbif_data()], [build_gbif_spp()], [get_taxonomies()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library("psoSppEvals")
#'
#' # Project directory path
#' t_path <- file.path("T:/path/to/project/directory/data")
#'
#' # Pull data from existing GBIF query
#' gbif_dat <- get_gbif_data(gbif_key = '9999999-999999999999999', 
#'                           t_path = t_path)
#' # Summarize species
#' gbif_list <- build_gbif_spp(gbif_dat)
#' # Subset data
#' birds <- dplyr::filter(gbif_list, class == "Aves")
#' # Subset spatial data
#' gbif_birds <- build_gbif_spatial_data(gbif_dat, birds)
#' }
build_gbif_spatial_data <- function(gbif_data, spp_list) {
  # Variable names to reduce data frame
  var_names <- c(
    "taxon_id", "gbifID", "occurrenceID", "scientificName",
    "acceptedScientificName", "verbatimScientificName", "vernacularName",
    "basisOfRecord", "eventDate", "parsed_date", 
    "countryCode", "stateProvince", "county", "locality", "verticalDatum",
    "coordinateUncertaintyInMeters", "coordinatePrecision",
    "georeferencedBy", "georeferencedDate", "georeferenceProtocol",
    "georeferenceSources", "georeferenceRemarks",
    "publisher", "institutionCode", "collectionCode", "datasetName",
    "gbif_occ_url",
    "taxonRank", "kingdom", "phylum", "class", "order", "family", "genus", 
    "specificEpithet", "infraspecificEpithet"
  )

  # Filter GBIF Data
  eligible_gbif <- gbif_data |>
    # dplyr::mutate(taxon_id = acceptedTaxonKey) |>
    dplyr::filter(taxon_id %in% spp_list$taxon_id) |>
    dplyr::mutate(gbif_occ_url = paste0("https://www.gbif.org/occurrence/",
                                        as.character(gbifID/1))) |>
    dplyr::select(dplyr::any_of(var_names))

  # Return final data frame
  return(eligible_gbif)
}


#' Get GBIF occurrence records for an area of analysis
#'
#' This function queries GBIF for species occurrence records for a given area,
#'     or polygon (`sf` object), and then reads the data into R.
#'
#' @param gbif_key The 22-digit GFIB key including hyphen for the data package.
#'     Use "new" for new GBIF queries.
#' @param t_path The directory path where the GBIF data package is or will be
#'     stored.
#' @param aoa_poly Spatial polygon (sf) of the area of analysis. This is 
#'     required when *gbif_key* is set to **new**. Default is NULL.
#' @param gbif_user Your GBIF user name. This is required when *gbif_key* is set
#'     to **new**. Default is NULL.
#' @param gbif_pwd Your GBIF password. This is required when *gbif_key* is set
#'     to **new**. Default is NULL.
#' @param gbif_email Your GBIF email address. This is required when *gbif_key*
#'     is set to **new**. Default is NULL.
#' @param gbif_format The format of the data returned from GBIF. Default is
#'     Darwin-Core Achrive (DWAC). See `rgbif::occ_download()` for more details.
#' @param crs Target coordinate reference system (CRS). Either and 
#'     `sf::st_crs()` object or accepted input string (e.g. "NAD83"). See 
#'     `sf::st_crs()` for more details. Default is NULL. If NULL, resulting `sf` 
#'     object CRS will be WGS84.
#' @param process_data Logical. Process data after reading them into R (TRUE ==
#'     yes, FALSE == no). Default is TRUE. The processing step
#' @param correct Logical. Run `correct_taxon_ids()` on data. Default is TRUE.
#'
#'   1. filters the data for species, subspecies, and varieties,
#'   2. filters the data for present records,
#'   3. filters against fossil records,
#'   4. assembles clean scientific names (i.e., without authority) from the
#'   genus, specific epithet, and infraspecific epithet, and
#'   5. attempts to parse dates, day of year, and year values.
#'
#' @return An sf class object.
#'
#' @details
#' This function submits a records request using the polygon provided to 
#'     spatially query GBIF records in 
#'     [Darwin Core Archive format](https://www.gbif.org/darwin-core) using 
#'     [rgbif::pred_within()]. GBIF records requests are staged on GBIF servers 
#'     and are then downloaded to a local or network directory using 
#'     [rgbif::occ_download()]. Finally, the data are read into R using 
#'     [rgbif::occ_download_get()] and [rgbif::occ_download_import()].
#'
#' @seealso [rgbif::pred_within()], [rgbif::occ_download()],
#'          [rgbif::occ_download_wait()], [rgbif::occ_download_get()],
#'          [rgbif::occ_download_import()], [sf::st_crs()], 
#'          [correct_taxon_ids()]
#' @export
#'
#' @examples
#' \dontrun{
#' library("psoGIStools")
#' library("psoSppEvals")
#'
#' # Read spatial data into R
#' t_path <- file.path("T:/path/to/project/directory")
#' gdb_path <- file.path(t_path, "GIS_Data.gdb")
#' sf_aoa <- read_fc(lyr = "AdminBdy_1kmBuffer", dsn = gdb_path, crs = "NAD83")
#'
#' # New GBIF data query
#' gbif_dat <- get_gbif_data(gbif_key = 'new',
#'                           t_path = file.path(t_path, "data"),
#'                           aoa_wkt = wkt_string(sf_aoa),
#'                           gbif_user = Sys.getenv("GBIF_USER"),
#'                           gbif_pwd = Sys.getenv("GBIF_PWD"),
#'                           gbif_email = Sys.getenv("GBIF_EMAIL"),
#'                           crs = 'NAD83')
#'
#' # Pull data from existing GBIF query
#' gbif_dat <- get_gbif_data(gbif_key = '9999999-999999999999999',
#'                           t_path = file.path(t_path, "data"),
#'                           crs = 'NAD83')
#' }
get_gbif_data <- function(gbif_key, t_path, aoa_poly = NULL, gbif_user = NULL,
                     gbif_pwd = NULL, gbif_email = NULL, gbif_format = "DWCA",
                     crs = NULL, process_data = TRUE, correct = TRUE){
  #-- Function variables
  # GBIF data package file path
  gbif_path = if(!gbif_key == "new"){
    file.path(t_path, paste0(gbif_key, ".zip"))
  } else(NULL)
  # Date formats
  date_formats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d", "%Y-%m", "%Y", "ymd HMS",
                   "ymd", "ymd HM")

  #-- Pull GBIF Data
  if(gbif_key == "new"){
    message("Requesting data from GBIF")
    gbifPred = rgbif::pred_within(wkt_string(aoa_poly))
    gbifDwnld = rgbif::occ_download(gbifPred, user = gbif_user, pwd = gbif_pwd,
                                     email = gbif_email, format = gbif_format)
    rgbif::occ_download_wait(gbifDwnld)
    gbif = rgbif::occ_download_get(gbifDwnld, path = file.path(t_path)) |>
      rgbif::occ_download_import()
  } else if(file.exists(gbif_path)){
    message("Reading GBIF data into R")
    gbif = rgbif::occ_download_import(key = gbif_key, path = file.path(t_path))
  } else({
    message("Downloading GBIF data")
    gbif = rgbif::occ_download_get(key = gbif_key, path = file.path(t_path)) |>
      rgbif::occ_download_import()
  })

  #-- Process GBIF data
  if(process_data){
    gbif = gbif |>
      # Filter for species & subspecies and not fossil records
      dplyr::filter(taxonRank %in% c("SPECIES", "SUBSPECIES", "VARIETY") &
                      occurrenceStatus == "PRESENT" &
                      !basisOfRecord == "FOSSIL_SPECIMEN") |>
      # Create clean scientific names
      dplyr::mutate_if(is.character, trimws) |> 
      dplyr::mutate(
        taxon_id = acceptedTaxonKey, 
        infraspecificEpithet = ifelse(grepl("^//s*$", infraspecificEpithet),
                                      NA, infraspecificEpithet),
        scientific_name = paste(trimws(genus), trimws(specificEpithet),
                                sep = " "),
        scientific_name = ifelse(!is.na(infraspecificEpithet),
                                 paste(scientific_name,
                                       trimws(infraspecificEpithet), sep = " "),
                                 scientific_name),
        scientific_name = trimws(scientific_name),
        # Parse date formats, day of year, and year
        parsed_date = lubridate::parse_date_time(eventDate, date_formats) |> 
          as.Date(),
        # parsed_date = ifelse(lubridate::year(parsed_date) == 9999, NA, date),
        day_of_year = lubridate::yday(parsed_date),
        parsed_year = lubridate::year(parsed_date),
        source = "GBIF"
      )
  }
  
  # Correct Taxon IDs
  if(correct) gbif = psoSppEvals::correct_taxon_ids(gbif) 

  #-- Return spatial GBIF data
  return(gbif_spatial(gbif))
}


#' Get GBIF occurrence records for a list of species
#' 
#' This function queries GBIF occurrence records for a species list using the 
#'     taxon ID from `get_taxonomies()` using [rgbif::occ_search()].
#'
#' @param spp_list A data frame of species with taxon ID's from 
#'     `get_taxonomies()`.
#' @param spatial Logical (TRUE/FALSE). Return spatial data. Default is TRUE.
#' @param crs Target coordinate reference system (CRS). Either and 
#'    `sf::st_crs()` object or accepted string (e.g. "EPSG:4326" or "NAD83"). 
#'    Default is EPSG:4326 (WGS84).
#' @param process_data Logical. Process data after reading them into R (TRUE ==
#'     yes, FALSE == no). Default is TRUE. The processing step
#' @param correct Logical. Run `correct_taxon_ids()` on data. Default is TRUE.
#'
#' @return An [sf] object or a [tibble::tibble()].
#' @seealso [get_taxonomies()], [rgbif::occ_search()], [correct_taxon_ids()]
#' @export
#'
#' @examples
#' \dontrun{
#' library(psoSppEvals)
#' 
#' # Species list with taxon ID's
#' spp <- get_taxonomies(psoSppEvals::sp_list_ex, correct = TRUE)
#' 
#' # Pull occurrence data
#' occ <- get_gbif_occ_data(spp)
#' }
get_gbif_occ_data <- function(spp_list, crs = "EPSG:4326", spatial = TRUE, 
                              process_data = TRUE, correct = TRUE){
  # Query GBIF data
  occ_ls = rgbif::occ_search(taxonKey = spp_list$taxon_id)
  # Convert list to data frame
  occ = lapply(1:length(occ_ls), function(x){
    dat = occ_ls[[x]]$data |> dplyr::mutate(taxon_id = names(occ_ls[x]))
    return(dat)
  }) |> 
    dplyr::bind_rows()
  
  # Convert to spatial
  if(spatial){
    occ = occ |> 
      dplyr::filter(!is.na(decimalLatitude) | !is.na(decimalLongitude)) |> 
      gbif_spatial(crs = crs)
  }
  
  #-- Process GBIF data
  if(process_data){
      # Date formats
  date_formats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d", "%Y-%m", "%Y", "ymd HMS",
                   "ymd", "ymd HM")
  occ = occ |>
      # Filter for species & subspecies and not fossil records
      dplyr::filter(taxonRank %in% c("SPECIES", "SUBSPECIES", "VARIETY") &
                      occurrenceStatus == "PRESENT" &
                      !basisOfRecord == "FOSSIL_SPECIMEN") |>
      dplyr::mutate_if(is.character, trimws) |>
      # Create clean scientific names
      dplyr::mutate(
        taxon_id = acceptedTaxonKey, 
        infraspecificEpithet = ifelse(grepl("^//s*$", infraspecificEpithet),
                                      NA, infraspecificEpithet),
        scientific_name = paste(trimws(genus), trimws(specificEpithet),
                                sep = " "),
        scientific_name = ifelse(!is.na(infraspecificEpithet),
                                 paste(scientific_name,
                                       trimws(infraspecificEpithet), sep = " "),
                                 scientific_name),
        scientific_name = trimws(scientific_name),
        # Parse date formats, day of year, and year
        parsed_date = lubridate::parse_date_time(eventDate, date_formats) |> 
          as.Date(),
        # parsed_date = ifelse(lubridate::year(parsed_date) == 9999, NA, date),
        day_of_year = lubridate::yday(parsed_date),
        parsed_year = lubridate::year(parsed_date),
        source = "GBIF"
      )
  }
  
  # Correct Taxon IDs
  if(correct) occ = psoSppEvals::correct_taxon_ids(occ) 
  
  return(occ)
}


#' Summarize GBIF data by species
#'
#' This function summarizes the spatial GBIF object from `get_gbif_data()` by
#'     species. Currently this function only works when
#'     `get_gbif_data(..., process_data = TRUE)`. The summary includes the 
#'     number of records per species, minimum and maximum year a species is 
#'     observed, and the GBIF occurrence ID if there are less than seven (7) 
#'     observations. This function then verifies taxonomy using the 
#'     `get_taxonomies()` function.
#'
#' @param gbif_data Spatial GBIF data from `get_gbif_data()`.
#' @param locale Logical. Location description of data (e.g., unit acronym or 
#'     "Buffer").
#' @param correct Logical. Run `correct_taxon_ids()` on data. Default is FALSE
#'
#' @return A tibble.
#' @seealso [get_gbif_data()], [get_taxonomies()], [correct_taxon_ids()]
#' @export
#'
#' @examples
#' \dontrun{
#' library("psoSppEvals")
#'
#' # Project directory path
#' t_path <- file.path("T:/path/to/project/directory")
#'
#' # Pull data from existing GBIF query
#' gbif_dat <- get_gbif_data(gbif_key = '9999999-999999999999999',
#'                           t_path = file.path(t_path, "data"))
#'
#' # Summarize species
#' gbif_list <- build_gbif_spp(gbif_dat)
#' }
build_gbif_spp <- function(gbif_data, locale = TRUE, correct = FALSE){
  # gbif_data = targets::tar_read(gbif_unit)
  
  # Date formats
  taxa_select = c("taxon_id", "scientific_name", "kingdom", "phylum", "class", 
                  "order", "family", "genus", "species", "subspecies", 
                  "variety", "form")
  # Locale
  if(isTRUE(locale)){
    locale = stringr::str_c(unique(gbif_data$locale), collapse = ", ")
  }
  
  # Subset species names
  t_ids = sf::st_drop_geometry(gbif_data) |>
    dplyr::select(taxon_id, scientific_name) |>
    dplyr::filter(!taxon_id == "" | !is.na(taxon_id)) |>
    dplyr::distinct() |>
    dplyr::mutate(dup_taxon = ifelse(duplicated(taxon_id) |
                                       duplicated(taxon_id, fromLast = TRUE),
                                     "Yes", "No"))
  
  # Summarize data
  t_dates = sf::st_drop_geometry(gbif_data) |>
    dplyr::select(taxon_id, scientific_name, parsed_year)  |>
    dplyr::filter(!is.na(taxon_id) | !(is.na(parsed_year))) |> 
    dplyr::group_by(taxon_id, scientific_name) |>
    dplyr::summarize(minYear = min(parsed_year, na.rm = TRUE),
                     maxYear = max(parsed_year, na.rm = TRUE),
                     .groups = "drop")
  
  t_id_sum = sf::st_drop_geometry(gbif_data) |>
    dplyr::select(taxon_id, scientific_name, occurrenceID)  |>
    dplyr::distinct() |>
    dplyr::group_by(taxon_id, scientific_name) |>
    dplyr::summarize(
      nObs = dplyr::n(),
      occID = ifelse(nObs <= 6, 
                     stringr::str_c(unique(occurrenceID), collapse = "; "),
                     NA),
      .groups = "drop"
      ) |>
    dplyr::left_join(t_dates, by = c("taxon_id", "scientific_name")) |> 
    dplyr::select(taxon_id, scientific_name, nObs, minYear, maxYear, occID) |> 
    dplyr::mutate(locale = locale, source = "GBIF") |>
    dplyr::filter(!is.na(taxon_id))

  
  # Subset taxonomy
  taxa = sf::st_drop_geometry(gbif_data) |>
    dplyr::select(dplyr::any_of(taxa_select)) |>
    dplyr::distinct()
  
  dat = dplyr::left_join(t_ids, t_id_sum, 
                         by = c("taxon_id", "scientific_name"),
                         relationship = 'many-to-many')|>
    dplyr::left_join(taxa, by = c("taxon_id", "scientific_name"), 
                     relationship = 'many-to-many') |>
    dplyr::arrange(kingdom, phylum, class, order, family, genus,
                   species, scientific_name)
  
  # Correct Taxon IDs
  if(correct) dat = psoSppEvals::correct_taxon_ids(dat) 

  return(dat)
}


#' Internal Function: Create a well-known text string (WTK) string
#'
#' Creates a well-known text string from a polygon (`sf` object). This function
#'     transforms the input polygon to WGS84 prior to calculating the wkt 
#'     string.
#'
#' @param my_polygon An `sf` polygon object.
#'
#' @return An `sf` vector object.
wkt_string <- function(my_polygon){
  fc = sf::st_transform(my_polygon, crs ="EPSG:4326" )
  wkt = sf::st_bbox(fc) |>
    sf::st_as_sfc() |>
    sf::st_as_text()
  return(wkt)
}


#' Internal Function: Convert GBIF data frame to an sf object
#' 
#' This internal function converts the output from `get_gbif_data()` to a 
#'     spatial (`sf`) object using `sf::st_as_sf()`. This function will also 
#'     transform the data to a target coordinate reference system.
#'
#' @param gbif_dat GBIF data frame from `get_gbif_data()`.
#' @param crs Target coordinate reference system (CRS). Either and 
#'    `sf::st_crs()` object or accepted string (e.g. "WGS84" or "NAD83"). 
#'    See `sf::st_crs()` for more details. Default is NULL. If NULL, resulting 
#'    `sf` object CRS will be EPSG:4326 (WGS84).
#' 
#' @return An [sf] object.
gbif_spatial <- function(gbif_dat, crs = NULL){
  fc = sf::st_as_sf(gbif_dat, coords = c("decimalLongitude", "decimalLatitude"),
                    crs = "EPSG:4326")
  if(!is.null(crs)){
    if(sf::st_crs(fc) != crs) fc = sf::st_transform(fc, crs = crs)
  }
  return(fc)
} 

