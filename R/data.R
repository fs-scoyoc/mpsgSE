#' 2024 US Fish & Wildlife Service Birds of Conservation Concern List
#'
#' @format A data frame of 333 observations and 17 variables.
#' \describe{
#'   \item{common_name}{Common name of bird species.}
#'   \item{scientific_name}{Scientific name of bird species.}
#'   \item{mbta}{Migratory Bird Treaty Act designation.}
#'   \item{federally_endangered}{Location description of where the species is Endangered.}
#'   \item{federally_threatened}{Location description of where the species is Threatened.}
#'   \item{bcc_rangewide_in_continental_us_and_or_pr_and_vi_or_hi_and_pacific_islands}{description}
#'   \item{bc_rs_for_bcc_listing_non_breeding}{List of Bird Conservation Region codes for non-breeding birds.}
#'   \item{bc_rs_for_bcc_listing_breeding}{List of Bird Conservation Region codes for breeding birds.}
#'   \item{gbif_taxonID}{GBIF taxon ID}
#'   \item{kingdom}{Taxonomic Kingdom}
#'   \item{phylum}{Taxonomic Phylum}
#'   \item{class}{Taxonomic Class}
#'   \item{order}{Taxonomic Order}
#'   \item{family}{Taxonomic Family}
#'   \item{genus}{Taxonomic Genus}
#'   \item{species}{Taxonomic Species}
#'   \item{taxon_id}{PSO taxon ID.}
#' }
#' @source https://www.fws.gov/media/usfws-bird-species-concern
"bcc_list"


#' North American Bird Conservation Initiative Bird Conservation Regions
#'
#' @format An `sf` object (polygon) of 4414 observations and 9 variables.
#' \describe{
#'   \item{bcr_label}{Birds Conservation Region codes.}
#'   \item{bcr_label_name}{Birds Conservation Regions names.}
#'   \item{name_en}{Birds Conservation Regions names in English.}
#'   \item{name_fr}{Birds Conservation Regions names in French.}
#'   \item{name_sp}{Birds Conservation Regions names in Spanish}
#'   \item{globalid}{Polygon ID.}
#'   \item{SHAPE_Length}{Polygon length.}
#'   \item{SHAPE_Area}{Polygon area.}
#'   \item{SHAPE}{Polygon geometry.}
#' }
#' @source https://www.birdscanada.org/bird-science/nabci-bird-conservation-regions
"bcc_regions"


#' List of Forest Service Units
#'
#' @format A data frame of 114 observations and 6 variables.
#' \describe{
#'   \item{adminforestid}{Administrative forest ID.}
#'   \item{region}{Forest Service Region code.}
#'   \item{forestnumber}{Forest number.}
#'   \item{forestorgcode}{Forest org code.}
#'   \item{forestname}{Forest name.}
#'   \item{gis_acres}{Acres calsulated by ESRI ArcGIS.}
#' }
#' @source Forest Service EDW Rest Services, https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_ForestSystemBoundaries_01/MapServer
"fs_units"


#' List of National Forests and Grasslands IMBCR surveys on.
#'
#' @format A vector of management units from the IMBCR data.
"imbcr_mgmt_units"


#' Scientific names with manually corrected taxon ID's
#' 
#' Manually generated through trial and error
#' @format A data frame of 5 observations and 3 variables
#' \describe{
#'   \item{common_name}{Common species name.}
#'   \item{scientific_name}{Scientific name of species.}
#'   \item{taxon_id}{Taxon ID.}
#' }
"manual_corrections"


#' Taxon ID and Scientific Name corrections
#' 
#' Manually generated through trial and error
#' @format A data frame of 27 observations and 4 variables.
#' \describe{
#'   \item{common_name}{Common species name.}
#'   \item{errored_name}{Name that is not being queried in GBIF.}
#'   \item{corrected_name}{A scientific name for the same speces in the GBIF backbome taxononmy.}
#'   \item{taxon_id}{Taxon ID.}
#' }
"name_corrections"


#' 2025 Regional Forester's Sensitive Species List for Forest Service Regions 1-10
#'
#' @format A data frame of 4142 observations and 11 variables.
#' \describe{
#'   \item{scientific_name}{Scientific name of species.}
#'   \item{common_name}{Common name of species.}
#'   \item{region}{Forest Service Region}
#'   \item{orig_scientific_name}{Origial, uncleaned and unverified scientific name of species.}
#'   \item{taxon_id}{PSO taxon ID.}
#'   \item{gbif_taxonID}{Taxon ID from GBIF.}
#'   \item{kingdom}{Taxonomic Kingdom}
#'   \item{phylum}{Taxonomic Phylum}
#'   \item{class}{Taxonomic Class}
#'   \item{order}{Taxonomic Order}
#'   \item{family}{Taxonomic Family}
#'   \item{genus}{Taxonomic Genus}
#'   \item{species}{Taxonomic Species}
#'   \item{subspecies}{Taxonomic Subspecies}
#'   \item{form}{Taxonomic Form}
#'   \item{variety}{Taxonomic Variety}
#' }
#' @source Compiled from 2025 Regional Forester's Sensitive Species Lists acquired 12 January 2026 from Nichole Panico.
"rfss"


#' An example dataset of common names and scientific names of species.
#'
#' @format A data frame of 427 observations and 2 variables.
#' \describe{
#'   \item{common_name}{Common name of species.}
#'   \item{scientific_name}{Scientific name of species.}
#' }
"sp_list_ex"
