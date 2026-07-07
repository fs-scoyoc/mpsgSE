# Planning Service Organization Species Evaluation (psoSppEvals) R Package

## Overview

Welcome to the Planning Service Organization (PSO; USDA Forest Service) Species Evaluation R package. This package is in development by PSO staff to standardize species data and produce template species evaluation documents as part of the [Species of Conservation Concern (SCC)](https://www.fs.usda.gov/naturalresources/programs/scc.shtml) process of land management plan revision. This package hosts several utilities that standardize species observation data from repositories like the Global Biodiversity Information Facility (GBIF; <https://www.gbif.org/>) and SEINet <https://swbiodiversity.org/seinet/index.php>), query species range data from eBird <https://science.ebird.org/en/status-and-trends>) and the Botanical Information and Ecological Network (<https://bien.nceas.ucsb.edu/bien/>), and produces template species evaluation documents in Microsoft Word format using a `Quarto` scrip.

**Notes:**

1.  Some functions access publicly available data sources like GBIF, eBird, and BIEN. Other functions access data on internal Forest Service servers and will not work unless you are on the Forest Service network with the servers mapped to your computer. We have tired to note which functions access Forest Service servers in the man pages. Please let us know if we missed anything by submitting an [Issue](https://github.com/fs-scoyoc/mpsgSE/issues).
2.  I am moving the spatial functions `clip_fc()`, `read_edw_lyr()`, and `read_fc()` to the `psoGIStools` package. The functions included in this package will no longer be maintained and will eventually be removed.

**Authors:** [Matt Van Scoyoc](https://github.com/fs-scoyoc) and [Michael Schmidt](https://github.com/fs-mschmidty)

**Maintainer:** [Matt Van Scoyoc](https://github.com/fs-scoyoc)

**Version:** 0.4.0

**License:** Mit + file [LICENSE](https://github.com/fs-scoyoc/mpsgSE/blob/main/LICENSE.md)

**Depends:** R (\>= 4.1.0)

**Imports:** arcgisbinding, arcgislayers, BIEN, devtools, dplyr, ebirdst, foreign, fs, glue, httr2, janitor, lubridate, methods, natserv, osmdata, purrr, psoGIStools, quarto, readr, readxl, remotes, rgbif, rlandfire, rnaturalearth, sf, stringr, taxize, terra, tibble, tidyr, tidyselect, units, writexl

**Issues:** This package is under active development and changes often with out warning. Functions may experience breaking changes at any time. If you find a bugs or have an idea for a feature, please submit an [Issue](https://github.com/fs-scoyoc/mpsgSE/issues).

**Documentation:** Just man pages for now.

## Installation

This package requires two packages not available on CRAN, `arcgisbinding` and `psoGIStools`, to install. The [`arcgisbinding`](https://github.com/R-ArcGIS/arcgisbinding) package is on hosted on ESRI and the [`psoGIStools`](https://github.com/fs-scoyoc/psoGIStools) package is on GitHub. The following code installs these two packages, then installs the `psoSppEvals` package.

``` r
# Requisite packages not on CRAN
install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")
remotes::install_github("fs-scoyoc/psoGIStools")

# Install psoSppEvals
remotes::install_github("fs-scoyoc/psoSppEvals")
```

## Template Species Evaluation Documents

[Here](https://github.com/fs-scoyoc/mpsgSE/blob/main/docs/Example_Olive-sided_Flycatcher_Contopus_cooperi.pdf) is an example of a template species evaluation document for Olive-sided Flycatcher on the Bitterroot National Forest. These documents are templates that are customized for each land management revision project (i.e., a National Forest or Grassland) and PSO biologists will add, revise, or remove information using their best professional judgment before this document becomes part of the project record.

## List of Functions

-   `build_basemap_data`: Build base map data for automated reports.

-   `build_gbif_spatial_data`: Subset eligible species from spatial GBIF data.

-   `build_gbif_spp`: Summarize GBIF data by species.

-   `build_imbcr_spatial_data`: Subset eligible species from spatial IMBCR data.

-   `build_imbcr_spp`: Summarize IMBCR data by species.

-   `build_iucn_maps`: **Deprecated**. See. `get_iucn_maps()`. Read spatial range data from International Union for Conservation of Nature (IUCN) into R. This function requires access to internal Forest Service servers.

-   `build_map_source`: Build a data frame of map source information for Quarto script.

-   `build_ns_spp_list`: Build a species list with global and state NatureServe conservation rankings from `get_ns_state_list().`

-   `build_quarto_params`: Build quarto parameters for automated evaluation templates.

-   `build_seinet_spatial_data`: Subset eligible species from spatial SEINet data and reduce variables.

-   `build_seinet_spp`: Summarize SEINet data by species.

-   `clip_fc`: Deprecated. Use the `psoGIStools` package moving forward. Clip an `sf` object to polygon.

-   `combine_natureserve_data`: Combine NatureServe data from `get_ns_state_list()`.

-   `combine_ns_habs`: Combine two NatureServe habitat data frames from `get_ns_habitat()`.

-   `combine_ns_spp_lists`: Combine two NatureServe species lists from `build_ns_spp_list()`.

-   `combine_ns_state_lists`: Combine two NatureServe State lists from `get_ns_state_list()`.

-   `correct_taxon_ids`: Correct known issues with taxon ID's.

-   `count_spp_by_hab`: Count species by NatureServe habitat type.

-   `download_bien_maps`: Download BIEN range map data and read them into R.

-   `download_ebird_range_maps`: Download eBird range map data and read them into R.

-   `download_ebird_status_maps`: Download eBird status map data and read them into R.

-   `download_ebird_trends_maps`: Download eBird trend data and read them into R.

-   `extract_landfire_evt`: Extract LANDFIRE EVT data to points.

-   `get_basemap_data`: Build base map data for automated reports.

-   `get_bc_regions`: Clip Birds of Conservation Regions to a polygon.

-   `get_bcr_list`: Get USGS Birds of Conservation Concern breeding bird list for National Forest or Grassland.

-   `get_ebird_regional_stats`: Download eBird regional statistics and read them into R.

-   `get_ebird_trends`: Calculate abundance trends from eBird data.

-   `get_gbif_data`: Get GBIF occurrence records for an area of analysis (polygon).

-   `get_gbif_occ_data`: Get GBIF occurrence records for a list of species.

-   `get_imbcr_data`: Read IMBCR occurrence data into R. This function requires access to internal Forest Service servers.

-   `get_imbcr_trends`: Read IMBCR trend data into R. This function requires access to internal Forest Service servers.

-   `get_iucn_maps`: Read IUCN range data into R.

-   `get_iucn_shp_paths`: **Deprecated**. See. `get_iucn_maps()`. Retrieve file paths to IUCN shapefiles. This function requires access to internal Forest Service servers.

-   `get_ns_habitat`: Read NatureServe habitat types for a given species into R.

-   `get_ns_state_list`: Import NatureServe species data for a given state into R.

-   `get_rfss_list`: Read a Forest Service Regional Forester's Sensitive Species List into R.

-   `get_seinet_data`: Read SEINet data in Darwin Core Archive format into R.

-   `get_synonyms`: Query taxonomic synonyms from the GBIF backbone taxonomies database.

-   `get_taxonomies`: Query taxon ID and taxonomic information from the GBIF backbone taxonomies database.

-   `list_regions`: List Forest Service regions used in `get_rfss_list()`.

-   `ns_ranks`: **Deprecated**. Generate a species list with NatureServe global and state conservation rankings.

-   `pull_edw_bio_data`: Pull species data from the Forest Service Enterprise Data Warehouse (EDW).

-   `pull_landfire`: Pull LANDFIRE data for an area of interest using the `rlandfire` package.

-   `read_edw_lyr`: Deprecated. Use the `psoGIStools` package moving forward. Read spatial data from Forest Service ArcGIS REST Services into R.

-   `read_fc`: Deprecated. Use the `psoGIStools` package moving forward. Read feature class from a geodatabase into R.

-   `set_up_directories`: Create directories for automated evaluations.

-   `set_up_pipeline`: Set up targets pipeline.

-   `wkt_string`: Create a well-known text string from an `sf` polygon object for `get_gbif()`.

-   `write_evals`: Produce automated evaluation template documents.

-   `write_habitat_xlsx`: Write an Excel workbook of NatureServe and LANDFIRE habitat types.

-   `write_range_data`: Write ebird, BIEN, and IUCN range data to a geodatabase.

## List of Data Sets

The data sets listed below are used in the PSO species evaluation process and not intended for distribution. Recommended best practice is to obtain these data from their sources.

-   `bcc_list`: 2024 US Fish & Wildlife Birds of Conservation Concern List.

-   `bcc_regions`: North American Bird Conservation Initiative Birds Conservation Regions.

-   `fs_units`: National Forests and Grasslands.

-   `imbcr_mgmt_units`: List of National Forests and Grasslands IMBCR conducts surveys on.

-   `manual_corrections`: List of species that do not return taxon ID's. Corrected taxon ID's for the same species have been manually queried in GBIF.

-   `name_corrections`: This data frame has common names, scientific names that do not return taxon ID's, and corrected names for the same species that will return taxon ID's.

-   `rfss`: 2025 Forest Service Regional Forester's Sensitive Species List for Regions 1-10.

-   `sp_list_ex`: An example data set of common names and scientific names of species.
