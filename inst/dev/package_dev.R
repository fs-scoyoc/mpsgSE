install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")
remotes::install_github("fs-scoyoc/psoGIStools")
remotes::install_github("fs-scoyoc/psoSppEvals")
remotes::install_github("fs-scoyoc/mpsgSEdata")


# library("devtools")
# library("usethis")
# packageVersion("devtools")

# create_package("~/path/to/package")
# use_r("get_gbif") # creates and/or opens a script
# use_mit_license() # set license

# Workflow ----
devtools::test()

devtools::document() # Update package documentation

devtools::check() # Check for package errors

devtools::load_all() # load package in development mode
devtools::install() # manually test

#-- Add, commit, and push package to GITHub in the terminal
# git pull
# git add .
# git commit -m "message"
# git push
#-- Switch to main branch and merge master to main.
# git checkout main
# git merge origin/master


library('psoSppEvals')
ls(package:psoSppEvals) |> sort()

?read_edw_lyr
?get_taxonomies
?build_basemap_data
?get_basemap_data

get_taxonomies(sp_list_ex[1:10, ])


