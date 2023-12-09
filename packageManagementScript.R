# package management script
# WARNING: this script is a log of steps taken to debug and make the pkg. do not run this file each time!


# install R packages for packaging
# install.packages(c("devtools", "usethis", "roxygen2"))
library(usethis)
library(devtools)
library(roxygen2)
library(desc)

# install.packages(c("momentchi2", "fields", "viridis", "signal", "fossil"))
# install.packages("stats")
# install.packages("compositions")
library(momentchi2)
library(fields)
library(viridis)
library(signal)
library(fossil)
library(stats)
library(compositions)
# install.packages(c("ggplot2"))
library(ggplot2)

# ignored files
use_build_ignore(c("packageManagementScript.R"))
use_build_ignore(c("mEBA_function.R"))

# add author and manager

# add Dr. Bruce
usethis::use_author(
  given = "Scott",
  family = "Bruce",
  role = c("aut"),
  email = "sabruce@tamu.edu"
)

# add Matt Byrom
use_author(
  given = "Matt",
  family = "Byrom",
  role = c("cre"),
  email = "matthew.james.byrom@tamu.edu"
)

# add Dylan Ward
use_author(
  given = "Dylan",
  family = "Ward",
)

# add Mohit Chhaparia
use_author(
  given = "Mohit",
  family = "Chhaparia",
)


# other desc file changes with usethis



# add GPL 3 liscense
usethis::use_gpl3_license()
# usethis::use_mit_license()

# create cpp infrastructure

# package dependencies for EBA
usethis::use_package("momentchi2", type = "Imports")
usethis::use_package("fields", type = "Imports")
usethis::use_package("viridis", type = "Imports")
usethis::use_package("signal", type = "Imports")
usethis::use_package("fossil", type = "Imports")
usethis::use_package("compositions", type = "Imports")
usethis::use_package("stats", type = "Imports")
devtools::install_github("mattByrom-tamu/EFBA")

vignette("rd", package = "roxygen2")


# testing EBA functions
devtools::load_all()
exists("eba.search", where = globalenv(), inherits = FALSE) # returned false so looks like it's working
devtools::check() # no errors currently

# add C++, rcpp, and armadillo to code base
usethis::use_rcpp()

# update documentation, have to delete namespace first sometimes
devtools::document()

# rmd readme document
usethis::use_readme_rmd()
devtools::build_readme()

load_all()

# build EBA vignette
usethis::use_vignette("EBA")

# test classes for EBA
usethis::use_testthat()
usethis::use_test("eba.search.R")
