
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eimsr

<!-- badges: start -->
<!-- badges: end -->

The goal of eimsr is to provide functions and workflows to make use of
ship’s underway data, EIMS data, and Oxygen optode data.

## Installation

You can install the development version of eimsr from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("blongworth/eimsr")
```

## Setup

An example script `get_AR66_data.R` shows how to pull in data for use
with the package. Generate a new script to complile data using this as
an example. Running this script should pull in all underway, eims, temp
server, and optode files, process them and combine them, lining up
timestamps.

## Plotting, etc.

The `underway_mapper.Rmd` markdown files give examples of plotting of
combined data.
