
<!-- README.md is generated from README.Rmd. Please edit that file -->

# instaeda

<!-- badges: start -->

[![R-CMD-check](https://github.com/UBC-MDS/instaeda_R/workflows/R-CMD-check/badge.svg)](https://github.com/UBC-MDS/instaeda_R/actions)
[![codecov](https://codecov.io/gh/UBC-MDS/instaeda_R/branch/main/graph/badge.svg?token=NFE0J10DH6)](https://codecov.io/gh/UBC-MDS/instaeda_R)

<!-- badges: end -->

Quick and easy way to clean data and build exploratory data analysis
plots.

This idea came up as we have been building data projects for quite some
time now in the UBC MDS program. We noticed that there are some
repetitive activities that occur when we first explore the data. This
project will help you take a given raw data set an conduct some data
cleansing and plotting with a minimal amount of code.

The main components of this package are:

-   **Data Checking**

    -   Plot basic summary information for a given data frame. An output
        ggplot will be returned with summary metrics including the basic
        description of the input data such as the distribution of the
        numeric columns, factor columns, complete rows and missing
        observations.

-   **Data Cleansing**

    -   Update missing values in a data frame. This function uses custom
        Imputation of missing values using mean, median and random. In
        addition, you can shuffle the data using a parameter.

-   **Exploratory Visualization**

    -   Generate a numerical correlation plot using your preferred
        correlation method.
    -   Generate basic distribution plots by column type.

There are a myriad of packages that provide similar functionality in the
R ecosystem. A few of the packages include:

-   [dlookr](https://CRAN.R-project.org/package=dlookr)
-   [SmartEDA](https://CRAN.R-project.org/package=SmartEDA)

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UBC-MDS/instaeda_R")
```

## Example

This is a basic example of usage for functions found inside the instaeda
package.

``` r
#library(instaeda)

#plot_info(example_dataframe)
#plot_corr(example_dataframe)
#plot_basic_distributions(example_dataframe)
#divide_and_fill(example_dataframe)
```
