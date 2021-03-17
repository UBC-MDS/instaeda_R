
<!-- README.md is generated from README.Rmd. Please edit that file -->

# instaeda

<!-- badges: start -->

[![R-CMD-check](https://github.com/UBC-MDS/instaeda_R/workflows/R-CMD-check/badge.svg)](https://github.com/UBC-MDS/instaeda_R/actions)
<!-- badges: end -->

Quick and easy way to clean data and build exploratory data analysis
plots.

This idea came up as we have been building data projects for quite some
time now in the UBC MDS program. We noticed that there are some
repetitive activities that occur when we first explore the data. This
project will help you take a given raw data set an conduct some data
cleansing and plotting with a minimal amount of code.

The main components of this package are:

  - **Data Checking**
      - Plot basic information for input data: Take the input data and
        declare the title of the plot and a list of configurations to be
        passed to themes to invisibly return the ggplot object with
        summary metrics including the basic description of the input
        data such as the distribution of the numeric columns, factor
        columns, complete rows and missing observations.
  - **Data Cleansing**
      - Custom Imputation of missing values in a data frame using
        additional techniques, i.e quantiles and randomization by
        dividing data set into several parts and returns combined
        imputed data frame.
  - **Exploratory Visualization**
      - Numerical Correlation Plot: takes in a data frame, selects the
        numerical columns and outputs a correlation plot object. User
        can optionally pass in subset of columns to define which columns
        to compare.
      - Plot Basic Distribution Plot by datatype: Pass in data frame and
        based on parameters, will return histograms, bar charts, or
        other chart types depending on the column’s datatype.

There are a myriad of packages that provide similar functionality in the
R ecosystem. A few of the packages include:

  - [dlookr](https://cran.r-project.org/web/packages/dlookr/)
  - [SmartEDA](https://cran.r-project.org/web/packages/SmartEDA/)

## Installation

You can install the released version of instaeda from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("instaeda")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UBC-MDS/instaeda_R")
```

## Example

This is a basic example of usage for functions found inside the instaeda
package:

``` r
#library(instaeda)

#plot_info(example_dataframe)
#plot_corr(example_dataframe)
#plot_basic_distributions(example_dataframe)
#divide_and_fill(example_dataframe)
```
