# labVisualizeR (!!! under development !!!)

<!-- badges: start -->
[![pipeline status](https://gitlab.miracum.org/miracum/labor/labvisualizer/badges/master/pipeline.svg)](https://gitlab.miracum.org/miracum/labor/labvisualizer/commits/master)
[![coverage report](https://gitlab.miracum.org/miracum/labor/labvisualizer/badges/master/coverage.svg)](https://gitlab.miracum.org/miracum/labor/labvisualizer/commits/master)
<!-- badges: end -->


The package includes a shiny web application that aims to interactively visualize measurements from SQL databases. It has primarily been developed to visualize laboratory mesaurements, but can in principal be used to visualize any continuous and discrete values. 

Currently supported databases:  
* Oracle (via R package `RJDBC` --> please provide the path to your 'ojdbc7.jar' when starting the application) 
* PostgreSQL (via R package `Rpostgres`)

# Installation

You can install *labVisualizeR* with the following commands in R:

``` r
options('repos' = 'https://ftp.fau.de/cran/')
install.packages("devtools")
devtools::install_git("https://gitlab.miracum.org/miracum/labor/labvisualizer.git")
```

# Configuration 

The database connection can be configured using environment variables. These can be set using the base R command `Sys.setenv()`. A detailed description on which environment variables need to be set for the specific databases can be found [here](https://gitlab.miracum.org/miracum/misc/dizutils/-/blob/master/README.md).

# Start shiny app

To start `labVisualizeR`, just run the following command in R. A browser tab should open displaying the web application. Alternatively you can type the URL "localhost:3838/" in your browser.

To run the analyses, please click on the button *Load example data* on the application's dashboard.

```r
library(labVisualizeR)
launch_app()
```

A custom app title can be used by setting the environment variable `LABVIZ_TITLE` before calling `launch_app()`, e.g. 

```r
library(labVisualizeR)
Sys.setenv("LABVIZ_TITLE" = "Custom Title")
launch_app()
```


# More Infos:

- about MIRACUM: [https://www.miracum.org/](https://www.miracum.org/)
- about the Medical Informatics Initiative: [https://www.medizininformatik-initiative.de/index.php/de](https://www.medizininformatik-initiative.de/index.php/de)
- about Shiny: https://www.rstudio.com/products/shiny/
- RStudio and Shiny are trademarks of RStudio, Inc.

