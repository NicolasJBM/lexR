
<!-- badges: start -->

[![R-CMD-check](https://github.com/NicolasJBM/lexR/workflows/R-CMD-check/badge.svg)](https://github.com/NicolasJBM/lexR/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![License:
GPL3](https://img.shields.io/badge/License-GPL3.0-yellow.svg)](https://opensource.org/licenses/GPL-3.0)
<!-- badges: end -->

# lexR <img src="man/figures/logo.svg" align="right" width="120" />

Toolbox to manipulate and analyze texts.

## Overview

The *lexR* package gathers several functions to prepare texts for
subsequent analyses (cleaning, semantic analysis, bags of words) and to
facilitate topic modeling.

## Installation

Before you can install *lexR* itself, you will need to install from CRAN
the following packages:

``` r
install.packages(c("dplyr", "tidyr", "stringr", "purrr", "stm", "tibble", "tidytext", "stats", "textclean", "stringi", "tm", "utils", "clipr", "koRpus.lang.en", "koRpus", "tidygraph", "rhandsontable", "udpipe", "shiny", "miniUI", "knitr"), dependencies = TRUE)
```

Then, install *lexR* from its GitHub public repository:

``` r
devtools::install.github("NicolasJBM/lexR")
```

## Usage

A first set of functions support the preparation of text for subsequent
analyses. See the vignette “Preparing Textual Data for analysis” for
more detail about these functions and the related work flow.

A second set of functions leverages several functions from other
packages for text analysis itself. For more details about this set, see
the vignette “Analyzing Textual Data”.

Finally, the packages also includes a function to count words in a
string or in the clipboard, *count\_words()*, and a small gadget to test
what a regex pattern captures, *test\_regex()*.

## Toolboxes

*LexR* is necessary to run
*[bibliogR](https://github.com/NicolasJBM/bibliogR)* which uses the
cleaning functions. The package
*[buildR](https://github.com/NicolasJBM/buildR)* inclides some function
helping with the analysis of synctatic network produced by the
*create\_syntrel()* and *create\_syntnet()* functions.
