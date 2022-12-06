<a href="https://zenodo.org/badge/latestdoi/266897409"><img src="https://zenodo.org/badge/266897409.svg" alt="DOI"></a>

# phylosamp

This repository provides code for the `phylosamp` R package, which was designed to help users conduct and evaluate sample size calculations for phylogenetic studies. Presently, the functions can be used to calculate sample size in two types of scenarios that frequently arise when analyzing pathogen genomic data: (1) trying to determine if pathogen infections are linked by transmission ([linkage scenario](#Determining-linkage-between-pathogen-infections)); (2) trying to estimate the frequency of a known pathogen lineage or variant of concern ([VOC scenario](#Determining-the-frequency-of-a-pathogen-VOC)).

All key functions of each scenario are documented, along with realistic examples, in the [associated vignettes](https://hopkinsidd.github.io/phylosamp/index.html). Vignettes are organized as follows:

* Vignettes L1-L4: linkage scenario vignettes and examples
* Vignettes V1-V6: VOC scenario vignettes and examples

## Determining linkage between pathogen infections

The package includes a suite of functions that can be used to determine the the ability of some phylogenetic criteria (e.g., genetic distance) to correctly identify pairs of pathogen infections linked by transmission, given a particular sample size or proportion. The package also includes functions that do the reverse: calculate the sample size needed to correctly identify true pairs at some particular rate.

The functions used to calculate the sample size or false discovery rate of the criteria require as input an estimate of the sensitivity and specificity of the linkage criteria used. Therefore, the current implementation of the package also includes functions to estimate the sensitivity and specificity of genetic distance as a linkage criteria, from the mutation rate of the pathogen. Future implementations may contain guidance on how to estimate these parameters for other phylogenetic criteria. 

All functions require the user to specify the underlying assumptions about transmissions and linkage, i.e., if an infected individual can transmit to more than one susceptible individual (single transmission/multiple transmissions), and if the criteria being used is capable of linking a case to more than one other case (single linkage/multiple linkage). Permitting multiple transmissions and multiple links ('mtml') is the default.

A detailed description of the linkage methods can be found in:

[Sample Size Calculation for Phylogenetic Case Linkage (Wohl, Giles, and Lessler 2020)](https://doi.org/10.1371/journal.pcbi.1009182)

## Determining the frequency of a pathogen VOC

The package includes another set of functions that can be used to determine the sample size needed to detect or estimate the frequency of a VOC in a population. It also includes functions that do the reverse: calculate the confidence in a detection or frequency estimate, given a number of sequences.

These functions require the user to specify a desired confidence in the results (either probability of detection or confidence in prevalence estimate), a desired or estimated variant prevalence, and (if applicable) a desired precision in the prevlance estimate. The user can also provide variant-specific parameters to help account for baises in VOC detection, such as the probability that an infection is asymptomatic, the testing sensitivity, the testing probability, and the sequencing success rate.

Functions are provided for sample size calculations in a cross-sectional scenario, where a single batch of samples will be collected and sequenced, and in a regular surveillance scenario, where samples are collected and sequenced repeatedly at some regular interval. In the later case, the user must also provide information on how the VOC frequency may be changing over time (in the form of an initial frequency and estimated growth rate).

All calculations assume a two-variant system; in other words, that there is a particular variant of interest that may behave differently from the rest of the pathogen population (in terms of asymptomatic rate, testing sensitivity, etc.). Detection biases due to these differences are incorporated into function calculations. That said, the framework could be easily extended to a multi-VOC system in the future.

A detailed description of the VOC estimation methods can be found in:

[Sample Size Calculations for Variant Surveillance in the Presence of Biological and Systematic Biases (Wohl, Lee, Diprete, and Lessler 2022)](https://doi.org/10.1101/2021.12.30.21268453)


## Installation

The [`phylosamp` package](https://cran.r-project.org/package=phylosamp) is available for download on CRAN.

To install the install the development version of the `phylosamp` package, first install the `devtools` package and then install `phylosamp` from source via GitHub:
```r
install.packages('devtools')
devtools::install_github('HopkinsIDD/phylosamp')
```

## Troubleshooting

This package is maintained by Elizabeth Lee ([@eclee25](https://github.com/eclee25)), Shirlee Wohl ([@shwohl](https://github.com/shwohl)), and Justin Lessler ([@jlessler](https://github.com/jlessler)).

For general questions, contact Shirlee Wohl (swohl@scripps.edu), or Justin Lessler (jlessler@unc.edu).

To report bugs or problems with documentation, please go to the [Issues](https://github.com/HopkinsIDD/phylosamp/issues) page associated with this GitHub page and click *new issue*.
