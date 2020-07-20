<a href="https://zenodo.org/badge/latestdoi/266897409"><img src="https://zenodo.org/badge/266897409.svg" alt="DOI"></a>

## phylosamp

This repository provides code for the `phylosamp` R package, which was designed to help users determine the the ability of some phylogenetic criteria (e.g., genetic distance) to correctly identify pairs of pathogen infections linked by transmission, given a particular sample size or proportion. The package also includes functions that do the reverse: calculate the sample size needed to correctly identify true pairs at some particular rate.

The functions used to calculate the sample size or false discovery rate of the criteria require as input an estimate of the sensitivity and specificity of the linkage criteria used. Therefore, the current implementation of the package also includes functions to estimate the sensitivity and specificity of genetic distance as a linkage criteria, from the mutation rate of the pathogen. Future implementations may contain guidance on how to estimate these parameters for other phylogenetic criteria. 

All functions require the user to specify the underlying assumptions about transmissions and linkage, i.e., if an infected individual can transmit to more than one susceptible individual (single transmission/multiple transmissions), and if the criteria being used is capable of linking a case to more than one other case (single linkage/multiple linkage). Permitting multiple transmissions and multiple links ('mtml') is the default.

The key functions of this package are documented, along with realistic examples, in the associated vignettes.

### Methodology

A detailed description of the methods can be found in:

[Sample Size Calculation for Phylogenetic Case Linkage (Wohl, Giles, and Lessler 2020)](https://www.medrxiv.org/content/10.1101/2020.07.10.20150920v1)

This package is maintained by John Giles ([@gilesjohnr](https://github.com/gilesjohnr)), Shirlee Wohl ([@shwohl](https://github.com/shwohl)) and Justin Lessler ([@jlessler](https://github.com/jlessler)) as part of the Johns Hopkins Bloomberg School of Public Health Infectious Disease Dynamics team ([@HopkinsIDD](https://github.com/HopkinsIDD)).

### Installation

To install the install the development version of the `phylosamp` package, first install the `devtools` package and then install `phylosamp` from source via GitHub:
```r
install.packages('devtools')
devtools::install_github('HopkinsIDD/phylosamp')
```

### Troubleshooting

For general questions, contact package maintainers John Giles (giles@jhu.edu), Shirlee Wohl (swohl@jhu.edu), or Justin Lessler (justin@jhu.edu).

To report bugs or problems with documentation, please go to the [Issues](https://github.com/HopkinsIDD/phylosamp/issues) page associated with this GitHub page and click *new issue*.
