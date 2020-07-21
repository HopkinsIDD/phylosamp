## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_knit$set(root.dir="../")
knitr::opts_chunk$set(fig.width=7.5, fig.height=6, fig.path='../figures/', warning=FALSE, message=FALSE,cache=FALSE)

library(phylosamp)

## ----samplesize---------------------------------------------------------------

samplesize(eta=0.99, chi=0.995, N=100, R=1, phi=0.75)


## ----samplesize_explinks------------------------------------------------------

samplesize(eta=0.99, chi=0.995, N=100, R=1, phi=0.75, min_pairs=30)


## ----samplesize_fail, error=TRUE----------------------------------------------

samplesize(eta=0.99, chi=0.995, N=100, R=1, phi=0.95)


## ----truediscoveryrate_explain------------------------------------------------

truediscoveryrate(eta=0.99, chi=0.995, rho=1, M=100, R=1)


