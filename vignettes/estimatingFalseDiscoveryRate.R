## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_knit$set(root.dir="../")
knitr::opts_chunk$set(fig.width=7.5, fig.height=6, fig.path='../figures/', warning=FALSE, message=FALSE,cache=FALSE)

library(phylosamp)

## ----truediscoveryrate--------------------------------------------------------

truediscoveryrate(eta=0.99, chi=0.95, rho=0.75, M=100, R=1)


## ----truediscoveryrate2-------------------------------------------------------

truediscoveryrate(eta=0.99, chi=0.995, rho=0.75, M=100, R=1)


## ----expected_pairs-----------------------------------------------------------

true_pairs(eta=0.99, rho=0.75, M=100, R=1)
exp_links(eta=0.99, chi=0.95, rho=0.75, M=100, R=1)


