##' Find optimal ROC threshold
##'
##' This function takes the dataframe output of the `sens_spec_roc()` function and finds the optimnal threshold 
##' of sensitivity and specificity by minimizing the distance to the top left corner of the Receiver Operating Characteriztic (ROC) curve
##'
##' @param roc a dataframe produced by the `sens_spec_roc()` function containing the Receiver Operating Characteriztic (ROC) curve
##' 
##' @return vector contaitng optimal thresholds ofsensitivity and specificity
##' 
##' @author Shirlee Wohl, John Giles, and Justin Lessler
##'
##' @example R/examples/get_optim_roc.R
##'
##' @family mutrate_functions
##'
##' @export
##'

get_optim_roc <- function(roc) {

  roc <- roc[-1,] # remove first row with zero threshold
  dist <- sqrt((1-roc$sensitivity)^2 + (roc$specificity)^2)
  as.list(roc[dist == min(dist),])
}
