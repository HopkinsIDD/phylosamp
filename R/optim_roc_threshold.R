##' Find optimal ROC threshold
##'
##' This function takes the dataframe output of the `gendist_roc_format()` function and finds the optimnal threshold
##' of sensitivity and specificity by minimizing the distance to the top left corner of the Receiver Operating Characteriztic (ROC) curve
##'
##' @param roc a dataframe produced by the `gendist_roc_format()` function containing the Receiver Operating Characteriztic (ROC) curve
##'
##' @return vector contaitng optimal thresholds ofsensitivity and specificity
##'
##' @author Shirlee Wohl, John Giles, and Justin Lessler
##'
##' @examples
##' # ebola-like pathogen
##' R <- 1.5
##' mut_rate <- 1
##'
##' # use simulated generation distributions
##' data("genDistSim")
##' mean_gens_pdf <- as.numeric(genDistSim[genDistSim$R == R, -(1:2)])
##'
##' # get theoretical genetic distance dist based on mutation rate and generation parameters
##' dists <- as.data.frame(gendist_distribution(mut_rate = mut_rate,
##'                        mean_gens_pdf = mean_gens_pdf,
##'                        max_link_gens = 1))
##'
##' # reshape dataframe for plotting
##' dists <- reshape2::melt(dists,
##'                         id.vars = "dist",
##'                         variable.name = "status",
##'                         value.name = "prob")
##'
##' # get sensitivity and specificity using the same paramters
##' roc_calc <- gendist_roc_format(cutoff = 1:(max(dists$dist)-1),
##'                           mut_rate = mut_rate,
##'                           mean_gens_pdf = mean_gens_pdf)
##'
##' # get the optimal value for the ROC plot
##' optim_point <- optim_roc_threshold(roc_calc)
##'
##' @family ROC functions
##'
##' @export
##'

optim_roc_threshold <- function(roc) {
  roc <- roc[-1, ] # remove first row with zero threshold
  dist <- sqrt((1 - roc$sensitivity)^2 + (roc$specificity)^2)
  as.list(roc[dist == min(dist), ])
}
