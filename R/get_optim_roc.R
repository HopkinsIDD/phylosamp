##' Find optimal ROC threshold
##'
##' @description
##' `r lifecycle::badge('deprecated')`
##' This function takes the dataframe output of the `sens_spec_roc()` function and finds the optimnal threshold
##' of sensitivity and specificity by minimizing the distance to the top left corner of the Receiver Operating Characteriztic (ROC) curve
##'
##' @param roc a dataframe produced by the `sens_spec_roc()` function containing the Receiver Operating Characteristic (ROC) curve
##'
##' @return vector contaitng optimal thresholds of sensitivity and specificity
##'
##' @author Shirlee Wohl, John Giles, and Justin Lessler
##'
##' @examples
##' # ebola-like pathogen
##' R <- 1.5
##' mut_rate <- 1
##'
##' # use simulated generation distributions
##' data('gen_dist_sim')
##' mean_gens_pdf <- as.numeric(gen_dist_sim[gen_dist_sim$R == R, -(1:2)])
##'
##' # get theoretical genetic distance dist based on mutation rate and generation parameters
##' dists <- as.data.frame(gen_dists(mut_rate = mut_rate,
##'                                  mean_gens_pdf = mean_gens_pdf,
##'                                  max_link_gens = 1))
##'
##' # reshape dataframe for plotting
##' dists <- reshape2::melt(dists,
##'                         id.vars = 'dist',
##'                         variable.name = 'status',
##'                         value.name = 'prob')
##'
##' # get sensitivity and specificity using the same paramters
##' roc_calc <- sens_spec_roc(cutoff = 1:(max(dists$dist)-1),
##'                           mut_rate = mut_rate,
##'                           mean_gens_pdf = mean_gens_pdf)
##'
##' # get the optimal value for the ROC plot
##' optim_point <- get_optim_roc(roc_calc)
##'
##' @family mutrate_functions
##'
##' @export
##'

get_optim_roc <- function(roc) {
    lifecycle::deprecate_soft("1.0.0", "get_optim_roc()", "optim_roc_threshold()")

    roc <- roc[-1, ]  # remove first row with zero threshold
    dist <- sqrt((1 - roc$sensitivity)^2 + (roc$specificity)^2)
    as.list(roc[dist == min(dist), ])
}
