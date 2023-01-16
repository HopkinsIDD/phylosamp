##' Calculate sensitivity and specificity
##'
##' Function to calculate the sensitivity and specificity of a genetic distance cutoff
##' given an underlying mutation rate and mean number of generations between cases
##'
##' @param cutoff the maximum genetic distance at which to consider cases linked
##' @param mut_rate mean number of mutations per generation, assumed to be poisson distributed
##' @param mean_gens_pdf the density distribution of the mean number of generations between cases;
##'       the index of this vector is assumed to be the discrete distance between cases
##' @param max_link_gens the maximium generations of separation for linked pairs
##' @param max_gens the maximum number of generations to consider, if \code{NULL} (default) value set to the highest
##'        number of generations in mean_gens_pdf with a non-zero probability
##' @param max_dist the maximum distance to calculate, if \code{NULL} (default) value set to max_gens * 99.9th percentile
##'       of mut_rate poisson distribution
##'
##' @return a data frame with the sensitivity and specificity for a particular genetic distance cutoff
##'
##' @author Shirlee Wohl and Justin Lessler
##'
##' @examples
##' # calculate the sensitivity and specificity for a specific genetic distance threshold of 2 mutations
##' gendist_sensspec_cutoff(cutoff=2,
##'                mut_rate=1,
##'                mean_gens_pdf=c(0.02,0.08,0.15,0.75),
##'                max_link_gens=1)
##'
##' # calculate the sensitivity and specificity for a a range of genetic distance thresholds
##' gendist_sensspec_cutoff(cutoff=1:10,
##'                mut_rate=1,
##'                mean_gens_pdf=c(0.02,0.08,0.15,0.75),
##'                max_link_gens=1)
##'
##' @family genetic distance functions
##'
##' @export
##'

gendist_sensspec_cutoff <- function(cutoff,
                           mut_rate,
                           mean_gens_pdf,
                           max_link_gens = 1,
                           max_gens = NULL,
                           max_dist = NULL) {
  if (is.null(max_gens)) max_gens <- which(mean_gens_pdf != 0)[length(which(mean_gens_pdf != 0))]
  if (is.null(max_dist)) max_dist <- suppressWarnings(max_gens * stats::qpois(.999, mut_rate))

  # check that we have used a sensible cutoff
  # the mutation rate should be high enough such that the cutoff used is less than the max possible distance
  if (max_dist < max(cutoff + 1)) {
    warning("Nonsensical cutoff given distances considered")
  }

  # get linked and unlinked probability distributions
  gendist <- gen_dists(
    mut_rate = mut_rate, mean_gens_pdf = mean_gens_pdf,
    max_link_gens = max_link_gens, max_gens = max_gens, max_dist = max_dist
  )

  linked_pdf <- gendist[, "linked_prob"]
  unlinked_pdf <- gendist[, "unlinked_prob"]

  linked_cdf <- cumsum(linked_pdf) / sum(linked_pdf)
  unlinked_cdf <- cumsum(unlinked_pdf) / sum(unlinked_pdf)

  # Utility function to allow for multiple sensitivity and specificity cutoffs
  get_sens_spec <- function(cutoff) {

    # remember that cdf[cutoff] represents the probability of cutoff-1
    # but this is what we want because the threshold is <cutoff (not <=)

    # decrease specificity by probability mass of unlinked
    spec <- 1 - unlinked_cdf[cutoff]

    # decrease sensitivity by probability mass of linked
    sens <- 1 - (1 - linked_cdf[cutoff])

    return(c(sens, spec))
  }

  rc <- t(sapply(cutoff, get_sens_spec))
  rc <- cbind(cutoff, rc)
  colnames(rc) <- c("cutoff", "sensitivity", "specificity")

  return(rc)
}
