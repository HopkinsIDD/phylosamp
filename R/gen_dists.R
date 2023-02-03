##' Calculate genetic distance distribution
##'
##' Function calculates the distribution of genetic distances in a population of viruses
##' with the given parameters
##'
##' @param mut_rate mean number of mutations per generation, assumed to be poisson distributed
##' @param mean_gens_pdf the density distribution of the mean number of generations between cases;
##'       the index of this vector is assumed to be the discrete distance between cases
##' @param max_link_gens the maximium generations of separation for linked pairs
##' @param max_gens the maximum number of generations to consider, if \code{NULL} (default) value is set to the highest
##'        number of generations in mean_gens_pdf with a non-zero probability
##' @param max_dist the maximum distance to calculate, if \code{NULL} (default) value is set to max_gens * 99.9th percentile
##'       of mut_rate poisson distribution
##'
##' @return a data frame with distances and probabilities
##'
##' @author Shirlee Wohl and Justin Lessler
##'
##' @examples
##' # ebola-like pathogen
##' R <- 1.5
##' mut_rate <- 1
##'
##' # use simulated generation distributions from the provided 'gen_dist_sim' data object
##' data('gen_dist_sim')
##' mean_gens_pdf <- as.numeric(gen_dist_sim[gen_dist_sim$R == R, -(1:2)])
##'
##' # get theoretical genetic distance dist based on mutation rate and generation parameters
##' gen_dists(mut_rate = mut_rate,
##'           mean_gens_pdf = mean_gens_pdf,
##'           max_link_gens = 1)
##'
##' @family mutrate_functions
##'
##' @export
##'

gen_dists <- function(mut_rate, mean_gens_pdf, max_link_gens = 1, max_gens = NULL,
    max_dist = NULL) {
    lifecyle::deprecate_soft("1.0.0", "gen_dists()", "gendist_distribution()")

    if (!all(is.numeric(mut_rate), mut_rate >= 0))
        stop("Mutation rate must have a positive value")

    if (is.null(max_gens))
        max_gens <- which(mean_gens_pdf != 0)[length(which(mean_gens_pdf != 0))]
    if (is.null(max_dist))
        max_dist <- suppressWarnings(max_gens * stats::qpois(0.999, mut_rate))

    if (!all(is.numeric(max_gens), max_gens > 0))
        stop("Maximum number of generations to consider must be numeric greater than zero")
    if (!all(is.numeric(max_dist), max_dist >= 0))
        stop("Maximum distance to consider must have a positive value")
    if (!all(is.numeric(max_link_gens), max_link_gens > 0))
        stop("Maximum number of generations to consider linked must be numeric greater than zero")

    if (sum(mean_gens_pdf) <= 0)
        stop("Generation distribution must have at least one non-zero value")
    if (any(mean_gens_pdf < 0))
        stop("Generation distribution cannot contain negative probabilities")

    # set up matrix
    gendist <- matrix(0, nrow = max_dist + 1, ncol = 3)
    colnames(gendist) <- c("dist", "linked_prob", "unlinked_prob")
    gendist[, 1] <- 0:max_dist

    # get the CDF from the PDF
    mean_gens_cdf <- cumsum(mean_gens_pdf)/sum(mean_gens_pdf)

    # calculate the probability distribution for linked cases
    for (i in 1:(max_dist + 1)) {
        for (j in 1:max_link_gens) {
            # calculate the probability of having a specific genetic distance
            # for all generation separations considered linked
            gendist[i, 2] <- gendist[i, 2] + mean_gens_pdf[j] * stats::dpois(i -
                1, j * mut_rate)
        }
    }

    # calculate the probability distribution for unlinked cases
    for (i in 1:(max_dist + 1)) {
        for (j in (max_link_gens + 1):max_gens) {
            # calculate the probability of having a specific genetic distance
            # for all generation separations considered unlinked
            gendist[i, 3] <- gendist[i, 3] + mean_gens_pdf[j] * stats::dpois(i -
                1, j * mut_rate)
        }
    }

    # normalize the probability distributions for linked and unlinked cases
    gendist[, "linked_prob"] <- gendist[, "linked_prob"]/sum(gendist[, "linked_prob"])
    gendist[, "unlinked_prob"] <- gendist[, "unlinked_prob"]/sum(gendist[, "unlinked_prob"])

    return(gendist)
}
