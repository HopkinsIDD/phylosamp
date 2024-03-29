% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gendist_roc_format.R
\name{gendist_roc_format}
\alias{gendist_roc_format}
\title{Make ROC curve from sensitivity and specificity}
\usage{
gendist_roc_format(
  cutoff,
  mut_rate,
  mean_gens_pdf,
  max_link_gens = 1,
  max_gens = NULL,
  max_dist = NULL
)
}
\arguments{
\item{cutoff}{the maximum genetic distance at which to consider cases linked}

\item{mut_rate}{mean number of mutations per generation, assumed to be Poisson distributed}

\item{mean_gens_pdf}{the density distribution of the mean number of generations between cases;
the index of this vector is assumed to be the discrete distance between cases}

\item{max_link_gens}{the maximum generations of separation for linked pairs}

\item{max_gens}{the maximum number of generations to consider, if \code{NULL} (default) value set to the highest
number of generations in mean_gens_pdf with a non-zero probability}

\item{max_dist}{the maximum distance to calculate, if \code{NULL} (default) value set to max_gens * 99.9th percentile
of mut_rate Poisson distribution}
}
\value{
data frame with cutoff, sensitivity, and 1-specificity
}
\description{
This is a wrapper function that takes output from the \code{gendist_sensspec_cutoff()} function and constructs values for the
Receiver Operating Characteristic (ROC) curve
}
\examples{
# ebola-like pathogen
R <- 1.5
mut_rate <- 1

# use simulated generation distributions
data('genDistSim')
mean_gens_pdf <- as.numeric(genDistSim[genDistSim$R == R, -(1:2)])

# get theoretical genetic distance dist based on mutation rate and generation parameters
dists <- as.data.frame(gendist_distribution(mut_rate = mut_rate,
                       mean_gens_pdf = mean_gens_pdf,
                       max_link_gens = 1))

dists <- reshape2::melt(dists,
                        id.vars = 'dist',
                        variable.name = 'status',
                        value.name = 'prob')

# get sensitivity and specificity using the same paramters
roc_calc <- gendist_roc_format(cutoff = 1:(max(dists$dist)-1),
                               mut_rate = mut_rate,
                               mean_gens_pdf = mean_gens_pdf)

}
\seealso{
Other genetic distance functions: 
\code{\link{gendist_distribution}()},
\code{\link{gendist_sensspec_cutoff}()}

Other ROC functions: 
\code{\link{optim_roc_threshold}()}
}
\author{
Shirlee Wohl and Justin Lessler
}
\concept{ROC functions}
\concept{genetic distance functions}
