##' Probability of transmission assuming single-transmission and single-linkage
##'
##' This function calculates the probabilitiy that two cases are linked by direct transmission
##' given that they have been linked by phylogenetic criteria. The single-transmission and single-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is the infector of only one other case \eqn{j} in the population (\eqn{N}).
##'      \item Each case \eqn{i} is linked by the linkage criteria to only one other case \eqn{j} in the sampled population (\eqn{M}).
##'      }
##'
##' For perfect sensitivity, set \code{eta = 1}.
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##'
##' @return scalar or vector giving the probability of transmission between two cases given linkage by phylogeneitic criteria
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @example R/examples/prob_trans_stsl.R
##'
##' @family prob_trans
##'
##' @export

prob_trans_stsl <- function(
  eta,    # sensitivity of the linkage criteria
  chi,    # specificity of the linkage criteria
  rho,    # sampling proportion
  M       # number of cases sampled
){

  if (!all(is.numeric(eta), eta >= 0 & eta <= 1,
           is.numeric(chi), chi >= 0 & chi <= 1,
           is.numeric(rho), rho >= 0 & rho <= 1)) stop('Arguments eta, chi, and rho must be numeric between 0 and 1')

  if (!all(is.numeric(M) | is.integer(M), M > 0)) stop('Sample size (M) must be integer or numeric greater than 0')

  (eta * rho) /
    ((eta * rho) + ((1 - chi^(M-2)) * (1 - eta) * rho) + ((1 - chi^(M-1)) * (1 - rho)))
}



##' Expected number of pairs assuming single-transmission and single-linkage
##'
##' This function calculates the expected number of link pairs observed in a sample of size \code{M}.
##' The single-transmission and single-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is the infector of only one other case \eqn{j} in the population (\eqn{N}).
##'      \item Each case \eqn{i} is linked by the linkage criteria to only one other case \eqn{j} in the sampled population (\eqn{M}).
##'      }
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##'
##' @return scalar or vector giving the expected number of true transmission pairs in the sample
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @example R/examples/obs_pairs_stsl.R
##'
##' @family obs_pairs
##'
##' @export

obs_pairs_stsl <- function(
  eta,
  chi,    # specificity of the linkage criteria
  rho,    # sampling proportion
  M       # number of cases sampled

){

  if (!all(is.numeric(eta), eta >= 0 & eta <= 1,
           is.numeric(chi), chi >= 0 & chi <= 1,
           is.numeric(rho), rho >= 0 & rho <= 1)) stop('Arguments eta, chi, and rho must be numeric between 0 and 1')

  if (!all(is.numeric(M) | is.integer(M), M > 0)) stop('Sample size (M) must be integer or numeric greater than 0')

  (M / 2) * ((eta * rho) + (rho * (1 - eta) * (1 - chi^(M-2))) + ((1 - rho) * (1 - chi^(M-1))))
}



##' Probability of transmission assuming multiple-transmission and single-linkage
##'
##' This function calculates the probabilitiy that two cases are linked by direct transmission
##' given that they have been linked by phylogenetic criteria. The multiple-transmission and single-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is the infector of \code{R} cases in the population (\eqn{N}), where \code{R} is Poisson distributed
##'      with its mean equal to the effective reproductive number of the pathogen.
##'      \item Each case \eqn{i} is linked by the linkage criteria to only one case \eqn{j} in the sampled population (\eqn{M}).
##'      }
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##' @param R scalar or vector giving the effective reproductive number of the pathogen
##'
##' @return scalar or vector giving the probability of transmission between two cases given linkage by phylogeneitic criteria
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @example R/examples/prob_trans_mtsl.R
##'
##' @family prob_trans
##'
##' @export
##'

prob_trans_mtsl <- function(
  chi,    # specificity of the linkage criteria
  eta,    # sensitivity of the linkage criteria
  rho,    # sampling proportion
  M,      # number of cases sampled
  R       # effective reproductive number
){

  if (!all(is.numeric(eta), eta >= 0 & eta <= 1,
           is.numeric(chi), chi >= 0 & chi <= 1,
           is.numeric(rho), rho >= 0 & rho <= 1)) stop('Arguments eta, chi, and rho must be numeric between 0 and 1')

  if (!all(is.numeric(M) | is.integer(M), M > 0)) stop('Sample size (M) must be integer or numeric greater than 0')
  if (!all(is.numeric(R), R > 0)) stop('Reproductive number (R) must be numeric greater than 0')

  (1 - exp(-rho * eta * (R+1))) /
    (1 - ((chi^(M-2)) * exp(rho * (R+1) * ((1-eta)/chi - 1))))
}



##' Expected number of pairs assuming multiple-transmission and single-linkage
##'
##' This function calculates the expected number of pairs observed in a sample of size \code{M}.
##' The multiple-transmission and single-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is the infector of \code{R} cases in the population (\eqn{N}), where \code{R} is Poisson distributed
##'      with its mean equal to the effective reproductive number of the pathogen.
##'      \item Each case \eqn{i} is linked by the linkage criteria to only one case \eqn{j} in the sampled population (\eqn{M}).
##'      }
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##' @param R scalar or vector giving the effective reproductive number of the pathogen
##'
##' @return scalar or vector giving the expected number of true transmission pairs in the sample
##'
##' @author John Giles, Shirlee Wohl and Justin Lessler
##'
##' @example R/examples/obs_pairs_mtsl.R
##'
##' @family obs_pairs
##'
##' @export
##'
##'

obs_pairs_mtsl <- function(
  chi,    # specificity of the linkage criteria
  eta,    # sensitivity of the linkage criteria
  rho,    # sampling proportion
  M,      # number of cases sampled
  R       # effective reproductive number
){

  if (!all(is.numeric(eta), eta >= 0 & eta <= 1,
           is.numeric(chi), chi >= 0 & chi <= 1,
           is.numeric(rho), rho >= 0 & rho <= 1)) stop('Arguements eta, chi, and rho must be numeric between 0 and 1')

  if (!all(is.numeric(M) | is.integer(M), M > 0)) stop('Sample size (M) must be integer or numeric greater than 0')
  if (!all(is.numeric(R), R > 0)) stop('Reproductive number (R) must be numeric greater than 0')

  (M * rho * (R+1) * eta * (1 - ((chi^(M-2))) * exp(rho * (R+1) * ((1-eta)/chi -1)))) /
    (2 * (1 - exp(-rho * (R+1) * eta)))

}



##' Probability of transmission assuming multiple-transmission and multiple-linkage
##'
##' This function calculates the probabilitiy that two cases are linked by direct transmission
##' given that they have been linked by phylogenetic criteria. The multiple-transmission and multiple-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is the infector of \code{R} cases in the population (\eqn{N}), where \code{R} is Poisson distributed
##'      with its mean equal to the effective reproductive number of the pathogen.
##'      \item Each case \eqn{i} is allowed to be linked by the linkage criteria to multiple cases \eqn{j} in the sampled population (\eqn{M}).
##'      }
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##' @param R scalar or vector giving the effective reproductive number of the pathogen
##'
##' @return scalar or vector giving the probability of transmission between two cases given linkage by phylogeneitic criteria
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @example R/examples/prob_trans_mtml.R
##'
##' @family prob_trans
##'
##' @export
##'

prob_trans_mtml <- function(
  eta,    # sensitivity of the linkage criteria
  chi,    # specificity of the linkage criteria
  rho,    # sampling proportion
  M,      # number of cases sampled
  R       # effective reproductive number
){

  if (!all(is.numeric(eta), eta >= 0 & eta <= 1,
           is.numeric(chi), chi >= 0 & chi <= 1,
           is.numeric(rho), rho >= 0 & rho <= 1)) stop('Arguements eta, chi, and rho must be numeric between 0 and 1')

  if (!all(is.numeric(M) | is.integer(M), M > 0)) stop('Sample size (M) must be integer or numeric greater than 0')
  if (!all(is.numeric(R), R > 0)) stop('Reproductive number (R) must be numeric greater than 0')


  (eta * rho * (R+1)) /
    ((eta * rho * (R+1)) + ((1-chi) * (M - 1 - (rho * (R+1)))))
}



##' Expected number of pairs assuming multiple-transmission and multiple-linkage
##'
##' This function calculates the expected number of pairs observed in a sample of size \code{M}.
##' The multiple-transmission and multiple-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is the infector of \code{R} cases in the population (\eqn{N}), where \code{R} is Poisson distributed
##'      with its mean equal to the effective reproductive number of the pathogen.
##'      \item Each case \eqn{i} is allowed to be linked by the linkage criteria to multiple cases \eqn{j} in the sampled population (\eqn{M}).
##'      }
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##' @param R scalar or vector giving the effective reproductive number of the pathogen
##'
##' @return scalar or vector giving the expected number of true transmission pairs in the sample
##'
##' @author John Giles, Shirlee Wohl and Justin Lessler
##'
##' @example R/examples/obs_pairs_mtml.R
##'
##' @family obs_pairs
##'
##' @export
##'
##'

obs_pairs_mtml <- function(
  chi,    # specificity of the linkage criteria
  eta,    # sensitivity of the linkage criteria
  rho,    # sampling proportion
  M,      # number of cases sampled
  R       # effective reproductive number
){

  if (!all(is.numeric(eta), eta >= 0 & eta <= 1,
           is.numeric(chi), chi >= 0 & chi <= 1,
           is.numeric(rho), rho >= 0 & rho <= 1)) stop('Arguements eta, chi, and rho must be numeric between 0 and 1')

  if (!all(is.numeric(M) | is.integer(M), M > 0)) stop('Sample size (M) must be integer or numeric greater than 0')
  if (!all(is.numeric(R), R > 0)) stop('Reproductive number (R) must be numeric greater than 0')

  (M / 2) * ((eta * rho * (R+1)) + ((1-chi) * (M - (rho * (R+1)))))
}



##' Calculate true discovery rate of a sample
##'
##' This function calculates the true discovery rate (proportion of true transmission pairs) in a sample given the sensitivity \eqn{\eta}
##' and specificity \eqn{\chi} of the linkage criteria, and sample size \eqn{M}. The reproductive number \eqn{R} of the pathogen is used when case i
##' is allowed to infect multple cases in the population \eqn{N} (multiple-transmission assumption).
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##' @param R scalar or vector giving the effective reproductive number of the pathogen (default=NULL)
##' @param assumption a character vector indicating which assumptions about transmission and linkage criteria. The function expects one of the following:
##' \enumerate{
##'      \item \code{'stsl'} for the single-transmission single-linkage assumption (\code{\link{prob_trans_stsl}}).
##'      \item \code{'mtsl'} for the multiple-transmission single-linkage assumption (\code{\link{prob_trans_mtsl}}).
##'      \item \code{'mtml'} for the multiple-transmission multiple-linkage assumption (\code{\link{prob_trans_mtml}}).
##'      }
##'
##' @return scalar or vector giving the true discovery rate
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @example R/examples/truediscoveryrate.R
##'
##' @family discovery_rate
##'
##' @export
##'

truediscoveryrate <- function(
  eta,    # sensitivity of the linkage criteria
  chi,    # specificity of the linkage criteria
  rho,    # sampling proportion
  M,      # number of cases sampled
  R=NULL,       # effective reproductive number
  assumption
){

  if (assumption == 'stsl') {

    message('Calculating true discovery rate assuming single-transmission and single-linkage')
    out <- prob_trans_stsl(eta=eta, chi=chi, rho=rho, M=M)

  } else if (assumption == 'mtsl') {

    message('Calculating true discovery rate assuming multiple-transmission and single-linkage')
    out <- prob_trans_mtsl(eta=eta, chi=chi, rho=rho, M=M, R=R)

  } else if (assumption == 'mtml') {

    message('Calculating true discovery rate assuming multiple-transmission and multiple-linkage')
    out <- prob_trans_mtml(eta=eta, chi=chi, rho=rho, M=M, R=R)

  } else {

    stop("Incorrect assumption argument")
  }

  return(out)
}



##' Calculate false discovery rate of a sample
##'
##' This function calculates the false discovery rate (proportion of linked pairs that are false positives) in a sample given the sensitivity \eqn{\eta}
##' and specificity \eqn{\chi} of the linkage criteria, and sample size \eqn{M}. The reproductive number \eqn{R} of the pathogen is used when case i
##' is allowed to infect multple cases in the population \eqn{N} (multiple-transmission assumption).
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##' @param R scalar or vector giving the effective reproductive number of the pathogen (default=NULL)
##' @param assumption a character vector indicating which assumptions about transmission and linkage criteria. The function expects one of the following:
##' \enumerate{
##'      \item \code{'stsl'} for the single-transmission single-linkage assumption (\code{\link{prob_trans_stsl}}).
##'      \item \code{'mtsl'} for the multiple-transmission single-linkage assumption (\code{\link{prob_trans_mtsl}}).
##'      \item \code{'mtml'} for the multiple-transmission multiple-linkage assumption (\code{\link{prob_trans_mtml}}).
##'      }
##'
##' @return scalar or vector giving the true discovery rate
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @example R/examples/falsediscoveryrate.R
##'
##' @family discovery_rate
##'
##' @export
##'

falsediscoveryrate <- function(
  eta,    # sensitivity of the linkage criteria
  chi,    # specificity of the linkage criteria
  rho,    # sampling proportion
  M,      # number of cases sampled
  R=NULL,       # effective reproductive number
  assumption
){

  1 - truediscoveryrate(eta=eta,
                        chi=chi,
                        rho=rho,
                        M=M,
                        R=R,
                        assumption=assumption)
}



##' Calculate expected number of links in a sample
##'
##' This function calculates the expected number of observed pairs in the sample that are linked by the linkage criteria. The function requires the sensitivity \eqn{\eta}
##' and specificity \eqn{\chi} of the linkage criteria, and sample size \eqn{M}. The reproductive number \eqn{R} of the pathogen is used when case i
##' is allowed to infect multple cases in the population \eqn{N} (multiple-transmission assumption).
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##' @param R scalar or vector giving the effective reproductive number of the pathogen (default=NULL)
##' @param assumption a character vector indicating which assumptions about transmission and linkage criteria. The function expects one of the following:
##' \enumerate{
##'      \item \code{'stsl'} for the single-transmission single-linkage assumption (\code{\link{prob_trans_stsl}}).
##'      \item \code{'mtsl'} for the multiple-transmission single-linkage assumption (\code{\link{prob_trans_mtsl}}).
##'      \item \code{'mtml'} for the multiple-transmission multiple-linkage assumption (\code{\link{prob_trans_mtml}}).
##'      }
##'
##' @return scalar or vector giving the expected number of observed links in the sample
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @example R/examples/exp_links.R
##'
##' @family obs_pairs
##'
##' @export
##'

exp_links <- function(
  eta,    # sensitivity of the linkage criteria
  chi,    # specificity of the linkage criteria
  rho,    # sampling proportion
  M,      # number of cases sampled
  R=NULL,       # effective reproductive number
  assumption
){

  if (assumption == 'stsl') {

    message('Calculating expected number of links assuming single-transmission and single-linkage')
    out <- obs_pairs_stsl(eta=eta, chi=chi, rho=rho, M=M)

  } else if (assumption == 'mtsl') {

    message('Calculating expected number of links assuming multiple-transmission and single-linkage')
    out <- obs_pairs_mtsl(eta=eta, chi=chi, rho=rho, M=M, R=R)

  } else if (assumption == 'mtml') {

    message('Calculating expected number of links assuming multiple-transmission and multiple-linkage')
    out <- obs_pairs_mtml(eta=eta, chi=chi, rho=rho, M=M, R=R)

  } else {

    stop("Incorrect assumption argument")
  }

  return(out)
}
