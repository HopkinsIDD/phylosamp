##' Probability of transmission assuming single-transmission and single-linkage
##'
##' This function calculates the probabilitiy that two cases are linked by direct transmission
##' given that they have been linked by phylogenetic criteria. The single-transmission and single-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is linked by transmission to only one other case \eqn{j} in the population (\eqn{N}).
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



##' Expected number of observed pairs assuming single-transmission and single-linkage
##'
##' This function calculates the expected number of link pairs observed in a sample of size \code{M}.
##' The single-transmission and single-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is linked by transmission to only one other case \eqn{j} in the population (\eqn{N}).
##'      \item Each case \eqn{i} is linked by the linkage criteria to only one other case \eqn{j} in the sampled population (\eqn{M}).
##'      }
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##'
##' @return scalar or vector giving the expected number of linked pairs observed in the sample
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



##' Expected number of true transmission pairs assuming single-transmission and single-linkage
##'
##' This function calculates the expected number of true transmission pairs in a sample of size \code{M}.
##' The single-transmission and single-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is linked by transmissino to only one other case \eqn{j} in the population (\eqn{N}).
##'      \item Each case \eqn{i} is linked by the linkage criteria to only one other case \eqn{j} in the sampled population (\eqn{M}).
##'      }
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##'
##' @return scalar or vector giving the expected number of true transmission pairs in the sample
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @example R/examples/true_pairs_stsl.R
##'
##' @family true_pairs
##'
##' @export

true_pairs_stsl <- function(
  eta,
  rho,    # sampling proportion
  M       # number of cases sampled
  
){
  
  if (!all(is.numeric(eta), eta >= 0 & eta <= 1,
           is.numeric(rho), rho >= 0 & rho <= 1)) stop('Arguments eta, chi, and rho must be numeric between 0 and 1')
  
  if (!all(is.numeric(M) | is.integer(M), M > 0)) stop('Sample size (M) must be integer or numeric greater than 0')
  
  (M / 2) * eta * rho
}



##' Probability of transmission assuming multiple-transmission and single-linkage
##'
##' This function calculates the probabilitiy that two cases are linked by direct transmission
##' given that they have been linked by phylogenetic criteria. The multiple-transmission and single-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is, on average, the infector of \code{R} cases in the population (\eqn{N})
##'      \item Each case \eqn{i} is allowed to be linked by the linkage criteria to only one other case \eqn{j} in the sampled population (\eqn{M}).
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
  if (!all(is.numeric(R), R <= 1)) warning('Reproductive number (R) is usually less than 1 for finite outbreaks')

  (1 - exp(-rho * eta * (R+1))) /
    (1 - ((chi^(M-1)) * exp(rho * (R+1) * ((1-eta)/chi - 1))))
}



##' Expected number of observed pairs assuming multiple-transmission and single-linkage
##'
##' This function calculates the expected number of pairs observed in a sample of size \code{M}.
##' The multiple-transmission and single-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is, on average, the infector of \code{R} cases in the population (\eqn{N})
##'      \item Each case \eqn{i} is allowed to be linked by the linkage criteria to only one other case \eqn{j} in the sampled population (\eqn{M}).
##'      }
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##' @param R scalar or vector giving the effective reproductive number of the pathogen
##'
##' @return scalar or vector giving the expected number of linked pairs observed in the sample
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
  if (!all(is.numeric(R), R <= 1)) warning('Reproductive number (R) is usually less than 1 for finite outbreaks')

  (M * rho * (R+1) * eta * (1 - ((chi^(M-1))) * exp(rho * (R+1) * (((1-eta)/chi) -1)))) /
    (2 * (1 - exp(-rho * (R+1) * eta)))

}



##' Expected number of true transmission pairs assuming multiple-transmission and single-linkage
##'
##' This function calculates the expected number true transmission pairs in a sample of size \code{M}.
##' The multiple-transmission and single-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is, on average, the infector of \code{R} cases in the population (\eqn{N})
##'      \item Each case \eqn{i} is allowed to be linked by the linkage criteria to only one other case \eqn{j} in the sampled population (\eqn{M}).
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
##' @example R/examples/true_pairs_mtsl.R
##'
##' @family true_pairs
##'
##' @export
##'
##'

true_pairs_mtsl <- function(
  eta,    # sensitivity of the linkage criteria
  rho,    # sampling proportion
  M,      # number of cases sampled
  R       # effective reproductive number
){
  
  if (!all(is.numeric(eta), eta >= 0 & eta <= 1,
           is.numeric(rho), rho >= 0 & rho <= 1)) stop('Arguements eta, chi, and rho must be numeric between 0 and 1')
  
  if (!all(is.numeric(M) | is.integer(M), M > 0)) stop('Sample size (M) must be integer or numeric greater than 0')
  if (!all(is.numeric(R), R > 0)) stop('Reproductive number (R) must be numeric greater than 0')
  if (!all(is.numeric(R), R <= 1)) warning('Reproductive number (R) is usually less than 1 for finite outbreaks')
  
  (M * rho * (R+1) * eta) / 2
}



##' Probability of transmission assuming multiple-transmission and multiple-linkage
##'
##' This function calculates the probabilitiy that two cases are linked by direct transmission
##' given that they have been linked by phylogenetic criteria. The multiple-transmission and multiple-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is, on average, the infector of \code{R} cases in the population (\eqn{N})
##'      \item Each case \eqn{i} is allowed to be linked by the linkage criteria to multiple cases \eqn{j} in the sampled population (\eqn{M}).
##'      \item Linkage events are independent of one another (i.e, linkage of case \eqn{i} to case \eqn{j} has no bearing on linkage of case \eqn{i} to any other sample).
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
  if (!all(is.numeric(R), R <= 1)) warning('Reproductive number (R) is usually less than 1 for finite outbreaks')


  (eta * rho * (R+1)) /
    ((eta * rho * (R+1)) + ((1-chi) * (M - 1 - (rho * (R+1)))))
}



##' Expected number of observed pairs assuming multiple-transmission and multiple-linkage
##'
##' This function calculates the expected number of pairs observed in a sample of size \code{M}.
##' The multiple-transmission and multiple-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is, on average, the infector of \code{R} cases in the population (\eqn{N})
##'      \item Each case \eqn{i} is allowed to be linked by the linkage criteria to multiple cases \eqn{j} in the sampled population (\eqn{M}).
##'      \item Linkage events are independent of one another (i.e, linkage of case \eqn{i} to case \eqn{j} has no bearing on linkage of case \eqn{i} to any other sample).
##'      }
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##' @param R scalar or vector giving the effective reproductive number of the pathogen
##'
##' @return scalar or vector giving the expected number of linked pairs observed in the sample
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
  if (!all(is.numeric(R), R <= 1)) warning('Reproductive number (R) is usually less than 1 for finite outbreaks')

  (M / 2) * ((eta * rho * (R+1)) + ((1-chi) * (M - 1- (rho * (R+1)))))
}



##' Expected number of true transmission pairs assuming multiple-transmission and multiple-linkage
##'
##' This function calculates the expected number of true transmission pairs in a sample of size \code{M}.
##' The multiple-transmission and multiple-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is, on average, the infector of \code{R} cases in the population (\eqn{N})
##'      \item Each case \eqn{i} is allowed to be linked by the linkage criteria to multiple cases \eqn{j} in the sampled population (\eqn{M}).
##'      \item Linkage events are independent of one another (i.e, linkage of case \eqn{i} to case \eqn{j} has no bearing on linkage of case \eqn{i} to any other sample).
##'      }
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##' @param R scalar or vector giving the effective reproductive number of the pathogen
##'
##' @return scalar or vector giving the expected number of true transmission pairs in the sample
##'
##' @author John Giles, Shirlee Wohl and Justin Lessler
##'
##' @example R/examples/true_pairs_mtml.R
##'
##' @family true_pairs
##'
##' @export
##'

true_pairs_mtml <- function(
  eta,    # sensitivity of the linkage criteria
  rho,    # sampling proportion
  M,      # number of cases sampled
  R       # effective reproductive number
){
  
  if (!all(is.numeric(eta), eta >= 0 & eta <= 1,
           is.numeric(rho), rho >= 0 & rho <= 1)) stop('Arguements eta, chi, and rho must be numeric between 0 and 1')
  
  if (!all(is.numeric(M) | is.integer(M), M > 0)) stop('Sample size (M) must be integer or numeric greater than 0')
  if (!all(is.numeric(R), R > 0)) stop('Reproductive number (R) must be numeric greater than 0')
  if (!all(is.numeric(R), R <= 1)) warning('Reproductive number (R) is usually less than 1 for finite outbreaks')
  
  (M * rho * (R+1) * eta) / 2
}


# <--- WRAPPER FUNCTIONS ---> #


##' Calculate true discovery rate of a sample
##'
##' This function calculates the true discovery rate (proportion of true transmission pairs) in a sample given the sensitivity \eqn{\eta}
##' and specificity \eqn{\chi} of the linkage criteria, and sample size \eqn{M}. Assumptions about transmission and linkage (single or multiple)
##' can be specified.
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
  assumption='mtml' # assume most general case if not specified
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
##' and specificity \eqn{\chi} of the linkage criteria, and sample size \eqn{M}. Assumptions about transmission and linkage (single or multiple)
##' can be specified.
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
  assumption='mtml' # assume most general case if not specified
){

  suppressMessages(
  1 - truediscoveryrate(eta=eta,
                        chi=chi,
                        rho=rho,
                        M=M,
                        R=R,
                        assumption=assumption)
  )
}



##' Calculate expected number of links in a sample
##'
##' This function calculates the expected number of observed pairs in the sample that are linked by the linkage criteria. The function requires the sensitivity \eqn{\eta}
##' and specificity \eqn{\chi} of the linkage criteria, and sample size \eqn{M}. Assumptions about transmission and linkage (single or multiple)
##' can be specified.
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
  assumption='mtml' # assume most general case if not specified
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



##' Calculate expected number of true transmission pairs
##'
##' This function calculates the expected number true transmission pairs in a sample of size \code{M}.
##' Assumptions about transmission and linkage (single or multiple) can be specified.
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
##' @return scalar or vector giving the expected number of true transmission pairs in the sample
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @example R/examples/true_pairs.R
##'
##' @family true_pairs
##'
##' @export
##'

true_pairs <- function(
  eta,    # sensitivity of the linkage criteria
  rho,    # sampling proportion
  M,      # number of cases sampled
  R=NULL,       # effective reproductive number
  assumption='mtml' # assume most general case if not specified
){
  
  if (assumption == 'stsl') {
    
    message('Calculating expected number of links assuming single-transmission and single-linkage')
    out <- true_pairs_stsl(eta=eta, rho=rho, M=M)
    
  } else if (assumption == 'mtsl') {
    
    message('Calculating expected number of links assuming multiple-transmission and single-linkage')
    out <- true_pairs_mtsl(eta=eta, rho=rho, M=M, R=R)
    
  } else if (assumption == 'mtml') {
    
    message('Calculating expected number of links assuming multiple-transmission and multiple-linkage')
    out <- true_pairs_mtml(eta=eta, rho=rho, M=M, R=R)
    
  } else {
    
    stop("Incorrect assumption argument")
  }
  
  return(out)
}


# <--- MLMT-ONLY INVERSE FUNCTIONS ---> #


##' Sample size required to obtain a defined false discovery rate given the final outbreak size
##'
##' This function calculates the sample size needed to obtain a defined false disovery rate given a final outbreak size \eqn{N}.
##' This function is only provided for the multiple-transmission and multiple-linkage method, which assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is, on average, the infector of \code{R} cases in the population (\eqn{N})
##'      \item Each case \eqn{i} is allowed to be linked by the linkage criteria to multiple cases \eqn{j} in the sampled population (\eqn{M}).
##'      \item Linkage events are independent of one another (i.e, linkage of case \eqn{i} to case \eqn{j} has no bearing on linkage of case \eqn{i} to any other sample).
##'      }
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param N scalar or vector giving the final outbreak size
##' @param R scalar or vector giving the effective reproductive number of the pathogen
##' @param phi scalar or vector giving the desired true discovery rate (1-false discovery rate)
##' @param min_pairs minimum number of linked pairs observed in the sample, defaults to 1 pair (2 samples); this is to ensure reasonable results are obtained
##'
##' @return scalar or vector giving the sample size needed to meet the given conditions
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @example R/examples/samplesize.R
##'
##' @family inverse_functions
##'
##' @export
##'

samplesize <- function(
  eta,         # sensitivity of the linkage criteria
  chi,         # specificity of the linkage criteria
  N,           # final outbreak size
  R,           # effective reproductive number
  phi,         # true discovery rate
  min_pairs=1  # minimum number of linked pairs, defaults to 1 (2 samples)
){
  
  if (!all(is.numeric(eta), eta >= 0 & eta <= 1,
           is.numeric(chi), chi >= 0 & chi <= 1,
           is.numeric(phi), phi >= 0 & phi <= 1)) stop('Arguements eta, chi, and phi must be numeric between 0 and 1')
  
  if (!all(is.numeric(N), N > 0)) stop('Final outbreak size (N) must be numeric greater than 0')
  if (!all(is.numeric(R), R > 0)) stop('Reproductive number (R) must be numeric greater than 0')
  if (!all(is.numeric(R), R <= 1)) warning('Reproductive number (R) is usually less than 1 for finite outbreaks')
  
  # calculate the sample size needed to obtain the desired true discovery rate
  M = (phi * N * (1-chi)) / 
    ((phi * eta * (R+1)) + (phi * N * (1-chi)) - (phi * (1-chi) * (R+1)) - (eta * (R+1)))
  
  if (M<0) { stop(sprintf('Input values do not produce a viable solution (M=%.2f); consider updating chi, eta, or phi', M)) }
  
  if (M>N) { stop(sprintf('Sample size is larger than final outbreak size (M=%.2f); consider updating chi, eta, or phi', M)) }
  
  # check if this sample size is at least 2*E(pairs observed)
  obs_pairs = suppressMessages(exp_links(eta = eta, chi = chi, rho = M/N, M = M, R = R, assumption = "mtml"))
  
  if (obs_pairs >= min_pairs){ return(M) } else{
    stop(sprintf('Resulting sample size (%.2f) does not produce enough linked pairs (%.2f);
                 consider requiring fewer linked pairs, or updating chi, eta, or phi', M,obs_pairs)) }
  
}


# <--- FUNCTIONS TO ESTIMATE SENSITIVITY AND SPECIFICITY USING GENETIC DISTANCE AS A LINKAGE CRITERIA ---> #


##' Function calculates the distribution of genetic distances in a population of viruses
##' with the given parameterts
##'
##' @param mut_rate mean number of mutations per generation, assumed to be poisson distributed
##' @param mean_gens_pdf the density distribution of the mean number of generations between cases;
##'       the index of this vector is assumed to be the discrete distance between cases
##' @param max_link_gens the maximium generations of separation for linked pairs
##' @param max_gens the maximum number of generations to consider, defaults to the highest
##'        number of generations in mean_gens_pdf with a non-zero probability
##' @param max_dist the maximum distance to calculate, defaults to max_gens * 99.9th percentile
##'       of mut_rate poisson distribution
##' 
##' @return a data frame with distances and probabilities
##' 
##' @author Shirlee Wohl and Justin Lessler
##'
##' @example R/examples/gen_dists.R
##'
##' @family mutrate_functions
##'
##' @export
##'

gen_dists <- function(mut_rate, mean_gens_pdf, max_link_gens=1,
                      max_gens=which(mean_gens_pdf!=0)[length(which(mean_gens_pdf!=0))],
                      max_dist = max_gens*qpois(.999,mut_rate)) {
  
  # set up matrix
  gendist <- matrix(0,nrow=max_dist+1, ncol=3)
  colnames(gendist) <- c("dist","linked_prob","unlinked_prob")
  gendist[,1] <- 0:max_dist
  
  # get the CDF from the PDF
  mean_gens_cdf <- cumsum(mean_gens_pdf)/sum(mean_gens_pdf)
  
  # calculate the probability distribution for linked cases
  for (i in 1:(max_dist+1)){
    for (j in 1:max_link_gens){
      # calculate the probability of having a specific genetic distance
      # for all generation separations considered linked
      gendist[i,2] <- gendist[i,2] + mean_gens_pdf[j] * dpois(i-1,j*mut_rate)
    }
  }
  
  # calculate the probability distribution for unlinked cases
  for (i in 1:(max_dist+1)){
    for (j in (max_link_gens+1):max_gens){
      # calculate the probability of having a specific genetic distance
      # for all generation separations considered unlinked
      gendist[i,3] <- gendist[i,3] + mean_gens_pdf[j] * dpois(i-1,j*mut_rate)
    }
  }
  
  # normalize the probability distributions for linked and unlinked cases
  gendist[,"linked_prob"] <- gendist[,"linked_prob"]/sum(gendist[,"linked_prob"])
  gendist[,"unlinked_prob"] <- gendist[,"unlinked_prob"]/sum(gendist[,"unlinked_prob"])
  
  return(gendist)
  
}

##' Function to calculate the sensitivity and specificity of a genetic distance cutoff
##' given an underlying mutation rate and mean number of generations between cases
##'
##' @param cutoff the maximum genetic distance at which to consider cases linked
##' @param mut_rate mean number of mutations per generation, assumed to be poisson distributed
##' @param mean_gens_pdf the density distribution of the mean number of generations between cases;
##'       the index of this vector is assumed to be the discrete distance between cases
##' @param max_link_gens the maximium generations of separation for linked pairs
##' @param max_gens the maximum number of generations to consider, defaults to the highest
##'        number of generations in mean_gens_pdf with a non-zero probability
##' @param max_dist the maximum distance to calculate, defaults to max_gens * 99.9th percentile
##'       of mut_rate poisson distribution
##'
##' @return a data frame with the sensitivity and specificity for a particular genetic distance cutoff
##' 
##' @author Shirlee Wohl and Justin Lessler
##'
##' @example R/examples/sens_spec_calc.R
##'
##' @family mutrate_functions
##'
##' @export
##'

sens_spec_calc <- function(cutoff, mut_rate, mean_gens_pdf, max_link_gens=1,
                           max_gens=which(mean_gens_pdf!=0)[length(which(mean_gens_pdf!=0))],
                           max_dist=max_gens*qpois(.999,mut_rate)) {
  
  # check that we have used a sensible cutoff
  # the mutation rate should be high enough such that the cutoff used is less than the max possible distance
  if (max_dist<max(cutoff+1)){warning("Nonsensical cutoff given distances considered")}
  
  # get linked and unlinked probability distributions
  gendist <- gen_dists(mut_rate = mut_rate, mean_gens_pdf = mean_gens_pdf,
                       max_link_gens = max_link_gens, max_gens = max_gens, max_dist = max_dist)
  
  linked_pdf <- gendist[,"linked_prob"]
  unlinked_pdf <- gendist[,"unlinked_prob"]
  
  linked_cdf <- cumsum(linked_pdf)/sum(linked_pdf)
  unlinked_cdf <- cumsum(unlinked_pdf)/sum(unlinked_pdf)
  
  # wrapping in function to allow for multiple cutoffs
  get_sens_spec <- function(cutoff) {
    
    # remember that cdf[cutoff] represents the probability of cutoff-1
    # but this is what we want because the threshold is <cutoff (not <=)
    
    # decrease specificity by probability mass of unlinked
    spec <- 1 - unlinked_cdf[cutoff]
    
    # decrease sensitivity by probability mass of linked
    sens <- 1 - (1 - linked_cdf[cutoff])
    
    return(c(sens,spec))
    
  }
  
  rc <- t(sapply(cutoff, get_sens_spec))
  rc <- cbind(cutoff, rc)
  colnames(rc) <- c("cutoff","sensitivity", "specificity")
  
  return(rc)
}


##' Wrapper function to turn output from sens_spec_calc to values to make an ROC curve
##'
##' @param cutoff the maximum genetic distance at which to consider cases linked
##' @param mut_rate mean number of mutations per generation, assumed to be poisson distributed
##' @param mean_gens_pdf the density distribution of the mean number of generations between cases;
##'       the index of this vector is assumed to be the discrete distance between cases
##' @param max_link_gens the maximium generations of separation for linked pairs
##' @param max_gens the maximum number of generations to consider, defaults to the highest
##'        number of generations in mean_gens_pdf with a non-zero probability
##' @param max_dist the maximum distance to calculate, defaults to max_gens * 99.9th percentile
##'       of mut_rate poisson distribution
##'
##' @return data frame with cutoff, sensitivity, and 1-specificity
##' 
##' @author Shirlee Wohl and Justin Lessler
##'
##' @example R/examples/sens_spec_roc.R
##'
##' @family mutrate_functions
##'
##' @export
##'

sens_spec_roc <- function(cutoff, mut_rate, mean_gens_pdf, max_link_gens=1,
                      max_gens=which(mean_gens_pdf!=0)[length(which(mean_gens_pdf!=0))],
                      max_dist=max_gens*qpois(.999,mut_rate)){
  
  rc <- sens_spec_calc(cutoff,mut_rate,mean_gens_pdf,max_link_gens,max_gens,max_dist)
  
  # turn this into a data frame that can be used for plotting ROC curves
  rc <- as.data.frame(rc)
  
  # calculate 1-specificity for plotting
  rc$specificity <- 1-rc$specificity
  
  # add the starting and ending points to make the complete curve
  rc <- rbind(c(-1,0,0),rc,c(Inf,1,1))
  
  return(rc)
  
}