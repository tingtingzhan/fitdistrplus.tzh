
# packageDate('fitdistrplus')
# On 2024-07-11
# authors added ?fitdistrplus:::AIC.fitdist; ?fitdistrplus:::BIC.fitdist
# I dont think their practice is as good as mine, though


#' @title Log-Likelihood of \link[fitdistrplus]{fitdist} Object
#' 
#' @description ..
#' 
#' @param object \link[fitdistrplus]{fitdist} object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details 
#' Output of \link[fitdistrplus]{fitdist} has elements `$loglik`, `$aic` and `$bic`, 
#' but they are simply \link[base]{numeric} scalars.
#' `fitdistrplus:::logLik.fitdist` simply returns these elements.
#' 
#' @returns 
#' Function [logLik.fitdist()] returns a \link[stats]{logLik} object, which 
#' could be further used by \link[stats]{AIC} and \link[stats]{BIC}.
#' 
#' (tzh have written to the authors.  long ago..)
#' 
#' @keywords internal
#' @importFrom stats logLik
#' @export logLik.fitdist
#' @export
logLik.fitdist <- function(object, ...) {
  ret <- object[['loglik']]
  attr(ret, which = 'nobs') <- object[['n']]
  attr(ret, which = 'df') <- length(object[['estimate']])
  class(ret) <- 'logLik'
  return(ret)
}


#' @title Number of Observations in \link[fitdistrplus]{fitdist} Object
#' 
#' @description ..
#' 
#' @param object \link[fitdistrplus]{fitdist} object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [nobs.fitdist()] returns an \link[base]{integer} scalar
#' 
#' @note
#' S3 method dispatch to generic \link[stats]{nobs} is needed in function `stats:::BIC.default()`.
#' 
#' @keywords internal
#' @importFrom stats nobs
#' @export nobs.fitdist
#' @export
nobs.fitdist <- function(object, ...) {
  object[['n']]
}


# ?fitdistrplus:::coef.fitdist # slow
