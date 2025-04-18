

#' @title \link[stats]{ks.test} on \link[fitdistrplus]{fitdist}
#' 
#' @param x a \link[fitdistrplus]{fitdist} object
#' 
#' @param y \link[base]{character} scalar, the cumulative distribution function
#' 
#' @param data \link[base]{numeric} \link[base]{vector}
#' 
#' @param ... parameters of the cumulative distribution. Defaults are 
#' the estimate from the \link[fitdistrplus]{fitdist} object
#' 
#' @examples
#' library(fitdistrplus)
#' data(groundbeef, package = 'fitdistrplus')
#' groundbeef$serving |> 
#'  fitdist(distr = 'gamma') |>
#'  ks.test() |> 
#'  suppressWarnings()
#' @method ks.test fitdist
#' @importFrom stats ks.test
#' @export ks.test.fitdist
#' @export
ks.test.fitdist <- function(
    x,
    y = paste0('p', x$distname),
    data = x$data, 
    ...
) {
  
  if (!length(data)) stop('Re-run ?fitdistrplus::fitdist with option `keepdata = TRUE`')
  
  ag <- list(...)
  est <- x$estimate |> as.list.default()
  nm <- setdiff(x = names(est), y = names(ag))
  if (length(nm)) ag[nm] <- est[nm]
  
  # ?stats:::ks.test.default
  do.call(what = ks.test, args = c(list(x = quote(data), y = y), ag))
  
}

