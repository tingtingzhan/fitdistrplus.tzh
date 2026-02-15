

#' @title Convert \link[fitdistrplus]{fitdist} Objects to \link[fmx]{fmx-class}
#' 
#' @description 
#' To convert \link[fitdistrplus]{fitdist} objects (from package \CRANpkg{fitdistrplus}) 
#' to \link[fmx]{fmx-class}.
#' 
#' @param x \link[fitdistrplus]{fitdist} object
#' 
#' @param ... ..
#' 
#' @returns 
#' Function [as.fmx.fitdist()] returns an \linkS4class{fmx} object.
#' 
#' @examples
#' library(fmx)
#' rnorm(1e2L) |> 
#'  fitdistrplus::fitdist(distr = 'norm') |>
#'  as.fmx()
#' @keywords internal
#' @importFrom fmx as.fmx
#' @importClassesFrom fmx fmx 
#' @importFrom methods new
#' @method as.fmx fitdist
#' @export as.fmx.fitdist
#' @export
as.fmx.fitdist <- function(x, ...) {
  
  if (!length(data <- x[['data']])) stop('Rerun ?fitdistrplus::fitdist with `keepdata = TRUE')
  
  pars <- x[['estimate']]
  dim(pars) <- c(1L, length(pars)) # becoming `matrix`
  
  distname <- x[['distname']]
  colnames(pars) <- switch(distname, 'norm' = {
    c('mean', 'sd')
  }) # else do nothing, for now
  
  new(Class = 'fmx', 
      pars = pars, 
      distname = distname,
      data = data, 
      vcov = if (length(x[['vcov']])) x[['vcov']] else array(dim = c(0, 0)))
}

