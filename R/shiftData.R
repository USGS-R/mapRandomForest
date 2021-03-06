#'Shift Data
#'
#'Returns a left- or right-shifted version of the input vector.
#'
#' This function is adapted from the USGS smwrBase package. It is
#' included here to enable the part function to work without
#' installing the dozens of external dependencies required by
#' the smwrBase package.
#'
#' @param x any vector.
#' @param k a positive or negative whole number of positions to shift the data.
#'Positive values shift data to the right; negative values shift
#'data to the left.
#' @param fill a scalar value like \code{x} used to fill in the first \code{k}
#'positions or the last -\code{k} positions if \code{circular}=\code{FALSE}. Ignored if
#'\code{circular}=\code{TRUE}. The default value is \code{NA}. If \code{x} is class
#'"factor," then \code{fill} must be \code{NA} or a valid level in \code{x}.
#' @param circular logical (\code{TRUE} or \code{FALSE}). If \code{TRUE}, then treat
#'\code{x} as a circular buffer, rotating values from the end into the beginning
#'if \code{k} is positive and vice versa if \code{k} is negative. If \code{FALSE},
#'then use the value of fill. The default value is \code{FALSE}.
#' @return A vector like \code{x}, with data shifted in position.
#' @seealso
#Flip for production/manual
#'\code{\link[stats]{lag}}
#\code{lag} (in stats package)
#' @keywords manip
#' @export
#' @examples
#'
#'shiftData(1:5, k=1)
#'# [1] NA  1  2  3  4
#'shiftData(1:5, k=1, circ=TRUE)
#'# [1] 5 1 2 3 4
shiftData <- function(x, k=1, fill=NA, circular=FALSE) {
  ## Coding history:
  ##    2005Mar18 DLLorenz Original
  ##    2007Dec19 DLLorenz Added fill argument
  ##    2008Feb11 DLLorenz Added circular argument
  ##    2011Jun02 DLLorenz Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb15 DLLorenz Prep for gitHub
  ##
  ## Offset a vector by an amount equal to k
  ## if k is positive the data are shifted down (fill at the beginning)
  ## otherwise, they are shifted up (fill at the end)
  ## unless circular is TRUE, then the data are treated as a circular
  ## buffer
  ##
  ## Required to paste NAs at the beginning, this logic works for most data types
  fill.temp <- fill
  fill <- x[1L]
  fill[1L] <- fill.temp
  # required for factors
  ckfact <- inherits(x, "factor")
  if(ckfact) { # preserve the info
    xlevs <- levels(x)
    xclass <- class(x)[1L] # ordered has 2
    x <- as.character(x)
    fill <- as.character(fill)
  }
  ## OK do it
  k <- as.integer(k)
  if(k == 0L) return(x)
  N <- length(x)
  if(k > 0L) {
    skip <- seq(k-1L, 0L) - N
    if(circular)
      x <- c(x[-skip], x[skip])
    else
      x <- c(rep(fill, k), x[skip])
  } else {
    skip <- seq(-1L, k)
    if(circular)
      x <- c(x[skip], x[-skip])
    else
      x <- c(x[skip], rep(fill, -k))
  }
  # restore factors if necessary
  if(ckfact) {
    if(xclass == "factor") {
      x <- factor(x, levels=xlevs)
    } else { # ordered
      x <- ordered(x, levels=xlevs)
    }
  }
  return(x)
}
