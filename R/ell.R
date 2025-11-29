#' Generate a 2D ellipse with a given a center and a variance matrix.
#'
#' The ellipse is the contour of the bivariate normal distribution
#' with variance 'shape'.
#'
#' @param center (default: c(0,0)) 
#' @param shape variance of bivariate normal. Default: 2 x 2 identity
#' @param radius of ellipse, equivalently: square root of deviance contour
#' @param n number of points on the ellipse
#' @export
ell <-
  function (center = rep(0, 2), 
            shape = diag(2), 
            radius = 1, 
            n = 100) 
  {
    fac <- function(x) {
      xx <- svd(x, nu = 0)
      t(xx$v) * sqrt(pmax(xx$d, 0))
    }

    angles = (0:n) * 2 * pi/n
    if (length(radius) > 1) {
      ret <- lapply(radius, function(r) rbind(r * cbind(cos(angles), 
                                                        sin(angles)), NA))
      circle <- do.call(rbind, ret)
    }
    else circle = radius * cbind(cos(angles), sin(angles))
    ret <- t(c(center) + t(circle %*% fac(shape)))
    attr(ret, "parms") <- list(center = rbind(center), 
                               shape = shape, radius = radius)
    class(ret) <- "ell"
    ret
  }
