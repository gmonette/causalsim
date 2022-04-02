#' Multiple regression coefficient from a linear DAG
#' 
#' Given a linear DAG, find the population regression 
#' coefficents using data with the marginal covariance
#' structure implied by the linear DAG.
#' 
#' @param fmla a linear model formula. The variables
#'        in the formula must be column names of 
#'        \code{dag}.
#' @param dag a square matrix defining a linear DAG. The
#'        column names and row names of A must be 
#'        identical.  The non-diagonal entries of 
#'        \code{dag} contain the causal of coefficients
#'        of arrows pointing from the column variable
#'        to the row variable.  The diagonal entries
#'        are standard deviations of the normally
#'        distributed independent component generating
#'        the row variable. A matrix
#'        defines a linear dag if the same permutation
#'        of its rows and columns can transform it into
#'        a lower diagonal matrix.
#' @param var variance matrix of variables if entered directly
#'        without a dag.
#' @param iv a one-sided formula with a single variable (at present)
#'        specifying a variable to be used as an instrumental variable
#' @return a list with class 'coefx' containing 
#'        the population coefficient for the first
#'        predictor variable in \code{fmla}, the residual 
#'        standard error of the regression, the conditional
#'        standard deviation of the residual of the first
#'        predictor and the ratio of the last two quantities
#'        which constitutes the 'standard error factor' which,
#'        if multiplied by 1/sqrt(n) is an estimate of the
#'        standard error of the estimate of the regression
#'        coefficient for the first predictor variable.
#' @importFrom stats terms
#' @examples
#' library(dagitty)
#' nams <- c('zc','zl','zr','c','x','y','m','i')
#' mat <- matrix(0, length(nams), length(nams))
#' rownames(mat) <- nams
#' colnames(mat) <- nams
#' 
#' # confounding back-door path
#' mat['zl','zc'] <- 2 
#' mat['zr','zc'] <- 2
#' mat['x','zl'] <- 1
#' mat['y','zr'] <- 2
#' 
#' # direct effect
#' mat['y','x'] <- 3
#' 
#' # indirect effect
#' mat['m','x'] <- 1
#' mat['y','m'] <- 1
#' 
#' # Instrumental variable 
#' mat['x','i'] <- 2
#' 
#' # 'Covariate'
#' mat['y','c'] <- 1
#' 
#' # independent error
#' diag(mat) <- 2
#' 
#' mat # not in lower diagonal form   
#' dag <- to_dag(mat) # can be permuted to lower-diagonal form
#' dag
#' 
#' coefx(y ~ x, dag)  # with confounding
#' coefx(y ~ x + zc, dag)  # blocking back-door path
#' coefx(y ~ x + zr, dag) # blocking with lower SE
#' coefx(y ~ x + zl, dag) # blocking with worse SE
#' coefx(y ~ x + zr + c, dag)  # adding a 'covariate'
#' coefx(y ~ x + zr + m, dag)  # including a mediator
#' coefx(y ~ x + zl + i, dag)  # including an instrument
#' coefx(y ~ x + zl + i + c, dag) # I and C
#' 
#' # plotting added-variable plot ellipse 
#' lines(
#'     coefx(y ~ x + zr, mat),  
#'     lwd = 2, xv= 5,xlim = c(-5,10), ylim = c(-25, 50))
#' lines(
#'     coefx(y ~ x + zl, mat), new = FALSE,
#'     col = 'red', xv = 5, lwd = 2)
#' lines(
#'     coefx(y ~ x + i, mat), new = FALSE,
#'     col = 'dark green', xv = 5)
#' 
#' # putting results in a data frame
#' # for easier comparison of SEs
#' 
#' fmlas <- list(
#'   y ~ x, 
#'   y ~ x + zc, 
#'   y ~ x + zr, 
#'   y ~ x + zl,
#'   y ~ x + zr + c, 
#'   y ~ x + zr + m, 
#'   y ~ x + zl + i,
#'   y ~ x + zl + i + c
#' )
#' res <- lapply(fmlas, coefx, dag)
#' res <- lapply(res, function(ll) {
#'     ll$fmla <- paste(as.character(ll$fmla)[c(2,1,3)], collapse = ' ')
#'     ll$beta <- ll$beta[1]
#'     ll
#' })
#' 
#' df <- do.call(rbind.data.frame, res)
#' df 
#' 
#' # simulation
#' 
#' head(sim(dag, 100))
#' var(sim(dag, 10000)) - covld(dag)
#' 
#' # plotting
#'
#' plot(dag) + ggdag::theme_dag()
#' 
#' @export
coefx <- function(fmla, dag, var = covld(to_dag(dag)), iv = NULL){
  ynam <- as.character(fmla)[2]
  xnam <- labels(terms(fmla))
  if(any(grepl('\\|', xnam)) | !is.null(iv)) {
    return(coefxiv(fmla, var = var, iv = iv))
  }
  var <- var[c(ynam,xnam),c(ynam,xnam)]
  beta <- solve(var[-1,-1], var[1,-1])
  sd_e <- sqrt(var[1,1] - sum(var[1,-1]*beta))
  sd_x_avp <- sqrt(1/solve(var[-1,-1])[1,1])
  label <- paste(as.character(fmla)[c(2,1,3)], collapse = ' ')
  ret <- list(beta=beta, sd_e =sd_e, sd_x_avp = sd_x_avp,
              sd_betax_factor = sd_e/sd_x_avp, fmla = fmla, label = label)
  class(ret) <- 'coefx'
  ret
}
#' Regression coefficient from a linear DAG possibly using an IV
#' 
#' Given a linear DAG, find the population regression 
#' coefficient for X using IV estimation
#' using data with the marginal covariance
#' structure implied by the linear DAG.
#' 
#' @param fmla a linear model formula. The variables
#'        in the formula must be column names of 
#'        \code{dag}. For IV estimation, the right
#'        hand side should have two variables, the
#'        first one will be treated as the 'X'
#'        variable and the second, as the IV.
#' @param dag a square matrix defining a linear DAG. The
#'        column names and row names of A must be 
#'        identical.  The non-diagonal entries of 
#'        \code{dag} contain the causal of coefficients
#'        of arrows pointing from the column variable
#'        to the row variable.  The diagonal entries
#'        are standard deviations of the normally
#'        distributed independent component generating
#'        the row variable. A matrix
#'        defines a linear dag if the same permutation
#'        of its rows and columns can transform it into
#'        a lower diagonal matrix.
#' @param iv a one-sided formula with a single variable (at present)
#'        specifying a variable to be used as an instrumental variable
#' @param var the variance matrix of the variables, as an alternative
#'        input to 'dag'. If 'var' is provided, then dag does not
#'        need to be provided.
#' @return a list with class 'coefx' containing 
#'        the population coefficient for the first
#'        predictor variable in \code{fmla}, the residual 
#'        standard error of the regression, the conditional
#'        standard deviation of the residual of the first
#'        predictor and the ratio of the last two quantities
#'        which constitutes the 'standard error factor' which,
#'        if multiplied by 1/sqrt(n) is an estimate of the
#'        standard error of the estimate of the regression
#'        coefficient for the first predictor variable.
#' @importFrom stats as.formula
#' @export
coefxiv <- function(fmla, dag, iv = NULL, var = covld(to_dag(dag))){
  sepiv <- function(fmla) {
    # separate model formula and iv from formula
    cfmla <- as.character(fmla)
    fmla <- as.formula(paste(cfmla[2], ' ~ ', sub('\\|.*$','',cfmla[3])))
    iv <- if(grepl('\\|', cfmla[3])) as.formula(paste( ' ~ ', sub('^.*\\|','',cfmla[3]))) else NULL
    list(fmla, iv)
  }
  v <- var
  if(grepl('\\|',as.character(fmla)[3] )){
    fmiv <- sepiv(fmla)
    fmla <- fmiv[[1]]
    iv <- fmiv[[2]]
  }
  ynam <- as.character(fmla)[2]
  xnam <- labels(terms(fmla))
  if(!is.null(iv)) {
    if(length(xnam) > 1) warning('coefxiv currently supports only one X variable for IV analyses')
    xnam <- xnam[1]
  }
  if(!is.null(iv)) ivnam <- as.character(iv)[2]
  
  if(!is.null(iv)) v <- v[c(ynam,xnam,ivnam),c(ynam,xnam,ivnam)]
  else  v <- v[c(ynam,xnam),c(ynam,xnam)]
  if(is.null(iv)) beta <- solve(v[-1,-1], v[1,-1])
  else beta <- v[1,3]/v[2,3]
  if(is.null(iv)) sd_e <- sqrt(v[1,1] - sum(v[1,-1]*beta))
  else sd_e <- sqrt(
    v[1,1] + beta^2 * v[2,2] - 2 * beta * v[1,2]
  )
  if(is.null(iv)) sd_x_avp <- sqrt(1/solve(v[-1,-1])[1,1])
  else sd_x_avp <- v[2,3]/sqrt(v[3,3])   # Fox, p. 233, eqn 9.29
  
  if(is.null(iv)) label <- 
    paste(as.character(fmla)[c(2,1,3)], collapse = ' ')
  else label <- paste0(ynam, ' ~ ', xnam, ' (IV = ', ivnam,')')
  ret <- list(beta=beta, sd_e =sd_e, sd_x_avp = sd_x_avp,
              sd_betax_factor = sd_e/sd_x_avp, fmla = fmla, 
              iv = if(is.null(iv)) '' else iv,
              label = label,
              dag = if(!missing(dag)) dag else '',
              var = v)
  class(ret) <- c('coefx')
  ret
}

#' Convert a coefx object to a data frame
#' 
#' @param x   a coefx object
#' @param ... ignored
#' @export
as.data.frame.coefx <- function(x, ...) {
  with(x, data.frame(beta_x = beta[1], sd_e = sd_e, sd_x_avp = sd_x_avp,
                     sd_factor = sd_betax_factor, label = label))
}

#' 
#' Plotting the added-variable plot for linear DAG
#' 
#' See \code{\link{coefx}} for an extended example.
#' 
#' @param x an object of class 'coefx' produced by
#'        \code{\link{coefx}}.
#' @param new if FALSE add to previous plot. Default: TRUE.
#' @param xvertical position of vertical line to help visualize
#'        standard error factor. Default: 1.
#' @param col color of ellipse and lines. Default: 'black'.
#' @param lwd line width.
#' @param \dots other arguments passed to \code{\link{plot}}.
#' @importFrom graphics lines abline
#' @export
lines.coefx <- function(x, new = TRUE, xvertical = 1,  col = 'black', lwd = 2, ...) {
  cx <- x
  fmla <- as.character(cx$fmla)
  xlab <- sub(' .*$','',fmla[3])
  ylab <- fmla[2]
  mlab <- paste(fmla[1], fmla[3])
  Tr <- with(cx, matrix(c(0, sd_e, sd_x_avp, 
                          sd_x_avp * beta[1]), nrow = 2))
  if(new) plot(ell(shape = Tr %*% t(Tr)), 
               col = col, type = 'l', xlab = xlab, ylab = ylab, lwd = lwd, ...)
  else lines(ell(shape = Tr %*% t(Tr)), 
             col = col, lwd = lwd) 
  abline(a=0, b=cx$beta[1], col = col, lwd = lwd)
  with(cx, abline(a=0, b=beta[1] - sd_e/sd_x_avp, col = col, lwd = lwd))
  with(cx, abline(a=0, b=beta[1] + sd_e/sd_x_avp, col = col, lwd = lwd))
  abline(v = xvertical, lwd = lwd)
  with(cx, text(xvertical, xvertical *(beta[1]- sd_e/sd_x_avp), paste(' ',mlab) , 
                adj = 0, cex = 1, col = col, ...))
}

#' No idea why this is here; doesn't need to be exported
#' 
#' @rdname lines.coefx
lines_ <-function(x, new = FALSE, xvertical = 1, col = 'black', lwd = 2, ...) {
  lines(x, col = col, xvertical = xvertical , new = new, lwd = lwd, ... )
}
