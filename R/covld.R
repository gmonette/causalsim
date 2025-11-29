#' Covariance matrix for a linear DAG
#' 
#' Computes the overall covariance matrix generated
#' by the coefficients of a linear DAG. 
#' See \code{\link{coefx}} for an extended example.
#' 
#' @param mat a matrix defining a linear DAG. See
#'        \code{\link{coefx}}.
#' @return the variance matrix implied by the 
#'         linear DAG.
#' @export
covld <- function(mat) {
  # mat is a lower triangular matrix representing 
  # causal coefficients along the arrows of a 
  # linear causal DAG with normal stochastic components.
  #
  # The diagonal elements show the standard deviation
  # of the unique normal errors added in generating
  # each variable.
  
  # check if strict lower diag
  # if(sum(abs(mat[col(mat)>row(mat)])) > 0) stop('matrix must be lower diagonal')
  mat <- to_dag(mat)
  # initialize returned v matrix
  v <- diag(nrow(mat))
  dimnames(v) <- dimnames(mat)
  v[1,1] <- mat[1,1]^2
  for(i in 2:ncol(v)) {
    A <- rbind(
      cbind(diag(i-1), 0),
      mat[i, seq_len(i)])
    v[seq_len(i),seq_len(i)] <- A %*% v[seq_len(i),seq_len(i)] %*% t(A)
  }
  v
}
