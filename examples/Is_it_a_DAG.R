#' ---
#' title: "Is it a DAG?"
#' geometry: paperheight=5in,paperwidth=8in,margin=0.5in # for slides on a 16:9 monitor
#' author: ''
#' output: 
#' # bookdown::tufte_book2
#'   bookdown::tufte_handout2
#' # bookdown::pdf_document2
#' # header-includes:
#' #    - \usepackage{setspace}
#' #    - \usepackage{pdfpages}
#' #    - \usepackage{fancyhdr}
#' #    - \pagestyle{fancy} 
#' #    - \raggedright
#' #    - \usepackage{booktabs}
#' #    - \usepackage{multirow}
#' #    - \usepackage{setspace}
#' #    - \usepackage{subcaption}
#' #    - \usepackage{caption}
#' #    - \usepackage{tikz}
#' #    - \usepackage{float}
#' #    - \usepackage{setspace}
#' ---
#' <!--
#' \AddToShipoutPicture{%
#'   \AtTextCenter{%
#'     \makebox[0pt]{%
#'       \scalebox{6}{%
#'         \rotatebox[origin=c]{60}{%
#'           \color[gray]{.93}\normalfont CONFIDENTIAL}}}}}
#' \newcommand{\Var}{\mathrm{Var}}
#' \newcommand{\E}{\mathrm{E}}
#' -->
#+ include=FALSE
# https://bookdown.org/yihui/rmarkdown/tufte-figures.html
# fig.fullwidth = TRUE, with fig.width and fig.height, out.width
# https://www.statmethods.net/RiA/lattice.pdf
# 
#+ setup, include=FALSE 
knitr::opts_chunk$set(
  fig.width=7, 
  fig.height=4, 
  fig.fullwidth=TRUE,
#  echo=FALSE,
#  include=FALSE,
  echo=TRUE,
  include=TRUE,
  comment = '  '
)
#'
#' Comparing different algorithms to find a permutation that, applied
#' to both rows and columns of a square matrix with zeros on the diagonal,
#' results in a lower triangular matrix.
#' 
#' # John's function
#' 
isLowerTriangular <- function(X){
  all(0 == X[upper.tri(X)])
}

triangularize <- function(X){
  if (is.null(rownames(X))) rownames(X) <- 1:nrow(X)
  if (isLowerTriangular(X)) return(rownames(X))
  tri <- function(X){ # recursion helper function
    if (length(X) == 0) return()
    diag(X) <- 0
    roots <- which(rowSums(X) == 0)
    c(rownames(X)[roots], tri(X[-roots, -roots, drop=FALSE]))
  }
  perm <- tri(X)
  if (length(perm) != nrow(X)) stop("matrix cannot be triangularized")
  perm
}
#' 
#' # Georges' function
#' 
to_dag <- function(mat){
  # check that mat has unique column and row names
  find_leaves <- function(m) {   # wasteful because we only need for find one
                                 # 'orphan', not all possible 'orphans'
    sumabs <- function(x) sum(abs(x))
    diag(m) <- 0 # in case they're used for epsilons
    which(apply(m,2,sumabs)==0)
  }
  if(nrow(mat) != ncol(mat)) stop('matrix must be square')
  if(any(sort(colnames(mat)) != sort(rownames(mat)))) stop('colnames not same as rownames')
  if(length(unique(colnames(mat))) != length(colnames(mat))) stop('names not unique')
  mat <- mat[colnames(mat), colnames(mat)]
  if(sum(abs(mat[row(mat) < col(mat)])) == 0) {
    # matrix already lower diagonal  
    class(mat) <- unique(c('dag', class(mat)))
    return(mat)
  }
  ret <- mat
  dag_perm <- rep('', nrow(mat))
  for(i in 1:nrow(ret)) {
    ll <- find_leaves(mat)
    if(length(ll) == 0) return(FALSE)
    dag_perm[i] <- names(ll[1])
    if(i < nrow(ret)) mat <- mat[-ll[1],-ll[1], drop = FALSE]
  }
  ret <- ret[rev(dag_perm),rev(dag_perm)]
  class(ret) <- unique(c('dag', class(ret)))
  ret
}
#'
#' 
#' # Georges' function #2
#' 
#' This function exploits in-place replacement of elements
#' of a matrix and borrows form John's practice of avoiding
#' passing arguments to a function defined within a function
#' 
to_dag <- function(mat){
  find_leaves <- function(m) {   # wasteful because we only need for find one
    # 'orphan', not all possible 'orphans'
    sumabs <- function(x) sum(abs(x))
    diag(m) <- 0 # in case they're used for epsilons
    which(apply(m,2,sumabs)==0)
  }
  if(nrow(mat) != ncol(mat)) stop('matrix must be square')
  if(any(sort(colnames(mat)) != sort(rownames(mat)))) stop('colnames not same as rownames')
  if(length(unique(colnames(mat))) != length(colnames(mat))) stop('names not unique')
  mat <- mat[colnames(mat), colnames(mat)]
  if(sum(abs(mat[row(mat) < col(mat)])) == 0) {
    # matrix already lower diagonal  
    class(mat) <- unique(c('dag', class(mat)))
    return(mat)
  }
  ret <- mat
  diag(mat) <- 0
  dag_perm <- rep('', nrow(mat))
  for(i in 1:nrow(ret)) {
    
## CONTINUE %>% HEERE TO %>% IMPROVE %>% FUNCTION %>% SO %>% IT ------
## MAKES REFERENCES IN PLACE ------    
    
    ll <- find_leaves(mat)
    if(length(ll) == 0) return(FALSE)
    dag_perm[i] <- names(ll[1])
    if(i < nrow(ret)) mat <- mat[-ll[1],-ll[1], drop = FALSE]
  }
  ret <- ret[rev(dag_perm),rev(dag_perm)]
  class(ret) <- unique(c('dag', class(ret)))
  ret
}
#'
#'
#' # Random triangularizable matrix
#' 
rdag <- function(size) {
  x <- matrix(0,size,size)
  x[lower.tri(x)] <- rnorm(size*(size-1)/2)
  perm <- sample(size)
  x[perm, perm]
}
#'
#' # Timing tests
#'
testn <- function(fun, size, reps = 1000) {
  system.time(
    lapply(seq_len(reps), function(i) fun(rdag(size)))
  )
}

testn(triangularize, 3)
testn(to_dag, 3)

testn(triangularize, 5)
testn(to_dag, 5)

testn(triangularize, 20)
testn(to_dag, 20)

testn(triangularize, 100)
testn(to_dag, 100)
