#' ---
#' title: "IV or Regression"
#' geometry: paperheight=5in,paperwidth=8in,margin=0.5in
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
#' -->
#' \newcommand{\Var}{\mathrm{Var}}
#' \newcommand{\E}{\mathrm{E}}
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
#' # Introduction
#' 
#' Purpose: to illustrate the relative power resulting from 
#' performing causal inference by using
#' a two-stage least squares with an instrumental variable
#' versus using a regression controlling on the confounder.
#'
#- setup, include=FALSE 
knitr::opts_chunk$set(comment='     ')
#'
library(causalsim)
#'
#' We create a simple causal model for the effect of X on Y
#' with a single confounder, Z, and a single instrumental
#' variable, I. In practice, instrumental variables
#' are used when Z is unknown or not fully measurable. 
#' 
#' We refer to I as a 'perfect instrument' if X is fully 
#' determined (with no additional error) by I and Z. 
#' The strength of confounding is the 
#' squared correlation
#' between
#' Z and X.   
#' 
#' If I is a perfect instrument then $Cov(Z,I)=0$ and
#' $$\rho^2_{XI} + \rho^2_{XZ} = 1$$.
#' 
#' Since  $\rho^2_{XI}$ represent the 'strength' of the 
#' instrument, it can be seen that a perfect instrument
#' achieves maximal strength as a function of the 
#' strength of confounding:
#' 
#' > The stronger the confounding, the weaker the maximally strong
#' > instrument.
#' 
#' Here's the causal model defined through a linear DAG by
#' specifying path coefficients. The diagonal elements represent
#' the independent SD's of the each term.
#' 
ivdag <- function(XZcor2 = 0.5, XIcor2 = 0.5, YZ = 1, YX = 1, eps = .1,
                  label = 'default')  {
  # XZcor2 + XIcor2 must be <= 1
  #   1 - (XZcor2 + XIcor2) is the degree to which X
  #   is not determined by Z and I and reflects
  #   sub-optimality of the instrument for a
  #   given degree of assignment confounding 
  # The causal coefficient is set at 1
  # The degree of response confounding is reflected by YZ
  # 'eps' is the SD of epsilon
  # 
  dag <- matrix(0, 4, 4, FALSE, list(c('Z','I','X','Y'),c('Z','I','X','Y')))
  dag['Z','Z'] <- 1 # SD of Z
  dag['I','I'] <- 1 # SD of I
  dag['X','Z'] <- sqrt(XZcor2)
  dag['X','I'] <- sqrt(XIcor2)
  dag['Y','Z'] <- YZ
  dag['Y','X'] <- YX
  dag['Y','Y'] <- eps
  dag['X','X'] <- sqrt(1 - XZcor2 - XIcor2)
  class(dag) <- 'dagmat'
  attr(dag, 'label') <- label
  dag
} 

#'
#' # Moderate confounding with perfect instrument
#'

ivdag() %>% to_dag %>% round(3)
z <- ivdag()
  library(causalsim)
library(spida2)
library(latticeExtra)
library(latex2exp)
library(p3d)
to_dag(z) %>% attributes

ivdag(.499) %>% 
  {
    list(
      coefxiv(Y ~ X, .), 
      coefxiv(Y ~ X + Z, .), 
      coefxiv(Y ~ X + I, .),
      coefxiv(Y ~ X + Z + I, .) ,
      coefxiv(Y ~ X, ., iv = ~ I) 
      
    )
  } %>% 
  lapply(as.data.frame.coefx) %>% 
  do.call(rbind, .) %>% 
  subset(var == 'X') %>% 
  within(
    {
      nfactor <- 100 * (sd_factor / sd_factor[fmla == 'Y ~ X + Z'])^2 
    }
  ) %>% {
    df <- .
  xyplot(nfactor ~ beta, df, 
         sub = "Moderate confouding -- 'perfect' instrument",
         scales = list(y = list(log = 10, equispaced.log = FALSE)),
         xlim = c(.8, 3.2), 
         pch = 16,
         xlab = TeX('$E(\\hat{\\beta})$'),
       #  ylab = TeX('$SE(\\hat{\\beta}_x)$ factor'),
         ylab = "n for equivalent power", 
        labs = sub('Y ','  ',df$fmla)) +
  layer(panel.text(..., labels = labs, adj = 0)) +
  layer(panel.abline(v = 1, lty = 3))
}

#'
#' # Moderate confounding with perfect instrument
#'

ivdag(XZ <- .799, XI =.2, label = 'high_full'  ) %>% 
  {
    list(
      coefxiv(Y ~ X, .), 
      coefxiv(Y ~ X + Z, .), 
      coefxiv(Y ~ X + I, .),
      coefxiv(Y ~ X + Z + I, .) ,
      coefxiv(Y ~ X, ., iv = ~ I) 
      
    )
  } %>% 
  lapply(as.data.frame.coefx) %>% 
  do.call(rbind, .) %>% 
  subset(var == 'X') -> df_high_full


ivdag(XZ <- .499, XI =.5, label = 'medium_full'  ) %>% 
  {
    list(
      coefxiv(Y ~ X, .), 
      coefxiv(Y ~ X + Z, .), 
      coefxiv(Y ~ X + I, .),
      coefxiv(Y ~ X + Z + I, .) ,
      coefxiv(Y ~ X, ., iv = ~ I) 
      
    )
  } %>% 
  lapply(as.data.frame.coefx) %>% 
  do.call(rbind, .) %>% 
  subset(var == 'X') -> df_med_full


ivdag(XZ <- .199, XI =.8, label = 'low_full'  ) %>% 
  {
    list(
      coefxiv(Y ~ X, .), 
      coefxiv(Y ~ X + Z, .), 
      coefxiv(Y ~ X + I, .),
      coefxiv(Y ~ X + Z + I, .) ,
      coefxiv(Y ~ X, ., iv = ~ I) 
      
    )
  } %>% 
  lapply(as.data.frame.coefx) %>% 
  do.call(rbind, .) %>% 
  subset(var == 'X') -> df_low_full


ivdag(XZ <- .8*.799, XI =.8*.2, label = 'high_partial'  ) %>% 
  {
    list(
      coefxiv(Y ~ X, .), 
      coefxiv(Y ~ X + Z, .), 
      coefxiv(Y ~ X + I, .),
      coefxiv(Y ~ X + Z + I, .) ,
      coefxiv(Y ~ X, ., iv = ~ I) 
      
    )
  } %>% 
  lapply(as.data.frame.coefx) %>% 
  do.call(rbind, .) %>% 
  subset(var == 'X') -> df_high_partial


ivdag(XZ <- .8*.499, XI =.8*.5, label = 'medium_partial'  ) %>% 
  {
    list(
      coefxiv(Y ~ X, .), 
      coefxiv(Y ~ X + Z, .), 
      coefxiv(Y ~ X + I, .),
      coefxiv(Y ~ X + Z + I, .) ,
      coefxiv(Y ~ X, ., iv = ~ I) 
      
    )
  } %>% 
  lapply(as.data.frame.coefx) %>% 
  do.call(rbind, .) %>% 
  subset(var == 'X') -> df_med_partial


ivdag(XZ <- .8*.199, XI =.8*.8, label = 'low_partial'  ) %>% 
  {
    list(
      coefxiv(Y ~ X, .), 
      coefxiv(Y ~ X + Z, .), 
      coefxiv(Y ~ X + I, .),
      coefxiv(Y ~ X + Z + I, .) ,
      coefxiv(Y ~ X, ., iv = ~ I) 
      
    )
  } %>% 
  lapply(as.data.frame.coefx) %>% 
  do.call(rbind, .) %>% 
  subset(var == 'X') -> df_low_partial

list(df_high_full, df_med_full, df_low_full, df_high_partial, df_med_partial, df_low_partial) %>% 
lapply(print)  

list(df_high_full, df_med_full, df_low_full, df_high_partial, df_med_partial, df_low_partial) %>% 
  do.call(rbind,.) %>% #print
  within(
    {
      nfactor <- 100 * (sd_factor / sd_factor[dag == 'medium_full' & fmla == 'Y ~ X + Z'])^2 
      confounding <- factor(sub('_.*$', '', dag), levels = c('low','medium','high'))
      instrument <- factor(sub('^.*_', '', dag), levels = c('partial','full'))
    }
  )  %>% 
  {
    df <- .
    xyplot(nfactor ~ beta| confounding*instrument, df, 
           sub = "Bias and Power: Confounding and Quality of Instrument",
           scales = list(
             y = list(log = 10, equispaced.log = FALSE),
             x = list(at=1:5)),
           xlim = c(.8, 4.3), 
           layout = c(3,2),
           pch = 16,
           cex = .7,
           font = 2,
           xlab = TeX('$E(\\hat{\\beta})$: causal effect = 1.0'),
           #  ylab = TeX('$SE(\\hat{\\beta}_x)$ factor'),
           ylab = "n for equivalent power", 
           labs = sub('Y ','  ',df$fmla)) +
      layer(panel.text(..., labels = labs, adj = 0)) +
      layer(panel.abline(v = 1, lty = 3))
    #Plot3d(beta ~ sd_e + sd_avp, df)
    
  } %>% useOuterStrips

#'
##' # Test using simulation ----
#'
#'
#'
ivdag(.499) -> meddag
df <- sim(meddag, 10000)
dim(df)
head(df)
coefxiv(Y ~ X, meddag) %>% as.data.frame.coefx
lm(Y~X, df) %>% summary
coefxiv(Y ~ X + Z, meddag) %>% as.data.frame.coefx
lm(Y~X+Z, df) %>% summary

HERE  <- function() {}

# pparallel prog.  
ff <- function(i) solve(matrix(rnorm(100),10,10))
system.time(mclapply(1:1000, ff))


ivdag() %>% coefxiv(Y ~ X, ., iv = ~ I) %>% as.data.frame

to_dag


nams <- c('z','zx','zy','cov','x','y','m','i', 'xd', 'yd', 'zd', 'col')
mat <- matrix(0, length(nams), length(nams))
rownames(mat) <- nams
colnames(mat) <- nams

# confounding back-door path
mat['zx','z'] <- 3 
mat['zy','z'] <- 3
mat['x','zx'] <- 1
mat['y','zy'] <- 2

# direct effect
mat['y','x'] <- 3

# indirect effect
mat['m','x'] <- 1
mat['y','m'] <- 1

# Instrumental variable 
mat['x','i'] <- 2

# 'Covariate'
mat['y','cov'] <- 2

# descendant of X
mat['xd','x'] <- 1

# descendant of Y
mat['yd','y'] <- 1

# descendant of z -- imperfect control
mat['zd','z'] <- 2

# collider
mat['col','y'] <- 1
mat['col','x'] <- 1

 
# independent error
diag(mat) <- 1

mat # not in lower diagonal form   
dag <- to_dag(mat) # can be permuted to lower-diagonal form
dag # this allows us to iteratively work out the covariance matrix
# 
# coefx(y ~ x, dag)  # with confounding
# coefx(y ~ x + z, dag)  # blocking back-door path
# coefx(y ~ x + zy, dag) # blocking with lower SE
# coefx(y ~ x + zx, dag) # blocking with worse SE
# coefx(y ~ x + zy + cov, dag)  # adding a 'covariate'
# coefx(y ~ x + zy + m, dag)  # including a mediator
# coefx(y ~ x + zy + xd, dag)  # including a descendant
# coefx(y ~ x + zx + i, dag)  # including an instrument
# coefx(y ~ x + zx + i + cov, dag) # I and C
# 
# # plotting added-variable plot ellipse 
# lines(
#   coefx(y ~ x + zy, mat),  
#   lwd = 2, xv= 5,xlim = c(-5,10), ylim = c(-25, 50))
# lines(
#   coefx(y ~ x + zx, mat), new = FALSE,
#   col = 'red', xv = 5, lwd = 2)
# lines(
#   coefx(y ~ x + i, mat), new = FALSE,
#   col = 'dark green', xv = 5)

# putting results in a data frame
# for easier comparison of SEs

fmlas <- list(
  y ~ x,             # with confounding
  y ~ x + z,         # unconfounded
  y ~ x + zy,        # unconfounded using generating model
  y ~ x + zx,        # uncoufounded using assignment model
  y ~ x + zx + zy,   # 'doubly robust'
  y ~ x + zy + cov,  # adding a 'covariate' unrelated to x
  y ~ x + zy + m,    # adding a 'mediator'
  y ~ x + zy + xd,   # adding a 'descendant of X'
  y ~ x + zy + yd,   # adding a 'descendant of Y'
  y ~ x + zy + col,  # adding a 'collider'
  y ~ x + zx + i,    # adding an instrumental variable
  y ~ x + zx + i + cov,  # adding an instrumental variable and a covariate
  y ~ x + i,         # using an instrumental variable as a control
  y ~ x + zd,        # imperfect control for counfounding
  y ~ x + zd + i     # bias amplification
)
res <- lapply(fmlas, coefx, dag)
res <- lapply(res, function(ll) {
  ll$fmla <- paste(as.character(ll$fmla)[c(2,1,3)], collapse = ' ')
  ll$beta <- ll$beta[1]
  ll
})

df <- do.call(rbind.data.frame, res)
isnum <- sapply(df, is.numeric)
df[,isnum] <- round(df[,isnum], 2)

df[, c(5,1,4,2,3)]
library(latticeExtra)
library(latex2exp)
#'
#' \clearpage
#' 
#' 
#- fig.height=7
xyplot(sd_betax_factor ~ beta, df, 
       # scales = list(y = list(log = 10)),
       xlim = c(-.5, 8), 
       pch = 16,
       xlab = TeX('$E(\\hat{\\beta})$'),
       ylab = TeX('$SE(\\hat{\\beta}_x)$ factor'),
       labs = sub('y ~ ','  ',as.character(df$fmla))) +
  layer(panel.text(..., labels = labs, adj = 0)) +
  layer(panel.abline(v = 4, lty = 3))

#'
#' \clearpage
#' 
#' ![](DAG.png)
#'
#' # 
#- include = FALSE
knitr::knit_exit()
#-
#' Testing a hypothesis about variances of 

vimat <- function(a, b, c, d, f, se) {
  v <- 
    rbind(c(1, 0, a, c),
          c(0, 1, b, d),
          c(a, b, 1, f),
          c(c, d, f, 0))
  dimnames(v) <- list(c('I','Z','X','Y'),c('I','Z','X','Y'))
  v[4,4] <- se^2 + v[4,1:3,drop=FALSE] %*% solve(v[1:3,1:3]) %*% v[1:3,4,drop = FALSE]
v
  }
vimat(.5, .5, .5 * .1, .1, .1, 2) %>% coefx(Y~ X + I, var = .) %>% {.$beta}
vimat(.5, .5, .5 * .1, .1, .1, 2) %>% coefx(Y~ X + I, var = .) %>% {.$beta}

vimat(.5, .5, .5 * .1, .1, .1, 2) %>% coefx(Y~ X + Z, var = .)%>% {.$beta}
vimat(.5, .5, .5 * .1, .1, .1, 2) %>% coefx(Y~ X + Z + I, var = .)%>% {.$beta}
vimat(.5, .5, .5 * .1, .1, .1, 2) %>% coefx(Y~ X + I + Z, var = .)%>% {.$beta}
#' coefficient of X in last two models should be the same but it's not!!!!
vi <- rbind(
  I = c(1, 0, .5, .15),
  Z = c(0, 1, .)
)

#'
#' Simplest model with I and Z
#'

coefxiv <- function(fmla, dag, iv = NULL, var = covld(to_dag(dag))){
  dagnam <- deparse1(substitute(dag))
  v <- var
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
  if(is.null(iv)) sd_x_avp <- sqrt(1/diag(solve(v[-1,-1])))
  else sd_x_avp <- v[2,3]/sqrt(v[3,3])   # Fox, p. 233, eqn 9.29
  
  if(is.null(iv)) label <- 
    paste(as.character(fmla)[c(2,1,3)], collapse = ' ')
  else label <- paste0(ynam, ' ~ ', xnam, ' (IV = ', ivnam,')')
  ret <- list(dag = attr(dag, 'label'), vnames = xnam, beta=beta, sd_e =sd_e, sd_x_avp = sd_x_avp,
              sd_betax_factor = sd_e/sd_x_avp, fmla = fmla, 
              iv = if(is.null(iv)) '' else iv,
              fmla_label = label,
              dag_name = dagnam,
              dag = if(!missing(dag)) dag else '',
              var = v)
  class(ret) <- c('coefx')
  ret
}

as.data.frame.coefx <- function(x, ...) {
  with(x, data.frame(dag = dag, var = vnames, beta = beta, sd_e = sd_e, sd_avp = sd_x_avp,
                     sd_factor = sd_betax_factor, fmla = fmla_label))
}

m4 <- matrix(0, 4,4)
dimnames(m4) <- list(c('I' ,'Z','X','Y'))[c(1,1)]
m4
#' simple 
makeivdag <- function(confound = .5, eps_sd = .1, X_sd = 0)
{
  dags <- matrix(0, 4,4)
  dimnames(dags) <- list(c('I' ,'Z','X','Y'))[c(1,1)]
  dags['I','I'] <- 1
  dags['Z','Z'] <- 1
  dags['X','I'] <- sqrt(1-confound)
  dags['X','Z'] <- sqrt(confound)
  dags['Y','Z'] <- sqrt(confound)
  dags['Y','X'] <- sqrt(1-confound)
  dags['Y','Y'] <- eps_sd
  dags['X','X'] <- X_sd
  class(dags) <- 'dagmat'
  dags
}
print.dagmat <- function(x,..., zero = '-') {
  xp <- unclass(x)
  xp[] <- format(x)
  len <- nchar(xp[1])
  lead <- len%/%2
  # zero <- paste0(rep(' ',lead), zero, rep(' ', len - lead - 1),collapse='')
  zero <- paste0(rep(c(' ', zero, ' '), c(lead, 1, len - lead - 1)), collapse = '')
  xp[x==0] <- zero
  invisible(print(xp, quote = F))
  invisible(x)
}
print.dag <- print.dagmat
#'
#' # Comparing IV and regression with different strengths of confounding
#'

  lowC_perfect_IV <- makeivdag(confound = .1, X_sd = 0)
  medC_perfect_IV <- makeivdag(confound = .5, X_sd = 0)
  highC_perfect_IV <- makeivdag(confound = .9, X_sd = 0)
  
  lowC_good_IV <- makeivdag(confound = .1, X_sd = .1)
  medC_good_IV <- makeivdag(confound = .5, X_sd = .1)
  highC_good_IV <- makeivdag(confound = .9, X_sd = .1)

list(
  coefxiv(Y ~ X, lowC_perfect_IV),
  coefxiv(Y ~ X + Z, lowC_perfect_IV),
  coefxiv(Y ~ X, lowC_perfect_IV, iv = ~ I),
  
  coefxiv(Y ~ X, medC_perfect_IV),
  coefxiv(Y ~ X + Z, medC_perfect_IV),
  coefxiv(Y ~ X, medC_perfect_IV, iv = ~ I),
  
  coefxiv(Y ~ X, highC_perfect_IV),
  coefxiv(Y ~ X + Z, highC_perfect_IV),
  coefxiv(Y ~ X, highC_perfect_IV, iv = ~ I),
  
  coefxiv(Y ~ X, lowC_good_IV),
  coefxiv(Y ~ X + Z, lowC_good_IV),
  coefxiv(Y ~ X, lowC_good_IV, iv = ~ I),
  
  coefxiv(Y ~ X, medC_good_IV),
  coefxiv(Y ~ X + Z, medC_good_IV),
  coefxiv(Y ~ X, medC_good_IV, iv = ~ I),
  
  coefxiv(Y ~ X, highC_good_IV),
  coefxiv(Y ~ X + Z, highC_good_IV),
  coefxiv(Y ~ X, highC_good_IV, iv = ~ I)
) %>% 
  lapply(as.data.frame.coefx)  %>% 
  do.call(rbind, .) -> fits

fits %>% 
  subset(var == 'X') %>% 
  xyplot(sd_factor ~ beta, .)
#-
knitr::knit_exit()
#-

str(lls)
names(lls)
# coefx
#   lapply(function(dag) {
#     lapply(
#       list(
#         coefxiv(Y ~ X, dags),
#         coefxiv(Y ~ X + Z, dags),
#         coefxiv(Y ~ X, dags, iv = ~ I)),
#       function(mod,)
#   }
#     lapply( 
#     ))
# )



dags
to_dag(dags)


library(causalsim)
covld(dags)
list(
coefxiv(Y ~ X, dags) %>% as.data.frame,
coefxiv(Y ~ X + Z, dags) %>% as.data.frame,
coefxiv(Y ~ X, dags, iv = ~ I) %>% as.data.frame) %>% 
  do.call(rbind, .)


coefxiv(Y ~ X + Z + I, dags) %>% as.data.frame
coefxiv(Y ~ X + I, dags)
list(
   coefxiv(Y ~ X, dags), 
   coefxiv(Y ~ X, dags, iv = ~ I), 
   coefxiv(Y ~ X + Z, dags))%>% 
  lapply(as.data.frame) %>% 
  do.call(rbind, .)

head(z)
svd(covld(dags))
trellis.focus()
panel.identify(labels = as.character(df$fmla))
# simulation

head(sim(dag, 100))
var(sim(dag, 10000)) - covld(dag)

# plotting

plot(dag)+theme_dag_blank()

pdag <-
function(dag, ...) {
  makedagitty <- function(mat) {
    string <- ''
    df <- as.data.frame(as.table(dag))
    df <- subset(df, Var1 != Var2)
    df <- subset(df, Freq > 0)
    for(i in seq_len(nrow(df))) {
      string <- paste(string,' ',df[i,'Var1'],' <- ', df[i,'Var2'] )
    }
    string <- paste('dag{', string, '}')
    dagitty::dagitty(string)
  }
  ggdag::ggdag_classic(makedagitty(dag), ...)
}

library(maptools)
n <- 50
x <- rnorm(n)*10
y <- rnorm(n)*10
plot(x, y, col = "red", pch = 20)
pointLabel(x, y, as.character(round(x,5)), offset = 0, cex = .7)

plot(x, y, col = "red", pch = 20)
zz <- pointLabel(x, y,  offset = 0, cex = .8, doPlot=F)
pointLabel

panel.text
ltext
methods(ltext)
lattice:::ltext.default

