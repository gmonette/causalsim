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
#- setup, include=FALSE 
knitr::opts_chunk$set(comment='     ')
library(causalsim)
#'
#' ![](dag_full.png)
#'
nams <- c('z','zx','zy','cov','x','y','m','i', 'xd', 'yd', 'zd', 'col')
mat <- matrix(0, length(nams), length(nams))
rownames(mat) <- nams
colnames(mat) <- nams

# confounding back-door path
mat['zx','z'] <- 3 
mat['zy','z'] <- 3
mat['x','zx'] <- 1
mat['y','zy'] <- 2

# direct effect of x on y
mat['y','x'] <- 3

# indirect effect: Note that the causal effect is 3 + 1x1 = 4
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

 
# independent SD of error for every variable
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
  y ~ x + zx,        # unconfounded using assignment model
  y ~ x + zx + zy,   # 'doubly robust'
  y ~ x + zy + cov,  # adding a 'covariate' unrelated to x
  y ~ x + zy + m,    # adding a 'mediator'
  y ~ x + zy + xd,   # adding a 'descendant of X'
  y ~ x + zy + yd,   # adding a 'descendant of Y'
  y ~ x + zy + col,  # adding a 'collider'
  y ~ x + zx + i,    # adding an instrumental variable
  y ~ x + zx + i + cov,  # adding an instrumental variable and a covariate
  y ~ x + i,         # using an instrumental variable as a control
  y ~ x + zd,        # imperfect control for confounding
  y ~ x + zd + i     # bias amplification
)
# TODO: 
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

