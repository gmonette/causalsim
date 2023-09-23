#' ---
#' title: "A Causal Zoo"
#' geometry: paperheight=10in,paperwidth=8in,margin=0.5in
#' author: ''
#' bibliography: bib.bib
#' output: 
#' # bookdown::tufte_book2
#' # bookdown::tufte_handout2
#'   bookdown::pdf_document2
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
#' \newcommand{\N}{\mathrm{N}}
#+ notes,include=FALSE
# https://bookdown.org/yihui/rmarkdown/tufte-figures.html
# fig.fullwidth = TRUE, with fig.width and fig.height, out.width
# https://www.statmethods.net/RiA/lattice.pdf
# 
# Adapted from IV_or_Regression.R to focus on general comparisons
# and incorporating Hugh McCague's idea of adding R-Squared or 
# some other measure of fit.
# 
#+ setup2, include=FALSE 
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
library(latticeExtra)
library(latex2exp)
library(p3d)
#'
#' \listoffigures
#' \clearpage
#' 
#' # A Causal Zoo
#' 
#' ![A DAG](dag_full.png){width=70%}
#'
  nams <- c('z','zx','zy','cov','x','y',
            'm','i', 'xd', 'yd', 'zd', 
            'col')
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
  
  # indirect effect: Note that the causal effect is 3 + 1 x 1 = 4
  
  mat['m','x'] <- 1
  mat['y','m'] <- 1
  
  # Instrumental variable 
  
  mat['x','i'] <- 1
  
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
  
  # but make SD of Y different 
  
  mat['y','y'] <- 4
  mat['i','i'] <- 2.4

#'
#' # DAG - not in lower triangular form
#'
  mat
#'
#' # DAG - in lower triangular form
#'
#' Expressing the DAG in lower triangular form makes it
#' easy to iteratively work out the variance matrix.
     
dag <- permute_to_dag(mat) # can be permuted to lower-diagonal form
dag 
#'
#' # Variance matrix
#'
covld(dag)
#' \clearpage
#' 
#' # Some models to try
#'
fmlas <- list(
  y ~ x,             # with confounding
  y ~ x + z,         # unconfounded
  y ~ x + zy,        # unconfounded using generating model
  y ~ x + zx,        # unconfounded using assignment model
  y ~ x + zx + zy,   # 'doubly robust'
  y ~ x + zy + cov,  # adding a covariate unrelated to x
  y ~ x + z + m,     # adding a mediator
  y ~ x + z + xd,    # adding a descendant of X
  y ~ x + z + yd,    # adding a descendant of Y
  y ~ x + z + yd + cov,    # adding a descendant of Y and a covariate
  y ~ x + z + col,   # adding a collider
  y ~ x + z + i,     # adding an instrumental variable
  y ~ x + z + i + cov,  # adding an instrumental variable and a covariate
  y ~ x + xd,        # adding a descendant of x
  y ~ x + i,         # using an instrumental variable as a control
  y ~ x + zd,        # imperfect control for confounding
  y ~ x + zd + cov,  # imperfect control + covariate
  y ~ x + zd + xd,   # imperfect control + descendant of x
  y ~ x + zd + i,    # bias amplification
  y ~ x | i          # instrumental variable using two-stage least squares
)
#' \clearpage
#'
#' # 'Fitting' the models
#'
fmlas %>% 
  lapply(coefx, dag) %>% 
  lapply(as.data.frame) %>% 
  do.call(rbind.data.frame, .) -> df

df <- within(
  df,
  { # label positions for plotting
    pos <- ifelse(grepl('IV|zy|m|zd.*z', label), 2, 4)
    pos2 <- ifelse(grepl('yd$|z . xd|zd$', label), 2, pos)
    pos2 <- ifelse(grepl('yd', label), 3, pos2)
    pos2 <- ifelse(grepl('yd.*cov$', label), 1, pos2)
  }
)
pdf <- df
sapply(pdf, is.numeric) %>% 
  {pdf[,.] <<- round(pdf[,.], 3)}
pdf[, c(6,1,4,2,3,5)] %>% print(row.names=F)

#'
#' \clearpage
#' 
cap = "$Var(\\hat{\\beta}_x)$ and $E(\\hat{\\beta}_x)$: variance versus bias."
#- bias, fig.height=7,fig.cap=cap
df %>% 
  xyplot(log2(sd_factor^2) ~ beta_x, . , font = 2, cex = 1,
         scales = list(y = list(at=seq(-3,10), labels=2^(5+seq(-3,10)))),
         xlim = c(-1.6, 6.5), 
         pch = 20,
         xlab = TeX('$E(\\hat{\\beta})$'),
         ylab = TeX('$Var(\\hat{\\beta})$ using relative $n$ to get equivalent power'),
         labs = sub('y ~ ','',.$label),
         pos = .$pos) +
  layer(panel.grid(h=-1,v=-1)) +
  layer(panel.abline(v = 4, lty = 3)) + 
  layer(panel.text(..., labels = labs, pos = pos)) 
#' \clearpage
#' 
#' ![Variance versus bias with DAG](bias_dag.png)
#' 
#' \clearpage
#' 
#' # Which model is best? 
#' 
#' __It depends on the purpose of the analysis!__ Thanks to Hugh McCague for
#' the idea of including the following figure to illustrate how focusing
#' on predictive power does not lead to a suitable model to estimate the
#' causal effect of X.
#-
caption <- "Adjusted measure of fit versus bias. Which model(s) would you choose?"
#- bias-gof, fig.height=7, fig.cap = caption
df %>% 
  subset(!grepl('i2', label)) %>%
  xyplot(var_e_adj ~ beta_x, . , font = 2,
         #scales = list(y = list(at=seq(-3,10), labels=2^(5+seq(-3,10)))),
         # xlim = c(-.6, 6), 
         pch = 16,
         xlab = TeX('$E(\\hat{\\beta})$'),
         ylab = TeX('Goodness of fit using an equivalent to adjusted R-squared (smaller is better)'),
         labs = sub('y ~ ','',.$label),
         pos = .$pos2) +
  layer(panel.text(..., labels = labs, pos = pos)) +
  layer(panel.grid(h=-1,v=-1)) +
  layer(panel.abline(v = 4, lty = 3))
#' \clearpage
#' 
#' # What's happening with IVs?
#' 
#' ![A simple DAG with an IV](dag1.png){width=50%}
#' 
#' Let's assume multivariate normality 
#' and build a variance matrix for Z, I, X, Y. 
#' 
#' We can scale $I$, $Z$ and
#' $X$ so they have unit variance and zero means. 
#' This eliminates irrelevant nuisance 
#' parameters.
#' 
#' Since $I$ is an instrument for the confounding effect of $Z$:
#' 
#' $$Var{
#' \begin{pmatrix}
#' Z \\ I \\ X
#' \end{pmatrix}
#' } =
#' \begin{pmatrix}
#' 1 & 0 & a \\ 0 & 1 & b \\ a & b & 1
#' \end{pmatrix}$$
#' with $a^2 + b^2 \le 1$.
#' 
#' Focus first on the _assignment model_, i.e. the model that determines
#' the value of $X$ from the values of $Z$, $I$ and $U_X$.
#' 
#' Letting $c^2 = 1 - a^2 - b^2$, $c^2$ represent the portion of the variance
#' in $X$ that is not attributed to the instrument, $I$, nor to the confounder,
#' $Z$, define
#' $$\rho_I = \frac{b^2}{b^2 + c^2}$$ 
#' the proportion of the variance in $X$ not
#' due to $Z$ that is 'explained' by $I$. 
#' 
#' For an instrument that captures
#' all of the variation not due to the confounder, $c^2 = 0$ and $\rho_I = 1$. 
#' 
#' Focusing next on the model generating $Y$, 
#' let 
#' $$Y = \alpha X + \beta Z + \gamma \varepsilon$$ 
#' with $\varepsilon \sim \N(0,1)$,
#' independent of
#' other variables.
#' 
#' The variance matrix is:
#' $$Var{
#' \begin{pmatrix}
#' Z \\ I \\ X \\ Y
#' \end{pmatrix}
#' } =
#' \begin{pmatrix}
#' 1 & 0 & a & a\alpha + \beta\\ 
#' 0 & 1 & b & b\alpha\\ 
#' a & b & 1 & \alpha + a \beta \\
#' a\alpha + \beta & b\alpha & \alpha + a \beta & v_{yy}
#' \end{pmatrix}$$
#' where $v_{yy} = \alpha^2 + \beta^2 + 2 a \alpha\beta + \sigma^2_\varepsilon$
#' 
#' We can verify that the regression coefficients for the regression of $Y$ on
#' $X$ and $Z$ are
#' $$ \begin{pmatrix}
#' 1 & a \\
#' a & 1
#' \end{pmatrix} ^{-1}
#' \begin{pmatrix}
#' \alpha + a\beta \\
#' a \alpha + \beta
#' \end{pmatrix} =
#' \begin{pmatrix}
#' \alpha \\ \beta
#' \end{pmatrix}$$ 
#' 
#' The variance of the least-squares estimator of $\alpha$ based on a regression
#' on $X$ and the confounder $Z$ is:
#' $$\begin{aligned}
#' \Var(\hat{\alpha}) & \approx \frac{1}{n}\;\frac{\sigma^2_\epsilon}{1-a^2} \\
#'         & = \frac{1}{n}\;\frac{\gamma^2}{b^2 + c^2}
#' \end{aligned}$$
#' 
#' The asymptotic expectation of the instrumental variable estimator $\tilde{\alpha}$ is 
#' $$\sigma_{IX}^{-1}\sigma_{IY} = \frac{1}{b}\times b\alpha = \alpha$$
#' 
#' The variance of $\tilde{\alpha}$ is
#' [@fox2016applied,p.241]:
#' 
#' $$\begin{aligned}
#' \Var(\tilde{\alpha}) & \approx \frac{1}{n}\;\sigma^2_{\epsilon IV} \sigma_{IX}^{-1}\sigma_{II}\sigma_{XI}^{-1}
#'         & = \frac{1}{n}\;(\beta^2 + \gamma^2)\frac{1}{b^2}
#' \end{aligned}$$
#' 
#' Thus, the variance inflation factor -- which is the same as the 'sample 
#' size inflation
#' factor to achieve the same power' -- using IV estimation instead of controlling for
#' a confounder (assuming that both approaches are available) is:
#' 
#' $$\begin{aligned}
#' IVVIF & = \frac{\Var(\tilde{\alpha})}{\Var(\hat{\alpha})} \\
#' & =  \frac{\beta^2 + \gamma^2}{b^2} / \frac{\gamma^2}{b^2 + c^2} \\
#' & =   \frac{\beta^2 + \gamma^2}{\gamma^2} / \frac{b^2}{b^2 + c^2} \\
#' & = 1/\left( {\frac{\gamma^2}{\gamma^2 + \beta^2} \times \frac{b^2}{b^2 + c^2}}\right) \\
#' & =   \left(1 + \frac{\beta^2}{\gamma^2}\right) \times \left(1 + \frac{c^2}{b^2} \right) \\
#' & = \frac{1}{1 - R^2_{Y,Z|X}} \times \frac{1}{R^2_{X,I|Z}}
#' \end{aligned}$$
#' 
#' The first term is structural in the sense that it is a consequence
#' of the problem, specifically the degree of confounding relative
#' to the residual error variance in the model. For a given problem,
#' the IV has no impact on this, so it represents a lower bound
#' for the IVVIF. The second term clarifies that it is not the _correlation
#' of the IV with X_ directly that affects the IVVIF, but its __partial
#' correlation__ adjusted for the relationship of X with confounders.
#' 
#' In conclusion: In any situation where you have a choice,
#' controlling for confounders will do better than using a corresponding IV,
#' i.e. an IV that annihilates the confounder. Fitting with IVs does
#' not take the same advantage of a model with a small error variance in
#' the same way that a regression model does. 
#' 
#' The lower bound for error variance
#' created by the confounder could swamp the benefit of
#' small residual variance in the generating model. In contrast,
#' a regression model takes full proportional advantage of a
#' reduction in residual error.
#' 
#' # References 
#' 
