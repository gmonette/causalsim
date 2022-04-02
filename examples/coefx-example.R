nams <- c('zc','zl','zr','c','x','y','m','i')
mat <- matrix(0, length(nams), length(nams))
rownames(mat) <- nams
colnames(mat) <- nams

# confounding back-door path
mat['zl','zc'] <- 2 
mat['zr','zc'] <- 2
mat['x','zl'] <- 1
mat['y','zr'] <- 2

# direct effect
mat['y','x'] <- 3

# indirect effect
mat['m','x'] <- 1
mat['y','m'] <- 1

# Instrumental variable 
mat['x','i'] <- 2

# 'Covariate'
mat['y','c'] <- 1

# independent error
diag(mat) <- 2

mat # not in lower diagonal form   
dag <- to_dag(mat) # can be permuted to lower-diagonal form
dag

coefx(y ~ x, dag)  # with confounding
coefx(y ~ x + zc, dag)  # blocking back-door path
coefx(y ~ x + zr, dag) # blocking with lower SE
coefx(y ~ x + zl, dag) # blocking with worse SE
coefx(y ~ x + zr + c, dag)  # adding a 'covariate'
coefx(y ~ x + zr + m, dag)  # including a mediator
coefx(y ~ x + zl + i, dag)  # including an instrument
coefx(y ~ x + zl + i + c, dag) # I and C

# plotting added-variable plot ellipse 
lines(
    coefx(y ~ x + zr, mat),  
    lwd = 2, xv= 5,xlim = c(-5,10), ylim = c(-25, 50))
lines(
    coefx(y ~ x + zl, mat), new = FALSE,
    col = 'red', xv = 5, lwd = 2)
lines(
    coefx(y ~ x + i, mat), new = FALSE,
    col = 'dark green', xv = 5)

# putting results in a data frame
# for easier comparison of SEs

fmlas <- list(
  y ~ x, 
  y ~ x + zc, 
  y ~ x + zr, 
  y ~ x + zl,
  y ~ x + zr + c, 
  y ~ x + zr + m, 
  y ~ x + zl + i,
  y ~ x + zl + i + c
)
res <- lapply(fmlas, coefx, dag)
res <- lapply(res, function(ll) {
    ll$fmla <- paste(as.character(ll$fmla)[c(2,1,3)], collapse = ' ')
    ll$beta <- ll$beta[1]
    ll
})

df <- do.call(rbind.data.frame, res)
df 

# simulation

head(sim(dag, 100))
var(sim(dag, 10000)) - covld(dag)

# plotting
#'
plot(dag) + ggdag::theme_dag()

