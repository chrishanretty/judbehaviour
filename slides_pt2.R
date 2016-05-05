## ----setopts, echo = FALSE-----------------------------------------------
library(knitr)
library(pander)
opts_chunk$set(echo = FALSE, results = "hide",
               message = FALSE, warning = FALSE)

## ----scdb----------------------------------------------------------------
require(reshape2)
scdb <- read.csv("data/SCDB_2015_03_justiceCentered_Citation.csv")
### subset to 
scdb <- subset(scdb, term >= 2010)
### Create dissent var based on dissent codes (http://supremecourtdatabase.org/documentation.php?var=vote)
scdb$dissented <- as.numeric(scdb$vote %in% c(2, 6, 7))
scdb$agreed <- 1 - scdb$dissented
### Cast so that judges on the columns
scdb.c <- dcast(scdb, caseId +caseName ~ justiceName, value.var = "agreed")

## ----scdbhead, results = "asis"------------------------------------------
cases.with.dissent <- (apply(scdb.c[,3:ncol(scdb.c)],1,var) > 0)
pander(head(scdb.c[cases.with.dissent,-2], 4))

## ----scdbagreement-------------------------------------------------------
presence.mat <-  tcrossprod(t(as.matrix(!is.na(scdb.c[,3:ncol(scdb.c)]))),t(as.matrix(!is.na(scdb.c[,3:ncol(scdb.c)]))))

agreement.mat <-  tcrossprod(t(as.matrix(scdb.c[,3:ncol(scdb.c)])),t(as.matrix(scdb.c[,3:ncol(scdb.c)])))
diag(agreement.mat) <- diag(presence.mat)


dis.mat <- (presence.mat - agreement.mat)
dis.mat2 <- (presence.mat - agreement.mat) / presence.mat
ag.mat <- agreement.mat / presence.mat
mean.ag <-  100 * round(mean(ag.mat, na.rm = T),2)
min.ag <-  100 * round(min(ag.mat, na.rm = T),2)
minj <- which(ag.mat == min(ag.mat), arr.ind = T)
minj1 <- rownames(minj)[1]
minj2 <- rownames(minj)[2]


## ----binomtest, echo = TRUE, results = "markup"--------------------------
times.judges.agreed <- 228
times.judges.sat.together <- 387
binom.test(times.judges.agreed,
           times.judges.sat.together,
           p = 0.75)

## ----binomtestuksc, echo = TRUE, results = "markup"----------------------
times.judges.agreed <- 21
times.judges.sat.together <- 26
binom.test(times.judges.agreed,
           times.judges.sat.together,
           p = 0.84)

## ----irtfig1, fig=TRUE, fig.cap = "Probability of voting with majority"----

logitfunc <- function(x, a, b) {
    y <- a + b*x
    y <- 1 + exp(-y)
    y <- 1/y
    return(y)
}

inx <- seq(-3,3,length.out = 100)
outy <- logitfunc(inx, a=0,b=1)
plot(inx, outy,
     main = "",
     type = "l",
     xlab = "Judge ideal point",
     ylab = "Probability of voting with majority",
     axes = FALSE)
points(0, 0.5, pch = 21, bg = 'red', fg = 'white')
text(0, 0.5, "Case location", pos = 4)
axis(1)
axis(2)

## ----casediscrim, fig = TRUE, fig.cap = "Varying discrimination parameters"----
inx <- seq(-3,3,length.out = 100)
outy <- logitfunc(inx, a=0,b=1)
plot(inx, outy,
     main = "",
     type = "l",
     xlab = "Judge ideal point",
     ylab = "Probability of voting with majority",
     axes = FALSE)
points(0, 0.5, pch = 21, bg = 'red', fg = 'white')
text(0, 0.5, "Case location", pos = 4)
outy <- logitfunc(inx, a = 0, b = 0.5)
lines(inx, outy, col = 'red')
outy <- logitfunc(inx, a = 0, b = -1)
lines(inx, outy, col = 'blue')
axis(1)
axis(2)

## ----scdbtranspose, echo = TRUE------------------------------------------
### Get the third column to the last column
vote.mat <- scdb.c[,3:ncol(scdb.c)]
### Store the judge names
judge.names <- names(vote.mat)
### Convert the data to a matrix
vote.mat <- as.matrix(vote.mat)
### Transpose it
vote.mat <- t(vote.mat)
### Show the first three judges and
### the first ten cases
vote.mat[1:3,1:10]

## ----domcmcpack, echo = TRUE, eval = FALSE-------------------------------
## library(MCMCpack)
## ### Test run
## model <- MCMCirt1d(vote.mat,
##                   theta.constraints = list("AScalia" = 1,
##                                            "SSotomayor" = -1),
##                   burnin=50,
##                   mcmc=100,
##                   thin=2,
##                   verbose=5,
##                         store.item=TRUE)
## 

## ----dopscl, echo = TRUE, eval = FALSE-----------------------------------
## library(pscl)
## my.rc <- rollcall(vote.mat,
##                   legis.names = judge.names)
## my.rc <- dropUnanimous(my.rc)
## cl <- constrain.legis(my.rc,
##                             x=list("AScalia"=1,
##                               "SSotomayor"=-1),
##                             d=1)
## model <- ideal(my.rc, d = 1,
##                maxiter = 100,
##                burnin = 50,
##                thin = 2,
##                priors = cl,
##                startvals = cl,
##                store.item = TRUE)

## ----dopsclforreals, echo = FALSE, eval = TRUE, cache = TRUE-------------
library(pscl)
my.rc <- rollcall(vote.mat,
                  legis.names = judge.names)
my.rc <- dropUnanimous(my.rc)
cl <- constrain.legis(my.rc,
                            x=list("AScalia"=1,
                              "SSotomayor"=-1),
                            d=1)
model <- ideal(my.rc, d = 1,
               maxiter = 1e6,
               burnin = .25e6,
               thin = 1e6/1000,
               priors = cl,
               startvals = cl,
               store.item = TRUE)               

## ----plotoutcomes, fig = TRUE, fig.cap = "Ideal points"------------------
require(ggplot2)
plot.df <- summary(model)
plot.df <- data.frame(Judge = names(plot.df$xm[,1]),
                      pos = plot.df$xm[,1],
                      lo = plot.df$xHDR[,"lower",],
                      hi = plot.df$xHDR[,"upper",])
plot.df <- plot.df[order(plot.df$pos),]
plot.df$Judge <- factor(plot.df$Judge,
                        levels = plot.df$Judge,
                        ordered = TRUE)
ggplot(plot.df, aes(x = Judge, y = pos, ymin = lo, ymax = hi)) +
    geom_pointrange() +
    scale_y_continuous("Position") + 
    theme_bw() +
    coord_flip() 


