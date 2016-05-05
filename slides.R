## ----setopts, echo = FALSE-----------------------------------------------
library(knitr)
library(memisc)
library(pander)
opts_chunk$set(echo = FALSE, results = "hide",
               message = FALSE, warning = FALSE)



## ----galtonfig, fig = TRUE, fig.cap = "Galton's height data"-------------
galton <- read.csv("data/Galton.csv")
plot(galton$Father, galton$Height,
     xlab = "Child's height [inches]",
     ylab = "Father's height [inches]",
     pch = 19,
          col = 'darkgrey')


## ----galton-bestfit, fig = TRUE, fig.cap = "Galton's height data"--------

mod <- lm(Height ~ Father, data = galton)

plot(galton$Father, galton$Height,
     xlab = "Child's height [inches]",
     ylab = "Father's height [inches]",
     pch = 19,
          col = 'darkgrey')

abline(mod)

text(65, 77,
     label = paste0("y = ",
         round(coef(mod)[1],2),
         " + ",
         round(coef(mod)[2],2),
         " * x"),
     pos = 1)

## ----tendergraph, fig.cap = "Sentence lengths"---------------------------
tender <- read.csv("data/fakedata.csv")

plot(jitter(tender$JudgeTerrible), tender$Sentence,
     xlab = "Judge is Judge Terrible",
     ylab = "Sentence length (months)",
     pch = 19,
     col = "darkgray",
     xlim = c(-2,2),
     axes = FALSE)
axis(1, at = c(0,1))
axis(2)



## ----tenderbestfit, fig.cap = "Sentence lengths with best-fitting line"----
plot(jitter(tender$JudgeTerrible), tender$Sentence,
     xlab = "Judge is Judge Terrible",
     ylab = "Sentence length (months)",
     pch = 19,
     col = "darkgray",
     xlim = c(-2,2),
     axes = FALSE)
axis(1, at = c(0,1))
axis(2)

mod <- lm(Sentence ~ JudgeTerrible, data = tender)
abline(mod)
     

## ----fakedatashow, echo = TRUE, results= "markup"------------------------
head(tender)

## ----tendermod, echo = TRUE----------------------------------------------
tender <- read.csv("data/fakedata.csv")
mod <- lm(Sentence ~ JudgeTerrible, data = tender)

## ----tendermodout, results = "markup", echo = TRUE-----------------------
summary(mod)

## ----tendermodpretty, results = "markup", echo = FALSE-------------------
pander(mtable(mod,
              summary.stats = c("N","AIC","R-squared")))

## ----tendermodlogged, echo = FALSE, results="asis"-----------------------
mod <- lm(log(Sentence) ~ JudgeTerrible, data = tender)
pander(mtable(mod,
              summary.stats = c("N","AIC","R-squared")))

## ----gaudetin, echo = TRUE, results = "markup"---------------------------
gaudet <- read.csv("data/gaudet.csv")
head(gaudet)

## ----gaudetmod, echo = TRUE, results = "markup"--------------------------
mod <- glm(I(Sentence == "Imprisonment")~Judge,
           family = binomial,
           data = gaudet)

## ----gaudetout, results = "markup", echo = T-----------------------------
summary(mod)

## ----gaudetpretty, results = "asis", echo = FALSE------------------------
tmp <- mtable(mod)
tmp$summaries <- subset(tmp$summaries,
                        rownames(tmp$summaries) %in% c("N","AIC","Log-likelihood","Nagelkerke R-sq."))
pander(tmp)

## ----sunsteinin, results = "markup"--------------------------------------
dat <- read.csv("data/Ch4Sunstein.csv")
ada <- subset(dat, circuit == 7 & dataset == 2)
head(ada[,c("judge_name", "conserve_vote", "cite", "dec_year", "party_pres")])

## ----adamod, echo = TRUE, results = "markup"-----------------------------
mod <- glm(conserve_vote~party_pres,
           data = ada,
           family = binomial)
summary(mod)

## ----reversemod, echo = TRUE---------------------------------------------
mod <- glm(party_pres ~ factor(circuit),
           data = dat,
           family = binomial)

## ----reversemodout, results = "markup"-----------------------------------
summary(mod)

## ----modwithcircuits, echo = TRUE, results = "markup"--------------------
mod <- glm(conserve_vote~party_pres+factor(circuit),
           data = dat,
           family = binomial)

## ----modwithcircuitsout, echo = TRUE, results = "markup"-----------------
summary(mod)

