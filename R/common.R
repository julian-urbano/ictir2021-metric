library(forcats)

.COLLECTIONS <- c("adhoc5", "adhoc6", "adhoc7", "adhoc8",
                  "web2010", "web2011", "web2012", "web2013")
.MEASURES <- c("ap", "ndcg20", "err20", "p10", "rr")
.N <- c(25, 50, 100)

.CORES <- parallel::detectCores()
.B <- 1000 # Number of bootstrap samples

# Utils ############################################################################################

#' Computes the observed type 1 error rate given p-values and alpha level
type1 <- function(p, alpha = .05) {
  sum(p <= alpha) / length(p)
}

# Functions to recode and rename variables for plotting ############################################

#' Clean a copula name by removing rotations. Tawn1 and Tawn2 reduce to Tawn
clean_cop_name <- function(name) {
  name <- gsub("(^S|_?(90|180|270))", "", name)
  name <- gsub("Tawn2", "Tawn", name)
  name
}

#' Recode and reorder a factor variable
recode_and_reorder <- function(x, ...) {
  suppressWarnings(fct_recode(fct_relevel(x, ...), ...))
}

recode_n <- function(x) {
  factor(x, levels = as.character(sort(.N)))
}

recode_cop <- function(x) {
  recode_and_reorder(x,
                     Gaussian = "N",
                     `Student t`= "t",
                     Clayton = "C",
                     Gumbel = "G",
                     Frank = "F",
                     Joe = "J",
                     BB1 = "BB1",
                     BB6 = "BB6",
                     BB7 = "BB7",
                     BB8 = "BB8",
                     Tawn = "Tawn",
                     Indep. = "I")
}

recode_margin <- function(x) {
  recode_and_reorder(x,
                     `Tr.Norm` = "norm",
                     Beta = "beta",
                     `Tr.Norm KS` = "nks",
                     `Beta KS`= "bks",
                     `Beta-Binom` = "bbinom",
                     `Disc. KS` = "dks(1)",
                     `Disc. KS (2)` = "dks(2)",
                     `Disc. KS (5)` = "dks(5)",
                     `Disc. KS (10)` = "dks(10)")
}

recode_test <- function(x) {
  recode_and_reorder(x,
                     Bootstrap = "b2",
                     `t-test` = "t2",
                     Permutation = "p2",
                     Wilcoxon = "w2",
                     Sign = "s2")
}

recode_measure <- function(x) {
  recode_and_reorder(x,
                     AP = "ap",
                     `nDCG@20` = "ndcg20",
                     `ERR@20` = "err20",
                     `P@10` = "p10",
                     RR = "rr")
}

recode_factor <- function(x) {
  recode_and_reorder(x,
                     Copula = "copula",
                     Copula = "cop",
                     Margin = "margin",
                     Skewness = "skew",
                     n = "n")
}

reorder_cop_mar_n <- function(x) {
  factor(x,
         levels = c("Indep.", "BB1", "BB6", "BB7", "BB8", "Clayton", "Frank",
                    "Gaussian", "Gumbel", "Joe", "Student t", "Tawn",
                    
                    "Tr.Norm", "Beta", "Tr.Norm KS", "Beta KS",
                    "Beta-Binom", "Disc. KS", "Disc. KS (2)", "Disc. KS (5)",
                    "Disc. KS (10)",
                    
                    "25", "50", "100")
  )
}
reorder_skew <- function(x) {
  factor(x,
         levels = c("(0,0.25]", "(0.25,0.5]", "(0.5,0.75]", "(0.75,1]",
                    "(1,1.25]", "(1.25,1.5]", "(1.5,1.75]", "(1.75,Inf]")
  )
}
