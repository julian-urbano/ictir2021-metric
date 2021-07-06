source("R/common.R")

library(rio)
library(dplyr)
library(VineCopula)
library(simIReff)
library(glue)
library(moments)
library(parallel)
library(doParallel)

registerDoParallel(cores = .CORES)

path_out <- "scratch/02-skew"
dir.create(path_out, recursive = TRUE)

#' Calculate the skewness of the per-topic score differences in 1) the original TREC data,
#' and 2) the simulated  scores.
#' We do this via Monte Carlo for simplicity, by running n2 simulations of size n1 each
skew <- function(cop, mar, x1, x2, n1 = 1000, n2 = 100) {
  s_trec <- skewness(x1-x2)
  s_cop <- mean(replicate(n2, {
    u <- BiCopSim(n1, obj = cop)
    u1 <- qeff(u[,1], mar)
    u2 <- qeff(u[,2], mar)
    
    d <- u1-u2
    skewness(d)
  }))
  
  c(s_trec, s_cop)
}

# Compute ------------------------------------------------------------------------------------------

for(measure in .MEASURES){
  d <- lapply(.COLLECTIONS, function(collection) {
    f <- glue("data/trec_scores/{collection}_{measure}.csv")
    if(file.exists(f)) {
      cat(f, "\n"); flush.console()
      
      trec <- import(f) # original TREC scores
      runs <- colnames(trec)
      
      d <- foreach(run1 = runs,
                   .packages = c("VineCopula", "simIReff", "glue", "rio", "dplyr", "moments"),
                   .combine = "rbind",
                   .export = c("measure", "collection", "skew")) %dopar% {
        f <- glue("data/01-margins/{collection}_{measure}/{run1}.rds")
        if(file.exists(f)) {
          mar <- import(f) # margin of run1
          
          # For every pair or runs, compute statistics of the copula and the margin (run1's)
          d <- sapply(runs, function(run2) {
            f <- glue("data/03-bicops/{collection}_{measure}/{run1}_{run2}.rds")
            if(file.exists(f)) {
              cop <- import(f) # copula for run1-run2
              
              # Cap to par<=50 in Gumbel because of new restriction in VineCopula >= 2.3.0
              # This appears to be for numerical stability 
              # https://github.com/tnagler/VineCopula/issues/62
              if(cop$family %in% c(4,14,24,34)) {
                cop <- BiCop(cop$family, min(cop$par, 50))
              }
              
              s <- skew(cop, mar, trec[,run1], trec[,run2])
              
              data.frame(run1 = as.integer(gsub("run", "", run1)),
                         run2 = as.integer(gsub("run", "", run2)),
                         family = BiCopName(cop$family),
                         skew_trec = s[1],
                         skew_cop = s[2],
                         collection = collection
              )
            }else{
              NULL
            }
          }) %>% bind_rows()
        }
      }
    }
  }) %>% bind_rows()
  
  export(d, glue("{path_out}/{measure}.csv"))
}

stopImplicitCluster()
