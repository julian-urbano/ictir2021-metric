source("R/common.R")

library(dplyr)
library(rio)
library(glue)
library(forcats)
library(emmeans)
library(doParallel)
library(parallel)
library(tidyr)

registerDoParallel(cores = .CORES)

path_out <- "scratch/03-emm2"

# Bootstrap and calculate effects ------------------------------------------------------------------

for(measure in .MEASURES) {
  cat(measure, "\n"); flush.console()
  dir.create(glue("{path_out}/{measure}"), recursive = TRUE)
  
  # read all p-values from this measure
  d <- lapply(.N, function(n) {
    import_list(list.files(glue("data/11-type_1/{measure}_{n}/"), full.names = TRUE),
                rbind = TRUE) %>%
      select(-`_file`, -t1:-p1) %>%
      mutate(n = n)
  }) %>% bind_rows()
  
  # remove the few (43) cases with NA in sign test
  d <- na.omit(d)
  # recode sample sizes as factor
  d$n <- recode_n(d$n)
  # clean copula families to ignore rotations and aggregate tawn1 and tawn2 into tawn
  d$cop <- clean_cop_name(d$cop)
  # run1 was the one with lower index, and run2 the one with higher index, so swap if necessary
  d <- d %>%
    mutate(run1 = pmin(b,e),
           run2 = pmax(b,e))
  
  # read skewness data for this measure
  s <- import(glue("scratch/02-skew/{measure}.csv")) %>%
    mutate(skew = cut(abs(skew_cop),
                      breaks = c(seq(0, 1.75, .25),Inf)))
  
  # join all p-value data and skewness data
  d <- left_join(d, s, by = c("collection", "run1", "run2"))
  
  # bootstrap .B times (b=0 for the original sample)
  # fit linear model and compute effects
  # in the end we'll use this to compute confidence intervals
  r <- foreach(b = 0:.B, .packages = c("rio", "glue", "dplyr", "emmeans"),
               .export = c("measure", "path_out")) %dopar% {
    set.seed(b)
    
    # bootstrap sample, stratified by (n,cop,margin)
    dd <- d %>%
      group_by(n, cop, margin) %>%
      sample_frac(size = 1,
                  replace = ifelse(b == 0, FALSE, TRUE)) %>% # don't resample when b=0
      ungroup()
    
    # compute observed mean score and type 1 errors
    dd <- dd %>%
      group_by(n, skew, margin) %>%
      summarize(d = mean(d),
                t2 = type1(t2),
                w2 = type1(w2),
                s2 = type1(s2),
                b2 = type1(b2),
                p2 = type1(p2))
    
    # fit model
    m <- lm(cbind(d, t2, w2, s2, b2, p2) ~ (skew + margin + n)^2, dd)
    
    # compute effects (we do this "manually" because some combinations (eg. BB6-beta)
    # do not have observations; emmeans does not handle those and just gives NA)
    g <- ref_grid(m, mult.names = "statistic") %>%
      as.data.frame() %>%
      mutate(b = b) %>% # identify bootstrap sample
      select(b, statistic, n, skew, margin, prediction)
    
    # and save
    export(g, glue("{path_out}/{measure}/{b}.csv"))
    
    # free up memory
    rm(dd); gc()
  }
  # free up memory
  rm(d); gc()
}

stopImplicitCluster()

# Calculate intervals ------------------------------------------------------------------------------

path_in <- path_out
path_out <- "output/03-emm2"
dir.create(path_out, recursive = TRUE)

for(measure in .MEASURES) {
  cat(measure, "\n"); flush.console()
  
  # read effects from all bootstrap samples
  g <- import_list(list.files(glue("{path_in}/{measure}/"), full.names = TRUE),
                   rbind = TRUE) %>%
    select(-`_file`) %>%
    na.omit() %>% # drop NA estimates (see above)
    rename(y = prediction) %>%
    pivot_longer(cols = n:margin, names_to = "factor", values_to = "level",
                 values_transform = list(level = as.character)) # turn into long format
  
  # compute estimated marginal means
  e <- g %>%
    group_by(statistic, b, factor, level) %>%
    summarize(e = mean(y)) %>%
    as.data.frame()
  
  # means with full dataset
  e0 <- e %>%
    filter(b == 0) %>%
    select(-b) %>%
    rename(e0 = e)
  # means with bootstrap samples
  e1 <- e %>%
    filter(b != 0) %>%
    select(-b)
  
  # join and compute BC intervals
  e <- inner_join(e1, e0)
  
  e <- e %>%
    group_by(statistic, factor, level) %>%
    summarize(
      z0 = qnorm(sum(e < e0) / n()),
      alpha_lwr = pnorm(2*z0 + qnorm(.025)),
      alpha_upr = pnorm(2*z0 + qnorm(.975)),
      lwr = quantile(e, alpha_lwr),
      upr = quantile(e, alpha_upr),
      est = mean(e0)) %>%
    select(-z0, -alpha_lwr, -alpha_upr)
  
  export(e, glue("{path_out}/{measure}.csv"))
}
