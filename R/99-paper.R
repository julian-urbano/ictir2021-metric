source("R/common.R")

library(dplyr)
library(rio)
library(glue)
library(ggplot2)
library(VineCopula)
library(simIReff)
library(forcats)
library(tidyr)

path_out <- "output/figures/"
path_paper <- glue("{path_out}/paper")
dir.create(path_out, recursive = TRUE, showWarnings = FALSE)
dir.create(path_paper, recursive = TRUE, showWarnings = FALSE)

theme_set(theme_bw())

# Fig 2: Distributions of copulas and margins ------------------------------------------------------

for(measure in .MEASURES) {
  # read data for n=25 (the others are from the same models)
  d <- import_list(list.files(glue("data/11-type_1/{measure}_25/"), full.names = TRUE),
                   rbind = TRUE)
  
  dd <- data.frame(margin = recode_margin(d$margin),
                   copula = recode_cop(clean_cop_name(d$cop))) %>%
    pivot_longer(cols = margin:copula) %>%
    mutate(name = recode_factor(name),
           value = reorder_cop_mar_n(value))
  
  pdf(glue("{path_out}/barplot_{measure}.pdf"), width = 14/3, height = 3)
  # We do *2 below because there are 2 factors. Otherwise the y-axis wouldn't sum 100% per panel
  print(ggplot(dd) + aes(value, y = (..count..)/sum(..count..)*2) +
    geom_bar() +
    facet_grid(~name, drop = TRUE,  space = "free", scale = "free") +
    labs(x = "", y = "Frequency", title = recode_measure(measure)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  )
  dev.off()
}

# Fig 3: EMM by copula, margin and n ---------------------------------------------------------------

make_fig <- function(measure, path, width, height, tests = NULL) {
  d <- import(glue("output/01-emm1/{measure}.csv")) %>%
    filter(statistic != "d") %>%
    rename(test = statistic)
  if(!is.null(tests)) d <- d %>% filter(test %in% tests)
  d <- d %>%
    mutate(test = recode_test(test),
           level = reorder_cop_mar_n(recode_margin(recode_cop(level))),
           factor = recode_factor(factor))
  
  pdf(path, width = width, height = height)
  print(ggplot(d, aes(est, level)) +
          geom_point() +
          geom_linerange(aes(xmin = lwr, xmax = upr)) +
          geom_vline(xintercept = .05, linetype = "dashed", size = .5) + 
          scale_x_continuous(minor_breaks = seq(-.5, .5, .005), breaks = seq(-.5, .5, .01)) + 
          facet_grid(factor~test, space = "free", scale = "free") +
          labs(x = "EMM Type I Error rate", y = NULL, title = recode_measure(measure)) +
          theme(panel.grid.major.y = element_blank()))
  dev.off()
}

for(measure in .MEASURES)
  make_fig(measure, glue("{path_out}/emm1_{measure}.pdf"), 14, 4)

make_fig("ap", glue("{path_paper}/emm1_ap.pdf"), 14, 4, c("t2", "w2", "b2"))
make_fig("p10", glue("{path_paper}/emm1_p10.pdf"), 7, 4, c("t2", "w2", "b2"))
make_fig("rr", glue("{path_paper}/emm1_rr.pdf"), 7, 4, c("t2", "w2", "b2"))

# Fig 6: EMM by skewness ---------------------------------------------------------------------------

make_fig <- function(measure, path, width, height, tests = NULL) {
  d <- import(glue("output/03-emm2/{measure}.csv")) %>%
    filter(statistic != "d",
           factor == "skew") %>%
    rename(test = statistic)
  if(!is.null(tests)) d <- d %>% filter(test %in% tests)
  d <- d %>%
    mutate(test = recode_test(test),
           level = reorder_skew(level),
           factor = recode_factor(factor))
  
  pdf(path, width = width, height = height)
  print(ggplot(d, aes(est, level)) +
          geom_point() +
          geom_linerange(aes(xmin = lwr, xmax = upr)) +
          geom_vline(xintercept = .05, linetype = "dashed", size = .5) + 
          scale_x_continuous(minor_breaks = seq(-.5, .5, .005), breaks = seq(-.5, .5, .01)) + 
          scale_y_discrete(drop=FALSE)+
          facet_grid(factor~test, space = "free", scale = "free") +
          labs(x = "EMM Type I Error rate", y = NULL, title = recode_measure(measure)) +
          theme(panel.grid.major.y = element_blank()))
  dev.off()
}

for(measure in .MEASURES)
  make_fig(measure, glue("{path_out}/emm2_{measure}.pdf"), 14, 2.5)

make_fig("ap", glue("{path_paper}/emm2_ap.pdf"), 14, 2.5, c("t2", "w2", "b2"))
make_fig("p10", glue("{path_paper}/emm2_p10.pdf"), 7, 2.5, c("t2", "w2", "b2"))
make_fig("rr", glue("{path_paper}/emm2_rr.pdf"), 7, 2.5, c("t2", "w2", "b2"))

# Fig 5: Skewness TREC vs simulated ----------------------------------------------------------------

d <- lapply(.MEASURES, function(measure) {
  import(glue("scratch/02-skew/{measure}.csv")) %>%
    mutate(copula = clean_cop_name(family),
           copula = ifelse(copula == "Tawn", "Tawn", "Others"),
           measure = recode_measure(measure))
}) %>% bind_rows()

pdf(glue("{path_out}/skewness.pdf"), width = 14, height = 4)
print(ggplot(d, aes(skew_trec, skew_cop, color = copula)) +
  geom_point(alpha = .1) +
  facet_wrap(~measure, ncol = 5) +
  lims(x = c(-3, 3), y = c(-3, 3)) +
  labs(x = "Skewness of TREC data", y = "Skewness of simulated data", color = "Copula") +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = "bottom"))
dev.off()

jpeg(glue("{path_paper}/skewness.jpg"), width = 7, height = 3, res = 400, units = "in")
print(ggplot(d %>% filter(measure %in% c("AP", "P@10", "RR")),
             aes(skew_trec, skew_cop, color = copula)) +
        geom_point(alpha = .1) +
        facet_wrap(~measure, ncol = 3) +
        lims(x = c(-3, 3), y = c(-3, 3)) +
        labs(x = "Skewness of TREC data", y = "Skewness of simulated data", color = "Copula") +
        guides(color = guide_legend(override.aes = list(alpha = 1))) +
        theme(legend.position = "bottom"))
dev.off()

# Fig 4: Example -----------------------------------------------------------------------------------

collection <- "adhoc7"
run1 <- "run46"; name1 <- "iit98au2"
run2 <- "run61"; name2 <- "LIAClass"
run3 <- "run65"; name3 <- "LNaTitDesc7"

trec <- import(glue("data/trec_scores/{collection}_ap.csv"))
mar <- import(glue("data/01-margins/{collection}_ap/{run1}.rds"))
cop12 <- import(glue("data/03-bicops/{collection}_ap/{run1}_{run2}.rds"))
cop13 <- import(glue("data/03-bicops/{collection}_ap/{run1}_{run3}.rds"))

pdf(glue("{path_paper}/example.pdf"), width = 14, height = 3)
par(mfrow = c(1, 5))
set.seed(0)

# 1)
x01 <- seq(0, 1, .02)
hist(trec[,run1], probability = TRUE, breaks = 20, xlim = range(x01), ylim = c(0, 8),
     xlab = "AP", ylab = "Density",
     main = glue("1) Margin\n{name1}"))
lines(x01, deff(x01, mar), lwd = 2)

# 2.A)
plot(qnorm(pobs(trec[,run1])), qnorm(pobs(trec[,run2])),
     pch = 19, xlim = c(-3, 3), ylim = c(-3, 3),
     xlab = name1, ylab = name2, main = glue("2.A) Dependence\n{name1} v. {name2} (Clayton)"))
contour(cop12, drawlabels = FALSE, add = TRUE)

# 2.B)
x01 <- seq(-1, 1, .02)
x <- BiCopSim(10000, obj = cop12)
x <- qeff(x[,1], mar) - qeff(x[,2], mar)

hist(x, probability = TRUE, breaks = 20, xlim = range(-.5, .5), ylim = c(0, 8),
     xlab = "Diff. AP", ylab = "Density",
     main = glue("2.B) Score differences\n{name1} - {name1}, Clayton"))
text(-.5, 7, pos = 4, glue("Skewness\n{round(moments::skewness(x),5)}"))
lines(density(x), lwd = 2)

# 3.A)
plot(qnorm(pobs(trec[,run1])), qnorm(pobs(trec[,run3])),
     pch = 19, xlim = c(-3, 3), ylim = c(-3, 3),
     xlab = name1, ylab = name3,
     main = glue("3.A) Dependence\n{name1} v. {name3} (Tawn)"))
contour(cop13, drawlabels = FALSE, add = TRUE)

# 3.B)
x <- BiCopSim(10000, obj = cop13)
x <- qeff(x[,1], mar) - qeff(x[,2], mar)
hist(x, probability = TRUE, breaks = 20, xlim = range(-1, .6), ylim = c(0, 8),
     xlab = "Diff. AP", ylab = "Density",
     main = glue("3.B) Score differences\n{name1} - {name1}, Tawn"))
text(-1, 7, pos = 4, glue("Skewness\n{round(moments::skewness(x),5)}"))
lines(density(x), lwd = 2)

dev.off()
