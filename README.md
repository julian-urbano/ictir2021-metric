This repository contains the data and source code for the following paper:

* J. Urbano, M. Corsi and A. Hanjalic, "[How do Metric Score Distributions affect the Type I Error Rate of Statistical Significance Tests in Information Retrieval?](http://julian-urbano.info/files/publications/047-metric-score-distributions-affect-type-i-error-rate-statistical-significance-tests-information-retrieval.pdf)", *ACM SIGIR International Conference on the Theory of Information Retrieval*, 2021.

A [single ZIP file](https://github.com/julian-urbano/ictir2021-metric/archive/master.zip) can be downloaded as well.

## Project Structure

* `data/` Input data files (from the SIGIR 2019 paper).
* `output/` Generated output files and figures.
* `R/` Source code in R.
* `scratch/` Temporary files generated in the process.

All code is written in [R](https://www.r-project.org). If you want to run it yourself, you will need the following packages installed from CRAN: `dplyr`, `rio`, `glue`, `emmeans`, `doParallel`, `parallel`, `tidyr`, `VineCopula`, `simIReff`, `ggplot2`, `forcats` and `moments`.

## How to reproduce the results in the paper 

The source files in `R/` need to be run in order. You can run each file individually by running `Rscript R/<file>.R`. They will store intermediate data in `scratch/` and the final data in `out/`.

**It is important that you always run from the base directory**.

1. `R/01-emm1.R` computes the estimated marginal means across copulas, margins and sample sizes, as well as confidence intervals.
2. `R/02-skew.R` computes the skewness of the original TREC data and the simulated data.
3. `R/03-emm2.R` computes the estimated marginal means across skewness levels, as well as confidence intervals.
4. `R/99-paper.R` generates all figures and stores them in `output/`

It takes some time to run all the code, so it is ready to run in parallel. Most of the above code parallelizes using function `foreach` in R's package [`doParallel`](https://cran.r-project.org/web/packages/doParallel/index.html). In particular, it will use all available cores in the machine. Edit file `R/common.R` to modify this behavior and other parameters.

Note that the script `R/99-paper.R` is intended to generate the figures in the paper, plus all other tests and metrics not reported there. If you customize something and want a similar analysis, you will need to extend this script yourself.

## License

* The TREC results in `data/` are anonymized and posted here with permission from the organizers.
* Databases and their contents are distributed under the terms of the [Creative Commons Attribution-ShareAlike 4.0 International License](http://creativecommons.org/licenses/by-sa/4.0/).
* Software is distributed under the terms of the [MIT License](https://opensource.org/licenses/MIT).

When using this archive, please cite the above paper:

    @inproceedings{urbano2021metric,
      author = {Urbano, Juli\'{a}n and Corsi, Matteo and Hanjalic, Alan},
      booktitle = {ACM SIGIR International Conference on the Theory of Information Retrieval},
      title = {{How do Metric Score Distributions affect the Type I Error Rate of Statistical Significance Tests in Information Retrieval?}},
      year = {2021},
      pages = {xxx--xxx}
    }
