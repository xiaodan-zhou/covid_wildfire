Steps for fitting MCMC with JAGS
================================================================================================

Installing `JAGS`
-----------------------

First, you will need to install JAGS (Just Another Gibbs Sampler) from here:

<https://sourceforge.net/projects/mcmc-jags/>

If you are using Windows, having `rtools` installed is also required. Details on installing this toolset can be found on the following webpage:

<https://cran.r-project.org/bin/windows/Rtools/>


Using `JAGS` in `R`
--------------------------

The `R` interface for `JAGS` is contained within the package `rjags`. This package can be installed from CRAN and loaded with the following code snippet:

``` r
if (!require(rjags)) {
	
  install.packages("rjags")
  library(rjags)

}

```

Any of the other libraries can be installed in the same manner, substituting `rjags` for whichever package you need to install. We will also need to load the glm module by running `load.module('glm')` which provides access to additional distributions that can be fit with JAGS including the half-t prior that we assume for many of the variance components in our model. We recommend running `bayes_sim.R` to start, but please be patient. The model takes a few hours to run. Also be sure to change the project directory paths when saving output!

Directory Contents
--------------------------

[`src/bayes/bayes_dlag.R`](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/src/bayes/bayes_dlag.R): Script for fitting constrained distributed lag models conditioned on PM2.5 exposure.

[`src/bayes/bayes_fun.R`](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/src/bayes/bayes_fun.R): Additional functions necessary to fit and analayze the distributed lag model output.

[`src/bayes/bayes_plot.R`](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/src/bayes/bayes_plot.R): Script for generating plots from the outputted distributed lag models.

[`src/bayes/bayes_sim.R`](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/src/bayes/bayes_sim.R): Code for running simulation study.

[`src/bayes/df_selection.R`](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/src/bayes/df_selection.R): Code which aids in determining the optimal/practical degrees of freedom for the spline bases of the adjustment variables.

[`src/bayes/dlag_constrained.jags`](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/src/bayes/dlag_constrained.jags), [`src/bayes/dlag_fit.jags`](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/src/bayes/dlag_fit.jags), [`src/bayes/dlag_unconstrained.jags`](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/src/bayes/dlag_unconstrained.jags): JAGS scripts that are inputted into `rjags` to sample posterior draws of the model parameters.

[`src/bayes/Model.R`](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/src/bayes/Model.R): Contains the function for fitting a constrained distributed lag model with MLE. Used by `df_selection.R`.

[`src/bayes/sensitivity.R`](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/src/bayes/sensitivity.R): Constructs plots for the models considered in our sensitivity analysis. Note that to get the MCMC objects extracted by this script, you will need to modify and rerun `bayes_dlag.R` under the different sensitivity scenarios.