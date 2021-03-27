Steps for fitting MCMC with JAGS
### ================================================================================================

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

Any of the other libraries can be installed in the same manner, substituting `rjags` for whichever package you need to install. We will also need to load the glm module by rinning `load.module('glm')`
which provides access to additional distributions that can be fit with JAGS including the half-t prior that we assume for many of the variance components in our model. We recommend running `bayes_sim.R` to start, but please be patient. The model takes a few hours to run. Also be sure to change the project directory paths when saving output.

Directory Contents
--------------------------

`bayes_dlag.R`: Script for fitting constrained distributed lag model on PM2.5 exposure.
`bayes_fun.R`: Additional functions necessary to fit and analayze the distributed lag model output.
`bayes_plot.R`: Script for generating plots from the outputted distributed lag models.
`bayes_sim.R`: Simulation code.
`df_selection.R`: Code which aids in determining the optimal/practical degrees of freedom for the spline bases of the adjustment variables.
`dlag_constrained`, `dlag_fit`, `dlag_unconstrained`: JAGS scripts which are inputted into `rjags` to sample posterior draws of the parameters.
`model.R`: Contains function for fitting distributed lag model with MLE. Used by `df_selection.R`
`sensitivity.R`: Constructs plots for the models considered in our sensitivity analysis. Note that to get the mcmc objects extracted by this script, you will need to modify and rerun `bayes_dlag.R` under the different sensitivity scenarios.