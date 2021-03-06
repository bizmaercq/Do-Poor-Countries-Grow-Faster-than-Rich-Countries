Do Poor Countries Grow Faster than Rich Countries
================

## Introduction

We provide an empirical example of using partialling-out with Lasso to
estimate the regression coefficient —1 in the high-dimensional linear
regressionmodel:

    Y = —1D + —2W +‘.

Specifically we are interested in how the rates at which economies of
different countries grow, denoted by Y , are related to the initial
wealth levels in each country, denoted by D, controlling for country’s
institutional, educational, and other similar characteristics, denoted
by W. The relationship is captured by the regression coefficient —1. In
this repo, this coefficient is called the “speed of
convergence/divergence”, as it measures the speed at which poor
countries catch up or fall behind wealthy countries, controlling for W .

## Inference

Our inference question here is: do poor countries grow faster than rich
countries, controlling for educational and other characteristics? In
other words, is the speed of convergence negative:

    —1 <0?

This is the Convergence Hypothesis predicted by the Solow Growth Model

## Barro-Lee Growth Data

  - The outcome (Y) is the realized annual growth rate of a country’s
    wealth (Gross Domestic Product per capita).
  - The target regressor (D) is the initial level of the country’s
    wealth
  - The target parameter —1 is the speed of convergence, which measures
    the speed at which poor countries catch up with rich countries.
  - The controls (W) include measures of education levels, quality of
    institutions, trade openness, and political stability in the
    country.

## High-Dimensional Setting

The sample contains 90 countries and about 60 controls. Thus

    p ~ 60, n = 90

and p/n is not small. We expect the least squares method to provide a
poor/noisy estimate of —1. We expect the method based on partialling-out
with Lasso to provide a high quality estimate of —1.

## Result

``` 
                          Estimate Std. Error
Least Squares              -0.0094      0.030
Partialling-out via lasso  -0.0498      0.014
```

As expected, least squares provides a rather noisy estimate of the speed
of convergence, and does not allow us to answer the question about the
convergence hypothesis. In sharp contrast, partialling-out via Lasso
provides a more precise estimate. The lasso based point estimate is ~4%
and the 95% confidence interval for the (annual) rate of convergence is
~7.5% to ~1.5%. This empirical evidence does support the convergence
hypothesis.

## Summary

In this repo, we have examined an empirical example in the
high-dimensionalsetting. Using least squares in this setting gives us a
very noisy estimate of the target regression coefficient and does not
allow us to answer an important empirical question. In sharp
contrast,using the partialling-out method with Lasso does give us a
precise estimate of the regression coefficient and does allow us to
answer that question. We have found significant empirical evidence
supporting the convergence hypothesis of Solow.
