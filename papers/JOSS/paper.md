---
title: 'mlr3proba: Probabilistic Supervised Learning for 'mlr3''
tags:
  - R
  - statistics
  - machine learning
  - survival analysis
  - density estimation
  - object-oriented
authors:
  - name: Raphael E.B. Sonabend
    orcid: 0000-0001-9225-4654
    affiliation: 1
  - name: Franz J. Kiraly
    affiliation: "1, 2"
affiliations:
 - name: Department of Statistical Science, University College London, Gower Street, London WC1E 6BT, United Kingdom
   index: 1
 - name: Shell
   index: 2
date: 17 February 2020
bibliography: paper.bib
---

# Summary

`mlr3proba` is a machine learning toolkit for making probabilistic predictions within the `mlr3` [@packagemlr3] ecosystem. Currently `mlr3proba` only implements survival analysis through the `Surv` task, however future updates will introduce probabilistic supervised regression, as well as unconditional distribution estimation.

Key features of **mlr3proba** are

* A unified fit/predict model interface to any probabilistic predictive model (frequentist, Bayesian, or other)
* Pipeline/model composition
* Task reduction strategies
* Domain-agnostic evaluation workflows using task specific algorithmic performance measures.

`mlr3proba` is an `R` [@packageR] that implements survival models in a machine learning framework using the `mlr3` [@packagemlr3] family of packages. `mlr3` is the official upgrade to `mlr` [@packagemlr], which makes use of the state-of-the-art R6 [@packageR6] object-oriented paradigm. `mlr3proba` builds on `mlr3` by introducing survival models and measures with a distributional predict type.

`mlr3proba`  makes use of `distr6` [@packagedistr6] in order to predict probability distributions that represent survival curves. This has a key advantage over other survival packages as it unifies the return type into a single object. 

Related software includes the original `mlr` [@packagemlr] package, which includes functionality for some survival learners. Additionally the `survival` [@packagesurvival] implements some classical survival models. Otherwise support for survival analysis is limited to packages that implement particular models or model classes.

# Key Use-Cases

1. **Fitting and predicting survival models** - This is the first and foremost use-case of the package in its current state, though this will later expand to density estimation and probabilistic regression. By making use of the `mlr3` train/predict methods, users can treat classical survival models just like machine learning ones.
2. **Inspection of fitted survival models** - Whilst different packages in `R` have different ways of inspecting fitted models, this is unified by the `model` field, which is again implemented through `mlr3`.
3. **Tuning of machine leaning models** - Users can make use of `mlr3tuning` [@packagemlr3tuning] with any of the implemented survival models, in order to tune and improve the available ML models.
4. **Evaluation of survival models with transparent measures** - Several `R` packages evaluate survival models however each produces slightly different results and are not transparent. `mlr3proba` implements transparent measures with clear documentation.

# Software Availability

`mlr3proba` is available on [GitHub](https://github.com/mlr-org/mlr3proba) and [CRAN](https://CRAN.R-project.org/package=mlr3proba). It can either be installed from GitHub using the `devtools` [@packagedevtools] library or directly from CRAN with `install.packages`. The package uses the MIT open-source licence. Contributions, issues, feature requests, and general feedback can all be found and provided on the project [GitHub](https://github.com/mlr-org/mlr3proba). Full tutorials and further details are available on the [project website](https://mlr3proba.mlr-org.com/).

# Acknowledgements
`mlr3proba` was originally built from `mlr3survival`, created by Michel Lang. mlr3survival included four learners (coxph, glmnet, ranger, rpart), two measures (harrellc, unoc), and original designs for the survival prediction, learner, and task classes. Since merging `mlr3survival` into `mlr3proba`, these have been slightly modied however much of this original code remains. Design discussions of `mlr3proba` were held with Bernd Bischl, Michel Lang, Jakob Richter, and Martin Binder. Additional contributions have been made by Andreas Bender.

# References