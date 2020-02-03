---
title: "Exploring, Describing and Reporting the Parameters of Statistical Models using R"
authors:
- affiliation: 1
  name: Daniel Lüdecke
  orcid: 0000-0002-8895-3206
- affiliation: 2
  name: Mattan S. Ben-Shachar
  orcid: 0000-0002-4287-4801
- affiliation: 3
  name: Indrajeet Patil
  orcid: 0000-0003-1995-6531
- affiliation: 4
  name: Dominique Makowski
  orcid: 0000-0001-5375-9967

date: "07 Junuary 2020"
output: pdf_document
bibliography: paper.bib
csl: apa.csl
tags:
- R
- Bayesian statistics
- rstan
- eaystats
- posterior distribution
- Region of practical equivalence
- ROPE
- probability of direction
- Bayes factor
affiliations:
- index: 1
  name:  University Medical Center Hamburg-Eppendorf, Germany
- index: 2
  name: Ben-Gurion University of the Negev, Israel
- index: 3
  name: Max Planck Institute for Human Development, Germany
- index: 4
  name: Nanyang Technological University, Singapore
---

# Introduction

The recent growth of data science that is transforming modern societies is partly fuelled by the ever-growing amount of data and the joint important developments in statistical modelling. Previously reserved to a handful of statisticians and expert data scientists, new and powerful models and frameworks are becomming accessible to a new type of users. Often lacking a formal background in statistics, these users are facing with a new challenge. 

How to best use, explore, understand, describe and report these models.

parameters’ primary goal is to provide utilities for processing the parameters of various statistical models. Beyond computing p-values, standard errors, confidence intervals, Bayesian indices and other measures for a wide variety of models, this package implements features like bootstrapping of parameters and models, feature reduction (feature extraction and variable selection).


It relies on the **insight** and the **bayestestR** packages [@ludecke2019insight; @makowski2019bayestestR] to access and process information contained in models.

# Examples of Features



## Parameters Tables

## Visualisation


**parameters** functions also include plotting capabilities via the [**see** package](https://easystats.github.io/see/) [@ludecke2019see].




# Licensing and Availability

**parameters** is licensed under the GNU General Public License (v3.0), with all source code stored at GitHub (https://github.com/easystats/parameters), and with a corresponding issue tracker for bug reporting and feature enhancements. In the spirit of honest and open science, we encourage requests/tips for fixes, feature updates, as well as general questions and concerns via direct interaction with contributors and developers.

# Acknowledgments

**parameters** is part of the [*easystats*](https://github.com/easystats/easystats) ecosystem, a collaborative project created to facilitate the usage of R. Thus, we would like to thank the [members of easystats](https://github.com/orgs/easystats/people) of easystats as well as the users.

# References
