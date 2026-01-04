---
title: "giniCI: An R Package for Constructing Composite Indicators with 
  Gini-based Weighting Approaches"
tags:
  - R
  - composite indicators
  - Gini coefficient
  - weighting methods
  - ranking comparison
authors:
  - given-names: Viet Duong
    surname: Nguyen
    orcid: 0009-0001-0227-3112
    corresponding: true 
    equal-contrib: true
    affiliation: 1
  - name: Chiara Gigliarano
    orcid: 0000-0003-2861-1316
    equal-contrib: true
    affiliation: 1
  - name: Mariateresa Ciommi
    orcid: 0000-0003-4131-4543
    equal-contrib: true
    affiliation: 2
affiliations:
 - name: Università Carlo Cattaneo -- LIUC, Castellanza (VA), Italy
   index: 1
 - name: Università Politecnica delle Marche, Ancona (AN), Italy
   index: 2
date: 23 December 2025
bibliography: sn-bibliography.bib
---

# Summary
`giniCI` is a package that implements Gini‐based weighting approaches [@cio2017]
for the construction of composite indicators. By integrating advanced
normalization techniques, multiple weighting methods, and ranking comparison 
functionalities, the package provides a transparent tool for solving the 
problem of multidimensional measurement based on distributional dispersion
across individual components. Its modular design enables users to 
seamlessly transition from raw data preprocessing through composite index 
aggregation and comparison analysis, enhancing robustness in benchmarking 
exercises and fostering data-driven decision-making processes in socioeconomic 
studies.

# Statement of Need

A long-standing challenge in composite indicator studies concerns the
justification of weighting schemes. Common strategies often assign
normative weights to all indicators (e.g., equal weights) or rely on
statistical techniques such as principal component analysis [@kla2000;
@nico2000; @grey2017] or data envelopment analysis [@cher2004; @cher2007].
While such approaches have certain strengths, they may fail to adequately 
capture information on the distributional characteristics of the inputs.

`giniCI` introduces a framework that derives weights directly from the Gini
coefficient [@gini1914; @gior2017] of individual dimensions. The Gini
coefficient is a concentration index intended to measure the distance from 
perfect equidistribution. By design, dimensions with greater dispersion 
(or greater homogeneity, in the case of reciprocal weighting) exert a
proportionally higher influence on the aggregated measure. This strategy 
aligns with the recommendations of the Commission on the Measurement of 
Economic Performance and Social Progress, according to which average 
measures should be accompanied by indicators that reflect their
distribution [@stig2009].

The application of Gini coefficient weights has been well-received by 
index developers, as exemplified in the construction of the Human All-Round
Development index [@li2009], the Equitable and Sustainable Well-being (BES)
domains [@cio2017], and the Mobility and Opportunity indices [@che2022].
Through a reproducible implementation of weighting procedures, `giniCI` enables
users to flexibly combine model configuration options and quickly generate
composite indices that account for distributional differences across
dimensions, thereby reflecting the relationship between dimensional
heterogeneity and overall performance in multifaceted contexts.

An additional advantage of Gini-based weighting lies in its computational
efficiency and compatibility with both compensatory and non-compensatory
aggregation methods, making it particularly useful for developers seeking a
lightweight yet robust tool for multidimensional measurement. Unlike other
well-known packages for developing composite indicators---such as `Compind` 
[@vid2025] and `COINr` [@bec2022]---`giniCI` follows a distinct path by
focusing on distribution-based approaches. This hence complements and extends
the existing ecosystem by introducing a statistically grounded foundation
for weighting, together with a toolbox that promote reproducible, transparent,
and reliable index development.

# Features

## Components and Workflow

The overall structure of the `giniCI` package reflects a modular
workflow designed to support the computation, analysis, and validation
of Gini-based composite indicators (\autoref{fig:structure}). The process
begins with the preparation of input data, which may optionally be 
standardized using the `normalize()` function. Once the data are suitably
transformed, the composite index is computed via the central function
`giniCI()`, which implements the Gini-based aggregation framework with a
user-defined parameter setting. The resulting outputs includes composite
scores, dimensional weights, and penalties if horizontal variability
adjustment is selected.

Following the computation of the composite index, the package provides
functionalities for rank-based comparison and visualization. The
`rankComp()` function enables users to compare the rankings derived from
a pair of indices, either obtained through `giniCI` or from external
sources. The accompanying `summary.rankComp()` method produces a set of
descriptive statistics summarizing consistency in ranking between the
reference and the alternative score set. To complement the numerical
summaries, the package includes several plotting functions designed to
enhance interpretability. Together, these tools provide an integrated
environment for both quantitative and graphical analysis.

![Graphical illustration of composite indicator construction using
`giniCI`. The rectangle boxes denotes inputs and outputs, while the
rounded boxes represent package functions. \label{fig:structure}](diagram.pdf){width=70%}

## Data Normalization

The `normalize()` function standardizes input variables so that they are
comparable in scale and direction of contribution to the composite index. Two methods
are available: `"min–max"`, which rescales each variable to the [0,1] range,
and `"goalpost"` [@maz2016], which maps indicators to a user-defined range
(default [70,130]) with the midpoint as their reference values. Both methods
account for the polarity of indicators and can incorporate a temporal
reference, allowing longitudinal normalization relative to a baseline time.

The following example demonstrates the normalization process for the dataset
`bli`, which includes eleven well-being indicators for 36 OECD countries
spanning the years 2014 to 2017 [@oecddb2024]. For longitudinal data, it
is recommended to use a specific reference time during normalization and
weighting. This ensures that normalization goalposts and weights remain
consistent (even when future data is added), enabling unbiased comparisons
over time. 
``` {.R bgcolor="bg"}
data(bli)
# Indicators' polatity
bli.pol <- c("neg", "pos", "pos", "pos", "pos", "neg",
             "pos", "pos", "pos", "neg", "pos")
# Goalpost normalization with 2014 as the reference time  
bli.norm.2014 <- normalize(inds = bli[, 3:13],
                           method = "goalpost",
                           ind.pol = bli.pol,
                           time = bli$YEAR, ref.time = 2014)
```

##  Composite Index Agggregation

The core function `giniCI()` aggregates normalized data into a single index
using alternative weighting schemes. Users can choose among three options:
`"equal"` (equal weights), `"gini"` (Gini-based weights), and `"reci"`
(reciprocal Gini-based weights). Aggregation can be performed using either 
the arithmetic or the geometric mean, with an option for horizontal 
variability adjustment (`hv = TRUE`) to penalize within-unit 
imbalance [@demu2011]. This ensures that the resulting index reflects both
inter-unit inequality and intra-unit dispersion. Below, we present a code snippet
for generating well-being (positive-polarity) composite indicators using
the Gini-based weighted arithmetic aggregation and the reciprocal Gini-based
weighted geometric aggregation, based on the normalized inputs from the previous
step.

``` {.R bgcolor="bg"}
bli.gini <- giniCI(bli.norm.2014,
                   method = "gini", ci.pol = "pos",
                   time = bli$YEAR, ref.time = 2014)
bli.reci <- giniCI(bli.norm.2014,
                   method = "reci", agg = "geo", ci.pol = "pos",
                   time = bli$YEAR, ref.time = 2014)       
```

### Ranking Comparison and Visualization

The ranking comparison functionality in the package is designed to evaluate the
consistency between a reference index and an alternative index. The function
`rankComp()` performs a ranking shift analysis and summarizes changes through 
key measures such as the average shift in ranking, the percentage of
equal rankings, and the average shift in quantile rankings [@mar2024].   

``` {.R bgcolor="bg"}
ci.gini <- giniCI(bli.norm.2014, method = "gini",
                  ci.pol = "pos", time = bli$YEAR,
                  ref.time = 2014, only.ci = TRUE)
ci.reci <- giniCI(bli.norm.2014, method = "reci", agg = "geo",
                  ci.pol = "pos", time = bli$YEAR,
                  ref.time = 2014, only.ci = TRUE)
ci.comp <- rankComp(ci.gini, ci.reci,
                    id = bli$COUNTRY, time = bli$YEAR)
                    summary(ci.comp)
##  Number of ranked units: 
##  2014 2015 2016 2017 
##    36   36   36   36 
##  
##  Ranking shift summary statistics: 
##         2014 2015 2016 2017
##  Min.     -3   -4   -6   -5
##  1st Q.   -1   -1   -1   -2
##  Median    0    0    0    0
##  Mean      0    0    0    0
##  3rd Q.    1    1    1    1
##  Max.      5    4    5    4
##  
##  Average shift in ranking: 
##             2014  2015  2016  2017
##  All units 1.333 1.278 1.111 1.611
##  Top 10    1.100 1.100 1.100 1.300
##  Bottom 10 1.600 1.200 0.400 1.000
##  
##  Percentage of equal rankings: 
##             2014  2015  2016  2017
##  All units 22.22 36.11 41.67 22.22
##  Top 10    30.00 40.00 40.00 30.00
##  Bottom 10 10.00 30.00 70.00 40.00
##  
##  Average shift in 10-quantile ranking:
##     2014   2015   2016   2017
##   0.2222 0.3889 0.3889 0.3889  
```

For visualization, three functions `rankScatterPlot()`, `rankShiftPlot()`,
and `rankRankPlot()` graphically illustrate ranking stability and movement
across time or computational settings, highlighting the impact of parameter
configurations on index performance (\autoref{fig:rank_scatter}, 
\autoref{fig:rank_shift}, and \autoref{fig:rank_rank}). These
functions return a plot object (or a list of plot objects if temporal
factors are present), which can be stored and printed. The plots are
customizable, allowing users to adjust colors, sizes, shapes, and label
displays to meet the desired results.

![Rank scatter plot for two ranking systems in 2014. The figure illustrates
the relationship between two rankings using a two-dimensional scatter plot.
A 45-degree reference line can be added to  facilitate the classification of
ranking changes. Units located below the reference line indicate an improvement
in performance, whereas those above the line indicate a deterioration. Units
positioned on the reference line have identical rankings in both indices.
\label{fig:rank_scatter}](p1.pdf){width=60%}

![Rank shift plot for two ranking systems in 2015. The figure depicts changes
in ranking by representing each unit as a pair of vertically aligned points. 
The first point (default: black-bordered circle) corresponds to the unit's 
position in the reference ranking, and the second point (default: solid red
circle) corresponds to its position in the alternative ranking. These points
are connected by a line segment, allowing users to identify both the direction
and magnitude of ranking shifts. When the reference and alternative points
overlap, the unit’s ranking remains unchanged.
\label{fig:rank_shift}](p2.pdf){width=60%}

![Rank-rank plot for two ranking systems in 2016. The figure arranges 
two ranking systems side by side and uses connecting lines to visualize how
the position of each unit changes between them. Upward-sloping segments 
indicate an improvement in the alternative ranking compared to the reference
ranking, while downward-sloping segments indicate a decline. The length of
non-horizontal segments represent the magnitude of ranking shifts, with longer
segments highlighting more substantial changes in position.
\label{fig:rank_rank}](p3.pdf){width=60%}

# Licensing and Availability

`giniCI` is licensed under the GNU General Public License (GPL, version 3.0).
The source code is publicly available on GitHub (https://github.com/novidu/giniCI),
where users can also access the corresponding issue tracker.


# Acknowledgements {#acknowledgements .unnumbered}

This work was supported by the Fondazione Cariplo, under the project
"MultiLocal: Multidimensional inequality and optimization in a local
perspective" [Rif. 2022-1548].

# References
