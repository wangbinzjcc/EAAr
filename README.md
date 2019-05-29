# EAAr

An R package for **Equal Area Annulus (EAA) neighborhood analysis method**.

## Introduction

The EAA method (Wills, et al. 2016; 2019) combines spatial point-pattern and individual-based neighborhood models. It is capable of refining and extending the research results of traditional neighborhood models at different spatial and phylogenetic distances. This method can be used to examine in detail many of the interactions between ecological factors and evolutionary divergence in forest diversity plots.

Most recently we examine data from 16 globally distributed forest dynamics plots (FDPs), using the equal-area-annulus (EAA) method. This method obtains detailed information on interactions between focal trees and trees that occupy concentric equal-area annuli around the focal trees. The interactions affect growth, clustering, recruitment, and mortality, and are consistent with the presence of negative density-dependent effects. Their strength increases smoothly with physical proximity, but fluctuates in a pattern unique to each FDP with increasing phylogenetic proximity. We show how one of these unique patterns, in the temperate Wind River FDP, can provide new information about a known allelopathic interaction. Such species-species interactions, along with convergent and divergent coevolutionary processes that may be uncorrelated with overall phylogenetic separation, contribute to these unique effects. EAA can be used to test Darwin’s prediction that unique patterns of coevolution in different ecosystems can be chiefly traced to interactions among each ecosystem’s unique mix of species.

## Authors

Bin Wang <wangbinzjcc@qq.com>

Christopher Wills <cwills@ucsd.edu>

## Installation

```r

devtools::install_github("wangbinzjcc/EAAr")

```

## Usage

```r

library(EAAr)

?EAAr
```


## References
Wills, C., K. E. Harms, T. Wiegand, R. Punchi-Manage, G. S. Gilbert, D. Erickson, W. J. Kress, S. P. Hubbell, C. V. S. Gunatilleke, I. A. U. N. Gunatilleke, 2016. Persistence of Neighborhood Demographic Influences Over Long Phylogenetic Distances May Help Drive Post-Speciation Adaptation in Tropical Forests. PLOS ONE | DOI: 10.1371/ journal.pone.0156913.

Wills, C., B. Wang, et al., 2019. In Forests Worldwide a Tree’s Influence on Neighbors Increases Steadily with Proximity and Idiosyncratically with Phylogenetic Relatedness: Tree Phylogenies Influence World's Forests.
