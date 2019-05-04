# Stat 133, Spring 2019

Private Repository for HW assignments of Stat 133 (Spring 2019)

- Name: Francesco Dalla Ricca
- Github username: FrancescsoDallaRicca
- Email: address francescodr@berkeley.edu
- Lab section: 109
- GSI: Miyabi Ishihara

-----

## Overview

Binomial is a package that has functions to run statistics on binomial distribution  and/or make graphs to better visualize the staistics

## Motivation

The package has been developed because of an assignment and to understand the concepts of the creation of packages.

## Usage

``` r
library(Binomial)

bindis <- bin_distribution(trials = 5, prob = 0.5)
bindis
#  success probability
#1       0     0.03125
#2       1     0.15625
#3       2     0.31250
#4       3     0.31250
#5       4     0.15625
#6       5     0.03125
plot(bindis)
#Plot of binomial distribution

cumdis <- bin_cumulative(trials = 5, prob = 0.5)
cumdis
#  success probability cumulative
#1       0     0.03125    0.03125
#2       1     0.15625    0.18750
#3       2     0.31250    0.50000
#4       3     0.31250    0.81250
#5       4     0.15625    0.96875
#6       5     0.03125    1.00000
plot(cumdis)

bin_choose(n = 5, k = 2)
#10
bin_choose(n = 5, k = 1:3)
#5 10 10

bin_probability(success = 2, trials = 5, prob = 0.5)
#0.3125
bin_probability(success = 1:3, trials = 5, prob = 0.5)
#0.15625 0.31250 0.31250

bin_variable(trials = 10, prob = 0.5)
#Binomial variable 
# 
# Parameters 
# - number of trials:  10 
# - prob of success :  0.5
```
