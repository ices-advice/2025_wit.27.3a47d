## Extract results of interest, write TAF output tables

## Before: model.RData
## After:

library(icesTAF)
library(stockassessment)

mkdir("output")
mkdir("report")

source("output_helpers.R")
load("model/model.RData")

## Make standard intercatch plots

source("InterCatch_Output_Analysis_script.r")
