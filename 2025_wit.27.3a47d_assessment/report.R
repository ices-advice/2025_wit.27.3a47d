## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)
library(stockassessment)
library(rmarkdown)

mkdir("report")
cp("boot/initial/report/*", "report/")

outdir <- "report/"

output_format <- NULL # "all"
quiet <- FALSE

icesTAF::msg("Report: Making catch/assessment presentation")
render("report/wit.27.3a47d_catch_assessment.Rmd", 
       output_format = output_format,
       clean=TRUE, quiet = quiet) #, output_dir = outdir)

render("report/wit.27.3a47d_forecast.Rmd", 
       output_format = output_format,
       clean=TRUE, quiet = quiet) #, output_dir = outdir)
       
render("report/Advicesheet_catch_scenario_table.Rmd", output_dir = outdir,
       output_format = output_format,
       clean=TRUE, quiet = quiet)

source("report/wit.27.3a47d.SAGupload.r")
