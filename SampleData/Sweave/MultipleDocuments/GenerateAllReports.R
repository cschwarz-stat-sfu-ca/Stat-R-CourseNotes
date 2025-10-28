# Generate multiple reports from a single Sweave documents

# You need two files.
#   This file which select the data to be analyzed and repeatively calls the Rmarkdown file
#   The RMarkdown document which generates the report


# File 1: Should be an R-Script 
    # contains a loop that iteratively calls an Rmarkdown file (i.e. File 2)

# load packages
library(knitr)
library(markdown)
library(rmarkdown)
library(brew)

cereal <- read.csv('cereal.csv', 
              header=TRUE, as.is=TRUE, strip.white=TRUE)

xtabs(~mfr, data=cereal)

# create the directory for the reports
dir.create("reports")

IndivReport <- function(cereal){
   # Create Sweave file names for individual report. 
   Knitfile <- file.path("reports",paste(cereal$mfr[1],".Rnw", sep=""))
   Knitfile <- gsub(" ","-", Knitfile)
   cat("Creating report for ", Knitfile, "\n")
   #browser()
   # Brew and knit (i.e. create the separate Sweave files and then generate report)
   brew::brew("Report.rnw", Knitfile)
   reportdir <- file.path("reports")
   opts_knit$set(base.dir = reportdir)
   reportfile <- gsub(".Rnw",".tex", Knitfile)
   knitr::knit2pdf(Knitfile, reportfile)
}

# Generate report for the separate manufacturers


plyr::d_ply(cereal[ cereal$mfr %in% c("K","G"),], "mfr", IndivReport)  # individual reports for each mfr
