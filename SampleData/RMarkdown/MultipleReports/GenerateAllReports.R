# Generate multiple reports from a single RMarkdown documents

# You need two files.
#   This file which select the data to be analyzed and repeatively calls the Rmarkdown file
#   The RMarkdown document which generates the report


# File 1: Should be an R-Script 
    # contains a loop that iteratively calls an Rmarkdown file (i.e. File 2)

# load packages
library(knitr)
library(markdown)
library(rmarkdown)

cereal <- read.csv('cereal.csv', 
              header=TRUE, as.is=TRUE, strip.white=TRUE)

xtabs(~mfr, data=cereal)

# create the directory for the reports
dir.create("reports")

# for each manufacturer create a report
  # these reports are saved in output_dir with the name specified by output_file
select <- cereal$mfr %in% c("K","G") # we only create two reports

plyr::d_ply(cereal[select ,], "mfr", function (my.cereal, type){
   #browser()
   # create the output file name, but if type is word, must change the ending to docx
   out.filename <- paste("report_", my.cereal$mfr[1], '_', Sys.Date(), ".", 
                                        substr(type,1,-1+regexpr("_", type,fixed=TRUE)), sep='')
   out.filename <- gsub("\\.word$", ".docx", out.filename)
   rmarkdown::render('Report.Rmd',  # file 2
                   output_format=type,
                   output_file = out.filename , 
                   output_dir = 'reports')
}, type="pdf_document")

# You can use word_document,  pdf_document, html_document etc for the type of file.
# Consult the documentation