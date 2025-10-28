This is a zip file of the boundaries of the forward sortation areas downloaded from Statistics Canada.

Due to the file size, it is stored as zip file with the following files inside:

 lfsa000b16a_e.dbf
 fsa000b16a_e.prj
 lfsa000b16a_e.shp
 lfsa000b16a_e.shx

Normally you would access the *.shp file using st_read("blah.shp") to access the boundary file. But, rather than unzipping the file, you can read directly from the *.zip file using

fsa.dir <- tempdir()
unzip(file.path("..","sampledata","2016-census","FSA","lfsa000b16a_e.zip"),  exdir=fsa.dir)
fsa <- sf::st_read(file.path(fsa.dir,"lfsa000b16a_e.shp"), stringsAsFactors=FALSE)


This creates a temporary directory, unzips to it, and then reads from the temporary directory. The temporary directory will be deleted at the end of the R session.


You can find the grouping of FSA by city at
https://www.canadapost.ca/cpo/mc/assets/pdf/business/nps/machineable_presort_fsalist_february2014.pdf