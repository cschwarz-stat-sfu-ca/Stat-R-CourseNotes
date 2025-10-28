This is a zip file of the boundaries of the forward sortation areas downloaded from Statistics Canada.

Due to the file size, it is stored as zip file with the following files inside:

 gpr_000b11a_e.dbf
 fsa000b16a_e.prj
 gpr_000b11a_e.shp
 gpr_000b11a_e.shx

Normally you would access the *.shp file using st_read("blah.shp") to access the boundary file. But, rather than unzipping the file, you can read directly from the *.zip file using

canada.map.dir <- tempdir()
unzip(file.path("..","sampledata","Climate-Daily-Canada","CanadaMap","gpr_000b11a_e.zip"),  exdir=canada.map.dir)
fsa <- sf::st_read(file.path(canada.map.dir,"gpr_000b11a_e.shp"), stringsAsFactors=FALSE)


This creates a temporary directory, unzips to it, and then reads from the temporary directory. The temporary directory will be deleted at the end of the R session.

