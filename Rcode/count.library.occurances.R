# Find out what packages are used in R course notes

system('rm temp1')
system('rm temp2')
system('grep -iR "library(" *.R >temp1 ')
system('grep -iR "library(" *.r >temp2 ')

# get the libraries
r1 <- read.table('temp1', stringsAsFactors=FALSE, sep="\n")
r2 <- read.table('temp2', stringsAsFactors=FALSE, sep="\n")

libs <- rbind(r1, r2)
head(libs)



libs$lib1 <- substring(libs$V1, 1+regexpr(":", libs$V1, fixed=TRUE))
libs$lib2 <- substr(libs$lib1, 1+regexpr("(", libs$lib1, fixed=TRUE),-1+regexpr(")", libs$lib1, fixed=TRUE))
head(libs)

lib.freq <- as.data.frame(xtabs(~lib2, data=libs))
lib.freq <- lib.freq[ order(lib.freq$lib),]
lib.freq

write.csv(lib.freq, 
          "Library-frequency.csv")
system('rm temp1')
system('rm temp2')


lib.freq
select <- grepl("unmarked", lib.freq$lib2)|
          grepl("grep"    , lib.freq$lib2)|
          grepl("as.char" , lib.freq$lib2)
lib.freq <- lib.freq[! select,]
lib.freq

plyr::a_ply(lib.freq, 1, function(x){
     cat(x$lib2, "\n");
     #browser()
     library(as.character(x$lib2), character.only=TRUE)
  })
