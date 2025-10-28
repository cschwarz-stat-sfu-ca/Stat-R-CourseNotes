# Odd behaviour with a string. Notice that the Acc_Index first column is read a character string

test.csv <- textConnection("
Acc_Index, Vehicle_Reference, Vehicle_Type
20100170003,   1,           19
20100170003,   2,            1
201001BS70004,   1,            9
201001BS70006,   1,           20
201001BS70006,   2,            1")

test.data <- read.csv(test.csv, as.is=TRUE, strip.white=TRUE)
test.data
str(test.data)

# but look what happens when I use ddply on it
library(plyr)
plyr::ddply(test.data, "Acc_Index", plyr::summarize,
            n.veh = length(Acc_Index))
