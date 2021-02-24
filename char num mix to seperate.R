# load library
library(stringr)
#
# load data
my.data <- c("aaa", "b11", "b21", "b101", "b111", "ccc1", "ddd1", "ccc20", "ddd13")
#
# extract numbers only
my.data.num <- as.numeric(str_extract(my.data, "[0-9]+"))
#
# check output
my.data.num
[1]  NA  11  21 101 111   1   1  20  13
#
# extract characters only
my.data.cha <- (str_extract(my.data, "[aA-zZ]+"))
# 
# check output
my.data.cha
[1] "aaa" "b"   "b"   "b"   "b"   "ccc" "ddd" "ccc" "ddd"


#remove a value in vector
a[!a=NUMBER_TO_REMOVE]
a[-1]#remove by position

names(tr)="col"
typeof(col)
as.character(col)
for(i in tr[,1]){
  col=gsub(" ","",col)
  
}

