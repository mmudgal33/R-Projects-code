getwd()
setwd("D:/mohit gate/edvancer/34-9")
file_names=list.files(pattern = "*.txt")
files=lapply(file_names,read.csv)

#df=read.table("23-1-1.txt", sep = ".")
tr = do.call(as.data.frame(t(),files)
library(stringr)

my.data=tr
#my.data <- tr[,1]
my.data.num <- as.numeric(str_extract(my.data, "[0-9]+"))
my.data.num
my.data.cha <- (str_extract(my.data, "[aA-zZ]+"))
num=my.data.num
num=paste0(num,".")
num
cha=my.data.cha
cha=cha[-1]
row=nrow(num)
num=num[-row]
d=data.frame(num,cha)
as.factor(d$cha)
write.table(d,"d.txt",sep="\t",row.names=FALSE,col.names=F,quote = FALSE)