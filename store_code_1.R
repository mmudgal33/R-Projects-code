setwd("D:/mohit gate/edvancer/edvancer pdf & codes/data_project_1")

s_train=read.csv("store_train.csv",stringsAsFactors = F)


h_train=read.csv("store_test.csv",stringsAsFactors = F)
h_train$pr=h_train$Price
h_train$Price=NULL
h_train$price=h_train$pr
h_train$pr=NULL


s_test=read.csv("store_test.csv",stringsAsFactors = F)