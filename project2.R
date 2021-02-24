library(vcd)
library(hflights)
library(ggplot2)
library(dplyr)
library(tidyr)

setwd("D:/mohit gate/edvancer/edvancer pdf & codes/data_project_2")

store_train=read.csv("store_train.csv",stringsAsFactors = F)
store_test=read.csv("store_test.csv",stringsAsFactors = F)

store_test$store=NA

glimpse(store_train)
glimpse(store_test)

store_train$data="train"
store_test$data="test"

store=rbind(store_train,store_test)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories)
  
  data[,var]=NULL
  return(data)
}


store$Id=NULL
glimpse(store)


# df=data.frame(months=c('jan','feb','mar','apr','jun','jul','aug','sep','oct','nov','dec'),
#               sales=c(10,8,11,13,9,7,14,15,18,20,12))
# p=ggplot(df,aes(x=months,y=sales,fill=months))+
#   geom_bar(stat="identity")

#data prep-----research on train, apply on full
#in full datasets lots of nas so see ggplot in train but not in full
#research on train change on full (for making categories)

#storecode
store$storecode=substr(store$storecode,1,5)
ggplot(store_train,aes(x=storecode1,color=factor(store)))+
geom_bar()

ggplot(store_train,aes(x=country,color=factor(store)))+
  geom_bar()


store = store %>% 
  mutate(storecode=ifelse(storecode=="METRO",1,0))

glimpse(store)

#state
table(store_train$State)
# state.count.L10=c(rownames(x<10))
# store[which(store$State %in% as.numeric(state.count.L10)),"county"]=10
store=CreateDummies(store ,"State",25)

#countyname
#change
store=store %>% 
  separate(countyname,into=c("county","extraname"),sep="\\ ") %>% 
  select(-extraname)
#research
ggplot(store,aes(x=county,color=factor(store)))+
  geom_bar()
store=CreateDummies(store,"county",20)
county_county=as.data.frame(table(store$county))
mean(county_county$freq,na.rm = T)
summary(county_county)

# class(county_county)
# county_county[[1]][1]
# min(county_county$Freq)
# county_1time=county_county[which(county_county$Freq<5),"Var1"]
# county_county[county_county$Freq]

store=store %>% 
  separate(countyname,into=c("county","extraname"),sep="\\ ") %>% 
  select(-extraname)
x=table(store$county)
sort(x,decreasing = T)
L10=c(rownames(x[x<10]))
store[which(store$county %in% L10),"county"]=10

# county.extra=c(rownames(county.extra1[county.extra1<7]))
#  store_train[which(store_train$county %in% county.extra),"county"]="Extra"
# store_train = store_train %>%
#   mutate(status_div=as.numeric(status=="Divorced/Separated"),
#          status_partner=as.numeric(status=="Partner"),
#          status_single=as.numeric(status=="Single/Never Married")) %>%
#   select(-status)
table(store_train$county)
ggplot(store_train,aes(x=county,color=factor(store)))+
  geom_bar()

#store_Type
table(store_train$store_Type)
store=CreateDummies(store ,"store_Type",0)

glimpse(store_train)
#state_alpha      not imp remove
table(store_train$state_alpha)
ggplot(store_train,aes(x=state_alpha,color=factor(store)))+
  geom_bar()
#store=CreateDummies(store ,"state_alpha",25)
store$state_alpha=NULL

#areaname and county

nrow(store_train[store_train$Areaname==store_train$county,])
store_train[,c("Areaname","county")]
table(store_train$Areaname)
store$Areaname=NULL    #it's mix of county name and state name
 
store=store %>% 
   separate(Areaname,into=c("first","second"),sep=",") 
 store=store %>%  select(-first)
 store=CreateDummies(store ,"second",50)
 table(store_train$State)
 ggplot(store_train,aes(x=State,color=factor(store)))+
   geom_bar()


#countytownname
#store$countytownname=NULL
table(store$countytownname)
ggplot(store,aes(x=countytownname,color=factor(store)))+
  geom_bar()
store=CreateDummies(store ,"countytownname",7)
#store=CreateDummies(store ,"countytownname",)

# countytown.name1=table(store_train$countytownname)
# county.name=c(rownames(county.extra1[county.extra1<7]))
# store_train[which(store_train$county %in% county.extra),"county"]="Extra"
# 
# store_train=store_train %>% 
#   separate(countytownname,into=c("county1","townname"),sep="\\ ")%>% 
#   select(-townname)
# 
# table(store_train$county1)

# 
# 
# ggplot(store_train,aes(x=county1,color=factor(store)))+
#   geom_bar()
# 
# 
# countytownname=table(store_train$county1)
# #rownames(which(county1name>1))
# 
# 
# countytown.extra1=c(rownames(countytownname[countytownname=1]))
# countytown.extra2=c(rownames(countytownname[countytownname=2]))
# countytown.extra3=c(rownames(countytownname[countytownname=3]))
# countytown.extra3=c(rownames(countytownname[countytownname>3]))
# 
# store_train[which(store_train$county1 %in% countytown.extra1),"county1"]="Extra1"

#country
table(store$country)
store=CreateDummies(store ,"country",35)
#use numeric here
summary(store$country)
ggplot(store,aes(x=country,color=factor(store)))+
  geom_bar()

names(store)[sapply(store, function(x) is.character(x))]

glimpse(store)

# cn=table(store_train$countyname,store_train$store)
# cn=data.frame(cn)
# cn5=cn[cn$Freq>5,]
# 
# ggplot(cn5,aes(x=Var1,y=Freq))+geom_bar(stat = "identity",aes(fill=factor(Var2)))
# #use country as numerical
# 
# ggplot(store_train,aes(x=store))



## separate train and test

glimpse(store)
store$store=as.numeric(store$store==1)
train=store %>% filter(data=='train') %>% select(-data)
test=store %>% filter(data=='test') %>% select(-data)
table(test$data)
store=store %>% 
  select(-countytownname,-Areaname)

glimpse(train)



 set.seed(2)
 s=sample(1:nrow(train),0.7*nrow(train))
 
 s_train=train[s,]
 s_test=train[-s,]
 
 glimpse(s_train)


library(car)
 table(store$store)

for_vif=lm(store~.-score1-sales0-sales2-sales3-county_Jackson-State_23,data=s_train)
sort(vif(for_vif),decreasing = TRUE)[1:3]
train=train %>% 
  select(-(contains(state_alpha)))

rg_fit=s_train %>%
  select(-sales0,-sales2,-sales3,-county_Jackson,-State_23)

#modeling
log_fit=glm(for_vif,family = "binomial")
log_fit=step(log_fit)
formula(log_fit)
summary(log_fit)

log_fit=glm(store ~ (sales0 + sales2 + sales3 + 
                       storecode + 
                       State_53 + 
                       State_47 + State_37 + State_17 + State_20 + 
                       State_50 + State_23 + county_Orleans + 
                       county_Addison + 
                       county_Jackson + county_Orange + 
                       county_Windsor + county_Bristol + county_Litchfield + 
                       county_Windham + 
                       country_39 + 
                       country_13 + 
                       countytownname_ButlerCounty) - 
              sales0 - sales2 - sales3 - county_Jackson - State_23,data=train,family = "binomial")
summary(log_fit)

train$score1=predict(log_fit,newdata=train,type = "response")

## ----message=F,warning=F-------------------------------------------------
library(ggplot2)
ggplot(train,aes(y=store,x=score1,color=factor(store)))+
  geom_point()+geom_jitter()

## ------------------------------------------------------------------------
cutoff=0.2
predicted=as.numeric(rg_train$score>cutoff)
TP=sum(predicted==1 & rg_train$Revenue.Grid==1)
FP=sum(predicted==1 & rg_train$Revenue.Grid==0)
FN=sum(predicted==0 & rg_train$Revenue.Grid==1)
TN=sum(predicted==0 & rg_train$Revenue.Grid==0)

# lets also calculate total number of real positives and negatives in the data 
P=TP+FN
N=TN+FP

# total number of observations
total=P+N


## ------------------------------------------------------------------------
cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=seq(0,1,length=100)


# lets remove the dummy data cotaining top row
for (cutoff in cutoffs){
  predicted=as.numeric(train$score1>cutoff)
  TP=sum(predicted==1 & train$store==1)
  FP=sum(predicted==1 & train$store==0)
  FN=sum(predicted==0 & train$store==1)
  TN=sum(predicted==0 & train$store==0)
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN))
cutoff_data=cutoff_data[-1,]
}
## ------------------------------------------------------------------------
cutoff_data=cutoff_data %>%
  mutate(Sn=TP/P, Sp=TN/N,dist=sqrt((1-Sn)**2+(1-Sp)**2),P=FN+TP,N=TN+FP) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  mutate(Accuracy=(TP+TN)/(P+N)) %>%
  mutate(Lift=(TP/P)/((TP+FP)/(P+N))) %>%
  mutate(M=(8*FN+2*FP)/(P+N)) %>%
  select(-P,-N)


## ----warning=F,message=F
## ------------------------------------------------------------------------
library(tidyr)
cutoff_viz=cutoff_data %>%
  select(cutoff,Sn,Sp,dist,KS,Accuracy,Lift,M) %>%
  gather(Criterion,Value,Sn:M) 

ggplot(filter(cutoff_viz,Criterion!="Lift"),aes(x=cutoff,y=Value,color=Criterion))+
  geom_line()

#We'll visualise lift separately because of its scale

cutoff_viz %>%
  filter(Criterion=="Lift") %>%
  ggplot(aes(x=cutoff,y=Value,color=Criterion))+geom_line()


test$score=predict(log_fit,newdata = test,type = "response")
st=test$score
test1 = test %>% 
  mutate(store1=ifelse(score>0.43,1,0))

write.csv(test1$store1,"mohit_mudgal_P2_part2.csv",row.names = F)
table(s_test$store,as.numeric(s_test$score>KS_cutoff))
library(pROC)
auc(roc(s_test$store,as.numeric(s_test$score>KS_cutoff)))
# lets also calculate total number of real positives and negatives in the data 
P=TP+FN
N=TN+FP
## ------------------------------------------------------------------------
#Cutoff with minimum KS:
KS_cutoff=cutoff_data$cutoff[which(cutoff_data$KS==max(cutoff_data$KS))][1]
KS_cutoff
table(s_test$store,as.numeric(s_test$score>KS_cutoff))

quiz=store_train %>% 
  select(store_Type,Areaname,contains("sales")) %>% 
  filter(store_train,Areaname="Kennebec County,ME")

quiz=filter(store_train,Areaname=="Kennebec County, ME"& store_Type=="Supermarket Type1")
which(store_train$Areaname=="Kennebec County,ME")
table(store_train$store_Type)
sum(quiz[1:nrow(quiz),"sales0"]+quiz[1:nrow(quiz),"sales1"]+quiz[1:nrow(quiz),"sales2"]+quiz[1:nrow(quiz),"sales3"]+quiz[1:nrow(quiz),"sales4"])
#sum(unique(store_train$Areaname))
lapply(store_train,function(x) length(unique(x)))
prop.table(table(store_train$store_Type,store_train$store),1)

lapply(store_train,function(x) sum(is.na(x)))
ggplot(store_train,aes(x=sales0))+geom_density(color="red")+
  geom_histogram(aes(y=..density..),alpha=0.5)+
  stat_function(fun=dnorm,args=list(mean=mean(store_train$sales0),sd=sd(store_train$sales0)),color="green")

#store_train$total_sales=sum(store_train$sales0+store_train$sales1+store_train$sales2+store_train$sales3+store_train$sales4)
for(i in 1:nrow(store_train)){
store_train[i,"total_sales"]=store_train[i,"sales0"]+store_train[i,"sales1"]+store_train[i,"sales2"]+store_train[i,"sales3"]+store_train[i,"sales4"]
}

p=ggplot(store_train,aes(x=total_sales,y=total_sales))+geom_boxplot()
summary(store_train$total_sales)
IQR(store_train$total_sales)
max=4969+1.5*(1546.75)
which(store_train$total_sales>max)
library(tidyr)
cutoff_viz=cutoff_data %>%
  select(cutoff,Sn,Sp,dist,KS,Accuracy,Lift,M) %>%
  gather(Criterion,Value,Sn:M) 

ggplot(filter(cutoff_viz,Criterion!="Lift"),aes(x=cutoff,y=Value,color=Criterion))+
  geom_line()
var(store_train$total_sales)
tapply(store_train$total_sales,store_train$store_Type,var)
