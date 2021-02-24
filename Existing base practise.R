library(dplyr)

setwd("D:/mohit gate/edvancer")
rg=read.csv("Existing Base.csv",stringsAsFactors = FALSE)

table(rg$children)
glimpse(rg)




CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("''","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

glimpse(ld_all)



table(rg$children)
rg=rg %>% 
  mutate(children=ifelse(children=="Zero",0,substr(children,1,1)), 
  children=as.numeric(children))

table(rg$age_band)
#round(prop.table(table(rg$age_band,rg$Revenue.Grid),2)) means round not apply on prop.table
prop.table(table(rg$age_band,rg$status),1)

# rg=rg %>% 
#   mutate(a1=as.numeric(substr(age_band,1,2)),
#          a2=as.numeric(substr(age_band,1,2)),
#          age=ifelse(age_band=="71",71,ifelse(age_band=="unknown",NA,0.5*(a1+a2)))) %>% 
#   select(-a1,-a2,-age_band)
# rg=rg %>% select(age) %>% na.omit

lapply(rg,function(x) length(unique(x)))
table(rg$age_band)

rg=rg %>% 
  mutate(ag1=as.numeric(age_band=="61-65"),
         ag2=as.numeric(age_band %in% c("45-50","51-55","65-70","71+")),
         ag3=as.numeric(age_band %in% c("55-60","41-45","31-35","26-30","22-25")),
         ag4=as.numeric(age_band=="36-40"),
         ag5=as.numeric(age_band=="18-21")) %>% 
  select(-age_band)

rg=rg %>% select(-post_code,-post_area)
names(rg)[sapply(rg, function(x) is.character(x))]

table(rg$family_income)
cat_col= c("status","occupation","occupation_partner","home_status","family_income",
   "self_employed","self_employed_partner","TVarea","gender","region")
for(cat in cat_col)
glimpse(rg)
sum(sapply(rg,function(x) is.character(x)))

rg$Revenue.Grid=as.numeric(rg$Revenue.Grid==1)



#performance check, break data
s=sample(1:nrow(rg),0.7*nrow(rg))
rg_train=rg[s,]
rg_test=rg[-s,]


#VIF
library(car)
for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity-region_SouthEast-ag3-TVarea_Central
           -occupation_Professional-`family_income_>=35,000`-region_Scotland
           -Portfolio.Balance,data=rg_train)
sort(vif(for_vif),decreasing = TRUE)[1:3]
glimpse(rg)
rg_fit=rg_train %>%
  select(-REF_NO,-Investment.in.Commudity,-Investment.in.Derivative,-Investment.in.Equity,
         -region_SouthEast,-ag3,-TVarea_Central,-occupation_Professional,
         -`family_income_>=35,000`,-region_Scotland,-Portfolio.Balance)

#modeling
log_fit=glm(for_vif,family = "binomial")
log_fit=step(log_fit)
summary(log_fit)
formula(log_fit)
log_fit=glm(Revenue.Grid ~ (REF_NO + Average.Credit.Card.Transaction + 
                              Balance.Transfer + Term.Deposit + Life.Insurance + Medical.Insurance + 
                              Average.A.C.Balance + Personal.Loan + Investment.in.Mutual.Fund + 
                              Investment.Tax.Saving.Bond + Home.Loan + Online.Purchase.Amount + 
                              Investment.in.Commudity + Investment.in.Equity + Investment.in.Derivative + 
                              Portfolio.Balance + ag3 + 
                              status_Partner + occupation_Professional + 
                              `family_income_LT_30,000,>=27,500` + 
                              `family_income_>=35,000` + 
                              TVarea_Meridian + TVarea_Central) - 
              REF_NO - Investment.in.Commudity - Investment.in.Derivative - 
              Investment.in.Equity - region_SouthEast - ag3 - TVarea_Central - 
              occupation_Professional - `family_income_>=35,000` - region_Scotland - 
              Portfolio.Balance,data=rg_train,family = "binomial")
summary(log_fit)

# library(ggplot2)
# x=runif(10000)
# y=log(x/(1-x))
# hist(y)
# d=data.frame(x,y)
# ggplot(d,aes(x=x,y=y))+geom_point()     #+geom_smooth()

rg_train$score=predict(log_fit,newdata=rg_train,type = "response")

## ----message=F,warning=F-------------------------------------------------
library(ggplot2)
ggplot(rg_train,aes(y=Revenue.Grid,x=score,color=factor(Revenue.Grid)))+
  geom_point()+geom_jitter()


## ------------------------------------------------------------------------
cutoff=0.2
predicted=as.numeric(train$score1>cutoff)
TP=sum(predicted==1 & train$store==1)
FP=sum(predicted==1 & train$store==0)
FN=sum(predicted==0 & train$store==1)
TN=sum(predicted==0 & train$store==0)

# lets also calculate total number of real positives and negatives in the data 
P=TP+FN
N=TN+FP

# total number of observations
total=P+N


## ------------------------------------------------------------------------
cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=seq(0,1,length=100)

for (cutoff in cutoffs){
  predicted=as.numeric(rg_train$score>cutoff)
  
  TP=sum(predicted==1 & rg_train$Revenue.Grid==1)
  FP=sum(predicted==1 & rg_train$Revenue.Grid==0)
  FN=sum(predicted==0 & rg_train$Revenue.Grid==1)
  TN=sum(predicted==0 & rg_train$Revenue.Grid==0)
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN))
}

# lets remove the dummy data cotaining top row
cutoff_data=cutoff_data[-1,]

## ------------------------------------------------------------------------
cutoff_data=cutoff_data %>%
  mutate(Sn=TP/P, Sp=TN/N,dist=sqrt((1-Sn)**2+(1-Sp)**2),P=FN+TP,N=TN+FP) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  mutate(Accuracy=(TP+TN)/(P+N)) %>%
  mutate(Lift=(TP/P)/((TP+FP)/(P+N))) %>%
  mutate(M=(8*FN+2*FP)/(P+N)) %>%
  select(-P,-N)


## ----warning=F,message=F-------------------------------------------------
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



## ------------------------------------------------------------------------
rg_test$score=predict(log_fit,newdata = rg_test,type = "response")


# lets also calculate total number of real positives and negatives in the data 
P=TP+FN
N=TN+FP
## ------------------------------------------------------------------------
#Cutoff with minimum KS:
KS_cutoff=cutoff_data$cutoff[which(cutoff_data$KS==max(cutoff_data$KS))][1]
KS_cutoff
table(rg_test$Revenue.Grid,as.numeric(rg_test$score>KS_cutoff))
#Cutoff with minimum distance
dist_cutoff=cutoff_data$cutoff[which(cutoff_data$dist==min(cutoff_data$dist))][1]
dist_cutoff
table(rg_test$Revenue.Grid,as.numeric(rg_test$score>dist_cutoff))

## ------------------------------------------------------------------------
#Cutoff with max Accuracy
Acc_cutoff=cutoff_data$cutoff[which(cutoff_data$Accuracy==max(cutoff_data$Accuracy))][1]
Acc_cutoff
table(rg_test$Revenue.Grid,as.numeric(rg_test$score>Acc_cutoff))
# Cutoff with minimum M ( The hypothetical business criterion)
M_cutoff=cutoff_data$cutoff[which(cutoff_data$M==min(cutoff_data$M))][1]
M_cutoff
table(rg_test$Revenue.Grid,as.numeric(rg_test$score>M_cutoff))
