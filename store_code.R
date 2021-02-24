setwd("D:/mohit gate/edvancer/edvancer pdf & codes/data_project_2")
rg.train=read.csv("store_train.csv",stringsAsFactors = FALSE)
rg.test=read.csv("store_test.csv",stringsAsFactors = FALSE)
rg.test$store = NA


rg.train$data='train'
rg.test$data='test'

rg.all=rbind(rg.train,rg.test)

library(dplyr)
glimpse(rg.all)

## ----message=FALSE,warning=FALSE-----------------------------------------
table(rg.all$country)
unique(rg.all$countyname)


## ------------------------------------------------------------------------
rg.all=rg.all %>% select(-Id)


rg = rg %>%
  mutate(children=ifelse(children=="Zero",0,substr(children,1,1)),
         children=as.numeric(children))


## ------------------------------------------------------------------------
table(rg$age_band)

## ------------------------------------------------------------------------
f=prop.table(table(rg.all$countyname,rg.all$store),1)
rownames(f)[which(f[,1]==0)]
rownames(f)[which(f[,1]>0 &f[,1]<0.25)]

glimpse(rg.all$CouSub)

lapply(rg.all,function(x) length(unique(x)))
names(f)[sapply(f,function(x) which(f[1,]==0))]




## ----warning=FALSE-------------------------------------------------------
rg=rg %>%
  mutate(a1=as.numeric(substr(age_band,1,2)),
         a2=as.numeric(substr(age_band,4,5)),
         age=ifelse(substr(age_band,1,2)=="71",71,ifelse(age_band=="Unknown",NA,0.5*(a1+a2)))
  ) %>%
  select(-a1,-a2,-age_band) %>%
  na.omit()


## ------------------------------------------------------------------------
table(rg$status)
rg = rg %>%
  mutate(status_div=as.numeric(status=="Divorced/Separated"),
         status_partner=as.numeric(status=="Partner"),
         status_single=as.numeric(status=="Single/Never Married")) %>%
  select(-status)

table(rg$occupation)


## ------------------------------------------------------------------------
round(prop.table(table(rg$occupation,rg$Revenue.Grid),1),2)
rg=rg %>%
  mutate(occ_BM_prof=as.numeric(occupation %in% c("Business Manager","Professional")),
         occ_Retired=as.numeric(occupation=="Retired"),
         occ_HW=as.numeric(occupation=="Housewife")) %>%
  select(-occupation)

round(prop.table(table(rg$occupation_partner,rg$Revenue.Grid),1),2)

rg=rg %>%
  mutate(op_1=as.numeric(occupation_partner %in% c("Other","Retired","Unknown")),
         op_2=as.numeric(occupation_partner %in% c("Student","Secretarial/Admin"))) %>%
  select(-occupation_partner)

table(rg$home_status)
unique(rg$home_status)
rg=rg %>%
  mutate(hs_livein=as.numeric(home_status=="Live in Parental Hom"),
         hs_own=as.numeric(home_status=="Own Home"),
         hs_rent_private=as.numeric(home_status=="Rent Privately"),
         hs_rent_council=as.numeric(home_status=="Rent from Council/HA")) %>%
  select(-home_status)

round(prop.table(table(rg$family_income,rg$Revenue.Grid),1),2)
rg=rg %>%
  mutate(fi_1=as.numeric(family_income %in% 
                           c("< 4,000","< 8,000, >= 4,000")),
         fi_2=as.numeric(family_income %in% 
                           c("<12,500, >=10,000","<25,000, >=22,500","<27,500, >=25,000")),
         fi_3=as.numeric(family_income %in% 
                           c("<10,000, >= 8,000","<15,000, >=12,500","<20,000, >=17,500",">=35,000")),
         fi_4=as.numeric(family_income %in% 
                           c("<17,500, >=15,000","<22,500, >=20,000","<30,000, >=27,500"))
  ) %>%
  select(-family_income)

table(rg$self_employed)
table(rg$self_employed_partner)
table(rg$gender)
rg=rg %>%
  mutate(self_emp_yes=as.numeric(self_employed=="Yes"),
         self_emp_part_yes=as.numeric(self_employed_partner=="Yes"),
         gender_f=as.numeric(gender=="Female"),
         gender_m=as.numeric(gender=="Male")) %>%
  select(-self_employed,-self_employed_partner,-gender)


## ------------------------------------------------------------------------
rg=rg %>%
  select(-TVarea,-post_code,-post_area,-region)

## ------------------------------------------------------------------------
rg=rg %>%
  filter(!(year_last_moved==0))
glimpse(rg)

## ------------------------------------------------------------------------
set.seed(2)
s=sample(1:nrow(rg),0.7*nrow(rg))
rg_train=rg[s,]
rg_test=rg[-s,]

## ----message=FALSE,warning=FALSE-----------------------------------------
library(car)
for_vif=lm(Revenue.Grid~.-REF_NO,data=rg_train)
vif(for_vif)

## ----message=FALSE,warning=FALSE-----------------------------------------
for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity,data=rg_train)
vif(for_vif)
for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative ,data=rg_train)
vif(for_vif)
for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity,data=rg_train)
vif(for_vif)
for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity-gender_m,data=rg_train)
vif(for_vif)
for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity-gender_m-hs_own,data=rg_train)
vif(for_vif)
for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity-gender_m-hs_own-fi_3,data=rg_train)
vif(for_vif)
for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity-gender_m-hs_own-fi_3-Portfolio.Balance,data=rg_train)
vif(for_vif)

## ----error=TRUE----------------------------------------------------------
rg_fit=rg_train %>%
  select(-REF_NO,-Investment.in.Commudity,-Investment.in.Derivative,
         -Investment.in.Equity,-gender_m,-hs_own,-fi_3,-Portfolio.Balance)
fit=glm(Revenue.Grid~.,family = "binomial",data=rg_fit)

## ----message=FALSE,warning=FALSE-----------------------------------------
rg$Revenue.Grid=as.numeric(rg$Revenue.Grid==1)
set.seed(2)
s=sample(1:nrow(rg),0.7*nrow(rg))
rg_train=rg[s,]
rg_test=rg[-s,]

rg_fit=rg_train %>%
  select(-REF_NO,-Investment.in.Commudity,-Investment.in.Derivative,
         -Investment.in.Equity,-gender_m,-hs_own,-fi_3,-Portfolio.Balance)
fit=glm(Revenue.Grid~.,family = "binomial",data=rg_fit)

## ----warning=FALSE-------------------------------------------------------
fit=step(fit)

## ------------------------------------------------------------------------
formula(fit)

## ----warning=FALSE-------------------------------------------------------
fit=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
          Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
          Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond + 
          Home.Loan + Online.Purchase.Amount + fi_2 + fi_4 + self_emp_part_yes + 
          gender_f, data=rg_train, family = "binomial")
summary(fit)
fit=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
          Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
          Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond + 
          Home.Loan + Online.Purchase.Amount +  fi_4 + self_emp_part_yes + 
          gender_f , data=rg_train, family = "binomial")
summary(fit)
fit=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
          Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
          Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond + 
          Home.Loan + Online.Purchase.Amount +self_emp_part_yes + 
          gender_f, data=rg_train, family = "binomial")
summary(fit)


## ----message=FALSE,warning=FALSE-----------------------------------------
formula(fit)
fit_final=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
                Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
                Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond + 
                Home.Loan + Online.Purchase.Amount + self_emp_part_yes + 
                gender_f,
              family = "binomial",data=rg_train)
summary(fit_final)

## ------------------------------------------------------------------------
rg_train$score=predict(fit,newdata=rg_train,type = "response")

## ----message=F,warning=F-------------------------------------------------
library(ggplot2)
ggplot(rg_train,aes(y=Revenue.Grid,x=score,color=factor(Revenue.Grid)))+
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
rg_test$score=predict(fit_final,newdata = rg_test,type = "response")

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

