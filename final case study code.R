setwd("D:/mohit gate/edvancer/edvancer pdf & codes/final case study")

#importing data

alldata=read.csv("adult.csv",stringsAsFactors = F,header = F)

names(alldata)=c("age","workclass","fnlwgt","education","education.num","marital.status",
                 "occupation","relationship","race","sex","capital.gain","capital.loss","hpw",
                 "native.country","target")

table(alldata$target)

alldata$target=as.numeric(alldata$target==" >50K")

library(dplyr)
glimpse(alldata)

table(alldata$workclass)
round(prop.table(table(alldata$workclass,alldata$target),1),2)

## groups = 1:"Never-worked","Without-pay" 2: "Self-emp-not-inc","State-gov","Local-gov"
# 3: "?" , 4:"Self-emp-inc", 5:"Private" , 6 :"Federal-gov"

alldata= alldata %>%
  mutate(wc2=as.numeric(workclass %in% c(" Self-emp-not-inc"," State-gov"," Local-gov")),
         wc3=as.numeric(workclass==" ?"),
         wc4=as.numeric(workclass==" Self-emp-inc"),
         wc5=as.numeric(workclass==" Private"),
         wc6=as.numeric(workclass==" Federal-gov")
  ) %>%
  select(-workclass)

glimpse(alldata)

table(alldata$education)

round(prop.table(table(alldata$education,alldata$target),1),2)

#groups 1: "10th","11th","12th","1st-4th","5th-6th","7th-8th","9th","Preschool"
# 2: "Some-college","HS-grad" 3:"Assoc-voc","Assoc-acdm" 4:"Prof-school","Doctorate"
# 5: "Bachelors"

alldata=alldata %>%
  mutate(ed2=as.numeric(education %in% c(" Some-college"," HS-grad")),
         ed3=as.numeric(education %in% c(" Assoc-voc"," Assoc-acdm")),
         ed4=as.numeric(education %in% c(" Prof-school"," Doctorate")),
         ed5=as.numeric(education==" Bachelors")) %>%
  select(-education)

glimpse(alldata)

table(alldata$marital.status)

round(prop.table(table(alldata$marital.status,alldata$target),1),2)

alldata=alldata %>%
  mutate(ms1=as.numeric(marital.status %in% c(" Married-AF-spouse"," Married-civ-spouse"))) %>%
  select(-marital.status)

glimpse(alldata)

table(alldata$occupation)

round(prop.table(table(alldata$occupation,alldata$target),1),1)

## 1: "Exec-managerial" , 2 :"Prof-specialty" , 3:"Protective-serv","Sales","Tech-support"
# 4 :"Transport-moving","Craft-repair" 
# 5:"?" , "Adm-clerical","Armed-Forces","Farming-fishing","Handlers-cleaners","Machine-op-inspct"
# 6 :"Other-service","Priv-house-serv"

alldata=alldata %>%
  mutate(oc1=as.numeric(occupation %in% c(" Exec-managerial")),
         oc2=as.numeric(occupation %in% c(" Prof-specialty")),
         oc3=as.numeric(occupation %in% c(" Protective-serv"," Sales"," Tech-support")),
         oc4=as.numeric(occupation %in% c(" Transport-moving"," Craft-repair")),
         oc6=as.numeric(occupation %in% c(" Other-service"," Priv-house-serv"))
  ) %>%
  select(-occupation)

glimpse(alldata)

table(alldata$relationship)

round(prop.table(table(alldata$relationship,alldata$target),1),1)

alldata=alldata %>%
  mutate(rl1=as.numeric(relationship %in% c(" Not-in-family"," Unmarried"," Other-relative"," Own-child"))
  ) %>%
  select(-relationship)

glimpse(alldata)

table(alldata$race)

round(prop.table(table(alldata$race,alldata$target),1),1)

alldata=alldata %>%
  mutate(race1=as.numeric(race %in% c(" White"," Asian-Pac-Islander"))
  ) %>%
  select(-race)

glimpse(alldata)

table(alldata$sex)

alldata=alldata %>%
  mutate(sexF=as.numeric(sex==" Female")) %>%
  select(-sex)

table(alldata$native.country)
round(prop.table(table(alldata$native.country,alldata$target),1),1)
## we are leaving this for you to explore

alldata=select(alldata,-native.country)

### breaking data into train , validation, test

set.seed(2)
s1=sample(1:nrow(alldata),0.8*nrow(alldata))

trainval=alldata[s1,]
test=alldata[-s1,]

s2=sample(1:nrow(trainval),0.7*nrow(trainval))

train=trainval[s2,]
val=trainval[-s2,]

## develope logistic regression model for train

# take care of vif first 
library(car)
vif_train=lm(target~.-wc5-rl1,data=train)
vif(vif_train)

fit_train=glm(target~.-wc5-rl1,data=train,family="binomial")

fit_train=step(fit_train)

vif_val=lm(target~.-wc5-rl1,data=val)
vif(vif_val)

fit_val=glm(target~.-wc5-rl1,data=val,family="binomial")
fit_val=step(fit_val)

##
fit_final=glm(target~age+education.num+capital.gain+capital.loss+hpw+wc2+wc6+ms1+ oc1 + oc2+oc3 +  oc4 ,data=train,family="binomial")

val$score=predict(fit_final,newdata=val,type="response")



scoring_data=val %>%
  select(target,score) %>%
  arrange(-score) %>%
  mutate(bins=cut(1:nrow(val),breaks=10,label=1:10)) %>%
  group_by(bins) %>%
  summarise(ones=sum(target),total=n(),zeroes=total-ones,bin_cutoff=min(score))

table(val$target)


scoring_data=scoring_data %>%
  mutate(one_p=ones/1872,zero_p=zeroes/5943) %>%
  mutate(c_one_p=cumsum(one_p),c_zero_p=cumsum(zero_p)) %>%
  mutate(diff=c_one_p-c_zero_p)

# cutoff = 0.1871094042

# performance of this logistic model on the test data

test$score=predict(fit_final,newdata=test,type="response")

test$predicted=as.numeric(test$score>0.1871094042)

table(test$target,test$predicted)

## DTree
library(tree)

trainval$target=as.factor(trainval$target)
single.tree=tree(target~.,data=trainval)


cv.single.tree=cv.tree(single.tree,FUN=prune.misclass)

plot(cv.single.tree$size,cv.single.tree$dev,type='b')

pruned.tree=prune.misclass(single.tree,best=5)

plot(pruned.tree)
text(pruned.tree,pretty = 0)

target.tree=predict(pruned.tree,newdata=test,type="class")

table(target.tree,test$target)

## Random Forest

library(randomForest)
rf=randomForest(target~.,data=trainval,mtry=10)

test.rf=predict(rf,newdata=test)

varImpPlot(rf)

# AUC ROC for logistic

library(pROC)
roccurve = roc(val$target ~ val$score)
plot(roccurve)
auc(roccurve)
