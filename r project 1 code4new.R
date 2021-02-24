setwd("D:/mohit gate/edvancer/edvancer pdf & codes/data_project_1")

h_train=read.csv("housing_train.csv",stringsAsFactors = F)
h_train$pr=h_train$Price
h_train$Price=NULL
h_train$price=h_train$pr
h_train$pr=NULL

h_train1=h_train[h_train$price<1325000,]
nrow(h_train1)

h_test=read.csv("housing_test.csv",stringsAsFactors = F)

h_test$price = NA

h_train$data='train'
h_test$data='test'

sum(is.na(h_train$price))

h_all=rbind(h_train,h_test)


library(dplyr)
glimpse(h_all)


#removing na from char type
lapply(h_all, function(x) sum(is.na(x)))
h_all=h_all%>%select(-YearBuilt,-BuildingArea)
#h_all=h_all[!(is.na(h_all$price)),]
#remove address
h_all$Address=NULL

column=names(h_all)[sapply(h_all,function(x) is.character(x))]


for(col in column){
  
  if(sum(is.na(h_all[,col]))>0){
    
    h_all[is.na(h_all[,col]),col]=mode(h_all[,"Landsize"])
  }
  
}
sum(is.na(h_all$CouncilArea))

names(h_all)[sapply(h_all,function(x) is.character(x))]



#removing na from predictors data numeric
for(col in c("Landsize","Bedroom2","Bathroom","Car")){
  
  if(sum(is.na(h_all[,col]))>0){
    
    h_all[is.na(h_all[,col]),col]=round(mean(h_all[,col],na.rm=T),0)
  }
  
}

#where Landsize=0 put 452 
h_all[h_all[,"Landsize"]==0,"Landsize"]=452

round(mean(h_all[,"YearBuilt"],na.rm=T),0)


table(h_all$Landsize)


glimpse(h_all)
sort(table(h_all$YearBuilt))
sum(is.na(h_all$YearBuilt))

#find number of na in price, and remove that observation
sum(is.na(h_train$price))
nrow(h_all)



lapply(h_all,function(x) length(unique(x)))

unique(h_all$Address)



tapply(h_all$price,h_all$Suburb,mean,na.rm=T)


#Type

h_all=h_all%>%
  mutate(Type_h=as.numeric(h_all$Type=="h"),
         Type_t=as.numeric(h_all$Type=="t"),
         Type_u=as.numeric(h_all$Type=="u"))%>%
  select(-Type)


#Rooms
h_all=h_all%>%
  mutate(Rooms_1=as.numeric(h_all$Rooms==1),
         Rooms_2=as.numeric(h_all$Rooms==2),
         Rooms_3=as.numeric(h_all$Rooms==3),
         Rooms_4=as.numeric(h_all$Rooms==4),
         Rooms_5=as.numeric(h_all$Rooms==5),
         Rooms_GT5=as.numeric(h_all$Rooms %in% c(6,7,8,9,10)))%>%
  select(-Rooms)



#method
h_all=h_all%>%
  mutate(Method_PI=as.numeric(h_all$Method=="PI"),
         Method_S=as.numeric(h_all$Method=="S"),
         Method_SA=as.numeric(h_all$Method=="SA"),
         Method_SP=as.numeric(h_all$Method=="SP"),
         Method_VB=as.numeric(h_all$Method=="VB"))%>%
  select(-Method)




#councilarea
table(h_all$CouncilArea)
#h_all=h_all%>%
 #      mutate(councilarea_5=as.numeric(h_all$CouncilArea %in% c("")),
  #            councilarea_6=as.numeric(h_all$CouncilArea %in% c("Banyule")),
   #           councilarea_8=as.numeric(h_all$CouncilArea %in% c("Bayside","Boroondara")),
    #          councilarea_9=as.numeric(h_all$CouncilArea %in% c("Brimbank","Darebin","Glen Eira","Hobsons Bay","Hume","Kingston")),
     #         councilarea_10=as.numeric(h_all$CouncilArea %in% c("Manningham","Maribyrnong")),
      #        councilarea_11=as.numeric(h_all$CouncilArea %in% c("Melbourne","Monash","Moonee Valley")),
       #       councilarea_12=as.numeric(h_all$CouncilArea %in% c("Moreland")),
        #      councilarea_13=as.numeric(h_all$CouncilArea %in% c("Port Phillip","Stonnington")),
         #     councilarea_16=as.numeric(h_all$CouncilArea %in% c("Whitehorse")),
          #    councilarea_17=as.numeric(h_all$CouncilArea %in% c("Yarra")))%>%
#  select(-CouncilArea)
  
glimpse(h_all)      

                                                                                                                    
#suburb
table(h_all$Suburb)
df=as.data.frame(tapply(h_all$price,h_all$Suburb,mean,na.rm=T),row.names = F)

colnames(df)="suburb_mean"

df$suburb_names=rownames(df)
df$sort=sort(df$suburb_mean)
df$sort_div=df$sort/100000
range(df$sort_div)
sum(is.na(h_all$Suburb))
which(df[,df$sort_div<5.5])#give first 5 obs, not sort_div<5
x=as.data.frame(table(h_all$Suburb))
x$sort=sort(x$Freq)
sum(x$sort)
sum(is.na(h_all$Suburb))#0





#fiter suburb by mean prices
df1=df%>%
  select(sort_div,suburb_names)%>%
  filter(sort_div>8.5&sort_div<9.5)
df1$suburb_names





#bedroom
table(h_all$Bedroom2)
h_all=h_all%>%
  mutate(Bedroom2_1=as.numeric(h_all$Bedroom2 %in% c(0,1)),
         Bedroom2_2=as.numeric(h_all$Bedroom2==2),
         Bedroom2_3=as.numeric(h_all$Bedroom2==3),
         Bedroom2_4=as.numeric(h_all$Bedroom2==4),
         Bedroom2_o=as.numeric(h_all$Bedroom2 %in% c(5,6)),
         Bedroom2_OT=as.numeric(h_all$Bedroom2 %in% c(7,8,9,20)))%>%
  select(-Bedroom2)

#bathroom
table(h_all$Bathroom)
h_all=h_all%>%
  mutate(Bathroom_0=as.numeric(h_all$Bathroom==0),
         Bedroom2_2=as.numeric(h_all$Bathroom %in% c(1,2)),
         Bedroom2_4=as.numeric(h_all$Bathroom %in% c(3,4)),
         Bedroom2_o=as.numeric(h_all$Bathroom %in% c(5,6,7,8)))%>%
  select(-Bathroom)


#car
table(h_all$Car)
h_all=h_all%>%
  mutate(car_0=as.numeric(h_all$Car==0),
         car_2=as.numeric(h_all$Car %in% c(1,2)),
         car_4=as.numeric(h_all$Car %in% c(3,4)),
         car_o=as.numeric(h_all$Car %in% c(5,6,7,8)))%>%
  select(-Car)
  
#councilarea
table(h_all$CouncilArea)
df=as.data.frame(tapply(h_all$price,h_all$CouncilArea,mean,na.rm=T))
df$CouncilArea_mean=tapply(h_all$price, h_all$CouncilArea, mean, na.rm = T)
colnames(df)="CouncilArea_mean"

df$sort=sort(df$CouncilArea_mean)
df$sort_div=df$sort/100000




glimpse(h_all)

#table(h_all$YearBuilt)
#h_all$YearBuilt=as.numeric(h_all$YearBuilt)

h_all=h_all%>%
  mutate(Postcode=as.numeric(Postcode),
         Distance=as.numeric(Distance),
         Landsize=as.numeric(Landsize),
         #BuildingArea=as.numeric(BuildingArea),
         #YearBuilt=as.numeric(YearBuilt),
         Distance=as.numeric(Distance))

glimpse(h_all)

#dummies creation

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
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}


h_all=CreateDummies(h_all ,"Suburb",70)
h_all=CreateDummies(h_all,"SellerG",80)
h_all=CreateDummies(h_all,"CouncilArea",20)


#postcode
unique(train$Postcode)
p=as.data.frame(table(train$Postcode))
p$sort=sort(p$Freq)

glimpse(h_all)

sum(is.na(h_all$YearBuilt))
year=as.data.frame(sort(table(h_all$YearBuilt)))
year=as.data.frame(tapply(h_all$price,h_all$YearBuilt,mean,na.rm=T))




glimpse(h_all)

lapply(h_all,function(x) length(unique(x)))

#data prep complete, modeling start
#seperate test and train
table(h_all$data)
h_train=h_all%>%filter(data=="train")%>%
  select(-data)
h_test=h_all%>%filter(data=="test")%>%
  select(-data)



#seperate for performance check
set.seed(2)
s=sample(1:nrow(h_train),0.8*nrow(h_train))
train=h_train[s,]
test=h_train[s,]
lapply(h_train, function(x) sum(is.na(x)))

fit=lm(price~.-car_o-Bathroom_0-Method_VB-Rooms_GT5-Type_u-Rooms_3-car_2-CouncilArea_-Bedroom2_2,data = train)
summary(fit)


#work on vif
library(car)
vif(fit)
sort(vif(fit),decreasing = T)

#p-value
fit=step(fit)
#p-value remove
fit=lm(price ~ Distance + Postcode + Landsize + Type_h + Type_t + Rooms_1 + 
         Rooms_2 + Rooms_4 + Rooms_5 + Method_S + Method_SA + 
         Method_SP + Bedroom2_4 + Bedroom2_o + Bedroom2_OT + 
         car_0 + car_4 + Suburb_Armadale + Suburb_Burwood + Suburb_Williamstown + 
         Suburb_Melbourne + Suburb_SunshineWest + 
         Suburb_Prahran + Suburb_SurreyHills + Suburb_Kensington + 
         Suburb_Toorak + 
         Suburb_Doncaster + 
         Suburb_Hampton + 
         Suburb_Balwyn + Suburb_MalvernEast + 
         Suburb_Camberwell + Suburb_PortMelbourne + 
         Suburb_BrightonEast + 
         Suburb_Hawthorn + Suburb_BalwynNorth + 
         Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + 
         Suburb_Essendon + Suburb_SouthYarra + 
         Suburb_StKilda + Suburb_Preston + Suburb_Richmond + 
         Suburb_Reservoir + 
         SellerG_Kay + 
         SellerG_Miles + SellerG_Greg + 
         SellerG_RT + 
         SellerG_Ray + 
         SellerG_Marshall + SellerG_hockingstuart + 
         SellerG_Jellis + CouncilArea_Whitehorse + 
         CouncilArea_Bayside + 
         CouncilArea_Banyule + CouncilArea_PortPhillip + 
         CouncilArea_Yarra + CouncilArea_Maribyrnong + 
         CouncilArea_Moreland + CouncilArea_Boroondara,data = train)
summary(fit)





#predict train_test
val.pred=predict(fit,newdata=test)
#RMSE
errors=test$price-val.pred
sum(is.na(test$price))
s=sqrt(mean(errors**2,na.rm = T))

# here RMSE over 4,00,000 thats problem

errors**2 %>% mean() %>% sqrt()

### model for predcition on the entire data

fit.final=fit=lm(price ~ Distance + Postcode + Landsize + Type_h + Type_t + Rooms_1 + 
                   Rooms_2 + Rooms_4 + Rooms_5 + Method_S + Method_SA + 
                   Method_SP + Bedroom2_4 + Bedroom2_o + Bedroom2_OT + 
                   car_0 + car_4 + Suburb_Armadale + Suburb_Burwood + Suburb_Williamstown + 
                   Suburb_Melbourne + Suburb_SunshineWest + 
                   Suburb_Prahran + Suburb_SurreyHills + Suburb_Kensington + 
                   Suburb_Toorak + 
                   Suburb_Doncaster + 
                   Suburb_Hampton + 
                   Suburb_Balwyn + Suburb_MalvernEast + 
                   Suburb_Camberwell + Suburb_PortMelbourne + 
                   Suburb_BrightonEast + 
                   Suburb_Hawthorn + Suburb_BalwynNorth + 
                   Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + 
                   Suburb_Essendon + Suburb_SouthYarra + 
                   Suburb_StKilda + Suburb_Preston + Suburb_Richmond + 
                   Suburb_Reservoir + 
                   SellerG_Kay + 
                   SellerG_Miles + SellerG_Greg + 
                   SellerG_RT + 
                   SellerG_Ray + 
                   SellerG_Marshall + SellerG_hockingstuart + 
                   SellerG_Jellis + CouncilArea_Whitehorse + 
                   CouncilArea_Bayside + 
                   CouncilArea_Banyule + CouncilArea_PortPhillip + 
                   CouncilArea_Yarra + CouncilArea_Maribyrnong + 
                   CouncilArea_Moreland + CouncilArea_Boroondara,data=h_train)

fit.final=step(fit.final)

summary(fit.final)
#after fit formula used
fit.final=fit=lm(price ~ Distance + Postcode + Landsize + Type_h + Type_t + Rooms_1 + 
                   Rooms_2 + Rooms_4 + Rooms_5 + Method_S + Method_SA + 
                   Method_SP + Bedroom2_4 + Bedroom2_o + Bedroom2_OT + 
                   car_0 + car_4 + Suburb_Armadale + Suburb_Burwood + Suburb_Williamstown + 
                   Suburb_Melbourne + Suburb_SunshineWest + 
                   Suburb_Prahran + Suburb_SurreyHills + Suburb_Kensington + 
                   Suburb_Toorak + 
                   Suburb_Doncaster + 
                   Suburb_Hampton + 
                   Suburb_Balwyn + Suburb_MalvernEast + 
                   Suburb_Camberwell + Suburb_PortMelbourne + 
                   Suburb_BrightonEast + 
                   Suburb_Hawthorn + Suburb_BalwynNorth + 
                   Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + 
                   Suburb_Essendon + Suburb_SouthYarra + 
                   Suburb_StKilda + Suburb_Preston + Suburb_Richmond + 
                   Suburb_Reservoir + 
                   SellerG_Kay + 
                   SellerG_Miles + SellerG_Greg + 
                   SellerG_RT + 
                   SellerG_Ray + 
                   SellerG_Marshall + SellerG_hockingstuart + 
                   SellerG_Jellis + CouncilArea_Whitehorse + 
                   CouncilArea_Bayside + 
                   CouncilArea_Banyule + CouncilArea_PortPhillip + 
                   CouncilArea_Yarra + CouncilArea_Maribyrnong + 
                   CouncilArea_Moreland + CouncilArea_Boroondara, data = h_train)

test.pred=predict(fit.final,newdata=h_test)
h_test$price=test.pred

write.csv(test.pred,"housing_project_1114.csv",row.names = F)


m=read.csv("housing_project_1114.csv")
sum(is.na(m[,1]))

plot(fit.final,3)

library(randomForest)
rf.model=randomForest(price~.,data=h_train,do.trace=T)
test.price=predict(rf.model,newdata = h_test)
write.csv(test.price,"housing_project_rf1114.csv",row.names = F)

#RMSE

sum(is.na(test$price))

#RMSE on rf applied on h_train
rf.model=randomForest(price~.,data=train,do.trace=T)
test.price=predict(rf.model,newdata= test)
test.price=as.data.frame(test.price)
errors=test$price-test.price 
test.price$errors=errors
sum(is.na(test$price))
s=sqrt(mean(errors**2,na.rm = T))
range(h_train$price)
table(h_train$price)