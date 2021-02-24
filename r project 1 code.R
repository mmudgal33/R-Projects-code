setwd("D:/mohit gate/edvancer/edvancer pdf & codes/data_project_1")

h_train=read.csv("housing_train.csv",stringsAsFactors = F)
h_train$pr=h_train$Price
h_train$Price=NULL
h_train$price=h_train$pr
h_train$pr=NULL


table(h_all$CouncilArea)

h_test=read.csv("housing_test.csv",stringsAsFactors = F)


h_test$price = NA

h_train$data='train'
h_test$data='test'

h_all=rbind(h_train,h_test)


#find number of na in price, and remove that observation
sum(is.na(h_all$price))
nrow(h_all)
h_all=h_all[!(is.na(h_all$price)),]



library(dplyr)
glimpse(h_train)



lapply(h_all,function(x) length(unique(x)))

unique(h_all$Address)
h_all$Address=NULL
h_all$price_standard=h_all$price/100000

h_all=h_all%>%
  mutate(hp_6=as.numeric(h_all$price_standard<6),
         hp_8=as.numeric(h_all$price_standard>6&h_all$price_standard<8),
         hp_10=as.numeric(h_all$price_standard>8&h_all$price_standard<10),
         hp_12=as.numeric(h_all$price_standard>10&h_all$price_standard<12),
         hp_14=as.numeric(h_all$price_standard>12&h_all$price_standard<14),
         hp_16=as.numeric(h_all$price_standard>14&h_all$price_standard<16),
         hp_18=as.numeric(h_all$price_standard>16&h_all$price_standard<18),
         hp_20=as.numeric(h_all$price_standard>18&h_all$price_standard<20),
         hp_22=as.numeric(h_all$price_standard>20&h_all$price_standard<22),
         hp_24=as.numeric(h_all$price_standard>22&h_all$price_standard<24))%>%
  select(-Suburb)
 















price_mean=tapply(h_all$price,h_all$Suburb,mean,na.rm=T)
x=as.data.frame(price_mean)

x$y=round(x[,1]/100000,1)

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
h_all=h_all%>%
       mutate(councilarea_5=as.numeric(h_all$CouncilArea %in% c("")),
              councilarea_6=as.numeric(h_all$CouncilArea %in% c("Banyule")),
              councilarea_8=as.numeric(h_all$CouncilArea %in% c("Bayside","Boroondara")),
              councilarea_9=as.numeric(h_all$CouncilArea %in% c("Brimbank","Darebin","Glen Eira","Hobsons Bay","Hume","Kingston")),
              councilarea_10=as.numeric(h_all$CouncilArea %in% c("Manningham","Maribyrnong")),
              councilarea_11=as.numeric(h_all$CouncilArea %in% c("Melbourne","Monash","Moonee Valley")),
              councilarea_12=as.numeric(h_all$CouncilArea %in% c("Moreland")),
              councilarea_13=as.numeric(h_all$CouncilArea %in% c("Port Phillip","Stonnington")),
              councilarea_16=as.numeric(h_all$CouncilArea %in% c("Whitehorse")),
              councilarea_17=as.numeric(h_all$CouncilArea %in% c("Yarra")))%>%
  select(-CouncilArea)
  
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



table(h_all$CouncilArea)
h_all=h_all%>%
  mutate(suburb_5=as.numeric(h_all$Suburb %in% c("Abbotsford","Aberfeldie","Airport West","Albert Park","Albion")),
         suburb_6=as.numeric(h_all$Suburb %in% c( "Alphington","Altona","Altona North","Armadale","Ascot Vale","Ashburton","Ashwood","Avondale Heights","Balaclava","Balwyn")),
         suburb_7=as.numeric(h_all$Suburb %in% c( "Balwyn North","Bellfield","Bentleigh","Bentleigh East","Box Hill","Braybrook","Brighton","Brighton East","Brooklyn","Brunswick","Brunswick East","Brunswick West","Bulleen","Burnley","Burwood","Camberwell","Campbellfield","Canterbury")),
         suburb_8=as.numeric(h_all$Suburb %in% c("Carlton North","Carnegie", "Caulfield", "Caulfield East" ,"Caulfield North","Caulfield South","Chadstone","Clifton Hill","Coburg", "Coburg North","Collingwood","Cremorne", "Docklands","Doncaster")),
         suburb_10=as.numeric(h_all$Suburb %in% c("Manningham","Maribyrnong")),
         suburb_11=as.numeric(h_all$Suburb %in% c("Melbourne","Monash","Moonee Valley")),
         suburb_12=as.numeric(h_all$Suburb %in% c("Moreland")),
         suburb_13=as.numeric(h_all$Suburb %in% c("Port Phillip","Stonnington")),
         suburb_16=as.numeric(h_all$Suburb %in% c("Whitehorse")),
         suburb_17=as.numeric(h_all$Suburb %in% c("Yarra")))%>%
  select(-Suburb)


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

table(h_all$YearBuilt)
h_all$YearBuilt=as.numeric(h_all$YearBuilt)

h_all=h_all%>%
  mutate(Postcode=as.numeric(Postcode),
         Distance=as.numeric(Distance),
         Landsize=as.numeric(Landsize),
         BuildingArea=as.numeric(BuildingArea),
         YearBuilt=as.numeric(YearBuilt))


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
y=as.data.frame(sort(table(h_all$SellerG)))
h_all=CreateDummies(h_all ,"Suburb",70)
h_all=CreateDummies(h_all,"SellerG",80)
h_all=CreateDummies(h_all,"Postcode",50)

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
h_train=h_all%>%filter(data=="train")%>%
  select(-data)
h_test=h_all%>%filter(data=="test")%>%
  select(-data)

#seperate for performance check
set.seed(2)
s=sample(1:nrow(h_train),0.8*nrow(h_train))
train=h_train[s,]
test=h_train[s,]

car_2
fit=lm(price~.-car_o-Bathroom_0-Bedroom2_o-councilarea_17-Type_u-councilarea_5-Method_VB-Rooms_GT5-Rooms_3-Bedroom2_2-car_2-Rooms_1-councilarea_8 ,data = train)
summary(fit)


#work on vif
library(car)
vif(fit)
sort(vif(fit),decreasing = T)

#p-value
fit=step(fit)
fit=lm(price ~ Distance + Postcode + Landsize + BuildingArea + YearBuilt + 
  Type_h + Type_t + Rooms_2 + Rooms_4 + Rooms_5 + Method_S + 
  Method_SP + councilarea_6 + councilarea_9 + councilarea_10 + 
  councilarea_11 + councilarea_12 + Bedroom2_1 + Bedroom2_3 + 
  Bedroom2_4 + car_0 + car_4 + Suburb_SurreyHills + Suburb_Maribyrnong + 
  Suburb_Elwood + Suburb_Prahran + Suburb_Toorak + Suburb_Hampton + 
  Suburb_Balwyn + Suburb_Doncaster + Suburb_PortMelbourne + 
  Suburb_PascoeVale + Suburb_BalwynNorth + Suburb_Kew + Suburb_Northcote + 
  Suburb_Brighton + Suburb_Essendon + Suburb_Brunswick + Suburb_StKilda + 
  Suburb_Preston + Suburb_Richmond + Suburb_Reservoir + SellerG_Miles + 
  SellerG_Greg + SellerG_Buxton + SellerG_Marshall + SellerG_hockingstuart + 
  SellerG_Jellis,data = train)
summary(fit)

#p-value remove
fit=lm(price ~ Distance + Postcode + Landsize + BuildingArea + YearBuilt + 
         Type_h + Type_t + Rooms_2 + Rooms_4 + Rooms_5 + Method_S + 
         Method_SP + councilarea_6 + councilarea_9 + councilarea_10 + 
         councilarea_11 + councilarea_12 + Bedroom2_1 + Bedroom2_3 + 
         Bedroom2_4 + car_0 + car_4 + Suburb_SurreyHills + Suburb_Maribyrnong + 
         Suburb_Elwood + Suburb_Prahran + Suburb_Toorak + Suburb_Hampton + 
         Suburb_Balwyn + Suburb_Doncaster + Suburb_PortMelbourne + Suburb_BalwynNorth + Suburb_Kew + Suburb_Northcote + 
         Suburb_Brighton + Suburb_Essendon + Suburb_StKilda + Suburb_Richmond + Suburb_Reservoir + SellerG_Miles + 
         SellerG_Greg + SellerG_Marshall + SellerG_hockingstuart + 
         SellerG_Jellis,data = train)
summary(fit)


#predict train_test
val.pred=predict(fit,newdata=test)
errors=test$price-val.pred
sum(is.na(test$price))
sqrt(mean(errors**2,na.rm = T))
errors**2 %>% mean() %>% sqrt()

### model for predcition on the entire data

fit.final=fit=lm(price ~ Distance + Postcode + Landsize + BuildingArea + YearBuilt + 
                   Type_h + Type_t + Rooms_2 + Rooms_4 + Rooms_5 + Method_S + 
                   Method_SP + councilarea_6 + councilarea_9 + councilarea_10 + 
                   councilarea_11 + councilarea_12 + Bedroom2_1 + Bedroom2_3 + 
                   Bedroom2_4 + car_0 + car_4 + Suburb_SurreyHills+
                   Suburb_Elwood + Suburb_Prahran + Suburb_Toorak + Suburb_Hampton + 
                   Suburb_Balwyn + Suburb_Doncaster + Suburb_PortMelbourne + Suburb_BalwynNorth + Suburb_Kew + Suburb_Northcote + 
                   Suburb_Brighton + Suburb_Essendon + Suburb_StKilda + Suburb_Richmond + Suburb_Reservoir + SellerG_Miles + 
                   SellerG_Greg + SellerG_Marshall + SellerG_hockingstuart + 
                   SellerG_Jellis,data=h_train)

fit.final=step(fit.final)

summary(fit.final)
#after fit formula used
fit_final=fit=lm(formula = price ~ Distance + Postcode + Landsize + BuildingArea + 
     YearBuilt + Type_h + Type_t + Rooms_2 + Rooms_4 + Rooms_5 + 
     Method_S + Method_SP + councilarea_6 + councilarea_9 + councilarea_10 + 
     councilarea_11 + councilarea_12 + Bedroom2_1 + Bedroom2_3 + 
     Bedroom2_4 + car_0 + car_4 + Suburb_SurreyHills + Suburb_Elwood + 
     Suburb_Prahran + Suburb_Toorak + Suburb_Hampton + Suburb_Balwyn + 
     Suburb_Doncaster + Suburb_PortMelbourne + Suburb_BalwynNorth + 
     Suburb_Kew + Suburb_Northcote + Suburb_Brighton + Suburb_Essendon + 
     Suburb_StKilda + Suburb_Richmond + Suburb_Reservoir + SellerG_Miles + 
     SellerG_Greg + SellerG_Marshall + SellerG_hockingstuart + 
     SellerG_Jellis, data = h_train)

test.pred=predict(fit.final,newdata=h_test)

write.csv(test.pred,"housing_project_1.csv",row.names = F)