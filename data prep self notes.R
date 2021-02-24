
h_tr=read.csv("housing_train.csv",stringsAsFactors = F)
h_tst=read.csv("housing_test.csv",stringsAsFactors = F)

h_all=h_all%>%select(-YearBuilt,-BuildingArea,-Method,-Landsize)#method cant be dropped

Year=as.data.frame(round(tapply(h_tr$Price,h_all$YearBuilt,mean,na.rm=T)))
table(h_tr$YearBuilt)
sum(is.na(h_tr$YearBuilt))
#tapply(h_all$Price,h_all$Method,mean,na.rm=T)
#h_all[h_all$Method=="S",]  #6032 rows have s so remove

lapply(h_all, function(x) sum(is.na(x)))
names(h_all)[sapply(h_all,function(x) is.character(x))]

L=h_tr%>%
  select(Landsize)%>%
  filter( Landsize%in% c(0,NA)) #3411 has 0 or na so remove them
table(h_tr$Method)
L=h_tr%>%
  select(YearBuilt)%>%
  filter( YearBuilt%in% c(NA))#3717 all NAs
L=h_tr%>%
  select(BuildingArea)%>%
  filter( BuildingArea%in% c(NA)) #0 observation method can't be dropped
tab=table(h_tr$YearBuilt,h_tr$Method)
addmargins(tab)

h_tr$YearBuilt[is.na(h_tr$YearBuilt)]

D=h_tr[!(h_tr$YearBuilt %in% c(NA)),]
D=h_tr[!(h_tr$Landsize %in% c(0,NA)),]

tabs=table(h_tr$YearBuilt,h_tr$SellerG)
long=as.data.frame(addmargins(tabs))
spread=long %>%
  spread(Var2,Freq)



#spread1=spread %>%
#  filter(Sum>10) 
  
#names(spread1)[sapply(spread1,function(x) spread1[]]
sum(is.na(h_tr$CouncilArea))
table(h_tr$CouncilArea)#1564 have " "
tabs=table(h_tr$Landsize,h_tr$Price)
long=as.data.frame(addmargins(tabs))
spread=long %>%
  spread(Var2,Freq)


store_train=store_train %>% 
  separate(countyname,into=c("county","extraname"),sep="\\ ") %>% 
  select(-extraname)
ggplot(store_train,aes(x=county,color=factor(store)))+
  geom_bar()
county.extra1=table(store_train$county)
county.extra=c(rownames(county.extra1[county.extra1<7]))
store_train[which(store_train$county %in% county.extra),"county"]="Extra"


#where CouncilArea=" ", others have na too
h_tr=h_tr %>%
  filter(!(councilArea==" "))
h_tr=h_tr[!(h_tr$CouncilArea==""),]

table(h_tr$YearBuilt,h_tr$Type)


pr1=h_tr$Price
h_tr$pr1=pr1/100000
round(prop.table(table(h_tr$YearBuilt,h_tr$Price),1),2)

#Ls=h_tr$Landsize[h_tr$Landsize<5000]
#prc=h_tr$Price[h_tr$Price<1500000]
h_tr1=h_tr[h_tr$Price<2000000,]
#h_tr1=h_tr1[h_tr1$Landsize<5000,]
h_tr1=h_tr1[is.na(h_tr1$BuildingArea),]


library(ggplot2)
ggplot(h_tr,aes(x=Price,y=1:nrow(h_tr)))+geom_point() #so put landsize 1000/2=500
ggplot(h_tr1,aes(x=Price,y=BuildingArea))+geom_point() #so put landsize 1000/2=500
#buildingarea <200

range(h_tr$Landsize)
l=as.data.frame(table(h_tr$Landsize))




lapply(h_all,function(x) length(unique(x)))

unique(h_all$Address)



tapply(h_all$price,h_all$Suburb,mean,na.rm=T)

tapply(h_all$Price,h_all$Postcode,mean,na.rm=T)
h_all[h_all$Bedroom2=="7",]