deathrates=read.csv("deathrates2.csv",header = T,sep=";",dec=",",na.strings = "")
deathrates[which.max(deathrates$All.Races.Rate),]
deathrates[order(deathrates$All.Races.Rate,decreasing = T),]
deathrates=deathrates[order(deathrates$year),]

plot(Florida.ts)
for(i in 1:length(deathrates$Area))

{
  a=deathrates[i,"Area"]
  a=as.character(a)
  b=geocode(a,source="dsk")
  deathrates[i,"long"]=b$lon
  deathrates[i,"lat"]=b$lat
  
}
for(i in 1:length(deathrates$Area))
{
  deathrates$long[i]=deathrates$long[i]*pi/180
  deathrates$lat[i]=deathrates$lat[i]*pi/180
}
R = 6371

str(deathrates)
head(deathrates)
attach(deathrates)
fu = function(s){
  d=deathrates[which(deathrates$Area==s),]
  return (d)
}
View(deathrates)
Florida=fu("Florida")
FloridaRate=Florida[,2]
Florida.ts=ts(FloridaRate,start =2005,frequency=1)
library(astsa)
acf(Florida.ts)
str(Flo)
plot(decompose(Florida.ts))
plot.ts(Florida.ts)
####
m=lm(All.Races.Rate~year,data=Florida)
summary(m)
adf.test(Florida.ts)
##Série temporelle:
library(forecast)
fit2<- auto.arima(Florida.ts)
diff(Florida.ts)
forecast(fit2, 22)
plot(forecast(fit2, 10))
autoplot(fit2) +forecast::geom_forecast()
fit3=forecast::nnetar(Florida.ts,size=8)
fcast3 <-forecast(fit3,h=12)
fcast3
autoplot(forecast(fit3))
##
m2=arima(Florida.ts,order=c(1,0,0))
tsdiag(m2)
plot()
library('forecast')
library('tseries')
prediction = forecast(m2,h=5)
plot(prediction)
library(tseries)

New_York=fu("New York")
New_YorkRate=New_York[,2]
New_York.ts=ts(New_YorkRate,start =2005,frequency=1)
Connecticut=fu("Connecticut")
ConnecticutRate=Connecticut[,2]
Connecticut.ts=ts(ConnecticutRate,start =2005,frequency=1)
Maine=fu("Maine")
MaineRate=Maine[,2]
Maine.ts=ts(MaineRate,start =2005,frequency=1)
Alabama=fu("Alabama")
AlabamaRate=Alabama[,2]
Alabama.ts=ts(AlabamaRate,start =2005,frequency=1)
NH=fu("New Hampshire")
NHRate=NH[,2]
NH.ts=ts(NHRate,start =2005,frequency=1)

plot.ts(Florida.ts)



ts.df=cbind.data.frame(Florida.ts,New_York.ts,NH.ts,Connecticut.ts,Alabama.ts)
#ts.df2=cbind(diff(Tun.ts),diff(alg.ts),diff(lib.ts))

ts.df=as.matrix(ts.df)
View(ts.df )
dist.df1=matrix(c(0,1,1,0,0,1,1,0,0),nrow = 3,byrow = T)
id=matrix(c(1,0,0,0,1,0,0,0,1),nrow = 3)

wlis=list(id,dist.df1)

library(starma)
model=starma(ts.df,wlis,ar=1,ma=0)
summary(model)
plot(model$residuals,pch=19)



####
p=2
s4=GSTAR(ts.df,S,p)
library(MASS)
class(m)
summary(s4)

s$Fit$residuals
k<-ncol(ts.df)                                   
# Number Location   
n<-nrow(ts.df)
Residual<-as.data.frame(matrix(s$Fit$residuals,(n-p),k,byrow = T))
Residual=ts(Residual)
plot(Residual,plot.type="single", lty=1:3, col = 4:2,main="Residual")
legend(1,-2, legend=c("Line 1", "Line 2","kk"),
       col=4:2, lty=1:3, cex=0.8)
zt<-(stack(as.data.frame(t(ts.df)))[,1]) 
ts.df=as.data.frame(ts.df)
??plot()
predict(s,zt)
nclass(s)


s2=GSTAR(ts.df,S,2)
summary(s2)
###distance
?geocode
a=geocode("Alabama",source="dsk")
a=deathrates$Area[2]
a=as.character(a)
b=geocode(a,source="dsk")
b$lon
deathrates[3,7]=b$lon
data2=rbind(Florida[1,c(1,7,8)],New_York[1,c(1,7,8)],NH[1,c(1,7,8)],Connecticut[1,c(1,7,8)],Alabama[1,c(1,7,8)])
for (i in 1:nrow(data2) )
{
  for (j in 1:nrow(data2) )
  {
    M[i,j]=R*acos(cos(data2$long[i]-data2$long[j])*cos(data2$lat[i])*cos(data2$lat[j])+(sin(data2$lat[i])*sin(data2$lat[j])))
  }
  
}
M=matrix(1:25,nrow=5)
S=matrix(1:25,nrow=5)
M[1,1]=0
?matrix
for (i in 1:5 )
{
  for (j in 1:5 )
  {
    if(i==1)
    {
    S[i,j]=(1/M[i,j])/(1/M[i,2]+1/M[i,3]+1/M[i,4]+1/M[i,5])
    }
    else if(i==2)
    {
      S[i,j]=(1/M[i,j])/(1/M[i,1]+1/M[i,3]+1/M[i,4]+1/M[i,5])
    }
    else if(i==3)
    {
      S[i,j]=(1/M[i,j])/(1/M[i,1]+1/M[i,2]+1/M[i,4]+1/M[i,5])
    }
    else if(i==4)
    {
      S[i,j]=(1/M[i,j])/(1/M[i,1]+1/M[i,3]+1/M[i,2]+1/M[i,5])
    }
    else if(i==5)
    {
      S[i,j]=(1/M[i,j])/(1/M[i,1]+1/M[i,3]+1/M[i,4]+1/M[i,2])
    }
    
  }
}
S[1,1]=S[2,2]=S[3,3]=S[4,4]=S[5,5]=0
sum(S[1,2]+S[1,3]+S[1,4]+S[1,5])
######


library("maps")
library("ggplot2")
d=deathrates[which(deathrates$year==2014),]
p = ggplot(d, aes(x = long, y = lat)) + 
  geom_polygon(aes(fill = d$All.Races.Rate)) + 
  expand_limits() + 
  theme_minimal()
