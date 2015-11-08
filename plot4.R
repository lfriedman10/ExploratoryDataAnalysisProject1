remove(list=ls())
library(dplyr)
setwd("C:/Users/leef/Dropbox/Coursera_R_Stuff/Course_4_ExploratoryAnalysis/Project1")
mydf <- read.csv("household_power_consumption.txt", header=TRUE, sep=";",
                 stringsAsFactors=FALSE)
mydf<-mutate(mydf, NewDate=as.Date(Date,"%d/%m/%Y"))
mydf<-mutate(mydf, Year=as.numeric(substr(NewDate,start=1,stop=4)))
mydf<-mutate(mydf, Month=as.numeric(substr(NewDate,start=6,stop=7)))
mydf<-mutate(mydf, Day=as.numeric(substr(NewDate,start=9,stop=10)))
mydf<-mutate(mydf, Hour=as.numeric(substr(Time,start=1,stop=2)))
mydf<-mutate(mydf, Min=as.numeric(substr(Time,start=4,stop=5)))
mydf<-filter(mydf,Year==2007 & Month == 2 & Day <3)
mydf<-arrange(mydf,Year,Month,Day,Hour,Min)
myrows<-as.numeric(row.names(mydf))

dev.off()
par(mfrow = c(2,2),plt=c(0.2, 0.9, 0.2, 0.8),oma=c(3,0,0,0))

Global_active_power<-as.numeric(mydf$Global_active_power)
plot(Global_active_power~myrows,type="n",ylab="Global Active Power"
     ,xlab="",xaxt="n")
lines(myrows,Global_active_power,type="l")
axis(1,at=c(1,1440,2880),labels=c("Thu","Fri","Sat"))

Voltage=as.numeric(mydf$Voltage)
plot(Voltage~myrows,type="n",ylab="Voltage",
     xaxt="n",xlab='')
lines(myrows,Voltage,type="l")
axis(1,at=c(1,1440,2880),labels=c("Thu","Fri","Sat"))
mtext("datetime",1,line=3,cex=1.0)

mydf<-rename(mydf,S1=Sub_metering_1,S2=Sub_metering_2,S3=Sub_metering_3)
S1<-as.numeric(mydf$S1)
S2<-as.numeric(mydf$S2)
S3<-as.numeric(mydf$S3)
plot(myrows,S1,type="n",ylab="Energy sub metering",xaxt="n",xlab='')
lines(myrows,S1,col="black")
lines(myrows,S2,col="red")
lines(myrows,S3,col="blue")
axis(1,at=c(1,1440,2880),labels=c("Thu","Fri","Sat"))
# par(xpd = TRUE)
legend(x=800,y=41,
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       lty=c(1,1),col=c("black","red","blue"),cex=0.75,bty="n")

reactive=as.numeric(mydf$Global_reactive_power)
plot(reactive~myrows,type="n",ylab="Global_reactive_power",
     xaxt="n",xlab='')
lines(myrows,reactive,type="l")
axis(1,at=c(1,1440,2880),labels=c("Thu","Fri","Sat"))
mtext("datetime",1,line=3,cex=1.0)

dev.copy(png,file="plot4.png")
dev.off()

