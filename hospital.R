best <- function(state, out) {
outco <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
hello<-outco[(outco$State==state),]
if(out=="heart attack")
{
rate<-as.numeric(hello[,11])
les<-min(rate,na.rm=TRUE)
if(les%%1==0)
{
les<-toString(les)
les<-paste(les,"0",sep=".")
}
else
{les<-toString(les)}
print(les)
hos<-hello[(hello[,11]==les),]
}
if(out=="heart failure")
{
rate<-as.numeric(hello[,17])
les<-min(rate,na.rm=TRUE)
if(les%%1==0)
{
les<-toString(les)
les<-paste(les,"0",sep=".")
}
else
{les<-toString(les)}
print(les)
hos<-hello[(hello[,17]==les),]
}
if(out=="pneumonia")
{
rate<-as.numeric(hello[,23]) 
les<-min(rate,na.rm=TRUE)
if(les%%1==0)
{
les<-toString(les)
les<-paste(les,"0",sep=".")
}
else
{les<-toString(les)}
print(les)
hos<-hello[(hello[,23]==les),]
}
print(hos[,2])
}
