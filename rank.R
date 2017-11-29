rankhospital<-function(state,out,num=1)
{
outco <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
hello<-outco[(outco$State==state),]
if(out=="heart attack" & is.numeric(num))
{
 p<-hello[(order(as.numeric(hello[,11]),hello[,2],na.last=TRUE)),]
 hos<-p[num,][,2]
}
else if(out=="heart attack" & num=="worst")
{
 p<-hello[(order(- as.numeric(hello[,11]),hello[,2],na.last=TRUE)),]
 hos<-p[1,][,2]
}
if(out=="heart failure" & is.numeric(num))
{
p<-hello[(order(as.numeric(hello[,17]),hello[,2],na.last=TRUE)),]
hos<-p[num,][,2]
}
else if(out=="heart failure" & num=="worst")
{
 p<-hello[(order(- as.numeric(hello[,17]),hello[,2],na.last=TRUE)),]
 hos<-p[1,][,2]
}

if(out=="pneumonia" & is.numeric(num))
{
p<-hello[(order(as.numeric(hello[,23]),hello[,2],na.last=TRUE)),]
hos<-p[num,][,2]
}
else if(out=="pneumonia" & num=="worst")
{
 p<-hello[(order(- as.numeric(hello[,23]),hello[,2],na.last=TRUE)),]
 hos<-p[1,][,2]
}
print(hos)
}
