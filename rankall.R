rankall<-function(out,num=1)
{
outco <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
x<-split(outco,outco$State)
len<-length(x)
if(out=="heart attack" & is.numeric(num))
{
for(i in 1:len)
{
x[[i]]<-x[[i]][(order(as.numeric(x[[i]][,11]),x[[i]][,2],na.last=TRUE)),]
}
}
else if(out=="heart attack" & num=="worst")
{
for(i in 1:len)
{
x[[i]]<-x[[i]][(order(-as.numeric(x[[i]][,11]),x[[i]][,2],na.last=TRUE)),]
}
}
if(out=="heart failure" & is.numeric(num))
{
for(i in 1:len)
{
x[[i]]<-x[[i]][(order(as.numeric(x[[i]][,17]),x[[i]][,2],na.last=TRUE)),]
}
}
else if(out=="heart failure" & num=="worst")
{
for(i in 1:len)
{
x[[i]]<-x[[i]][(order(-as.numeric(x[[i]][,17]),x[[i]][,2],na.last=TRUE)),]
}
}

if(out=="pneumonia" & is.numeric(num))
{
for(i in 1:len)
{
x[[i]]<-x[[i]][(order(as.numeric(x[[i]][,23]),x[[i]][,2],na.last=TRUE)),]
}
}
else if(out=="pneumonia" & num=="worst")
{
for(i in 1:len)
{
x[[i]]<-x[[i]][(order(-as.numeric(x[[i]][,23]),x[[i]][,2],na.last=TRUE)),]
}
}
if(is.numeric(num))
{
for(i in 1:len)
{
print(x[[i]][num,][,2])
}
}
else if(num=="worst")
{
for(i in 1:len)
{
print(x[[i]][1,][,2])
}

}
}