#db92728 VAN HOU HEI 
library(scatterplot3d) 
#we import the library by default
#-----------notes----------------------
#data like births  . earthquakes , aid are 2d - array
#selecting columns denote as 2dArray$column
#--------------------------------------
#---------commit------------------
#   -added function to each graph
#   -added read table method
#---------------------------------
dumbbellGraph <- function(DB)
{
  #the data are provided below so no reading.
  x= DB$Dumbbell
  y=DB$Scale	
  plot(x,y,xlab="Dumbbell Weights",ylab="Scale Weights")
  lines(x,x,col=2)
}
birthdayGraph <- function(BIRTHS) 
{
  x=BIRTHS$Population
  y=BIRTHS$Births
  fit=lm(y~x)
  co=fit$coefficients
  plot(x,y,xlab="Population (thousands)",ylab="Births (thousands)")
  lines(x,co[1]+co[2]*x,col=2,lty=3)
  
}
earthquakesGraph <- function(EARTHQUAKES) 
{
  
  L=EARTHQUAKES$Length
  W=EARTHQUAKES$Width
  A=L*W
  M_S=EARTHQUAKES$Magnitude
  plot(A,M_S,xlab="Area",ylab="Magnitude")
  logA=log(A)
  fit=lm(M_S~logA)
  co=fit$coefficients
  plot(logA,M_S,xlab="log(A)",ylab="Magnitude")
  lines(logA,co[1]+co[2]*logA,col=2,lty=3)
  D=EARTHQUAKES$Displacement
  logD=log(D)
  fit=lm(M_S~logD)
  co=fit$coefficients
  plot(logD,M_S,xlab="log(D)",ylab="Magnitude",ylim=c(5,9))
  lines(logD,co[1]+co[2]*logD,col=2,lty=3)
}
aidGraph <- function(AIDS) 
{
  x=AIDS$Year
  y=AIDS$Cumulative
  fit=lm(y~x)
  co=fit$coefficients
  plot(x,y,xlab="Year",ylab="Cumulative cases",bty="n")
  lines(x,co[1]+co[2]*x,col=2,lty=3)
  
  #x=AIDS$Year
 # y=AIDS$Cumulative
  #fit=lm(y~x)
  #co=fit$coefficients
  plot(x,log(y),xlab="Year",ylab="log Cumulative cases",bty="n")
  
  #x=AIDS$Year
  #y=AIDS$Cumulative
  fit=lm(y^(1/4)~x)
  co=fit$coefficients
  plot(x,y^(1/4),xlab="Year",ylab="4th Root Cumulative cases",ylim=c(0,30),bty="n")
  lines(x,co[1]+co[2]*x,col=2,lty=3)
}

#since the sepercator is tab , so \t , also contain header
DB=read.table("SCALE.txt", header = T,sep='\t') 
BIRTHS=read.table("BIRTHS.txt", header = T,sep='\t') #so we have to put the txt to the project folder
EARTHQUAKES=read.table("EARTHQUAKES.txt", header = T,sep='\t')
AIDS=read.table("AIDS.txt", header = T,sep='\t')
dumbbellGraph(DB); 
birthdayGraph(BIRTHS);
earthquakesGraph(EARTHQUAKES);
aidGraph(AIDS)
print("just testing all data are read correctly") 
head(BIRTHS)
head(EARTHQUAKES)
head(AIDS)
