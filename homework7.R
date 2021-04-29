# VAN HOU HEI 
# DB92728-3
# ASssignment 7
# Program for question 4 ,5
question4 <- function() 
{
  SIZE = 20
  for(x in 1:4) 
  {
    tempX = rt(SIZE , df=2) 
    qqnorm(tempX , main=paste("X" , x))
    qqline(tempX)
  }
}
SDE <- function(x,meanX ,res) #duplicate name of residuals
{
   n= length(x)
   SYX = sum( res ^ 2) / (n-2)
   return ( sqrt( 1-1/n-((x-meanX)^2) / sum((x-meanX)^2) ) * SYX)
}
question5 <- function() 
{
   x= c( 62 ,66, 69, 67, 70, 69, 73)
   y= c(123.4 ,132.2 ,147.2 ,146.8, 161.2 ,164.6 ,195.9)
   fit=lm(y~x)
   summary(fit)
   STRES= fit$residuals / SDE(x , mean(x) , fit$residuals)
   plot(fit$fitted.values,STRES)
   #question 5b
   qqnorm(STRES , main="question 5b" )
   qqline(STRES)
   #question 5c 
   plot(x,y,main="question 5c")
}
question4()
question5()
