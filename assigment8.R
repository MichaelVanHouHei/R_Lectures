#db927283 VAN HOU HEI
#remotes::install_github("cran/alr3")
#since normal install with wrong version,so install it from github
#alr3 package is used to find sspe and sslof 
library(alr3)
##begin of 3(a)
Magnitude <- c(5.9 ,6.2 ,6.7, 6.8, 7 ,7, 7, 7.3 ,7.6, 8, 8)
Energy <- c(23.4 ,24.01, 24.6, 24.57, 24.53, 25.6, 25.48, 25.42, 26.09, 26.73, 26.59)
fit <- lm(Energy~Magnitude)
SS_res <- sum((fit$residuals)^2)
cof <-coef(fit)
print(paste("the cof are a_bar , b_bar",cof ) )
print(paste("and the SS_res is " , SS_res))
#end of 3(a) 
#begin 3(b)
standardizedResidulals <- rstandard(fit)
plot(fit$fitted.values,fit$residuals)
qqnorm(standardizedResidulals)
qqline(standardizedResidulals)
#end of 3(b) 
#begin 3(c)
#the pureErrorAnova is from alr3
#beside using library alr3, we can factor(energy),factor(mangitude) , make a fit,anvoa(two fit)
#the result will be equivalence  as pureErrorAnova
anovaData <- pureErrorAnova(fit)
SS_pe <-anovaData$`Sum Sq`[4]
pe_df <- anovaData$Df[4]
SS_lof <- anovaData$`Sum Sq`[3] 
#since SS_lof = SS_res - ss_pe
# SS_lof <- SS_res - SS_pe (the question required subtraction ??? ,if yes comment the upper code)
lof_df <- anovaData$`Df`[3]
print(anovaData)
print(paste("SS_pe" , SS_pe))
print(paste("SS_lof" ,SS_lof ))
#(SS_lof / lof_df) / (SS_pe/pe_df) this is how F calcuated 
print(paste("F-test:" , anovaData$`F value`[3]  ))
print(paste("we reject the region H0 0.05 since the f-value " , anovaData$`F value`[3]  , "larger than 0.05"))
