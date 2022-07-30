install.packages("dplyr")
library(dplyr)

install.packages("e1071")
library(e1071)

#Loading Data Set
data("USArrests")
summary(USArrests)

##############################
#Function 1  #Create function#
##############################

x <- USArrests
x

calc <- function(i=10){
  y = (x$UrbanPop * i)
  return(y)
}

calc()

##################################
#draw the 2 plots in the same raw#
##################################

  par(mfrow=c(1,2))

#Function 2  #Histogram plot
############################

hist(x$Rape, col = rainbow(0), xlab = "rape", main = "", )
mtext(text = "USArrests Plot", side = 3)


#Function 3  #scatter plot
##########################
plot(x$Assault, ylim = c(45,100), ylab = "Assault", pch = 4)

#############################
#Function 4  #Data wrangling#
#############################

x %>% filter(Murder < 5) %>% arrange(desc(Murder))

#################################
#Function 5  #Hypothesis testing#
#################################

t.test(x$Murder,x$Rape,var.equal = TRUE)
qt(p = 0.05, df = 98, lower.tail = FALSE)
print("Reject H0 then Murder and rape aren't equal in variance")

t.test(x$Murder,x$Assault,var.equal = FALSE)
qt(p = 0.05, df = 49.268, lower.tail = FALSE)
print("Reject H0 then Murder and Assault are equal in variance")

################################
#Function 6  #Linear regression#
################################

fit <- lm(x$UrbanPop ~ x$Murder + x$Assault +x$Rape)
fit

attributes(fit) 

fit$coefficients

residuals(fit) 

#correlation
cor(x$UrbanPop,x$Rape)

#prediction
UrbanPopPrediction <- fit$coefficients[1] + fit$coefficients[2]*11 + fit$coefficients[3]*110 + fit$coefficients[4]*21.2
UrbanPopPrediction

####################
#Function 7 (Bonus)#
####################
y <- USArrests
y
y$UrbanPop[USArrests$UrbanPop >= 65] = "high"
y$UrbanPop[USArrests$UrbanPop < 65] = "low"
y
View(USArrests)

newdt <- rbind(USArrests[1:25,])
newdt
testdt <- rbind(USArrests[26:50,])
testdt

y
classX <- naiveBayes(UrbanPop ~ Murder + Assault + Rape, data = USArrests)
prediction <- predict(classX, as.data.frame(USArrests))
#prediction

arr <- array(prediction)
#arr

arr[arr >= 65] = "high"
arr[arr < 65] = "low"
arr
length(arr)

table(prediction)


