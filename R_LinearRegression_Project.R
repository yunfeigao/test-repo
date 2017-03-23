# You work for Motor Trend, a magazine about the automobile industry. 
# Looking at a data set of a collection of cars, they are interested 
# in exploring the relationship between a set of variables and miles 
# per gallon (MPG) (outcome). They are particularly interested in the 
# following two questions:
#         1. "Is an automatic or manual transmission better for MPG"
#         2. "Quantify the MPG difference between automatic and manual 
#             transmissions"

data(mtcars)
boxplot(mpg~am, mtcars, varwidth=TRUE, main="mpg vs. transmissino type", 
        xlab="transmission type 0-auto 1-manual", 
        ylab="miles per gallon")

Acars<-mtcars[mtcars$am == 0, ]
Mcars<-mtcars[mtcars$am == 1, ]
meanA<-mean(Acars$mpg)
sdA<-sd(Acars$mpg)
t.test(Acars$mpg)
meanM<-mean(Mcars$mpg)
sdM<-sd(Mcars$mpg)
t.test(Mcars$mpg)

fitcars<-lm(mpg~am, mtcars)
yhat<-predict(fitcars)
plot(mtcars$am, mtcars$mpg, 
     xlab= "transmission type",
     ylab= "miles per gallon",
     # axis=(side=1, at=c(0,1)),
     main= "mpg vs. transmission type",
     pch=21, bg="lightblue", cex=1)
lines(mtcars$am, yhat, col="red", lwd=2)

# residual diagnostic
plot(mtcars$am, resid(fitcars),
     xlab="transmission type 0-auto 1-manual",
     ylab="residual",
     main="mpg vs. transmission type")
abline(0,0, col="red") # plot the residual plot
plot(fitcars, which=1) # residual vs. fitted values
dfbeta(fitcars) # calculate the beta changes when omitting each point, 
                # the large value mean influential
hatvalues(fitcars) # the percentage if y value change when omitting each 
                # point, btw 0 and 1, the large value mean influential
plot(fitcars, which=3) # Scale-location plot
plot(fitcars, which=2) # Normal QQ plot
cooks.distance((fitcars)) # the cook's distance for each sample
plot(fitcars, which=5) # cook's distance plot


