names(data)<-c("age","fev","ht","sex","smoke")
cor(data.frame(data$age, data$ht, data$sex, data$smoke))
plot(predict(lm(fev~age+ht+sex+smoke,data)), resid(lm(fev~age+ht+sex+smoke,data)), ylab="Residuals",xlab="Fitted Values",main="Level-Level Residuals vs Fits Plot")
abline(0,0,col="red")
plot(predict(lm(log(fev)~age+ht+sex+smoke,data)), resid(lm(log(fev)~age+ht+sex+smoke,data)), ylab="Residuals",xlab="Fitted Values",main="Log-Level Residuals vs Fits Plot")
abline(0,0,col="red")
lm(log(fev)~age+ht+sex+smoke,data)
summary(lm(log(fev)~age+ht+sex+smoke,data))
hist(data$fev,main="Histogram of FEV for all", xlab="FEV")
hist(data$fev,main="Histogram of airex for all", xlab="Airex", col="light blue")
hist(log(data$fev),main="Histrogram of log(FEV) for all", xlab="log(FEV)")
max(data$age)
min(data$age)
library(moments)
skewness(data$fev)
logfev<-log(data$fev)
skewness(logfev)
library(lmtest)
bptest(lm(log(fev)~smoke+age+ht+sex, data))
bptest(lm(fev~smoke+age+ht+sex, data))
smokers<-data$fev[data$smoke==1]
nonsmokers<-data$fev[data$smoke==0]
t.test(smokers, nonsmokers)

cor(data.frame(data$age, data$ht, data$sex, data$smoke))
library(psych)
pairs.panels(data.frame(data$age, data$ht, data$sex, data$smoke))
library(car)
vif(lm(log(fev)~age+ht+sex+smoke,data))
install.packages("corrplot")
library(corrplot)
Age<-data$age
Height<-data$ht
Gender<-data$sex
Smoke<-data$smoke
corplot<-cor(data.frame(Age, Height, Gender, Smoke))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corplot, method="color", col=col(200),type="upper", order="hclust", addCoef.col = "black", tl.col="black", tl.srt=45, sig.level = 0.01)

summary(lm(log(fev)~age+ht+sex,data))
summary(lm(log(fev)~age+ht+sex+smoke,data))
summary(lm(log(fev)~age,data))
summary(lm(log(fev)~smoke,data))
