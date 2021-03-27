#ALY6015 M4 Final_Project
#Author: Chieh-Yu.Chen
setwd("/Users/joyce/Desktop/R-Project")
getwd()
install.packages('lattice')
install.packages('ggplot2')
install.packages("dplyr")
install.packages("Rmisc")
install.packages("extrafont")
install.packages("ggthemes")
library(readr)
library(dplyr)
library(ggplot2)
library(Rmisc)
library(gridExtra)
library(extrafont)
library(ggthemes)
Heartdata<-read_csv("ALY6015_FinalProjectData.csv")
sum(is.na(Heartdata))#Luckily we have no missing values, so we can continue our EDA without having to worry about NULL values*
str(Heartdata)
# class of our data
class(Heartdata)
as.factor(Heartdata)
head(Heartdata,5)  # gives us the first 5 observations 
tail(Heartdata,5) # gives us the last 5 observations
hd<-subset(Heartdata,Heartdata$target=="1")
hd

#Sex ：because sex is in binary order，so I am using the chi-square test.
#chisq.test() to judge sex and hd
chisq.test(Heartdata$sex,Heartdata$target)

#age：because age is continuous，so I am using t.test()to see age and the target
t.test(Heartdata$age,Heartdata$target)
#t.test() of cholesterol and hd, max heart rate and hd
t.test(Heartdata$cholesterol,Heartdata$target)
t.test(Heartdata$max_heart_rate,Heartdata$target)


##Age##
# quick summary for age statistics
summary(Heartdata$age)
#Distribution for age/sex (Diseased)
g1 <- ggplot(hd, aes(x = as.factor(sex),y = age,fill=as.factor(sex)))+
  geom_boxplot() +
  labs(x="Sex", caption = " 0 = female 1 = male", fill = "sex")+
  theme(plot.caption = element_text(hjust = 0.5))
grid.arrange(g1, nrow = 1)

# chest pain type bar graph
g1 <- ggplot(Heartdata,aes(as.factor(Heartdata$`chest pain type`),fill=as.factor(Heartdata$target)))+
  geom_bar(stat="count",position="fill")+
  labs(x="Chest Pain Type",fill="Disease",y="stacked count")
g1#- Value 1: typical angina Value 2: atypical angina Value 3: non-anginal pain Value 4: asymptomatic

# Simple Horizontal Bar Plot with Added Labels
counts <- table(Heartdata$target)
barplot(counts, main="HeartDisease Distribution", horiz=FALSE,
        names.arg=c("Disease", "NotDisease"))

#boxplot for max heart rate
x<-Heartdata$target
y<-Heartdata$max_heart_rate
boxplot(x,y, main=' boxplot for continuous variables Max Heart Rate',xlab = "Disease",
        ylab = "Max Heart Rate",
        horizontal = FALSE,
        notch = FALSE)

# Stacked Bar Plot with Colors and Legend 
#(0 is normal and 1 is Disease)
#(0 is female and 1 is male)
counts <- table(Heartdata$target, Heartdata$sex)
barplot(counts, main="HeartDisease Distribution by sex",
        xlab="Disease", col=c("darkblue","red"),
        legend = rownames(counts))

##Plotting the Data##
# age and cholesterol
g_age_chol <- ggplot(Heartdata,aes(x=Heartdata$age,y=Heartdata$cholesterol))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Chol Level")+
  ggtitle("Age & Cholesterol")+
  theme(plot.title = element_text(hjust = 0.5))
g_age_chol
# age and max heart rate
g_age_maxhr <- ggplot(Heartdata,aes(x=Heartdata$age,y=Heartdata$max_heart_rate))+
  geom_point()+geom_smooth(method = "lm", se= FALSE)+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Max heart rate")+
  ggtitle("Age & Max Heart Rate")+
  theme(plot.title = element_text(hjust = 0.5))
g_age_maxhr

##Total Target##
# total cases of heart disease (target = 1)
ggplot(Heartdata, aes(as.factor(target),fill=as.factor(target)))+
  geom_bar(stat="count")+
  guides(fill=F)+
  labs(x="Target", y = "count", caption = "    0 = no heart diease
    1 = heart diease")+
  theme(plot.caption = element_text(hjust = 0.5))+
  ggtitle("Total target")+
  theme(plot.title = element_text(hjust = 0.5))

#Here I would want to build a regression prediction model.
#ensure max hr chros age and sex towards the effect of hd.
model<- glm(data = Heartdata, Heartdata$target ~ Heartdata$age +Heartdata$sex + Heartdata$max_heart_rate+Heartdata$cholesterol,family = "binomial")
summary(model)  #We know that age, sex, maximum heartrate is the important variable.
fit1<-lm(target~age+sex+max_heart_rate,data=Heartdata)
summary(fit1)
pred<-predict(fit1,Heartdata,type='response')
Heartdata$target<-ifelse(pred>=0.5, 1, 0)
newdata<-data.frame(age=29,sex=1,max_heart_rate=135)
p_new<-predict(fit1,newdata,type='response')
p_new



#Training and testing data for validation
#Split the data into Training (70%) and Testing (30%) data. Percentage of heart disease 
#or not must be same in training and testing
library(caret)
set.seed(1101)
inTrainRows <- createDataPartition(Heartdata$target,p=0.7,list=FALSE)
trainData <- Heartdata[inTrainRows,]
testData <-  Heartdata[-inTrainRows,]
nrow(trainData)/(nrow(testData)+nrow(trainData))#checking whether really 70% -> OK



