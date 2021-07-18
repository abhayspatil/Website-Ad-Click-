library(caTools)
library(ROSE)
library(RColorBrewer)
library(caret)
library(naniar)
library(data.table)
library(ggplot2)
library(dplyr)
library(Amelia)
library(moments)
library(car)
df = read.csv("Web_data.csv")

# names of column
names(df)

# we drop some features those are insignificant for analysis

df = subset(df,select = -c(VistID,Year))
names(df)

# Check shape of data
dim(df)

# checking structure of data
str(df)

# convert some feature into  feature in factor
df$Clicked = as.factor(df$Clicked)
df$Male = as.factor(df$Male)
df$Time_Period = as.factor(df$Time_Period)
df$Weekday = as.factor(df$Weekday)
df$Month = as.factor(df$Month)

str(df)

# check missing values
colSums(is.na(df))


# Statistical functions
continuous_columns = c("Time_Spent","Age","Avg_Income","Internet_Usage")
sapply(df[,continuous_columns], mean)
sapply(df[,continuous_columns], median)
sapply(df[,continuous_columns], range)
sapply(df[,continuous_columns], sd)
sapply(df[,continuous_columns], var)


# check outluiers in data


#Time_Spent
boxplot(df$Time_Spent)

# No outleris

# Age
boxplot(df$Age)

# NO outliers in Age feature


#Avg_Income
boxplot(df$Avg_Income)
quantile(df$Avg_Income)
upper_lim = quantile(df$Avg_Income,0.75) + (1.5*(IQR(df$Avg_Income)))
upper_lim
lower_lim = quantile(df$Avg_Income,0.25) - (1.5*(IQR(df$Avg_Income)))
lower_lim

df$Avg_Income=ifelse(df$Avg_Income < 22199.50, 22199.50,df$Avg_Income)

boxplot(df$Avg_Income)

#Internet_Usage
boxplot(df$Internet_Usage)
# NO outliers in Internet_Usage feature

#Checking whether data is balanced or not
prop.table(table(df$Clicked))

# There is not that insignificant imbalance in data. so we dont need to balance.


# -------------------------------------------        EDA                      ------------------------------------

######continuous columns#####
continuous_columns = c("Time_Spent","Age","Avg_Income","Internet_Usage")
par(mfrow=c(2,2))

for (i in continuous_columns){
  hist(df[,c(i)], xlab = i, main=paste('Histogram of:',i),col=brewer.pal(8,"Paired"))
}

#####categorical columns#####
categorical_columns = c("Ad_Topic","Country_Name","City_code","Time_Period","Male","Weekday","Month","Clicked")
par(mfrow=c(3,3))

for (i in categorical_columns){
  barplot(table(df[,c(i)]), xlab = i, main=paste('Barplot of:',i),col=brewer.pal(8,"Paired"))
}


##############################Bivariate Analysis#################################

#####Categorical target vs Continuous predictors#####
par(mfrow=c(2,2))
for (i in continuous_columns){
  boxplot(df[,c(i)] ~ Clicked, ylab=i, data=df, main=paste('Boxplot of: Clicked vs ', i), 
          col=brewer.pal(8,'Paired'))
}


#ANOVA test

for (i in continuous_columns){
  test_summary=summary(aov(df[,i]~Clicked, data = df))
  print(paste("The Anova test with",i))
  print(test_summary)
}

######## Categorical Vs Categorical -- Chi-square test##############
#H0:the two columns are not correlated

Chisqcols=c("Ad_Topic","City_code","Male","Time_Period","Weekday","Month")

for (Chi_cols in Chisqcols) {
  crossTabResult=table(df[ ,c("Clicked", Chi_cols)])
  ChisResult=chisq.test(crossTabResult)
  print(Chi_cols)
  print(ChisResult)
}

#Step 1 # Split data into train and test
set.seed(333)
split = sample.split(df,SplitRatio = 0.70)

train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

str(train)
str(test)

# Checking whether Clicked feature split in proper percentage.
prop.table(table(train$Clicked))
prop.table(table(test$Clicked))


#Step 2 # 
# Train Model with logistic regression by using glm 
logit_model = glm(Clicked ~.,data = train, family = "binomial")
summary(logit_model)

# after removing country and product we get lower AIC earlier AIC was 2067 and current AIC is 1954.90

# use only some factor who are having p value < 0.05

logit_model_2 = glm(Clicked ~ Time_Spent+Age+Avg_Income+Internet_Usage, data = train, family = 'binomial')
summary(logit_model_2)

# Multicollinearity  Check
VIF=sort(vif(logit_model_2))
data.frame(VIF)

# There is no Multicollinearity  


Step 3: # predit test data
  
fitted.result = predict(logit_model_2, test, type = "response")
#fitted.result
#test$Clicked

Step 4 : # Clicked probabilty to class of 0 and 1
  
#fitted.result = ifelse(fitted.result > 0.60,1,0)  
  
fitted.result = ifelse(fitted.result > 0.55,1,0)  
#fitted.result = ifelse(fitted.result > 0.65,1,0)
#fitted.result


Step 5: # Evaluate model  Accuracy using confusion matrix
  
table(test$Clicked, fitted.result)
misclasserror = mean(fitted.result != test$Clicked)

accuracy = print(1 - misclasserror) 

# Model evaluation using confusion matrix

confusionMatrix(table(test$Clicked, fitted.result))



# Calculate F1 Score of model

library(MLmetrics)
y_pred=fitted.result
y_actual = test$Clicked
res = F1_Score(y_pred,y_actual)
print(res)

#F1 score for 0
F1_Score(y_pred = fitted.result, y_true = test$Clicked, positive = "0")


# F1 score for 1
F1_Score(y_pred = fitted.result, y_true = test$Clicked, positive = "1")




