# loading libraries
library(tidyverse)
library(caret)
library(knitr)
library(kableExtra)

# loading data
setwd('/Users/radeksobon/Documents/Attendance-class-problem-git/Attendance-class-problem/Raw-code')
df<-read.csv('fitness_class_2212.csv')
df<-as.data.frame(df)

# First view of data

## first look the 10 rows of data
df%>%
head(10)%>%kable(booktabs = TRUE, align = "c")%>%
  row_spec(row = 0, color = "white", background = "#808080")%>%kable_styling(position = "center")

## checking for what data types contains dataframe
glimpse(df)

## cheking a basic statistics and missing values
df%>%
  select(months_as_member, weight, days_before)%>%
  summary()%>%
  kable(booktabs = TRUE, align = "c")%>%
  row_spec(row = 0, color = "white", background = "#808080")%>%kable_styling(position = "center")

# Data wrangling and cleaning

## replace missing values with the overall average weight.
df<-df%>%
mutate(weight = if_else(is.na(weight), mean(weight, na.rm = TRUE),weight),
	   category = if_else(category == '-', 'Unknown',category))

## rounded to 2 decimal places.
df$weight <- round(df$weight,2)

## the minimum possible value is 40.00 kg
df<-df%>%
mutate(weight = if_else(weight == min(weight), 40.00,weight))

## info
summary(df)%>%kable(booktabs = TRUE, align = "c")%>%
row_spec(row = 0, color = "white", background = "#808080")
glimpse(df)

## Changing levels of factors
df <- df %>%
  mutate(day_of_week = recode(day_of_week,
                              'Fri.' = 'Fri',
                              'Monday' = 'Mon',
                              'Wednesday' = 'Wed'))

## Converts data
df$booking_id <- as.integer(df$booking_id)
df$months_as_member <- as.integer(df$months_as_member)
df$weight <- as.numeric(df$weight)
df$category<-as.factor(df$category)
df$day_of_week  <- as.factor(df$day_of_week)
df$days_before  <- as.integer(df$days_before)
df$attended   <- as.factor(df$attended)
df$time   <- as.factor(df$time)

## info
summary(df)%>%kable(booktabs = TRUE, align = "c")%>%
  row_spec(row = 0, color = "white", background = "#808080")%>%kable_styling(position = "center")
glimpse(df)

## Replacing NA's with 0
df<-df%>%
mutate(days_before = if_else(is.na(days_before), 0, days_before))

## info
summary(df)%>%kable(booktabs = TRUE, align = "c")%>%
  row_spec(row = 0, color = "white", background = "#808080")%>%kable_styling(position = "center")
glimpse(df)

## Checking for validation and missing values

## is there any much more NA'S?
any(is.na(df))
for (col in colnames(df)) {
  if (any(is.na(df[[col]]))) {
    print(paste("Column", col, "contains NA values"))
} else{
	  print('There is no NA values')
  }
}

## levels of factors
list(
'levels day_of_week' = levels(df$day_of_week),
'levels time' = levels(df$time),
'levels category' = levels(df$category),
'levels attended' = levels(df$attended))

# Visualization

## how many bookings attended the class
df%>%
mutate(attended_lab = factor(df$attended,label = c('NO', 'YES')))%>%
ggplot(aes(x = category, fill = attended_lab))+
geom_bar(position = 'dodge')+
labs(title = 'Amount of booking attended to class',
	 x = 'Class name',
	 y = 'Number of attended')+
coord_flip()+
theme_dark()

## top 3 attended categories of classes

df<-df%>%
mutate(attended_lab = factor(df$attended,label = c('NO', 'YES')))

df%>%
select(category, attended_lab)%>%
count(category, attended_lab)%>%
rename('quantity' ='n')%>%
top_n(3)%>%
arrange(desc(quantity))

# The most attended variable of category is HIIT with 454 quantity There is not balanced across categories because the quantity are significantly different

# The most attended variable of category is HIIT with 454 quantity
   
# There is not balanced across categories because the quantity of are significantly different


# Distribution

# Calculating range for making sure which binwidth will best fitting for distribution

# How much there is the rows?
df%>%nrow()

# Calculate the range for being sure how much binwidth i have to set
df%>%
mutate(range_month = (max(months_as_member)-min(months_as_member)) / 1500)%>%
select(range_month)%>%
head(1)


df%>%
ggplot(aes(months_as_member)) + 
geom_histogram(binwidth = 0.098, color = 'grey')+
labs(title = 'Distribution of the number of months as a number',
	 x = 'Number of months as a member',
	 y = 'Counts')+
theme_dark()

# This is the right skewed histogram displaying the number of months as fitness class member. From this visualization we can pull some information like the most frequency observation is in the interval [0,25] months. So we can conclude that mode the number of most months members is in approximately **12-15** months.

df%>%
ggplot(aes(months_as_member, fill = attended_lab)) + 
geom_boxplot()+
facet_wrap(~attended_lab)+
labs(title = 'Distribution months_as_member', x = 'Counts', y = 'Number of monts as a member')+
coord_flip()+
theme_dark()

## attendent
df%>%
filter(attended==1)%>%
select(months_as_member)%>%
summary()%>%kable(booktabs = TRUE, align = "c")%>%
  row_spec(row = 0, color = "white", background = "#808080")%>%kable_styling(position = "center")

## non-attendent
df%>%
filter(attended==0)%>%
select(months_as_member)%>%
summary()%>%kable(booktabs = TRUE, align = "c")%>%
  row_spec(row = 0, color = "white", background = "#808080")%>%kable_styling(position = "center")

# For both attendent and non-attendent groups, the median (or 50th percentile) number of months as a member is relatively low, with values of 10 and 20, respectively. This suggests that a significant proportion of members have joined relatively recently.

# The range of values for the non-attendent group (1 to 57 months) is smaller than for the attendent group (4 to 148 months). This suggests that there is more variability in the number of months as a member among attendent members.

# The mean number of months as a member for the non-attendent group (11.5 months) is lower than for the attendent group (25.15 months). This suggests that members who non-attend classes are, on average, less experienced than those who do attend.

# The median months as a member for those who non-attended the class is slightly higher than for those who did attend.  

# The distribution of months_as_member for people who attend classes shows several outliers with a big range, indicating that there are individuals who have been members for a significantly longer period than the majority of the attendants. However, the distribution of non-attendants is small to the attendants' outliers, which suggests that there may not be a significant difference between the two groups in terms of long-term membership.

# Relationship between attendance and number of months as member

df%>%
ggplot(aes(months_as_member,fill = attended))+
geom_bar() +
labs(title = 'Relationship beetween attendance and number of months as a member', x = "Number the month of member", y = 'Counts') +
facet_wrap(~attended_lab, nrow = 1)+
theme_dark()


# Bar chart below displays the relationship beetween attendance and number of months as a member

# It shows that even if attendence is "Yes" or "No", we can conclude that, most of attend is in relation with number of members class, because there is the same relationship in separate panels for each level of "attended".

# Problem classification

# The business wants to predict whether members will attend using the provided data, which is a binary outcome (attend or not attend). Therefore, this is a classification problem.


# Classification model

## Baseline logistic model

# split the data into training and testing sets
set.seed(123)
train <- sample(nrow(df), nrow(df)*0.7)
train_data <- df[train, ]
test_data <- df[-train, ]

# fit a logistic regression model
model_glm <- glm(attended ~ months_as_member, data=train_data, family=binomial)

# make predictions on the testing set
pred <- predict(model_glm, newdata=test_data, type="response")

# convert probabilities to binary predictions
pred_class <- ifelse(pred >= 0.5, 1, 0)
accuracy <- mean(pred_class == test_data$attended)
summary(model_glm)
accuracy


## Comparison model
model_glm2 <- glm(attended ~ months_as_member + weight + time + category, data = train_data, family=binomial)

# make predictions on the testing set
pred2 <- predict(model_glm2, newdata=test_data, type="response")

#  onvert probabilities to binary predictions
pred_class2 <- ifelse(pred2 >= 0.5, 1, 0)
summary(model_glm2)
accuracy2 <- mean(pred_class2 == test_data$attended)
accuracy2

# The problem of predicting in this case is classification because of binary system as "0" coressponding to "not attending" and "1" to "attending"

Logistic regression is common used algorithm for binary system. It can provide insights from the models probability of the binary outcome using a logistic function.
Therefore, if we add more variables to comparison model which is also logistic model, to check impact on attendace and we can try to increase accuracy.

## Suitable model performance 

cm1 <- confusionMatrix(table(pred_class, test_data$attended), positive = "1")
cm2 <- confusionMatrix(table(pred_class2, test_data$attended), positive = "1")

cm1
cm2


# Conclusions

# Based on the confusion matrix and statistics provided, it appears that the model is predicting whether a member will attend the class or not. The confusion matrix shows the count of true positive (TP), true negative (TN), false positive (FP), and false negative (FN) predictions. In this case, the "Positive" class is the class attendance (1), and the "Negative" class is non-attendance (0).

## The precision score (also called positive predictive value) for the "Positive" class is 0.6914 in the first confusion matrix and 0.6625 in the second confusion matrix. This means that when the model predicts that a member will attend the class (1), it is correct approximately 69.1% of the time in the first matrix and 66.25% of the time in the second matrix.

## The recall score (also called sensitivity) for the "Positive" class is 0.4628 in the first confusion matrix and 0.4380 in the second confusion matrix. This means that the model correctly identifies approximately 46.28% of the members who attended the class (1) in the first matrix and 43.80% in the second matrix.

## The F1 score is the harmonic mean of precision and recall and provides a balance between the two. The F1 score for the "Positive" class is not explicitly given in the provided statistics, but it can be calculated based on precision and recall values.

# Overall, the model appears to have a moderate level of accuracy in predicting class attendance, with an accuracy of 0.8 in the first confusion matrix and 0.7889 in the second matrix. However, the sensitivity scores for the "Positive" class are relatively low, indicating that the model may miss a significant proportion of members who attend the class. Further analysis and evaluation of the model may be necessary to determine its overall effectiveness in predicting class attendance.

