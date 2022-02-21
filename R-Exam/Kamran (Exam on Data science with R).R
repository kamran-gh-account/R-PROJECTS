

# Importing required libraries
library(tidyverse)
library(readxl)
library(caret)
library(yardstick)
library(lime)
library(funModeling)
library(rsample)
library(recipes)
library(purrr)
library(ggthemes)
library(tidyquant)
library(corrplot)



### Q.1 Write an R program to create a sequence of numbers from 20 
#to 50 and find the mean of numbers from 20 to 60 and the 
#sum of numbers from 51 to 91

#Sequence of numbers from 20 to 50 
print(seq(20,50))

#Mean of numbers from 20 to 60
print(mean(20:60))

#Sum of numbers from 51 to 91
print(sum(51:91))



### Q.2 A student scored 70 marks in English, 95 marks in Science, 80 
#marks in Maths and 74 marks in History. Write an R program 
#to plot a simple bar chart displaying the scores of the given 
#subjects.

marks = c(70, 95, 80, 74)
barplot(marks,
        main = "Scores of 5 subjects",
        xlab = "Marks",
        ylab = "Subject",
        names.arg = c("English", "Science", "Math.", "Hist."),
        col = "grey",
        horiz = F)



### Q.3 Write a R program to create a data frame to store the 
#following details of 5 employees.

employees = data.frame(Name=c("Anastasia S","Dima R","Katherine S", "JAMES A","LAURA MARTIN"),
                       Gender=c("M","M","F","F","M"),
                       Age=c(23,22,25,26,32),
                       Designation=c("Clerk","Manager","Exective","CEO","ASSISTANT"),
                       SSN=c("123-34-2346","123-44-779","556-24-433","123-98-987","679-77-576"))

print(employees)



### Q.4 Write a R program to create a list of heterogeneous data, 
#which includes character, numeric and logical vectors. Print 
#the list.

info = list(exam_name="R", duration = 3 , Online=TRUE)
print(info)

typeof(info)

print(class(info$exam_name))
print(class(info$duration))
print(class(info$Online))



### Q.5 Write an R program to convert a given matrix to a 1-
#dimensional array.

Matrix=matrix(1:12,3,4)
print(Matrix)
one_D = as.vector(Matrix)
print(one_D)
dim(one_D)



### Q.6 Write a R program to create a list containing a given vector, a 
#matrix, and a list and add an element at the end of the list

list_data <- list(c("Red","Green","Black"),
                  matrix(c(1,3,5,7,9,11), nrow = 2),
                  list("Python", "PHP", "Java"))
print("List:")
print(list_data)
print("Add a new element at the end of the list:")
list_data[4] = "New element"
print("New list:")
print(list_data)



### Q.7 Write an R program to merge two given lists into one list.
#List1= list(1,2,3)
#List2 = list("Red", "Green", "Black")

List1= list(1,2,3)
List2 = list("Red", "Green", "Black")
print("Original lists:")
print(List1)
print(List2)
print("Merge List1 and List2 lists:")
mlist =  c(List1, List2)
print("Merged list:")
print(mlist)



### Q.8 Write a R program to convert a given data frame to a list by rows

exam_data = data.frame(
  name = c('Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'),
  score = c(12.5, 9, 16.5, 12, 9, 20, 14.5, 13.5, 8, 19),
  attempts = c(1, 3, 2, 3, 2, 3, 1, 1, 2, 1),
  qualify = c('yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes')
)
print("Original dataframe:")
print(exam_data)
new_list = split(exam_data, seq(nrow(exam_data)))
print("dataframe to list by rows:")
print(new_list)



### Q.9 Write an R program to create a correlation matrix from a data 
#frame of the same data type.

d = data.frame(x1=rnorm(5),
               x2=rnorm(5),
               x3=rnorm(5))
print(d)

result = cor(d) 
print("Correlation matrix:")
print(result)



### Q.10 Write an R program to rotate a given matrix 90 degrees clockwise

x =  matrix(1:9, 3)
print("Original matrix:")
print(x)
rotate = t(apply(x, 2, rev))
print("Rotate the given matrix 90 degree clockwise:")
print(rotate)



### Q.11 Check for missing values in the 'mtcars' data set. 

#Importing the 'mtcars' dataset as df
df <- mtcars
head(df)
view(df)

# Checking null values in train data
NA_values = data.frame(na_no = colSums(is.na(df)))
NA_values
#No null values are present



### Q.12 Check which attributes are important to determine the mpg of 
#a car in the 'mtcars' data set.

model_mtcars <- lm(mpg ~ . , data = mtcars)  
summary(model_mtcars) 

importance <- coef(model_mtcars)
sort(importance)



### Q.13 Build a simple linear model to predict the mpg of a car in the 
#'mtcars' data set. 

mtcars$amfactor <- factor(mtcars$am, labels = c("automatic", "manual")) 
summary(lm(mpg ~ factor(amfactor), data = mtcars))$coef



### Q.14 Build a logistic regression model using the glm function to 
#know the effect of admission into graduate school. The target 
#variable, admit/don't admit, is a binary variable
#Use the given "binary.csv" dataset

# Logistic Regression

# Read data file
mydata <- read.csv('C:/download/R Datasets/Exam on data science with R/Dataset/binary.csv',header = T)

str(mydata)
mydata$admit <- as.factor(mydata$admit)
mydata$rank <- as.factor(mydata$rank)

# Two-way table of factor variables
xtabs(~admit + rank, data = mydata)

# Partition data - train (80%) & test (20%)
set.seed(1234)
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.8, 0.2))
train <- mydata[ind==1,]
test <- mydata[ind==2,]

# Logistic regression model
mymodel <- glm(admit ~ gpa + rank, data = train, family = 'binomial')
summary(mymodel)

# Prediction
p1 <- predict(mymodel, train, type = 'response')
confusionMatrix(p1,test$admit)
head(p1)
head(train)

# Misclassification error - train data
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$admit)
tab1
1 - sum(diag(tab1))/sum(tab1)

# Misclassification error - test data
p2 <- predict(mymodel, test, type = 'response')
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$admit)
tab2
1 - sum(diag(tab2))/sum(tab2)

# Goodness-of-fit test
with(mymodel, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail = F))



### Q.15 Use the given variables from the titanic dataset and build the 
#decision tree on train data.
#Variables from dataset: survived, embarked, sex, sibsp, parch, fare

#Importing the Titanic datasetas df_tc

df_tc <- read.csv('C:/Imarticus/Titanic dataset/train.csv')
df_tc
view(df_tc)
str(df_tc)

dim(df_tc)

#check for null values
sum(is.na(df_tc))

library(dplyr)
# Exclude the missing observations
new_titanic <-df_tc %>%
  na.omit()		
dim(new_titanic)

#Splitting the data
.8 * 714
s <- sample(714,572)
train <- new_titanic[s,]
test <- new_titanic[-s,]
dim(train)
dim(test)


#Build the decision tree model
library(rpart)
DT <- rpart(Survived~ Embarked + Sex + SibSp + Parch + Fare 
            ,train,method = 'class')    #Build decision tree model

DT



### Q.16 Create a plot to display the result of decision tree.

library(rpart.plot)
rpart.plot(DT, tweak=1.6)
rpart.plot(DT, type = 4,extra=101, tweak = 1.6)



### Q.17 Create the confusion matrix for the above model. 

pred <- predict(DT,test, type = 'class')
library(caret)
confusionMatrix(table(pred,test$Survived))



### Q.18 Perform k-means clustering on USArrest dataset. Scale the data 
#before performing clustering.

data(USArrests)
View(USArrests)

# Scale the data
us_arrest <- scale(USArrests)
head(us_arrest)

set.seed(123)
kmean_arrest <- kmeans(us_arrest, centers = 4, nstart = 50)
kmean_arrest

# visualisation of clusters 
plot(x=sus_arrests[,1], y=us_arrest[,2], col=kmean_arrest$cluster)
points(kmean_arrest$centers, pch=3, cex=2)



### Q.19 Print the cluster number for each observation and cluster size 
#for the above k-means model.

fviz_nbclust(us_arrest, kmeans, method = 'wss') +
  geom_vline(xintercept = 4, linetype=5, col= "darkred")

aggregate(USArrests, by=list(cluster=kmean_arrest$cluster), mean)

# Cluster number for each of the observations
kmean_arrest$cluster
head(kmean_arrest$cluster, 4)

# Cluster size
kmean_arrest$size

# Cluster means
kmean_arrest$centers



### Q.20 Plot the result of the k-means cluster. 

#install.packages("factoextra")
library("factoextra")

fviz_cluster(kmean_arrest, data = us_arrest,
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot")


