#Machine Learning Model for weather forecast Events (Rain, Snow, Fog)

#I have changed tables to weather2 at some places as later in the checking 
#of density plot we found out that we have to remove outliers. Hence first 
#checking normally and then without the outliers

#Read file and importing it into global environment
weather <- read.csv("weather2.csv")

install.packages("caret")
install.packages("lattice")
library(caret)
library(ggplot2)
library(lattice)
library(tidyverse)

#Attach the weather dataset to the environment
View(weather)
names(weather)

# Select essential variables required and remove the rest
mlweather <- weather %>% 
  filter(Events %in% c("Rain", "Snow", "Fog")) %>% 
  select(Date, Events, CloudCover, Mean.VisibilityMiles, PrecipitationIn, Mean.Humidity)

#For creating a list of 80% of the rows in the original dataset we can use for training
validation_indexml <- createDataPartition(mlweather$Events, p=0.80, list=FALSE)
#For selecting 20% of the data for validation
validationml <- mlweather2[-validation_indexml,]
#For using the remaining 80% of data to training and testing the models
mlweather <- mlweather[validation_indexml,]


#For Checking no of rows and columns
dim(mlweather)
#For Checking name and data types of all columns
sapply(mlweather, class)
#For Checking first 7 rows for outlook of data
head(mlweather)
#For Checking for levels in a specific variable in a group
levels(mlweather$Events)
#Changing data type from character to factor
mlweather$Events <- as.factor(mlweather$Events)


# summarize the class distribution (Checking frequency and % of specific variable)
percentml <- prop.table(table(mlweather$Events)) * 100
cbind(freq=table(mlweather$Events), percentage = percentml)

#Check the basic stats of data for attribute distributions 
summary(mlweather)


#After getting some understanding of data visualise relational variables
#Here we will look at univariate and multivariate plot

#Univariate plots
#It is helpful with visualization to have a way to refer to just the 
#input attributes and just the output attributes. 
#Let’s set that up and call the inputs attributes x and the output attribute (or class) y.
# split input and output
inputml <- mlweather2[,3:6]
outputml <- mlweather2[,2]

rm(inputml)
#Given that the input variables are numeric, we can create box and whisker plots of each
# boxplot for each attribute on one image
par(mfrow=c(3,4,5,6))
for(i in 3:6) {
  boxplot(inputml[,i], main=names(mlweather2)[i])
}

#We can also create a barplot of the Events class variable to get 
#a graphical representation of the class distribution 
plot(outputml)

install.packages("ellipse")
install.packages("latticeExtra")
library("ellipse")
library("latticeExtra")

#Multivariate plots
# scatterplot matrix for all variables
featurePlot(x = inputml, 
            y = outputml, plot = "ellipse")
#We can see some clear relationships between the input attributes (trends) 
#and between attributes and the class values (ellipses)

# box and whisker plots for each attribute
featurePlot(x = inputml, 
            y = outputml, plot = "box")
#This is useful to see that there are clearly different distributions of the attributes for each class value.


#Drawing the ggplot to get the sense of how variables interrelate with each other
mlweather2 %>% 
  ggplot(aes (x = Mean.Humidity, y = PrecipitationIn, size = CloudCover, color = Mean.VisibilityMiles))+
  geom_point()+
  facet_wrap(~Events)+
  labs(title = "Weather",
       x = "Mean Humidity (g.m-3)", y = "Precipitation (mm)")+ 
  theme_bw()


#Next we can get an idea of the distribution of each attribute, again like the box and whisker plots,
#broken down by class value. Sometimes histograms are good for this, 
#but in this case we will use some probability density plots to give nice smooth lines for each distribution.
# density plots for each attribute by class value
scaleml <- list(inputml = list(relation = "free"), 
                outputml = list(relation = "free"))
featurePlot(x = inputml, 
            y = outputml, plot = "density", scales = scaleml)
#Like the boxplots, we can see the difference in distribution of each attribute by class value. 

#Seeing few outliers in precipitationIn which can skew the whole model and thus will remove that outlier.
unique(mlweather$PrecipitationIn)

mlweather2 <- mlweather %>% 
  filter(!PrecipitationIn %in% c("1.72", "1.09", "T", "1.43", "1.12"))


#10-fold crossvalidation to estimate accuracy
#This will split our dataset into 10 parts, train in 9 and test on 1 and release for all combinations of train-test splits. 
#We will also repeat the process 3 times for each algorithm with different splits of the data into 10 groups, in an effort to get a more accurate estimate.

# Run algorithms using 10-fold cross validation
controlml <- trainControl(method="cv", number=10)
metricml <- "Accuracy"

#We are using the metric of “Accuracy” to evaluate models. 
#This is a ratio of the number of correctly predicted instances in divided by 
#the total number of instances in the dataset multiplied by 100 to give a percentage (e.g. 95% accurate). 
#We will be using the metric variable when we run build and evaluate each model next

#Building Models
#We don’t know which algorithms would be good on this problem or what
#configurations to use. We get an idea from the plots that some of the classes
#are partially non linearly separable in some dimensions, so non linear algoritms can give us good results.

#4 different algorithms:

#1.Classification and Regression Trees (CART).
#2.k-Nearest Neighbors (kNN).
#3.Support Vector Machines (SVM) with a linear kernel.
#4.Random Forest (RF)

#Using mixture of nonlinear (CART, kNN) and complex nonlinear methods (SVM, RF). Linear model is not applicable for the variables. 
#We reset the random number seed before reach run to ensure that the evaluation of each algorithm is performed using exactly the same data splits. It ensures the results are directly comparable


# a) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Events~., data=mlweather2, method="rpart", metric=metricml, trControl=controlml)
# kNN
set.seed(7)
fit.knn <- train(Events~., data=mlweather2, method="knn", metric=metricml, trControl=controlml)
#Tree Based Model
set.seed(7)
fit.rpart <- train(Events~., data=mlweather2, method="rpart", metric=metricml, trControl=controlml)

# b) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Events~., data=mlweather2, method="svmRadial", metric=metricml, trControl=controlml)
# Random Forest
set.seed(7)
fit.rf <- train(Events~., data=mlweather2, method="rf", metric=metricml, trControl=controlml)


#summarize accuracy of models
resultsml <- resamples(list(cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(resultsml)

#In summary we can see accuracy and kappa values
#the kappa statistic is a measure of how closely the instances classified 
#by the machine learning classifier matched the data labeled as ground truth

#Compare accuracy of models
dotplot(resultsml)

#We can see that the most accurate model in this case is cart

#Summarize Best Model
print(fit.cart)
#p value is less than 0.05 which means statistical significant results are obtained through our model
#82% Accuracy of model in weather forecasting. 
#0.89 of F1 Statistic
#Looking at confusion matrix data indiactes that model is good enough for implementation.

#Running predictive analytics
#Estimate skill of knn on the validation dataset
predictionsml1 <- predict(fit.cart, validationml)
confusionMatrix(predictionsml1, validationml$Events)

