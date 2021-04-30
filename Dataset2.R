#The World Happiness Report is a landmark survey of the state of global happiness. 
#The first report was published in 2012, the second in 2013, the third in 2015, and the fourth in the 2016 Update.
#The World Happiness 2017, which ranks 155 countries by their happiness levels, was released at the United Nations at an event celebrating International Day of Happiness on March 20th.


# Importing the dataset 
happiness <- read.csv("happiness.csv", header = TRUE, sep = ",") 
happiness <- happiness[3:5] # Taking columns 2-5 for implementation
happiness

str(happiness)


# Normally we encoding the target feature as factor but here it has already been factor

happiness$Region
happiness$Region[happiness$Region == "Australia and New Zealand"] <- 1
happiness$Region[happiness$Region == "Europe"] <- 2
happiness$Region[happiness$Region == "Asia"] <- 3
happiness$Region[happiness$Region == "America"] <- 4
happiness$Region[happiness$Region == "Africa"] <- 5




happiness$Region<-as.numeric(happiness$Region)
happiness$Region
str(happiness)

# Splitting the dataset into the Training set and Test set 
install.packages('caTools') 
library(caTools) 

set.seed(100) 
split = sample.split(happiness$Region, SplitRatio = 0.70) 

training_set_2 = subset(happiness, split == TRUE) 
test_set_2 = subset(happiness, split == FALSE)

split
training_set_2
test_set_2

# Fitting SVM to the Training set 
install.packages('e1071') 
library(e1071) 

#Linear Support Vector Machines Classifier

classifier_happiness <- svm(formula =Region ~ ., 
                  data = training_set_2, 
                  type = 'C-classification', 
                  kernel = 'linear') 

classifier_happiness

# Predicting the Test set results 
region_prediction = predict(classifier_happiness, newdata = test_set_2) 
region_prediction

# Making the Confusion Matrix 
confusion_matrix_2 = table(test_set_2[, 3],region_prediction) 
confusion_matrix_2

#We determine performance of the model by calculating accuracy.
accurancy_rate_2<-sum(diag(confusion_matrix_2))/sum(confusion_matrix_2)
accurancy_rate_2

#We calculate missclassification by using accurancy
1-accurancy_rate_2



#Visualization of data with SVM classification
library(ggplot2)

plot(training_set_2$Happiness.Score,training_set_2$Freedom)
plot(test_set_2$Happiness.Score,test_set_2$Freedom)


ggplot(data = training_set_2)+
  geom_point(aes(x =Happiness.Score, y =Freedom,color=Region))

classifier_happiness <- svm(formula =Region ~ ., 
                  data = training_set_2, 
                  type = 'C-classification', 
                  kernel = 'linear') 

classifier_happiness

plot(classifier_happiness,training_set_2)
plot(classifier_happiness,test_set_2)

classifier_happiness_2 <- svm(formula =Region ~ ., 
                    data = training_set_2, 
                    type = 'C-classification', 
                    kernel = 'linear',
                    scale = FALSE)

classifier_happiness_2

plot(classifier_happiness_2,training_set_2)
plot(classifier_happiness_2,test_set_2)

classifier_happiness_3 <- svm(formula =Region ~ ., 
                    data = training_set_2, 
                    type = 'C-classification', 
                    kernel = 'linear',
                    cost=10)

classifier_happiness_3

plot(classifier_happiness_3,training_set_2)
plot(classifier_happiness_3,test_set_2)

classifier_happiness_4 <- svm(formula =Region ~ ., 
                    data = training_set_2, 
                    type = 'C-classification', 
                    kernel = 'linear',
                    cost=10,
                    scale=FALSE) 

classifier_happiness_4

plot(classifier_happiness_4,training_set_2)
plot(classifier_happiness_4,test_set_2)


#Non-Linear(Radial) Support Vector Machine Classifier

classifier_happiness_5 <- svm(formula =Region ~ ., 
                    data = training_set_2, 
                    type = 'C-classification', 
                    kernel = "radial") 

classifier_happiness_5

# Predicting the Test set results 
region_prediction_2 = predict(classifier_happiness_5, newdata = test_set_2) 
region_prediction_2

# Making the Confusion Matrix 
confusion_matrix_3 = table(test_set_2[, 3],region_prediction_2) 
confusion_matrix_3

#We determine performance of the model by calculating accuracy.
accurancy_rate_3<-sum(diag(confusion_matrix_3))/sum(confusion_matrix_3)
accurancy_rate_3

#We calculate missclassification by using accurancy
1-accurancy_rate_3

plot(classifier_happiness_5,training_set_2)
plot(classifier_happiness_5,test_set_2)


#Non-Linear(Polynomial) Support Vector Machine Classifier


classifier_happiness_6 <- svm(formula =Region ~ ., 
                    data = training_set_2, 
                    type = 'C-classification', 
                    kernel = "polynomial") 

classifier_happiness_6

# Predicting the Test set results 
region_prediction_3 = predict(classifier_happiness_6, newdata = test_set_2) 
region_prediction_3

# Making the Confusion Matrix 
confusion_matrix_4 = table(test_set_2[, 3],region_prediction_3) 
confusion_matrix_4

#We determine performance of the model by calculating accuracy.
accurancy_rate_4<-sum(diag(confusion_matrix_4))/sum(confusion_matrix_4)
accurancy_rate_4

#We calculate missclassification by using accurancy
1-accurancy_rate_4


plot(classifier_happiness_6,training_set_2)
plot(classifier_happiness_6,test_set_2)


#Non-Linear(Sigmoid) Support Vector Machine Classifier


classifier_happiness_7 <- svm(formula =Region ~ ., 
                              data = training_set_2, 
                              type = 'C-classification', 
                              kernel = "sigmoid") 

classifier_happiness_7

# Predicting the Test set results 
region_prediction_4 = predict(classifier_happiness_7, newdata = test_set_2) 
region_prediction_4

# Making the Confusion Matrix 
confusion_matrix_5 = table(test_set_2[, 3],region_prediction_4) 
confusion_matrix_5

#We determine performance of the model by calculating accuracy.
accurancy_rate_5<-sum(diag(confusion_matrix_5))/sum(confusion_matrix_5)
accurancy_rate_5

#We calculate missclassification by using accurancy
1-accurancy_rate_5


plot(classifier_happiness_7,training_set_2)
plot(classifier_happiness_7,test_set_2)



