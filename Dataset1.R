
#Dataset on Amazon's Top 50 bestselling books from 2009 to 2019. Contains 550 books, 
#Data has been categorized into fiction and non-fiction using Goodreads

# Importing the dataset 
bestseller_books <- read.csv("bestsellers with categories.csv", header = TRUE, sep = ",") 
bestseller_books <- bestseller_books[3:5] # Taking columns 3-5 for implementation
bestseller_books

str(bestseller_books) 
bestseller_books<-as.data.frame(bestseller_books)

# Normally we encoding the target feature as factor but here it has already been factor

bestseller_books$Genre
bestseller_books$Genre[bestseller_books$Genre == "Non Fiction"] <- 0
bestseller_books$Genre[bestseller_books$Genre == "Fiction"] <- 1

bestseller_books$Genre<-as.numeric(bestseller_books$Genre)
bestseller_books$Genre
str(bestseller_books)

# Splitting the dataset into the Training set and Test set 
install.packages('caTools') 
library(caTools) 

set.seed(150) 
split = sample.split(bestseller_books$Genre, SplitRatio = 0.75) 

training_set = subset(bestseller_books, split == TRUE) 
test_set = subset(bestseller_books, split == FALSE)

split
training_set
test_set

# Fitting SVM to the Training set 
install.packages('e1071') 
library(e1071) 

#Linear Support Vector Machines Classifier

classifier <- svm(formula =Genre ~ ., 
                 data = training_set, 
                 type = 'C-classification', 
                 kernel = 'linear') 

classifier

# Predicting the Test set results 
book_prediction = predict(classifier, newdata = test_set) 
book_prediction

# Making the Confusion Matrix 
confusion_matrix = table(test_set[, 3],book_prediction) 
confusion_matrix

#We determine performance of the model by calculating accuracy.
accurancy_rate<-sum(diag(confusion_matrix))/sum(confusion_matrix)
accurancy_rate

#We calculate missclassification by using accurancy
1-accurancy_rate



#Visualization of data with SVM classification
library(ggplot2)

plot(training_set$User.Rating,training_set$Reviews)

ggplot(data = training_set)+
  geom_point(aes(x =Reviews, y =User.Rating,color=Genre))

classifier <- svm(formula =Genre ~ ., 
                  data = training_set, 
                  type = 'C-classification', 
                  kernel = 'linear') 

classifier
plot(classifier,training_set)
plot(classifier,test_set)

classifier_2 <- svm(formula =Genre ~ ., 
                  data = training_set, 
                  type = 'C-classification', 
                  kernel = 'linear',
                  scale = FALSE)
                                 
classifier_2
plot(classifier_2,training_set)
plot(classifier_2,test_set)

classifier_3 <- svm(formula =Genre ~ ., 
                    data = training_set, 
                    type = 'C-classification', 
                    kernel = 'linear',
                    cost=10)

classifier_3
plot(classifier_3,training_set)
plot(classifier_3,test_set)

classifier_4 <- svm(formula =Genre ~ ., 
                  data = training_set, 
                  type = 'C-classification', 
                  kernel = 'linear',
                  cost=10,
                  scale=FALSE) 

classifier_4

plot(classifier_4,training_set)
plot(classifier_4,test_set)


#Non-Linear(Radial) Support Vector Machine Classifier

classifier_5 <- svm(formula =Genre ~ ., 
                  data = training_set, 
                  type = 'C-classification', 
                  kernel = "radial") 

classifier_5

# Predicting the Test set results 
book_prediction_5 = predict(classifier_5, newdata = test_set) 
book_prediction_5

# Making the Confusion Matrix 
confusion_matrix_5 = table(test_set[, 3],book_prediction_5) 
confusion_matrix_5

#We determine performance of the model by calculating accuracy.
accurancy_rate_5<-sum(diag(confusion_matrix_5))/sum(confusion_matrix_5)
accurancy_rate_5

#We calculate missclassification by using accurancy
1-accurancy_rate_5

plot(classifier_5,training_set)
plot(classifier_5,test_set)


#Non-Linear(Polynomial) Support Vector Machine Classifier


classifier_6 <- svm(formula =Genre ~ ., 
                    data = training_set, 
                    type = 'C-classification', 
                    kernel = "polynomial") 
classifier_6

# Predicting the Test set results 
book_prediction_6 = predict(classifier_6, newdata = test_set) 
book_prediction_6

# Making the Confusion Matrix 
confusion_matrix_6 = table(test_set[, 3],book_prediction_6) 
confusion_matrix_6

#We determine performance of the model by calculating accuracy.
accurancy_rate_6<-sum(diag(confusion_matrix_6))/sum(confusion_matrix_6)
accurancy_rate_6

#We calculate missclassification by using accurancy
1-accurancy_rate_6


plot(classifier_6,training_set)
plot(classifier_6,test_set)