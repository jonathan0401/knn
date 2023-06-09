% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Knn.R
\name{knn}
\alias{knn}
\title{K-nearest Neighbors Algorithm for Classification}
\usage{
knn(train_data, test_data, k = 3)
}
\arguments{
\item{train_data}{training set of your data}

\item{test_data}{test set of your data}

\item{k}{number of neighbors(default is 3)}
}
\value{
Predicted response for either classification data
}
\description{
This function will help you perform K-Nearest Neighbors Algorithm on
classification data. The default distance that was used is euclidean distance.
And the default parameter k is 3. It will returned the stored prediction of the response
only if you have your data make sure the response is in the last
column and before call out the knn function you need to separate your data into train and test set.
}
\examples{
# load iris dataset
data(iris)

# set seed for reproducibility
set.seed(123)
# split iris data into 70-30 train-test split
train_index <- sample(seq_len(nrow(iris)), size = round(0.7 * nrow(iris)), replace = FALSE)
train_data <- iris[train_index, ]
test_data <- iris[-train_index, ]
# run knn on iris train and test data
predicted_classes <- knn(train_data, test_data, k = 3)
# print predicted classes
print(predicted_classes)
# compute accuracy of KNN predictions
actual_classes <- test_data[, ncol(test_data)]
accuracy <- mean(predicted_classes == actual_classes)
cat("Accuracy:", accuracy, "\n")

}
