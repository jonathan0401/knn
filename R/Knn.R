#' K-nearest Neighbors Algorithm for Classification
#'
#' This function will help you perform K-Nearest Neighbors Algorithm on
#' classification data. The default distance that was used is euclidean distance.
#' And the default parameter k is 3. It will returned the stored prediction of the response
#' only if you have your data make sure the response is in the last
#' column and before call out the knn function you need to separate your data into train and test set.
#'
#' @param train_data training set of your data
#' @param test_data test set of your data
#' @param k number of neighbors(default is 3)
#' @return Predicted response for either classification data
#' @examples
#' # load iris dataset
#' data(iris)
#'
#' # set seed for reproducibility
#' set.seed(123)

#' # split iris data into 70-30 train-test split
#' train_index <- sample(seq_len(nrow(iris)), size = round(0.7 * nrow(iris)), replace = FALSE)
#' train_data <- iris[train_index, ]
#' test_data <- iris[-train_index, ]

#' # run knn on iris train and test data
#' predicted_classes <- knn(train_data, test_data, k = 3)

#' # print predicted classes
#' print(predicted_classes)


#' # compute accuracy of KNN predictions
#' actual_classes <- test_data[, ncol(test_data)]
#' accuracy <- mean(predicted_classes == actual_classes)
#' cat("Accuracy:", accuracy, "\n")
#'
#' @export

# KNN function
knn <- function(train_data, test_data, k = 3) {

  # number of rows in training data
  train_rows <- nrow(train_data)

  # number of rows in test data
  test_rows <- nrow(test_data)

  # vector to store predicted classes
  predicted_classes <- vector("character", test_rows)

  # iterate over each row in test data
  for (i in 1:test_rows) {

    # compute distances between test point and all training points
    distances <- numeric(train_rows)
    for (j in 1:train_rows) {
      distances[j] <- euclidean_distance(test_data[i, -ncol(test_data)], train_data[j, -ncol(train_data)])
    }

    # find indices of k nearest neighbors
    nearest_indices <- order(distances)[1:k]

    # get the response variable values of the k nearest neighbors
    nearest_responses <- train_data[nearest_indices, ncol(train_data)]

    # predict class based on majority vote
    predicted_classes[i] <- names(sort(table(nearest_responses), decreasing = TRUE))[1]
  }

  # return vector of predicted classes
  return(predicted_classes)
}



# KNN function for regression
knn_reg <- function(train_data, test_data, k = 3) {

  # number of rows in training data
  train_rows <- nrow(train_data)

  # number of rows in test data
  test_rows <- nrow(test_data)

  # vector to store predicted values
  predicted_values <- vector("numeric", test_rows)

  # iterate over each row in test data
  for (i in 1:test_rows) {

    # compute distances between test point and all training points
    distances <- numeric(train_rows)
    for (j in 1:train_rows) {
      distances[j] <- euclidean_distance(test_data[i, -ncol(test_data)], train_data[j, -ncol(train_data)])
    }

    # find indices of k nearest neighbors
    nearest_indices <- order(distances)[1:k]

    # get the response variable values of the k nearest neighbors
    nearest_responses <- train_data[nearest_indices, ncol(train_data)]

    # predict value based on mean of k nearest neighbors
    predicted_values[i] <- mean(nearest_responses)
  }

  # return vector of predicted values
  return(predicted_values)
}




