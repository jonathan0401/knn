#' Euclidean Distance
#'
#' This function will help you perform Euclidean Distance.
#'
#' @param x1 numeric variable
#' @param x2 numeric variable
#' @return Euclidean distance value
#' @export
# function to compute Euclidean distance between two points
euclidean_distance <- function(x1, x2) {
  sqrt(sum((x1 - x2) ^ 2))
}

