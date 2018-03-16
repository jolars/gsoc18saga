#' Benchmarks for medium task
#'
#' A benchmark of L2-regularized logistic regression in the
#' \pkg{bigoptim} and \pkg{glmnet} packages based on subsets of the
#' [bigoptim::covtype.libsvm] dataset.
#' @format A data frame with 800 rows and 5 variables:
#' \describe{
#'   \item{rows}{number of rows in data}
#'   \item{cols}{number of columns in data}
#'   \item{time}{time in nanoseconds}
#'   \item{loss}{value of objective function}
#'   \item{package}{package}
#' }
"data_medium"

#' Benchmarks for medium-hard task
#'
#' A benchmark of L1-regularized logistic regression in the
#' \pkg{scikit-learn} python package and \pkg{glmnet} R package based
#' on subsets of the [bigoptim::covtype.libsvm] dataset.
#'
#' @format A data frame with 800 rows and 5 variables:
#' \describe{
#'   \item{rows}{number of rows in data}
#'   \item{cols}{number of columns in data}
#'   \item{time}{time in nanoseconds}
#'   \item{loss}{value of objective function}
#'   \item{package}{package}
#' }
"data_mediumhard"

#' IJCNN1
#'
#' @format A data frame with 800 rows and 5 variables:
#' \describe{
#'   \item{rows}{number of rows in data}
#'   \item{cols}{number of columns in data}
#'   \item{time}{time in nanoseconds}
#'   \item{loss}{value of objective function}
#'   \item{package}{package}
#' }
"ijcnn1"


#' a9a
#'
#' @format A data frame with 800 rows and 5 variables:
#' \describe{
#'   \item{rows}{number of rows in data}
#'   \item{cols}{number of columns in data}
#'   \item{time}{time in nanoseconds}
#'   \item{loss}{value of objective function}
#'   \item{package}{package}
#' }
"a9a"

#' phishing
#'
#' @format A data frame with 800 rows and 5 variables:
#' \describe{
#'   \item{rows}{number of rows in data}
#'   \item{cols}{number of columns in data}
#'   \item{time}{time in nanoseconds}
#'   \item{loss}{value of objective function}
#'   \item{package}{package}
#' }
"phishing"
