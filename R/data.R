#' Benchmarks for medium task
#'
#' A benchmark of L2-regularized logistic regression in the
#' \pkg{bigoptim} and \pkg{glmnet} packages based on subsets of the
#' [covtype] dataset.
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
#' A benchmark of SAGA L1-regularized logistic regression in the
#' \pkg{scikit-learn} python package and the python version of
#' \pkg{glmnet} based
#' on subsets of the [covtype], [ijcnn1], [a9a], [mushrooms],
#' [skin_noskin], and [phishing] datasets.
#'
#' @format A data frame with 800 rows and 5 variables:
#' \describe{
#'   \item{dataset}{dataset}
#'   \item{package}{package, glmnet or saga}
#'   \item{time}{runtime in seconds}
#'   \item{loss}{value of objective function}
#' }
"data_mediumhard"

#' ijcnn1
#'
#' @format A dataset of 49,990 observations and 22 features.
#' @source https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary.html
"ijcnn1"

#' a9a
#'
#' @format A dataset of 32,561 observations and 123 features.
#' @source https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary.html
"a9a"

#' phishing
#'
#' @format A dataset of 11,055 observations and 68 features.
#' @source https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary.html
"phishing"

#' mushrooms
#'
#' @format A dataset of 8,124 observations and 112 features.
#' @source https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary.html
"mushrooms"

#' covtype
#'
#' @format A dataset of 581,012 observations and 54 features.
#' @source https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary.html
"covtype"

#' skin_nonskin
#'
#' @format A dataset of 245,057 observations and 3 features.
#' @source https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary.html
"skin_nonskin"
