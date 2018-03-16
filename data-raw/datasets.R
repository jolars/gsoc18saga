read.libsvm <- function(filename, sparse = TRUE, dims = NULL) {
  content <- readLines(filename)
  num_lines <- length(content)
  space_ind <- regexpr('\\s+', content)
  tomakemat <- cbind(1:num_lines, -1, substr(content, 1, space_ind - 1))

  # loop over lines
  makemat <- rbind(tomakemat,
                   do.call(rbind,
                           lapply(1:num_lines, function(i) {
                             # split by spaces, remove lines
                             line <-
                               as.vector(strsplit(content[i], ' ')[[1]])
                             cbind(i, t(simplify2array(strsplit(line[-1],
                                                                ':'))))
                           })))
  class(makemat) <- "numeric"

  if (!is.null(dims)) {
    yx <- sparseMatrix(
      i = makemat[, 1],
      j = makemat[, 2] + 2,
      x = makemat[, 3],
      dims = dims
    )
  } else {
    yx <- sparseMatrix(i = makemat[, 1],
                       j = makemat[, 2] + 2,
                       x = makemat[, 3])
  }

  if (!sparse)
    yx <- as(yx, 'matrix')
  yx
}

rowl2norm <- function(x) {
  rs <- 1/sqrt(rowSums(x^2))
  Diagonal(x = rs) %*% x
}

rownorm <- function(x) {
  rs <- 1/rowSums(x)
  Diagonal(x = rs) %*% x
}


# IJCNN1 ------------------------------------------------------------------

download.file(
  'http://www.csie.ntu.edu.tw/%7Ecjlin/libsvmtools/datasets/binary/ijcnn1.bz2',
  'ijcnn1.bz2'
)
ijcnn1 <- read.libsvm(bzfile('ijcnn1.bz2'))
X <- ijcnn1[, -1]
y <- ijcnn1[, 1]
X <- rowl2norm(X)
ijcnn1 <- list(X = X, y = y)
devtools::use_data(ijcnn1)


# a9a ---------------------------------------------------------------------

download.file(
  'https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/a9a',
  'a9a'
)
a9a <- e1071::read.matrix.csr("a9a", fac = FALSE)
X <- as.matrix(a9a$x)
y <- a9a$y
a9a <- list(X = X, y = y)
devtools::use_data(a9a)


# phishing ----------------------------------------------------------------

download.file(
  'https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/phishing',
  'phishing'
)
phishing <- e1071::read.matrix.csr("phishing", fac = FALSE)
X <- as.matrix(phishing$x)
y <- phishing$y
phishing <- list(X = X, y = y)
devtools::use_data(phishing)


# w8a ---------------------------------------------------------------------

download.file(
  'https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/w8a',
  'w8a'
)
w8a <- e1071::read.matrix.scr("w8a", fac = FALSE)
X <- as.matrix(w8a$X)
y <- w8a$y
w8a <- list(X = X, y = y)
devtools::use_data(w8a)
