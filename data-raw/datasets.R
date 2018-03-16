# IJCNN1 ------------------------------------------------------------------

download.file(
  "http://www.csie.ntu.edu.tw/%7Ecjlin/libsvmtools/datasets/binary/ijcnn1.bz2",
  "ijcnn1.bz2"
)
ijcnn1 <- e1071::read.matrix.csr(bzfile("ijcnn1.bz2"), fac = FALSE)
devtools::use_data(ijcnn1, overwrite = TRUE, internal = TRUE)
unlink("ijcnn1.bz2")

# a9a ---------------------------------------------------------------------

download.file(
  "https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/a9a",
  "a9a"
)
a9a <- e1071::read.matrix.csr("a9a", fac = FALSE)
devtools::use_data(a9a, overwrite = TRUE, internal = TRUE)
unlink("a9a")

# phishing ----------------------------------------------------------------

download.file(
  "https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/phishing",
  "phishing"
)
phishing <- e1071::read.matrix.csr("phishing", fac = FALSE)
devtools::use_data(phishing, overwrite = TRUE, internal = TRUE)
unlink("phishing")

# w8a ---------------------------------------------------------------------

download.file(
  "https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/w8a",
  "w8a"
)
w8a <- e1071::read.matrix.csr("w8a", fac = FALSE)
devtools::use_data(w8a, overwrite = TRUE, internal = TRUE)
unlink("w8a")

# covtype -----------------------------------------------------------------

download.file(
  "https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/covtype.libsvm.binary.scale.bz2",
  "covtype.libsvm.binary.scale.bz2"
)
covtype <- e1071::read.matrix.csr(bzfile("covtype.libsvm.binary.scale.bz2"), fac = FALSE)
devtools::use_data(covtype, overwrite = TRUE, internal = TRUE)
unlink("covtype.libsvm.binary.scale.bz2")

#
# # susy --------------------------------------------------------------------
#
# download.file(
#   "https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/SUSY.bz2",
#   "SUSY.bz2"
# )
# susy <- e1071::read.matrix.csr(bzfile("SUSY.bz2"), fac = FALSE)
# susy <- list(X = SparseM::as.matrix(susy$x), y = susy$y)
# devtools::use_data(susy)
# unlink("SUSY.bz2")

# skin --------------------------------------------------------------------

download.file(
  "https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/skin_nonskin",
  "skin_nonskin"
)
skin <- e1071::read.matrix.csr("skin_nonskin", fac = FALSE)
devtools::use_data(skin, overwrite = TRUE, internal = TRUE)
unlink("skin_nonskin")
