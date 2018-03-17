# IJCNN1 ------------------------------------------------------------------

download.file(
  "http://www.csie.ntu.edu.tw/%7Ecjlin/libsvmtools/datasets/binary/ijcnn1.bz2",
  "ijcnn1.bz2"
)
ijcnn1 <- e1071::read.matrix.csr(bzfile("ijcnn1.bz2"), fac = FALSE)
devtools::use_data(ijcnn1, overwrite = TRUE)
unlink("ijcnn1.bz2")

# a9a ---------------------------------------------------------------------

download.file(
  "https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/a9a",
  "a9a"
)
a9a <- e1071::read.matrix.csr("a9a", fac = FALSE)
devtools::use_data(a9a, overwrite = TRUE)
unlink("a9a")

# phishing ----------------------------------------------------------------

download.file(
  "https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/phishing",
  "phishing"
)
phishing <- e1071::read.matrix.csr("phishing", fac = FALSE)
devtools::use_data(phishing, overwrite = TRUE)
unlink("phishing")


# mushrooms ---------------------------------------------------------------

download.file(
  "https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/mushrooms",
  "mushrooms"
)
mushrooms <- e1071::read.matrix.csr("mushrooms", fac = FALSE)
devtools::use_data(mushrooms, overwrite = TRUE)
unlink("mushrooms")

# covtype -----------------------------------------------------------------

download.file(
  "https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/covtype.libsvm.binary.scale.bz2",
  "covtype.libsvm.binary.scale.bz2"
)
covtype <- e1071::read.matrix.csr(bzfile("covtype.libsvm.binary.scale.bz2"), fac = FALSE)
devtools::use_data(covtype, overwrite = TRUE)
unlink("covtype.libsvm.binary.scale.bz2")

# skin_nonskin -------------------------------------------------------------

download.file(
  "https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/skin_nonskin",
  "skin_nonskin"
)
skin_nonskin <- e1071::read.matrix.csr("skin_nonskin", fac = FALSE)
devtools::use_data(skin_nonskin, overwrite = TRUE)
unlink("skin_nonskin")
