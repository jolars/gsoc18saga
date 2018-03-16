read.libsvm = function( filename, sparse = TRUE, dims = NULL) {
  content = readLines( filename )
  num_lines = length( content )
  space_ind = regexpr('\\s+',content)
  tomakemat = cbind(1:num_lines, -1, substr(content,1,space_ind-1))

  # loop over lines
  makemat = rbind(tomakemat,
                  do.call(rbind,
                          lapply(1:num_lines, function(i){
                            # split by spaces, remove lines
                            line = as.vector( strsplit( content[i], ' ' )[[1]])
                            cbind(i, t(simplify2array(strsplit(line[-1],
                                                               ':'))))
                          })))
  class(makemat) = "numeric"

  if (!is.null(dims)) {
    yx = sparseMatrix(i = makemat[,1],
                      j = makemat[,2]+2,
                      x = makemat[,3],
                      dims = dims)
  } else {
    yx = sparseMatrix(i = makemat[,1],
                      j = makemat[,2]+2,
                      x = makemat[,3])
  }

  if (!sparse)
    yx = as(yx,'matrix')
  return( yx )
}

rowl2norm = function(x) {
  rs = 1/sqrt(rowSums(x^2))
  new.x = Diagonal(x=rs) %*% x
  return(new.x)
}

rownorm = function(x) {
  rs = 1/rowSums(x)
  new.x = Diagonal(x=rs) %*% x
  return(new.x)
}


# IJCNN1 ------------------------------------------------------------------

download.file('http://www.csie.ntu.edu.tw/%7Ecjlin/libsvmtools/datasets/binary/ijcnn1.bz2',
              'ijcnn1.bz2')
download.file('http://www.csie.ntu.edu.tw/%7Ecjlin/libsvmtools/datasets/binary/ijcnn1.t.bz2',
              'ijcnn1.t.bz2')
ijcnn1 = read.libsvm(bzfile('ijcnn1.bz2'))
ijcnn1.t = read.libsvm(bzfile('ijcnn1.t.bz2'))
ijcnn1[,-1] = rowl2norm(ijcnn1[,-1])
ijcnn1.t[,-1] = rowl2norm(ijcnn1.t[,-1])
ijcnn1 = list(ijcnn1,ijcnn1.t)
save(ijcnn1,file='ijcnn1.RData',compress = 'xz')


# MNIST --------------------------------------------------------------------

download.file('http://www.csie.ntu.edu.tw/%7Ecjlin/libsvmtools/datasets/multiclass/mnist.bz2',
              'mnist.bz2')
download.file('http://www.csie.ntu.edu.tw/%7Ecjlin/libsvmtools/datasets/multiclass/mnist.t.bz2',
              'mnist.t.bz2')
mnist = read.libsvm(bzfile('mnist.bz2'), dims=c(60000,782))
mnist.t = read.libsvm(bzfile('mnist.t.bz2'),dim=c(10000,782))
mnist[,-1] = rowl2norm(mnist[,-1])
mnist.t[,-1] = rowl2norm(mnist.t[,-1])

mnist38 = mnist[which(mnist[,1]==3 | mnist[,1]==8),]
mnist38.t = mnist.t[which(mnist.t[,1]==3 | mnist.t[,1]==8),]
mnist38[,1] = as.numeric(mnist38[,1]==3)
mnist38.t[,1] = as.numeric(mnist38.t[,1]==3)

mnist49 = mnist[which(mnist[,1]==4 | mnist[,1]==9),]
mnist49.t = mnist.t[which(mnist.t[,1]==4 | mnist.t[,1]==9),]
mnist49[,1] = as.numeric(mnist49[,1]==4)
mnist49.t[,1] = as.numeric(mnist49.t[,1]==4)

mnistoe = mnist
mnistoe[,1] = as.numeric(as.vector(mnist[,1])%%2==0)
mnistoe.t = mnist.t
mnistoe.t[,1] = as.numeric(as.vector(mnist.t[,1])%%2==0)

mnist = list(mnist38,mnist38.t,mnist49,mnist49.t,mnistoe,mnistoe.t)
save(mnist, file='mnist.RData',compress = 'xz')

# COVERTYPE ---------------------------------------------------------------

data("covtype.libsvm", package = "bigoptim")

# RCV1 --------------------------------------------------------------------

data("rcv1_train", package = "bigoptim")
