load_image_file = function(filename) {
  ret = list()
  f = file(filename,'rb')
  readBin(f, integer() ,n=1, endian='big') #  Magic number
  ret$n = readBin(f,integer(),n=1,endian='big')
  nrow = readBin(f,integer(),n=1,endian='big')
  ncol = readBin(f,integer(),n=1,endian='big')
  x = readBin(f,integer(),n=ret$n*nrow*ncol,size=1,signed=F)
  ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
  close(f)
  ret
}

load_label_file <- function(filename) {
  f = file(filename,'rb')
  readBin(f, integer() ,n=1 ,endian='big') # Magic number
  n = readBin(f, integer(),n=1,size=4,endian='big')
  y = readBin(f, integer(),n=n,size=1,signed=F)
  close(f)
  y
}

## load MNIST image data
train = load_image_file("/Users/xuchenghuiyun/Desktop/STA243/data/train-images-idx3-ubyte")
train$x = t(apply(train$x,1,rev))

## load MNIST label data
train$y = load_label_file("/Users/xuchenghuiyun/Desktop/STA243/data/train-labels-idx1-ubyte")

train_sub = list()
train_sub$x = train$x[train$y %in% c(0,1,2,3,4),]
train_sub$y = train$y[train$y %in% c(0,1,2,3,4)]
train_sub$n = length(train_sub$y)

par(mfrow=c(5,5))
par(mar=c(0.1,0.1,0.1,0.1))
for (i in 1:25) {
  ok = matrix(train_sub$x[i,], 28, 28)
  image(ok[28:1,])
}


compress14 = function (image) {
  compress = vector(length = 14*14)
  for (i in 0:13) {
    for (j in 0:13) {
      x = 28 * 2*i + 2*j + 1
      y = 28 * (2*i + 1) + 2*j + 1
      compress[i*14+j+1] = ( image[x] + image[x+1] + image[y] + image[y+1] )/4
    }
  }
  compress
}

train_compress = t(apply(train_sub$x, 1, compress14))

par(mfrow=c(5,5))
par(mar=c(0.1,0.1,0.1,0.1))
for (i in 1:25) {
  ok = matrix(train_compress[i,], 14, 14)
  image(ok[14:1,])
}
