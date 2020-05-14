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

# par(mfrow=c(5,5))
# par(mar=c(0.1,0.1,0.1,0.1))
# for (i in 1:25) {
#   ok = matrix(train$x[i,], 28, 28)
#   image(ok[28:1,])
# }

compress14 = function (image) {
  compress = vector(length = 14*14)
  for (i in 1:14) {
    for (j in 1:14) {
      compress[(i-1)*14+j] = ( image[28*(2*i-2) + (2*j-1)] + image[28*(2*i-2) + (2*j)] 
                                + image[28*(2*i-1) + (2*j-1)] + image[28*(2*i-1) + (2*j)] )
    }
  }
  compress
}

train_sub$x = t(apply(train_sub$x, 1, compress14))

par(mfrow=c(5,5))
par(mar=c(0.1,0.1,0.1,0.1))
for (i in 1:25) {
  ok = matrix(train_sub$x[i,], 14, 14)
  image(ok[14:1,])
}
