f = file("/Users/xuchenghuiyun/Downloads/train-images-idx3-ubyte", "rb")
m = (matrix(readBin(f,integer(), size=1, n=28*28, endian="big"),28,28))

ok = rbind(m[17:28,],m[1:16,])
image(ok)

par(mfrow=c(5,5))
par(mar=c(0.1,0.1,0.1,0.1))
for(i in 1:25){
  m = matrix(readBin(f,integer(), size=1, n=28*28, endian="big"),28,28)
  ok = rbind(m[17:28,],m[1:16,])
  image(ok[,28:1])
  }
