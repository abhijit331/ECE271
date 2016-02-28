"~"
install.packages("tuneR")
install.packages("randomForest")
library(randomForest)
library(rpart)
library(tuneR)
# Metal,Jazz,Classical,Pop,Electronica,Blues
blue = readWave("blues.00000.wav")
x = melfcc(blue)
par(new = F)
for( i in 1:3006)
{
  plot(x[i,],type="l")
  par(new = T)
}

metal = readWave("C:/Users/abhijit331/Desktop/271B/genres_wav/metal_wav/metal.00000.wav")
y = melfcc(metal)
par(new = F)
for(i in 1:3005)
{
  plot(y[i,],type = "l")
  par(new = T)
}
x = cbind(1,x)
y = cbind(0,y)
train = rbind(x,y)
colnames(train) = c("label","coeff1","coeff2","coeff3","coeff4","coeff5","coeff6","coeff7","coeff8","coeff9","coeff10","coeff11","coeff12")
train = as.data.frame(train)
mod = rpart(label ~ . , data = train)

test =  melfcc(readWave("blues.00001.wav"))
test = cbind(-1,test)
colnames(test) = c("coeff1","coeff2","coeff3","coeff4","coeff5","coeff6","coeff7","coeff8","coeff9","coeff10","coeff11","coeff12")
rf = randomForest(as.factor(label) ~ . , data = train)
pre = predict(rf,test)
test2 =  melfcc(readWave("blues.00004.wav"))
colnames(test2) = c("coeff1","coeff2","coeff3","coeff4","coeff5","coeff6","coeff7","coeff8","coeff9","coeff10","coeff11","coeff12")
pre2 = predict(rf,test2)
pre2

test3 = melfcc(readWave("metal.00007.wav"))
colnames(test3) = c("coeff1","coeff2","coeff3","coeff4","coeff5","coeff6","coeff7","coeff8","coeff9","coeff10","coeff11","coeff12")
pred3 = predict(rf,test3)
length(which(pred3 == 0))
