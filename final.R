
install.packages("tuneR")
install.packages("randomForest")
library(randomForest)
library(rpart)
library(tuneR)
# Metal,Jazz,Classical,Pop,Electronica,Blues

#Blues

setwd("/home/abhijit331/Desktop/ECE271B/genres_wav/blues_wav")
files = list.files(pattern = "*.wav")
temp = readWave(files[1])
blues = melfcc(temp)
for(i in 2:50)
{
  temp = readWave(files[i])
  blues = rbind(blues,melfcc(temp))
}
blues = cbind(1,blues) 

#Metal 
setwd("/home/abhijit331/Desktop/ECE271B/genres_wav/metal_wav")
files = list.files(pattern = "*.wav")
temp = readWave(files[1])
metal = melfcc(temp)
for(i in 2:50)
{
  temp = readWave(files[i])
  metal = rbind(metal,melfcc(temp))
}
metal = cbind(0,metal)
train = rbind(blues,metal)
colnames(train) = c("label","coeff1","coeff2","coeff3","coeff4","coeff5","coeff6","coeff7","coeff8","coeff9","coeff10","coeff11","coeff12")
train = as.data.frame(train)

test =  melfcc(readWave("blues.00051.wav"))
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
