
install.packages("tuneR")
install.packages("randomForest")
library(randomForest)
library(rpart)
library(tuneR)
library(plyr)
# Metal,Jazz,Classical,Pop,Electronica,Blues

#Blues

#setwd("/home/abhijit331/Desktop/ECE271B/genres_wav/blues_wav")
setwd("C:/Users/abhijit331/Desktop/Math 271b/genres_wav/blues_wav")
files.blues = list.files(pattern = "*.wav")
temp = readWave(files.blues[1])
blues = melfcc(temp)
for(i in 2:10)
{
  temp = readWave(files.blues[i])
  blues = rbind(blues,melfcc(temp))
}
blues = cbind(0,blues) 

#Metal 
#setwd("/home/abhijit331/Desktop/ECE271B/genres_wav/metal_wav")
setwd("C:/Users/abhijit331/Desktop/Math 271b/genres_wav/metal_wav")
files.metal = list.files(pattern = "*.wav")
temp = readWave(files.metal[1])
metal = melfcc(temp)
for(i in 2:10)
{
  temp = readWave(files.metal[i])
  metal = rbind(metal,melfcc(temp))
}
metal = cbind(1,metal)

# Jazz
setwd("C:/Users/abhijit331/Desktop/Math 271b/genres_wav/jazz_wav")
files.jazz  = list.files(pattern = "*.wav")
temp = readWave(files.jazz[1])
jazz = melfcc(temp)
for(i in 2:10)
{
  temp = readWave(files.jazz[i])
  jazz = rbind(jazz,melfcc(temp))
}
jazz = cbind(2,jazz)

# train
train = rbind(blues,metal,jazz)
colnames(train) = c("label","coeff1","coeff2","coeff3","coeff4","coeff5","coeff6","coeff7","coeff8","coeff9","coeff10","coeff11","coeff12")
train = as.data.frame(train)
rf = randomForest(as.factor(label) ~ . , data = train)

# interesting = blues 61
result.blues = c()
result.metal = c()
result.jazz  = c()
for(i in 31:100)
{
setwd("C:/Users/abhijit331/Desktop/Math 271b/genres_wav/blues_wav")
files.blues = list.files(pattern = "*.wav")
test.blues = melfcc(readWave(files.blues[i]))
setwd("C:/Users/abhijit331/Desktop/Math 271b/genres_wav/metal_wav")
files.metal = list.files(pattern = "*.wav")  
test.metal = melfcc(readWave(files.metal[i]))
setwd("C:/Users/abhijit331/Desktop/Math 271b/genres_wav/jazz_wav")
files.jazz = list.files(pattern = "*.wav")
test.jazz = melfcc(readWave(files.jazz[i]))
colnames(test.blues) = c("coeff1","coeff2","coeff3","coeff4","coeff5","coeff6","coeff7","coeff8","coeff9","coeff10","coeff11","coeff12")
colnames(test.metal) = c("coeff1","coeff2","coeff3","coeff4","coeff5","coeff6","coeff7","coeff8","coeff9","coeff10","coeff11","coeff12")
colnames(test.jazz) = c("coeff1","coeff2","coeff3","coeff4","coeff5","coeff6","coeff7","coeff8","coeff9","coeff10","coeff11","coeff12")
pred.blues = predict(rf,test.blues)
pred.metal = predict(rf,test.metal)
pred.jazz = predict(rf,test.jazz)
temp =rev(order(count(pred.blues)$freq))
result.blues[i-30] = as.integer(as.matrix(count(pred.blues))[temp[1],1])
temp = rev(order(count(pred.metal)$freq))
result.metal[i-30] =as.integer(as.matrix(count(pred.metal))[temp[1],1])
temp = rev(order(count(pred.jazz)$freq))
result.jazz[i-30] =as.integer(as.matrix(count(pred.jazz))[temp[1],1])
}
table(result.blues)
table(result.metal)
table(result.jazz)