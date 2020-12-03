library(ggplot2)
library(lattice)
library(caret)
library(class)

df<-read.csv("wine.csv",sep=",")
head(df)
str(df)
#buat data class menjadi berbentuk factor
df$class<-as.factor(df$class)
#cek missing value
library(visdat)
vis_miss(df)
#data aman tidak ada missing value

#- Membagi Data Menjadi Test (25%) dan Train (75%)
set.seed(123)
sampel <- sample(2,nrow(df),replace = T, prob = c(0.75,0.25))
#- Data Training
trainingdat <- df[sampel==1, ]
head(trainingdat)
#- Data Testing
testingdat<- df[sampel==2, ]
head(testingdat)
print(paste("Jumlah Train Data: ", nrow(trainingdat), "| Jumlah Test Data: ", nrow(testingdat)))
#- Membuat Model, dengan mencoba k=20 `cl` merupakan faktor dari klasifikasi yang benar dari training set
prediksi <- knn(train = trainingdat, test = testingdat, cl=trainingdat$class ,k=20)
#- Model Evaluation, Confusion Matrix dengan k-=20
confusionMatrix(table(prediksi, testingdat$class))

for(i in 1:40){
  prediksi_knn <- knn(train=trainingdat, test = testingdat, cl=trainingdat[,1], k=i)
  akurasi <- 100*sum(testingdat$class==prediksi_knn)/nrow(testingdat)
  cat("K = ", i," akurasinya ",akurasi, '%', '\n')
}
prediksi <- knn(train = trainingdat, test = testingdat, cl=trainingdat$class ,k=4)

#- Model Evaluation, Confusion Matrix
confusionMatrix(table(prediksi, testingdat$class))

