iris_data <- read.table(file='D:\\HP\\Documents\\Studia\\S6\\Sztuczna_Inteligencja\\Sprawozdania\\do_3\\breast-cancer-wisconsin.data',header = FALSE)
colnames(iris_data) <- c('ID','CT', 'UoCSi', 'UoCSh', 'MA', 'SECS', 'BN','BC','NN', 'Mit','class')

library(neuralnet)
set.seed(101)
size.sample <- floor(0.5 * nrow(iris_data))#powinno byc 0.5, 0.75
samples_id <- sample(1:nrow(iris_data), size.sample)
iristrain <- iris_data[c(samples_id),]
irisvalidation <- iris_data[-c(samples_id),]

nnet_iristrain <- iristrain
# Binarize the categorical output
unique(nnet_iristrain$class)
nnet_iristrain$benign <- iristrain$class == 'benign'
nnet_iristrain$malignant <- iristrain$class == 'malignant'


nnet_iristrain2 <- iristrain
for(a in unique(as.character(iris_data$class))){
  nnet_iristrain2[,a] <- nnet_iristrain2$class == a
}

colnames(nnet_iristrain2)#colnames(nnet_iristrain2)

# wyr <- unique(as.character(iris_data$class))[1]
# for (a in unique(as.character(iris_data$class))[-1])
# {
#   wyr <- paste(wyr, a, sep = "+")
# }
# 
# wyr <- paste(wyr, "~", sep = "")
# wyr2 <- colnames(iris_data)[-c(1,11)][1]
# for (a in colnames(iris_data)[-c(1,11)][-1])
# 
# {
#   wyr2 <- paste(wyr2, a, sep = "+")
# }
# wyr <- paste(wyr, wyr2, sep="")
# 
# my.exp <- as.formula(wyr)
# nn <- neuralnet(my.exp,
#                 data=nnet_iristrain,
#                 hidden=c(5), stepmax = 1e+06)


nn <- neuralnet(benign+malignant ~
                  CT+UoCSi+UoCSh+MA+SECS+BN+BC+NN+Mit,
               data=nnet_iristrain,
               hidden=c(10),stepmax = 1e+06)

plot(nn)

mypredict <- compute(nn, irisvalidation[-c(1,11)])$net.result
# Put multiple binary output to categorical output
maxidx <- function(arr) {
  return(which(arr == max(arr)))
}



idx <- apply(mypredict, c(1), maxidx)
#Confusion matrix
prediction <- c('benign', 'malignant')[idx]
table(prediction, irisvalidation$class)

#Dla 5
#recognition rate
# (223+105)/nrow(irisvalidation)  #0.9371429
# table(nnet_iristrain$class)
# table(iris_data$class)
# table(irisvalidation$class)
# #TPR true positive rate
# #benign
# 223/234 #0.9529915
# #malignant
# 105/116 #0.9051724


# #Dla 10 neuronow w warstwie urytej
# #recognition rate
# (226+101)/nrow(irisvalidation)  #0.9342857
# table(nnet_iristrain$class)
# table(iris_data$class)
# table(irisvalidation$class)
# #TPR true positive rate
# #benign
# 226/234 #0.965812
# #malignant
# 101/116 #0.8706897


#Dla 15
# (227+99)/nrow(irisvalidation) #0.9314286
# table(nnet_iristrain$class)
# table(iris_data$class)
# table(irisvalidation$class)
# #TPR true positive rate
# #benign
# 227/234 #0.9700855
# #malignant
# 99/116  #0.8534483


#Dla 20
# (224+101)/nrow(irisvalidation)  #0.9285714
# table(nnet_iristrain$class)
# table(iris_data$class)
# table(irisvalidation$class)
# #TPR true positive rate
# #benign
# 224/234 #0.957265
# #malignant
# 101/116 #0.8706897


#Dla 25
# (227+98)/nrow(irisvalidation) #0.9285714
# table(nnet_iristrain$class)
# table(iris_data$class)
# table(irisvalidation$class)
# #TPR true positive rate
# #benign
# 227/234 #0.9700855
# #malignant
# 98/116  #0.8448276


#Dla 30
# (227+105)/nrow(irisvalidation)  #0.9485714
# table(nnet_iristrain$class)
# table(iris_data$class)
# table(irisvalidation$class)
# #TPR true positive rate
# #benign
# 227/234 #0.9700855
# #malignant
# 105/116 #0.9051724
