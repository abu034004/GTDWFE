# Confusion matrix for oversampling

library(ROSE) 
library(e1071)
library(caret)
library(ROCR)

library(klaR)


sval<-123

list_under<-c()


list_under<-c(list_under, title)


file_n<-paste0("E:\\Research\\All ML works\\Extracted Feature_revised\\CD-HIT Resistance Genes\\Merged_BL\\Merged_BL_Nature\\GT\\subset3\\Merged_BL_with non_AR_S_18", ".csv")
file_other<-paste0("E:\\Research\\All ML works\\Extracted Feature_revised\\CD-HIT Resistance Genes\\Merged_BL\\Merged_BL_Nature\\pse_vib_ent\\BL_merged_feature_over18",".csv")


data <- read.csv(file_n, header = TRUE)
data_other <- read.csv(file_other, header = TRUE)

data$Output <- as.factor(data$Output)
data_other$Output <- as.factor(data_other$Output)
data_other$Output

set.seed(sval)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(1, 0))
set.seed(sval)
ind_other <- sample(2, nrow(data_other), replace = TRUE, prob = c(1, 0))

train <- data[ind==1,]
test <- data_other[ind_other==1,]
summary(test$Output)

k<-summary(train$Output)

maxim<-0
minim<-0
if(k[1]>=k[2]){
  maxim=k[1]
  minim=k[2]
} else {
  maxim =k[2]
  minim=k[1]
}


under <- ovun.sample(Output~., data = train, method = "over", N = 2*maxim)$data

set.seed(sval)

tmodel2<-tune(svm, Output~., data = under, ranges = list(epsilon =seq(0,1,0.1), cost=2^(2:7)))
mymodel2<-tmodel2$best.model
mymodel2
conf2_1<-confusionMatrix(predict(mymodel2, test), test$Output, positive = '1')
conf2_1$table
predict(mymodel2, test)
acc_under<-conf2_1$overall[1]
acc_under


list_under<-c(list_under, round(acc_under,4))

#file_under<-paste0("E:\\Research\\GT\\pseudovibenterononarwith histone\\svm_results\\over_statistics_sep",sval, ".txt")

#write.csv(list_under, file_under, row.names = FALSE)




# Draw confusion matrix

draw_confusion_matrix <- function(cm) {
  
 # layout(matrix(c(1,2)))
  par(mar=c(2,2,2,2))
  plot(c(127, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n',axes=T)
 # title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Negative', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Positive', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Negative', cex=1.2, srt=90)
  text(140, 335, 'Positive', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
}  


draw_confusion_matrix(conf2_1)