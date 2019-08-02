# Evaluate k feature subsets (1<=k<=30) both for oversampling and undersampling

library(ROSE) 
library(e1071)
library(caret)
library(ROCR)
library(FSelector)
list_over<-c()
list_under<-c()
title<-paste0("Subset", ",", "GT_3", ",", "ReliefF_30")
list_over<-c(list_over, title)
list_under<-c(list_under, title)

# Coalition size
p<-3



for(i in 1:30){
  file_n<-paste0("E:\\Research\\All ML works\\Extracted Feature_revised\\CD-HIT Resistance Genes\\Merged_BL\\Merged_BL_Nature\\GT\\subset3\\Merged_BL_with non_AR_S_",i, ".csv")
  file_n_rel<-paste0("E:\\Research\\All ML works\\Extracted Feature_revised\\CD-HIT Resistance Genes\\Merged_BL\\Merged_BL_Nature\\relief\\BL_best_feature",i, ".csv")
  
  data <- read.csv(file_n, header = TRUE)
  data_rel <- read.csv(file_n_rel, header = TRUE)
  
  data$Output <- as.factor(data$Output)
  data_rel$Output <- as.factor(data_rel$Output)
  
# 70% training data and 30% testing data 
  set.seed(123)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  set.seed(123)
  ind_rel <- sample(2, nrow(data_rel), replace = TRUE, prob = c(0.7, 0.3))
  
  train <- data[ind==1,]
  train_rel <- data_rel[ind_rel==1,]
  
  k<-summary(train$Output)
  k_rel<-summary(train_rel$Output)
  
  maxim<-0
  minim<-0
  if(k[1]>=k[2]){
    maxim=k[1]
    minim=k[2]
  } else {
    maxim =k[2]
    minim=k[1]
  }
  maxim_rel<-0
  minim_rel<-0
  if(k_rel[1]>=k_rel[2]){
    maxim_rel=k_rel[1]
    minim_rel=k_rel[2]
  } else {
    maxim_rel =k_rel[2]
    minim_rel=k_rel[1]
  }
  
  test <- data[ind==2,] 
  test_rel <- data_rel[ind_rel==2,] 

  
  over <- ovun.sample(Output~., data = train, method = "over", N = 2*maxim)$data
  table(over$Output)
  over_rel <- ovun.sample(Output~., data = train_rel, method = "over", N = 2*maxim_rel)$data
  summary(over_rel)
  
  set.seed(123)
  
  tmodel1<-tune(svm, Output~., data = over, ranges = list(epsilon =seq(0,1,0.1), cost=2^(2:7)))
  mymodel1<-tmodel1$best.model
  mymodel1
  
  conf1<-confusionMatrix(predict(mymodel1, test), test$Output, positive = '1')
  acc<-conf1$overall[1]
  
  set.seed(123)
  tmodel1_rel<-tune(svm, Output~., data = over_rel, ranges = list(epsilon =seq(0,1,0.1), cost=2^(2:7)))
  mymodel1_rel<-tmodel1_rel$best.model
  conf1_rel<-confusionMatrix(predict(mymodel1_rel, test_rel), test_rel$Output, positive = '1')
  acc_rel<-conf1_rel$overall[1]
  
  add_over<-paste0(i, ",", round(acc,4), ",", round(acc_rel,4))
  list_over<-c(list_over, add_over)
  
  
  
  under <- ovun.sample(Output~., data = train, method = "under", N = 2*minim)$data
  under_rel <- ovun.sample(Output~., data = train_rel, method = "under", N = 2*minim_rel)$data
  
  set.seed(123)
  
  tmodel2<-tune(svm, Output~., data = under, ranges = list(epsilon =seq(0,1,0.1), cost=2^(2:7)))
  mymodel2<-tmodel2$best.model
  mymodel2
  
  conf2<-confusionMatrix(predict(mymodel2, test), test$Output, positive = '1')
  acc_under<-conf2$overall[1]
  set.seed(123)
  tmodel2_rel<-tune(svm, Output~., data = under_rel, ranges = list(epsilon =seq(0,1,0.1), cost=2^(2:7)))
  mymodel2_rel<-tmodel2_rel$best.model
  conf2_rel<-confusionMatrix(predict(mymodel2_rel, test_rel), test_rel$Output, positive = '1')
  acc_under_rel<-conf2_rel$overall[1]
  
  
  add_under<-paste0(i, ",", round(acc_under,4), ",", round(acc_under_rel,4))
  list_under<-c(list_under, add_under)
  
  
}


file_over<-paste0("E:\\Research\\All ML works\\Extracted Feature_revised\\CD-HIT Resistance Genes\\Merged_BL\\Merged_BL_Nature\\svm_results\\Over_statistics_svm_final",i,"_",p, ".txt")
file_under<-paste0("E:\\Research\\All ML works\\Extracted Feature_revised\\CD-HIT Resistance Genes\\Merged_BL\\Merged_BL_Nature\\svm_results\\Under_statistics_svm_final",i,"_",p, ".txt")
write.csv(list_over, file_over, row.names = FALSE)
write.csv(list_under, file_under, row.names = FALSE)



























