# RReliefF feature selection

library(FSelector)

file_n<-paste0("E:\\Research\\All ML works\\Extracted Feature_revised\\CD-HIT Resistance Genes\\Merged_BL\\Merged_BL_Nature\\Merged_BL_with non_AR_Final_exclude1", ".csv")
df <- read.csv(file_n, header=TRUE)
nc<-ncol(df)


l<-c()

# Calculate weights
weights <- relief(Output~., df, neighbours.count = 5, sample.size = 30)

# Selected features
subset <- cutoff.k(weights, 30)

coln<-colnames(df)

# Create k feature subsets (1<=k<=30)

for(i in 1:30){
  list_f<-c()
  colpos<-match(subset[i],coln)
  l<-c(l,df[colpos])
  list_f<-c(l,df[nc])
  file_over<-paste0("E:\\Research\\All ML works\\Extracted Feature_revised\\CD-HIT Resistance Genes\\Merged_BL\\Merged_BL_Nature\\relief\\BL_best_feature",i, ".csv")
  
  write.csv(list_f, file_over, row.names = FALSE)
  
  
}


