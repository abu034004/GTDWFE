# Creating k feature subsets (1<=k<=30)

file_n<-paste0("E:\\Research\\All ML works\\Extracted Feature_revised\\CD-HIT Resistance Genes\\Merged_BL\\Merged_BL_Nature\\Merged_BL_with_non_AR_subset_Final_exclude130_3", ".csv")
data <- read.csv(file_n, header = TRUE)
z<-length(data)
lp<-z-1
list_dat<-c()
for(i in 1: lp){
  list_add<-c()
  list_dat<-c(list_dat, data[i])
  list_add<-c(list_add, list_dat, data[z])
  file_name<-paste0("E:\\Research\\All ML works\\Extracted Feature_revised\\CD-HIT Resistance Genes\\Merged_BL\\Merged_BL_Nature\\GT\\subset3\\Merged_BL_with non_AR_S_", i, ".csv")
  write.csv(list_add, file_name, row.names = FALSE)
  
}

