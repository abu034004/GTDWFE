# Selecting features for Pseudomonas, Vibrio and Enterobacter 
list_z<-c()
file_n<-paste0("E:\\Research\\All ML works\\Extracted Feature_revised\\CD-HIT Resistance Genes\\Merged_BL\\Merged_BL_Nature\\GT\\subset3\\Merged_BL_with non_AR_S_11", ".csv")
data <- read.csv(file_n, header = TRUE)

z<-length(data)

col_n<-colnames(data)


file_name1<-paste0("E:\\Research\\All ML works\\Each AMR gene\\split\\Sequence\\Test1\\joined\\separate\\Filtered\\Pseudomonas Vibrio Enterobacter\\bl\\merged_bl\\bl_merged",".csv")
data1 <- read.csv(file_name1, header = TRUE)

d<-length(data1)

col_n1<-colnames(data1)


for(i in 1:z){
  for(j in 2:d){
    if(col_n[i]==col_n1[j]){
      list_z<-c(list_z, data1[j])
    }
  }
  
}

file_over<-paste0("E:\\Research\\All ML works\\Extracted Feature_revised\\CD-HIT Resistance Genes\\Merged_BL\\Merged_BL_Nature\\pse_vib_ent\\BL_merged_feature_under11",".csv")

write.csv(list_z, file_over, row.names = FALSE)


