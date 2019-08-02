# Game theoretic dynamic weighting based feature evaluation (GTDWFE)

library(sets)
require('HapEstXXR')
library(stringr)
library(stringi)
library(infotheo)
library(proxy)

# 621D features with values 
df <- read.csv("E:\\Data files\\Ac_Sa_Ca_Kl_Ec\\bla_all.csv", header=TRUE)
dat<-discretize(df)

# Number of selected features
Th<-3
# Coalition size
p<-3


w<-matrix(1,1,621)
sum_RR<-matrix(0,1,621)
lf<-matrix(0,1,621)
flag<-matrix(0,1,621)
Banzhaf_power<-matrix(0,1,621)

list_z<-c()
col_added<-c()
t<-1
CMI<-0
MI<-0

# Calculate Pearson's correlation coefficient and Tanimoto coefficient
for (i in 2:622){
  summation<-0
  corre<-abs (cor(df[[i]], df[[623]], method = "pearson"))
  for (j in 2:622){
    if(i!=j){
      Tanimoto_coeff<-dist(rbind(df[[i]],df[[j]]), method ="eJaccard")
      #eucl
      summation<-summation+Tanimoto_coeff
    }
  }
  Tanimoto_coeff_avg =summation/620
  sum_RR[i-1]= corre+Tanimoto_coeff_avg
}



while(t<=Th){
  for(i in 1:621){
    if(flag[i]!=1){
      lf[i]<-sum_RR[i]*w[i]
    }
  }
  
  # Select feature with largest lf
    maximum<-0
    index<-0
    for(i in 1:621){
      if(flag[i]!=1){
        if(maximum<lf[i]){
          maximum =lf[i]
          index =i
        }
      }
      
    }
    flag[index]<-1
    
    list_z<-c(list_z, df[index+1])
    
    col_added<-c(col_added,colnames(df[index+1]))
    
    
    len_col<-length(col_added)

    # Calculate Banzhaf power index
    
    if(t!=Th) {
    
    for (x in 1:621){
      if(flag[x]!=1){
      
        combin<-0
        
       
        for(y in 1: p){
          if(len_col>=y){
          aa<-combn(col_added, y, simplify = FALSE)
          len<-length(aa)
          combin<-combin+len
          
          for(g in 1: len){
            
            if(any(aa[[g]]==colnames(df[index+1]))==TRUE) {
            h<-aa[[g]]
            
            count<-0
            sum_col_MI <-0
            for(q in 1: y){
              col_posit<-match(h[q], colnames(df))
              sum_col_MI<-sum_col_MI+condinformation(dat[,col_posit], dat[,623], S=dat[,x+1], method="emp")-mutinformation(dat[,col_posit], dat[,623], method="emp")
            }
            sum_col_MI<-sum_col_MI/y
            for(v in 1: y){
              posit<-match(h[v], colnames(df))
              CMI<-condinformation(dat[,posit], dat[,623], S=dat[,x+1], method="emp")
              MI<-mutinformation(dat[,posit], dat[,623], method="emp")
              if(CMI>MI){
                count<-count+1
              }
            }
            if(sum_col_MI>=0 && count>=ceiling(y/2)){
              Banzhaf_power[x]<-Banzhaf_power[x]+1
            }
            
          }
            
          }
         }
        }
        w[x]=w[x]+Banzhaf_power[x]/combin
      
      }
      
    }
    
}
    
   t<-t+1 
  
  
}
  
# Selected features
list_z<-c(list_z, df[623])

file_name<-paste0("E:\\Data files\Ac_Sa_Ca_Kl_Ec\Best 30 features\\delta_3\\bla_best_30 features", ".csv")
write.csv(list_z, file_name, row.names = FALSE)



