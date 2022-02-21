library(dplyr)
library(tidyr)

all_pop <- read.csv("PhenoStats.csv")

#Test difference between Indica and Japonica
head(all_pop)

ind_jap <- subset(all_pop,all_pop$Subtype=="indica" | all_pop$Subtype=="japonica")
dim(ind_jap)
[1] 637  24

t.test(ind_jap$Grain_Length  ~  ind_jap$Subtype,var.equal = TRUE) 
Two Sample t-test

data:  ind_jap$Grain_Length by ind_jap$Subtype
t = -2.3632, df = 471, p-value = 0.01852
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  -0.37970155 -0.03493452
sample estimates:
  mean in group indica mean in group japonica 
8.761390               8.968708 


t.test(ind_jap$Grain_Width  ~  ind_jap$Subtype,var.equal = TRUE)
Two Sample t-test
data:  ind_jap$Grain_Width by ind_jap$Subtype
t = -14.367, df = 473, p-value < 2.2e-16
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  -0.5895265 -0.4476672
sample estimates:
  mean in group indica mean in group japonica 
2.821178               3.339775 


t.test(ind_jap$GL_GW_ratio  ~  ind_jap$Subtype,var.equal = TRUE) 

Two Sample t-test

data:  ind_jap$GL_GW_ratio by ind_jap$Subtype
t = 8.25, df = 470, p-value = 1.606e-15
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  0.3375101 0.5485577
sample estimates:
  mean in group indica mean in group japonica 
3.172780               2.729746 

t.test(ind_jap$Heading_Date  ~  ind_jap$Subtype,var.equal = TRUE) 

t.test(ind_jap$Culm_Strength  ~  ind_jap$Subtype,var.equal = TRUE)
Two Sample t-test

data:  ind_jap$Culm_Strength by ind_jap$Subtype
t = 3.6749, df = 422, p-value = 0.0002686
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  0.4491357 1.4821287
sample estimates:
  mean in group indica mean in group japonica 
5.236220               4.270588 



t.test(ind12$Grain_Length  ~  ind12$subpop,var.equal = TRUE) 
t.test(ind12$Grain_Width  ~  ind12$subpop,var.equal = TRUE) 
t.test(ind12$GL_GW_ratio  ~  ind12$subpop,var.equal = TRUE) 
t.test(ind12$Heading_Date  ~  ind12$subpop,var.equal = TRUE) 
t.test(ind12$Culm_Strength  ~  ind12$subpop,var.equal = TRUE)




jap<- subset(all_pop,all_pop$subpops=="J1" | all_pop$subpops=="J2" | all_pop$subpops=="J3"| all_pop$subpops=="J4")

jap12<- subset(all_pop,all_pop$subpops=="J1"  | all_pop$subpops=="J2" )
jap13<- subset(all_pop,all_pop$subpops=="J1"  | all_pop$subpops=="J3" )
jap14<- subset(all_pop,all_pop$subpops=="J1"  | all_pop$subpops=="J4" )
jap24<- subset(all_pop,all_pop$subpops=="J2"  | all_pop$subpops=="J4" )
jap23<- subset(all_pop,all_pop$subpops=="J2"  | all_pop$subpops=="J3" )
jap34<- subset(all_pop,all_pop$subpops=="J3"  | all_pop$subpops=="J4" )



ind12 <- subset(ind,ind$subpops=="I1" | ind$subpops=="I2" )

t.test(ind12$Grain_Length  ~  ind12$subpop,var.equal = TRUE) 
t.test(ind12$Grain_Width  ~  ind12$subpop,var.equal = TRUE) 
t.test(ind12$GL_GW_ratio  ~  ind12$subpop,var.equal = TRUE) 
t.test(ind12$Heading_Date  ~  ind12$subpop,var.equal = TRUE) 
t.test(ind12$Culm_Strength  ~  ind12$subpop,var.equal = TRUE)

t.test(jap12$GL_GW_ratio  ~  jap12$subpop,var.equal = TRUE) 
t.test(jap13$GL_GW_ratio  ~  jap13$subpop,var.equal = TRUE) 
t.test(jap14$GL_GW_ratio  ~  jap14$subpop,var.equal = TRUE) 
t.test(jap24$GL_GW_ratio  ~  jap24$subpop,var.equal = TRUE) 
t.test(jap23$GL_GW_ratio  ~  jap23$subpop,var.equal = TRUE) 
t.test(jap34$GL_GW_ratio  ~  jap34$subpop,var.equal = TRUE) 

t.test(jap12$Heading_Date  ~  jap12$subpop,var.equal = TRUE) 
t.test(jap13$Heading_Date  ~  jap13$subpop,var.equal = TRUE) 
t.test(jap14$Heading_Date  ~  jap14$subpop,var.equal = TRUE) 
t.test(jap24$Heading_Date  ~  jap24$subpop,var.equal = TRUE) 
t.test(jap23$Heading_Date  ~  jap23$subpop,var.equal = TRUE) 
t.test(jap34$Heading_Date  ~  jap34$subpop,var.equal = TRUE) 

