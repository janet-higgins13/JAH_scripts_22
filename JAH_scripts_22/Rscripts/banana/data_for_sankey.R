library(dplyr)
library(tidyr)

list <- read.csv("../metadata_13_clusters.csv",header=TRUE)

str(list)

table(list$our_group)

dim(list)

cl1_Sucrier <- subset(list,list$our_group == "cl1_Sucrier")
table(cl1_Sucrier$MGIS_AGROSAVIA)
table(cl1_Sucrier$Sub.species.or.Sub.group)

cav <- subset(list,list$our_group == "cl2_Cavendish")
table(cav$MGIS_AGROSAVIA)
table(cav$Sub.species.or.Sub.group)


table(cav$Sub_ref)

cav <- subset(list,list$our_group == "cl3_GrosMichel")
table(cav$MGIS_AGROSAVIA)
table(cav$Sub.species.or.Sub.group)

cav <- subset(list,list$our_group == "cl4_GrosMichel")
table(cav$MGIS_AGROSAVIA)
table(cav$Sub.species.or.Sub.group)

cav <- subset(list,list$our_group == "cl5_Red")
table(cav$MGIS_AGROSAVIA)
table(cav$Sub.species.or.Sub.group)

cav <- subset(list,list$our_group == "cl6_AAAA")
table(cav$MGIS_AGROSAVIA)
table(cav$Sub.species.or.Sub.group)

cav <- subset(list,list$our_group == "cl7_Mutika")
table(cav$MGIS_AGROSAVIA)
table(cav$Sub.species.or.Sub.group)


cav <- subset(list,list$our_group == "cl8_Plantain-like")
table(cav$MGIS_AGROSAVIA)
table(cav$Sub.species.or.Sub.group)


cav <- subset(list,list$our_group == "cl9_AAAB")
table(cav$MGIS_AGROSAVIA)
table(cav$Sub.species.or.Sub.group)


cav <- subset(list,list$our_group == "cl10_Popoulu")
table(cav$MGIS_AGROSAVIA)
table(cav$Sub.species.or.Sub.group)

cav <- subset(list,list$our_group == "cl11_Plantain")
table(cav$MGIS_AGROSAVIA)
table(cav$Sub.species.or.Sub.group)


cav <- subset(list,list$our_group == "cl9_AAAB")
table(cav$MGIS_AGROSAVIA)
table(cav$Sub.species.or.Sub.group)


cav <- subset(list,list$our_group == "cl12_ABB_Bluggoe")
table(cav$MGIS_AGROSAVIA)
table(cav$Sub.species.or.Sub.group)


cav <- subset(list,list$our_group == "cl13_ABB_Pelipita")
table(cav$MGIS_AGROSAVIA)
table(cav$Sub.species.or.Sub.group)


