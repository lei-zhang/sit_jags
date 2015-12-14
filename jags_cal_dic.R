source("_scripts/_read_data.R")
library(R.matlab)

#### calculate DIC####
dic1  <- out1$model$dic$dic
dic2  <- out2$model$dic$dic
dic3  <- out3$model$dic$dic
dic4  <- out4$model$dic$dic
dic5  <- out5$model$dic$dic
dic6  <- out6$model$dic$dic
dic7  <- out7$model$dic$dic
dic8  <- out8$model$dic$dic
dic9  <- out9$model$dic$dic
# dic10 <- out10$model$dic$dic
dic11 <- out11$model$dic$dic
dic12 <- out12$model$dic$dic
dic13 <- out13$model$dic$dic
dic14 <- out14$model$dic$dic

dic6_wide  <- out6_wide$model$dic$dic
dic7_wide  <- out7_wide$model$dic$dic
dic8_wide  <- out8_wide$model$dic$dic
dic9_wide  <- out9_wide$model$dic$dic

## save them into aa vector
dic_vec <- c(dic1, dic2, dic3, dic4, dic5,     # simple RL & RLnc
             dic6, dic7, dic8, dic9, #dic10,   # RLcoh
             dic11, dic13, dic12, dic14)       # RLcumrew

dic_vec_wide  <- c(dic6_wide, dic7_wide, dic8_wide, dic9_wide)

#### write ####
# write the dic variable to a .mat file
filename = "_outputs/jags_dic.mat"
writeMat(filename, dic_vec=dic_vec, 
                   dic_vec_wide=dic_vec_wide)


#### calculate deviance ####
dev1  <- out1$model$deviance.sum
dev2  <- out2$model$deviance.sum
dev3  <- out3$model$deviance.sum
dev4  <- out4$model$deviance.sum
dev5  <- out5$model$deviance.sum
dev6  <- out6$model$deviance.sum
dev7  <- out7$model$deviance.sum
dev8  <- out8$model$deviance.sum
dev9  <- out9$model$deviance.sum
# dev10 <- out10$model$deviance.sum
dev11 <- out11$model$deviance.sum
dev12 <- out12$model$deviance.sum
dev13 <- out13$model$deviance.sum
dev14 <- out14$model$deviance.sum


dev_mat <- rbind(dev1, dev2, dev3, dev4, dev5,       # RL 
                 dev6, dev7, dev8, dev9, # dev10,    # RLcoh
                 dev11, dev13, dev12, dev14)         # RLcumrew

#### write ####
filename = "_outputs/jags_dev.mat"
writeMat(filename, dev_mat=dev_mat)

