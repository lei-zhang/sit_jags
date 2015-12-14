## inits
source("_scripts/_read_data.R")
library(R.matlab)
ns <- 130  # nsubject


#### median ####
# simple RL model
mdn1    <- as.matrix(out1$model$summary$quantiles[,3])
param1  <- cbind( mdn1[10:(10+ns-1),1],             # lr
                  mdn1[(10+ns):(10+ns+ns-1),1])     # temp

# RLnc
mdn2    <- as.matrix(out2$model$summary$quantiles[,3])
param2  <- cbind( mdn2[10:(10+ns-1),1],             # lr
                  mdn2[(10+ns):(10+ns+ns-1),1])     # temp

# RLnc_2lr
mdn3    <- as.matrix(out3$model$summary$quantiles[,3])
param3  <- cbind( mdn3[14:(14+ns-1),1],             # lr1
                  mdn3[(14+ns):(14+ns*2-1),1],      # lr2
                  mdn3[(14+ns*2):(14+ns*3-1),1])    # temp

# RLnc_cfa
mdn4    <- as.matrix(out4$model$summary$quantiles[,3])
param4  <- cbind( mdn4[14:(14+ns-1),1],             # lr
                  mdn4[(14+ns):(14+ns*2-1),1],      # temp
                  mdn4[(14+ns*2):(14+ns*3-1),1])    # cfa

# RLnc_2lr_cfa
mdn5    <- as.matrix(out5$model$summary$quantiles[,3])
param5  <- cbind( mdn5[18:(18+ns-1),1],             # lr1
                  mdn5[(18+ns):(18+ns*2-1),1],      # lr2
                  mdn5[(18+ns*2):(18+ns*3-1),1],    # temp
                  mdn5[(18+ns*3):(18+ns*4-1),1])    # cfa

# RLcoh
mdn6    <- as.matrix(out6$model$summary$quantiles[,3])
param6  <- cbind( mdn6[14:(14+ns-1),1],             # lr1
                  mdn6[(14+ns):(14+ns*2-1),1],      # temp
                  mdn6[(14+ns*2):(14+ns*3-1),1],    # coha
                  mdn6[(14+ns*3):(14+ns*4-1),1])    # cohw

# RLcoh_2lr
mdn7    <- as.matrix(out7$model$summary$quantiles[,3])
param7  <- cbind( mdn7[18:(18+ns-1),1],             # lr1
                  mdn7[(18+ns):(18+ns*2-1),1],      # lr2
                  mdn7[(18+ns*2):(18+ns*3-1),1],    # temp
                  mdn7[(18+ns*3):(18+ns*4-1),1],    # coha
                  mdn7[(18+ns*4):(18+ns*5-1),1])    # cohw

# RLcoh_cfa
mdn8    <- as.matrix(out8$model$summary$quantiles[,3])
param8  <- cbind( mdn8[18:(18+ns-1),1],             # lr1
                  mdn8[(18+ns):(18+ns*2-1),1],      # temp
                  mdn8[(18+ns*2):(18+ns*3-1),1],    # coha
                  mdn8[(18+ns*3):(18+ns*4-1),1],    # cohw
                  mdn8[(18+ns*4):(18+ns*5-1),1])    # cfa

# RLcoh_2lr_cfa
mdn9    <- as.matrix(out9$model$summary$quantiles[,3])
param9  <- cbind( mdn9[22:(22+ns-1),1],             # lr1
                  mdn9[(22+ns):(22+ns*2-1),1],      # lr2
                  mdn9[(22+ns*2):(22+ns*3-1),1],    # temp
                  mdn9[(22+ns*3):(22+ns*4-1),1],    # coha
                  mdn9[(22+ns*4):(22+ns*5-1),1],    # cohw
                  mdn9[(22+ns*5):(22+ns*6-1),1])    # cfa

# RLcoh_2lr_bet
mdn10    <- as.matrix(out10$model$summary$quantiles[,3])
param10  <- cbind( mdn10[21:(21+ns-1),1],             # lr1
                   mdn10[(21+ns):(21+ns*2-1),1],      # lr2
                   mdn10[(21+ns*2):(21+ns*3-1),1],    # temp
                   mdn10[(21+ns*3):(21+ns*4-1),1],    # coha
                   mdn10[(21+ns*4):(21+ns*5-1),1],    # cohw
                   mdn10[(21+ns*5):(21+ns*6-1),1],    # tau1
                   mdn10[(21+ns*6):(21+ns*7-1),1])    # tau2



#RLcumrew
mdn11   <- as.matrix(out11$model$summary$quantiles[,3])
param11 <- cbind( mdn11[18:(18+ns-1),1],             # lr
                  mdn11[(18+ns):(18+ns*2-1),1],      # temp
                  mdn11[(18+ns*2):(18+ns*3-1),1],    # cra
                  mdn11[(18+ns*3):(18+ns*4-1),1],    # crw
                  mdn11[(18+ns*4):(18+ns*5-1),1])    # disc
                  

# RLcumrew_cfa
mdn12   <- as.matrix(out12$model$summary$quantiles[,3])
param12 <- cbind( mdn12[22:(22+ns-1),1],             # lr
                  mdn12[(22+ns):(22+ns*2-1),1],      # temp
                  mdn12[(22+ns*2):(22+ns*3-1),1],    # cra
                  mdn12[(22+ns*3):(22+ns*4-1),1],    # crw
                  mdn12[(22+ns*4):(22+ns*5-1),1],    # disc
                  mdn12[(22+ns*5):(22+ns*6-1),1])    # cfa

# RLcumrew_2lr
mdn13   <- as.matrix(out13$model$summary$quantiles[,3])
param13 <- cbind( mdn13[22:(22+ns-1),1],             # lr1
                  mdn13[(22+ns):(22+ns*2-1),1],      # lr2
                  mdn13[(22+ns*2):(22+ns*3-1),1],    # temp
                  mdn13[(22+ns*3):(22+ns*4-1),1],    # cra
                  mdn13[(22+ns*4):(22+ns*5-1),1],    # crw
                  mdn13[(22+ns*5):(22+ns*6-1),1])    # disc

# RLcumrew_2lr_cfa
mdn14   <- as.matrix(out14$model$summary$quantiles[,3])
param14 <- cbind( mdn14[26:(26+ns-1),1],             # lr1
                  mdn14[(26+ns):(26+ns*2-1),1],      # lr2
                  mdn14[(26+ns*2):(26+ns*3-1),1],    # temp
                  mdn14[(26+ns*3):(26+ns*4-1),1],    # cra
                  mdn14[(26+ns*4):(26+ns*5-1),1],    # crw
                  mdn14[(26+ns*5):(26+ns*6-1),1],    # disc
                  mdn14[(26+ns*6):(26+ns*7-1),1])    # cfa

#### write1 ####
## write all the fitting parameters to a .mat file
filename = "_outputs/jags_paramMdn.mat"
writeMat(filename, param1=param1,   param2=param2,
                   param3=param3,   param4=param4,
                   param5=param5,   param6=param6,
                   param7=param7,   param8=param8,
                   param9=param9,   param10=param10,
                   param11=param11, param12=param12,
                   param13=param13, param14=param14)



#### mean ####
# simple RL model
avg1        <- as.matrix(out1$model$summary$statistics[,1])
param1_avg  <- cbind( avg1[10:(10+ns-1),1],             # lr
                      avg1[(10+ns):(10+ns+ns-1),1])     # temp

# RLnc
avg2        <- as.matrix(out2$model$summary$statistics[,1])
param2_avg  <- cbind( avg2[10:(10+ns-1),1],             # lr
                      avg2[(10+ns):(10+ns+ns-1),1])     # temp

# RLnc_2lr
avg3        <- as.matrix(out3$model$summary$statistics[,1])
param3_avg  <- cbind( avg3[14:(14+ns-1),1],             # lr1
                      avg3[(14+ns):(14+ns*2-1),1],      # lr2
                      avg3[(14+ns*2):(14+ns*3-1),1])    # temp

# RLnc_cfa
avg4        <- as.matrix(out4$model$summary$statistics[,1])
param4_avg  <- cbind( avg4[14:(14+ns-1),1],             # lr
                      avg4[(14+ns):(14+ns*2-1),1],      # temp
                      avg4[(14+ns*2):(14+ns*3-1),1])    # cfa

# RLnc_2lr_cfa
avg5        <- as.matrix(out5$model$summary$statistics[,1])
param5_avg  <- cbind( avg5[18:(18+ns-1),1],             # lr1
                      avg5[(18+ns):(18+ns*2-1),1],      # lr2
                      avg5[(18+ns*2):(18+ns*3-1),1],    # temp
                      avg5[(18+ns*3):(18+ns*4-1),1])    # cfa

# RLcoh
avg6        <- as.matrix(out6$model$summary$statistics[,1])
param6_avg  <- cbind( avg6[14:(14+ns-1),1],             # lr1
                      avg6[(14+ns):(14+ns*2-1),1],      # temp
                      avg6[(14+ns*2):(14+ns*3-1),1],    # coha
                      avg6[(14+ns*3):(14+ns*4-1),1])    # cohw

# RLcoh_2lr
avg7        <- as.matrix(out7$model$summary$statistics[,1])
param7_avg  <- cbind( avg7[18:(18+ns-1),1],             # lr1
                      avg7[(18+ns):(18+ns*2-1),1],      # lr2
                      avg7[(18+ns*2):(18+ns*3-1),1],    # temp
                      avg7[(18+ns*3):(18+ns*4-1),1],    # coha
                      avg7[(18+ns*4):(18+ns*5-1),1])    # cohw

# RLcoh_cfa
avg8        <- as.matrix(out8$model$summary$statistics[,1])
param8_avg  <- cbind( avg8[18:(18+ns-1),1],             # lr1
                      avg8[(18+ns):(18+ns*2-1),1],      # temp
                      avg8[(18+ns*2):(18+ns*3-1),1],    # coha
                      avg8[(18+ns*3):(18+ns*4-1),1],    # cohw
                      avg8[(18+ns*4):(18+ns*5-1),1])    # cfa

# RLcoh_2lr_cfa
avg9        <- as.matrix(out9$model$summary$statistics[,1])
param9_avg  <- cbind( avg9[22:(22+ns-1),1],             # lr1
                      avg9[(22+ns):(22+ns*2-1),1],      # lr2
                      avg9[(22+ns*2):(22+ns*3-1),1],    # temp
                      avg9[(22+ns*3):(22+ns*4-1),1],    # coha
                      avg9[(22+ns*4):(22+ns*5-1),1],    # cohw
                      avg9[(22+ns*5):(22+ns*6-1),1])    # cfa

# RLcoh_2lr_bet
avg10       <- as.matrix(out10$model$summary$statistics[,1])
param10_avg <- cbind( avg10[21:(21+ns-1),1],             # lr1
                      avg10[(21+ns):(21+ns*2-1),1],      # lr2
                      avg10[(21+ns*2):(21+ns*3-1),1],    # temp
                      avg10[(21+ns*3):(21+ns*4-1),1],    # coha
                      avg10[(21+ns*4):(21+ns*5-1),1],    # cohw
                      avg10[(21+ns*5):(21+ns*6-1),1],    # tau1
                      avg10[(21+ns*6):(21+ns*7-1),1])    # tau2

#RLcumrew
avg11       <- as.matrix(out11$model$summary$statistics[,1])
param11_avg <- cbind( avg11[18:(18+ns-1),1],             # lr
                      avg11[(18+ns):(18+ns*2-1),1],      # temp
                      avg11[(18+ns*2):(18+ns*3-1),1],    # cra
                      avg11[(18+ns*3):(18+ns*4-1),1],    # crw
                      avg11[(18+ns*4):(18+ns*5-1),1])    # disc


# RLcumrew_cfa
avg12       <- as.matrix(out12$model$summary$statistics[,1])
param12_avg <- cbind( avg12[22:(22+ns-1),1],             # lr
                      avg12[(22+ns):(22+ns*2-1),1],      # temp
                      avg12[(22+ns*2):(22+ns*3-1),1],    # cra
                      avg12[(22+ns*3):(22+ns*4-1),1],    # crw
                      avg12[(22+ns*4):(22+ns*5-1),1],    # disc
                      avg12[(22+ns*5):(22+ns*6-1),1])    # cfa

# RLcumrew_2lr
avg13       <- as.matrix(out13$model$summary$statistics[,1])
param13_avg <- cbind( avg13[22:(22+ns-1),1],             # lr1
                      avg13[(22+ns):(22+ns*2-1),1],      # lr2
                      avg13[(22+ns*2):(22+ns*3-1),1],    # temp
                      avg13[(22+ns*3):(22+ns*4-1),1],    # cra
                      avg13[(22+ns*4):(22+ns*5-1),1],    # crw
                      avg13[(22+ns*5):(22+ns*6-1),1])    # disc

# RLcumrew_2lr_cfa
avg14       <- as.matrix(out14$model$summary$statistics[,1])
param14_avg <- cbind( avg14[26:(26+ns-1),1],             # lr1
                      avg14[(26+ns):(26+ns*2-1),1],      # lr2
                      avg14[(26+ns*2):(26+ns*3-1),1],    # temp
                      avg14[(26+ns*3):(26+ns*4-1),1],    # cra
                      avg14[(26+ns*4):(26+ns*5-1),1],    # crw
                      avg14[(26+ns*5):(26+ns*6-1),1],    # disc
                      avg14[(26+ns*6):(26+ns*7-1),1])    # cfa

#### write2 #### 
## write all the fitting parameters to a .mat file
filename = "_outputs/jags_paramAvg.mat"
writeMat(filename, param1_avg=param1_avg,   param2_avg=param2_avg,
                   param3_avg=param3_avg,   param4_avg=param4_avg,
                   param5_avg=param5_avg,   param6_avg=param6_avg,
                   param7_avg=param7_avg,   param8_avg=param8_avg,
                   param9_avg=param9_avg,   param10_avg=param10_avg,
                   param11_avg=param11_avg, param12_avg=param12_avg,
                   param13_avg=param13_avg, param14_avg=param14_avg)


#### median_wide ####
# RLcoh
mdn6w    <- as.matrix(out6_wide$model$summary$quantiles[,3])
param6w  <- cbind( mdn6w[14:(14+ns-1),1],             # lr1
                   mdn6w[(14+ns):(14+ns*2-1),1],      # temp
                   mdn6w[(14+ns*2):(14+ns*3-1),1],    # coha
                   mdn6w[(14+ns*3):(14+ns*4-1),1])    # cohw

# RLcoh_2lr
mdn7w    <- as.matrix(out7_wide$model$summary$quantiles[,3])
param7w  <- cbind( mdn7w[18:(18+ns-1),1],             # lr1
                   mdn7w[(18+ns):(18+ns*2-1),1],      # lr2
                   mdn7w[(18+ns*2):(18+ns*3-1),1],    # temp
                   mdn7w[(18+ns*3):(18+ns*4-1),1],    # coha
                   mdn7w[(18+ns*4):(18+ns*5-1),1])    # cohw

# RLcoh_cfa
mdn8w    <- as.matrix(out8_wide$model$summary$quantiles[,3])
param8w  <- cbind( mdn8w[18:(18+ns-1),1],             # lr1
                   mdn8w[(18+ns):(18+ns*2-1),1],      # temp
                   mdn8w[(18+ns*2):(18+ns*3-1),1],    # coha
                   mdn8w[(18+ns*3):(18+ns*4-1),1],    # cohw
                   mdn8w[(18+ns*4):(18+ns*5-1),1])    # cfa

# RLcoh_2lr_cfa
mdn9w    <- as.matrix(out9_wide$model$summary$quantiles[,3])
param9w  <- cbind( mdn9w[22:(22+ns-1),1],             # lr1
                   mdn9w[(22+ns):(22+ns*2-1),1],      # lr2
                   mdn9w[(22+ns*2):(22+ns*3-1),1],    # temp
                   mdn9w[(22+ns*3):(22+ns*4-1),1],    # coha
                   mdn9w[(22+ns*4):(22+ns*5-1),1],    # cohw
                   mdn9w[(22+ns*5):(22+ns*6-1),1])    # cfa



#### write3 ####
filename = "_outputs/jags_paramMdn_wide.mat"
writeMat(filename, param6w=param6w, param7w=param7w,
                   param8w=param8w, param9w=param9w)


#### mean_wide ####
# RLcoh
avg6w        <- as.matrix(out6_wide$model$summary$statistics[,1])
param6w_avg  <- cbind( avg6w[14:(14+ns-1),1],             # lr1
                       avg6w[(14+ns):(14+ns*2-1),1],      # temp
                       avg6w[(14+ns*2):(14+ns*3-1),1],    # coha
                       avg6w[(14+ns*3):(14+ns*4-1),1])    # cohw

# RLcoh_2lr
avg7w        <- as.matrix(out7_wide$model$summary$statistics[,1])
param7w_avg  <- cbind( avg7w[18:(18+ns-1),1],             # lr1
                       avg7w[(18+ns):(18+ns*2-1),1],      # lr2
                       avg7w[(18+ns*2):(18+ns*3-1),1],    # temp
                       avg7w[(18+ns*3):(18+ns*4-1),1],    # coha
                       avg7w[(18+ns*4):(18+ns*5-1),1])    # cohw

# RLcoh_cfa
avg8w        <- as.matrix(out8_wide$model$summary$statistics[,1])
param8w_avg  <- cbind( avg8w[18:(18+ns-1),1],             # lr1
                       avg8w[(18+ns):(18+ns*2-1),1],      # temp
                       avg8w[(18+ns*2):(18+ns*3-1),1],    # coha
                       avg8w[(18+ns*3):(18+ns*4-1),1],    # cohw
                       avg8w[(18+ns*4):(18+ns*5-1),1])    # cfa

# RLcoh_2lr_cfa
avg9w        <- as.matrix(out9_wide$model$summary$statistics[,1])
param9w_avg  <- cbind( avg9w[22:(22+ns-1),1],             # lr1
                       avg9w[(22+ns):(22+ns*2-1),1],      # lr2
                       avg9w[(22+ns*2):(22+ns*3-1),1],    # temp
                       avg9w[(22+ns*3):(22+ns*4-1),1],    # coha
                       avg9w[(22+ns*4):(22+ns*5-1),1],    # cohw
                       avg9w[(22+ns*5):(22+ns*6-1),1])    # cfa


#### write4 #### 
filename = "_outputs/jags_paramAvg_wide.mat"
writeMat(filename, param6w_avg=param6w_avg, param7w_avg=param7w_avg,
                   param8w_avg=param8w_avg, param9w_avg=param9w_avg)   





#### old bet model ####
mdn10n    <- as.matrix(out10_old$model$summary$quantiles[,3])
param10n  <- cbind( mdn10n[25:(25+ns-1),1],             # lr1
                    mdn10n[(25+ns):(25+ns*2-1),1],      # lr2
                    mdn10n[(25+ns*2):(25+ns*3-1),1],    # temp
                    mdn10n[(25+ns*3):(25+ns*4-1),1],    # coha
                    mdn10n[(25+ns*4):(25+ns*5-1),1],    # cohw
                    mdn10n[(25+ns*5):(25+ns*6-1),1],    # tau1
                    mdn10n[(25+ns*6):(25+ns*7-1),1])    # tau2


