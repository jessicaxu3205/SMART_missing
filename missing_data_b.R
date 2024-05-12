#--Missing example 2b-------------
# Stage 1 responder missing dependent on Y;and stage 2 outcome Y missing dependent on A2 
missing.mar.s2b <- function (data, 
                            intercept1= -1.8,
                            intercept2= -1.8,
                            a2_or = 1.6,
                            y_or  = 1.6)
{
  data_miss <- as.data.frame(data)
  
  alpha1    <- log(y_or)
  alpha2    <- log(a2_or)
  
  #o2 missing dependent on Y 
  data_miss$logit_y <- intercept1 + alpha1*data_miss$Y 
  
  data_miss$p_y     <- expit(data_miss$logit_y)
  
  data_miss$m_o2  <- rbinom(n_patients, 1, data_miss$p_y)
  
  data_miss$o2[data_miss$m_o2==1] <- NA
  
  #missing y dependent on a2
  data_miss$logit_a2 <- intercept2 + alpha2*data_miss$a2
  data_miss$p_a2     <- expit(data_miss$logit_a2)
  
  data_miss$m_y  <- rbinom(n_patients, 1, data_miss$p_a2)
  
  data_miss$m_y[data_miss$m_o2==1]  <- 1
  
  data_miss$a2[data_miss$m_o2==1] <- NA
  data_miss$Y[data_miss$m_y==1] <- NA
  
  data_miss$o2_a2[is.na(data_miss$o2)] <- NA
  data_miss$a1_a2[is.na(data_miss$a2)] <- NA
  
  
  return(data_miss)
}



#--Missing example 3b-------------
# Data missing due to drop out in stage1 and therefore missing Y
#O2 missing dependent on A1,O1 and Y; Y missing if O2 missing

missing.drops1b <- function (data, intercept= -1.8 ,
                            o1_or = 1.6,
                            a1_or = 1.6,
                            y_or  = 1.6)
{
  data_miss <- as.data.frame(data)
  
  alpha1    <- log(o1_or)
  alpha2    <- log(a1_or)
  alpha3    <- log(y_or)
  
  #missing o2 dependent on o1, a1 and Y
  data_miss$logit <- intercept + alpha1*data_miss$o1 
  + alpha2*data_miss$a1 
  + alpha3*data_miss$Y 
  
  data_miss$p     <- expit(data_miss$logit)
  
  data_miss$m_o2  <- rbinom(n_patients, 1, data_miss$p)
  
  data_miss$o2[data_miss$m_o2==1] <- NA
  
  
  #all participants with missing o2 will have missing a2 and also missing Y
  data_miss$a2[is.na(data_miss$o2)] <- NA
  data_miss$Y[is.na(data_miss$a2)]  <- NA
  
  data_miss$o2_a2[is.na(data_miss$o2)] <- NA
  data_miss$a1_a2[is.na(data_miss$a2)] <- NA
  
  
  return(data_miss)
} 


#--Missing example 4-------------
#stage 1 treatment responders more likely to drop out before stage 2 
# o2 -> Ma2 -> My
# A2 missing dependent on O2 and Y

missing.responder.drops2b <- function (data, intercept= -1.8 ,
                                      o2_or = 1.6,
                                      y_or  = 1.6)
{
  data_miss <- as.data.frame(data)
  
  alpha1    <- log(o2_or)
  alpha2    <- log(y_or)
  
  
  #missing a2 dependent on o2 and Y
  data_miss$logit <- intercept + alpha1*data_miss$o2 +alpha2*data_miss$Y
  data_miss$p     <- expit(data_miss$logit)
  
  data_miss$m_a2  <- rbinom(n_patients, 1, data_miss$p)
  
  data_miss$a2[data_miss$m_a2==1] <- NA
  
  
  #all participants with missing a2 will have missing Y
  data_miss$Y[is.na(data_miss$a2)]  <- NA
  
  data_miss$o2_a2[is.na(data_miss$a2)] <- NA
  data_miss$a1_a2[is.na(data_miss$a2)] <- NA
  
  
  return(data_miss)
}


