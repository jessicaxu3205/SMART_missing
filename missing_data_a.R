#--Missing example 1-------------------------------------
# Data MCAR at both stages with 20%

missing.mcar.s1s2 <- function (data, missing.percentage = 0.2)
{
  
  data_miss <- as.data.frame(data)
  
  #randomly deleting % data; univariate
  
  #o2 MCAR 
  data_miss$m_o2 <- rbinom(n_patients, 1, missing.percentage) 
  data_miss$o2[data_miss$m_o2==1] <- NA
  
  data_miss$m_y <- rbinom(n_patients, 1, missing.percentage) 
  data_miss$Y[data_miss$m_y==1] <- NA
  
  data_miss$o2_a2[is.na(data_miss$o2)] <- NA
  
  
  return(data_miss)
}


#--Missing example 2-------------------------------------
# Stage 1 responder status MCAR and stage 2 outcome missing dependent on A2 
missing.mar.s2 <- function (data, missing.percentage = 0.2, 
                            intercept= -1.8 ,
                            a2_or = 1.6)
{
  data_miss <- as.data.frame(data)
  
  alpha1    <- log(a2_or)
  
  #o2 MCAR 
  data_miss$m_o2 <- rbinom(n_patients, 1, missing.percentage) 
  data_miss$o2[data_miss$m_o2==1] <- NA
  
  #all participants with missing o2 will have missing a2 and also missing Y
  data_miss$a2[is.na(data_miss$o2)] <- NA
  data_miss$Y[is.na(data_miss$a2)]  <- NA
  
  #in addition missing y dependent on a2
  data_miss$logit <- intercept + alpha1*data_miss$a2
  data_miss$p     <- expit(data_miss$logit)
  
  data_miss$m_y  <- rbinom(n_patients, 1, data_miss$p)
  data_miss$m_y[data_miss$m_o2==1]  <- 1
  data_miss$Y[data_miss$m_y==1] <- NA
  
  data_miss$o2_a2[is.na(data_miss$o2)] <- NA
  data_miss$a1_a2[is.na(data_miss$a2)] <- NA
  
  
  return(data_miss)
}


#--Missing example 3-------------------------------------
# Data missing due to drop out in stage1 and therefore missing Y
#Default around 20% missingness in o2

missing.drops1 <- function (data, intercept= -1.8 ,
                            o1_or = 1.6,
                            a1_or = 1.6)
{
  data_miss <- as.data.frame(data)
  
  alpha1    <- log(o1_or)
  alpha2    <- log(a1_or)
  
  #missing o2 dependent on o1 and a1
  data_miss$logit <- intercept + alpha1*data_miss$o1 + alpha2*data_miss$a1
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



#--Missing example 4-------------------------------------
#stage 1 treatment responders more likely to drop out before stage 2 
# o2 -> Ma2 -> My
# Default around 20% missingness in a2

missing.responder.drops2 <- function (data, intercept= -1.8 ,
                                      o2_or = 1.6)
{
  data_miss <- as.data.frame(data)
  
  alpha1    <- log(o2_or)
  
  #missing a2 dependent on o2
  data_miss$logit <- intercept + alpha1*data_miss$o2
  data_miss$p     <- expit(data_miss$logit)
  
  data_miss$m_a2  <- rbinom(n_patients, 1, data_miss$p)
  
  data_miss$a2[data_miss$m_a2==1] <- NA
  
  
  #all participants with missing a2 will have missing Y
  data_miss$Y[is.na(data_miss$a2)]  <- NA
  
  
  data_miss$o2_a2[is.na(data_miss$a2)] <- NA
  data_miss$a1_a2[is.na(data_miss$a2)] <- NA
  
  
  return(data_miss)
}



