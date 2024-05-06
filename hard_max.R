# Hard max estimator  

# Original hard max estimator function from Chakraborty's simulation study
hard_max_only <- function(data)
{
  data <- as.data.frame(data)
  q2   <- lm(Y ~ o1 + a1 + o1_a1 + a2 + o2_a2 + a1_a2, data = data)
  
  # Yprimeprime (also called the 'blip' function, or 'tailoring' vars)
  pseudo_Y_blip <- (coef(q2)[5]*data[["a2"]] + 
                      coef(q2)[6]*data[["o2_a2"]] + 
                      coef(q2)[7]*data[["a1_a2"]])/data[["a2"]] 
  #Bibhas factors a2 out, with -1,1 coding it doesn't matter cause we take an absolute.
  
  # Yprime (also called the 'treatment free' function)
  pseudo_Y_free  <- coef(q2)[1] + 
    coef(q2)[2]*data[["o1"]] + 
    coef(q2)[3]*data[["a1"]] + 
    coef(q2)[4]*data[["o1_a1"]]
  
  pseudo_Y    <- pseudo_Y_free +  abs(pseudo_Y_blip) 
  data[["Y"]] <- pseudo_Y
  q1          <- lm(pseudo_Y ~ o1 + a1 + o1_a1, data = data)
  return(list("q1" = q1, 
              "q2" = q2))
}

# Hard max estimator function for multiple imputation

hard_max_only_mi <- function(data)
{
  data <- as.data.frame(data)
  
  #data$o1_a1<-data$o1*data$a1
  #data$a1_a2<-data$a1*data$a2
  #data$o2_a2<-data$o2*data$a2
  
  q2   <- lm(Y ~ o1 + a1 + o1_a1 + a2 + o2_a2 + a1_a2, data = data)
  
  # Yprimeprime (also called the 'blip' function, or 'tailoring' vars)
  pseudo_Y_blip <- (coef(q2)[5]*data[["a2"]] + 
                      coef(q2)[6]*data[["o2_a2"]] + 
                      coef(q2)[7]*data[["a1_a2"]])/data[["a2"]] 
  #Bibhas factors a2 out, with -1,1 coding it doesn't matter cause we take an absolute.
  
  # Yprime (also called the 'treatment free' function)
  pseudo_Y_free  <- coef(q2)[1] + 
    coef(q2)[2]*data[["o1"]] + 
    coef(q2)[3]*data[["a1"]] + 
    coef(q2)[4]*data[["o1_a1"]]
  
  pseudo_Y    <- pseudo_Y_free +  abs(pseudo_Y_blip) 
  data[["Y"]] <- pseudo_Y
  q1          <- lm(pseudo_Y ~ o1 + a1 + o1_a1, data = data)
  return(q1)
}


