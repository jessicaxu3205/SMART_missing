# Function will generate the data from


expit <- function(x)
{
  exp(x)/(1 + exp(x))
}


# Generate the data for a single individual:

generate_patient <- function(delta = c(0.5, 0.5),
                             gamma = c(0, 0, 0, 0, 0, 0, 0))
{
  
  o1      <- rbinom(1, 1, 0.5)
  o1      <- 2*o1 - 1 # Converted to {-1,1} coding
  
  a1      <- rbinom(1, 1, 0.5)
  a1      <- 2*a1 - 1 # Converted to {-1,1} coding
  
  o2_prob <- expit(delta[1] * o1 + 
                     delta[2] * a1) 
  
  o2      <- rbinom(1, 1, o2_prob)
  
  #o2 responder variable, 1= responder 0= non-responder 
  
  #generate a2 when o2==1 responder & a1==-1 treatment A, p2% s2_treatment
  a2      <- 0
  a2_temp <- 0
  
  a2_temp[o2==1 & a1==-1] <- rbinom( 1,1,0.5)
  a2[a2_temp==1]          <- 1
  a2[a2_temp==0]          <- 0
  
  a2_temp[o2==1 & a1==-1] <- NA
  
  
  #generate a2 when o2==0 non-responder & a1==-1 treatment A, p2% s2_treatment 
  
  a2_temp[o2==0 & a1==-1] <- rbinom( 1,1,0.5)
  
  a2[a2_temp==1]          <- 1
  a2[a2_temp==0]          <- 0
  
  a2_temp[o2==0 & a1==-1] <- NA
  
  #generate a2 when o2==1 responder & a1==1 treatment B, p3% s2_treatment 
  
  a2_temp[o2==1 & a1==1]  <- rbinom( 1,1,0.5)
  
  a2[a2_temp==1]          <- 1
  a2[a2_temp==0]          <- 0
  
  a2_temp[o2==1 & a1==1]  <- NA
  
  
  #generate a2 when o2==0 non-responder & a1==1 treatment B, p3% s2_treatment 
  
  a2_temp[o2==0 & a1==1]  <- rbinom( 1,1,0.5)
  
  a2[a2_temp==1]          <- 1
  a2[a2_temp==0]          <- 0
  
  a2_temp[o2==0 & a1==1]  <- NA
  
  
  #a2 <- rbinom(1, 1, 0.5)
  
  a2 <- 2*a2 - 1 # Converted to {-1,1} coding
  o2 <- 2*o2 - 1 # Converted to {-1,1} coding

  
  # Design matrix.
  X <- c("Intercept" = 1, 
         "o1"    = o1, 
         "a1"    = a1, 
         "o1_a1" = o1*a1, 
         "a2"    = a2, 
         "o2_a2" = o2*a2, 
         "a1_a2" = a1*a2)
  
  # Mean vector.
  mu_y <- crossprod(X, gamma)
  
  # Observed vector.
  epsilon <- rnorm(1, 0, 1)
  Y <- mu_y + epsilon
  return(c("Y" = Y, X, "o2"= o2 ))
}



#Generate full trial

generate_trial <- function(n = 10, delta = c(0.5, 0.5),
                           gamma = c(0, 0, 0, 0, 0, 0, 0))
{
  t(replicate(n, generate_patient(delta, gamma)))
}



