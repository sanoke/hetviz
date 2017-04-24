# # # # # # # # # # # # # # # # # # # # # # # #
# simpleData.R                                #
# script used to simulate "simple" data       #
# # # # # # # # # # # # # # # # # # # # # # # #

library(MASS) # needed to simulate from a multivariate Normal

expit <- function(x) { exp(x) / (1 + exp(x)) }
logit <- function(x) { log( x / (1 - x) ) }
  
datagen <- function(n,                    # sample size
                    effMod = TRUE,        # is effect modification present?
                    confound = FALSE,     # is confounding present?
                    confoundEMs = FALSE,  # is confounding by EMs present?                    
                    alpha = NULL,         # optional: user can specify their own coeff
                    beta = NULL,          # optional: user can specify their own coeff
                    varOutcome = 1) {     # outcome variance
    
  if(!is.null(alpha) & length(alpha) != 9) {
    stop("The alpha vector is not the correct length; please review.")
  }
  
  if(!is.null(beta) & length(beta) != 13) {
    stop("The beta vector is not the correct length; please review.")
  }  
        
  # determine the parameter values (if not set already)
  if(is.null(alpha)) {
    alpha0  <-  0   # intercept
    alpha1  <-  0.1 # X1
    alpha2  <- -0.1 # X2
    alpha3  <-  1.1 # X3
    alpha4  <- -1.1 # X4
    alpha7  <-  0.4 # X5
    alpha8  <- -0.1 # E1
    alpha9  <-  1.1 # E2
    alpha10 <- -4   # E3
  } else {
    alpha0  <-  alpha[1] # intercept
    alpha1  <-  alpha[2] # X1
    alpha2  <-  alpha[3] # X2
    alpha3  <-  alpha[4] # X3
    alpha4  <-  alpha[5] # X4
    alpha7  <-  alpha[6] # X5
    alpha8  <-  alpha[7] # E1
    alpha9  <-  alpha[8] # E2
    alpha10 <-  alpha[9] # E3
  }
    
  # determine the parameter values (if not set already)
  if(is.null(beta)) {
    beta0  <- -3.85 # intercept
    gamma1 <-  5    # trt
    beta1  <-  0.5  # X1
    beta2  <- -2    # X2
    beta3  <- -0.5  # X3
    beta4  <-  2    # X4
    beta5  <-  1    # X6
    beta6  <- -1    # E1
    beta7  <-  0    # E2
    beta8  <- -2    # E3
    beta9  <-  1    # T*E1
    beta10 <-  4    # T*E2
    beta11 <- -4    # T*E3
  } else {
    beta0  <- beta[1]  # intercept
    gamma1 <- beta[2]  # trt
    beta1  <- beta[3]  # X1
    beta2  <- beta[4]  # X2
    beta3  <- beta[5]  # X3
    beta4  <- beta[6]  # X4
    beta5  <- beta[7]  # X6
    beta6  <- beta[8]  # E1
    beta7  <- beta[9]  # E2
    beta8  <- beta[10] # E3
    beta9  <- beta[11] # T*E1
    beta10 <- beta[12] # T*E2
    beta11 <- beta[13] # T*E3
  }
    
  
  # determine the simulation scenario
  if( effMod ) {
    if( confound ) {
      # effect modification + confounding
      scenario <- "C"
      # effect modification + confounding (including by EMs)
      if ( confoundEMs ) scenario <- "D"
    } else {
      # effect modification + no confounding
      scenario <- "B"
    }
  } else {
    if( confound ) {
      # no effect modification + confounding
      scenario <- "A"
    } else {
      return(NULL)
    }
  }
  
  # set the parameter values
  if(scenario == "A") { 
    # confounding + no effect modification
    
    alpha <- numeric(10)
    names(alpha) <- c("constant", paste0("X",1:6), paste0("Z",1:3))
    alpha["constant"] <- alpha0
    alpha["X1"] <- alpha1
    alpha["X2"] <- alpha2
    alpha["X3"] <- alpha3
    alpha["X4"] <- alpha4
    alpha["X5"] <- alpha7
    
    beta  <- numeric(14)
    names(beta) <- c("constant", "T", paste0("X",1:6),  
                     paste0("Z",1:3), "TZ1", "TZ2", "TZ3")
    beta["constant"] <- beta0
    beta["T"]    <- gamma1 
    beta["X1"]   <- beta1
    beta["X2"]   <- beta2
    beta["X3"]   <- beta3
    beta["X4"]   <- beta4
    beta["X6"]   <- beta5
    
  } else if(scenario == "B") { 
    # effect modification + no confounding
    
    alpha <- numeric(10)
    names(alpha) <- c("constant", paste0("X",1:6), paste0("Z",1:3))
    alpha["constant"] <- alpha0
    alpha["X5"] <- alpha7
  
    beta  <- numeric(14)
    names(beta) <- c("constant", "T", paste0("X",1:6),  
                     paste0("Z",1:3), "TZ1", "TZ2", "TZ3")
    beta["constant"] <- beta0
    beta["T"]    <- gamma1 
    beta["X6"]   <- beta5
    beta["Z1"]   <- beta6
    beta["Z2"]   <- beta7
    beta["Z3"]   <- beta8
    beta["TZ1"]  <- beta9
    beta["TZ2"]  <- beta10
    beta["TZ3"]  <- beta11 
  } else if(scenario == "C") {
    # effect modification + confounding (not by EMs)
    
    alpha <- numeric(10)
    names(alpha) <- c("constant", paste0("X",1:6), paste0("Z",1:3))
    alpha["constant"] <- alpha0
    alpha["X1"] <- alpha1
    alpha["X2"] <- alpha2
    alpha["X3"] <- alpha3
    alpha["X4"] <- alpha4
    alpha["X5"] <- alpha7
    
    beta  <- numeric(14)
    names(beta) <- c("constant", "T", paste0("X",1:6), 
                     paste0("Z",1:3), "TZ1", "TZ2", "TZ3")
    beta["constant"] <- beta0
    beta["T"]    <- gamma1 
    beta["X1"]   <- beta1
    beta["X2"]   <- beta2
    beta["X3"]   <- beta3
    beta["X4"]   <- beta4
    beta["X6"]   <- beta5
    beta["Z1"]   <- beta6
    beta["Z2"]   <- beta7
    beta["Z3"]   <- beta8
    beta["TZ1"]  <- beta9
    beta["TZ2"]  <- beta10
    beta["TZ3"]  <- beta11
  } else if(scenario == "D") {
    # effect modification + confounding (EMs assoc w trt)
    alpha <- numeric(10)
    names(alpha) <- c("constant", paste0("X",1:6), paste0("Z",1:3))
    alpha["constant"] <- alpha0
    alpha["X1"] <- alpha1
    alpha["X2"] <- alpha2
    alpha["X3"] <- alpha3
    alpha["X4"] <- alpha4
    alpha["X5"] <- alpha7
    alpha["Z1"] <- alpha8
    alpha["Z2"] <- alpha9
    alpha["Z3"] <- alpha10
    
    beta  <- numeric(14)
    names(beta) <- c("constant", "T", paste0("X",1:6),  
                     paste0("Z",1:3), "TZ1", "TZ2", "TZ3")
    beta["constant"] <- beta0
    beta["T"]    <- gamma1 
    beta["X1"]   <- beta1
    beta["X2"]   <- beta2
    beta["X3"]   <- beta3
    beta["X4"]   <- beta4
    beta["X6"]   <- beta5
    beta["Z1"]   <- beta6
    beta["Z2"]   <- beta7
    beta["Z3"]   <- beta8
    beta["TZ1"]  <- beta9
    beta["TZ2"]  <- beta10
    beta["TZ3"]  <- beta11
  } 
  
  baseCovars <- mvrnorm(n, numeric(6), diag(6))
  
  E1 <- rbinom(n, 1, 0.5)
  E2 <- rbinom(n, 1, 0.5)
  E3 <- rbinom(n, 1, 0.5)
  
  allCovars <- cbind(baseCovars, E1, E2, E3)
  colnames(allCovars) <- c( paste0("X",1:6), paste0("E", 1:3) )

  pTreat <- expit( cbind(1, allCovars) %*% alpha )
  trt <- rbinom(n, 1, pTreat)
  
  # generating potential outcomes
  Y0mean <- cbind(1, 0, allCovars, 0*allCovars[,"E1"], 0*allCovars[,"E2"], 0*allCovars[,"E3"]) %*% beta
  Y0     <- rnorm(n, Y0mean, sqrt(varOutcome))
  Y1mean <- cbind(1, 1, allCovars, 1*allCovars[,"E1"], 1*allCovars[,"E2"], 1*allCovars[,"E3"]) %*% beta
  Y1     <- rnorm(n, Y1mean, sqrt(varOutcome))
  
  Y <- ifelse(trt, Y1, Y0) # observed outcome
  
  trueGrp <- round(Y1mean - Y0mean) # treatment effect group
  
  ds <- data.frame(trt, Y, allCovars, Y0, Y1, trueGrp)
  
  return( ds )
}

# - # - # - # - # 

# - STEP 1: Generate data
simpleDataA <- datagen(n = 1500, effMod = FALSE, confound = TRUE,  confoundEMs = FALSE)
simpleDataB <- datagen(n = 1500, effMod = TRUE,  confound = FALSE, confoundEMs = FALSE)
simpleDataC <- datagen(n = 1500, effMod = TRUE,  confound = TRUE,  confoundEMs = FALSE)
simpleDataD <- datagen(n = 1500, effMod = TRUE,  confound = TRUE,  confoundEMs = TRUE)



# - STEP 2: Estimate ITEs
#   We recommend using bart() from the BayesTree package.
#   Below we demonstrate how to estimate ITEs for a particular dataset.
# install.packages("BayesTree")
library(BayesTree)

# CONSTANT: the number of subgroups we want to partition our data into
numGrp <- 10 
# CONSTANT: number of digits to round our data to
roundDigits <- 3

# training data
varsExcl <- match(c("Y", "Y0", "Y1", "trueGrp"), names(simpleDataB), nomatch=0)
xt <- as.matrix(simpleDataB[,-varsExcl])

# data used to generate predictions
# (same as xt but with trt assnmt flipped)
trt.rev <- simpleDataB$trt + 1
trt.rev[trt.rev == 2] <- 0
varsExcl <- match(c("Y", "Y0", "Y1", "trueGrp", "trt"), names(simpleDataB), nomatch=0)
xp <- as.matrix(cbind(trt.rev, simpleDataB[,-varsExcl]))

# estimating ITEs 
# note: this will take a few minutes to complete
bartResB <- bart(x.train=xt, y.train=simpleDataB$Y, x.test=xp, verbose=FALSE)

# bartResB$yhat.train has a row for every draw from the posterior,
#   and a column for every person in the sample.
# bartResB$yhat.test has a row for every draw from the posterior predictive,
#   and a column for every person in the test data (so nrow(xp) columns)
# (the default is 1000 posterior draws)
diffs <- matrix(nrow=nrow(bartResB$yhat.train), ncol=ncol(bartResB$yhat.train))
diffs[,  as.logical(simpleDataB$trt) ] <- round(bartResB$yhat.train - bartResB$yhat.test, 
                                                roundDigits)[, as.logical(simpleDataB$trt)]
diffs[, !as.logical(simpleDataB$trt) ] <- round(bartResB$yhat.test - bartResB$yhat.train, 
                                                roundDigits)[, !as.logical(simpleDataB$trt)]                             
indivMMT <- apply(diffs, 2, mean)



# - STEP 3: assign each person to a stratum based on their posterior mean TE value
cutpts0 <- quantile(indivMMT, seq(0,1,1/numGrp))
cutpts0 <- sort(unique( round(cutpts0,roundDigits) ))
cutpts0 <- c(-Inf, cutpts0, Inf)
BARTgrp0 <- as.numeric(cut( indivMMT , breaks = cutpts0, 
                            labels = 1:(length(cutpts0)-1) ))
                            
# calculate the treatment effect in each subgroup
grps <- unique(BARTgrp0)
TE.BART <- rep(NA, length(grps))
for(j in 1:length(grps)) {
	TE.BART[j] <- mean( indivMMT[ BARTgrp0 == grps[j] ] )
}

# reorder the group assignments, so the group w the largest TE
#   has the largest group #
lvls <- grps[order(TE.BART, decreasing=TRUE, na.last=FALSE)]
BARTgrp00 <- rep(NA, nrow(simpleDataB))
numGrpCtr <- length(grps)
for(j in 1:length(lvls)) {
	# the group w/ the largest TE has the largest group #
	BARTgrp00[BARTgrp0 == lvls[j]] <- numGrpCtr
	numGrpCtr <- numGrpCtr - 1
}

# append the subgroup assignment to the dataset
simpleDataB$estGrp <- BARTgrp00

# append the estimated ITEs to the dataset
simpleDataB$indivMMT <- indivMMT

# preview our additions
round( head(simpleDataB) , 3)
#   trt      Y     X1     X2     X3     X4     X5     X6 E1 E2 E3     Y0     Y1 trueGrp estGrp indivMMT
# 1   1 -2.165 -0.354 -0.008 -2.193  0.934  0.497  0.834  0  0  1 -5.081 -2.165       1      3    1.180
# 2   0 -3.731 -1.161  1.063 -0.013 -1.519  0.822  1.346  1  0  0 -3.731  1.898       6      6    5.726
# 3   1  1.354 -1.403 -0.889 -0.571  0.894  0.167  0.016  0  1  1 -6.585  1.354       5      6    5.192
# 4   0  0.335 -3.131 -0.979 -0.049 -0.414 -0.840  1.801  0  1  0  0.335  7.216       9      9    8.856
# 5   1  1.502  0.043 -1.304 -0.416  0.138  0.051 -0.214  0  0  0 -3.483  1.502       5      4    4.831
# 6   1  2.465  1.306  1.619 -1.778  1.400  0.546 -1.185  1  1  0 -6.798  2.465      10     11    9.858

# save the dataset 
write.csv(simpleDataB, file = "simpleDataB-userGen.csv",
                       quote = FALSE,
                       row.names = FALSE)