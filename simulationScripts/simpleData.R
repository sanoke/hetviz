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
