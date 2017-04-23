expit <- function(x) { exp(x) / (1 + exp(x)) }
logit <- function(x) { log( x / (1 - x) ) }
  
datagen <- function(n,                    # sample size
                    effMod = TRUE,        # is effect modification present?
                    confound = FALSE,     # is confounding present?
                    confoundEMs = FALSE,  # is confounding by EMs present?                    
                    alpha = NULL,         # optional: user can specify their own coeff
                    beta = NULL) {        # optional: user can specify their own coeff
                    	
    gamma1 <- 5          # main effect (association of T with Y)
    varOutcome <- 1      # outcome variance
    		
	# determine the parameter values (if not set already)
	if(is.null(alpha) & is.null(beta)) {
		alpha1  <-  0.1 # weak.T
		alpha2  <- -0.1 # weak.T
		alpha3  <-  1.1 # strong.T
		alpha4  <- -1.1 # strong.T
		alpha7  <-  0.4 # moderate.T
		alpha8  <- -0.1 # weak.T
		alpha9  <-  1.1 # strong.T
		alpha10 <- -4   # strongg.T
		
		beta0  <- -3.85
		beta1  <-  0.5  # weak.Y
		beta2  <- -2    # strong.Y
		beta3  <- -0.5  # weak.Y
		beta4  <-  2    # strong.Y
		beta5  <-  1    # moderate.Y
		beta6  <- -1    # moderate.Y
		beta7  <-  0
		beta8  <- -2    # strong.Y
		beta9  <-  1    # moderate.Y
		beta10 <-  4    # strongg.Y
		beta11 <- -4    # strongg.Y
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
	
	Z1 <- rbinom(n, 1, 0.5)
	Z2 <- rbinom(n, 1, 0.5)
	Z3 <- rbinom(n, 1, 0.5)
	
	allCovars <- cbind(baseCovars, Z1, Z2, Z3)
	colnames(allCovars) <- c( paste0("X",1:6), paste0("E", 1:3) )

	pTreat <- expit( cbind(1, allCovars) %*% alpha )
	trt <- rbinom(n, 1, pTreat)
	
	# intercept, treatment, interaction terms
	Y0mean <- cbind(1, 0, allCovars, 0*allCovars[,"E1"], 0*allCovars[,"E2"], 0*allCovars[,"E3"]) %*% beta
    Y0     <- rnorm(n, Y0mean, sqrt(varOutcome))
	Y1mean <- cbind(1, 1, allCovars, 1*allCovars[,"E1"], 1*allCovars[,"E2"], 1*allCovars[,"E3"]) %*% beta
	Y1     <- rnorm(n, Y1mean, sqrt(varOutcome))
	
	Y <- ifelse(trt, Y1, Y0) # observed outcome
	
	trueGrp <- round(Y1mean - Y0mean) # treatment effect group
	
	ds <- data.frame(trt, Y, allCovars, Y0, Y1, trueGrp)
	
	return( ds )
}

forestPlotData <- function(ds) {
	
	grpNum <- sort(unique(ds$estGrp))
	
	plotDataNames <- c("grp", paste0("q", c("025", "25", "50", "75", "975")))
	
	plotData <- matrix(nrow=length(grpNum), ncol=length(plotDataNames))
	colnames(plotData) <- plotDataNames
	plotData <- as.data.frame(plotData)
	plotData$grp <- factor(paste0("Stratum ",1:length(grpNum)), 
	                         levels=paste0("Stratum ",1:length(grpNum)))
	
	for(k in 1:length(grpNum)) {
		subgrp <- ds$mmt[ ds$estGrp == k ]
		plotData[k, c("q025", "q25", "q50", "q75", "q975")] <- 
		                 quantile( subgrp , probs=c(0.025, 0.25, 0.50, 0.75, 0.975))			
	}
	
	return(plotData)
	
}
