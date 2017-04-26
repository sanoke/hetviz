# # # # # # # # # # # # # # # # # # # # # # # #
# complexData-medicareSim.R                   #
#                                             #
# script used to simulate "complex" data,     #
# structure following analytic dataset from   #
# CER analysis of Medicare data               # 
# # # # # # # # # # # # # # # # # # # # # # # #

expit <- function(x) { exp(x) / (1 + exp(x)) }

# CONSTANT: sample size 
n <- 54099  

# CONSTANT: number of digits to round our data to
roundDigits <- 4

# CONSTANT: the number of subgroups we want to partition our data into
numGrp <- 500


# - # - # - # - # 


# - STEP 1: Generate data
#   Below we generate the covariates that will comprise the final 
#   dataset. These covariates are simulated from parameters whose 
#   values were informed by real Medicare patient data. These patients 
#   were admitted to a hospital for their first myocardial infarction 
#   (AMI), and received a single type of stent (bare metal or drug-eluting)
#   as treatment. (Thus any use of the word prior means prior to this 
#   initial AMI.) These data are discussed in 
#   Anoke SC, Normand S-L, Zigler C (2017). 
#   "Approaches to treatment effect heterogeneity in the presence of confounding"
#   (submitted).

# four regions, one variable for each region in the continental US
Region          <- runif(n)
Regionsouth     <- as.numeric(Region <= 0.3985)
Regionmidwest   <- as.numeric(Region >  0.3985          & Region <= 0.3985 + 0.2870)
Regionwest      <- as.numeric(Region >  0.3985 + 0.2870 & Region <= 0.3985 + 0.2870 + 0.1500)
Regionnortheast <- as.numeric(Region >  0.3985 + 0.2870 + 0.1500)

# an indicator of race
# Race <- factor(rbinom(n, 1, 0.9020), level = 0:1, labels = c("nonwhite", "white"))
Race <- rbinom(n, 1, 0.9020)

# an indicator of simultaneous Medicaid eligibility, thus a proxy for socioeconomic status 
Dual <- rbinom(n, 1, 0.1252)

# an indicator of biological sex
# Male <- factor(rbinom(n, 1, 0.5742), level = 0:1, labels = c("female", "male"))
Male <- rbinom(n, 1, 0.5742)

# an indicator for prior depression diagnosis 
Depression <- rbinom(n, 1, 0.0486)

# an indicator for prior anemic diagnosis 
Anemias_Blood <- rbinom(n, 1, 0.1617)

# an indicator for prior dementia diagnosis
Dementia <- rbinom(n, 1, 0.0418)

# an indicator for prior hypertension diagnosis 
HTN <- rbinom(n, 1, 0.6775)

# an indicator for prior diabetes diagnosis, simulated to be correlated with hypertension 
#  (as is known clinically to be true)
Diabetes <- rep.int(NA, n)
Diabetes[ as.logical(HTN)] <- rbinom(sum( HTN), 1, 11887 / sum( HTN))
Diabetes[!as.logical(HTN)] <- rbinom(sum(!HTN), 1,  3450 / sum(!HTN))

# an indicator for prior pneumonia diagnosis
Pneumon <- rbinom(n, 1, 0.0747)

# an indicator for prior renal failure 
RenFail <- rbinom(n, 1, 0.0576)

# an indicator for a prior atherosclerosis diagnosis
Atherosc <- rbinom(n, 1, 0.8961)

# indicator for a prior congestive heart failure
Hx_CHF <- rbinom(n, 1, 0.0690)

# an indicator for a prior COPD diagnosis
COPD <- rbinom(n, 1, 0.1621)

# age in years 
minAge <- 65
sdAge  <- 7.3560
age    <- abs(rnorm(n)) * sdAge + minAge

covars  <- data.frame(age, COPD, Hx_CHF, Atherosc, HTN, RenFail,
                      Pneumon, Diabetes, Dementia, Anemias_Blood,
                      Depression, Male, Dual, Race, Regionwest,
                      Regionmidwest, Regionsouth)

# logistic regression coefficients for treatment mean
coeff.trtMean <- c( 1.6727, -0.0228, -0.0598,  0.0818,  0.1976,
                    0.1504, -0.0806, -0.1652,  0.1548, -0.3760,
                   -0.2156,  0.0094, -0.1270, -0.1455,  0.0449,
                    0.2893,  0.0366,  0.1512)
names(coeff.trtMean) <- c("Intercept", "age", "COPD", "Hx_CHF", "Atherosc",
                          "HTN", "RenFail", "Pneumon", "Diabetes", "Dementia",
                          "Anemias_Blood", "Depression", "Male", "Dual", 
                          "Race", "Regionwest", "Regionmidwest", "Regionsouth")
                          
trtMean   <- as.matrix(cbind(1, covars)) %*% coeff.trtMean

treatment <- rbinom(n, 1, expit(trtMean))

# logistic regression coefficients for outcome mean
coeff.outcomeMean <- c(-1.9641, -0.2627,  0.0111,  0.1758,  0.4215,
                       -0.1110, -0.1355,  0.4737,  0.3974,  0.2665,
                        0.1582,  0.1259,  0.0699,  0.1001,  0.1475,
                        0.0089, -0.0713,  0.0379, -0.0003)
names(coeff.outcomeMean) <- c( "Intercept",
                               "treatment",
                               names(coeff.trtMean)[2:length(coeff.trtMean)]) 

outcomeMean   <- as.matrix(cbind(1, treatment, covars)) %*% coeff.outcomeMean

outcome       <- rbinom(n, 1, expit(outcomeMean))

# putting together final data
syntheticData  <- data.frame(age, COPD, Hx_CHF, Atherosc, HTN, RenFail,
                             Pneumon, Diabetes, Dementia, Anemias_Blood,
                             Depression, Male, Dual, Race, Regionwest,
                             Regionmidwest, Regionsouth, treatment, outcome)



# - STEP 2: Estimate ITEs
#   We recommend using bart() from the BayesTree package.
#   Below we demonstrate how to estimate ITEs for a particular dataset.
# install.packages("BayesTree")
library(BayesTree)

# training data
xt <- cbind(syntheticData[,1:17], trt = syntheticData$treatment)

# data used to generate predictions
# (same as xt but with trt assnmt flipped)
trt.rev <- syntheticData$treatment + 1
trt.rev[trt.rev == 2] <- 0
xp <- cbind(syntheticData[,1:17], trt = trt.rev)

# coerce covariates into 'double' because BART breaks otherwise	
xt[1,] <- xt[1,]+1e-4
xp[1,] <- xp[1,]+1e-4

# estimating ITEs 
# note: this will take up to 12 hours to complete!
#       however an example of bartRes has been saved in the 
#       same directory as this script.
system.time({
	bartRes <- bart(x.train=xt, y.train=syntheticData$outcome, 
                x.test=xp, verbose=TRUE,
                ndpost=5000, keepevery=5)
})

# bartResB$yhat.train has a row for every draw from the posterior,
#   and a column for every person in the sample.
# bartResB$yhat.test has a row for every draw from the posterior predictive,
#   and a column for every person in the test data (so nrow(xp) columns)
# (the default is 1000 posterior draws)
diffs <- matrix(nrow=nrow(bartRes$yhat.train), ncol=ncol(bartRes$yhat.train))
diffs[,  as.logical(syntheticData$treatment) ] <- round(
                                                    pnorm(bartRes$yhat.train) - pnorm(bartRes$yhat.test), 
                                                    roundDigits)[, as.logical(syntheticData$treatment)]
diffs[, !as.logical(syntheticData$treatment) ] <- round(
                                                    pnorm(bartRes$yhat.test) - pnorm(bartRes$yhat.train), 
                                                    roundDigits)[, !as.logical(syntheticData$treatment)]                             
indivMMT <- apply(diffs, 2, mean)


# - STEP 3: assign each person to a stratum based on their posterior mean TE value
cutpts0 <- quantile(indivMMT, seq(0,1,1/numGrp))
cutpts0 <- sort(unique( round(cutpts0, roundDigits) ))
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
BARTgrp00 <- rep(NA, nrow(syntheticData))
numGrpCtr <- length(grps)
for(j in 1:length(lvls)) {
	# the group w/ the largest TE has the largest group #
	BARTgrp00[BARTgrp0 == lvls[j]] <- numGrpCtr
	numGrpCtr <- numGrpCtr - 1
}                          

# append the subgroup assignment to the dataset
syntheticData$estGrp   <- BARTgrp00

# append the estimated ITEs to the dataset
syntheticData$indivMMT <- indivMMT

# preview our additions
head(syntheticData)

# save the dataset 
write.csv(syntheticData, file = "syntheticData-userGen.csv",
                       quote = FALSE,
                       row.names = FALSE)