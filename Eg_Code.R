# Load the DynTxRegime package
library(DynTxRegime)

# Load the bmiData dataset
data("bmiData")

# Take a peek at the bmiData
head(bmiData, 5)

bmiData <- dplyr::mutate(bmiData, 
     pctBMIchange = -100 * (month12BMI - baselineBMI)/baselineBMI)

# Fit the second stage regression model (Q2)
stage2mod <- lm(pctBMIchange ~ gender + race + parentBMI + 
                  baselineBMI + A1 + month4BMI + A2 + 
                  A2:baselineBMI + A2:month4BMI + A2:parentBMI + 
                  A2:A1, data = bmiData)

# Fitted values when A2 == "MR"
stage2MRfitted <- predict(stage2mod, 
                          newdata = dplyr::mutate(bmiData, A2 = "MR"))

# Fitted values when A2 == "CD"
stage2CDfitted <- predict(stage2mod, 
                          newdata = dplyr::mutate(bmiData, A2 = "CD"))

# Optimal Stage 2 treatment recommendations
d2opt <- dplyr::if_else(stage2MRfitted > stage2CDfitted, 
                        "MR", "CD")

# Allocation of Stage 2 treatments under the optimal rule
table(d2opt)

# Compute yhat
yhat <- dplyr::if_else(stage2MRfitted > stage2CDfitted, 
                       stage2MRfitted, stage2CDfitted)

# Append yhat to bmiData
bmiData <- cbind(bmiData, yhat)

# Fit the first stage regression model (Q1)
stage1mod <- lm(yhat ~ gender + race + parentBMI + baselineBMI + 
                  A1 + A1:baselineBMI + A1:parentBMI, 
                data = bmiData)

# Fitted values when A1 == "MR"
stage1MRfitted <- predict(stage1mod, 
                          newdata = dplyr::mutate(bmiData, A1 = "MR"))

# Fitted values when A1 == "CD"
stage1CDfitted <- predict(stage1mod, 
                          newdata = dplyr::mutate(bmiData, A1 = "CD"))

# Optimal Stage 1 treatment recommendations
d1opt <- dplyr::if_else(stage1MRfitted > stage1CDfitted, 
                        "MR", "CD")

# Allocation of Stage 1 treatments under the optimal rule
table(d1opt)

# Q-learning to learn the optimal dynamic treatment regime 
# - using DynTxRegime package
y12 <- -100*(bmiData[,6L] - bmiData[,4L])/bmiData[,4L]
moMain <- buildModelObj(model = ~parentBMI + month4BMI + gender + race + 
                          baselineBMI + A1, solver.method = 'lm')
moCont <- buildModelObj(model = ~baselineBMI + parentBMI + month4BMI + A1,
                        solver.method = 'lm')
fitSS <- qLearn(moMain = moMain, moCont = moCont,
                data = bmiData, response = y12,  txName = 'A2')
moMain <- buildModelObj(model = ~parentBMI + baselineBMI + race + gender,
                        solver.method = 'lm')
moCont <- buildModelObj(model = ~parentBMI + baselineBMI,
                        solver.method = 'lm')
fitFS <- qLearn(moMain = moMain, moCont = moCont,
                data = bmiData, response = fitSS,  txName = 'A1')
