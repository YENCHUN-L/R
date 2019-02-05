#------------------------------------------------------------------------------------#
# EVALUATION OF THE MODEL USING LIFT CURVE, CUMULATIVE GAINS and CUMULATIVE RESPONSE
#------------------------------------------------------------------------------------#
#######################################################################################
# EVALUATING THE RESULTS OF LOGISTIC REGRESSION MODEL BUILT USING MIXED SAMPLING 
######################################################################################
if(!require("pROC")) install.packages("pROC"); library("pROC")

#-----------------------------------------------------#
# CUMULATIVE GAINS CURVE
#-----------------------------------------------------#
pred <- prediction(predicttest,basetable_test$target)
# Calculate the necessary data
perf <- performance(pred,"tpr","rpp")
# Plot the cumulative gains curve:
plot(perf, main="cumulative gains", col="blue")


pred <- prediction(predicttrain,basetable_train$target)
# Calculate the necessary data
perf <- performance(pred,"tpr","rpp")
par(new=TRUE)
# Plot the cumulative gains curve:
plot(perf, main="cumulative gains", col="red")
abline(a=0, b=1, lty=1)


#---------------------------------------------------#
#                    LIFT CURVE
#---------------------------------------------------#
calculate_lift <- function(predicted, reference, val){
  
  if (is.factor(reference)) 
    reference <- as.integer(as.character(reference))
  
  lift_val <- data.frame(predicted, reference)
  lift_val <- lift_val[order(-lift_val[, 1]), ]
  lift_val <- lift_val[1:floor(nrow(lift_val)*val/100),]
  lift_val$predicted <- ifelse(lift_val$predicted > 0.5,1,0)
  lift_val$result <- ifelse(lift_val$predicted == lift_val$reference & lift_val$predicted == 1, 1,0)
  
  res <- as.numeric(mean(lift_val$result)/mean(reference))
  return(res)
}

# Plotting lift rates for each decile
predicted <- as.numeric(predicttest)
reference <- basetable_test$target
lifts <- c()
for (x in seq(0:10)){
  lift <- calculate_lift(predicted, reference, x*10)
  lifts <- c(lifts, lift)}
plot(seq(10, 110, by=10), lifts, type='l', col="green", lwd=2, ylab="Lift rate", xlab="Top-probability decile", main="Lift curve")
points(seq(10, 110, by=10), lifts)
abline(a=0, b=1, lty=1)



#-------------------------#
# BUILDING A BUSINESS CASE
#-------------------------#

# Defining useful variables
totpop <- nrow(basetable_test)
posrate <- mean(basetable_test$target)
num_targets <- sum(basetable_test$target)

# Cost per customer reached-out to
cost <- .5

# Get the expected donation amount for a giver (taking the mean donation of donors for 2013)
target13 <- read_delim(paste0(path, "campaign20130411.csv"), ";", escape_double = FALSE, trim_ws = TRUE)
target13 <- target13 %>% group_by(donorID) %>% summarise(target = sum(amount))
target13 <- target13[target13$target > 35,]
estimated_gain <- mean(target13$target)

# Plot expected profits for a cost of 0.5 per targeted customer
# Based on the lift ratio computed, and an expected value of 35 per customer won over
profits_lift <- c()
for (i in seq(1:10)){
  decided_rate <- i / 10
  lift_rate <- lifts[i]
  prof <- round(totpop*decided_rate*lift_rate*posrate*estimated_gain - decided_rate*totpop*cost,0)
  profits_lift <- c(profits_lift, prof)}
plot(x=seq(1,10), y=profits_lift, ylab="Expected profits", xlab="Population targeted", type="l", 
     lty=1, col="orange", lwd=2, main="")
points(x=seq(1,10), y=profits_lift)
text(x=seq(1,10), y=profits_lift, labels=profits_lift, pos=1)
