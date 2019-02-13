if(!require("pROC")) install.packages("pROC"); library("pROC")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("caret")) install.packages("caret", dependencies = T); library("caret")
if(!require("randomForest")) install.packages("randomForest"); library("randomForest")
if(!require("ROSE")) install.packages("ROSE"); library("ROSE")
if(!require("BiocManager")) install.packages("BiocManager"); library("BiocManager")
#BiocManager::install("Biobase", version = "3.8")
#---------------------------------------------------------------------------#
#BUILD STEP WISE LOGISTIC REGRESSION MODEL
#---------------------------------------------------------------------------#


# Read the data:
data = basetable_train
# Inspect the data:
# Show the first lines
head(data)
# Show the fields
names(data)
# Show the number of targets
table(data$target)


# Custom function to calculate AUC: 
auc = function(trueval, predval){
  df = as.data.frame(cbind(trueval,predval))
  names(df) = c("trueval","predval")
  auc = roc(trueval~predval,data=df)$auc
  return(auc)
}

#FILTER (PEARSON CORRELATION)

# For each variable, calculate the pearson correlation between the variable and churn, and the corresponding p-value
# Calculate train and test AUC of this model


# STEPWISE LOGISTIC REGRESSION

# All possible variables:
variables = names(basetable_train)[-10]
variablesorder = c()

# Construct a logistic regression model with no variables
model = glm(target ~ 1,data=basetable_train,family=binomial)

# Construct a formula with all the variables
formula<-formula(paste("target","~",paste(variables,collapse="+")))

# Stepwise procedure
for(i in c(1:length(variables))){
  #calculate AIC of each model
  info = add1(model,scope=formula,data=basetable_train)
  print(info)
  #get variable with lowest AIC
  orderedvariables = rownames(info[order(info$AIC),])
  v = orderedvariables[orderedvariables!="<none>"][1]
  #add variable to formula
  variablesorder = append(variablesorder,v)
  formulanew = formula(paste("target","~",paste(variablesorder,collapse = "+")))
  model = glm(formulanew,data=basetable_train,family=binomial)
  print(v)
}

# Compute AUC for each model size
auctrain = rep(0,length(variablesorder)-1)
auctest = rep(0,length(variablesorder)-1)
for(i in c(1:(length(variablesorder)-1))){
  vars = variablesorder[0:i+1]
  print(vars)
  formula<-paste("target","~",paste(vars,collapse="+"))
  model<-glm(formula,data=basetable_train,family="binomial")	
  predicttrain<-predict(model,newdata=basetable_train,type="response")
  predicttest<-predict(model,newdata=basetable_test,type="response")
  auctrain[i] = auc(basetable_train$target,predicttrain)
  auctest[i] = auc(basetable_test$target,predicttest)
} 

# Plot AUCs according to number of variables
plot(auctrain, main="AUC", col="red",ylim=c(0.5, 0.7), type="l")
par(new=TRUE)
lines(auctest,col="blue",ylim=c(0.5, 0.7))
points(auctrain)
points(auctest)

#Select the model with optimal number of variables:
finalvariables = variablesorder[c(0:9)]
formula<-paste("target","~",paste(finalvariables,collapse="+"))
model<-glm(formula,data=basetable_train,family="binomial")	
predicttrain<-predict(model,newdata=basetable_train,type="response")
predicttest<-predict(model,newdata=basetable_test,type="response")
auctrain = auc(basetable_train$target,predicttrain)
auctest = auc(basetable_test$target,predicttest)

auctrain
auctest
# AUC TRAIN IS 62.31
# AUC TEST IS 60.35
summary(model)

#BUILD THE CONFUSION MATRIX TO CHECK SPECIFICITY and sensitivity CUT OFF 0.5
predicted <- as.numeric(predicttest>0.5)
reference <- basetable_test$target
u <- union(predicted, reference)
xtab <- table(factor(predicted, u), factor(reference, u))
confusionMatrix(xtab)

# THERE IS ZERO PREDICTION


#---------------------------------------------------------------------#
# BUILD RANDOM FOREST MODEL
#---------------------------------------------------------------------#
basetable_train2 <- basetable_train

rForest <- randomForest(target ~ .,data = basetable_train2, ntree=200)

predictions_test <- predict(object = rForest, newdata = basetable_test, type = "class")
predictions_train <- predict(object = rForest, newdata = basetable_train2, type = "class")

auc(basetable_train2$target,predictions_train)
auc(basetable_test$target,predictions_test)

#AUC TRAIN 99.8
#AUC TEST 58.8

#BUILD THE CONFUSION MATRIX TO CHECK SPECIFICITY and sensitivity WITH CUT OFF 0.5
predicted_rdmforest <- as.numeric(predictions_test>0.5)
reference_rdmforest <- basetable_test$target
u <- union(predicted, reference)
xtab_rdmforest <- table(factor(predicted_rdmforest, u), factor(reference_rdmforest, u))
confusionMatrix(xtab_rdmforest)

# STILL THE PREDICTION IS 0



#---------------------------------------------------------#
# PERFORM  MIXED SAMPLING ON TRAIN DATASET
#---------------------------------------------------------#
# cheking for proportion of 0s and 1s in the target
table(basetable_train$target)
# number of 0 <- 34365
# number of 1 <- 518 

# Perform mixed sampling
data_balanced_both <- ovun.sample(target ~ ., data = basetable_train, method = "both", p=0.5,N=34883,seed=1)$data

# cheking for proportion of 0s and 1s in the target after mixed sampling
table(data_balanced_both$target)
# number of 0 <- 17516
# number of 1 <- 17516 

#---------------------------------------------------------#
# USE MIXED SAMPLING DATSET TO BUILD LOGISTIC REGRESSION
#---------------------------------------------------------#

# Read the data:
data = data_balanced_both
# Inspect the data:
# Show the first lines
head(data)
# Show the fields
names(data)
# Show the number of targets
table(data$target)


# Custom function to calculate AUC: 
auc = function(trueval, predval){
  df = as.data.frame(cbind(trueval,predval))
  names(df) = c("trueval","predval")
  auc = roc(trueval~predval,data=df)$auc
  return(auc)
}

#FILTER (PEARSON CORRELATION)

# For each variable, calculate the pearson correlation between the variable and churn, and the corresponding p-value
# Construct a model on train data that has only those variables that have a p-value lower than 0.001
# Calculate train and test AUC of this model

# STEPWISE LOGISTIC REGRESSION

# All possible variables:
variables = names(data_balanced_both)[-10]
variablesorder = c()

# Construct a logistic regression model with no variables
model = glm(target ~ 1,data=data_balanced_both,family=binomial)

# Construct a formula with all the variables
formula<-formula(paste("target","~",paste(variables,collapse="+")))

# Stepwise procedure
for(i in c(1:length(variables))){
  #calculate AIC of each model
  info = add1(model,scope=formula,data=data_balanced_both)
  print(info)
  #get variable with lowest AIC
  orderedvariables = rownames(info[order(info$AIC),])
  v = orderedvariables[orderedvariables!="<none>"][1]
  #add variable to formula
  variablesorder = append(variablesorder,v)
  formulanew = formula(paste("target","~",paste(variablesorder,collapse = "+")))
  model = glm(formulanew,data=data_balanced_both,family=binomial)
  print(v)
}

# Compute AUC for each model size
auctrain = rep(0,length(variablesorder)-1)
auctest = rep(0,length(variablesorder)-1)
for(i in c(1:(length(variablesorder)-1))){
  vars = variablesorder[0:i+1]
  print(vars)
  formula<-paste("target","~",paste(vars,collapse="+"))
  model<-glm(formula,data=data_balanced_both,family="binomial")	
  predicttrain<-predict(model,newdata=data_balanced_both,type="response")
  predicttest<-predict(model,newdata=basetable_test,type="response")
  auctrain[i] = auc(data_balanced_both$target,predicttrain)
  auctest[i] = auc(basetable_test$target,predicttest)
} 

# Plot AUCs according to number of variables
plot(auctrain, main="AUC", col="red",ylim=c(0.5,0.75))
par(new=TRUE)
plot(auctest,col="blue",ylim=c(0.5,0.75))

#Select the model with optimal number of variables:
finalvariables = variablesorder[c(5:22)]
formula<-paste("target","~",paste(finalvariables,collapse="+"))
model<-glm(formula,data=data_balanced_both,family="binomial")	
predicttrain<-predict(model,newdata=data_balanced_both,type="response")
predicttest<-predict(model,newdata=basetable_test,type="response")
auctrain = auc(data_balanced_both$target,predicttrain)
auctest = auc(basetable_test$target,predicttest)

auctrain
auctest
# AUC TRAIN IS 65.21
# AUC TEST IS 64.14
summary(model)

#BUILD THE CONFUSION MATRIX TO CHECK SPECIFICITY and sensitivity CUT OFF 0.5
predicted <- as.numeric(predicttest>0.5)
reference <- basetable_test$target
u <- union(predicted, reference)
xtab <- table(factor(predicted, u), factor(reference, u))
confusionMatrix(xtab)

#---------------------------------------------------------------------#
# BUILD RANDOM FOREST MODEL
#---------------------------------------------------------------------#
basetable_train3 <- data_balanced_both

rForest <- randomForest(target ~ .,data = basetable_train3, ntree=200)

predictions_test <- predict(object = rForest, newdata = basetable_test, type = "class")
predictions_train <- predict(object = rForest, newdata = basetable_train3, type = "class")

auc(basetable_train3$target,predictions_train)
auc(basetable_test$target,predictions_test)


#BUILD THE CONFUSION MATRIX TO CHECK SPECIFICITY and sensitivity WITH CUT OFF 0.5
predicted_rdmforest <- as.numeric(predictions_test>0.5)
reference_rdmforest <- basetable_test$target
u <- union(predicted, reference)
xtab_rdmforest <- table(factor(predicted_rdmforest, u), factor(reference_rdmforest, u))
confusionMatrix(xtab_rdmforest)


