library(doParallel)
core <- makeCluster(16) 
registerDoParallel(core)

library(dplyr)
library(pROC)
library(rpart)
library(emulator)
library(psych)
library(caret)
library(MLmetrics)
library(ROSE)
library(randomForest)
setwd("C:/Users/yliu10/Desktop/SML_IND/")
set.seed(2)

##################
#Data preparation#
##################

TBN_CIF <- read.csv("TBN_CIF.csv", sep=",",header = TRUE)
TBN_CUST_BEHAVIOR <- read.csv("TBN_CUST_BEHAVIOR.csv", sep=",",header = TRUE)
TBN_RECENT_DT <- read.csv("TBN_RECENT_DT.csv", sep=",",header = TRUE)
TBN_FX_TXN <- read.csv("TBN_FX_TXN.csv", sep=",",header = TRUE)
TBN_LN_APPLY <- read.csv("TBN_LN_APPLY.csv", sep=",",header = TRUE)
TBN_WM_TXN <- read.csv("TBN_WM_TXN.csv", sep=",",header = TRUE)
TBN_CC_APPLY <- read.csv("TBN_CC_APPLY.csv", sep=",",header = TRUE)

length(unique(TBN_CIF$CUST_NO))
length(unique(TBN_CUST_BEHAVIOR$CUST_NO))
length(unique(TBN_RECENT_DT$CUST_NO))
length(unique(TBN_FX_TXN$CUST_NO))
length(unique(TBN_LN_APPLY$CUST_NO))
length(unique(TBN_WM_TXN$CUST_NO))
length(unique(TBN_CC_APPLY$CUST_NO))

summary(TBN_RECENT_DT)
summary(TBN_CC_APPLY)

df <- distinct(TBN_CUST_BEHAVIOR,TBN_CUST_BEHAVIOR$CUST_NO, .keep_all= TRUE)
df$VISITDATE <- df$PAGE <- df$`TBN_CUST_BEHAVIOR$CUST_NO` <- NULL

TBN_CC_APPLYFRQ <- TBN_CC_APPLY %>%  group_by(CUST_NO) %>%  summarise(countcc = n())
TBN_CC_APPLYFRQ$ccapply <- ifelse(TBN_CC_APPLYFRQ$countcc>0,1,0)

data0 <- left_join(df, TBN_CIF, by="CUST_NO")
data0 <- left_join(data0, TBN_CC_APPLYFRQ, by="CUST_NO")

data1 <- left_join(data0, TBN_RECENT_DT, by="CUST_NO")


TBN_CUST_BEHAVIOR <- TBN_CUST_BEHAVIOR %>%  group_by(CUST_NO) %>%  summarise(countbehavior = n())
data2 <- merge(data1, TBN_CUST_BEHAVIOR, key="CUST_NO", all.x = TRUE)


TBN_FX_TXNSUM <- TBN_FX_TXN %>%  group_by(CUST_NO) %>%  summarise(FXSUM = sum(FX_TXN_AMT))
TBN_FX_TXNFRQ <- TBN_FX_TXN %>%  group_by(CUST_NO) %>%  summarise(countfx = n())
TBN_FX_TXNFRQ$fxapply <- ifelse(TBN_FX_TXNFRQ $countfx>0,1,0)
data3 <- merge(data2 , TBN_FX_TXNSUM, key="CUST_NO", all.x = TRUE)
data3 <- merge(data3 , TBN_FX_TXNFRQ, key="CUST_NO", all.x = TRUE)

TBN_LN_APPLYSUM <- TBN_LN_APPLY %>%  group_by(CUST_NO) %>%  summarise(LNSUM = sum(LN_AMT))
TBN_LN_APPLYFRQ <- TBN_LN_APPLY %>%  group_by(CUST_NO) %>%  summarise(countln = n())
TBN_LN_APPLYFRQ$lnapply <- ifelse(TBN_LN_APPLYFRQ$countln>0,1,0)
TBN_LN_APPLYUNQ <- distinct(TBN_LN_APPLY,TBN_LN_APPLY$CUST_NO, .keep_all= TRUE)
TBN_LN_APPLYUNQ$TXN_DT <- TBN_LN_APPLYUNQ$LN_AMT <- TBN_LN_APPLYUNQ$`TBN_LN_APPLY$CUST_NO` <- NULL
data4 <- merge(data3, TBN_LN_APPLYSUM, key="CUST_NO", all.x = TRUE)
data4 <- merge(data4, TBN_LN_APPLYFRQ, key="CUST_NO", all.x = TRUE)
data4 <- merge(data4, TBN_LN_APPLYUNQ, key="CUST_NO", all.x = TRUE)

TBN_WM_TXNSUM <- TBN_WM_TXN %>%  group_by(CUST_NO) %>%  summarise(WMSUM = sum(WM_TXN_AMT))
TBN_WM_TXNFRQ <- TBN_WM_TXN %>%  group_by(CUST_NO) %>%  summarise(countwm = n())
TBN_WM_TXNFRQ$wmapply <- ifelse(TBN_WM_TXNFRQ$countwm>0,1,0)
TBN_WM_TXNUNQ <- distinct(TBN_WM_TXN,TBN_WM_TXN$CUST_NO, .keep_all= TRUE)
TBN_WM_TXNUNQ$TXN_DT <- TBN_WM_TXNUNQ$WM_TXN_AMT <- TBN_WM_TXNUNQ$`TBN_WM_TXN$CUST_NO` <- NULL
data5 <- merge(data4, TBN_WM_TXNUNQ, key="CUST_NO", all.x = TRUE)
data5 <- merge(data5, TBN_WM_TXNFRQ, key="CUST_NO", all.x = TRUE)
data5 <- merge(data5, TBN_WM_TXNSUM, key="CUST_NO", all.x = TRUE)

colnames(data5) <- tolower(names(data5))

data5$m <- ifelse(data5$gender_code=="M",1,0)
data5$f <- ifelse(data5$gender_code=="F",1,0)
data5$gender_code <- NULL
data5 <- data5%>%select(-ccapply,everything())
data5$ln_use <- as.numeric(data5$ln_use)
data5$age <- as.numeric(data5$age)
data5[is.na(data5)] <- 0
hist(data5$ccapply)
data5$cust_no <- NULL
cor(data5)

#data5$ccapply <- ifelse(data5$ccapply == 1, "Y", "N")
#data5$ccapply <- as.factor(data5$ccapply)

#Drop
data5$countcc <- NULL

# Mix sampling
#data5 <- ovun.sample(ccapply ~ ., data = data5, method = "both", p=0.5, N=195000, seed = 1)$data

# Under sampling
#data5 <- ovun.sample(ccapply ~ ., data = data5, method = "under",N=38062,seed = 2)$data

# Over Sampling
#data5 <- ovun.sample(ccapply ~ ., data = data5, method = "over",N = 351938)$data

# ROSE sampling
#data5 <- ROSE(ccapply ~ ., data = data5, seed = 1)$data

hist(data5$ccapply)
sum(data5$ccapply)
#Cut test and train sets
data5_train_size = floor(0.7*nrow(data5))
data5_train_ind = sample(seq_len(nrow(data5)),size = data5_train_size)
data5_train = data5[data5_train_ind,]
data5_test  = data5[-data5_train_ind,]


#AUC
auc = function(trueval, predval){
  df = as.data.frame(cbind(trueval,predval))
  names(df) = c("trueval","predval")
  auc = roc(trueval~predval,data=df)$auc
  return(auc)
}

#Stepwise logistic regression
variables <- c("age",               "children_cnt",      "cust_start_dt",     "edu_code",          "income_range_code",
               "work_mths",         "cc_recent_dt",      "fx_recent_dt",      "ln_recent_dt",      "wm_recent_dt",     
               "countbehavior",     "fxsum",             "countfx",           "fxapply",           "lnsum",            
               "countln",           "lnapply",           "ln_use",            "cust_risk_code",    "invest_type_code", 
               "countwm",           "wmapply",           "wmsum",             "m",                 "f")

variablesorder = c()
model = glm(ccapply~ 1,data=data5,binomial(link='logit'),control=list(maxit=100))
formula<-formula(paste("ccapply","~",paste(variables,collapse="+")))

for(i in c(1:length(variables))){
  #calculate AIC of each model
  info = add1(model,scope=formula,data=data5_train)
  print(info)
  #get variable with lowest AIC
  orderedvariables = rownames(info[order(info$AIC),])
  v = orderedvariables[orderedvariables!="<none>"][1]
  #add variable to formula
  variablesorder = append(variablesorder,v)
  formulanew = formula(paste("ccapply","~",paste(variablesorder,collapse = "+")))
  model = glm(formulanew,data=data5_train,family=binomial)
  print(v)
}


auctrain = rep(0,length(variablesorder)-1)
auctest = rep(0,length(variablesorder)-1)

for(i in c(1:(length(variablesorder)-1))){
  vars = variablesorder[0:i+1]
  print(vars)
  formula<-paste("ccapply","~",paste(vars,collapse="+"))
  model<-glm(formula,data=data5_train,family="binomial")	
  predicttrain<-predict(model,newdata=data5_train,type="response")
  predicttest<-predict(model,newdata=data5_test,type="response")
  auctrain[i] = auc(data5_train$ccapply,predicttrain)
  auctest[i] = auc(data5_test$ccapply,predicttest)
} 

print(auc(data5_train$ccapply,predicttrain))
print(auc(data5_test$ccapply,predicttest))

plot(auctrain, main="AUC", col="red")
par(new=TRUE)
plot(auctest,col="blue",add=TRUE)


predicttest <- as.numeric(predicttest>0.5)
reference <- data5_test$ccapply
u <- union(predicttest, reference)
xtab <- table(factor(predicttest, u), factor(reference, u))
confusionMatrix(xtab, positive = "1")
result <- confusionMatrix(xtab)
df <- data.frame(result$byClass)

presicion <- df[5,]
recall <- df[6,]


#F1_Score

F1Score <- function (recall, precision)
{2*((precision*recall)/(precision+recall))
}

F1Score(recall,presicion)
