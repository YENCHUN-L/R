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
library(ROCR)
library(fBasics)
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

#write.csv(data5, "data_1_0.csv")

data6 <- data5
data6$cust_no <- NULL
cor(data6)

#x <- describe(data6)
#write.csv(x, "describe.csv")

#Drop
data5$cust_no <- NULL
data5$countcc <- NULL

# T 19031 F 175969

# Mix sampling (Decrease 0 and increase 1 when data is balance)
#data5 <- ovun.sample(ccapply ~ ., data = data5, method = "both", p=0.5, N=195000, seed = 1)$data

# Under sampling (Decrease 0 to the same amount of 1)
#data5 <- ovun.sample(ccapply ~ ., data = data5, method = "under",N=38062,seed = 2)$data

# Over Sampling (Increase 1 to the same amount of 0)
#data5 <- ovun.sample(ccapply ~ ., data = data5, method = "over",N = 351938)$data

# ROSE sampling (Synthetic data)
#data5 <- ROSE(ccapply ~ ., data = data5, seed = 1)$data

hist(data5$ccapply)


#Cut test and train sets
data5_train_size = floor(0.7*nrow(data5))
data5_train_ind = sample(seq_len(nrow(data5)),size = data5_train_size)
data5_train = data5[data5_train_ind,]
data5_test  = data5[-data5_train_ind,]

sum(data5_train$ccapply)
data5_train$ccapply <- ifelse(data5_train$ccapply == 1, "Y", "N")
data5_train$ccapply <- as.factor(data5_train$ccapply)


sum(data5_test$ccapply)
data5_test$ccapply <- ifelse(data5_test$ccapply == 1, "Y", "N")
data5_test$ccapply <- as.factor(data5_test$ccapply)


#########
# Model #
#########
tunerf <- tuneRF(data5_train[,2:25],data5_train[,26], stepFactor=0.5)
plot(tunerf, type = "line")
library(doParallel)
core <- makeCluster(16) 
registerDoParallel(core)

model = randomForest(ccapply ~ age+cc_recent_dt+children_cnt+cust_start_dt+edu_code+income_range_code+work_mths+
                       fx_recent_dt+ln_recent_dt+wm_recent_dt+countbehavior+fxsum+countfx+fxapply+lnsum+
                       countln+lnapply+ln_use+cust_risk_code+invest_type_code+countwm+wmapply+wmsum+m+f,
                     data=data5_train, ntree = 100, mtry =4, importance=TRUE)
model
plot(model)
importance(model)   
varImpPlot(model)


#AUC
rf_p_train <- predict(model, newdata= data5_train, type = "prob")
rf_pr_train <- prediction(rf_p_train[,2], data5_train$ccapply)
r_auc_train <- performance(rf_pr_train, measure = "auc")@y.values[[1]] 
r_auc_train


rf_p_test <- predict(model, newdata= data5_test, type = "prob")
rf_pr_test <- prediction(rf_p_test[,2], data5_test$ccapply)
r_auc_test <- performance(rf_pr_test, measure = "auc")@y.values[[1]]
r_auc_test


#F1_Score
predict_test <- predict(model, newdata= data5_test, type = "class")
table(predict_test, data5_test$ccapply)
data8 <- data.frame(matrix(nrow=length(data5_test$ccapply), ncol=0))
data8$ccapply <- data5_test$ccapply
data8$predict <- predict_test
data8$ccapply <- ifelse(data8$ccapply == "Y",1,0)
data8$predict <- ifelse(data8$predict == "Y",1,0)
F1_Score(data8$ccapply,data8$predict)


confusionMatrix(table(data8$predict,data8$ccapply), positive = "1")

MSE(y_pred = data8$predict, y_true = data8$ccapply)
