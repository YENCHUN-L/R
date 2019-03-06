#Installing packages
library(pROC)
library(rpart)
library(randomForest)

#Data preparation
titanic = read.csv("C:/Users/jason/Desktop/titanic.csv",sep=";",header = TRUE)
titanic$sex=factor(titanic$sex,levels=c('male','female'),labels = c(1,0))
titanic$age=gsub(',','.',titanic$age)
titanic$age<-as.numeric(titanic$age)
titanic$age[is.na(titanic$age)]=mean(titanic$age, na.rm=T)
titanic$age <- round(titanic$age)
titanic$ticket=gsub(',','.',titanic$ticket)
titanic$fare=gsub(',','.',titanic$fare)
titanic$fare=as.numeric(titanic$fare)
titanic$boat <- ifelse(titanic$boat!='',1,0)
titanic$name=NULL
titanic$home.dest=NULL
titanic$body=NULL
titanic$embarked=NULL
titanic$cabin=NULL
titanic$ticket=NULL
titanic$sex<-as.numeric(titanic$sex)

#Detemrining test and train sets
titanic_train_size = floor(0.7*nrow(titanic))
titanic_train_ind = sample(seq_len(nrow(titanic)),size = titanic_train_size)
titanic_train = titanic[titanic_train_ind,]
titanic_test  = titanic[-titanic_train_ind,]

#Defining AUC function
auc = function(trueval, predval){
  df = as.data.frame(cbind(trueval,predval))
  names(df) = c("trueval","predval")
  auc = roc(trueval~predval,data=df)$auc
  return(auc)
}


#Stepwise logistic regression
variables = c('pclass', 'sex', 'age','sibsp','parch','fare')
variablesorder = c()
model = glm(survived ~ 1,data=titanic_train,family=binomial)
formula<-formula(paste("survived","~",paste(variables,collapse="+")))

for(i in c(1:length(variables))){
  #calculate AIC of each model
  info = add1(model,scope=formula,data=titanic_train)
  print(info)
  #get variable with lowest AIC
  orderedvariables = rownames(info[order(info$AIC),])
  v = orderedvariables[orderedvariables!="<none>"][1]
  #add variable to formula
  variablesorder = append(variablesorder,v)
  formulanew = formula(paste("survived","~",paste(variablesorder,collapse = "+")))
  model = glm(formulanew,data=titanic_train,family=binomial)
  print(v)
}


auctrain = rep(0,length(variablesorder)-1)
auctest = rep(0,length(variablesorder)-1)

for(i in c(1:(length(variablesorder)-1))){
  vars = variablesorder[0:i+1]
  print(vars)
  formula<-paste("survived","~",paste(vars,collapse="+"))
  model<-glm(formula,data=titanic_train,family="binomial")	
  predicttrain<-predict(model,newdata=titanic_train,type="response")
  predicttest<-predict(model,newdata=titanic_test,type="response")
  auctrain[i] = auc(titanic_train$survived,predicttrain)
  auctest[i] = auc(titanic_test$survived,predicttest)
} 

print(auc(titanic_test$survived,predicttest))
print(auc(titanic_train$survived,predicttrain))


plot(auctrain, type = "line", main="AUC", col="red")
par(new=TRUE)
plot(auctest, type = "line",col="blue",add=TRUE)


