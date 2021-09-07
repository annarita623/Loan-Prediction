
#Step - Data Loading and viewing its features and other datatype and structure

#loading Data
loantrain=read.csv(choose.files(),na.strings = c("",'NA'))
loantest=read.csv(choose.files(),na.strings = c("",'NA'))
#Checking top and last 5 obs of data
head(loantrain)
tail(loantrain)
#ncol = no. of columns
ncol(loantrain)
#nrow = no. of rows
nrow(loantrain)
#class- data type 
class(loantrain)
#str - structure of data
str(loantrain)
colnames(loantrain)
summary(loantrain$Loan_Status)
#to check outliers we use boxplot
boxplot(loantrain$LoanAmount)
histogram(loantrain$LoanAmount)
densityplot(loantrain$LoanAmount)#positive skweness
install.packages("moments")

library(moments)
skew(loantrain$LoanAmount, na.rm = TRUE)

#-------------------------------------------------------------------------------
  
#psych libary is used to get values in numeric i.e mean , max, min
install.packages("psych")
library(psych)
#describe will give all vaules
describe(loantrain)
#table is same as agg function 
table(loantrain$Loan_Status) #no-192 Yes -422

#-------------------------------------------------------------------------------

#Step 2 - Cleaning data i.e NA
#Checking na in both i.e train and test
sort(colSums(is.na(loantest)),decreasing = T)
sort(colSums(is.na(loantrain)),decreasing = T)

#As Married has 3 Nas which is in Train dataset -replace with mode  
loantrain$Married=ifelse(is.na(loantrain$Married),
                           "Yes",loantrain$Married)

# now both test and train has nas with same column 
# so now we can combine them
# to combine no of columns should be same, so adding Loan_status in test data set
loantest['Loan_Status']='test'
#rbind is nothing but binding 2 table row wise
combinedf=rbind(loantrain,loantest)
View(combinedf)#

sort(colSums(is.na(combinedf)),decreasing = T)
# Missing Values are in Categorical Variables
#1 replacing na with new class i.e 2 because na were more and cant take decision, so classified in new class
combinedf$Credit_History=ifelse(is.na(combinedf$Credit_History),
                         "2",combinedf$Credit_History)
table(combinedf$Credit_History)

#2 Has mode for self_employed was No so na are replaced with NO 
combinedf$Self_Employed=ifelse(is.na(combinedf$Self_Employed),
                                "No",combinedf$Self_Employed)

#3 Has loan amount is numeric and fisible solution was to replace with mean or median so replaced with mean 
combinedf$LoanAmount=ifelse(is.na(combinedf$LoanAmount),
                               mean(combinedf$LoanAmount,na.rm = T),combinedf$LoanAmount)

#4 has mode for gender was male i.e male =799 so replaced with male / can classify new class too.
combinedf$Gender=ifelse(is.na(combinedf$Gender),
                                "Male",combinedf$Gender)

#5as na were less in no so replaced with max .
combinedf$Dependents=ifelse(is.na(combinedf$Dependents),
                               "0",combinedf$Dependents)
table(combinedf$Dependents)


#6 As loan amount with loan amount_term was more so replaced with 360 value
aggregate(combinedf$LoanAmount~combinedf$Loan_Amount_Term,
          FUN=mean)
combinedf$Loan_Amount_Term[is.na(combinedf$Loan_Amount_Term)]=360

#Checking any na is remaining
sort(colSums(is.na(combinedf)),decreasing = T)

#7 adding new column Debt_Income_Ratio
combinedf['Debt_Income_Ratio']=combinedf$ApplicantIncome/combinedf$LoanAmount


colnames(combinedf)
str(combinedf)

loantraindf=subset(combinedf,combinedf$Loan_Status!='test')
loantestdf=subset(combinedf,combinedf$Loan_Status=='test')
loantestdf=loantestdf[-13]

loantraindf$Loan_Status=as.factor(loantraindf$Loan_Status)

#-------------------------------------------------------------------------------

# Build logistic Regression Model

loanlogistic=glm(Loan_Status~.,data=loantraindf[-1],
               family = 'binomial')
summary(loanlogistic)
loanlogisticpredict=predict(loanlogistic,type='response')
table(Actual=loantraindf$Loan_Status,
      Predicted=loanlogisticpredict>0.50)
(414+84)/(84+8+414+108) #acc-0.8110749
loantestpredict=predict(loanlogistic,loantestdf,type="response")
loantestpredict
loantestpredictclass=ifelse(loantestpredict>=0.5,'Y','N')
write.csv(loantestpredictclass,"logreg.csv")

--------------------------------------------------------------------------------
install.packages('rpart.plot')
library(rpart) # Recursive Partitioning Trees
library(rpart.plot)
loandectree=rpart(Loan_Status~.,data=loantraindf[-1])
summary(loandectree)
rpartpredict=predict(loandectree,type="class")
table(Actual=loantraindf$Loan_Status,
      Predicted=rpartpredict)
(110+404)/(82+404+110+18) #acc-0.8371336
rpart.plot(loandectree,cex=0.50)
rpart.rules(loandectree)
rparttestpredict=predict(loandectree,loantestdf,
                         type="class")
table(loantestpredictclass,rparttestpredict)
write.csv(rparttestpredict,"rpart.csv")

library(caret)
confusionMatrix(loantraindf$Loan_Status,rpartpredict)

#--------------------------------------------------------------------------------

# DECISION TREE with rpart package with information gain split
loandectree2=rpart(Loan_Status~.,data=loantraindf[-1],parms=list(split='information'))
summary(loandectree2)
loandectreepredict=predict(loandectree2,type="class")
table(Actual=loantraindf$Loan_Status,Predicted=loandectreepredict)
#(403+112)/(112+80+19+403) - acc = 0.8387622
confusionMatrix(loantraindf$Loan_Status,loandectreepredict)

#-------------------------------------------------------------------------------
install.packages('randomForest')
library(randomForest)
loanRF=randomForest(Loan_Status~.,data=loantraindf[-1],
                    ntree=2000,do.trace=100)
plot(loanRF)
print(loanRF)
#100-20.03  acc=79.97
loantestRF=predict(loanRF,loantestdf,type="class")
write.csv(loantestRF,"RF.csv")
#--------------------------------------------------------------------------------
install.packages('gbm')
library(gbm)
loantraindf2=loantraindf
loantraindf2$Gender=ifelse(loantraindf2$Gender=='Male',1,0)
loantraindf2$Married=ifelse(loantraindf2$Married=='Yes',1,0)
loantraindf2$Self_Employed=ifelse(loantraindf2$Self_Employed=='Yes',1,0)
loantraindf2$Married=ifelse(loantraindf2$Married=='Urban',1,0)
loantraindf2$Dependents=as.factor(loantraindf2$Dependents)
loantraindf2$Education=as.factor(loantraindf2$Education)
loantraindf2$Credit_History=as.factor(loantraindf2$Credit_History)
loantraindf2$Property_Area=as.factor(loantraindf2$Property_Area)
loantraindf2$Loan_Status=ifelse(loantraindf2$Loan_Status=='Y',1,0)

loangbm=gbm(Loan_Status~.,data=loantraindf2[-1],
            n.trees=2000,cv.folds = 5,distribution = "gaussian")
print(loangbm)
summary(loangbm)
plot(loangbm)
best.iter <- gbm.perf(loangbm,method="cv")
best.iter
gbmpredict=predict(loangbm,loantestdf,best.iter,type="response")
gbmpredictclass=ifelse(gbmpredict>=0.5,"Y","N")
write.csv(gbmpredictclass,"gbm.csv")
#--------------------------------------------------------------------------------




