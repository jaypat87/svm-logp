#Using e1071 (R interface to libsvm) for predicting logp
library('e1071')
#Loading a preprocessed and cleaned file with descriptors and obs Logp
logp <- read.table("logp.txt", header=T, sep="\t", as.is=T )
View(logp)
LogpTraining <-logp[1:11370,]
LogpTest <-logp[11371:14207,]
dim(LogpTraining)
dim(LogpTest)
#splitting the dataset into training and test set
MeasuredLogpTraining<-LogpTraining$LogP
MeasuredLogpTest<-LogpTraining$LogP
#Assigning measured/observed logP values into a separate variable
LogPSVM <- svm(LogP~., data= LogpTraining, cost = 150, epsilon = 0.05, gamma = 0.00014)
PredLogPtrainingSVM<-predict(LogPSVM, LogpTraining)
CorrLogPtrainingSVM<-lm(PredLogPtrainingSVM ~ MeasuredLogPTraining)
summary(CorrLogPtrainingSVM)

# Predict logP from the test set
PredLogPtestSVM<-predict(LogPSVM, LogPdata600BitsTest)
# Correlation between measured and predicted logP values for the test set
CorrLogPtestSVM<-lm(PredLogPtestSVM ~ MeasuredLogPTest)
summary(CorrLogPtestSVM)

# Apply the built-in cross validation feature. Set the argument cross to 10.
LogPSVMcv <- svm(LogP~., data = LogPdata600BitsTraining, cross = 10, cost = 150, epsilon = 0.05, gamma = 0.00014)
# Summary for 10-fold cross validation.
summary(LogPSVMcv)
