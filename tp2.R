require(ROCR)
require(rpart)

GermanData  <- read.csv("/Users/alexprovencher/Desktop/tpR/GermanData.csv",header=F)
CarData     <- read.csv("/Users/alexprovencher/Desktop/tpR/CarData.csv",header=F)


#names(GermanData) <- c("chk_acct", "duration", "history", "purpose", "amount", "sav_acct", "employment", "install_rate", "pstatus", "other_debtor", "time_resid", "property", "age", "other_install", "housing", "other_credits", "job", "num_depend", "telephone", "foreign", "response")
#names(CarData) <- c("id","col1","col2","col3","col4","col5","col6")



GermanData_training_idx <-sample(1:nrow(GermanData), nrow(GermanData) * 0.2, replace=F)
GermanData_test_idx <- setdiff(1:nrow(GermanData), GermanData_training_idx)

GermanDatatraining <- GermanData[GermanData_training_idx,]
GermanDatatest <- GermanData[GermanData_test_idx,]


GermanDatatraining
