
validation = read.csv("Validation.csv")
development = read.csv("Development.csv")

library(dplyr)
library(ggplot2)
library(reshape2)
library(readr)

#import stlouisfed datasets
#S&P 500 Closing Values https://finance.yahoo.com/quote/%5EGSPC/history?period1=662709600&period2=1523682000&interval=1mo&filter=history&frequency=1mo
X_GSPC <- read_csv("external datasets/^GSPC.csv", col_types = cols_only(Close = col_guess(), Date = col_guess()))
X_GSPC_train <- X_GSPC[X_GSPC$Date >= "1992-01-01" & X_GSPC$Date <= "2015-12-01",]
X_GSPC_test <- X_GSPC[X_GSPC$Date > "2015-12-01" & X_GSPC$Date <= "2017-12-01",]

#Real Disposable Income Per Capita (chained 2009 dollars) https://fred.stlouisfed.org/series/A229RX0
A229RX0 <- read_csv("external datasets/A229RX0.csv", na = "NA")
A229RX0_train <- A229RX0[A229RX0$DATE >= "1992-01-01" & A229RX0$DATE <= "2015-12-01",]
A229RX0_test <- A229RX0[A229RX0$DATE > "2015-12-01" & A229RX0$DATE <= "2017-12-01",]

#Consumer Price Index for Urban Housing (1982-1984 = 100) https://fred.stlouisfed.org/series/CPIHOSSL
CPIHOSSL <- read_csv("external datasets/CPIHOSSL.csv", na = "NA")
CPIHOSSL_train <- CPIHOSSL[CPIHOSSL$DATE >= "1992-01-01" & CPIHOSSL$DATE <= "2015-12-01",]
CPIHOSSL_test <- CPIHOSSL[CPIHOSSL$DATE > "2015-12-01" & CPIHOSSL$DATE <= "2017-12-01",]

#Case-Shiller US National Home Price Index (index January 2000 = 100) https://fred.stlouisfed.org/series/CSUSHPINSA
CSUSHPINSA <- read_csv("external datasets/CSUSHPINSA.csv", na = "NA")
CSUSHPINSA_train <- CSUSHPINSA[CSUSHPINSA$DATE >= "1992-01-01" & CSUSHPINSA$DATE <= "2015-12-01",]
CSUSHPINSA_test <- CSUSHPINSA[CSUSHPINSA$DATE > "2015-12-01" & CSUSHPINSA$DATE <= "2017-12-01",]

#Effective Federal Funds Rate (percent) https://fred.stlouisfed.org/series/FEDFUNDS
FEDFUNDS <- read_csv("external datasets/FEDFUNDS.csv", na = "NA")
FEDFUNDS_train <- FEDFUNDS[FEDFUNDS$DATE >= "1992-01-01" & FEDFUNDS$DATE <= "2015-12-01",]
FEDFUNDS_test <- FEDFUNDS[FEDFUNDS$DATE > "2015-12-01" & FEDFUNDS$DATE <= "2017-12-01",]

# #US Gross Domestic Product (billions) https://fred.stlouisfed.org/series/GDP
# datasetGDP <- read_csv("external datasets/GDP.csv")
# datasetGDP <- datasetGDP[datasetGDP >= "1992-01-01" & datasetGDP <= "2015-12-01",]
# 
# 
# #30-Year Conventional Mortgage Rate (percent)(NA after 9-1-2016) https://fred.stlouisfed.org/series/MORTG
# MORTG <- read_csv("external datasets/MORTG.csv")

#Personal Savings Rate (percent) https://fred.stlouisfed.org/series/PSAVERT
PSAVERT <- read_csv("external datasets/PSAVERT.csv")
PSAVERT_train <- PSAVERT[PSAVERT$DATE >= "1992-01-01" & PSAVERT$DATE <= "2015-12-01",]
PSAVERT_test <- PSAVERT[PSAVERT$DATE > "2015-12-01" & PSAVERT$DATE <= "2017-12-01",]

#Household Estimates (thousands) https://fred.stlouisfed.org/series/TTLHHM156N
#datasetHE <- read_csv("external datasets/TTLHHM156N.csv", na = "NA")
#datasetHE$TTLHHM156N = as.numeric(datasetHE$TTLHHM156N)
#datasetHE_train <- datasetHE[datasetHE$DATE >= "1992-01-01" & datasetHE$DATE <= "2015-12-01",]
#datasetHE_test <-  datasetHE[datasetHE$DATE > "2015-12-01",]


#Development CSV
devcsv <- read_csv("Development.csv")

#Formatting and additional columns
myDataFrame_train <- data.frame(SP500Close=X_GSPC_train$Close, RDI=A229RX0_train$A229RX0, CPI=CPIHOSSL_train$CPIHOSSL, HPI=CSUSHPINSA_train$CSUSHPINSA, FFR=FEDFUNDS_train$FEDFUNDS, PSR=PSAVERT_train$PSAVERT)
myDataFrame_train <- cbind(devcsv$`Sales in $MM`, myDataFrame_train)
myDataFrame_train <- cbind(X_GSPC_train$Date, myDataFrame_train)
names(myDataFrame_train)[names(myDataFrame_train) == 'devcsv$`Sales in $MM`'] <- 'Sales'
myDataFrame_train$Sales <- as.numeric(gsub('[$,]', '', myDataFrame_train$Sales))

names(myDataFrame_train)[names(myDataFrame_train) == 'X_GSPC_train$Date'] <- 'Date'



#Dataframe for predicted sales (in $mm)

validationcsv <- read_csv("validation.csv")

#Formatting and additional columns
myDataFrame_test <- data.frame(SP500Close=X_GSPC_test$Close, RDI=A229RX0_test$A229RX0, CPI=CPIHOSSL_test$CPIHOSSL, HPI=CSUSHPINSA_test$CSUSHPINSA, FFR=FEDFUNDS_test$FEDFUNDS, PSR=PSAVERT_test$PSAVERT)
myDataFrame_test <- cbind(validationcsv$`Sales in $MM`, myDataFrame_test)
myDataFrame_test <- cbind(X_GSPC_test$Date, myDataFrame_test)
names(myDataFrame_test)[names(myDataFrame_test) == 'validationcsv$`Sales in $MM`'] <- 'Sales'
myDataFrame_test$Sales <- as.numeric(gsub('[$,]', '', myDataFrame_test$Sales))

names(myDataFrame_test)[names(myDataFrame_test) == 'X_GSPC_test$Date'] <- 'Date'

merged_df = rbind(myDataFrame_train, myDataFrame_test)


#Model
#attach(myDataFrame)

train_df = merged_df[1:288,]
test_df = merged_df[289:nrow(merged_df),]

colnames(train_df) = colnames(test_df)

model1 <- lm(Sales ~ SP500Close + RDI + CPI + HPI + FFR + PSR, data = train_df)
summary(model1)




test_prediction <- predict(model1, test_df)
mse = rmse(test_prediction, test_df$Sales)

