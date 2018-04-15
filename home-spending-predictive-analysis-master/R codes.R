
validation = read.csv("Validation.csv")
development = read.csv("Development.csv")

library(dplyr)
library(ggplot2)
library(readr)

#dataframe
x <- c("SP500closing", "RealDisposableIncome", "CPI", "HPI", "FFR", "PSR", "HE")
dFrame <- data.frame(matrix(ncol = length(x)))
colnames(dFrame) <- x

#import stlouisfed datasets
#S&P 500 Closing Values https://finance.yahoo.com/quote/%5EGSPC/history?period1=662709600&period2=1523682000&interval=1mo&filter=history&frequency=1mo
X_GSPC <- read_csv("external datasets/^GSPC.csv", col_types = cols_only(Close = col_guess(), Date = col_guess()))
X_GSPC <- X_GSPC[X_GSPC >= "1992-01-01" & X_GSPC <= "2015-12-01",]
X_GSPC <- slice(X_GSPC, 1:288)

#Real Disposable Income Per Capita (chained 2009 dollars) https://fred.stlouisfed.org/series/A229RX0
A229RX0 <- read_csv("external datasets/A229RX0.csv", na = "NA")
A229RX0 <- A229RX0[A229RX0 >= "1992-01-01" & A229RX0 <= "2015-12-01",]
A229RX0 <- slice(A229RX0, 1:288)

#Consumer Price Index for Urban Housing (1982-1984 = 100) https://fred.stlouisfed.org/series/CPIHOSSL
CPIHOSSL <- read_csv("external datasets/CPIHOSSL.csv", na = "NA")
CPIHOSSL <- CPIHOSSL[CPIHOSSL >= "1992-01-01" & CPIHOSSL <= "2015-12-01",]
CPIHOSSL <- slice(CPIHOSSL, 1:288)

#Case-Shiller US National Home Price Index (index January 2000 = 100) https://fred.stlouisfed.org/series/CSUSHPINSA
CSUSHPINSA <- read_csv("external datasets/CSUSHPINSA.csv", na = "NA")
CSUSHPINSA <- CSUSHPINSA[CSUSHPINSA >= "1992-01-01" & CSUSHPINSA <= "2015-12-01",]
CSUSHPINSA <- slice(CSUSHPINSA, 1:288)

#Effective Federal Funds Rate (percent) https://fred.stlouisfed.org/series/FEDFUNDS
FEDFUNDS <- read_csv("external datasets/FEDFUNDS.csv", na = "NA")
FEDFUNDS <- FEDFUNDS[FEDFUNDS >= "1992-01-01" & FEDFUNDS <= "2015-12-01",]
FEDFUNDS <- slice(FEDFUNDS, 1:288)

# #US Gross Domestic Product (billions) https://fred.stlouisfed.org/series/GDP
# datasetGDP <- read_csv("external datasets/GDP.csv")
# datasetGDP <- datasetGDP[datasetGDP >= "1992-01-01" & datasetGDP <= "2015-12-01",]
# 
# 
# #30-Year Conventional Mortgage Rate (percent)(NA after 9-1-2016) https://fred.stlouisfed.org/series/MORTG
# MORTG <- read_csv("external datasets/MORTG.csv")

#Personal Savings Rate (percent) https://fred.stlouisfed.org/series/PSAVERT
PSAVERT <- read_csv("external datasets/PSAVERT.csv")
PSAVERT <- PSAVERT[PSAVERT >= "1992-01-01" & PSAVERT <= "2015-12-01",]
PSAVERT <- slice(PSAVERT, 1:288)

#Household Estimates (thousands) https://fred.stlouisfed.org/series/TTLHHM156N
datasetHE <- read_csv("external datasets/TTLHHM156N.csv", na = "NA")
datasetHE <- datasetHE[datasetHE >= "1992-01-01" & datasetHE <= "2015-12-01",]
datasetHE <- slice(datasetHE, 1:288)

myDataFrame <- data.frame(SP500Close=X_GSPC$Close, RDI=A229RX0$A229RX0, CPI=CPIHOSSL$CPIHOSSL, HPI=CSUSHPINSA$CSUSHPINSA, FFR=FEDFUNDS$FEDFUNDS, PSR=PSAVERT$PSAVERT, HE=datasetHE$TTLHHM156N)
myDataFrame <- cbind(X_GSPC$Date, myDataFrame)
names(myDataFrame)[names(myDataFrame) == 'X_GSPC$Date'] <- 'Date'
