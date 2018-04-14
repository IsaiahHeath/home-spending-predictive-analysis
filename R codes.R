library("ggplot2")
library("stringr")
library("dplyr")
library("magrittr")
library("tidyr")
library("ggthemes")

#read into R
validation = read.csv("Validation.csv")
development = read.csv("Development.csv")


#changing column names
colnames(validation) = c("date", "sale")
colnames(development) = c("date", "sale")

#changing sale to numeric
development$sale = str_extract(development$sale, pattern = "[^$]+") %>% 
  str_replace(pattern = ",", replacement = "") %>%
  as.numeric()
 
#changing date to d/m/YYYY format 
year = str_extract(development$date, pattern = "[:digit:]{2}$") 
year = ifelse(str_detect(year, pattern = "9[0-9]") == TRUE, paste0("19", year), paste0("20", year))
month = str_extract(development$date, pattern = "(?<=/)[:digit:]+(?=/)")
day = str_extract(development$date, pattern = "^[:digit:]+(?=/)")
development$date = paste(day, month, year, sep = "/")

#plotting sale against date
ggplot(development, aes(x = as.Date(development$date, format = "%m/%d/%Y"), y = sale)) + 
  geom_line(aes(color = "yellow")) + labs(title = "Housing Spending (in millions)", x = "date") + 
  theme_hc() + 
  theme(legend.position = "none")

#read HPI monthly

hpi = read.csv("HPI_monthly.csv")

#collapse by year and month

agg.hpi = aggregate(hpi, by = list(as.factor(hpi$yr), as.factor(hpi$period)), FUN = "mean", na.rm = TRUE) %>%
  .[order(agg.hpi$yr, agg.hpi$period),]

#extracting HPI variable

hpi_index = subset(agg.hpi, agg.hpi$yr < 2016) %>% subset( yr > 1991) 
hpi_nsa = hpi_index$index_nsa
hpi_sa = hpi_index$index_sa

#adding HPI into development data

development = data.frame(development, hpi_nsa, hpi_sa)

#analyzing correlation between HPI and sale

ggplot(development, aes(hpi_nsa, sale)) + geom_point(alpha = .3)
ggplot(development, aes(hpi_sa, sale)) + geom_point(alpha = .3)
#looks like there is a positive correlation





