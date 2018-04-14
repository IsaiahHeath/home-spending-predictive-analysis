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
housing_lineplot = ggplot(development, aes(x = as.Date(development$date, format = "%m/%d/%Y"), y = sale)) + 
  geom_line(aes(color = "yellow")) + labs(x = "date") + 
  theme_hc() + 
  theme(legend.position = "none")







