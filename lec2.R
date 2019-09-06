# get working directory
getwd()

# set working directory

setwd("~/Desktop/TU Delft/EPA1315/lecture2")

# read various formats - read.table()
# separator is a , 

survey_comma = read.table("survey_comma.csv", header = 1,sep = ',')
survey_sent = read.table("survey_semi.csv",header=1,sep=';')
survey_tab = read.table("survey_tab.txt",header=1,sep='\t')

install.packages("xlsx")
library("xlsx")
survey_xlsx = read.xlsx("survey_xls.xlsx",header=1,sheetIndex=1)

#what the data set contains
str(survey_tab)
# more detailed summary
summary(survey_tab)

#write data
write.table(survey_tab,"survey_written.csv",sep=',')

