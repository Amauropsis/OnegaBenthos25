## LIBS ####
install.packages("xlsx")
library(tidyverse)
install.packages("writexl")
library(writexl)
library(readxl)

## data-set ####
data_OP <- read_delim("https://raw.githubusercontent.com/Amauropsis/OnegaBenthos25/refs/heads/main/OP_NB.csv", delim = ";")

write_xlsx(data_OP, "data_OP.xlsx")

data_OP <- read_xlsx(file.choose(), sheet = "Sheet1")

data_OP1 <- read.csv("https://raw.githubusercontent.com/Amauropsis/OnegaBenthos25/refs/heads/main/OP_NB.csv", delim = "data_OP1") 
