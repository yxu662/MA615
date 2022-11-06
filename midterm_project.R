myName <- "Yin Xu"

strawb <- read_xlsx("/Users/kellyvaxu/Downloads/strawberries-2022oct30-a.xlsx")

library(tidyverse)
library(magrittr)
library(readxl)

cnames <- colnames(strawb)
x <- 1:dim(strawb)[2]

unique(strawb[1])
unique(strawb[2])
unique(strawb[3])

T <- NULL
for(i in x){T <- c(T, dim(unique(strawb[i]))[1])}
drop_cols <- cnames[which(T == 1)]
strawb %<>% select(!all_of(drop_cols))
strawb %<>% arrange(Year, State)

colnames(strawb)
temp1 <- strawb %>% select(`Data Item`) %>% 
  distinct()
strawb2 <- strawb %>% separate(col=`Data Item`,
                               into = c("Strawberries", "items", "units"),
                               sep = ",",
                               fill = "right")
strawb3 <- strawb %>% separate(col=`Data Item`,
                               into = c("Strawberries", "type", "items", "units"),
                               sep = ",",
                               fill = "right")
rm(strawb2, strawb3)

strawb %<>% separate(col=`Data Item`,
                     into = c("Strawberries", "type", "items", "units"),
                     sep = ",",
                     fill = "right")

#1
#285CWT 
#28500LB

#2
organsales_2016_ca <- filter(strawb, State == 'CALIFORNIA' & 
                               Year == 2016 & 
                               Domain == 'ORGANIC STATUS')
margin1 <- 231304956*1.96*0.137
upper1 <- 231304956+62110007
lower1 <- 231304956-62110007

#3
nonorgansales_2016_ca <- filter(strawb, State == 'CALIFORNIA' & 
                                  Year == 2016 & 
                                  Domain != 'ORGANIC STATUS')
new_non <- filter(nonorgansales_2016_ca, Value != "(NA)" & 
                    Value != "(D)" & 
                    Domain != "TOTAL")
library(gmodels)
library(Rmisc)
CI(as.numeric(new_non$Value))

#4
unique(strawb[10])
chemical <- filter(strawb, Domain != 'ORGANIC STATUS' & 
                     Domain != 'TOTAL')
grep("TOTAL",
     chemical$`Domain Category`,
     ignore.case = T)
unique(chemical[11])
175 - 36
#ans_4 = 139

#5
chemical_fl <- filter(strawb, State == 'FLORIDA' & 
                             Domain != 'ORGANIC STATUS' & 
                             Domain != 'TOTAL')
chemical_ca <- filter(strawb, State == 'CALIFORNIA' & 
                        Domain != 'ORGANIC STATUS' & 
                        Domain != 'TOTAL')
grep("TOTAL",
     chemical_fl$`Domain Category`,
     ignore.case = T)
#chemical_fl contains 16 "TOTAL", which should be delete
unique(chemical_fl[11])
#contains 119
grep("TOTAL",
     chemical_ca$`Domain Category`,
     ignore.case = T)
#16
unique(chemical_ca[11])
#142
#ans_5 = (142-16) - (119-16) = 23





