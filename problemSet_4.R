myName <- "Yin Xu"

library(magrittr)
library(readr)
library(tidyr)
library(dplyr)

#1
print_order <- function(x){
  
  y <- c()  
  max <- max(x)
  min <- min(x)
  
  for (i in 1:3){
    
    if (x[i] == max){y[1] = x[i]}
    else if (x[i] == min){y[3] = x[i]}
    else{y[2] = x[i]}
    
  }
  return(y)
}

#2
print_string <- function(x){
  for (i in 1:x) {
    if (i %% 3 == 0 & i %% 5 == 0){
      print("Unknow")
    }else if (i %% 5 == 0){
      print("No")
    }else if (i %% 3 ==0){
      print("Yes")
    }else {
      print(i)
    }
  }
}

#3
factorlist <- NULL
calc_sum_of_factor <- function(x){
  for (i in 1:x) {
    if (x %% i == 0){
    factorlist <- append(factorlist, i)
    }
  }
  sum(sapply(factorlist**2, sum))
}
calc_sum_of_factor(12)

#4
intersect_list <- NULL
find_intersect <- function(x, y, z){
  for (i in x) {
    if (i %in% y & i %in% z){
      intersect_list <- append(intersect_list, i)
    }
  }
  print(intersect_list)
}

#5
ques_5 <- 1
factorial_base <- function(x){
  for (i in 1:x) {
    ques_5 <- ques_5 * i
  }
  print(ques_5)
}

#6
T_n <- function(n){
  ques_6 = n*(n+1)/2
  return(ques_6)
}

perfect_sqr <- function(x){
  sqrt <- sqrt(x)
  ifelse(sqrt == trunc(sqrt), return(TRUE),return(FALSE))
}

num_tri_sqr <- function(n){
  vec <- c()
  logic <- c()
  for( i in 1:n){
    vec[i] = T_n(i)
    logic[i] = perfect_sqr(T_n(i))
  }
  index <- which(logic == TRUE)
  return(vec[index])
}

q6_sum <- sum(num_tri_sqr(1500000))
q6_sum

##2022 H-1B Employer Data Hub
#7
h1b_2022 <- read_csv('https://www.uscis.gov/sites/default/files/document/data/h1b_datahubexport-2022.csv')

#8
na_num <- sum(is.na(h1b_2022))
na_num
h1b_2022a <- h1b_2022 %>% drop_na()
h1b_2022a <- h1b_2022a[h1b_2022a$City !="-",]

#9
df_num <-h1b_2022a %>% 
          group_by(State) %>% 
            summarise('Init App' = sum(`Initial Approval`) + sum(`Initial Denial`),
                      'Conti App' = sum(`Continuing Approval` + `Continuing Denial`),
                      Approve = sum(`Initial Approval`),
                      Denial = sum(`Initial Denial`))
df_num

#10
app_num <- sum(df_num$Approve)
den_num <- sum(df_num$Denial)

#11
city_num <- h1b_2022a %>% 
  select(3,10) %>% group_by(City) %>% 
  count(`Initial Approval`) %>% 
  arrange(City) %>% 
  transmute(Count=sum(n)) %>% 
  unique()

#12
visa_num <- h1b_2022a %>%
  group_by(NAICS) %>%
  arrange(NAICS) %>%
  count(NAICS) %>%
  transmute(Number=sum(n)) %>%
  unique()
visa_num$Percentage <- round(visa_num$Number *100 / sum(visa_num$Number), digits = 3)

    




