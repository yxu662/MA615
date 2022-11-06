myName <- "Yin Xu"

#Vector

#1
v1 <- 1:20
v2 <- 20:1
v3 <- seq(1,19,by = 2)
v4 <- rep(seq(3,11,by = 4),10)
v5 <- c(rep(seq(3,11,by = 4),10),3)

#2
x <- seq(3.0,6.0,by = 0.1)
x1 <- exp(x)*sin(x)

#3
i <- 10:100
sum1 <- sum(i^3+4*i^2)

#4
str1 <- paste(rep("label",30),1:30)
str2 <- paste0(rep("function",30),1:30)

#5
vs <- c(1,'function',NA, seq(1,5,2), 0.125)
vs <- paste(vs, collapse = ",")

#Matrix

#6
"%^%" = function(a,n){
  for (i in 1:n) {
    if (i == 1) {
      b = a}
    else{
      b = a %*% b
    }
  }
  return = b
}
A <- matrix(1:9, nrow = 3)
m1_ans <- A %^% 3

#7
B <- matrix(rep(c(12,-12,12),17),ncol = 3, byrow = T)
m2_ans <- t(B)%*%B
m2_ans

#8
A <- matrix(c(1:5,c(2,1:4),c(3:2,1:3),c(4:1,2),c(5:1)), nrow = 5)
y <- c(7,-1,-3,5,17)
m3_ans <- solve(A,y)


#Function

#9

#a)
xv <- seq(0, 1, length.out = 11)
function1 <- function(x,n){
  x^(1:n)
}
func1_ans <- function1(xv,length(xv))

#b)
function2 <- function(x,n){
  (x^(1:n))/(1:n)
}
func2_ans <- function2(xv,length(xv))

#c)
function3 <- function(x,n){
  1+sum((x^(1:n))/(1:n))
}
func3_ans <- function3(xv,length(xv))


#10
cel_to_far <- function(x){
  x*1.8+32
}
far_to_cel <- function(x){
  (x-32)/1.8
}

#11
odd_ans <- function(x){
  for (x in 1:2000) {
    if (x %% 2 == 1)
      print(x)
  }
}

#12
sum_ans <- function(r){
  for (s in 1:r) {
    sum(s^0.5/(11+3.5*r^1.2))
  }
}
sapply(1:10,sum_ans)

#13
modNumber <- function(x,y){
  if (x %% y == 0){
   return(x)
  }
  else{
    output <- y*ceiling(x/y)
    return(output)
  }
}

#14
numberOfWheels <- function(x){
  switch(x, "unicycle" = 1, "bike" = 2, "car" = 4,
                         "truck" = 4, "tricycle" = 3, "motorcycle" = 2)
}
numberOfWheels("bike")

#15
myFactorial <- function(x){
  factorial(x)
}
myFactorial(10)

#16
myCustomFactorial <- function(x,y){
  factorial(y)/factorial(x-1)
}
myCustomFactorial(1,10)

#17
data.frame(rivers)
customRiverMean <- function(max_length){
  y <- rivers[rivers < max_length]
  output <- mean(y)
  return(output)
}

#18
for (i in 60){
  longteeth1 <- ToothGrowth$len >= 15
  longTeeth <- ToothGrowth[longteeth1,]$len
  print(longTeeth)
}

#19
apply(mtcars, 2, mean)
averageHousePower <- c(mean(mtcars$hp))
averageHousePower
averageWeight <- c(mean(mtcars$wt))
averageWeight

#20



