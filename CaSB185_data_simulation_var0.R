final_DF_v1 <- data.frame(matrix(ncol = 24, nrow = 0))
final_DF_v2 <- data.frame(matrix(ncol = 24, nrow = 0))
final_DF_v3 <- data.frame(matrix(ncol = 24, nrow = 0))
tableHeadings <- c("ICU", "Age", "Sex", "Survival Probability", "Score", "Kidney", "Blood", "Cholesterol", "Immune", "ApacheII", "CHM", "Urine Volume", "Creatinine", "Urea", "O2", "HR", "BP", "RBC", "LDL", "HDL", "Platelet", "WBC", "INR", "Death")
colnames(final_DF_v3)  <- tableHeadings



Select_Age_Gp <- function()
{
  x <- as.integer(sample(c(1:10), size = 1, prob=c(0.076, 0.071,0.128,0.208,0.121,0.105,0.118,0.103,0.054,0.016)))
  return(x)
  #age <- switch(x, sample(c(18:34), size = 1), sample(c(35:44), size = 1),sample(c(45:54), size = 1),sample(c(55:64), size = 1),sample(c(65:69), size = 1), sample(c(70:74), size = 1), sample(c(75:79), size = 1), sample(c(80:84), size = 1),sample(c(85:89), size = 1),sample(c(90:100), size = 1)) 
  #return(age)
}

Select_Gender <- function()
{
  return(sample(c("M", "F"), size = 1, prob = c(0.599,0.401)))
}

Select_H_L <- function()
{
  return(as.integer(sample(c(1, 2), size = 1, prob = c(0.5,0.5))))
}

v_h <- function(i)
{
  return(Feature_Health[[i,4]])
}

v_l <- function(i)
{
  return(Feature_Health[[i,2]])
}

v_o <- function(i)
{
  return(Feature_Health[[i,3]])
}

truncate <- function(x)
{
  if (x < 0)
    return(0)
  if (x > 1)
    return(1)
  else
    return(x)
}


mortality_ICU <- data.frame(matrix(ncol = 2, nrow = 10))
tableHeadings <- c("Age", "Mortality")
colnames(mortality_ICU) <- tableHeadings
mortality_ICU$Age <- c(1:10)
mortality_ICU$Mortality[[1]] <- 9307/10122
mortality_ICU$Mortality[[2]] <- 8148/9514
mortality_ICU$Mortality[[3]] <- 13425/17120
mortality_ICU$Mortality[[4]] <- 20253/27899
mortality_ICU$Mortality[[5]] <- 11236/16144
mortality_ICU$Mortality[[6]] <- 9381/14130
mortality_ICU$Mortality[[7]] <- 9878/15754
mortality_ICU$Mortality[[8]] <- 7813/13820
mortality_ICU$Mortality[[9]] <- 3478/7280
mortality_ICU$Mortality[[10]] <- 858/2183


Feature_Health <-  data.frame(matrix(ncol = 4, nrow = 13))
tableHeadings <- c("Features", "Low", "Optimal", "High")
colnames(Feature_Health) <- tableHeadings
Feature_Health$Features <- c("ApacheII",  "Urine", "Creatinine", "Urea", "Oxygen", "HR", "BP", "RBC", "LDL", "HDL", "Platelet", "WBC", "INR")
Feature_Health$Low <- c(-1000,  50, 0.5, 7, 95, 60, 60,4.2, -1100, 35, 20000, 4000, -1000)
Feature_Health$Optimal <- c(0,  1400, 0.9, 13.5, 100, 80,85, 5.15, 100, 60, 253000, 7500, 1.1)
Feature_Health$High <- c(30, 2500, 5, 60, 10000, 100, 100, 6.1, 160, 95, 450000, 10500, 8)
Feature_Health<-na.omit(Feature_Health)

N <- 100
for (i in 1:N)
{
  n <- log(9)/log(2)
  Age_Gp <- Select_Age_Gp()
  Age <- switch(Age_Gp, sample(c(18:34), size = 1), sample(c(35:44), size = 1),sample(c(45:54), size = 1),sample(c(55:64), size = 1),sample(c(65:69), size = 1), sample(c(70:74), size = 1), sample(c(75:79), size = 1), sample(c(80:84), size = 1),sample(c(85:89), size = 1),sample(c(90:100), size = 1)) 
  Sex <- Select_Gender()
  Health <- rnorm(1, mortality_ICU$Mortality[Age_Gp] , 0.0) #number of samples, mean, standard deviation
  Health <- truncate(Health)
  
  #0 indicates that the patient survived, 1 indicates that the patient dies
  death <- as.integer(sample(c(0, 1), size = 1, prob = c(Health,1-Health)))
  
  Scores <- truncate(rnorm(1, Health, 0.0))
  Kidney <- truncate(rnorm(1, Health, 0.0)) 
  Blood <- truncate(rnorm(1, Health, 0.0))
  Cholesterol <- truncate(rnorm(1, Health, 0.0))
  Immune <- truncate(rnorm(1, Health, 0.0))
  
  
  j = 1
  mu_H <- (((1-Scores)/Scores)^(1/n))*(v_h(j) - v_o(j)) + v_o(j)
  ApacheII <- rnorm(1, mu_H, 0.0)
  
  while (ApacheII <0) {
    ApacheII <- rnorm(1, mu_H, 0.0)
  }
  
  j = 2
  H_L <- as.integer(Select_H_L())
  mu_H <- (((1-Kidney)/Kidney)^(1/n))*(v_h(j) - v_o(j)) + v_o(j)
  mu_L <-  v_o(j) - (((1-Kidney)/Kidney)^(1/n))*(v_o(j) - v_l(j))
  Urine <- switch(H_L, rnorm(1, mu_H, 0.0), rnorm(1, mu_L, 0.0))
  while (Urine <0) {
    Urine <- switch(H_L, rnorm(1, mu_H, 0.0), rnorm(1, mu_L, 0.0))
  }
  
  j = 3
  H_L <- as.integer(Select_H_L())
  mu_H <- (((1-Kidney)/Kidney)^(1/n))*(v_h(j) - v_o(j)) + v_o(j)
  mu_L <-  v_o(j) - (((1-Kidney)/Kidney)^(1/n))*(v_o(j) - v_l(j))
  Creatinine <- switch(H_L, rnorm(1, mu_H, 0.0), rnorm(1, mu_L, 0.0))
  while (Creatinine <0) {
    Creatinine <- switch(H_L, rnorm(1, mu_H, 0.0), rnorm(1, mu_L, 0.0))
  }
  
  j = 4
  H_L <- as.integer(Select_H_L())
  mu_H <- (((1-Kidney)/Kidney)^(1/n))*(v_h(j) - v_o(j)) + v_o(j)
  mu_L <-  v_o(j) - (((1-Kidney)/Kidney)^(1/n))*(v_o(j) - v_l(j))
  Urea <- switch(H_L, rnorm(1, mu_H, 0.0), rnorm(1, mu_L, 0.0))
  while (Urea <0) {
    Urea <- switch(H_L, rnorm(1, mu_H, 0.0), rnorm(1, mu_L, 0.0))
  }
  
  j = 5
  mu_L <-  v_o(j) - (((1-Blood)/Blood)^(1/n))*(v_o(j) - v_l(j))
  Oxygen <- rnorm(1, mu_L, 0.0)
  while (Oxygen <0 || Oxygen >100) {
    Oxygen <- rnorm(1, mu_L, 0.0)
  }
  
  j = 6
  H_L <- as.integer(Select_H_L())
  mu_H <- (((1-Blood)/Blood)^(1/n))*(v_h(j) - v_o(j)) + v_o(j)
  mu_L <-  v_o(j) - (((1-Blood)/Blood)^(1/n))*(v_o(j) - v_l(j))
  HR <- switch(H_L, rnorm(1, mu_H, 0.0), rnorm(1, mu_L, 0.0))
  while (HR <0) {
    HR <- switch(H_L, rnorm(1, mu_H, 0.0), rnorm(1, mu_L, 0.0))
  }
  
  j = 7
  H_L <- as.integer(Select_H_L())
  mu_H <- (((1-Blood)/Blood)^(1/n))*(v_h(j) - v_o(j)) + v_o(j)
  mu_L <-  v_o(j) - (((1-Blood)/Blood)^(1/n))*(v_o(j) - v_l(j))
  BP <- switch(H_L, rnorm(1, mu_H, 0.0), rnorm(1, mu_L, 0.0))
  while (BP <0) {
    BP <- switch(H_L, rnorm(1, mu_H, 0.0), rnorm(1, mu_L, 0.0))
  }
  
  j = 8
  H_L <- as.integer(Select_H_L())
  mu_H <- (((1-Blood)/Blood)^(1/n))*(v_h(j) - v_o(j)) + v_o(j)
  mu_L <-  v_o(j) - (((1-Blood)/Blood)^(1/n))*(v_o(j) - v_l(j))
  RBC <- switch(H_L, rnorm(1, mu_H, 0.0), rnorm(1, mu_L, 0.0))
  while (RBC <0) {
    RBC <- switch(H_L, rnorm(1, mu_H, 0.0), rnorm(1, mu_L, 0.0))
  }
  
  j = 9
  mu_H <- (((1-Cholesterol)/Cholesterol)^(1/n))*(v_h(j) - v_o(j)) + v_o(j)
  LDL <- rnorm(1, mu_H, 0.0)
  while (LDL <0) {
    LDL <- rnorm(1, mu_H, 0.0)
  }
  
  j = 10
  H_L <- as.integer(Select_H_L())
  mu_H <- (((1-Cholesterol)/Cholesterol)^(1/n))*(v_h(j) - v_o(j)) + v_o(j)
  mu_L <-  v_o(j) - (((1-Cholesterol)/Cholesterol)^(1/n))*(v_o(j) - v_l(j))
  HDL <- switch(H_L, rnorm(1, mu_H, 0.0), rnorm(1, mu_L, 0.0))
  while (HDL <0) {
    HDL <- switch(H_L, rnorm(1, mu_H, 0.0), rnorm(1, mu_L, 0.0))
  }
  
  j = 11
  H_L <- as.integer(sample(c(1, 2), size = 1, prob = c(0.9,0.1)))
  mu_H <- (((1-Immune)/Immune)^(1/n))*(v_h(j) - v_o(j)) + v_o(j)
  mu_L <-  v_o(j) - (((1-Immune)/Immune)^(1/n))*(v_o(j) - v_l(j))
  Platelet <- switch(H_L, rnorm(1, mu_H, 0.0), rnorm(1, mu_L, 0.0))
  while (Platelet <0) {
    Platelet <- switch(H_L, rnorm(1, mu_H, 0.0), rnorm(1, mu_L, 0.0))
  }
  
  j = 12
  H_L <- as.integer(sample(c(1, 2), size = 1, prob = c(0.5,0.5)))
  mu_H <- (((1-Immune)/Immune)^(1/n))*(v_h(j) - v_o(j)) + v_o(j)
  mu_L <-  v_o(j) - (((1-Immune)/Immune)^(1/n))*(v_o(j) - v_l(j))
  WBC <- switch(H_L, rnorm(1, mu_H, 0.0), rnorm(1, mu_L, 0.0))
  while (WBC <0) {
    WBC <- switch(H_L, rnorm(1, mu_H, 0.0), rnorm(1, mu_L, 0.0))
  }
  
  j = 13
  mu_H <- (((1-Immune)/Immune)^(1/n))*(v_h(j) - v_o(j)) + v_o(j)
  INR <- rnorm(1, mu_H, 0.0)
  while (INR <0) {
    INR <- rnorm(1, mu_H, 0.0)
  }
  
  
  newRow <- c(1,Age,Sex,Health,Scores,Kidney,Blood,Cholesterol,Immune,ApacheII,NA,Urine,Creatinine,Urea,Oxygen,HR,BP,RBC,LDL,HDL,Platelet,WBC,INR, death) 
  final_DF_v3[nrow(final_DF_v3)+1,] <- newRow
}

