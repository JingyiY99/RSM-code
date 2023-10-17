##PACKAGES
install.packages("emmeans")
library(emmeans)
install.packages("car")
library(car)
###functions
#pred_r_squared
pred_r_squared <- function(linear.model) {
  lm.anova <- anova(linear.model)
  tss <- sum(lm.anova$"Sum Sq")
  # predictive R^2
  pred.r.squared <- 1 - PRESS(linear.model)/(tss)
  return(pred.r.squared)
}
PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
  PRESS <- sum(pr^2)
  return(PRESS)
}
###DATA
MTG <- read.csv("C:/Users/15154/Desktop/ACS manu/MTG_10.16.23.csv",fileEncoding="UTF-8-BOM")
##### quadratic model(degree = 2)
RSL_R1 = lm(Resilience~MMF+TG+CLG+I(MMF^2)+I(TG^2)+I(CLG^2)+MMF:TG+MMF:CLG+TG:CLG, data = MTG)
#regression summary
summary(RSL_R1)
##Type III Anova
drop1(RSL_R1,~.,test="F")
## predictive R^2
pred.r.squared <- pred_r_squared(RSL_R1)
pred.r.squared


##### quadratic model(degree = 2)
GS_R1 = lm(Gel.Strength~MMF+TG+CLG+I(MMF^2)+I(TG^2)+I(CLG^2)+MMF:TG+MMF:CLG+TG:CLG, data = MTG)
#regression summary
summary(GS_R1)
##Type III Anova
drop1(GS_R1,~.,test="F")
## predictive R^2
pred.r.squared <- pred_r_squared(GS_R1)
pred.r.squared


##
SP_R1 = lm(Springiness~MMF+TG+CLG+I(MMF^2)+I(TG^2)+I(CLG^2)+MMF:TG+MMF:CLG+TG:CLG, data = MTG)
summary(SP_R1)
pred_r_squared(SP_R1)
##Type III Anova
drop1(SP_R1,~.,test="F")


##
CO_R1 = lm(Cohesiveness~MMF+TG+CLG+I(MMF^2)+I(TG^2)+I(CLG^2)+MMF:TG+MMF:CLG+TG:CLG, data = MTG)
summary(CO_R1)
pred_r_squared(CO_R1)
##Type III Anova
drop1(CO_R1,~.,test="F")

##
HD_R1 = lm(Hardness~MMF+TG+CLG+I(MMF^2)+I(TG^2)+I(CLG^2)+MMF:TG+MMF:CLG+TG:CLG, data = MTG)
summary(HD_R1)
##Type III Anova
drop1(HD_R1,~.,test="F")
pred_r_squared(HD_R1)



###desirability
install.packages("desirability", repos="http://R-Forge.R-project.org")
###
library(package = "lattice")
library(desirability)
###
GSPred <- function(x) 0.226 - 0.214 * x[1] - 0.081 * x[2] - 0.066 * x[3] + 
  0.174 * x[1]^2 + 0.092 * x[2]^2 +0.01 * x[3]^2 +0.02 * x[1] * x[2] + 
  0.006 * x[1] * x[3] + 0.003 * x[2] * x[3]
HDPred <- function(x) 412 - 252 * x[1] + 60 * x[2] - 116 * x[3] + 
  251 * x[1]^2 + 57 * x[2]^2 + 19 * x[3]^2 - 118 * x[1] * x[2] + 
  15 * x[1] * x[3] + 2 * x[2] * x[3]
SPPred <- function(x) 0.725 + 0.019 * x[1] + 0.416 * x[2] + 0.056 * x[3] - 
  0.064 * x[1]^2 - 0.327 * x[2]^2 - 0.009 * x[3]^2 + 0.06 * x[1] * x[2] - 
  0.001 * x[1] * x[3] + 0.004 * x[2] * x[3]
COPred <- function(x) 1.18 - 0.40 * x[1] - 0.66 * x[2] - 0.29 * x[3] + 
  0.34 * x[1]^2 + 0.44 * x[2]^2 + 0.045 * x[3]^2 + 0.04 * x[1] * x[2] - 
  0.003 * x[1] * x[3] + 0.02 * x[2] * x[3]
RSLPred <- function(x) 0.34 - 0.19 * x[1] + 0.15 * x[2] - 0.05 * x[3] - 
  0.07 * x[1]^2 - 0.09 * x[2]^2 + 0.01 * x[3]^2 - 0.03 * x[1] * x[2] - 
  0.03 * x[1] * x[3] + 0.01 * x[2] * x[3]
##
summary(MTG)
##
optim(c(0.1,0.1,1), HDPred,hessian = TRUE, method= "L-BFGS-B", lower = c(0.5, 0.1, 1),
      upper = c(1.0,0.11,5.0),
      control = list(fnscale = -1))

