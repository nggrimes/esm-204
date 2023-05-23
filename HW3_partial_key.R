# HW 3, solution (partial, Spring 2023)
library(ggplot2)
library(dplyr)
rm(list=ls())
setwd("/Users/tammacarleton/Dropbox/Teaching/UCSB/ESM_204/materials_2023/homework/")

dat = read.csv("HW3_data.csv")

############ Q1 #############

# simple plot
ggplot(dat) + geom_point(aes(x=Q_DAC,y=Price_dollars), color="red") +
  geom_point(aes(x=Q_NonDAC,y=Price_dollars),color="blue") + xlab("Q") + ylab("P")

# coeffs
intD = coefficients(lm(dat$Price_dollars~dat$Q_DAC))[1]
intND = coefficients(lm(dat$Price_dollars~dat$Q_NonDAC))[1]
betaD = coefficients(lm(dat$Price_dollars~dat$Q_DAC))[2]
betaND = coefficients(lm(dat$Price_dollars~dat$Q_NonDAC))[2]

# plot low and high D
q = seq(0,80000,1000)
DACd = intD+betaD*q
NDACd = intND+betaND*q
sub = data.frame(cbind(q,DACd,NDACd))
ggplot(sub) + geom_line(aes(x=q,y=NDACd), color="red") +  geom_line(aes(x=q,y=DACd),color="blue") 

############ Q2 #############

mec = 63*300

############ Q3 #############

# agg demand, solve for q, sum q's, solve again for p
sub$AGGd = ((betaD*betaND)/(betaD+betaND))*(q+(intD/betaD)+(intND/betaND))
sub$AGGd = ifelse(sub$NDACd>sub$AGGd,sub$NDACd,sub$AGGd)

# Drop negatives
sub$NDACd = ifelse(sub$NDACd<0,NA,sub$NDACd)
sub$DACd = ifelse(sub$DACd<0,NA,sub$DACd)
sub$AGGd = ifelse(sub$AGGd<0,NA,sub$AGGd)

ggplot(sub) + geom_line(aes(x=q,y=NDACd), color="red") +  geom_line(aes(x=q,y=DACd),color="blue") + 
  geom_line(aes(x=q,y=AGGd),color="black") 

# supply curve
q0 = 50000*(1/betaD + 1/betaND) - (intD/betaD) - (intND/betaND)
q0

# plot it 
sub$supply = (50000/q0)*q
ggplot(sub) + geom_line(aes(x=q,y=NDACd), color="red") +  geom_line(aes(x=q,y=DACd),color="blue") + 
  geom_line(aes(x=q,y=AGGd),color="black") + geom_line(aes(x=q,y=supply),color="black") 

# env cost = number of cars times externality
envcost0 = q0*mec
envcost0

############ Q4 #############

############ Q5 #############
sub$msc = sub$supply+mec
ggplot(sub) + geom_line(aes(x=q,y=NDACd), color="red") +  geom_line(aes(x=q,y=DACd),color="blue") + 
  geom_line(aes(x=q,y=AGGd),color="black") + geom_line(aes(x=q,y=supply),color="black") +
  geom_line(aes(x=q,y=msc),color="black", linetype="dashed") 

