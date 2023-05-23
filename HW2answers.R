rm(list = ls(all = TRUE)) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)   # Data visualization
library(tidyr)   # Tidy data management
library(dplyr)
library(cowplot)

DF = read.csv(file="CarbonUC.csv")

model_UCSB = lm(UCSB ~ 0 + Abatement,data=DF)
model_UCLA = lm(UCLA ~ 0 + Abatement,data=DF)
model_UCI = lm(UCI ~ 0 + Abatement,data=DF)
model_UCD = lm(UCD ~ 0 + Abatement,data=DF)

#Question 1
a_UCSB = model_UCSB$coefficients
a_UCLA = model_UCLA$coefficients
a_UCI = model_UCI$coefficients
a_UCD = model_UCD$coefficients

B_UCSB = 90000
B_UCLA = 250000
B_UCI = 100000
B_UCD = 160000

#Question 2
P_UCSB = function(E)
{
  P = a_UCSB*(B_UCSB - E)
  return(P)
}
P_UCLA = function(E)
{
  P = a_UCLA*(B_UCLA - E)
  return(P)
}
P_UCI = function(E)
{
  P = a_UCI*(B_UCI - E)
  return(P)
}
P_UCD = function(E)
{
  P = a_UCD*(B_UCD - E)
  return(P)
}

DF2 = data.frame(E=seq(0,250000,length.out=100)) %>%
  mutate(price_UCSB = pmax(0,P_UCSB(E))) %>%
  mutate(price_UCLA = pmax(0,P_UCLA(E))) %>%
  mutate(price_UCI = pmax(0,P_UCI(E))) %>%
  mutate(price_UCD = pmax(0,P_UCD(E))) 

P2 = ggplot(data=DF2) +
  geom_line(aes(x=E,y=price_UCSB),color="green") + 
  geom_line(aes(x=E,y=price_UCLA),color="red") + 
  geom_line(aes(x=E,y=price_UCI),color="orange") + 
  geom_line(aes(x=E,y=price_UCD),color="blue") 
P2

#Question 3a
DF3a = data.frame(P=50) %>%
  mutate(Emissions_UCSB = B_UCSB - P/a_UCSB) %>%
  mutate(Abatement_UCSB = B_UCSB - Emissions_UCSB) %>%
  mutate(Emissions_UCLA = B_UCLA - P/a_UCLA) %>%
  mutate(Abatement_UCLA = B_UCLA - Emissions_UCLA) %>%
  mutate(Emissions_UCI = B_UCI - P/a_UCI) %>%
  mutate(Abatement_UCI = B_UCI - Emissions_UCI) %>%
  mutate(Emissions_UCD = B_UCD - P/a_UCD) %>%
  mutate(Abatement_UCD = B_UCD - Emissions_UCD) %>%
  mutate(Emissions_TOTAL = Emissions_UCSB + Emissions_UCLA + Emissions_UCI + Emissions_UCD) %>%
  mutate(Tax_Revenue = Emissions_TOTAL*P)

#Question 3b
DF3b = data.frame(Ban_UCSB = .5*B_UCSB*(a_UCSB*B_UCSB)) %>%
  mutate(Ban_UCLA = .5*B_UCLA*(a_UCLA*B_UCLA)) %>%
  mutate(Ban_UCI = .5*B_UCI*(a_UCI*B_UCI)) %>%
  mutate(Ban_UCD = .5*B_UCD*(a_UCD*B_UCD))
  
#Question 3c
DF3c = data.frame(Cap=100000) %>%
  mutate(Abate_UCSB = max(0,B_UCSB-Cap)) %>%
  mutate(Abate_UCLA = max(0,B_UCLA-Cap)) %>%
  mutate(Abate_UCI = max(0,B_UCI-Cap)) %>%
  mutate(Abate_UCD = max(0,B_UCD-Cap)) %>%
  mutate(MC_UCSB = max(0,P_UCSB(Cap))) %>%
  mutate(MC_UCLA = max(0,P_UCLA(Cap))) %>%
  mutate(MC_UCI = max(0,P_UCI(Cap))) %>%
  mutate(MC_UCD = max(0,P_UCD(Cap))) %>%
  mutate(TC_UCSB = .5*Abate_UCSB^2*a_UCSB) %>%
  mutate(TC_UCLA = .5*Abate_UCLA^2*a_UCLA) %>%
  mutate(TC_UCI = .5*Abate_UCI^2*a_UCI) %>%
  mutate(TC_UCD = .5*Abate_UCD^2*a_UCD) 

#Question 3d
DF2new = data.frame(P=seq(0,500,length.out=100)) %>%
  mutate(E_UCSB = pmax(0,B_UCSB - P/a_UCSB)) %>%
  mutate(E_UCLA = pmax(0,B_UCLA - P/a_UCLA)) %>%
  mutate(E_UCI = pmax(0,B_UCI - P/a_UCI)) %>%
  mutate(E_UCD = pmax(0,B_UCD - P/a_UCD)) %>%
  mutate(E_Agg = E_UCSB+E_UCLA+E_UCI+E_UCD)


Pstar = spline(x=DF2new$E_Agg,y=DF2new$P,xout=4e5)$y

P3d = ggplot(data=DF2new) +
  geom_line(aes(x=E_UCSB,y=P),color="green") + 
  geom_line(aes(x=E_UCLA,y=P),color="red") + 
  geom_line(aes(x=E_UCI,y=P),color="orange") + 
  geom_line(aes(x=E_UCD,y=P),color="blue") +
  geom_line(aes(x=E_Agg,y=P),color="purple",size=2) +
  geom_segment(aes(x=0,xend=4e5,y=Pstar,yend=Pstar),size=2)
P3d

DF3d = data.frame(P=Pstar) %>%
  mutate(E_UCSB = pmax(0,B_UCSB - P/a_UCSB)) %>%
  mutate(E_UCLA = pmax(0,B_UCLA - P/a_UCLA)) %>%
  mutate(E_UCI = pmax(0,B_UCI - P/a_UCI)) %>%
  mutate(E_UCD = pmax(0,B_UCD - P/a_UCD)) %>%
  mutate(E_Agg = E_UCSB+E_UCLA+E_UCI+E_UCD)


  
  
  
  
  

