### Dataset and packages loading ###

library(plm)
library(ggplot2)
library(tidyr)
library(tibble)
library(spatstat)
library(dplyr)
library(haven)
library(readr)
library(stargazer)
library(estimatr)
library(DiagrammeR)
library(texreg)
library(psych)
library(ivpack)
library(texreg)
library(REndo)
library(finalfit)

# Datasets

hours_commodities <- read_dta("~/Dissertation/Commodity Price Shocks DATA/Data/origmun_hours_commodities.dta")
migrant_commodities <- read_dta("~/Dissertation/Commodity Price Shocks DATA/Data/origmun_migrant_commodities.dta")
violence_commodities <- read_dta("~/Dissertation/Commodity Price Shocks DATA/Data/origmun_violence_commodities.dta")
wages_commodities <- read_dta("~/Dissertation/Commodity Price Shocks DATA/Data/origmun_wages_commodities.dta")
municipality_violence_commodities_online_appendix <- read_dta("~/Dissertation/Commodity Price Shocks DATA/App_Data/no_split_municipality_violence_commodities_for_online_appendix.dta")
violence_commodities_online_appendix <- read_dta("~/Dissertation/Commodity Price Shocks DATA/App_Data/origmun_violence_commodities_for_online_appendix.dta")

################################################################################################################################################################################################################################################

###Table 1, Key Statistics###

table1 <- stargazer(as.data.frame(violence_commodities,hours_commodities,wages_commodities), type = "text", 
                    omit = c("department_name","origmun_name","year","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002", "2003","2004","2005","region"),
                                      title = "Table 1 Descriptive Statistics Original Paper",
                                      out = "C:/Users/Owner/Documents/Dissertation/table1.html")

###############################################################################################################################################################################################################################################

### Table 2 Regression ###

###Guerilla Attack Regression with Robust SE's###

table2greg <- plm(gueratt ~ cofintxlinternalp + lpop + coca94indxyear + oilprod88xlop +as.factor(year)*as.factor(region) | 
    rxltop3cof*txltop3cof  + lpop + coca94indxyear + oilprod88xlop + as.factor(year)*as.factor(region),
  index=c('origmun','year'), data=violence_commodities, model='within') ## Includes interactions between year and region, rainfall and temperature
##Unable to cluster by department because of syntax errors
summary(table2greg)

#SE Clusters

table2gregVcov <- vcovHC(table2greg, type = "HC2")
table2gregRSE<-sqrt(diag(table2gregVcov))
##Since no clustering by department, robust standard errors by HC2 are done instead


###Paramilitary Attack Regression with Robust SE's###

table2preg <- plm(paratt ~ cofintxlinternalp + lpop + coca94indxyear + oilprod88xlop +as.factor(year)*as.factor(region) | 
                    rxltop3cof*txltop3cof  + lpop + coca94indxyear + oilprod88xlop +as.factor(year)*as.factor(region),
                  index=c('origmun','year'), data=violence_commodities, model='within')
summary(table2preg)
#SE Clusters

table2pregVcov <- vcovHC(table2preg, type = "HC2")
table2pregRSE <- sqrt(diag(table2pregVcov))



###Clashes Regression with Robust SE's###

table2clreg <- plm(clashes ~ cofintxlinternalp + lpop + coca94indxyear + oilprod88xlop +as.factor(year)*as.factor(region)| 
                    rxltop3cof*txltop3cof  + lpop + coca94indxyear + oilprod88xlop +as.factor(year)*as.factor(region),
                  index=c('origmun','year'), data=violence_commodities, model='within')
#SE Clusters

table2clregVcov <- vcovHC(table2clreg, type = "HC2")
table2clregRSE <- sqrt(diag(table2clregVcov))


###Casualties Regression with Robust SE's###

table2careg <- plm(casualties ~ cofintxlinternalp + lpop + coca94indxyear + oilprod88xlop +as.factor(year)*as.factor(region) | 
                    rxltop3cof*txltop3cof  + lpop + coca94indxyear + oilprod88xlop +as.factor(year)*as.factor(region),
                  index=c('origmun','year'), data=violence_commodities, model='within')

#SE Clusters

table2caregVcov <- vcovHC(table2careg, type = "HC2")
table2caregRSE <- sqrt(diag(table2caregVcov))



###Printed Regression###


         
stargazer(table2greg,table2preg,table2careg,table2clreg,
          se = list(table2gregRSE,table2pregRSE,table2caregRSE,table2clregRSE),
          omit = c("lpop","coca94indxyear","1989","1990","1991","1992",
                   "1993","1994","1995","1996","1997",
                   "1998","1999","2000","2001","2002",
                   "2003","2004","2005"),
          title = "Replicated Results of Table 2",
          dep.var.labels = c("Guerrilla Attacks","Paramilitary Attacks","Casualties","Clashes"),
          covariate.labels = c("Coffee int. x log coffee price","Oil production x log oil price"),
          out = "C:/Users/Owner/Documents/Dissertation/table2.html",
          type = "text")


#########################################################################################################################################################################


na.omit(violence_commodities)

### Table 3 Regression: Opportunity Cost and Rapacity Mechanism ###

###Log Wage Regression With Robust SE's###

table3lwreg <- multilevelIV(lwage ~ 
                              cofintxlinternalp+ oilprod88xlop+gender+age+agesq+married+edyrs+coca94indxyear +
                              as.factor(year)+as.factor(region) + rxltop3cof+txltop3cof +
                              (1|origmun) |   endo(rxltop3cof,txltop3cof),data=wages_commodities)


summary(table3lwreg)

#Robust SE's

table3lwregVcov <- vcovHC(table3lwreg,type = "HC2")
table3lwregRSE <- sqrt(diag(table3lwregVcov))

#P Values


table3lwregCOEF <- table3lwreg$coefficients
table3lwregtstat <- table3lwregCOEF/table3lwregRSE
table3lwregDOF <- table3lwreg$df.residual
table3lwregpvalues <- (pt(abs(table3lwregtstat),df=table3lwregDOF))
table3lwregpvalues

###Log Hours Regression With Robust SE's###

table3lhreg <- multilevelIV(lhours ~ 
                                 cofintxlinternalp+ oilprod88xlop+gender+age+agesq+married+edyrs+coca94indxyear +
                                 as.factor(year)+as.factor(region) + rxltop3cof+txltop3cof +
                                 (1|origmun) |   endo(rxltop3cof,txltop3cof),data=hours_commodities)



summary(table3lhreg)

#Robust SE's

table3lhregVcov <- vcovHC(table3lhreg, type = "HC2")
table3lhregRSE <- sqrt(diag(table3lhregVcov))


#P Values

table3lhregCOEF <- table3lhreg$coefficients
table3lhregtstat <- table3lhregCOEF/table3lhregRSE
table3lhregDOF <- table3lhreg$df.residual
table3lhregpvalues <- (pt(abs(table3lhregtstat),df=table3lhregDOF))
table3lhregpvalues


###Log Capital Revenue Regression with Robust SE's###


table3lcapreg <- multilevelIV(lcaprev ~ 
                                 cofintxlinternalp+ oilprod88xlop+lpop + coca94indxyear+
                                 as.factor(year)+as.factor(region) + rxltop3cof+txltop3cof +
                                 (1|origmun) |   endo(rxltop3cof,txltop3cof),data=violence_commodities)


summary(table3lcapreg)

#Robust SE's

table3lcapVcov <- vcovHC(table3lcapreg, type = "HC2")
table3lcapregRSE <- sqrt(diag(table3lcapVcov))

#P Values

table3lcapCOEF <- table3lcapreg$coefficients
table3lcapregtstat <- table3lcapCOEF/table3lcapregRSE
table3lcapregDOF <- table3lcapreg$df.residual
table3lcapregpvalues <- (pt(abs(table3lcapregtstat),df=table3lcapregDOF))
table3lcapregpvalues


###Paramilitary Kidnappings Regression with Robust SE's###

table3parkidreg <- plm(parkidpol ~ cofintxlinternalp+oilprod88xlop +lpop+as.factor(region)*as.factor(year)+coca94indxyear |
                            rxltop3cof*txltop3cof  + lpop +as.factor(region)*as.factor(year),
                          index=c('origmun','year'), data=violence_commodities, model='within')



#Robust SE's

table3parkidVcov <- vcovHC(table3parkidreg, type = "HC2")
table3parkidregRSE <- sqrt(diag(table3parkidVcov))


###Guerrilla Kidnapping Regression with Robust SE's###

table3guerkidreg <- plm(guerkidpol ~ cofintxlinternalp+oilprod88xlop +lpop+as.factor(region)*as.factor(year)+coca94indxyear |
                            rxltop3cof*txltop3cof  + lpop +as.factor(region)*as.factor(year),
                          index=c('origmun','year'), data=violence_commodities, model='within')


#Robust SE's

table3guerkidVcov <- vcovHC(table3guerkidreg, type = "HC2")
table3guerkidregRSE <- sqrt(diag(table3guerkidVcov))


###Printed Regression###


table3alt <- stargazer(table3lwreg,table3lhreg,table3lcapreg,
                       dep.var.labels = c("Log Wage","Log Hours","Log Capital Revenue"),
                       covariate.labels = c("Coffee int. x log coffee price","Oil production x log oil price","Gender","Age","Age Squared","Married",
                                            "Education (years)","Log Population","Region and Municipalities cultivating Coca"),
                       out = "C:/Users/Owner/Documents/Dissertation/table3.html",
                       type = "text",
                       title = "Table 3: Opportunity Cost and Rapacity Mechanism")


table3guerpar <- stargazer(table3parkidreg,table3guerkidreg,
          omit = c("lpop","coca94indxyear","1989","1990","1991","1992",
                   "1993","1994","1995","1996","1997",
                   "1998","1999","2000","2001","2002",
                   "2003","2004","2005"),
          se = list(table3parkidregRSE,table3guerkidregRSE),
          title = "Replicated Table 3 Paramilitary and Guerrilla Kidnappings",
          covariate.labels = c("Coffee int. x log coffee price","Oil production x log oil price"),
          dep.var.labels = c("Paramilitary Political Kidnappings", "Guerrilla Political Kidnappings"),
          out = "C:/Users/Owner/Documents/Dissertation/table3par&guer.html",
          type = 'text')




