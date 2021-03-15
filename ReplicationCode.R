### Datasets Used ###

colombiadatanew <- read_csv("colombiadatanew.csv")
colombiadatanew <- na.omit(colombiadatanew)


### Packages ###

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
library(fastDummies)


### Creating Region and Year dummy's plus interactions ###



colombiadatanew <- dummy_cols(colombiadatanew,select_columns=c('year','region')) %>% 
  mutate_at(vars(starts_with('year_')),list(region_1=~./region_1,region_2=~./region_2,
                                            region_3=~./region_3,region_4=~./region_4))

write.csv(colombiadatanew, file = "C:/Users/Owner/Documents/Dissertation/Dissertation Code/colombiadatanew.csv")

################################################################################################################################################################################################################################################

###Table 1, Key Statistics###


table1extended <- stargazer(as.data.frame(colombiadatanew), type = "text", omit = c("1989","1990","1991","1992",
                                                                  "2006","2007","2008","2009","2010",
                                                                  "2011","2012","2013","2014","2015",
                                                                  "2016","2017","2018","region","X1"),
          title = "Table 1 Descriptive Statistics for Extended time Series",
          out = "C:/Users/Owner/Documents/Dissertation/table1extended.html")

##############################################################################################################################################################


### Table 2 Regression ###

###Guerilla Attack Regression with Robust SE's###

table2greg <- plm(gueratt ~ cofintxlinternalp + lpop + coca94indxyear + oilprod88xlop +as.factor(year)*as.factor(region) | 
                    rxltop3cof*txltop3cof  + lpop + coca94indxyear + oilprod88xlop + as.factor(year)*as.factor(region),
                  index=c('origmun','year'), data=colombiadatanew, model='within') ## Includes interactions between year and region, rainfall and temperature
##Unable to cluster by department because of syntax errors
summary(table2greg)

#SE Clusters

table2gregVcov <- vcovHC(table2greg, type = "HC2")
table2gregRSE<-sqrt(diag(table2gregVcov))
##Since no clustering by department, robust standard errors by HC2 are done instead

##########################################################################################################################################################
############################Only one paramilitary attack across the whole sample therefore excluded from regression#######################################
##########################################################################################################################################################

###Paramilitary Attack Regression with Robust SE's###

table2preg <- plm(paratt ~ cofintxlinternalp + lpop + coca94indxyear + oilprod88xlop +as.factor(year)*as.factor(region) | 
                    rxltop3cof*txltop3cof  + lpop + coca94indxyear + oilprod88xlop +as.factor(year)*as.factor(region),
                  index=c('origmun','year'), data=colombiadatanew, model='within')

#SE Clusters

table2pregVcov <- vcovHC(table2preg, type = "HC2")
table2pregRSE <- sqrt(diag(table2pregVcov))


#########################################################################################################################################################

###Clashes Regression with Robust SE's###

table2clreg <- plm(clashes ~ cofintxlinternalp + lpop + coca94indxyear + oilprod88xlop +as.factor(year)*as.factor(region)| 
                     rxltop3cof*txltop3cof  + lpop + coca94indxyear + oilprod88xlop +as.factor(year)*as.factor(region),
                   index=c('origmun','year'), data=colombiadatanew, model='within')
#SE Clusters

table2clregVcov <- vcovHC(table2clreg, type = "HC2")
table2clregRSE <- sqrt(diag(table2clregVcov))

###Casualties Regression with Robust SE's###

table2careg <- plm(casualties ~ cofintxlinternalp + lpop + coca94indxyear + oilprod88xlop +as.factor(year)*as.factor(region) | 
                     rxltop3cof*txltop3cof  + lpop + coca94indxyear + oilprod88xlop +as.factor(year)*as.factor(region),
                   index=c('origmun','year'), data=colombiadatanew, model='within')

#SE Clusters

table2caregVcov <- vcovHC(table2careg, type = "HC2")
table2caregRSE <- sqrt(diag(table2caregVcov))


###Printed Regression###



stargazer(table2greg,table2careg,table2clreg,
          se = list(table2gregRSE,table2caregRSE,table2clregRSE),
          omit = c("lpop","coca94indxyear","1989","1990","1991","1992",
                   "2006","2007","2008","2009","2010",
                   "2011","2012","2013","2014","2015",
                   "2016","2017","2018","region"),
          notes = "Paramilitary attacks model removed from regression due as there is only one observation in time series",
          title = "Results of Table 2 with extended time series",
          dep.var.labels = c("Guerrilla Attacks","Casualties","Clashes"),
          covariate.labels = c("Coffee int. x log coffee price","Oil production x log oil price"),
          out = "C:/Users/Owner/Documents/Dissertation/table2new.html",
          type = "text")
