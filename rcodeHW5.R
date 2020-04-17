#Part 1
# EDA analysis of the data 

#Read ski.xlxs data into skiData
#Delete first cloumn

install.packages("readxl")
library(readxl)
ski=read_excel("ski.xlsx")
skiData=ski[-1]
skiData
#install required packages and libraries

install.packages("funModeling")
install.packages("tidyverse")
install.packages("Hmisc")
install.packages("DataExplorer")
library(funModeling)
library(tidyverse)
library(Hmisc)
library(DataExplorer)

# Plot data

plot_str(ski)   #show data structure
glimpse(ski)    #show data overview
plot_missing(skiData)  #show missing data
profiling_num(skiData)


####Univariate stats
describe(skiData)
plot_num(skiData)     #plot histogram of all numeric data
plot_density(skiData) #show density plot of all data

#Boxplot
boxplot(skiData)   #boxplot of all data


####Multivariate stats
install.packages("ggplot2")
library(ggplot2)

plot_correlation(skiData)  #show correlation in the data

#scatter plots with features with Selling price
ggplot(data = ski) +
  geom_point(mapping = aes(x = Sq_Ft, y = `Selling price`))

ggplot(data = ski) +
  geom_point(mapping = aes(x = Bedrooms, y = `Selling price`))

ggplot(data = ski) +
  geom_point(mapping = aes(x = Bathrooms, y = `Selling price`))

ggplot(data = ski) +
  geom_point(mapping = aes(x =`List price`, y = `Selling price`))

ggplot(data = ski) +
  geom_point(mapping = aes(x = Mountain, y = `Selling price`))

ggplot(data = ski) +
  geom_point(mapping = aes(x = Downtwon, y = `Selling price`))

ggplot(data = ski) +
  geom_point(mapping = aes(x = `Lot size`, y = `Selling price`))

ggplot(data = ski) +
  geom_point(mapping = aes(x = Garage, y = `Selling price`))

pairs(skiData)  #scatter plot of all the data


#correlation matrix
corMatrix= cor(skiData)   #correlation matrix

install.packages("Hmisc")
library(Hmisc)
corMatrix.rcorr= rcorr(as.matrix(skiData))
corMatrix.rcorr
corMatrix.rcorr$P       #P values
corMatrix.rcorr$r       #correlation coefficients

install.packages("corrplot")
library(corrplot)
corrplot(corMatrix)     #visualizing the correlation matrix


#Heatmap
heatmap(corMatrix,symm=TRUE)

#correlation coefficients for Selling Price against all variables

cor(skiData[-10],skiData$`Selling price`)


"""
> cor(skiData[-10],skiData$`Selling price`)
                  [,1]
Bedrooms    0.61097980
Bathrooms   0.74268187
Sq_Ft       0.63714824
Downtwon   -0.27076285
Mountain   -0.40217178
Lot size   -0.04422749
Garage      0.32672161
Age        -0.19714627
On market   0.11046973
List price  0.98992618

"""




############ Analysis per Variable Vs Selling Price

library(dplyr)

#Selling price per bedroom

data = mutate(skiData, Selling_price_per_bedroom =skiData$`Selling price`/skiData$Bedrooms )
median(data$Selling_price_per_bedroom)

#Selling price per bathroom
data =  mutate(skiData, Selling_price_per_bathroom =skiData$`Selling price`/skiData$Bathrooms )
median(data$Selling_price_per_bathroom)

#Selling price per Sq_Ft
data =  mutate(skiData, Selling_price_per_sqft =skiData$`Selling price`/skiData$Sq_Ft )
median(data$Selling_price_per_sqft)

#Selling price per Downtwon
data =  mutate(skiData, Selling_price_per_dwntn =skiData$`Selling price`/skiData$Downtwon )
median(data$Selling_price_per_dwntn)

#Selling price per Mountain
data =  mutate(skiData, Selling_price_per_mntn =skiData$`Selling price`/skiData$Mountain )
median(data$Selling_price_per_mntn)

#Selling price per Lot size
data =  mutate(skiData, Selling_price_per_lotsize =skiData$`Selling price`/skiData$`Lot size` )
median(data$Selling_price_per_lotsize)

#Selling price per Garage
data =  mutate(skiData, Selling_price_per_garage =skiData$`Selling price`/skiData$Garage )
median(data$Selling_price_per_garage)

#Selling price per Age
data =  mutate(skiData, Selling_price_per_age =skiData$`Selling price`/skiData$Age )
median(data$Selling_price_per_age)

#Selling price per On Martket
data =  mutate(skiData, Selling_price_per_onMarket =skiData$`Selling price`/skiData$`On market` )
median(data$Selling_price_per_onMarket)

#Selling price per listPrice
data =  mutate(skiData, Selling_price_per_listPrice =skiData$`Selling price`/skiData$`List price` )
median(data$Selling_price_per_listPrice)

"""
> #Selling price per bedroom
> data = mutate(skiData, Selling_price_per_bedroom =skiData$`Selling price`/skiData$Bedrooms )
> median(data$Selling_price_per_bedroom)
[1] 121.6667

> #Selling price per bathroom
> data =  mutate(skiData, Selling_price_per_bathroom =skiData$`Selling price`/skiData$Bathrooms )
> median(data$Selling_price_per_bathroom)
[1] 174.8

> #Selling price per Sq_Ft
> data =  mutate(skiData, Selling_price_per_sqft =skiData$`Selling price`/skiData$Sq_Ft )
> median(data$Selling_price_per_sqft)
[1] 0.214207

> #Selling price per Downtwon
> data =  mutate(skiData, Selling_price_per_dwntn =skiData$`Selling price`/skiData$Downtwon )
> median(data$Selling_price_per_dwntn)
[1] 75.71429

> #Selling price per Mountain
> data =  mutate(skiData, Selling_price_per_mntn =skiData$`Selling price`/skiData$Mountain )
> median(data$Selling_price_per_mntn)
[1] 67.14286

> #Selling price per Lot size
> data =  mutate(skiData, Selling_price_per_lotsize =skiData$`Selling price`/skiData$`Lot size` )
> median(data$Selling_price_per_lotsize)
[1] 1106.061

> #Selling price per Garage
> data =  mutate(skiData, Selling_price_per_garage =skiData$`Selling price`/skiData$Garage )
> median(data$Selling_price_per_garage)
[1] 228.75

> #Selling price per Age
> data =  mutate(skiData, Selling_price_per_age =skiData$`Selling price`/skiData$Age )
> median(data$Selling_price_per_age)
[1] 24.33333

> #Selling price per On Martket
> data =  mutate(skiData, Selling_price_per_onMarket =skiData$`Selling price`/skiData$`On market` )
> median(data$Selling_price_per_onMarket)
[1] 3.8125

> #Selling price per listPrice
> data =  mutate(skiData, Selling_price_per_listPrice =skiData$`Selling price`/skiData$`List price` )
> median(data$Selling_price_per_listPrice)
[1] 0.9798489
> 
"""

##############################################################################################

#Regression Analysis


linear_model = lm(sp ~ lp,data=skiData)
summary(linear_model)
anova(linear_model)

"""
> linear_model = lm(sp ~ lp,data=skiData)
> summary(linear_model)

Call:
lm(formula = sp ~ lp, data = skiData)

Residuals:
    Min      1Q  Median      3Q     Max 
-28.814  -4.063   2.367   5.665  10.719 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -8.11486    9.92229  -0.818    0.419    
lp           0.99433    0.02338  42.529   <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 8.437 on 37 degrees of freedom
Multiple R-squared:   0.98,	Adjusted R-squared:  0.9794 
F-statistic:  1809 on 1 and 37 DF,  p-value: < 2.2e-16

> anova(linear_model)
Analysis of Variance Table

Response: sp
          Df Sum Sq Mean Sq F value    Pr(>F)    
lp         1 128737  128737  1808.7 < 2.2e-16 ***
Residuals 37   2633      71                      
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
> 
"""

"""
fit <- lm(lp~ bed+bath+sqft+dwntn+mntn+lot_size+garage+age+on_market,data=skiData)
step <- stepAIC(fit, direction="both")
step$anova # display results

> step$anova # display results
Stepwise Model Path 
Analysis of Deviance Table

Initial Model:
lp ~ bed + bath + sqft + dwntn + mntn + lot_size + garage + age + 
    on_market

Final Model:
lp ~ bed + sqft + mntn + lot_size + garage


         Step Df   Deviance Resid. Df Resid. Dev      AIC
1                                  29   19377.84 262.1246
2     - dwntn  1   4.233574        30   19382.07 260.1331
3       - age  1  83.034421        31   19465.11 258.2999
4 - on_market  1 427.752661        32   19892.86 257.1476
5      - bath  1 907.400460        33   20800.26 256.8872
> 

"""
######################################################################################

#Part 2 : Multi Linear Regression


#Store the columns separate

bed=skiData$Bedrooms        
bath=skiData$Bathrooms       
sqft=skiData$Sq_Ft           
dwntn=skiData$Downtwon        
mntn=skiData$Mountain        
lot_size=skiData$'Lot size'      
garage=skiData$Garage          
age=skiData$Age             
on_market=skiData$'On market'     
sp=skiData$'Selling price'
lp=skiData$'List price'


#Model 1 with all variables as predictors

#Case 1 : (Using AIC Stepwise regression both sides) 

library(MASS)
fit <- lm(sp~ bed+bath+sqft+dwntn+mntn+lot_size+garage+age+on_market+lp,data=skiData)
step <- stepAIC(fit, direction="both")
step$anova # display results

"""
> fit <- lm(sp~ bed+bath+sqft+dwntn+mntn+lot_size+garage+age+on_market+lp,data=skiData)
> step <- stepAIC(fit, direction="both")
Start:  AIC=174.58
sp ~ bed + bath + sqft + dwntn + mntn + lot_size + garage + age + 
    on_market + lp

            Df Sum of Sq     RSS    AIC
- mntn       1       0.0  1950.7 172.58
- bath       1       0.2  1950.9 172.59
- age        1      32.2  1982.9 173.22
- garage     1      37.3  1988.1 173.32
- bed        1      50.9  2001.6 173.59
- sqft       1      70.2  2020.9 173.96
<none>                    1950.7 174.58
- on_market  1     120.3  2071.1 174.92
- lot_size   1     179.2  2129.9 176.01
- dwntn      1     234.0  2184.7 177.00
- lp         1   15933.3 17884.0 259.00

Step:  AIC=172.58
sp ~ bed + bath + sqft + dwntn + lot_size + garage + age + on_market + 
    lp

            Df Sum of Sq     RSS    AIC
- bath       1       0.2  1950.9 170.59
- age        1      33.1  1983.8 171.24
- garage     1      37.7  1988.4 171.33
- bed        1      56.3  2007.0 171.69
- sqft       1      80.0  2030.8 172.15
<none>                    1950.7 172.58
- on_market  1     128.5  2079.2 173.07
+ mntn       1       0.0  1950.7 174.58
- lot_size   1     214.3  2165.0 174.65
- dwntn      1     521.4  2472.1 179.82
- lp         1   21198.6 23149.3 267.06

Step:  AIC=170.59
sp ~ bed + sqft + dwntn + lot_size + garage + age + on_market + 
    lp

            Df Sum of Sq     RSS    AIC
- age        1      33.0  1983.9 169.24
- garage     1      42.3  1993.2 169.42
- bed        1      72.2  2023.1 170.00
- sqft       1      85.5  2036.4 170.26
<none>                    1950.9 170.59
- on_market  1     131.4  2082.3 171.13
+ bath       1       0.2  1950.7 172.58
+ mntn       1       0.0  1950.9 172.59
- lot_size   1     230.9  2181.8 172.95
- dwntn      1     523.9  2474.8 177.86
- lp         1   23544.8 25495.7 268.82

Step:  AIC=169.24
sp ~ bed + sqft + dwntn + lot_size + garage + on_market + lp

            Df Sum of Sq     RSS    AIC
- garage     1      43.7  2027.6 168.09
- sqft       1      75.4  2059.3 168.69
- bed        1      78.3  2062.1 168.75
<none>                    1983.9 169.24
- on_market  1     141.5  2125.4 169.93
+ age        1      33.0  1950.9 170.59
- lot_size   1     213.1  2197.0 171.22
+ mntn       1       0.9  1982.9 171.22
+ bath       1       0.1  1983.8 171.24
- dwntn      1     491.5  2475.3 175.87
- lp         1   24550.5 26534.4 268.38

Step:  AIC=168.09
sp ~ bed + sqft + dwntn + lot_size + on_market + lp

            Df Sum of Sq   RSS    AIC
- bed        1        41  2068 166.87
- sqft       1        84  2111 167.67
<none>                    2028 168.09
- on_market  1       121  2148 168.34
+ garage     1        44  1984 169.24
- lot_size   1       181  2208 169.42
+ age        1        34  1993 169.42
+ bath       1         4  2023 170.01
+ mntn       1         0  2027 170.08
- dwntn      1       449  2477 173.90
- lp         1     33506 35534 277.77

Step:  AIC=166.87
sp ~ sqft + dwntn + lot_size + on_market + lp

            Df Sum of Sq   RSS    AIC
<none>                    2068 166.87
- sqft       1       130  2198 167.24
- lot_size   1       163  2231 167.83
- on_market  1       177  2245 168.07
+ bed        1        41  2028 168.09
+ age        1        39  2030 168.13
+ bath       1        17  2051 168.55
+ garage     1         6  2062 168.75
+ mntn       1         1  2067 168.84
- dwntn      1       433  2501 172.28
- lp         1     39083 41151 281.50
> step$anova # display results
Stepwise Model Path 
Analysis of Deviance Table

Initial Model:
sp ~ bed + bath + sqft + dwntn + mntn + lot_size + garage + age + 
    on_market + lp

Final Model:
sp ~ sqft + dwntn + lot_size + on_market + lp


      Step Df     Deviance Resid. Df Resid. Dev      AIC
1                                 28   1950.729 174.5835
2   - mntn  1 6.304123e-05        29   1950.729 172.5835
3   - bath  1 1.700475e-01        30   1950.899 170.5869
4    - age  1 3.295124e+01        31   1983.851 169.2401
5 - garage  1 4.374427e+01        32   2027.595 168.0907
6    - bed  1 4.066041e+01        33   2068.255 166.8651
> 
"""

#Case 2: (Using Ols_step best subset)

library(olsrr)
ols_step_best_subset(fit)

"""
> ols_step_best_subset(fit)
                        Best Subsets Regression                         
------------------------------------------------------------------------
Model Index    Predictors
------------------------------------------------------------------------
     1         lp                                                        
     2         dwntn lp                                                  
     3         dwntn on_market lp                                        
     4         dwntn lot_size on_market lp                               
     5         sqft dwntn lot_size on_market lp                          
     6         bed sqft dwntn lot_size on_market lp                      
     7         bed sqft dwntn lot_size garage on_market lp               
     8         bed sqft dwntn lot_size garage age on_market lp           
     9         bed bath sqft dwntn lot_size garage age on_market lp      
    10         bed bath sqft dwntn mntn lot_size garage age on_market lp 
------------------------------------------------------------------------

                                                     Subsets Regression Summary                                                     
------------------------------------------------------------------------------------------------------------------------------------
                       Adj.        Pred                                                                                              
Model    R-Square    R-Square    R-Square     C(p)        AIC         SBIC        SBC         MSEP         FPE       HSP       APC  
------------------------------------------------------------------------------------------------------------------------------------
  1        0.9800      0.9794      0.9777     2.7998    280.9645    170.4123    285.9552    2776.0221    74.8248    1.9771    0.0222 
  2        0.9818      0.9808      0.9794     1.3363    279.2167    169.3175    285.8709    2593.7143    71.5608    1.8986    0.0212 
  3        0.9828      0.9813       0.977     1.4044    278.9582    169.8264    287.2760    2519.7719    71.1179    1.8971    0.0211 
  4        0.9833      0.9813      0.9752     2.5466    279.9118    171.4856    289.8932    2527.4003    72.9290    1.9588    0.0217 
  5        0.9843      0.9819      0.9747     2.6869    279.5423    172.4328    291.1872    2452.7386    72.3166    1.9586    0.0215 
  6        0.9846      0.9817      0.9732     4.1033    280.7679    174.6202    294.0764    2482.0847    74.7351    2.0439    0.0222 
  7        0.9849      0.9815      0.9704     5.4754    281.9173    176.8805    296.8894    2509.4862    77.1224    2.1332    0.0229 
  8        0.9851      0.9812      0.9705     7.0024    283.2641    179.3476    299.8997    2552.9010    80.0369    2.2424    0.0238 
  9        0.9852      0.9805      0.9679     9.0000    285.2607    182.1319    303.5599    2643.8455    84.5144    2.4024    0.0251 
 10        0.9852      0.9798      0.9624    11.0000    287.2607    184.9177    307.2234    2741.7657    89.3191    2.5803    0.0265 
------------------------------------------------------------------------------------------------------------------------------------
AIC: Akaike Information Criteria 
 SBIC: Sawa's Bayesian Information Criteria 
 SBC: Schwarz Bayesian Criteria 
 MSEP: Estimated error of prediction, assuming multivariate normality 
 FPE: Final Prediction Error 
 HSP: Hocking's Sp 
 APC: Amemiya Prediction Criteria 

>
"""

###Case 3 : (Using regsubsets)

library(leaps)
library(car)
attach(skiData)
regsubsets.out<-regsubsets(sp~bed+bath+sqft+dwntn+mntn+lot_size+garage+age+on_market+lp,data=skiData,nbest=1)
regsubsets.out
summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)
which.max(summary.out$adjr2)
summary.out$which[5,]

"""
> attach(skiData)
> regsubsets.out<-regsubsets(sp~bed+bath+sqft+dwntn+mntn+lot_size+garage+age+on_market+lp,data=skiData,nbest=1)
> regsubsets.out
Subset selection object
Call: regsubsets.formula(sp ~ bed + bath + sqft + dwntn + mntn + lot_size + 
    garage + age + on_market + lp, data = skiData, nbest = 1)
10 Variables  (and intercept)
          Forced in Forced out
bed           FALSE      FALSE
bath          FALSE      FALSE
sqft          FALSE      FALSE
dwntn         FALSE      FALSE
mntn          FALSE      FALSE
lot_size      FALSE      FALSE
garage        FALSE      FALSE
age           FALSE      FALSE
on_market     FALSE      FALSE
lp            FALSE      FALSE
1 subsets of each size up to 8
Selection Algorithm: exhaustive
> summary.out <- summary(regsubsets.out)
> as.data.frame(summary.out$outmat)
         bed bath sqft dwntn mntn lot_size garage age on_market lp
1  ( 1 )                                                         *
2  ( 1 )                   *                                     *
3  ( 1 )                   *                                  *  *
4  ( 1 )                   *             *                    *  *
5  ( 1 )             *     *             *                    *  *
6  ( 1 )   *         *     *             *                    *  *
7  ( 1 )   *         *     *             *      *             *  *
8  ( 1 )   *         *     *             *      *   *         *  *

> which.max(summary.out$adjr2)
[1] 5
> summary.out$which[5,]
(Intercept)         bed        bath        sqft       dwntn        mntn 
       TRUE       FALSE       FALSE        TRUE        TRUE       FALSE 
   lot_size      garage         age   on_market          lp 
       TRUE       FALSE       FALSE        TRUE        TRUE 
> 

"""
layout(matrix(1:2, ncol = 2))
## Adjusted R2
res.legend <-
  subsets(regsubsets.out, statistic="adjr2", legend = FALSE, min.size = 5, main = "Adjusted R^2")
## Mallow Cp
res.legend <-
  subsets(regsubsets.out, statistic="cp", legend = FALSE, min.size = 5, main = "Mallow Cp")
abline(a = 1, b = 1, lty = 2)
res.legend


#Model Building

install.packages("boot")
install.packages("carData")
library(boot)
library(carData)
library(car)
set.seed(4)

# The best model from all above regression methods is 
# with 5 variables
#sp ~ sqft + dwntn + lot_size + on_market + lp

model1=lm( sp ~ sqft + dwntn + lot_size + on_market +lp,data=skiData)
summary(model1)
anova(model1)
vif(model1)

"""
> model1=lm( sp ~ sqft + dwntn + lot_size + on_market + lp,data=skiData)
> summary(model1)

Call:
lm(formula = sp ~ sqft + dwntn + lot_size + on_market + lp, data = skiData)

Residuals:
     Min       1Q   Median       3Q      Max 
-21.3916  -2.5665  -0.5458   6.1540  14.6149 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  9.959120  11.868016   0.839   0.4074    
sqft         0.004975   0.003460   1.438   0.1599    
dwntn       -0.519804   0.197749  -2.629   0.0129 *  
lot_size     0.447494   0.277291   1.614   0.1161    
on_market   -0.025186   0.014982  -1.681   0.1022    
lp           0.943459   0.037781  24.972   <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
Residual standard error: 7.917 on 33 degrees of freedom
Multiple R-squared:  0.9843,	Adjusted R-squared:  0.9819 
F-statistic: 412.6 on 5 and 33 DF,  p-value: < 2.2e-16

> anova(model1)
Analysis of Variance Table

Response: sp
          Df Sum Sq Mean Sq  F value Pr(>F)    
sqft       1  53331   53331 850.9186 <2e-16 ***
dwntn      1  18356   18356 292.8860 <2e-16 ***
lot_size   1  18522   18522 295.5254 <2e-16 ***
on_market  1     10      10   0.1546 0.6967    
lp         1  39083   39083 623.5915 <2e-16 ***
Residuals 33   2068      63                    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

> vif(model1)
     sqft     dwntn  lot_size on_market        lp 
 3.205170  2.703068  2.450604  1.233597  2.965515 
> 
"""


# Diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(model1)

###########################################################

##Model 2 : All variables except listing price as predictors

#Case 1 : (Using AIC Stepwise regression both sides) 

library(MASS)
fit <- lm(sp~bed+bath+sqft+dwntn+mntn+lot_size+garage+age+on_market,data=skiData)
step <- stepAIC(fit, direction="both")
step$anova # display results

"""
> fit <- lm(sp~bed+bath+sqft+dwntn+mntn+lot_size+garage+age+on_market,data=skiData)
> step <- stepAIC(fit, direction="both")
Start:  AIC=259
sp ~ bed + bath + sqft + dwntn + mntn + lot_size + garage + age + 
    on_market

            Df Sum of Sq   RSS    AIC
- on_market  1      46.4 17930 257.10
- age        1     200.2 18084 257.43
- dwntn      1     294.6 18179 257.63
- bath       1     534.2 18418 258.14
<none>                   17884 259.00
- garage     1    2893.2 20777 262.84
- bed        1    3776.5 21660 264.47
- mntn       1    5265.3 23149 267.06
- sqft       1    7140.2 25024 270.10
- lot_size   1   16638.6 34523 282.65

Step:  AIC=257.1
sp ~ bed + bath + sqft + dwntn + mntn + lot_size + garage + age

            Df Sum of Sq   RSS    AIC
- age        1     198.2 18129 255.53
- dwntn      1     416.6 18347 255.99
- bath       1     660.8 18591 256.51
<none>                   17930 257.10
+ on_market  1      46.4 17884 259.00
- garage     1    3038.6 20969 261.20
- bed        1    3801.6 21732 262.60
- mntn       1    5295.1 23226 265.19
- sqft       1    7106.9 25037 268.12
- lot_size   1   20099.4 38030 284.42

Step:  AIC=255.53
sp ~ bed + bath + sqft + dwntn + mntn + lot_size + garage

            Df Sum of Sq   RSS    AIC
- dwntn      1     293.5 18422 254.15
- bath       1     624.0 18753 254.85
<none>                   18129 255.53
+ age        1     198.2 17930 257.10
+ on_market  1      44.4 18084 257.43
- garage     1    3163.0 21292 259.80
- bed        1    4186.0 22315 261.63
- mntn       1    6103.6 24232 264.84
- sqft       1    7110.0 25239 266.43
- lot_size   1   20050.2 38179 282.57

Step:  AIC=254.15
sp ~ bed + bath + sqft + mntn + lot_size + garage

            Df Sum of Sq   RSS    AIC
- bath       1     539.3 18961 253.28
<none>                   18422 254.15
+ dwntn      1     293.5 18129 255.53
+ on_market  1     144.1 18278 255.85
+ age        1      75.1 18347 255.99
- garage     1    2982.1 21404 258.00
- bed        1    4269.6 22692 260.28
- sqft       1    6932.9 25355 264.61
- lot_size   1   19791.9 38214 280.61
- mntn       1   31413.9 49836 290.96

Step:  AIC=253.28
sp ~ bed + sqft + mntn + lot_size + garage

            Df Sum of Sq   RSS    AIC
<none>                   18961 253.28
+ bath       1       539 18422 254.15
+ on_market  1       276 18686 254.71
+ dwntn      1       209 18753 254.85
+ age        1        67 18894 255.14
- garage     1      5287 24248 260.87
- bed        1      8788 27750 266.13
- sqft       1     10671 29632 268.69
- lot_size   1     19804 38765 279.17
- mntn       1     38755 57716 294.69
> step$anova # display results
Stepwise Model Path 
Analysis of Deviance Table

Initial Model:
sp ~ bed + bath + sqft + dwntn + mntn + lot_size + garage + age + 
    on_market

Final Model:
sp ~ bed + sqft + mntn + lot_size + garage


         Step Df  Deviance Resid. Df Resid. Dev      AIC
1                                 29   17883.98 258.9959
2 - on_market  1  46.44029        30   17930.42 257.0970
3       - age  1 198.19433        31   18128.62 255.5257
4     - dwntn  1 293.50690        32   18422.13 254.1521
5      - bath  1 539.25150        33   18961.38 253.2773
>

"""

#Case 2: (Using Ols_step best subset)

library(olsrr)
ols_step_best_subset(fit)

"""

> ols_step_best_subset(fit)
                       Best Subsets Regression                       
---------------------------------------------------------------------
Model Index    Predictors
---------------------------------------------------------------------
     1         bath                                                   
     2         bath mntn                                              
     3         sqft mntn lot_size                                     
     4         bath sqft mntn lot_size                                
     5         bed sqft mntn lot_size garage                          
     6         bed bath sqft mntn lot_size garage                     
     7         bed bath sqft dwntn mntn lot_size garage               
     8         bed bath sqft dwntn mntn lot_size garage age           
     9         bed bath sqft dwntn mntn lot_size garage age on_market 
---------------------------------------------------------------------

                                                       Subsets Regression Summary                                                       
----------------------------------------------------------------------------------------------------------------------------------------
                       Adj.        Pred                                                                                                  
Model    R-Square    R-Square    R-Square     C(p)        AIC         SBIC        SBC          MSEP          FPE         HSP       APC  
----------------------------------------------------------------------------------------------------------------------------------------
  1        0.5516      0.5395      0.5178    60.5255    402.1649    288.4205    407.1556    62098.3918    1673.7989    44.2264    0.4969 
  2        0.6068      0.5849      0.5518    50.7705    399.0437    284.5886    405.6979    56012.7070    1545.3956    41.0003    0.4588 
  3        0.7776      0.7586      0.6311    16.3692    378.8095    266.6564    387.1273    32604.7325     920.2340    24.5480    0.2732 
  4        0.8234      0.8026      0.6124     8.6196    371.8220    261.5091    381.8034    26678.6352     769.8207    20.6770    0.2285 
  5        0.8557      0.8338      0.7066     3.7471    365.9545    258.3542    377.5994    22486.2479     662.9852    17.9558    0.1968 
  6        0.8598      0.8335      0.6724     4.8726    366.8293    260.2430    380.1378    22551.4848     679.0206    18.5707    0.2016 
  7        0.8620      0.8308      0.6543     6.3967    368.2029    262.5392    383.1750    22931.9273     704.7519    19.4931    0.2092 
  8        0.8635      0.8271      0.6489     8.0753    369.7742    265.0082    386.4098    23463.3310     735.6071    20.6097    0.2184 
  9        0.8639      0.8216      0.6267    10.0000    371.6731    267.6546    389.9723    24238.3660     774.8145    22.0246    0.2300 
----------------------------------------------------------------------------------------------------------------------------------------
AIC: Akaike Information Criteria 
 SBIC: Sawa's Bayesian Information Criteria 
 SBC: Schwarz Bayesian Criteria 
 MSEP: Estimated error of prediction, assuming multivariate normality 
 FPE: Final Prediction Error 
 HSP: Hocking's Sp 
 APC: Amemiya Prediction Criteria 

> 

"""


###Case 3 : (Using regsubsets)
  
library(leaps)
attach(skiData)
library(car)
regsubsets.out<-regsubsets(sp~bed+bath+sqft+dwntn+mntn+lot_size+garage+age+on_market,data=skiData,nbest=1)
regsubsets.out
summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)
which.max(summary.out$adjr2)
summary.out$which[5,]

"""
> library(leaps)
> attach(skiData)
> library(car)
> regsubsets.out<-regsubsets(sp~bed+bath+sqft+dwntn+mntn+lot_size+garage+age+on_market,data=skiData,nbest=1)
> regsubsets.out
Subset selection object
Call: regsubsets.formula(sp ~ bed + bath + sqft + dwntn + mntn + lot_size + 
    garage + age + on_market, data = skiData, nbest = 1)
9 Variables  (and intercept)
          Forced in Forced out
bed           FALSE      FALSE
bath          FALSE      FALSE
sqft          FALSE      FALSE
dwntn         FALSE      FALSE
mntn          FALSE      FALSE
lot_size      FALSE      FALSE
garage        FALSE      FALSE
age           FALSE      FALSE
on_market     FALSE      FALSE
1 subsets of each size up to 8
Selection Algorithm: exhaustive
> summary.out <- summary(regsubsets.out)
> as.data.frame(summary.out$outmat)
         bed bath sqft dwntn mntn lot_size garage age on_market
1  ( 1 )        *                                              
2  ( 1 )        *               *                              
3  ( 1 )             *          *        *                     
4  ( 1 )        *    *          *        *                     
5  ( 1 )   *         *          *        *      *              
6  ( 1 )   *    *    *          *        *      *              
7  ( 1 )   *    *    *     *    *        *      *              
8  ( 1 )   *    *    *     *    *        *      *   *          
> which.max(summary.out$adjr2)
[1] 5
> summary.out$which[5,]
(Intercept)         bed        bath        sqft       dwntn        mntn    lot_size      garage         age 
       TRUE        TRUE       FALSE        TRUE       FALSE        TRUE        TRUE        TRUE       FALSE 
  on_market 
      FALSE 
> 
"""


layout(matrix(1:2, ncol = 2))
## Adjusted R2
res.legend <-
  subsets(regsubsets.out, statistic="adjr2", legend = FALSE, min.size = 5, main = "Adjusted R^2")
## Mallow Cp
res.legend <-
  subsets(regsubsets.out, statistic="cp", legend = FALSE, min.size = 5, main = "Mallow Cp")
abline(a = 1, b = 1, lty = 2)
res.legend


#Model Building

install.packages("boot")
install.packages("carData")
library(boot)
library(carData)
library(car)
set.seed(4)

# The best model from all above regression methods is 
# with 5 variables
# sp ~ bed + sqft + mntn + lot_size + garage

model2=lm( sp ~ bed + sqft + mntn + lot_size + garage,data=skiData)
summary(model2)
anova(model2)
vif(model2)

"""
> model2=lm( sp ~ bed + sqft + mntn + lot_size + garage,data=skiData)
> summary(model2)

Call:
lm(formula = sp ~ bed + sqft + mntn + lot_size + garage, data = skiData)

Residuals:
    Min      1Q  Median      3Q     Max 
-59.104 -11.554  -2.188  15.104  47.318 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) 263.063011  16.343425  16.096  < 2e-16 ***
bed          21.836826   5.583632   3.911 0.000433 ***
sqft          0.040099   0.009305   4.309 0.000139 ***
mntn         -4.302038   0.523829  -8.213 1.75e-09 ***
lot_size      4.084563   0.695742   5.871 1.41e-06 ***
garage       13.657168   4.502471   3.033 0.004688 ** 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 23.97 on 33 degrees of freedom
Multiple R-squared:  0.8557,	Adjusted R-squared:  0.8338 
F-statistic: 39.13 on 5 and 33 DF,  p-value: 6.087e-13

> anova(model2)
Analysis of Variance Table

Response: sp
          Df Sum Sq Mean Sq F value    Pr(>F)    
bed        1  49040   49040 85.3483 1.132e-10 ***
sqft       1  12113   12113 21.0814 6.116e-05 ***
mntn       1  20765   20765 36.1385 9.325e-07 ***
lot_size   1  25204   25204 43.8654 1.560e-07 ***
garage     1   5287    5287  9.2007  0.004688 ** 
Residuals 33  18961     575                      
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

> vif(model2)
     bed     sqft     mntn lot_size   garage 
2.264996 2.528167 1.595349 1.682800 1.326219 
>

"""
# Diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(model2)

#Residual Plots against independent variables

plot(bed,model2$residuals , main = 'Residual Plot against Bedrooms')
abline(h=0, col = 'red')

plot(sqft,model2$residuals, main = 'Residual Plot against Sq_ft')
abline(h=0, col = 'red')

plot(mntn,model2$residuals, main = 'Residual Plot against Mountain View')
abline(h=0, col = 'red')

plot(lot_size,model2$residuals, main = 'Residual Plot against Lot_size')
abline(h=0, col = 'red')

plot(garage,model2$residuals, main = 'Residual Plot against Garage')
abline(h=0, col = 'red')

#############################################################################

#split the data into training and testing

library(caret)
install.packages("boot")
install.packages("carData")
library(boot)
library(carData)
library(car)
set.seed(4)
n=nrow(skiData)
shuffled=skiData[sample(n),]
train=shuffled[1:round(0.85 * n),]
test = shuffled[(round(0.85 * n) + 1):n,]

############### Case 1 : Validation of Model by Data Splitting  ###########

#Model 1 - Validation with Traning Data

Vm1=lm(`Selling price`~ Sq_Ft+Downtwon+`Lot size` + `On market` + `List price`,data=train)
summary(Vm1)
vif(Vm1)

#Prediction 
prediction=predict.lm(Vm1,newdata=test)
prediction
test$`Selling price`

#Compute metrics R2, RMSE, MAE

R2(prediction, test$'Selling price')
RMSE(prediction, test$'Selling price')
MAE(prediction, test$'Selling price')

"""
> summary(Vm1)

Call:
lm(formula = `Selling price` ~ Sq_Ft + Downtwon + `Lot size` + 
    `On market` + `List price`, data = train)

Residuals:
     Min       1Q   Median       3Q      Max 
-21.4104  -3.0630   0.2741   3.6272  12.2826 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)   9.021384  11.606212   0.777   0.4437    
Sq_Ft         0.002036   0.003323   0.613   0.5451    
Downtwon     -0.359600   0.193405  -1.859   0.0739 .  
`Lot size`    0.222097   0.271473   0.818   0.4205    
`On market`  -0.013364   0.015509  -0.862   0.3964    
`List price`  0.955545   0.036335  26.298   <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 7.104 on 27 degrees of freedom
Multiple R-squared:  0.9877,	Adjusted R-squared:  0.9854 
F-statistic: 432.8 on 5 and 27 DF,  p-value: < 2.2e-16

> vif(Vm1)
       Sq_Ft     Downtwon   `Lot size`  `On market` `List price` 
    3.157480     3.087699     2.874854     1.401524     2.952450 
> 

#######################################################

> prediction=predict.lm(Vm1,newdata=test)

> prediction
       1        2        3        4        5        6 
416.0652 351.3674 353.6140 461.1635 390.3674 332.5647 

> test$`Selling price`
[1] 410.0 326.0 350.0 470.0 386.3 339.0


> R2(prediction, test$'Selling price')
[1] 0.9562514

> RMSE(prediction, test$'Selling price')
[1] 11.7572


> MAE(prediction, test$'Selling price')
[1] 9.064281

"""

#########################################################

#Model2 - Validation

Vm2=lm(`Selling price`~Bedrooms+Sq_Ft+Mountain+Garage+`Lot size`,data=train)
summary(Vm2)
vif(Vm2)


#predict and compute r2,RMSE
prediction=predict.lm(Vm2,newdata=test)
prediction
test$`Selling price`

R2(prediction, test$'Selling price')
RMSE(prediction, test$'Selling price')
MAE(prediction, test$'Selling price')

"""
> summary(Vm2)

Call:
lm(formula = `Selling price` ~ Bedrooms + Sq_Ft + Mountain + 
    Garage + `Lot size`, data = train)

Residuals:
    Min      1Q  Median      3Q     Max 
-60.647 -11.706  -1.705  13.391  42.812 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) 266.257830  18.547033  14.356 3.71e-14 ***
Bedrooms     20.648675   5.981347   3.452 0.001847 ** 
Sq_Ft         0.042030   0.009896   4.247 0.000229 ***
Mountain     -4.555631   0.563856  -8.079 1.11e-08 ***
Garage       14.139980   4.689123   3.015 0.005531 ** 
`Lot size`    4.160842   0.723339   5.752 4.07e-06 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 24.32 on 27 degrees of freedom
Multiple R-squared:  0.8556,	Adjusted R-squared:  0.8289 
F-statistic:    32 on 5 and 27 DF,  p-value: 1.547e-10

> vif(Vm2)
  Bedrooms      Sq_Ft   Mountain     Garage `Lot size` 
  2.189031   2.389787   1.664013   1.259722   1.741934 
> 


##################################################################

> prediction
       1        2        3        4        5        6 
442.5593 296.3310 368.0926 475.3602 384.1502 370.2530


> test$`Selling price`
[1] 410.0 326.0 350.0 470.0 386.3 339.0

> library(caret)
> RMSE(prediction, test$'Selling price')
[1] 23.37307

> MAE(prediction, test$'Selling price')
[1] 19.8473

> R2(prediction, test$'Selling price')
[1] 0.8649214

"""

#############################################################################
######### Case 2 : Validation of Model by K FOLD CROSS VALIDATION  ###########

#Model 1

Model_CV1 <- train(`Selling price`~ Sq_Ft+Downtwon+`Lot size` + `On market` + `List price`,
            training,method="lm",trControl=trainControl(method="cv",number=10,verboseIter=TRUE))
Model_CV1

"""
> Model_CV1 <- train(`Selling price`~ Sq_Ft+Downtwon+`Lot size` + `On market` + `List price`,
training,method="lm",trControl=trainControl(method="cv",number=10,verboseIter=TRUE))
+ Fold01: intercept=TRUE 
- Fold01: intercept=TRUE 
+ Fold02: intercept=TRUE 
- Fold02: intercept=TRUE 
+ Fold03: intercept=TRUE 
- Fold03: intercept=TRUE 
+ Fold04: intercept=TRUE 
- Fold04: intercept=TRUE 
+ Fold05: intercept=TRUE 
- Fold05: intercept=TRUE 
+ Fold06: intercept=TRUE 
- Fold06: intercept=TRUE 
+ Fold07: intercept=TRUE 
- Fold07: intercept=TRUE 
+ Fold08: intercept=TRUE 
- Fold08: intercept=TRUE 
+ Fold09: intercept=TRUE 
- Fold09: intercept=TRUE 
+ Fold10: intercept=TRUE 
- Fold10: intercept=TRUE 
Aggregating results
Fitting final model on full training set


> Model_CV1
Linear Regression 

33 samples
 5 predictor

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 30, 29, 30, 29, 30, 30, ... 
Resampling results:

  RMSE      Rsquared   MAE     
  7.642107  0.9807507  6.077977

Tuning parameter 'intercept' was held constant at a value of TRUE
>

"""


#######  Model 2 : Kfold cross validation  #################################################

Model_CV2 <- train(`Selling price`~Bedrooms+Sq_Ft+Mountain+Garage+`Lot size`,training,method="lm",
                   trControl=trainControl(method="cv",number=10,verboseIter=TRUE))
Model_CV2

"""
> Model_CV2 <- train(`Selling price`~Bedrooms+Sq_Ft+Mountain+Garage+`Lot size`,training,method="lm",
trControl=trainControl(method="cv",number=10,verboseIter=TRUE))
+ Fold01: intercept=TRUE 
- Fold01: intercept=TRUE 
+ Fold02: intercept=TRUE 
- Fold02: intercept=TRUE 
+ Fold03: intercept=TRUE 
- Fold03: intercept=TRUE 
+ Fold04: intercept=TRUE 
- Fold04: intercept=TRUE 
+ Fold05: intercept=TRUE 
- Fold05: intercept=TRUE 
+ Fold06: intercept=TRUE 
- Fold06: intercept=TRUE 
+ Fold07: intercept=TRUE 
- Fold07: intercept=TRUE 
+ Fold08: intercept=TRUE 
- Fold08: intercept=TRUE 
+ Fold09: intercept=TRUE 
- Fold09: intercept=TRUE 
+ Fold10: intercept=TRUE 
- Fold10: intercept=TRUE 
Aggregating results
Fitting final model on full training set

> Model_CV2
Linear Regression 

33 samples
 5 predictor

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 30, 30, 30, 30, 30, 29, ... 
Resampling results:

  RMSE      Rsquared   MAE     
  28.57572  0.7697075  23.04362

Tuning parameter 'intercept' was held constant at a value of TRUE
>
"""








