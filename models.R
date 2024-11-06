library(lavaan)
library(tidyverse)

##########################
model_1 <- '
  # Cross-lagged paths
  Income_W8 ~ 1 + Income_W7 + Health_HRS_W7
  Income_W9 ~ 1 + Income_W8 + Health_HRS_W8
  Income_W10 ~ 1 + Income_W9 + Health_HRS_W9

  Health_HRS_W8 ~ 1 + Health_HRS_W7 + Income_W7
  Health_HRS_W9 ~ 1 + Health_HRS_W8 + Income_W8
  Health_HRS_W10 ~ 1 + Health_HRS_W9 + Income_W9
  
  # correlation
  Health_HRS_W7 ~~ Income_W7 
  Health_HRS_W8 ~~ Income_W8
  Health_HRS_W9 ~~ Income_W9
  Health_HRS_W10 ~~ Income_W10
'

# model 2: equal cross-effects wave 7
model_2 <- '
  # Cross-lagged paths
  Income_W8 ~ 1 + Income_W7 + a*Health_HRS_W7
  Income_W9 ~ 1 + Income_W8 + Health_HRS_W8
  Income_W10 ~ 1 + Income_W9 + Health_HRS_W9

  Health_HRS_W8 ~ 1 + Health_HRS_W7 + a*Income_W7
  Health_HRS_W9 ~ 1 + Health_HRS_W8 + Income_W8
  Health_HRS_W10 ~ 1 + Health_HRS_W9 + Income_W9
  
  # correlation
  Health_HRS_W7 ~~ Income_W7 
  Health_HRS_W8 ~~ Income_W8
  Health_HRS_W9 ~~ Income_W9
  Health_HRS_W10 ~~ Income_W10
'

# model 3: equal cross-effects wave 8
model_3 <- '
  # Cross-lagged paths
  Income_W8 ~ 1 + Income_W7 + a*Health_HRS_W7
  Income_W9 ~ 1 + Income_W8 + b*Health_HRS_W8
  Income_W10 ~ 1 + Income_W9 + Health_HRS_W9

  Health_HRS_W8 ~ 1 + Health_HRS_W7 + a*Income_W7
  Health_HRS_W9 ~ 1 + Health_HRS_W8 + b*Income_W8
  Health_HRS_W10 ~ 1 + Health_HRS_W9 + Income_W9
  
  # correlation
  Health_HRS_W7 ~~ Income_W7 
  Health_HRS_W8 ~~ Income_W8
  Health_HRS_W9 ~~ Income_W9
  Health_HRS_W10 ~~ Income_W10
'

# model 4: equal cross-effects wave 9
model_4 <- '
  # Cross-lagged paths
  Income_W8 ~ 1 + Income_W7 + a*Health_HRS_W7
  Income_W9 ~ 1 + Income_W8 + Health_HRS_W8
  Income_W10 ~ 1 + Income_W9 + b*Health_HRS_W9

  Health_HRS_W8 ~ 1 + Health_HRS_W7 + a*Income_W7
  Health_HRS_W9 ~ 1 + Health_HRS_W8 + Income_W8
  Health_HRS_W10 ~ 1 + Health_HRS_W9 + b*Income_W9
  
  # correlation
  Health_HRS_W7 ~~ Income_W7 
  Health_HRS_W8 ~~ Income_W8
  Health_HRS_W9 ~~ Income_W9
  Health_HRS_W10 ~~ Income_W10
'


# model 5 : stability on income
model_5 <- '
  # Cross-lagged paths
  Income_W8 ~ 1 + b*Income_W7 + a*Health_HRS_W7
  Income_W9 ~ 1 + Income_W8 + Health_HRS_W8
  Income_W10 ~ 1 + Income_W9 + Health_HRS_W9

  Health_HRS_W8 ~ 1 + b*Health_HRS_W7 + a*Income_W7
  Health_HRS_W9 ~ 1 + Health_HRS_W8 + Income_W8
  Health_HRS_W10 ~ 1 + Health_HRS_W9 + Income_W9

  # correlation
  Health_HRS_W7 ~~ Income_W7
  Health_HRS_W8 ~~ Income_W8
  Health_HRS_W9 ~~ Income_W9
  Health_HRS_W10 ~~ Income_W10
'

model_6 <- '
  # Cross-lagged paths
  Income_W8 ~ 1 + b*Income_W7 + a*Health_HRS_W7
  Income_W9 ~ 1 + c*Income_W8 + Health_HRS_W8
  Income_W10 ~ 1 + Income_W9 + Health_HRS_W9

  Health_HRS_W8 ~ 1 + b*Health_HRS_W7 + a*Income_W7
  Health_HRS_W9 ~ 1 + c*Health_HRS_W8 + Income_W8
  Health_HRS_W10 ~ 1 + Health_HRS_W9 + Income_W9

  # correlation
  Health_HRS_W7 ~~ Income_W7
  Health_HRS_W8 ~~ Income_W8
  Health_HRS_W9 ~~ Income_W9
  Health_HRS_W10 ~~ Income_W10
'

model_7 <- '
    # Cross-lagged paths
    Income_W8 ~ 1 + b*Income_W7 + a*Health_HRS_W7
    Income_W9 ~ 1 + c*Income_W8 + Health_HRS_W8
    Income_W10 ~ 1 + d*Income_W9 + Health_HRS_W9
  
    Health_HRS_W8 ~ 1 + b*Health_HRS_W7 + a*Income_W7
    Health_HRS_W9 ~ 1 + c*Health_HRS_W8 + Income_W8
    Health_HRS_W10 ~ 1 + d*Health_HRS_W9 + Income_W9
  
    # correlation
    Health_HRS_W7 ~~ Income_W7
    Health_HRS_W8 ~~ Income_W8
    Health_HRS_W9 ~~ Income_W9
    Health_HRS_W10 ~~ Income_W10
'



# model 8 : cross-lagged model with random intercept for Income

model_8 <- '
    # Cross-lagged paths
    Income_W8 ~ 1 + b*Income_W7 + a*Health_HRS_W7
    Income_W9 ~ 1 + c*Income_W8 + Health_HRS_W8
    Income_W10 ~ 1 + d*Income_W9 + Health_HRS_W9
  
    Health_HRS_W8 ~ 1 + b*Health_HRS_W7 + a*Income_W7
    Health_HRS_W9 ~ 1 + c*Health_HRS_W8 + Income_W8
    Health_HRS_W10 ~ 1 + d*Health_HRS_W9 + Income_W9
  
    # correlation
    Health_HRS_W7 ~~ Income_W7
    Health_HRS_W8 ~~ Income_W8
    Health_HRS_W9 ~~ Income_W9
    Health_HRS_W10 ~~ Income_W10
    
    rm =~ 1*Income_W7 + 1*Income_W8 + 1*Income_W9 + 1*Income_W10
    rm =~ 1*Health_HRS_W7 + 1*Health_HRS_W8 + 1*Health_HRS_W9 + 1*Health_HRS_W10
'


# Fit all models specs
fit_1 <- sem(model_1, data = data_w_std)
fit_2 <- sem(model_2, data = data_w_std)
fit_3 <- sem(model_3, data = data_w_std)
fit_4 <- sem(model_4, data = data_w_std)
fit_5 <- sem(model_5, data = data_w_std)
fit_6 <- sem(model_6, data = data_w_std)
fit_7 <- sem(model_7, data = data_w_std)
fit_8 <- sem(model_8, data = data_w_std)

# Compare models progressively
anova(fit_1, fit_2) # m2 is better
anova(fit_2, fit_3) # m3 is better
anova(fit_2, fit_4) # m2 is better
anova(fit_2, fit_5) # m5 is better
anova(fit_5, fit_6) # m6 is better
anova(fit_6, fit_7) # m7 is better
anova(fit_7, fit_8) # m8 is better
