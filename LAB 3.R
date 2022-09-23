attach(acs2017_ny)
library(tidyverse)
library(plyr)


dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1)&(acs2017_ny$AGE > 20) & (acs2017_ny$AGE < 66))
attach(dat_NYC)

borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))

norm_varb <- function(X_in) {(X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )}

norm_inc_tot <- norm_varb(INCTOT)
norm_poverty <- norm_varb(POVERTY)
norm_migration <- norm_varb(MIGRATE1D)
norm_rent <- norm_varb(RENT)
norm_fstamps <- norm_varb(FOODSTMP)
norm_incwge <- norm_varb(INCWAGE) 

norm_inc_tot <- as.numeric(norm_inc_tot)
norm_poverty <- as.numeric(norm_poverty)
norm_migration <- as.numeric(norm_migration)
norm_rent <- as.numeric(norm_rent)
norm_fstamps <- as.numeric(norm_fstamps)
norm_incwge <- as.numeric(norm_incwge)

library(data.table)
library(ggplot2)
library(tidyverse)
library(DT)
library(dplyr)
library(tidyr)
library(psych)


data_use_prelim <- data.frame(norm_inc_tot , norm_poverty , norm_rent , norm_migration , norm_fstamps , norm_incwge)

good_obs_data_use <- complete.cases(data_use_prelim,borough_f)

dat_use <- subset(data_use_prelim,good_obs_data_use)

y_use <- subset(borough_f,good_obs_data_use)

set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]

summary(cl_data)
prop.table(summary(cl_data))
summary(train_data)
require(class)
for (indx in seq(1, 9, by= 2)) {
  pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_borough == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}

cl_data_n <- as.numeric(cl_data)

# I took norm_incwage out .
# We could've more accuracy if we added more variables but now "Queens" borough gives the highest majority of people in sample.

model_ols1 <- lm(cl_data_n ~ train_data$norm_inc_tot + train_data$norm_poverty + train_data$norm_migration + train_data$norm_rent + train_data$norm_fstamp)

y_hat <- fitted.values(model_ols1)

mean(y_hat[cl_data_n == 1])
mean(y_hat[cl_data_n == 2])
mean(y_hat[cl_data_n == 3])
mean(y_hat[cl_data_n == 4])
mean(y_hat[cl_data_n == 5])

cl_data_n1 <- as.numeric(cl_data_n == 1)

model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_inc_tot + train_data$norm_poverty + train_data$norm_migration + train_data$norm_rent + train_data$norm_fstamp)

y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 1])
mean(y_hat_v1[cl_data_n1 == 0])

cl_data_n1 <- as.numeric(cl_data_n == 2)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_inc_tot + train_data$norm_poverty + train_data$norm_migration + train_data$norm_rent + train_data$norm_fstamp)
y_hat <- fitted.values(model_ols1)
mean(y_hat_v1[cl_data_n1 == 1])
mean(y_hat_v1[cl_data_n1 == 0])

cl_data_n1 <- as.numeric(cl_data_n == 3)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_inc_tot + train_data$norm_poverty + train_data$norm_migration + train_data$norm_rent + train_data$norm_fstamp)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 1])
mean(y_hat_v1[cl_data_n1 == 0])

cl_data_n1 <- as.numeric(cl_data_n == 4)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_inc_tot + train_data$norm_poverty + train_data$norm_migration + train_data$norm_rent + 
                     train_data$norm_fstamp)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 1])
mean(y_hat_v1[cl_data_n1 == 0])

cl_data_n1 <- as.numeric(cl_data_n == 5)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_inc_tot + train_data$norm_poverty + train_data$norm_migration + train_data$norm_rent + 
                     train_data$norm_fstamp)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 1])
mean(y_hat_v1[cl_data_n1 == 0])


table(cl_data)





