install.packages("tidyverse")
library(tidyverse)
install.packages("readr")
library(readr)
install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot")
library(corrplot)
install.packages("dplyr")
library(dplyr)
install.packages("reshape2")
library(reshape2)
library(tidyr)

#import data from csv file
train_data <- read_csv("train.csv")
t <- train_data


#analysis of every column
#view(train_data)
summary(train_data)

#check the count of groups
tapply(t$MSZoning, t$MSZoning, length)
table(t$LotShape)

#check correlation between LotArea and SalePrice
cor(t$LotArea, t$SalePrice)

#check corelation between YearBuilt and YearRemod
nt <- select(t, SalePrice, LotArea, YearBuilt, YearRemodAdd)
cor_m <- cor(nt, method = "pearson")
cor_m
corrplot(cor_m, method = "number")

#select all numeric column
nums <- unlist(lapply(t, is.numeric))
nums_tab <- t[, nums]

#select all categotical column
categor_tab <- select_if(t, negate(is.numeric))

#check and analysis of categorical and continuous tab
View(nums_tab)
View(categor_tab)
categor_tab


#transform continuous features to see corelations between features
nums_tabel <- gather(data = nums_tab, MSSubClass:YrSold , key = "feature", value = "feat_amount")
View(nums_tabel)

#all values in relation to SalePrice
ggplot(data = nums_tabel, mapping = aes(x = feat_amount, y = SalePrice)) + 
  geom_line() +
  facet_wrap(~feature, nrow = 5, scales = "free")


#corelation matrix
cor_m <- cor(nums_tab, method = "pearson")
corrplot(cor_m, method = "number", type = "upper", number.cex = .7)


#check corelation of chosen features (for better view)
temp_tabel <- select(nums_tab, SalePrice, LotArea, YearBuilt, YearRemodAdd, OverallQual, OverallCond, `1stFlrSF`, GrLivArea, FullBath, GarageArea)
cor_m <- cor(temp_tabel, method = "pearson")
cor_m
corrplot(cor_m, method = "number", type = "upper")

#check corelation - Basement related features
temp_tabel2 <- select(nums_tab, BsmtFinSF1:GrLivArea, SalePrice)
cor_m <- cor(temp_tabel2, method = "pearson")
cor_m
corrplot(cor_m, method = "number", type = "upper")
#Take TotalBsmtSF


#check corelation - Bath and Fireplaces related features
temp_tabel3 <- select(nums_tab, SalePrice, GrLivArea, OverallQual, BsmtFullBath:Fireplaces)
cor_m <- cor(temp_tabel3, method = "pearson")
cor_m
corrplot(cor_m, method = "number", type = "upper")
#Take Fireplaces

#check corelation - Garage related features
temp_tabel4 <- select(nums_tab, SalePrice, GrLivArea, OverallQual, GarageYrBlt:GarageArea)
cor_m <- cor(temp_tabel4, method = "pearson")
cor_m
corrplot(cor_m, method = "number", type = "upper")

#check new created IsGarage feature (because of NAs)
temp_tabel4 <- select(nums_tab, SalePrice, GrLivArea, OverallQual, GarageYrBlt:GarageArea)
temp_tabel4 <- mutate(temp_tabel4, IsGarage = ifelse(GarageCars == 0, 0, 1))
cor_m <- cor(temp_tabel4, method = "pearson")
cor_m
corrplot(cor_m, method = "number", type = "upper")
#Take GarageCars

#check corelation - Other
temp_tabel5 <- select(nums_tab, SalePrice, GrLivArea, OverallQual, WoodDeckSF:YrSold)
cor_m <- cor(temp_tabel5, method = "pearson")
cor_m
corrplot(cor_m, method = "number", type = "upper")

#check corelation between features using barplot
#garage and YearBuilt
garage.mean <- t(tapply(nums_tab$GarageCars, nums_tab$YearBuilt, mean))
barplot(garage.mean)

#GrLivArea and YearBuilt
size.mean <- t(tapply(nums_tab$GrLivArea, nums_tab$YearBuilt, mean))
barplot(size.mean)

#SalePrice and YearBuilt - visible trend
value.mean <- t(tapply(nums_tab$SalePrice, nums_tab$YearBuilt, mean))
barplot(value.mean)

#SalePrice and GrLivArea - strong trend
value2.mean <- t(tapply(nums_tab$SalePrice, nums_tab$GrLivArea, mean))
barplot(value2.mean)

#continuous columns to further analysis
#YearRemodAdd, FullBath, GarageArea, OverallCond, 1stFlrArea, GarageArea

#create dataset to analysis
tab_continuous <- select(nums_tab, OverallQual, YearRemodAdd, TotalBsmtSF, Fireplaces, GarageCars, GrLivArea, SalePrice)

tab_continuous
View(tab_continuous)

#tranform Neighborhood categorical feature into numerical (to use in regression - more expensive districts etc.)
neighborhood <- as.numeric(factor(categor_tab$Neighborhood))
neighborhood
cor_m <- cor(nums_tab$SalePrice, neighborhood, method = "pearson")
tab_continuous <- add_column(tab_continuous, neighborhood)

#save dataset to csv to compare results using Azure ML
#write.csv(tab_continuous, "HousePrices.csv")

#check corelation final
cor_final <- cor(tab_continuous, method = "pearson")
cor_final
corrplot(cor_final, method = "number", type = "upper")

lm_test <- lm(SalePrice ~ log(GrLivArea), data = tab_continuous)
resid_sd <- residuals(lm_test)
tab_test <- add_column(tab_continuous, resid_sd)
cor_test <- cor(tab_test, method = "pearson")
corrplot(cor_test, method = "number", type = "upper")

###############################################################################################################################

#Feature Engeenering

ggplot(tab_continuous, mapping = aes(x = TotalBsmtSF, y = SalePrice)) +
  scale_x_continuous(limits = c(0, 2500)) +
  geom_line() +
  geom_smooth()

ggplot(tab_continuous, mapping = aes(x = YearRemodAdd, y = SalePrice)) +
  geom_line() +
  geom_smooth()

ggplot(tab_continuous, mapping = aes(x = GrLivArea, y = SalePrice)) +
  geom_line() +
  scale_x_continuous(limits = c(0, 3000)) +
  geom_smooth()

summary(tab_continuous)
tab_continuous <- mutate(tab_continuous, YearRemodAdd = 2019 - YearRemodAdd)

boxplot(tab_continuous$GrLivArea)
plot(tab_continuous$GrLivArea)
boxplot(tab_continuous$YearRemodAdd)
plot(tab_continuous$YearRemodAdd)
summary(tab_continuous$YearRemodAdd)
data.frame(table(tab_continuous$YearRemodAdd))
#count number of rows with specific year - last year 1950
View(filter(tab_continuous, tab_continuous$YearRemodAdd == 69))

summary(tab_continuous$OverallQual)
summary(tab_continuous$TotalBsmtSF)
boxplot(tab_continuous$TotalBsmtSF)
#one value over 6000 - delete as outliers
tab_continuous <- filter(tab_continuous, tab_continuous$TotalBsmtSF < 5000)

summary(tab_continuous$Fireplaces)

summary(tab_continuous$GarageCars)

summary(tab_continuous$GrLivArea)
boxplot(tab_continuous$GrLivArea)
tab_continuous <- filter(tab_continuous, tab_continuous$GrLivArea < 4000)
tab_continuous <- mutate(tab_continuous, price = tab_continuous$SalePrice/tab_continuous$GrLivArea)
tab_continuous <- mutate(tab_continuous, under_priced = ifelse(price<60 & OverallQual > 8, 1, 0))
tab_continuous <- filter(tab_continuous, under_priced == 0)
tab_continuous <- select(tab_continuous, OverallQual:neighborhood)


#View(tab_continuous)

tab_continuous <- tab_continuous[c(1,2,3,4,5,6,8,7)]

#stats for taken columns
nums_tabel_part1 <- gather(data = tab_continuous, OverallQual:neighborhood , key = "feature", value = "feat_amount")

ggplot(data = nums_tabel_part1, mapping = aes(x = feat_amount, y = SalePrice)) + 
  geom_point() +
  facet_wrap(~feature, nrow = 5, scales = "free") +
  geom_smooth()

ggplot(data = nums_tabel_part1, mapping = aes(x = SalePrice, y = feat_amount, group = feature)) + 
  geom_boxplot(outlier.size = 1.5, na.rm = FALSE, notch = FALSE) +
  coord_flip() +
  facet_wrap(~feature, nrow = 7, scales = "free")


#check corelation 
cor_final <- tab_continuous
corf <- cor(cor_final, method = "pearson")
corf
corrplot(corf, method = "number", type = "upper")

#split data into train and test dataset
smp_size <- floor(0.85 * nrow(tab_continuous))
set.seed(123)
train_ind <- sample(seq_len(nrow(tab_continuous)), size = smp_size)
train <- tab_continuous[train_ind, ]
test <- tab_continuous[-train_ind, ]
train
test


#check the autocorrelation of residuals by Durbin - Watson test
#1450.   7.  1.90535  1.92197
#If D-W > Du = 1.92 then autocorrelation does not exists
install.packages("car")
library(car)
lm_con <- lm(SalePrice ~ OverallQual + YearRemodAdd + TotalBsmtSF + Fireplaces + GarageCars + GrLivArea + neighborhood, data = train)
lm_con

p <- predict(lm_con, test)
test <- add_column(test, p)
test <- mutate(test, diff = (p - SalePrice))
sum(abs(test$diff/nrow(test)))
#View(predict(lm_con, train))
test <- mutate(test, diff_prc = (abs(diff)/SalePrice)*100)
mean(test$diff_prc)
prc_results <- sort(test$diff_prc)
barplot(prc_results)
summary(prc_results)

#Check without neighborhood feature
# lm_con <- lm(SalePrice ~ OverallQual + YearRemodAdd + TotalBsmtSF + Fireplaces + GarageCars + GrLivArea, data = train)
# test <- select(test, -neighborhood)
# p <- predict(lm_con, test)
# test <- add_column(test, p)
# test <- mutate(test, diff = (p - SalePrice))
# sum(abs(test$diff/nrow(test)))
# View(predict(lm_con, train))
# test <- mutate(test, diff_prc = (abs(diff)/SalePrice)*100)
# mean(test$diff_prc)
# prc_results <- sort(test$diff_prc)
# barplot(prc_results)
# summary(prc_results)


#check results for original test dataset

test_dataset <- read.csv("test.csv")
neighborhood <- as.numeric(factor(test_dataset$Neighborhood))
test_dataset <- add_column(test_dataset, neighborhood)
test_dataset <- select(test_dataset, OverallQual, YearRemodAdd, TotalBsmtSF, Fireplaces, GarageCars, GrLivArea, neighborhood)
test_dataset <- mutate(test_dataset, YearRemodAdd = 2019 - YearRemodAdd)

lm_con <- lm(SalePrice ~ OverallQual + YearRemodAdd + TotalBsmtSF + Fireplaces + GarageCars + GrLivArea + neighborhood, data = tab_continuous)

Price_Predicted <- predict(lm_con, test_dataset)
test_dataset <- add_column(test_dataset, Price_Predicted)
#View(test_dataset)

durbinWatsonTest(lm_con)


install.packages("olsrr")
library(olsrr)

#check if resids have normal distribution
ols_plot_resid_qq(lm_con)
ols_test_correlation(lm_con)
ols_plot_resid_hist(lm_con)



#check model with reducing features

lm_con2 <- lm(SalePrice ~ OverallQual + TotalBsmtSF +  GrLivArea, data = train)
lm_con2

p2 <- predict(lm_con2, test)
test <- add_column(test, p2)
test <- mutate(test, diff = (p2 - SalePrice))
sum(abs(test$diff/219))
#View(predict(lm_con, train))
#View(test)
test <- mutate(test, diff_prc = (abs(diff)/SalePrice)*100)
mean(test$diff_prc)




