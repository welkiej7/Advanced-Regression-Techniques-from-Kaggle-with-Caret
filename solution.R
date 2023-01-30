library(tidyverse)
library(mice) 
library(caret)
library(corrplot)
library(rsample)
library(fastDummies)



train <- read.csv("/Users/onurtuncaybal/Documents/Code/Advanced Regression Techniques/house-prices-advanced-regression-techniques/train.csv", stringsAsFactors = TRUE) # nolint: line_length_linter.
test <- read.csv("~/Documents/Code/Advanced Regression Techniques/house-prices-advanced-regression-techniques/test.csv",stringsAsFactors = TRUE)
proportion_na_values <- sum(is.na(train)) / (nrow(train) * ncol(train))
#Which columns contain more NA values
columns_with_na_values <- vector()
for (col in 1:ncol(train)){
    columns_with_na_values[col] <- print(sum(is.na(train[, col])))
}
na_df <- tibble(values = columns_with_na_values/nrow(train))
#NA values indicate the absence of certain criteria. Except the one with electrical
#We are going to drop that row. 
train <- train[-is.na(train$Electrical),]
#Changing NA values to a factor. Take factors and treat them as a different data frame. #nolint
train_factors <- vector()
for(col in 1:ncol(train)){
    if(is.factor(train[,col])){
        train_factors[col] <- TRUE
    } else {
        train_factors[col] <- FALSE
    }
}

proportion_na_values <- sum(is.na(test)) / (nrow(test) * ncol(test))
#Which columns contain more NA values
columns_with_na_values <- vector()
for (col in 1:ncol(test)){
    columns_with_na_values[col] <- print(sum(is.na(test[, col])))
}
test_na_df <- tibble(values = columns_with_na_values/nrow(train))

train_factors <- train[,train_factors]
for (col in 1:ncol(train_factors)){
    train_factors[,col] <- factor(train_factors[,col], 
    levels = c(levels(train_factors[,col]),"!E"))
} 
#replace
train_factors <- replace(train_factors,is.na(train_factors),"!E")
#replace altered columns
factor_columns <- which(colnames(train) %in% colnames(train_factors))
train <- cbind(train_factors,train[,-factor_columns]) 
##Now we should only have NA values in numeric variables, let's check
sum(is.na(train)) #348 total NA. Now we can impute the numeric variables.
##Yearsold and Monthsold should also be factors. 
train$YrSold <- as.factor(train$YrSold)
train$MoSold <- as.factor(train$MoSold)
##NOTE: Numeric NA values ara available, please find them and impute them.
#(Find a logical relationship)
columns_with_na_values <- vector()
for (col in 1:ncol(train)){
    columns_with_na_values[col] <- print(sum(is.na(train[, col])))
}
na_df <- tibble(values = columns_with_na_values/nrow(train))
#Let's focus on 46th,52nd,69th columns with logic and fill the NA values. 
#What is missing? 
colnames(train)[c(46,52,69)]
#Lot frontage = Linear feet of street connected to property
#MasVnrArea = Masonry veneer area in square feet
#GaragaYrBlt = Year garage was built

#Let's start with the year that the garage is built.
which(train$GarageYrBlt %in% c(NA)) == which(train$GarageType %in% c("!E"))
#It turns out we don't have missing data in here and GarageYrBlt is missing due to 
#lack of a garage. Therefore We don't need to impute this variable.

#########GARAGE
#Let's look at the relationship between garage quality and the year built. 
ggplot(data = train, mapping = aes(GarageYrBlt, color = GarageCond)) + geom_boxplot()

#Indeed we can see a correlation with the garage quality and the year build. Therefore
#We can change year value with a factor, with a mean approach.

max(train$GarageYrBlt,na.rm = TRUE) #Newest Garage 2010
min(train$GarageYrBlt,na.rm = TRUE) #Oldest Garage 1900

train%>%mutate(GarageYrBlt = case_when(GarageYrBlt < 1923 ~ "Extremely Old",
                                    GarageYrBlt < 1955 ~ "Old",
                                    GarageYrBlt > 1955 ~ "New",
                                    GarageYrBlt > 1978 ~ "Extremely New")) -> train_garage
train_garage$GarageYrBlt <- as.factor(train_garage$GarageYrBlt)
is.factor(train_garage$GarageYrBlt) #Transformation is okay we can set this as our training set.

train <- train_garage
train$GarageYrBlt <- factor(train$GarageYrBlt, levels = c(levels(train$GarageYrBlt),"!E"))
train$GarageYrBlt <- replace_na(train$GarageYrBlt, "!E")
#Factoring is complete! 
#Now let's change the year built in to a factor variable
max(train$YearBuilt) #2010 is the newest house

min(train$YearBuilt)# 1872 is the oldest house
 #1946 is the middle year and 1909 is the quarter, 1983 is the upper quantile
train <- train%>%mutate(YearBuilt = case_when(YearBuilt > 1983 ~ "Extremely New",
                                     YearBuilt > 1946 ~ "New",
                                     YearBuilt > 1909 ~ "Old",
                                     YearBuilt > 1872 ~ "Extremely Old"))
train$YearBuilt <- as.factor(train$YearBuilt)
#One NA value, probably an old lot. We can analyze this. 
train%>%filter(Neighborhood == "OldTown")%>%ggplot(mapping = aes(x = YearBuilt)) + geom_bar()

#It is likely that this building is also old. We can manually median impute and say "Old"
train$YearBuilt[1349] <- "Old"
#Another important variable is remodeling in year. Generally Remodeling increases the value of the house,
#Let's factorize this column too. 
max(train$YearRemodAdd) #2010 is the max value
min(train$YearRemodAdd) #1950 is the min value

train <- train%>%mutate(YearRemodAdd = case_when(YearRemodAdd >= 1950 ~ "Extremely Old",
                                        YearRemodAdd > 1965 ~ "Old",
                                        YearRemodAdd > 1980 ~ "New",
                                        YearRemodAdd>1995 ~ "Extremely New"))
columns_with_na_values <- vector()
for (col in 1:ncol(test)){
    columns_with_na_values[col] <- print(sum(is.na(test[, col])))
}
test_na_df <- tibble(values = columns_with_na_values/nrow(train))

#For not to mismatch everything I have a habit to name variables with dots
# in training set instead of underscores. 
test.factors.test <- vector()
for(col in 1:ncol(test)){
    if(is.factor(test[,col])){
        test.factors.test[col] <- TRUE
    } else {
        test.factors.test[col] <- FALSE
    }
}

test.factors <- test[,test.factors.test]
for(col in 1:ncol(test.factors)){
    test.factors[,col] <- factor(test.factors[,col],
    levels = c(levels(test.factors[,col]), "!E"))
}
test.factors <- replace(test.factors,is.na(test.factors),"!E")
factor.columns <- which(colnames(test) %in% colnames(test.factors))
test <- cbind(test.factors,test[,-factor.columns])

sum(is.na(test)) ##330 NA values, let's see how we can handle these. 

test$YrSold <- as.factor(test$YrSold)
test$MoSold <- as.factor(test$MoSold)


columns.with.na.values <- vector()
for(col in 1:ncol(test)){
    columns.with.na.values[col] <- print(sum(is.na(test[,col])))
}
na.df <- tibble(values = columns.with.na.values/nrow(test))
View(na.df)
na.df <- rowid_to_column(na.df,var = "rowid")
colnames(test)[which(na.df$values > 0)]


test%>%mutate(GarageYrBlt = case_when(GarageYrBlt < 1923 ~ "Extremely Old",
                                      GarageYrBlt < 1955 ~ "Old",
                                      GarageYrBlt > 1955 ~ "New",
                                      GarageYrBlt > 1978 ~ "Extremely New")) -> test.garage
test.garage$GarageYrBlt <- as.factor(test.garage$GarageYrBlt)
is.factor(test.garage$GarageYrBlt)
test <- test.garage
test$GarageYrBlt <- factor(test$GarageYrBlt, levels = c(levels(test$GarageYrBlt),"!E"))
test$GarageYrBlt <- replace_na(test$GarageYrBlt,"!E")



test <- test%>%mutate(YearBuilt = case_when(YearBuilt > 1983 ~ "Extremely New",
                                     YearBuilt > 1946 ~ "New",
                                     YearBuilt > 1909 ~ "Old",
                                     YearBuilt > 1872 ~ "Extremely Old"))
test$YearBuilt <- as.factor(test$YearBuilt)

test <- test%>%mutate(YearRemodAdd = case_when(YearRemodAdd >= 1950 ~ "Extremely Old",
                                        YearRemodAdd > 1965 ~ "Old",
                                        YearRemodAdd > 1980 ~ "New",
                                        YearRemodAdd>1995 ~ "Extremely New"))
mice.train <- mice(train, m = 3, method = "rf", maxit = 2)
mice.test <- mice(test, method =  "rf", m= 3, maxit = 2)

train_miced <- complete(mice.train)
test.miced <- complete(mice.test)

#Seperate SalePrice
sale_price <- train_miced[,which("SalePrice" == colnames(train_miced))]
train_miced <- train_miced[,-which("SalePrice" == colnames(train_miced))]

 all_data <- rbind(train_miced,test.miced)
all_data_dummy <- dummy_cols(all_data, remove_selected_columns = TRUE)

#1459 - 1459 seperation.

train_dummy <- all_data_dummy[1:1459,]
test.dummy <- all_data_dummy[1460:nrow(all_data_dummy),]


##Center and Scale. Drop the Correlated Values

train_dummy <- train_dummy[,-which("OverallQual" == colnames(train_dummy))]
test.dummy <- test.dummy[,-which("OverallQual" == colnames(test.dummy))]

train_id <- train_dummy$Id
test.id <- test.dummy$Id

train_dummy <- train_dummy[,-1]
test.dummy <-  test.dummy[,-1]















train_dummy_numeric <- train_dummy[,1:31]
test.dummy.numeric <- test.dummy[,1:31]




pp_train<- preProcess(train_dummy_numeric, c("center","scale"))
pp.test <- preProcess(test.dummy.numeric, c("center","scale"))


train_dummy_numeric <- predict(pp_train, train_dummy_numeric)
test.dummy.numeric <- predict(pp.test, test.dummy.numeric)


train_dummy_factor <- train_dummy[,32:ncol(train_dummy)]
test.dummy.factor <- test.dummy[,32:ncol(test.dummy)]


train_data_without_saleprice <- cbind(train_dummy_numeric,train_dummy_factor)
test.data <- cbind(test.dummy.numeric,test.dummy.factor)

train_data <- cbind(train_data_without_saleprice,sale_price)
colnames(train_data)[ncol(train_data)] <- "SalePrice"
##Datasets are ready

dim(train_data)
dim(test.data)


##It is time to train our model
set.seed(20)
split_object <- rsample::initial_split(train_data, prop = 0.75)
split_training <- rsample::training(split_object)
split_testing <- rsample::testing(split_object)


control.object <- trainControl(method = "repeatedcv",
                                number = 5,
                                repeats = 5)

gs.object <- expand.grid(nrounds = 250,
            max_depth = c(3,4),
            eta = 0.3,
            gamma = 0.1,
            colsample_bytree = c(0.5,0.6),
            min_child_weight = 1,
            subsample = 1)

##Our grid search is comprehensive, therefore it is possible that model will take 
## a lot of time to train (Approx ~ 13 - 20m in Apple M1) depending on the machine. 

model.initial <- train(SalePrice ~ .,
                    data = split_training,
                    trControl = control.object,
                    verbose = TRUE,
                    method = "xgbTree",
                    tuneGrid = gs.object)




results.initial <- as.data.frame(model.initial$results)
##Let's predict on the test data. 


pred <- predict(model.initial, split_testing)

postResample(pred, split_testing$SalePrice)

##0.86 R Squared is pretty good. Let's train the final model. 

model.initial <- train(SalePrice ~ .,
                    data = split_training,
                    trControl = control.object,
                    verbose = TRUE,
                    method = "xgbTree",
                    tuneGrid = gs.object)
View(as.data.frame(model.initial$results))
##I have selected two models,
##Following is the grid parameters.
#1 - eta = 0.3, max_depth = 3, gamma = 0.1, colsample = 0.6, 1 , 1, 250)
#2 - eta 0.3, max_depth 4, gamma 0.1, colsample 0.5, 1, 1, 250)

model.fin.data<- rbind(split_training,split_testing)


grid.object.fin <-  expand.grid(eta = 0.3,
                                    max_depth = 3, 
                                    gamma = 0.1,
                                    colsample_bytree = 0.6,
                                    min_child_weight = 1,
                                    subsample = 1,
                                    nrounds = 250)
control.object.fin <- trainControl(method = "repeatedcv",
                                    repeats = 5,
                                    number = 5)

model.fin.fin <- train(SalePrice ~ .,
                    data = model.fin.data,
                    trControl = control.object.fin,
                    verbose = TRUE,
                    method = "xgbTree",
                    tuneGrid = grid.object.fin)


##Let's predict on test data.
test.prediction <- predict(model.fin.fin,test.data)


plot(test.prediction)
sample_submission <- read.csv("~/Documents/Code/Advanced Regression Techniques/house-prices-advanced-regression-techniques/sample_submission.csv",stringsAsFactors = TRUE)
sample_submission$SalePrice <- test.prediction


write.csv(sample_submission, "~/Desktop/submission.csv",row.names = FALSE)
