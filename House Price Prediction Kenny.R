setwd('G:/Imarticus/DSP_Online_2/Project1PropertyPricePrediction/Dataset')

# Initiating the libraries
library(Amelia) # For accessing the missmap function
library(dplyr)
library(Hmisc) # For using the impute function
library(ggplot2)
library(ModelMetrics) # For in built RMSE Calculation

property_train = read.csv('Property_Price_Train.csv')
str(property_train)

missmap(property_train)

# From the Missingness Map, we see there are 6% missing values.
# However, we observe that some of the rows have all data empty
# Lets omit the missing data in a new variable and check the number of remaining observations

trial_data = na.omit(property_train)

nrow(trial_data)
# We see that omitting the missing data is causing all the rows to be omitted.
# Hence omitting the missing data will not help.
rm(trial_data)

# Lets look at each of the variable observation distribution
table(property_train$Lot_Extent)

# Doing the same for 42 variables would be a tedious task.
# Hence, we will try and let R automatically do so for us.

# For that, we first store the variables of character datatype in one variable.
char_names = names(property_train[,sapply(property_train, is.character)])

# We have now stored all variables of character datatype in the variable "char_names"
char_names

# As they are categorical variables, let us convert them into factors.
property_train[,char_names]=lapply(property_train[,char_names], factor)

# Using summary, we can have a look at each of the factor variable and its distribution.
print(summary(property_train[char_names]))

# While summary does give us a concise view of the data distribution,
# some of it is missing as this function only outputs 6
# Another way to observe the distribution of each categorical variable is:

for (val in char_names){

    print(val)

    val_table = table(property_train[[val]], useNA = 'ifany')

    val_percent = round(prop.table(val_table)*100,2)

    print(cbind(val_table, val_percent))

    cat("\n")
}

rm(val,val_percent,val_table)

# Observing the data distribution, we see some highly skewed variables
# We need to exclude those variables

remove_vals = c("Road_Type",
                "Utility_Type",
                "Condition2",
                "Roof_Quality",
                "Heating_Type",
                "Pool_Quality",
                "Miscellaneous_Feature",
                "Id")

# The above variables are those in which one category had more than 95% observations.
# Since such variables may skew the regression analysis, we will remove them.



filtered_data = select(property_train, -all_of(remove_vals))

rm(remove_vals)

missmap(filtered_data)
# We notice there are still some rows that have lot of data missing.
# We need to remove those variables too.

# Let us look at the summary of each variable again
summary(filtered_data)

# Again summary function is useful, but not clear
# We need to see the number of NAs in each variable.
sapply(filtered_data, function(x) sum(is.na(x)))

# An alternate and aesthetic way to see the columnwise NAs
NA_columns <- colSums(is.na(filtered_data))
NA_columns = subset(NA_columns, NA_columns>0)
cat(paste(names(NA_columns), NA_columns, sep = ' : ', collapse = '\n'))

rm(NA_columns)
# From the above function, we notice the below variable have large number of NAs.
# Lane_Type = 1368
# Fireplace_Quality = 689
# Fence_Quality = 1178


filtered_data = subset(filtered_data,
                       select = -c(Lane_Type,
                                   Fireplace_Quality,
                                   Fence_Quality))


missmap(filtered_data)
# Upon checking the Missingness Map again, we see the Lot_Extent has 259 NAs.

259/1459

# That is 17% of the data is missing.
# There are two ways to go about this.
# 1. Remove all the 259 observations by using na.omit
# 2. Substitute the NAs by median or mean data

# Let us check the summary of the Lot_Extent first
summary(property_train$Lot_Extent)

# We see that median and mean are close to each other.
# Hence we can choose either of these to substitute the NAs.
# We will choose to replace NA with the median value of 69


filtered_data$Lot_Extent=impute(filtered_data$Lot_Extent, median)

missmap(filtered_data)
# Lets check the summary of the remaining variables with missing data

summary(filtered_data$Garage_Condition)
summary(filtered_data$BsmtFinType2)

81/1459
38/1459
# Since both the above variables are categorical,
# and the number of observations are just about 5%,
# we choose to omit instead of impute

filtered_data=na.omit(filtered_data)

missmap(filtered_data)

# Now that we have completed data cleaning, we will perform EDA


# Bar charts of Zoning Class vs Sum of Sale Price and mean of Sale Price
ggplot(filtered_data, aes(x=Zoning_Class, y=Sale_Price/1000))+
    geom_col()+
    ylab("Sale Price \n (in thousands)")+
    geom_text(stat = "summary", aes(label=round(stat(y),1)), fun=sum, vjust=-0.3)

ggplot(filtered_data, aes(x=Zoning_Class, y=Sale_Price/1000))+
    stat_summary(fun=mean, geom="bar")+
    ylab("Mean Sale Price (in thousands)")+
    geom_text(aes(label=round(stat(y),1)), stat = "summary", fun = mean, vjust = -0.3)

# As there are 70 variables, it might not make sense to draw charts for each.
# Hence we will select few variables to make charts on.
# To decide which variables to choose, we need to see the variables and their distribution
fact_names = names(filtered_data[,sapply(filtered_data, is.factor)])
fact_names


for (val in fact_names){
    print(val)
    val_table = table(filtered_data[[val]], useNA = 'ifany')
    val_percent = round(prop.table(val_table)*100,2)
    print(cbind(val_table,val_percent))
}

rm(val,val_percent,val_table)

Sale_Price_Mn = filtered_data$Sale_Price/1000000
SP_M = "Sale_Price_Mn"
Sale_Price_K = filtered_data$Sale_Price/1000
SP_K = "Sale_Price_K"

for (val in fact_names){
    print(val)

    plot_sum = ggplot(filtered_data, aes_string(x=val, y=SP_M))+
        geom_col(fill = "skyblue")+
        ylab("Sale Price \n (in millions)")+
        geom_text(stat = "summary", aes(label=round(stat(y),1)), fun=sum, vjust=-0.3)

    print(plot_sum)

    plot_mean = ggplot(filtered_data, aes_string(x=val, y=SP_K))+
        stat_summary(fun=mean, geom="bar", fill = "pink")+
        ylab("Mean Sale Price \n(in thousands)")+
        geom_text(aes(label=round(stat(y),1)), stat = "summary", fun = mean, vjust = -0.3)

    print(plot_mean)
}

# We have plotted charts for sum of Sale Price and mean of Sale Price for all categories of each variable
# Now lets see the other variables
other_vars = select(filtered_data, -all_of(fact_names))

summary(other_vars)

str(other_vars)

# We are satisfied with the observations.
# We will now proceed with the linear regression

# Sampling the data into train and test
# While splitting the data, we need to ensure that at least observation from each factor is within the train dataset
# Because if not, while applying the analysis on the test set, it will throw an error saying new factor found
# Upon trying multiple sampling methods, below is the one that was more convenient

# Kenny's Unique Sampling Method
# 1. Create sample of each factor variable with each unique category
# 2. Bind them all together and call it sample_z
# 3. Remove the duplicates
# 4. Remove those rows from the primary dataset
# 5. Split that dataset to train and test in 7:3 ratio.
# 6. Attach the sample_z to the train dataset

sample_z = filtered_data[0,]

for (name in fact_names){
  sample_x = filtered_data[unlist(by(rownames(filtered_data), filtered_data[name],
                                     function(x) sample(x, min(length(x),1) ) ) ),]
  sample_z = rbind(sample_x,sample_z)
}

filtered_data = anti_join(filtered_data,sample_z)


library(caTools) # To split the data into train and test
set.seed(124)
sample = sample.split(filtered_data, 0.7)
train = subset(filtered_data, sample == T)
test = subset(filtered_data, sample == F)

train = rbind(train,sample_z)
train=na.omit(train)

initial_model = lm(Sale_Price~., train)
summary(initial_model)
# The above linear model gives us R square of 0.8781.
# This means about 87% of the Sale_Price data is explained by all other variables

# Now we do the stepwise mode
stepwise_mode = step(initial_model, direction = "both")
summary(stepwise_mode)

# stepwise regression gives us the optimum variable combination for the model

optimum_model = lm(formula = Sale_Price ~ Lot_Extent + Lot_Size + Land_Outline +
                     Lot_Configuration + Neighborhood + Condition1 + House_Type +
                     House_Design + Overall_Material + House_Condition + Construction_Year +
                     Exterior1st + Exterior_Material + Basement_Height + Basement_Condition +
                     Exposure_Level + BsmtFinType1 + First_Floor_Area + Second_Floor_Area +
                     Underground_Full_Bathroom + Full_Bathroom_Above_Grade + Kitchen_Above_Grade +
                     Kitchen_Quality + Rooms_Above_Grade + Functional_Rate + Fireplaces +
                     Garage_Size + W_Deck_Area + Enclosed_Lobby_Area + Three_Season_Lobby_Area +
                     Month_Sold + Sale_Condition, data = train)

summary(optimum_model)

# We use the optimum model to predict the test data

test$predict = predict(optimum_model, test)

rmse(test$Sale_Price,test$predict)
mae(test$Sale_Price,test$predict)
range(test$Sale_Price)

# Considering the range of the dataset, our model seems pretty efficient.


boxplot(test$Sale_Price/1000,test$predict/1000,
        names = c('Actual','Predicted'),
        horizontal = TRUE,
        main = 'Comparision of Actual vs Predicted Sale Price')

# Predicting the Sale Price for the test file

property_test = read.csv('Property_Price_Test.csv')

property_test$Est_Sale_Price = predict(optimum_model,property_test)
# We notice the error that a new level 'NORMD' is present in the property_test data,
# which wasn't present in the property_train data.

# Let us drop that particular level.
# To do that we first need to convert the character datatypes into factors.

property_test <- property_test %>% mutate_if(is.character,as.factor)

#Merging the extra levels in the test data
levels(property_test$Condition1)
levels(property_test$Condition1) = c("Artery","Feedr","Norm","Norm","PosA",
                                     "PosN","RRAe","RRAn","RRNe","RRNn")

# Now let us re-run the prediction again.
property_test$Est_Sale_Price = predict(optimum_model,property_test)

#Merging the extra levels in the test data
levels(property_test$Exterior1st)
levels(property_test$Exterior1st) = c("AsbShng" ,"AsbShng", "BrkComm",
                                      "BrkFace", "CemntBd", "CemntBd",
                                      "HdBoard", "MetalSd" ,"Plywood",
                                      "Stucco","VinylSd","Wd Sdng","WdShing")


# Now let us re-run the prediction again.
property_test$Est_Sale_Price = predict(optimum_model,property_test)

#Merging the extra levels in the test data
levels(property_test$Sale_Condition)
levels(property_test$Sale_Condition) = c("Abnorml","Abnorml",
                                         "AdjLand","Alloca",
                                         "Family","Normal",
                                         "Normal","Partial")

# Now let us re-run the prediction again.
property_test$Est_Sale_Price = predict(optimum_model,property_test)
