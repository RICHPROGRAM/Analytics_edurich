library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(data.table)
library(dplyr)

set.seed(100)
## setting working directory
path <- "C:/Users/windows/Desktop/R/Big Mart Sales prediction"
setwd(path)

## loading data
train <- as.data.frame(fread("Train.csv"))
test <- as.data.frame(fread("Test.csv"))

target_train <- train[,12]

train$Item_Outlet_Sales <- NULL

tr_num <- nrow(train)
te_num <- nrow(test)
num <- tr_num + te_num

work <- rbind(train, test)

#Imputing missing data
for(i in 1: nrow(work))
{
  if (work$Outlet_Identifier[i] == "OUT010")
  {work$Outlet_Size[i] <- "Small"}
  else
    if (work$Outlet_Identifier[i] == "OUT017")
    {work$Outlet_Size[i] <- "High"}
  else if (work$Outlet_Identifier[i] == "OUT045")
  {work$Outlet_Size[i] <- "Medium"}
  #else
  #{work$Outlet_Size[i] <- work$Outlet_Size[i]}
}


# reg to Regular
for (i in 1:nrow(work))
{if (work$Item_Fat_Content[i] == "reg")
{work$Item_Fat_Content[i] <- "Regular"}
  else
    if (work$Item_Fat_Content[i] == "LF")
    {work$Item_Fat_Content[i] <- "Low Fat"}
  else
    if (work$Item_Fat_Content[i] == "low fat")
    {work$Item_Fat_Content[i] <- "Low Fat"}
}

# interpreting the missing weights

mean_item <- aggregate(work$Item_Weight, list(work$Item_Identifier), mean, na.rm=TRUE)

for (i in 1:nrow(work))
{
  if (is.na(work$Item_Weight[i]))
  {iden <- work$Item_Identifier[i];
  value <- mean_item[(which(mean_item$Group.1 == iden, arr.ind = TRUE)),2];
  work$Item_Weight[i] <- value}
}


# loading labesl of train data
#labels <- train['Outlet_Identifier']
#train <- train[-grep('labels', colnames(train))]
# one-hot-encoding categorical features

#work$Item_Fat_Content = gsub(1, "Regular", work$Item_Fat_Content)
#work$Item_Fat_Content = gsub(0,"Low Fat", work$Item_Fat_Content)

# one-hot-encoding categorical features
ohe_feats =c('Outlet_Size', 'Outlet_Location_Type', 'Outlet_Type')


dummies <- dummyVars(~Outlet_Size + Outlet_Location_Type + Outlet_Type, data=work)

work_ohe <- as.data.frame(predict(dummies, newdata = work))
work_combined <- cbind(work[,-c(which(colnames(work) %in% ohe_feats))], work_ohe)


ohe_ture =c('Item_Identifier' , 'Item_Fat_Content', 'Item_Type', 'Outlet_Identifier', 'Outlet_Establishment_Year' )

dummies2 <- dummyVars(~ Item_Identifier + Item_Fat_Content + Item_Type + Outlet_Identifier + Outlet_Establishment_Year, data = work_combined)
work_ture <- as.data.frame(predict(dummies2, newdata = work_combined))
work_all <- cbind(work_combined[,-c(which(colnames(work_combined) %in% ohe_ture))], work_ture)
#work_all$
work_all$Item_Identifier <- NULL

w_train <- work_all[(1:tr_num),]
w_test <- work_all[(tr_num+1:num),]


#training the model with xgboost
set.seed(1)
xgb <- xgboost(data = data.matrix(w_train), 
               label = target_train,
               eta = 0.05,
              # max_depth =15,
               nrounds = 25,
               #subsample =0.5,
               #colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "rmse",
               objective = "reg:linear",
               missing = NaN)
               #num_class = 12,
               #nthread = 3
               
predicted <- predict(xgb, data.matrix(w_test), missing = NaN)

Sub <- fread("Submission.csv")
head(Sub)
Sub$Item_Outlet_Sales <- predicted

write.csv(Sub, "Submission.csv")


model <- xgb.dump(xgb, with.stats = T)
names <- dimnames(data.matrix(w_train))[[2]]

importance_matrix <- xgb.importance(names, model = xgb)
xgb.plot.importance(importance_matrix[1:10,])

barplot(importance_matrix)

find <- which(work_all$`Item_Fat_Contentlow fat` == 1, arr.ind = TRUE)

for (i in 1:nrow(work))
{ if (work$Item_Fat_Content[i] == "low fat")
{work$Item_Fat_Content[i] <- "Low Fat"}
}
