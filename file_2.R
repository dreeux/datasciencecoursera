library(xgboost); library(data.table); library(doParallel); library(pROC); library(bit64)

set.seed(3*04*14*2016)

train <- fread("D:\\local\\KAGGLE\\SANTANDER\\DATA\\train.csv", data.table = F)

test <- fread("D:\\local\\KAGGLE\\SANTANDER\\DATA\\test.csv", data.table = F)


# Removing IDs-------------------------------------------------------------------------------------------

train$ID <- NULL

test.id <- test$ID

test$ID <- NULL


# Extracting TARGET--------------------------------------------------------------------------------------

train.y <- train$TARGET

train$TARGET <- NULL


# 0 count per line---------------------------------------------------------------------------------------

count0 <- function(x) {

    return( sum(x == 0) )

  }

train$n0 <- apply(train, 1, FUN=count0)

test$n0 <- apply(test, 1, FUN=count0)


# Removing constant features-----------------------------------------------------------------------------

cat("\n## Removing the constants features.\n")

for (f in names(train)) {

    if (length(unique(train[[f]])) == 1) {

          cat(f, "is constant in train. We delete it.\n")

          train[[f]] <- NULL

              test[[f]] <- NULL

                }

  }


# Removing identical features----------------------------------------------------------------------------

features_pair <- combn(names(train), 2, simplify = F)

toRemove <- c()

for(pair in features_pair) {

    f1 <- pair[1]

      f2 <- pair[2]
  

        if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {

              if (all(train[[f1]] == train[[f2]])) {

                      cat(f1, "and", f2, "are equals.\n")

                      toRemove <- c(toRemove, f2)

                          }

            }

      }


feature.names <- setdiff(names(train), toRemove)

# feature.names <- (names(train))

train <- train[, feature.names]

test <- test[, feature.names]


# creation of holdout set-------------------------------------------------------------------------------------

h <- sample(nrow(train), 10000)

training <- train[-h, ]; training_response <- train.y[-h]

holdout <- train[h, ]; holdout_response <- train.y[h]


# training a model--------------------------------------------------------------------------------------------


dtrain <- xgb.DMatrix(data=data.matrix(training), label=training_response, missing = NaN)

watchlist <- list(train=dtrain)

param <- list(  objective           = "binary:logistic", 
                
                booster             = "gbtree",
                
                eval_metric         = "auc",
                
                eta                 = 0.0202048,
                
                max_depth           = 5,
                
                subsample           = 0.6815,
                
                colsample_bytree    = 0.701
)


cl <- makeCluster(detectCores()); registerDoParallel(cl)

clf <- xgb.train( params              = param, 
                  
                  data                = dtrain, 
                  
                  nrounds             = 570, 
                  
                  verbose             = 1,
                  
                  watchlist           = watchlist,
                  
                  maximize            = T
)


# prediction and auc calculation using holdout------------------------------------------------------------------------------------

# check below code for NA

preds <- predict(clf, data.matrix(holdout), missing = NaN)

auc(response = holdout_response, predictor = preds )


# create submission file---------------------------------------------------------------------------------------


preds <- predict(clf, data.matrix(test), missing = NaN)

preds <- predict(clf, data.matrix(test), missing = NaN, ntreelimit = 560)



submission <- data.frame(ID=test.id, TARGET=preds)

cat("saving the submission file\n")

write.csv(submission, "submission_04222016.csv", row.names = F)


##############################################################################################################

library('pROC')
for (i in 0:4) {
  roc1 <- roc(cv[cv$fold == i,]$TARGET,cv[cv$fold == i,]$pred.x,percent=TRUE,
              plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,print.auc=TRUE, 
              show.thres=TRUE)
  roc2 <- roc(cv[cv$fold == i,]$TARGET,cv[cv$fold == i,]$pred.y,plot=TRUE, 
              add=TRUE, percent=roc1$percent, col="red")
  Sys.sleep(10)
}
