set.seed(02152016)

# create folds------------------------------------------------------------------------------------------

dataset_blend_train = matrix(0, nrow(train), 1)

dataset_blend_test_j = matrix(0, nrow(test), 5)

dataset_blend_test = matrix(0, nrow(test), 1)

# start iteration loop---------------------------------------------------------------------------------
  
for(j in 1:5)
  

  j = 1 
  
  print(paste("starting xgboost iteration ; number :", j))
  
  set.seed(2*12*2016*j)
  
  require(caret)
  
  skf = createFolds(response, k = 5)
    
  print(paste(nrow(dataset_blend_test_j),ncol(dataset_blend_test_j)))

  # start fold loop------------------------------------------------------------------------------------
  
    ### Loop over the folds
    
    i <- 0
    
    for (sk in skf) {
      
      i <- i + 1
      
      print(paste("Fold", i))
      
      ### Extract and fit the train/test section for each fold
      
      tmp_train <- unlist(skf[i])
      
      x_train = train[-tmp_train,]
      
      y_train = response[-tmp_train]
      
      x_test  = train[tmp_train,]
      
      y_test  = response[tmp_train]

        
        feature.names <- names(train)
        
        dtrain<-xgb.DMatrix(data=data.matrix(x_train),label=y_train, missing = NaN)

        param <- list( objective           = "reg:linear",
                        
                        depth = 21,
                        
                        min_child_weight = 40,
                        
                        subsample = 0.71,
                        
                        eta = 0.01,
                        
                        silent = 0
        )
        
        # start training------------------------------------------------------------------------------
        
        print(paste("training xgboost for iteration :", j, "Fold ; number :", i))
        
        mod <- xgb.train(   params              = param,
                            
                            booster = "gbtree",
                            
                            data                = dtrain,
                            
                            nrounds             = 3000,
                            
                            verbose             = 1,
                            
                            maximize            = F,
                            
                            nthread = 4
                            
        )
        
        dataset_blend_train[tmp_train, j] <- predict(mod, data.matrix(x_test), missing = NaN)
        
      
      
  print(paste("predicting xgboost for test set iteration:", j, " ; Fold :", i))
  
  dataset_blend_test_j[, i] <- predict(mod, data.matrix(test), missing = NaN)
  
}

  dataset_blend_test[, j] <- rowMeans(dataset_blend_test_j)
  


require(readr)

write_csv(data.frame(dataset_blend_train), "D:\\kaggle\\PRUDENTIAL\\blend\\bag\\xgb\\blend_train_xgb_02152016_6.csv")

write_csv(data.frame(dataset_blend_test), "D:\\kaggle\\PRUDENTIAL\\blend\\bag\\xgb\\blend_test_xgb_02152016_6.csv")

importance_matrix <- xgb.importance(feature_names = names(train), model = mod)

write_csv(data.frame(importance_matrix), "D:\\kaggle\\PRUDENTIAL\\Data\\imp_mat_raw.csv")

  
######################################################################################################
import apiai
import json
#print apiai.__version__
CLIENT_ACCESS_TOKEN='bdafa714ad254d06a9d289f08fcd79b0'
brand=''
type=''
price=''


def handle_query(query):
    ai = apiai.ApiAI(CLIENT_ACCESS_TOKEN)
    request = ai.text_request()
    request.lang = 'en'
    request.query = query
    response = request.getresponse()
    return (json.loads(response.read()))
def saveType(t):
    global type
    type=type

def saveBrand(b):
    global brand
    brand=brand

def savePrice(p):
    global price
    price=p

def main():
	user_input = ''

	#loop the queries to API.AI so we can have a conversation client-side
	while user_input != 'exit':
        print 'hello'
		user_input  = raw_input("me: ")
		#query the console with the user input, retrieve the response
		response = handle_query(user_input)
		#parse the response
		result = response['result']
		fulfillment = result['fulfillment']

		print 'bot: ' + fulfillment['speech']

		#if an action is deteted, fire the appropriate function
		if result['action'] == 'saveType':
			saveType(user_input)
		if result['action'] == 'saveBrand':
			saveBrand(user_input)
		if result['action'] == 'savePrice':
			savePrice(user_input)


#if __name__ == "__main__":
main()
print brand,type,price
   
