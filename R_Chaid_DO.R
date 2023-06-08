

##Essentially I am constructing a model and then predicting it on the same data set we used to construct it.

##Take a random sample of the data and create the training data set.



set.seed(32)

train.flag <- createDataPartition(y=cola_binary$choice,p=0.5,list=FALSE)
#Line above is flagging observations for the training data set and the testing data set.The(p=0.5) represents giving us a 50/50 split. He leaves his at 70/30 so 0.7
training <- cola_binary[train.flag,]
#Line above says: For the training data set, "cola_binary" is the data set we use. "train.flag" says pull out ones with the training flag
testing <- cola_binary[-train.flag,]
#Line above says give me the rest.

#This is where I create two data sets, ones called testing and one is called training ^^
  
#Looking at summary statistics right quick.
  
table(training$price3)

table(testing$price3)

#now I do what I did before, run the exact same model but using only the training data. When I ran it before we used the full data set.(Training below)

set.seed(32)

training_cola <- chaid(choice - price3 + feature + display, data - training)
        
plot(training_cola, type = "simple", gp = gpar(fontsize = 8)
     
#Comparing our orginal binary tree with the full data set and the training model, I notice fewer observations because using only p=0.7 of the data. 
#My tree structure doesnt change. Thats a good thing in the sense that our sub-sample is pretty represenative of our full sample. It is not a biased sample one way or other. Theoretically it should be pretty similar to the full model.


#do what I did before with the full data set, constructed the model with the full data set and tested it on it self,lets do the same thing with the training data set.

predicted_training <- predict(training_cola,data = training_cola,data = training)

#Above is the predicted for training,the model and predicted on self. Now I construct confusion matrix.

confusionMatrix(predicted_training, training$choice, positive = '1')

#Above we have our predicted, and our actuals, but it is the actuals of just the training data. So now I compare this with model I ran before and use excel to compare (copy table, data dropdown excel, text to columns, delimitted, other, then choose colon :


####### NOW I'M GOING TO TAKE THE MODEL I CONSTRUCTED IN THE TRAINING DATA SET AND PREDICT IT ONTO THE TESTING DATA. 

predicted_test <- predict(training_cola, newdata = testing)

### above I have our training model, and next is the dataset we predict onto, using the word newdata. Essentially we take training data set and predict it onto the newdata set which is the testing data.

confusionMatrix(predicted_test, testing$choice, positive = '1')

#Reminder about confusion matrix....takes our predictions and these are our predictions for our training model to our test data which he called predicted_test and want to predict onto the testing data (testing$choice)
#Copy pastes results and puts them into excel with the other confusion matrix.( Before was our Training, this is our Testing)

#Cross-Validation

train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = TRUE,
                              savePredictions = "final")

#Breaking down code above: "cv" = cross validation..."number 5" is the number of cross validations, I want 10 CV's.... Verbos means we want to see the output, and then save the output in something called final.
#Note---trainControl...has a capital C on purpose...

features <- setdiff(names(training[,c(-1,-2,-3,-6)]), "choice")
x <- training[, features]
y <- training$choice

#Above is me fine tuning the model. 

cola_chaid1 <- train(
  x = x,
  y = y,
  method = "chaid",
  metric = "Kappa",
  trControl = train_control
)

#Before in line 65 I save predictions under "final"..so the next code relates to that.

cola_chaid1$finalModel

#Now plot final model

plot(cola_chaid1$finalModel,type = "simple",
     gp = gpar(fontsize = 8))

#My final model changes, it got rid of a lot of stuff that wasnt needed, one way to improve model, trim it down. And our cross validation got rid of it.

confusionMatrix(cola_chaid1)
confusionMatrix(predict(cola)chaid1), y)

cola_chaid1$bestTune

#This best tune finds the best alphas, the alphas that maximize our cappa coeffcient.

varimp(training_cola)

#Above is a variable importance from the part kit package, essentially it just ranks my variables in terms of importance, which when you have relatively big models one of the hardest thing to do is deciding which variables to trim out.
#Could also do it on testing too instead of training.

#Lroc Curve. Receiver Operating Curve. There is a package for that..

install.packages("ROCR")
library(ROCR)


predicted_prob <- predict(training_cola, newdata = testing,type = "prob")

prediction <- prediction(predicted_prob[,2],testing$choice)

#Line 113 are the predictions for the training data on testing data.
#Line 115 is taking predicted probabilitys which we just did, and create prediction which are the predictions again on the testing data.

performance <- performance(prediction, "tpr","fpr")

#Above new variable called performance, the command takes the prediction, then the true positive rate & false positive rate which is what we use in the roc curve.

plot(performance,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1, lwd = 2, lty = 3,col = "black")

#We got our ROC Curve. (1:19:00 in lecture) Now we ask for area under the curve.

auc_perf - performance(prediction, measure - "auc")
auc_perf@y.values

#Remember a bad model is an area under curve of about 50%. You want the roc curve to be hugging the borders, ideally.. in a good model.








#**Helpful commands. rm(list - ls())..... this clears your environment when it gets too busy..... is.factor(price3))....self explanatory.


cola_binary$price3<- cut(cola_binary$price, 3)




set.seed(32)

cola_chaid1 <- chaid(choice ~ price3 + feature + display, data = cola_binary)

plot(cola_chaid1,
     main = "CHAID Model",
     gp = gpar(fontsize = 8),
     type = "simple")

?chaid_control

ctrl <- chaid_control(minbucket = 5, minsplit = 10, alpha2 = .05, alpha4 =.05)

#Min bucket is number of obs needed in terminal node. 5 is small. Min split size is at each node.

set.seed(32)

cola_chaid2 <- chaid(choice ~ price3 + feature + display,
                     data = cola_binary,control = ctrl)

plot(cola_chaid1,
     main = "CHAID Model with controls",
     gp = gpar(fontsize = 8),
     type = "simple")

#Nothing changed, didnt matter. Sometimes you want to do opposite and increase min buckets.

ctrl <- chaid_control(minbucket = 30, minsplit = 60, alpha2 = .05, alpha4 =.05)

#Changing splits

plot(cola_chaid2,
     main = "CHAID Model with controls",
     gp = gpar(fontsize = 8),
     type = "simple")



ctrl <- chaid_control(minbucket = 30, minsplit = 60, alpha2 = .5, alpha4 =.05)

#changes alpha2

plot(cola_chaid2,
     main = "CHAID Model with controls",
     gp = gpar(fontsize = 8),
     type = "simple")



library(caret)

predicted_cola1 <- predict(cola_chaid1)

confusionMatrix(predicted_cola1, cola_binary$choice, positive = '1')

set.seed(32)

train.flag <- createDataPartition(y=cola_binary$choice, p=0.5,list=FALSE)
training <- cola_binary[train.flag,]
testing <- cola_binary[-train.flag,]

training_cola <- chaid(choice ~ price3 + feature + display, data = training)

predicted_training <- predict(training_cola,data = training)

confusionMatrix(predicted_test, testing$choice, positive = '1')



train_control <- trainControl(method = "cv",
                              number = 10,
                              verboseIter = TRUE,
                              savePredictions = "final")


features <- setdiff(names(training[,c(-1,-3)]), "choice")
x <- training[, features]
y <- training$choice
                        
#What this says above is include everything in the data set except the first variable and the third variable. The first variable for him is Id, the third variable is price, doesnt include price because price is a continuous variable and model doesn't use continuous.
#This differs from class before by taking out less.

cola_chaid_cv <- train(
  x = x,
  y = y,
  method = "chaid",
  metric = "Accuracy",
  trControl = train_control
  
#Uses accuracy here, Kappa last time.

print(cola_chaid_cv)

cola_chaid_cv$finalModel

plot(cola_chaid_cv)

confusionMatrix(predict(cola_chaid_cv), y, positive = '1')

cola_chaid_cv$bestTune

#Above line comes after rest (cv)..you ask what parameters to use to get best accuracy.

table(cola_binary$choice)

install.packages("ROSE")
library(ROSE)

3644*2


##########################IDEALLY WE WOULD LIKE BALANCE IN THIS DATA, I DOES THIS EQUATION BECAUSE IF WE WERE TO HAVE THE SAME NUMBER OF 1'S THAT WE HAVE 0'S, WE WOULD HAVE A TOTAL DATA SET OF 7288 (THE ANSWER TO 3644*2)
#I HAVE 3644 0'S, WE WANT SAME NUMBER OF 1'S. ROSE DOES THAT.

over_sample <- ovun.sample(choice~.,data=cola_binary,
                           method="over",N=7288)$data
#Oversample
#CONSTRUCTING THIS NEW DATASET THE . AFTER CHOICE MEANS "THROW EVERYTHING IN THERE" INCLUDING ID WHICH DOES NEED, BUT DOESNT MATTER.


#Basically we want a data set with as many 1's of everything, feature, display, etc.

table(over_sample$choice)

#I have 1822 1's


1822*2
#(3644...if we wanted the exact same number of 0's as ones, were going to want a total sample of 3644.)

#Under Sample

under_sample <- ovun.sample(choice ~ ., data = cola_binary,
                            method = "under", N = 3644, seed = 32)$data

####################################################This would be the data set we now use for testing and training, its a new data set.3644 variables, obs of 6 variables.

table(under_sample$choice)

#####OUR DATA IS PERFECTLY BALANCED NOW


#####           SMOTE         ########### !!!!!



synthetic_sample <- ROSE(choice ~ ., data = cola_binary, seed = 32)$data

#This has synthetically created observations. Will have same size as original data set. (1:45:00 in lecture Thursday)

table(synthetic_sample$choice)

round(prop.table(table(synthetic_sample$choice)),3)

cola_chaid_synthetic <- chaid(choice ~ price3 + feature + display, data = synthetic_sample)

#Model I started with but changed data sets using synthetic.

plot(cola_chaid_synthetic,
     main = "CHAID Model",
     gp = gpar(fontsize = 8),
     type = "simple")

#NOW I HAVE A MODEL THAT MAKES DECENT PREDICTIONS WITH 1'S

predicted_synthetic<-predict(cola_chaid_synthetic)

confusionMatrix(predicted_synthetic, synthetic_sample$choice)

















