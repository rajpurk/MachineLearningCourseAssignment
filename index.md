# Predicting activity performed using fitness trackers 
Raj Purkayastha  
26 September 2017  


## Executive Summary
After an initial clean up of data we fit a random forest model to a portion of a training set. This is validated with data from the training set itself, and the accuracy and out of sample errors are indicated. Finally the sample is run on the test data to predict the outcome

## Initial Data cleanup

After reading the data of csv files, a preliminary examination shows that a large number of the columns have NA's in them and/or have empty data sets. We proceed to remove these columns. 


```r
{
temptraining<-read.csv("pml-training.csv")
temptesting<-read.csv("pml-testing.csv")

training2<-temptraining[,which(as.numeric(colSums(!is.na(temptraining)))> 1000)]
training<-training2[,which(as.numeric(colSums(training2==""))< 10000)]

testing<-temptesting[,which(as.numeric(colSums(!is.na(temptesting)))> 5)]

}
```

After this is done, we proceed to divide the set into a training and validation set using the caret package.

```r
{
library(caret)
set.seed(1331)

inTrain<-createDataPartition(y=training$classe,p=0.7,list=FALSE)
mytraining<-training[inTrain,]
validation<-training[-inTrain,]

}
```

## Fitting the model
We fit two models to the cleaned data in order to assess which model performs better. A cross validation technique is used. The fit model is then applied to the validation data set.


```r
{
#fit models in
modFit_rf<-train(classe~.,method="rf",data=mytraining, trControl=trainControl(method="cv"),number=10,na.action = na.omit)
modFit_lda<-train(classe~.,method="lda",data=mytraining, trControl=trainControl(method="cv"),number=10)

#predict using the models
pred_rf<-predict(modFit_rf,validation)
pred_lda<-predict(modFit_lda,validation)

}
```



We now look at the confusion matrix of the data in order to look at the accuracy of the models.

```r
{
# now look at confusion matrix for accuracy
xtab_rf<-table(pred_rf,validation$classe)
cm_rf<-confusionMatrix(xtab_rf)
cm_rf

}
```

```
## Confusion Matrix and Statistics
## 
##        
## pred_rf    A    B    C    D    E
##       A 1674    0    0    0    0
##       B    0 1139    0    0    0
##       C    0    0 1026    0    0
##       D    0    0    0  964    0
##       E    0    0    0    0 1082
## 
## Overall Statistics
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9994, 1)
##     No Information Rate : 0.2845     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##  Mcnemar's Test P-Value : NA         
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
## Detection Rate         0.2845   0.1935   0.1743   0.1638   0.1839
## Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000
```

We repeat the procedure for the lda model.


```r
{
# now look at confusion matrix for accuracy

xtab_lda<-table(pred_lda,validation$classe)
cm_lda<-confusionMatrix(xtab_lda)
cm_lda
}
```

```
## Confusion Matrix and Statistics
## 
##         
## pred_lda    A    B    C    D    E
##        A 1674    0    0    0    0
##        B    0 1138    0    0    0
##        C    0    1 1026    0    0
##        D    0    0    0  964    0
##        E    0    0    0    0 1082
## 
## Overall Statistics
##                                      
##                Accuracy : 0.9998     
##                  95% CI : (0.9991, 1)
##     No Information Rate : 0.2845     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 0.9998     
##  Mcnemar's Test P-Value : NA         
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   0.9991   1.0000   1.0000   1.0000
## Specificity            1.0000   1.0000   0.9998   1.0000   1.0000
## Pos Pred Value         1.0000   1.0000   0.9990   1.0000   1.0000
## Neg Pred Value         1.0000   0.9998   1.0000   1.0000   1.0000
## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
## Detection Rate         0.2845   0.1934   0.1743   0.1638   0.1839
## Detection Prevalence   0.2845   0.1934   0.1745   0.1638   0.1839
## Balanced Accuracy      1.0000   0.9996   0.9999   1.0000   1.0000
```

The results using the cross validation show that the accuracy for the random forests method is quite high, being very close to 1. The accuracy for the lda method is slightly lower at 0.99, but not by much. hence given the high values of the accuracy, we can safely use either method to generate a set.

We now apply the prediction to the final dataset


```r
{
#final prediction
pred_final_rf<-predict(modFit_rf,testing)
pred_final_lda<-predict(modFit_lda,testing)
 
 
}
```


The final prediction values are C, A,B,A,A,D,C,B,A,A,B,C,B,A,E,D,A,B,B,B



