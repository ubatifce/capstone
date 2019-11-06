################################
# INIT LOADING DATASETS. SKIP IF DATA IS ALREADY LOADED
#################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(downloader)) install.packages("downloader", repos = "http://cran.us.r-project.org")


#Set default directory to C:/ or change the path in setwd(...)
#Place dataset at the default directory

setwd("C:/")

# If you downloaded bank-additional-full.r from edx you can rename it to bank-additional-full.csv
#****************************************************************
#if you do not rename it , anywhay it worked for me to upload as .r file
# bank-additional-full.csv was renamed as  bank-additional-full.r just to download using edx 
bank<-read.csv2("bank-additional-full.r") 
#****************************************************************
# if you renamed it back as bank-additional.csv you must use
#bank<-read.csv2("bank-additional-full.csv") 
#****************************************************************


 ncol(bank)
 nrow(bank)

# clean the data  


#From "Has the client subscribed a term deposit yes/no?"
#To      "Has the client subscribed a term deposit 1/0"

  bank$Y<-  1
  bank$Y[bank$y=="no"]<-  0



#Has the client subscribed a term deposit 
y <- bank$Y

#Set the seed to 42, then using the caret package to create a 20% data partition based on the "y" data.

#Assign the 20% partition to test_set and the remaining 80% partition to train_set.

set.seed(42, sample.kind="Rounding") 
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
train_set <- bank %>% slice(-test_index)
test_set <- bank %>% slice(test_index)
print("Train SET")
nrow(train_set)
ncol(train_set)
print("test SET")
nrow(test_set)
ncol(test_set)


# Proportion of clients who subscribed a term deposit 

testy<-subset(test_set,test_set$y=="yes")

nrow(testy)/nrow(test_set)


#Baseline: Using a  sample 0,1 
y_hat <- sample(c(0,1),nrow(test_set),replace=TRUE)
#Baseline Accuracy
mean(y_hat ==test_set$Y)

results <- data_frame(method="Baseline: A sample 0,1 ", Accuracy =  mean(y_hat ==test_set$Y)
)

#Proportion of clients who subscribed a term deposit contacted by type of phone

train_set  %>% filter(contact=="cellular") %>% summarize(mean(Y==1))
train_set  %>% filter(contact=="telephone") %>% summarize(mean(Y==1))

#Predict clients who subscribed a term deposit  using contact on the test set:
# Accuracy of this contact-based prediction method on the test set
y_hat <- if_else(test_set$contact=="telephone",1,0)
# Accuracy
mean(y_hat ==test_set$Y)

results <- bind_rows(results,
                          data_frame(method="clients who subscribed a term deposit  using contact ",  
                           Accuracy = mean(y_hat ==test_set$Y)
))




   




#Predicting clients who subscribed a term deposit  by contact & education
#Accuracy of this contact & education-based prediction method on the test set




# Accuracy

train_set  %>% group_by( contact,education) %>% summarize(mean(Y==1))
y_hat <- if_else(test_set$contact=="telephone" & test_set$education %in% c("professional.course","university.degree") ,1,0)
mean(y_hat ==test_set$Y)

# see a graph 
graph<-bank %>% ggplot(aes(education,contact)) +
   geom_bar(width = 0.4, stat = "identity", color = "green") +
   coord_flip()  
graph 


results <- bind_rows(results,
          data_frame(method="clients who subscribed a term deposit  using contact,education ",  
           Accuracy = mean(y_hat ==test_set$Y)
))

#Confusion matrices for the contact  model, education model, and combined contact & #education model.

# see a graph 
graph<-bank %>% ggplot(aes(age,contact)) +
   geom_bar(width = 0.4, stat = "identity", color = "red") +
   coord_flip()  
graph 


set.seed(42, sample.kind="Rounding") 


#Models:

#Training  a model using linear discriminant analysis (LDA) with the caret lda method using job as the only predictor.
#LDA y ~ contact+education+job+age
train_lda <- train(y ~ contact+education+job+age, method = "lda", data = train_set)
y_hat <- predict(train_lda, test_set)
# Accuracy

cm<-confusionMatrix(data = y_hat, reference = test_set$y)$overall["Accuracy"]
cm

results <- bind_rows(results,
       data_frame(method="clients who subscribed a term deposit  using LDA ",  
        Accuracy =cm)
)



#Training  using quadratic discriminant analysis (QDA) with the caret qda method using job as the only predictor.
#QDA y ~ contact+education+job+age
train_qda <- train(y ~  contact+education+job+age, method = "qda", data = train_set)
y_hat <- predict(train_qda, test_set)
# Accuracy

cm<-confusionMatrix(data = y_hat, reference = test_set$y)$overall["Accuracy"]
cm

results <- bind_rows(results,
       data_frame(method="clients who subscribed a term deposit  using QDA ",  
        Accuracy = cm)
)



#GLM y ~ contact+education+job+age

# see a graph 
graph<-bank %>% ggplot(aes(job,contact)) +
   geom_bar(width = 0.4, stat = "identity", color = "blue") +
   coord_flip()  
graph 



train_glm <- train(y ~ contact+education+job+age, method = "glm", data = train_set)
y_hat <- predict(train_glm, test_set)
# Accuracy
mean(y_hat ==test_set$y)
cm<-confusionMatrix(data = y_hat, reference = test_set$y)$overall["Accuracy"]
cm

results <- bind_rows(results,
data_frame(method="clients who subscribed a term deposit  using GLM Y ~ contact+education+job+age",  
        Accuracy = cm)
)





results %>% knitr::kable()
max(results[2])


