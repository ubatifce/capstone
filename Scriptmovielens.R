################################
# Create edx set, validation set
#  No need to run this if ratings,edx,validation etc. have already been loaded 
#################################

# Note: this process could take a couple of minutes

# INIT LOADING DATASETS. SKIP IF DATA IS ALREADY LOADED

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")

library(dslabs)
library(tidyverse)



# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")


# if using R 3.5 or earlier, use `set.seed(1)` instead


test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]


# Make sure userId and movieId in validation set are also in edx set


validation <- temp %>% 
     semi_join(edx, by = "movieId") %>%
     semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set


removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# END LOADING DATASETS

# FIELDS

 colnames(edx)
#[1] "X"         "userId"    "movieId"   "rating"    "timestamp" "title"    
#[7] "genres"   

# ROWS 
 nrow(edx)
#[1] 9000055

 nrow(validation)
#[1] 999999

# FINAL ALGORITHM

set.seed(1, sample.kind="Rounding") 

# FINAL ALGORITHM

#training datasets
#test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.1, list = FALSE)
#train_set <- edx[-test_index,]
#test_set <- edx[test_index,]
#test_set <- test_set %>% 
#     semi_join(train_set, by = "movieId") %>%
#     semi_join(train_set, by = "userId")

# final datasets
train_set <- edx
test_set <- validation



#A generic (taken fron theory) RMSE function

RMSE <- function(true_ratings, predicted_ratings){
     sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Methods used:

               
          
#5 Using sum(rating - mu)/(n()+lambda), n_i = n() with min(lambda) 


mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
     group_by(movieId) %>% 
     summarize(b_i = mean(rating - mu))

user_avgs <- test_set %>% 
     left_join(movie_avgs, by='movieId') %>%
     group_by(userId) %>%
     summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
     left_join(movie_avgs, by='movieId') %>%
     left_join(user_avgs, by='userId') %>%
     mutate(pred = mu + b_i + b_u) %>%
     .$pred


test_set %>% 
     left_join(movie_avgs, by='movieId') %>%
     mutate(residual = rating - (mu + b_i)) %>%
     arrange(desc(abs(residual))) %>% 
     select(title,  residual) %>% slice(1:10) 

movie_titles <- movielens %>% 
     select(movieId, title) %>%
     distinct()

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
     arrange(b_i) %>% 
     select(title, b_i) %>% 
     slice(1:10) 


train_set %>% dplyr::count(movieId) %>% 
     left_join(movie_avgs) %>%
     left_join(movie_titles, by="movieId") %>%
     arrange(b_i) %>% 
     select(title, b_i, n) %>% 
     slice(1:10) 


mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
     group_by(movieId) %>% 
     summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

data_frame(Original = movie_avgs$b_i, 
           Regularized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
     ggplot(aes(Original, Regularized, size=sqrt(n))) + 
     geom_point(shape=3, alpha=0.25, color="red")


train_set %>%
     dplyr::count(movieId) %>% 
     left_join(movie_reg_avgs) %>%
     left_join(movie_titles, by="movieId") %>%
     arrange(b_i) %>% 
     select(title, b_i, n) %>% 
     slice(1:10) 


#5 Using sum(rating - mu)/(n()+lambda), n_i = n() with min(lambda)  


lambdas <- seq(2.3, 2.5, 0.125)

rmses <- sapply(lambdas, function(l){
     mu <- mean(train_set$rating)
     b_i <- train_set %>%
          group_by(movieId) %>%
          summarize(b_i = sum(rating - mu)/(n()+l))
     b_u <- train_set %>% 
          left_join(b_i, by="movieId") %>%
          group_by(userId) %>%
          summarize(b_u = sum(rating - b_i - mu)/(n()+l))
     predicted_ratings <- 
          test_set %>% 
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          mutate(pred = mu + b_i + b_u) %>%
          .$pred
     return(RMSE(predicted_ratings, test_set$rating))
     
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda


rmse_results <- data_frame(method="Using sum(rating - mu)/(n()+lambda), n_i = n() with min(lambda)",  
                                     RMSE = min(rmses)) #)
rmse_results %>% knitr::kable()
min(rmse_results[2])


# END FINAL ALGORITHM
 