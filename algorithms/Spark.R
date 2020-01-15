source("common/libs.R")

# connect to spark
sc <- spark_connect(master = "local", version="2.0.0")

# connect to mongo
mcon <- mongo(collection="Hotel_Reviews_Collection", db="Rstudio", url="mongodb://localhost:27017")

# accuracy increased which is not what I expected at all can i increase it further?
# let's make the whole thing into a function with parameters for tuning so i can stop copy - pasting

# first test aimed at increasing the negative reviews score, for increasing positive i need a new query
sparkNbTesting <- function(posScore, negScore, limit) {
  posQ <- paste0('{"sentiment": 1, "Reviewer_Score": {"$gt" : ',posScore,'}}')
  negQ <- paste0('{"sentiment": 0, "Reviewer_Score": {"$lt" : ',negScore,'}}')
  pos <- mcon$find(posQ, fields = '{"Positive_Review": true, "Reviewer_Score": true, "sentiment": true}', limit = limit) 
  pos$`_id` <- NULL
  colnames(pos) <- c('review', 'score', 'sentiment')
  neg <- mcon$find(negQ, fields = '{"Negative_Review": true, "Reviewer_Score": true, "sentiment": true}', limit = limit)
  neg$`_id` <- NULL
  colnames(neg) <- c('review', 'score', 'sentiment')
  print(nrow(pos))
  print(nrow(neg))
  df <- rbind(pos, neg)
  reviews_tbl <- copy_to(sc, df, name = "reviews_tbl", overwrite = T)
  rm(pos)
  rm(neg)
  rm(df)
  partitions <- reviews_tbl %>%
    sdf_random_split(training = 0.7, test = 0.3, seed = 1011)
  pipeline <- ml_pipeline(sc) %>%
    ft_tokenizer(input_col = "review", output_col = "raw_tokens") %>%
    ft_stop_words_remover(input_col = "raw_tokens", output_col = "tokens") %>%
    ft_count_vectorizer("tokens", "vectokens") %>%
    ft_r_formula(sentiment ~ vectokens) %>%
    ml_naive_bayes()
  fitted_pipeline <- ml_fit(
    pipeline,
    partitions$training
  )
  predictions <- ml_transform(
    fitted_pipeline,
    partitions$test
  )
  table <- predictions %>%
    group_by(sentiment, prediction) %>%
    tally()
  accuracy <- ml_multiclass_classification_evaluator(predictions) *100
  print(paste0("accuracy: ", round(accuracy, 2), "%"))
  print(table)
}


# now let's test some different parameters
# @Param 
# *pos = positive review must have higher user score than param
# *neg = negative review must have lower user score than param
# *limit = amount of reviews to get for both positive and negative reviews
# sparkNbTesting(pos, neg, limit)
# original
sparkNbTesting(9, 4, 10000) # "accuracy: 93.66%"

# increase the number for negative reviews because otherwise there are no 150000 negative reviews
sparkNbTesting(9, 5, 15000) # "accuracy: 93.9%" unexpected

# test for user score -- accuracy seems to increase for reviews with a high user score on negative reviews? that's weird
sparkNbTesting(9, 5, 10000) # "accuracy: 94.61%"
sparkNbTesting(9, 6, 10000) # "accuracy: 95.05%" very weird and unexpected
sparkNbTesting(9, 7, 10000) # "accuracy: 94.24%" thank god it goes down again
sparkNbTesting(9, 8, 10000) # "accuracy: 94.68%" I don't get it
sparkNbTesting(9, 9, 10000) # "accuracy: 94.68%" - again, it is possible the same dataset it retrieved for these parameters

# moving on to testing with higher amount of reviews - 30k
sparkNbTesting(9, 6, 15000) # "accuracy: 94.56%"
sparkNbTesting(9, 7, 15000) # "accuracy: 94.87%"
sparkNbTesting(9, 8, 15000) # "accuracy: 94.88%"
sparkNbTesting(9, 9, 15000) # "accuracy: 94.88%"

# lets go to 40k
sparkNbTesting(9, 5, 20000) # "accuracy: 94.25%"
sparkNbTesting(9, 6, 20000) # "accuracy: 94.87%"
sparkNbTesting(9, 7, 20000) # "accuracy: 94.91%"
sparkNbTesting(9, 8, 20000) # "accuracy: 95.09%"
sparkNbTesting(9, 9, 20000) # "accuracy: 95.09%"

# let's see what happens when i decrease the psotive number and leave negative at 8 as it had the highest accuracy across prev tests
sparkNbTesting(8, 8, 10000) # "accuracy: 94.51%"
sparkNbTesting(7, 8, 10000) # "accuracy: 94.43%"
sparkNbTesting(6, 8, 10000) # "accuracy: 94.43%"
sparkNbTesting(5, 8, 10000) # same, i will have to change the function as right now it's getting the same data

# try again but with new query for positive reviews, change is documented in NBTests.R
sparkNbTesting2(8, 8, 10000) # "accuracy: 92.41%"  ==============!!!!!Lowest achieved!!!!!============
sparkNbTesting2(7, 8, 10000) # no positive reviews below 7 due to sentiment in query, sentiment is based on the user score >= 8 i think..
                             # This is also why I get the same outcome for having a negative under 8 and 9, didn't think about that. 

# lets only test 8 / 9 /10 for pos reviews but increase number of reviews
# putting it on 9.9 will only retireve pos reviews with user score of 10
sparkNbTesting(9.9, 8, 10000) # "accuracy: 95.05%"
sparkNbTesting(9, 8, 10000) # "accuracy: 94.68%"
sparkNbTesting(8, 8, 10000) # "accuracy: 94.51%"

# lets go to 30k
sparkNbTesting(9.9, 8, 15000) # "accuracy: 95.38%"  ========!!!!!Highest yet achieved!!!!!=========
sparkNbTesting(9, 8, 15000) # "accuracy: 94.88%"
sparkNbTesting(8, 8, 15000) # "accuracy: 94.73%" it seems to decrease the accuracy when i lower the positive score

# lets go to 40k
sparkNbTesting(9.9, 8, 20000) # "accuracy: 95.02%"
sparkNbTesting(9, 8, 20000) # "accuracy: 95.09%"
sparkNbTesting(8, 8, 20000) # "accuracy: 94.76%"

# range: 92.41 - 95.38

spark_disconnect(sc)
