Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_191")
library(sparklyr)
library(dplyr)
library(ggplot2)
library(DBI)
library(mongolite)

# connect to spark
sc <- spark_connect(master = "local", version="2.0.0")

# connect to mongo
mcon <- mongo(collection="Hotel_Reviews_Collection", db="Rstudio", url="mongodb://localhost:27017")


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

# second test that will chnage the way positive reviews are retrieved a bit
sparkNbTesting2 <- function(posScore, negScore, limit) {
  # changed the query below from $gt to $lt for positive only
  posQ <- paste0('{"sentiment": 1, "Reviewer_Score": {"$lt" : ',posScore,'}}')
  negQ <- paste0('{"sentiment": 0, "Reviewer_Score": {"$lt" : ',negScore,'}}')
  pos <- mcon$find(posQ, fields = '{"Positive_Review": true, "Reviewer_Score": true, "sentiment": true}', limit = limit) 
  pos$`_id` <- NULL
  colnames(pos) <- c('review', 'score', 'sentiment')
  print('problem before this')
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
