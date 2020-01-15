library(ff)
library(ffbase)
library(dplyr)
library(mongolite)

mcon <- mongo(collection="Hotel_Reviews_Collection", db="Rstudio", url="mongodb://localhost:27017")

dir_common <- paste0(getwd(),"/common")
options(fftempdir = dir_common)

# stringsAsFactors doesn't seem to work
hotel_data <- as.data.frame(mcon$find('{}'), stringsAsFactors=T)

format(object.size(hotel_data),"Mb")

# all character columns to factor, ff doesn't like char values
hotel_data <- mutate_if(hotel_data, is.character, as.factor)
hotel_data <- subset(hotel_data, Positive_Review != "No Positive")
hotel_data <- subset(hotel_data, Negative_Review != "No Negative")
hotel_data <- subset(hotel_data, Negative_Review != "Nothing")


hotel_pos <- hotel_data[,c("Positive_Review", "Reviewer_Score")]
colnames(hotel_pos) <- c("Review", "Reviewer_Score")
hotel_pos$sentiment <- 1

hotel_neg <- hotel_data[,c("Negative_Review", "Reviewer_Score")]
colnames(hotel_neg) <- c("Review", "Reviewer_Score")
hotel_neg$sentiment <- 0

hotels_pos.ff <- as.ffdf(hotel_pos)
hotels_neg.ff <- as.ffdf(hotel_neg)

format(object.size(hotels_pos.ff),"Mb")
format(object.size(hotels_neg.ff),"Mb")
format(object.size(hotel_pos),"Mb")
format(object.size(hotel_neg),"Mb")

write.csv.ffdf(hotels_pos.ff, "Review_pos.csv")
write.csv.ffdf(hotels_neg.ff, "Review_neg.csv")
