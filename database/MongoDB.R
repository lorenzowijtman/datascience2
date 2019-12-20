library(mongolite)

mcon <- mongo(collection="Hotel_Reviews_Collection", db="Rstudio", url="mongodb://localhost:27017")

dfAll <-mcon$find('{}', limit=5000)

# Find_specific_date <-mcone$find('{"Review_Date":{"$eq":"4/5/2017"}}')