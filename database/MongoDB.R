# mcon <- mongo(collection="Hotel_Reviews_Collection", db="Rstudio", url="mongodb://localhost:27017")

# dfAll <-mcon$find('{}', limit=5000)
# dfAll <-mcon$find('{}')

# Find_specific_date <-mcone$find('{"Review_Date":{"$eq":"4/5/2017"}}')
'%!in%' <- function(x,y)!('%in%'(x,y))

# get the different countries in data from the hotel adress string
# *1* Special case for United Kingdom, no other countries of two words existent in data set
# cities <- list()
# countries <- list()

comboDf <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(comboDf) <- c("country", "city")

# countries["All"] <- "All"
# cities["All"] <- "All"

comboDf <- rbind(comboDf, data.frame(country="All", city="All"))

for(string in unique(dfAll$Hotel_Address)) {
  country <- word(string,-1)
  city <- word(string, -2)
  #*1*
  if(country == "Kingdom") {
    country <- paste(word(string,-2), word(string,-1), sep = " ")
    city <- word(string, -5)
    if(city %!in% comboDf$city) {
      comboDf <- rbind(comboDf, data.frame(country=country, city=city))
    }
  }

  if(city %!in% comboDf$city) {
    comboDf <- rbind(comboDf, data.frame(country=country, city=city))
  }
}


# for(string in unique(dfAll$Hotel_Address)) {
#   country <- word(string,-1)
#   city <- word(string, -2)
#   #*1* 
#   if(country == "Kingdom") {
#     country <- paste(word(string,-2), word(string,-1), sep = " ")
#     city <- word(string, -5)
#     if(city %!in% cities) {
#       cities[city] <- paste(city, country, sep = "")
#       countries[country] <- country
#     }
#   }
#   
#   if(city %!in% cities) {
#     cities[city] <- paste(city, country, sep = " ")
#     countries[country] <- country
#   }
#   
# }

