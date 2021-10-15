library(gapminder)
library(tidyverse)

# exercise 1

# we want to add defensive programming

calcGDP <- function(data, year=NULL, country=NULL) { #year and country function arguments
  if(!is.null(year)) { #subsets data by year if not NULL
    data <- data[data$year %in% year,]
  }
  if (!is.null(country)) {
    data <- data[data$country %in% country,] #subsets data by country if not NULL
  }
  
  #calculation of gdp 
  GDP <- data$pop * data$gdpPercap
  
  new <- cbind(data, GDP=GDP)
  return(new)
}

head(calcGDP(gapminder, year=2007))

#the function excludes the year 2017 since it is not present in the dataset


calcGDP(gapminder, year=c(1967, 1977, 1987, 1997, 2007, 2017), country="Denmark")




# exercise 2

#thresholdValue <- 50
candidateCountries <- grep("^B", unique(gapminder$country), value = TRUE) #we use regex to find the countries with b

for (iCountry in candidateCountries) {
  tmp <- mean(gapminder[gapminder$country == iCountry, "lifeExp"])
  
  if (tmp < thresholdValue) {
    cat("Average Life Expectancy in", iCountry, "is less than", thresholdValue, "plotting life expectancy graph... \n")
    
    with(subset(gapminder, country == iCountry),
         plot(year, lifeExp,
              type = "o",
              main = paste("Life Expectancy in", iCountry, "over time"),
              ylab = "Life Expectancy",
              xlab = "Year"
         ) # end plot
    ) # end with
  } # end if
  rm(tmp)
}















