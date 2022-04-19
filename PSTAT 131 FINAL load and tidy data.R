library(ggplot2)
library(tidyverse)
library(tidymodels)
library(corrplot)
library(ggthemes)
tidymodels_prefer()
library(ISLR)
library(yardstick)
library(tidyr)
install.packages("devtools")
library(devtools)
library(stringr)

movies <- read.csv("C:\\movies.csv")
view(movies)

# PART 1 - TIDYING THE DATA 
# seperate released column into date released and country released
movies <- separate(movies, released, c("date_released", "country_released"),
                    sep = "\\(")
movies$country_released <- gsub("\\)", "", as.character(movies$country_released))
movies$country_released
view(movies)

# separate data released into month + day released and year released
movies <- separate(movies, date_released, c("month_released","year_released"),
                    sep = ",")
view(movies)

# remove day from month_released (bc day irrelevant)
movies$month_released <- substr(movies$month_released,1, nchar(movies$month_released)-2)
view(movies1)

# remove 1 observation with viewing rating different from rest
movies1 <- movies[-c(121), ]

# remove trailing white space in month_released
movies1$month_released <- str_trim(movies1$month_released)

# replace missing values with NA
movies1[movies1 == ""] <- NA

# removing observations with NA (about 2000 observations)
## best option bc missing values were typically 
## movie viewability rating, budget, and gross income
## all predictors that i think will have largest effect on 
## predicting IMDb rating 
movies1 <- na.omit(movies1)

# factor month_released with levels refering to month number 
movies1$month_released <- factor(movies1$month_released, levels = c(
  "January", "Febuary", "March", "April", "May", "June", "July",
  "August", "September", "October", "November", "December")
)

# can now view month released as categorical variable
# factored so that can view seasonality better 
ggplot(movies1, aes(x = month_released)) +
  geom_bar()

### combine unrated and non rated rating into same category
movies1[movies1 == "Unrated"] <- "Not Rated"

# factor rating into levels to organize from child viewing
## to adult
movies1$rating <- factor(movies1$rating, levels = c(
  "G", "PG", "PG-13", "TV-MA", "R", "X", "NC-17", "Not Rated"))
### want to factorize so can order in such a way we divide up into subgroups of age
### that is easy to visualize 

# rename country -> country_created
# rename year -> year_created 
movies1 <- movies1 %>% rename(country_created = country, 
                   year_created = year)
view(movies1)

