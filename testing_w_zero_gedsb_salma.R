# testing w countries with sum(ged_sb) == 0

library(tidyverse)
library(rlang)
library(sf)
library(rnaturalearth)
library(MASS)
library(statmod)
library(tweedie)
library(HDtweedie)
library(scoringutils)

# loading in data
load("VIEWS-alldownloaded.RData")

# merging data by country
df <- merge(cm, countries, 
            by.x = "country_id", by.y="id")

# merging on month_id
df <- merge(df, month_ids[,2:4],
            by.x = "month_id", by.y="month_id")

# make factors for countries, years, and months
df$country_factor <- as.factor(df$isoab)
df$year_factor <- as.factor(df$Year)
df$month_factor <- factor(df$month_id)

# filtering for 2010 data and later (since that is our range)
df <- df |>
  filter(month_id >= 361)

# list of countries
countries <- unique(df$name)

# filtering data into train, test splits
train <- df |>
  filter(month_id < 517)

test <- df |>
  filter(month_id > 528)

# extracting countries with sum(ged_sb) == 0 (no training data to work with!)
no_sb <- character()

for(country in countries)
{
  country_data <- df |>
    filter(name == country)
  
  total_sb <- sum(country_data$ged_sb)
  
  if(total_sb == 0)
  {
    no_sb <- c(no_sb, country)
  }
}

# copy paste from other scripts..
country_fit <- function(country_name, train, xi = seq(1.2, 1.8, by=0.05)) 
{
  country_data <- train |>
    filter(name == country_name)
  
  browser()
  
  out <- list(name = country_data$name[1], gleditsch_ward = country_data$gleditsch_ward[1], phi = NULL, xi = NULL)
  
  # fitting models here
  
  # tweedie 
  
  param.tw <- tweedie.profile(ged_sb ~ year_factor, 
                              xi.vec=xi,
                              data = country_data,
                              do.plot=TRUE,
                              control=list(maxit=50),
                              method="series",
                              verbose=2)
  
  tw <- glm(ged_sb ~ year_factor, 
            data = country_data,
            family=statmod::tweedie(var.power=param.tw$xi.max, link.power=0),
            control=list(maxit=50))
  
  out$phi <- param.tw$phi.max
  out$xi <- param.tw$xi.max
  
  return(out)
}

# an empty data frame for the data from each country is created
db <- data.frame(name = character(),
                 gleditsch_ward = integer(),
                 phi = numeric(),
                 xi = numeric())

# looping through every country in no_sb, we are inputting the country name and 
# training data and outputting that country's fitted parameter list 
# finally, each list is inputted into the data frame

for(country in no_sb)
{
  country_list <- (country_fit(country_name = country, train = train))
  browser()
  db <- db |>
    add_row(!!!country_list)
}

view(db)

# as expected: the function didn't even work for the first value. this confirms 
# my suspicions that my function in the other script didn't work because of the 
# lack of non-zero values in a specific country set.