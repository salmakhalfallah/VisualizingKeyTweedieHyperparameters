# extending NA_MLE_parameters.R to entire african continent

library(tidyverse)
library(rlang)
library(sf)
library(rnaturalearth)
library(MASS)
library(statmod)
library(tweedie)
library(HDtweedie)
library(scoringutils)

# plotting the entire continent
africa <- ne_countries(continent = "Africa", returnclass = "sf")

ggplot() +
  geom_sf(data = africa)

# loading in data
load("VIEWS-alldownloaded.RData")

# merging data by country
df <- merge(cm, countries, 
            by.x = "country_id", by.y="id")

# merging on month_id
df <- merge(df, month_ids[,2:4],
            by.x = "month_id", by.y="month_id")

# typecasting from character to integer
as.numeric(colnames(df) == "isonum")

# make factors for countries, years, and months
df$country_factor <- as.factor(df$isoab)
df$year_factor <- as.factor(df$Year)
df$month_factor <- factor(df$month_id)

# filtering for 2010 data and later (since that is our range)
df <- df |>
  filter(month_id >= 361)

# filtering for africa data
africa.data <- df[df$in_africa==1,]

af_countries <- unique(africa.data$name)
af_iso <- unique(africa.data$isonum)

# filtering data into train, eval, test splits
africa.train <- africa.data |>
  filter(month_id < 517)

africa.test <- africa.data |>
  filter(month_id > 528)

## fitting models here

# note to self: perform clean up later for memory management
# creating a function that fits a model based on a specific country's data using 
# tweedie, poisson, and negative binomial distributions, as well as MLE estimations

# the parameters of each distribution are saved in a vector and later outputted

# note: add log(xi), log(phi) columns, remove poisson and neg. binomials
country_fit <- function(country_name, train, xi = seq(1.1, 1.9, by=0.05)) 
{
  country_data <- train |>
    filter(name == country_name)
  
  out <- list(name = country_data$name[1], gleditsch_ward = country_data$gleditsch_ward[1], isonum = unique(country_data$isonum), phi = NULL, xi = NULL)
  
  sb_total <- sum(country_data$ged_sb)
  
  if(sb_total <= 2)
  {
    out$phi <- 0
    out$xi <- 0
    return(out)
  }
  
  
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
  
  # poisson 
  
  # psn <- glm(ged_sb ~ year_factor, 
  #            data = country_data,
  #            family=poisson(),
  #            control=list(maxit=50))
  # 
  # out$rate <- mean(train$ged_sb)
  # 
  # # neg. binomial 
  # 
  # library(MASS)
  # nb <- glm.nb(ged_sb ~ year_factor,
  #              data = country_data)
  # 
  # fit.nb <- fitdistr(train$ged_sb, "negative binomial")
  # nb_params <- fit.nb$estimate
  # out$size <- nb_params[1]
  # out$mu <- nb_params[2]
  
  return(out)
}

# an empty data frame for the data from each country is created
af_db <- data.frame(name = character(),
                 gleditsch_ward = integer(),
                 isonum = integer(),
                 phi = numeric(),
                 xi = numeric())

# looping through every country in NA, we are inputting the country name and 
# training data and outputting that country's fitted parameter list 
# finally, each list is inputted into the data frame

for(country in af_countries)
{
  country_list <- (country_fit(country_name = country, train = africa.train))
  db <- db |>
    add_row(!!!country_list)
}

view(db)

# adjusting datasets for easy merging

colnames(africa)[colnames(africa) == "iso_n3"] <- "isonum"
africa$isonum <- as.numeric(africa$isonum)

# DEBUG: MERGE BASED ON ISONUM INSTEAD OF NAME 
# combining SF data with db that we just made
AF <- merge(africa, af_db, by.x = "isonum", by.y = "isonum")

#visualizing here...
ggplot(data = AF) +
  geom_sf(mapping = aes(fill = pop_est))

# WHERE DID THE DEMOCRATIC REPUBLIC OF THE CONGO GO???????

ggplot(data = AF) +
  geom_sf(mapping = aes(fill = log(phi)))

# mapping xi parameters
ggplot(data = AF) +
  geom_sf(mapping = aes(fill = xi))