# Exercise in P1: Introduction to R

################################################################################

# Topics: dataframes, data types, functions, conditional statements, plots

################################################################################

# This script was written in R v 4.3.0
# to verify your R version, uncomment and run the following line:
# R.version

################################################################################

# Solution

# define columns of the dataframe 
city <- c("Barcelona","L'Hospitalet de Llobregat","Terrassa","Badalona")
pop <- c(1664182,269382,223627,223166)
lon <- c(2.176944, 2.1, 2.013, 2.246111)
lat <- c(41.382778, 41.359722, 41.57, 41.448889)

# define dataframe
city_pop_df <- data.frame(city,pop,lon,lat)

# initialise "category" column 
city_pop_df$category <- NA

# define function to associate a category to the population
which_category <- function(x){
  try(if(!is.numeric(x)) stop("The input must be a numerical value"))
  if (x >= 1000000){category <- "large"}
  else if (x < 1000000 & x>= 250000){category <- "medium"}
  else if (x < 250000){category <- "small"}
  return(category)
}

# assign a category to each city using the above function
for (i in 1:nrow(city_pop_df)) {
  city_pop_df$category[i] <- which_category(city_pop_df$pop[i])
}

# transform the column into factors
city_pop_df$category <- factor(city_pop_df$category)

# check the data types in the df
str(city_pop_df)

# produce a basic plot
plot(city_pop_df$lon, 
     city_pop_df$lat, 
     cex = sqrt(city_pop_df$pop/100000),
     pch = 16,
     col = city_pop_df$category,
     xlab = "longitude",
     ylab = "latitude",
     xlim = c(1.8,2.3),
     ylim = c(41.3,41.7),
     asp = 1,
     main = "Largest Catalan cities"
)

# add text labels to each point
text(cbind(city_pop_df$lon,
           city_pop_df$lat), 
     city_pop_df$city, 
     pos = c(4,2,1,3))

# add a legend to explain the category categories
legend(
  x = 2.2,
  y = 41.7,
  legend = levels(city_pop_df$category),
  fill = c("black","red","green")
)

