# Load R packages for data science
library(tidyverse)
# Data in starwars data-set 
starwars

# Salient features of the data-set
?starwars

# Catch a glimpse starwars data-set
glimpse(starwars)

# Access column "height"
starwars$height

# Access column "mass"
starwars$mass

# Access column "gender"
starwars$gender

# Access column "gender"
starwars$films[1:3]

# Rows of interest
filter_rows <- c("Luke Skywalker","R2-D2","Darth Vader") # Extract row corresponding to Luke Skywalker starwars %>% filter(name%in%filter_rows)
# Rows of interest
filter_rows <- c("Luke Skywalker","R2-D2")
# Extract row corresponding to Luke Skywalker
starwars %>% filter(name%in%filter_rows) %>% select(name,height,mass,homeworld,films)


# Rows of interest
filter_rows <- c("Luke Skywalker","R2-D2")
# Extract rows in 'rows'
starwars %>% filter(name%in%filter_rows) %>% pull(films)

nrow(starwars)
ncol(starwars)
dim(starwars)


# Invoke the library
library(Tmisc)
# Filter data-set I in quartet 
quartet %>% filter(set=="I")

quartet %>% filter(set=="II")

quartet %>% filter(set=="III")

quartet %>% filter(set=="IV")

# Obtain the needed statistics
grouped_quartet %>%
  summarise(
    mean_x = mean(x),
    mean_y = mean(y),
    sd_x = sd(x),
    sd_y = sd(y),
    r = cor(x, y) )

# Plot the data
ggplot(data=starwars)

# Plot height along x-axis
ggplot(data=starwars,mapping=aes(x=height))


# Plot mass along y-axis
ggplot(data=starwars,mapping=aes(x=height,y=mass))

ggplot(data=starwars,mapping=aes(x=height,y=mass)) +
  geom_point()


ggplot(data=starwars,mapping=aes(x=height,y=mass)) + geom_point() +
  labs(x="Height (cm)",y="Weight (Kg)")


ggplot(data=starwars,mapping=aes(x=height,y=mass)) +
  geom_point() +
  labs(x="Height (cm)",y="Weight (Kg)",
       title="Mass versus Height")

ggplot(data=starwars,mapping=aes(x=height,y=mass)) +
  geom_point() +
  labs(x="Height (cm)",y="Weight (Kg)",
       title="Mass versus Height",
       caption="Source: tidyverse/ starwars dataset")

# Install package
install.packages("shiny") 
# Invoke the package 
library(shiny)
# Run an example from the library 
runExample("01_hello")

# Run an example from the library 
runExample("06_tabsets")