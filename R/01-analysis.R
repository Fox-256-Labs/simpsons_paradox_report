# Messy R code

data <- read.csv("https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/data_2.csv")

summary(data)

print(round(mean(subset(na.omit(data), species == "Adelie" & island == "Torgersen")$bill_length_mm),2))
print(round(mean(subset(na.omit(data), species == "Adelie" & island == "Biscoe")$bill_length_mm),2))
print(round(mean(subset(na.omit(data), species == "Adelie" & island == "Dream")$bill_length_mm),2))


# Plot
penguins_clean <- na.omit(   data  )
plot(penguins_clean$bill_length_mm, penguins_clean$bill_depth_mm, type='n', xlab='Bill Length (mm)', ylab='Bill Depth (mm)', main='Penguin Bill Dimensions')
points(
  penguins_clean$bill_length_mm[penguins_clean$species  ==  "Adelie"], penguins_clean$bill_depth_mm[penguins_clean$species == "Adelie"], col='red', pch=16)
points(penguins_clean$bill_length_mm[penguins_clean$species == "Chinstrap"], penguins_clean$bill_depth_mm[penguins_clean$species == "Chinstrap"], col='green', pch=17)
points(penguins_clean$bill_length_mm[penguins_clean$species == "Gentoo"],
       penguins_clean$bill_depth_mm[penguins_clean$species == "Gentoo"], col='blue', pch=18)
legend("topright", legend=unique(penguins_clean$species),
       col=c('red'
             , 'green',
             'blue'), pch=c(16, 17, 18))

# -------------------------------------------------------------------------------------------------------------------------------
# An improvement to the above code using tidyverse collection of packages
library(tidyverse)

# Saving the penguin dataset in the input folder
write.csv(data,file = "input/data.csv")

# Using ggplot to visualize
ggplot(data = penguins_clean,
       mapping = aes(x = bill_length_mm, y = bill_depth_mm,color = species)
) +
  geom_point()

# Creating some basic functions

multiply_by_234 <- function(a) {
  return(a*234)
}

multiply_by_234(311)

add_two_numbers <- function(a,b) {
  return((a+b))
}

add_two_numbers(3256,8934)

# My version of a function that creates a scatterplot 
# I missed a few important features, but I came close

create_scatter_plot <- function(island, species) {
  scatter_data <- penguins_clean %>% 
    filter(island,species)
  scatter_plot <- ggplot(data = scatter_data,
         mapping = aes(x = bill_length_mm, y = bill_depth_mm,color = species)
  ) +
    geom_point()
  return(scatter_plot)
  
}

create_scatter_plot(Adelie,Torgersen)

# Better version of function that creates a scatterplot

# plot function
create_scatterplot <- function(data, selected_species, selected_island) {
  # Filter the data for the specified species and island
  filtered_data <- data %>%
    na.omit() %>%
    filter(species == selected_species, island == selected_island)
  
  # Create the scatterplot
  plot <- ggplot(
    filtered_data,
    aes(x = bill_length_mm, y = bill_depth_mm, color = species, shape = species)
  ) +
    geom_point() +
    labs(
      x = "Bill Length (mm)",
      y = "Bill Depth (mm)",
      title = paste("Penguin Bill Dimensions -", selected_species, "-", selected_island)
    )
  
  return(plot)
}

# Use the function
create_scatterplot(data, "Adelie", "Torgersen")

source(file="R/00-functions.R")

# -----------------------------------------------------------------------------------------

# Creating a quarto document

library(rmarkdown)
