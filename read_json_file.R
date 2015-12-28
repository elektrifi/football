install.packages("jsonlite", dep = TRUE)
library(jsonlite)

# Read FPL player data
data <- fromJSON('E:/datasci/Football/player.json')
# Show data
data

# Convert to data frame
df <- data.frame(data)

# Collapse the list names into a new column
install.packages("dplyr", dep = TRUE)
library(dplyr)
data <- bind_rows(data, .id = 'play')

# Alternative
players <- fromJSON("E:/datasci/Football/player.json", flatten=TRUE)
colnames(players)
players[,c("web_name")]
