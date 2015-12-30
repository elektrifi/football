install.packages('jsonlite', dep=TRUE)
library(jsonlite)
# Retrieve player data for latest game week
#
game_week <- 19
number_of_players <- 636
#
# Create an empty data frame with the correct dimensions
load(paste("E:/datasci/Football/player_full_df_gw", game_week-1, ".Rda", sep=""))
#
player_dimensions=dim(player_full_df)
num_rows<-player_dimensions[1]
# Now delete the existing rows...
for(j in 1:num_rows)
{
  message(sprintf("Deleting row %d...", j))
  player_full_df<-player_full_df[-1*j,]
}

# Now scrape the data...
start.time <- Sys.time()
for(i in 1:number_of_players) 
{
  message(sprintf("Player %d...", i))
  url <- sprintf("http://fantasy.premierleague.com/web/api/elements/%s/?format=json", i)
  json <- fromJSON(url, flatten=TRUE)
  player_full_df<-rbind(player_full_df, json)

  # Pause before getting next player   
  Sys.sleep(runif(1,min=0.5,max=1.5))
  message(sprintf("...player %d done.", i))
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Save down the player data
save(player_full_df,file=paste("E:/datasci/Football/player_full_df_gw", game_week, ".Rda", sep=""))