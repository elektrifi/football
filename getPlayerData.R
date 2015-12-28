library(jsonlite)
# Retrieve player data for latest game week
#
number_of_players<-636
#
# Create an empty data frame wit the correct dimensions
# Create Data Frame with NA's
load("E:/datasci/Football/player_full_df_gw18.Rda")
#player_full_df=data.frame(matrix(NA, nrow=number_of_players+1, ncol=63))

# Confirm Size of Data Frame
#dim(player_full_df)

# Change Variable Names
#names(player_full_df) <- c("SNP","P_value")

# Old way of populating the structure & content
#load("E:/datasci/Football/player_full_df_gw17.Rda")
# Now clean out the content but leave the structure
#player_full_df[,]=matrix(ncol=ncol(player_full_df), rep(NA, prod(dim(player_full_df))))

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
  
  # Add this json to the overall data frame
  #player_full_df[nrow(paste(json$first_name, json$second_name, sep=" ")) + 1, ] <- json
  Sys.sleep(runif(1,min=0.5,max=1.5))
  #player_full_df
  message(sprintf("...player %d done.", i))
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# Alert job completion
#message <- paste("Time was ", time.taken, " mins")
#pbPost("note", "FPL player scrap done", message)

# Save down the player data
save(player_full_df,file="E:/datasci/Football/player_full_df_gw18.Rda")
