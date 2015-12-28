library(lpSolve)
library(stringr)
library(RCurl)
library(jsonlite)
library(plyr)
library(microbenchmark)
library(data.table)
library(RPushbullet)

# scrape the data
start.time <- Sys.time()
player_df <- ldply(1:636, function(x){
  # Scrape responsibly kids, we don't want to ddos
  # the Fantasy Premier League's website
  Sys.sleep(runif(1,min=0.5,max=2.0))
  url <- sprintf("http://fantasy.premierleague.com/web/api/elements/%s/?format=json", x)
  json <- fromJSON(getURL(url))
  json$now_cost <- json$now_cost / 10
  data.frame(json[names(json) %in% c('web_name', 'team_name', 'type_name', 
                                     'now_cost', 'total_points')])
})
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# Alert job completion
message <- paste("Time was ", time.taken, " mins")
pbPost("note", "FPL player scrap done", message)

#================
# Alternative way to get all the data
#================
library(jsonlite)
# Create a data frame (use existing data to do the hard work)
#player_full_df = fromJSON("E:/datasci/Football/player.json")
# Now clean out the content but leave the structure
#player_full_df[,]=matrix(ncol=ncol(player_full_df), rep(NA, prod(dim(player_full_df))))
#
# Populate the content
load("E:/datasci/Football/player_full_df_gw17.Rda")
# Now clean out the content but leave the structure
player_full_df[,]=matrix(ncol=ncol(player_full_df), rep(NA, prod(dim(player_full_df))))
start.time <- Sys.time()
for(i in 1:636) 
{
  message(sprintf("Player %d", i))
  url <- sprintf("http://fantasy.premierleague.com/web/api/elements/%s/?format=json", i)
  json <- fromJSON(url, flatten=TRUE)
  player_full_df<-rbind(player_full_df, json)
  
  # Add this json to the overall data frame
  #player_full_df[nrow(paste(json$first_name, json$second_name, sep=" ")) + 1, ] <- json
  Sys.sleep(runif(1,min=0.5,max=1.5))
  player_full_df
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# Alert job completion
#message <- paste("Time was ", time.taken, " mins")
#pbPost("note", "FPL player scrap done", message)

# Save down the player data
save(player_full_df,file="E:/datasci/Football/player_full_df_gw18.Rda")

# Tidy it up as Evra still in there
#df <- subset(player_df, !web_name %in% c('Evra'))

# The vector to optimize on
objective <- player_df$total_points

# Fitting Constraints
num_gk <- 2
num_def <- 5
num_mid <- 5
num_fwd <- 3
max_cost <- 100.7

# Create vectors to constrain by position
player_df$Goalkeeper <- ifelse(player_df$type_name == "Goalkeeper", 1, 0)
player_df$Defender <- ifelse(player_df$type_name == "Defender", 1, 0)
player_df$Midfielder <- ifelse(player_df$type_name == "Midfielder", 1, 0)
player_df$Forward <- ifelse(player_df$type_name == "Forward", 1, 0)

# Create constraint vectors to constrain by max number of players allowed per team
team_constraint <- unlist(lapply(unique(player_df$team_name), function(x, player_df){
  ifelse(player_df$team_name==x, 1, 0)
}, player_df=player_df))

# next we need the constraint directions
const_dir <- c("=", "=", "=", "=", rep("<=", 21))

# Now put the complete matrix together
const_mat <- matrix(c(player_df$Goalkeeper, player_df$Defender, player_df$Midfielder, player_df$Forward, 
                      player_df$now_cost, team_constraint), 
                    nrow=(5 + length(unique(player_df$team_name))), byrow=TRUE)
const_rhs <- c(num_gk, num_def, num_mid, num_fwd, max_cost, rep(3, 20))

# then solve the matrix
x <- lp ("max", objective, const_mat, const_dir, const_rhs, all.bin=TRUE, all.int=TRUE)

# And this is our team!
print(arrange(player_df[which(x$solution==1),], desc(Goalkeeper), desc(Defender), 
              desc(Midfielder), desc(Forward), desc(total_points)))
print(str_c('Total Price: ', sum(player_df[which(x$solution==1), 'now_cost'])))
print(str_c('Total Points: ', sum(player_df[which(x$solution==1), 'total_points'])))
