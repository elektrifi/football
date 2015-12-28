# load data into data.frame
# Data from http://www.football-data.co.uk/englandm.php
# Data dictionary at http://www.football-data.co.uk/notes.txt
# Just for reference and understanding
#rawdf <- read.csv('http://www.football-data.co.uk/mmz4281/1516/E0.csv')

df <- read.csv('http://www.football-data.co.uk/mmz4281/1516/E0.csv')

# munge data into format compatible with glm function
df <- apply(df, 1, function(row){
  data.frame(team=c(row['HomeTeam'], row['AwayTeam']),
             opponent=c(row['AwayTeam'], row['HomeTeam']),
             goals=c(row['FTHG'], row['FTAG']),
             home=c(1, 0))
})
df <- do.call(rbind, df)

# Teams
my_team<-"Sunderland"
my_opponent<-"Liverpool"

# ensure we've not ended up with factors!
df$goals <- as.numeric(as.character(df$goals))

# fit the model
model <- glm(goals ~ home + team + opponent, 
             family=poisson(link=log), data=df)

# let's make some predictions!
av_home_goals <- predict(model, 
                         data.frame(home=1, team=my_team, 
                                    opponent=my_opponent), 
                         type="response")

av_away_goals <- predict(model, 
                         data.frame(home=0, team=my_team, 
                                    opponent=my_opponent), 
                         type="response")

# get probabilities per goal
home_goals <- dpois(0:10, av_home_goals) 
away_goals <- dpois(0:10, av_away_goals)

# convert probability vectors into score matrix
m <- home_goals %o% away_goals

# get probabilities for home, draw, away win
draw <- sum(diag(m))
away <- sum(m[upper.tri(m)])
home <- sum(m[lower.tri(m)])
home
away
draw