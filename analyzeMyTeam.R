#
library(jsonlite)
library(zoo)
library(calibrate)

# Load data
load("E:/datasci/Football/player_full_df_gw17.Rda", .GlobalEnv,verbose=TRUE)

# player_full_df is a matrix at this point so convert explicitly to a data.frame
player_full_df<-as.data.frame(player_full_df)

# Define my team
#
bathgateNoMoar=c(
  "Jack Butland", 
  "Simon Mignolet", 
  "Seamus Coleman", 
  "Aleksandar Kolarov", 
  "Toby Alderweireld", 
  "Craig Dawson", 
  "Chris Smalling", 
  "Marko Arnautovic", 
  "Mesut Özil", 
  "Ross Barkley", 
  "Riyad Mahrez", 
  "Yannick Bolasie", 
  "Odion Ighalo", 
  "Romelu Lukaku", 
  "Jamie Vardy")

# Set player full name
player_full_df$full_name<-paste(player_full_df$first_name, player_full_df$second_name, sep = " ")

# Delete first record (carry-over issue from getPlayerData)
player_full_df<-player_full_df[-1,]

# Initialise & get basic stats
team_df<-data.frame()
for(i in 1:636)
{
  # Get element i of the player_full_df matrix
  row<-player_full_df[i,]
  
  #
  for (j in 1:15) 
  {
    if (row$full_name == bathgateNoMoar[j]) 
    {
      team_df<-rbind(team_df, row)
    }
  }
}
# Dedupe the data.frame, just in case!
team_df<-unique(team_df)

# Segment data according to Goalkeeper, Forward etc using type_name
goalkeepers_df<-data.frame()
defenders_df <- data.frame()
midfielders_df <- data.frame()
forwards_df <- data.frame()

for(i in 1:636)
{
  # Get element i of the player_full_df matrix
  row<-player_full_df[i,]
  if (row$type_name == "Goalkeeper" & row$status == "a" & row$total_points >= 10 & row$form > 0.0 )
  {
    goalkeepers_df <- rbind(goalkeepers_df, row)
  }
  else if (row$type_name == "Defender" & row$status == "a" & row$total_points >= 10 & row$form > 0.0 )
  {
    defenders_df <- rbind(defenders_df, row)
  }
  else if (row$type_name == "Midfielder" & row$status == "a" & row$total_points >= 10 & row$form > 0.0 )
  {
    midfielders_df <- rbind(midfielders_df, row)
  }
  else if (row$type_name == "Forward" & row$status == "a" & row$total_points >= 10 & row$form > 0.0 )
  {
    forwards_df <- rbind(forwards_df, row)
  }
}
# Set up data frames of all player types
# Goalkeepers
gk_points <- goalkeepers_df$fixture_history$json$all[,20]
#gk_total_points <- goalkeepers_df$total_points
#gk_name <- goalkeepers_df$full_name
gk_bonus <- goalkeepers_df$fixture_history$json$all[,15]
gk_saves <- goalkeepers_df$fixture_history$json$all[,14]
gk_goals_con <- goalkeepers_df$fixture_history$json$all[,8]
gk_clean_sheets <- gk_goals_con <- goalkeepers_df$fixture_history$json$all[,7]
gk_mins_played <- goalkeepers_df$fixture_history$json$all[,4]
gk_week_num <- goalkeepers_df$fixture_history$json$all[,2]
gk_df <- data.frame(gk_week_num, gk_mins_played, gk_clean_sheets, gk_goals_con, gk_saves, gk_bonus, gk_points) 
# Defenders
def_points <- defenders_df$fixture_history$json$all[,20]
def_bonus <- defenders_df$fixture_history$json$all[,15]
def_goals_con <- defenders_df$fixture_history$json$all[,8]
def_assists <- defenders_df$fixture_history$json$all[,6]
def_mins_played <- defenders_df$fixture_history$json$all[,4]
def_week_num <- defenders_df$fixture_history$json$all[,2]
def_df <- data.frame(def_week_num, def_mins_played, def_goals_con, def_assists, def_bonus, def_points) 
# Midfielders
mid_points <- midfielders_df$fixture_history$json$all[,20]
mid_bonus <- midfielders_df$fixture_history$json$all[,15]
mid_assists <- midfielders_df$fixture_history$json$all[,6]
mid_goals_scored <- midfielders_df$fixture_history$json$all[,5]
mid_mins_played <- midfielders_df$fixture_history$json$all[,4]
mid_week_num <- midfielders_df$fixture_history$json$all[,2]
mid_df <- data.frame(mid_week_num, mid_mins_played, mid_goals_scored, mid_assists, mid_bonus, mid_points) 
# Forwards
for_points <- forwards_df$fixture_history$json$all[,20]
for_bonus <- forwards_df$fixture_history$json$all[,15]
for_assists <- forwards_df$fixture_history$json$all[,6]
for_goals_scored <- forwards_df$fixture_history$json$all[,5]
for_mins_played <- forwards_df$fixture_history$json$all[,4]
for_week_num <- forwards_df$fixture_history$json$all[,2]
for_df <- data.frame(for_week_num, for_mins_played, for_goals_scored, for_assists, for_bonus, for_points) 

# Analyse a player's point-scoring history
# See http://fantasy.premierleague.com/my-team/#353 for meta-data
# player$json$all[,2] = game week
# player$json$all[,20] = totoal points scored inc bonuses

# Use this to manage plotting loop below
player_all<-team_df[,]$fixture_history # e.g. use [1,] for Ozil [,] for all

# initialise 3 week rolling average
roll_avg_3 <- 0

# Set up the plot
cl<-rainbow(15)
par(mfrow=c(1,1))
pdf("E:/datasci/Football/BathgateNoMoar_plots_gw_17.pdf", width=28, height = 18, paper="A4r")
y_ticks = c(1:25)
x_ticks = c(1:20)
for ( i in 1:length(player_all) )
{
  # Which player...
  player_name     <- team_df[i,]$full_name
  player_position <- team_df[i,]$type_name
  player_team     <- team_df[i,]$team_name
  game_week       <- length(gk_week_num)

  message(sprintf("Player %s...", player_name))
  
  # Get basic stats
  player         <- team_df[i,]$fixture_history
  week_num       <- as.numeric(player$json$all[,2])
  points         <- as.numeric(player$json$all[,20])
  mean_points    <- mean(as.numeric(player$json$all[,20]) )
  value          <- as.numeric(player$json$all[,19])
  net_transfers  <- as.numeric(player$json$all[,18])
  bps            <- as.numeric(player$json$all[,17])
  eps            <- as.numeric(player$json$all[,16])
  bonus          <- as.numeric(player$json$all[,15])
  saves          <- as.numeric(player$json$all[,14])
  red_cards      <- as.numeric(player$json$all[,13])
  yellow_cards   <- as.numeric(player$json$all[,12])
  pen_missed     <- as.numeric(player$json$all[,11])
  pen_saved      <- as.numeric(player$json$all[,10])
  own_goals      <- as.numeric(player$json$all[,9])
  goals_con      <- as.numeric(player$json$all[,8])
  clean_sheets   <- as.numeric(player$json$all[,7])
  assists        <- as.numeric(player$json$all[,6])
  goals_scored   <- as.numeric(player$json$all[,5])
  mins_played    <- as.numeric(player$json$all[,4]) 
  opponent       <- player$json$all[,3] 
  
  # Calc rolling average
  for (j in 1:length(as.numeric(player$json$all[,20]))) 
  {
    if (j <= 2) 
    {
      roll_avg_3[[j]] = 0
    }
    else
    {
      roll_avg_3[[j]] = ( as.numeric(player$json$all[j,20]) + 
                          as.numeric(player$json$all[j-1,20]) + 
                          as.numeric(player$json$all[j-2,20]) ) /3 
    }
  }
  roll_avg_3
    
  # Plot stuff
  plot(week_num, points, main=paste(paste("Week", game_week, sep = " "), player_name, player_position, player_team, sep = " - "), 
       xlab="Game Week", ylab="Points", 
       xlim=c(0,20), ylim=c(0,25), xaxt="n", yaxt="n",
       type="b", col="blue")
  axis(2, at=y_ticks)
  axis(1, at=x_ticks)
  grid()
  textxy(week_num, points, opponent, offset=0.6*runif(1.5,min=1,max=1.5))
  #legend("topright", bty="n", inset=0.01, c("Points", "Mean", "Rolling 3 Week Mean", "Indiv Best Fit", "Group Best Fit"), lty=c(1,1,2,1,4), lwd=c(1,1,1,1,2), col=c("blue","green","black","red", "purple"))
  #legend("topright", bty="n", inset=0.01, c("Points", "Mean", "Rolling 3 Week Mean", "Indiv Best Fit"), lty=c(1,1,2,1), lwd=c(1,1,1,1), col=c("blue","green","black","red"))
  legend("topright", bty="n", inset=0.01, c("Actual", "Model"), lty=c(1,1), lwd=c(1,2), col=c("blue","brown"))
  #lines(week_num, points, main=player_name, xlim=c(0,20), ylim=c(0,20), type="b", col=cl[i])
  # Rolling average
  #lines(week_num, roll_avg_3, col="black", lty=2)
  
  # plot mean points
  #abline(h=mean_points, col="green", lwd=1, lty=1)
  
  # plot 3 week rollng average
  #abline(h=roll_avg_3, col="darkgreen", lwd=1)
  
  # plot linear regression
  #fit <- lm( points ~ week_num + bps + bonus +
  #            saves + red_cards + yellow_cards + pen_missed + pen_saved + own_goals + 
  #            goals_con + clean_sheets + assists + goals_scored + mins_played, team_df[i,])
  if (player_name<-team_df[i,]$type_name == "Goalkeeper") 
  {
    #fit <- lm( points ~ goals_con + saves + clean_sheets + mins_played, data=team_df[i,])
    #fit <- lm( points ~ goals_con + saves + clean_sheets + bonus + mins_played, data=gk_df)
    #co <- coef(fit)
    #abline(fit, col="blue", lwd=1)
    #abline(lm(gk_df$gk_points ~ gk_df$gk_week_num), col="purple", lwd=2, lty=4, data=gk_df)
  } 
  else if (player_name<-team_df[i,]$type_name == "Defender") 
  {
    #fit <- lm( points ~ goals_con + assists + mins_played + own_goals, data=def_df)
    #co <- coef(fit)
    #abline(fit, col="blue", lwd=1)
    #abline(lm(def_df$def_points ~ def_df$def_week_num), col="purple", lwd=2, lty=4, data=def_df)
  }
  else if (player_name<-team_df[i,]$type_name == "Midfielder")
  {
    #fit <- lm( points ~ goals_scored + assists + mins_played, data=mid_df)
    #co <- coef(fit)
    #abline(fit, col="blue", lwd=1)
    #abline(lm(mid_df$mid_points ~ mid_df$mid_week_num), col="purple", lwd=2, lty=4, data=mid_df)
  }
  else if (player_name<-team_df[i,]$type_name == "Forward")
  {
    #fit <- lm( points ~ goals_scored + assists + mins_played, data=for_df)
    #co <- coef(fit)
    #abline(fit, col="blue", lwd=1)
    #abline(lm(for_df$for_points ~ for_df$for_week_num), col="purple", lwd=2, lty=4, data=for_df)
  }
  
  # Linear best fit line
  #abline(fit <-lm(points ~ week_num), col="red", lwd=1)
  #legend("topleft", bty="n", legend=paste("R2 is ", format( summary(fit)$adj.r.squared, digits=4 ) ) )
  
  # Non-linear best fit line
  #p1 = mean_points
  #p2 = 0.1
  #fit2 <- nls(points ~ p1*cos(p2*week_num) + p2*sin(p1*week_num), start=list(p1=p1,p2=p2), control=list(maxiter=50), alg='plinear', trace=TRUE)
  #fit2_line = data.frame(week_num = seq(min(week_num),max(week_num),len=17))
  #lines(fit2_line$week_num, predict(fit2, newdata=fit2))
  
  # Non-linear best fit line
  fit3 <- lm( points ~ poly(week_num, 3, raw = TRUE) )
  #fit3_line = data.frame(week_num = seq(min(week_num),max(week_num), len=17))
  #lines(fit3_line$week_num, predict(fit3, newdata=fit3))
  
  # Create function for the third order polynomial
  third_order <- function(newdist, model) {
    coefs <- coef(model)
    #y = d + cx + bx^2 + ax^3
    res <- coefs[1] + (coefs[2] * newdist) + (coefs[3] * newdist^2) + (coefs[4] * newdist^3)
    return(res)
  }
  yy <- third_order(week_num, fit3)
  #lines(week_num, yy, type="l")
    
  # Try sine/cosine curve
  xc<-cos(2*pi*week_num/3)
  xs<-sin(2*pi*week_num/3)
  fitcs.lm <- lm( points ~ roll_avg_3 + ( ( roll_avg_3 * xc ) + ( roll_avg_3 * xs ) ) )
  fitcs <- fitted(fitcs.lm)
  
  # Predictions
  pred <- predict(fitcs.lm, newdata=data.frame(week_num))
  #lines(fitcs, col="yellow", lwd=2)
  lines(week_num, pred, col="brown", lwd=2)
  
  r2 <- summary(fitcs.lm)$adj.r.squared
  #p_value <- fitcs.lm$coefficients[2,4]
  legend("topleft", bty="n", legend=paste("R2", format( r2, digits=4 ) ) )
  
  # Prediction
  #model <- lm(points ~ week_num, data=gk_df)
  #new.df <- data.frame(week_num=c(18, 19, 20))
  #predict.lm(points ~ week_num)
  
}
dev.off()