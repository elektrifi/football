# Set game week
game_week <- 18
# Load data
load(paste("E:/datasci/Football/player_full_df_gw",game_week,".Rda", sep=""), .GlobalEnv,verbose=TRUE)
#
# player_full_df is a matrix at this point so convert explicitly to a data.frame
player_full_df<-as.data.frame(player_full_df)
# Delete first record (carry-over issue from getPlayerData)
player_full_df<-player_full_df[-1,]
#head(player_full_df)
# Set player full name
player_full_df$full_name<-paste(player_full_df$first_name, player_full_df$second_name, sep = " ")
#
player_name_v<-c(player_full_df$full_name)
player_last_name_v<-c(as.character(player_full_df$second_name))
player_type_v<-c(as.character(player_full_df$type_name))
player_club_v<-c(as.character(player_full_df$team_name))
player_total_points_v<-c(as.numeric(player_full_df$total_points))

# Initialise & get basic stats
player_row_df <- data.frame(player_name_v, player_type_v, player_club_v, player_total_points_v, player_last_name_v, stringsAsFactors = TRUE)
# Dedupe the data.frame, just in case!
player_row_df<-unique(player_row_df)

# See the current relationship between type_name and points grouped into club
library(ggplot2)
library(plyr)
#
plot1 <- ggplot(player_row_df, aes(x=player_club_v, y=player_total_points_v, color=player_type_v) ) + geom_point(size=4)
# Add a title
plot1 <- plot1 + ggtitle(paste("Game Week", game_week, " - Total Points Per Player by Club"))
plot1 <- plot1 + theme(plot.title = element_text(size=16, face="bold", vjust=2))
# Add axes titles
plot1 <- plot1 + labs(x="Club", y="Total Points Per Player")
# Change axes labels
plot1 <- plot1 + theme(axis.title.x = element_text(face="bold", colour="#000000", size=16), axis.text.x  = element_text(angle=90, vjust=0.5, size=16))
plot1 <- plot1 + theme(axis.title.y = element_text(face="bold", colour="#000000", size=16), axis.text.y  = element_text(size=16))
# Make legend title more appealing
plot1 <- plot1 + scale_color_manual("Position\n", c("Defender", "Forward", "Goalkeeper", "Midfielder" ), values=c("red", "green", "purple", "blue"))
#plot1 <- plot1 + scale_shape_discrete(name="Position")
# plot1 <- plot1 + scale_fill_discrete(name="Player\nPosition",
#                          breaks=c("Defender", "Forward", "Goalkeeper", "Midfielder" ),
#                          labels=c("Defender", "Forward", "Goalkeeper", "Midfielder" ) )
# Change legend appearance
plot1 <- plot1 + theme(legend.title = element_text(colour="black", size=14, face="bold"))
# Change legend label Label appearance
plot1 <- plot1 + theme(legend.text = element_text(colour="black", size = 14))
# Add data labels (but only if over a certain value of points)
plot1 <- plot1 + geom_text(data=subset(player_row_df, player_total_points_v >= 90), aes(y=player_total_points_v+5, label=(player_last_name_v)  ) )
#
plot1 #draw plot1
#
set.seed(20)
playerPointsCluster <- kmeans( player_row_df$player_total_points_v, 4, nstart=20 )
playerPointsCluster
#
# Compare cluster output with original dataset
#table(player_row_df$player_name_v, player_row_df$player_total_points_v)
#table(player_row_df$player_name_v, playerPointsCluster$cluster)
#
# Plot the cluster
playerPointsCluster$cluster <- as.factor(playerPointsCluster$cluster)
ggplot(player_row_df, aes(player_row_df$player_club_v, player_row_df$player_total_points_v, color= playerPointsCluster$cluster)) + geom_point()
#
ggplot(player_row_df, aes(player_row_df$player_total_points_v, player_row_df$player_total_points_v, color= playerPointsCluster$cluster)) + geom_point()
