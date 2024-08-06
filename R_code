### Strava Study

setwd("C:/Users/Mark/Desktop/ST8002 Project/Strava/")

# Import Strava data

data <- read.csv("strava.csv")

# Create a new dataframe containing only selected columns

data <- data.frame(ID = data$Activity.ID, Date = data$Activity.Date, Type = data$Activity.Type,
                   ElapsedTime = data$Elapsed.Time, MovingTime = data$Moving.Time,
                   Distance = data$Distance.1, MaxSpeed = data$Max.Speed, 
                   AverageSpeed = data$Average.Speed, ElevationGain = data$Elevation.Gain)

# Create new column containing Date/Time extracted from character

names(data)[2] <- "Datetime"
data$Datetime <- strptime(data$Datetime, format =  "%b %d, %Y, %H:%M:%S")
data$Date <- as.Date(data$Date, format =  "%b %d, %Y, %H:%M:%S")

# Remove entires with invalid/missing data

data <- na.omit(data)
data <- data[data$Type == "Ride", ]
data <- data[data$AverageSpeed > 3.2, ]

# Visualise data and check for normality

par(mfrow = c(2,2)) # create a 2*2 array for plotting

qqnorm(data$AverageSpeed, main = "Normal Q-Q Plot: Average Speed", cex.lab = 1.2)
qqline(data$AverageSpeed)

qqnorm(data$MaxSpeed, main = "Normal Q-Q Plot: Max Speed", cex.lab = 1.2)
qqline(data$MaxSpeed)

hist(data$AverageSpeed, main = "Histogram: Average Speed", xlab = "Average Speed (m/s)",
     ylim = c(0,200), xlim = c(3,7), cex.lab = 1.2)
hist(data$MaxSpeed, main = "Histogram: Max Speed", xlab = "Max Speed (m/s)",
     ylim = c(0,140), xlim = c(5,25), cex.lab = 1.2)

shapiro.test(data$AverageSpeed)
shapiro.test(data$MaxSpeed)

# Create two subset dataframes for 2020 and 2021

twenty <- data[data$Date >= "2020-01-01" & data$Date <= "2020-12-31", ]
twentyone <- data[data$Date >= "2021-01-01" & data$Date <= "2021-12-31", ]

# Obtain statistics

summary(twenty$AverageSpeed)
sd(twenty$AverageSpeed)
summary(twentyone$AverageSpeed)
sd(twentyone$AverageSpeed)


par(mfrow=c(2,2)) # reset the plotting area

qqnorm(twenty$AverageSpeed, main = "Normal Q-Q Plot: Average Speed (2020)", cex.lab = 1.2)
qqline(twenty$AverageSpeed)
qqnorm(twentyone$AverageSpeed, main = "Normal Q-Q Plot: Average Speed (2021)", cex.lab = 1.2)
qqline(twentyone$AverageSpeed)

hist(twenty$AverageSpeed, main = "Histogram: Average Speed (2020)", xlab = "Average Speed (m/s)",
     ylim = c(0,80), xlim = c(3,7), cex.lab = 1.2)
hist(twentyone$AverageSpeed, main = "Histogram: Average Speed (2021)", xlab = "Average Speed (m/s)",
     ylim = c(0,120), xlim = c(3,7), cex.lab = 1.2)

shapiro.test(twenty$AverageSpeed)
shapiro.test(twentyone$AverageSpeed)

par(mfrow=c(1,2)) # reset the plotting area

boxplot(twenty$AverageSpeed, twentyone$AverageSpeed, main = "Average Speed sorted by Year",
        xlab = "Year", ylab = "Average Speed (m/s)", cex.lab = 1.2, col = c("forestgreen", "gold"),
        names = c("2020", "2021"))

var.test(twenty$AverageSpeed, twentyone$AverageSpeed, alternative = "two.sided")


# Create a subset dataframe for each season

data$month <- factor(format(data$Datetime, "%B"), levels = month.name)
spring <- data[data$month == c("March", "April", "May"), ]
summer <- data[data$month == c("June", "July", "August"), ]
autumn <- data[data$month == c("September", "October", "November"), ]
winter <- data[data$month == c("December", "January", "February"), ]

boxplot(spring$AverageSpeed, summer$AverageSpeed, autumn$AverageSpeed, winter$AverageSpeed, 
        main = "Average Speed sorted by Season", xlab = "Season", ylab = "Average Speed (m/s)", 
        cex.lab = 1.2, col = c("pink", "firebrick", "darkorange", "skyblue"), 
        names = c("Spring", "Summer", "Autumn", "Winter"))

# Obtain statistics

summary(spring$AverageSpeed)
sd(spring$AverageSpeed)
summary(summer$AverageSpeed)
sd(summer$AverageSpeed)
summary(autumn$AverageSpeed)
sd(autumn$AverageSpeed)
summary(winter$AverageSpeed)
sd(winter$AverageSpeed)

# F test

var.test(autumn$AverageSpeed, winter$AverageSpeed, alternative = "two.sided")

# t-test

t.test(autumn$AverageSpeed, winter$AverageSpeed, var.equal = TRUE)



### Football Study

setwd("C:/Users/Mark/Desktop/ST8002 Project/Football")

# Import data

epl <- read.csv("epl_2019.csv")
efl <- read.csv("efl_2019.csv")

# Chi-squared test: Are team and number of goals scored independent?

goals_per_game <- data.frame(Round = epl$Round, Team = c(epl$Team.1, epl$Team.2), Goals = c(epl$S1, epl$S2))

goals_per_game$Goals[goals$Goals == 1] <- "1-2"
goals_per_game$Goals[goals$Goals == 2] <- "1-2"
goals_per_game$Goals[goals$Goals >= 3] <- "3+"

table <- table(goals_per_game$Goals, goals_per_game$Team)

chisq.test(table)

# Adjust plotting margins 

par(mar = c(4,13,4,8))

barplot(table, horiz = TRUE, las = 1, main = "Number of Goals Scored per Match in the Premier League",
        xlab = "Number of Matches", legend.text = rownames(table), args.legend = list(x = "topright",
        inset = c(-0.2, 0.035)), cex.lab = 1.2)

# Proportion Test: Are home teams equally likely to win in the premier league and championship?

epl$home_result <- ifelse(epl$S1 > epl$S2, "Win", ifelse(epl$S1 < epl$S2, "Loss", "Draw"))
efl$home_result <- ifelse(efl$S1 > efl$S2, "Win", ifelse(efl$S1 < efl$S2, "Loss", "Draw"))

epl_win = sum(epl$home_result == "Win")
efl_win = sum(efl$home_result == "Win")
n_epl = nrow(epl)
n_efl = nrow(efl)

prop.test(x = c(epl_win, efl_win), n = c(n_epl, n_efl), correct = FALSE)

# Are they equally likely to lose?

epl_loss = sum(epl$home_result == "Loss")
efl_loss = sum(efl$home_result == "Loss")

prop.test(x = c(epl_loss, efl_loss), n = c(n_epl, n_efl), correct = FALSE)

# Are draws equally likely?

epl_draw = sum(epl$home_result == "Draw")
efl_draw = sum(efl$home_result == "Draw")

prop.test(x = c(epl_draw, efl_draw), n = c(n_epl, n_efl), correct = FALSE)


# What is the probability >30 goals scored in a round?

epl$Goals <- epl$S1 + epl$S2
goals_per_round <- aggregate(epl$Goals, by = list(epl$Round), FUN = sum)
colnames(goals_per_round) <- c("Round", "Goals")

sum(goals_per_round$Goals > 30)/nrow(goals_per_round)

# What is the probability that no goals are scored in a match?

sum(epl$Goals == 0)/nrow(epl)

# What is the probability of a home team scoring more than one goal and losing?

sum(epl$home_result == "Loss" & epl$S1 > 1)/nrow(epl)

# What is the probability of an away team winning by more than two goals?

sum(epl$home_result == "Loss" & epl$S2 - epl$S1 > 2)/nrow(epl)

# What proportion of draws are goalless? 

sum(epl$Goals == 0)/sum(epl$home_result == "Draw")

# Given that at least one goal has been scored, what is the probability of a draw?

p_goal = sum(epl$Goals > 0)/nrow(epl)
p_goal_draw = sum(epl$S1 == epl$S2 & epl$Goals > 0)/nrow(epl)
p_goal_draw/p_goal



### Marathon Study

setwd("C:/Users/Mark/Desktop/ST8002 Project/Marathon/")

# Import libraries

library(chron)
library(nortest)

# Import data

data <- read.csv("marathon_results_2015.csv")

# Convert time character to duration in seconds

data$Official.Time <- as.numeric(chron(time = data$Official.Time))*86400

# Create age groups

data$Age[data$Age <= 25] <- "<= 25"
data$Age[data$Age >25 & data$Age <= 30] <- "26-30"
data$Age[data$Age >30 & data$Age <= 35] <- "31-35"
data$Age[data$Age >35 & data$Age <= 40] <- "36-40"
data$Age[data$Age >40 & data$Age <= 45] <- "41-45"
data$Age[data$Age >45 & data$Age <= 50] <- "46-50"
data$Age[data$Age >50 & data$Age <= 55] <- "51-55"
data$Age[data$Age >55] <- "56+"

tb1 <- table(data$Age)

par(mfrow=c(1,2)) # Create a 1*2 matrix for plotting

barplot(tb1, ylim = c(0,5000), main = "Runners by Age Group",
        xlab = "Age Group", cex.lab = 1.2)

# Create time groups

data$TimeGroup <- data$Official.Time

data$TimeGroup[data$TimeGroup < (3*60*60)] <- " <3 hours"
data$TimeGroup[data$TimeGroup >= (3*60*60) & data$TimeGroup < (3.5*60*60)] <- " 3-3.5 hrs"
data$TimeGroup[data$TimeGroup >= (3.5*60*60) & data$TimeGroup < (4*60*60)] <- " 3.5-4 hrs"
data$TimeGroup[data$TimeGroup >= (4*60*60) & data$TimeGroup < (4.5*60*60)] <- " 4-4.5 hrs"
data$TimeGroup[data$TimeGroup >= (4.5*60*60) & data$TimeGroup < (5*60*60)] <- " 4.5-5 hrs"
data$TimeGroup[data$TimeGroup >= (5*60*60)] <- " 5+ hrs"

tb2 <- table(data$TimeGroup)
barplot(tb2, ylim = c(0, 10000), main = "Runners by Finish Time",
        xlab = "Time Group", cex.lab = 1.2)

# For runners with times under 3 hours

par(mfrow=c(1,1)) # Reset the plotting area

tb3 <- table(data$Age[data$Official.Time < 3*60*60])
barplot(tb3, ylim = c(0,700), main = "Runners with Finish Times <3 hours by Age Group",
        xlab = "Age Group", cex.lab = 1.2)

# Normality check: Anderson-Darling test

hist(data$Official.Time, main = "Histogram of Finish Time", xlab = "Official Finish Time (s)",
     xlim = c(5000, 30000))

ad.test(data$Official.Time)

# Print contingency table and perform chi-square test

tb3 <- table(data$TimeGroup, data$Age)
tb3 <- addmargins(table(data$TimeGroup, data$Age),c(1,2))
tb3

chisq.test(tb3)

# Single Proportion Test

sub3 <- nrow(data[data$TimeGroup == " <3 hours", ])
n <- nrow(data)

prop.test(x = sub3, n = n, p = 0.025, correct = FALSE)


# Two Sample Proportion test

sub3_25 <- nrow(data[data$Age == "<= 25" & data$TimeGroup == " <3 hours", ])
sub3_30 <- nrow(data[data$Age == "26-30" & data$TimeGroup == " <3 hours", ])
n_25 <- sum(data$Age == "<= 25")
n_30 <- sum(data$Age == "26-30")

prop.test(x = c(sub3_25, sub3_30), n = c(n_25, n_30), correct = FALSE)


# Subset runners with a time of less than 3 hours

data <- tot_data[tot_data$Official.Time < 3*60*60, ]

# What proportion of runners finished in less than 3 hours?

nrow(data)/nrow(tot_data)
