'title: make shots data script'
'description: data preparation'
'input: csv files'
'output: text file summaries'

minutemaker <- function(x, y){
  period <- x
  minute <- y
  return(12*x -y)
}

colclasses <- c('factor', 'character', 'double', 'double', 'double', 'double', 'factor', 'factor', 'factor', 'factor', 'character', 'double', 'double')
andre <- read.csv("data/andre-iguodala.csv", stringsAsFactors = FALSE, colClasses = colclasses)
andre$name <- "Andre Iguodala"
andre$minute <- (minutemaker(andre$period, andre$minutes_remaining))
andre$shot_made_flag <- as.character(andre$shot_made_flag)
andre$shot_made_flag[andre$shot_made_flag == 'y'] <- 'shot_yes'
andre$shot_made_flag[andre$shot_made_flag == 'y'] <- 'shot_yes'
andre$shot_made_flag[andre$shot_made_flag == 'n'] <- 'shot_no'
andre$shot_made_flag <- as.factor(andre$shot_made_flag)
sink("output/andre-iguodala-summary.txt")
summary(andre)
sink()

draymond <- read.csv("data/draymond-green.csv", stringsAsFactors = FALSE, colClasses = colclasses)
draymond$name <- "Draymond Green"
draymond$minute <- (minutemaker(draymond$period, draymond$minutes_remaining))
draymond$shot_made_flag <- as.character(draymond$shot_made_flag)
draymond$shot_made_flag[draymond$shot_made_flag == 'y'] <- 'shot_yes'
draymond$shot_made_flag[draymond$shot_made_flag == 'y'] <- 'shot_yes'
draymond$shot_made_flag[draymond$shot_made_flag == 'n'] <- 'shot_no'
draymond$shot_made_flag <- as.factor(draymond$shot_made_flag)
sink("output/draymond-green-summary.txt")
summary(draymond)
sink()

kevin <- read.csv("data/kevin-durant.csv", stringsAsFactors = FALSE, colClasses = colclasses)
kevin$name <- "Kevin Durant"
kevin$minute <- (minutemaker(kevin$period, kevin$minutes_remaining))
kevin$shot_made_flag <- as.character(kevin$shot_made_flag)
kevin$shot_made_flag[kevin$shot_made_flag == 'y'] <- 'shot_yes'
kevin$shot_made_flag[kevin$shot_made_flag == 'y'] <- 'shot_yes'
kevin$shot_made_flag[kevin$shot_made_flag == 'n'] <- 'shot_no'
kevin$shot_made_flag <- as.factor(kevin$shot_made_flag)
sink("output/kevin-durant-summary.txt")
summary(kevin)
sink()

klay <- read.csv("data/klay-thompson.csv", stringsAsFactors = FALSE, colClasses = colclasses)
klay$name <- "Klay Thompson"
klay$minute <- (minutemaker(klay$period, klay$minutes_remaining))
klay$shot_made_flag <- as.character(klay$shot_made_flag)
klay$shot_made_flag[klay$shot_made_flag == 'y'] <- 'shot_yes'
klay$shot_made_flag[klay$shot_made_flag == 'y'] <- 'shot_yes'
klay$shot_made_flag[klay$shot_made_flag == 'n'] <- 'shot_no'
klay$shot_made_flag <- as.factor(klay$shot_made_flag)
sink("output/klay-thompson-summary.txt")
summary(klay)
sink()

stephen <- read.csv("data/stephen-curry.csv", stringsAsFactors = FALSE, colClasses = colclasses)
stephen$name <- "Stephen Curry"
stephen$minute <- (minutemaker(stephen$period, stephen$minutes_remaining))
stephen$shot_made_flag <- as.character(stephen$shot_made_flag)
stephen$shot_made_flag[stephen$shot_made_flag == 'y'] <- 'shot_yes'
stephen$shot_made_flag[stephen$shot_made_flag == 'y'] <- 'shot_yes'
stephen$shot_made_flag[stephen$shot_made_flag == 'n'] <- 'shot_no'
stephen$shot_made_flag <- as.factor(stephen$shot_made_flag)
sink("output/stephen-curry-summary.txt")
summary(stephen)
sink()

shots_data <- rbind(andre, draymond, kevin, klay, stephen)

write.csv(shots_data, "data/shots-data.csv")

sink("output/shots-data-summary.txt")
summary(shots_data)
sink()