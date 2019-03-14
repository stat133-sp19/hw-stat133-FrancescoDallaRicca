'title: make shot charts script'
'description: make pdf chart of where players shot hoops and whether they hit or missed'
'input: csv files'
'output: pdf'

library(ggplot2)
library(jpeg)
library(grid)

#CSV Files Needed

colclasses <- c('factor', 'character', 'double', 'double', 'double', 'double', 'factor', 'factor', 'factor', 'factor', 'character', 'double', 'double')
andre <- read.csv("data/andre-iguodala.csv", stringsAsFactors = FALSE, colClasses = colclasses)
andre$shot_made_flag <- as.character(andre$shot_made_flag)
andre$shot_made_flag[andre$shot_made_flag == 'y'] <- 'shot_yes'
andre$shot_made_flag[andre$shot_made_flag == 'y'] <- 'shot_yes'
andre$shot_made_flag[andre$shot_made_flag == 'n'] <- 'shot_no'
andre$shot_made_flag <- as.factor(andre$shot_made_flag)

draymond <- read.csv("data/draymond-green.csv", stringsAsFactors = FALSE, colClasses = colclasses)
draymond$shot_made_flag <- as.character(draymond$shot_made_flag)
draymond$shot_made_flag[draymond$shot_made_flag == 'y'] <- 'shot_yes'
draymond$shot_made_flag[draymond$shot_made_flag == 'y'] <- 'shot_yes'
draymond$shot_made_flag[draymond$shot_made_flag == 'n'] <- 'shot_no'
draymond$shot_made_flag <- as.factor(draymond$shot_made_flag)

kevin <- read.csv("data/kevin-durant.csv", stringsAsFactors = FALSE, colClasses = colclasses)
kevin$shot_made_flag <- as.character(kevin$shot_made_flag)
kevin$shot_made_flag[kevin$shot_made_flag == 'y'] <- 'shot_yes'
kevin$shot_made_flag[kevin$shot_made_flag == 'y'] <- 'shot_yes'
kevin$shot_made_flag[kevin$shot_made_flag == 'n'] <- 'shot_no'
kevin$shot_made_flag <- as.factor(kevin$shot_made_flag)

klay <- read.csv("data/klay-thompson.csv", stringsAsFactors = FALSE, colClasses = colclasses)
klay$shot_made_flag <- as.character(klay$shot_made_flag)
klay$shot_made_flag[klay$shot_made_flag == 'y'] <- 'shot_yes'
klay$shot_made_flag[klay$shot_made_flag == 'y'] <- 'shot_yes'
klay$shot_made_flag[klay$shot_made_flag == 'n'] <- 'shot_no'
klay$shot_made_flag <- as.factor(klay$shot_made_flag)

stephen <- read.csv("data/stephen-curry.csv", stringsAsFactors = FALSE, colClasses = colclasses)
stephen$shot_made_flag <- as.character(stephen$shot_made_flag)
stephen$shot_made_flag[stephen$shot_made_flag == 'y'] <- 'shot_yes'
stephen$shot_made_flag[stephen$shot_made_flag == 'y'] <- 'shot_yes'
stephen$shot_made_flag[stephen$shot_made_flag == 'n'] <- 'shot_no'
stephen$shot_made_flag <- as.factor(stephen$shot_made_flag)


court_file <- "images/nba-court.jpg"

court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc")
)

andre_scatterplot <- ggplot(data = andre) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)')+
  theme_minimal()

pdf('images/andre-iguodala-shot-chart.pdf', width = 6.5, height = 5)
andre_scatterplot
dev.off()

draymond_scatterplot <- ggplot(data = draymond) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)')+
  theme_minimal()
pdf('images/draymond-green-shot-chart.pdf', width = 6.5, height = 5)
draymond_scatterplot
dev.off()


kevin_scatterplot <- ggplot(data = kevin) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)')+
  theme_minimal()
pdf('images/kevin-durant-shot-chart.pdf', width = 6.5, height = 5)
kevin_scatterplot
dev.off()


klay_scatterplot <- ggplot(data = klay) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)')+
    theme_minimal()
pdf('images/klay-thompson-shot-chart.pdf', width = 6.5, height = 5)
klay_scatterplot
dev.off()



stephen_scatterplot <- ggplot(data = stephen) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)')+
  theme_minimal()
pdf('images/stephen-curry-shot-chart.pdf', width = 6.5, height = 5)
stephen_scatterplot
dev.off()

shot_data <- read.csv('data/shots-data.csv')

gsw_shot_chart <- ggplot(shot_data)+
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  theme_minimal()+
  facet_wrap(~name)

pdf('images/gsw-shot-chart.pdf', width = 8, height = 7)
gsw_shot_chart
dev.off()

png('images/gsw-shot-chart.png', width = 8, height = 7, units = 'in', res = 1000)
gsw_shot_chart
dev.off()

