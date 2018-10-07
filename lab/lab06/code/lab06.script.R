#==============================================================
#Title: Cleaning Data
#Description: 
  #This script performs clenaing tasks and transformations on
  #various columns of the raw data file.
#Inputs(s): data file 'raw-data.csv'
#Outpust(s): data file 'clean-data.csv'
#Author:Jose Lucar
#Date: 2-27-2018
#==============================================================

#Packages
library(readr) #Importing Data
library(dplyr) #Data wrangling
library(ggplot2) #Graphics


data<- read_csv('../data/nba2018-players.csv')
warriors<-arrange(filter(data, team== 'WSW'), salary)
write.csv(warriors, file = '../data/warriors.csv', row.names = FALSE)
lakers<-arrange(filter(data, team== 'LAL'), desc(experience))
#This time I didn't use row_names=FALSE to avoid including first column of numbers
write.csv(lakers, file = '../data/lakers.csv')

#Exporting outputs to output folder using sink() function:

#example:

sink(file = '../output/summary-height-weight.txt')
summary(data[ ,c('height', 'weight')])
sink()

#Your turn:

sink(file = '../output/data-structure.txt')
summary(data)
sink()


sink(file = '../output/summary-warriors.txt')
summary(warriors)
sink()


sink(file = '../output/summary-lakers.txt')
summary(lakers)
sink()

#Exporting graphics: 

#Base R provides a wide array of functions to saveimages in most common formats:

#png()
#jpeg()
#tiff()
#bmp()
#svg()
#pdf()

# first example:

png(filename = "../images/scatterplot-height-weight.png")
plot(data$height, data$weight, pch = 20, 
     xlab = 'Height', ylab = 'Height')
dev.off()


#The function png() tells R to save the image in PNG format, using the provided filename.
#Invoking png() will open a graphics device; not the graphics device of RStudio, so you won’t be able to see the graphic.
#The plot() function produces the scatterplot.
#The function dev.off() closes the graphics device.

#Open the help documentation of png() and related graphic devices.
help(png)

#Use png() to save a scatterplot of height and weight with plot(). Save the graph in the images/ folder

png(filename = "../images/scatterplot-height-weight.png")
plot(data$height, data$weight, pch = 20, 
     xlab = 'Height', ylab = 'Height')
dev.off()

#Save another version of the scatterplot between height and
#weight, but now try to get an image with higher resolution.
#Save the plot in images/.

png(filename = '../images/scatterplot2-height-weight.png', pointsize = 20)
plot(data$height, data$weight, las = 1, pch = 19,
     xlab = 'Height', ylab = 'Weight')
dev.off()

#Save a histogram in JPEG format of age with dimensions 
#(width x height) 600 x 400 pixels. Save the plot in images/.


jpeg(filename = '../images/histogram-age.jpeg', width = 600, height = 400, units="px")
hist(data$age, xlab = 'Age', las = 1, col = 'gray80')
dev.off()

#Use pdf() to save the previous histogram of age in PDF format, 
#with dimensions (width x height) 7 x 5 inches.

?hist
?pdf
pdf(file = '../images/histogram-age.pdf', width = 7, height = 5)
hist(data$age, xlab = 'Age', las = 1, col = 'gray80')
dev.off()


#This last pdf() option didn't let me use any units specification.

#Exporting some ggplot

gg_pts_salary <- ggplot(data = data, aes(x = points, y = salary)) +
  geom_point()

gg_pts_salary

ggsave('../images/points_salary.pdf', 
       plot = gg_pts_salary,
       width = 7, height = 5, units = "in")

#Use ggplot() to create a scatterplot of height and weight, faceting by position. Store this in a ggplot object gg_ht_wt_positions 
#Then use ggsave() to save the plot with dimensions
#(width x height) 6 x 4 inches; in the images/ folder 
#as height_weight_by_position.pdf

gg_ht_wt_positions <- ggplot(data = data, aes(x = height, y = weight)) +
geom_point(alpha = 0.7) +
facet_wrap(~ position)

ggsave('../images/height_weight_by_position.pdf', 
       plot = gg_ht_wt_positions,
       width = 6, height = 4, units = "in")

# Your Turn

#display the player names of Lakers 'LAL'
data%>%
filter(team == "LAL") %>%
select(player)

#display the name and salary of GSW point guards ‘PG’.
data%>%
filter(team =="GSW" & position== "PG")%>%
select(player, salary)

#dislay the name, age, and team, of players with more than 10 years of experience, making 10 million dollars or less.
data%>%
filter(experience>10 & salary<= 10000000)%>%
select(player, age, team, salary)

#select the name, team, height, and weight, of rookie players, 20 years old, displaying only the first five occurrences (i.e. rows).
data$min_per_game<- data$minutes/data$games
gsw_mpg<- data%>%
  filter(team=="GSW")%>%
  select(player, experience, min_per_game)%>%
  arrange(desc(min_per_game))

#display the average triple points by team, in ascending order, of the bottom-5 teams (worst 3pointer teams).
data%>%
group_by(team)%>%
select(team, points3)%>%
summarise(avg_points3= mean(points3))%>%
arrange(avg_points3)%>%
slice(1:5)

#obtain the mean and standard deviation of age, for Power Forwards, with 5 and 10 years (including) of experience.

data %>%
  filter(position=="PF" & experience %in% 5:10) %>%
  summarise(mean_age= mean(age), sd_age= sd(age))
  


