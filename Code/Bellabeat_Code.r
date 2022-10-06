# Required libraries
library(tidyverse) ; library(e1071) ; library(ggthemes) ; library(sjPlot);
library(reshape2)

# Reading data
setwd('~/Desktop/Universidad/Otros/Courses/Google Data Analytics/Project/Data')
activity <- read.csv('dailyActivity_merged.csv', header=T, sep=',', dec='.')
sleep <- read.csv('sleepDay_merged.csv', header=T, sep=',', dec='.')
weight <- read.csv('weightLogInfo_merged.csv', header=T, sep=',', dec='.')
threshold_query <- read.csv('SQL_query_summary.csv', header=T, sep=',', dec='.')
sql_query_user_types <- read.csv('SQL_Query_1.csv', header=T, sep=',', dec='.')
query3 <- read.csv('SQL_Query_3.csv', header=T, sep=',', dec='.')
query5 <- read.csv('SQL_Query_5.csv')
query6 <- read.csv('SQL_Query_6.csv', header=T, sep=',', dec='.')
boxplot_data <- read.csv("SQL_Query_8.csv", header = T, sep = ",", dec = ".")
relations <- read.csv("relations.csv", header = T, sep = ",", dec = ".")
regression <- read.csv("regression.csv", header = T, sep = ",", dec = ".")

# Unique number of runners
length(unique(activity$Id))

# Unique number of users reporting sleeping hours
length(unique(sleep$Id))

# Unique number of users reporting exercise activity
length(unique(activity$ActivityDate))

# Retrieving activity days per user
days_per_user <- activity %>% 
    select(ActivityDate) %>% 
    group_by(activity$Id) %>% 
    summarize(days_per_user=length(unique(ActivityDate)))
   
as.vector(as.matrix(days_per_user)[, 2])

# Average activity days
round(mean(as.vector(as.matrix(days_per_user)[, 2])), 2)

# Categorizing runners according to their activity
matrix_query <- as.matrix(threshold_query)
colnames(matrix_query) <- c('Average best runner', 'Minimum average distance', 'Average Distance of All Users')

# Setting up the data frame I will work with
average_by_user_data <- activity %>% 
    select(Id, TotalDistance) %>% 
    group_by(Id) %>% 
    summarize(avgerage_distance=mean(TotalDistance)) %>% 
    arrange(-avgerage_distance)

# Transforming the data frame above into a matrix so I can extract the average distance column
average_by_user_data <- as.matrix(average_by_user_data)
average_by_user <- as.vector(average_by_user_data[, 2])

# Algorithm to categorize users based on the metrics threshold rule
categorize_users <- function(x) {
    i <- 0 
    runner_type <- c(1:length(x))
    while (i < length(x)) {
        i <- i + 1
        if (x[i] < 5) {
            runner_type[i] <- 'Beginner Level'
        } else if (x[i] >= 5 & x[i] < 8) {
            runner_type[i] <- 'Intermediate Level'
        } else {
            runner_type[i] <- 'Pro Level'
        }
    }
    return(runner_type)
}

# Adding a column with the user types
average_by_user_data <- cbind(average_by_user_data, categorize_users(average_by_user))
colnames(average_by_user_data) <- c('Id', 'average_distance', 'user_type')

# Removing Id's for privacy
average_by_user_data <- average_by_user_data[, colnames(average_by_user_data) != 'Id']

# Showing average distance and user type per runner
average_by_user_data

# User counting algorithm
count_user_types <- function(x) {
    i <- 0 ; intermediate <- 0
    beginner <- 0 ; pro <- 0
    while (i < length(x)) {
        i <- i + 1
        if (x[i] < 5) {
            beginner <- beginner + 1
        } else if (x[i] >= 5 & x[i] < 8) {
            intermediate <- intermediate + 1
        } else {
            pro <- pro + 1
        }
    }
    return(paste('Beginners: ', beginner, ', Intermediate: ', intermediate, ', Pro users: ', pro))
}

# Group composition
count_user_types(as.numeric(average_by_user_data[,1]))

# Setting up the pie chart with categories and number of users by type
user_types <- c('Pro Level', 'Intermediate Level', 'Beginner Level')

group_composition <- c(count_user_types(as.numeric(average_by_user_data[,1]))[3], 
           count_user_types(as.numeric(average_by_user_data[,1]))[2], 
           count_user_types(as.numeric(average_by_user_data[,1]))[1])

# Creating a data frame with the information above
datafr <- data.frame(user_types, group_composition)

# Calculating the relevant values that will make up the plot
data <- datafr %>% 
    arrange(desc(user_types)) %>% 
    mutate(prop=group_composition / sum(group_composition) * 100) %>% 
    mutate(ypos=cumsum(prop) - 0.5 * prop)

# Plot runner type pie chart
pie_chart <- ggplot(data=data, aes(x=2, y=prop, fill=user_types)) +
                    geom_bar(stat='identity', color='white') +
                    coord_polar('y', start=0) + theme_void() + 
                    geom_text(aes(y=ypos, label=round(prop, 2)), color='white', size=6) +
                    scale_fill_brewer(palette='Set1') +
                    labs(title='User type Pie Chart', subtitle='Expressed as percentage') + xlim(0.5, 2.5)

# Plot activity distribution plot
distribution <- ggplot(data=query3, aes(x=totaldistance, color=user_type, fill=user_type)) +
                      geom_histogram(aes(y=..density..), position='identity', alpha=0.5) + 
                      geom_density(alpha=0.6) +
                      geom_vline(data=query3, aes(xintercept=mean(totaldistance), color=user_type), linetype='dashed') +
                      scale_color_manual(values=c('#999999', '#E69F00', '#56B4E9')) +
                      scale_fill_manual(values=c('#999999', '#E69F00', '#56B4E9')) +
                      labs(title='Running Distance Distribution', x='Distance (km)', y='Density') + theme_economist() +
                      annotate('text', x=10, y=0.21, label='Average: 5.49 km', color='#FE7F9C', fontface='bold', size=4.5) +
                      theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, vjust=-2))

# Probability is calculated as the number of times a user ran less than 5.49 km with respect to its total runs
prob_pro <- length(query3$totaldistance[query3$user_type == 'Pro level' & query3$totaldistance < 5.49])/length(query3$totaldistance[query3$user_type == 'Pro level'])
prob_mid <- length(query3$totaldistance[query3$user_type == 'Intermediate Level' & query3$totaldistance < 5.49])/length(query3$totaldistance[query3$user_type == 'Intermediate Level'])
prob_noob <- length(query3$totaldistance[query3$user_type == 'Beginner Level' & query3$totaldistance < 5.49])/length(query3$totaldistance[query3$user_type == 'Beginner Level'])
round(c(prob_noob, prob_mid, prob_pro), 2)

# Scatter plot per user type
sedentary <- ggplot(data=query5, aes(y=calories, x=sedentary_minutes, color=user_type)) + geom_point() +
                    theme_economist() + geom_smooth(method='lm') +
                    labs(title='Calories vs Sedentary Intensity', x='Sedentary Minutes', y='Calories') +
                    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, vjust=-2))

fair <- ggplot(data=query5, aes(y=calories, x=fairly_active_minutes, color=user_type)) + geom_point() +
              theme_economist() + geom_smooth(method='lm') +
              labs(title='Calories vs Fair Intensity', x='Fairly Active Minutes', y='Calories') +
              theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, vjust=-2))

light <- ggplot(data=query5, aes(y=calories, x=lightly_minutes, color=user_type)) + geom_point() +
                theme_economist() + geom_smooth(method='lm') +
                labs(title='Calories vs Light Intensity', x='Lightly Active Minutes', y='Calories') +
                theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, vjust=-2))

active <- ggplot(data=query5, aes(y=calories, x=very_active_minutes, color=user_type)) + geom_point() +
                theme_economist() + geom_smooth(method='lm') +
                labs(title='Calories vs Active Intensity', x='Very Active Minutes', y='Calories') +
                theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, vjust=-2))

# Changing the data format to 'long'
means.long <- melt(query6, id.vars='user_type')

# Setting up our visualization
intensity_by_type <- ggplot(means.long, aes(x=variable, y=value, fill=user_type)) +
                          geom_bar(stat='identity',position='dodge') +
                          scale_fill_discrete(name='User Type',
                          labels=c('Beginner', 'Intermediate', 'Pro')) +
                          scale_x_discrete(labels=c('Sedentary','Fair','Light', 'Active')) +
                          xlab('Intensity') + ylab('Number of minutes') + theme_economist() +
                          labs(title='Average Intensity Minutes') +
                          theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, vjust=-2))

# Converting strings into factors so that we can manually order days
boxplot_data$dates <- factor(boxplot_data$dates, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

# Boxplot 
boxplot <- ggplot(data = boxplot_data, aes(x = dates, y = bed_hours, fill= dates)) + 
                 geom_boxplot(alpha = 0.8) +
                 guides(fill=FALSE, color = FALSE) +
                 theme(legend.position="none") + theme_economist() +
                 labs(title = "Sleep hours", x = "", y = "Hours") +
                 theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, vjust = -2))

# Visualizing a violin plot
violin_plot <- ggplot(data = relations, aes(x = distance, y = sleep_hours, fill = user_type)) + 
                      geom_violin() + theme_economist() +
                      scale_fill_manual(values = c("#FE7F9C" , "#AF69EF" , "#A1045A")) +
                      scale_color_manual(values = c("#FE7F9C" , "#AF69EF" , "#A1045A")) +
                      labs(title = "Sleep hours vs Total Distance", x = "Distance (kms)", y = "Hours") +
                      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, vjust = -2))

# Building the model
model <- lm(formula = calories ~ pro + intermediate + veryactiveminutes + lightlyactiveminutes + sedentaryminutes + totaltimeinbed, data = regression)

# Getting Results
model_summary <- tab_model(model)