#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
#               AUWU Workforce Australia Survey Data Analysis
#                          Theresa O'Brien 2022
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Packages
library(tidyverse)
library(DescTools)
library(ggtext)

#Load in data
data_raw <- read.csv("WfA_Q2Q3Q4Only.csv")
# Data has been pre-processed so that the 'Unsure' 
# response to question 4 is coded as -1.

#3-word titles for statements
titles <- c(Q.1 = "Exclusion from consultation",
            Q.2 = "Government automation fears",
            Q.3 = "Reliable internet access",
            Q.4 = "Lack of communication",
            Q.5 = "New provider inconvenient",
            Q.6 = "Previous activities insufficient",
            Q.7 = "Uncertain target fairness",
            Q.8 = "Digital preference ignored",
            Q.9 = "System induced penalties",
            Q.10 = "Digital support lacking",
            Q.11 = "Missed points debt",
            Q.12 = "Gaining 100 points")
xlabs_strings <- c("Exclusion<br>from<br>consultation",
                   "Government<br>automation<br>fears",
                   "Reliable<br>internet<br>access",
                   "Lack of<br>communication",
                   "New provider<br>inconvenient",
                   "Previous<br>activities<br>insufficient",
                   "Uncertain<br>target<br>fairness",
                   "Digital<br>preference<br>ignored",
                   "System<br>induced<br>penalties",
                   "Digital<br>support<br>lacking",
                   "Missed<br>points debt",
                   "Gaining<br>100 points")

#Data cleaning
#Remove empty responses
data <- data_raw %>% na.omit()

#Construct transformed variables
for(i in 1:nrow(data)){
  #Get number of unsure (recoded as -1) in responses
  data$CountUnsures[i] <- sum(data[i, 15:26] ==-1)
  
  #Get number of 0s in responses
  data$Count0s[i] <- sum(data[i, 15:26] ==0)
  
  #Get number of 3s in responses
  data$Count3s[i] <- sum(data[i, 15:26] ==3)
  
  #Get number of 2s and 3s in responses
  data$Count0and1s[i] <- sum(data[i, 15:26] <=1)
  
  #Get number of 2s and 3s in responses
  data$Count2and3s[i] <- sum(data[i, 15:26] >=2)
  
  #Is there at least one Unsure?
  data$AtLeastOneUnsure[i] <- data$CountUnsures[i] >= 1
  
  #Is there at least one 0?
  data$AtLeastOne0[i] <- data$Count0s[i] >= 1
  
  #Is there at least one 3?
  data$AtLeastOne3[i] <- data$Count3s[i] >= 1
  
  #Is there at least one 0 or 1?
  data$AtLeastOne0or1[i] <-  data$Count0and1s[i] >= 1
  
  #Is there at least one 2 or 3?
  data$AtLeastOne2or3[i] <-  data$Count2and3s[i] >= 1
}

#These lines convert the data for Q4 into a long format
transpose_data <- data.frame(t(data[,15:26]))
transpose_data$Question <- as.factor(c(1:12))
data_long <- pivot_longer(transpose_data, 
                              cols=starts_with("X"), 
                              names_to="response")

#Dataset with unsure in Q4 excluded by replacing them with NA.
data_NAUnsure <- data
for(i in 1:ncol(data_NAUnsure)){
  data_NAUnsure[which(data_NAUnsure[,i] == -1),i] <- NA
}
data_NoUnsure <- data_NAUnsure %>% na.omit()


#~~~~~~~~~~~~~~~~~~~~~~~ Correlations ~~~~~~~~~~~~~~~~~~~~~~~~#
#Calculate the correlation between rank of issue and level of anticipated harm
#These results do not appear in the report and were part of data exploration.
# Uses Kendalls's tau-b as these are ranked data with ties.
corr_vals <- c()
corr_vals_U <- c()
corr_vals_L <- c()
for(i in 1:12){
  result <- KendallTauB(x=data_NoUnsure[,i+2],
                        y=data_NoUnsure[,i+14], conf.level=0.95)
  corr_vals[i] <- result[1]
  corr_vals_U[i] <- result[3]
  corr_vals_L[i] <- result[2]
}
corr_results <- data.frame(Estimate = corr_vals,
                           CI_Lower = corr_vals_L, 
                           CI_Upper = corr_vals_U)


#~~~~~~~~~~~~~~~~~~~~ Headline Statistics ~~~~~~~~~~~~~~~~~~~~#
#Functions
prop_ci <- function(p, n, alpha){
  CI <- c()
  CI[1] <- p - qnorm((1-alpha/2), 0, 1)*sqrt(p*(1-p)/n)
  CI[2] <- p + qnorm((1-alpha/2), 0, 1)*sqrt(p*(1-p)/n)
  return(CI)
}

#Sample stats
n_obs_raw <- nrow(data_raw)
n_obs <- nrow(data)
n_obs_no_unsure <- nrow(data_NoUnsure)

#Summary statistics for Q2
frequencies_Q2 <- table(data_raw$Q2)
percent_Q2 <- 100 * frequencies_Q2/n_obs_raw

#Summary statistics for Q3
mean_ranks <- colMeans(data[,3:14])
sorted_mean_ranks <- data.frame(mean = sort(mean_ranks))

#Means for each group of statements
mean_rank_points_system <- mean_ranks[c(6, 7, 10, 11)]
mean_rank_government_trust <- mean_ranks[c(1, 2, 4)]
mean_rank_placement <- mean_ranks[c(5, 8)]
mean_rank_online_system <- mean_ranks[c(3, 9, 10)]
#Overall means for each group of statements
group_mean_ranks <- c(point_system = mean(mean_rank_points_system),
                 online_system = mean(mean_rank_online_system),
                 government_trust = mean(mean_rank_government_trust),
                 placement = mean(mean_rank_placement))

#Summary statistics for Question 4
percent_filled <- n_obs/n_obs_raw * 100
n_unfilled <- n_obs_raw - n_obs
percent_at_least_one_unsure <- sum(data$AtLeastOneUnsure)/n_obs * 100
percent_at_least_one_0 <- sum(data$AtLeastOne0)/n_obs * 100
percent_at_least_one_3 <- sum(data$AtLeastOne3)/n_obs * 100
percent_at_least_one_0_or_1 <- sum(data$AtLeastOne0or1)/n_obs * 100
percent_at_least_one_2_or_3 <- sum(data$AtLeastOne2or3)/n_obs * 100
mean_number_unsure <- mean(data$CountUnsures)
mean_number_3s <- mean(data$Count3s)
median_number_3s <- median(data$Count3s)
mean_number_2_and_3s <- mean(data$Count2and3s)
median_number_2_and_3s <- median(data$Count2and3s)
mean_impact <- colMeans(data_NAUnsure[,15:26], na.rm=TRUE)
med_impact <- apply(data_NAUnsure[,15:26],2,median, na.rm=TRUE)
automation_fears_23_percent <- (length(which(data$Q4.2 >= 2))/n_obs) * 100
lack_of_communication_23_percent <- (length(which(data$Q4.4 >= 2))/n_obs) * 100
digital_support_lacking_23_percent <- (length(which(data$Q4.10 >= 2))/n_obs) * 100
previous_activities_23_percent <- (length(which(data$Q4.6 >= 2))/n_obs) * 100
target_fairness_23_percent <- (length(which(data$Q4.7 >= 2))/n_obs) * 100 

#CIs
prop_ci(percent_at_least_one_0/100, n_obs, 0.05)
prop_ci(percent_Q2[5]/100, n_obs, 0.05)

#Determine the number and percentage of responses which are Unsure
unsure_count <- c()
for(i in 1:12){
  unsure_count[i] <- sum(is.na(data_NAUnsure[,i+14])) 
}
unsure_percent <- round((unsure_count/n_obs) * 100, digits=2)
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Tables ~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Marginal distribution tables
marginals <- matrix(nrow=12, ncol=13)
# i: index of question
# j: index of rank
# Column 13 is the total for 1:12 in the row to check table orientation
for(i in 1:12){
  for(j in 1:12){
    marginals[i,j]=length(which(data[,i+2]==j))
  }
  marginals[i,13] = sum(marginals[i,1:12])
}
#Table with percentages
marginal_percent <- round((marginals/marginals[1,13]) * 100, digits=1) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Parameters
default_breaks <- seq(from=-0.5, to=12.5, by=1)

#Functions
#Plot nice histogram
plot_hist <- function(hist_data, xlab, main, y_points, y_labs, xlim=NULL,
                      ylim=NULL, axes=TRUE, breaks){
  hist(hist_data, xlab=xlab, ylab="% Responses",
       main=main,
       col="lightblue",
       breaks=breaks,
       freq=FALSE, 
       yaxt="n",
       xlim=xlim,
       ylim=ylim,
       axes=axes)
  axis(2, at=y_points, label=y_labs)
}

#Grid of histograms
#Shift is the required number to get 1+shift as the first column 
hist_grid_plot <- function(data, shift, order_stat, xlab, y_points, breaks,
                           y_labs, xlim=NULL, ylim=NULL, x_ticks, x_labs, titles){
  temp <- unname(order_stat)
  #Adjust ranks of pairs by adding random uniform noise
  fuzzed <- temp
  for(i in 1:length(temp)){ 
    if(length(which(temp == temp[i])) > 1){
      vec <- which(temp == temp[i])
      fuzzed[vec] <-  temp[vec] - runif(length(vec), min=0.00001, max=0.00005)
    } 
  }
  order_stat_ranked <- rank(fuzzed)
  par(mfrow = c(4, 3))
  for(i in 1:12){
    index <- which(order_stat_ranked == i)
    plot_hist(data[,index+shift], xlab=xlab[index], 
              main=titles[index],
              y_points=y_points, 
              y_labs=y_labs,
              xlim=xlim,
              ylim=ylim,
              axes=FALSE,
              breaks=breaks)
    axis(1, at=x_ticks, labels=x_labs)
  }
  par(mfrow = c(1, 1))
}

#Plot stacked bar chart of percentages
plot_stacked_bar_chart <- function(plot_data, title, xlabs, 
                                   xlab="Question Number"){
  ggplot(plot_data, aes(x = Question, fill = as.factor(value))) + 
    geom_bar(position="fill") +
    labs(title=title) +
    xlab(xlab) +
    ylab("Percentage ") + 
    scale_y_continuous(breaks=seq(from=0, to=1, by=0.1),
                       labels=seq(from=0, to=100, by=10)) +
    scale_x_discrete(labels=xlabs) +
    theme_classic() +
    theme(legend.title=element_blank(), axis.text.x=element_markdown()) +
    scale_fill_manual(values=c("#999999", "#009E73", "#56B4E9",
                               "#F0E442", "#D55E00"),
                      breaks=c(-1,0,1,2,3),
                      labels=c("Unsure", "No impact", "Inconvenient",
                               "Some", "Severe"))
}

#Histogram of number of severe concerns
plot_hist(hist_data=data$Count3s, xlab="Number of 3s in Q4, n=351", 
          main="Number of Issues of Severe Concern", 
          y_points=c(0.00, 0.05, 0.10, 0.15), 
          y_labs=c(0, 5, 10, 15), 
          breaks=default_breaks,
          ylim = c(0, 0.15),
          xlim = c(-0.2, 12.2))

#Histogram of number of some or severe concerns
plot_hist(hist_data=data$Count2and3s, xlab="Number of 2s or 3s in Q4, n=351", 
          main="Number of Issues of Some or Severe Concern", 
          y_points=c(0.00, 0.05, 0.10, 0.15, 0.2, 0.25), 
          y_labs=c(0, 5, 10, 15, 20, 25),
          breaks=default_breaks,
          ylim = c(0, 0.25),
          xlim = c(-0.2, 12.2))

#Histogram of number of unsures
plot_hist(hist_data=data$CountUnsures, xlab="Number of Unsure in Q4, n=351", 
          main="Number of Unsure Answers", 
          y_points=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), 
          y_labs=c(0, 10, 20, 30, 40, 50),
          breaks=default_breaks,
          ylim = c(0, 0.5),
          xlim = c(-0.2, 12.2))

#Stacked bar charts of responses to Q4 statements
#All questions together
plot_stacked_bar_chart(data_long, 
                       title="Percentage response for each part of Q4", 
                       xlabs=colnames(data[,15:26]))

#Points and payments
points_data <- data_long %>% filter(Question %in% c(6, 7, 10, 11))
xlabs <- xlabs_strings[c(6, 7, 10, 11)]
plot_stacked_bar_chart(points_data, 
                       title="Percentage Responses for Points and Payments", 
                       xlabs=xlabs, xlab="")

#Trust in government
trust_government_data <- data_long %>% filter(Question %in% c(1, 2, 4))
xlabs <- xlabs_strings[c(1, 2, 4)]
plot_stacked_bar_chart(trust_government_data, 
                       title="Percentage Responses for Trust in Government", 
                       xlabs=xlabs, xlab="")

#Placement Decisions
placement_decision_data <- data_long %>% filter(Question %in% c(5, 8))
xlabs <- xlabs_strings[c(5, 8)]
plot_stacked_bar_chart(placement_decision_data, 
                       title="Percentage Responses for Placement Decisions", 
                       xlabs=xlabs, xlab="")

#Navigating the online system
online_system_data <- data_long %>% filter(Question %in% c(3, 9, 10))
xlabs <- xlabs_strings[c(3,9,10)]
plot_stacked_bar_chart(online_system_data, 
                       title="Percentage Responses for Navigating the Online System", 
                       xlabs=xlabs, xlab="")

#Histograms of ranks for each statement in Q3
hist_grid_plot(data, shift=2, order_stat=mean_ranks, 
               xlab=paste("Priority (mean: ", round(mean_ranks, digits=2), ")", sep=""),
               y_points=c(0.00, 0.20, 0.4), 
               y_labs=c(0, 20, 40),
               ylim=c(0, 0.4), 
               xlim=c(0.8, 12.5),
               x_ticks=c(1, 3, 5, 8, 10, 12), 
               x_labs=c(1, 3, 5, 8, 10, 12),
               breaks=default_breaks,
               titles)

#Histograms of impact for questions in Q4
# Excludes unsure results as these are not used to calculate the mean
# As there are pairs in the mean impacts we add noise to get the order of
# the plots with the same rank of mean.
count_non_NA <- c()
for(i in 1:12){
  count_non_NA[i] <- sum(!is.na(data_NAUnsure[,i+13]))
}
hist_grid_plot(data_NAUnsure, shift=14, order_stat=-med_impact, 
               xlab=paste("Impact (median: ", med_impact,", n=", count_non_NA, ")", sep=""),
               y_points=c(0.00, 0.40, 0.8), 
               y_labs=c(0, 40, 80),
               ylim=c(0, 0.8),
               xlim=c(0.8, 3.5),
               x_ticks=c(0, 1, 2, 3), 
               x_labs=c(0, 1, 2, 3), 
               titles,
               breaks=seq(from=-0.5, to=3.5, by=1))


#~~~~~~~~~~~~~~~~~~~~~~~~~ Inference ~~~~~~~~~~~~~~~~~~~~~~~~~#
#Calculating test statistic for test of equal marginals
# See Anderson 1959 for details.
calc_matrix <- marginals[,1:12]
q_sq_vec <- c()
for(i in 1:12){
  q_sq_vec[i] <- 12 * sum( (calc_matrix[i,]-c(1:12)/12)^2 / n_obs )
}
q_sq <- sum(q_sq_vec)*(12-1)/12 
df <- (12-1)^2 #Degrees of freedom for chi-squared
q_pval <- pchisq(q_sq, df, lower.tail = FALSE) #Calculate p-value

