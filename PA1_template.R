
############################# Facilitating ###############

# set the linguistic enviermnent 
Sys.setlocale(category = "LC_ALL", locale = "english")

# set the working directory
setwd("C:/Users/micke/Documents/John Hopkins/Course 5/Week 2 Marrkdown/Assignment");

# Install the packages needed, if they are not installed
requiredPackages <- c('knitr', 'ggplot2', 'MASS')
for(p in requiredPackages){
        if(!require(p,character.only = TRUE)) install.packages(p)
        library(p,character.only = TRUE)
}
rm(p, requiredPackages);

# Upload the data
activity <- read.csv(file = "activity.csv");

# Observe the data
str(activity);

# Setting the date variable to a date form
activity$date <- as.Date(activity$date, format = "%Y-%m-%d");


############################# Mean and median per date ###############

# creating an aggregated database of steps per date:
library(stats);
activity_per_date <- aggregate(steps ~ date, data = activity, sum);
head(activity_per_date);

# Creating an histogram of the total number of steps per day
plot(activity_per_date$date, activity_per_date$steps,
     main = "The total number of steps counted in the data per day", 
     xlab = "Dates", ylab = "Steps", type = "h", lwd = 10, lend=2,
     col = "blue");



# For calculating the mean steps collected in the data-set:
n <- length(activity_per_date$steps);
mean_X   <- round(mean(activity_per_date$steps), 2);
std_dev  <- sd(activity_per_date$steps)/sqrt(n);
crit_val <- qt(0.95, df=n);   
margin_of_error <- std_dev * crit_val;
min_mean <- round(mean_X - margin_of_error, 2);
max_mean <- round(mean_X + margin_of_error, 2);
mean_steps <- paste(mean_X, " (", "95% CI [", min_mean, ", ", max_mean, "])", sep = "")
print(paste("The mean of the steps per day recorded is", mean_steps));
rm(n, mean_X, std_dev, crit_val, margin_of_error, min_mean, max_mean, mean_steps);

# For checking the the median
median_X   <- median(activity_per_date$steps);
median_X_paste <- paste("The median of the steps per day recorded is", 
                        median_X, sep = " ")
print(median_X_paste);
rm(median_X, median_X_paste, activity_per_date);


############################# Mean and median per observation ###############

# Create a new aggregated table of the average steps per interval
library(stats);
avg_steps_per_interval <- aggregate(steps ~ interval, data = activity, mean);
avg_steps_per_interval$steps <- round(avg_steps_per_interval$steps, 1);
head(avg_steps_per_interval);

# Create a plot of line, representing the the interval of steps each interval
plot(avg_steps_per_interval$interval, avg_steps_per_interval$steps,
     main = "The average number of steps per interval", 
     xlab = "Interval", ylab = "Steps", type = "l", lwd = 2, lend=2,
     col = "blue");
rm(avg_steps_per_interval);

# Create a new aggregated table of the average steps per day
library(stats);
avg_steps_per_date <- aggregate(steps ~ date, data = activity, mean);
avg_steps_per_date$steps <- round(avg_steps_per_date$steps, 1);
head(avg_steps_per_date);


# Finding the observation with the maximum value 
max_avg_steps_per_date <- avg_steps_per_date[which.max(avg_steps_per_date$steps),];
max_avg_steps_per_date
rm(max_avg_steps_per_date, avg_steps_per_date);

####################### Missing data ###############

# checking how many NA's there are
detailes_NA <- data.frame(n = length(activity$steps), 
                      NAs = sum(is.na(activity$steps)), 
                      prevalance_NA =  sum(is.na(activity$steps))/length(activity$steps));
detailes_NA$prevalance_NA <- paste(round(detailes_NA$prevalance_NA*100, 1), "%", sep = "");
rm(detailes_NA);

# check the number of observations per date
activity_count_dates <- aggregate(interval ~ date, data = activity, length);
head(activity_count_dates);
summary(activity_count_dates$interval);
rm(activity_count_dates);

# Count the NA's by days
countNA <- subset(activity, is.na(activity$steps));
countNA$weekday <- weekdays(countNA$date);
countNA$missing_veriables <- 1;
NA_by_date_count <- aggregate(missing_veriables ~ date, data = countNA, length);
NA_by_date_count$existing_variables <- 288 - NA_by_date_count$missing_veriables
NA_by_date_count$weekday <- weekdays(NA_by_date_count$date);
NA_by_date_count;
rm(countNA);

NA_by_date_count <- NA_by_date_count[c(-1, -8),];
NA_by_date_count;


# creating a new data.frame by days
activity$weekday <- weekdays(activity$date);
activity_woNA <- subset(activity, !is.na(activity$steps));
total_obs <- aggregate(interval ~ weekday, data = activity, length);
names(total_obs) <- c("weekday", "n");
no_steps_by_weekday_count <- aggregate(steps ~ weekday, data = subset(activity_woNA, steps == 0), length);
names(no_steps_by_weekday_count) <- c("weekday", "n_non_act");
active_steps_by_weekday_count <- aggregate(steps ~ weekday, data = subset(activity_woNA, steps != 0), length);
names(active_steps_by_weekday_count) <- c("weekday", "n_act");
steps_by_weekday_mean <- aggregate(steps ~ weekday, data = subset(activity_woNA, steps != 0), mean);
names(steps_by_weekday_mean) <- c("weekday", "mean_act");
steps_by_weekday_median <- aggregate(steps ~ weekday, data = subset(activity_woNA, steps != 0), median);
names(steps_by_weekday_median) <- c("weekday", "median_act");

# merge the separate aggregated data-frames to one data-frame
activity_weekday_data <- merge(total_obs, no_steps_by_weekday_count, 
                               by.x = "weekday");
activity_weekday_data$n_non_act_ratio <- paste(
        round(activity_weekday_data$n_non_act/ activity_weekday_data$n*100, 1),
        "%", sep = "");
activity_weekday_data <- merge(activity_weekday_data, active_steps_by_weekday_count, 
                               by.x = "weekday");
activity_weekday_data <- merge(activity_weekday_data, steps_by_weekday_mean, 
                               by.x = "weekday");
activity_weekday_data <- merge(activity_weekday_data, steps_by_weekday_median, 
                               by.x = "weekday");
activity_weekday_data[,6:7] <- round(activity_weekday_data[,6:7], 0)
rm(total_obs, steps_by_weekday_mean, active_steps_by_weekday_count, 
   steps_by_weekday_median, no_steps_by_weekday_count, activity_woNA);


# Observing the distribution
library(ggplot2);
Non_zero_steps <- subset(activity, steps != 0);
Non_zero_steps$weekday <- weekdays(Non_zero_steps$date);
p <- ggplot(Non_zero_steps, aes(x=steps)) +
        geom_density(aes(color = weekday)) + 
        ggtitle("The distribution of steps by weekdays") + 
        labs(x = "Number of steps at a single observation", 
             y = "Density", 
             color = "The weekday");
p;
rm(p);


# Create a dataset identical to the original with NA's
activity_with_complition <- activity

# add a column which define which data is real and which is dummy
activity_with_complition$source[!is.na(activity_with_complition$steps)] <- "real data"

# calculate how many to simulate which are non 0
activity_weekday_data$to_complete <- 288 * (1-(activity_weekday_data$n_non_act/activity_weekday_data$n));
activity_weekday_data$to_complete <- round(activity_weekday_data$to_complete, 0)

# Fill the 0-steps missing data
activity_weekday_data$to_complete_zero <- 288 * (activity_weekday_data$n_non_act/activity_weekday_data$n)
activity_weekday_data$to_complete_zero <- round(activity_weekday_data$to_complete_zero, 0)


# Creating a Gamma Simulation
gamma_simulator <- function(mon){
        only_month <- Non_zero_steps$steps[Non_zero_steps$weekday == mon]
        only_month_comp <- activity_weekday_data$to_complete[activity_weekday_data$weekday == mon]
        day_egamma <- fitdistr(only_month, densfun = "gamma"); 
        shape <- as.numeric(day_egamma[[1]][1])/3
        rate <- as.numeric(day_egamma[[1]][2])
        
        dummy_f <- rgamma(
                n = only_month_comp, 
                shape = shape, 
                rate = rate);
        dummy_f <- round(dummy_f, 0);
        summary(dummy_f)
        
        # Complete the fields with "0" to the vector
        count_f <- 0
        while(count_f < activity_weekday_data$to_complete_zero[activity_weekday_data$weekday == mon]) {
                dummy_f <- c(dummy_f,0)
                count_f <- count_f+1
        }
        
        # Randomize the dummy
        dummy_f <- sample(dummy_f, replace=FALSE)
        
        return(dummy_f);
}

# add the dummy to the data
activity_with_complition$steps[activity_with_complition$date == NA_by_date_count$date[NA_by_date_count$weekday == "Monday"]] <- gamma_simulator("Monday");
activity_with_complition$source[activity_with_complition$date == NA_by_date_count$date[NA_by_date_count$weekday == "Monday"]] <- "dummy";
activity_with_complition$steps[activity_with_complition$date == NA_by_date_count$date[NA_by_date_count$weekday == "Wednesday"]] <- gamma_simulator("Wednesday");
activity_with_complition$source[activity_with_complition$date == NA_by_date_count$date[NA_by_date_count$weekday == "Wednesday"]] <- "dummy";
activity_with_complition$steps[activity_with_complition$date == NA_by_date_count$date[NA_by_date_count$weekday == "Thursday"]] <- gamma_simulator("Thursday");
activity_with_complition$source[activity_with_complition$date == NA_by_date_count$date[NA_by_date_count$weekday == "Thursday"]] <- "dummy";
activity_with_complition$steps[activity_with_complition$date == NA_by_date_count$date[NA_by_date_count$weekday == "Friday"]] <- gamma_simulator("Friday");
activity_with_complition$source[activity_with_complition$date == NA_by_date_count$date[NA_by_date_count$weekday == "Friday"]] <- "dummy";
activity_with_complition$steps[activity_with_complition$date == NA_by_date_count$date[NA_by_date_count$weekday == "Saturday"]] <- gamma_simulator("Saturday");
activity_with_complition$source[activity_with_complition$date == NA_by_date_count$date[NA_by_date_count$weekday == "Saturday"]] <- "dummy";
activity_with_complition$steps[activity_with_complition$date == NA_by_date_count$date[NA_by_date_count$weekday == "Sunday"]] <- gamma_simulator("Sunday");
activity_with_complition$source[activity_with_complition$date == NA_by_date_count$date[NA_by_date_count$weekday == "Sunday"]] <- "dummy";
rm(NA_by_date_count, Non_zero_steps, gamma_simulator);

# histogram plot after data completion
activity_with_complition$source <- factor(activity_with_complition$source);
levels(activity_with_complition$source)
palette(c("gray","blue"));
plot(activity_with_complition$date, activity_with_complition$steps,
     main = "The total number of steps counted in the data per day", 
     xlab = "Dates", ylab = "Steps", ylim = c(0,900), type = "h", lty = "solid", lwd = 11, lend=2,
     col = activity_with_complition$source);
rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = rgb(0.211,0.211,0.211,0.1));
legend("topright", inset=c(0.05,0.75),  
       legend = levels(activity_with_complition$source),
       col = c("blue","gray"), pch = 15, bty = 'o', xjust = 10,
       border = "black", bg = "white")


# The mean of steps per day with dummy
n_comp <- length(activity_with_complition$steps);
mean_comp   <- round(mean(activity_with_complition$steps, na.rm = TRUE), 2);
std_dev_comp  <- sd(activity_with_complition$steps, na.rm = TRUE)/sqrt(n_comp);
crit_val_comp <- qt(0.95, df=n_comp);   
margin_of_error_comp <- std_dev_comp * crit_val_comp;
min_mean_comp <- round(mean_comp - margin_of_error_comp, 2);
max_mean_comp <- round(mean_comp + margin_of_error_comp, 2);
mean_steps_comp <- paste(mean_comp, " (", "95% CI [", min_mean_comp, ", ", max_mean_comp, "])", sep = "");
print(paste("The mean of the steps per day recorded is", mean_steps_comp));

# The median of steps per day with dummy
median_comp   <- median(activity_with_complition$steps);
median_comp_paste <- paste("The median of the steps per day recorded is", 
                           median_comp, sep = " ");
print(median_comp_paste);
rm(n_comp, mean_comp, std_dev_comp, crit_val_comp, 
   margin_of_error_comp, min_mean_comp, max_mean_comp, 
   mean_steps_comp, median_comp, median_comp_paste);

# define observations during weekdays, and weekend
activity_with_complition$weekend_type <- "weekdays";
activity_with_complition$weekend_type[activity_with_complition$weekday %in% "Saturday"] <- "weekend";
activity_with_complition$weekend_type[activity_with_complition$weekday %in% "Sunday"] <- "weekend";
activity_with_complition$interval

# a comparison plotbetween weekdays, and weekend
activity_with_complition$interval <- as.numeric(activity_with_complition$interval);
avg_weekday <- aggregate(steps ~ interval + weekend_type, 
                         data = activity_with_complition,
                         mean)
ggplot(activity_with_complition, aes(x = interval, y = steps)) + 
        geom_line() +
        facet_wrap(~weekend_type, nrow = 2);


library(lattice)
xyplot(steps ~ interval|weekend_type, 
       data = avg_steps_weekday_type, 
       type = "l",
       ylab = "Number of steps",
       layout = c(1,2));

knit2html("PA1_template.Rmd");
