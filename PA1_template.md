---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

For this assignment I will need to use the package "knitr", as it is required to run the markdown using tools of this package. 
The _knitr_ package is needed to be installed, if it is not already installed, and activated. In addition, I used for this analysis the packages _lattice_, _ggplot2_ and _MASS_. 
For installing and activating these packages, I added to this marksown, the following script (if the packages are not installed in your RStudio, the script will install the packages for you):

```r
requiredPackages <- c('knitr', 'ggplot2', 'MASS', 'lattice');
for(p in requiredPackages){
        if(!require(p,character.only = TRUE)) install.packages(p)
        library(p,character.only = TRUE)
}
```

```
## Loading required package: knitr
```

```
## Loading required package: ggplot2
```

```
## Loading required package: MASS
```

```
## Loading required package: lattice
```


there is a need to set the global setting, according to the assignment, so all chunks will be tags in their default configuration as "echo=TRUE".

```r
opts_chunk$set(echo = TRUE);
```


## Loading and preprocessing the data

I uploaded the data to the RStudio Console, using the following script (other users need to deffine their working directory identicly to the directory where the data file is stored, using the function 'setwd()):

```r
activity <- read.csv(file = "activity.csv");
```
  
### Transfering the data into a format suitable for the analysis  

For understanding the configurations needed for the analysis according to this assignment, I, first, observed the data, for understanding its structure, using the following script:

```r
str(activity);
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

As an outcome of my observation on the data, I saw that the data is build as a data-frame, which contain 3 vectors, as follow:  
1. steps - a numeral field (define correctly)  
2. date - time/date field (not defined correctly)  
3. interval - a numeral field (define correctly)  

Which means, that I needed to correct the definition of the field called "date", to a time/date field. I used the following script for that:

```r
activity$date <- as.Date(activity$date, format = "%Y-%m-%d");
str(activity);
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
  
  
## What is mean total number of steps taken per day?


The assignment requires an analysis the total number of steps taken per day.  
For this analysis, I created an aggregated table, which summarize the total number of steps observed by dates, using the following script:

```r
library(stats);
activity_per_date <- aggregate(steps ~ date, data = activity, sum);
head(activity_per_date);
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

### A histogram of the total steps per day

The assignment requires a production of a histogram of the total number of steps taken each day.  
For creating the histogram, I used the following script:

```r
plot(activity_per_date$date, activity_per_date$steps,
     main = "The total number of steps counted in the data per day", 
     xlab = "Dates", ylab = "Steps", type = "h", lwd = 10, lend=2,
     col = "blue");
rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = rgb(0.211,0.211,0.211,0.1));
```

![](PA1_template_files/figure-html/totaldayhist-1.png)<!-- -->

### The mean and the median of steps per day

The assignment requires a calculation of the mean and the median of the total number of steps measured per day.  
For calculating the mean of steps collected in the data-set, I used the following script: 

```r
n <- length(activity_per_date$steps);
mean_X   <- round(mean(activity_per_date$steps), 2);
std_dev  <- sd(activity_per_date$steps)/sqrt(n);
crit_val <- qt(0.95, df=n);   
margin_of_error <- std_dev * crit_val;
min_mean <- round(mean_X - margin_of_error, 2);
max_mean <- round(mean_X + margin_of_error, 2);
mean_steps <- paste(mean_X, " (", "95% CI [", min_mean, ", ", max_mean, "])", sep = "");
print(paste("The mean of the steps per day recorded is", mean_steps));
```

```
## [1] "The mean of the steps per day recorded is 10766.19 (95% CI [9784.46, 11747.92])"
```
  
And for checking the the median, I used the following script:

```r
median_X   <- median(activity_per_date$steps);
median_X_paste <- paste("The median of the steps per day recorded is", 
                        median_X, sep = " ");
print(median_X_paste);
```

```
## [1] "The median of the steps per day recorded is 10765"
```

The outcome tells us that the mean of the steps per day recorded is10766.19 (95% CI [9784.46, 11747.92]), and The median of the steps per day recorded is 10765.  


## What is the average daily activity pattern?

The assignment requires an analysis of  the average daily activity pattern per observation.   
  
  
### A plot of the avarage steps per obsorvation by days.
  
For this analysis, I created additional aggregated table of the mean steps per date. For that, I used the following script:

```r
library(stats);
avg_steps_per_interval <- aggregate(steps ~ interval, data = activity, mean);
avg_steps_per_interval$steps <- round(avg_steps_per_interval$steps, 1);
head(avg_steps_per_interval);
```

```
##   interval steps
## 1        0   1.7
## 2        5   0.3
## 3       10   0.1
## 4       15   0.2
## 5       20   0.1
## 6       25   2.1
```


Out of this table, I Created a plot representing a time series of the 5-minutes interval (x-axis), and the average number of steps taken across all days (y-axis).  
For creating the histogram, I used the following script:

```r
plot(avg_steps_per_interval$interval, avg_steps_per_interval$steps,
     main = "The average number of steps per interval", 
     xlab = "Interval", ylab = "Steps", type = "l", lwd = 2, lend=2,
     col = "blue");
rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = rgb(0.211,0.211,0.211,0.1));
```

![](PA1_template_files/figure-html/avgplot-1.png)<!-- -->


### The maximum average number of steps per observation

The assignment requires a production of an output, which contains the maximum number of average steps done by 5-minute interval across all the days in the dataset.  

For that, I used the function "which.max()" for locating the date.  

```r
max_avg_steps_per_interval <- avg_steps_per_interval[which.max(avg_steps_per_interval$steps),];
max_avg_steps_per_interval;
```

```
##     interval steps
## 104      835 206.2
```

The output shows that across all the days in the dataset, the day, which showed the highest average steps per day is found on the interval 835. At that day, the average 5-minutes interval was 206.2.


## Imputing missing values

In order to build a strategy for compliting the missing data, I had to learn first:  
- How many observations **should be**?  
- How many observations **there are**?  
- How many observations **are missing**?  
  
According to the results, I could make the right strategy, according to the existing data and what is missing.  

### How many observations should be, there are and missing

I used the following script for calculating the number of NA's in the text:

```r
detailes_NA <- data.frame(n = length(activity$steps), 
                      NAs = sum(is.na(activity$steps)), 
                      prevalance_NA =  sum(is.na(activity$steps))/length(activity$steps));
detailes_NA$prevalance_NA <- paste(round(detailes_NA$prevalance_NA*100, 1), "%", sep = "");
detailes_NA;
```

```
##       n  NAs prevalance_NA
## 1 17568 2304         13.1%
```

The summary above shows that out of a total observations in the data set of 17,568, there are 2304 observations with missing variables at the field of "steps" (13.1% of all the data). Such high rate of NA's can influence the results if the NA's are not distributed equally in the data. 
  
    
### Checking bias influance of NA's by date

The only independent parameter available for checking the influence of the missing data at the field "steps", is "date" (i.e. since the interval is a calculated field of the steps, and therefore dependent on the field "steps" and can not be used). 

Moreover, The field "date" will have a significant role in the next analysis, since the date define, whether the results represents steps done on weekdays or weekends.

First, I wished to understand how many observation are expected to be each day. I made an aggregate table, which summarizes the number of observations per date. for that, I used the following script:

```r
activity_count_dates <- aggregate(interval ~ date, data = activity, length);
head(activity_count_dates);
```

```
##         date interval
## 1 2012-10-01      288
## 2 2012-10-02      288
## 3 2012-10-03      288
## 4 2012-10-04      288
## 5 2012-10-05      288
## 6 2012-10-06      288
```
and than I checked, if the number of observations are identical by all the dates in the data-set. I used the summary function, for testing a variation between the dates:

```r
summary(activity_count_dates$interval);
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     288     288     288     288     288     288
```

The output shows clearly  the minimum and maximum scale regarding the number of observations per day, as well as the mean and median are identical. This means that in all the dates there are an equal number of observations:- 288 observations per day.

Now, I wished to know how many missing variables there are out of the expected 288 observations per day. For that, I created another table, which summarizes the number of missing variables by date.

I defined a separate data-set, which contained only the observations with a missing variables at the "steps" field, using the following script, and added a field for counting the number of observations, for an a following aggregated calculation of missing veriables:

```r
countNA <- subset(activity, is.na(activity$steps));
countNA$weekday <- weekdays(countNA$date);
countNA$missing_veriables <- 1;
head(countNA)
```

```
##   steps       date interval weekday missing_veriables
## 1    NA 2012-10-01        0  Monday                 1
## 2    NA 2012-10-01        5  Monday                 1
## 3    NA 2012-10-01       10  Monday                 1
## 4    NA 2012-10-01       15  Monday                 1
## 5    NA 2012-10-01       20  Monday                 1
## 6    NA 2012-10-01       25  Monday                 1
```
Than I created an aggregated table out of the new data-set, which count the number of missing observations by date, using the following script:

```r
NA_by_date_count <- aggregate(missing_veriables ~ date, data = countNA, length);
NA_by_date_count$existing_variables <- 288 - NA_by_date_count$missing_veriables
NA_by_date_count$weekday <- weekdays(NA_by_date_count$date);
NA_by_date_count;
```

```
##         date missing_veriables existing_variables   weekday
## 1 2012-10-01               288                  0    Monday
## 2 2012-10-08               288                  0    Monday
## 3 2012-11-01               288                  0  Thursday
## 4 2012-11-04               288                  0    Sunday
## 5 2012-11-09               288                  0    Friday
## 6 2012-11-10               288                  0  Saturday
## 7 2012-11-14               288                  0 Wednesday
## 8 2012-11-30               288                  0    Friday
```

The results clear our understanding of the NA's distribution in the data-set.  
1. **Missing some date and not missing some observations in certain dates** - In each date that had a missing variables, the number of NA's were 288. since each day contain 288 observations, and since the number of missing variables is equal to the total number of observations, all the observations at that date ore missing. Since the table do not show any dates which the number of missing variables is lower than 288, we can say that there are'nt any dates which some of the variables are missing.  
2. **The data is complete in dates which contain data** - On the opposite direction, There are no missing data in dates, which contain variables with figures. Therefore, we can use the dates with data as a reliable sources, without a jeopardy of bias out of a missing variables.  
3. **No data collected at the first day of the experiment and the last day** - The data-set represent observation starting the begging of October till the end of November. lines 1 and 8, refer to the first day of October and the last day of November.


### The strategy for filling in all the missing values (the model) 

For reaching a higher reliability of the results, I decided to ignore the first day and the last day of the experiment, since I do not know whether the experiment even was done at the first day of October and the last day of November. Moreover, since these two dates are at the beginning of the period and at the end of the period, ignoring them won't limit my ability to create a time sequence starting the day after 1 October till the day before 30 November.  

I limited the table only to the period at the day after 1 October till the day before 30 November, using the following script:

```r
NA_by_date_count <- NA_by_date_count[c(-1, -8),];
NA_by_date_count;
```

```
##         date missing_veriables existing_variables   weekday
## 2 2012-10-08               288                  0    Monday
## 3 2012-11-01               288                  0  Thursday
## 4 2012-11-04               288                  0    Sunday
## 5 2012-11-09               288                  0    Friday
## 6 2012-11-10               288                  0  Saturday
## 7 2012-11-14               288                  0 Wednesday
```

By dropping the first and the last day of the experiment, I  narrowed the number dates with missing data to 6, each in a different day at the week.

Since there is no missing data at dates which contain data, we can use the statistical characteristics regarding these dates as reference for equivalent dates at the same weekday, which do not contain any data, and produce a simulation of variables for each missing date according to the finds known to us by non-missing dates at the same weekday. 

e.g. We can complete the data for 8 October 2012, according to the mean, standard deviation and the data regression.

for that, I added to the dataset a new field, by using the weekdays() function, which defining the day in the week, which each observation took place.

```r
activity$weekday <- weekdays(activity$date);
head(activity);
```

```
##   steps       date interval weekday
## 1    NA 2012-10-01        0  Monday
## 2    NA 2012-10-01        5  Monday
## 3    NA 2012-10-01       10  Monday
## 4    NA 2012-10-01       15  Monday
## 5    NA 2012-10-01       20  Monday
## 6    NA 2012-10-01       25  Monday
```

For doing so, I made the following script for creating a table, which calculate the following statistical characteristics per weekday:  
- **n_non_act (static):** the number of observations, which the participant' monitoring device counted zero steps during the 5-minutes period of measurement at that day (steps = 0)  
n_act (active): the number of observations, which the participant' monitoring device counted some steps during the 5-minutes period of measurement at that day (steps > 0).  
- **mean_act:** the mean of the steps measured among all the observations at a certain weekday, limited to the active observations (steps > 0).  
- **median_act:** the mean of the steps measured among all the observations at a certain weekday, limited to the active observations (steps > 0).  

```r
activity_woNA <- subset(activity, !is.na(activity$steps));
head(activity_woNA);
```

```
##     steps       date interval weekday
## 289     0 2012-10-02        0 Tuesday
## 290     0 2012-10-02        5 Tuesday
## 291     0 2012-10-02       10 Tuesday
## 292     0 2012-10-02       15 Tuesday
## 293     0 2012-10-02       20 Tuesday
## 294     0 2012-10-02       25 Tuesday
```
And summarize the statistical characteristics of each day in a table 

```r
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
activity_weekday_data[,6:7] <- round(activity_weekday_data[,6:7], 0);
activity_weekday_data;
```

```
##     weekday    n n_non_act n_non_act_ratio n_act mean_act median_act
## 1    Friday 2592      1419           54.7%   597      145         55
## 2    Monday 2592      1485           57.3%   531      131         51
## 3  Saturday 2304      1398           60.7%   618      142         60
## 4    Sunday 2304      1347           58.5%   669      128         60
## 5  Thursday 2592      1786           68.9%   518      127         47
## 6   Tuesday 2592      1925           74.3%   667      121         56
## 7 Wednesday 2592      1654           63.8%   650      145         62
```

The table reviles the following enlightenments:  
1. More than half of the observations marked "0" steps, and should be held as a separate reference group for completing the missing data.  
2. the median of steps each weekday is much lower than the mean, which means that the distribution of the field "steps" is not normal.

For understanding the type of distribution type equivalent to the distribution of the real data each weekday, I created the following plot. I ignored the observations, which no steps were done, because of their extrimely high frequancy:

```r
Non_zero_steps <- subset(activity, steps != 0);
Non_zero_steps$weekday <- weekdays(Non_zero_steps$date);
Non_zero_steps$weekday <- factor(Non_zero_steps$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"));

library(ggplot2)
p <- ggplot(Non_zero_steps, aes(x=steps)) +
        geom_density(aes(color = weekday)) + 
        ggtitle("The distribution of steps by weekdays") + 
        labs(x = "Number of steps at a single observation", 
             y = "Density", 
             color = "The weekday");
p
```

![](PA1_template_files/figure-html/stepsdistribution-1.png)<!-- -->

The result shows clearly that the data distributed in a form of a gamma distribution, therefore, I had to create a simulation suitable to a gamma distribution, identical in mean, median and standard deviation for each weekday.   


### Creating the simulation and compliting the missing data
  
The assignment demanded that the missing data will be added to a new dataset, that is equal to the original dataset but with the missing data. Therefore, I created a duplicate dataset to the original, using the following script:

```r
activity_with_complition <- activity;
```
and since we will have to types of data (real data/dummy), I added additional field which deffine between the two types of data, and marked the existing data as real, using the following script:

```r
activity_with_complition$source[!is.na(activity_with_complition$steps)] <- "real data";
```



I added columns in the summary table of the data, which calculated the expected number of non-zero vs. zero fields need to complete in the missing data per weekday:

```r
activity_weekday_data$to_complete <- 288 * (1-(activity_weekday_data$n_non_act/activity_weekday_data$n));
activity_weekday_data$to_complete <- round(activity_weekday_data$to_complete, 0);

# Fill the 0-steps missing data
activity_weekday_data$to_complete_zero <- 288 * (activity_weekday_data$n_non_act/activity_weekday_data$n);
activity_weekday_data$to_complete_zero <- round(activity_weekday_data$to_complete_zero, 0);
```

By creating these columns, I produced a referrance for the dummy, so its zero/non-zero fields will be the same.

For the actual completion of the data, I used the 'EnvStats' package to calculate the shape and scale of the gamma distribution, as it is found for each weekday, and produced a function, which create a simulation, using the function rgamma() of the 'stats' package:

```r
library(MASS);
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

        # Complete the fields with "0" to the vector
        count_f <- 0
        while (count_f < activity_weekday_data$to_complete_zero[activity_weekday_data$weekday == mon]) {
                dummy_f <- c(dummy_f,0)
                count_f <- count_f+1
        }
        
        # Randomize the dummy
        dummy_f <- sample(dummy_f, replace=FALSE)
        
        return(dummy_f);
}
str(gamma_simulator("Monday"));
```

```
##  num [1:288] 34 0 0 0 6 0 0 200 0 0 ...
```
and complite the missing data for the dates between 2 October 2012 and 29 November 2012

```r
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

countNA2 <- subset(activity_with_complition, is.na(activity_with_complition$steps));
countNA2$missing_veriables <- 1;
NA_by_date_count <- aggregate(missing_veriables ~ date, data = countNA2, length);
NA_by_date_count$existing_variables <- 288 - NA_by_date_count$missing_veriables
NA_by_date_count$weekday <- weekdays(NA_by_date_count$date);
NA_by_date_count;
```

```
##         date missing_veriables existing_variables weekday
## 1 2012-10-01               288                  0  Monday
## 2 2012-11-30               288                  0  Friday
```

Since, it is unclear whether the experiment  held at the first day of October and the last day of November (the first day and the last day), those days were not filled with data. In order not to influance the calculation of the avarage steps per day (mainly the MOE), I delleted these ampty rows, by using the following script:

```r
activity_with_complition <- subset(activity_with_complition, !is.na(activity_with_complition$steps));
head(activity_with_complition);
```

```
##     steps       date interval weekday    source
## 289     0 2012-10-02        0 Tuesday real data
## 290     0 2012-10-02        5 Tuesday real data
## 291     0 2012-10-02       10 Tuesday real data
## 292     0 2012-10-02       15 Tuesday real data
## 293     0 2012-10-02       20 Tuesday real data
## 294     0 2012-10-02       25 Tuesday real data
```



### Creating a histogram of the total number of steps per day

Now, after completing the missing data, we could produce a new histogram, which represent the number of steps per day for a complete sequence of time. I used the following script to do so:

```r
activity_with_complition$source <- factor(activity_with_complition$source);
palette(c("gray","blue"));
plot(activity_with_complition$date, activity_with_complition$steps,
     main = "The total number of steps per day with dummy", 
     xlab = "Dates", ylab = "Steps", ylim = c(0,1100), type = "h", lty = "solid", lwd = 11, lend=2,
     col = activity_with_complition$source);
rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = rgb(0.211,0.211,0.211,0.1));
legend("topright", inset=c(0.05,0.75),  
       legend = levels(activity_with_complition$source),
       col = c("gray","blue"), pch = 15, bty = 'o', xjust = 10,
       border = "black", bg = "white");
```

![](PA1_template_files/figure-html/totaldayhistcomp-1.png)<!-- -->

The histogram shows beautifully, how the dummy completion fit in the whale sequence of the activities during the period of the experiment.   
  
After completing the missing data by simulating a dummy, I calculated  the mean and the median of the total number of steps measured per day, using the following script: 

```r
n_comp <- length(activity_with_complition$steps);
mean_comp   <- round(mean(activity_with_complition$steps, na.rm = TRUE), 2);
std_dev_comp  <- sd(activity_with_complition$steps, na.rm = TRUE)/sqrt(n_comp);
crit_val_comp <- qt(0.95, df=n_comp);   
margin_of_error_comp <- std_dev_comp * crit_val_comp;
min_mean_comp <- round(mean_comp - margin_of_error_comp, 2);
max_mean_comp <- round(mean_comp + margin_of_error_comp, 2);
mean_steps_comp <- paste(mean_comp, " (", "95% CI [", min_mean_comp, ", ", max_mean_comp, "])", sep = "");
print(paste("The mean of the steps per day recorded is", mean_steps_comp));
```

```
## [1] "The mean of the steps per day recorded is 35.35 (95% CI [33.99, 36.71])"
```
  
And for checking the the median, I used the following script:

```r
median_comp   <- median(activity_with_complition$steps);
median_comp_paste <- paste("The median of the steps per day recorded is", 
                        median_comp, sep = " ");
print(median_comp_paste);
```

```
## [1] "The median of the steps per day recorded is 0"
```

The outcome tells us that the mean of the steps per day recorded is35.35 (95% CI [33.99, 36.71]), and The median of the steps per day recorded is 0.  

  
## Are there differences in activity patterns between weekdays and weekends?

Following the assignment demand, I could, now, create a secuance which fully represent the different pattern between weekdays and weekends. for this task, I had to produce additional column, which define the observations done during weekdays (Monday-Friday), and which are done during weekend (Saturday-Sunday). For that, I used the following script:

```r
activity_with_complition$weekend_type <- "weekdays";
activity_with_complition$weekend_type[activity_with_complition$weekday %in% "Saturday"] <- "weekend";
activity_with_complition$weekend_type[activity_with_complition$weekday %in% "Sunday"] <- "weekend";
table(activity_with_complition$weekend_type, activity_with_complition$weekend);
```

```
##           
##            weekdays weekend
##   weekdays    12384       0
##   weekend         0    4608
```

To illustrate the patterns of activity during weekdays in comparison to weekend by a plot, I created a new dataset, which calculate the mean of steps per interval, as a source for the plot, using the following script:

```r
avg_weekday <- aggregate(steps ~ interval + weekend_type, 
                         data = activity_with_complition,
                         mean)
```
and used the following script, for creating the plot:

```r
library(lattice)
xyplot(steps ~ interval|weekend_type, 
       data = avg_weekday, 
       type = "l",
       ylab = "Number of steps",
       layout = c(1,2));
```

![](PA1_template_files/figure-html/weekdayweekendplot-1.png)<!-- -->

From the plot, we can see that the interval above 1,000 is slightly more active during the weekends in comparison to other weekdays. On the other hand, no real deference was found among intervals below 500.
