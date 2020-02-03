
###################################################
# Biomedical Data Science Day 2020
# Using R to Process and Analyze Accelerometer Data
# Author: David Aaby
# Updated: February 3 2020
###################################################


# NHANES 2003-2004 Physical Activity Monitor
# DESCRIPTION OF VARIABLES
# https://wwwn.cdc.gov/nchs/nhanes/2003-2004/PAXRAW_C.htm

# PAXSTAT: Component status code with PAXSTAT=1 for records with data that are deemed reliable. 
#   A PAXSTAT=2 was used to code records that had some questionable data; 
#   analysts may wish to examine these records more closely.
# 
# PAXDAY: Day of the week; PAXDAY=1 for Sunday, 2 for Monday and so forth.
# 
# PAXN: Sequential observation number in minutes as recorded by the monitor device. 
#   The range begins with minute 1 on Day 1 (PAXN=1) and ends with the last minute of day 7 
#   of monitor wear (PAXN=10080). Each day of wear produces 1440 individual minute records. 
#   The PAXN values for Day 1 range from 1 to 1440; Day 2 PAXN range from 1441-2880, and so forth.
# 
# PAXHOUR-hour of day the intensity data were recorded in military time or 24 hour clock.
# 
# PAXMINUT- minute value associated with a particular hour (PAXHOUR). The minute value shown is 
#   the start of the minute. For example, for the time 1201 hours, the start of the minute occurs 
#   at 1200 hours and PAXMINUT would be 00.
# 
# PAXINTEN is the intensity value recorded by the device. Each minute has an intensity value.
# 
# PAXCAL- Denotes whether the monitor was in calibration when it was returned by the subject. 
#   The data for monitors that were out of calibration (PMACAL=2) may be less reliable.




##################
# load libraries #
##################

# you must first install the package if it is not already installed #
install.packages("accelerometry")

# load package into R #
library(accelerometry)


#########################
# set working directory #
#########################

setwd("workshop_files")


#############
# load data #
#############

# this file contains 100 subjects from the NHANES 2003-2004 Physical Activity Monitor  #
df = read.csv("data/NHANES_accel_100.csv", header=T)


# quick look at data #
head(df)
tail(df)
dim(df)

length(unique(df$seqn))  # check how many subject IDs are in the data



##########################
# create one file per ID #
##########################

# NOTE: it will be easier for us to process the data if every participant has a separate accelerometer file

ids = unique(df$seqn)


# create one file per ID #
for(i in 1:length(ids)) {
  print(c(i,ids[i]))
  oneperid = df[which(df$seqn == ids[i]),]
  write.csv(oneperid, file=paste("data/one_per_id/ID_", oneperid$seqn[1], ".csv", sep=""), row.names = FALSE)
}



##############################################################################################
# Example 1: Process and analyze accelerometer data for one participant - default parameters #
##############################################################################################

# read in data #
unidata = read.csv("data/one_per_id/ID_21005.csv", header=T)
head(unidata)


# plot all counts for one participant #
plot(unidata$paxinten, 
     main=paste("Counts Per Minute for ID ", unidata$seqn[1], sep=""),
     xlab="Index minute",
     ylab="Count per minute (cpm)")

# we could show how often the subject reaches the 2020 cpm threshold for MVPA #
abline(h=2020, col="red", lty=2, lwd=2)

# we could add lines to show where each new day begins #
days = unidata[!duplicated(unidata[, "paxday"], fromLast=TRUE),]
abline(v=days$paxn, col="black", lty=2, lwd=2)



# Using the process_uni() function, process the cpm data for ID 21005
# Keep all other parameters set to the default values


# INSERT CODE HERE #




#############################################################################################
# Example 2: Process and analyze accelerometer data for one participant - custom parameters #
#############################################################################################


# Modify the example code below, changing some of the default parameters to output
  # additional indicators for activity intensities, bouts, sedentary and peak activity
  # output per-day summaries


summary.daily.i = process_uni(counts=unidata$paxinten,
                               steps = NULL,  
                               nci_methods = FALSE,  
                               start_day = unidata$paxday[1],
                               start_date = NULL,  
                               id = unidata$seqn[1],
                               brevity = 1,   
                               valid_days = 1,     
                               valid_wk_days = 0, 
                               valid_we_days = 0, 
                               int_cuts = c(100,760,2020,5999),    
                               cpm_nci = FALSE ,  
                               days_distinct = FALSE, 
                               nonwear_window = 60,  
                               nonwear_tol = 0,  
                               nonwear_tol_upper = 99,
                               nonwear_nci = FALSE, 
                               weartime_minimum = 600,   
                               weartime_maximum = 1440,  
                               active_bout_length = 10, 
                               active_bout_tol = 0,   
                               mvpa_bout_tol_lower = 0,
                               vig_bout_tol_lower = 0, 
                               active_bout_nci = FALSE,  
                               sed_bout_tol = 0, 
                               sed_bout_tol_maximum = 759,  
                               artifact_thresh = 25000,    
                               artifact_action = 1,        
                               weekday_weekend = FALSE,  
                               return_form = "averages")  


summary.daily.i




# what does the output give us?
  # id:	Participant ID number.
  # day:	Day of week (1 = Sunday, ..., 7 = Saturday).
  # valid_day:	1 if day is considered valid for analysis, otherwise 0.
  # valid_min:	Number of minutes classified as valid wear time.
  # counts:	Total counts accumulated during wear time minutes.
  # cpm:	counts/valid_min.
  # sed_min:	Sedentary minutes, or minutes with counts < int_cuts[1].
  # light_min:	Light intensity minutes, or minutes with int.cuts[1] <= counts < int.cuts[2].
  # life_min:	Lifestyle intensity minutes, or minutes with int.cuts[2] <= counts < int.cuts[3].
  # mod_min:	Moderate intensity minutes, or minutes with int.cuts[3] <= counts < int.cuts[4].
  # vig_min:	Vigorous intensity minutes, or minutes with counts >= int.cuts[4].
  # lightlife_min:	Light-to-lifestyle intensity minutes.
  # mvpa_min:	Moderate-to-vigorous intensity minutes.
  # active_min:	Active (i.e. non-sedentary) minutes.
  # sed_percent:	sed_min/valid_min.
  # light_percent:	light_min/valid_min.
  # life_percent:	life_min/valid_min.
  # mod_percent:	mod_min/valid_min.
  # vig_percent:	vig_min/valid_min.
  # lightlife_percent:	lightlife_min/valid_min.
  # mvpa_percent:	mvpa_min/valid_min.
  # active_percent:	active_min/valid_min.
  # sed_counts:	Counts accumulated during sedentary time.
  # light_counts:	Counts accumulated during light intensity activity.
  # life_counts:	Counts accumulated during lifestyle intensity activity.
  # mod_counts:	Counts accumulated during moderate intensity activity.
  # vig_counts:	Counts accumulated during vigorous intensity activity.
  # lightlife_counts:	Counts accumulated during light-to-lifestyle intensity activity.
  # mvpa_counts:	Counts accumulated during MVPA.
  # active_counts:	Counts accumulated during active (i.e. non-sedentary) time.
  # sed_bouted_10min:	Sedentary minutes accumulated in bouts of length = 10 min.
  # sed_bouted_30min:	Sedentary minutes accumulated in bouts of length = 30 min.
  # sed_bouted_60min:	Sedentary minutes accumulated in bouts of length = 60 min.
  # sed_breaks:	Number of times participant transitions from sedentary to non-sedentary minute.
  # max_1min_counts:	Maximum count value.
  # max_5min_counts:	Maximum average count value over a 5-min interval.
  # max_10min_counts:	Maximum average count value over a 10-min interval.
  # max_30min_counts:	Maximum average count value over a 30-min interval.
  # num_mvpa_bouts:	Number of MVPA bouts.
  # num_vig_bouts:	Number of vigorous bouts.
  # mvpa_bouted:	MVPA minutes accumulated in bouts of length = active_bout_length.
  # vig_bouted:	Vigorous intensity minutes accumulated in bouts of length = active_bout_length
  # guideline_min:	Minutes towards physical activity guidelines (150 min/wk of MVPA or 75 min/wk of VPA).



 

# If we are interested in looking at sedentary time and MVPA, we could subset our results to make it
# easier to read
summary.daily.i = data.frame(summary.daily.i)
summary.daily.i[which(summary.daily.i$valid_day==1),
                c("id", "day", "valid_day", "sed_min", "mvpa_min", "sed_percent", "mvpa_percent")]




###############################################################################
# Example 3: Process and analyze accelerometer data for multiple participants #
###############################################################################

accel.filenames = list.files("data/one_per_id")   # creates vector of all files in folder
accel.filenames = paste("data/one_per_id/", accel.filenames, sep="")  # paste filepath onto filename so we can read in each file


# initialze blank object to store results #
summary.daily = NULL


# loop through all files #
for(i in 1:length(accel.filenames)) {

  file = accel.filenames[i]
  
  unidata <- read.csv(file, header=T)  # read in accelerometer data for subject i
  
  print(c(i, unidata$seqn[1]))  # print iteration and ID number so we can see that loop is running
  
  
  # process accelerometer data for subject i #
  summary.daily.i <- process_uni(counts=unidata$paxinten,
                                 steps = NULL,   # default, we are not using steps
                                 nci_methods = FALSE,  # default
                                 start_day = unidata$paxday[1],
                                 start_date = NULL,  # NHANES data example does not have dates
                                 #start.time = strtime,
                                 id = unidata$seqn[1],
                                 brevity = 2,   #default is 1, we want more info
                                 valid_days = 1,     # we will remove subjects with < 4 valid days after we run the function
                                 valid_wk_days = 0, # default
                                 valid_we_days = 0, # default
                                 int_cuts = c(100,760,2020,5999),    # default values
                                 cpm_nci = FALSE ,  # default
                                 days_distinct = FALSE, # default
                                 nonwear_window = 60,   # default
                                 nonwear_tol = 0,  # default is 0
                                 nonwear_tol_upper = 99,
                                 nonwear_nci = FALSE, # default
                                 weartime_minimum = 600,   # default
                                 weartime_maximum = 1440,  # default
                                 active_bout_length = 10,  # default
                                 active_bout_tol = 0,   # default is 0
                                 mvpa_bout_tol_lower = 0, # default
                                 vig_bout_tol_lower = 0, # default
                                 active_bout_nci = FALSE, # default
                                 sed_bout_tol = 0, # default
                                 sed_bout_tol_maximum = 759,  # int.cuts[2] - 1
                                 artifact_thresh = 25000,    # default
                                 artifact_action = 3,        # if 3, replace artifacts with average of neighboring count values
                                 weekday_weekend = FALSE,  # default
                                 return_form = "daily")  # get per-day summary for each day of wear
    
    # convert to data frame #
    summary.daily.i = data.frame(summary.daily.i)
    
    # append results to summary.daily #
    summary.daily = rbind(summary.daily, summary.daily.i)

}


# check results for one ID #
summary.daily[which(summary.daily$id==21005),]
nrow(summary.daily)



##################################################################
# Example 3 continued: Summarize results across all participants #
##################################################################

# Let's compare minutes of MVPA in males and females #


# merge in demographics data #
demo = read.csv(file="data/NHANES_demo_subset.csv", header = TRUE)
summary.daily = merge(summary.daily, demo, by = "id", all.x=TRUE)


# We want to keep only those participants with 4 or more valid days 
# how many participants have at least 4 valid days?

# INSERT CODE HERE #




# keep only the valid days #
sum.daily.4vd = sum.daily.4vd[which(sum.daily.4vd$valid_day==1),]

# calculate average minutes of MVPA per person #
mvpa.avg = aggregate(mvpa_min ~ id, data=sum.daily.4vd, FUN=mean)
mvpa.avg = merge(mvpa.avg, demo, by = "id", all.x=TRUE)

# calculate total minutes of MVPA per person #
mvpa.sum = aggregate(mvpa_min ~ id, data=sum.daily.4vd, FUN=sum)
mvpa.sum = merge(mvpa.sum, demo, by = "id", all.x=TRUE)

# compare means, males and females #
mean(mvpa.avg$mvpa_min[which(mvpa.avg$male==1)])
mean(mvpa.avg$mvpa_min[which(mvpa.avg$male==0)])

# compare totals, males and females #
mean(mvpa.sum$mvpa_min[which(mvpa.sum$male==1)])
mean(mvpa.sum$mvpa_min[which(mvpa.sum$male==0)])



#################################################################
# Example 4: Use custom intensity cutpoints and re-run analysis #
#################################################################

# create new data frame that includes filepaths and cutpoints #
filenames.ex4 = data.frame(id = demo$id,
                             cutpt = demo$cutpt,
                             accel.filenames = accel.filenames,
                             stringsAsFactors = FALSE)


summary.daily.ex4 = NULL

for(i in 1:nrow(filenames.ex4)) {
  
  file = filenames.ex4$accel.filenames[i]
  
  unidata <- read.csv(file, header=T)
  
  mvpa.cutpt = demo$cutpt[i]
  
  print(c(i, unidata$seqn[1]))
  
  
  summary.daily.i <- process_uni(counts=unidata$paxinten,
                                 steps = NULL,   # default, we are not using steps
                                 nci_methods = FALSE,  # default
                                 start_day = unidata$paxday[1],
                                 start_date = NULL,  # NHANES data example does not have dates
                                 #start.time = strtime,
                                 id = unidata$seqn[1],
                                 brevity = 2,   #default is 1, we want more info
                                 valid_days = 1,     # we will remove subjects with < 4 valid days after we run the function
                                 valid_wk_days = 0, # default
                                 valid_we_days = 0, # default
                                 
                                 int_cuts = c(1,2,mvpa.cutpt,16000),    # NEW CUTSTOM CUTPOINTS
                                 cpm_nci = FALSE ,  # default
                                 days_distinct = FALSE, # default
                                 nonwear_window = 60,   # default
                                 nonwear_tol = 2,  # default is 0
                                 nonwear_tol_upper = 99,
                                 nonwear_nci = FALSE, # default
                                 weartime_minimum = 600,   # default
                                 weartime_maximum = 1440,  # default
                                 active_bout_length = 10,  # default
                                 active_bout_tol = 2,   # default is 0
                                 mvpa_bout_tol_lower = 0, # default
                                 vig_bout_tol_lower = 0, # default
                                 active_bout_nci = FALSE, # default
                                 sed_bout_tol = 0, # default
                                 sed_bout_tol_maximum = 759,  # int.cuts[2] - 1
                                 artifact_thresh = 25000,    # default
                                 artifact_action = 3,        # if 3, replace artifacts with average of neighboring count values
                                 weekday_weekend = FALSE,  # default
                                 return_form = "daily")  # get per-day summary for each day of wear
  
  summary.daily.i = data.frame(summary.daily.i)
  
  summary.daily.ex4 = rbind(summary.daily.ex4, summary.daily.i)
  
}

summary.daily[which(summary.daily$id==21006),]
summary.daily.ex4[which(summary.daily.ex4$id==21006),]




summary.daily = merge(summary.daily.ex4, demo, by = "id", all.x=TRUE)

# keep only those IDs with 4 or more valid days #
# INSERT CODE FROM EXAMPLE 3 HERE #



# keep only the valid days #
sum.daily.4vd = sum.daily.4vd[which(sum.daily.4vd$valid_day==1),]

mvpa.avg = aggregate(mvpa_min ~ id, data=sum.daily.4vd, FUN=mean)
mvpa.avg = merge(mvpa.avg, demo, by = "id", all.x=TRUE)

mvpa.sum = aggregate(mvpa_min ~ id, data=sum.daily.4vd, FUN=sum)
mvpa.sum = merge(mvpa.sum, demo, by = "id", all.x=TRUE)

mean(mvpa.avg$mvpa_min[which(mvpa.avg$male==1)])
mean(mvpa.avg$mvpa_min[which(mvpa.avg$male==0)])

mean(mvpa.sum$mvpa_min[which(mvpa.sum$male==1)])
mean(mvpa.sum$mvpa_min[which(mvpa.sum$male==0)])



#############
# Example 5 #  
#############

# Suppose an investigator defines a valid day of accelerometer wear to be at least 8 hours of wear time.
# Modify the code in Example 3 and see how the total sum and average daily minutes for males and females change






# END OF TUTORIAL #

################################################################################################################
################################################################################################################


