library(data.table)
library(tidyverse)
library(knitr)
library(ggplot2)
library(zoo)

#initializing working directory
setwd("/Users/super/Downloads/datasets/datasets")

# Data Loading ------------------------------------------------------------


#Reading in important datasets
Jan_2013 <- fread("monthly_data/2013_1.csv")
Dec_2019<-fread("monthly_data/2019_12.csv")
aggregate_firm_sales <- fread('aggregate_firm_sales.csv')
firm_info  <- fread('firm_information.csv')

# Read in all monthly_data CSV files
files <- list.files(
  path = "monthly_data",
  pattern = "\\.csv$",
  full.names = TRUE)
#order numerically, not alphabetically
files <- files[order(as.numeric(gsub("\\D", "", basename(files))))]
files

# Read in each file and make dates consistent 
combined_data <- rbindlist(lapply(files, function(file) {
  
  # Extract year and month from file name 
  parts <- strsplit(basename(file), "_|\\.")[[1]]
  year <- as.integer(parts[1])
  month <- as.integer(parts[2])
  
  # Make date = first of the month
  #date_fixed <- as.Date(sprintf("%04d-%02d-01", year, month))
  
  # Read CSV
  dt <- fread(file)
  
  # add date column
  #dt[, date := date_fixed]
  dt$date_fixed <- as.Date(sprintf("%04d-%02d-01", year, month))
  
  return(dt)
}))

# Read and combine all CSVs into one data.table
#combined_data <- rbindlist(lapply(files, fread), fill = TRUE)

# Checking the combined data set
dim(combined_data)


# Data Cleaning -----------------------------------------------------------


#We need a list of adopters, but there are some changes in adopters over time.
#we first create a list of adopters in Jan 2013 (beginning of the program)
# Filter rows where adopt_t == 1
adopters_0113 <- Jan_2013[adopt_t == 1]
dim(adopters_0113)#174 adopters
#we then create a list of adopters at the end of the experiment (Dec 2019)
adopters_1219 <- Dec_2019[adopt_t == 1]
dim(adopters_1219) #172 adopters

#We need to see what changed between those years
# firm_ids in adopters_0113 but not in adopters_1219
diff_13_not_19 <- setdiff(adopters_0113$firm_id, adopters_1219$firm_id)
length(diff_13_not_19) #32 differences
diff_13_not_19

# firm_ids in adopters_1219 but not in adopters_0113
diff_19_not_13 <- setdiff(adopters_1219$firm_id, adopters_0113$firm_id)
length(diff_19_not_13)#30 differences
diff_19_not_13

#looks like the main differences are in the inclusion of '00' at the end of the firm_id. I will assume this is a reporting error, and clean the data so that firm_ids with and without the 00 will be counted as the same firm. 
adopters_0113[, firm_id_clean := sub("00$", "", as.character(firm_id))]
adopters_1219[, firm_id_clean := sub("00$", "", as.character(firm_id))]
# Now compare using the cleaned IDs
diff_1_not_2 <- setdiff(adopters_0113$firm_id_clean, adopters_1219$firm_id_clean)
diff_2_not_1 <- setdiff(adopters_1219$firm_id_clean, adopters_0113$firm_id_clean)

diff_1_not_2 #there are 9 differences not attributed to the '00' issue
#"CXMB-63" "DVKY-33" "ETME-37" "HMPF-83" "PYLZ-99" "QIXA-60" "TDWL-27" "VETZ-83" "XIOT-55" all may have dis-adopted between 2013 and 2019. I might consider these as Outliers. 

diff_2_not_1 #there are an additional 7 differences 
#"AJMB-61" "EZIB-95" "FRJA-10" "HTAK-79" "PDDA-53" "RRNY-47" "ZPJS-12" have adopted after Jan 2013.

#However, it seems as if different companies are not reporting for certain months, but are actually still part of the program. Therefore I find the number of UNIQUE adopters in the combined data.
combined_data[, firm_id_clean := sub("00$", "", as.character(firm_id))]

combined_adopters<- combined_data[adopt_t == 1]

num_unique_adopters <- uniqueN(combined_adopters$firm_id_clean)
num_unique_adopters #number of unique adopters, removing any duplicates is 181

adopters_list<- unique(combined_adopters$firm_id_clean)
head(combined_adopters)

#Average Revenue for all firms
avg_revenue_all <- combined_data[, .(mean_revenue = mean(revenue_t, na.rm = TRUE)), by = date_fixed]
avg_revenue_all
#Average Revenue for only qualifying firms
avg_revenue_q <- qualified_data[, .(mean_revenue = mean(revenue_t, na.rm = TRUE)), by = date_fixed]
avg_revenue_all[, Group := "All Firms"]
avg_revenue_q[, Group := "Qualified Firms"]

# one dataset
avg_revenue_both <- rbind(avg_revenue_all, avg_revenue_q)

# Plot both lines
ggplot(avg_revenue_both, aes(x = date_fixed, y = mean_revenue, color = Group)) +
  geom_line(size = 1.2) +
  labs(
    title = "Average Revenue Over Time",
    x = "Date",
    y = "Average Revenue",
    color = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom"
  )
  


#To find the number of unique qualified non-adopters, let us first remove all unqualified non-adopters from the list.
num_unique_firms <- uniqueN(combined_data$firm_id_clean)
num_unique_firms #number of unique companies is 498

unqualified_firms<- combined_data[employment_t>100]
num_unqualified_firms<-uniqueN(unqualified_firms$firm_id_clean)
num_unqualified_firms #there are 293 unqualified companies

Jan_2013[, firm_id_clean := sub("00$", "", as.character(firm_id))]
qualified_firms<- Jan_2013[employment_t<=100]
num_qualified_firms<-uniqueN(qualified_firms$firm_id_clean)
#qualified_companies<- Jan_2013[employment_t<=100]
num_qualified_firms #there are 243 qualified companies at Jan 2013, but, adding in unqualified companies, there are more than 498 companies. That means that there are some companies that may have become unqualified at different points in the 10 year period. Because the program started January 2013, we will only remove the companies that had greater than 100 employees in that month. 

qualified_data<-combined_data[firm_id_clean %in% qualified_firms$firm_id_clean]

#Then we must alter the adopt_t column of the data before 2013 January to take into account whether the firm ever adopted the program.

qualified_data[firm_id_clean %in% combined_adopters$firm_id_clean & date_fixed < as.Date("2013-01-01"),
              adopt_t := 1]
head(qualified_data)

#Another important part of data cleaning is considering what to do with NaN values
#there are 559 rows containing missing values. Each column has NaN values, but missing values should be handled differently in different columns. 
#missing values in employment_t and wage_bill_t cannot be replaced by neighbors, or interpolated because they are discrete.missing values in Revenue_t also cannot by replaced, but because we will be taking the mean of our variable for diff in diff
sum(is.na(qualified_data$revenue_t)) 
qualified_data[, revenue_t := na.locf(revenue_t, na.rm = FALSE)]
qualified_data[, revenue_t := na.locf(revenue_t, fromLast = TRUE, na.rm = FALSE)]

#Finally, get rid of any last rows with Nans
qualified_clean <- qualified_data %>%
  na.omit()

sum(is.na(qualified_data$date)) #0 na in date column
combined_adopters[employment_t=='NaN']


# Empirical Strategies ----------------------------------------------------


#The first step to understand the data and begin to understand causality, is a simple Difference-in-Differences assessment on the before data (Dec 2012), and the final end data (Dec 2019).The only companies that we care about are those that qualify, ie that have less than or equal to 100 employees in Dec 2012.
# Calculate pre and post means for each group
qualified_data[, post := ifelse(date_fixed >= as.Date("2013-01-01"), 1, 0)]
#sum(is.na(qualified_data$post))
head(qualified_data$date_fixed,60)
qualified_data$post

summary_data <- qualified_data %>%
  group_by(adopt_t, post) %>%
  summarise(mean_outcome = mean(revenue_t))
summary_data$post
summary_data

# Calculate DiD estimate
did_estimate <- (summary_data$mean_outcome[summary_data$adopt_t == 1 & summary_data$post == 1] -
                   summary_data$mean_outcome[summary_data$adopt_t == 0 & summary_data$post == 1]) -
  (summary_data$mean_outcome[summary_data$adopt_t == 1 & summary_data$post == 0] -
     summary_data$mean_outcome[summary_data$adopt_t == 0 & summary_data$post == 0])

did_estimate #117936

#Robustness Check--- 
dim(Jan_2013[adopt_t==1]) #174, 181 total, 
summary_data$Group <- ifelse(summary_data$adopt_t == 1, "Adopted", "Control")
#summary_data$Period <- ifelse(summary_data$post == 0, "After","Before")
summary_data$Period <- factor(
  ifelse(summary_data$post == 1, "Post", "Pre"),
  levels = c("Pre", "Post")  # desired order
)
# Plot
ggplot(summary_data, aes(x = Period, y = mean_outcome, group = Group, color = Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Difference-in-Differences Simple Graph",
    x = "Period",
    y = "Mean Revenue",
    color = "Group"
  ) +
  theme_minimal(base_size = 14)+
theme(
  plot.title = element_text(size = 12) 
)

#However, this DD brings up many confounding variables. The first is what if there are differences between the three types of industry--- Hospitality, Fast Food, and Retail---
firm_info[, .N, by = firm_sector] #shows that there are 149 firms in Hospitality, 140 in Fast Food, and 209 in Retail. 

merged_df_1_2 <- merge(firm_info, aggregate_firm_sales, by = "firm_id", all = TRUE)
data_finfo<-merge(firm_info,qualified_data,by="firm_id", all=TRUE)


#data for hospitality
data_h<-data_finfo[firm_sector=='Hospitality']
summary_data <- data_h %>%
  group_by(adopt_t, post) %>%
  summarise(mean_outcome = mean(revenue_t))
summary_data$post
summary_data

did_estimate_h <- (summary_data$mean_outcome[summary_data$adopt_t == 1 & summary_data$post == 1] -
                   summary_data$mean_outcome[summary_data$adopt_t == 0 & summary_data$post == 1]) -
  (summary_data$mean_outcome[summary_data$adopt_t == 1 & summary_data$post == 0] -
     summary_data$mean_outcome[summary_data$adopt_t == 0 & summary_data$post == 0])

did_estimate_h

#data for Fast Food
data_f<-data_finfo[firm_sector=='Fast Food']
summary_data <- data_f %>%
  group_by(adopt_t, post) %>%
  summarise(mean_outcome = mean(revenue_t))
summary_data$post
summary_data

did_estimate_f <- (summary_data$mean_outcome[summary_data$adopt_t == 1 & summary_data$post == 1] -
                     summary_data$mean_outcome[summary_data$adopt_t == 0 & summary_data$post == 1]) -
  (summary_data$mean_outcome[summary_data$adopt_t == 1 & summary_data$post == 0] -
     summary_data$mean_outcome[summary_data$adopt_t == 0 & summary_data$post == 0])

did_estimate_f

#data for Retail
data_r<-data_finfo[firm_sector=='Retail']
summary_data <- data_r %>%
  group_by(adopt_t, post) %>%
  summarise(mean_outcome = mean(revenue_t))
summary_data$post
summary_data

did_estimate_r <- (summary_data$mean_outcome[summary_data$adopt_t == 1 & summary_data$post == 1] -
                     summary_data$mean_outcome[summary_data$adopt_t == 0 & summary_data$post == 1]) -
  (summary_data$mean_outcome[summary_data$adopt_t == 1 & summary_data$post == 0] -
     summary_data$mean_outcome[summary_data$adopt_t == 0 & summary_data$post == 0])

did_estimate_r



