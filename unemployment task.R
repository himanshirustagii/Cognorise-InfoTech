# The following report provides an analysis of unemployment rates across
# different states of India, with a focus on the impact of the COVID-19 
# pandemic. By exploring various dimensions of the dataset, including state-wise
# unemployment, urban vs. rural disparities, and labor participation rates, we
# aim to uncover patterns and insights that could inform policy decisions.


# The dataset contains monthly unemployment data from Kaggle spanning
# from 31st May, 2019 to 30th june 2020. 

# DATASET OVERVIEW

"Dataset Overview:

The provided dataset delves into the unemployment landscape across diverse states in India:

States: Various states constituting the Indian subcontinent.
Date: The specific dates of unemployment rate recordings.
Measuring Frequency: The regularity of measurement collection (Monthly).
Estimated Unemployment Rate (%): The proportion of unemployed individuals in each Indian state.
Estimated Employed Individuals: The tally of presently engaged individuals.
Estimated Labour Participation Rate (%): The percentage of the working-age populace 
  (16-64 years) actively involved in the job market, including both employed individuals and 
  those actively seeking jobs."

"Step 1 : Loading the data"

# Install and load the readxl package
install.packages("readxl")
library(readxl)

# Load the data from the Excel file
data <- read_excel(file.choose())
data


"Step 2 : Cleaning and preprocessing the data "

"Duplicate entries were removed, and rows with missing values were omitted to
ensure the accuracy of the analysis. Date formats were standardized, and 
extraneous whitespace was trimmed from the 'Frequency' column."

installed.packages("dplyr")
library(dplyr)
# Remove duplicate rows
data <- distinct(data)

# Remove rows with all NAs
data <- na.omit(data)

# Convert the Date column to Date objects
data$Date <- as.Date(data$Date, format = "%d-%m-%Y")

# Strip leading/trailing whitespace from the Frequency column
data$Frequency <- trimws(data$Frequency)
data

Region         Date       Frequency Estimated Unemployment Ra…¹
<chr>          <date>     <chr>                           <dbl>
  1 Andhra Pradesh 2019-05-31 Monthly                          3.65
2 Andhra Pradesh 2019-06-30 Monthly                          3.05
3 Andhra Pradesh 2019-07-31 Monthly                          3.75
4 Andhra Pradesh 2019-08-31 Monthly                          3.32
5 Andhra Pradesh 2019-09-30 Monthly                          5.17
6 Andhra Pradesh 2019-10-31 Monthly                          3.52
7 Andhra Pradesh 2019-11-30 Monthly                          4.12
8 Andhra Pradesh 2019-12-31 Monthly                          4.38
9 Andhra Pradesh 2020-01-31 Monthly                          4.84
10 Andhra Pradesh 2020-02-29 Monthly                          5.91
# ℹ 730 more rows
# ℹ abbreviated name: ¹​`Estimated Unemployment Rate (%)`
# ℹ 3 more variables: `Estimated Employed` <dbl>,
#   `Estimated Labour Participation Rate (%)` <dbl>, Area <chr>
# ℹ Use `print(n = ...)` to see more rows


"Step 3 : Exploratory data analysis"

# Summary statistics
summary(data$`Estimated Unemployment Rate (%)`)
sd(data$`Estimated Unemployment Rate (%)`)

"Unemployment Rate
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   4.657   8.350  11.788  15.887  76.740 
  
The unemployment rate has a wide range, from 0% to 76.74%, with a mean around 11.79%.
The standard deviation is quite high at 10.72%, indicating significant variability.
50% of the data (the median) falls below 8.35%, which suggests that the distribution
   might be right-skewed with some very high unemployment rates pulling the mean upwards.
"

summary(data$`Estimated Employed`)
sd(data$`Estimated Employed`)

"Employed

 Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
   49420  1190404  4744178  7204460 11275490 45777509 
   
The number of employed individuals also varies widely across the dataset.
The standard deviation is nearly as large as the mean, indicating a substantial 
    spread in the number of employed people across different regions or times.
The median is less than half of the maximum value, again suggesting a right-skewed 
   distribution."

summary(data$`Estimated Labour Participation Rate (%)`)
sd(data$`Estimated Labour Participation Rate (%)`)

"Labour Participation Rate

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  13.33   38.06   41.16   42.63   45.51   72.57
  
The labor participation rate has a mean of 42.63% with a standard deviation of 8.11%, 
    suggesting moderate variability.
The median is very close to the mean, which may indicate a more symmetric distribution
    of labor participation rates."


"Next, we should visualize these distributions and look at how the unemployment rate
changed over time, especially in light of the COVID-19 pandemic, as well as
differences between regions and urban vs. rural areas.

Let's start with visualizations for the distribution of the unemployment rate,
employed population, and labor participation rate. We'll use histograms
for this purpose."



# Install and load necessary packages
install.packages(c("ggplot2", "dplyr"))
library(ggplot2)
library(dplyr)

# Histogram of Estimated Unemployment Rate (%)
ggplot(data, aes(x = `Estimated Unemployment Rate (%)`)) +
  geom_histogram(binwidth = 2.5, fill = "blue",colour = "black", alpha = 0.7) + 
  labs(title = "Distribution of Estimated Unemployment Rate (%)", x = "Unemployment Rate (%)", y = "Frequency")


"The distribution shows a peak at lower percentages, indicating that most data points 
    have a relatively low unemployment rate.
There is a long tail toward higher percentages, confirming the right-skewness
    suggested by the summary statistics.
The presence of very high unemployment rates may indicate extreme cases or 
    specific times of distress (such as the COVID-19 pandemic)."


ggplot(data, aes(x = `Estimated Labour Participation Rate (%)`)) +
  geom_histogram(binwidth = 3, fill = "blue",colour = "black", alpha = 0.7) + 
  labs(title = "Distribution of Estimated Labour Participation Rate(%) ", 
       x = "Labour Participation Rate(%)", y = "Frequency")

"The distribution appears more symmetric than the other two, with a clear central
     peak and tails on both sides.
This suggests that the labor participation rate varies less dramatically across
    different regions or over time than the unemployment rate or the number 
    of employed individuals."

" Step 4"

"Next, let's analyze how the unemployment rate has changed over time. We'll 
    create a time series plot to visualize this trend. This will help us 
    understand if there were any significant changes that might be associated
    with the COVID-19 pandemic or other events. We'll also look at the 
    unemployment rate's trend in rural versus urban areas."

# Line plot for Unemployment Rate over time

ggplot(data, aes(x = Date, y = `Estimated Unemployment Rate (%)`, 
                 group = Area, color = Area)) +
  geom_line() 
labs(title = "Unemployment Rate Over Time by Area", x = "Date",
     y = "Unemployment Rate (%)")

"The time series plots provide the following insights:

Overall Trend:

The unemployment rate fluctuates over time, with some peaks and troughs indicating
  periods of higher and lower unemployment.
There is a noticeable spike around mid-2020, which aligns with the onset of the 
  COVID-19 pandemic and the subsequent lockdowns and economic disruptions.
  
By Area (Rural vs. Urban):

The plot shows the overall trend without specifically focusing on the 
COVID-19 period. The unemployment rate in urban areas seems to be generally higher
  than in rural areas.
The plot, highlights the impact of the COVID-19 pandemic more clearly. Both rural and urban areas 
experienced a significant increase in unemployment rates during the pandemic, 
with urban areas being more affected.
After the spike, there is a downward trend indicating a recovery phase,
but the rates have not returned to the pre-pandemic levels, especially in 
urban areas.
"
"step 5"

# State-wise analysis
state_unemployment <- data %>%
  group_by(Region) %>%
  summarize(Average_Unemployment = mean(`Estimated Unemployment Rate (%)`)) %>%
  arrange(Average_Unemployment)

ggplot(state_unemployment, aes(x = Average_Unemployment, y = reorder(Region, Average_Unemployment))) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Average Unemployment Rate by State", x = "Average Unemployment Rate (%)", y = "State")


"The bar chart shows the average unemployment rate for each state over the time
period covered by the dataset. Here are some observations:

There is a wide range in the average unemployment rates across different states,
indicating regional disparities in employment conditions.
Some states have notably higher average unemployment rates, while others have
managed to maintain lower averages.
This state-wise analysis is valuable for policymakers to identify which regions
may need more attention and resources to combat unemployment."

# Urban vs. Rural analysis
urban_rural_unemployment <- data %>%
  group_by(Area, Date) %>%
  summarize(Average_Unemployment = mean(`Estimated Unemployment Rate (%)`))

ggplot(urban_rural_unemployment, aes(x = Date, y = Average_Unemployment, group = Area, color = Area)) +
  geom_line() +
  labs(title = "Urban vs. Rural Unemployment Rate Over Time", x = "Date", y = "Average Unemployment Rate (%)")

"The line plot illustrates the urban versus rural unemployment rate trends over time:

Both urban and rural areas show fluctuations in unemployment rates, with urban 
areas generally experiencing higher rates than rural areas throughout the dataset's timeframe.
The spike around mid-2020, likely corresponding to the COVID-19 pandemic's impact,
is clearly visible in both urban and rural areas, with urban areas showing a sharper increase.
Post the spike, there appears to be a gradual decline, suggesting a recovery from
the peak unemployment rates experienced during the pandemic."


"Step 6"

# Pre and Post COVID-19 analysis
pre_pandemic <- filter(data, Date < as.Date("2020-03-01"))
post_pandemic <- filter(data, Date >= as.Date("2020-03-01"))

mean(pre_pandemic$`Estimated Unemployment Rate (%)`)
mean(post_pandemic$`Estimated Unemployment Rate (%)`)

"The comparison of average unemployment rates before and after the onset 
of the COVID-19 pandemic reveals:

Pre-pandemic average unemployment rate: approximately 9.51%
Post-pandemic average unemployment rate: approximately 17.77%
This indicates a significant increase in the unemployment rate following the onset of the
pandemic. The data suggests that the economic impact of COVID-19 was considerable,
nearly doubling the unemployment rate on average across the regions in the dataset.

"