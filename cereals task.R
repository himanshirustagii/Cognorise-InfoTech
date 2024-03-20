# Cereal dataset from Kaggle - https://www.kaggle.com/datasets/crawford/80-cereals

"Fields in the dataset:

Name: Name of cereal
mfr: Manufacturer of cereal
A = American Home Food Products;
G = General Mills
K = Kelloggs
N = Nabisco
P = Post
Q = Quaker Oats
R = Ralston Purina
type:cold and hot
calories: calories per serving
protein: grams of protein
fat: grams of fat
sodium: milligrams of sodium
fiber: grams of dietary fiber
carbo: grams of complex carbohydrates
sugars: grams of sugars
potass: milligrams of potassium
vitamins: vitamins and minerals - 0, 25, or 100, indicating the typical 
          percentage of FDA recommended
shelf: display shelf (1, 2, or 3, counting from the floor)
weight: weight in ounces of one serving
cups: number of cups in one serving
rating: a rating of the cereals (Possibly from Consumer Reports?)"


# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(cluster)
library(corrplot)
library(gridExtra)
library(reshape2) 


# Read the dataset
cereal_df <- read_excel(file.choose())

# Inspect the first few rows of the dataframe
head(cereal_df)

# A tibble: 6 × 16
name    mfr   type  calories protein   fat sodium fiber carbo sugars potass
<chr>   <chr> <chr>    <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>  <dbl>  <dbl>
  1 100% B… N     C           70       4     1    130  10     5        6    280
2 100% N… Q     C          120       3     5     15   2     8        8    135
3 All-Br… K     C           70       4     1    260   9     7        5    320
4 All-Br… K     C           50       4     0    140  14     8        0    330
5 Almond… R     C          110       2     2    200   1    14        8     -1
6 Apple … G     C          110       2     2    180   1.5  10.5     10     70
# ℹ 5 more variables: vitamins <dbl>, shelf <dbl>, weight <dbl>, cups <dbl>,
#   rating <dbl>

# Explore the structure of the data
str(cereal_df)

# Checking for missing values
sum(is.na(cereal_df))

# Descriptive statistics for the dataset
summary(cereal_df)

# Replace -1 values with NA (R uses NA for missing values)
cereal_df[cereal_df == -1] <- NA

# Impute missing values with the median of their respective columns
cereal_df$carbo[is.na(cereal_df$carbo)] <- median(cereal_df$carbo, na.rm = TRUE)
cereal_df$sugars[is.na(cereal_df$sugars)] <- median(cereal_df$sugars, na.rm = TRUE)
cereal_df$potass[is.na(cereal_df$potass)] <- median(cereal_df$potass, na.rm = TRUE)

# Descriptive statistics post imputation
summary(cereal_df)

"# Summary Interpretation:
# Calories: Range from 50 to 160 per serving, with most around 110, indicating a 
   moderate calorie range.
# Protein: Generally between 1 and 6 grams, with an average slightly above 
   2.5 grams, suggesting low protein content.
# Fat: Ranges from 0 to 5 grams per serving, mostly around 1 gram, indicating 
   low fat content in most cereals.
# Sodium: Wide range (0 to 320 mg per serving), with an average of 160 mg, 
   suggesting varied sodium levels.
# Fiber: Varies significantly (0 to 14 grams per serving), with an average
  of 2 grams, indicating varied fiber content.
# Carbohydrates: Range from 5 to 23 grams, with most cereals having moderate
  carbohydrate levels.
# Sugars: Vary significantly from 0 to 15 grams per serving, with an average
  of 7 grams, indicating varied sweetness.
# Potassium: Wide range (15 to 330 mg per serving), with some cereals being
  good potassium sources.
# Vitamins: Vary widely, indicating a range of fortification levels, with 
  many cereals fortified.
# Shelf: Ranges from 1 to 3, with no clear correlation to nutritional value.
# Weight: Serving weight varies from 0.5 to 1.5, typically around 1, 
   standardizing serving sizes.
# Cups: Recommended serving size varies, typically between 0.67 and 1 cup.
# Rating: Wide range (18 to 94), with an average of 42.7, indicating diverse 
  consumer preferences.
"

# Visualizing the distribution of various nutrients
# Creating individual plots for each nutrient
# Histogram with a normal distribution curve for Calories


install.packages("ggplot2")
library(ggplot2)


p_calories <- ggplot(cereal_df, aes(x=calories)) + 
  geom_histogram(aes(y=..density..), binwidth = 10, fill="blue", color="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(cereal_df$calories, 
                                                     na.rm = TRUE), sd = sd(cereal_df$calories, na.rm = TRUE)),
                color = "red", linewidth = 1) + ggtitle("Calories Distribution")

# Histogram with a normal distribution curve for Protein
p_protein <- ggplot(cereal_df, aes(x=protein)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, fill="green", color="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(cereal_df$protein, na.rm = 
                                                       TRUE), sd = sd(cereal_df$protein, na.rm = TRUE)),
                color = "red", size = 1) + 
  ggtitle("Protein Distribution")

# Histogram with a normal distribution curve for Fat
p_fat <- ggplot(cereal_df, aes(x=fat)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, fill="red", color="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(cereal_df$fat, na.rm = TRUE), 
                                         sd = sd(cereal_df$fat, na.rm = TRUE)), color = "red", size = 1) + 
  ggtitle("Fat Distribution")

# Histogram with a normal distribution curve for Sodium
p_sodium <- ggplot(cereal_df, aes(x=sodium)) + 
  geom_histogram(aes(y=..density..), binwidth = 30, fill="orange", color="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(cereal_df$sodium, na.rm = 
                                                       TRUE), sd = sd(cereal_df$sodium, na.rm = TRUE)), color = "red",
                size = 1) +  ggtitle("Sodium Distribution")

# Histogram with a normal distribution curve for Fiber
p_fiber <- ggplot(cereal_df, aes(x=fiber)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, fill="brown", color="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(cereal_df$fiber, na.rm = TRUE),
                                         sd = sd(cereal_df$fiber, na.rm = TRUE)), color = "red", linewidth = 1)+ 
  ggtitle("Fiber Distribution")

# Histogram with a normal distribution curve for Carbohydrates
p_carbo <- ggplot(cereal_df, aes(x=carbo)) + 
  geom_histogram(aes(y=..density..), binwidth = 2, fill="purple", color="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(cereal_df$carbo, na.rm = TRUE),
                                         sd = sd(cereal_df$carbo, na.rm = TRUE)), color = "red", linewidth = 1)+ 
  ggtitle("Carbohydrates Distribution")

# Histogram with a normal distribution curve for Sugars
p_sugars <- ggplot(cereal_df, aes(x=sugars)) + 
  geom_histogram(aes(y=..density..), binwidth = 2, fill="pink", color="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(cereal_df$sugars, na.rm = TRUE),
                                         sd = sd(cereal_df$sugars, na.rm = TRUE)), color = "red", linewidth = 1)+ 
  ggtitle("Sugars Distribution")

# Histogram with a normal distribution curve for Potassium
p_potass <- ggplot(cereal_df, aes(x=potass)) + 
  geom_histogram(aes(y=..density..), binwidth = 30, fill="yellow", color="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(cereal_df$potass, na.rm = TRUE),
                                         sd = sd(cereal_df$potass, na.rm = TRUE)), color = "red", linewidth = 1)+ 
  ggtitle("Potassium Distribution")


# Arrange the plots in a grid
install.packages("gridExtra")

library(gridExtra)

grid.arrange(p_calories, p_protein, p_fat, p_sodium, p_fiber, p_carbo, p_sugars,
             p_potass, ncol=3)

# Interpretation: The visualizations indicate that calories, fat, and sugars are 
#generally lower in higher-rated cereals, while fibre and protein are higher. 
# This suggests that healthier cereals tend to be rated more favourably.


# Correlation heat map
install.packages("dplyr")
library(dplyr)

correlation_matrix <- cor(cereal_df %>% select(-name, -mfr, -type, -shelf, -weight,
                                               -cups), use="complete.obs") 
# excluding non-numeric columns
install.packages("corrplot")
library(corrplot)
corrplot(correlation_matrix, method="color")

# Interpretation: Strong negative correlations between rating and sugars
#/sodium/calories suggest consumers prefer cereals that are lower in these 
#attributes. Positive correlations between fibre/protein and rating indicate 
#these are desirable attributes.

# Bar plots for average ratings by manufacturer
avg_rating_by_mfr <- cereal_df %>% group_by(mfr) %>% summarise(avg_rating =
                                                                 mean(rating, na.rm = TRUE))
ggplot(avg_rating_by_mfr, aes(x=mfr, y=avg_rating)) + geom_bar(stat="identity", 
                                                               colour ="lightblue") + ggtitle("Average Cereal Rating by
                                                   Manufacturer")

# Interpretation: The bar plot reveals that certain manufacturers consistently 
#produce higher-rated cereals, possibly indicating a better overall product 
#quality or alignment with consumer preferences.

# Comparing cold and hot cereals

install.packages("reshape2")
library(reshape2)
melted_avg_nutrients_by_type <- melt(avg_nutrients_by_type, id.vars = "type")


avg_nutrients_by_type <- cereal_df %>% group_by(type) %>% summarise(across(c
                                                                           (calories, protein, fat, sodium, fiber, carbo, sugars, 
                                                                             potass), mean, na.rm = TRUE))
melted_avg_nutrients_by_type <- melt(avg_nutrients_by_type, id.vars = "type")
ggplot(melted_avg_nutrients_by_type, aes(x=type, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge") + facet_wrap(~variable, scales=
                                                             "free_y") + theme(axis.text.x = element_text(angle=90)) + labs(fill="Nutrient")

# Interpretation: Hot cereals are generally higher in fibre and protein and 
#lower in sugar and sodium, which aligns with their higher average ratings.

# Scatter plots of nutrients vs ratings
p_fiber_rating <- ggplot(cereal_df, aes(x=fiber, y=rating)) + geom_point() +
  ggtitle("Fiber vs Rating")+ geom_smooth(method=lm, color="red", se = F)
p_protein_rating <- ggplot(cereal_df, aes(x=protein, y=rating)) + geom_point() + 
  ggtitle("Protein vs Rating") + geom_smooth(method=lm, color="blue", se = F)
p_sugars_rating <- ggplot(cereal_df, aes(x=sugars, y=rating)) + geom_point() +
  ggtitle("Sugars vs Rating")+ geom_smooth(method=lm, color="green", se = F)
p_fat_rating <- ggplot(cereal_df, aes(x=fat, y=rating)) + geom_point() + 
  ggtitle("Fat vs Rating")+ geom_smooth(method=lm, color="purple", se = F)


# Display scatter plots
grid.arrange(p_fiber_rating, p_protein_rating, p_sugars_rating, p_fat_rating,
             ncol=2)

# Interpretation: Higher fiber and protein contents are positively associated 
#with higher cereal ratings, suggesting a consumer preference for these nutrients.
#Higher sugar and fat content is negatively associated with ratings, indicating a 
#preference for less sugary and fatty cereals.
