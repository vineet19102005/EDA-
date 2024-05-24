
# First I am seeing my current path and changing the path based on where i have saved the gapminder and mapping files so that i can import them easily.

getwd()
setwd("C:/")
getwd()

# Now loading library

library(tidyverse)
library(tidyr)

# Next I am clearing the enviornment.

rm(list = ls())

# Now I am actually storing the file in variable df1 and df2 with sheet 1 of gapminder excel file being in df1 and sheet 2 being in df2 and then viewing it.

df1 <- readxl::read_excel("gapminder_full.xlsx", sheet = 1)
View(df1)

df2 <- readxl::read_excel("gapminder_full.xlsx", sheet = 2)
View(df2)

# Changing df1 and df2 to tibble form, as similar to dataframe tibble comes with more functions that one can use around a dataset.

df1 <- as_tibble(df1)
df2 <- as_tibble(df2)

# Here I am checking the structure of boht datasets by using str

str(df1)
str(df2)

# By seeing the structure, both datasets have same columns and same number of columns. str also helped with what kind of datatype is present in each of the columns also tells us the column names.

# Now checking first 5 rows of df1 and df2 we can see how the datasets looks, we have used head for the same which shows the data on the console itself which is easy to see.

head(df1,5)
head(df2,5)
tail(df1,5)
tail(df2,5)

# By looking at tail of df1 and head of df2 one can make out the data present in df1 is a further continued and is stored in df2. So now we can append both the datasets so that working on it becomes easier and appending them won't be a problem as both df1 and df2 have same column names and same datatype amongst the columns.

df <- rbind(df1, df2)
View(df)

# Now we have a universal dataset consisting both sheets from gapminder named as df.

# We even have a mapping dataset which we will map later which contains country continent mapping but we will use that later first let's stick to country. (I have mapped the mapping csv later)

# Lets check the amount of null values we have in the universal dataset now.

null_val <- colSums(is.na(df))
null_val

# There is good chunk of null values in 3 columns lets deal with it with the help of locf function.

df$population <- zoo::na.locf(df$population)
df$`life exp` <- zoo::na.locf(df$`life exp`)
df$gdp_cap <- zoo::na.locf(df$gdp_cap)

# Now we have dealt with null values. 

null_vals <- colSums(is.na(df))

# Now the data is clean.

# Firstly I have calculated a yearwise growth in the world population by grouping year and taking sum of population column.

#Further more I have mutated the subset data and calculating growth in population yearwise by calculating difference using lag and then further also calculated growth by using basic math.

wrld_popln_yearwise <- df %>%
  group_by(year) %>%
  summarise(wrld_popln = (sum(population, na.rm = T)/1000000000))

wrld_popln_yearwise

wrld_popln_grwth <- df %>%
  group_by(year) %>%
  summarise(wrld_popln = (sum(population, na.rm = T))/1000000000) %>%
  mutate(popln_grwth = wrld_popln - lag(wrld_popln),
         grwth_rate = (wrld_popln - lag(wrld_popln)) / lag(wrld_popln) * 100)

wrld_popln_grwth

# I am dividing values by a Billion so as to scale the number down so its readable.

# By above calculation firstly there is decent growth happening in population till 1987. But in the year interval from 1987 to 1992 also there is decline from 1997 to 2002 there is a decline in the same and that too significant decline.

# The reason to decline ?, so while I was researching on this I found that during this period there was substantial fertility decline in India and China and different Asian Countries as well as in Latin America. During these times the death rate were more in populous countries and birth rates were comparatively less than before.  

# This might be a major reason for such decline.

# Now as I was working on Life Expectancy, after coding for a few while realised there was some discrepancy in the column name Life exp, it had space in between and `` this symbol too so lets change that first.

df <- df %>%
  rename(lifeExp = `life exp`)

# Now that's done I am looking at each country's life expectancy year-wise.

life_exp_yearwise <- df %>%
  select(year, country, lifeExp)

life_exp_yearwise

# Next what I want to see is whether all countires have their best life expectancy in 2007 i.e the last or latest year present in this dataset.
# This will let us know which countries saw a decline in thier life expectancy.

max_life_exp <- df %>%
  group_by(country) %>%
  filter(lifeExp == max(lifeExp)) %>%
  select(country, year, lifeExp)

max_life_exp

is_max_frm_2007 <- all(max_life_exp$year == 2007)

is_max_frm_2007

# SO the answer is false, not all countires have their best life exp in 2007, so lets find out which countries dont have thier best expectancy in 2007 but in different year.

cntries_with_max_not_2007 <- max_life_exp %>%
  filter(year != 2007)

print(cntries_with_max_not_2007, n = Inf)

# So there are 26 countries which do not have their best exp in 2007 and as one can see majorly it is African countries lets see which are top 5.

head(cntries_with_max_not_2007,5)

# All top 5 countries are African, lets further compare their best life exp with their expectancy in 2007.

selected_countries <- c("Botswana", "Cameroon", "Central African Republic", "Chad", "Congo, Dem. Rep.")

life_exp_2007 <- df %>%
  filter(country %in% selected_countries & year == 2007) %>%
  select(country, year, lifeExp)

life_exp_2007


# As you can see there is decline in life exp in these African Countries

# The reason ?, while researching came to know that according to a National Institutes of Health (NIH) study shows that

# all the subregions in Africa reached peak levels of life expectancy about 1990

# but they have since shown a decline, largely due to AIDS mortality. 

# Nowhere has the decrease in life expectancy been steeper and greater than in Southern Africa, where 40 years of increases in life expectancy were reversed in a period of 10 years.

# Further lets check average life expectancy of countries and lets see top 5 countries with great average life expectancies.

avg_life_exp_cntry <- df %>%
  group_by(country) %>%
  summarise(average_life_expectancy = mean(lifeExp)) %>%
  arrange(desc(average_life_expectancy))

head(avg_life_exp_cntry,5)

# So all European Countries, big ups Europe.

# Now as Europe has come up lets map all countries based on their continents.

mapping_df <- read.csv("mapping.csv")
View(mapping_df)

mapping_df <- mapping_df %>%
  separate("country.continent", into = c("country", "continent"), sep = ":")

df <- df %>%
  left_join(mapping_df, by = "country")

df

# So mapping is done and must say, that was one of a task to map it as the mapping csv was'nt so straight forward, anyhow let's move forward with our analysis.

# Lets further see based on continents, each continent's growth with respect to population and then further their life expectation yearly.

# Will do this based on my new found knowledge in plotting graphs.

summary_table <- df %>%
  group_by(year, continent) %>%
  summarize(Total_Population = sum(population))
summary_table

ggplot(summary_table, aes(x = year, y = Total_Population, color = continent, group = continent)) +
  geom_line() +
  labs(title = "Total Population by Continent and Year",
       x = "Year",
       y = "Total Population",
       color = "Continent") +
  theme_bw()

# One can make out the population has always been high in Asiatic countries and technically it is the largest continent in the world so this was no surprise. Top 2 populous countries are present in Asia too.

# Let's do something similar with life expectation

summary_table <- df %>%
  group_by(year, continent) %>%
  summarize(lifeexp = mean(lifeExp))
summary_table

ggplot(summary_table, aes(x = year, y = lifeexp, color = continent, group = continent)) +
  geom_line() +
  labs(title = "Continents and Life Expectation",
       x = "Year",
       y = "Life Expectation",
       color = "Continent") +
  theme_bw()

# No surprise here as well that Africa as the continent have not done well in this department and as I had analysed above this graph further proves my analysis to be right.

# Living in Asia, it being 4th here disappoints me.

# Now what I want to do is analyse some super powers of the world and let's see how they fair out in this dataset.

countries <- c("United States", "United Kingdom", "China", "France", "Germany")

filter <- df[df$country %in% countries,]
print(filter, n = Inf)
View(df)

ggplot(filter, aes(x = year, y = lifeExp, color = country)) +
  geom_line() +
  labs(title = "Life Expectancy Growth Over Time",
       x = "Year",
       y = "Life Expectancy",
       color = "Country") +
  theme_bw()

# Growth can bee seen in almost all countries at a decent pace but one can see in China the tenure between 1955 to 1965 there can be a bit of up and down.

# As I researched China faced Famine between 1958 to 1961. The widespread starvation and significant reduction in calorie intake during the famine had a devastating impact on population health and led to a sharp increase in mortality 
# France have had a steady growth.

# Now let's see which nation amongst these had the best life expectation in 2007

filtered <- df[df$country %in% countries & df$year == 2007,]

ggplot(filtered, aes(x = country, y = lifeExp, fill = country)) +
  geom_bar(stat = "identity") +
  labs(title = "Life Expectancy Comparison in 2007",
       x = "Country",
       y = "Life Expectancy",
       fill = "Country") +
  theme_bw()

# France amongst them has best Life Expectancy and all countries are neck to neck in this parameter.

# Now let's check this with respect to gdp cap.

ggplot(filter, aes(x = year, y = gdp_cap, color = country)) +
  geom_line() +
  labs(title = "Gdp Cap Growth Over Time",
       x = "Year",
       y = "Gdp Cap",
       color = "Country") +
  theme_bw()

# China here as well does'nt far out very well.

# China's GDP per capita is relatively low due to several factors. One reason is the substitution bias in consumption, which has led to an underestimation of real GDP per capita by the World Bank. Additionally, the reliance on urban prices, which are higher than rural prices, has further contributed to the lower estimates. This is what I came to know when I researched about this.

# Other than that all other countries have had increase in a decent pace.

ggplot(filtered, aes(x = country, y = gdp_cap, fill = country)) +
  geom_bar(stat = "identity") +
  labs(title = "Gdp Cap Comparison in 2007",
       x = "Country",
       y = "Gdp Cap",
       fill = "Country") +
  theme_bw()

# The US seems to dominate here while China struggles here and rest of them are more or less on the same level.

# I Love My India.

# Lets see how my country fairs out against these so called super powers.

# So using colors is something new which I have learnt and Red is for India, its showing Red in graph as label and not India, I was not sure how would I chnge that so Red is for India.

countries <- c("United States", "United Kingdom", "China", "France", "Germany", "India")

filter <- df[df$country %in% countries,]

filter$clr <- ifelse(filter$country == "India", "Red", "Other")

ggplot(filter, aes(x = year, y = lifeExp, color = clr, group = country)) +
  geom_line() +
  scale_color_manual(values = c("yellow", "red")) +  
  labs(title = "Life Expectancy Growth Over Time",
       x = "Year",
       y = "Life Expectancy",
       color = "Country") +
  theme_bw()

# One can see India also having a steady growth unlike China.

# Let's compare them in 2007

filtered <- df[df$country %in% countries & df$year == 2007,]

ggplot(filtered, aes(x = country, y = lifeExp, fill = country)) +
  geom_bar(stat = "identity") +
  labs(title = "Life Expectancy Comparison in 2007",
       x = "Country",
       y = "Life Expectancy",
       fill = "Country") +
  theme_bw()

# India seems to be a bit far off from most countries except China.


ggplot(filter, aes(x = year, y = gdp_cap, color = clr, group = country)) +
  geom_line() +
  scale_color_manual(values = c("yellow", "red")) +  
  labs(title = "Gdp Cap Growth Over Time",
       x = "Year",
       y = "Gdp Cap",
       color = "Country") +
  theme_bw()

# India here as well is not fairing out well.

# But one can see for India here in 2007 there is increase which has been its most increase from before and as per research its 9.6% increase which was recorded impressive back then.

filtered <- df[df$country %in% countries & df$year == 2007,]

ggplot(filtered, aes(x = country, y = gdp_cap, fill = country)) +
  geom_bar(stat = "identity") +
  labs(title = "Gdp Cap Comparison in 2007",
       x = "Country",
       y = "Gdp Cap",
       fill = "Country") +
  theme_bw()

# By the looks of bar graph India seems to be very poor here with respect to super powers back in 2007.

# As I live in Asia let us do some comparison amongst Asia countries

asian_filt <- df[df$continent == "Asia", ]

ggplot(asian_filt, aes(x = year, y = country, fill = lifeExp)) + geom_tile() +
  scale_fill_gradient(low = "lightgreen", high = "darkred") + 
  labs(title = "Heatmap of Life Expectancy in Asia",
       x = "Year",
       y = "Country",
       fill = "Life Expectancy") +
  theme_bw() 

# So by use of heatmap which for me was new to use as well for which I took the help of my notes from an online course. One can makeout top countries who had great life expectancy through out the time period given in the dataset which are

# Japan, Israel, Hong Kong and even Taiwan and Singapore.

# Let's plot which is top 5 gpd cap in Asia

asia_2007 <- df[df$continent == "Asia" & df$year == 2007,]

asia_2007_sort <- asia_2007 %>%
  arrange(desc(gdp_cap))

asia_2007_sort

best_asia = head(asia_2007_sort, n = 5)

ggplot(best_asia, aes(x = country, y = gdp_cap, fill = country)) +
  geom_bar(stat = "identity") +
  labs(title = "Asian Countries in 2007 by Gdp Cap",
       x = "Country",
       y = "Gdp Cap",
       fill = "Country") +
  theme_minimal()

# So as far as the dataset is concerned in 2007 Kuwait is best in Asia in terms of gdp cap.

# And here I conclude my analysis and I learnt very well with this assignment about the knowledge of graphs and even heatmaps.