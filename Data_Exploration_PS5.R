# Read in ggplot / diamonds dataset
library(ggplot2)

# Create a histogram of diamond prices and facet the histogram by
# color.  Use cut to color the histogram bars.
ggplot(aes(x = price, color = cut), data = diamonds) +
  geom_histogram(aes(color = cut, fill = cut), binwidth = 100) + 
  facet_wrap(~ color) +
  scale_x_continuous(breaks = c(1000, 10000), lim = c(0, 12000))

# Create a scatterplot of price vs table and color the points using cut
ggplot(aes(x = table, y = price), data = diamonds) + 
  geom_point(aes(color = cut)) + 
  scale_color_brewer(type = 'qual') +
  scale_x_continuous(breaks = seq(50, 80, 2), lim = c(50, 80)) +
  scale_y_continuous(lim = c(0, 19000))

# Create a scatterplot of price vs volume and color the points by
# clarity.  y-axis scale is log10.  Omit top 1% of volumes.
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(aes(x = volume, y = price), data = subset(diamonds, volume > 0)) +
  geom_point(aes(color = clarity)) +
  scale_x_continuous(lim = c(0, quantile(diamonds$volume, .99))) +
  scale_y_log10()

# Load facebook data
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')

# Create a new variable called "prop_initiated" that has the
# ratio of friendships_initiated to friend_count
pf$prop_initiated <- pf$friendships_initiated / pf$friend_count

# Recreate year_joined.bucket
pf$year_joined <- trunc(2014 - pf$tenure / 365)
pf$year_joined.bucket <- cut(pf$year_joined, 
                             breaks=c(2004, 2009, 2011, 2012, 2014))

# Create a line graph of the median of prop_initiated vs tenure, 
# using year_joined.bucket for color
ggplot(aes(x = tenure, y = prop_initiated), 
       data = subset(pf, !is.na(prop_initiated))) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = median)

# Smooth the plot
ggplot(aes(x = tenure, y = prop_initiated), 
       data = subset(pf, !is.na(prop_initiated))) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', 
            fun.y = median) +
  geom_smooth()

  # On average, the people who joined after 2012 have the greatest
  # proportion of initiating Facebook friendships

# Find the mean of prop_initiated for those who joined after 2012
library(dplyr)
pf %>% group_by(year_joined.bucket) %>%
       filter(year_joined.bucket == '(2012,2014]') %>%
       summarise(mean_prop_initiated = mean(prop_initiated, na.rm = TRUE))

# Create a scatter plot of price/carat ratio vs cut.  Color by diamond 
# color, facet by clarity.
diamonds$pvc <- diamonds$price / diamonds$carat
ggplot(aes(x = cut, y = pvc, color = color), data = diamonds) +
  geom_point(aes(color = color), position = position_jitter()) + 
  facet_wrap(~ clarity) 

#######################################################
# Look at Life Expectancy Data from Gapminder Website #
#######################################################

# Read in files
life <- read.csv("indicator life_expectancy_at_birth.csv", header = TRUE)
hspp <- read.csv("indicator health spending per person (US $).csv", header = TRUE)
gdpp <- read.csv("indicator gapminder gdp_per_capita_ppp.csv", header = TRUE)

# Change name of column 1
colnames(life)[1] = "Country"
colnames(hspp)[1] = "Country"
colnames(gdpp)[1] = "Country"

# Select relevant fields -- Use 2010 because that is the most 
# recent value for "geoh"
life2010 <- life %>% select(Country, X2010)
hspp2010 <- hspp %>% select(Country, X2010)
gdpp2010 <- gdpp %>% select(Country, X2010)

# Remove any "NAs" 
life2010 <- life2010 %>% filter(!is.na(X2010))
hspp2010 <- hspp2010 %>% filter(!is.na(X2010))
gdpp2010 <- gdpp2010 %>% filter(!is.na(X2010))

# Rename 2010 column to prevent conflict
colnames(life2010)[2] = "Life"
colnames(hspp2010)[2] = "HSPP"
colnames(gdpp2010)[2] = "GDPP"

# Join the data
all2010 <- inner_join(life2010, hspp2010)
all2010 <- inner_join(all2010, gdpp2010)

# Divide GDP Per Person into buckets.  "dig.lab = 10" used to avoid
# scientific notation in bucket labels.
summary(all2010$GDPP)
all2010$gdpp.bucket <- cut(all2010$GDPP, 
                           breaks=c(0, 3337, 16770, 21710, 128000),
                           dig.lab = 10)

# Set up colors
cvalues <- c('Black', 'Green', 'Orange', 'Blue')

# Make a scatterplot of Life Expectancy vs Health Spending Per Person,
# using the 4 GDP Per Person buckets for color
ggplot(aes(x = HSPP, y = Life), data = all2010) + 
  geom_point(aes(color = gdpp.bucket)) +
  scale_colour_manual(values = cvalues)

# Find the country at about $3000 HSPP but a life expectancy near 85
all2010 %>% filter(HSPP > 3000, HSPP < 3500, Life > 83) 

# Focus closer on GDPP is 0 to 1000
ggplot(aes(x = HSPP, y = Life), data = all2010) + 
  geom_point(aes(color = gdpp.bucket)) +
  xlim(0, 1000) +
  scale_colour_manual(values = cvalues)

# Focus closer on the area where life expectancy > 75
ggplot(aes(x = HSPP, y = Life), data = all2010) + 
  geom_point(aes(color = gdpp.bucket)) +
  xlim(0, 1000) +
  ylim(75, 80) +
  scale_colour_manual(values = cvalues)

# Find the 5 countries with high GDP but HSPP lower than 1000
all2010 %>% filter(HSPP < 1000, GDPP > 21710, Life >= 75) %>% arrange(desc(Life))

# Find the 8 countries where life expectancy >= 77 and HSPP is below 750
all2010 %>% filter(HSPP < 750, Life >= 77) %>% arrange(desc(Life))


# Read in more data to compare with life expectancy
corr <- read.csv("indicator ti cpi 2009.csv", header = TRUE)      # Corruption Index
demo <- read.csv("indicatorpolityiv.csv", header = TRUE)          # Democracy Index
litr <- read.csv("indicator SE_ADT_LITR_ZS.csv", header = TRUE)   # Literacy Rate
oilc <- read.csv("Oil Consumption per capita.csv", header = TRUE) # Oil Consumption

# Data Wrangle
colnames(corr)[1] = "Country"
colnames(demo)[1] = "Country"
colnames(litr)[1] = "Country"
colnames(oilc)[1] = "Country"
corr2009 <- corr %>% select(Country, X2009)  # 2009 is most recent
demo2010 <- demo %>% select(Country, X2010)
litr2010 <- litr %>% select(Country, X2010)
oilc2010 <- oilc %>% select(Country, X2010)
corr2009 <- corr2009 %>% filter(!is.na(X2009))
demo2010 <- demo2010 %>% filter(!is.na(X2010))
litr2010 <- litr2010 %>% filter(!is.na(X2010))
oilc2010 <- oilc2010 %>% filter(!is.na(X2010))
colnames(corr2009)[2] = "CORR"
colnames(demo2010)[2] = "DEMO"
colnames(litr2010)[2] = "LITR"
colnames(oilc2010)[2] = "OILC"

# Join data together
all2010 <- left_join(all2010, corr2009)  # Use 2009 data -- close enough for this analysis
all2010 <- left_join(all2010, demo2010)
all2010 <- left_join(all2010, litr2010)
all2010 <- left_join(all2010, oilc2010)

# Remove the "gdpp_bucket" parameter
all2010 <- all2010[,-5]

# Melt data into long form
library(reshape2)
all2010_long <- melt(data = all2010,
                     id = 1:2,
                     variable.name = "Parameter",
                     value.name = "Value",
                     na.rm = TRUE)

# Make scatterplots faceted by the 6 parameters
ggplot(aes(x = Value, y = Life), data = all2010_long) +
  geom_point() +
  facet_wrap(~ Parameter, scales = "free_x") 
  
                     
                     
