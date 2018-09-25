library(ggplot2)
library(dplyr)

### Load Data

load("~/MEGA/CURSOS_ONLINE/statistics_with_r_spec/week5/week5_project/brfss2013.RData")

df = brfss2013

### Juice Excellent
prop.fruits.ex = df %>% 
      filter(fruitju1 <= 299 & fruitju1 >=201) %>%
      filter(genhlth == "Excellent")

prop.fruits.ex = as.data.frame(table(prop.fruits.ex$fruitju1))
prop.fruits.ex = as.data.frame(prop.fruits.ex[1:10, 2])

### Juice Very Good
prop.fruits.vergood = df %>% 
      filter(fruitju1 <= 299 & fruitju1 >=201) %>%
      filter(genhlth == "Very good")

prop.fruits.vergood = as.data.frame(table(prop.fruits.vergood$fruitju1))
prop.fruits.vergood = as.data.frame(prop.fruits.vergood[1:10, 2])

### Juice Good
prop.fruits.good = df %>% 
      filter(fruitju1 <= 299 & fruitju1 >=201) %>%
      filter(genhlth == "Good")

prop.fruits.good = as.data.frame(table(prop.fruits.good$fruitju1))
prop.fruits.good = as.data.frame(prop.fruits.good[1:10, 2])

### Juice Fair
prop.fruits.fair = df %>% 
      filter(fruitju1 <= 299 & fruitju1 >=201) %>%
      filter(genhlth == "Fair")

prop.fruits.fair = as.data.frame(table(prop.fruits.fair$fruitju1))
prop.fruits.fair = as.data.frame(prop.fruits.fair[1:10, 2])

### Juice Poor
prop.fruits.poor = df %>% 
      filter(fruitju1 <= 299 & fruitju1 >=201) %>%
      filter(genhlth == "Poor")

prop.fruits.poor = as.data.frame(table(prop.fruits.poor$fruitju1))
prop.fruits.poor = as.data.frame(prop.fruits.poor[1:10, 2])

### Adjust Data
times = c("1 time", "2 times", "3 times", "4 times", 
          "5 times", "6 times", "7 times", "8 times", 
          "9 times", "10 times")

colnames = c("Excellent", "VeryGood", "Good", "Fair", "Poor")

df.juice = cbind(prop.fruits.ex,
                 prop.fruits.vergood,
                 prop.fruits.good,
                 prop.fruits.fair,
                 prop.fruits.poor)

names(df.juice) = colnames

### Analyzing Data
df.juice = mutate(df.juice, ExcellentProportion = Excellent/sum(Excellent))
df.juice = mutate(df.juice, VeryGoodProportion = VeryGood /sum(VeryGood))
df.juice = mutate(df.juice, GoodProportion = Good/sum(Good))
df.juice = mutate(df.juice, fairProportion = Fair/sum(Fair))
df.juice = mutate(df.juice, PoorProportion = Poor/sum(Poor))







