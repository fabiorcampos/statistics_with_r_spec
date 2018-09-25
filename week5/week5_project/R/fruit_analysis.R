library(ggplot2)
library(dplyr)

### Load Data

load("~/MEGA/CURSOS_ONLINE/statistics_with_r_spec/week5/week5_project/brfss2013.RData")

df = brfss2013

### Juice Excellent
prop.fruit.ex = df %>% 
      filter(fruit1 <= 299 & fruit1 >=201) %>%
      filter(genhlth == "Excellent")

prop.fruit.ex = as.data.frame(table(prop.fruit.ex$fruit1))
prop.fruit.ex = as.data.frame(prop.fruit.ex[1:10, 2])

### Juice Very Good
prop.fruit.vergood = df %>% 
      filter(fruit1 <= 299 & fruit1 >=201) %>%
      filter(genhlth == "Very good")

prop.fruit.vergood = as.data.frame(table(prop.fruit.vergood$fruit1))
prop.fruit.vergood = as.data.frame(prop.fruit.vergood[1:10, 2])

### Juice Good
prop.fruit.good = df %>% 
      filter(fruit1 <= 299 & fruit1 >=201) %>%
      filter(genhlth == "Good")

prop.fruit.good = as.data.frame(table(prop.fruit.good$fruit1))
prop.fruit.good = as.data.frame(prop.fruit.good[1:10, 2])

### Juice Fair
prop.fruit.fair = df %>% 
      filter(fruit1 <= 299 & fruit1 >=201) %>%
      filter(genhlth == "Fair")

prop.fruit.fair = as.data.frame(table(prop.fruit.fair$fruit1))
prop.fruit.fair = as.data.frame(prop.fruit.fair[1:10, 2])

### Juice Poor
prop.fruit.poor = df %>% 
      filter(fruit1 <= 299 & fruit1 >=201) %>%
      filter(genhlth == "Poor")

prop.fruit.poor = as.data.frame(table(prop.fruit.poor$fruit1))
prop.fruit.poor = as.data.frame(prop.fruit.poor[1:10, 2])

### Adjust Data
times = c("1 time", "2 times", "3 times", "4 times", 
          "5 times", "6 times", "7 times", "8 times", 
          "9 times", "10 times")

colnames = c("Excellent", "VeryGood", "Good", "Fair", "Poor")

df.fruit = cbind(prop.fruit.ex,
                 prop.fruit.vergood,
                 prop.fruit.good,
                 prop.fruit.fair,
                 prop.fruit.poor)

names(df.fruit) = colnames

### Analyzing Data
df.fruit = mutate(df.fruit, ExcellentProportion = Excellent/sum(Excellent))
df.fruit = mutate(df.fruit, VeryGoodProportion = VeryGood /sum(VeryGood))
df.fruit = mutate(df.fruit, GoodProportion = Good/sum(Good))
df.fruit = mutate(df.fruit, fairProportion = Fair/sum(Fair))
df.fruit = mutate(df.fruit, PoorProportion = Poor/sum(Poor))