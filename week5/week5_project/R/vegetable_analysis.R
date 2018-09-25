library(ggplot2)
library(dplyr)

### Load Data

load("~/MEGA/CURSOS_ONLINE/statistics_with_r_spec/week5/week5_project/brfss2013.RData")

df = brfss2013

### Vegetables Excellent
prop.veg.ex = df %>% 
      filter(vegetab1 <= 299 & vegetab1 >=201) %>%
      filter(genhlth == "Excellent")

prop.veg.ex = as.data.frame(table(prop.veg.ex$vegetab1))
prop.veg.ex = as.data.frame(prop.veg.ex[1:10, 2])

### Vegetable Very Good
prop.veg.vergood = df %>% 
      filter(vegetab1 <= 299 & vegetab1 >=201) %>%
      filter(genhlth == "Very good")

prop.veg.vergood = as.data.frame(table(prop.veg.vergood$vegetab1))
prop.veg.vergood = as.data.frame(prop.veg.vergood[1:10, 2])

### Vegetable Good
prop.veg.good = df %>% 
      filter(vegetab1 <= 299 & vegetab1 >=201) %>%
      filter(genhlth == "Good")

prop.veg.good = as.data.frame(table(prop.veg.good$vegetab1))
prop.veg.good = as.data.frame(prop.veg.good[1:10, 2])

### Vegetable Fair
prop.veg.fair = df %>% 
      filter(vegetab1 <= 299 & vegetab1 >=201) %>%
      filter(genhlth == "Fair")

prop.veg.fair = as.data.frame(table(prop.veg.fair$vegetab1))
prop.veg.fair = as.data.frame(prop.veg.fair[1:10, 2])

### vegetable Poor
prop.veg.poor = df %>% 
      filter(vegetab1 <= 299 & vegetab1 >=201) %>%
      filter(genhlth == "Poor")

prop.veg.poor = as.data.frame(table(prop.veg.poor$vegetab1))
prop.veg.poor = as.data.frame(prop.veg.poor[1:10, 2])

### Adjust Data
times = c("1 time", "2 times", "3 times", "4 times", 
          "5 times", "6 times", "7 times", "8 times", 
          "9 times", "10 times")

colnames = c("Excellent", "VeryGood", "Good", "Fair", "Poor")

df.vegetable = cbind(prop.veg.ex,
                 prop.veg.vergood,
                 prop.veg.good,
                 prop.veg.fair,
                 prop.veg.poor)

names(df.vegetable) = colnames

### Analyzing Data
df.vegetable = mutate(df.vegetable, ExcellentProportion = Excellent/sum(Excellent))
df.vegetable = mutate(df.vegetable, VeryGoodProportion = VeryGood /sum(VeryGood))
df.vegetable = mutate(df.vegetable, GoodProportion = Good/sum(Good))
df.vegetable = mutate(df.vegetable, fairProportion = Fair/sum(Fair))
df.vegetable = mutate(df.vegetable, PoorProportion = Poor/sum(Poor))