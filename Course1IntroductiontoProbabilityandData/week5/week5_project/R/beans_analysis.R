library(ggplot2)
library(dplyr)

### Load Data

load("~/MEGA/CURSOS_ONLINE/statistics_with_r_spec/week5/week5_project/brfss2013.RData")

df = brfss2013

### beans Excellent
prop.beans.ex = df %>% 
      filter(fvbeans <= 299 & fvbeans >=201) %>%
      filter(genhlth == "Excellent")

prop.beans.ex = as.data.frame(table(prop.beans.ex$fvbeans))
prop.beans.ex = as.data.frame(prop.beans.ex[1:10, 2])

### beans Very Good
prop.beans.vergood = df %>% 
      filter(fvbeans <= 299 & fvbeans >=201) %>%
      filter(genhlth == "Very good")

prop.beans.vergood = as.data.frame(table(prop.beans.vergood$fvbeans))
prop.beans.vergood = as.data.frame(prop.beans.vergood[1:10, 2])

### beans Good
prop.beans.good = df %>% 
      filter(fvbeans <= 299 & fvbeans >=201) %>%
      filter(genhlth == "Good")

prop.beans.good = as.data.frame(table(prop.beans.good$fvbeans))
prop.beans.good = as.data.frame(prop.beans.good[1:10, 2])

### beans Fair
prop.beans.fair = df %>% 
      filter(fvbeans <= 299 & fvbeans >=201) %>%
      filter(genhlth == "Fair")

prop.beans.fair = as.data.frame(table(prop.beans.fair$fvbeans))
prop.beans.fair = as.data.frame(prop.beans.fair[1:10, 2])

### beans Poor
prop.beans.poor = df %>% 
      filter(fvbeans <= 299 & fvbeans >=201) %>%
      filter(genhlth == "Poor")

prop.beans.poor = as.data.frame(table(prop.beans.poor$fvbeans))
prop.beans.poor = as.data.frame(prop.beans.poor[1:10, 2])

### Adjust Data
times = c("1 time", "2 times", "3 times", "4 times", 
          "5 times", "6 times", "7 times", "8 times", 
          "9 times", "10 times")

colnames = c("Excellent", "VeryGood", "Good", "Fair", "Poor")

df.beans = cbind(prop.beans.ex,
                     prop.beans.vergood,
                     prop.beans.good,
                     prop.beans.fair,
                     prop.beans.poor)

names(df.beans) = colnames

### Analyzing Data
df.beans = mutate(df.beans, ExcellentProportion = Excellent/sum(Excellent))
df.beans = mutate(df.beans, VeryGoodProportion = VeryGood /sum(VeryGood))
df.beans = mutate(df.beans, GoodProportion = Good/sum(Good))
df.beans = mutate(df.beans, fairProportion = Fair/sum(Fair))
df.beans = mutate(df.beans, PoorProportion = Poor/sum(Poor))