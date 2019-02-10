library(ggplot2)
library(dplyr)

### Load Data

load("~/MEGA/CURSOS_ONLINE/statistics_with_r_spec/week5/week5_project/brfss2013.RData")

df = brfss2013

### Male Vegetables Excellent
prop.fruit.ex.male = df %>% 
      filter(fruit1 <= 299 & fruit1 >=201) %>%
      filter(genhlth == "Excellent") %>%
      filter(sex == "Male")

prop.fruit.ex.male = as.data.frame(table(prop.fruit.ex.male$fruit1))
prop.fruit.ex.male = as.data.frame(prop.fruit.ex.male[1:10, 2])

### Female Vegetables Excellent
prop.fruit.ex.female = df %>% 
      filter(fruit1 <= 299 & fruit1 >=201) %>%
      filter(genhlth == "Excellent") %>%
      filter(sex == "Female")

prop.fruit.ex.female = as.data.frame(table(prop.fruit.ex.female$fruit1))
prop.fruit.ex.female = as.data.frame(prop.fruit.ex.female[1:10, 2])

### Male Vegetable Very Good
prop.fruit.vergood.male = df %>% 
      filter(fruit1 <= 299 & fruit1 >=201) %>%
      filter(genhlth == "Very good") %>%
      filter(sex == "Male")

prop.fruit.vergood.male = as.data.frame(table(prop.fruit.vergood.male$fruit1))
prop.fruit.vergood.male = as.data.frame(prop.fruit.vergood.male[1:10, 2])

### Female Vegetable Very Good
prop.fruit.vergood.female = df %>% 
      filter(fruit1 <= 299 & fruit1 >=201) %>%
      filter(genhlth == "Very good") %>%
      filter(sex == "Female")

prop.fruit.vergood.female = as.data.frame(table(prop.fruit.vergood.female$fruit1))
prop.fruit.vergood.female = as.data.frame(prop.fruit.vergood.female[1:10, 2])

### Male Vegetable Good
prop.fruit.good.male = df %>% 
      filter(fruit1 <= 299 & fruit1 >=201) %>%
      filter(genhlth == "Good") %>%
      filter(sex == "Male")

prop.fruit.good.male = as.data.frame(table(prop.fruit.good.male$fruit1))
prop.fruit.good.male = as.data.frame(prop.fruit.good.male[1:10, 2])

### Female Vegetable Good
prop.fruit.good.female = df %>% 
      filter(fruit1 <= 299 & fruit1 >=201) %>%
      filter(genhlth == "Good") %>%
      filter(sex == "Female")

prop.fruit.good.female = as.data.frame(table(prop.fruit.good.female$fruit1))
prop.fruit.good.female = as.data.frame(prop.fruit.good.female[1:10, 2])

### Male Vegetable Fair
prop.fruit.fair.male = df %>% 
      filter(fruit1 <= 299 & fruit1 >=201) %>%
      filter(genhlth == "Fair") %>%
      filter(sex == "Male")

prop.fruit.fair.male = as.data.frame(table(prop.fruit.fair.male$fruit1))
prop.fruit.fair.male = as.data.frame(prop.fruit.fair.male[1:10, 2])

### Female Vegetable Fair
prop.fruit.fair.female = df %>% 
      filter(fruit1 <= 299 & fruit1 >=201) %>%
      filter(genhlth == "Fair") %>%
      filter(sex == "Female")

prop.fruit.fair.female = as.data.frame(table(prop.fruit.fair.female$fruit1))
prop.fruit.fair.female = as.data.frame(prop.fruit.fair.female[1:10, 2])

### Male fruitetable Poor
prop.fruit.poor.male = df %>% 
      filter(fruit1 <= 299 & fruit1 >=201) %>%
      filter(genhlth == "Poor") %>%
      filter(sex == "Male")

prop.fruit.poor.male = as.data.frame(table(prop.fruit.poor.male$fruit1))
prop.fruit.poor.male = as.data.frame(prop.fruit.poor.male[1:10, 2])

### Male fruitetable Poor
prop.fruit.poor.female = df %>% 
      filter(fruit1 <= 299 & fruit1 >=201) %>%
      filter(genhlth == "Poor") %>%
      filter(sex == "Female")

prop.fruit.poor.female = as.data.frame(table(prop.fruit.poor.female$fruit1))
prop.fruit.poor.female = as.data.frame(prop.fruit.poor.female[1:10, 2])

### Adjust Data
times = c("1 time", "2 times", "3 times", "4 times", 
          "5 times", "6 times", "7 times", "8 times", 
          "9 times", "10 times")

colnames = c("Excellent.Male","Excellent.Female", 
             "VeryGood.Male", "VeryGood.Female",
             "Good.Male", "Good.Female",
             "Fair.Male", "Fair.Female",
             "Poor.Male", "Poor.Female")

df.fruitetable.sex = cbind(prop.fruit.ex.male,prop.fruit.ex.female,
                         prop.fruit.vergood.male, prop.fruit.vergood.female,
                         prop.fruit.good.male, prop.fruit.good.female,
                         prop.fruit.fair.male, prop.fruit.fair.female,
                         prop.fruit.poor.male, prop.fruit.poor.female)

names(df.fruitetable.sex) = colnames

### Analyzing Data
df.fruitetable.sex = mutate(df.fruitetable.sex, ExcellentProportionMale = Excellent.Male/sum(Excellent.Male))
df.fruitetable.sex = mutate(df.fruitetable.sex, ExcellentProportionFemale = Excellent.Female/sum(Excellent.Female))
df.fruitetable.sex = mutate(df.fruitetable.sex, VeryGoodProportionMale = VeryGood.Male/sum(VeryGood.Male))
df.fruitetable.sex = mutate(df.fruitetable.sex, VeryGoodProportionFemale = VeryGood.Female/sum(VeryGood.Female))
df.fruitetable.sex = mutate(df.fruitetable.sex, GoodProportionMale = Good.Male/sum(Good.Male))
df.fruitetable.sex = mutate(df.fruitetable.sex, GoodProportionFemale = Good.Female/sum(Good.Female))
df.fruitetable.sex = mutate(df.fruitetable.sex, fairProportionMale = Fair.Male/sum(Fair.Male))
df.fruitetable.sex = mutate(df.fruitetable.sex, fairProportionFemale = Fair.Female/sum(Fair.Female))
df.fruitetable.sex = mutate(df.fruitetable.sex, PoorProportionMale = Poor.Male/sum(Poor.Male))
df.fruitetable.sex = mutate(df.fruitetable.sex, PoorProportionFemale = Poor.Female/sum(Poor.Female))


