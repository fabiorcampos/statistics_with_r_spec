library(ggplot2)
library(dplyr)

### Load Data

load("~/MEGA/CURSOS_ONLINE/statistics_with_r_spec/week5/week5_project/brfss2013.RData")

df = brfss2013

### Male Vegetables Excellent
prop.veg.ex.male = df %>% 
      filter(vegetab1 <= 299 & vegetab1 >=201) %>%
      filter(genhlth == "Excellent") %>%
      filter(sex == "Male")

prop.veg.ex.male = as.data.frame(table(prop.veg.ex.male$vegetab1))
prop.veg.ex.male = as.data.frame(prop.veg.ex.male[1:10, 2])

### Female Vegetables Excellent
prop.veg.ex.female = df %>% 
      filter(vegetab1 <= 299 & vegetab1 >=201) %>%
      filter(genhlth == "Excellent") %>%
      filter(sex == "Female")

prop.veg.ex.female = as.data.frame(table(prop.veg.ex.female$vegetab1))
prop.veg.ex.female = as.data.frame(prop.veg.ex.female[1:10, 2])

### Male Vegetable Very Good
prop.veg.vergood.male = df %>% 
      filter(vegetab1 <= 299 & vegetab1 >=201) %>%
      filter(genhlth == "Very good") %>%
      filter(sex == "Male")

prop.veg.vergood.male = as.data.frame(table(prop.veg.vergood.male$vegetab1))
prop.veg.vergood.male = as.data.frame(prop.veg.vergood.male[1:10, 2])

### Female Vegetable Very Good
prop.veg.vergood.female = df %>% 
      filter(vegetab1 <= 299 & vegetab1 >=201) %>%
      filter(genhlth == "Very good") %>%
      filter(sex == "Female")

prop.veg.vergood.female = as.data.frame(table(prop.veg.vergood.female$vegetab1))
prop.veg.vergood.female = as.data.frame(prop.veg.vergood.female[1:10, 2])

### Male Vegetable Good
prop.veg.good.male = df %>% 
      filter(vegetab1 <= 299 & vegetab1 >=201) %>%
      filter(genhlth == "Good") %>%
      filter(sex == "Male")

prop.veg.good.male = as.data.frame(table(prop.veg.good.male$vegetab1))
prop.veg.good.male = as.data.frame(prop.veg.good.male[1:10, 2])

### Female Vegetable Good
prop.veg.good.female = df %>% 
      filter(vegetab1 <= 299 & vegetab1 >=201) %>%
      filter(genhlth == "Good") %>%
      filter(sex == "Female")

prop.veg.good.female = as.data.frame(table(prop.veg.good.female$vegetab1))
prop.veg.good.female = as.data.frame(prop.veg.good.female[1:10, 2])

### Male Vegetable Fair
prop.veg.fair.male = df %>% 
      filter(vegetab1 <= 299 & vegetab1 >=201) %>%
      filter(genhlth == "Fair") %>%
      filter(sex == "Male")

prop.veg.fair.male = as.data.frame(table(prop.veg.fair.male$vegetab1))
prop.veg.fair.male = as.data.frame(prop.veg.fair.male[1:10, 2])

### Female Vegetable Fair
prop.veg.fair.female = df %>% 
      filter(vegetab1 <= 299 & vegetab1 >=201) %>%
      filter(genhlth == "Fair") %>%
      filter(sex == "Female")

prop.veg.fair.female = as.data.frame(table(prop.veg.fair.female$vegetab1))
prop.veg.fair.female = as.data.frame(prop.veg.fair.female[1:10, 2])

### Male vegetable Poor
prop.veg.poor.male = df %>% 
      filter(vegetab1 <= 299 & vegetab1 >=201) %>%
      filter(genhlth == "Poor") %>%
      filter(sex == "Male")

prop.veg.poor.male = as.data.frame(table(prop.veg.poor.male$vegetab1))
prop.veg.poor.male = as.data.frame(prop.veg.poor.male[1:10, 2])

### Male vegetable Poor
prop.veg.poor.female = df %>% 
      filter(vegetab1 <= 299 & vegetab1 >=201) %>%
      filter(genhlth == "Poor") %>%
      filter(sex == "Female")

prop.veg.poor.female = as.data.frame(table(prop.veg.poor.female$vegetab1))
prop.veg.poor.female = as.data.frame(prop.veg.poor.female[1:10, 2])

### Adjust Data
times = c("1 time", "2 times", "3 times", "4 times", 
          "5 times", "6 times", "7 times", "8 times", 
          "9 times", "10 times")

colnames = c("Excellent.Male","Excellent.Female", 
             "VeryGood.Male", "VeryGood.Female",
             "Good.Male", "Good.Female",
             "Fair.Male", "Fair.Female",
             "Poor.Male", "Poor.Female")

df.vegetable.sex = cbind(prop.veg.ex.male,prop.veg.ex.female,
                         prop.veg.vergood.male, prop.veg.vergood.female,
                         prop.veg.good.male, prop.veg.good.female,
                         prop.veg.fair.male, prop.veg.fair.female,
                         prop.veg.poor.male, prop.veg.poor.female)

names(df.vegetable.sex) = colnames

### Analyzing Data
df.vegetable.sex = mutate(df.vegetable.sex, ExcellentProportionMale = Excellent.Male/sum(Excellent.Male))
df.vegetable.sex = mutate(df.vegetable.sex, ExcellentProportionFemale = Excellent.Female/sum(Excellent.Female))
df.vegetable.sex = mutate(df.vegetable.sex, VeryGoodProportionMale = VeryGood.Male/sum(VeryGood.Male))
df.vegetable.sex = mutate(df.vegetable.sex, VeryGoodProportionFemale = VeryGood.Female/sum(VeryGood.Female))
df.vegetable.sex = mutate(df.vegetable.sex, GoodProportionMale = Good.Male/sum(Good.Male))
df.vegetable.sex = mutate(df.vegetable.sex, GoodProportionFemale = Good.Female/sum(Good.Female))
df.vegetable.sex = mutate(df.vegetable.sex, fairProportionMale = Fair.Male/sum(Fair.Male))
df.vegetable.sex = mutate(df.vegetable.sex, fairProportionFemale = Fair.Female/sum(Fair.Female))
df.vegetable.sex = mutate(df.vegetable.sex, PoorProportionMale = Poor.Male/sum(Poor.Male))
df.vegetable.sex = mutate(df.vegetable.sex, PoorProportionFemale = Poor.Female/sum(Poor.Female))





