library(ggplot2)
library(dplyr)

### Load Data

load("~/MEGA/CURSOS_ONLINE/statistics_with_r_spec/week5/week5_project/brfss2013.RData")

df = brfss2013

### Male Vegetables Excellent
prop.beans.ex.male = df %>% 
      filter(fvbeans <= 299 & fvbeans >=201) %>%
      filter(genhlth == "Excellent") %>%
      filter(sex == "Male")

prop.beans.ex.male = as.data.frame(table(prop.beans.ex.male$fvbeans))
prop.beans.ex.male = as.data.frame(prop.beans.ex.male[1:10, 2])

### Female Vegetables Excellent
prop.beans.ex.female = df %>% 
      filter(fvbeans <= 299 & fvbeans >=201) %>%
      filter(genhlth == "Excellent") %>%
      filter(sex == "Female")

prop.beans.ex.female = as.data.frame(table(prop.beans.ex.female$fvbeans))
prop.beans.ex.female = as.data.frame(prop.beans.ex.female[1:10, 2])

### Male Vegetable Very Good
prop.beans.vergood.male = df %>% 
      filter(fvbeans <= 299 & fvbeans >=201) %>%
      filter(genhlth == "Very good") %>%
      filter(sex == "Male")

prop.beans.vergood.male = as.data.frame(table(prop.beans.vergood.male$fvbeans))
prop.beans.vergood.male = as.data.frame(prop.beans.vergood.male[1:10, 2])

### Female Vegetable Very Good
prop.beans.vergood.female = df %>% 
      filter(fvbeans <= 299 & fvbeans >=201) %>%
      filter(genhlth == "Very good") %>%
      filter(sex == "Female")

prop.beans.vergood.female = as.data.frame(table(prop.beans.vergood.female$fvbeans))
prop.beans.vergood.female = as.data.frame(prop.beans.vergood.female[1:10, 2])

### Male Vegetable Good
prop.beans.good.male = df %>% 
      filter(fvbeans <= 299 & fvbeans >=201) %>%
      filter(genhlth == "Good") %>%
      filter(sex == "Male")

prop.beans.good.male = as.data.frame(table(prop.beans.good.male$fvbeans))
prop.beans.good.male = as.data.frame(prop.beans.good.male[1:10, 2])

### Female Vegetable Good
prop.beans.good.female = df %>% 
      filter(fvbeans <= 299 & fvbeans >=201) %>%
      filter(genhlth == "Good") %>%
      filter(sex == "Female")

prop.beans.good.female = as.data.frame(table(prop.beans.good.female$fvbeans))
prop.beans.good.female = as.data.frame(prop.beans.good.female[1:10, 2])

### Male Vegetable Fair
prop.beans.fair.male = df %>% 
      filter(fvbeans <= 299 & fvbeans >=201) %>%
      filter(genhlth == "Fair") %>%
      filter(sex == "Male")

prop.beans.fair.male = as.data.frame(table(prop.beans.fair.male$fvbeans))
prop.beans.fair.male = as.data.frame(prop.beans.fair.male[1:10, 2])

### Female Vegetable Fair
prop.beans.fair.female = df %>% 
      filter(fvbeans <= 299 & fvbeans >=201) %>%
      filter(genhlth == "Fair") %>%
      filter(sex == "Female")

prop.beans.fair.female = as.data.frame(table(prop.beans.fair.female$fvbeans))
prop.beans.fair.female = as.data.frame(prop.beans.fair.female[1:10, 2])

### Male beansetable Poor
prop.beans.poor.male = df %>% 
      filter(fvbeans <= 299 & fvbeans >=201) %>%
      filter(genhlth == "Poor") %>%
      filter(sex == "Male")

prop.beans.poor.male = as.data.frame(table(prop.beans.poor.male$fvbeans))
prop.beans.poor.male = as.data.frame(prop.beans.poor.male[1:10, 2])

### Male beansetable Poor
prop.beans.poor.female = df %>% 
      filter(fvbeans <= 299 & fvbeans >=201) %>%
      filter(genhlth == "Poor") %>%
      filter(sex == "Female")

prop.beans.poor.female = as.data.frame(table(prop.beans.poor.female$fvbeans))
prop.beans.poor.female = as.data.frame(prop.beans.poor.female[1:10, 2])

### Adjust Data
times = c("1 time", "2 times", "3 times", "4 times", 
          "5 times", "6 times", "7 times", "8 times", 
          "9 times", "10 times")

colnames = c("Excellent.Male","Excellent.Female", 
             "VeryGood.Male", "VeryGood.Female",
             "Good.Male", "Good.Female",
             "Fair.Male", "Fair.Female",
             "Poor.Male", "Poor.Female")

df.beansetable.sex = cbind(prop.beans.ex.male,prop.beans.ex.female,
                           prop.beans.vergood.male, prop.beans.vergood.female,
                           prop.beans.good.male, prop.beans.good.female,
                           prop.beans.fair.male, prop.beans.fair.female,
                           prop.beans.poor.male, prop.beans.poor.female)

names(df.beansetable.sex) = colnames

### Analyzing Data
df.beansetable.sex = mutate(df.beansetable.sex, ExcellentProportionMale = Excellent.Male/sum(Excellent.Male))
df.beansetable.sex = mutate(df.beansetable.sex, ExcellentProportionFemale = Excellent.Female/sum(Excellent.Female))
df.beansetable.sex = mutate(df.beansetable.sex, VeryGoodProportionMale = VeryGood.Male/sum(VeryGood.Male))
df.beansetable.sex = mutate(df.beansetable.sex, VeryGoodProportionFemale = VeryGood.Female/sum(VeryGood.Female))
df.beansetable.sex = mutate(df.beansetable.sex, GoodProportionMale = Good.Male/sum(Good.Male))
df.beansetable.sex = mutate(df.beansetable.sex, GoodProportionFemale = Good.Female/sum(Good.Female))
df.beansetable.sex = mutate(df.beansetable.sex, fairProportionMale = Fair.Male/sum(Fair.Male))
df.beansetable.sex = mutate(df.beansetable.sex, fairProportionFemale = Fair.Female/sum(Fair.Female))
df.beansetable.sex = mutate(df.beansetable.sex, PoorProportionMale = Poor.Male/sum(Poor.Male))
df.beansetable.sex = mutate(df.beansetable.sex, PoorProportionFemale = Poor.Female/sum(Poor.Female))