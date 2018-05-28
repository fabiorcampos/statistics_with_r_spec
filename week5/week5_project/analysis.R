library(ggplot2)
library(dplyr)

### Select variables

df %>%
  select(genhlth, fruitju1, fruit1, fvgreen, vegetab1) %>%
  str()

### What the consume style of fruits and vegetables by health status?
df$fruitju1 = as.factor(df$fruitju1)
purejuice = df %>% 
  filter(fruitju1 == 101) 

prop.table(table(purejuice$fruitju1, purejuice$genhlth))

### understand the fruits and vegetables structure
df$fruit1 = as.factor(df$fruit1)
fruit = df %>% 
  filter(fruit1 == 101)

table(fruit$fruitju1, fruit$genhlth)
prop.table(table(fruit$fruitju1, fruit$genhlth))


#fruitju1: How Many Times Did You Drink 100 Percent Pure Fruit Juices?
  
table(df$fruitju1) 

#fruit1: How Many Times Did You Eat Fruit?

#fvbeans: How Many Times Did You Eat Beans Or Lentils?

#fvgreen: How Many Times Did You Eat Dark Green Vegetables?

#fvorang: How Many Times Did You Eat Orange-Colored Vegetables?

#vegetab1:How Many Times Did You Eat Other Vegetables?
