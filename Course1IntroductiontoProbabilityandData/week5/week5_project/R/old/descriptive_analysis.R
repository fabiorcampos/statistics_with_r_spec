library(ggplot2)
library(dplyr)

### Load Data

load("~/MEGA/CURSOS_ONLINE/statistics_with_r_spec/week5/week5_project/brfss2013.RData")

df = brfss2013

### Select variables
df %>%
      select(genhlth, fruitju1, fruit1, fvgreen, vegetab1) %>%
      str()

# Convert as factor 
df$genhlth = as.factor(df$genhlth)
df$fruitju1 = as.factor(df$fruitju1)
df$vegetab1 = as.factor(df$vegetab1)
df$fvgreen = as.factor(df$fvgreen)

# Fruit Analsys
fruit_analysis = df %>% 
      filter(genhlth == 'Excellent' | genhlth == 'Very good' | genhlth == 'Good' | genhlth == 'Fair' | genhlth == 'Poor') %>%
      filter(fruitju1 == 101 | fruitju1 == 102 | fruitju1 == 103 | fruitju1 == 555)

fruit_analysis = fruit_analysis[,c(19,59,77)]

# Vegetables analysis
veg_analysis = df %>% 
      filter(genhlth == 'Excellent' | genhlth == 'Very good' | genhlth == 'Good' | genhlth == 'Fair' | genhlth == 'Poor') %>%
      filter(vegetab1 == 101 | vegetab1 == 102 | vegetab1 == 555)

veg_analysis = veg_analysis[,c(19,59,82)]

# beens analysis
beens_analysis = df %>% 
      filter(genhlth == 'Excellent' | genhlth == 'Very good' | genhlth == 'Good' | genhlth == 'Fair' | genhlth == 'Poor') %>%
      filter(fvgreen == 101 | fvgreen == 102 | fvgreen == 555)

beens_analysis = beens_analysis[,c(19,59,80)]

# genhlth analysis
Percentage = prop.table(table(df$genhlth))
Frequencies = summary(df$genhlth)
Frequencies = Frequencies[1:5]
genhlth_df = data.frame(Frequencies, Percentage)
genhlth_df = genhlth_df[,-2]

# Foods analysis

### Juice Excellent
prop.fruits = df %>% 
      filter(fruitju1 <= 299 & fruitju1 >=201) %>%
      filter(genhlth == "Excellent")

prop.fruits = as.data.frame(table(prop.fruits$fruitju1))
`row.names<-`(prop.fruits, prop.fruits$Var1)

### Juice Poor
prop.fruits.poor = df %>% 
      filter(fruitju1 <= 299 & fruitju1 >=201) %>%
      filter(genhlth == "Poor")

prop.fruits.poor = as.data.frame(table(prop.fruits.poor$fruitju1))

### Veg
prop.veg = df %>% 
      filter(vegetab1 <= 199 & vegetab1 >=101)

prop.veg = as.data.frame(table(prop.veg$vegetab1))
`row.names<-`(prop.veg, prop.veg$Var1)

### Beens
prop.beens = df %>% 
      filter(fvgreen <= 199 & fvgreen >=101)

prop.beens = as.data.frame(table(prop.beens$fvgreen))
`row.names<-`(prop.beens, prop.beens$Var1)

sumfruit = sum(prop.fruits[c(1:3),2])/sum(prop.fruits[,2])
sumveg = sum(prop.veg[c(1:3),2])/sum(prop.veg[,2])
sumbeens = sum(prop.beens [c(1:3),2])/sum(prop.beens [,2])





