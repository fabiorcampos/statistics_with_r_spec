library(ggplot2)
library(dplyr)

### Load Data

load("~/MEGA/CURSOS_ONLINE/statistics_with_r_spec/week5/week5_project/brfss2013.RData")

df = brfss2013

### Select variables

df %>%
  select(genhlth, fruitju1, fruit1, fvgreen, vegetab1) %>%
  str()

### What the consume style of fruits and vegetables by health status?
df$fruitju1 = as.factor(df$fruitju1)
purejuice1 = df %>% 
      filter(fruitju1 == 101)

purejuice2 = df %>% 
      filter(fruitju1 == 102)

purejuice3 = df %>% 
      filter(fruitju1 == 103)

a = prop.table(table(purejuice1$fruitju1, purejuice1$genhlth))
b = prop.table(table(purejuice2$fruitju1, purejuice2$genhlth))
c = prop.table(table(purejuice3$fruitju1, purejuice3$genhlth))

a = a[3,1:5]
b = b[4,1:5]
c = c[5,1:5]

times_p_day = c("1 time","2 time","3 time")

one = as.data.frame(a)
two = as.data.frame(b)
three = as.data.frame(c)

juice = data.frame("One" = one, "Two" = two, "Three" = three, row.names = c("Excellent", "Very Good",
                                                      "Good", "Fair", "Poor"))

juice = as.data.frame(t(juice))
juice = data.frame(juice, row.names = c("One Juice", "Two Juices", "Three Juices"))

summary(juice)

### understand the fruits and vegetables structure
df$fruit1 = as.factor(df$fruit1)
fruit1 = df %>% 
      filter(fruit1 == 101)

fruit2 = df %>% 
      filter(fruit1 == 102)

fruit3 = df %>% 
      filter(fruit1 == 103)

fa = prop.table(table(fruit1$fruit1, fruit1$genhlth))
fb = prop.table(table(fruit2$fruit1, fruit2$genhlth))
fc = prop.table(table(fruit3$fruit1, fruit3$genhlth))

fa = fa[4,1:5]
fb = fb[5,1:5]
fc = fc[6,1:5]

times_p_day = c("1 time","2 time","3 time")

one_f = as.data.frame(fa)
two_f = as.data.frame(fb)
three_f = as.data.frame(fc)

fruit = data.frame("One" = one_f, "Two" = two_f, "Three" = three_f, row.names = c("Excellent", "Very Good",
                                                                            "Good", "Fair", "Poor"))

fruit = as.data.frame(t(fruit))
fruit = data.frame(fruit, row.names = c("One fruit", "Two fruits", "Three fruits"))

#fvbeans: How Many Times Did You Eat Beans Or Lentils?

df$fvgreen = as.factor(df$fvgreen)
fvgreen1 = df %>% 
      filter(fvgreen == 101)

fvgreen2 = df %>% 
      filter(fvgreen == 102)

fvgreen3 = df %>% 
      filter(fvgreen == 103)

ga = prop.table(table(fvgreen1$fvgreen, fvgreen1$genhlth))
gb = prop.table(table(fvgreen2$fvgreen, fvgreen2$genhlth))
gc = prop.table(table(fvgreen3$fvgreen, fvgreen3$genhlth))

ga = ga[4,1:5]
gb = gb[5,1:5]
gc = gc[6,1:5]

times_p_day = c("1 time","2 time","3 time")

one_g = as.data.frame(ga)
two_g = as.data.frame(gb)
three_g = as.data.frame(gc)

green = data.frame("One" = one_g, "Two" = two_g, "Three" = three_g, row.names = c("Excellent", "Very Good",
                                                                            "Good", "Fair", "Poor"))

green = as.data.frame(t(green))
green = data.frame(green, row.names = c("One greens", "Two green", "Three green"))

#fvgreen: How Many Times Did You Eat Dark Green Vegetables?

df$vegetab1 = as.factor(df$vegetab1)
vegetab1 = df %>% 
      filter(vegetab1 == 101)

vegetab2 = df %>% 
      filter(vegetab1 == 102)

vegetab3 = df %>% 
      filter(vegetab1 == 103)

va = prop.table(table(vegetab1$vegetab1, vegetab1$genhlth))
vb = prop.table(table(vegetab2$vegetab1, vegetab2$genhlth))
vc = prop.table(table(vegetab3$vegetab1, vegetab3$genhlth))

va = va[4,1:5]
vb = vb[5,1:5]
vc = vc[6,1:5]

times_p_day = c("1 time","2 time","3 time")

one_v = as.data.frame(va)
two_v = as.data.frame(vb)
three_v = as.data.frame(vc)

veg = data.frame("One" = one_v, "Two" = two_v, "Three" = three_v, row.names = c("Excellent", "Very Good",
                                                                                  "Good", "Fair", "Poor"))

veg = as.data.frame(t(veg))
veg = data.frame(veg, row.names = c("One vegetables", "Two vegetables", "Three vegetables"))

df_health = rbind(juice,fruit, veg, green)

