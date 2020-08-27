#Following Code is from https://www.youtube.com/watch?v=ANMuuq502rE

#install.packages("gapminder")
library(gapminder)
data("gapminder")

summary(gapminder)
x <- mean(gapminder$gdpPercap)

attach(gapminder)
median(pop)
hist(lifeExp)
hist(log(pop))
boxplot(lifeExp ~ continent)
plot(lifeExp ~ log(gdpPercap))

#T Test
#install.packages("dplyr")
library(dplyr)
# pipe operator %>%; allows you to select variables of intesterst and filter out rows
gapminder %>%
  select(country, lifeExp) %>%
  filter(country == "South Africa" | 
           country == "Ireland") %>%
  group_by(country) %>%
  summarise(Average_life = mean(lifeExp))

#conduct T test 
df1 <- gapminder %>%
  select(country, lifeExp) %>%
  filter(country == "South Africa" | 
           country == "Ireland")
t.test(data = df1, lifeExp ~ country)
#NullHypothesis = there is a 0 difference in life expectancy
#If Null is true, what are the chances (p-value) that from our sample we get 
#our observed 20year life expectancy difference? = p-value = 4.466e-09; very close to
#zero therefore we can reject the NULL and go w/ 85 % confidece interval = 15.07022 22.97794


library(ggplot2)

gapminder %>%
  filter(gdpPercap < 50000) %>%
  ggplot(aes(x = log(gdpPercap), y = lifeExp, col = year, size = pop)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = lm) + #lm gives you the line of best fit
  facet_wrap(~continent) #splits the single graph into multiple based on the variable (continent)

#Linear Regression = represent model with a line of best fit
summary(lm(lifeExp ~ gdpPercap))

#p-value = <2e-16 = probability that the slope is zero = NULL; since p is very small
#we can conclude that the NULL is wrong and not zero = coefficent is statistically significant

summary(lm(lifeExp ~ gdpPercap + pop))
#p-value = 4.72e-05





