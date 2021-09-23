setwd("C:/Users/Johannes/Documents/Johannes/Data Science")
library(dplyr)
library(ggplot2)
library("tidyverse")
library(dslabs)
library(RColorBrewer)

#EXPLORING THE GAPMINDER DATASET
#Life expectancy vs fertility - part 1

data(gapminder)
## 1) fill out the missing parts in filter and aes
gapminder %>% filter(continent=="Africa" & year=="2012") %>%
  ggplot(aes(fertility , life_expectancy)) +
  geom_point()

# 2) Life expectancy vs fertility - part 2 - coloring your plot
gapminder %>% filter(year=="2012" & continent=="Africa") %>%
  ggplot(aes(fertility, life_expectancy, color=region)) +
  geom_point()

# 3) Life expectancy vs fertility - part 3 - selecting country and region
df <- gapminder %>% filter(year=="2012" & continent=="Africa") %>% 
  filter(fertility <= 3 & life_expectancy >= 70) %>%
  select(country, region)

# 4) Life expectancy and the Vietnam War - part 1
tab <- gapminder %>% filter(country %in% c("Vietnam", "United States") & year %in% seq(1960, 2010))

#5. Life expectancy and the Vietnam War - part 2
p <- tab %>% ggplot(aes(year, life_expectancy, color=country)) +
  geom_line()
p

#6. Life expectancy in Cambodia
gapminder %>% filter(country=="Cambodia" & year %in% seq(1960, 2010)) %>%
  ggplot(aes(year, life_expectancy))+ geom_line()

#7. Dollars per day - part 1
daydollars <- gapminder %>% filter(continent=="Africa" & year==2010) %>%
  na.omit() %>% mutate(dollar_per_day = gdp/population/365)

#8. Dollars per day - part 2
daydollars %>% ggplot(aes(dollars_per_day)) + geom_density() +
  scale_x_continuous(trans="log2")

#9. Dollars per day - part 3 - multiple density plots
gapminder %>%  mutate(dollars_per_day=gdp/population/365) %>% 
  filter(continent=="Africa" & year %in% c(1970, 2010) & !is.na(dollars_per_day)) %>%
  ggplot(aes(dollars_per_day)) + 
  geom_density() + scale_x_continuous(trans="log2")

#10. Dollars per day - part 4 - stacked density plot
gapminder %>% mutate(dollars_per_day=gdp/population/365) %>% 
  filter(year %in% c(1970, 2010) & continent=="Africa" & !is.na(dollars_per_day)) %>% 
  ggplot(aes(dollars_per_day, fill=region)) + 
  geom_density(bw=.5, position="stack") + scale_x_continuous(trans="log2") + 
  facet_grid(year~.)

#11. Infant mortality scatter plot - part 1
gapminder_Africa_2010 <- gapminder %>% mutate(dollars_per_day=gdp/population/365) %>% 
  filter(continent=="Africa" & year==2010 & !is.na(dollars_per_day))

gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day, infant_mortality, color=region)) + 
  geom_point()

#12. Infant mortality scatter plot - part 2 - logarithmic axis
gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day, infant_mortality, color=region)) +
  geom_point() + scale_x_continuous(trans="log2")

#13. Infant mortality scatter plot - part 3 - adding labels
gapminder_Africa_2010 %>% 
  ggplot(aes(dollars_per_day, infant_mortality, color=region, label=country)) +
  geom_point() + scale_x_continuous(trans="log2") + geom_text()

#14. Infant mortality scatter plot - part 4 - comparison of scatter plots
gapminder %>% na.omit() %>% mutate(dollars_per_day=gdp/population/365) %>% 
  filter(year %in% c(1970, 2010) & continent=="Africa") %>% 
  ggplot(aes(dollars_per_day, infant_mortality, color=region, label=country)) +
  geom_text() + scale_x_continuous(trans="log2") + facet_grid(year~.)

#DATA VISUALIZATION PRINCIPLES PART 2
#1: Customizing plots - watch and learn
data(us_contagious_diseases)
str(us_contagious_diseases)
dat <- us_contagious_diseases %>%
  filter(year == 1967 & disease=="Measles" & !is.na(population)) %>% 
  mutate(rate = count / population * 10000 * 52 / weeks_reporting)
state <- reorder(dat$state, dat$rate) 
rate <- dat$count/(dat$population/10000)*(52/dat$weeks_reporting)
levels(state)

#2: Customizing plots - redefining
dat <- us_contagious_diseases %>% 
  filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>%
  mutate(state = reorder(state, rate))
dat %>% ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip()

#3: Making a box plot
data("murders")
murders %>% mutate(rate = total/population*100000) %>% 
  mutate(region=reorder(region, rate, FUN=median)) %>% 
  ggplot(aes(region, rate)) + geom_boxplot() + geom_point()

#DATA VISUALIZATION PRINCIPLES PART 3
#1: Tile plot - measles and smallpox
data(us_contagious_diseases)

the_disease = "Smallpox"
dat <- us_contagious_diseases %>% 
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & !weeks_reporting<10) %>% 
  mutate(rate = count / population * 10000) %>% 
  mutate(state = reorder(state, rate))

dat %>% ggplot(aes(year, state, fill = rate)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  theme_minimal() + 
  theme(panel.grid = element_blank()) + 
  ggtitle(the_disease) + 
  ylab("") + 
  xlab("")

#2. Time series plot - measles and smallpox
the_disease = "Smallpox"
dat <- us_contagious_diseases %>%
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting>=10) %>%
  mutate(rate = count / population * 10000) %>%
  mutate(state = reorder(state, rate))

avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>% ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")

#3: Time series plot - all diseases in California

data(us_contagious_diseases)
head(us_contagious_diseases)
us_contagious_diseases %>% filter(state=="California" & weeks_reporting>=10) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color=disease)) + 
  geom_line()

#4: Time series plot - all diseases in the United States

data(us_contagious_diseases)
names(us_contagious_diseases)
us_contagious_diseases %>% filter(!is.na(population)) %>% 
  group_by(year, disease) %>% summarize(rate=sum(count)/sum(population)*10^3) %>%
  ggplot(aes(year, rate, color=disease)) + geom_line()
