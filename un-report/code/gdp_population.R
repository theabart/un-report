#######################################################################
## Setup section
#######################################################################
#Load package by library(name)
library(tidyverse)
gapminder_1997 <- read_csv(file ="un-report/data/gapminder_1997.csv")

add_two <- 2+2

cat_name <- "Krieger"

cat_name <- "Cyril"






sum(5,6,7,8)
sum(5,6)

round(3.1415)
round(x=3.1415,digits=3)
round(digits=3,x=3.1415)


library(readxl)

#####################################################
## Day 1 work: Intro to R and plotting
#####################################################


ggplot(data=gapminder_1997)+

  #add variables to axes
  aes(x=gdpPercap,y=lifeExp)+
  #add pretty labels to axes
  labs(y="Life Expectancy",x="GDP per Capita")+
  #add actual data as a scatter plot
  geom_point()+
  #give title to plot
  labs(title="Do people in wealthy countries live longer?")+
  #change colors to match countries
  aes(color = continent)+
  #change the color to a different one
  scale_color_brewer(palette="Set3")+
  #center the title text
  theme(plot.title=element_text(hjust=0.5))+
  #geom_point(aes(size = pop))
  #different way to change the size of the points to reflect population
  aes(size = pop/1000000)+
  labs(size = "Population (in millions)")+
  #aes(size = continent)
  theme_minimal()
  
  


  

#list all pallettes
RColorBrewer::display.brewer.all()

#load in full gapminder dataset
gapminder_data <-read_csv("un-report/data/gapminder_data.csv")

#useful ways to view very large datasets
dim(gapminder_data)
#summary of data
head(gapminder_data)
#brief summary of info
glimpse(gapminder_data)


ggplot(data = gapminder_data)+
  aes(x = year, y=lifeExp, color = continent,
      group = country)+
  geom_line(alpha = 0.5)+
  theme_classic()

#Plot life expectancy vs pop

ggplot(data = gapminder_data)+
  aes(x = pop, y = lifeExp, color = continent, group = country)+
  geom_line()


ggplot(data = gapminder_data)+
  aes(x = year, y = pop, color = continent, group = country)+
  geom_line()



#making a box and whisker plot, distribution of life expectancy per continent
ggplot(data = gapminder_data)+
  aes(x = continent, y = lifeExp)+
  geom_boxplot()



#newer plot to show distributions, violin plot
ggplot(data = gapminder_data)+
  aes(x = continent, y = lifeExp)+
  geom_violin()


#newer plot to show distributions, violin plot add datasets on top of each other
ggplot(data = gapminder_data)+
  aes(x = continent, y = lifeExp)+
  geom_violin()+
  geom_jitter(width = 0.05, alpha =0.2)
  
ggsave("un-report/figures/another_plot.png")

#####################################################################
## Day 2 Work: Data Manipulation and Cleaning


glimpse(gapminder_data)
#We want to find a brief summary of the data in our file

summarize(gapminder_data, avgLifeExp=mean(lifeExp))

summarize(gapminder_data, maxLifeExp=max(lifeExp),avgLifeExp=mean(lifeExp),
          minLifeExp=min(lifeExp))

#pipes
#%>%
#means "and then do this thing"

# option 1 without a pipe
summarize(gapminder_data, avgLifeExp=mean(lifeExp))
#option 2 with a pipe
gapminder_data %>% summarize(avgLifeExp = mean(lifeExp))

#save the summary table
gapminder_data_summary <- gapminder_data %>% 
  summarize(avgLifeExp = mean(lifeExp))

#filtering data
gapminder_data %>% filter(year==2007) %>%
  summarize(avgLifeExp07 = mean(lifeExp))

#find the earliest year in the data set using summarize and min
summarize(gapminder_data, earliestYear=min(year))
gapminder_data %>% summarize(startpoint =min(year))

#find the avg GPD per cap for 1952
gapminder_data %>% filter(year==1952) %>% 
  summarize(avgGDP1952 = mean(gdpPercap))

#calculate life expectancy by year
gapminder_data %>% group_by(year) %>% 
  summarize(avg = mean(lifeExp)) 

#calculate life expectancy by continent
gapminder_data %>% group_by(continent) %>% 
  summarize(avg = mean(lifeExp),
            min(lifeExp),
            max(lifeExp))

#adding columns to dataset using mutate
gapminder_data %>% mutate(gdp = gdpPercap*pop)
#View(gapminder_data) <- shows the file

#we have a column called pop
#use mutate to create a col for popInMillions
gapminder_data %>% mutate(popInMillions = pop/1000000)

#filter to select rows
#select will select based on cols
gapminder_data %>% select(pop, year)
#"negatively"select, exclude specific columns
gapminder_data %>% select(-continent)


#print a dataframe with only country, continent,
#year, and lifeExp
gapminder_data %>% select(country, continent, year, lifeExp)
#helper functions, starts with is useful if you don't remember exact
#names of cols etc
gapminder_data %>% select(year, starts_with("c"))

#print a dataframe of all cols that end in the letter "p"
gapminder_data %>% select(ends_with("p"))


#"pivot tables in excel" change the shape of data 
gapminder_data %>% select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)

#save a dataframe that contains only the Americas in 2007
#& tells the funtion to satisfy both conditions
#logical symbols : & is and | is for OR
gapminder_data_2007 <- gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent)
#by naming the function with the "<-" symbol, it will 
#create the new file

#cleaning messy data
####"tidy" = conforming to specific parameters
co2_emmisions_dirty <- read.csv(file = "un-report/data/co2-un-data.csv", skip=2,
         col.names = c("region","country","year", 
                       "series","value", "footnotes","source"))
#because there was no "<-" this file is still not actually in the environment

glimpse(co2_emmisions_dirty)

#select the country, year, series, and value columns
co2_emmisions_dirty %>% select(country, year, series, value)

co2_emmisions_dirty %>% select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" 
                         = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)"
                         = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value)

##> unique(co2_emmisions_dirty$series)
##[1] "Emissions (thousand metric tons of carbon dioxide)"  
#####[2] "Emissions per capita (metric tons of carbon dioxide)"
#### gives unique values from a given column, useful when info is not well known

#filter out data from 2005 and drop the year column
co2_emissions_2005 <- co2_emmisions_dirty %>% select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" 
                         = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)"
                         = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year)

#previously we created gapminder for 2007
#now we have co2 emissions for 2005

#Google how to join 2 data frames using tidyverse
#gonna talk about inner-join and anti-join
#inner join, order of data frames matters less
inner_join(gapminder_data,co2_emissions, by = "country")


anti_join(gapminder_data, co2_emissions, by = "country")
View(gapminder_data)
View(co2_emissions)


#there are issues with names not matching and having different countries in the
#two data sets

co2_emissions <- read_csv("un-report/data/co2-un-data.csv", skip=2,
         col_names = c("region","country","year", 
                       "series","value", "footnotes","source")) %>% 
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" 
                         = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)"
                         = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value)%>%
  filter(year == 2005) %>%
  select(-year) %>%
  mutate(country = recode(country, 
                          "Bolivia (Plurin. State of)" = "Bolivia",
                          "United States of America" = "United States",
                          "Venezuela (Boliv. Rep. of)" = "Venezuela"))
anti_join(gapminder_data, co2_emissions, by = "country")


unique(gapminder_data$country)
unique(co2_emissions$country)

#address Puerto Rico
gapminder_data <- gapminder_data %>% mutate(country = recode(country, 
                                           "Puerto Rico" = "United States"))
gapminder_co2 <- inner_join(gapminder_data, co2_emissions, by = "country")
glimpse(gapminder_co2)


gapminder_co2 %>% group_by(continent) %>% 
  summarize(avg = mean(lifeExp))



gapminder_co2 %>%
  filter(continent == "Americas") %>%
  mutate(region = ifelse(country == "United States"| 
                           country == "Canada" | 
                           country == "Mexico", 
                         "north", "south")) %>% View()



gapminder_co2 <- gapminder_co2 %>%
  filter(continent == "Americas" & year == 2007)

#write out new clean dataset as a csv
write.csv(gapminder_co2, "un-report/data/gapminder_co2.csv")

##################################################################
#Post lunch re-start - Writing Reports with R Markdown
###############################################################

















