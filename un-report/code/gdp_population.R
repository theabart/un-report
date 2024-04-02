#Load package by library(name)
library(tidyverse)
gapminder_1997 <- read_csv(file ="un-report/data/gapminder_1997.csv")

add_two <- 2+2

cat_name <- "Krieger"

cat_name <- "Cyril"

(?name)

read_csv()
Sys.Date()
Sys.Time()
getwd()
?read_csv()


sum(5,6,7,8)
sum(5,6)
?round
round(3.1415)
round(x=3.1415,digits=3)
round(digits=3,x=3.1415)


library(readxl)



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
gapminder_data <-read_csv("un-report/gapminder_data.csv")

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
  




