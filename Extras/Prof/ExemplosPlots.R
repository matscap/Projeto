library(COVID19)

### time series
### accumated by day or data aggregate by day
### Country, State or County levels

### Date from 1 jan 2020
### level 1: country
### level 2: state
### level 3: county or city
### Lat/Lon
### population

### Covid measures: tests, confirmed, recovered, deaths, vaccines


########

library(dplyr)
CovidDF <- covid19(start = "2021-04-14", end = "2021-04-14")
CovidDF <- CovidDF %>% 
  select(id,
         administrative_area_level_1, # this is Country
         date,
         population,
         confirmed,
         recovered,
         deaths,
         vaccines) %>% 
  rename(Country = administrative_area_level_1)


### Create new variables to get comparable data

CovidDF <- CovidDF %>% 
  mutate(ConfirmedCasesPercent = round( (confirmed/population)*100, 2),
         RecoveredPercent = round((recovered/population)*100, 2),
         DeathsTotalPercent = round((deaths/population)*100, 2),
         PopulationVaccinatedPercent = round((vaccines/population)*100, 2))

### vaccines percentage superior 100% because of the two doses

########
########################
#### Visualization
library(COVID19)
library(dplyr)
library(ggplot2)
library(plotly)

rm(list = ls())

# one day Data 
CovidDf <- covid19(start = "2021-04-14", end = "2021-04-14")

# Select useful column 
CovidDf <- CovidDf %>% 
  select(id,
         administrative_area_level_1, # this is Country
         date,
         population,
         confirmed,
         recovered,
         deaths,
         vaccines) %>% 
  rename(Country = administrative_area_level_1)

# Top 25 Countries based in Confirmed cases count
Df.plot <- CovidDf %>%
  ungroup() %>% 
  mutate(ConfirmedCasesPercent = round((confirmed/population)*100, 2),
         RecoveredPercent      = round((recovered/population)*100, 2),
         DeathTollPercent      = round((deaths/population)*100, 2),
         PopulationVaccinatedPercent = round((vaccines/population)*100, 2)) %>%  
  arrange(-confirmed) %>% 
  slice_head(n = 25)

p1 <- ggplot(Df.plot,
             aes(x = id,
                 y = confirmed))+
  geom_point(aes(color = id,
                 size = confirmed)) + # bubble size
  labs(title = 'Top 25 countries for confirmed cases',
       x= '',                
       y = 'Confirmed cases' ) +
  theme_minimal()+
  theme(legend.position = "none")


plot(p1)
# New Measure (PopulationVaccinatedPercent)
Df.plot <- CovidDf %>%
  ungroup() %>% 
  mutate(ConfirmedCasesPercent = round((confirmed/population)*100, 2),
         RecoveredPercent      = round((recovered/population)*100, 2),
         DeathTollPercent      = round((deaths/population)*100, 2),
         PopulationVaccinatedPercent = round((vaccines/population)*100, 2)) %>%  
  arrange(-PopulationVaccinatedPercent) %>% 
  slice_head(n = 25)

p3 <- ggplot(Df.plot,
             aes(x = id,
                 y = PopulationVaccinatedPercent))+
  geom_point(aes(color = id,
                 size = PopulationVaccinatedPercent)) + # bubble size
  labs(title = 'Top 25 countries for Population Vaccinated Percent',
       x= '',                
       y = 'Population Vaccinated %' ) +
  theme_minimal()+
  theme(legend.position = "none")


plot(p3)



p2 <- ggplot(Df.plot,
             aes(x = id,
                 y = confirmed))+
  geom_bar(stat="identity",
           fill="steelblue") + # bar size
  labs(title = 'Top 25 countries for confirmed cases',
       x= '',                
       y = 'Confirmed cases' ) +
  theme(legend.position = "none") +
  theme_minimal()

plot(p2)



# Dynamic plot using plotly
ggplotly(p1) # Dynamic, Make sure plotly library is loaded


# store plot as pdf

pdf(file = "Data/Top25countiesBarAndBubbleCharts.pdf",
    width = 14,
    height = 10)

plot(p1)
plot(p2)
plot(p3)

dev.off()



### Bar charts and bubble charts

# Top 25 Countries based in Confirmed cases count
Df.plot <- CovidDF %>%
  ungroup() %>% 
  mutate(ConfirmedCasesPercent = round((confirmed/population)*100, 2),
         RecoveredPercent      = round((recovered/population)*100, 2),
         DeathTollPercent      = round((deaths/population)*100, 2),
         PopulationVaccinatedPercent = round((vaccines/population)*100, 2)) %>%  
  arrange(-confirmed) %>% 
  slice_head(n = 25)
### Create bubble plot
p1 <- ggplot(Df.plot,
             aes(x = id,
                 y = confirmed))+
  geom_point(aes(color = id,
                 size = confirmed)) + # bubble size
  labs(title = 'Top 25 countries for confirmed cases',
       x= '',                
       y = 'Confirmed cases' ) +
  theme_minimal()+
  theme(legend.position = "none")


plot(p1)
# New Measure (PopulationVaccinatedPercent)
Df.plot <- CovidDf %>%
  ungroup() %>% 
  mutate(ConfirmedCasesPercent = round((confirmed/population)*100, 2),
         RecoveredPercent      = round((recovered/population)*100, 2),
         DeathTollPercent      = round((deaths/population)*100, 2),
         PopulationVaccinatedPercent = round((vaccines/population)*100, 2)) %>%  
  arrange(-PopulationVaccinatedPercent) %>% 
  slice_head(n = 25)

p3 <- ggplot(Df.plot,
             aes(x = id,
                 y = PopulationVaccinatedPercent))+
  geom_point(aes(color = id,
                 size = PopulationVaccinatedPercent)) + # bubble size
  labs(title = 'Top 25 countries for Population Vaccinated Percent',
       x= '',                
       y = 'Population Vaccinated %' ) +
  theme_minimal()+
  theme(legend.position = "none")


plot(p3)



p2 <- ggplot(Df.plot,
             aes(x = id,
                 y = confirmed))+
  geom_bar(stat="identity",
           fill="steelblue") + # bar size
  labs(title = 'Top 25 countries for confirmed cases',
       x= '',                
       y = 'Confirmed cases' ) +
  theme(legend.position = "none") +
  theme_minimal()

plot(p2)



# Dynamic plot using plotly
ggplotly(p1) # Dynamic, Make sure plotly library is loaded


# store plot as pdf

pdf(file = "Data/Top25countiesBarAndBubbleCharts.pdf",
    width = 14,
    height = 10)

plot(p1)
plot(p2)
plot(p3)

dev.off()


##############
##################

### automate the procedure


library(COVID19)
library(dplyr)
library(ggplot2)
library(plotly)

rm(list = ls())

CovidDf <- covid19(start = "2021-04-14", end = "2021-04-14")

# Select useful columns 
CovidDf <- CovidDf %>% 
  select(id,
         administrative_area_level_1,
         date,
         population,
         confirmed,
         recovered,
         deaths,
         vaccines) %>% 
  mutate(ConfirmedCasesPercent        = round((confirmed/population)*100, 2),
         RecoveredPercent             = round((recovered/population)*100, 2),
         DeathTollPercent             = round((deaths/population)*100, 2),
         PopulationVaccinatedPercent  = round((vaccines/population)*100, 2)) %>% 
  rename(Country = administrative_area_level_1) %>% 
  filter(!id %in% c('CAC',
                    'VAT',
                    'MSZ',
                    'GPC',
                    'DPC')) # Removing Cruises!


Measures <- names(CovidDf)[5:ncol(CovidDf)]

setwd("~/Desktop/LCC_PROJETO")

pdf(file = "Data_Visualizations.pdf",
    width = 14
    , height = 12)

for (i in 1:length(Measures)){
  # Filter data
  MeasureForPloting <- Measures[i]
  
  Df.plot <- CovidDf[ , c('id', MeasureForPloting)]
  
  Df.plot <- Df.plot[order(Df.plot[ ,2],
                           decreasing = TRUE ),  ]
  Df.plot <- Df.plot[1:25, ] # Top 25
  
  # plot
  p <- ggplot(Df.plot, 
              mapping = aes_string(x = names(Df.plot)[1],
                                   y = names(Df.plot)[2])) +
    geom_point(aes(color = id), size = 3) + # bubble size
    labs(title = paste('Top 25 countries for', MeasureForPloting, 'on', max(CovidDf$date)),
         x= '',                
         y = MeasureForPloting ) +
    theme(legend.position = "none",
          axis.text.x = element_text(size=12, face="bold")   
    )
  plot(p)
} 

dev.off()

####################
######################
###########################
##################

# Set up: Install/load various required packages
pkg_reqd <- list('dplyr',            # Data wrangling
                 'COVID19',          # Access Covid19 data
                 'ggplot2',          # Data visualization
                 'gganimate',        # animation plot
                 'gifski',           # Create gif
                 'png',              # Create png
                 'av')               # encode multiple images to video

# Check if pkg is present on machine and if not then install it
install.packages(as.character(pkg_reqd[!pkg_reqd %in% installed.packages()[, 'Package']]))

# load all required libraries
sapply(pkg_reqd, require, character.only = TRUE)

rm(list = ls())
# Country level

# Data download
Countries <- covid19() %>% 
  rename(Country = administrative_area_level_1) %>% 
  filter(!id %in% c('CAC',
                    'VAT',
                    'MSZ',
                    'GPC',
                    'SMR',
                    'DPC')) # Removing Cruises!

# Total cases for top 25 Countries

# select top 35 countries with covid cases

top_35Counties <- Countries %>% 
  group_by(Country) %>% 
  summarise(maxconfirmed = max(confirmed,  na.rm = TRUE)) %>% 
  top_n(35, wt = maxconfirmed)


p <- ggplot(Countries %>% 
              filter(Country %in% top_25Counties$Country), 
            aes(x = Country,
                y = confirmed,
                label = '',
                col = Country)) +
  geom_point(stat = "identity", 
             size = 3) +
  geom_segment(aes(y = 0,
                   x = Country,
                   yend = confirmed,
                   xend = Country)) +
  geom_text(color = 'black',
            size = 3,
            nudge_x = 0.7) +
  coord_flip() +
  ggtitle("Covid-19 confirmed cases across top 35 countries",
          subtitle = '{frame_time}') +
  theme(legend.position = "none",
        axis.title = element_text(size = 20,
                                  face = 'bold'),
        plot.subtitle = element_text(size = 20,
                                     face = "bold",
                                     color = 'red'),
        axis.text = element_text(size = 10,
                                 face = "bold"),
        axis.text.x = element_text(size = 12,
                                   face = "bold")) +
  labs(x = '',
       y = 'Covid-19 Confirmed Cases',
       caption =  paste0("Data Credit: ",
                         "https://covid19datahub.io")) +
  transition_time(date) +
  ease_aes('cubic-in-out')

# As gif
animate(p,
        fps = 3, 
        renderer = gifski_renderer(loop = F),
        end_pause = 10,
        width = 850,
        height = 900)

anim_save("animations_perCapita_Top35CountriesCovid19ConfirmedTotal.gif")

# As movie
p2 <- animate(p,
              fps = 3,
              renderer = av_renderer(),
              width = 850,
              height = 900)

anim_save("animations_perCapita_Top25CountriesCovid19ConfirmedTotal.mp4", p2)



# per 10 K for top 25 Countries
df2 <- Countries %>%  
  mutate(confirmed_per_10k = (confirmed/population)*10000)

# select top 25 countries with covid cases

top_25Counties <- df2 %>% 
  group_by(Country) %>% 
  summarise(max_cases = max(confirmed_per_10k,  na.rm = TRUE)) %>% 
  top_n(25, wt = max_cases)


p <- ggplot(df2 %>% 
              filter(Country %in% top_25Counties$Country), 
            aes(x = Country,
                y = confirmed_per_10k,
                label = '',
                col = Country)) +
  geom_point(stat = "identity", 
             size = 3) +
  geom_segment(aes(y = 0,
                   x = Country,
                   yend = confirmed_per_10k,
                   xend = Country)) +
  geom_text(color = 'black',
            size = 3,
            nudge_x = 0.7) +
  coord_flip() +
  ggtitle("Covid-19 per 10K across the world",
          subtitle = '{frame_time}') +
  theme(legend.position = "none",
        axis.title = element_text(size = 20,
                                  face = 'bold'),
        plot.subtitle = element_text(size = 20,
                                     face = "bold",
                                     color = 'red'),
        axis.text = element_text(size = 10,
                                 face = "bold"),
        axis.text.x = element_text(size = 12,
                                   face = "bold")) +
  labs(x = '',
       y = 'Covid-19 Confirmed Cases/10K',
       caption =  paste0("Data Credit: ",
                         "https://covid19datahub.io")) +
  transition_time(date) +
  ease_aes('cubic-in-out')

# As gif
animate(p,
        fps = 3, 
        renderer = gifski_renderer(loop = F),
        end_pause = 10,
        width = 850,
        height = 900)

anim_save("animations/perCapita/Top25CountriesCovid19Confirmed.gif")

# As movie
p2 <- animate(p,
              fps = 3,
              renderer = av_renderer(),
              width = 850,
              height = 900)

anim_save("animations/perCapita/Top25CountriesCovid19Confirmed.mp4", p2)




# Dead
df2 <- Countries %>%  
  mutate(deaths_per_10k = (deaths/population)*10000)

# select top 25 countries with covid cases

top_25Counties <- df2 %>% 
  group_by(Country) %>% 
  summarise(max_cases = max(deaths_per_10k,  na.rm = TRUE)) %>% 
  top_n(25, wt = max_cases)


p <- ggplot(df2 %>% 
              filter(Country %in% top_25Counties$Country), 
            aes(x = Country,
                y = deaths_per_10k,
                label = '',
                col = Country)) +
  geom_point(stat = "identity", 
             size = 3) +
  geom_segment(aes(y = 0,
                   x = Country,
                   yend = deaths_per_10k,
                   xend = Country)) +
  geom_text(color = 'black',
            size = 3,
            nudge_x = 0.7) +
  coord_flip() +
  ggtitle("Covid-19 per 10K across the world",
          subtitle = '{frame_time}') +
  theme(legend.position = "none",
        axis.title = element_text(size = 20,
                                  face = 'bold'),
        plot.subtitle = element_text(size = 20,
                                     face = "bold",
                                     color = 'red'),
        axis.text = element_text(size = 10,
                                 face = "bold"),
        axis.text.x = element_text(size = 12,
                                   face = "bold")) +
  labs(x = '',
       y = 'Covid-19 deaths Cases/10K',
       caption =  paste0("Data Credit: ",
                         "https://covid19datahub.io")) +
  transition_time(date) +
  ease_aes('cubic-in-out')

# As gif
animate(p,
        fps = 3, 
        renderer = gifski_renderer(loop = F),
        end_pause = 10,
        width = 850,
        height = 900)

anim_save("animations/perCapita/Top25CountriesCovid19deaths.gif")

# As movie
p2 <- animate(p,
              fps = 3,
              renderer = av_renderer(),
              width = 850,
              height = 900)

anim_save("animations/perCapita/Top25CountriesCovid19deaths.mp4", p2)

#### Vaccines
df2 <- Countries %>%  
  mutate(vaccines_percent = (vaccines/population)*100) %>% 
  filter(!is.na(vaccines))


# select top 25 countries with covid cases

top_25Counties <- df2 %>% 
  group_by(Country) %>% 
  summarise(max_cases = max(vaccines_percent,  na.rm = TRUE)) %>% 
  top_n(25, wt = max_cases)


p <- ggplot(df2 %>% 
              filter(Country %in% top_25Counties$Country), 
            aes(x = Country,
                y = vaccines_percent,
                label = '',
                col = Country)) +
  geom_point(stat = "identity", 
             size = 3) +
  geom_segment(aes(y = 0,
                   x = Country,
                   yend = vaccines_percent,
                   xend = Country)) +
  geom_text(color = 'black',
            size = 3,
            nudge_x = 0.7) +
  coord_flip() +
  ggtitle("Covid-19 vaccinated population percent",
          subtitle = '{frame_time}') +
  theme(legend.position = "none",
        axis.title = element_text(size = 20,
                                  face = 'bold'),
        plot.subtitle = element_text(size = 20,
                                     face = "bold",
                                     color = 'red'),
        axis.text = element_text(size = 10,
                                 face = "bold"),
        axis.text.x = element_text(size = 12,
                                   face = "bold")) +
  labs(x = '',
       y = 'Vaccinated population',
       caption =  paste0("Data Credit: ",
                         "https://covid19datahub.io")) +
  transition_time(date) +
  ease_aes('cubic-in-out')

# As gif
animate(p,
        fps = 3, 
        renderer = gifski_renderer(loop = F),
        end_pause = 10,
        width = 850,
        height = 900)

anim_save("animations/perCapita/Top25CountriesCovid19vaccines.gif")

# As movie
p2 <- animate(p,
              fps = 3,
              renderer = av_renderer(),
              width = 850,
              height = 900)

anim_save("animations/perCapita/Top25CountriesCovid19vaccines.mp4", p2)


#####################

#########################

# Set up: Install/load various required packages
pkg_reqd <- list('tidyverse',            # Data wrangling - visualization
                 'COVID19', 
                 'hrbrthemes',
                 'scales',
                 'plotly',
                 'gridExtra')
# Check if pkg is present on machine and if not then install it
install.packages(as.character(pkg_reqd[!pkg_reqd %in% installed.packages()[, 'Package']]))

# load all required libraries
sapply(pkg_reqd, require, character.only = TRUE)

rm(list = ls())



Countries <- covid19() %>% 
  rename(Country = administrative_area_level_1) %>% 
  filter(!id %in% c('CAC',
                    'VAT',
                    'MSZ',
                    'GPC',
                    'SMR',
                    'DPC'))

top_5Countries <- Countries %>% 
  group_by(Country) %>% 
  summarise(counts = max((vaccines/population)*100,
                         na.rm = TRUE)) %>% 
  top_n(5, wt = counts)



df <- Countries %>% 
  filter(Country %in% top_5Countries$Country
         &
           date > '2020-12-01') %>% 
  mutate(Vaccinated = (vaccines/population)*100)


VaccinatedGraph <- ggplot(data = df,
                          aes(x = date,
                              y = Vaccinated,
                              group = Country,
                              color = Country)) +
  geom_line(lwd = 1.2) +
  scale_y_continuous(labels = comma) + 
  labs(title = "Top 10 Vaccinated countries2",
       x = "Time",
       y = "Vaccinated Population") +
  theme_bw()

VaccinatedGraph

ggplotly(VaccinatedGraph)

# Create a function
### get() function will take the input string and force R to interpret
### its value as if a column name has been passed to it

PlotPercentMeasure = function(measure){
  top_5Countries <- Countries %>% 
    group_by(Country) %>% 
    summarise(counts = max(get(measure)/population,  na.rm = TRUE)) %>% 
    top_n(5, wt = counts)
  
  df <- Countries %>% 
    filter(Country %in% top_5Countries$Country) %>% 
    mutate(MeasurePerCent = (get(measure)/population)*100)
  
  p <-  ggplot(data = df,
               aes(x = date,
                   y = MeasurePerCent,
                   group = Country,
                   color = Country)) +
    geom_line(lwd = 1) +
    scale_y_continuous(labels = comma) + 
    labs(title = paste("Top 5 Countries:",
                       as.name(measure),
                       "as % of population"),
         x = "",
         y = as.name(measure)) +
    theme_bw()
  plot(p)
}

PlotPercentMeasure('vaccines')

grid.arrange(PlotPercentMeasure('vaccines'),
             PlotPercentMeasure("confirmed"),
             PlotPercentMeasure("deaths"))


# Plot as Raw values
PlotRawMeasure = function(measure){
  top_5Countries <- Countries %>% 
    group_by(Country) %>% 
    summarise(counts = max(get(measure),  na.rm = TRUE)) %>% 
    top_n(5, wt = counts)
  
  df <- Countries %>% 
    filter(Country %in% top_5Countries$Country) %>% 
    mutate(MeasurePerCent = get(measure))
  
  p <-  ggplot(data = df,
               aes(x = date,
                   y = MeasurePerCent,
                   group = Country,
                   color = Country)) +
    geom_line(lwd = 1) +
    scale_y_continuous(labels = comma) + 
    labs(title = paste("Top 5 Countries:",
                       as.name(measure)),
         x = "",
         y = as.name(measure)) +
    theme_bw()
  plot(p)
}

grid.arrange(PlotRawMeasure('vaccines'),
             PlotRawMeasure("confirmed"),
             PlotRawMeasure("deaths"))


