##################################################
## Project:
## Script purpose:
## Date: 2021-09-30
## Author: Jessica Dyer
##################################################


packages <- c("redcapAPI", "dplyr", "ggplot2", "DiagrammeR", "tidyverse", "Hmisc", "tibble", 
              "readxl", "viridis")

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
invisible(lapply(packages, library, character.only = TRUE))

df <- read_excel("data/WI_PopulationNumbers.xlsx")

## ADD COLUMNS
df$population <- df$WaterInsecure + df$WaterSecure

df$prop_water_insecure <- round((df$WaterInsecure/df$population)*100, 2)
df$prop_water_secure <- round((df$WaterSecure/df$population)*100, 2)

## GENERATE NEW LONG DATAFRAME 
df_water_insecure <- 
        df %>%
        select(Country, WaterInsecure, prop_water_insecure, population) %>%
        mutate(type = "Water insecure")

df_water_secure <- 
        df %>%
        select(Country, WaterSecure, prop_water_secure, population) %>%
        mutate(type = "Water secure")

col_names <- c("country", 'number', "proportion", 'population', "type")
names(df_water_insecure) <- col_names
names(df_water_secure) <- col_names

df_long <- rbind(df_water_insecure, df_water_secure)


## STACKED BAR CHART
df_long %>%
        group_by(type) %>%
        mutate(name = fct_reorder(country, proportion)) %>%
        ggplot(aes(fill=type, x=name, y=proportion)) +
                geom_bar(position='fill', stat='identity') + 
                theme_ipsum() + 
                theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1)) +
        labs(x = '', y = 'Proportion of total population water insecure', fill = "") +
        scale_color_viridis_c()

## BUBBLE CHART 
library(hrbrthemes)
df_long %>%
        filter(type == 'Water insecure') %>%
        mutate(name = fct_reorder(country, proportion)) %>%
        ggplot(aes(x=name, y=proportion, size = population)) +
                geom_point(alpha=0.5) +
                scale_size(range = c(.1, 30), name='Population (M)') + 
        theme_ipsum() + 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1)) +
        labs(x = '', y = 'Proportion of total population water insecure', fill = "") 
        
        