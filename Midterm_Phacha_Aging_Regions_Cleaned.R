
#rm(list = ls())

setwd("/Users/phacha/Documents/R Projects/Presentation502")

df = read.csv("Data/POP_DPND.csv", skip = 4)


ls()

# load the required libraries
#install.packages("ggridges")
#install.packages("ggplot2")
#install.packages("viridis")
library(ggplot2)
library(ggridges)
library(viridis) 
library(dplyr)
library(tidyr)


#check column name first.
colnames(df)

#check how clean of the data, quite clean, is there any nonnumerical data in the ratios columns
#there are two empty rows, [Not classified(INX), (West Bank and Gaza(PSE)], can ignore them for now.
colSums(is.na(df))
df$X2000[is.na(as.numeric(df$X2000))]
sum(is.na(df$X2020))  # how many missing
df$X2000[!grepl("^[0-9.]+$", df$X2000)]


#change the columns name first, it have . not good for var name
df <- df %>%
  select('Country.Name','Country.Code','X2000','X2024') %>%
  rename(
    country_name = 'Country.Name',
    country_code = 'Country.Code',
    dp_2000 = 'X2000',
    dp_2024 = 'X2024'
  ) %>%
  drop_na()


# # Use this code,in case that you want to drop the two empty rows of INX, PSE
# df <- df[!df$country_code %in% c('INX', 'PSE'), ]
# any(df$country_code %in% c('INX', 'PSE'))


#create this function to find country or code name to use in next step,
#if you knew country name or country code can ignore this section

find_country <- function(keyword) {
  df %>%
    filter(
      grepl(keyword, country_name, ignore.case = TRUE) |
        grepl(keyword, country_code, ignore.case = TRUE)
    )
 }
#
  find_country('North')
  

df_chart <- df %>%
  #change to the countries or regions or world that you 'd like to see its data
  #it must be the exact name show in column_name
  #now, we want to examine data by regions using UN classifications(7 regions)
  filter(country_name %in% c("East Asia & Pacific", "Europe & Central Asia","Latin America & Caribbean", "Middle East, North Africa, Afghanistan & Pakistan", "North America", "South Asia", "Sub-Saharan Africa"))%>%
  #shorten the country name if it too long, so it show nicely on Y
  mutate(country_name  = ifelse(country_name == "Middle East, North Africa, Afghanistan & Pakistan","Middle East & North Africa", country_name)) %>%
  mutate(change = dp_2024 - dp_2000) %>%
  arrange((dp_2024)) %>%
  mutate(country_name = factor(country_name, levels = country_name))

#create the dumbbells figures
fig <- ggplot(df_chart)+
  geom_segment(aes(x = dp_2000, xend = dp_2024, y = country_name, yend = country_name),
               color = 'gray75', linewidth = 2) +
  geom_point(aes(x = dp_2000, y = country_name, color = '2000'), size = 6, alpha = 0.8) +
  geom_point(aes(x = dp_2024, y = country_name, color = '2024'), size = 5.8, alpha = 0.8) +

 
  #work on the labels, aesthetics them
  geom_text(
    aes(x = (dp_2000 + dp_2024) / 2,
        y = country_name,

      
        # label = sprintf("%+.2f", change)),  
        # use the simply code above if needs the all label show 2 decimals
        # create if else to control label not to show 0.0 if it is a tiny number
        label = ifelse(abs(change) < 0.1,
                        sprintf("%+.2f", change),  # shows 2 decimals for tiny numbers
                        sprintf("%+.1f", change))  # shows 1 decimals for normal numbers
    ),
    
       # negative v just mean above the line, and selected the magoteen for label color
        vjust = -1.2, color = "#613b5e", fontface = "bold", size = 3 
  ) +

  # Set the x-axis to show numbers from 0 to 40 in increments of 5, but hide the 0.
  scale_x_continuous(
    breaks = seq(0, 40, by = 5),
    limits = c(0, 35),
    labels = function(x) ifelse(x== 0, "", x) #if the axis label is 0, make it blank; otherwise, print the number
  )+
 
  # if you like to change the color of dumbbell, change it here.##6B2D5C is a mangoteen color it is nice too
  # this set is nice too, pink and mangoteen set,('2000' = '#E598A8', '2024' = '#6B2D5C')
  # need to set name = NULL, otherwise it will show the label.
  # earlier code, I set color ='2000' and '2004', so I just assign color here.
   scale_color_manual(
    name = NULL,
    values = c('2000' = '#B0B0B0', '2024' = '#D2042D')
  )+
  
  #let work on labels
  labs(
    title = "The Rising Demographic Tide",
    subtitle = 'Tracking the shift in old-age dependency ratios (%) across global regions (2000 vs. 2024)',
    x = NULL,
    y = '',
    caption = "Source: Age dependency ratio, old (% of working-age population), World Bank Data (2000–2024)"
  )+
  
  #set it clean, so it is easy to decorate the graph
  theme_minimal() +
  theme(
    #optional title #222222, sub #666666, red #, #990F3D
    plot.title = element_text(face ='bold', size =18, color ='#5D1E24', hjust = 0),
    plot.subtitle = element_text(face = 'bold',color ='#A62639', size=13, margin=margin(b=20), hjust = 0),
   
   
    #I want the title pushed all the way to the absolute far left edge of the entire image.
    plot.title.position = 'plot',
    
    # let set x, y axis text to grey, so the chart not overwhelm with too many colors
    axis.text.y = element_text(face='bold',size =11, color = '#666666',margin=margin(r =1)),
    axis.text.x = element_text(face= 'bold', size = 11, color = "#666666", margin=margin(r=3)),

    panel.grid.major.y = element_blank(), # we can't use y-axis gridlines because they make the dumbbell chart look messy.
    panel.grid.minor.x = element_blank(),  # it is still show major grid-line from 5, 10, 15...but it wont show minor gridlines

    plot.margin = margin(t = 22, r =22, b= 25, l =22), 
    
    
    # if want to change the position of color label, to the precise position like under the sub-title name (that what i tried), try this code below
    # legend.position = c(-0.25, 1.05), #top, need to adjust number until it looks alright
    # legend.direction = 'horizontal',
    # legend.justification = 'left',
    # legend.margin = margin(t = 20, b=12),
    
    # pin it by the top-left corner
    # legend.justification = c(0, 1),
    
    # lay flat
    # legend.direction = 'horizontal',
    # legend.margin = margin(t = 0, b=30),
    # legend.margin = margin(t = 0, b = 0, l = 0, r = 0),
       
   
    
      legend.justification = "right",
      
      legend.direction = "vertical",
  
      legend.margin = margin(b = 20),
   
      legend.box.margin = margin(t = 10),  
    
    # need to set the caption color, size and position.
    plot.caption = element_text(
      size = 9, 
      color = "#666666", 
      hjust = 0,         
      margin = margin(t = 17) # adds space between the chart and the source
    ),
    
    #let make it not too white
    plot.background = element_rect(fill = "#FAF9F6", color = NA),

    panel.background = element_rect(fill = "#FAF9F6", color = NA)

    )

print(fig)

# uncomment this if ready to export new chart.
# ggsave(filename = "Aging_Demographics_Dumbbell.png", 
#        plot = fig, 
#        width = 10, 
#        height = 6, 
#        dpi = 300,
#        bg = "#FAF9F6")
