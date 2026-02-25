
#rm(list = ls())

setwd("/Users/phacha/Documents/R Projects/Presentation502")

df = read.csv("Data/POP_DPND.csv", skip = 4)
#find the method to drop the empty row
#df <- df[rowSums(is.na(df)) != ncol(df), ]

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
# library(countrycode) # automated region dictionary, end up didnt use this.

#check columns names 
#check column name first, sometime we seem something R see recognizeds somethingelse
colnames(df)
#check how clean of the data, result is great, quite clean but we found one na 
#it is actually the empty row, not all data we are going to use but let drop it
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


# colSums(is.na(df))
# colnames(df)
# head(df,5)   
# 
# # we need to mutate it if it is not numeric
# # check na one moretime
# df$X2000[is.na(as.numeric(df$X2000))]
# sum(is.na(df$X2020))  # how many missing
# df$X2000[!grepl("^[0-9.]+$", df$X2000)]

# Check if any 'INX' or 'PSE' remain
#any(df$country_code %in% c("INX","PSE"))
# drop GaZa and not classifiled data, these two are the empthy rows
# df <- df[!df$country_code %in% c('INX','PSE')]


# #filter by use filter(column %in%("...") to list regions or counties in chart
# #filter(country_code %in% c" ")
# 
# # find_country <- function(keyword) {
# #   df %>%
# #     filter(
# #       grepl(keyword, country_name, ignore.case = TRUE) |
# #         grepl(keyword, country_name, ignore.case = TRUE)
# #     )
# #  }
# # # 
# #   find_country('North Africa')
# 
df_chart <- df %>%
  filter(country_name %in% c("East Asia & Pacific", "Europe & Central Asia","Latin America & Caribbean", "Middle East, North Africa, Afghanistan & Pakistan", "North America", "South Asia", "Sub-Saharan Africa"))%>%
  mutate(country_name  = ifelse(country_name == "Middle East, North Africa, Afghanistan & Pakistan","Middle East & North Africa", country_name)) %>%
  mutate(change = dp_2024 - dp_2000) %>%
  arrange((dp_2024)) %>%
  mutate(country_name = factor(country_name, levels = country_name))

fig <- ggplot(df_chart)+
  geom_segment(aes(x = dp_2000, xend = dp_2024, y = country_name, yend = country_name),
               color = 'gray75', linewidth = 2) +
  geom_point(aes(x = dp_2000, y = country_name, color = '2000'), size = 6, alpha = 0.8) +
  geom_point(aes(x = dp_2024, y = country_name, color = '2024'), size = 5.8, alpha = 0.8) +

  #geom_text(aes(x = dp_2024, y = country_name, label = paste0("+", round(change, 1))), vjust = -1.2,
           # color = "#4B1F3F", fontface = 'bold', size = 4 ) + #use hjust = -0.8 if want it show at the end

  geom_text(
    aes(x = (dp_2000 + dp_2024) / 2,
        y = country_name,

        # %+.1f forces the +/- sign AND exactly 1 decimal place!
        # label = sprintf("%+.2f", change)),

        label = ifelse(abs(change) < 0.1,
                        sprintf("%+.2f", change),  # Shows -0.04 for tiny numbers
                        sprintf("%+.1f", change))  # Shows +9.7 for normal numbers
    ),
    vjust = -1.2, color = "#613b5e", fontface = "bold", size = 3
  ) +


  scale_x_continuous(
    breaks = seq(0, 40, by = 5),
    limits = c(0, 35),
    labels = function(x) ifelse(x== 0, "", x)
  )+

  scale_color_manual(
    name = NULL,
    values = c('2000' = '#B0B0B0', '2024' = '#D2042D') ##6B2D5C'magoteen #D2042D bright cherry
  )+

  # last color c('2000' = '#E598A8', '2024' = '#6B2D5C')
  labs(
    title = "The Rising Demographic Tide",
    subtitle = 'Tracking the shift in old-age dependency ratios (%) across global regions (2000 vs. 2024)',
    x = NULL,
    y = '',
    caption = "Source: Age dependency ratio, old (% of working-age population), World Bank Data (2000–2024)"
  )+

  theme_minimal() +
  theme(

    plot.title = element_text(face ='bold', size =18, color ='#5D1E24', hjust = 0),
    plot.subtitle = element_text(face = 'bold',color ='#A62639', size=13, margin=margin(b=20), hjust = 0),
    #optional titile #222222, sub #666666, red #, #990F3D
    plot.title.position = 'plot',

    axis.text.y = element_text(face='bold',size =11, color = '#666666',margin=margin(r =1)),
    axis.text.x = element_text(face= 'bold', size = 11, color = "#666666", margin=margin(r=3)),

    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),

    plot.margin = margin(t = 22, r =22, b= 25, l =22),

    # legend.position = c(-0.25, 1.05), #top
    # legend.direction = 'horizontal',
    # legend.justification = 'left',
    # legend.margin = margin(t = 20, b=12),
    


    # precise placement
    # legend.position =  c(-0.50, 1.02),
    
    # pin it by the top-left corner
    # legend.justification = c(0, 1),
    
    # lay flat
    # legend.direction = 'horizontal',
    # legend.margin = margin(t = 0, b=30),
     
    #Dont use this for now
    #legend.margin = margin(t = 0, b = 0, l = 0, r = 0),
       
      # the legend here was slightly different than figure i submitted. I didnt rerun again after submitted
      # The simple position
      legend.position = "top",
    
      legend.justification = "right",
      
      legend.direction = "horizontal",
      
      #adds space below legend
      legend.margin = margin(b = 20),
      
      #adds space above legend
      legend.box.margin = margin(t = 10),
    

    plot.caption = element_text(
      size = 9, 
      color = "#666666", 
      hjust = 0,         
      margin = margin(t = 17) # adds space between the chart and the source
    ),
    
    plot.background = element_rect(fill = "#FAF9F6", color = NA),

    panel.background = element_rect(fill = "#FAF9F6", color = NA)

    )

print(fig)


# uncomment this if want to export new chart.
# ggsave(filename = "Aging_Demographics_Dumbbell.png", 
#        plot = fig, 
#        width = 10, 
#        height = 6, 
#        dpi = 300,
#        bg = "#FAF9F6")
