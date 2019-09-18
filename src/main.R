install.packages("pacman")
pacman::p_load(formatR, readr, dplyr, rgdal, ggplot2, 
               maps, ggthemes, hablar, mapproj, scales, RColorBrewer, 
               ggmap)

# set wd
setwd("C:/Projects/Personal/kosovo-innovation-per-municipality/src/")

# Load data
url_csv <- "../data/interim/joined.csv"
business.data <- read_csv(url_csv)
prishtina.data <- read.csv("../data/interim/prishtina_sections.csv", encoding="UTF-8", stringsAsFactors=FALSE)

# Set map
shapefile <- readOGR(dsn = path.expand("../data/kosovo-shapefile"), 
                     "XK_EA_2018", use_iconv = TRUE, encoding = "UTF-8")

# Next the shapefile has to be converted to a
# dataframe for use in ggplot2
shapefile_df <- fortify(shapefile, name = "XK_NAME")


merged_df <- merge(shapefile_df, business.data, by = "id", 
                   all.x = TRUE)
final_df <- merged_df[order(merged_df$order), ]

# aggregate data to get mean latitude and mean
# longitude for each state
cnames <- aggregate(cbind(long, lat) ~ komuna, 
                    data = final_df, FUN = function(x) mean(range(x)))

# modify some lats for better text placement
# Gjilan
cnames$lat[7] = 42.40343 + 0.05
# Viti
cnames$lat[34] = 42.33861 - 0.04

# map plot with new business density
ggplot() + geom_polygon(data = final_df, aes(x = long, 
                                             y = lat, group = group, fill = business_density), 
                        color = "black", size = 0.25) + coord_map() + 
  labs(title = "Kosovo new business density per municipality (new registrations per 1,000 people ages 15+)", 
    subtitle = "Based on data from the first quarter of 2019") + 
  scale_fill_distiller(name = "New business density", 
                       palette = "Blues", direction = 1, breaks = pretty_breaks(n = 7), 
                       limits = c(min(final_df$business_density, na.rm = TRUE), 
                                  max(final_df$business_density, na.rm = TRUE))) + 
  theme_nothing(legend = TRUE) + geom_text(data = cnames, 
                                           aes(long, lat, label = komuna), size = 3, fontface = "bold")

ggsave("visualization/kosovo-new-business-density.png")

# map plot with highest new business section
ggplot() + geom_polygon(data = final_df, aes(x = long, 
                                             y = lat, group = group, fill = section), 
                        color = "black", size = 0.25) + coord_map() + theme_nothing(legend = TRUE) +
  geom_text(data = cnames, aes(long, lat, label = komuna), size = 3, fontface = "bold")  + 
  labs(title = "Biggest sections of economic activity per municipality", 
       subtitle = "Section of economic activity with the highest number of new businesses registered in first quarter of 2019 (Absolute number)",
       fill="Sections of economic activity") + scale_fill_brewer(palette = "Set2",direction=-1)

ggsave("visualization/kosovo-new-business-type.png")


# barplot of all sections of new businesses in Prishtina
# use descending counts (-count)
prishtina.data <- transform(prishtina.data, section = reorder(section, -section_amount))

ggplot(prishtina.data, aes(x=section, y=section_amount, fill=section)) + geom_bar(stat="identity")

# pie chart

# Use brewer palette
pie + scale_fill_brewer("Pastel1") + theme_minimal()