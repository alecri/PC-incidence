#devtools::install_github("thomasp85/transformr")
#devtools::install_github("reinholdsson/swemaps")
pacman::p_load(tidyverse, plotly, swemaps, ggthemes, gridExtra, gganimate, magick)
theme_set(theme_bw())

# read data ----
# https://sdb.socialstyrelsen.se/if_can/val.aspx
# https://sdb.socialstyrelsen.se/if_dor/val.aspx
incidence <- read_csv2("data/Statistikdatabasen_2019-11-28 12_56_57.csv", skip = 1, n_max = 1056)
mortality <- read_csv2("data/Statistikdatabasen_2019-11-28 12_56_54.csv", skip = 1, n_max = 484)

# data management ----
incidence <- incidence %>% 
  pivot_longer(cols = -c("År", "Region"), names_to = "Age category", values_to = "Incidence") %>% 
  mutate(`Age category` = iconv(`Age category`, from = "UTF-8", to = "LATIN1"),
         `Age category` = fct_inorder(gsub("\xad", "-", `Age category`)),
         Region = replace(Region, Region == "Riket", "Sweden")) %>% 
  rename(Year = År)
mortality <- mortality %>% 
  mutate_at(vars(-Region), function(x) as.double(gsub(",", ".", x))) %>% 
  pivot_longer(cols = -c("År", "Region"), names_to = "Age category", values_to = "Mortality") %>% 
  mutate(`Age category` = iconv(`Age category`, from = "UTF-8", to = "LATIN1"),
         `Age category` = fct_inorder(gsub("\xad", "-", `Age category`)),
         Region = replace(Region, Region == "Riket", "Sweden")) %>% 
  rename(Year = År)
indices <- full_join(incidence, mortality, by = c("Year", "Region", "Age category")) %>% 
  pivot_longer(cols = c("Incidence", "Mortality"), names_to = "Rate") %>% 
  na.omit()
 
# analyses ----

# incidence and mortality over time
filter(indices, Region == "Sweden", `Age category` == "Totalt") %>% 
  ggplot(aes(Year, value, linetype = Rate)) +
  geom_point() +
  geom_line() +
  labs(y = "Rate (x 100 000)") -> p1_ind
p1_ind

# incidence and mortality over time by regions
filter(indices, `Age category` == "Totalt", 
       Region %in% c("Sweden", "Stockholms län", "Skåne", "Norrbottens län")) %>% 
  ggplot(aes(Year, value, linetype = Rate, col = Region)) +
  geom_point() +
  geom_line() +
  labs(y = "Rate (x 100 000)") -> p2_ind
p2_ind

# incidence and mortality over time by regions and age categories
filter(indices, `Age category` %in% c("60-64", "65-69", "70-74", "Totalt"), 
       Region %in% c("Sweden", "Stockholms län", "Skåne", "Norrbottens län")) %>% 
  ggplot(aes(Year, value, linetype = Rate, col = Region)) +
  geom_point() +
  geom_line() +
  labs(y = "Rate (x 100 000)") +
  facet_wrap(`Age category` ~ ., scales = "free") -> p3_ind
p3_ind


# interactive plots
ggplotly(p3_ind)


# maps for shiny app
map_indices <- mutate(map_ln, Region = as.character(lnnamn)) %>% 
  mutate(Region = replace(Region, Region == "", "Sweden")) %>% 
  full_join(indices, by = "Region")
# save data for shiny app
save(incidence, mortality, indices, map_indices, file = "PC_inc_mort/www/data_pc.Rdata")

# maps for rest of the code
map_indices <- mutate(map_ln, Region = as.character(lnnamn)) %>% 
  mutate(Region = replace(Region, Region == "", "Sweden")) %>% 
  full_join(filter(indices, `Age category` == "Totalt", Region != "Sweden", 
                   Year >= 2000 & Year <= 2017), by = "Region") %>% 
  mutate(Year = as.integer(Year))

# map in one instance of time
p_map_inc1 <- filter(map_indices, Rate == "Incidence") %>% 
  filter(Year == 2010) %>% 
  ggplot(aes(ggplot_long, ggplot_lat, group = Region, fill = value)) +
  geom_polygon() +
  coord_equal() +
  theme_map() + 
  theme(legend.position = c(.9, .2)) +
  labs(title = "Incidence rate (x 100 000)") +
  scale_fill_gradientn(colours = heat.colors(n = 7, rev = T))
p_map_mort1 <- filter(map_indices, Rate == "Mortality") %>% 
  filter(Year == 2010) %>% 
  ggplot(aes(ggplot_long, ggplot_lat, group = Region, fill = value)) +
  geom_polygon() +
  coord_equal() +
  theme_map() + 
  theme(legend.position = c(.9, .2)) +
  labs(title = "Mortality rate (x 100 000)") +
  scale_fill_gradientn(colours = heat.colors(n = 7, rev = T))
grid.arrange(p_map_inc1, p_map_mort1, nrow = 1)


# gif map
p_map_inc <- filter(map_indices, Rate == "Incidence") %>% 
  ggplot(aes(ggplot_long, ggplot_lat, group = Region, fill = value)) +
  geom_polygon() +
  coord_equal() +
  theme_map() + 
  theme(legend.position = c(.9, .2)) +
  labs(title = "Incidence rate (x 100 000)") +
  scale_fill_gradientn(colours = heat.colors(n = 7, rev = T))
p_map_mort <- filter(map_indices, Rate == "Mortality") %>% 
  ggplot(aes(ggplot_long, ggplot_lat, group = Region, fill = value)) +
  geom_polygon() +
  coord_equal() +
  theme_map() + 
  theme(legend.position = c(.9, .2)) +
  labs(title = "Mortality rate (x 100 000)") +
  scale_fill_gradientn(colours = heat.colors(n = 7, rev = T))

# create a gif from the previous maps (note: it takes some time)
gif_inc <- animate(p_map_inc +
                     transition_time(Year) + 
                     labs(title = "Incidence rate (x 100 000), Year: {frame_time}"), 
                   nframes = 18, duration = 18, width = 300, height = 560)
gif_inc
image_write(gif_inc, path = "gif/gif_inc.gif")
gif_mort <- animate(p_map_mort +
                      transition_time(Year) + 
                      labs(title = "Mortality rate (x 100 000), Year: {frame_time}"), 
                    nframes = 18, duration = 18, width = 300, height = 560)
gif_mort
image_write(gif_mort, path = "gif/gif_mort.gif")

# putting the two previous gifs close to each other (in a grid 1x2)
inc_mgif <- image_read(path = "gif/gif_inc.gif")
mort_mgif <- image_read(path = "gif/gif_mort.gif")
map_gif <- image_append(c(inc_mgif[1], mort_mgif[1]))
for(i in 2:18){
  combined <- image_append(c(inc_mgif[i], mort_mgif[i]))
  map_gif <- c(map_gif, combined)
}
map_gif

# save gif
image_write(map_gif, path = "gif/map_rate.gif")
