# extension od http://jkunst.com/r/pokemon-visualize-em-all/

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(rvest)
library(purrr)

# Function to allow injection of different file names without repeating full path
path <- function(x) paste0("https://raw.githubusercontent.com/phalt/pokeapi/master/data/v2/csv/", x)

# Pkmn with specied_id, height, weight, base_experience
dfpkmn <- read_csv(path("pokemon.csv")) %>% # read
  select(-order, -is_default) %>% # drop cols
  rename(pokemon = identifier)

# Pkmn id with attack, defense, hp, special_attack, special_defense and speed
dfstat <- read_csv(path("stats.csv")) %>% # read
  rename(stat_id = id) %>% 
  right_join(read_csv(path("pokemon_stats.csv")),
             by = "stat_id") %>% # joins stat_id to each pokemon
  mutate(identifier = str_replace(identifier, "-", "_")) %>%  # converts "-" to "_"
  select(pokemon_id, identifier, base_stat) %>% # pluck cols
  spread(identifier, base_stat) %>% # effectively cast
  rename(id = pokemon_id)

# Pkmn id with type_1 and type_2
dftype <- read_csv(path("types.csv")) %>% 
  rename(type_id = id) %>% 
  right_join(read_csv(path("pokemon_types.csv")), by = "type_id") %>% # joins type table to each pokemon with duplicates for dual type 
  select(pokemon_id, identifier, slot) %>%  # pluck cols
  mutate(slot = paste0("type_", slot)) %>%  # remelt
  spread(slot, identifier) %>% # cost out each type, inducing NA
  rename(id = pokemon_id)

# Pkmn species_id with egg_group_1 and egg_group_2
dfegg <- read_csv(path("egg_groups.csv")) %>% 
  rename(egg_group_id = id) %>% 
  right_join(read_csv(path("pokemon_egg_groups.csv")), by = "egg_group_id") %>% # joins eggs to each pokemon weith duplication
  group_by(species_id) %>% # builds grouped tibble?
  mutate(ranking = row_number(),
         ranking = paste0("egg_group_", ranking)) %>% # builds group 1 or 2 id
  select(species_id, ranking, identifier) %>% 
  spread(ranking, identifier) 

# Pkmn id with url_image (name of file)
dfimg <- "https://github.com/phalt/pokeapi/tree/master/data/Pokemon_XY_Sprites" %>% 
  read_html() %>% 
  html_nodes("tr.js-navigation-item > .content > .css-truncate a") %>% 
  map_df(function(x){
    url <- x %>% html_attr("href")
    data_frame(
      id = str_extract(basename(url), "\\d+"),
      url_image = basename(url)
    )
  }) %>%
  mutate(id = as.numeric(id))

# Get images (except for mega)
url_bulbapedia_list <- "http://bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_by_base_stats_(Generation_VI-present)" 

# Checks what's mega and drops it?
id <- url_bulbapedia_list %>% 
  read_html(encoding = "UTF-8") %>% 
  html_node("table.sortable") %>% 
  html_table() %>% 
  .[[1]] %>% 
  as.numeric()

# Finds url of each .png
url_icon <-  url_bulbapedia_list %>% 
  read_html() %>%
  html_nodes("table.sortable img") %>% 
  html_attr("src")

# Does this actually hold the icons?
dficon <- data_frame(id, url_icon) %>% 
  filter(!is.na(id)) %>% 
  distinct(id)

# type with hex colour, colour_1 is colour for first type, colour_2 is colour for secondary (if present)
dfcolor <- map_df(na.omit(unique(c(dftype$type_1, dftype$type_2))), function(t){
  # t <- "bug"
  col <- "http://pokemon-uranium.wikia.com/wiki/Template:%s_color" %>% 
    sprintf(t) %>%
    read_html() %>% 
    html_nodes("span > b") %>% 
    html_text()
  data_frame(type = t, color = paste0("#", col))
})

# colour_f looks to be colour ramp between primary and secondar colours to make hybrid "total type" colour
dfcolorf <- expand.grid(color_1 = dfcolor$color, color_2 = dfcolor$color,
                        stringsAsFactors = FALSE) %>% 
  tbl_df() %>% 
  group_by(color_1, color_2) %>% 
  do({
    n = 100;p = 0.25
    data_frame(color_f = colorRampPalette(c(.$color_1, .$color_2))(n)[round(n*p)])
  })

# Pkmn id with generation_id, evolves_from_species_id, evolution_chain_id and shape_id
dfspecies <- read_csv(path("pokemon_species.csv")) %>%
  select(id, generation_id, evolves_from_species_id, evolution_chain_id, shape_id)

# Pkmn shape_id with shape
dfshape <- read_csv(path("pokemon_shapes.csv")) %>%
  rename(shape_id = id) %>%
  rename(shape = identifier)

# Pkmn id with abilities
dfabilities <- read_csv(path("abilities.csv")) %>%
  rename(ability_id = id) %>%
  right_join(read_csv(path("pokemon_abilities.csv")), by = "ability_id") %>%
  mutate(ranking = paste0("ability_", slot)) %>%
  select(pokemon_id, identifier,ranking) %>%
  spread(ranking, identifier) %>%
  rename(ability_hidden = ability_3) %>%
  rename(id = pokemon_id)
  
# THE join
df <- dfpkmn %>% 
  left_join(dftype, by = "id") %>% 
  left_join(dfstat, by = "id") %>% 
  left_join(dfabilities, by = "id") %>%
  left_join(dfcolor %>% rename(type_1 = type, color_1 = color), by = "type_1") %>% 
  left_join(dfcolor %>% rename(type_2 = type, color_2 = color), by = "type_2") %>% 
  left_join(dfcolorf, by =  c("color_1", "color_2")) %>%
  left_join(dfegg, by = "species_id") %>% 
  left_join(dfimg, by = "id") %>% 
  left_join(dficon, by = "id") %>%
  left_join(dfspecies, by = "id") %>%
  left_join(dfshape, by = "shape_id")
  

rm(dftype, dfstat, dfcolor, dfcolorf, dfegg, dfimg, dficon)
rm(id, url_bulbapedia_list, url_icon)