library(circlize)
library(scales)
library(viridis)
library(tidyverse)
library(rsample)

# setwd()
setwd("C:/Users/Stephen/Desktop/R/circlize")

# https://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html

# load attrition data
data(attrition)
attrition <- attrition %>% as_tibble()
attrition
attrition %>% glimpse()
attrition %>% count(Department)
attrition %>% count(RelationshipSatisfaction)

# create attrition_chord data
attrition_chord <- attrition %>% count(Department, RelationshipSatisfaction) %>% 
        rename(from = Department, to = RelationshipSatisfaction) %>%
        mutate(from = as.character(from), to = as.character(to))
attrition_chord

# check sums
attrition_chord %>% group_by(from) %>% summarize(sum = sum(n))
attrition_chord %>% group_by(to) %>% summarize(sum = sum(n))


##########################################


# create chord diagram

circos.clear()
# note the plots look better saved as pdfs
# note you need to run circos.clear() between plots to clear graphics device
chordDiagram(attrition_chord)

# set gaps between sectors
circos.par(gap.after = c(rep(5, attrition_chord %>% distinct(from) %>% nrow() - 1), 25, 
                         rep(5, attrition_chord %>% distinct(to) %>% nrow() - 1), 25))

# set degree from which to start listing sectors, and whether to list clockwise
circos.par(start.degree = 42, clock.wise = FALSE)

# set outer rim ("grid") colors with grid.col
# note that grid.col can be passed an unnamed vector of same length as sectors, which is applies in order sectors appear
# or grid.col can be passed a named vector with sectors as the names
# note the named vector option for the grid.col and col arguments seems to work better
# note that chordDiagram() colors the flow based on the first column of the data
grid_color_list <- attrition_chord %>% distinct(from) %>% rename(sector = from) %>%
        bind_rows(., attrition_chord %>% distinct(to) %>% rename(sector = to)) %>%
        mutate(color = case_when(sector == "Research_Development" ~ "#66ff33",
                                 sector == "Sales" ~ "#006600",
                                 sector == "Human_Resources" ~ "#00cc99", TRUE ~ "#999999")) %>%
        deframe()
grid_color_list

# create chord_color_list
chord_color_list <- attrition_chord %>% 
        mutate(chord_color = case_when(from == "Research_Development" ~ "#66ff33",
                                       from == "Sales" ~ "#006600",
                                       from == "Human_Resources" ~ "#00cc99", TRUE ~ "#999999")) %>%
        select(from, chord_color) %>% deframe()
chord_color_list

# create chord diagram
chordDiagram(attrition_chord, grid.col = grid_color_list, col = chord_color_list)


##################


# clear settings
circos.clear()

# setting sector color based on grid_col, as above, but setting flow color based on "value" variable 
# will first scale the value variable to be a percentage, 
# then the col argument will be passed get_viridis_color_for_percentage function to pull scaled color
show_col(viridis_pal()(100))
viridis_percentage_colors <- tibble(color = viridis_pal()(100)) %>% mutate(pct = row_number()) %>%
        select(pct, color)
viridis_percentage_colors 

# create chord_color_list
chord_color_list <- attrition_chord %>% mutate(pct = round((n / max(n)) * 100)) %>% 
        left_join(., viridis_percentage_colors, by = "pct") %>% select(from, color) %>% deframe()
chord_color_list

# create chord diagram
chordDiagram(attrition_chord, grid.col = grid_color_list, col = chord_color_list)
chordDiagram(attrition_chord, grid.col = grid_color_list, col = chord_color_list, transparency = 0)


#######################


# save plot
pdf("attrition_chord_plot.pdf")
chordDiagram(attrition_chord, grid.col = grid_color_list, col = chord_color_list)
dev.off()


##################################


