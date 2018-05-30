library(dplyr)
library(circlize)
library(stringr)

starwars_df <- starwars %>% group_by(homeworld, species) %>% count() %>% arrange(desc(n)) %>%
        rename(from = homeworld, to = species, value = n) %>% ungroup() %>%
        filter(!is.na(from), !is.na(to), !is.na(value))
starwars_df
glimpse(starwars_df)

starwars_df_small <- starwars_df %>% slice(1:10)
starwars_df_small
glimpse(starwars_df_small)

circos.clear()
# note the plots look better saved as pdfs
chordDiagram(starwars_df_small)
chordDiagram(starwars_df)

# starwars_df_small <- starwars_df_small %>% mutate(value = df$value[1:10])
# starwars_df_small <- starwars_df_small %>% mutate(from = df$from[1:10])
# # starwars_df_small <- starwars_df_small %>% mutate(to = df$to[1:10])
# starwars_df_small <- starwars_df_small %>% mutate(from = str_c("test_", from))



##################################


set.seed(999)
mat = matrix(sample(18, 18), 3, 6) 
rownames(mat) = paste0("S", 1:3)
colnames(mat) = paste0("E", 1:6)
mat

df = data.frame(from = rep(rownames(mat), times = ncol(mat)),
                to = rep(colnames(mat), each = nrow(mat)),
                value = as.vector(mat),
                stringsAsFactors = FALSE)
df
glimpse(df)

chordDiagram(df)
circos.clear()
