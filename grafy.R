library(tidyverse)
library(igraph)

profile_df <- readRDS("grabed_profiles.RDS") %>% distinct() %>% filter(nchar(wartosc) != 0)

# korekty wpisanych regionów
profile_df[(profile_df$cecha == "Region") & (profile_df$wartosc == "pila "), "wartosc"] <- "wielkopolskie"
profile_df[(profile_df$cecha == "Region") & (profile_df$wartosc == "Bielsko-Biała "), "wartosc"] <- "śląskie"
nprofiles <- length(unique(profile_df$nick))

get_df <- function(f_cechaA, f_cechaB) {
  inner_join(profile_df %>% filter(cecha == f_cechaA) %>% separate_rows(wartosc, sep = ",") %>% mutate(wartosc = trimws(wartosc)) %>% select(-cecha),
             profile_df %>% filter(cecha == f_cechaB) %>% separate_rows(wartosc, sep = ",") %>% mutate(wartosc = trimws(wartosc)) %>% select(-cecha),
             by = "nick") %>%
    set_names(c("a", "nick", "b")) %>%
    count(a, b) %>%
    ungroup()
}


df <- get_df("Photo_URL", "Wiek") %>% filter(a != b)

g <- graph_from_data_frame(df, directed = FALSE)

E(g)$weigth <- df$n
V(g)$cluster <- clusters(g)$membership

edge_betweenness(g)

walktrap.community(g, weights = E(g)$weigth)

plot(g,
     vertex.size = 2,
     vertex.label.cex = 0.8,
     vertex.label.color = V(g)$cluster,
     edge.width = E(g)$weigth*quantile(E(g)$weigth, 0.25)/max(E(g)$weigth))


