setwd("~/RProjects/Sympatia")

library(tidyverse)
library(corrplot)

# some better colours theme :)
theme_set(theme_minimal() +
            theme(plot.title = element_text(family = NULL, face = "bold", size = 18, color = "black"),
                  plot.subtitle = element_text(family = NULL, face = "plain", size = 9, color = "black"),
                  plot.caption = element_text(family = NULL, face = "italic", size = 9, color = "darkgray"),
                  plot.background = element_rect(fill="#efefef", color="#aaaaaa"),
                  panel.background = element_rect(fill = "white", color="black"),
                  strip.text.x = element_text(face = "bold")))

#### wczytanie profili pań ----

profile_df <- readRDS("grabed_profiles_women.RDS") %>% distinct() %>% filter(nchar(wartosc) != 0)

# korekty wpisanych regionów
profile_df[(profile_df$cecha == "Region") & (profile_df$wartosc == "pila "), "wartosc"] <- "wielkopolskie"
profile_df[(profile_df$cecha == "Region") & (profile_df$wartosc == "Bielsko-Biała "), "wartosc"] <- "śląskie"

profile_women <- profile_df



#### wczytanie profili panów ----

profile_df <- readRDS("grabed_profiles_men.RDS") %>% distinct() %>% filter(nchar(wartosc) != 0)

# szukamy facetów powyżej 40 - to jakieś błędy
ponad_40 <- profile_df %>%
  filter(cecha == "Wiek") %>%
  mutate(wartosc = as.numeric(wartosc)) %>%
  filter(wartosc > 40) %>%
  pull(nick)

profile_men <- profile_df %>% filter(!nick %in% ponad_40)

profile_men[(profile_men$cecha == "Region") & (profile_men$wartosc == "Inowłódz"), "wartosc"] <- "łódzkie"


# wyrównujemy liczbę profili - po prostu losowo z większej próbu (panowie) wybieramy tyle ile mamy kobiet

# męskie nicki
mens_nick <- unique(profile_men$nick)

# losowo zostawiamy ich tyle ile mamy kobiecych nicków
mens_nick <- sample(mens_nick, size = length(unique(profile_women$nick)))

# zostawiamy proflile zoswaionych nicków
profile_men <- profile_men %>% filter(nick %in% mens_nick)

# sprzątamy
rm(profile_df, ponad_40, mens_nick)


# liczba profili wg województwa i wieku
wiek_region <- full_join(
  profile_women %>%
    filter(cecha %in% c("Region", "Wiek")) %>%
    spread(cecha, wartosc) %>%
    count(Region, Wiek),
  profile_men %>%
    filter(cecha %in% c("Region", "Wiek")) %>%
    spread(cecha, wartosc) %>%
    count(Region, Wiek),
  by = c("Region", "Wiek")) %>%
  mutate(n_women = if_else(is.na(n.x), 0, as.numeric(n.x)),
         n_men = if_else(is.na(n.y), 0, as.numeric(n.y)),
         Wiek = as.numeric(Wiek)) %>%
  select(-n.x, -n.y) %>%
  group_by(Region) %>%
  mutate(n_women = 100*n_women/sum(n_women),
         n_men = 100*n_men/sum(n_men)) %>%
  ungroup()

wiek_region %>%
  gather("plec", "liczba", 3:4) %>%
  ggplot() +
  geom_line(aes(Wiek, liczba, color = plec),
            size = 1.5) +
  scale_color_manual(values = c("n_women" = "red", "n_men" = "blue"),
                     labels = c("n_women" = "kobiety", "n_men" = "mężczyźni")) +
  facet_wrap(~Region) +
  theme(legend.position = "bottom") +
  labs(title = "Profile według płci, w podziale na województwa i wiek właściciela profilu",
       subtitle = sprintf("Współczynnik korelacji: %.2f", cor(wiek_region$n_women, wiek_region$n_men)),
       fill = "", color = "",
       x = "Wiek", y = "% profili")

wiek_region %>%
  split(.$Region) %>%
  map(~cor(.$n_men, .$n_women)) %>%
  bind_rows() %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("Region") %>%
  rename(WspKor = V1) %>%
  mutate(WspKor = round(WspKor, 2))


#### liczba profili ze względu na cechę ----
cechy_podzial <- function(f_cecha, numbers = FALSE) {
  cechy <- full_join(
    profile_women %>%
      filter(cecha == f_cecha) %>%
      count(wartosc) %>%
      mutate(n = 100*n/sum(n)) %>%
      rename(women = n),
    profile_men %>%
      filter(cecha == f_cecha) %>%
      count(wartosc) %>%
      mutate(n = 100*n/sum(n)) %>%
      rename(men = n),
    by = "wartosc") %>%
    mutate(men = if_else(is.na(men), 0, men),
           women = if_else(is.na(women), 0, women))

  if(numbers) cechy$wartosc <- as.numeric(cechy$wartosc)

  plot <- cechy %>%
    gather("plec", "liczba", 2:3) %>%
    ggplot() +
    geom_col(aes(wartosc, liczba, fill = plec),
             position = position_dodge()) +
    scale_fill_manual(values = c("women" = "red", "men" = "blue")) +
    theme(legend.position = "bottom") +
    labs(title = paste0("Procent profili według płci, w podziale na ", f_cecha),
         subtitle = sprintf("Współczynnik korelacji: %.2f", cor(cechy$women, cechy$men)),
         x = "", y = "% profili", fill = "")

  if(!numbers) plot <- plot + coord_flip()

  return(plot)
}

cechy_podzial("Region")

cechy_podzial("Wykształcenie")
cechy_podzial("Stan cywilny")
cechy_podzial("Zawód")

cechy_podzial("Budowa ciała")
cechy_podzial("Kolor oczu")
cechy_podzial("Kolor włosów")

cechy_podzial("Alkohol")
cechy_podzial("Papierosy")

cechy_podzial("Chce dzieci")
cechy_podzial("Dzieci")

cechy_podzial("Wyznanie")
cechy_podzial("Małżeństwo")

cechy_podzial("Wzrost", numbers = TRUE)
cechy_podzial("Wiek", numbers = TRUE)
cechy_podzial("Znak zodiaku")


#### deklaracje vs oczekiwania ----
deklaracje_oczekiwania <- function(f_kategoria, f_cecha_women, f_cecha_men) {
  cecha_women_typ <- if_else(str_sub(f_cecha_women, 1, 2) == "Ja", "deklaracje", "oczekiwania")
  cecha_men_typ <- if_else(str_sub(f_cecha_men, 1, 2) == "Ja", "deklaracje", "oczekiwania")

  tab <- full_join(
    # deklaracje kobiet
    profile_women %>%
      filter(cecha == f_cecha_women) %>%
      select(-cecha) %>%
      separate_rows(wartosc, sep = ",") %>%
      filter(!is.na(wartosc)) %>%
      mutate(wartosc = trimws(wartosc)) %>%
      count(wartosc) %>%
      rename(deklaracja=n),
    # oczekiwania mężczyzn
    profile_men %>%
      filter(cecha == f_cecha_men) %>%
      select(-cecha) %>%
      separate_rows(wartosc, sep = ",") %>%
      filter(!is.na(wartosc)) %>%
      mutate(wartosc = trimws(wartosc)) %>%
      count(wartosc) %>%
      rename(oczekiwanie=n),
    by = "wartosc") %>%
    arrange(wartosc) %>%
    # interesują nas procenty, a nie wartości bezwzględne
    mutate(deklaracja = 100*deklaracja/sum(deklaracja),
           oczekiwanie = 100*oczekiwanie/sum(oczekiwanie))

  tab %>%
    gather("klucz", "liczba", 2:3) %>%
    arrange(desc(wartosc)) %>%
    mutate(wartosc = fct_inorder(wartosc)) %>%
    ggplot() +
    geom_col(aes(wartosc, liczba, fill = klucz),
             position = position_dodge()) +
    scale_fill_manual(values = c("deklaracja" = "red", "oczekiwanie" = "blue"),
                      labels = c("deklaracja" = paste0("kobiety (", cecha_women_typ, ")"),
                                 "oczekiwanie" = paste0("mężczyzni (", cecha_men_typ, ")"))) +
    coord_flip() +
    theme(legend.position = "bottom") +
    labs(title = paste0("Deklaracje a oczekiwania: ", f_kategoria),
         subtitle = sprintf("Współczynnik korelacji: %.2f",
                            cor(tab$deklaracja, tab$oczekiwanie)),
         x = "", y = "% profili", fill = "")
}

deklaracje_oczekiwania("zainteresowania", "Ja Zainteresowania", "Partner Zainteresowania")
deklaracje_oczekiwania("zainteresowania", "Partner Zainteresowania", "Ja Zainteresowania")

deklaracje_oczekiwania("czas wolny", "Ja Czas wolny", "Partner Czas wolny")
deklaracje_oczekiwania("czas wolny", "Partner Czas wolny", "Ja Czas wolny")

deklaracje_oczekiwania("film", "Ja Film", "Partner Film")
deklaracje_oczekiwania("film", "Partner Film", "Ja Film")

deklaracje_oczekiwania("muzyka", "Ja Muzyka", "Partner Muzyka")
deklaracje_oczekiwania("muzyka", "Partner Muzyka", "Ja Muzyka")

deklaracje_oczekiwania("sport", "Ja Sport", "Partner Sport")
deklaracje_oczekiwania("sport", "Partner Sport", "Ja Sport")


# Osobowość
# TODO odmiana!
deklaracje_oczekiwania <- full_join(
  # deklaracje kobiet
  profile_women %>%
    filter(cecha == "Osobowość") %>%
    select(-cecha) %>%
    separate_rows(wartosc, sep = ",") %>%
    filter(!is.na(wartosc)) %>%
    mutate(wartosc = trimws(wartosc)) %>%
    count(wartosc) %>%
    rename(deklaracja=n),
  # oczekiwania mężczyzn
  profile_men %>%
    filter(cecha == "Partner Osobowość") %>%
    select(-cecha) %>%
    separate_rows(wartosc, sep = ",") %>%
    filter(!is.na(wartosc)) %>%
    mutate(wartosc = trimws(wartosc)) %>%
    count(wartosc) %>%
    rename(oczekiwanie=n),
  by = "wartosc") %>%
  arrange(wartosc) %>%
  # interesują nas procenty, a nie wartości bezwzględne
  mutate(deklaracja = 100*deklaracja/sum(deklaracja),
         oczekiwanie = 100*oczekiwanie/sum(oczekiwanie))

deklaracje_oczekiwania %>%
  gather("klucz", "liczba", 2:3) %>%
  arrange(desc(wartosc)) %>%
  mutate(wartosc = fct_inorder(wartosc)) %>%
  ggplot() +
  geom_col(aes(wartosc, liczba, fill = klucz),
           position = position_dodge()) +
  scale_fill_manual(values = c("deklaracja" = "red", "oczekiwanie" = "blue")) +
  coord_flip() +
  theme(legend.position = "bottom") +
  labs(title = "Deklaracje a oczekiwania: osobowość",
       subtitle = sprintf("Współczynnik korelacji: %.2f",
                          cor(deklaracje_oczekiwania$deklaracja, deklaracje_oczekiwania$oczekiwanie)
       ))

