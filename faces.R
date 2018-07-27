library(tidyverse)

face_df <- readRDS("face_api.RDS")
women_df <- readRDS("grabed_profiles_women.RDS")%>% distinct() %>% filter(nchar(wartosc) != 0)
# korekty wpisanych regionów
women_df[(women_df$cecha == "Region") & (women_df$wartosc == "pila "), "wartosc"] <- "wielkopolskie"
women_df[(women_df$cecha == "Region") & (women_df$wartosc == "Bielsko-Biała "), "wartosc"] <- "śląskie"

# Cechy z zdjęc
## wiek deklarowany vs wiek ze zdjecia
wiek_porownanie <- left_join(face_df,
                             women_df %>%
                               filter(cecha %in% c("Wiek", "Photo_URL")) %>%
                               spread(cecha, wartosc) %>%
                               filter(Photo_URL %in% face_df$Photo_URL),
                             by = "Photo_URL") %>%
  mutate(Wiek = as.numeric(Wiek))


ggplot(wiek_porownanie, aes(Wiek, age)) +
  geom_jitter(width = 0.25, alpha = 0.5, size = 0.8) +
  geom_smooth() +
  geom_abline(slope = 1, color = "red", alpha = 0.5) +
  labs(x = "Wiek podany w profilu",
       y = "Wiek rozpoznany ze zdjęciu (wg AI)")

# na zdjęciach młodziej niż w rzeczywistości
# dobrze się trzymają czy dają stare zdjęcia?


## emocje a wiek
wiek_porownanie %>%
  select(Wiek, starts_with("emotion_")) %>%
  group_by(Wiek) %>%
  summarise_all(mean) %>%
  ungroup() %>%
  gather("cecha", "val", -Wiek) %>%
  ggplot(aes(Wiek, val, color = cecha)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~cecha, scales = "free_y")


## emocje a wiek - szczęście i neutralność
wiek_porownanie %>%
  select(Wiek, starts_with("emotion_")) %>%
  group_by(Wiek) %>%
  summarise_all(mean) %>%
  ungroup() %>%
  gather("cecha", "val", -Wiek) %>%
  filter(cecha %in% c("emotion_neutral", "emotion_happines")) %>%
  ggplot(aes(Wiek, val, fill = cecha)) +
  geom_col(position = position_fill())


## wiek a kolor włosów
wiek_porownanie %>%
  count(Wiek, hair_color) %>%
  ungroup() %>%
  # procentowy udział w ramach wieku - żeby oderwać się od
  group_by(Wiek) %>%
  mutate(p = n/sum(n)) %>%
  ungroup() %>%
  ggplot(aes(Wiek, p, color = hair_color)) +
  geom_line()


## okulary a wiek
wiek_porownanie %>%
  count(Wiek, glasses) %>%
  ggplot() +
  geom_col(aes(Wiek, n, fill = glasses),
           position = position_fill())


# czytenie a okulary
## te co czytają:
czytajace <- women_df %>%
  filter(cecha == "Ja Zainteresowania" & grepl("czytanie", wartosc)) %>%
  pull(nick)

czytanie_okulary <- bind_rows(
  # czytajace
  women_df %>%
    filter(nick %in% czytajace & cecha == "Photo_URL") %>%
    select(wartosc, nick) %>%
    left_join(face_df, by = c("wartosc" = "Photo_URL")) %>%
    filter(!is.na(faceId)) %>%
    count(glasses) %>%
    mutate(p = 100*n/sum(n)) %>%
    select(-n) %>%
    mutate(typ = "czytanie") %>%
    spread(glasses, p),
  # wszystkie
  wiek_porownanie %>%
    count(glasses) %>%
    mutate(p = 100*n/sum(n)) %>%
    select(-n) %>%
    mutate(typ = "ogol") %>%
    spread(glasses, p))

czytanie_okulary  %>%
  gather("okulary", "procent", -typ) %>%
  ggplot(aes(okulary, procent, fill = typ)) +
  geom_col(position = position_dodge())

czytanie_okularytab <- t(czytanie_okulary) %>%
  as.data.frame() %>%
  rownames_to_column("typ") %>%
  set_names(c("Okulary", "Czytające", "Wszystkie"))
czytanie_okularytab <- czytanie_okularytab[-1, ]

knitr::kable(czytanie_okularytab, row.names = FALSE)


## usmiech vs kolor wlosow
ggplot(wiek_porownanie, aes(hair_color, smile)) +
  geom_boxplot(color = "black", fill = "lightgreen") +
  geom_jitter(width = 0.25, alpha = 0.2)


# usmiech a posiadanie dzieci
inner_join(face_df,
  women_df %>%
    filter(cecha %in% c("Dzieci", "Photo_URL")) %>%
    spread(cecha, wartosc) %>%
    filter(!is.na(Dzieci)),
  by = "Photo_URL") %>%
  ggplot(aes(Dzieci, smile)) +
  geom_boxplot(color = "black", fill = "lightgreen") +
  geom_jitter(width = 0.25, alpha = 0.2)


# uśmiech a wiek i posiadanie dzieci
inner_join(face_df,
           women_df %>%
             filter(cecha %in% c("Dzieci", "Photo_URL", "Wiek")) %>%
             spread(cecha, wartosc) %>%
             filter(!is.na(Dzieci)),
           by = "Photo_URL") %>%
  mutate(Wiek = as.numeric(Wiek)) %>%
  group_by(Wiek, Dzieci) %>%
  summarise(smile = mean(smile)) %>%
  ungroup() %>%
  ggplot(aes(Wiek, smile)) +
  geom_point(aes(color = Dzieci), alpha = 0.6) +
  geom_smooth(aes(color = Dzieci), se = FALSE)


# uśmiech a region
inner_join(face_df,
           women_df %>%
             filter(cecha %in% c("Region", "Photo_URL")) %>%
             spread(cecha, wartosc) %>%
             filter(!is.na(Region)),
           by = "Photo_URL") %>%
  group_by(Region) %>%
  filter(smile >= quantile(smile, 0.125), smile <= quantile(smile, 0.875)) %>%
  mutate(m_smile = median(smile)) %>%
  ungroup() %>%
  arrange(m_smile) %>%
  mutate(Region = fct_inorder(Region)) %>%
  ggplot(aes(Region, smile)) +
  geom_boxplot(color = "black", fill = "lightgreen") +
  geom_jitter(width = 0.25, alpha = 0.1) +
  coord_flip()


# uśmiech a osobowość
inner_join(face_df,
           women_df %>%
             filter(cecha %in% c("Osobowość", "Photo_URL")) %>%
             spread(cecha, wartosc) %>%
             filter(!is.na(`Osobowość`)) %>%
             rename(Osobowosc = `Osobowość`) %>%
             separate_rows(Osobowosc, sep = ",") %>%
             mutate(Osobowosc = trimws(Osobowosc)),
           by = "Photo_URL") %>%
  group_by(Osobowosc) %>%
  filter(smile >= quantile(smile, 0.125), smile <= quantile(smile, 0.875)) %>%
  mutate(m_smile = median(smile)) %>%
  ungroup() %>%
  arrange(m_smile) %>%
  mutate(Osobowosc = fct_inorder(Osobowosc)) %>%
  ggplot(aes(Osobowosc, smile)) +
  geom_boxplot(color = "black", fill = "lightgreen") +
  geom_jitter(width = 0.25, alpha = 0.1) +
  coord_flip()

# uśmiech a muzyka
inner_join(face_df,
           women_df %>%
             filter(cecha %in% c("Ja Muzyka", "Photo_URL")) %>%
             spread(cecha, wartosc) %>%
             filter(!is.na(`Ja Muzyka`)) %>%
             separate_rows(`Ja Muzyka`, sep = ","),
           by = "Photo_URL") %>%
  rename(muzyka = `Ja Muzyka`) %>%
  group_by(muzyka) %>%
  # odrzucamy skrajności - dość grubo, zostawiając 75% ze środka
  filter(smile >= quantile(smile, 0.125), smile <= quantile(smile, 0.875)) %>%
  mutate(m_smile = median(smile)) %>%
  ungroup() %>%
  arrange(m_smile) %>%
  mutate(muzyka = fct_inorder(muzyka)) %>%
  ggplot(aes(muzyka, smile)) +
  geom_boxplot(color = "black", fill = "lightgreen") +
  geom_jitter(width = 0.25, alpha = 0.1) +
  coord_flip()


