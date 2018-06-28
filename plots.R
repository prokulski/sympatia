library(tidyverse)

profile_df <- readRDS("grabed_profiles.RDS") %>% distinct()
# liczba profili:
length(unique(profile_df$nick))

#### FUNKCJE RYSUJACE WYKRESY ----
plot_histogram <- function(f_cecha) {
  profile_df %>%
    filter(cecha == f_cecha) %>%
    mutate(wartosc = as.numeric(wartosc)) %>%
    ggplot() +
    geom_histogram(aes(wartosc), binwidth = 1)
}


plot_density <- function(f_cecha) {
  profile_df %>%
    filter(cecha == f_cecha) %>%
    mutate(wartosc = as.numeric(wartosc)) %>%
    ggplot() +
    geom_density(aes(wartosc))
}


plot_bars <-  function(f_cecha, f_flip = FALSE, f_sort = TRUE) {
  p <- profile_df %>%
    filter(cecha == f_cecha) %>%
    separate_rows(wartosc, sep = ",") %>%
    mutate(wartosc = trimws(wartosc)) %>%
    count(wartosc)

  if(f_sort) {
    if(f_flip) {
      p <- p %>% arrange(n)
    } else {
      p <- p %>% arrange(desc(n))
    }
    p <- p %>% mutate(wartosc = fct_inorder(wartosc))
  }

  p <- p %>%
    ggplot() +
    geom_col(aes(wartosc, n))

  if(f_flip) p <- p + coord_flip()

  return(p)
}

plot_heatmap <- function(f_cechaA, f_cechaB) {
  full_join(profile_df %>% filter(cecha == f_cechaA) %>% separate_rows(wartosc, sep = ",") %>% mutate(wartosc = trimws(wartosc)) %>% select(-cecha),
            profile_df %>% filter(cecha == f_cechaB) %>% separate_rows(wartosc, sep = ",") %>% mutate(wartosc = trimws(wartosc)) %>% select(-cecha),
            by = "nick") %>%
    na.omit() %>%
    set_names(c("a", "nick", "b")) %>%
    count(a, b) %>%
    ungroup() %>%
    group_by(a) %>%
    mutate(p = 100*n/sum(n)) %>%
    mutate(m = ifelse(p == max(p), TRUE, FALSE)) %>%
    ungroup() %>%
    arrange(desc(b)) %>% mutate(b = fct_inorder(b)) %>%
    ggplot() +
    geom_tile(aes(a, b, fill = p, color = m), show.legend = FALSE) +
    geom_text(aes(a, b, label = sprintf("%.0f%%", p), color = m, size = m), show.legend = FALSE) +
    scale_fill_distiller(palette = "RdYlGn") +
    scale_color_manual(values = c("TRUE" = "darkred", "FALSE" = "gray30")) +
    scale_size_manual(values = c("TRUE" = 4, "FALSE" = 3)) +
    labs(x = f_cechaA, y = f_cechaB)
}

plot_bars_top <- function(f_cechaA, f_cechaB) {
  full_join(profile_df %>% filter(cecha == f_cechaA) %>% separate_rows(wartosc, sep = ",") %>% mutate(wartosc = trimws(wartosc)) %>% select(-cecha),
            profile_df %>% filter(cecha == f_cechaB) %>% separate_rows(wartosc, sep = ",") %>% mutate(wartosc = trimws(wartosc)) %>% select(-cecha),
            by = "nick") %>%
    na.omit() %>%
    set_names(c("a", "nick", "b")) %>%
    count(a, b) %>%
    filter(a != "", b != "") %>%
    ungroup() %>%
    group_by(b) %>%
    mutate(p = 100*n/sum(n)) %>%
    top_n(5, p) %>%
    mutate(m = n == max(n)) %>%
    ungroup() %>%
    arrange(desc(a)) %>%
    mutate(a = fct_inorder(a)) %>%
    ggplot() +
    geom_col(aes(a, p, color = m, fill = m), show.legend = FALSE) +
    geom_text(aes(a, p, label = sprintf("%.0f%%", p), size = m), hjust = -0.1, show.legend = FALSE) +
    facet_wrap(~b, scales = "free_y", ncol = 4) +
    scale_y_continuous(expand = c(0.3, 0)) +
    scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
    scale_fill_manual(values = c("TRUE" = "darkred", "FALSE" = "lightblue")) +
    scale_size_manual(values = c("TRUE" = 3, "FALSE" = 2)) +
    coord_flip() +
    labs(y = "% profili", x = "",
         title = paste0("Najpopularniejsze ", f_cechaA, " według ", f_cechaB))
}


#### WYKRESY ----

plot_bars("Wiek", f_sort = FALSE)

plot_density("Wzrost")
plot_bars("Wzrost", f_sort = FALSE)

plot_bars("Wykształcenie")
plot_bars("Miasto", f_flip = TRUE)
plot_bars("Region", f_flip = TRUE)
plot_bars("Dzieci")
plot_bars("Papierosy")
plot_bars("Alkohol")
plot_bars("Stan cywilny")


plot_bars("Kolor włosów")
plot_bars("Wyznanie")
plot_bars("Budowa ciała")
plot_bars("Małżeństwo")
plot_bars("Znak zodiaku")
plot_bars("Chce dzieci")
plot_bars("Kolor oczu")
plot_bars("Języki", f_flip = TRUE)
plot_bars("Szukam", f_flip = TRUE)
plot_bars("Zawód", f_flip = TRUE)
plot_bars("Osobowość", f_flip = TRUE)

plot_bars("Partner Sport", f_flip = TRUE)
plot_bars("Partner Czas wolny", f_flip = TRUE)
plot_bars("Partner Zainteresowania", f_flip = TRUE)
plot_bars("Partner Film", f_flip = TRUE)
plot_bars("Partner Muzyka", f_flip = TRUE)
plot_bars("Partner Osobowość", f_flip = TRUE)

plot_bars("Ja Sport", f_flip = TRUE)
plot_bars("Ja Czas wolny", f_flip = TRUE)
plot_bars("Ja Zainteresowania", f_flip = TRUE)
plot_bars("Ja Film", f_flip = TRUE)
plot_bars("Ja Muzyka", f_flip = TRUE)


plot_heatmap("Papierosy", "Alkohol")
plot_bars_top("Alkohol", "Papierosy")


plot_heatmap("Budowa ciała", "Wzrost")
plot_bars_top("Wzrost", "Budowa ciała")

plot_heatmap("Wzrost", "Budowa ciała")
# Bayes!

plot_heatmap("Kolor włosów", "Kolor oczu")
plot_heatmap("Kolor oczu", "Kolor włosów")
plot_bars_top("Kolor oczu", "Kolor włosów")

plot_heatmap("Wykształcenie", "Zawód")
plot_heatmap("Zawód", "Wykształcenie") + theme(axis.text.x = element_text(angle = 90))
plot_bars_top("Zawód", "Wykształcenie")

plot_heatmap("Dzieci", "Chce dzieci")

plot_heatmap("Stan cywilny", "Dzieci")

plot_heatmap("Wyznanie", "Małżeństwo")

plot_heatmap("Wyznanie", "Stan cywilny")


plot_heatmap("Szukam", "Osobowość")
plot_heatmap("Osobowość", "Szukam") + theme(axis.text.x = element_text(angle = 90))


plot_heatmap("Języki", "Osobowość")


plot_heatmap("Stan cywilny", "Osobowość")
plot_heatmap("Stan cywilny", "Szukam")

plot_heatmap("Alkohol", "Szukam")
plot_heatmap("Szukam", "Alkohol")

plot_heatmap("Chce dzieci", "Szukam")

plot_heatmap("Wyznanie", "Szukam")
plot_heatmap("Małżeństwo", "Szukam")


plot_heatmap("Ja Sport", "Partner Sport") + theme(axis.text.x = element_text(angle = 90))
plot_heatmap("Ja Czas wolny", "Partner Czas wolny") + theme(axis.text.x = element_text(angle = 90))
plot_heatmap("Ja Zainteresowania", "Partner Zainteresowania") + theme(axis.text.x = element_text(angle = 90))
plot_heatmap("Ja Film", "Partner Film") + theme(axis.text.x = element_text(angle = 90))
plot_heatmap("Ja Muzyka", "Partner Muzyka") + theme(axis.text.x = element_text(angle = 90))


plot_heatmap("Wiek", "Partner Czas wolny")
plot_heatmap("Partner Czas wolny", "Wiek")
plot_bars_top("Partner Czas wolny", "Wiek")
plot_bars_top("Wiek", "Partner Czas wolny")

plot_heatmap("Wiek", "Ja Czas wolny")
plot_heatmap("Wiek", "Ja Film")
plot_heatmap("Wiek", "Ja Muzyka")

plot_heatmap("Ja Muzyka", "Wiek")


plot_heatmap("Stan cywilny", "Partner Czas wolny")
plot_heatmap("Stan cywilny", "Ja Czas wolny")

plot_heatmap("Wykształcenie", "Partner Czas wolny")
plot_heatmap("Wykształcenie", "Ja Czas wolny")

plot_heatmap("Wykształcenie", "Wiek")
plot_heatmap("Wiek", "Wykształcenie")




plot_bars_top("Ja Czas wolny", "Wiek")

plot_bars_top("Ja Film", "Wiek")
plot_bars_top("Ja Muzyka", "Wiek")

plot_bars_top("Wiek", "Ja Muzyka")


plot_bars_top("Partner Czas wolny", "Stan cywilny")
plot_bars_top("Ja Czas wolny", "Stan cywilny")

plot_bars_top("Partner Czas wolny", "Wykształcenie")
plot_bars_top("Ja Czas wolny", "Wykształcenie")


plot_bars_top("Partner Sport", "Ja Sport")
plot_bars_top("Ja Sport", "Partner Sport")

