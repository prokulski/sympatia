#### FUNKCJE RYSUJACE WYKRESY ----
library(tidyverse)

plot_density <- function(f_cecha) {
  profile_df %>%
    filter(cecha == f_cecha) %>%
    mutate(wartosc = as.numeric(wartosc)) %>%
    ggplot() +
    geom_density(aes(wartosc), fill = "lightblue", color = "gray50") +
    labs(title = "",
         x = f_cecha, y = "",
         subtitle = paste("Na podstawie analizy", nprofiles, "profili z Sympatia.onet.pl"),
         caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")
}



plot_bars <-  function(f_cecha, f_flip = FALSE, f_sort = TRUE, numbers = TRUE) {
  p <- profile_df %>%
    filter(cecha == f_cecha) %>%
    separate_rows(wartosc, sep = ",") %>%
    mutate(wartosc = trimws(wartosc)) %>%
    count(wartosc) %>%
    ungroup() %>%
    mutate(proc = 100*n/sum(n))

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
    geom_col(aes(wartosc, proc), fill = "lightgreen", color = "gray50")

  if(numbers) {
    p <- p + geom_text(aes(wartosc, proc, label = sprintf("%.0f%%", proc)))
  }

  if(f_flip) p <- p + coord_flip()


  p <- p + labs(title = "",
                x = f_cecha, y = "% profili",
                subtitle = paste("Na podstawie analizy", nprofiles, "profili z Sympatia.onet.pl"),
                caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")


  return(p)
}



plot_heatmap <- function(f_cechaA, f_cechaB,
                         numbers = TRUE,
                         a_numeric = FALSE, b_numeric = FALSE) {
  plot_data <- inner_join(profile_df %>% filter(cecha == f_cechaA) %>% separate_rows(wartosc, sep = ",") %>% mutate(wartosc = trimws(wartosc)) %>% select(-cecha),
                          profile_df %>% filter(cecha == f_cechaB) %>% separate_rows(wartosc, sep = ",") %>% mutate(wartosc = trimws(wartosc)) %>% select(-cecha),
                          by = "nick") %>%
    set_names(c("a", "nick", "b")) %>%
    count(a, b) %>%
    ungroup() %>%
    group_by(a) %>%
    mutate(p = 100*n/sum(n)) %>%
    mutate(m = ifelse(p == max(p), TRUE, FALSE)) %>%
    ungroup() %>%
    arrange(desc(b)) %>% mutate(b = fct_inorder(b))

  if(a_numeric) plot_data <- plot_data %>% mutate(a = as.numeric(a))
  if(b_numeric) plot_data <- plot_data %>% mutate(b = as.numeric(b))

  if(numbers) {
    plot_data %>%
      ggplot() +
      geom_tile(aes(a, b, fill = p, color = m), show.legend = FALSE) +
      geom_text(aes(a, b, label = sprintf("%.0f%%", p), color = m, size = m), show.legend = FALSE) +
      scale_fill_distiller(palette = "RdYlGn") +
      scale_color_manual(values = c("TRUE" = "darkred", "FALSE" = "gray30")) +
      scale_size_manual(values = c("TRUE" = 4, "FALSE" = 3)) +
      labs(title = "",
           x = f_cechaA, y = f_cechaB,
           subtitle = paste("Na podstawie analizy", nprofiles, "profili z Sympatia.onet.pl"),
           caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")
  } else {
    plot_data %>%
      ggplot() +
      geom_tile(aes(a, b, fill = p, color = m), show.legend = FALSE) +
      scale_fill_distiller(palette = "RdYlGn") +
      scale_color_manual(values = c("TRUE" = "darkred", "FALSE" = "gray30")) +
      labs(title = "",
           x = f_cechaA, y = f_cechaB,
           subtitle = paste("Na podstawie analizy", nprofiles, "profili z Sympatia.onet.pl"),
           caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")
  }
}



plot_bars_top <- function(f_cechaA, f_cechaB, n_col = 4) {
  inner_join(profile_df %>% filter(cecha == f_cechaA) %>% separate_rows(wartosc, sep = ",") %>% mutate(wartosc = trimws(wartosc)) %>% select(-cecha),
             profile_df %>% filter(cecha == f_cechaB) %>% separate_rows(wartosc, sep = ",") %>% mutate(wartosc = trimws(wartosc)) %>% select(-cecha),
             by = "nick") %>%
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
    facet_wrap(~b, scales = "free_y", ncol = n_col) +
    scale_y_continuous(expand = c(0.3, 0)) +
    scale_color_manual(values = c("TRUE" = "darkred", "FALSE" = "darkgreen")) +
    scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "lightgreen")) +
    scale_size_manual(values = c("TRUE" = 3, "FALSE" = 2)) +
    coord_flip() +
    labs(title = paste0("Najpopularniejsze ", f_cechaA, " według ", f_cechaB),
         x = f_cechaA, y = "% profili",
         subtitle = paste("Na podstawie analizy", nprofiles, "profili z Sympatia.onet.pl"),
         caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")
}
