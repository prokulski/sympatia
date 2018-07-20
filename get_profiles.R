library(tidyverse)
library(rvest)
library(jsonlite)

#### LOGOWANIE ----

# login i hasło do serwisu
# sympatia_login = "***"
# sympatia_pass = "***"

# moje dane zapisane w pliku
load("sympatia_cred.Rda")

base_url <- "https://sympatia.onet.pl"

# tworzymy sesję, w ramach której wypełnimy formularz:
www_sess <- html_session(base_url)

# szukamy wszystkich formularzy
form <- html_form(www_sess)

# wypełniamy pola pierwszego formularza (w kopii)
filled_form <- set_values(form[[1]], login = sympatia_login, pass = sympatia_pass)

# wysyłamy formularz w ramach sesji - powinniśmy być zalogowani
form_response <- submit_form(www_sess, filled_form)


#### POBRANIE LISTY PROFILI ----

# w pętli obsłużyć wszystkie strony wyników szukania - ostatni parametr pytania ajaxa mówi o numerze strony
# po zalogowaniu idziemy do wyszukiwarki - pytamy bezpośrednio o JSONa z ajaxa

no_page <- 0
profile_links_all <- NULL
offset <- 0
current_profile <- 1e5 # jakaś liczba na początek większa od offset, w pętli zostanie zmieniona

while(offset < current_profile) {

  # w ramach tej samej sesji pobieramy kolejne strony z wynikami wyszukiwania
  search_results <- www_sess %>%
    # parametry wyszukiwania:
    ## płec: kobieta
    ## wiek: 18-40
    ## kraj: PL
    ## ostatnie logowanie: 1 miesiąc
    ## sortowanie po dacie ostatniego logowania
    jump_to(paste0("https://sympatia.onet.pl/ajax-search-results.html/?params%5Bsex%5D=m&params%5BageRange%5D%5B%5D=18&params%5BageRange%5D%5B%5D=40&params%5Bcountry%5D:PL&params%5BlastLoginPeriod%5D=1month&params%5Bsort%5DloginDate&params%5BcurrentPage%5D=", no_page))

  # sprawdzić czy się udało pobrać JSONa czy coś innego
  if(search_results$response$headers$`content-type` == "application/json") {
    # wyciągamy to co jest w JSONie
    json <- rawToChar(search_results$response$content)

    response <- fromJSON(json)

    # do którego profilu w wynikach doszliśmy i ile jest wszystkich?
    offset <- response$metrics$offset
    current_profile <- response$metrics$total
    cat(paste0("offset = ", offset, " / total ", current_profile, "\r"))


    # w JSONie jest HTML z jedną stroną wyników
    response_html <- response$html$items

    if(response_html == "") break;

    # wyciągamy z tego linki do profili
    profile_links <- read_html(response_html) %>%
      html_nodes("div.item") %>%
      html_node("a") %>%
      html_attr("href") %>%
      .[!is.na(.)] %>%
      paste0(base_url, .)

    # doklejamy je do już zebranych
    profile_links_all <- c(profile_links_all, profile_links)
  }

  # kolejna strona
  no_page <- no_page + 1

  # chwile czekamy
  # Sys.sleep(0.5)
}


# ile zebranych linków?
length(profile_links_all)

# zostawiamy tylko unikalne (w czasie zbierania linków coś mogło się zmienić w wynikach wyszukiwania)
profile_links_all <- unique(profile_links_all)

# ile unikatowych?
length(profile_links_all)

saveRDS(profile_links_all, "profile_links_all_men.RDS")


#### POBRANIE ZAWARTOSCI PROFILU ----


# funkcja pobiera informacje z profilu i zwraca je w postaci DF
get_profile_info <- function(f_profile_url) {

  # wczytanie strony w ramach sesji
  profile_page <- www_sess %>% jump_to(f_profile_url)

  # błąd 404
  if(profile_page$response$status_code != 200)  return(tibble())

  # nick
  nick <- profile_page %>% html_node("h1") %>% html_text() %>% trimws()

  # dla tych co nie chcą sie pokazywać kontom bez zdjęć
  if(nick == "Tej osobie zależy, żeby widzieć z kim rozmawia") return(tibble())

  # dla zawieszonych konta
  if(grepl("zawiesił swoje konto", nick)) return(tibble())


  # wiek i płeć
  wiek_plec <- profile_page %>% html_node("div.age") %>% html_text()
  wiek <- str_split(wiek_plec, ", ") %>% unlist() %>% .[[1]] %>% str_split(" ") %>% unlist() %>% .[[1]]
  plec <- str_split(wiek_plec, ", ") %>% unlist() %>% .[[2]]

  # region
  region <- profile_page %>% html_node("div.region") %>% html_text() %>% trimws() %>% str_replace_all("\n|\t|\r", "") %>% str_split(", ") %>% unlist()
  miasto <- region[[1]]
  region <- region[[2]]

  # o mnie
  if(profile_page %>% html_node("div.aboutMeBox") %>% is.na()) {
    o_mnie <- NA
  } else {
    o_mnie <- profile_page %>% html_node("div.aboutMeBox") %>% html_node("div.content") %>% html_text() %>% trimws()
  }

  # podstawowe info:
  infogroup <- profile_page %>% html_node("div#moreInfo") %>% html_nodes("div.infoGroup") %>% html_text() %>% trimws() %>% str_split(":", simplify = TRUE) %>% as_tibble() %>% set_names(c("cecha", "wartosc")) %>% mutate(wartosc = str_replace_all(wartosc, "\n|\t|\r", ""))


  # dodać - to może nie istnieć w profilu!
  ## Mój wymarzony partner
  partner_df <- tibble()
  if(!profile_page %>% html_node("div#partnerToSlide") %>% is.na()) {
    partner_page <- profile_page %>% html_node("div#partnerToSlide") %>% html_node("div.profileSlideContainer")
    ### Sport
    if(!partner_page %>% html_node("div.idealPartner-partnerSport") %>% is.na()) {
      partner_df <- bind_rows(partner_df,
                              tibble(cecha = "Partner Sport",
                                     wartosc = partner_page %>% html_node("div.idealPartner-partnerSport") %>% html_node("div.value") %>% html_text()
                              ))
    }
    ### Czas wolny
    if(!partner_page %>% html_node("div.idealPartner-partnerFreetime") %>% is.na()) {
      partner_df <- bind_rows(partner_df,
                              tibble(cecha = "Partner Czas wolny",
                                     wartosc = partner_page %>% html_node("div.idealPartner-partnerFreetime") %>% html_node("div.value") %>% html_text()
                              ))
    }
    ### Zainteresowania
    if(!partner_page %>% html_node("div.idealPartner-partnerInterest") %>% is.na()) {
      partner_df <- bind_rows(partner_df,
                              tibble(cecha = "Partner Zainteresowania",
                                     wartosc = partner_page %>% html_node("div.idealPartner-partnerInterest") %>% html_node("div.value") %>% html_text()
                              ))
    }
    ### Film
    if(!partner_page %>% html_node("div.idealPartner-partnerMovie") %>% is.na()) {
      partner_df <- bind_rows(partner_df,
                              tibble(cecha = "Partner Film",
                                     wartosc = partner_page %>% html_node("div.idealPartner-partnerMovie") %>% html_node("div.value") %>% html_text()
                              ))
    }
    ### Muzyka
    if(!partner_page %>% html_node("div.idealPartner-partnerMusic") %>% is.na()) {
      partner_df <- bind_rows(partner_df,
                              tibble(cecha = "Partner Muzyka",
                                     wartosc = partner_page %>% html_node("div.idealPartner-partnerMusic") %>% html_node("div.value") %>% html_text()
                              ))
    }
    ### Osobowość
    if(!partner_page %>% html_node("div.idealPartner-partnerCharacter") %>% is.na()) {
      partner_df <- bind_rows(partner_df,
                              tibble(cecha = "Partner Osobowość",
                                     wartosc = partner_page %>% html_node("div.idealPartner-partnerCharacter") %>% html_node("div.value") %>% html_text()
                              ))
    }
  }

  ## Moje zainteresowania
  ja_df <- tibble()
  if(!profile_page %>% html_node("div#interestToSlide") %>% is.na()) {
    ja_page <- profile_page %>% html_node("div#interestToSlide") %>% html_node("div.profileSlideContainer")
    ### Sport
    if(!ja_page %>% html_node("div.aboutMeGroup-sport") %>% is.na()) {
      ja_df <- bind_rows(ja_df,
                         tibble(cecha = "Ja Sport",
                                wartosc = ja_page %>% html_node("div.aboutMeGroup-sport") %>% html_node("div.value") %>% html_text() %>% gsub("\n", ",", .) %>% trimws() %>% gsub("^,|,$|\r|\t", "", .)
                         ))
    }
    ### Czas wolny
    if(!ja_page %>% html_node("div.aboutMeGroup-freetime") %>% is.na()) {
      ja_df <- bind_rows(ja_df,
                         tibble(cecha = "Ja Czas wolny",
                                wartosc = ja_page %>% html_node("div.aboutMeGroup-freetime") %>% html_node("div.value") %>% html_text() %>% gsub("\n", ",", .) %>% trimws() %>% gsub("^,|,$|\r|\t", "", .)
                         ))
    }
    ### Zainteresowania
    if(!ja_page %>% html_node("div.aboutMeGroup-interest") %>% is.na()) {
      ja_df <- bind_rows(ja_df,
                         tibble(cecha = "Ja Zainteresowania",
                                wartosc = ja_page %>% html_node("div.aboutMeGroup-interest") %>% html_node("div.value") %>% html_text() %>% gsub("\n", ",", .) %>% trimws() %>% gsub("^,|,$|\r|\t", "", .)
                         ))
    }
    ### Film
    if(!ja_page %>% html_node("div.aboutMeGroup-movie") %>% is.na()) {
      ja_df <- bind_rows(ja_df,
                         tibble(cecha = "Ja Film",
                                wartosc = ja_page %>% html_node("div.aboutMeGroup-movie") %>% html_node("div.value") %>% html_text() %>% gsub("\n", ",", .) %>% trimws() %>% gsub("^,|,$|\r|\t", "", .)
                         ))
    }
    ### Muzyka
    if(!ja_page %>% html_node("div.aboutMeGroup-music") %>% is.na()) {
      ja_df <- bind_rows(ja_df,
                         tibble(cecha = "Ja Muzyka",
                                wartosc = ja_page %>% html_node("div.aboutMeGroup-music") %>% html_node("div.value") %>% html_text() %>% gsub("\n", ",", .) %>% trimws() %>% gsub("^,|,$|\r|\t", "", .)
                         ))
    }
  }

  # skladamy w jedna tabelke
  profil <- bind_rows(infogroup,
                      partner_df,
                      ja_df,
                      tibble(cecha = "Wiek", wartosc = wiek),
                      tibble(cecha = "Płeć", wartosc = plec),
                      tibble(cecha = "Miasto", wartosc = miasto),
                      tibble(cecha = "Region", wartosc = region),
                      tibble(cecha = "O mnie", wartosc = o_mnie),
                      tibble(cecha = "Photo_URL", wartosc = profile_page %>% html_node("div.wrapMain") %>% html_node("img") %>% html_attr("src"))
  ) %>%
    mutate(nick = nick) %>%
    na.omit()

  return(profil)
}



#### POBRANIE PROFILI ----

# teraz wszystkie zebrane linki analizujemy powyższą funkcją:

# purrr way
# profile_df <- map_df(profile_links_all, get_profile_info)


# klasycznie:
profiles_df <- tibble()
i <- 1





# jak sie wypierdoli można zacząc z tego miejsca
start_i <- i + 1

for(i in start_i:length(profile_links_all)) {
  cat(paste0(i, " / ", length(profile_links_all), "\r"))

  # pobieramy dane z kolejnego profilua
  profiles_df <- bind_rows(profiles_df, get_profile_info(profile_links_all[[i]]))

  # co 25 profili zapisujemy dane
  if(i %% 25 == 0) saveRDS(profiles_df, "grabed_profiles_men.RDS")

  # chwile czekamy
  # Sys.sleep(0.5)
}

# liczba zebranych profili - potrzeba okolo 20 tys
profiles_df %>% select(nick) %>% distinct() %>% nrow()






#### ZAPISANIE WSZYSTKICH PROFILI ----

# zapisujemy finalnie zebrane linki
saveRDS(profiles_df, "grabed_profiles_men.RDS")
