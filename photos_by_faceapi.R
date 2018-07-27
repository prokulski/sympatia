# tutaj potrzebujesz swojego klucza z https://azure.microsoft.com/pl-pl/services/cognitive-services/face/
face_api_key <- "***"

face_api_url <- "https://westcentralus.api.cognitive.microsoft.com/face/v1.0/detect?returnFaceAttributes=age,gender,smile,glasses,emotion,hair,makeup,occlusion,accessories,noise"


library(tidyverse)
library(httr)

profile_df <- readRDS("grabed_profiles_women.RDS") %>% distinct() %>% filter(nchar(wartosc) != 0)

# unikalne URLe do zdjęć
photos <- profile_df %>%
  filter(cecha == "Photo_URL") %>%
  group_by(wartosc) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n == 1)


face_df <- tibble()

# wszystkie kolejne zdjęcia puszamy przez rozpoznawanie wieku
for(i in 1:length(photos$wartosc)) {
  cat(paste0("i = ", i))

  body_image <- photos$wartosc[i]

  result <- POST(face_api_url,
                 add_headers(.headers = c("Content-Type"="application/json",
                                          "Ocp-Apim-Subscription-Key"=face_api_key)),
                 body = paste0("{ \"url\": \"", body_image, "\" }"))


  if(rawToChar(result$content) != "[]") {
    cat(" - taken!")
    # Coverting Output into R Dataframe
    df <- fromJSON(rawToChar(result$content), flatten = TRUE)

    df <- df %>%
      select(faceId,
             smile=faceAttributes.smile,
             gender=faceAttributes.gender,
             age=faceAttributes.age,
             glasses=faceAttributes.glasses,
             emotion_anger=faceAttributes.emotion.anger,
             emotion_contempt=faceAttributes.emotion.contempt,
             emotion_disgust=faceAttributes.emotion.disgust,
             emotion_fear=faceAttributes.emotion.fear,
             emotion_happines=faceAttributes.emotion.happiness,
             emotion_neutral=faceAttributes.emotion.neutral,
             emotion_sadness=faceAttributes.emotion.sadness,
             emotion_surprise=faceAttributes.emotion.surprise,
             faceAttributes.hair.hairColor)

    if(!is.null(nrow(df$faceAttributes.hair.hairColor[[1]]))) {
      df <- df %>%
        unnest(faceAttributes.hair.hairColor) %>%
        group_by(faceId) %>%
        filter(confidence == max(confidence)) %>%
        ungroup() %>%
        select(-confidence) %>%
        rename(hair_color = color) %>%
        mutate(Photo_URL = body_image)
    } else {
      df <- df %>%
       select(-faceAttributes.hair.hairColor) %>%
        mutate(Photo_URL = body_image,
               hair_color = NULL)
    }
    face_df <- bind_rows(face_df, df)

  }
  cat("\n")
  Sys.sleep(4) # 20 zapytan na minute - czekamy nieco ponad 3 sekundy pomiedzy zapytaniami

}


saveRDS(face_df, "face_api.RDS")
