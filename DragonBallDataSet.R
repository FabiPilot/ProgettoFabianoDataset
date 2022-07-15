library(stringr)
library(dplyr)
library(readr)
library(tidyverse)

dragonball <- read.csv("D:/Downloads/DragonBallDS.csv")
nameDivided <-read.csv("D:/Downloads/DragonBallDS.csv")
nameDivided[c('Name', 'Transformation')] <- str_split_fixed(nameDivided$Character, ' ', 2)
nameDivided <- nameDivided[c('Name', 'Power_Level', 'Saga_or_Movie')]
numberCharacter <- nameDivided %>% group_by(Name, Saga_or_Movie) %>% summarise()
numberCharacter <- numberCharacter %>% group_by(Saga_or_Movie) %>% summarise(count = n()) %>% arrange(-count)
numberCharacter <- head(numberCharacter, 10)

numberTransformations <- nameDivided %>% group_by(Saga_or_Movie) %>% summarise(count = n()) %>% arrange(-count)

totalePersonaggi <- nameDivided %>% group_by(Name, Saga_or_Movie) %>% summarise()
totalePersonaggi <- totalePersonaggi %>% group_by(Saga_or_Movie) %>% summarise(count = n()) %>% arrange(-count)

media <- dragonball %>% 
  group_by(Saga_or_Movie) %>%
  summarise_at(vars(Power_Level), list(name = mean))

dragonball <- read.csv("D:/Downloads/DragonBallDS.csv", header=TRUE, stringsAsFactors=FALSE)

dragonball$Power_Level <- gsub(",", "", dragonball$Power_Level) %>%
  as.numeric(dragonball$Power_Level)

mediaLivelli <-  aggregate(Power_Level ~ Saga_or_Movie, dragonball, mean) %>%
  rename(Media = Power_Level)

minori <- full_join(dragonball, mediaLivelli)


j<- 1
for(i in 1:nrow(minori)){
  if(is.na(minori[j,2])){
    minori[j,2] <- 0
  }
  if(is.na(minori[j,5])){
    minori[j,5] <- 0
  }
  if(minori[j,2] < minori[j,5] ){
     minori <- minori[-j,]
  }else{
    j <- j + 1
  }
}

minori <- minori %>% group_by(Saga_or_Movie) %>% summarise(count = n())
minoriRate <- minori %>% inner_join(totalePersonaggi, by = "Saga_or_Movie")

minoriRate<- transform(minoriRate, Ratio = count.x/count.y)
minoriRate<- minoriRate %>% arrange(-Ratio)

minoriRate<- minoriRate[-1,]
minoriRate<- minoriRate[-2,]
minoriRate<- minoriRate[-4,]
minoriRate<- minoriRate[-5,]
minoriRate<- minoriRate[-7,]
minoriRate<- minoriRate[-8,]
minoriRate<- head(minoriRate,10)

kamehameha<- dplyr::filter(dragonball, grepl('Kamehameha', Character))
kamehameha[21, ]["Character"] <- "Super 17's Kamehameha"
kamehameha[23, ]["Character"] <- "Student-Teacher's Kamehameha"
kamehameha[28, ]["Character"] <- "Family's Kamehameha"
kamehameha<- kamehameha %>% mutate(Character = str_remove_all(Character, "\\'.+"))

kamehamehaMedia<- kamehameha %>% 
  group_by(Character) %>%
  summarise_at(vars(Power_Level), list(Media = mean))

kamehamehaGrafico<- kamehamehaMedia %>%
  ggplot(aes(x = Character,
             y = Media,
             fill = Character)) +
  geom_col() +
  scale_y_continuous(trans='log2')

kamehamehaGraficoGoku<- kamehamehaGoku %>%
  ggplot(aes(x = reorder(Saga_or_Movie, Power_Level),
             y = Power_Level)) +
  geom_col(fill = "#A42069") +
  scale_y_continuous(trans='log2') +
  theme_minimal() +
  labs(x = "Saga or Movie")


kamehamehaGoku<- dplyr::filter(kamehameha, grepl('Goku', Character))
kamehamehaGoku<-head(kamehamehaGoku,10)

comparsaPersonaggi<- dragonball %>% group_by(Character) %>% summarise(count = n()) %>% arrange(-count)

evoluzionePrincipali <- head(dragonball, 563)
evoluzionePrincipali<- dplyr::filter(evoluzionePrincipali, grepl('Goku|Krillin|Gohan|Vegeta|Piccolo', Character))
evoluzionePrincipali[c('Name', 'Transformation')] <- str_split_fixed(evoluzionePrincipali$Character, ' ', 2)
evoluzionePrincipali <- evoluzionePrincipali[c('Name', 'Power_Level')]
target <- c("Goku","Krillin","Gohan","Vegeta","Piccolo")
evoluzionePrincipali<- filter(evoluzionePrincipali, Name %in% target)

evoluzioneGokuFreezer<- dragonball[c(184:275),]
evoluzioneGokuFreezer[c('Name', 'Transformation')] <- str_split_fixed(evoluzioneGokuFreezer$Character, ' ', 2)
evoluzioneGokuFreezer <- evoluzioneGokuFreezer[c('Name', 'Power_Level')]
evoluzioneGokuFreezer["Name"][evoluzioneGokuFreezer["Name"] == "Goku's"] <- "Goku"
evoluzioneGokuFreezer["Name"][evoluzioneGokuFreezer["Name"] == "Frieza's"] <- "Frieza"
gokuFrieza<- c("Goku","Frieza")
evoluzioneGokuFreezer<- filter(evoluzioneGokuFreezer, Name %in% gokuFrieza)

evoluzionePrincipali <- evoluzionePrincipali %>%
  group_by(Name) %>%
  summarise(Name = Name,
            Power_Level = Power_Level,
            Count = seq_along(Name))

livelliPotenzaPrincipali <- evoluzionePrincipali %>%
  ggplot(aes(x = Count, y  = Power_Level, group = Name, color = Name)) +
  geom_line() +
  scale_y_continuous(trans='log2')

evoluzioneGokuFreezer <- evoluzioneGokuFreezer %>%
  group_by(Name) %>%
  summarise(Name = Name,
            Power_Level = Power_Level,
            Count = seq_along(Name))

livelliPotenzaGokuFreezer <- evoluzioneGokuFreezer %>%
  ggplot(aes(x = Count, y  = Power_Level, group = Name, color = Name)) +
  geom_line() +
  scale_y_continuous(trans='log2')

sagaComparazioneNumeroPersonaggi <- numberCharacter %>%
  ggplot(aes(y = reorder(Saga_or_Movie, count), x = count)) +
  geom_col() +
  labs(y = "Saga or Movie")

sagaComparazioneLivelli <- minoriRate %>%
  ggplot(aes(y = reorder(Saga_or_Movie, Ratio), x = Ratio)) +
  geom_col() +
  labs(y = "Saga or Movie")

sagaComparazioneNumeroPotenziamenti <- numberTransformations %>%
  arrange(desc(count)) %>%
  head(10) %>%
  ggplot(aes(y = reorder(Saga_or_Movie, count), x = count)) +
  geom_col() +
  labs(y = "Saga or Movie")