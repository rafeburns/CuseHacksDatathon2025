library(vader)
library(tidyverse)
library(stringr)
library(tidytext)
library(pbapply)
library(ggplot2)

data <- read.csv('/Users/rafeburns/Downloads/SYRCityline_Requests_(2021-Present).csv')

data <- data %>%
  filter(Rating %in% c(1,2,3,4,5),
         !is.na(Lat),
         !str_detect(Summary, "(?i)internal"))

cleanedData <- data %>%
  unnest_tokens(word, Description) %>% 
  anti_join(stop_words, by = "word") %>%
  group_by(ObjectId) %>%
  summarise(Description = paste(word, collapse = " "), .groups = "drop") %>%
  left_join(data %>% select(ObjectId, X, Y, Id, Summary, Rating, Address, Agency_Name, Request_type, Lat, Lng, 
                            Created_at_local, Acknowledged_at_local, Closed_at_local, Minutes_to_Acknowledge,
                            Minutes_to_Close, Assignee_name, Category, Sla_in_hours, Report_Source), by = "ObjectId")

#write.csv(data, '/Users/rafeburns/Downloads/SYRCityline_Requests_(2021-Present)_Cleaned.csv')

vaderResults <- cleanedData %>%
  mutate(vaderOutput = pblapply(Description, get_vader))
vaderResults <- vaderResults %>%
  unnest_wider(vaderOutput)
vaderResults <- vaderResults %>%
  mutate(resolved = ifelse(Closed_at_local == "", 0, 1))
resolvedVaderResuls <- vaderResults %>%
  filter(resolved == 1)
resolvedVaderResuls$compound <- as.numeric(resolvedVaderResuls$compound)
resolvedVaderResuls <- resolvedVaderResuls %>%
  mutate(normMintoClose = (Minutes_to_Close - min(Minutes_to_Close)) / 
           (max(Minutes_to_Close) - min(Minutes_to_Close)) * 100)
resolvedVaderResuls <- resolvedVaderResuls %>%
  mutate(normComp = (compound - min(compound)) / 
           (max(compound) - min(compound)) * 100)
resolvedVaderResuls <- resolvedVaderResuls %>%
  filter(normMintoClose < 75)

model <- lm(Minutes_to_Close ~ compound, data = resolvedVaderResuls)
summary(model)
modelB <- lm(Sla_in_hours ~ compound, data = resolvedVaderResuls)
summary(modelB)
plot <- ggplot(data = resolvedVaderResuls) + geom_point(aes(x = compound, y = Minutes_to_Close))
plot

write_csv(vaderResults, '/Users/rafeburns/Downloads/sentimentResults.csv')
