library(readr)
library(tidytuesdayR)
library(tidyverse)
library(tidytext) #new package
library(textdata) #new package
library(jsonlite)
library(dplyr)
library(tidytext)
library(tidyr)
library(DescTools)
library(randomForest) # Install new package for Random Forest
library(openintro) # Install new package for data
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(shadowtext)
library(ggfx)
library(ggtext)
library(showtext)
library(sysfonts)

# --- LOAD DATA ---

stranger_words <- read_csv("stranger_words.csv")

tidytuesday <- tidytuesdayR::tt_load('2022-10-18')
st_episodes <- tidytuesday$episodes
st_dialogue <- tidytuesday$stranger_things_all_dialogue

url <- "https://raw.githubusercontent.com/jeffreylancaster/stranger-things/master/data/episodes.json"
raw_json <- fromJSON(url)

episodes <- raw_json$episodes
scenes <- episodes$scenes

# --- GET FONT --- 

showtext_auto()
font_add(family = "Benguiat", regular = "itc-benguiat-std/ITCBenguiatStdBoldCn.OTF")

# --- DATA CLEANING --- 

#Unnest scene data from the dataset 
scenes_data <- raw_json$episodes %>% 
  unnest(scenes) %>% 
  mutate(scene_id = row_number()) #scene identification

#List of the prominent Season 1 and Season 2 characters, as 
#they are the most important interactions
main_cast <- c("Eleven", "Mike Wheeler", "Dustin Henderson", "Lucas Sinclair", 
               "Will Byers", "Jim Hopper", "Joyce Byers", "Nancy Wheeler",
               "Jonathan Byers", "Steve Harrington", "Max Mayfield")


scene_matrix <- scenes_data %>%
  unnest_wider(characters)  %>% #regular unnest did not work ->   https://tidyr.tidyverse.org/reference/unnest_wider.html
  #https://tidyr.tidyverse.org/reference/separate_rows.html
  separate_rows(name, sep = ",") %>% 
  
  #This selects only those in the main cast
  filter(name %in% main_cast)%>%
  dplyr::select(1:12, scene_id) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = name, 
              values_from = present, 
              values_fill = 0) #LINK

#This creates a start and end time for each scene; the data from JSON is 
#counting down, so the first scene begins at 48:21. That had to be adjusted 
clean_scenes <- scene_matrix %>%
  mutate(
    raw_start = period_to_seconds(hms(sceneStart)), #https://lubridate.tidyverse.org/reference/period_to_seconds.html, 
    #https://lubridate.tidyverse.org/reference/hms.html
    raw_end   = period_to_seconds(hms(sceneEnd)) 
  ) %>%
  group_by(seasonNum, episodeNum) %>% #groups into every episode
  mutate(anchor_time = max(raw_start)) %>% #finds what the largest start time 
  #is, as that is the length of the show 
  ungroup() %>%
  mutate(
    scene_start_elapsed = anchor_time - raw_start,
    scene_end_elapsed   = anchor_time - raw_end
  ) 

clean_dialogue <- st_dialogue %>%
  filter(!is.na(start_time)) %>% #if scene doesn't have start time, it is cut 
  mutate(
    dialogue_start = period_to_seconds(hms(start_time)) #finds the second 
    #that the dialogue started so that it can be matched into a scene
  )

#most likely not exact, so it catches that
scenes_ready <- clean_scenes %>%
  mutate(
    start_buffer = scene_start_elapsed - 2,  
    end_buffer   = scene_end_elapsed + 2
  )

#Joins the two datasets on when the dialogue starts 
final_dataset <- clean_dialogue %>%
  inner_join(
    scenes_ready, 
    by = join_by(
      season == seasonNum,   
      episode == episodeNum, 
      dialogue_start >= start_buffer,
      dialogue_start <= end_buffer   
    )
  )

#Cleans up the final dataset
final_dataset <- final_dataset %>%
  group_by(season, episode, dialogue_start) %>%
  slice(1) %>% #CHECK THIS 
  ungroup() %>%
  filter(!is.na(dialogue))

final_dataset_15 <- final_dataset %>%
  filter(season==1&episode==5)

#This unnests words 
final_clean_words <- final_dataset %>%
  unnest_tokens(word, dialogue)

#This counts the words 
final_count_words_2 <- final_clean_words %>%
  count(word, sort=TRUE)

# --- SENTIMENT ANALYSIS ---

data(stop_words)

#Joins the word counts to stop words, eliminating stop words through an anti-join
tidy_fcw_2 <- final_count_words_2 %>%
  anti_join(stop_words, by="word")

#Loads sentiments data 
sentiments_afinn <-get_sentiments("afinn")
sentiments_bing <-get_sentiments("bing")
sentiments_nrc <-get_sentiments("nrc")

#joins with sentiments from Bing 
sentiment_st <-tidy_fcw_2%>%
  inner_join(sentiments_bing, by="word")

#This filters so its only the most common words 
sentiment_st%>%
  filter(n > 16) %>%
  mutate(word= reorder(word, n)) #makes sure it is a factor

#creates sentiment as a factor 
sentiment_st$sentiment <- as.factor((sentiment_st$sentiment))
levels(sentiment_st$sentiment)
levels(sentiment_st$sentiment) <- c("Negative", "Positive")

#pivots the sentiments to be wider, and to only do distinct words
sentiments_nrc <- sentiments_nrc %>%
  mutate(val = 1) %>%
  distinct(word, sentiment, .keep_all = TRUE) %>% 
  pivot_wider(names_from = sentiment, values_from = val, values_fill = 0)

#Binds the Stranger Things words and sentiment words together 
sentiments_st <- rbind(sentiments_nrc, stranger_words)

#Changes existing words that are expletives
target_words <- c("god", "jesus", "christ", "lord")

sentiments_st <- sentiments_st %>%
  mutate(
    joy      = ifelse(word %in% target_words, 0, joy),
    trust    = ifelse(word %in% target_words, 0, trust),
    positive = ifelse(word %in% target_words, 0, positive),
    negative = ifelse(word %in% target_words, 1, negative),
    anger    = ifelse(word %in% target_words, 1, anger), 
    surprise = ifelse(word %in% target_words, 1, surprise)
  )

#Changes words that are negative in Stranger Things
trauma_words <- c("rainbow", "sunflower", "gate")
sentiments_st <- sentiments_st %>%
  mutate(
    joy      = ifelse(word %in% trauma_words, 0, joy),
    trust    = ifelse(word %in% trauma_words, 0, trust),
    positive = ifelse(word %in% trauma_words, 0, positive),
    fear     = ifelse(word %in% trauma_words, 1, fear),
    negative = ifelse(word %in% trauma_words, 1, negative),
    joy      = ifelse(word == "crazy", 1, joy),
    trust    = ifelse(word == "crazy", 1, trust),
    fear     = ifelse(word == "ghost", 0, fear),
    joy      = ifelse(word == "ghost", 1, joy)
  )

# --- GRAPH 1: SENTIMENT GRAPH WORDS ---

sentiment_st %>%
  group_by(sentiment) %>%
  slice_max(n, n = 25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  scale_fill_manual(values = c("Negative" = "#B1281E", "Positive"="#81A1C1"))+
  geom_col(show.legend = FALSE, color="black") +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to Sentiment",y = NULL, 
       title="Most Common Words in Stranger Things Seasons 1 and 2", 
       subtitle="Grouped by Sentiment")+theme_minimal()+
  theme(text = element_text(color="white",family = "Benguiat", size = 12))

# --- MORE DATA CLEANING --- 

#Creates row number so data is not lost and can still be grouped by row
processed_data <- final_dataset %>%
  mutate(row_id = row_number()) 

#Unnests words and joins with sentiments on words
sentiment_scores <- processed_data %>%
  unnest_tokens(word, dialogue)  %>%
  inner_join(sentiments_st, by = "word")

#Sums the sentiments of each line grouping by row id
sentiment_scores_2 <- sentiment_scores %>%
  group_by(row_id) %>%
  summarize(
    has_fear = sum(fear),
    has_disgust = sum(disgust),
    has_anger = sum(anger),
    has_anticipation = sum(anticipation),
    has_trust = sum(trust), 
    has_sadness = sum(sadness),
    has_surprise = sum(surprise),
    has_joy = sum(joy),
    has_negative = sum(negative),
    has_positive = sum(positive)
  )

#joins this data with the rest of the Stranger Things data
test_join <- processed_data %>%
  right_join(sentiment_scores_2, by="row_id")

# --- SENTIMENT SCORING --- 

#The equation used to determine sentiment
score_data <- test_join %>%
  mutate(total_score = (-has_fear)-has_disgust-has_anger+
           has_trust-has_sadness+has_joy+(has_positive*0.25)-
           (has_negative*0.25))%>%
  mutate(total_score_int = case_when(
    has_disgust!=0&has_fear!=0 ~ total_score - (0.5*has_disgust*has_fear),
    has_trust!=0&has_fear!=0 ~ total_score + (0.5*has_trust*has_fear),
    has_trust!=0&has_sadness!=0 ~ total_score - (0.5*has_trust*has_sadness),
    TRUE ~ total_score
  ))%>%
  mutate(total_score_surp = case_when(
    total_score == 0 ~ (-1.5*has_surprise),
    has_surprise != 0 ~ (1+(has_surprise*0.25))*total_score_int,
    TRUE ~ total_score_int
  )) %>%
  mutate(final_total = case_when(
    total_score_surp == 0 ~ (-has_anticipation),
    has_anticipation==0 ~ total_score_surp,
    has_anticipation != 0 ~ total_score_surp*(1+(0.25*has_anticipation))
  )) 

# --- MORE DATA CLEANING --- 

#
score_data_clean <- score_data %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) 
#replaces blank values with 0 if the column is numeric,
#https://dplyr.tidyverse.org/reference/across.html

#Selects only the columns needed 
stranger_things <- score_data_clean %>%
  dplyr::select(season, episode, dialogue, location, subLocation, 20:30, final_total)

#This makes the names cleaner 
names(stranger_things) <- gsub(" ", "_", names(stranger_things)) #R's version of RegEx, https://www.digitalocean.com/community/tutorials/sub-and-gsub-function-r

#This creates factors for every character that they either have a 1 or 0 if present
stranger_things$Mike_Wheeler <- as.factor(stranger_things$Mike_Wheeler)
stranger_things$Dustin_Henderson <- as.factor(stranger_things$Dustin_Henderson)
stranger_things$Lucas_Sinclair <- as.factor(stranger_things$Lucas_Sinclair)
stranger_things$Will_Byers <- as.factor(stranger_things$Will_Byers)
stranger_things$Nancy_Wheeler <- as.factor(stranger_things$Nancy_Wheeler)
stranger_things$Jim_Hopper <- as.factor(stranger_things$Jim_Hopper)
stranger_things$Joyce_Byers <- as.factor(stranger_things$Joyce_Byers)
stranger_things$Jonathan_Byers <- as.factor(stranger_things$Jonathan_Byers)
stranger_things$Steve_Harrington <- as.factor(stranger_things$Steve_Harrington)
stranger_things$Eleven <- as.factor(stranger_things$Eleven)
stranger_things$Max_Mayfield <- as.factor(stranger_things$Max_Mayfield)

#This changes the levels into "Yes" and "No"
levels(stranger_things$Mike_Wheeler) <- c("No", "Yes")
levels(stranger_things$Dustin_Henderson) <- c("No", "Yes")
levels(stranger_things$Lucas_Sinclair) <- c("No", "Yes")
levels(stranger_things$Will_Byers) <- c("No", "Yes")
levels(stranger_things$Nancy_Wheeler) <- c("No", "Yes")
levels(stranger_things$Jim_Hopper) <- c("No", "Yes")
levels(stranger_things$Joyce_Byers) <- c("No", "Yes")
levels(stranger_things$Jonathan_Byers) <- c("No", "Yes")
levels(stranger_things$Steve_Harrington) <- c("No", "Yes")
levels(stranger_things$Eleven) <- c("No", "Yes")
levels(stranger_things$Max_Mayfield) <- c("No", "Yes")


#This selects all columns but dialogue 
stranger_things_train <- stranger_things %>%
  select(-dialogue)

# --- MACHINE LEARNING PIPELINE --- 

#Splits the data into training and testing data 
train.index <- createDataPartition(stranger_things_train$Mike_Wheeler, 
                                   p = 0.8, list = FALSE)
mike_train <- stranger_things_train[train.index,]
mike_test <- stranger_things_train[-train.index,]

#Creates a random forest model with every variable 
set.seed(233)
rf_model <- randomForest(Mike_Wheeler ~ ., data = mike_train)

oob.error.data <-data.frame(
  Trees=rep(1:nrow(rf_model$err.rate),times=3),
  Type=rep(c("OOB", "Not Present", "Present"), each=nrow(rf_model$err.rate)),
  Error=c(rf_model$err.rate[,"OOB"],
          rf_model$err.rate[,"No"],
          rf_model$err.rate[,"Yes"]))

#Plots error rate 
ggplot(data=oob.error.data, aes(x=Trees, y=Error))+
  geom_line(aes(color=Type))+theme_minimal()+
  theme(text = element_text(family = "Benguiat", size = 12))

# Define tuning grid: try several values of mtry
tune_grid <- expand.grid(mtry = 1:10)
# Set up the training function
ctrl <- trainControl(method = "oob") # Use OOB estimate for training evaluation
# Fit the randomForest using different mtry values
set.seed(345)

#Tunes the model with only three characters, subLocation, and 
#final sentiment total 
rf_tuned_small <- train(Mike_Wheeler ~ Dustin_Henderson+Will_Byers+
                          Lucas_Sinclair+subLocation+final_total,
                        data = mike_train,
                        method = "rf",
                        trControl = ctrl,
                        tuneGrid = tune_grid,
                        ntree = 150,
)

#Creates this model 
set.seed(5632)
rf_model_final_small <- randomForest(Mike_Wheeler ~ Dustin_Henderson+Will_Byers+
                                       Lucas_Sinclair+subLocation+final_total, 
                                     data = mike_train,
                                     mtry=rf_tuned_small$finalModel$tuneValue$mtry, 
                                     ntree=150, importance=T)

#Tunes the model with every variable 
set.seed(345)
rf_tuned <- train(Mike_Wheeler ~ .,
                  data = mike_train,
                  method = "rf",
                  trControl = ctrl,
                  tuneGrid = tune_grid,
                  ntree = 150,
)

#Creates the model with every variable 
set.seed(5632)
rf_model_final <- randomForest(Mike_Wheeler ~ ., 
                               data = mike_train,
                               mtry=rf_tuned$finalModel$tuneValue$mtry, 
                               ntree=150, importance=T)

#Model evaluation: All variables 
predicted_test <- predict(rf_model_final, newdata=mike_test, type="response")
predicted_prob_test <- predict(rf_model_final, newdata=mike_test, type="prob")
ROC<- roc(mike_test$Mike_Wheeler~predicted_prob_test[,2], plot=TRUE,legacy.axes=T)
test_pred <- predict(rf_model_final, newdata = mike_test)
conf_matrix <- confusionMatrix(data=test_pred, mike_test$Mike_Wheeler)
cat("\n\n-----ALL VARIABLES-----\n\n")
auc(ROC)
print(rf_model_final)
print(conf_matrix)


#Model evaluation: Small
predicted_test_small <- predict(rf_model_final_small, 
                                newdata=mike_test, type="response")
predicted_prob_test_small <- predict(rf_model_final_small, 
                                     newdata=mike_test, type="prob")
ROC_small<- roc(mike_test$Mike_Wheeler~predicted_prob_test_small[,2], 
                plot=TRUE,legacy.axes=T)
test_pred_small <- predict(rf_model_final_small, newdata = mike_test)
conf_matrix_small <- confusionMatrix(data=test_pred_small, mike_test$Mike_Wheeler)
cat("\n\n-----SMALL-----\n\n")
auc(ROC_small)
print(rf_model_final_small)
print(conf_matrix_small)

varImpPlot(rf_model_final, main = "Influential Factors Random Forest")
varImpPlot(rf_model_final_small, main = "Influential Factors Random Forest Small")


# --- FINAL SENTIMENT ANALYSIS GRAPH --- 

stranger_things <- stranger_things %>%
  group_by(season, episode) %>%
  mutate(row_id = row_number()) %>%
  ungroup()

stranger_things_mike_will_onlysubl <- stranger_things %>%
  filter(Mike_Wheeler=="Yes"|Will_Byers=="Yes") %>%
  dplyr::select(season, episode, dialogue, location, subLocation, Mike_Wheeler, 
                Will_Byers, final_total, row_id) %>%
  group_by(subLocation, season, Mike_Wheeler, Will_Byers) %>%
  summarize(sent_average = mean(final_total, na.rm = TRUE)) %>%
  mutate(character_app = case_when(
    Mike_Wheeler=="Yes"&Will_Byers=="Yes"~"Mike+Will",
    Mike_Wheeler=="No"&Will_Byers=="Yes"~"Will",
    Mike_Wheeler=="Yes"&Will_Byers=="No"~"Mike",
  )) %>%
  filter(subLocation %in% c("Arcade", "Byers Home","Wheeler Home"))%>%
  ungroup()%>%
  filter(season==2)

ggplot(stranger_things_mike_will_onlysubl, aes(x=subLocation, y=sent_average)) + 
  geom_col(aes(fill=character_app), position=position_dodge(preserve="single"), color="black")+scale_fill_manual(values = c("Mike" = "#20265c", "Will"="#e3c52d",                                                                                             "Mike+Will"="#099112"))+
  labs(x = "Sublocation",y = "Sentiment Average", 
       title="Sentiment Average per Sub-Location in Season 2", 
       subtitle="Based on Mike & Will", fill="Character Appearance")+
  theme_minimal()+theme(text = element_text(family = "Benguiat", size = 12)) 