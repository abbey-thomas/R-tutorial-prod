# Load necessary packages----
# Note: download the latest version of speechcollectr using the following command:
# install.packages("devtools")
devtools::install_github("abbey-thomas/speechcollectr")
library(dplyr)
library(tidyr)
library(shiny)
library(speechcollectr)

# Demographic survey prep----
# Get the basic demographic survey data frame from speechcollectr
data("demographics")
# If desired, view this table of survey questions using the following:
# View(demographics)

# Create another vector with the same names as the column in the demographics file
# To ask participants a question about their native language
# Note that since this is a free-form text response (as denoted by `type = 'textInput'`),
# We do not need to add an answer options (hence options = NA),
# but we still need to include the options element in the vector
# to ensure it joins properly to the table of other questions in "demographics"
nat_lang <- c(id = "native",
              priority = "required",
              label = "What do you consider to be your first or native language?",
              type = "textInput",
              options = NA)

# Bind the native language to the rest of the demographic survey
survey <- rbind(demographics, nat_lang)

# Save the complete survey
write.csv(survey, "survey.csv", row.names = FALSE)

# Create the application's "www" directory
# Move the survey into the 'www' directory
# Both of these can be accomplished with a single call to speechcollectr::wwwPrep()
wwwPrep("survey.csv")

# Check the survey to make sure everything is formatted properly
feedback <- surveyPrep(questionFile = "www/survey.csv",
                       notListedLab = "Not listed:")

# Production Stimuli prep----
# Emotionally neutral words used by Kim & Sumner (2017) go in one column,
# Repeated four times, one set per block
# Each block is assigned an emotion that we want the participants to use when producing the words
# Each emotion will be associated with a simple line drawing of an emoji expressing that emotion
# Icon names correspond to those in the "Font Awesome" library (http://fontawesome.io)
stim_short <- data.frame(word = rep(c("adequate", "multiply", "compose",
                                      "pending", "specialist"), 4),
                         block = sort(rep(1:4, 5))) %>%
  mutate(emotion = ifelse(block == 1, "NEUTRAL",
                          ifelse(block == 2, "HAPPY",
                                 ifelse(block == 3, "SAD", "ANGRY"))))  %>%
  mutate(icon = ifelse(emotion == "NEUTRAL", "meh",
                       ifelse(emotion == "ANGRY", "angry",
                              ifelse(emotion == "SAD", "sad-tear",
                                     "smile"))))

write.csv(stim_short, "www/stimuli_short.csv", row.names = FALSE)

# Finally, make a folder inside 'www' to hold the data from the participants
dir.create("www/outputs")
