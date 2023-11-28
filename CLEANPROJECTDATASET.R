library(dplyr)
library(stringr)

disorder_df <- read.csv("Mental health Depression disorder Data.csv")
gender_df <- read.csv("prevalence-of-depression-males-vs-females.csv")

proj_df <- merge(x = disorder_df, y = gender_df, by = c("Entity", "Year", "Code"), all.x = TRUE)

proj_df <- filter(proj_df, !is.na(Depression....))
proj_df <- filter(proj_df, !is.na(Prevalence...Depressive.disorders...Sex..Male...Age..Age.standardized..Percent.))

#Catergorical: 

proj_df$depression.population <- round(((proj_df$Depression....)/100) * proj_df$Population..historical.estimates.)

# Continuous:
mean_depression<- mean(proj_df$Depression....)
proj_df$depression.severity <- ifelse(proj_df$Depression....> mean_depression, "Above average Depression percent", "Below average Depression percent")

#Summarization:
Entity_grp <- group_by(proj_df, Entity)
Entity_grp_df <- summarise(
  Entity_grp,
  avg_depression = mean(Depression...., na.rm = TRUE)
)

write.csv(proj_df, "projectdataframe.csv")