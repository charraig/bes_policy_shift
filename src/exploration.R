library(ggplot2)
library(skimr)

dim(raw_df)
relevant_df %>% distinct(id) %>% count()
View(wave_id_wide)
View(wave_id_wide %>% filter(wave1==0 & wave2==0))
View(outcome_df)

# Spot check to see if things are right
# Use id == 30590 for a respondent who joined in wave 2
outcome_df %>% filter(id == 30590) %>% View()
outcome_df %>% filter(is.na(lr1)) %>% View()
outcome_df %>% summarize(minwave = min(wave), maxwave=max(wave))
outcome_df %>% 
  group_by(id) %>% 
  summarize(ind_sd = sd(redistSelf, na.rm=TRUE)) %>% 
  ggplot(aes(x=ind_sd)) +
  geom_histogram()
tidy_outcome <- outcome_df %>% 
  spread('wave',redistSelf)
drop_na()
tidy_outcome %>% View()
tidy_outcome %>% skim()
outcome_df %>% ggplot(aes(x=factor(wave))) +
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()

relevant_df %>% names() %>% find_cols('selfOccStatus')

# Examine a tibble nested in clean_groups
clean_groups %>% slice(1) %>% pull(profile_work_type)
ls <- clean_groups %>% slice(1) %>% pull(profile_work_type)
View(ls[[1]])
