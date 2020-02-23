library(DataExplorer)
library(ggplot2)

# Generic profiling report
create_report(outcome_df, 
              output_file = 'outcome_report.html',
              output_dir = './eda')

# Find ids with no responses
missing_id <- outcome_df %>% 
  select(-wave) %>% 
  group_by(id) %>% 
  summarize_all(funs(sum(!is.na(.)))) %>% 
  gather(key = 'feature', value = 'response', -id) %>% 
  filter(response >= 3)
outcome_df %>% 
  filter(id %in% pull(missing_id, id)) %>% 
  arrange(id, wave) %>% 
  View()
View(missing_id)

pca_df <- na.omit(outcome_df %>% select(-id, -wave))
plot_prcomp(pca_df, variance_cap = 1)
# 53.8% of variance explained by first component -- combines most of lr and redist
# 14.6% explained by second component -- uses mostly leftRight

missing <- outcome_df %>%
  select(-id) %>%
  group_by(wave) %>% 
  summarise_all(funs(sum(is.na(.))/length(.))) %>% 
  gather(key = 'feature', value = 'missing', -wave)
missing %>% 
  ggplot(aes(x=feature, y=missing)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    facet_wrap(vars(wave))
