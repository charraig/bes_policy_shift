library(DataExplorer)
library(ggplot2)

# Generic profiling report
create_report(info_df, 
              output_file = 'predictors_report.html',
              output_dir = './eda')

