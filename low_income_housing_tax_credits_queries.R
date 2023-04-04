#Load necessary packages for analysis: 
library(bigrquery)
library(DBI)
library(dplyr)

#Pull in the database table from Google BigQuery using BigQuery R API: 

low_income_data_con <- 'deep-span-382614'
sql <- "SELECT * FROM `low_income_housing_tax_credit_program.2017_lihtc_database_hud`"

tb <- bq_project_query(low_income_data_con, sql)

bq_table_download(tb, n_max = 10)
