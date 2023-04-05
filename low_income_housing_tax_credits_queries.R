#Load necessary packages for analysis: 
library(bigrquery)
library(DBI)
library(dplyr)
library(ggplot2)

#Wikipedia resource for this program: 
#https://en.wikipedia.org/wiki/Low-Income_Housing_Tax_Credit#:~:text=The%20LIHTC%20provides%20funding%20for%20the%20development%20costs,the%20low-income%20units%20in%20a%20rental%20housing%20project.

#Pull in the database table from Google BigQuery using BigQuery R API: 
low_income_data_BQproj_id <- 'deep-span-382614'

#Question/Query 1: What is the distribution of property states?

#Question/Query 2: What does the geographic distribution of low income housing tax credits look like (i.e., use the laditude/longtitude data provided to make a sort of heatmap for this)

#Question/Query 3: Exploratory Analysis: Across all properties, which states and zip codes received the highest average low-income housing tax credit funding? 
q1_query <- "SELECT hud_id, project, proj_add, proj_st, proj_cty, proj_zip, allocamt, FROM `low_income_housing_tax_credit_program.2017_lihtc_database_hud` 
WHERE allocamt IS NOT NULL
AND hud_id IS NOT NULL
AND project IS NOT NULL
AND proj_add IS NOT NULL
AND proj_st IS NOT NULL 
AND proj_cty IS NOT NULL
AND proj_zip IS NOT NULL"

q1_tb <- bq_project_query(low_income_data_BQproj_id, q1_query)
q1_dat <- bq_table_download(q1_tb)
q1_dat <- q1_dat %>% group_by(proj_st) %>% mutate(`Average Allocation` = mean(allocamt)) %>% rename(State = proj_st, Allocation = allocamt)

#Create box plot to help answer this question: 
q1_box_plot <- ggplot(q1_dat, aes(x = State, y = Allocation)) + 
  geom_boxplot() + ylim(0, 2500000)

q1_box_plot

#Question 4: How well do average state incomes correlate with average LIHTC allocations per state? 
#Pull in average state income data: 