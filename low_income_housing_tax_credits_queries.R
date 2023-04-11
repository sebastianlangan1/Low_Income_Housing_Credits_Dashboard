#Load necessary packages for analysis: 
library(bigrquery)
library(DBI)
library(dplyr)
library(ggplot2)
library(sf)
library(openxlsx)

#Wikipedia resource for this program: 
#https://en.wikipedia.org/wiki/Low-Income_Housing_Tax_Credit#:~:text=The%20LIHTC%20provides%20funding%20for%20the%20development%20costs,the%20low-income%20units%20in%20a%20rental%20housing%20project.

#The goal of this analysis is to determine the primary factors causing certain states and zip codes to have received more LITHC funding than others. 
#After its completion, I'll compare final results against online sources to see how well my predictions line up with the reality of this government program. 

#Pull in the database table from Google BigQuery using BigQuery R API: 
low_income_data_BQproj_id <- 'deep-span-382614'

#Questions 1/Query 1: What is the distribution of property states?
q1_query <- "SELECT hud_id, project, proj_add, proj_st, allocamt, FROM `low_income_housing_tax_credit_program.2017_lihtc_database_hud` 
WHERE allocamt IS NOT NULL
AND hud_id IS NOT NULL
AND project IS NOT NULL
AND proj_add IS NOT NULL
AND proj_st IS NOT NULL 
AND proj_cty IS NOT NULL
AND proj_zip IS NOT NULL"

q1_tb <- bq_project_query(low_income_data_BQproj_id, q1_query)
q1_dat <- bq_table_download(q1_tb)
q1_dat <- q1_dat %>% rename(State = proj_st)

q1_hist <- ggplot(q1_dat, aes(x = State)) + geom_histogram(stat = "count")

q1_hist

#Question/Query 2: What does the geographic distribution of low income housing tax credits look like (i.e., use the latitude/longitude data provided to make a sort of heat map for this)
q2_query <- "SELECT hud_id, project, proj_st, latitude, longitude, FROM `low_income_housing_tax_credit_program.2017_lihtc_database_hud` 
WHERE allocamt IS NOT NULL
AND hud_id IS NOT NULL
AND project IS NOT NULL
AND proj_add IS NOT NULL
AND proj_st IS NOT NULL 
AND latitude IS NOT NULL 
AND longitude IS NOT NULL"

#Use the 'sf' package to plot geospatial data: 
q2_tb <- bq_project_query(low_income_data_BQproj_id, q2_query)
q2_dat <- bq_table_download(q2_tb)
q2_dat <- q2_dat %>% rename(State = proj_st)

q2_sf <- st_as_sf(q2_dat, coords = c('latitude', 'longitude'))
q2_sf <- st_set_crs(q2_sf, 4326)

#Fix the axes here later: 
#ggplot(q2_sf) + geom_sf(aes(color = State))

#Question/Query 3: Exploratory Analysis: Across all properties, which states and zip codes received the highest average low-income housing tax credit funding? 
q3_query <- "SELECT hud_id, project, proj_add, proj_st, proj_cty, proj_zip, allocamt, FROM `low_income_housing_tax_credit_program.2017_lihtc_database_hud` 
WHERE allocamt IS NOT NULL
AND hud_id IS NOT NULL
AND project IS NOT NULL
AND proj_add IS NOT NULL
AND proj_st IS NOT NULL 
AND proj_cty IS NOT NULL
AND proj_zip IS NOT NULL"

q3_tb <- bq_project_query(low_income_data_BQproj_id, q3_query)
q3_dat <- bq_table_download(q3_tb)
q3_dat <- q3_dat %>% group_by(proj_st) %>% rename(State = proj_st, Allocation = allocamt)

#Create box plot to help answer this question: 
q3_box_plot <- ggplot(q3_dat, aes(x = State, y = Allocation)) + 
  geom_boxplot() + ylim(0, 2500000)

q3_box_plot

#It looks like Maryland and Florida are roughly tied for the highest average allocation amount. 

#Question 4: Exploratory Analysis: Across all properties, what is the correlation between average per capita state income and total allocations per state? 
#Given the aims of the LIHTC program, it would make sense for poorer states to receive more housing tax credit allocations. 
#Does this relationship change after normalizing each housing project's allocations by state population? 
#If state populations are a moderator or mediator of this relationship, then controlling for population this way might decrease the correlation 
#between mean state income and LIHTC allocation, warranting further investigation. 

#Pull in raw state income data from 2017: 
incomes_bls_2017 <- read.xlsx("state_M2017_dl.xlsx") %>% filter(OCC_TITLE == 'All Occupations')
#Link to table: https://www.bls.gov/oes/tables.htm

#Left-join average state incomes into the dataframe from the previous question: 
incomes_bls_2017 <- incomes_bls_2017 %>% select(STATE, A_MEAN)

#Create column with state abbreviations in the BLS data: 
incomes_bls_2017 <- incomes_bls_2017 %>% mutate(STATE_ABB = state.abb[match(STATE, state.name)])
q3_dat <- q3_dat %>% rename(STATE_ABB = State)

#Join BLS state income data onto the main LIHTC data frame for this question: 
q3_dat <- q3_dat %>% left_join(incomes_bls_2017, by = c('STATE_ABB')) #Join by state abbreviation.
q3_dat$A_MEAN <- as.numeric(q3_dat$A_MEAN)

#Plot mean income per state (across all occupations) versus projects' allocation amounts. Also overlay the OLS linear regression trendline and its statistics. 
stateincome_allocation_lm <- lm(A_MEAN ~ Allocation, data = q3_dat)
summary(stateincome_allocation_lm)
#There does appear to be a significant correlation between mean state income and total LIHTC allocations. 
q4_lr_plot <- ggplot(q3_dat, aes(x = A_MEAN, y = Allocation)) + geom_point() + geom_smooth(method = 'lm', col = 'black') + ylim(0, 2500000) + xlab('2017 Mean State Incomes ($)') + 
  ylab('2017 LIHTC Allocation ($)') + ggtitle('2017 Mean State Incomes vs. 2017 LIHTC Allocations per Housing Project')

q4_lr_plot

#Checking whether this relationship still holds when project allocations are normalized 


#Question 5:
