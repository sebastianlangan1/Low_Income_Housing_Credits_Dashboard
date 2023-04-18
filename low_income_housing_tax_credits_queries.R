#Load necessary packages for analysis: 
library(bigrquery)
library(DBI)
library(dplyr)
library(ggplot2)
library(sf)
library(openxlsx)
library(stringr)

#Wikipedia resource for this program: 
#https://en.wikipedia.org/wiki/Low-Income_Housing_Tax_Credit#:~:text=The%20LIHTC%20provides%20funding%20for%20the%20development%20costs,the%20low-income%20units%20in%20a%20rental%20housing%20project.
#U.S. Census Bureau State Population Resource: https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html

#The goal of this analysis is to determine the top five primary factors causing certain states and zip codes to receive more LIHTC funding than others. 
#I'll try to build a machine learning model using the variables I predict to be the most critical in determining the LIHTC allocation amount of a given project. 
#After the model's completion, I'll compare it against online sources to see how well its structure lines up with the reality of the decision-making
#in this government program. 

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

q1_hist <- ggplot(q1_dat, aes(x = State)) + geom_histogram(stat = "count") + ylab('Count') + ggtitle('Counts of Housing Projects with 2017 LIHTC Allocations By State')

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

#Checking whether this relationship between mean state incomes and states' LIHTC allocations still holds when allocations are normalized to states populations
#First, pull in estimated state population data from the U.S. Census Bureau: 
state_pops <- read.xlsx("nst-est2019-01.xlsx") 
state_pops <- state_pops[-c(1:2), ] #Remove top two rows since they aren't contributing any relevant data here 
colnames(state_pops) <- state_pops[1, ] #Replace column names with first row values, and then delete the first row 
state_pops <- state_pops[-1, ]
state_pops <- state_pops[-c(1:5), ]
state_pops <- state_pops[-c(53:57), ]
colnames(state_pops) <-  c("STATE", "Census", "Estimates Base", "2010",  "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")  
#Remove '.' characters from the starts of state names in the STATE column: 
state_pops$STATE <- str_replace(state_pops$STATE, '.', '')
state_pops_2017 <- state_pops %>% select('STATE', '2017')
#Join state populations into q3_dat: 
q3_dat <- q3_dat %>% left_join(state_pops_2017, by = c('STATE'))
q3_dat <- q3_dat %>% rename('POP.2017' = '2017')

q3_dat <- q3_dat %>% mutate(Allocation_PopNorm = Allocation/POP.2017) #Normalize LIHTC allocations by state populations 

#Re-conduct this linear regression with state population-normalized allocations and compare against earlier results:
stateincomepopnorm_allocation_lm <- lm(A_MEAN ~ Allocation_PopNorm, data = q3_dat)
summary(stateincomepopnorm_allocation_lm)

q4_lr_plot2 <- ggplot(q3_dat, aes(x = A_MEAN, y = Allocation_PopNorm)) + geom_point() + geom_smooth(method = 'lm', col = 'black') + ylim(0, 1.5) + xlab('2017 Mean State Incomes ($)') + 
  ylab('2017 LIHTC Allocations Normalized by State Population ($/resident)') + ggtitle('2017 Mean State Incomes vs. 2017 LIHTC Allocations per Housing Project, Normalized by State Populations')

q4_lr_plot2

#Interestingly, it looks like state population moderates the effect of mean state income on LIHTC allocations; without normalizing 
#for mean state income, there is a significant positive correlation between mean state income of projects' states and the allocation amounts projects received. 
#After normalizing for mean state income, there is a significant negative correlation between mean state income and allocation. 

#What is the relationship between state population alone and LIHTC allocations? I would expect the correlation to be positive. 


#Question 5: What were the average allocations provided to LIHTC projects per construction type (see type column) and the distributions of those allocations? Do 
#these allocation amounts significantly differ from one type to the next? 
