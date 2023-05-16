#Load necessary packages for analysis: 
library(bigrquery)
library(DBI)
library(dplyr)
library(ggplot2)
library(sf)
library(openxlsx)
library(stringr)
library(cowplot)


##RESOURCES##
#Wikipedia resource for this program: 
#https://en.wikipedia.org/wiki/Low-Income_Housing_Tax_Credit#:~:text=The%20LIHTC%20provides%20funding%20for%20the%20development%20costs,the%20low-income%20units%20in%20a%20rental%20housing%20project.
#U.S. Census Bureau State Population Resource: https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html
#https://dzone.com/articles/variable-importance-and-how-it-is-calculated Variable importance article 
#https://www.cuemath.com/algebra/covariance-matrix/ Covariance matrix article 
#https://statisticsbyjim.com/regression/identifying-important-independent-variables/ Metrics for feature importance in regression or classification 
#https://www.learnbymarketing.com/603/variable-importance/#:~:text=A%20Decision%20Tree%20crawls%20through%20your%20data%2C%20one,to%20help%20determine%20which%20variables%20are%20most%20important.
#https://towardsdatascience.com/11-dimensionality-reduction-techniques-you-should-know-in-2021-dcb9500d388b
#http://sthda.com/english/wiki/ggplot2-density-plot-quick-start-guide-r-software-and-data-visualization #Combine density plots and histograms 

##GOAL##
#The primary goal of this analysis is to determine the top five primary factors causing certain states and zip codes to receive more LIHTC funding than others. 
#I'll try to build a machine learning model using the variables I predict to be the most critical in determining the LIHTC allocation amount of a given project. 
#After the model's completion, I'll compare it against online sources to see how well its structure lines up with the reality of the decision-making
#in this government program. 

#Pull in the database table from Google BigQuery using BigQuery R API: 
low_income_data_BQproj_id <- 'deep-span-382614'

########Section 1: Descriptive Statistics of the LIHTC Project (What are the data, broken out into categories such as physical construction, geography, resident demographics, etc.?)#####################

##DESCRIPTIVE STATISTICS OF ALLOCATION AMOUNTS: Average Allocation, Std. of Allocation, etc.
#Should create a basic table for this data. 
#(Will not make complex plots for these basic statistics; just text or a basic table most likely in PowerBI)
allocs_descriptive_stats_query <- "SELECT hud_id, project, proj_add, proj_st, proj_cty, proj_zip, allocamt FROM `low_income_housing_tax_credit_program.2017_lihtc_database_hud` 
  WHERE CONCAT(hud_id, project, proj_add, proj_st, proj_cty, proj_zip, allocamt) IS NOT NULL"
allocs_descriptive_stats <- bq_project_query(low_income_data_BQproj_id, allocs_descriptive_stats_query)
allocs_descriptive_stats_dat <- bq_table_download(allocs_descriptive_stats)

#Computing descriptive stats for allocation amounts, both broken out by state and overall: 
allocs_descriptive_stats_dat <- allocs_descriptive_stats_dat %>% mutate(MEAN_ALLOC = mean(as.numeric(allocamt)), STD_ALLOC = sd(as.numeric(allocamt)), MED_ALLOC = median(as.numeric(allocamt)),
                                                                        MIN_ALLOC = min(as.numeric(allocamt)), MAX_ALLOC = max(as.numeric(allocamt)), VAR_ALLOC = var(as.numeric(allocamt))) 
#Compute descriptive stats by state and format dataframe for use in PowerBI table: 
allocs_descriptive_stats_dat <- allocs_descriptive_stats_dat %>% group_by(proj_st) %>% mutate(MEAN_ALLOC_BYSTATE = mean(as.numeric(allocamt)), STD_ALLOC_BYSTATE = sd(as.numeric(allocamt)), MED_ALLOC_BYSTATE = median(as.numeric(allocamt)),
                                                                                              MIN_ALLOC_BYSTATE = min(as.numeric(allocamt)), MAX_ALLOC_BYSTATE = max(as.numeric(allocamt)), VAR_ALLOC_BYSTATE = var(as.numeric(allocamt)),
                                                                                              COUNTS_ALLOC_BYSTATE = n()) %>% ungroup() 

allocs_descriptive_stats_dat <- allocs_descriptive_stats_dat %>% rename(State = proj_st, Allocation = allocamt)

allocs_descriptive_stats_PowerBISummTable <- allocs_descriptive_stats_dat %>% select(State, MEAN_ALLOC, STD_ALLOC, MED_ALLOC, MIN_ALLOC, MAX_ALLOC, VAR_ALLOC, MEAN_ALLOC_BYSTATE,
                                                                                     STD_ALLOC_BYSTATE, MED_ALLOC_BYSTATE, MIN_ALLOC_BYSTATE, MAX_ALLOC_BYSTATE, VAR_ALLOC_BYSTATE, 
                                                                                     COUNTS_ALLOC_BYSTATE) %>% distinct()

#Create box plot visualization of allocation amounts to be provided alongside descriptive stats table in PowerBI:
allocs_box_plot <- ggplot(allocs_descriptive_stats_dat, aes(x = State, y = Allocation)) + 
  geom_boxplot() + ylim(0, 2500000) + ylab('LIHTC Allocation Amount ($)') + ggtitle('2017 LIHTC Allocations by State') + scale_fill_manual(values = c('red'))
allocs_box_plot






###DESCRIPTIVE STATISTICS OF GEOGRAPHICAL HOUSING PROJECT CHARACTERISTICS###



###DESCRIPTIVE STATISTICS OF PHYSICAL HOUSING PROJECT CHARACTERISTICS###



###DESCRIPTIVE STATISTICS OF HOUSING PROJECT DEMOPGRAPHIC CHARACTERISTICS###




#Questions 1/Query 1: What is the distribution of property states?
# q1_query <- "SELECT hud_id, project, proj_add, proj_st, allocamt, FROM `low_income_housing_tax_credit_program.2017_lihtc_database_hud` 
# WHERE allocamt IS NOT NULL
# AND hud_id IS NOT NULL
# AND project IS NOT NULL
# AND proj_add IS NOT NULL
# AND proj_st IS NOT NULL 
# AND proj_cty IS NOT NULL
# AND proj_zip IS NOT NULL"
# 
# q1_tb <- bq_project_query(low_income_data_BQproj_id, q1_query)
# q1_dat <- bq_table_download(q1_tb)
# q1_dat <- q1_dat %>% rename(State = proj_st)
# 
# q1_hist <- ggplot(q1_dat, aes(x = State)) + geom_histogram(stat = "count") + ylab('Count') + ggtitle('Counts of Housing Projects with 2017 LIHTC Allocations By State')
# 
# q1_hist
# 
# #Question/Query 3: Exploratory Analysis: Across all properties, which states and zip codes received the highest average low-income housing tax credit funding? 
# q3_query <- "SELECT hud_id, project, proj_add, proj_st, proj_cty, proj_zip, allocamt, FROM `low_income_housing_tax_credit_program.2017_lihtc_database_hud` 
# WHERE allocamt IS NOT NULL
# AND hud_id IS NOT NULL
# AND project IS NOT NULL
# AND proj_add IS NOT NULL
# AND proj_st IS NOT NULL 
# AND proj_cty IS NOT NULL
# AND proj_zip IS NOT NULL"
# 
# q3_tb <- bq_project_query(low_income_data_BQproj_id, q3_query)
# q3_dat <- bq_table_download(q3_tb)
# q3_dat <- q3_dat %>% group_by(proj_st) %>% rename(State = proj_st, Allocation = allocamt)
# 
# #Create box plot to help answer this question: 
# q3_box_plot <- ggplot(q3_dat, aes(x = State, y = Allocation)) + 
#   geom_boxplot() + ylim(0, 2500000)
# 
# q3_box_plot
# 
# #It looks like Maryland and Florida are roughly tied for the highest average allocation amount. 
# 
# geographical_gridplot <- cowplot::plot_grid(q1_hist, q3_box_plot)
# geographical_gridplot
# 
# 
# #Question 5: What were the average allocations provided to LIHTC projects per construction type (see type column) and the distributions of those allocations? Do 
# #these allocation amounts significantly differ from one type to the next? How do the other physical or housing construction variables relate to LIHTC allocation amounts, 
# #and how do these construction variables relate to one another (co-vary, etc.)? 
# 
# ###COULD DO A PIE CHART HERE for percent of total allocations for each housing type. 
# q5_query <- "SELECT hud_id, project, type, allocamt FROM `low_income_housing_tax_credit_program.2017_lihtc_database_hud` 
# WHERE allocamt IS NOT NULL
# AND hud_id IS NOT NULL
# AND project IS NOT NULL
# AND type is NOT NULL" 
# 
# q5_tb <- bq_project_query(low_income_data_BQproj_id, q5_query)
# q5_dat <- bq_table_download(q5_tb)
# q5_dat <- q5_dat %>% rename(Allocation = allocamt)
# q5_dat <- q5_dat %>% mutate(type2 = case_when(type == 1 ~ 'New Construction',
#                                               type == 2 ~ 'Acquisition and Rehab', 
#                                               type == 3 ~ 'Both New Construction and A/R',
#                                               type == 4 ~ 'Existing',
#                                               TRUE ~ '')) #Use key of values for the 'type' column in BigQuery to relabel each value in a new column 
# q5_dat$type2 <- factor(q5_dat$type2, levels = c('Existing', 'Acquisition and Rehab', 'Both New Construction and A/R', 'New Construction'))
# q5_dat <- q5_dat %>% arrange(type2)
# 
# q5_boxplot <- ggplot(aes(x = type2), data = q5_dat) + geom_histogram(stat = 'count')
# q5_boxplot
# 
# #The majority are new construction and acquisition and rehab. How do the allocation amount distributions for each type appear when plotted next to each other (try a density plot)? 
# #Do the allocation amounts vary significantly between construction types? 
# q5_dat <- q5_dat %>% rename('Housing Type' = 'type2')
# q5_density_plot <- ggplot(q5_dat, aes(x = Allocation, colour = `Housing Type`)) + geom_density() + xlim(0, 1000000)
# q5_density_plot
# 
# #A far greater percentage of existing housing projects received lower LIHTC allocations than the other three types of projects.
# 
# #Examining the descriptive statistics for the other physical housing project characteristics: number of bedrooms, number of low-income units, number of total units, etc.: 
# #Place these together in a single grid-type layout: 
# q6_query <- "SELECT hud_id, project, proj_add, proj_st, proj_cty, proj_zip, allocamt, n_0br, n_1br, n_2br, n_3br, n_4br, n_units FROM `low_income_housing_tax_credit_program.2017_lihtc_database_hud` 
# WHERE CONCAT(allocamt, hud_id, project, proj_add, proj_st, proj_cty, proj_zip, n_0br, n_1br, n_2br, n_3br, n_4br, n_units) IS NOT NULL"
# q6_tb <- bq_project_query(low_income_data_BQproj_id, q6_query)
# q6_dat <- bq_table_download(q6_tb)
# 
# #Lay these side-by-side in a grid: 
# #Create histogram of average bedroom counts: 
# q6_dat <- q6_dat %>% mutate(AVG_BR = ((0 * n_0br) + (1 * n_1br) + (2 * n_2br) + (3 * n_3br) + (4 * n_4br)) / n_units)
# q6_bedroomcount_density <- ggplot(aes(x = AVG_BR), data = q6_dat) + geom_density(color = "darkblue", fill = "lightblue") + xlab('Average Number of Bedrooms') + ylab('Fraction of Housing Projects') + xlim(c(0, 4))
# q6_bedroomcount_density
# #Create scatter plot of average bedroom counts vs. LIHTC allocations to look for an indication of a relationship: 
# q6_bedroomcounts_vs_allocations <- ggplot(data = q6_dat, aes(x = AVG_BR, y = allocamt)) + geom_point() + xlab('Average Number of Bedrooms') + ylab('LIHTC Allocation ($)') + xlim(c(0, 4)) + ylim(c(0, 12500000)) +
#   geom_smooth(method = 'lm')
# q6_bedroomcounts_vs_allocations
# 
# q6_plotgrid <- cowplot::plot_grid(q6_bedroomcount_density, q6_bedroomcounts_vs_allocations)
# q6_plotgrid


########Section 2: Statistical Modeling of LIHTC Allocations####################################################

#Finding the top-5 most independent variables for predicting LIHTC allocation amounts using:
#Begin approaching this problem using properties of the covariates matrix for the indpendent features in the dataset: 


