#SET WORKING DIRECTORY
setwd("~/Desktop/Somalia/Baboou/ReDSS")
#####LOAD DATA#####
excel_file_name <- "ReDSS_data.xlsx"
excel_sheet_name <- "Raw Data"
dataset <- "rdss"

#FIRST AND LAST DEMOGRAPHIC COLUMNS
first_demo_col_name<- "males_0_5_months"
last_demo_col_name <- "unaccompanied_or_separated_girls"
last_pop_demo_col_name <- "females_60_or_older"
dependent_persons <- FALSE
independent_persons <- FALSE

#DEFINE GEOGRAPHIC AGGREGATION CATEGORY: If none; then "all"
geo_aggregation_indicator <- "all"
#DEFINE GROUPING AGGREGATION CATEGORY: If none; then "all"
grouping_indicator <- "has_your_household_always_lived_in_the_settlement_you_are_currently_living_in"
  
#DEFINE FIRST AND LAST INDICATORS TO AGGREGATE
first_var_to_analyze <- "what_is_the_name_of_the_village_settlement_not_the_idp_site_name"
last_var_to_analyze <- "which_of_the_following_languages_would_you_prefer_to_receive_information_in"

##############################################BEGIN AGGREGATION##############################################
options(scipen = 999)
##LOAD DATA
data_file <- read_excel(excel_file_name,sheet = excel_sheet_name)
#REMOVE SPECIAL CHARACTERS & "PLEASE SPECIFY" COLUMNS
data_file <- remove_special_char_colnames(data_file) #REMOVE SPECIAL CHARACTERS FROM COLUMN HEADERS
data_file <- data_file[,!grepl("if_other_please_specify", colnames(data_file))]    #REMOVE "PLEASE SPECIFY" COLUMNS 

#OBTAIN DEMO COLUMN INDICIES
first_demo_col <- grep(paste0("^",first_demo_col_name,"$"),colnames(data_file))
last_demo_col <-  grep(paste0("^",last_demo_col_name,"$"),colnames(data_file))

#REMOVE DEMOGRAPHIC INDICATORS FOR AGGREGATION
data_file_analyze <- data_file[,-c(first_demo_col:last_demo_col)]
###DATA AGGREGATION
aggregated_data <- loop_over_dataset(data_file_analyze,first_var_to_analyze,last_var_to_analyze,geo_aggregation_indicator, grouping_indicator)
write.csv(aggregated_data,paste0(dataset,"_","aggregated.csv"))

###DEMOGRAPHICS###
#CALCULATE DEMOGRAPHIC STATISTICS
agged_demos <- agg_demographics(data_file, grouping_indicator ,geo_aggregation_indicator , 
                                first_demo_col_name , 
                                last_demo_col_name, 
                                first_demo_col_name,
                                last_pop_demo_col_name,
                                dependent_persons ,
                                dependent_persons)
write.csv(agged_demos,paste0(dataset,"_","agged_demos.csv"))

############STATISTICAL TESTS###############
#####PREPARE DATA FOR STATISTICAL TESTS
stat_test_preps <- indicators_for_stat_tests(data_file,first_var_to_analyze,last_var_to_analyze,geo_aggregation_indicator,grouping_indicator)

#SEPARATED CATE & NUMERIC DATASETS
cateez <- select_var_type(stat_test_preps, "categorical", geo_aggregation_indicator,grouping_indicator)
numerik <- select_var_type(stat_test_preps, "numeric",geo_aggregation_indicator,grouping_indicator)

###CHI2 LOOP###
chi2_result <- chi2_tests(cateez,grouping_indicator)
write.csv(chi2_result,"chi2_result.csv")

###T-TEST LOOP###
detach(package:plyr)
ttest_results <- ttest_loop(numerik,grouping_indicator)
write.csv(ttest_results,"ttest_results.csv")

