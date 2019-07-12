#SET WORKING DIRECTORY
setwd("~/Desktop/Somalia/Baboou/ReDSS")
options(scipen = 999)
#####LOAD DATA#####
excel_file_name <- "ReDSS_data.xlsx"
excel_sheet_name <- "Raw Data"
##LOAD DATA
rdss_data <- read_excel(excel_file_name,sheet = excel_sheet_name)
#FAKE GEOGRAPHIC UNITS
vals_to_rep <- c("Aplace","Bplace")
tms_to_rep <- c(59, 180)
  vals_to_rep_expanded = rep(vals_to_rep, length.out = length(tms_to_rep))
  rdss_data$locale_district <-  rep(vals_to_rep_expanded, times = tms_to_rep)
######################BEGIN AGGREGATION######################
#REMOVE SPECIAL CHARACTERS & "PLEASE SPECIFY" COLUMNS
rdss_data <- remove_special_char_colnames(rdss_data) #REMOVE SPECIAL CHARACTERS FROM COLUMN HEADERS
rdss_data <- rdss_data[,!grepl("if_other_please_specify", colnames(rdss_data))]    #REMOVE "PLEASE SPECIFY" COLUMNS 
 
#REMOVE DEMOGRAPHIC INDICATORS FOR "TRADITIONAL" AGGREGATION
 first_demo_col <- grep(paste0("^","males_0_5_months","$"),colnames(rdss_data))
last_demo_col <-  grep(paste0("^","unaccompanied_or_separated_girls","$"),colnames(rdss_data))
  rdss_data_analyze <- rdss_data[,-c(first_demo_col:last_demo_col)]
  
  #DEFINE AGGREGATION CATEGORIES
  geo_aggregation_indicator <- "locale_district"
  grouping_indicator <- "has_your_household_always_lived_in_the_settlement_you_are_currently_living_in"
  
  #DEFINE FIRST AND LAST VARIABLES TO AGGREGATE
  first_var_to_analyze <- "what_is_the_name_of_the_village_settlement_not_the_idp_site_name"
    last_var_to_analyze <- "which_of_the_following_languages_would_you_prefer_to_receive_information_in"

#DATA AGGREGATION
rdss_aggregated <- loop_over_dataset(rdss_data_analyze,first_var_to_analyze,last_var_to_analyze,geo_aggregation_indicator, grouping_indicator)
write.csv(rdss_aggregated,"rdss_aggregated.csv")

###DEMOGRAPHICS###
#REMOVE NON-COUNT DEMOGRAPHIC VARIABLES IN SEQUENCE
rdss_data$please_confirm_the_total_number_of_household_members_is_total_hh_ <- NULL
rdss_data$how_many_of_the_following_people_are_in_the_household_currently <- NULL
#CALCULATE DEMOGRAPHIC STATISTICS
agged_demos <- agg_demographics(rdss_data, grouping_indicator ,geo_aggregation_indicator , 
                                "males_0_5_months" , 
                                "unaccompanied_or_separated_girls", 
                                "males_0_5_months",
                                "females_60_or_older",
                                FALSE ,
                               FALSE)
write.csv(agged_demos,"agged_demos.csv")

############STATISTICAL TESTS###############
#####PREPARE DATA FOR STATISTICAL TESTS
stat_test_preps <- indicators_for_stat_tests(rdss_data,first_var_to_analyze,last_var_to_analyze,geo_aggregation_indicator,grouping_indicator)

#SEPARATED CATE & NUMERIC DATASETS
cateez <- select_var_type(stat_test_preps, "categorical", geo_aggregation_indicator,grouping_indicator)
numerik <- select_var_type(stat_test_preps, "numeric",geo_aggregation_indicator,grouping_indicator)


grep("what_is_the_name_of_the_village_settlement_not_the_idp_site_name",colnames(rdss_data))
#9
colnames(numerik[[1]])[4]
keepped <- list()
for(y in 1:length(numerik)){
keepped[[y]] <- colnames(numerik[[y]])[4]
}
keeped <- do.call(rbind.data.frame, keepped)
write.csv(keeped,"keeped.csv")


###CHI2 LOOP###
chi2_result <- chi2_tests(cateez,grouping_indicator)
write.csv(chi2_result,"chi2_result.csv")

###T-TEST LOOP###
ttest_results <- ttest_loop(numerik,grouping_indicator)
write.csv(ttest_results,"ttest_results.csv")



