
#####LOAD PACKAGES#####
if (!require(readxl)) install.packages('readxl')
library(readxl)

if (!require(weights)) install.packages('weights')
library(weights)

if (!require(pls)) install.packages('pls')
library(pls)

if (!require(gmodels)) install.packages('gmodels')
library(gmodels)

if (!require(stringi)) install.packages('stringi')
library(stringi)

if (!require(splitstackshape)) install.packages('splitstackshape')
library(splitstackshape)

if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

if (!require(reshape2)) install.packages('reshape2')
library(reshape2)

if (!require(data.table)) install.packages('data.table')
library(data.table)

if (!require(magrittr)) install.packages('magrittr')
library(magrittr)

if (!require(scales)) install.packages('scales')
library(scales)

if (!require(tm)) install.packages('tm')
library(tm)

if (!require(SDMTools)) install.packages('SDMTools')
library(SDMTools)

if (!require(dplyr)) install.packages('dplyr')
library(dplyr)

if (!require(plotly)) install.packages('plotly')
library(plotly)

if (!require(tibble)) install.packages('tibble')
library(tibble)

if (!require(plyr)) install.packages('plyr')
library(plyr)

if (!require(tidyr)) install.packages('tidyr')
library(tidyr)

if (!require(stringr)) install.packages('stringr')
library(stringr)

if (!require(ggrepel)) install.packages('ggrepel')
library(ggrepel)

if (!require(MASS)) install.packages('MASS')
library(MASS)

if (!require(foreign)) install.packages('foreign')
library(foreign)

if (!require(sandwich)) install.packages('sandwich')
library(sandwich)

if (!require(lmtest)) install.packages('lmtest')
library(lmtest)

if (!require(corrplot)) install.packages('corrplot')
library(corrplot)

if (!require(RColorBrewer)) install.packages('RColorBrewer')
library(RColorBrewer)

if (!require(xtable)) install.packages('xtable')
library(xtable)

if (!require(Hmisc)) install.packages('Hmisc')
library(Hmisc)

if (!require(car)) install.packages('car')
library(car)

if (!require(readr)) install.packages('readr')
library(readr)

######################FUNCTIONS######################

############Calculate proportions with or without "dont know"
#data == dataframe
#agg_var == Name of the geographic aggregation unit(e.g., lga)
#indicator_index == Column INDEX of the first column to be aggregated
#dont_denom = IN QUOTATIONS: "exclude" if "dont know" should be excluded OR "included" if "dont know" should included
#dontknow_format == IN QUOTATIONS: How "dont know" is spelled in the data
make_proportions <- function(data,agg_var,indicator_index, dont_denom, dontknow_format){
  var_name <- colnames(data)[indicator_index]
  locationz <- as.vector(unique(data[grep(paste0("^",agg_var,"$"),colnames(data))]))
  #locationz <- sort(locationz, decreasing=TRUE)
  data<- add_column(data, onesz = 1)
  dontknow_format <- paste0("'",dontknow_format,"'")
  if(dont_denom == "exclude"){
    dont_denom <- FALSE
  } else if (dont_denom == "include") {
    dont_denom <- TRUE
  }
  if(dont_denom == FALSE){
    data  <- data %>%
      dplyr ::filter(!is.na(data[grep(paste0("^",var_name,"$"), colnames(data))])) 
    data  <- dplyr:: filter_(data, paste(var_name,"!=", dontknow_format,sep=" "))
  } else {
    data  <- data %>%
      dplyr ::  filter(!is.na(data[grep(paste0("^",var_name,"$"), colnames(data))]))
  }
  agg_var_indicator <- as.formula(paste0(agg_var,"~",var_name))  
  result <- data %>% dcast(agg_var_indicator, fun.aggregate = sum,value.var="onesz", na.rm=TRUE)
  namez <- paste0(names(result)[2:ncol(result)],"_",var_name)
  names(result)[2:ncol(result)] <- namez
  result<- add_column(result, total_respondents = rowSums(result[2:ncol(result)]))
  denom_column <- ncol(result)
  props <- list()
  for(i in 2:(ncol(result)-1)){
    props[[i]]<-  result[i]/result[denom_column]
  }
  props[sapply(props, is.null)] <- NULL
  props <- as.data.frame(props)
  names(props) <- paste0("pr_",names(props))
  result <- data.frame(result, props )
  result<-merge(locationz, result, by=agg_var , all.x=TRUE)
  return(result)
}
#aa<- make_proportions(nonnumeric,lga, 3,TRUE)

############RANK VALUES: VERSION 2.0##################
#df == Dataframe of columns -- NOTE THAT IT MUST BE THE ID COLUMNS AND THE REST ARE THE COLUMNS TO BE RANKED
#aggunit == IN QUOTATIONS: Aggregation unit
#toprank == Top-n ranking (e.g., 5 produces the top 5; -5 produces bottom 5)
rank_money2 <- function(df, aggunit, toprank) {
  callag <- melt(df, id.vars = c(aggunit))
  id_index <- grep(paste("^",aggunit,"$", sep=""),colnames(callag))
  unique_units <- unique(callag[id_index])
  unique_units<-as.data.frame(unique_units)
  if(toprank >= 1){
    direction <- TRUE
  } else(
    direction <- FALSE
  )
  snowflakes <- vector("list")
  toprank <- abs(toprank)
  for (i in 1:nrow(unique_units)){
    snowflakes[[i]] <- subset(callag, get(aggunit) == unique_units[i,])
  }
  snowflakes<-  lapply(snowflakes, function(x) x[!duplicated(x), ])
  sorted_dataframes_list <- lapply(snowflakes, function(df){
    df[order(df$value,decreasing = direction),]
  })
  rankked <- lapply(sorted_dataframes_list,head,n=toprank)
  castedd <- lapply(rankked, function(df){
    units_variable <- as.formula(paste0(as.symbol(aggunit),"~", "factor(",as.symbol("variable"),",levels=unique(",as.symbol("variable"),"))","+",as.symbol("value")))
    dcast(df, units_variable) 
  }) 
  trimcast <- lapply(castedd, function(df){
    sub("_[^_]+$", "", names(df[2:(toprank+1)]))
  })
  for (k in 1: nrow(unique_units)){
    for (j in (toprank+2):(toprank+1+toprank)){
      castedd[[k]][j]<-NA
    }
  }
  for (k in 1: nrow(unique_units)){
    for (j in 1: toprank){
      castedd[[k]][j+toprank+1] <- trimcast[[k]][j] 
    }
  }
  named <-c()  
  for (h in 1:toprank){
    named[h] <- paste0("rank",h,sep="")
  }
  ranknamed <-c() 
  for (l in 1:toprank ){
    ranknamed[l] <- paste0("name",l,sep="")
  }
  titles <- c("geounit", named,ranknamed)
  castedd <- lapply(castedd, setNames, titles)
  locations <- df[grep(paste0("^",aggunit,"$"),colnames(df))]
  locations <- unique(locations)
  ordername <- data.frame(matrix(unlist(castedd), nrow=nrow(unique_units), byrow=T),stringsAsFactors=FALSE)
  colnames(ordername) <- titles
  for (j in 1: toprank+1){
    ordername[j]<-round(as.numeric(unlist(ordername[j])),4)
  }
  ordername$geounit<-locations
  ordername[ordername == 0] <- NA
  names(ordername)[1]<-aggunit
  for(i in 2:(1+toprank)){
    ordername[,i+toprank] <- ifelse(is.na(ordername[,i]),NA,ordername[,i+toprank])
  }
  return(ordername)
}
#aaa <- rank_money2(resulttorank, "lga_group", 3, "highest")



#MOVE COLUMNS--NOT MINE
moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", 
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}

######INDICATOR SUMMARY 2.0: UNWEIGHTED: Use inside a loop over a whole dataset
#dataa == dataset with indicators to aggregate
#agg_varble == IN QUOTATIONS: The name of the aggregation variable in the dataset OR "all" (all geographies together [no aggregation])
#i == Column index inside the dataset
#pop_group_var == IN QUOTATIONS: column defining populaton groups (e.g., "pop_group") OR "all" (all groups together [no aggregation])
indicator_summary3 <- function(dataa,agg_varble, i, pop_group_var){
  names(dataa) <- names(dataa) %>%
    gsub(",", "", .) %>%
    gsub("/", " ", .) %>%
    gsub("\\)", "", .) %>%
    gsub("\\(", "", .) %>%
    gsub("-", "", .) %>%
    gsub(" ", "_", .) %>%
    gsub("\\.", "", .) %>%
    gsub("__", "_", .) %>%
    gsub("___", "_", .) %>%
    gsub("'", "", .)  
  dataa[[i]] <- gsub(' ', '_', dataa[[i]])
  if(agg_varble == "all"){
    dataa$global_agg <- "constant_agg"
    agg_varble <- "global_agg"
  } else{
  }
  if(pop_group_var == "all"){
    dataa$global_group <- "constant_group"
    pop_group_var <- "global_group"
  } else {
  }
  var_name <- colnames(dataa)[grep(paste0("^",colnames(dataa)[i],"$"),colnames(dataa))]
  agg_and_var <- c(agg_varble, var_name)
  agg_varble_index <- grep(paste0("^",agg_varble,"$"), colnames(dataa))
  agg_pop_group <- as.data.frame(str_split_fixed(unique(paste0( unlist(dataa[[grep(paste0("^",agg_varble,"$"), colnames(dataa))]]) ,";", unlist(dataa[[grep(paste0("^",pop_group_var,"$"), colnames(dataa))]]))),";",2))
  if(all(is.na(dataa[,i]))==TRUE){ #IF COLUMN IS BLANK
    print("ALL NA")
    aggs <- as.data.frame(agg_pop_group[1])
    pop_group_column <- as.data.frame(agg_pop_group[2])
    indicator <-  as.data.frame(rep(var_name, nrow(unique(agg_pop_group))))
    answer_option <- as.data.frame(rep("NA", length(unique(agg_pop_group))))
    values <-  as.data.frame(rep("NA", length(unique(agg_pop_group))))
    sample_sizee <- as.data.frame(rep("NA", length(unique(agg_pop_group))))
    result <- cbind(aggs, pop_group_column,indicator,answer_option, values,sample_sizee)
    result <- as.data.frame(result)
    names(result) <-  c("geo_level","pop_group","indicator","answer_option","value","sample_size")
    ###IF NUMERIC
  } else if (is.double(dataa[[i]])){
    print("NUMERIC")
    if(pop_group_var == "all"){
      print("ALL TOGETHER")
      dataa$pop_group <- "all"
    } else {
      print("SPECIFIC GROUP")
      dataa$pop_group <- dataa[[grep(paste0("^",pop_group_var,"$"),colnames(dataa))]]
    } 
    result <- dataa %>% mutate(onesz = 1) %>%
      mutate(weighted_num_var = get(var_name)*onesz) %>%
      dplyr:: select(one_of(c(agg_varble,"pop_group","weighted_num_var","onesz"))) %>%
      dplyr:: rename(!!var_name :=  weighted_num_var) %>%
      group_by_at(.vars=  vars(one_of(c(agg_varble,"pop_group")))) %>%
      dplyr::summarise_all(funs(sum)) %>%
      mutate(weighted_num_avg = get(var_name)/onesz) %>%
      dplyr:: select(-c(var_name)) %>%
      dplyr:: rename(!!var_name:=weighted_num_avg) %>%
      dplyr:: rename(sample_size =onesz)
    indicator <- as.data.frame(rep(var_name, nrow(result)))
    answer_option <- as.data.frame(rep("NA", nrow(result)))
    aggs <- result[[1]]
    pop_group_col <- result[2]
    sample_size <- result[3]
    value <- result[4]
    result <- data.frame(aggs,pop_group_col,indicator,answer_option, value, sample_size)
    colnames(result) <- c("geo_level","pop_group","indicator","answer_option","value","sample_size")
    result <- na.omit(result)
    ###IF CATEGORICAL
  } else if(is.character(dataa[[i]])|is.logical(dataa[[i]])){   #OR IS 0/1
    print("PERCENTS")  
    colnames(dataa)[grep("uuid",colnames(dataa))] <- "uuid"   #DEFINE "uuid" COLUMN
    categorical_formula <- as.formula(paste0(agg_varble,"~",var_name)) 
    #MAKE BINARY
    binary <- dataa %>% mutate(onesz = 1) %>%
      dplyr::select(one_of(c("uuid","onesz",var_name,agg_varble,pop_group_var))) %>%
      group_by_at(.vars=  vars(one_of(c("uuid", "onesz",agg_varble,pop_group_var)))) %>%
      dplyr:: rename(weight_var = onesz) %>%
      drop_na(eval(var_name)) %>%
      mutate(uuid_weight = paste0(uuid,";",get(agg_varble) ,";",get(pop_group_var),";",weight_var)) %>% 
      dcast(eval(paste0("uuid_weight","~", var_name)), fun.aggregate = sum, value.var= "weight_var", na.rm=TRUE) %>%
      separate(uuid_weight,c("uuid",agg_varble,pop_group_var,"weight_var"), ";") 
    #CHANGE COLNAMES
    names(binary) <- names(binary) %>%
      gsub(",", "", .) %>%
      gsub("/", " ", .) %>%
      gsub("\\)", "", .) %>%
      gsub("\\(", "", .) %>%
      gsub(" ", "_", .) %>%
      gsub("\\.", "", .) %>%
      gsub("-", "", .) %>%
      gsub("___", "_", .) %>%
      gsub("'", "", .)
    first_bianized <- grep(paste0("^","weight_var","$"),colnames(binary)) +1
    weight_col <- grep(paste0("^","weight_var","$"),colnames(binary))
    uuid_geo_keep <- binary[1:3]
    list_binary <- list()
    for(p in first_bianized:ncol(binary)){
      list_binary[[p]] <- as.data.frame(as.double(binary[,p])*as.double(binary[,weight_col]))
    }
    list_binary[sapply(list_binary, is.null)] <- NULL
    bianized <- as.data.frame(unlist(list_binary,recursive = FALSE))   
    names(bianized)<- names(binary[first_bianized:ncol(binary)])
    bianized[is.na(bianized)] <- 0
    bianized <- data.frame(uuid_geo_keep,bianized)
    first_bianized <- grep(paste0("^",pop_group_var,"$")  ,colnames(bianized)) +1
    bianized[ncol(bianized)+1] <- rowSums(bianized[first_bianized:ncol(bianized)])
    colnames(bianized)[ncol(bianized)] <- "tlt_resp"
    #AGGREGATE--STEP 1
    first_var_agg <- grep(paste0("^",pop_group_var,"$"),colnames(bianized))+1
    last_var_agg <-  grep("^tlt_resp$",colnames(bianized))-1
    step1_agg <- list()
    for(h in first_var_agg:last_var_agg){
      var_name_agg <- colnames(bianized)[h]
      step1_agg_formula <- as.formula(paste0(agg_varble,"+",pop_group_var,"~","constant")) 
      step1_agg[[h]] <- bianized %>%  group_by(!!agg_varble) %>%
        dplyr::filter(!is.na(eval(var_name_agg))) %>%
        mutate(constant = "constant") %>%
        dcast(step1_agg_formula, fun.aggregate = sum, value.var= var_name_agg, na.rm=TRUE) %>%
        dplyr:: rename(!!var_name_agg := constant)
    }   
    step1_agg[sapply(step1_agg, is.null)] <- NULL
    step1_agg <- data.frame(unlist(step1_agg,recursive = FALSE))
    step1_agg <- step1_agg[,!grepl(paste0(agg_varble,"."), colnames(step1_agg))]
    step1_agg <- step1_agg[,!grepl(paste0(pop_group_var,"."), colnames(step1_agg))]
    names(step1_agg) <- names(step1_agg) %>%
      gsub("\\.", "", .)
    #TOTAL RESPONSE
    step1_agg_formula <- as.formula(paste0(agg_varble,"+",pop_group_var,"~","constant")) 
    total_respondents <- bianized %>%  group_by(!!agg_varble) %>%
      dplyr::filter(!is.na(tlt_resp)) %>%
      mutate(constant = "constant") %>%
      dcast(step1_agg_formula, fun.aggregate = sum, value.var= "tlt_resp", na.rm=TRUE) %>%
      dplyr:: rename(tlt_resp = constant)
    result <- merge(step1_agg,total_respondents, by=c(agg_varble,pop_group_var),all=TRUE)
    #PERCENTS
    total_resp_column <- grep("^tlt_resp$",colnames(result))
    result_pr <- list() 
    for(k in 3:(total_resp_column-1)){
      colnames(result)[k] <- paste0(var_name ,"_",colnames(result)[k])
      pr_result <- as.data.frame(result[k]/result[total_resp_column])
      colnames(pr_result)<- paste0("pr_", colnames(pr_result))
      result_pr[[k]] <- pr_result
    }
    result_pr[sapply(result_pr, is.null)] <- NULL
    percents <- data.frame(result_pr)
    result <- data.frame(result, percents)
    idx <- result[1]
    groupp<- result[2] 
    result <- result %>% dplyr::  select(matches('tlt_resp|pr_')) 
    result <- data.frame(idx,groupp,result)
    result<- melt(result)
    #FINALIZE FORMAT
    tlt_resp <- result %>% dplyr::filter(variable=="tlt_resp")
    tlt_resp[grep("variable",colnames(tlt_resp))] <- NULL
    colnames(tlt_resp)[grep("value",colnames(tlt_resp))] <- "sample_size"
    result <- result %>% dplyr::filter(variable !="tlt_resp")
    result <- merge(result, tlt_resp, by=c(agg_varble,pop_group_var), all.x=TRUE)
    result$variable <- gsub("pr_","",result$variable)
    result$answer_option <- gsub(paste0(colnames(dataa)[i],"_"),"",result$variable)
    result$variable <- colnames(dataa)[i]
    result <- result[,c(1,2,3,6,4,5)]
    colnames(result) <- c("geo_level","pop_group","indicator","answer_option","value","sample_size")
  }
  return(result)
}
#categorical <- indicator_summary3(profile, "lga",67, "group")
#NAs <- indicator_summary3(profile, "lga",204, "group")
#numeric <- indicator_summary3(profile, "lga",13, "group")


###########################LOOP OVER DATASET FUNCTION###########################
#datasett == Dataset that should be aggregted 
#first_var_name  == IN QUOTATIONS: Name of the FIRST indicator to be aggregated (as found in the dataset)
#last_var_name == IN QUOTATIONS: Name of the LAST indicator to be aggregated (as found in the dataset)
#agg_level == IN QUOTATIONS: Name of the aggregation variable (as is it found in the dataset)
#pop_grouping == IN QUOTATIONS: Name of the population group variable (as found in the dataset)
loop_over_dataset  <- function(datasett,first_var_name,last_var_name, agg_level,pop_grouping){
  first_indicator <- grep(paste0("^",first_var_name,"$"),colnames(datasett))
  last_indicator <- grep(paste0("^",last_var_name,"$"),colnames(datasett))
  dataset_sums <- list()
  for(i in first_indicator:last_indicator){
    if(max(nchar(datasett[[i]]),na.rm=TRUE)>100){
      print("TEXT TOO LONG")
      next
    } else if(max(nchar(datasett[[i]]),na.rm=TRUE)<100) {
      print("TEXT OK")
      dataset_sums[[i]] <- indicator_summary3(datasett, agg_level,i, pop_grouping)
    }
  }
  dataset_sums[sapply(dataset_sums, is.null)] <- NULL
  dataset_sums <- do.call("rbind", dataset_sums)
  #REMOVE UNEANTED ANSWER OPTIONS
  dataset_sums <- dataset_sums %>% 
    dplyr::filter(answer_option !="NA") %>%
    dplyr::filter(answer_option !="X0") %>%
    dplyr::filter(answer_option !="No") %>%
    dplyr::filter(answer_option !="Dont_know") %>%
    dplyr::filter(answer_option !="Do_not_know") %>%
    dplyr::filter(answer_option !="I_dont_know_or_I_dont_want_to_answer") %>%
    mutate(freq = round(as.numeric(value)*as.numeric(sample_size),0) ) 
  dataset_sums <-  dataset_sums[moveme(names(dataset_sums), "sample_size last" )]
  dataset_sums <- as.data.frame(dataset_sums)
  #REMOVE COMBINED LARGE NAMES--FROM SELECT MULTIPLE
  indicators <- as.character(unique(dataset_sums$indicator))
  #FIRST LOOP TO FILTER OUT COMPOUND NAMES FROM SELECT MULTIPLE
  indix_to_use_keep <- list()
  for(k in 1: length(indicators)){
    indix_to_use <- min(grep(indicators[k],indicators))
    indix_to_use_name <- indicators[min(grep(indicators[k],indicators))]
    if( length(grep(indix_to_use_name,indicators))>1 ){
      print(as.character(indix_to_use_name))
      indix_to_use_keep[[k]] <- as.character(indix_to_use_name)
    }
  }
  indix_to_use_keep[sapply(indix_to_use_keep, is.null)] <- NULL
  indix_to_use_keep  <- do.call(rbind.data.frame, indix_to_use_keep)
  indix_to_use_keep <- data.frame(lapply(indix_to_use_keep,as.character),stringsAsFactors = FALSE)
  colnames(indix_to_use_keep)[1] <- "to_take"
  #KEEP INDICATORS WITH OK NAMES
  for(h in 1: nrow(indix_to_use_keep)){
    dataset_sums <- dataset_sums  %>% dplyr::filter(indicator != indix_to_use_keep[h,1])
  }
  #ENSURE "X1" to "yes" is correct
  answers_x1 <- dataset_sums %>% dplyr:: filter(answer_option == "X1" & indicator != "in_total_how_many_locations_have_you_lived_in_since_leaving_your_area_of_origin")
  x1_indicators <- unique(answers_x1$indicator)
  indics_go <- list()
  change_to_yes <- list()
  for(f in 1: length(x1_indicators)){
    subset_x1 <- answers_x1 %>% dplyr:: filter(indicator == x1_indicators[f])
    if(max(nchar(subset_x1$indicator))>50 ){
      indics_go[[f]] <- x1_indicators[f]
    } else{
      change_to_yes[[f]]<-x1_indicators[f]
    }
  }
  indics_go[sapply(indics_go, is.null)] <- NULL
  indics_go <- as.character(indics_go)
  change_to_yes[sapply(change_to_yes, is.null)] <- NULL
  change_to_yes <- as.character(change_to_yes)
  #HARD-CODED EXCEPTION :(
  change_to_yes <- change_to_yes[change_to_yes != "difference_arrived_today_days"]
  #CHANGE "X1" indicators to "yes"
  data_change <- dataset_sums %>% dplyr::filter(indicator %in% indics_go) %>%
    mutate(answer_option = "yes")
  data_no_change <- dataset_sums %>% dplyr::filter(!indicator %in% indics_go) 
  dataset_sums <- rbind(data_change,data_no_change)
  #START MANIPULATION: UNIQUE SUBSTRINGS
  apple<- dataset_sums %>% dplyr:: filter(answer_option=="yes") %>%
    arrange(indicator)
  apple$prefix_indix <-  substr(apple$indicator,1,50)
  apple$group <- NA
  apple[1,grep("^group$",colnames(apple))] <- 1
  apple[,grep("^group$",colnames(apple))]  <- as.numeric( apple[,grep("^group$",colnames(apple))] )
  #CREATE GROUPS
  for(i in 2:nrow(apple)){
    apple[i,grep("^group$",colnames(apple))] <- ifelse( apple[i,grep("prefix_indix",colnames(apple))] == apple[i-1,grep("prefix_indix",colnames(apple))],apple[i-1,grep("^group$",colnames(apple))], (apple[i-1,grep("^group$",colnames(apple))]+1) )
  }
  #COMMON SUBSTRING
  min_group <- min(apple$group)
  max_group <- max(apple$group)
  initial_grabs <- list()
  for(m in min_group:max_group){
    words_trim <- apple %>% dplyr:: filter(group==m)
    words <- words_trim$indicator
    words.split <- strsplit(words, '')
    words.split <- lapply(words.split, `length<-`, max(nchar(words)))
    words.mat <- do.call(rbind, words.split)
    common.substr.length <- which.max(apply(words.mat, 2, function(col) !length(unique(col)) == 1)) - 1
    common_string <- substr(words[1], 1, common.substr.length)
    initial_grabs[[m]] <- substr(common_string, 1,nchar(common_string)-1)
  }
  change_yes <- unique(as.character(initial_grabs))
  #match_to_X1 <- which("" == change_yes)[[1]] 
  change_yes <- change_yes[change_yes != ""]
  change_yes <- data.frame(change_yes)
  colnames(change_yes) <- "to_take"
  indix_to_use_keep <- rbind(indix_to_use_keep,change_yes)
  #REVERT MISCATEGORIZED--IF NECESSARY
  # revert_to_X1 <- apple %>% dplyr:: filter(group==match_to_X1)
  #revert_to_X1 <- unique(revert_to_X1$indicator)
  #to_revert <- dataset_sums %>% dplyr::filter(indicator==revert_to_X1) %>%
  # mutate(answer_option= "X1")
  #rest <- dataset_sums %>% dplyr::filter(indicator!=revert_to_X1) 
  #dataset_sums <- rbind(rest,to_revert)
  #FIX LONG NAMES
  indicators <- as.character(unique(dataset_sums$indicator))
  indix_to_use_change <- list()
  for(j in 1: nrow(indix_to_use_keep)){
    all_indicies <- grep(indix_to_use_keep[j,],indicators)
    print(length(all_indicies))
    max_indix <- max(grep(indix_to_use_keep[j,],indicators))
    indix_to_use_name <- indix_to_use_keep[j,]
    remove_indix_names <- indicators[all_indicies]
    if(length(all_indicies)>1){
      short_names <- gsub(paste0(indix_to_use_name,"_"),"",remove_indix_names)
      to_change <- as.data.frame(cbind(remove_indix_names,indix_to_use_name,short_names),stringsAsFactors = FALSE)
      colnames(to_change) <- c("indicator","new_indicator","new_answer_option")
      changed_data  <- merge(dataset_sums,to_change,by ="indicator" ,all.x = TRUE)
      changed_data <- data.frame(lapply(changed_data,as.character),stringsAsFactors = FALSE)
      changed_data[[grep("^indicator$",colnames(changed_data))]] <- ifelse(is.na(changed_data[[grep("^new_indicator$",colnames(changed_data))]]),changed_data[[grep("^indicator$",colnames(changed_data))]],changed_data[[grep("^new_indicator$",colnames(changed_data))]])
      changed_data[[grep("^answer_option$",colnames(changed_data))]] <- ifelse(is.na(changed_data[[grep("^new_answer_option$",colnames(changed_data))]]),changed_data[[grep("^answer_option$",colnames(changed_data))]],changed_data[[grep("^new_answer_option$",colnames(changed_data))]])
      changed_data <- changed_data[1:7]
      changed_data <- changed_data[colnames(dataset_sums)]
      dataset_sums <- changed_data
    } 
  }
  #CHANGE X1 to yes on y/n questions
  to_X1 <- dataset_sums %>% dplyr::filter(indicator %in% change_to_yes) %>%
    mutate(answer_option="yes")
  rest_no_to_X1 <- dataset_sums %>% dplyr::filter(!indicator %in% change_to_yes)
  dataset_sums <- as.data.frame(rbind(rest_no_to_X1,to_X1))
  dataset_sums <- as.data.frame(dataset_sums)
   return(dataset_sums)
}


############################SUMMARIZE DEMOGRAPHICS: WIDE FORMAT########################
#SUBSET DEMOGRAPHIC QUESTIONS
#datasett  ==  DATASET TO AGGREGATE (DEMOGRAPHICS) (e.g., profile)
#group == IN QUOTATIONS: POPULATION GROUP/OTHER GROUPING (e.g., "group" ) OR "all" (all groups together [no aggregation])
#agg_level == IN QUOTATIONS: AGGREGATION VARIABLE (e.g., "lga" ) OR "all" (all geographies together [no aggregation])
#first_demo_indicator ==  IN QUOTATIONS: Column name of the FIRST indicator in the demographic section (e.g., "a_demographics_a_hh_members_a_hh_members_maleinfant")
#last_demo_indicator == #IN QUOTATIONS: Column name of the LAST indicator in the demographic section (e.g., "a_demographics_a_total_independent")
#first_pop_group == IN QUOTATIONS: Column name of the FIRST listed population group (e.g., "a_demographics_a_hh_members_a_hh_members_maleinfant)
#last_pop_group ==  IN QUOTATIONS: Column name of the LAST listed population group (e.g., "a_demographics_a_hh_members_a_hh_members_femaleelder")
#independent_people == IN QUOTATIONS: Column name of the number of independent people (e.g., a_demographics_a_total_independent)
#dependent_people == IN QUOTATIONS: Column name of the number of dependent people (e.g., a_demographics_a_total_dependent)
agg_demographics <- function(datasett,group,agg_level,first_demo_indicator,last_demo_indicator,first_pop_group,last_pop_group,independent_people,dependent_people){
  ###START###
  if(agg_level == "all"){
    datasett$global_agg <- "constant_agg"
    agg_level <- "global_agg"
  } else{
  }
  if(group == "all"){
    datasett$global_group <- "constant_group"
    group <- "global_group"
  } else {
  }
  grouping_var <- grep(paste0("^",group,"$"),colnames(datasett))
  aggregation_var <- grep(paste0("^",agg_level,"$"),colnames(datasett))
  first_demo <- grep(paste0("^",first_demo_indicator,"$"),colnames(datasett))
  last_demo <- grep(paste0("^",last_demo_indicator,"$"),colnames(datasett))
  #SAVE ATTRIBUTE FIELDS
  attributes <- datasett[c(aggregation_var,grouping_var)]
  demographics<- sapply( datasett[,first_demo:last_demo], as.numeric )
  demographics<- as.data.frame(demographics)
  demographics[is.na(demographics)] <- 0
  #CALCULATE CORRECT POPULATION SUM COLUMN
  demographics$tlt_people <- rowSums(demographics[grep(paste0("^",first_pop_group,"$"),colnames(demographics)):grep(paste0("^",last_pop_group,"$"),colnames(demographics))])
  demographics <- data.frame(attributes,demographics)
  #SELECT ALL DEMOGRAPHIC GROUPS
  agg_demographic <- demographics %>% 
    dplyr:: select_all() %>%
    group_by_at(c(agg_level,group)) %>%
    summarise_all(funs(sum), na.rm = TRUE)
  #RENAME SUMS
  names(agg_demographic)[grep(paste0("^",first_demo_indicator,"$"),colnames(agg_demographic)):ncol(agg_demographic)] <- paste0("sum_",names(agg_demographic)[grep(paste0("^",first_demo_indicator,"$"),colnames(agg_demographic)):ncol(agg_demographic)])
  #DEPENDENCY RATIO
  if( dependent_people == FALSE | independent_people == FALSE){
  } else {
    agg_demographic <- agg_demographic %>% mutate(dependency_ratio = get(paste0("sum_",dependent_people))/get(paste0("sum_",independent_people)))
  }
  #CREATE DEMOGRAPHIC PROPORTION COMPOSITION
  last_to_be_proportion <- grep(paste0("sum_",last_demo_indicator),colnames(agg_demographic))
  first_demo_tobe_aggregated <- grep(paste0("sum_",first_demo_indicator),colnames(agg_demographic))
  last_demo_tobe_aggregated <- grep(paste0("sum_",last_demo_indicator),colnames(agg_demographic))
  ncol_agg_demo_sums <- ncol(agg_demographic)
  #CALCULATE PROPORTION COLUMNS
  percents <- list()
  for(i in first_demo_tobe_aggregated:last_demo_tobe_aggregated ){
    percents[[i]] <- agg_demographic[i]/agg_demographic[grep("tlt_people",colnames(agg_demographic))]
  }
  percents[sapply(percents, is.null)] <- NULL
  percents <- as.data.frame(unlist(percents,recursive = FALSE))   
  names(percents) <- gsub(x = names(percents), pattern = "sum_", replacement = "")  
  names(percents) <- paste0("pr_",names(percents))
  agg_demographic <- data.frame(agg_demographic,percents)
  #RENAME DEMOGRAPHIC PROPORTION INDICATORS
  ###STACK 
  last_attribute_index <- grep(group, colnames(agg_demographic))
  demo_names <- gsub("pr_", "",names(  agg_demographic[1, c(1:last_attribute_index, min( grep("pr_",colnames(agg_demographic))):ncol(agg_demographic))] ))
  pairz <- list()
  for(j in 1:nrow(agg_demographic)){
    percents <- as.data.frame(agg_demographic[j, c(1:last_attribute_index, min( grep("pr_",colnames(agg_demographic))):ncol(agg_demographic),grep("tlt_people",colnames(agg_demographic)))])
    colnames(percents) <- demo_names
    sums <- as.data.frame(agg_demographic[j, c(1:last_attribute_index, min( grep("sum_",colnames(agg_demographic))):(min( grep("tlt_people",colnames(agg_demographic)))-1),grep("tlt_people",colnames(agg_demographic)))])
    colnames(sums) <- demo_names
    binded <- rbind(sums, percents)
    pairz[[j]] <- binded
  }
  pairz <- ldply(pairz, data.frame)
  measure <- c("counts","proportions")
  pairz_unique_aggregation <- paste0(pairz[,1],pairz[,2])
  measure <- rep(measure, length(unique(pairz_unique_aggregation)))
  pairz <- cbind(pairz, measure)
  pairz <-  pairz[moveme(names(pairz), paste("measure", "before", first_demo_indicator, sep=" ") )]
  colnames(pairz)[ncol(pairz)] <- "tlt_people"
  return(pairz)
}

######PREPARE DATA FOR STATISTICAL TESTS--ASSIGN NUMERIC AND CATEGORICAL CATEGORIZATION######
#dataa == dataset with indicators to aggregate
#agg_varble == IN QUOTATIONS: The name of the aggregation variable in the dataset OR "all" (all geographies together [no aggregation])
#i == Column index inside the dataset
#pop_group_var == IN QUOTATIONS: column defining populaton groups (e.g., "pop_group") OR "all" (all groups together [no aggregation])
prep_stat_tests <- function(dataa,agg_varble, i, pop_group_var){
  id_columns <-dataa[grep("uuid",colnames(dataa))]
  colnames(id_columns)[1] <- "uuid"
  if(agg_varble == "all"){
    dataa$global_agg <- "constant_agg"
    agg_varble <- "global_agg"
  } else{
  }
  print(agg_varble)
  if(pop_group_var == "all"){
    dataa$global_group <- "constant_group"
    pop_group_var <- "global_group"
  } else {
  }
  var_name <- colnames(dataa)[grep(paste0("^",colnames(dataa)[i],"$"),colnames(dataa))]
  agg_and_var <- c(agg_varble, var_name)
  agg_varble_index <- grep(paste0("^",agg_varble,"$"), colnames(dataa))
  agg_pop_group <- as.data.frame(str_split_fixed(unique(paste0( unlist(dataa[[grep(paste0("^",agg_varble,"$"), colnames(dataa))]]) ,";", unlist(dataa[[grep(paste0("^",pop_group_var,"$"), colnames(dataa))]]))),";",2))
  if(all(is.na(dataa[[i]]))==TRUE){ #IF COLUMN IS BLANK
    print("ALL NA")
    uuid_na <- dataa[grep("uuid", colnames(dataa))]
    note_na <- as.data.frame(rep(NA, nrow(as.data.frame(dataa)) ))
    colnames(note_na) <- paste0( colnames(dataa[i]) , ";","NA" )
    bianized <- data.frame(uuid_na,note_na)
    type <- rep("NA",nrow(bianized))
    bianized <- data.frame(bianized,type)
    ###IF NUMERIC
  } else if ( (is.double(dataa[[i]]) && !grepl(c("have_you|what|who|which|was_the"),as.character(colnames(dataa[i])))) |  (max(unique(as.numeric(dataa[[i]])),na.rm = TRUE)>=2 && max(nchar(dataa[[9]]))<10) ){
    print("NUMERIC")
    uuid <- dataa[grep("uuid",colnames(dataa))]
    agg <- dataa[grep(agg_varble,colnames(dataa))]
    group <- dataa[grep(paste0("^",pop_group_var,"$"),colnames(dataa))]
    data_get <- as.data.frame(dataa[i])
    bianized <- data.frame(uuid,agg,group,data_get)
    colnames(bianized)[ncol(bianized)] <- paste0(colnames(dataa[i]),";",colnames(bianized)[ncol(bianized)])
    type <- rep("numeric",nrow(bianized))
    bianized <- data.frame(bianized,type)
    #EXTRA NUMERIC
    if(pop_group_var == "all"){
      print("ALL TOGETHER")
      dataa$pop_group <- "all"
    } else {
      print("SPECIFIC GROUP")
      dataa$pop_group <- dataa[[grep(paste0("^",pop_group_var,"$"),colnames(dataa))]]
    } 
    result <- dataa %>% mutate(onesz = 1) %>%
      mutate(weighted_num_var = as.numeric(get(var_name))*onesz) %>%
      dplyr:: select(one_of(c(agg_varble,"pop_group","weighted_num_var","onesz"))) %>%
      dplyr:: rename(!!var_name :=  weighted_num_var) %>%
      group_by_at(.vars=  vars(one_of(c(agg_varble,"pop_group")))) %>%
      dplyr::summarise_all(funs(sum)) %>%
      mutate(weighted_num_avg = get(var_name)/onesz) %>%
      dplyr:: select(-c(var_name)) %>%
      dplyr:: rename(!!var_name:=weighted_num_avg) %>%
      dplyr:: rename(sample_size =onesz)
    indicator <- as.data.frame(rep(var_name, nrow(result)))
    answer_option <- as.data.frame(rep("NA", nrow(result)))
    aggs <- result[[1]]
    pop_group_col <- result[2]
    sample_size <- result[3]
    value <- result[4]
    result <- data.frame(aggs,pop_group_col,indicator,answer_option, value, sample_size)
    colnames(result) <- c("geo_level","pop_group","indicator","answer_option","value","sample_size")
    result <- na.omit(result)
    ###IF CATEGORICAL
  } else if(is.character(dataa[[i]])|(is.double(dataa[[i]]) && grepl(c("have_you|what|who|which|was_the"),as.character(colnames(dataa[i]))))|is.logical(dataa[[i]])){   #OR IS 0/1
    print("PERCENTS")  
    colnames(dataa)[grep("uuid",colnames(dataa))] <- "uuid"   #DEFINE "uuid" COLUMN
    categorical_formula <- as.formula(paste0(agg_varble,"~",var_name)) 
    #MAKE BINARY
    binary <- dataa %>% mutate(onesz = 1) %>%
      dplyr::select(one_of(c("uuid","onesz",var_name,agg_varble,pop_group_var))) %>%
      group_by_at(.vars=  vars(one_of(c("uuid", "onesz",agg_varble,pop_group_var)))) %>%
      dplyr:: rename(weight_var = onesz) %>%
      drop_na(eval(var_name)) %>%
      mutate(uuid_weight = paste0(uuid,";",get(agg_varble) ,";",get(pop_group_var),";",weight_var)) %>% 
      dcast(eval(paste0("uuid_weight","~", var_name)), fun.aggregate = sum, value.var= "weight_var", na.rm=TRUE) %>%
      separate(uuid_weight,c("uuid",agg_varble,pop_group_var,"weight_var"), ";") 
    ##PROCESS BINARY
    first_bianized <- grep(paste0("^","weight_var","$"),colnames(binary)) +1
    weight_col <- grep(paste0("^","weight_var","$"),colnames(binary))
    uuid_geo_keep <- binary[1:3]
    list_binary <- list()
    for(p in first_bianized:ncol(binary)){
      list_binary[[p]] <- as.data.frame(as.double(binary[,p])*as.double(binary[,weight_col]))
    }
    list_binary[sapply(list_binary, is.null)] <- NULL
    bianized <- as.data.frame(unlist(list_binary,recursive = FALSE))   
    names(bianized)<- names(binary[first_bianized:ncol(binary)])
    bianized[is.na(bianized)] <- 0
    bianized <- data.frame(uuid_geo_keep,bianized)
    first_bianized <- grep(paste0("^",pop_group_var,"$")  ,colnames(bianized)) +1
    bianized[ncol(bianized)+1] <- rowSums(bianized[first_bianized:ncol(bianized)])
    colnames(bianized)[ncol(bianized)] <- "tlt_resp"
    bianized[ncol(bianized)] <- NULL
    bianized <- merge(id_columns,bianized, by="uuid", all.x=TRUE)
    for(k in (grep(paste0("^",pop_group_var,"$"),colnames(bianized))+1) : ncol(bianized)) {
      colnames(bianized)[k] <- paste0(colnames(dataa[i]),".",colnames(bianized)[k])
    }
    type <- rep("categorical",nrow(bianized))
    bianized <- data.frame(bianized,type)
  }
  return(bianized)
}
#apple <- prep_stat_tests(data_file, geo_aggregation_indicator,61, grouping_indicator)

#grep("females_5_12_years",colnames(data_file))
#max(nchar(dataa[[i]]))
#i=61
#grepl(c("have_you|what|who|which|was_the"),as.character(colnames(data_file[i])))
#as.character(colnames(data_file[61])) %in% c("have_you", "what", "who", "which", "was_the")
#typeof(data_file$females_5_12_years [,61])

############FUNCTION: REMOVE SPECIAL CHARACTERS FROM COLNAMES############
#datasett == Name of the dataframe that needs the headers "cleaned"
remove_special_char_colnames <- function(datasett){
  names(datasett) <- names(datasett) %<>%
    gsub("/", "_", .) %>%
    gsub("-", "_", .) %>%
    gsub("/", "_", .) %>%
    gsub("\\)", "", .) %>%
    gsub("\\(", "", .) %>%
    gsub(",", "", .) %>%
    gsub("'", "", .) %>%
    gsub(":", "_", .) %>%
    gsub("’", "", .) %>%
    gsub("\\?", "", .) %>%
    gsub(" ", "_", .) %>%
    gsub("\\..", "_", .) %>% 
    gsub("\\.", "_", .) %>% 
    gsub("_–_", "_", .) %>%
    gsub("=", "_", .) %>%
    gsub("<", "_", .) %>%
    gsub(">", "_", .) %>%
    gsub("\\$", "", .) %>%
    gsub("\\{", "_", .) %>%
    gsub("\\}", "_", .) %>%
    gsub("pr_", "", .) %>%
    gsub("__", "_", .) %>%
    gsub("___", "_", .) %>%
    gsub("__", "_", .) 
  names(datasett) <-tolower(names(datasett))
  data_clean_names<-as.data.frame(datasett)
  return(data_clean_names)
}



###################################CHI2 TEST##########################################
#NOTE: SIMULATE CHI2 STATISTIC (10,000 iterations) AND IDENTIFY P-VALUE FROM TABLE BASED ON 1-DoF
###################################################################################
#NEED FOR CHI2:
#dataa = Dataframe inside a list that was produced using the "prep_stat_test" function
#grouping_var == IN QUOTATIONS: Categorical indicator by which the data will be tested (i.e., "displaced/non-displaced")
###################################################################################
#REMOVE COLUMNS THAT ARE COMPRISED OF ALL NAs#REMOVE IF COLUMNS ARE ALL 1 AND BLANK ROWS
chi2_go <- function(dataa,grouping_var) {
  forchi2 <- dataa %>% dplyr::filter(!is.na(UQ(as.name(grouping_var)))) %>%
    dplyr::select_if(~ length(unique(.)) > 1)  
  #FIRST VARIABLE TO CONDUCT CHI2
  first_chi2_var_index <- (grep(grouping_var,colnames(dataa)))+1
  last_chi2_var_index <- ncol(dataa)-1
  #GROUPING VARIABLE
  chi2_grouping_var <- grep(paste0("^",grouping_var,"$"), colnames(dataa))
  #CHI2 LOOP
  result_summary <- list()
  for(i in first_chi2_var_index: last_chi2_var_index){
    #REMOVE NAs FROM ANALYZED COLUMN
    forchi2_no_NA <- dataa %>% dplyr:: filter(!is.na(UQ(as.name(colnames(dataa)[i]))))
    forchi2_no_NA$constant <-1
    forchi2_no_NA[i] <- as.numeric(unlist(forchi2_no_NA[i]))
    #REMOVE IF ALL VALUES ARE 1 OR 0
    #ENSURE BOTH GROUPS ARE PRESENT (nrows--should be more than 1 rows [1 EACH GROUP])
    formula_chi2 <- as.formula(paste0(grouping_var, "~","constant" ))
    outed2 <- forchi2_no_NA %>% dcast(formula_chi2, fun.aggregate = sum,value.var=colnames(forchi2_no_NA)[i], na.rm=TRUE)
    all_true <- lapply(forchi2_no_NA[i], function(x) all(x == 1))
    all_false <- lapply(forchi2_no_NA[i], function(x) all(x == 0))
    if(nrow(outed2)>1 &  all_true[[1]]==FALSE & all_false[[1]] ==FALSE ){
      forchi2_no_NA$constant <- NULL
      question_name <- colnames(forchi2_no_NA)[i]
      chi2_result <- chisq.test( as.vector(unlist(forchi2_no_NA[chi2_grouping_var])),as.vector(unlist(forchi2_no_NA[i])), simulate.p.value = TRUE, B=10000)
      group1_name <- rownames(chi2_result$observed)[1]
      group2_name <- rownames(chi2_result$observed)[2] 
      chi2_stat <- chi2_result$statistic
      calc_pval <- pchisq(chi2_stat, df=1, lower.tail=FALSE)
      #ADD FLEXIBLE QUESTION
      forchi2_no_NA <- as.vector(forchi2_no_NA)
      group1_mean <- forchi2_no_NA %>% dplyr:: filter(UQ(as.name(grouping_var)) == group1_name) 
      group1_mean <-ifelse(is.na( mean(group1_mean[[i]],na.rm = TRUE)),0,  mean(group1_mean[[i]],na.rm = TRUE) )
      group2_mean <- forchi2_no_NA %>% dplyr:: filter(UQ(as.name(grouping_var)) == group2_name) 
      group2_mean <- ifelse(is.na(mean(group2_mean[[i]],na.rm = TRUE)),0,mean(group2_mean[[i]],na.rm = TRUE))
      group1_sd <- forchi2_no_NA %>% dplyr:: filter(UQ(as.name(grouping_var)) == group1_name) 
      group1_sd <- ifelse(is.na(sd(group1_sd[[i]],na.rm = TRUE)), 0, sd(group1_sd[[i]],na.rm = TRUE))
      group2_sd <- forchi2_no_NA %>% dplyr:: filter(UQ(as.name(grouping_var)) == group2_name) 
      group2_sd <- ifelse(is.na(sd(group2_sd[[i]],na.rm = TRUE)), 0, sd(group2_sd[[i]],na.rm = TRUE))
      group1_no <- chi2_result$observed[[1]]
      group2_no <- chi2_result$observed[[2]]
      group1_yes <- chi2_result$observed[[3]]
      group2_yes <- chi2_result$observed[[4]]
      sample_size_group1 <- group1_no+group1_yes 
      sample_size_group2 <- group2_no + group2_yes
    } else {
      print("PROBLEM")
      question_name <- NA
      group1_name <- NA
      group2_name <- NA
      sample_size_group1 <- NA
      sample_size_group2 <- NA
      group1_no <- NA
      group2_no <- NA
      group1_yes <- NA
      group2_yes <- NA
      group1_mean <- NA
      group2_mean <- NA
      group1_sd <- NA
      group2_sd <- NA
      chi2_stat <- NA
      calc_pval <- NA
    }
    result_summary[[i]] <- data.frame(question_name, group1_name,group2_name,sample_size_group1,sample_size_group2,group1_no,group2_no,group1_yes,group2_yes,group1_mean,group2_mean,group1_sd,group2_sd, chi2_stat,calc_pval)
  }
  result_summary[sapply(result_summary, is.null)] <- NULL
  chi2_summary  <- do.call(rbind.data.frame, result_summary)
  #CREATE COLUMN OF SIGNIFICANT RESULTS AND IDENTIFY THE GROUP WITH THE LARGER MEAN
  chi2_summary$stat_sig_larger_group <- ifelse(chi2_summary$calc_pval<=.05 & chi2_summary$group1_mean>chi2_summary$group2_mean, as.character(chi2_summary$group1_name),ifelse(chi2_summary$calc_pval<=.05 & chi2_summary$group1_mean<chi2_summary$group2_mean, as.character(chi2_summary$group2_name),""))
  return(chi2_summary)
}


###################################T-TEST##########################################
#NEED FOR TTEST:
#dataa = Dataframe inside a list that was produced using the "prep_stat_test" function
#grouping_var == IN QUOTATIONS: Categorical indicator by which the data will be tested (i.e., "displaced/non-displaced")
############################################################################################
#REMOVE COLUMNS THAT ARE COMPRISED OF ALL NAs#REMOVE IF COLUMNS ARE ALL 1 AND BLANK ROWS
ttest_go <- function(dataa,grouping_var){
  for_ttest <-  dataa %>% dplyr::filter(!is.na(UQ(as.name(grouping_var))))  %>%
    dplyr::select_if(~ length(unique(.)) > 1) 
  #FIRST VARIABLE TO CONDUCT TTEST
  i <- ncol(for_ttest)
  ttest_var_name <- colnames(for_ttest)[i]
  #GROUPING COLUMN INDEX
  ttest_grouping_var <- grep(paste0("^",grouping_var,"$"), colnames(for_ttest))
  ttest_uuid <-  grep("uuid", colnames(for_ttest))
  #ENSURE NO NAs
  for_ttest <- for_ttest %>% dplyr::filter( !is.na(UQ(as.name(ttest_var_name)))) #%>% dcast(for_ttest[,ttest_grouping_var]~ for_ttest[,ttest_uuid] , fun.aggregate = sum,value.var= colnames(for_ttest)[ncol(for_ttest)], na.rm=TRUE)
  ttest_pivot <- for_ttest %>% dplyr:: select(c(colnames(for_ttest)[ttest_grouping_var],colnames(for_ttest)[ncol(for_ttest)])) %>% 
    group_by_(c(colnames(for_ttest)[ttest_grouping_var])) %>%
    summarise_all(funs(sum), na.rm = TRUE)
  test_for_observations_per_group <- for_ttest %>% mutate(onesz=1) %>%
    dcast(as.formula(paste0(grouping_var,"~","onesz")), fun.aggregate=sum,value.var="onesz",na.rm=TRUE)
   #TEST IF GROUPING VAR HAS MORE THAN 1 GROUP
  if( nrow(unique(for_ttest[ttest_grouping_var]))<2 |any(test_for_observations_per_group[[2]]<=1)|ncol(ttest_pivot)<=1){
    print("LESS THAN 2 GROUPING CATEGORIES OR NO POSITIVE NUMBERS IN ONE GROUP")
    question_name <- NA
    group1_sample_size <- NA
    group2_sample_size <- NA
    group1_name <- NA
    group2_name <- NA
    group1_mean <- NA
    group2_mean <- NA
    group1_sd <- NA
    group2_sd <- NA
    ttest_stat <- NA
    ttest_pval <- NA
    lower_confid_interval <- NA
    upper_confid_interval <- NA
    stat_sig_larger_group <-NA
    ttest_summary <- data.frame(question_name,group1_sample_size,group2_sample_size,group1_name,group2_name,group1_mean,group2_mean,group1_sd,group2_sd, ttest_stat,ttest_pval,lower_confid_interval,upper_confid_interval,stat_sig_larger_group)
  } else if ( nrow(unique(for_ttest[ttest_grouping_var]))>=2 ) {
    for_ttest[i] <- as.numeric(unlist(for_ttest[i]))
    question_name <- colnames(for_ttest)[i]
    group1_name <- unique(for_ttest[[ttest_grouping_var]])[1]
    group2_name <- unique(for_ttest[[ttest_grouping_var]])[2]
    #ADD FLEXIBLE QUESTION
    group1_mean <- for_ttest %>% dplyr:: filter(UQ(as.name(grouping_var)) == group1_name) 
    group1_sample_size <- sum(ifelse(is.na( group1_mean[[i]]),0, 1 ))
    group1_mean <-ifelse(is.na( mean(group1_mean[[i]],na.rm = TRUE)),0,  mean(group1_mean[[i]],na.rm = TRUE) )
    group2_mean <- for_ttest %>% dplyr:: filter(UQ(as.name(grouping_var)) == group2_name) 
    group2_sample_size <- sum(ifelse(is.na( group2_mean[[i]]),0, 1 ))
    group2_mean <- ifelse(is.na(mean(group2_mean[[i]],na.rm = TRUE)),0,mean(group2_mean[[i]],na.rm = TRUE))
    group1_sd <- for_ttest %>% dplyr:: filter(UQ(as.name(grouping_var)) == group1_name) 
    group1_sd <- ifelse(is.na(sd(group1_sd[[i]],na.rm = TRUE)), 0, sd(group1_sd[[i]],na.rm = TRUE))
    group2_sd <- for_ttest %>% dplyr:: filter(UQ(as.name(grouping_var)) == group2_name) 
    group2_sd <- ifelse(is.na(sd(group2_sd[[i]],na.rm = TRUE)), 0, sd(group2_sd[[i]],na.rm = TRUE))
    ttest_result <- t.test(for_ttest[,i]~for_ttest[[ttest_grouping_var]])
    ttest_stat <- ttest_result$statistic
    ttest_pval <- ttest_result$p.value
    lower_confid_interval <- ttest_result$conf.int[1]
    upper_confid_interval <- ttest_result$conf.int[2]
    ttest_summary <- data.frame(question_name,group1_sample_size,group2_sample_size,group1_name,group2_name,group1_mean,group2_mean,group1_sd,group2_sd, ttest_stat,ttest_pval,lower_confid_interval,upper_confid_interval)
    #CREATE COLUMN OF SIGNIFICANT RESULTS AND IDENTIFY THE GROUP WITH THE LARGER MEAN
    ttest_summary$stat_sig_larger_group <- ifelse(ttest_summary$ttest_pval<=.05 & ttest_summary$group1_mean>ttest_summary$group2_mean, as.character(ttest_summary$group1_name),ifelse(ttest_summary$ttest_pval<=.05 & ttest_summary$group1_mean<ttest_summary$group2_mean, as.character(ttest_summary$group2_name),""))
  }
  return(ttest_summary)
}

###FUNCTION: CATEGORIZE CATEGORICAL AND NUMERIC
#dataa == dataset with data categorized as "numeric" or "categorical"
#type_var == IN QUOTATIONS: desired data type  ("numeric" or "categorical")
#agg_var == IN QUOTATIONS: Column name of the geographic aggregation indicator
#grouping_var == IN QUOTATIONS: Column name of the grouping indicator
select_var_type <- function(dataa, type_var, agg_var, grouping_var){
  gathered_var_bytype <- list()
  for(h in 1: length(dataa)){
    if( ((all(dataa[[h]][grep("^type$",colnames(dataa[[h]]))]=="categorical"))==TRUE) ){
      if(((all(dataa[[h]][grep("^type$",colnames(dataa[[h]]))]==type_var))==TRUE) &  length(dataa[[h]])>5  & grepl(grouping_var, colnames(dataa[[h]])[4]) ==FALSE &  grepl(agg_var, colnames(dataa[[h]])[4]) ==FALSE  )
        gathered_var_bytype[[h]] <- dataa[[h]]
    } else  if( ((all(dataa[[h]][grep("^type$",colnames(dataa[[h]]))]=="numeric"))==TRUE) ){
      if(((all(dataa[[h]][grep("^type$",colnames(dataa[[h]]))]==type_var))==TRUE) &  length(dataa[[h]])>4  & grepl(grouping_var, colnames(dataa[[h]])[4]) ==FALSE &  grepl(agg_var, colnames(dataa[[h]])[4]) ==FALSE  )
        gathered_var_bytype[[h]] <- dataa[[h]]
    } else{
      next
    }
  }
  gathered_var_bytype[sapply(gathered_var_bytype, is.null)] <- NULL
  return(gathered_var_bytype)
}

###FUNCTION: PREPARE DATA FOR STATISTICAL TESTS
#dataa == KoBo output (main dataset) 
#first_indic_analyzed == IN QUOTATIONS: Column name of the first indicator to be processed 
#last_indic_analyzed == IN QUOTATIONS: Column name of the last indicator to be processed 
#agg_var == IN QUOTATIONS: Column name of the geographic aggregation indicator
#grouping_var == IN QUOTATIONS: Column name of the grouping indicator
indicators_for_stat_tests <- function(dataa, first_indic_analyzed, last_indic_analyzed,agg_var,grouping_var){
  stat_test_preps <- list()
  for( j in grep(first_indic_analyzed,colnames(dataa)): grep(last_indic_analyzed,colnames(dataa)) ){
    stat_test_preps[[j]] <- prep_stat_tests(dataa, agg_var,j, grouping_var)
  }
  stat_test_preps[sapply(stat_test_preps, is.null)] <- NULL
  return(stat_test_preps)
}

###FUNCTION: CHI2 TESTS###
#cate_data == Categorical dataset produced by "select_var_type"
#group_var == IN QUOTATIONS: Column name of the grouping indicator
chi2_tests <- function(cate_data, group_var){
  chi2_results_go <- list()
  for(g in 1:length(cate_data)){
    chi2_results_go[[g]] <- chi2_go(cate_data[[g]] , group_var )
  }
  chi2_results_go[sapply(chi2_results_go, is.null)] <- NULL
  chi2_summary  <- do.call(rbind.data.frame, chi2_results_go)
  chi2_summary <- chi2_summary %>% dplyr::filter(!is.na(question_name))
  #SEPARATE 
  chi2_summary <- chi2_summary %>% separate(question_name,c("question_name","answer_option"), "\\.",extra = "merge") 
  return(chi2_summary)
}

###FUNCTION: T-TEST LOOP
#ttest_data == Numeric dataset produced by "select_var_type"
#group_var == IN QUOTATIONS: Column name of the grouping indicator
ttest_loop <- function(ttest_data,group_var){
  ttest_results_go <- list()
  for(d in 1:length(ttest_data)){
   print(d)
      ttest_results_go[[d]] <- ttest_go(ttest_data[[d]],group_var)
      }
  ttest_summary  <- do.call(rbind.data.frame, ttest_results_go)
  ttest_summary <- ttest_summary %>% dplyr::filter(!is.na(group1_name))
  return(ttest_summary)
}

##############################RANKED DELUX###################################
#indicator_to_rank == IN QUOTATIONS: name of the last portion of the indicator to rank
#rank_type == IN QUOTATIONS: "count" or "percent"
#top_howmany == Top N (3 == top 3; -3 == bottom 3)
#ranklabels == Label of these ranked indicators
#geovary == IN QUOTATIONS: column of aggregate units
rank_delux <- function(dataa,indicator_to_rank,rank_type,top_howmany,ranklabels, geovary){
  ###SUBSET RANKED INDICATORS
  #RANKED: PUSH REASONS
  first_sub <- min(grep(indicator_to_rank ,colnames(dataa))) 
  last_sub <- max(grep(indicator_to_rank ,colnames(dataa))) 
  #AGGREGATION UNIT
  geounit <- grep(paste0("^",geovary,"$"),colnames(dataa))
  to_rank <- dataa[,c(geounit,first_sub:last_sub)]
  to_rank <- dplyr:: select(to_rank,contains(indicator_to_rank))
  geoidd <- dataa[,c(geounit)]
  to_rank <- data.frame(geoidd, to_rank)
  ######PERCENT OR COUNT#########
  type <- rank_type
  if(type == "count"){
    iddd <- to_rank[1]
    to_rank[1] <- NULL
    to_rank <- to_rank[ ,!grepl("pr_", names( to_rank ))]
    to_rank <- to_rank[ ,!grepl("total_respondents", names( to_rank ))]
  } else if(type=="percent"){
    print("PERCENT")
    iddd <- to_rank[1]
    to_rank[1] <- NULL
    to_rank <- to_rank[ ,grepl("pr_", names( to_rank ))]
    to_rank <- to_rank[ ,!grepl("total_respondents", names( to_rank ))]
  }
  to_rank <- data.frame(iddd,to_rank)
  colnames(to_rank)[1] <- geovary
  to_rank[sapply(to_rank, is.na)] <- 0
  #RANK TOP 3 INDICATORS
  to_rank <- rank_money2(to_rank, geovary, top_howmany,ranklabels)
  #LABEL COLUMNS
  names(to_rank)[2:ncol(to_rank)] <- paste0(ranklabels,"_",names(to_rank)[2:ncol(to_rank)])
  colnames(to_rank)[1] <- "group"
  to_rank$group <- as.data.frame(lapply(to_rank$group, unlist))
  to_rank <-  data.frame(t(apply(to_rank, 1, unlist)))
  #REMOVE "pr_" FROM RESULT STRINGS
  if(type=="percent"){
    changed_percent <- list()
    for(j in grep("name1",colnames(to_rank)): ncol(to_rank)){
      to_rank[,j] <- gsub("^.{0,3}", "", to_rank[,j])
    }
  } else {
    to_rank <- to_rank
  }
  return(to_rank)
}
appled <- rank_delux(data_file,"N_info_source_who","percent",2,"N_info_source_who", "LGA")

############RANK VALUES: VERSION 2.0##################
#df == Dataframe of columns -- NOTE THAT IT MUST BE THE ID COLUMNS AND THE REST ARE THE COLUMNS TO BE RANKED
#aggunit == IN QUOTATIONS: Aggregation unit
#toprank == Top-n ranking (e.g., 5 produces the top 5; -5 produces bottom 5)
rank_money2 <- function(df, aggunit, toprank) {
  callag <- melt(df, id.vars = c(aggunit))
  id_index <- grep(paste("^",aggunit,"$", sep=""),colnames(callag))
  unique_units <- unique(callag[id_index])
  unique_units<-as.data.frame(unique_units)
  if(toprank >= 1){
    direction <- TRUE
  } else(
    direction <- FALSE
  )
  snowflakes <- vector("list")
  toprank <- abs(toprank)
  for (i in 1:nrow(unique_units)){
    snowflakes[[i]] <- subset(callag, get(aggunit) == unique_units[i,])
  }
  snowflakes<-  lapply(snowflakes, function(x) x[!duplicated(x), ])
  sorted_dataframes_list <- lapply(snowflakes, function(df){
    df[order(df$value,decreasing = direction),]
  })
  rankked <- lapply(sorted_dataframes_list,head,n=toprank)
  castedd <- lapply(rankked, function(df){
    units_variable <- as.formula(paste0(as.symbol(aggunit),"~", "factor(",as.symbol("variable"),",levels=unique(",as.symbol("variable"),"))","+",as.symbol("value")))
    dcast(df, units_variable) 
  }) 
  trimcast <- lapply(castedd, function(df){
    sub("_[^_]+$", "", names(df[2:(toprank+1)]))
  })
  for (k in 1: nrow(unique_units)){
    for (j in (toprank+2):(toprank+1+toprank)){
      castedd[[k]][j]<-NA
    }
  }
  for (k in 1: nrow(unique_units)){
    for (j in 1: toprank){
      castedd[[k]][j+toprank+1] <- trimcast[[k]][j] 
    }
  }
  named <-c()  
  for (h in 1:toprank){
    named[h] <- paste0("rank",h,sep="")
  }
  ranknamed <-c() 
  for (l in 1:toprank ){
    ranknamed[l] <- paste0("name",l,sep="")
  }
  titles <- c("geounit", named,ranknamed)
  castedd <- lapply(castedd, setNames, titles)
  locations <- df[grep(paste0("^",aggunit,"$"),colnames(df))]
  locations <- unique(locations)
  ordername <- data.frame(matrix(unlist(castedd), nrow=nrow(unique_units), byrow=T),stringsAsFactors=FALSE)
  colnames(ordername) <- titles
  for (j in 1: toprank+1){
    ordername[j]<-round(as.numeric(unlist(ordername[j])),4)
  }
  ordername$geounit<-locations
  ordername[ordername == 0] <- NA
  names(ordername)[1]<-aggunit
  for(i in 2:(1+toprank)){
    ordername[,i+toprank] <- ifelse(is.na(ordername[,i]),NA,ordername[,i+toprank])
  }
  ordername <- as.data.frame(ordername,stringsAsFactors = FALSE)
 ordername[1] <- unlist(ordername[1])
  return(ordername)
}
apple <- rank_money2(after, "aggregations", 3)

#########SMALL LOOP TO UPDATE RANKED NAMES#########
#old_new_names == excel file with "old" and "new" columns
update_ranked_names <- function(ranked_data,old_new_names){ 
  for(i in 1:nrow(old_new_names)){
    ranked_data <-  data.frame(lapply(ranked_data, function(x) gsub(as.character(old_new_names[i,1]), as.character(old_new_names[i,2]), x)), stringsAsFactors=F)
  }
  return(ranked_data)
}


#################################RANK FUNCTION######################################
#OUTPUTS CSVs OF RANKED INDICATORS FROM LIST OF [KOBO] QUESTION NAMES
#dataset == dataframe with specified indicators
#geo_level == IN QUOTATIONS: The column name of the header defining the aggregation level of "dataset"
#percent_count == IN QUOTATIONS: Define as "percent" or "count" -- whether raw counts or %s are used to rank the data
#processed_outputs_folder == IN QUOTATIONS: Name of the output folder where the ranked indicator CSVs should be stored
#indics_to_rank_df == A character vector of the names of indicators to rank
#name_replace == A dataframe with old name-option strings and their "clean" definitions
rank_indicators <- function(dataset, geo_level, percent_count, processed_outputs_folder,indics_to_rank_df,name_replace) {
  #REMOVE "NO/0" FROM THE AGGREGATED DATASET
  dataset <- dplyr:: select(dataset, -contains("_0"))
  dataset <- as.data.frame(dataset)
  #RENAME FIRST COLUMN OF LIST OF INDICATORS TO-BE-RANKED
  colnames(indics_to_rank_df)[1] <- "indic_to_rank"
  #CONVERT TO CHARACTER VECTOR
  indics_to_rank_df <- as.character(indics_to_rank_df$indic_to_rank)
  print(indics_to_rank_df)
  #LOOP: RANK EACH INDICATOR
  for(i in 1: length(indics_to_rank_df)){
    print("indicator")
    print(i)
    #COUNT RESPONSES PER_QUESTION
    indices_per_question <-  grep(indics_to_rank_df[i], colnames(dataset))
    question_subset <- dataset[,indices_per_question]
    question_subset <- question_subset %>% dplyr::select(-contains("pr_"))
    num_question_subset <- length(names(question_subset))
    #DEFINE TOP-X 
    if(num_question_subset == 3 | num_question_subset == 4){
      top_x <- 3
    } else if (num_question_subset >=5){
      top_x <- 5
    } else {
      print("NOT ENOUGH TO RANK")
      next
    }
    #RANK
    ranked <- rank_delux(dataset,indics_to_rank_df[i],percent_count,top_x,indics_to_rank_df[i],geo_level)
    ranked <- update_ranked_names(ranked,name_replace)
    write.csv(ranked,paste0("Analysis/Indicator_Ranking/",processed_outputs_folder,"/",indics_to_rank_df[i],".csv"))
  }
}


#############RANK INDICATORS FROM AGGREGATED DATA#############
#dataa --- Aggregated dataset: The output from "loop_over_dataset.R"
#indics_to_rank --- Dataframe with a list of indicators to rank and the answer options to consider (e.g., rank "X0"["no] or "X1"["yes"])
#indicator_names_translated --- Dataframe with the indicator name translations into a clean format
#indicator_name_translation --- Column vector (e.g., c("97","98") ) containing answer options to remove from the ranking. Note the values search as partial strings within the name of the indicator (e.g., column "income_sources_97" is a TRUE/FALSE response to "what is your source of income--other")
ranked_quant <- function(dataa, indics_to_rank, indicator_names_translated,indicator_name_translation) {
  for(i in 1:nrow(indics_to_rank)){
    print(i)
    remove_answer_options <- indicator_name_translation
    #RANK BEGIN
    qname<- as.character(indics_to_rank[i,1])
    option_to_rank <- as.character(indics_to_rank[i,2])
    #IF REMOVE CHARACTERS IN INDICATOR, THEN OK
    if(remove_answer_options=="no"){
      print("INCLUDE ALL ANSWER OPTIONS--single option")
      remove_indics <- "no"
    } else if(length(remove_answer_options)>0 & option_to_rank !="text"){
      print("REMOVE UNWANTED ANSWER OPTIONS--single option")
      remove_answer_options <- tolower(remove_answer_options)
      remove_answer_options <- paste0("_",remove_answer_options)
      remove_answer_options <- as.list(remove_answer_options)
      remove_answer_options_go <- stri_join_list(remove_answer_options, sep = "", collapse = "|")
      remove_indics <- "yes"
    } else if(length(remove_answer_options)>0 & option_to_rank =="text"){
      print("REMOVE UNWANTED ANSWER OPTIONS--text")
      cap_remove <- gsub("_i_","_I_",remove_answer_options)
      cap_remove <- capitalize(cap_remove)
      remove_answer_options <- as.list(cap_remove)
      remove_answer_options_go <- stri_join_list(remove_answer_options, sep = "", collapse = "|")
      remove_indics <- "yes"
    }
    ####RANK LISTED INDICATORS AND EXPORT####
    if(remove_indics=="no"){
      print("INCLUDE ALL ANSWER OPTIONS")
      one_question_ready <- dataa %>%  dplyr::filter(grepl(qname, indicator)) %>%
        mutate(answer_specific = gsub(paste0(qname,"_"), "", indicator))  %>%
        dplyr::filter(answer_option == option_to_rank) %>%  
        mutate(value =  as.numeric(as.character(value))) %>%
        mutate(aggregations = paste0(geo_level,";",pop_group)) %>%
        dcast(aggregations ~ answer_specific,  fun.aggregate = mean,value.var="value", na.rm=TRUE) 
    } else if(remove_indics == "yes" & option_to_rank != "text"){
      print("REMOVE UNWANTED ANSWER OPTIONS--single option")
      print(remove_answer_options_go)
       one_question_ready <- dataa %>%  dplyr::filter(grepl(qname, indicator)) %>%
        mutate(answer_specific = gsub(paste0(qname,"_"), "", indicator))  %>%
        dplyr::filter(answer_option == option_to_rank) %>% 
        dplyr::filter(!grepl(remove_answer_options_go, indicator)) %>%
         mutate(value =  as.numeric(as.character(value))) %>%
         mutate(aggregations = paste0(geo_level,";",pop_group)) %>%
        dcast(aggregations ~ answer_specific,  fun.aggregate = mean,value.var="value", na.rm=TRUE) 
    }else if(remove_indics == "yes" & option_to_rank == "text"){
      print("REMOVE UNWANTED ANSWER OPTIONS--text")
      one_question_ready <- dataa %>%  dplyr::filter(grepl(qname, indicator)) %>%
        dplyr::filter(!grepl(remove_answer_options_go, answer_option)) %>%
        mutate(value =  as.numeric(as.character(value))) %>%
        mutate(aggregations = paste0(geo_level,";",pop_group)) %>%
        dcast(aggregations ~ answer_option,  fun.aggregate = mean,value.var="value", na.rm=TRUE) 
       }
       if( (ncol(one_question_ready)-1)>=5){
      topx <- 5
    } else if(ncol(one_question_ready)-1 ==3 |4){
      topx <- 3
    } else if(ncol(one_question_ready)-1<3){
      print("NOT ENOUGH COLUMNS TO RANK")
    }
    rankedup <- rank_money2(one_question_ready, "aggregations", topx)
    rankedup <- rankedup %>% separate(aggregations, c("geo_level", "pop_group"),";", extra = "merge")
    rankedup <- update_ranked_names(rankedup,indicator_names_translated)
     write.csv(rankedup, paste0("Ranked_Indicators/",qname,".csv"),row.names = FALSE)
  }
}


#####GET AVERAGE OF VARY####
#Calculate the mean of an indicator produced in long format
#dataa == Initial dataframe/dataset
#indix_name == IN QUOTATIONS: Name of the indicator to be averaged
#isdate == IN QUOTATIONS: "date" or "not_date" indicating the indicator is a date field and must have days calculated
average_of_vary <- function(dataa, indix_name,isdate){
  just_vary_avg <- dataa %>% dplyr::filter(indicator== indix_name)
  just_vary_avg$answer_option <- gsub("X","",just_vary_avg$answer_option)
  if(isdate =="date"){
    just_vary_avg$answer_option <- as.Date(as.character(just_vary_avg$answer_option),format="%Y%m%d")
    just_vary_avg$answer_option <- Sys.Date()-just_vary_avg$answer_option
  }
  just_vary_avg$answer_option <- as.numeric(just_vary_avg$answer_option)
  new_val_sum <- just_vary_avg %>%
    mutate(new_var = paste0(geo_level,";",pop_group)) %>%
    mutate(indic_to_avg = as.numeric(answer_option)*as.numeric(freq)) %>%
    dcast(new_var ~ new_var,  fun.aggregate = sum,value.var="indic_to_avg", na.rm=TRUE) 
  new_val_freq <- just_vary_avg %>%
    mutate(new_var = paste0(geo_level,";",pop_group)) %>%
    mutate(freq = as.numeric(freq)) %>%
    dcast(new_var ~ new_var,  fun.aggregate = sum,value.var="freq", na.rm=TRUE) 
  new_val_avg <- as.numeric(new_val_sum[1,2]/new_val_freq[1,2])
  #ADD NEW VALUE
  keep_avg <- just_vary_avg[1,]
  keep_avg$answer_option <- "mean_days"
  keep_avg$value <- round(new_val_avg,0)
  keep_avg$freq <- new_val_freq[1,2]
  print(keep_avg)
  remove_needed_avg <- dataa %>% dplyr::filter(indicator != indix_name)
  #ADD ADJUSTED (1 row) BACK INTO MAIN DATASET
  resultt <- rbind(keep_avg,remove_needed_avg)
  return(resultt)
}
