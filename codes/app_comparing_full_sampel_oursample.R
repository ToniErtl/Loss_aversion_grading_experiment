
# Comparing Full sample to our sample
rm(list = ls())

library(tidyverse)
library(ggthemes)
library(readxl)
library(RCT)

library(gtsummary)
library(crosstable)


mydata <- readxl::read_xlsx(".//data_anon.xlsx")


full_data <- mydata %>% mutate(final_test==case_when(final_test_orig<=0 & Treatment == "Loss" ~ final_test_orig + 40,
                                                 final_test_orig<=0 & Treatment == "Hybrid" ~ final_test_orig + 40,
                                                 TRUE~final_test_orig)) %>% 
    mutate(test1 =as.numeric(test1),
         test2 =as.numeric(test2),
         test3 =as.numeric(test3),
         test4 =as.numeric(test4),
          final_test = as.numeric(case_when(is.na(final_test)~0,
                                           TRUE~final_test))
         ) %>% 
    mutate(test1 = replace_na(test1,0),
           test2 = replace_na(test2,0),
           test3 = replace_na(test3,0),
           test4 = replace_na(test4,0) 
           ) %>% 
  #filter out those who did not do anything
      filter( (test1+test2+test3+test4+final_test)!=0)


mydata<- mydata %>% mutate(final_test==case_when(final_test_orig<=0 & Treatment == "Loss" ~ final_test_orig + 40,
                                                 final_test_orig<=0 & Treatment == "Hybrid" ~ final_test_orig + 40,
                                                 TRUE~final_test_orig)) %>% 
  mutate(test1 =as.numeric(test1),
         test2 =as.numeric(test2),
         test3 =as.numeric(test3),
         test4 =as.numeric(test4),
         final_test = as.numeric(case_when(is.na(final_test)~0,
                                           TRUE~final_test))
  )

mydata<- mydata %>%  mutate(test1 = replace_na(test1,0),
                            test2 = replace_na(test2,0),
                            test3 = replace_na(test3,0),
                            test4 = replace_na(test4,0) 
) %>% 
  
  #filter out those who did not do anything
  filter( (test1+test2+test3+test4+final_test)!=0 ) %>% 
  rename(
    macro_interest = colnames(mydata)[52],
    macro_grade_goal = colnames(mydata)[53],
    risk_pref = colnames(mydata)[54],
    comp_pref = colnames(mydata)[55],
    time_pref = colnames(mydata)[56],
  ) 

 
# get minimum values for test1,test2,test3,test4:
  
mydata$worst_test <- apply(mydata[,c("test1","test2","test3","test4")], 1, FUN = min)  
full_data$worst_test <- apply(full_data[,c("test1","test2","test3","test4")], 1, FUN = min)     

 # filter out strategic decisionmaking by defining the maximum of the 4 tests
mydata <-   mydata %>%
    mutate(semester_tests = test1+test2+test3+test4-worst_test) %>% 
   dplyr::filter(gender!="Egyéb, nem kívánok válaszolni") %>% 
  mutate(df_type = "Our Sample") %>% 
  mutate(test1 = test1 / 16 * 100,
         test2 = test2 / 16 * 100,
         test3 = test3 / 16 * 100,
         test4 = test4 / 16 * 100,
         semester_tests = semester_tests /48 *100, 
         final_test = final_test / 40 * 100)  
    
    
full_data <- full_data %>% 
  mutate(semester_tests = test1+test2+test3+test4-worst_test) %>% 
  mutate(df_type = "Full Sample") %>% 
  mutate(test1 = test1 / 16 * 100,
         test2 = test2 / 16 * 100,
         test3 = test3 / 16 * 100,
         test4 = test4 / 16 * 100,
         semester_tests = semester_tests / 48 *100, 
         final_test = final_test / 40 * 100) 



comparison <- merge(mydata,full_data, all = T) %>% 
  select(test1,test2,test3,test4,semester_tests, final_test,Treatment,df_type)







# Create a summary table
summary_table <- comparison %>%
  tbl_summary(by = df_type,
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label ~ "Variable") %>%
  modify_spanning_header(all_stat_cols() ~ "Databases") 

# GET tests using crosstable:
my_crosstable <- crosstable(comparison, by = df_type, test = TRUE, funs = c(mean = mean, "std error" = sd))

# Convert crosstable object to a data frame
my_dataframe <- as.data.frame(my_crosstable) %>% 
  select(-.id) %>% 
  rename(Variable = label)
rows<- seq(2, nrow(my_dataframe), by = 2)

test_results <- my_dataframe[rows,c("Variable","test")]
test_results$test <-  stringr::str_sub(test_results$test, start = 10)

# Print the summary table -- TABLE 19
summary_table %>% 
  as.data.frame(row.names = NULL) %>% 
  left_join(.,test_results, by = "Variable") %>% 
  #mutate(Test_results = test_results) %>% 
  stargazer::stargazer(summary = FALSE, rownames = F)



# -------------
# Comparing best 3 tests and final tests in the two smaples




summary_table1 <- mydata %>%
  select(Treatment,semester_tests,final_test) %>% 
  tbl_summary(by = Treatment,
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label ~ "Variable") %>%
  modify_spanning_header(all_stat_cols() ~ "Main Analysis Database") 


tests_1 <- crosstable((mydata %>%
              select(Treatment,semester_tests,final_test)), 
              by = Treatment, test = TRUE, funs = c(mean = mean, "std error" = sd))


tests_1_t <- as.data.frame(tests_1) %>% 
  select(-.id) %>% 
  rename(Variable = label)
rows<- seq(2, nrow(tests_1_t), by = 2)
test_results1 <- tests_1_t[rows,c("Variable","test")]
test_results1$test <-  stringr::str_sub(test_results1$test, start = 10)


summary_table1 <-summary_table1  %>% 
  as.data.frame(row.names = NULL) %>% 
  left_join(.,test_results1, by = "Variable")



summary_table2 <- full_data %>%
  select(Treatment,semester_tests,final_test) %>% 
  tbl_summary(by = Treatment,
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label ~ "Variable") %>%
  modify_spanning_header(all_stat_cols() ~ "Full Data") 

tests_2 <- crosstable((full_data %>%
                         select(Treatment,semester_tests,final_test)),
                      by = Treatment, test = TRUE, funs = c(mean = mean, "std error" = sd))

tests_2_t <- as.data.frame(tests_2) %>% 
  select(-.id) %>% 
  rename(Variable = label)
rows<- seq(2, nrow(tests_2_t), by = 2)
test_results2 <- tests_2_t[rows,c("Variable","test")]
test_results2$test <-  stringr::str_sub(test_results2$test, start = 10)


summary_table2 <-summary_table2  %>% 
  as.data.frame(row.names = NULL) %>% 
  left_join(.,test_results2, by = "Variable")
  
## Two parts of TABLE 20  

stargazer::stargazer(summary_table2, summary = F)
stargazer::stargazer(summary_table1, summary = F)



#### TABLE 21




full_data <- full_data %>% 
  mutate(d_okt = case_when(Teacher_id ==1 ~7,
                                             TRUE~Teacher_id),
         d_szfvar = case_when(Group=="Székesfehérvár"~1,
                              TRUE~0),)


mydata <- mydata %>% 
  mutate(d_okt = case_when(Teacher_id ==1 ~7,
                           TRUE~Teacher_id),
         d_szfvar = case_when(Group=="Székesfehérvár"~1,
                              TRUE~0),)



summary(lm_final_test_all_obs <- lm(final_test~Treatment+semester_tests+factor(d_okt)+d_szfvar, data = full_data))
summary(lm_final_test_w_questionaire <- lm(final_test~Treatment+semester_tests+factor(d_okt)+d_szfvar, data = mydata))


library(sandwich)
library(lmtest)

# Clustered VCOV matrices
vcov_cluster_all  <- vcovCL(lm_final_test_all_obs, cluster = ~Group)
vcov_cluster_quest <- vcovCL(lm_final_test_w_questionaire, cluster = ~Group)

# Extract clustered SEs
cluster_se_all  <- sqrt(diag(vcov_cluster_all))
cluster_se_quest <- sqrt(diag(vcov_cluster_quest))

stargazer::stargazer(lm_final_test_all_obs,
          lm_final_test_w_questionaire,
          se = list(cluster_se_all, cluster_se_quest),
          type = "latex",
          column.labels = c("All Observations", "With Questionnaire"),
          dep.var.labels = "Final Test Score",
          omit.stat = c("f", "ser"),
          notes = "Clustered standard errors in parentheses (clustered at Group level).")










