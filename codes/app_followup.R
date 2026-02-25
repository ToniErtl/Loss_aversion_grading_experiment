
# save the merged datafile with the questionnaire questions



library(tidyverse)
library(gtsummary)
library(crosstable)


wilcox_only <- function(x, g) {
  keep <- complete.cases(x, g)
  x <- as.numeric(x[keep])
  g <- droplevels(as.factor(g[keep]))
  if (nlevels(g) != 2L) {
    return(list(p.value = NA_real_, method = "Wilcoxon rank-sum (needs 2 groups)"))
  }
  w <- suppressWarnings(stats::wilcox.test(x ~ g, exact = FALSE))
  list(p.value = unname(w$p.value), method = "Wilcoxon rank-sum test")
}

# Display p only (no "p value:" prefix, no method)
display_p_only <- function(test, digits = 4, method = FALSE) {
  if (all(sapply(test, is.null))) return(NA_character_)
  crosstable::plim(test$p.value, digits = digits)
}

my_test_args <- crosstable_test_args(
  test_summarize = wilcox_only,
 # test_display  = display_p_only,
#  show_method   = F
)


#---------------------#



mydata <- readxl::read_xlsx("./_all_data_newnames.xlsx")


mydata<- mydata %>% mutate(final_test==case_when(final_test_orig<=0 & Treatment == "Loss" ~ final_test_orig + 40,
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
  filter( (test1+test2+test3+test4+final_test)!=0 ) %>% 
  rename(Neptun_id = `Neptun kód`,
         macro_interest = colnames(mydata)[52],
         macro_grade_goal = colnames(mydata)[53],
         risk_pref = colnames(mydata)[54],
         comp_pref = colnames(mydata)[55],
         time_pref = colnames(mydata)[56],
  ) 

# get minimum values for test1,test2,test3,test4:

mydata$worst_test <- apply(mydata[,c("test1","test2","test3","test4")], 1, FUN = min)  

# filter out strategic decisionmaking by defining the maximum of the 4 tests
mydata <-   mydata %>%
  mutate(semester_tests = test1+test2+test3+test4-worst_test) %>% 
  dplyr::filter(gender!="Egyéb, nem kívánok válaszolni") 






mydata <- mydata %>% 
  #dplyr::filter(gender!="Egyéb, nem kívánok válaszolni" & final_test!=0) %>% # & !is.na(final_test)) %>% # ha a final test hiányzik, akkor nem akarjuk figyelembe venni, mert akkor komolytalan volt az ember 
  dplyr::mutate(total_score = test1+test2+test3+test4+hw1+hw2+hw3+hw4+hw5+hw6+hw7+hw8+hw9+hw10+hw11+hw12+final_test,
                total_score_without_hw = test1+test2+test3+test4+final_test,
                final_score = semester_tests+hw1+hw2+hw3+hw4+hw5+hw6+hw7+hw8+hw9+hw10+hw11+hw12+final_test,
                d_szfvar = case_when(Group=="Székesfehérvár"~1,
                                     TRUE~0),
                d_nowork = case_when(`Dolgozik vagy tervez dolgozni ebben a félévben az egyetem mellett?`=="Nem"~1,
                                     TRUE~0),
                d_okt = case_when(Teacher_id ==1 ~7,
                                  TRUE~Teacher_id),
                d_mothereduc_uni =case_when(mother_educ=="Főiskola, egyetem"~1,
                                            TRUE~0),
                d_derivation = case_when(`Tanult deriválni a középiskolás évei alatt?`=="Igen"~1,
                                         TRUE~0),
                time_slot_tuesday = case_when(Time_slot == "K:08:00-09:30(E.3.328)" | 
                                                Time_slot == "K:09:50-11:20(S.A.EA1)" | 
                                                Time_slot == "K:13:40-15:10(C 664)" | 
                                                Time_slot == "K:17:20-18:50(E.3.395)" ~1,
                                              TRUE~0),
                time_slot_wednesday = case_when(Time_slot == "SZE:08:00-09:30(E.3.309)" | 
                                                  Time_slot == "SZE:09:50-11:20(C 103)" | 
                                                  Time_slot == "SZE:15:30-17:00(E.3.309)" | 
                                                  Time_slot == "SZE:15:30-17:00(E.3.324)" ~1,
                                                TRUE~0), 
                time_slot_thursday = case_when(Time_slot == "CS:08:00-09:30(E.3.326)" ~1,
                                               TRUE~0),
                time_slot_friday = case_when(time_slot_tuesday == 1| 
                                               time_slot_wednesday==1 | 
                                               time_slot_wednesday==1 ~0,
                                             TRUE~1),
                treatment_hybrid = case_when(Treatment=="Hybrid"~1,
                                             TRUE ~0),
                treatment_loss = case_when(Treatment =="Loss"~1,
                                           TRUE~0),
                treatment_gain = case_when(Treatment=="Gain" ~1,
                                           TRUE~0),
                homework_total = hw1+hw2+hw3+hw4+hw5+hw6+hw7+hw8+hw9+hw10+hw11+hw12,
                d_no = ifelse(gender == "Nő",1,0),
                semester_tests_total = test1 + test2 + test3 + test4) %>% 
  dplyr::select(test1,test2,test3,test4, final_test,homework_total,total_score,final_score,semester_tests_total,gender,
                treatment_gain,treatment_loss,
                treatment_hybrid, 
                homework_total,semester_tests,total_score,final_test,
                d_no,d_mothereduc_uni,d_derivation,
                time_slot_thursday,time_slot_friday,
                #d_okt2,d_okt3,d_okt4,d_okt5,d_okt6,d_okt7,
                Group,Neptun_id,
                macro_interest,
                macro_grade_goal,
                risk_pref,
                comp_pref,
                time_pref) %>% 
  #drop_na() %>% 
  as.data.frame()
  


mydata$treatment_gain <- factor(mydata$treatment_gain)
mydata$treatment_loss <- factor(mydata$treatment_loss)
mydata$treatment_hybrid <- factor(mydata$treatment_hybrid)
mydata$d_no <- factor(mydata$treatment_hybrid)
mydata$d_derivation <- factor(mydata$d_derivation)
mydata$d_mothereduc_uni <- factor(mydata$d_mothereduc_uni)

followup <-  readxl::read_xlsx("./data_origin/data_merge/loss_aversion_macro_endsurvey.xlsx") %>% 
  select(Neptun,
         Know_other_groups,
         minutes_per_week,
         fairness_1_7,
         Comment) %>% 
  rename(Neptun_id = Neptun) %>% 
  filter(!is.na(Neptun_id))

merged <-  left_join(mydata, followup, by = "Neptun_id", unmatched = "drop") %>% 
  filter(!is.na(fairness_1_7)) %>% 
  filter(final_test>0)

merged_anon <- merged %>% 
  select(-Neptun_id,gender) %>% 
  rename(d_female = d_no) %>% 
  mutate(knew_other_groups = case_when(Know_other_groups=="Nem tudtam róla"~0,
                                T~1)) %>% 
  select(-Know_other_groups)


write.csv(merged_anon, "./regression_data/followup_merged_anon.csv")




#----------------------
# combined descriptive tables:

desc_main <-  mydata %>%  
  dplyr::select(test1,test2,test3,test4,semester_tests,semester_tests_total, final_test,homework_total,total_score,final_score,treatment_gain,treatment_loss,
              treatment_hybrid, d_mothereduc_uni,d_derivation,gender) %>% 
  as.data.frame() %>% 
  mutate(df_type = "Main Text Database")

desc_followup <- merged %>% 
  dplyr::select(test1,test2,test3,test4,semester_tests,semester_tests_total, final_test,homework_total,total_score,final_score,treatment_gain,treatment_loss,
                treatment_hybrid, d_mothereduc_uni,d_derivation,gender) %>% 
  drop_na() %>% 
  as.data.frame() %>% 
  mutate(df_type = "Follow-up Database")

full_descriptive_tech <- merge(desc_main,desc_followup, all = T)



# Create a summary table
summary_table <- full_descriptive_tech %>%
  tbl_summary(by = df_type,
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label ~ "Variable") %>%
  modify_spanning_header(all_stat_cols() ~ "Databases") 

# GET tests using crosstable:
my_crosstable <- crosstable(full_descriptive_tech, by = df_type, test = TRUE, funs = c(mean = mean, "std error" = sd))

# Convert crosstable object to a data frame
my_dataframe <- as.data.frame(my_crosstable) %>% 
  select(-.id) %>% 
  rename(Variable = label)
rows<- seq(2, nrow(my_dataframe), by = 2)

test_results <- my_dataframe[rows,c("Variable","test")]
test_results$test <-  stringr::str_sub(test_results$test, start = 10)

# Print the summary table
summary_table %>% 
  as.data.frame(row.names = NULL) %>% 
  left_join(.,test_results, by = "Variable") %>% 
  #mutate(Test_results = test_results) %>% 
  stargazer::stargazer(summary = FALSE, rownames = F)

#----------------------

rm(summary_table)

#### balance table for the follow up questions by treatments

balance_final_tech <- merged %>% 
  dplyr::mutate(Treatment = case_when(treatment_gain == 1 ~"Gain",
                                      treatment_loss == 1 ~ "Loss",
                                      treatment_hybrid == 1 ~ "Hybrid")) %>% 
  dplyr::select(test1, test2, test3, test4, semester_tests, final_test, final_score,
                minutes_per_week, fairness_1_7, Treatment,Know_other_groups) %>% 
  mutate(Know_other_groups = case_when(Know_other_groups=="Nem tudtam róla"~0,
                                       TRUE~1))
  

balance_final_tech$fairness_1_7 <- as.numeric(balance_final_tech$fairness_1_7)
balance_final_tech$Know_other_groups <- as.factor(balance_final_tech$Know_other_groups)

summary_table <- balance_final_tech %>%
  tbl_summary(by = Treatment,
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              type = list(fairness_1_7 ~ "continuous")) %>%
  modify_header(label ~ "Variable") %>%
  modify_spanning_header(all_stat_cols() ~ "Treatments")

# Helper to build pairwise crosstables with Wilcoxon
pair_ct <- function(dat, groups) {
  dat %>%
    filter(Treatment %in% groups) %>%
    mutate(Treatment = droplevels(factor(Treatment))) %>%
    crosstable(by = Treatment, test = TRUE, test_args = my_test_args,
               funs = c(mean = mean, "std error" = sd)) %>%
    as.data.frame() %>%
    select(-.id)
}

# ---- Pairwise comparison: 1 vs 2 ----
crosstab_1_vs_2 <- pair_ct(balance_final_tech, c("Gain", "Hybrid"))
rows_1_vs_2 <- seq(2, nrow(crosstab_1_vs_2), by = 2)
test_results_1_vs_2 <- crosstab_1_vs_2[rows_1_vs_2, "test"]

# ---- Pairwise comparison: 1 vs 3 ----
crosstab_1_vs_3 <- pair_ct(balance_final_tech, c("Gain", "Loss"))
rows_1_vs_3 <- seq(2, nrow(crosstab_1_vs_3), by = 2)
test_results_1_vs_3 <- crosstab_1_vs_3[rows_1_vs_3, "test"]

# ---- Pairwise comparison: 2 vs 3 ----
crosstab_2_vs_3 <- pair_ct(balance_final_tech, c("Hybrid", "Loss"))
rows_2_vs_3 <- seq(2, nrow(crosstab_2_vs_3), by = 2)
test_results_2_vs_3 <- crosstab_2_vs_3[rows_2_vs_3, "test"]

# ---- Final summary table with added test columns ----
final_table <- summary_table %>%
  as.data.frame(row.names = NULL) %>%
  filter(Variable!=0) %>% 
  mutate(
    `Gain vs. Hybrid` = c(test_results_1_vs_2, NA),
    `Gain vs. Loss`   = c(test_results_1_vs_3,NA),
    `Hybrid vs. Loss` = c(test_results_2_vs_3,NA)
  )

colnames(final_table)[1:4] <- c("Variable", "Gain, N = 24", "Hybrid, N = 27", "Loss, N = 50")

# Output using stargazer
stargazer::stargazer(final_table, summary = FALSE, rownames = FALSE)






# --- 
# regression table:

last_reg_df <- merged %>% 
  mutate(macro_grade_max_motivation = case_when(macro_grade_goal =="5" ~1,
                                   T ~ 0),
         fairness_1_7 = as.numeric(fairness_1_7)) %>% 
  mutate(fair = case_when(fairness_1_7>=4~1,
         T~0),
         knew_other_groups = case_when(Know_other_groups=="Nem tudtam róla"~0,
                                       T~1)
) %>% 
  mutate(final_test=final_test / 40 * 100,
         semester_tests == semester_tests / 48 *100,
         Female = ifelse(gender=="Nő",1,0)
  )




m1 <- lm(final_test~ treatment_loss+treatment_hybrid+factor(gender)+time_slot_thursday+time_slot_friday, data = last_reg_df
                 )
m2 <- lm(final_test~ treatment_loss+treatment_hybrid+factor(gender)+knew_other_groups+d_mothereduc_uni+time_slot_thursday+time_slot_friday, data = last_reg_df)

m3 <- lm(final_test~ treatment_loss+treatment_hybrid+factor(gender)+fair+d_mothereduc_uni+time_slot_thursday+time_slot_friday, data = last_reg_df)

m4 <- lm(final_test~ treatment_loss+treatment_hybrid+factor(gender)+minutes_per_week+d_mothereduc_uni+time_slot_thursday+time_slot_friday, data = last_reg_df)

m5 <- lm(final_test~ treatment_loss+treatment_hybrid+factor(gender)+macro_grade_max_motivation+d_mothereduc_uni+time_slot_thursday+time_slot_friday, data = last_reg_df)

m6 <- lm(final_test~ treatment_loss+treatment_hybrid+factor(gender)+knew_other_groups+fair+minutes_per_week+macro_grade_max_motivation+d_mothereduc_uni+time_slot_thursday+time_slot_friday, data = last_reg_df)

stargazer::stargazer(m1,m2,m3,m4,m5,m6)


# to make clusteres standard errors, import to stata:

write.csv(last_reg_df, "C:/r_projects/cikkek/loss_aversion_classroom/new_stata_codes/app_data_merged.csv")


