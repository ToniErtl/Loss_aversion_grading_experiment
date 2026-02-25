knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(ggthemes)
library(readxl)
library(RCT)



mydata <- readxl::read_xlsx(".//data_anon.xlsx")


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

# filter out strategic decisionmaking by defining the maximum of the 4 tests
mydata <-   mydata %>%
  mutate(semester_tests = test1+test2+test3+test4-worst_test) %>% 
  dplyr::filter(gender!="Egyéb, nem kívánok válaszolni") 


#descriptive table


regressions_data <- mydata %>% 
  dplyr::filter(gender!="Egyéb, nem kívánok válaszolni" ) %>% 
  dplyr::mutate(total_score = test1+test2+test3+test4+hw1+hw2+hw3+hw4+hw5+hw6+hw7+hw8+hw9+hw10+hw11+hw12+final_test,
                total_score_without_hw = test1+test2+test3+test4+final_test,
                final_score = semester_tests+hw1+hw2+hw3+hw4+hw5+hw6+hw7+hw8+hw9+hw10+hw11+hw12+final_test,
                semester_tests_perc = semester_tests /88,
                d_szfvar = case_when(Group=="Székesfehérvár"~1,
                                     TRUE~0),
                d_okt = case_when(Teacher_id ==1 ~7,
                                  TRUE~Teacher_id),
                d_mothereduc_uni =case_when(mother_educ=="Főiskola, egyetem"~1,
                                            TRUE~0),
                d_derivation = case_when(`Tanult deriválni a középiskolás évei alatt?`=="Igen"~1,
                                         TRUE~0))


descriptive_table <- regressions_data %>% 
  dplyr::filter(gender!="Egyéb, nem kívánok válaszolni"  & !is.na(final_test)) %>% # ha a final test hiányzik, akkor nem akarjuk figyelembe venni, mert akkor komolytalan volt az ember 
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
                d_no = case_when(gender=="Nő"~1,
                                 TRUE~0),
                semester_tests_total = test1 + test2 + test3 + test4
  ) %>% 
  dplyr::select(test1,test2,test3,test4,semester_tests,semester_tests_total, final_test,homework_total,total_score,final_score,treatment_gain,treatment_loss,
                treatment_hybrid, d_mothereduc_uni,d_derivation,d_no, d_nowork,classes_this_semester,credits_this_semester,
                time_slot_friday,time_slot_tuesday,time_slot_wednesday,time_slot_thursday) %>% 
  drop_na() %>% 
  as.data.frame()



stargazer::stargazer(descriptive_table, summary = TRUE, summary.stat = c("n","mean","median","sd","min","max"))



# Sane descriptive data with percentages on scores



descriptive_table_perc <- regressions_data %>% 
  dplyr::filter(gender!="Egyéb, nem kívánok válaszolni"  & !is.na(final_test)) %>% # ha a final test hiányzik, akkor nem akarjuk figyelembe venni, mert akkor komolytalan volt az ember 
  dplyr::mutate(total_score_perc = (semester_tests+hw1+hw2+hw3+hw4+hw5+hw6+hw7+hw8+hw9+hw10+hw11+hw12+final_test),
                semester_tests_perc = semester_tests / 48 *100,
                test1_perc =test1/16 *100,
                test2_perc =test2/16 *100,
                test3_perc =test3/16 *100,
                test4_perc =test4/16 *100,
                final_test_perc = final_test / 40 *100, 
                hw_perc = (hw1+hw2+hw3+hw4+hw5+hw6+hw7+hw8+hw9+hw10+hw11+hw12)/12 *100,
                #  total_score_without_hw = test1+test2+test3+test4+final_test,
                # final_score = semester_tests+hw1+hw2+hw3+hw4+hw5+hw6+hw7+hw8+hw9+hw10+hw11+hw12+final_test,
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
                d_no = case_when(gender=="Nő"~1,
                                 TRUE~0),
                semester_tests_total = test1 + test2 + test3 + test4
  ) %>% 
  dplyr::select(test1_perc,test2_perc,test3_perc,test4_perc,semester_tests_perc, final_test_perc,hw_perc, total_score_perc,treatment_gain,treatment_loss,
                treatment_hybrid, d_mothereduc_uni,d_derivation,d_no, d_nowork,classes_this_semester,credits_this_semester,
                time_slot_friday,time_slot_tuesday,time_slot_wednesday,time_slot_thursday) %>% 
  drop_na() %>% 
  as.data.frame()


stargazer::stargazer(descriptive_table_perc, summary = TRUE, summary.stat = c("n","mean","median","sd","min","max"))






