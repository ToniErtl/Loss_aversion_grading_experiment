library(tidyverse)
library(ggthemes)
library(readxl)


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
  dplyr::filter(gender!="Egyéb, nem kívánok válaszolni"  & !is.na(final_test)) 



regressions_data <- mydata %>% 
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
                                 TRUE~0)
  )





regressions_data <- mydata %>% 
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
                d_okt2 = case_when(d_okt ==2~1,
                                   TRUE~0),
                d_okt3 = case_when(d_okt ==3~1,
                                   TRUE~0),
                d_okt4 = case_when(d_okt ==4~1,
                                   TRUE~0),
                d_okt5 = case_when(d_okt ==5~1,
                                   TRUE~0),
                d_okt6 = case_when(d_okt ==6~1,
                                   TRUE~0),
                d_okt7 = case_when(d_okt ==7~1,
                                   TRUE~0),
                female_loss = case_when(Treatment=="Loss" & gender=="Nő" ~1,
                                        TRUE~0),
                female_hybrid = case_when(Treatment == "Hybrid" & gender == "Nő" ~1,
                                          TRUE~0)
  ) %>% 
  mutate(loss_semester = case_when(treatment_loss== 1 ~1,
                                    TRUE ~ 0),
         loss_final = case_when(treatment_loss== 1 | treatment_hybrid== 1 ~1,
         TRUE ~ 0)) %>% 
  select(-Neptun_id) %>% 
  dplyr::select(test1,test2,test3,test4,final_test,semester_tests,
    total_score, total_score_without_hw, final_score,
    d_szfvar, d_nowork, d_okt, d_mothereduc_uni, d_derivation,
    time_slot_tuesday, time_slot_wednesday, time_slot_thursday, time_slot_friday,
    treatment_hybrid, treatment_loss, treatment_gain,
    homework_total, d_no,
    d_okt2, d_okt3, d_okt4, d_okt5, d_okt6, d_okt7,
    female_loss, female_hybrid, Group,loss_final,loss_semester
  )



regressions_data$Group_cat <- as.numeric(factor(
  regressions_data$Group,
  levels = c("G01","G02","G03","G04","G05","G06","G07","G08",
             "G09","G10","G11","G12","G13","G14","Székesfehérvár")
))


write_csv2(regressions_data,"./data_clean.csv" )
