
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



#---- FIGURE 1


mydata %>% 
  ggplot(aes(semester_tests, col = Treatment))+
  geom_boxplot()+
  theme_minimal()+
  labs(#title = "Points earned throughout the first part of the semester",
    # subtitle="Without Székesfehérvár",
    caption = "Out of the 4 tests, the worst did not count; max. 48 point could be earned")+
  scale_color_colorblind()+
  xlab("Points scored")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size = 18))

ggsave("../plots/02_05_semester_test_score_boxplots.pdf", width = 12, height = 6)


#---- FIGURE 2

mydata %>% 
  ggplot(aes(final_test, col = Treatment))+
  geom_boxplot()+
  theme_minimal()+
  labs( #title = "Boxplot of Final test scores",
    #subtitle="Without Székesfehérvár",
    caption = "Maximum of 40 points could be earned; note that 'Hybrid' here got the 'Loss' treatment ")+
  scale_color_colorblind()+
  xlab("Points scored")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size = 18))


ggsave("../plots/03_05_exam_scores.pdf", width = 12, height = 6)



#---- FIGURE 3

final_score_g <- mydata %>% 
  filter(Group!="Székesfehérvár") %>% 
  mutate(final_score = semester_tests+final_test+hw1+hw2+hw3+hw4+hw5+hw6+hw7+hw8+hw9+hw10+hw11+hw12) %>% 
  ggplot(aes(final_score, fill = Treatment))+
  geom_histogram(aes(y =..density..),position = "dodge", binwidth = 3)+
  geom_density(bw = 3, alpha = 0.5)+
  theme_minimal()+
  #labs(title = "Distribution of Final Scores among Treatment groups")+
  #  subtitle="Without Székesfehérvár -- maximum points = 16 per test")+
  scale_color_colorblind()+
  xlab("Points scored")+
  ylab("Density")+
  theme( text = element_text(size = 14))+
  scale_fill_colorblind()+
  scale_color_colorblind()+
  geom_vline(xintercept = 86, linetype=2,
             color = "black", size=0.5)+
  geom_vline(xintercept = 74, linetype=2,
             color = "black", size=0.5)+
  geom_vline(xintercept = 62, linetype=2,
             color = "black", size=0.5)+
  geom_vline(xintercept = 50, linetype=2,
             color = "black", size=0.5)+
  facet_wrap(~Treatment, nrow = 3)+
  theme( text = element_text(size = 14))

final_score_g

ggsave("../plots/01_final_score_distributions.pdf", width =12, height = 8)




