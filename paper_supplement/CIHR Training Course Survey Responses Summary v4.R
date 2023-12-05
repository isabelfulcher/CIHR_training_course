#############################################################################
############## CIHR Training Course Survey Responses Summary v4 #############
#############################################################################

## Note: Likert scale plot x-axes were created in word for simplicity - no convenient way in R

## INPUTS ##

library(dplyr)
library(openxlsx)
library(tidyverse)
library(summarytools)
library(stringr)
library(prettyGraphs)
library(BuenColors)
library(plotly)
library(lubridate)
library(cowplot)
library(scales)
library(likert)

lecture1 <- read.xlsx("Survey_Responses.xlsx", sheet = 1, startRow = 3)
lecture2 <- read.xlsx("Survey_Responses.xlsx", sheet = 2, startRow = 3)
lecture3 <- read.xlsx("Survey_Responses.xlsx", sheet = 3, startRow = 3)
lecture4 <- read.xlsx("Survey_Responses.xlsx", sheet = 4, startRow = 3)
CourseEnd <- read.xlsx("Survey_Responses.xlsx", sheet = 5, startRow = 3)

## Content, Pace, and Clarity across lectures ##

LectureRatings <- rbind(lecture1 %>% mutate(lecture = 1) %>% 
          select(c(lecture, RateContent, RatePace, RateClarity)),
                 lecture2 %>% mutate(lecture = 2) %>% 
          select(c(lecture, RateContent, RatePace, RateClarity))) %>%
  rbind(.,lecture3 %>% mutate(lecture = 3) %>% 
          select(c(lecture, RateContent, RatePace, RateClarity))) %>%
  rbind(.,lecture4 %>% mutate(lecture = 4) %>% 
          select(c(lecture, RateContent, RatePace, RateClarity))) %>%
  mutate(RateClarity = ifelse(is.na(RateClarity), 5, RateClarity),
         CatClarity = ifelse(RateClarity > 6.5, "Very clear", NA),
         CatClarity = ifelse(6.5 > RateClarity & RateClarity > 4.5, "Clear", CatClarity),
         CatClarity = ifelse(4.5 > RateClarity & RateClarity > 2.5, "Unclear", CatClarity),
         CatClarity = ifelse(RateClarity < 2.5, "Not at all clear", CatClarity),
         CatContent = ifelse(RateContent > 6.5, "Too complex", NA),
         CatContent = ifelse(6.5 > RateContent & RateContent > 4.5, "Complex", CatContent),
         CatContent = ifelse(4.5 > RateContent & RateContent > 2.5, "Basic", CatContent),
         CatContent = ifelse(RateContent < 2.5, "Too basic", CatContent),
         CatPace = ifelse(RatePace > 6.5, "Too fast", NA),
         CatPace = ifelse(6.5 > RatePace & RatePace > 4.5, "Fast", CatPace),
         CatPace = ifelse(4.5 > RatePace & RatePace > 2.5, "Slow", CatPace),
         CatPace = ifelse(RatePace < 2.5, "Too slow", CatPace)) %>%
  select(c(lecture,CatClarity,CatContent,CatPace)) %>%
  mutate(lecture = ifelse(lecture == "1", "Lecture 1 (n=15)", lecture),
         lecture = ifelse(lecture == "2", "Lecture 2 (n=13)", lecture),
         lecture = ifelse(lecture == "3", "Lecture 3 (n=14)", lecture),
         lecture = ifelse(lecture == "4", "Lecture 4 (n=14)", lecture)
         ) %>%
  mutate(lecture = factor(lecture, 
                          levels = c("Lecture 4 (n=14)","Lecture 3 (n=14)","Lecture 2 (n=13)","Lecture 1 (n=15)")),
         CatClarity = factor(CatClarity, levels = c("Very unclear","Unclear", "Clear", "Very clear")),
         CatContent = factor(CatContent, levels = c("Too basic","Basic", "Complex", "Too complex")),
         CatPace = factor(CatPace, levels = c("Too slow","Slow", "Fast", "Too fast"))) %>%
  arrange(lecture)

names(LectureRatings) <- c(
  "Lecture",
  "How would you rate the clarity of this lecture?",
  "How would you rate the content of this lecture?",
  "How would you rate the pace of this lecture?"
)

clear <- likert(LectureRatings %>% select(`How would you rate the clarity of this lecture?`), grouping=LectureRatings$Lecture)
clear[["results"]]$`Very unclear` = c(0,0,0,0)

clarity <- plot(clear, center = 2.5, include.center = FALSE, 
     group.order=names(LectureRatings$lecture), col=c("grey75","grey50","grey25","black"))

content <- likert(LectureRatings %>% select(`How would you rate the content of this lecture?`),grouping=LectureRatings$Lecture) 
content[["results"]]$`Too basic` = c(0,0,0,0)
                  
contents <- plot(content, center = 2.5, include.center = FALSE, 
     group.order=names(LectureRatings$lecture), col=c("grey75","grey50","grey25","black"))

pace <- likert(LectureRatings %>% select(`How would you rate the pace of this lecture?`),grouping=LectureRatings$Lecture)
pace[["results"]]$`Too slow` = c(0,0,0,0)

paces <- plot(pace, center = 2.5, include.center = FALSE, 
     group.order=names(LectureRatings$lecture), col=c("grey75","grey50","grey25","black"))

plot_grid(clarity,contents,paces, ncol=1, rel_heights = c(2,2,2))

## Feedback on effectiveness of lecture components ##

lectures <- lecture1 %>% mutate(lecture = 1) %>%
  select(-c(HowEffectiveMentiPolls,HowEffectiveRInstructions)) %>%
  rbind(.,lecture2 %>% mutate(lecture = 2)) %>%
  rbind(.,lecture3 %>% mutate(lecture = 3)) %>%
  rbind(.,lecture4 %>% mutate(lecture = 4)) %>%
  select(-c(Timestamp)) %>% mutate(
    lecture = ifelse(lecture == "1", "Lecture 1 (n=15)", lecture),
    lecture = ifelse(lecture == "2", "Lecture 2 (n=13)", lecture),
    lecture = ifelse(lecture == "3", "Lecture 3 (n=14)", lecture),
    lecture = ifelse(lecture == "4", "Lecture 4 (n=14)", lecture),
    lecture = factor(lecture, levels = c("Lecture 4 (n=14)","Lecture 3 (n=14)","Lecture 2 (n=13)","Lecture 1 (n=15)"))
) 

comp_levels = c("Not at all effective","Very small effect","Somewhat effective","Very effective")

Components <- lectures %>% select(c(lecture, HowEffectiveAllowGroupQs, HowEffectiveGroupQs, HowEffectiveSmallGroups)) %>%
  filter(!is.na(HowEffectiveAllowGroupQs)) %>% mutate(
    HowEffectiveAllowGroupQs = factor(HowEffectiveAllowGroupQs, levels = comp_levels),
    HowEffectiveGroupQs = factor(HowEffectiveGroupQs, levels = comp_levels),
    HowEffectiveSmallGroups = factor(HowEffectiveSmallGroups, levels = comp_levels)
  ) 

names(Components) <- c(
  "Lecture",
  "How effective was it to allow questions from the group during lectures?",
  "How effective was it to ask the group questions during the lectures?",
  "How effective was it to use small group breakouts during lectures?"
)

qs <- Components %>% select(-Lecture)

comp <- likert(qs, grouping=Components$Lecture)
comp[["results"]]$`Not at all effective` = c(0)

plot(comp, low.color="grey80", high.color="grey20") + theme(strip.text.x = element_text(size = 15))

## Final Survey Aspect Ratings ##

rating_levels = c(1,2,3,4,5,6,7,8)

EndRatings <- CourseEnd %>% 
  select(c(RateNumber, RateLength, RateContent, RatePace, 
           RateClarity, RateEngagement)) %>% mutate(
             RateClarity = ifelse(is.na(RateClarity), 5, RateClarity),
             Number = factor(RateNumber, levels = rating_levels),
             Length = factor(RateLength, levels = rating_levels),
             Content = factor(RateContent, levels = rating_levels),
             Pace = factor(RatePace, levels = rating_levels),
             Clarity = factor(RateClarity, levels = rating_levels),
             Engagement = factor(RateEngagement, levels = rating_levels)
           )

Ratings <- EndRatings %>% dplyr::select(Number,Length,Content,Pace,Clarity,Engagement)

names(Ratings) <- c("Too few lectures                                                         Number                                                     Too many lectures",
                    "Lectures were too short                                             Length                                                Lectures were too long",
                    "Too basic                                                                      Content                                                                Too complex",
                    "Too slow                                                                          Pace                                                                          Too fast",
                    "Unclear                                                                           Clarity                                                                              Clear",
                    "Too little engagement                                           Engagement                                        Too much engagement")

Ratings = Ratings %>% likert()

plot(Ratings, type="density") + geom_vline(xintercept = 4.5, color = "grey50") 

## How did the course affect your understanding? ##

und_levels = c("No improvement",
               "Very little improvement",
               "Some improvement",
               "Great improvement")

Understanding <- CourseEnd %>% 
  select(c(CourseEffectUnderstandHMIS,CourseEffectUnderstandTimeSeries,
           CourseEffectUnderstandSSMethods,CourseEffectUnderstandInterpretation,
           CourseEffectUnderstandR)) %>%
  mutate(CourseEffectUnderstandHMIS = ifelse(CourseEffectUnderstandHMIS =="Not applicable", NA, CourseEffectUnderstandHMIS),
         CourseEffectUnderstandR = ifelse(CourseEffectUnderstandR =="Not applicable", NA, CourseEffectUnderstandR),
         CourseEffectUnderstandHMIS = factor(CourseEffectUnderstandHMIS, levels=und_levels),
         CourseEffectUnderstandTimeSeries = factor(CourseEffectUnderstandTimeSeries, levels=und_levels),
         CourseEffectUnderstandSSMethods = factor(CourseEffectUnderstandSSMethods, levels=und_levels),
         CourseEffectUnderstandInterpretation = factor(CourseEffectUnderstandInterpretation, levels=und_levels),
         CourseEffectUnderstandR = factor(CourseEffectUnderstandR, levels=und_levels)
  )

names(Understanding) = c(
  "*HMIS data",
  "Time series modeling",
  "Syndromic Surveillance methods",
  "Your ability to interpret results",
  "**Your ability with R"
)

Understanding = Understanding %>% likert()

plot(Understanding, centered=TRUE, center = 2.5, low.color="grey80", high.color="black")

## Lab Component Ratings ##

LabRatings <- CourseEnd %>% 
  select(c(RateLabNumber, RateLabLength, RateLabContent, RateLabPace, 
           RateLabClarity, RateLabEngagement)) %>%
  filter(!is.na(RateLabNumber)) %>% mutate(
             RateLabEngagement = ifelse(is.na(RateLabEngagement), 5, RateLabEngagement),
             RateLabContent = ifelse(is.na(RateLabContent), 5, RateLabContent),
             Number = factor(RateLabNumber, levels = rating_levels),
             Length = factor(RateLabLength, levels = rating_levels),
             Content = factor(RateLabContent, levels = rating_levels),
             Pace = factor(RateLabPace, levels = rating_levels),
             Clarity = factor(RateLabClarity, levels = rating_levels),
             Engagement = factor(RateLabEngagement, levels = rating_levels)
           ) %>% dplyr::select(Number,Length,Content,Pace,Clarity,Engagement)

names(LabRatings) <- c("Too few labs                                                                 Number                                                              Too many labs",
                      "Labs were too short                                                     Length                                                       Labs were too long",
                      "Too basic                                                                       Content                                                                  Too complex",
                      "Too slow                                                                           Pace                                                                             Too fast",
                      "Unclear                                                                            Clarity                                                                                Clear",
                      "Too little engagement                                              Engagement                                         Too much engagement")

LabRatings = LabRatings %>% likert()



plot(LabRatings, type="density") + geom_vline(xintercept = 4.5, color = "grey50")

## Lab individual component ratings

ILabRatings <- lecture1 %>% mutate(lecture = "Lab 1 (n=15)") %>% 
          select(c(lecture, RateLabContent, RateLabPace, RateLabClarity)) %>%
  rbind(.,lecture2 %>% mutate(lecture = "Lab 2 (n=12)") %>% 
          select(c(lecture, RateLabContent, RateLabPace, RateLabClarity))) %>%
  rbind(.,lecture3 %>% mutate(lecture = "Lab 3 (n=12)") %>% 
          select(c(lecture, RateLabContent, RateLabPace, RateLabClarity))) %>%
  rbind(.,lecture4 %>% mutate(lecture = "Lab 4 (n=12)") %>% 
          select(c(lecture, RateLabContent, RateLabPace, RateLabClarity))) %>%
  filter(!is.na(RateLabClarity)) %>%
  mutate(RateLabContent = ifelse(is.na(RateLabContent), 5, RateLabContent),
         Clarity = ifelse(RateLabClarity > 6.5, "Very clear", NA),
         Clarity = ifelse(6.5 > RateLabClarity & RateLabClarity > 4.5, "Clear", Clarity),
         Clarity = ifelse(4.5 > RateLabClarity & RateLabClarity > 2.5, "Unclear", Clarity),
         Clarity = ifelse(RateLabClarity < 2.5, "Not at all clear", Clarity),
         Content = ifelse(RateLabContent > 6.5, "Too complex", NA),
         Content = ifelse(6.5 > RateLabContent & RateLabContent > 4.5, "Complex", Content),
         Content = ifelse(4.5 > RateLabContent & RateLabContent > 2.5, "Basic", Content),
         Content = ifelse(RateLabContent < 2.5, "Too basic", Content),
         Pace = ifelse(RateLabPace > 6.5, "Too fast", NA),
         Pace = ifelse(6.5 > RateLabPace & RateLabPace > 4.5, "Fast", Pace),
         Pace = ifelse(4.5 > RateLabPace & RateLabPace > 2.5, "Slow", Pace),
         Pace = ifelse(RateLabPace < 2.5, "Too slow", Pace)) %>%
  select(c(lecture,Clarity,Content,Pace)) %>%
  mutate(lecture = factor(lecture, levels = c("Lab 4 (n=12)","Lab 3 (n=12)","Lab 2 (n=12)","Lab 1 (n=15)")),
         Clarity = factor(Clarity, levels = c("Not at all clear","Unclear", "Clear", "Very clear")),
         Content = factor(Content, levels = c("Too basic","Basic", "Complex", "Too complex")),
         Pace = factor(Pace, levels = c("Too slow","Slow", "Fast", "Too fast"))) %>%
  arrange(lecture)

names(ILabRatings) <- c(
  "Lecture",
  "How would you rate the clarity of this lab?",
  "How would you rate the content of this lab?",
  "How would you rate the pace of this lab?"
)

labclear <- likert(ILabRatings %>% select(`How would you rate the clarity of this lab?`), grouping=ILabRatings$Lecture)

labclarity <- plot(labclear, center = 2.5, include.center = FALSE, 
                group.order=names(ILabRatings$lecture), col=c("grey75","grey50", "grey25", "black"))

labcontent <- likert(ILabRatings %>% select(`How would you rate the content of this lab?`), grouping=ILabRatings$Lecture)
labcontent[["results"]]$`Too basic` = c(0)

labcontents <- plot(labcontent, center = 2.5, include.center = FALSE, 
                 group.order=names(ILabRatings$lecture), col=c("grey75","grey50", "grey25", "black"))

labpace <- likert(ILabRatings %>% select(`How would you rate the pace of this lab?`), grouping=ILabRatings$Lecture)

labpaces <- plot(labpace, center = 2.5, include.center = FALSE, 
              group.order=names(ILabRatings$lecture), col=c("grey75","grey50", "grey25", "black"))

plot_grid(labclarity,labcontents,labpaces, ncol=1, rel_heights = c(2,2,2))

## Feedback on effectiveness of lab components ##

labs <- rbind(lecture1 %>% mutate(lecture = "Lab 1 (n=15)") %>% 
                select(c(lecture, HowEffectiveCodeTime, HowEffectiveLabHandout, HowEffectiveTABreakouts)),
              lecture2 %>% mutate(lecture = "Lab 2 (n=12)") %>% 
                select(c(lecture, HowEffectiveCodeTime, HowEffectiveLabHandout, HowEffectiveTABreakouts))) %>%
        rbind(.,lecture3 %>% mutate(lecture = "Lab 3 (n=12)") %>% 
                select(c(lecture, HowEffectiveCodeTime, HowEffectiveLabHandout, HowEffectiveTABreakouts))) %>%
        rbind(.,lecture4 %>% mutate(lecture = "Lab 4 (n=12)") %>% 
                select(c(lecture, HowEffectiveCodeTime, HowEffectiveLabHandout, HowEffectiveTABreakouts))) %>%
        filter(!is.na(HowEffectiveLabHandout)) %>% mutate(
          lecture = factor(lecture, levels = c("Lab 4 (n=12)","Lab 3 (n=12)","Lab 2 (n=12)","Lab 1 (n=15)")),
    HowEffectiveCodeTime = factor(HowEffectiveCodeTime, levels = comp_levels),
    HowEffectiveLabHandout = factor(HowEffectiveLabHandout, levels = comp_levels),
    HowEffectiveTABreakouts = factor(HowEffectiveTABreakouts, levels = comp_levels)
  ) 

names(labs) <- c(
  "Lecture",
  "How effective was it to provide time for coding during labs?",
  "How effective was it to provide handouts for the lab?",
  "How effective was it to provide breakout rooms for TA Help during labs?"
)

labcomps <- labs %>% select(-Lecture)

labcomp <- likert(labcomps, grouping=labs$Lecture)

plot(labcomp, col=c("grey75","grey50","grey25","black")) + theme(strip.text.x = element_text(size = 15))

##########################################################################
########################## END CODE ######################################
##########################################################################
