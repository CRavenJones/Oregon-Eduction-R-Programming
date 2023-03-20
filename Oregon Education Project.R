library(tidyverse)

# This data is provided by Oregon's Department of Education. (https://www.oregon.gov/ode/reports-and-data/students/Pages/Cohort-Graduation-Rate.aspx)
# I have made some changes to the original data by cutting "<5.0%" and ">95.0%" as it is unnecessary for this analysis.

# Loading the data that I will be using. These are from Oregon Department of Education (ODE). 

state_testing20212022 <- read.csv('C:\\Users\\shysh\\OneDrive\\Documents\\pagr_schools_ela_tot_ecd_hom_mlc_sfc_2122.csv')
state_testing20182019 <- read.csv("C:\\Users\\shysh\\OneDrive\\Documents\\pagr_schools_ela_tot_ecd_hom_mlc_1819.csv")

# I am replacing the '*' and '--" values that ODE uses to mark less than 10 students participation and no student participation with '0' to have all numerical values. 

state_testing20182019[state_testing20182019 == '--'| state_testing20182019 == '*'] <- '0'
state_testing20212022[state_testing20212022 == '--'| state_testing20212022 == '*'] <- '0'

#

#Changing data types on number level, percent level, participants, and participation rate. I also condensed the table data that I want to work with.

state_testing20182019_v2 <- 
  state_testing20182019 %>%
  select(Number.Proficient, Percent.Proficient..Level.3.or.4., Number.Level.1, Number.Level.2, Number.Level.3, Number.Level.4, Number.of.Participants, Percent.Level.1, Percent.Level.2, Percent.Level.3, Percent.Level.4, Participation.Rate,
         District, School.ID, School, Subject, Grade.Level) %>%
  mutate(Number.Proficient= as.numeric(Number.Proficient), 
         Percent.Proficient..Level.3.or.4.= as.numeric(Percent.Proficient..Level.3.or.4.),
         Number.Level.1= as.numeric(Number.Level.1),
         Number.Level.2= as.numeric(Number.Level.2),
         Number.Level.3= as.numeric(Number.Level.3),
         Number.Level.4= as.numeric(Number.Level.4),
         Number.of.Participants= as.numeric(Number.of.Participants),
         Percent.Level.1= as.numeric(Percent.Level.1),
         Percent.Level.2= as.numeric(Percent.Level.2),
         Percent.Level.3= as.numeric(Percent.Level.3),
         Percent.Level.4= as.numeric(Percent.Level.4),
         Participation.Rate= as.numeric(Participation.Rate))

state_testing20212022_v2 <-
  state_testing20212022 %>%
  select(Number.Proficient, Percent.Proficient..Level.3.or.4., Number.Level.1, Number.Level.2, Number.Level.3, Number.Level.4, Number.of.Participants, Percent.Level.1, Percent.Level.2, Percent.Level.3, Percent.Level.4, Participation.Rate,
         District, School.ID, School, Subject, Grade.Level) %>%
  mutate(Number.Proficient= as.numeric(Number.Proficient), 
         Percent.Proficient..Level.3.or.4.= as.numeric(Percent.Proficient..Level.3.or.4.),
         Number.Level.1= as.numeric(Number.Level.1),
         Number.Level.2= as.numeric(Number.Level.2),
         Number.Level.3= as.numeric(Number.Level.3),
         Number.Level.4= as.numeric(Number.Level.4),
         Number.of.Participants= as.numeric(Number.of.Participants),
         Percent.Level.1= as.numeric(Percent.Level.1),
         Percent.Level.2= as.numeric(Percent.Level.2),
         Percent.Level.3= as.numeric(Percent.Level.3),
         Percent.Level.4= as.numeric(Percent.Level.4),
         Participation.Rate= as.numeric(Participation.Rate))

# Filter out zeros, group by schools, and find median/mean percent proficient

state_testing20182019_v3 <- 
  state_testing20182019_v2 %>%
  select(Number.Proficient, Percent.Proficient..Level.3.or.4., Number.Level.1, Number.Level.2, Number.Level.3, Number.Level.4, Number.of.Participants, Percent.Level.1, Percent.Level.2, Percent.Level.3, Percent.Level.4, Participation.Rate,
         District, School.ID, School, Subject, Grade.Level) %>%
  filter(Number.of.Participants >0) %>%
  group_by(Grade.Level,School.ID, School, Number.of.Participants) %>%
  summarise(medianPerP= median(Percent.Proficient..Level.3.or.4.), meanPerP= mean(Percent.Proficient..Level.3.or.4.)) %>%
  arrange(desc(medianPerP))

state_testing20212022_v3 <-
  state_testing20212022_v2 %>%
  select(Number.Proficient, Percent.Proficient..Level.3.or.4., Number.Level.1, Number.Level.2, Number.Level.3, Number.Level.4, Number.of.Participants, Percent.Level.1, Percent.Level.2, Percent.Level.3, Percent.Level.4, Participation.Rate,
         District, School.ID, School, Subject, Grade.Level) %>%
  filter(Number.of.Participants >0) %>%
  group_by(Grade.Level,School.ID, School, Number.of.Participants) %>%
  summarise(medianPerP= median(Percent.Proficient..Level.3.or.4.), meanPerP= mean(Percent.Proficient..Level.3.or.4.)) %>%
  arrange(desc(medianPerP))


