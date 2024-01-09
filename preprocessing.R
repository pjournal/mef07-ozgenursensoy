### Step 1:  import libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
# library(ggridges)
library(knitr)
# library(kableExtra)
# library(DT)
# library(shiny)

# pisa <- read.csv("qqq.csv")


# names(pisa)
# save as Rdata
# save(pisa, file = "pisa.RData")

# Step 2:  import data

# load Rdata
load("pisa.RData")
# check the data
str(pisa)
length(pisa)
head(pisa)


# drop x column
pisa <- pisa %>% select(-X)
# check summary
summary(pisa)

#sapply(pisa, function(x) sum(is.na(x)))
# check the data

# str(pisa)
length(pisa)

# Step 3:  EDA

# calculate the PV1MATH+PV2MATH+PV3MATH+PV4MATH+PV5MATH+PV6MATH+PV7MATH+PV8MATH+PV9MATH+PV10MATH and save it as a new column
# get the column names of the math scores avg
colnames(pisa)[5:14]
pisa$SUMMATH <- rowSums(pisa[, 5:14], na.rm = TRUE)
pisa$mean_math <- rowMeans(pisa[, 5:14], na.rm = TRUE)
#calculate mean mat
# get READ scores columns
colnames(pisa)[15:24]
# calculate the SCI score and save it as a new column
pisa$SUMREAD <- rowSums(pisa[, 15:24], na.rm = TRUE)
pisa$mean_read <- rowMeans(pisa[, 15:24], na.rm = TRUE)
# get SCI scores columns
colnames(pisa)[25:34]
# calculate the SCI score and save it as a new column
pisa$SUMSCI <- rowSums(pisa[, 25:34], na.rm = TRUE)
pisa$mean_sci <- rowMeans(pisa[, 25:34], na.rm = TRUE)

# caluculate the mean of the math, read and sci scores by country
pisa_mean <- pisa %>% group_by(CNT) %>% summarise(mean_math = mean(mean_math, na.rm = TRUE),
                                                  mean_read = mean(mean_read, na.rm = TRUE),
                                                  mean_sci = mean(mean_sci, na.rm = TRUE))
# sort the data by mean_math
pisa_mean <- pisa_mean %>% arrange(desc(mean_math))
# visualize the data by mean_math
ggplot(pisa_mean, aes(x = reorder(CNT, mean_math), y = mean_math)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Mean Math Score by Country",
       x = "Country",
       y = "Mean Math Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# sort the data by sci score
pisa_mean <- pisa_mean %>% arrange(desc(mean_sci))
# visualize the data by mean_sci
ggplot(pisa_mean, aes(x = reorder(CNT, mean_sci), y = mean_sci)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Mean Science Score by Country",
       x = "Country",
       y = "Mean Science Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# sort the data by read score
pisa_mean <- pisa_mean %>% arrange(desc(mean_read))
# visualize the data by mean_read
ggplot(pisa_mean, aes(x = reorder(CNT, mean_read), y = mean_read)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Mean Reading Score by Country",
       x = "Country",
       y = "Mean Reading Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# SC017Q01NA Instruction hindered by: A lack of teaching staff Not at all # Very little # To some extent # A lot
pisa$SC017Q01NA
# group by SC017Q01NA calculate mean math score , mean read score and mean sci score
pisa_lack_of_teaching_staff <- pisa %>% group_by(SC017Q01NA) %>% summarise(mean_math = mean(SUMMATH, na.rm = TRUE),
                                                                           mean_read = mean(SUMREAD, na.rm = TRUE),
                                                                           mean_sci = mean(SUMSCI, na.rm = TRUE))
# line plot
pisa_lack_of_teaching_staff %>% gather(key = "subject", value = "score", mean_math, mean_read, mean_sci) %>%
  ggplot(aes(x = SC017Q01NA, y = score, color = subject)) +
  geom_line() +
  labs(title = "Mean Math, Reading and Science Score by Lack of Teaching Staff",
       x = "Lack of Teaching Staff",
       y = "Mean Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#  SC184*** mean math score, mean read score and mean sci score "Does your school offer professional development to
# mathematics teachers in any of the following?
pisa$SC184Q01JA # 1Yes 2No
# set 1 as yes and 2 as no
pisa_SC184Q01JA <- pisa %>% mutate(SC184Q01JA = ifelse(SC184Q01JA == 1, "Yes", "No"))
# drp the NA
pisa_SC184Q01JA <- pisa_SC184Q01JA %>% filter(!is.na(SC184Q01JA))
professional_development_to_mathematics_teachers <- pisa_SC184Q01JA %>% group_by(SC184Q01JA) %>% summarise(mean_math = mean(mean_math, na.rm = TRUE))
# line plot normalize the data
ggplot(professional_development_to_mathematics_teachers, aes(x = SC184Q01JA, y = mean_math)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Math Score by Professional Development to Mathematics Teachers",
       x = "Professional Development to Mathematics Teachers",
       y = "Mean Math Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
pisa_SC184Q01JA$SC184Q01JA

# During the last three months, what percentage of teaching
# staff in your school has attended a programme of
# professional development?
# SC025Q01NA A Staff who teach mathematics at your
# school
pisa$SC025Q02NA
# 0-25 26-50 51-75 76-100
pisa_SC025Q02NA <- pisa %>% mutate(SC025Q02NA = ifelse(SC025Q02NA <= 25, 25, ifelse(SC025Q02NA <= 50, 50, ifelse(SC025Q02NA <= 75, 75, 100))))

pisa_SC025Q02NA$SC025Q02NA
# drop nas
pisa_SC025Q02NA <- pisa_SC025Q02NA %>% filter(!is.na(SC025Q02NA))
# group by SC025Q02NA calculate mean math score
pisa_SC025Q02NA <- pisa_SC025Q02NA %>% group_by(SC025Q02NA) %>% summarise(mean_maths = mean(mean_math, na.rm = TRUE))
# line plot normalize the data
ggplot(pisa_SC025Q02NA, aes(x = SC025Q02NA, y = mean_maths)) +
  geom_line() +
  labs(title = "Mean Math Score by Percentage of Teaching Staff Attended a Programme of Professional Development",
       x = "Percentage of Teaching Staff Attended a Programme of Professional Development",
       y = "Mean Math Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# SC013 Is your school a public or a private school?
pisa$SC013Q01TA
# group by SC013Q01TA calculate mean math score # 1Public 2Private
pisa_SC013Q01TA <- pisa %>% group_by(SC013Q01TA) %>% summarise(mean_maths = mean(mean_math, na.rm = TRUE))
pisa_SC013Q01TA$SC013Q01TA <- ifelse(pisa_SC013Q01TA$SC013Q01TA == 1, "Public", "Private")
# drop nas
pisa_SC013Q01TA <- pisa_SC013Q01TA %>% filter(!is.na(SC013Q01TA))
#  plot  the data
ggplot(pisa_SC013Q01TA, aes(x = SC013Q01TA, y = mean_maths)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Math Score by Public or Private School",
       x = "Public or Private School",
       y = "Mean Math Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# SC182Q08JA01 Teachers of math with at least an [ISCED Level 6 ] qualification [with a major] in mathematics: Full-time
pisa$SC182Q08JA01
pisaSC182Q08JA01 <- pisa %>% group_by(SC182Q08JA01) %>% summarise(mean_maths = mean(mean_math, na.rm = TRUE))
pisaSC182Q08JA01 <- pisaSC182Q08JA01 %>% filter(!is.na(SC182Q08JA01))
ggplot(pisaSC182Q08JA01, aes(x = SC182Q08JA01, y = mean_maths)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Math Score by Teachers of math with at least an [ISCED Level 6 ] qualification [with a major] in mathematics: Full-time",
       x = "Teachers of math with at least an [ISCED Level 6 ] qualification [with a major] in mathematics: Full-time",
       y = "Mean Math Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# install.packages("dataMaid")
library(dataMaid)
makeDataReport(pisa, "pisa.html", replace = TRUE, echo = FALSE, style = "grid")

install.packages("DataExplorer")
library(DataExplorer)
create_report(pisa)
data(pisa)

install.



