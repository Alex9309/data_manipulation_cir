
# Packages ----------------------------------------------------------------

#install.packages('labelled')

library(tidyverse)
library(haven)
library(labelled)

# Data --------------------------------------------------------------------

?read_sav() # user_na = FALSE, 99 - останется самим собой
data <- read_sav('Data/МСЖ_19_финальная.sav')
#data <- read_sav('Data/МСЖ_19_финальная.sav', user_na = T)

# Просмотр данных ---------------------------------------------------------

View(data)
head(data)
glimpse(data)

# Просмотр данных из SPSS -------------------------------------------------

look_for(data)
look_for(data, details = T)
View(look_for(data, details = T))

info_on_data <- look_for(data, details = T)

# Написание кода ----------------------------------------------------------

## изнутри-кнаружи

as.character(sum(as.logical(sort(c(1, 2, 3, 4, 5, 6, 3)))))

## слева-направо

c(1, 2, 3, 4, 5, 6, 3) %>% 
  sort(decreasing = T) %>% 
  as.logical() %>%  
  sum() %>% 
  as.character()

# Посмотреть labels нашей переменной --------------------------------------

val_labels(data$VV1)

# Filters -----------------------------------------------------------------

## Один фильтр

data %>% 
  filter(VV1 == 1) -> data_bachelors_1

data %>% 
  filter(VV1 %in% c(1, 3, 8))

data %>% 
  filter(!is.na(USL2A))

## Несколько фильтров 

data %>% 
  filter(VV1 == 1, VV0 == 1)

## Работа фильтров изнутри

data$VV1 == 1

!(data$VV1 %in% c(1, 3, 8))

!is.na(data$USL2A)
is.na(data$USL2A)

c(1, 2, 3, NA) %in% c(1, 2)

# Select ------------------------------------------------------------------

## Простой выбор
data %>% 
  select(VV1, VV0, USL1s2_r1, USL1s2_r2, USL1s2_r3, USL1s2_r4, SERV1_1, SERV1_2, SERV1_3)

## Выбор с помощью помощника

data %>% 
  select(starts_with("VV"))

data %>% 
  select(ends_with("_r1"))

data %>% 
  select(contains("_r"))

data %>% 
  select(num_range("USL1s2_r", 1:3))

## Выбор по порядковому номеру переменной

data %>% 
  select(2:581)

data %>% 
  select(2:(ncol(.) - 1))

## Выбор по названиям

data %>% 
  select(VV0:USL1s2_r1)

## Выбор через нескольких помощников

data %>% 
  select(starts_with("VV"), num_range("USL1s2_r", 1:4), num_range('SERV1_', 1:3))

## Красивая форма записи, ни на что не влияет, но читабельность!

data %>% 
  select(
    starts_with("VV"),
    num_range("USL1s2_r", 1:4),
    num_range('SERV1_', 1:3)
  )

data %>% 
  select(starts_with("VV") & ends_with("1"))

data %>% 
  select(starts_with("SERV") & ends_with("1"))


# Summarize ---------------------------------------------------------------

data %>% 
  summarize(
    N = n()
  )

data %>% 
  filter(USL1s2_r1 != 99) %>% 
  summarize(
    mean = mean(USL1s2_r1, na.rm = T),
    sd = sd(USL1s2_r1, na.rm = T), 
    n = n()
  )


# Group_by ----------------------------------------------------------------

## Одна группа 

data %>% 
  filter(USL1s2_r1 != 99) %>% 
  group_by(VV1) %>% 
  summarize(
    mean = mean(USL1s2_r1, na.rm = T),
    sd = sd(USL1s2_r1, na.rm = T), 
    n_of_people = n()
  ) %>% 
  mutate(
    percentage = n_of_people / sum(n_of_people)
  )

data %>% 
  group_by(VV1) %>% 
  filter(USL1s2_r1 > mean(USL1s2_r1, na.rm = T)) %>% 
  ungroup() %>% 
  filter()

## Несколько групп

data %>% 
  filter(USL1s2_r1 != 99) %>% 
  group_by(VV1, VV0) %>% 
  summarize(
    mean = mean(USL1s2_r1, na.rm = T),
    sd = sd(USL1s2_r1, na.rm = T), 
    n = n()
  ) %>% 
  ungroup() %>% 
  mutate(
    percentage = n / sum(n)
  )




