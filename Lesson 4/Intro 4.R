
# Packages ----------------------------------------------------------------

library(tidyverse)
library(labelled)
library(haven)

# Data --------------------------------------------------------------------

data <- read_sav('Data/МСЖ_19_финальная.sav')

# Пивотирование ------------------------------------------------------------

data %>% 
  filter(USL1s2_r1 != 99) %>% 
  group_by(VV1, VV0) %>% 
  summarize(
    mean = mean(USL1s2_r1, na.rm = T), 
    sd = sd(USL1s2_r1, na.rm = T),
    n = n()
  ) %>% 
  pivot_longer(
    cols = c(mean, sd, n)
  ) %>% 
  mutate(
   # VV1 = as.factor(VV1) не срадботает как решение, использовать вместо этого to_factor()
    VV1 = to_factor(VV1)
  ) %>%
  pivot_wider(
    names_from = VV1,
    values_from = value
  ) 

# Работа с факторами ------------------------------------------------------

data %>%
  mutate(VV1 = to_factor(VV1)) %>%
  group_by(VV1) %>%
  summarize(n())

data %>% 
  mutate(
    across(c(VV1, VV0), ~ to_factor(.x)),
    # VV1 = case_when(VV1, 
    #                 VV1 == '4 курс специалитета' ~ '4 курс бакалавриата',
    #                 TRUE ~ VV1
    #                 )
    #case_when далеко не всегда успешно работает с факторами, поэтому рекомендуется использовать fct_recode()
    VV1 = fct_recode(VV1,
                     `4 курс бакалавриата` = '4 курс специалитета',
                     `4 курс бакалавриата` = '5 курс специалитета',
                     `4 курс бакалавриата` = '6 курс специалитета'),
    ) %>% 
  group_by(VV1) %>% 
  summarize(n())


# fct_collapse ------------------------------------------------------------

data %>% 
  mutate(
    across(c(VV1, VV0), ~ to_factor(.x)),
    VV1 = fct_collapse(VV1,
      `4 курс бакалавриата` = c('4 курс специалитета', '5 курс специалитета', '6 курс специалитета')
    )
  ) %>% 
  group_by(VV1) %>% 
  summarize(n())

# fct_other ---------------------------------------------------------------

data %>% 
  mutate(
    across(c(VV1, VV0), ~ to_factor(.x)),
    VV1 = fct_other(VV1,
                    keep = c('1 курс бакалавриата/специалитета', '2 курс бакалавриата/специалитета'),
                    other_level = 'Другое')
  ) %>% 
  group_by(VV1) %>% 
  summarize(n())

# fct_anon ----------------------------------------------------------------

data %>% 
  mutate(
    CommonStudent = to_factor(CommonStudent),
    CommonStudent_anon = fct_anon(CommonStudent)
  ) %>% 
  select(CommonStudent, CommonStudent_anon)

# Работа со строками ------------------------------------------------------

data %>% 
  mutate(across(c(VV1, VV0), ~to_factor(.))) %>% 
  mutate(
    VV_new = str_c(VV1, " ", VV0)
  ) %>% 
  select(VV_new)

# data %>% 
#   mutate(VV1 = to_factor(VV1)) %>% 
#   select(VV1)

data %>% 
  mutate(across(c(VV1, VV0), ~to_factor(.))) %>% 
  # filter(VV1 %in% c('1 курс бакалавриата/специалитета', и т.д.)) старый способ поиска
  filter(str_detect(VV1, 'бакалавриата'))

## str_replace 
data %>% 
  mutate(across(c(VV1, VV0), ~to_factor(.)),
         VV1 = str_replace(VV1, '/специалитета', '')) %>%
  group_by(VV1) %>% 
  summarize(n())

## str_remove
data %>% 
  mutate(across(c(VV1, VV0), ~to_factor(.)),
         VV1 = str_remove(VV1, '/специалитета')) %>%
  group_by(VV1) %>% 
  summarize(n())

## Использование функции в across (см. третье занятие)

### Один из стилей написания

data %>% 
  mutate(across(c(VV1, VV0), to_factor))

### Написание через list с тильдой

data %>% 
  summarize(
    across(c(USL1s2_r1, USL1s2_r2),
           list(
             ~ mean(.x, na.rm = T),
             ~ sd(.x, na.rm = T)
           )
           )
  )

### Написание через тильду по одной функции

data %>% 
  summarize(across(c(USL1s2_r1, USL1s2_r2), ~mean(.x, na.rm = T)))
