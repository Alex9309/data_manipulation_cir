rm(list = ls())

# Packages ----------------------------------------------------------------

library(tidyverse)
library(labelled)
library(haven)

# Data --------------------------------------------------------------------

data <- read_sav('Data/МСЖ_19_финальная.sav')

# Slice -------------------------------------------------------------------

data %>% 
  slice(
    -((nrow(.)-2):(nrow(.)))
    )


data %>% 
  slice_head(n = 5)

data %>% 
  slice_tail(n = 5)

?slice_sample()

# Arrange -----------------------------------------------------------------

data %>% 
  arrange(desc(USL1s1_r1)) %>% 
  select(USL1s1_r1)

data %>% 
  group_by(VV1) %>% 
  arrange(desc(USL1s1_r1), .by_group = TRUE)

# Mutate ------------------------------------------------------------------

look_for(data, details = T) %>% 
  View()

?look_for()


data %>% 
  mutate(
    USL1s1_r_all = USL1s1_r1 + USL1s1_r2 + USL1s1_r3 
  ) %>% 
  summarize(
    mean = mean(USL1s1_r_all, na.rm = T),
    sd = sd(USL1s1_r_all, na.rm = T)
  )

data %>% 
  mutate(
    USL1s2_r1 = na_if(USL1s2_r1, 99),
    USL1s2_r2 = na_if(USL1s2_r2, 99),
    USL1s2_r3 = na_if(USL1s2_r3, 99),
    USL1s2_r4 = na_if(USL1s2_r4, 99),
    USL1s2_r_all = USL1s2_r1 + USL1s2_r2 + USL1s2_r3 + USL1s2_r4
  ) %>% 
  summarize(
    mean = mean(USL1s2_all, na.rm = T),
    sd = sd(USL1s2_all, na.rm = T)
  )

data %>% 
  mutate(
    USL1s2_r1 = case_when(
      USL1s2_r1 %in% c(4, 5) ~ 'checked',
      USL1s2_r1 %in% c(1:3) ~ 'unchecked',
      TRUE ~ NA_character_
    )
  )

# Across ------------------------------------------------------------------

## Summarize с Across

## Написание функции без скобок

data %>% 
  summarize(
    across(
      starts_with("USL1s2_r"),
      mean, na.rm = TRUE
    )
  )

## Написание через purr

data %>% 
  summarize(
    across(
      starts_with("USL1s2_r"),
      ~ mean(.x, na.rm = T)
    )
  )

## Написание через list

data %>% 
  summarize(
    across(
      starts_with('USL1s2_r'),
      list(
        ~ mean(.x, na.rm = T)
      )
    )
  )

## Написание через list, несколько операций

data %>% 
  summarize(
    across(
      starts_with('USL1s2_r'),
      list(
        ~ mean(.x, na.rm = T),
        ~ sd(.x, na.rm = T),
        ~ n()
      )
    )
  )

## Mutate с Across

data %>% 
  mutate(
    across(
      starts_with("USL1s2_r"),
      ~ na_if(.x, 99)
    )
  ) -> data

data %>% 
  summarize(
    across(
      starts_with('USL1s2_r'),
      list(
        ~ mean(.x, na.rm = T),
        ~ sd(.x, na.rm = T),
        ~ n()
      )
    )
  )

## Использование mutate c across в случае,
## когда нужно перекодировать пропущенные значения в ноль

data %>% 
  mutate(
    across(
      starts_with("USL1s2_r"),
      ~ case_when(
        .x == 99 ~ 0,
        is.na(.x) ~ 0,
        TRUE ~ as.numeric(.x)
      )
    )
  ) %>% 
  select(starts_with("USL1s2_r"))


data %>% 
  mutate(
    across(
      everything(),
      ~ case_when(
        .x == 'checked' ~ '1',
        .x == 'unchecked' ~ '0',
        TRUE ~ as.character(.x)
      )
    )
  )


# JOINS  ------------------------------------------------------------------

data %>% 
  mutate(
    USL1s2_r1 = na_if(USL1s2_r1, 99),
    USL1s2_r2 = na_if(USL1s2_r2, 99),
    USL1s2_r3 = na_if(USL1s2_r3, 99),
    USL1s2_r4 = na_if(USL1s2_r4, 99),
    USL1s2_all = USL1s2_r1 + USL1s2_r2 + USL1s2_r3 + USL1s2_r4
  ) %>% 
  select(
    id, CommonStudent, USL1s2_all
  ) -> data_with_summed_USL1s2

data_with_summed_USL1s2 %>% 
  bind_rows(
    data_with_summed_USL1s2 %>% 
      slice(1:3)
  ) -> data_with_summed_USL1s2

data %>% 
  right_join(data_with_summed_USL1s2)




data %>% 
  mutate(
    USL1s2_r1 = na_if(USL1s2_r1, 99),
    USL1s2_r2 = na_if(USL1s2_r2, 99),
    USL1s2_r3 = na_if(USL1s2_r3, 99),
    USL1s2_r4 = na_if(USL1s2_r4, 99),
    USL1s2_all = USL1s2_r1 + USL1s2_r2 + USL1s2_r3 + USL1s2_r4
  ) %>% 
  select(
    NEW_ID = id, 
    NEW_COM = CommonStudent,
    USL1s2_all
  ) -> data_with_summed_USL1s2_new_names

data %>% 
  left_join(data_with_summed_USL1s2_new_names,
            by = c('CommonStudent' = 'NEW_COM')
            )

data %>% 
  left_join(
    data_with_summed_USL1s2_new_names,
    by = c('id' = 'NEW_ID', 'CommonStudent' = 'NEW_COM')
  ) %>% 
  colnames()

?left_join()

# Аргументы с точкой ------------------------------------------------------

.data = c(1, 2, 3) # создается отдельно, но в окружении ее нет
.data = data

func_1 <- function(.data) {
  .data %>% 
    group_by(VV1) %>% 
    summarize(
      mean(USL1s1_r1, na.rm = T)
      ) %>% 
    return()
}

func_1(data)
