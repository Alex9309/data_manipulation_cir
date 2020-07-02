sapply(data, function(x)
  attributes(x)[['label']]
  ) %>% 
  enframe() %>% 
  View()

data %>% 
  select(id, Q1s1, Q1s2, Q1s3, Q2s1, starts_with("Q2s1v4")) %>% 
  mutate(
    id = factor(id),
    id = forcats::fct_anon(id)
  ) %>% 
  write_csv("lesson_2.csv")
