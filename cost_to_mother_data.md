cost\_to\_mother\_data
================
Alicia Dagle
November 23, 2019

``` r
library(tidyverse)
library(rvest)
library(httr)
```

``` r
url = "https://www.businessinsider.com/how-much-does-it-cost-to-have-a-baby-2018-4#35-maine-16"

birth_cost_us = read_html(url)

#pull datasets, clean after
state_vec =
  birth_cost_us %>% 
  html_nodes(".slide-title-text") %>% 
  html_text()

all_costs = 
  birth_cost_us %>% 
  html_nodes(".clearfix p+ p") %>% 
  html_text()

view(state_vec)
view(all_costs)
```

``` r
#clean state names
state_df = tibble(
  state = state_vec %>% 
    str_replace(".\\. ","") %>%
    str_replace("[1-9]",""),
  "Vaginal_birth" = 1,
  "C-section" = 0,
  "with_ins" = 2,
  "without_ins" = 3
) %>% 
  pivot_longer(
    "Vaginal_birth": "C-section",
    names_to = "type",
    values_to = "dummy"
  ) %>% 
  pivot_longer(
    "with_ins": "without_ins",
    names_to = "insurance",
    values_to = "dummy2"
  ) %>% 
  select(state, type, insurance, -dummy, -dummy2)

view(state_df)
```

Time to clean the cost dataset...this is gonna be fun...

``` r
#pivot longer so all costs are in one column


cost_df = tibble(
  all_costs
) %>% 
  janitor::clean_names()

cost_df_clean = cost_df %>% 
    separate(all_costs, into = c("with_ins", "other", "cost_w"), sep = "\\:") %>% 
    separate(other, into = c("without_ins", "cost_wo"), sep = "C-section without insurance" ) %>% 
    separate(without_ins, into = c("without_ins", "cost_wo"), sep = "Vaginal birth without insurance")%>%
mutate(
  type = factor(with_ins), 
  type = recode(type,
    "Vaginal birth with insurance" = "Vaginal_birth",
    "C-section with insurance" = "C-section")
) %>% 
  select(-with_ins, -cost_wo) %>% 
  rename("with_ins" = "cost_w") %>% 
  select(type,with_ins,without_ins) %>% 
  pivot_longer(
    with_ins: without_ins,
    names_to = "insurance",
    values_to = "cost"
  )
```

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 50 rows
    ## [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37,
    ## 39, ...].

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 50 rows
    ## [2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38,
    ## 40, ...].

``` r
view(cost_df_clean)
```

Unfortunately, web scraping gets a bit messy. We had to assume the order of the entries was preserved, which was verified for the first few entries. This is the final clean dataset which will be saved as an excel file for other team mates to use more easily.

``` r
# merge the state information with the cost information
# write to csv

US_cost_to_mother_df = 
  cost_df_clean %>% 
  mutate(state = state_df$state) %>% 
  select(state,type, insurance, cost)

view(US_cost_to_mother_df)

write.csv(US_cost_to_mother_df, file = "US_cost_to_mother_df.csv")
```
