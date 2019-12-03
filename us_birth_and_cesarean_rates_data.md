us\_birth\_and\_cesarean\_rates\_data
================
Alicia Dagle
December 1, 2019

<<<<<<< HEAD
## Data Source:

All data for the birth rates and cesearan rates in the US was pulled
from the CDC. By default, the data from 2018 is pulled but we can change
this manually if we want to look at a different year or multiple years.
This natality data was provided by the Center for Disease Control and
Prevention and can be found at [CDC
WONDER](https://wonder.cdc.gov/natality.html).

\#Delivery method per state

The births were stored as factors for some reason so the numbering had
many errors when they were converted directly to numeric. To solve this,
I first had to convert the factors to characters and then to numeric
variable types. Then, I calculated the total number of births per state
by using group and summarize. Finally, I joined these two datasets using
a left\_join by state of residence. Then, I was used the number of
cesarean births divided by the total number of births per state to
produce a cesarean rate per state.
=======
Data Source:
------------

All data for the birth rates and cesearan rates in the US was pulled from the CDC. By default, the data from 2018 is pulled but we can change this manually if we want to look at a different year or multiple years. This natality data was provided by the Center for Disease Control and Prevention and can be found at [CDC WONDER](https://wonder.cdc.gov/natality.html).

Delivery method per state
=========================

The births were stored as factors for some reason so the numbering had many errors when they were converted directly to numeric. To solve this, I first had to convert the factors to characters and then to numeric variable types. Then, I calculated the total number of births per state by using group and summarize. Finally, I joined these two datasets using a left\_join by state of residence. Then, I was used the number of cesarean births divided by the total number of births per state to produce a cesarean rate per state.
>>>>>>> ec1897e96e196e0305f2fbc2168653ebd43aa9b4

``` r
#load number of births per delivery method per state
birth_type =
  read.delim("./dataset_births_us/birth_delivery_by_state.txt") %>% 
  janitor::clean_names() %>% 
  filter(delivery_method != "Unknown or Not Stated") %>% 
  na.omit() %>% 
  mutate(births = as.numeric(as.character(births))) %>% 
  select(state_of_residence, delivery_method, births) 


#calculate total number births per state
birth_sum_by_state =
  birth_type %>%
  group_by(state_of_residence) %>% 
  summarize(
    total_births = sum(births)
    ) %>% 
  ungroup()

#calculate percent of c-sections per state by 1st merging and then creating a new variable
birth_df =
  left_join(birth_type, birth_sum_by_state, by = "state_of_residence") %>% 
  filter(delivery_method == "Cesarean") %>%
  select(-delivery_method) %>% 
  mutate(c_rate  = births/total_births) %>% 
  select(state_of_residence, total_births, c_rate)
  

view(birth_df)
```

<<<<<<< HEAD
## Birth rates by state

Please note that this shows the dataset when I only pulled the birth
numbers per state.
=======
Birth rates by state
--------------------

Please note that this shows the dataset when I only pulled the birth numbers per state.
>>>>>>> ec1897e96e196e0305f2fbc2168653ebd43aa9b4

``` r
births_by_state =
  read.delim("./dataset_births_us/Natality_2016-2018_expanded.txt") %>% 
  janitor::clean_names() %>% 
  select(state_of_residence,births,total_population)

view(births_by_state)
```

<<<<<<< HEAD
These numbers are off ever so slightly from the data used to calculate
cesarean rates. For example the births in alabama differ by 3 people.
This difference may be attributed to the data which was supressed
because the delivery method of the birth was not known.

A decision must be made for which total to use for the mapping of our
project.

## Data Cleaning Summary

Here we have 3 cleaned variables: `state_of_residence`, `total_births`
and `c_rate` (cesarean rate defined by the percent of births used a
cesarean delivery method.)

\#Write data to
csv
=======
These numbers are off ever so slightly from the data used to calculate cesarean rates. For example the births in alabama differ by 3 people. This difference may be attributed to the data which was supressed because the delivery method of the birth was not known.

A decision must be made for which total to use for the mapping of our project.

Data Cleaning Summary
---------------------

Here we have 3 cleaned variables: `state_of_residence`, `total_births` and `c_rate` (cesarean rate defined by the percent of births used a cesarean delivery method.)

Write data to csv
=================
>>>>>>> ec1897e96e196e0305f2fbc2168653ebd43aa9b4

``` r
write.csv(birth_df, "./dataset_births_us/US_cesarean_rates.csv", row.names = FALSE)


write.csv(births_by_state, "./dataset_births_us/US_number_births_per_state.csv", row.names = FALSE)
```
