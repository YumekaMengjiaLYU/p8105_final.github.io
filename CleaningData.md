Cleaning\_Data
================
Alicia Dagle
November 7, 2019

# \<\<\<\<\<\<\< HEAD

``` r
#Maternal Mortality ratio from gapminder dataset
maternal_mort = read_csv("./data/Gapminder/maternal_mortality_ratio_per_100000_live_births.csv") %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    x1800:x2013,
    names_to = "year",
    values_to = "maternal_mortality"
  ) %>% 
  mutate(year = str_remove(year,"x"))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   country = col_character(),
    ##   `1981` = col_logical(),
    ##   `1982` = col_logical(),
    ##   `1983` = col_logical(),
    ##   `1984` = col_logical(),
    ##   `1985` = col_logical(),
    ##   `1986` = col_logical(),
    ##   `1987` = col_logical(),
    ##   `1988` = col_logical(),
    ##   `1989` = col_logical(),
    ##   `1991` = col_logical(),
    ##   `1992` = col_logical(),
    ##   `1993` = col_logical(),
    ##   `1994` = col_logical(),
    ##   `1996` = col_logical(),
    ##   `1997` = col_logical(),
    ##   `1998` = col_logical(),
    ##   `1999` = col_logical(),
    ##   `2001` = col_logical(),
    ##   `2002` = col_logical()
    ##   # ... with 8 more columns
    ## )

    ## See spec(...) for full column specifications.

``` r
view(maternal_mort)
```

``` r
#births attended by skilled health staff (percent of total)
staff = read_csv("./data/Gapminder/births_attended_by_skilled_health_staff_percent_of_total.csv")  %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    x1984:x2017,
    names_to = "year",
    values_to = "perc_births_attended"
  ) %>% 
  mutate(year = str_remove(year,"x"))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   country = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
#This code is incomplete, I was just exploring a bit:
  
mortality_births_attend = inner_join(maternal_mort, staff, by = c("country","year")) 

  
 view(mortality_births_attend) 
  

  
#This is the process I would follow for merging the datasets for gdp sepnding, healthcare spending, and individual cost  (except this is done for the births attended here)
  
  maternal_mort_leading = maternal_mort %>%
  filter(year=='2013') %>% 
  filter(country %in% c("United States", "United Kingdom", "Finland", "Canada","Germany", "New Zealand", "Norway", "China", "Netherlands", "Switzerland", "Singapore", "Japan", "Luxembourg", "Hong Kong", "Qatar"))
  
view(maternal_mort_leading)


leading_2013 = left_join(maternal_mort_leading,staff, by = c("country","year"))

view(leading_2013)
```

> > > > > > > 6c7379a38ae6121321548d882d52ac194f9c1eac
