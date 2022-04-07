File I/O
================
Paula Wu
4/5/2022

This file is used to read in files and write files with selected

## Rough summary of missing Data

### M2P3

``` r
m2p3 = read_tsv("./data/ICPSR_25281/DS0001/25281-0001-Data.tsv")

missing = m2p3 %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_each(funs(sum(is.na(.))))

missing %>%
  pivot_longer(M2FAMNUM:B3TEFZ2,names_to = "variables", values_to = "missing_counts") %>% 
  mutate(labels = c("Family number", "BTACT Comp Z - National", "Episodic Memory Z - National", 
                    "Exe Func Z - National", "BTACT Comp Z - Milwaukee",
                    "Episodic Memory Z - Milwaukee", "Exe Func Z - Milwaukee"),
         missing_perc = paste0(round(missing_counts/4512, 4)*100, "%")) %>% 
  select(variables, labels, everything()) %>% 
  knitr::kable(col.names = c("Variable", "Labels", "Missing Counts", "Missing Percentage"), caption = "M2 Missing Data")
```

| Variable  | Labels                        | Missing Counts | Missing Percentage |
|:----------|:------------------------------|---------------:|:-------------------|
| M2FAMNUM  | Family number                 |            306 | 6.78%              |
| B3TCOMPZ1 | BTACT Comp Z - National       |            539 | 11.95%             |
| B3TEMZ1   | Episodic Memory Z - National  |            323 | 7.16%              |
| B3TEFZ1   | Exe Func Z - National         |            314 | 6.96%              |
| B3TCOMPZ2 | BTACT Comp Z - Milwaukee      |           4206 | 93.22%             |
| B3TEMZ2   | Episodic Memory Z - Milwaukee |           4206 | 93.22%             |
| B3TEFZ2   | Exe Func Z - Milwaukee        |           4206 | 93.22%             |

M2 Missing Data

### M3P3

``` r
m3p3 = read.table(file = "./data/ICPSR_37095/DS0001/37095-0001-Data.tsv", sep = '\t', header = TRUE)
missing_m3p3 = 
  m3p3 %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_each(funs(sum(is.na(.))))
missing_m3p3 %>% 
  pivot_longer(M2FAMNUM:C3TSPXBB, names_to = "var", values_to = "missing") %>% 
  mutate(labels = c("M2 Family Number", "Backward Counting: # Correct", "Mixed-task normal nonswitch trials %correct", "Mixed-task reverse nonswitch trials %correct", "Mixed-task nonswitch trials %correct", "Mixed-task normal switch trials %correct", "Mixed-task reverse switch trials %correct", "Mixed-task switch trials %correct", "All mixed-task %correct"),
         miss_perc = paste0(round(missing/3291, 4)*100,"%")) %>% 
  select(var, labels, everything()) %>% 
  knitr::kable(col.names = c("Variable", "Labels", "Missing Counts", "Missing Percentage"), caption = "M3 Missing Data")
```

| Variable | Labels                                       | Missing Counts | Missing Percentage |
|:---------|:---------------------------------------------|---------------:|:-------------------|
| M2FAMNUM | M2 Family Number                             |            330 | 10.03%             |
| C3TBKTOT | Backward Counting: # Correct                 |             32 | 0.97%              |
| C3TSPXNO | Mixed-task normal nonswitch trials %correct  |             15 | 0.46%              |
| C3TSPXRO | Mixed-task reverse nonswitch trials %correct |             15 | 0.46%              |
| C3TSPXBO | Mixed-task nonswitch trials %correct         |             15 | 0.46%              |
| C3TSPXNS | Mixed-task normal switch trials %correct     |             15 | 0.46%              |
| C3TSPXRS | Mixed-task reverse switch trials %correct    |             16 | 0.49%              |
| C3TSPXBS | Mixed-task switch trials %correct            |             15 | 0.46%              |
| C3TSPXBB | All mixed-task %correct                      |             15 | 0.46%              |

M3 Missing Data

## Detailed examination

### M2P3

Which subject? missing what. how many NA does each subject has?

``` r
# Milwaukee sample first
#milwaukee_only = c("B3TCOMPZ2", "B3TEMZ2", "B3TEFZ2")
milwaukee_only = 
  m2p3 %>% 
  select(M2ID, B3TCOMPZ2, B3TEMZ2, B3TEFZ2) %>% 
  filter(!(is.na(B3TCOMPZ2) | is.na(B3TEMZ2) | is.na(B3TEFZ2)))
```

Milwaukee sample is, surprisingly, complete. at least in the variables
for Milwaukees only. What about other variables?

``` r
m2p3 %>% 
  filter(M2ID %in% milwaukee_only$M2ID) %>% 
  select(-c(B3TCOMPZ1, B3TEMZ1, B3TEFZ1)) %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_each(funs(sum(is.na(.))))
```

    ## # A tibble: 1 × 1
    ##   M2FAMNUM
    ##      <int>
    ## 1      306

For Milwaukee sample only, the only missing variable is “M2FAMNUM”, i.e,
MIDUS 2 Family Numbers. And it seems that this variable is not collected
for the Milwaukee sample at all.

Milwaukee samples have their own ID

``` r
# national only
#national_only = c("B3TCOMPZ1", "B3TEMZ1", "B3TEFZ1")
national_only = m2p3 %>% 
  filter(!(M2ID %in% milwaukee_only$M2ID)) %>% 
  select(M2ID, B3TCOMPZ2, B3TEMZ2, B3TEFZ2) 

m2p3 %>% 
  select(M2ID, B3TCOMPZ1, B3TEMZ1, B3TEFZ1, B3TCOMPZ2, B3TEMZ2, B3TEFZ2) %>% 
  mutate(national = ifelse(is.na(B3TCOMPZ2) | is.na(B3TEMZ2) | is.na(B3TEFZ2), 1,0),
         milwaukee = ifelse(is.na(B3TCOMPZ1) | is.na(B3TEMZ1) | is.na(B3TEFZ1), 1,0)) %>% 
  filter(national==1 & milwaukee == 1)
```

``` r
m2p3$NASUM = rowSums(is.na(m2p3))
# national sample
m2p3 %>%
  filter(!(M2ID %in% milwaukee_only$M2ID)) %>% 
  mutate(NASUM = as.integer(NASUM)) %>% 
  select(M2ID, NASUM) %>% 
  ggplot(aes(x = NASUM))+
  geom_histogram(bins = 3)+
  labs(x = "Number of NA", y = "Number of Subjects (Counts)")+
  ggtitle("Distribution of NA - M2P3 (no Milwaukee)")
```

![](reading_files_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# M3P3

``` r
m3p3$NASUM = rowSums(is.na(m3p3))
m3p3 %>% 
  select(M2ID, NASUM) %>% 
  ggplot(aes(x = as.integer(NASUM)))+
  geom_histogram(bins = 10)+
  labs(x = "Number of NAs", y = "Number of Subjects (Counts)")+
  ggtitle("Distribution of NA - M3P3")
```

![](reading_files_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Time gap between M2 and M3

``` r
m2_res_time =
  m2p3 %>% 
  select(M2ID, B1PAGE_M2, B3PIDATE_MO, B3PIDATE_YR)
```

``` r
m3_res_time = 
  m3p3 %>% 
  select(M2ID, C1PRAGE, C3IDATE_MO, C3IDATE_YR)
```

<details>
<summary>
Click to expand the data wrangling part
</summary>

``` r
m2_m3_time_lapse_raw = 
  left_join(m2_res_time, m3_res_time, by = "M2ID") 

m2_m3_time_lapse = 
  m2_m3_time_lapse_raw %>% 
  filter(!(is.na(C1PRAGE)|is.na(C3IDATE_MO)|is.na(C3IDATE_YR))) %>% 
  mutate(m2_date = ifelse(B3PIDATE_MO<10, paste0(B3PIDATE_YR, "0",B3PIDATE_MO), paste0(B3PIDATE_YR, B3PIDATE_MO)),
         m3_date = ifelse(C3IDATE_MO<10, paste0(C3IDATE_YR, "0",C3IDATE_MO), paste0(C3IDATE_YR, C3IDATE_MO)),
         intv_mo = interval(ym(m2_date), ym(m3_date))%/% months(1),
         intv_yr = round(interval(ym(m2_date), ym(m3_date))/ years(1),2),
         age_change = C1PRAGE - B1PAGE_M2) %>% 
  select(M2ID, m2_date, m3_date, intv_mo, intv_yr, age_change)

a = m2_m3_time_lapse_raw %>%  
  filter(is.na(C1PRAGE)|is.na(C3IDATE_MO)|is.na(C3IDATE_YR)) %>% 
  mutate(m2_date = ifelse(B3PIDATE_MO<10, paste0(B3PIDATE_YR, "0",B3PIDATE_MO), paste0(B3PIDATE_YR, B3PIDATE_MO)),
         m3_date = NA,
         intv_mo = NA,
         intv_yr = NA,
         age_change = NA) %>% 
  select(M2ID, m2_date, m3_date, intv_mo, intv_yr, age_change)
m2_m3_time_lapse = 
  rbind(m2_m3_time_lapse, a) %>% 
  arrange(M2ID)
```

</details>

Summary Statistics

``` r
skimr::skim(m2_m3_time_lapse[,c("M2ID", "intv_mo", "intv_yr", "age_change")])
```

|                                                  |                             |
|:-------------------------------------------------|:----------------------------|
| Name                                             | m2_m3_time_lapse\[, c(“M2I… |
| Number of rows                                   | 4512                        |
| Number of columns                                | 4                           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                             |
| Column type frequency:                           |                             |
| numeric                                          | 4                           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                             |
| Group variables                                  | None                        |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |     mean |      sd |      p0 |      p25 |      p50 |      p75 |     p100 | hist  |
|:--------------|----------:|--------------:|---------:|--------:|--------:|---------:|---------:|---------:|---------:|:------|
| M2ID          |         0 |          1.00 | 14609.92 | 2655.35 | 10002.0 | 12321.75 | 14616.00 | 16885.75 | 19193.00 | ▇▇▇▇▇ |
| intv_mo       |      1572 |          0.65 |   114.47 |   12.88 |    90.0 |   107.00 |   111.00 |   116.00 |   163.00 | ▂▇▁▁▁ |
| intv_yr       |      1572 |          0.65 |     9.54 |    1.07 |     7.5 |     8.92 |     9.25 |     9.67 |    13.59 | ▂▇▁▁▁ |
| age_change    |      1572 |          0.65 |     9.56 |    1.30 |    -1.0 |     9.00 |     9.00 |    10.00 |    17.00 | ▁▁▇▃▁ |

Graphs

``` r
a %>% 
  ggplot(aes(x = m2_date))+
  geom_bar() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust= 1))+
  ggtitle("Missing Subject M2 Date distribution")
```

![](reading_files_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
den_plot = 
  m2_m3_time_lapse %>% 
  ggplot(aes(x = intv_yr))+
  geom_density()+
  ggtitle("Distirbution of Years Overlapped")
```

``` r
box_plot = 
  m2_m3_time_lapse %>% 
  ggplot(aes(x = intv_yr))+
  geom_boxplot()
den_plot/box_plot
```

![](reading_files_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Wide distribution with a large amount of outliers, person time may be a
better choice.

Question: invalid meaning, how to distinguish?