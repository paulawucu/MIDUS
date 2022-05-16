investigations
================
Paula Wu
5/11/2022

``` r
m2_df = 
  read_csv("./data/m2_df.csv") %>% 
  select(-1)
```

### Invalid investigation

``` r
# Dependent variable
invalid_dep = 
  m2_df %>% 
  filter(B3TCOMPZ3 == 8 | B3TEMZ3 == 8 | B3TEFZ3 == 8) %>% 
  pull(M2ID)

tibble(
  B3TCOMPZ3 = as.numeric(count(m2_df[m2_df$B3TCOMPZ3 == 8,])),
  B3TEMZ3 = as.numeric(count(m2_df[m2_df$B3TEMZ3 == 8,])),
  B3TEFZ3 = as.numeric(count(m2_df[m2_df$B3TEFZ3 == 8,]))
) %>% 
  knitr::kable(caption = "Invalid counts - Dependent Variable")
```

| B3TCOMPZ3 | B3TEMZ3 | B3TEFZ3 |
|----------:|--------:|--------:|
|        40 |       4 |       0 |

Invalid counts - Dependent Variable

``` r
# CTQ scores
invalid_ctq = 
  m2_df %>% 
  filter(B4QCT_SA == 98 | B4QCT_EN == 98 | B4QCT_MD == 8 | B4QCT_PN == 98 | B4QCT_EA == 98 | B4QCT_PA == 98) %>% 
  pull(M2ID)

tibble(
  B4QCT_SA = as.numeric(count(m2_df[m2_df$B4QCT_SA == 98,])),
  B4QCT_EN = as.numeric(count(m2_df[m2_df$B4QCT_EN == 98,])),
  B4QCT_MD = as.numeric(count(m2_df[m2_df$B4QCT_MD == 98,])),
  B4QCT_PN = as.numeric(count(m2_df[m2_df$B4QCT_PN == 98,])),
  B4QCT_EA = as.numeric(count(m2_df[m2_df$B4QCT_EA == 98,])),
  B4QCT_PA = as.numeric(count(m2_df[m2_df$B4QCT_PA == 98,]))
)%>% 
  knitr::kable(caption = "Invalid counts - CTQ scores")
```

| B4QCT_SA | B4QCT_EN | B4QCT_MD | B4QCT_PN | B4QCT_EA | B4QCT_PA |
|---------:|---------:|---------:|---------:|---------:|---------:|
|        7 |        3 |        0 |        2 |        3 |        2 |

Invalid counts - CTQ scores

``` r
# Covariates
invalid_cov = 
  m2_df %>% 
  filter(B1PTSEI == 999 | B4HMETMW == 99998 | B1PB1 == 97 | B1PF7A >= 7) %>%
  pull(M2ID)

tibble(
  B1PTSEI = as.numeric(count(m2_df[m2_df$B1PTSEI == 999,])),
  B4HMETMW = as.numeric(count(m2_df[m2_df$B4HMETMW == 99998,])),
  B1PB1 = as.numeric(count(m2_df[m2_df$B1PB1 == 97,])),
  B1PF7A = as.numeric(count(m2_df[m2_df$B1PF7A >= 7,]))
)%>% 
  knitr::kable(caption = "Invalid counts - Covariates")
```

| B1PTSEI | B4HMETMW | B1PB1 | B1PF7A |
|--------:|---------:|------:|-------:|
|     352 |        5 |     3 |      2 |

Invalid counts - Covariates

``` r
# Resilience factor part 1
invalid_res_1 = 
  m2_df %>% 
  filter(B1SPWBA2 == 98 | B1SPWBE2 == 98 | B1SPWBG2 == 98 | B1SPWBR2 == 98 | B1SPWBU2 == 98 | B1SPWBS2 == 98) %>% 
  pull(M2ID)

tibble(
  B1SPWBA2 = as.numeric(count(m2_df[m2_df$B1SPWBA2 == 98,])),
  B1SPWBE2 = as.numeric(count(m2_df[m2_df$B1SPWBE2 == 98,])),
  B1SPWBG2 = as.numeric(count(m2_df[m2_df$B1SPWBG2 == 98,])),
  B1SPWBR2 = as.numeric(count(m2_df[m2_df$B1SPWBR2 == 98,])),
  B1SPWBU2 = as.numeric(count(m2_df[m2_df$B1SPWBU2 == 98,])),
  B1SPWBS2 = as.numeric(count(m2_df[m2_df$B1SPWBS2 == 98,])),
) %>% 
  knitr::kable(caption = "Invalid counts - Resilience Factor Part 1")
```

| B1SPWBA2 | B1SPWBE2 | B1SPWBG2 | B1SPWBR2 | B1SPWBU2 | B1SPWBS2 |
|---------:|---------:|---------:|---------:|---------:|---------:|
|        3 |        3 |        3 |        3 |        3 |        3 |

Invalid counts - Resilience Factor Part 1

``` r
# Resilience factor part 2
# B1SINTER, B1SINDEP both have two missing values (national and milwaukee samokes has different criteria??)
invalid_res_2 = 
  m2_df %>% 
  filter(B1SMASTE == 8 | B1SCONST == 8 | B1SCTRL == 8 | B1SESTEE == 98 | B1SINTER == 8 | B1SINTER == 98 |
         B1SINDEP == 8 | B1SINDEP == 98 | B1SAGENC == 8 | B1SAGREE == 8 | B1SEXTRA == 8 | B1SNEURO == 8 |
          B1SCONS1 == 8 ) %>% 
  pull(M2ID)

tibble(
  B1SMASTE = as.numeric(count(m2_df[m2_df$B1SMASTE == 8,])),
  B1SCONST = as.numeric(count(m2_df[m2_df$B1SCONST == 8,])),
  B1SCTRL = as.numeric(count(m2_df[m2_df$B1SCTRL == 8,])),
  B1SESTEE = as.numeric(count(m2_df[m2_df$B1SESTEE == 98,])),
  B1SINTER = as.numeric(count(m2_df[m2_df$B1SINTER == 8,])),
  BASINTER = as.numeric(count(m2_df[m2_df$B1SINTER == 98,])),
  B1SINDEP = as.numeric(count(m2_df[m2_df$B1SINDEP == 8,])),
  BASINDEP = as.numeric(count(m2_df[m2_df$B1SINDEP == 98,])),
  B1SAGENC = as.numeric(count(m2_df[m2_df$B1SAGENC == 8,])),
  B1SAGREE = as.numeric(count(m2_df[m2_df$B1SAGREE == 8,])),
  B1SEXTRA = as.numeric(count(m2_df[m2_df$B1SEXTRA == 8,])),
  B1SNEURO = as.numeric(count(m2_df[m2_df$B1SNEURO == 8,])),
  B1SCONS1 = as.numeric(count(m2_df[m2_df$B1SCONS1 == 8,]))
) %>% 
  knitr::kable(caption = "Invalid counts - Resilience Factor Part 2")
```

| B1SMASTE | B1SCONST | B1SCTRL | B1SESTEE | B1SINTER | BASINTER | B1SINDEP | BASINDEP | B1SAGENC | B1SAGREE | B1SEXTRA | B1SNEURO | B1SCONS1 |
|---------:|---------:|--------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|
|        5 |        5 |       5 |        7 |       15 |        2 |        6 |        2 |        6 |        5 |        5 |        5 |        5 |

Invalid counts - Resilience Factor Part 2

``` r
# total number of subjects who have at least one invalid entry
invalid_all = c(invalid_dep, invalid_ctq, invalid_cov, invalid_res_1, invalid_res_2)
```

``` r
# further investigation
table(invalid_all) %>% 
  as.tibble() %>% 
  arrange(desc(n))
```

    ## # A tibble: 400 × 2
    ##    invalid_all     n
    ##    <chr>       <int>
    ##  1 11763           3
    ##  2 12106           3
    ##  3 16500           3
    ##  4 10231           2
    ##  5 10476           2
    ##  6 10511           2
    ##  7 10524           2
    ##  8 10644           2
    ##  9 12738           2
    ## 10 13396           2
    ## # … with 390 more rows

## SES and Spouse SES Investigation

For now, I’m thinking about impute those missing numbers with their
spouse’s SES, if any. First, investigate: how many people have their
spouse’s SES filled (out of 352)

``` r
m2_df %>% 
  filter(B1PTSEI == 999) %>% 
  select(M2ID, M2FAMNUM, B1PTSEI, B1PTSEIS) %>% 
  mutate(nul = ifelse(B1PTSEIS == 999, 1, 0)) %>% 
  group_by(nul) %>% 
  summarize(n = n())
```

    ## # A tibble: 2 × 2
    ##     nul     n
    ##   <dbl> <int>
    ## 1     0   223
    ## 2     1   129

(<content:references:ses)=> ### imputation

Out of 352 missing, 129 of them still don’t have the spouse SES while
223 of them did. Maybe we can impute them using the median/mean of their
subgroups.

``` r
# if use the above-mentioned imputation
m2_df_tend = m2_df # new dataset to prevent data contamination

with_sps = 
  m2_df %>% 
  filter(B1PTSEI == 999) %>% 
  select(M2ID, M2FAMNUM, B1PTSEI, B1PTSEIS) %>% 
  filter(B1PTSEIS != 999) %>% 
  pull(M2ID)
m2_df_tend[m2_df_tend$M2ID %in% with_sps, which(colnames(m2_df_tend) == "B1PTSEI")] = m2_df_tend[m2_df_tend$M2ID %in% with_sps, which(colnames(m2_df_tend) == "B1PTSEIS")]

# without spouse people, impute with subgroups - sex, education, race
wo_sps = 
  m2_df %>% 
  filter(B1PTSEI == 999) %>% 
  select(M2ID, M2FAMNUM, B1PTSEI, B1PTSEIS) %>% 
  filter(B1PTSEIS == 999) %>% 
  pull(M2ID)
  
# function for imputation
impute = function(obs){
  sex = as.numeric(obs[9])
  edu = as.numeric(obs[11])
  race = as.numeric(obs[10])
  sub_mean = m2_df_tend %>% 
    filter(B1PRSEX == sex & B1PB1 == edu & B1PF7A == race & B1PTSEI != 999) %>% 
    pull(B1PTSEI) %>% 
    mean()
  return(sub_mean)
}

# begin row-by-row imputation
for (id in wo_sps){
  obs = m2_df_tend[m2_df_tend$M2ID == id, ]
  sub_mean = impute(obs)
  m2_df_tend[m2_df_tend$M2ID == id, which(colnames(m2_df_tend) == "B1PTSEI")] = sub_mean
}
```

# Modeling

right now I just completely filter out all the observations with at
least one invalid feature

``` r
m2_df_no_inval = 
  m2_df %>% 
  filter(!(M2ID %in% unique(invalid_all)))

# random intercept for each family
ctqs = c("B4QCT_EA", "B4QCT_EN", "B4QCT_MD", "B4QCT_PA", "B4QCT_PN", "B4QCT_SA")
for (ctq in ctqs){
  lme(B3TCOMPZ3 ~ parse(text = ctq)[[1]], random = ~1 | M2FAMNUM,  data = m2_df_no_inval, method='REML')
}


lmm1 = lme(B3TCOMPZ3 ~ B4QCT_MD, random = ~1 | M2FAMNUM,  data = m2_df_no_inval, method='REML') 
summary(lmm1)
```
