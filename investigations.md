investigations
================
Paula Wu
5/11/2022

**7.7/7.14 Updates** (all updates are located at the bottom of this
document):

1.  Moderator effects of self-administered drugs are investigated and
    reported.

2.  I also explored another method called “multivariate multilevel
    regression”, which takes all three independent variables as the
    outcome variables while fitting other dependent variables as either
    predictors or covariates (details see below).

``` r
m2_df = 
  read_csv("./data/m2_df.csv") %>% 
  select(-1) %>% 
  filter(B3TCOMPZ3 != 8 & B3TEMZ3 != 8 & B3TEFZ3 != 8) %>% 
  filter(B4QCT_SA != 98 & B4QCT_EN != 98 & B4QCT_MD != 8 & B4QCT_PN != 98 & B4QCT_EA != 98 & B4QCT_PA != 98) %>% 
  select(-c(37:47))
```

## SES and Spouse SES Investigation

For now, I’m thinking about impute those missing numbers with their
spouse’s SES, if any. First, investigate: how many people have their
spouse’s SES filled (out of 334)

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
    ## 1     0   209
    ## 2     1   125

Out of 334 missing, 125 of them still don’t have the spouse SES while
209 of them did.

### imputation - Jun 2nd (final) version:

Impute spouses’ SES for those who doesn’t have SES; for the rest use LM
(education as predictor) for imputation

``` r
# for those whose spouse has a valid SES
with_sps = 
  m2_df %>% 
  filter(B1PTSEI == 999) %>% 
  select(M2ID, M2FAMNUM, B1PTSEI, B1PTSEIS) %>% 
  filter(B1PTSEIS != 999) %>% 
  pull(M2ID)
m2_df[m2_df$M2ID %in% with_sps, which(colnames(m2_df) == "B1PTSEI")] = m2_df[m2_df$M2ID %in% with_sps, which(colnames(m2_df) == "B1PTSEIS")]

# for those whose spouse doesn't have a valid SES, fit an LM 
m2_df_lm = m2_df %>% 
  select(B1PTSEI, B1PAGE_M2, B1PB1) %>% 
  filter(B1PTSEI != 999 & B1PB1 != 97)
lm_ses = lm(B1PTSEI ~ B1PB1, data = m2_df_lm)


# prediction
m2_pred_id = m2_df %>% 
  filter(B1PTSEI == 999) %>% 
  pull(M2ID)

for (i in m2_pred_id){
  edu = m2_df[m2_df$M2ID == i, which(colnames(m2_df) == "B1PB1")]
  pred_ses = predict(lm_ses, newdata = edu)
  m2_df[m2_df$M2ID == i, which(colnames(m2_df) == "B1PTSEI")] = pred_ses
}
```

``` r
m2_df_copy = m2_df
m2_df_invalid = 
  m2_df_copy %>% 
  mutate(B3TCOMPZ3_ = ifelse(B3TCOMPZ3 == 8, 1, 0),
         B3TEMZ3_ = ifelse(B3TEMZ3 == 8, 1, 0),
         B3TEFZ3_ = ifelse(B3TEFZ3 == 8, 1, 0),
         B4QCT_SA_ = ifelse(B4QCT_SA == 98, 1, 0),
         B4QCT_EN_ = ifelse(B4QCT_EN == 98, 1, 0),
         B4QCT_MD_ = ifelse(B4QCT_MD == 8, 1, 0),
         B4QCT_PN_ = ifelse(B4QCT_PN == 98, 1, 0),
         B4QCT_EA_ = ifelse(B4QCT_EA == 98, 1, 0),
         B4QCT_PA_ = ifelse(B4QCT_PA == 98, 1, 0),
         B1PTSEI_ = ifelse(B1PTSEI == 999, 1, 0),
         B4HMETMW_ = ifelse(B4HMETMW == 99998, 1, 0),
         B1PB1_ = ifelse(B1PB1 == 97, 1, 0),
         B1PF7A_ = ifelse(B1PF7A >= 7, 1, 0),
         B1SA62A_ = ifelse(B1SA62A == 8, 1, 0),
         B1SA62B_ = ifelse(B1SA62B == 8, 1, 0),
         B1SA62C_ = ifelse(B1SA62C == 8, 1, 0),
         B1SA62D_ = ifelse(B1SA62D == 8, 1, 0),
         B1SA62E_ = ifelse(B1SA62E == 8, 1, 0),
         B1SA62F_ = ifelse(B1SA62F == 8, 1, 0),
         B1SA62G_ = ifelse(B1SA62G == 8, 1, 0),
         B1SA62H_ = ifelse(B1SA62H == 8, 1, 0),
         B1SA62I_ = ifelse(B1SA62I == 8, 1, 0),
         B1SA62J_ = ifelse(B1SA62J == 8, 1, 0),
         B1SPWBA2_ = ifelse(B1SPWBA2 == 98, 1, 0),
         B1SPWBE2_ = ifelse(B1SPWBE2 == 98, 1, 0),
         B1SPWBG2_ = ifelse(B1SPWBG2 == 98, 1, 0),
         B1SPWBR2_ = ifelse(B1SPWBR2 == 98, 1, 0),
         B1SPWBU2_ = ifelse(B1SPWBU2 == 98, 1, 0),
         B1SPWBS2_ = ifelse(B1SPWBS2 == 98, 1, 0),
         B1SMASTE_ = ifelse(B1SMASTE == 8, 1, 0),
         B1SCONST_ = ifelse(B1SCONST == 8, 1, 0),
         B1SCTRL_ = ifelse(B1SCTRL == 8, 1, 0),
         B1SESTEE_ = ifelse(B1SESTEE == 98, 1, 0),
         B1SINTER_ = ifelse(B1SINTER == 8, 1, 0),
         BASINTER_ = ifelse(B1SINTER == 98, 1, 0),
         B1SINDEP_ = ifelse(B1SINDEP == 8, 1, 0),
         BASINDEP_ = ifelse(B1SINDEP == 98, 1, 0),
         B1SAGENC_ = ifelse(B1SAGENC == 8, 1, 0),
         B1SAGREE_ = ifelse(B1SAGREE == 8, 1, 0),
         B1SEXTRA_ = ifelse(B1SEXTRA == 8, 1, 0),
         B1SNEURO_ = ifelse(B1SNEURO == 8, 1, 0),
         B1SCONS1_ = ifelse(B1SCONS1 == 8, 1, 0),
         invalid_ind = NA) %>% 
  select(-c(2:60)) %>% 
  select(M2ID, invalid_ind, everything()) %>% 
  rename_at(.vars = vars(ends_with("_")),
            .funs = funs(sub("_$", "", .)))


for (i in 1:1099){
  obs = m2_df_invalid[i,]
  invalid_str = invalid_var(obs)
  m2_df_invalid[i,2] = invalid_str
}
invalid_full = 
  m2_df_invalid %>% 
  select(M2ID, invalid_ind) %>% 
  filter(!invalid_ind == "") %>% 
  mutate(invalid_count = str_count(invalid_ind, pattern = ","))

invalid_full %>% 
  group_by(invalid_count) %>% 
  summarize(n = n()) %>% 
  knitr::kable()

invalid_full %>% 
  ggplot(aes(x = invalid_count))+
  geom_histogram() +
  stat_bin(aes(y=..count.., label=ifelse(..count..== 0,"",..count..)), geom="text", vjust= -0.5)
```

``` r
a = m2_df %>% 
  filter(!(B4QCT_EA == 98 | B4QCT_EN == 98 | B4QCT_SA == 98 | B4QCT_PA == 98 | B4QCT_PN == 98 | B4QCT_MD == 8 )) %>% 
  mutate(ctq_total = B4QCT_EA + B4QCT_EN + B4QCT_SA + B4QCT_PA + B4QCT_PN + B4QCT_MD)

mean_ctq = a %>% 
  pull(ctq_total) %>% 
  mean() %>% 
  signif(6)

median_ctq = a %>% 
  pull(ctq_total) %>% 
  median() %>% 
  signif(6)

a %>% 
  ggplot(aes(x = ctq_total)) +
  geom_density()+
  geom_vline(xintercept=mean_ctq, size=0.5, color="red")+
  geom_text(aes(x=mean_ctq + 5, label=paste0("Mean\n",mean_ctq), y=0.03), color = "red")+
  geom_vline(xintercept=median_ctq, size=0.5, color="blue")+
  geom_text(aes(x=median_ctq - 3, label=paste0("Median\n",median_ctq), y=0.03), color = "blue")+
  theme(legend.position = "none")+
  xlab("CTQ Total Score")+
  ylab("Density")

a %>% 
  mutate(below_49 = ifelse(ctq_total <= 39, 1, 0)) %>% 
  group_by(below_49) %>% 
  summarize(n = n())
#ggsave("ctq_density.jpeg", width = 10, height = 7)
```

## Invalid investigation, after imputation

``` r
m2_df_cc = m2_df %>% 
  mutate(B1PTSEI_ = ifelse(B1PTSEI == 999, 1, 0),
         B4HMETMW_ = ifelse(B4HMETMW == 99998, 1, 0),
         B1PB1_ = ifelse(B1PB1 == 97, 1, 0),
         B1PF7A_ = ifelse(B1PF7A >= 7, 1, 0),
         B1SA62A_ = ifelse(B1SA62A == 8, 1, 0),
         B1SA62B_ = ifelse(B1SA62B == 8, 1, 0),
         B1SA62C_ = ifelse(B1SA62C == 8, 1, 0),
         B1SA62D_ = ifelse(B1SA62D == 8, 1, 0),
         B1SA62E_ = ifelse(B1SA62E == 8, 1, 0),
         B1SA62F_ = ifelse(B1SA62F == 8, 1, 0),
         B1SA62G_ = ifelse(B1SA62G == 8, 1, 0),
         B1SA62H_ = ifelse(B1SA62H == 8, 1, 0),
         B1SA62I_ = ifelse(B1SA62I == 8, 1, 0),
         B1SA62J_ = ifelse(B1SA62J == 8, 1, 0),
         B1SPWBA2_ = ifelse(B1SPWBA2 == 98, 1, 0),
         B1SPWBE2_ = ifelse(B1SPWBE2 == 98, 1, 0),
         B1SPWBG2_ = ifelse(B1SPWBG2 == 98, 1, 0),
         B1SPWBR2_ = ifelse(B1SPWBR2 == 98, 1, 0),
         B1SPWBU2_ = ifelse(B1SPWBU2 == 98, 1, 0),
         B1SPWBS2_ = ifelse(B1SPWBS2 == 98, 1, 0),
         invalid_ind = NA) %>% 
  select(-c(2:49)) %>% 
  select(M2ID, invalid_ind, everything()) %>% 
  rename_at(.vars = vars(ends_with("_")),
            .funs = funs(sub("_$", "", .)))

invalid_var = function(obs){
  invalid_str = ""
  for (i in 3:22){
    cell = as.numeric(obs[i])
    if(cell == 1){
      invalid_str = paste0(invalid_str, colnames(obs)[i], sep = ", ")
    }
  }
  return(invalid_str)
}
for (i in 1:1099){
  obs = m2_df_cc[i,]
  invalid_str = invalid_var(obs)
  m2_df_cc[i,2] = invalid_str
}
invalid_cc = 
  m2_df_cc %>% 
  select(M2ID, invalid_ind) %>% 
  filter(!invalid_ind == "") %>% 
  mutate(invalid_count = str_count(invalid_ind, pattern = ","))

# quick summary: invalid entries of each variable
colSums(m2_df_cc[3:22])
```

    ##  B1PTSEI B4HMETMW    B1PB1   B1PF7A  B1SA62A  B1SA62B  B1SA62C  B1SA62D 
    ##        0        5        3        2        3        3        4        3 
    ##  B1SA62E  B1SA62F  B1SA62G  B1SA62H  B1SA62I  B1SA62J B1SPWBA2 B1SPWBE2 
    ##        4        6        4        3        3        4        3        3 
    ## B1SPWBG2 B1SPWBR2 B1SPWBU2 B1SPWBS2 
    ##        3        3        3        3

``` r
# quick summary: count of people for different numbers of invalid entries. Total: 40 subjects
invalid_cc %>%
  group_by(invalid_count) %>% 
  summarize(n = n()) %>% 
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total")) %>% 
  mutate(invalid_count = ifelse(row_number() == 6, "Total", invalid_count)) %>% 
  knitr::kable()
```

| invalid_count |   n |
|:--------------|----:|
| 1             |  16 |
| 2             |   1 |
| 6             |   3 |
| 9             |   1 |
| 10            |   2 |
| Total         |  23 |

``` r
# deselect unnecessary variables, deselect invalid entries
invalid_cc_id = invalid_cc %>% 
  pull(M2ID) %>% 
  unique()

m2_df = m2_df %>% 
  select(-c(B3PIDATE_MO, B3PIDATE_YR, B1PTSEIS, B1PA37, B1PA38A, B1SA11Z, B4H33, B4H34, B4H36, B4H38, B4H40)) %>% 
  filter(!(M2ID %in% invalid_cc_id))
```

# M3 dataframe

``` r
m2_id = m2_df %>% pull(M2ID)
length(m2_id)
```

    ## [1] 1076

``` r
m3_df = 
  read_csv("./data/m3_df.csv") %>% 
  select(-1)


# only 911 left after joining, the rest 165 don't have M3 record
# 867 left after filtering out invalid entries
full_df = 
  inner_join(m2_df, m3_df, by = "M2ID") %>% 
  select(-M2FAMNUM.y) %>% 
  rename(M2FAMNUM = M2FAMNUM.x) %>%
  filter(C3TCOMP != 8 & C3TEM != 8 & C1PA6A != 7) %>% 
  mutate(D3TCOMP = C3TCOMP - B3TCOMPZ3,
         D3TEM = C3TEM - B3TEMZ3,
         D3TEF = C3TEF - B3TEFZ3,
         D1PB19 = C1PB19 - B1PB19,
         B1PF7A = as.factor(B1PF7A),
         B1PA39 = as.factor(B1PA39))
```

# Univariate Analysis

### Univariate Analysis of Cognition, CTQ total

``` r
joined_date = read_csv("./data/time_lasped.csv") %>% 
  mutate(yr_lapsed = lapsed/12) %>% 
  select(M2ID, yr_lapsed)  # year lapsed
```

``` r
full_df_no_invalid = full_df %>% 
  mutate(ctq_total = B4QCT_EA + B4QCT_EN + B4QCT_SA + B4QCT_PA + B4QCT_PN + B4QCT_MD,
         B1PRSEX = as.factor(B1PRSEX),
         B4ALCOH = factor(B4ALCOH, levels = c("former_light/abs", "former_moderate", "former_heavy", "current_light", "current_moderate", "current_heavy")),
         D1PB19 = factor(D1PB19, levels = c(0,-1, 1)),
         B1PF7A = ifelse(as.numeric(B1PF7A) != 1, 2, as.numeric(B1PF7A)),
         B1PF7A = as.factor(B1PF7A),
         B1PA39 = recode_factor(B1PA39, `9` = "non_smoker", `2` = "former_smoker", `1` = "current_smoker"),
         D1PA6A = factor(ifelse((C1PA6A - B1PA6A) == -1, 1, 0), levels = c(0,1)),
         B1SA11W = ifelse(B1SA11W == 2, 0, 1),
         B1SA62A = ifelse(B1SA62A == 2, 0, 1),
         B1SA62B = ifelse(B1SA62B == 2, 0, 1),
         B1SA62C = ifelse(B1SA62C == 2, 0, 1),
         B1SA62D = ifelse(B1SA62D == 2, 0, 1),
         B1SA62E = ifelse(B1SA62E == 2, 0, 1),
         B1SA62F = ifelse(B1SA62F == 2, 0, 1),
         B1SA62G = ifelse(B1SA62G == 2, 0, 1),
         B1SA62H = ifelse(B1SA62H == 2, 0, 1),
         B1SA62I = ifelse(B1SA62I == 2, 0, 1),
         B1SA62J = ifelse(B1SA62J == 2, 0, 1),
         thr_total = B4QCT_EA + B4QCT_SA + B4QCT_PA,
         dep_total = B4QCT_EN + B4QCT_PN + B4QCT_MD,
  )
#write.csv(full_df_no_invalid, "./data/full_df.csv")

# scaling age, SES, modifiers (?), exercise
full_df_no_invalid =  full_df_no_invalid %>% 
  mutate(B1SA11W = as.factor(B1SA11W),
         B1SA62A = as.factor(B1SA62A),
         B1SA62B = as.factor(B1SA62B),
         B1SA62C = as.factor(B1SA62C),
         B1SA62D = as.factor(B1SA62D),
         B1SA62E = as.factor(B1SA62E),
         B1SA62F = as.factor(B1SA62F),
         B1SA62G = as.factor(B1SA62G),
         B1SA62H = as.factor(B1SA62H),
         B1SA62I = as.factor(B1SA62I),
         B1SA62J = as.factor(B1SA62J)) %>% 
   mutate_each_(funs(scale(.)), c(6,10,25:30, 37)) %>% 
  select(-D1PA6A) 

full_df_no_invalid = left_join(full_df_no_invalid, joined_date, by = "M2ID")
# sum(is.na(joined_date$yr_lapsed)==TRUE) # no invalid
full_df_no_invalid = full_df_no_invalid %>% 
  mutate(D3TCOMPR = D3TCOMP/yr_lapsed,
         D3TEMR = D3TEM/yr_lapsed,
         D3TEFR = D3TEF/yr_lapsed)
```

``` r
#write.csv(full_df_no_invalid, "./data/full_df_inval.csv")
```

# Modeling

I completely filter out all the observations with at least one invalid
feature, in total there are 23 of them, which is an arbitrarily small
number.

first, fit the model with a list of covariates

-   ctq total score
-   age
-   sex
-   race
-   education (or SES)
-   (imputed) SES
-   changes in marital status
-   changes in stroke (deleted 9.28)
-   smoking, drug use, alcohol consumption
-   exercise and chronic sleep problem

## Base Model

``` r
lmm_base_1 = lme(D3TCOMP~ ctq_total + B3TCOMPZ3, random = ~1 | M2FAMNUM, data = full_df_no_invalid)
summary(lmm_base_1)$tTable %>% 
  knitr::kable(caption = "Base Model - Composite Score ($\\Delta$)")
```

|             |      Value | Std.Error |  DF |    t-value |   p-value |
|:------------|-----------:|----------:|----:|-----------:|----------:|
| (Intercept) | -0.0485871 | 0.0432958 | 765 |  -1.122212 | 0.2621243 |
| ctq_total   | -0.0014657 | 0.0010514 |  99 |  -1.394006 | 0.1664367 |
| B3TCOMPZ3   | -0.4323184 | 0.0159102 |  99 | -27.172437 | 0.0000000 |

Base Model - Composite Score (*Δ*)

``` r
lmm_base_2 = lme(D3TEM~ ctq_total + B3TEMZ3, random = ~1 | M2FAMNUM, data = full_df_no_invalid)
summary(lmm_base_2)$tTable %>% 
  knitr::kable(caption = "Base Model - Episodic Memory ($\\Delta$)")
```

|             |      Value | Std.Error |  DF |    t-value |   p-value |
|:------------|-----------:|----------:|----:|-----------:|----------:|
| (Intercept) |  0.0550330 | 0.0849678 | 765 |   0.647692 | 0.5173785 |
| ctq_total   | -0.0028144 | 0.0020769 |  99 |  -1.355064 | 0.1784811 |
| B3TEMZ3     | -0.4385340 | 0.0317557 |  99 | -13.809617 | 0.0000000 |

Base Model - Episodic Memory (*Δ*)

``` r
lmm_base_3 = lme(D3TEF~ ctq_total + B3TEFZ3, random = ~1 | M2FAMNUM, data = full_df_no_invalid)
summary(lmm_base_3)$tTable %>% 
  knitr::kable(caption = "Base Model - Executive Function ($\\Delta$)")
```

|             |      Value | Std.Error |  DF |     t-value |   p-value |
|:------------|-----------:|----------:|----:|------------:|----------:|
| (Intercept) | -0.2694083 | 0.0462593 | 765 |  -5.8238776 | 0.0000000 |
| ctq_total   |  0.0005408 | 0.0011192 |  99 |   0.4832297 | 0.6299996 |
| B3TEFZ3     | -0.3756244 | 0.0176892 |  99 | -21.2347179 | 0.0000000 |

Base Model - Executive Function (*Δ*)

``` r
full_df_test = read_csv("./data/full_df.csv") %>% 
  select(-1)
# D3TCOMP
full_df_test %>% 
  select(D3TCOMP, B1PAGE_M2) %>% 
  ggplot(aes(x = B1PAGE_M2, y = D3TCOMP)) + 
  geom_point() +
  geom_smooth(aes(color = 'red'))
```

![](investigations_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
# D3TEF
full_df_no_invalid %>% 
  select(D3TEF, ctq_total) %>% 
  ggplot(aes(x = ctq_total, y = D3TEF)) + 
  geom_point() +
  geom_smooth(aes(color = 'red'))
```

![](investigations_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
# D3TEM
full_df_no_invalid %>% 
  select(D3TEM, ctq_total) %>% 
  ggplot(aes(x = ctq_total, y = D3TEM)) + 
  geom_point() +
  geom_smooth(aes(color = 'red'))
```

![](investigations_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

## Adding more covariates (LMM)

``` r
# add sex, age, and race as covariates
lmm1_pt1 = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PRSEX + B1PAGE_M2 + B1PF7A, random = ~1|M2FAMNUM,  data = full_df_no_invalid, method = "REML")
summary(lmm1_pt1)$tTable %>% 
  knitr::kable(caption = "Model 1 - Composite Scores ($\\Delta$)")
```

|             |      Value | Std.Error |  DF |     t-value |   p-value |
|:------------|-----------:|----------:|----:|------------:|----------:|
| (Intercept) |  0.0323839 | 0.0435963 | 765 |   0.7428126 | 0.4578232 |
| ctq_total   | -0.0025559 | 0.0010286 |  96 |  -2.4848918 | 0.0146897 |
| B3TCOMPZ3   | -0.4903434 | 0.0169799 |  96 | -28.8778094 | 0.0000000 |
| B1PRSEX2    | -0.0021125 | 0.0282484 |  96 |  -0.0747839 | 0.9405423 |
| B1PAGE_M2   | -0.1343204 | 0.0151749 |  96 |  -8.8514738 | 0.0000000 |
| B1PF7A2     | -0.1388579 | 0.0407281 |  96 |  -3.4093884 | 0.0009534 |

Model 1 - Composite Scores (*Δ*)

``` r
lmm2_pt1 = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PRSEX + B1PAGE_M2 + B1PF7A, random = ~1|M2FAMNUM,  data = full_df_no_invalid, method = "REML")
summary(lmm2_pt1)$tTable %>% 
  knitr::kable(caption = "Model 1 - Episodic Memory ($\\Delta$)")
```

|             |      Value | Std.Error |  DF |     t-value |   p-value |
|:------------|-----------:|----------:|----:|------------:|----------:|
| (Intercept) | -0.0096880 | 0.0843723 | 765 |  -0.1148244 | 0.9086144 |
| ctq_total   | -0.0062601 | 0.0020251 |  96 |  -3.0912936 | 0.0026085 |
| B3TEMZ3     | -0.5436013 | 0.0327778 |  96 | -16.5844152 | 0.0000000 |
| B1PRSEX2    |  0.4029617 | 0.0585654 |  96 |   6.8805371 | 0.0000000 |
| B1PAGE_M2   | -0.1965642 | 0.0286379 |  96 |  -6.8637770 | 0.0000000 |
| B1PF7A2     | -0.0831738 | 0.0768121 |  96 |  -1.0828216 | 0.2815999 |

Model 1 - Episodic Memory (*Δ*)

``` r
lmm3_pt1 = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PRSEX + B1PAGE_M2 + B1PF7A, random = ~1|M2FAMNUM,  data = full_df_no_invalid, method = "REML")
summary(lmm3_pt1)$tTable %>% 
  knitr::kable(caption = "Model 1 - Executive Function ($\\Delta$)")
```

|             |      Value | Std.Error |  DF |     t-value |   p-value |
|:------------|-----------:|----------:|----:|------------:|----------:|
| (Intercept) | -0.1543837 | 0.0471497 | 765 |  -3.2743329 | 0.0011068 |
| ctq_total   | -0.0002328 | 0.0010974 |  96 |  -0.2121143 | 0.8324676 |
| B3TEFZ3     | -0.4395234 | 0.0189900 |  96 | -23.1449443 | 0.0000000 |
| B1PRSEX2    | -0.0772976 | 0.0301372 |  96 |  -2.5648581 | 0.0118711 |
| B1PAGE_M2   | -0.1320880 | 0.0162211 |  96 |  -8.1429537 | 0.0000000 |
| B1PF7A2     | -0.1368604 | 0.0434582 |  96 |  -3.1492382 | 0.0021823 |

Model 1 - Executive Function (*Δ*)

``` r
# add (on top of previous ones), SES, alcohol, smoking, change in stroke, exercise (MET), sleeping disorder etc. 
lmm1 = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PRSEX + B1PAGE_M2 + B1PF7A + B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W, random = ~1|M2FAMNUM,  data = full_df_no_invalid, method = "REML")
summary(lmm1)$tTable %>% 
  knitr::kable(caption = "Model 2 - Composite Scores ($\\Delta$)")
```

|                         |      Value | Std.Error |  DF |     t-value |   p-value |
|:------------------------|-----------:|----------:|----:|------------:|----------:|
| (Intercept)             | -0.0434274 | 0.0560991 | 765 |  -0.7741192 | 0.4390995 |
| ctq_total               | -0.0019798 | 0.0010563 |  84 |  -1.8742613 | 0.0643707 |
| B3TCOMPZ3               | -0.5106305 | 0.0174791 |  84 | -29.2137402 | 0.0000000 |
| B1PRSEX2                |  0.0146839 | 0.0292471 |  84 |   0.5020631 | 0.6169367 |
| B1PAGE_M2               | -0.1385387 | 0.0154536 |  84 |  -8.9648384 | 0.0000000 |
| B1PF7A2                 | -0.1186062 | 0.0409183 |  84 |  -2.8986087 | 0.0047805 |
| B1PTSEI                 |  0.0452071 | 0.0145436 |  84 |   3.1083752 | 0.0025676 |
| B1PA39former_smoker     |  0.0059370 | 0.0332531 |  84 |   0.1785411 | 0.8587280 |
| B1PA39current_smoker    | -0.0237652 | 0.0500212 |  84 |  -0.4751029 | 0.6359466 |
| B4ALCOHformer_moderate  |  0.0954826 | 0.0543270 |  84 |   1.7575539 | 0.0824667 |
| B4ALCOHformer_heavy     |  0.0276367 | 0.0605290 |  84 |   0.4565859 | 0.6491472 |
| B4ALCOHcurrent_light    |  0.0455146 | 0.0699924 |  84 |   0.6502794 | 0.5172864 |
| B4ALCOHcurrent_moderate |  0.1158741 | 0.0453691 |  84 |   2.5540329 | 0.0124534 |
| B4ALCOHcurrent_heavy    |  0.0163838 | 0.0471467 |  84 |   0.3475063 | 0.7290802 |
| D1PB19-1                | -0.0219701 | 0.0458150 |  84 |  -0.4795409 | 0.6328001 |
| D1PB191                 |  0.0158295 | 0.0675998 |  84 |   0.2341647 | 0.8154269 |
| B4HMETMW                |  0.0208640 | 0.0137041 |  84 |   1.5224628 | 0.1316490 |
| B1SA11W1                | -0.0996088 | 0.0450166 |  84 |  -2.2127102 | 0.0296305 |

Model 2 - Composite Scores (*Δ*)

``` r
lmm2 = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PRSEX + B1PAGE_M2 + B1PF7A + B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W, random = ~1|M2FAMNUM,  data = full_df_no_invalid, method = "REML")
summary(lmm2)$tTable %>% 
  knitr::kable(caption = "Model 2 - Episodic Memory ($\\Delta$)")
```

|                         |      Value | Std.Error |  DF |     t-value |   p-value |
|:------------------------|-----------:|----------:|----:|------------:|----------:|
| (Intercept)             | -0.1319578 | 0.1101605 | 765 |  -1.1978691 | 0.2313390 |
| ctq_total               | -0.0056372 | 0.0020892 |  84 |  -2.6983307 | 0.0084223 |
| B3TEMZ3                 | -0.5514595 | 0.0329993 |  84 | -16.7112389 | 0.0000000 |
| B1PRSEX2                |  0.4150267 | 0.0606390 |  84 |   6.8442189 | 0.0000000 |
| B1PAGE_M2               | -0.2063046 | 0.0293060 |  84 |  -7.0396753 | 0.0000000 |
| B1PF7A2                 | -0.0401149 | 0.0780299 |  84 |  -0.5140965 | 0.6085344 |
| B1PTSEI                 |  0.0436374 | 0.0282501 |  84 |   1.5446816 | 0.1261825 |
| B1PA39former_smoker     |  0.0537312 | 0.0659893 |  84 |   0.8142416 | 0.4178089 |
| B1PA39current_smoker    | -0.1349153 | 0.0991325 |  84 |  -1.3609594 | 0.1771667 |
| B4ALCOHformer_moderate  |  0.1251868 | 0.1076824 |  84 |   1.1625556 | 0.2483021 |
| B4ALCOHformer_heavy     |  0.0498072 | 0.1199557 |  84 |   0.4152131 | 0.6790445 |
| B4ALCOHcurrent_light    |  0.1987230 | 0.1388289 |  84 |   1.4314246 | 0.1560188 |
| B4ALCOHcurrent_moderate |  0.1844225 | 0.0898981 |  84 |   2.0514614 | 0.0433374 |
| B4ALCOHcurrent_heavy    | -0.0191641 | 0.0934183 |  84 |  -0.2051433 | 0.8379565 |
| D1PB19-1                | -0.1111912 | 0.0907739 |  84 |  -1.2249247 | 0.2240275 |
| D1PB191                 |  0.0052129 | 0.1341217 |  84 |   0.0388670 | 0.9690886 |
| B4HMETMW                |  0.0538464 | 0.0271771 |  84 |   1.9813151 | 0.0508271 |
| B1SA11W1                | -0.0094406 | 0.0892153 |  84 |  -0.1058178 | 0.9159792 |

Model 2 - Episodic Memory (*Δ*)

``` r
lmm3 = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PRSEX + B1PAGE_M2 + B1PF7A + B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W, random = ~1|M2FAMNUM,  data = full_df_no_invalid, method = "REML")
summary(lmm3)$tTable %>% 
  knitr::kable(caption = "Model 2 - Executive Function ($\\Delta$)")
```

|                         |      Value | Std.Error |  DF |     t-value |   p-value |
|:------------------------|-----------:|----------:|----:|------------:|----------:|
| (Intercept)             | -0.1931443 | 0.0603294 | 765 |  -3.2014944 | 0.0014237 |
| ctq_total               |  0.0003353 | 0.0011310 |  84 |   0.2964795 | 0.7675957 |
| B3TEFZ3                 | -0.4569546 | 0.0196109 |  84 | -23.3010906 | 0.0000000 |
| B1PRSEX2                | -0.0649380 | 0.0312466 |  84 |  -2.0782437 | 0.0407391 |
| B1PAGE_M2               | -0.1322069 | 0.0165487 |  84 |  -7.9889513 | 0.0000000 |
| B1PF7A2                 | -0.1209603 | 0.0438272 |  84 |  -2.7599353 | 0.0070962 |
| B1PTSEI                 |  0.0441080 | 0.0156033 |  84 |   2.8268314 | 0.0058744 |
| B1PA39former_smoker     | -0.0200912 | 0.0357427 |  84 |  -0.5621059 | 0.5755408 |
| B1PA39current_smoker    | -0.0425444 | 0.0537286 |  84 |  -0.7918401 | 0.4306844 |
| B4ALCOHformer_moderate  |  0.0663582 | 0.0583057 |  84 |   1.1381086 | 0.2583108 |
| B4ALCOHformer_heavy     |  0.0239244 | 0.0650330 |  84 |   0.3678806 | 0.7138877 |
| B4ALCOHcurrent_light    | -0.0496072 | 0.0752125 |  84 |  -0.6595603 | 0.5113403 |
| B4ALCOHcurrent_moderate |  0.0579850 | 0.0487246 |  84 |   1.1900572 | 0.2373765 |
| B4ALCOHcurrent_heavy    |  0.0177271 | 0.0505900 |  84 |   0.3504073 | 0.7269102 |
| D1PB19-1                |  0.0575710 | 0.0492036 |  84 |   1.1700568 | 0.2452872 |
| D1PB191                 |  0.0779124 | 0.0728383 |  84 |   1.0696618 | 0.2878357 |
| B4HMETMW                |  0.0172242 | 0.0147475 |  84 |   1.1679392 | 0.2461356 |
| B1SA11W1                | -0.0946382 | 0.0483349 |  84 |  -1.9579688 | 0.0535532 |

Model 2 - Executive Function (*Δ*)

``` r
# adding education (one example)
lmm1_edu = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PRSEX + B1PAGE_M2 + B1PF7A + B1PB1 + B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W, random = ~1|M2FAMNUM,  data = full_df_no_invalid, method = "REML")
summary(lmm1_edu)$tTable %>% 
  knitr::kable()
```

|                         |      Value | Std.Error |  DF |     t-value |   p-value |
|:------------------------|-----------:|----------:|----:|------------:|----------:|
| (Intercept)             | -0.2126135 | 0.0787301 | 765 |  -2.7005369 | 0.0070758 |
| ctq_total               | -0.0018094 | 0.0010533 |  83 |  -1.7178568 | 0.0895515 |
| B3TCOMPZ3               | -0.5245217 | 0.0179494 |  83 | -29.2222636 | 0.0000000 |
| B1PRSEX2                |  0.0217872 | 0.0292099 |  83 |   0.7458836 | 0.4578450 |
| B1PAGE_M2               | -0.1386999 | 0.0153966 |  83 |  -9.0084520 | 0.0000000 |
| B1PF7A2                 | -0.1195814 | 0.0407456 |  83 |  -2.9348334 | 0.0043151 |
| B1PB1                   |  0.0209659 | 0.0068918 |  83 |   3.0421561 | 0.0031441 |
| B1PTSEI                 |  0.0237812 | 0.0161161 |  83 |   1.4756209 | 0.1438293 |
| B1PA39former_smoker     |  0.0094635 | 0.0330979 |  83 |   0.2859260 | 0.7756472 |
| B1PA39current_smoker    | -0.0071665 | 0.0500568 |  83 |  -0.1431684 | 0.8865042 |
| B4ALCOHformer_moderate  |  0.0850278 | 0.0541733 |  83 |   1.5695532 | 0.1203238 |
| B4ALCOHformer_heavy     |  0.0329815 | 0.0602503 |  83 |   0.5474090 | 0.5855668 |
| B4ALCOHcurrent_light    |  0.0342367 | 0.0697395 |  83 |   0.4909230 | 0.6247764 |
| B4ALCOHcurrent_moderate |  0.1094722 | 0.0452052 |  83 |   2.4216716 | 0.0176280 |
| B4ALCOHcurrent_heavy    |  0.0145074 | 0.0469302 |  83 |   0.3091264 | 0.7580001 |
| D1PB19-1                | -0.0195256 | 0.0455927 |  83 |  -0.4282605 | 0.6695708 |
| D1PB191                 |  0.0065851 | 0.0672952 |  83 |   0.0978532 | 0.9222848 |
| B4HMETMW                |  0.0217360 | 0.0136316 |  83 |   1.5945322 | 0.1146180 |
| B1SA11W1                | -0.1003691 | 0.0447963 |  83 |  -2.2405668 | 0.0277256 |

Check model assumptions

``` r
plot(lmm1, main = "Change in Composite Scores: resid vs. fitted")
```

![](investigations_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
plot(lmm2, main = "Change in Episodic Memory: resid vs. fitted")
```

![](investigations_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

``` r
plot(lmm3, main = "Change in Executive Function: resid vs. fitted")
```

![](investigations_files/figure-gfm/unnamed-chunk-19-3.png)<!-- -->

# Modifiers

-   Stratified analysis can be one way to analyze the effect
    modification; however, as this analysis is usually done in a 2-by-2
    table, we either need dichotomous variables (e.g. sex) or an
    arbitrary cutoff point for continuous variables.

The following part is to investigate whether adding an interaction term
will improve the model fit. Use **likelihood-ratio test** for model
comparison. Need to refit the model using maximum likelihood (ML).

Autonomy (B1SPWBA2)

``` r
# Composite
lmm1_1 = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBA2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
lmm1_autonomy = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBA2 + ctq_total*B1SPWBA2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm1_1, lmm1_autonomy)
```

    ##               Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## lmm1_1            1 22 872.6073 977.4381 -414.3036                        
    ## lmm1_autonomy     2 23 873.9829 983.5788 -413.9914 1 vs 2 0.624363  0.4294

``` r
# EM
lmm2_1 = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBA2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm2_autonomy = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBA2 + ctq_total*B1SPWBA2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
anova(lmm2_1, lmm2_autonomy)
```

    ##               Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## lmm2_1            1 22 2063.141 2167.972 -1009.570                         
    ## lmm2_autonomy     2 23 2064.965 2174.561 -1009.482 1 vs 2 0.1756932  0.6751

``` r
# EF
lmm3_1 = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBA2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm3_autonomy = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBA2 + ctq_total*B1SPWBA2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
anova(lmm3_1, lmm3_autonomy)
```

    ##               Model df       AIC      BIC    logLik   Test   L.Ratio p-value
    ## lmm3_1            1 22  999.3977 1104.228 -477.6988                         
    ## lmm3_autonomy     2 23 1000.7712 1110.367 -477.3856 1 vs 2 0.6264816  0.4286

``` r
#plot(lmm3_autonomy)
```

Environmental Mastery (B1SPWBE2)

``` r
# Composite
lmm1_2 = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBE2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
lmm1_em = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBE2 + ctq_total*B1SPWBE2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
anova(lmm1_2, lmm1_em)
```

    ##         Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## lmm1_2      1 22 872.4598 977.2906 -414.2299                        
    ## lmm1_em     2 23 872.3235 981.9193 -413.1617 1 vs 2 2.136317  0.1438

``` r
# EM
lmm2_2 = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBE2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm2_em = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBE2 + ctq_total*B1SPWBE2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
anova(lmm2_2, lmm2_em)
```

    ##         Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## lmm2_2      1 22 2063.774 2168.604 -1009.887                         
    ## lmm2_em     2 23 2065.456 2175.052 -1009.728 1 vs 2 0.3173035  0.5732

``` r
#plot(lmm2_autonomy)

# EF
lmm3_2 = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBE2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm3_em = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBE2 + ctq_total*B1SPWBA2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
anova(lmm3_2, lmm3_em)
```

    ##         Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## lmm3_2      1 22 1000.597 1105.428 -478.2986                        
    ## lmm3_em     2 24 1002.675 1117.036 -477.3374 1 vs 2 1.922329  0.3824

Personal Growth (B1SPWBG2)

``` r
# Composite
lmm1_3 = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBG2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
lmm1_pg = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBG2 + ctq_total*B1SPWBG2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
anova(lmm1_3, lmm1_pg)
```

    ##         Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## lmm1_3      1 22 872.7432 977.5740 -414.3716                         
    ## lmm1_pg     2 23 874.5398 984.1357 -414.2699 1 vs 2 0.2033483   0.652

``` r
# EM
lmm2_3 = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBG2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm2_pg = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBG2 + ctq_total*B1SPWBG2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
anova(lmm2_3, lmm2_pg)
```

    ##         Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## lmm2_3      1 22 2064.237 2169.068 -1010.118                         
    ## lmm2_pg     2 23 2066.093 2175.689 -1010.046 1 vs 2 0.1441885  0.7042

``` r
# EF
lmm3_3 = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBG2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm3_pg = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBG2 + ctq_total*B1SPWBG2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
anova(lmm3_3, lmm3_pg)
```

    ##         Model df       AIC      BIC    logLik   Test  L.Ratio p-value
    ## lmm3_3      1 22  999.9347 1104.766 -477.9674                        
    ## lmm3_pg     2 23 1000.6254 1110.221 -477.3127 1 vs 2 1.309304  0.2525

Positive Relations with Others (B1SPWBR2)

``` r
# Composite
lmm1_4 = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBR2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
lmm1_pr = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBR2 + ctq_total*B1SPWBR2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
anova(lmm1_4, lmm1_pr)
```

    ##         Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## lmm1_4      1 22 871.4490 976.2799 -413.7245                         
    ## lmm1_pr     2 23 873.4214 983.0173 -413.7107 1 vs 2 0.0275933  0.8681

``` r
# EM
lmm2_4 = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBR2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm2_pr = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBR2 + ctq_total*B1SPWBR2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
anova(lmm2_4, lmm2_pr)
```

    ##         Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## lmm2_4      1 22 2064.356 2169.187 -1010.178                         
    ## lmm2_pr     2 23 2066.188 2175.784 -1010.094 1 vs 2 0.1679829  0.6819

``` r
# EF
lmm3_4 = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBR2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm3_pr = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBR2 + ctq_total*B1SPWBA2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
anova(lmm3_4, lmm3_pr)
```

    ##         Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## lmm3_4      1 22 1000.557 1105.388 -478.2787                        
    ## lmm3_pr     2 24 1002.013 1116.374 -477.0065 1 vs 2 2.544459  0.2802

Purpose in Life (B1SPWBU2)

``` r
# Composite
lmm1_5 = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBU2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
lmm1_pl = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBU2 + ctq_total*B1SPWBU2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
anova(lmm1_5, lmm1_pl)
```

    ##         Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## lmm1_5      1 22 872.6220 977.4528 -414.3110                         
    ## lmm1_pl     2 23 874.5048 984.1007 -414.2524 1 vs 2 0.1172279  0.7321

``` r
# EM
lmm2_5 = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBU2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm2_pl = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBU2 + ctq_total*B1SPWBU2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
anova(lmm2_5, lmm2_pl)
```

    ##         Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## lmm2_5      1 22 2063.867 2168.698 -1009.934                        
    ## lmm2_pl     2 23 2064.430 2174.026 -1009.215 1 vs 2 1.437588  0.2305

``` r
# EF
lmm3_5 = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBU2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm3_pl = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBU2 + ctq_total*B1SPWBU2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
anova(lmm3_5, lmm3_pl)
```

    ##         Model df      AIC      BIC    logLik   Test    L.Ratio p-value
    ## lmm3_5      1 22 1000.623 1105.454 -478.3117                          
    ## lmm3_pl     2 23 1002.603 1112.199 -478.3014 1 vs 2 0.02060604  0.8859

Self-Acceptance (B1SPWBS2)

``` r
# Composite
lmm1_6 = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBS2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
lmm1_sa = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBS2 + ctq_total*B1SPWBS2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
anova(lmm1_6, lmm1_sa)
```

    ##         Model df      AIC      BIC    logLik   Test    L.Ratio p-value
    ## lmm1_6      1 22 870.7782 975.6091 -413.3891                          
    ## lmm1_sa     2 23 872.7447 982.3406 -413.3724 1 vs 2 0.03347297  0.8548

``` r
# EM
lmm2_6 = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBS2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm2_sa = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBS2 + ctq_total*B1SPWBS2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
anova(lmm2_6, lmm2_sa)
```

    ##         Model df      AIC      BIC    logLik   Test     L.Ratio p-value
    ## lmm2_6      1 22 2064.359 2169.189 -1010.179                           
    ## lmm2_sa     2 23 2066.356 2175.952 -1010.178 1 vs 2 0.002690267  0.9586

``` r
# EF
lmm3_6 = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBS2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm3_sa = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SPWBS2 + ctq_total*B1SPWBS2, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML") 
anova(lmm3_6, lmm3_sa)
```

    ##         Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## lmm3_6      1 22 1000.113 1104.944 -478.0567                        
    ## lmm3_sa     2 23 1002.101 1111.697 -478.0507 1 vs 2 0.012018  0.9127

-   the results are the same using `lmer` package.

### part 2: Self-administered Drugs

Results being significant: Using Sedative has a moderating effect on the
change in episodic memory (0.0198) Results being near the threshold:
Using Painkillers upon change in episodic memory (0.0996); Using LDS
upon change in Executive Functioning (0.06)

“Sedative”(A)

``` r
# COMP
lmm1_1_base = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62A, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm1_1_a = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62A*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm1_1_base, lmm1_1_a)
```

    ##             Model df      AIC      BIC    logLik   Test    L.Ratio p-value
    ## lmm1_1_base     1 22 869.1618 973.9926 -412.5809                          
    ## lmm1_1_a        2 23 871.0984 980.6943 -412.5492 1 vs 2 0.06335034  0.8013

``` r
# EM
lmm2_1_base = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62A, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm2_1_a = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62A*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm2_1_base, lmm2_1_a)
```

    ##             Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## lmm2_1_base     1 22 2063.325 2168.156 -1009.663                        
    ## lmm2_1_a        2 23 2059.895 2169.491 -1006.947 1 vs 2 5.430399  0.0198

``` r
# EF
lmm3_1_base = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62A, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm3_1_a = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62A*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm3_1_base, lmm3_1_a)
```

    ##             Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## lmm3_1_base     1 22 996.0513 1100.882 -476.0257                        
    ## lmm3_1_a        2 23 997.0369 1106.633 -475.5185 1 vs 2 1.014426  0.3138

“Tranquilizer”(B)

``` r
# COMP
lmm1_2_base = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62B, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm1_2_a = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62B*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm1_2_base, lmm1_2_a)
```

    ##             Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## lmm1_2_base     1 22 872.4624 977.2932 -414.2312                        
    ## lmm1_2_a        2 23 873.2376 982.8335 -413.6188 1 vs 2 1.224822  0.2684

``` r
# EM
lmm2_2_base = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62B, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm2_2_a = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62B*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm2_2_base, lmm2_2_a)
```

    ##             Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## lmm2_2_base     1 22 2060.695 2165.526 -1008.347                         
    ## lmm2_2_a        2 23 2061.796 2171.391 -1007.898 1 vs 2 0.8992667   0.343

``` r
# EF
lmm3_2_base = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62B, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm3_2_a = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62B*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm3_2_base, lmm3_2_a)
```

    ##             Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## lmm3_2_base     1 22 1000.677 1105.507 -478.3382                         
    ## lmm3_2_a        2 23 1002.049 1111.645 -478.0247 1 vs 2 0.6270175  0.4285

“Stimulant”(C)

``` r
# COMP
lmm1_3_base = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62C, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm1_3_a = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62C*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm1_3_base, lmm1_3_a)
```

    ##             Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## lmm1_3_base     1 22 872.2382 977.0690 -414.1191                         
    ## lmm1_3_a        2 23 873.7579 983.3538 -413.8789 1 vs 2 0.4802682  0.4883

``` r
# EM
lmm2_3_base = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62C, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm2_3_a = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62C*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm2_3_base, lmm2_3_a)
```

    ##             Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## lmm2_3_base     1 22 2063.820 2168.650 -1009.910                        
    ## lmm2_3_a        2 23 2065.703 2175.299 -1009.851 1 vs 2 0.116765  0.7326

``` r
# EF
lmm3_3_base = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62C, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm3_3_a = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62C*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm3_3_base, lmm3_3_a)
```

    ##             Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## lmm3_3_base     1 22 999.9159 1104.747 -477.9579                        
    ## lmm3_3_a        2 23 999.7895 1109.385 -476.8947 1 vs 2 2.126399  0.1448

“Painkiller” (D)

``` r
# COMP
lmm1_4_base = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62D, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm1_4_a = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62D*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm1_4_base, lmm1_4_a)
```

    ##             Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## lmm1_4_base     1 22 871.1367 975.9676 -413.5684                         
    ## lmm1_4_a        2 23 872.4871 982.0830 -413.2435 1 vs 2 0.6496331  0.4202

``` r
# EM
lmm2_4_base = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62D, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm2_4_a = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62D*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm2_4_base, lmm2_4_a)
```

    ##             Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## lmm2_4_base     1 22 2063.525 2168.356 -1009.763                        
    ## lmm2_4_a        2 23 2062.812 2172.408 -1008.406 1 vs 2 2.712431  0.0996

``` r
# EF
lmm3_4_base = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62D, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm3_4_a = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62D*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm3_4_base, lmm3_4_a)
```

    ##             Model df      AIC      BIC    logLik   Test     L.Ratio p-value
    ## lmm3_4_base     1 22 1000.656 1105.487 -478.3281                           
    ## lmm3_4_a        2 23 1002.648 1112.244 -478.3242 1 vs 2 0.007872413  0.9293

“Depress Medication” (E)

``` r
# COMP
lmm1_5_base = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62E, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm1_5_a = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62E*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm1_5_base, lmm1_5_a)
```

    ##             Model df      AIC      BIC    logLik   Test     L.Ratio p-value
    ## lmm1_5_base     1 22 865.7931 970.6239 -410.8965                           
    ## lmm1_5_a        2 23 867.7884 977.3843 -410.8942 1 vs 2 0.004708808  0.9453

``` r
# EM
lmm2_5_base = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62E, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm2_5_a = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62E*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm2_5_base, lmm2_5_a)
```

    ##             Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## lmm2_5_base     1 22 2061.595 2166.425 -1008.797                         
    ## lmm2_5_a        2 23 2063.337 2172.933 -1008.668 1 vs 2 0.2576297  0.6118

``` r
# EF
lmm3_5_base = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62E, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm3_5_a = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62E*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm3_5_base, lmm3_5_a)
```

    ##             Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## lmm3_5_base     1 22 993.9699 1098.801 -474.9850                         
    ## lmm3_5_a        2 23 995.4829 1105.079 -474.7414 1 vs 2 0.4870597  0.4852

“Inhalant” & “Heroin” (F/J)

``` r
# COMP
lmm1_6_base = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62F, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm1_6_a = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62F*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm1_6_base, lmm1_6_a)

# EM
lmm2_6_base = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62F, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm2_6_a = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62F*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm2_6_base, lmm2_6_a)

# EF
lmm3_6_base = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62F, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm3_6_a = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62F*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm3_6_base, lmm3_6_a)
```

“Marijuana” (G)

``` r
# COMP
lmm1_7_base = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62G, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm1_7_a = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62G*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm1_7_base, lmm1_7_a)
```

    ##             Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## lmm1_7_base     1 22 872.7299 977.5608 -414.3650                         
    ## lmm1_7_a        2 23 874.4190 984.0149 -414.2095 1 vs 2 0.3109475  0.5771

``` r
# EM
lmm2_7_base = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62G, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm2_7_a = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62G*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm2_7_base, lmm2_7_a)
```

    ##             Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## lmm2_7_base     1 22 2060.930 2165.761 -1008.465                         
    ## lmm2_7_a        2 23 2062.504 2172.100 -1008.252 1 vs 2 0.4266585  0.5136

``` r
# EF
lmm3_7_base = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62G, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm3_7_a = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62G*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm3_7_base, lmm3_7_a)
```

    ##             Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## lmm3_7_base     1 22 1000.589 1105.420 -478.2944                         
    ## lmm3_7_a        2 23 1002.021 1111.617 -478.0107 1 vs 2 0.5675265  0.4512

“Cocaine” (H)

``` r
# COMP
lmm1_8_base = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62H, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm1_8_a = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62H*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm1_8_base, lmm1_8_a)
```

    ##             Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## lmm1_8_base     1 22 872.4662 977.2970 -414.2331                        
    ## lmm1_8_a        2 23 874.1934 983.7893 -414.0967 1 vs 2 0.272739  0.6015

``` r
# EM
lmm2_8_base = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62H, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm2_8_a = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62H*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm2_8_base, lmm2_8_a)
```

    ##             Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## lmm2_8_base     1 22 2064.055 2168.886 -1010.028                         
    ## lmm2_8_a        2 23 2065.724 2175.320 -1009.862 1 vs 2 0.3312035   0.565

``` r
# EF
lmm3_8_base = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62H, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm3_8_a = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62H*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm3_8_base, lmm3_8_a)
```

    ##             Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## lmm3_8_base     1 22 1000.167 1104.998 -478.0834                        
    ## lmm3_8_a        2 23 1000.892 1110.488 -477.4462 1 vs 2 1.274444  0.2589

“LDS” (I)

``` r
# COMP
lmm1_9_base = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62I, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm1_9_a = lme(D3TCOMP ~ ctq_total + B3TCOMPZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62I*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm1_9_base, lmm1_9_a)
```

    ##             Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## lmm1_9_base     1 22 868.8418 973.6727 -412.4209                        
    ## lmm1_9_a        2 23 869.3964 978.9923 -411.6982 1 vs 2 1.445407  0.2293

``` r
# EM
lmm2_9_base = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62I, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm2_9_a = lme(D3TEM ~ ctq_total + B3TEMZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62I*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm2_9_base, lmm2_9_a)
```

    ##             Model df      AIC      BIC    logLik   Test    L.Ratio p-value
    ## lmm2_9_base     1 22 2061.986 2166.817 -1008.993                          
    ## lmm2_9_a        2 23 2063.954 2173.550 -1008.977 1 vs 2 0.03235308  0.8573

``` r
# EF
lmm3_9_base = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62I, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
lmm3_9_a = lme(D3TEF ~ ctq_total + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + B1SA62I*ctq_total, random = ~1 | M2FAMNUM, data = full_df_no_invalid, method = "ML")
anova(lmm3_9_base, lmm3_9_a)
```

    ##             Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## lmm3_9_base     1 22 994.4434 1099.274 -475.2217                        
    ## lmm3_9_a        2 23 992.9290 1102.525 -473.4645 1 vs 2 3.514465  0.0608

## Multilevel Models

*What is the multilevel?*

First, the reason we would like to use multilevel modeling is because we
have measured the same participants multiple times, and there are
correlation within subjects that we cannot ignore.

The **assumption** we were making when constructing the model is:
participants from different families would have different intercepts of
cognitive functioning at the baseline (i.e. having different intercept).
We may expect each family has some differences that affects the
intercept but not the slope (all families will have the same rate of
change throughout time), and that members of the same family would share
the same baseline cognitive functioning.

*What would the model be like?* In Dr. Lachman’s paper, we have seen
that they used multilevel models to evaluate different predictors of
both level of and change in cognitive functioning. They have defined a
model with level 1 slope (non time-varying) and level 2 random intercept
(different baseline for each family). However, one thing I have
understood wrong is the equation: It’s not necessarily true that we put
two predictor on the LHS of the equation as independent variables and
called it multilevel modeling. We are, indeed, predicting both level and
intercept, but the model itself will yield fitted results of intercept
and slope. We are still predicting one independent variable: change in
composite score, episodic memory, or executive functioning. The slopes
and intercepts are by-products of the modeling.

*Thinking about the change per month/years*

Last time, we’ve seen that there’s a huge variance associated with the
elapsed time between M2 and M3 for the population. Thus, maybe we would
like to move from the “change in magnitude” to “rate of change per year
or per month”. In this case, I don’t think we still need to index the
baseline. Or, I could include the time between M2 and M3 as a covariate.

*What are the variables included in the model?*

Dependent variable:

-   D3TCOMP: change in composite scores
-   D3TEM: change in episodic memory
-   D3TEF: change in executive functioning

Independent variable:

-   B3TCOMPZ3, B3TEFZ3, B3TEMZ3: three baseline variables (of cognitive
    functioning) measured at M2
-   M2FAMNUM: family number
-   yr_lapsed: years between M2 and M3 for each participants
-   B1PRSEX: sex, male = 1 (reference), female = 2
-   B1PAGE_M2: age measured at M2
-   B1PF7A: racial origin, white = 1 (reference), non-white = 2
-   B1PTSEI: SES
-   B1PA39: cigarette smoking, reference: non_smoker
-   B4ALCOH: alcohol drinking, reference: former_light/abs
-   D1PB19: marital status change. 0 = no change (reference), -1 =
    divorce or equivalent, 1 = married
-   B4HMETMW: exercise minutes per week
-   B1SA11W: chronic sleep problem in the past 12 months

### random intercept model (the very basic)

Change in Composite Scores

``` r
lmm_model1_cs <- lmer(D3TCOMP ~ B3TCOMPZ3 + ctq_total + (1|M2FAMNUM) + yr_lapsed, REML = FALSE, data = full_df_no_invalid)  # random intercept vary across families about 0.053
summary(lmm_model1_cs)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: D3TCOMP ~ B3TCOMPZ3 + ctq_total + (1 | M2FAMNUM) + yr_lapsed
    ##    Data: full_df_no_invalid
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    950.9    979.5   -469.5    938.9      861 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4589 -0.5491 -0.0154  0.5117  3.5406 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  M2FAMNUM (Intercept) 0.05342  0.2311  
    ##  Residual             0.12154  0.3486  
    ## Number of obs: 867, groups:  M2FAMNUM, 766
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)   0.406129   0.160510 865.709382   2.530  0.01157 *  
    ## B3TCOMPZ3    -0.437992   0.015923 853.527788 -27.507  < 2e-16 ***
    ## ctq_total    -0.001217   0.001048 833.763861  -1.162  0.24568    
    ## yr_lapsed    -0.048800   0.016597 866.904260  -2.940  0.00337 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##           (Intr) B3TCOM ctq_tt
    ## B3TCOMPZ3 -0.157              
    ## ctq_total -0.172  0.062       
    ## yr_lapsed -0.963  0.122 -0.081

Change in Episodic Memory

``` r
lmm_model1_em <- lmer(D3TEM ~ B3TEMZ3 + ctq_total + yr_lapsed + (1|M2FAMNUM), REML = FALSE, data = full_df_no_invalid)
summary(lmm_model1_em)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: D3TEM ~ B3TEMZ3 + ctq_total + yr_lapsed + (1 | M2FAMNUM)
    ##    Data: full_df_no_invalid
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   2146.2   2174.8  -1067.1   2134.2      861 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.24336 -0.59205 -0.07577  0.50272  2.94263 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  M2FAMNUM (Intercept) 0.1968   0.4436  
    ##  Residual             0.4965   0.7047  
    ## Number of obs: 867, groups:  M2FAMNUM, 766
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)   0.225691   0.316465 864.584477   0.713    0.476    
    ## B3TEMZ3      -0.439296   0.031742 863.604734 -13.840   <2e-16 ***
    ## ctq_total    -0.002712   0.002081 830.521259  -1.303    0.193    
    ## yr_lapsed    -0.018384   0.032862 866.667286  -0.559    0.576    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##           (Intr) B3TEMZ ctq_tt
    ## B3TEMZ3   -0.067              
    ## ctq_total -0.164  0.001       
    ## yr_lapsed -0.963  0.053 -0.089

Change in Executive Functioning

``` r
lmm_model1_ef <- lmer(D3TEF ~ B3TEFZ3 + ctq_total + yr_lapsed + (1|M2FAMNUM), REML = FALSE, data = full_df_no_invalid)
summary(lmm_model1_ef)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: D3TEF ~ B3TEFZ3 + ctq_total + yr_lapsed + (1 | M2FAMNUM)
    ##    Data: full_df_no_invalid
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1045.0   1073.6   -516.5   1033.0      861 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.3619 -0.5033  0.0451  0.5164  2.9217 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  M2FAMNUM (Intercept) 0.0629   0.2508  
    ##  Residual             0.1324   0.3638  
    ## Number of obs: 867, groups:  M2FAMNUM, 766
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)  4.823e-01  1.693e-01  8.660e+02   2.848   0.0045 ** 
    ## B3TEFZ3     -3.846e-01  1.756e-02  8.475e+02 -21.906  < 2e-16 ***
    ## ctq_total    9.527e-04  1.108e-03  8.360e+02   0.860   0.3901    
    ## yr_lapsed   -8.065e-02  1.749e-02  8.669e+02  -4.610 4.62e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##           (Intr) B3TEFZ ctq_tt
    ## B3TEFZ3   -0.150              
    ## ctq_total -0.174  0.073       
    ## yr_lapsed -0.963  0.108 -0.081

### Add covariates

Change in Composite Scores

``` r
lmm_model2_cs <- lmer(D3TCOMP ~ B3TCOMPZ3 + ctq_total + B1PRSEX + B1PAGE_M2 + B1PF7A + B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W + yr_lapsed + (1|M2FAMNUM), REML = FALSE, data = full_df_no_invalid)
anova(lmm_model1_cs, lmm_model2_cs)
```

    ## Data: full_df_no_invalid
    ## Models:
    ## lmm_model1_cs: D3TCOMP ~ B3TCOMPZ3 + ctq_total + (1 | M2FAMNUM) + yr_lapsed
    ## lmm_model2_cs: D3TCOMP ~ B3TCOMPZ3 + ctq_total + B1PRSEX + B1PAGE_M2 + B1PF7A + B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W + yr_lapsed + (1 | M2FAMNUM)
    ##               npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## lmm_model1_cs    6 950.93 979.52 -469.47   938.93                         
    ## lmm_model2_cs   21 871.59 971.66 -414.80   829.59 109.34 15  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(lmm_model2_cs)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: D3TCOMP ~ B3TCOMPZ3 + ctq_total + B1PRSEX + B1PAGE_M2 + B1PF7A +  
    ##     B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W +  
    ##     yr_lapsed + (1 | M2FAMNUM)
    ##    Data: full_df_no_invalid
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    871.6    971.7   -414.8    829.6      846 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6218 -0.5719  0.0007  0.5528  3.7287 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  M2FAMNUM (Intercept) 0.04669  0.2161  
    ##  Residual             0.10751  0.3279  
    ## Number of obs: 867, groups:  M2FAMNUM, 766
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)               0.421411   0.167707 866.950341   2.513  0.01216 *  
    ## B3TCOMPZ3                -0.510887   0.017210 862.844380 -29.686  < 2e-16 ***
    ## ctq_total                -0.001959   0.001040 835.946963  -1.884  0.05990 .  
    ## B1PRSEX2                  0.018925   0.028824 841.337669   0.657  0.51164    
    ## B1PAGE_M2                -0.141431   0.015232 771.836500  -9.285  < 2e-16 ***
    ## B1PF7A2                  -0.070094   0.043488 853.407666  -1.612  0.10738    
    ## B1PTSEI                   0.043687   0.014335 850.764323   3.048  0.00238 ** 
    ## B1PA39former_smoker       0.008674   0.032771 854.839237   0.265  0.79132    
    ## B1PA39current_smoker     -0.020742   0.049276 865.822332  -0.421  0.67390    
    ## B4ALCOHformer_moderate    0.095599   0.053495 866.933676   1.787  0.07428 .  
    ## B4ALCOHformer_heavy       0.035668   0.059683 866.759075   0.598  0.55024    
    ## B4ALCOHcurrent_light      0.046059   0.068942 860.610110   0.668  0.50426    
    ## B4ALCOHcurrent_moderate   0.114338   0.044677 866.993040   2.559  0.01066 *  
    ## B4ALCOHcurrent_heavy      0.020767   0.046444 864.646779   0.447  0.65489    
    ## D1PB19-1                 -0.017130   0.045149 861.627771  -0.379  0.70447    
    ## D1PB191                   0.007266   0.066674 796.659339   0.109  0.91325    
    ## B4HMETMW                  0.020950   0.013503 836.995117   1.552  0.12115    
    ## B1SA11W1                 -0.094809   0.044357 866.705100  -2.137  0.03284 *  
    ## yr_lapsed                -0.050605   0.017256 866.297173  -2.933  0.00345 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Change in Episodic Memory

``` r
lmm_model2_em <- lmer(D3TEM ~ B3TEMZ3 + ctq_total + B1PRSEX + B1PAGE_M2 + B1PF7A + B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W + yr_lapsed + (1|M2FAMNUM), REML = FALSE, data = full_df_no_invalid)
anova(lmm_model1_em, lmm_model2_em)
```

    ## Data: full_df_no_invalid
    ## Models:
    ## lmm_model1_em: D3TEM ~ B3TEMZ3 + ctq_total + yr_lapsed + (1 | M2FAMNUM)
    ## lmm_model2_em: D3TEM ~ B3TEMZ3 + ctq_total + B1PRSEX + B1PAGE_M2 + B1PF7A + B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W + yr_lapsed + (1 | M2FAMNUM)
    ##               npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## lmm_model1_em    6 2146.2 2174.8 -1067.1   2134.2                         
    ## lmm_model2_em   21 2064.5 2164.5 -1011.2   2022.5 111.73 15  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(lmm_model2_em)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: D3TEM ~ B3TEMZ3 + ctq_total + B1PRSEX + B1PAGE_M2 + B1PF7A +  
    ##     B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W +  
    ##     yr_lapsed + (1 | M2FAMNUM)
    ##    Data: full_df_no_invalid
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   2064.5   2164.5  -1011.2   2022.5      846 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.59358 -0.58463 -0.06758  0.49442  2.73694 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  M2FAMNUM (Intercept) 0.1773   0.4211  
    ##  Residual             0.4325   0.6576  
    ## Number of obs: 867, groups:  M2FAMNUM, 766
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)               0.304434   0.333249 866.912773   0.914  0.36122    
    ## B3TEMZ3                  -0.551837   0.032636 849.992882 -16.909  < 2e-16 ***
    ## ctq_total                -0.005618   0.002064 838.954544  -2.722  0.00663 ** 
    ## B1PRSEX2                  0.419433   0.060012 850.491432   6.989 5.59e-12 ***
    ## B1PAGE_M2                -0.208977   0.029007 771.104095  -7.204 1.39e-12 ***
    ## B1PF7A2                   0.005453   0.083792 855.710924   0.065  0.94812    
    ## B1PTSEI                   0.042185   0.027949 852.370428   1.509  0.13158    
    ## B1PA39former_smoker       0.056577   0.065274 856.849506   0.867  0.38632    
    ## B1PA39current_smoker     -0.131542   0.098024 866.368926  -1.342  0.17997    
    ## B4ALCOHformer_moderate    0.124529   0.106438 866.771236   1.170  0.24234    
    ## B4ALCOHformer_heavy       0.057098   0.118721 866.996112   0.481  0.63068    
    ## B4ALCOHcurrent_light      0.199109   0.137254 862.902079   1.451  0.14724    
    ## B4ALCOHcurrent_moderate   0.182314   0.088863 866.728064   2.052  0.04051 *  
    ## B4ALCOHcurrent_heavy     -0.015430   0.092378 864.002604  -0.167  0.86738    
    ## D1PB19-1                 -0.106769   0.089790 862.710701  -1.189  0.23473    
    ## D1PB191                  -0.001815   0.132757 808.835156  -0.014  0.98910    
    ## B4HMETMW                  0.053923   0.026875 842.614559   2.006  0.04513 *  
    ## B1SA11W1                 -0.005398   0.088242 866.896896  -0.061  0.95123    
    ## yr_lapsed                -0.047508   0.034338 866.548093  -1.384  0.16685    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Change in Executive Functioning

``` r
lmm_model2_ef <- lmer(D3TEF ~ B3TEFZ3 + ctq_total + B1PRSEX + B1PAGE_M2 + B1PF7A + B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W + yr_lapsed + (1|M2FAMNUM), REML = FALSE, data = full_df_no_invalid)
anova(lmm_model1_ef, lmm_model2_ef)
```

    ## Data: full_df_no_invalid
    ## Models:
    ## lmm_model1_ef: D3TEF ~ B3TEFZ3 + ctq_total + yr_lapsed + (1 | M2FAMNUM)
    ## lmm_model2_ef: D3TEF ~ B3TEFZ3 + ctq_total + B1PRSEX + B1PAGE_M2 + B1PF7A + B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W + yr_lapsed + (1 | M2FAMNUM)
    ##               npar     AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## lmm_model1_ef    6 1044.98 1073.6 -516.49  1032.98                         
    ## lmm_model2_ef   21  980.72 1080.8 -469.36   938.72 94.256 15  1.584e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(lmm_model2_ef)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: D3TEF ~ B3TEFZ3 + ctq_total + B1PRSEX + B1PAGE_M2 + B1PF7A +  
    ##     B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W +  
    ##     yr_lapsed + (1 | M2FAMNUM)
    ##    Data: full_df_no_invalid
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    980.7   1080.8   -469.4    938.7      846 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6674 -0.5141  0.0333  0.5596  3.1926 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  M2FAMNUM (Intercept) 0.04621  0.215   
    ##  Residual             0.12819  0.358   
    ## Number of obs: 867, groups:  M2FAMNUM, 766
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)              6.094e-01  1.785e-01  8.668e+02   3.414 0.000669 ***
    ## B3TEFZ3                 -4.560e-01  1.916e-02  8.530e+02 -23.801  < 2e-16 ***
    ## ctq_total                3.822e-04  1.105e-03  8.330e+02   0.346 0.729500    
    ## B1PRSEX2                -5.734e-02  3.057e-02  8.405e+02  -1.876 0.061056 .  
    ## B1PAGE_M2               -1.365e-01  1.619e-02  7.742e+02  -8.427  < 2e-16 ***
    ## B1PF7A2                 -3.607e-02  4.638e-02  8.517e+02  -0.778 0.436959    
    ## B1PTSEI                  4.136e-02  1.525e-02  8.568e+02   2.712 0.006821 ** 
    ## B1PA39former_smoker     -1.501e-02  3.493e-02  8.607e+02  -0.430 0.667521    
    ## B1PA39current_smoker    -3.637e-02  5.250e-02  8.669e+02  -0.693 0.488667    
    ## B4ALCOHformer_moderate   6.636e-02  5.696e-02  8.663e+02   1.165 0.244309    
    ## B4ALCOHformer_heavy      3.885e-02  6.361e-02  8.670e+02   0.611 0.541536    
    ## B4ALCOHcurrent_light    -4.798e-02  7.347e-02  8.645e+02  -0.653 0.513869    
    ## B4ALCOHcurrent_moderate  5.594e-02  4.760e-02  8.664e+02   1.175 0.240213    
    ## B4ALCOHcurrent_heavy     2.513e-02  4.945e-02  8.625e+02   0.508 0.611379    
    ## D1PB19-1                 6.514e-02  4.809e-02  8.636e+02   1.354 0.175947    
    ## D1PB191                  6.469e-02  7.120e-02  8.138e+02   0.909 0.363819    
    ## B4HMETMW                 1.752e-02  1.440e-02  8.442e+02   1.217 0.224116    
    ## B1SA11W1                -8.668e-02  4.725e-02  8.670e+02  -1.835 0.066904 .  
    ## yr_lapsed               -8.756e-02  1.838e-02  8.666e+02  -4.764 2.23e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

**Interpretation:**

Take the last table for example (dependent variable: change in EF):

                         Estimate   Std. Error       df   t value Pr(>|t|)  
                         

B1PAGE_M2 -1.365e-01 1.619e-02 7.742e+02 -8.427 \< 2e-16 \*** B1PTSEI
4.136e-02 1.525e-02 8.568e+02 2.712 0.006821 **

My interpretation is: age significantly predicts the change in executive
functioning, i.e. on average, 1 year increase of age will result in
0.137 more decline in Executive Functioning scores. Similarly, SES
significantly predicts the change in executive functioning, i.e. on
average, 1 unit increase in SES will result in 0.0414 more increase in
the Executive Functioning scores.

For those insignificant results, the interpretation will thus be: the
<independent variable> did not significantly predict the
<dependent variable>. For example, the ctq_total did not significantly
predict the change in executive functioning.

*Since we are also concerning the threat and deprivation*

**threat**

Change in Composite Scores & threat

``` r
lmm_model2_cs_thr <- lmer(D3TCOMP ~ B3TCOMPZ3 + thr_total + B1PRSEX + B1PAGE_M2 + B1PF7A + B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W + yr_lapsed + (1|M2FAMNUM), REML = FALSE, data = full_df_no_invalid)
```

``` r
summary(lmm_model2_cs_thr)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: D3TCOMP ~ B3TCOMPZ3 + thr_total + B1PRSEX + B1PAGE_M2 + B1PF7A +  
    ##     B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W +  
    ##     yr_lapsed + (1 | M2FAMNUM)
    ##    Data: full_df_no_invalid
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    872.7    972.8   -415.4    830.7      846 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6233 -0.5633  0.0069  0.5563  3.7358 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  M2FAMNUM (Intercept) 0.04662  0.2159  
    ##  Residual             0.10776  0.3283  
    ## Number of obs: 867, groups:  M2FAMNUM, 766
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)               0.404486   0.167052 866.980030   2.421  0.01567 *  
    ## B3TCOMPZ3                -0.510455   0.017218 862.702736 -29.647  < 2e-16 ***
    ## thr_total                -0.002532   0.001623 833.178095  -1.560  0.11912    
    ## B1PRSEX2                  0.018816   0.028920 839.932695   0.651  0.51546    
    ## B1PAGE_M2                -0.141374   0.015284 772.376302  -9.250  < 2e-16 ***
    ## B1PF7A2                  -0.071266   0.043501 853.394443  -1.638  0.10174    
    ## B1PTSEI                   0.044045   0.014340 851.118223   3.071  0.00220 ** 
    ## B1PA39former_smoker       0.008691   0.032806 854.649694   0.265  0.79114    
    ## B1PA39current_smoker     -0.024180   0.049201 865.756913  -0.491  0.62324    
    ## B4ALCOHformer_moderate    0.094823   0.053553 866.881296   1.771  0.07697 .  
    ## B4ALCOHformer_heavy       0.033474   0.059687 866.800884   0.561  0.57506    
    ## B4ALCOHcurrent_light      0.043528   0.068976 860.592588   0.631  0.52818    
    ## B4ALCOHcurrent_moderate   0.112406   0.044670 866.979247   2.516  0.01204 *  
    ## B4ALCOHcurrent_heavy      0.019321   0.046456 864.463343   0.416  0.67759    
    ## D1PB19-1                 -0.016770   0.045178 861.753215  -0.371  0.71059    
    ## D1PB191                   0.007896   0.066759 797.867724   0.118  0.90587    
    ## B4HMETMW                  0.020980   0.013512 837.053029   1.553  0.12086    
    ## B1SA11W1                 -0.096869   0.044456 866.840926  -2.179  0.02960 *  
    ## yr_lapsed                -0.050773   0.017267 866.298500  -2.941  0.00336 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Change in Episodic Memory

``` r
lmm_model2_em_thr <- lmer(D3TEM ~ B3TEMZ3 + thr_total + B1PRSEX + B1PAGE_M2 + B1PF7A + B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W + yr_lapsed + (1|M2FAMNUM), REML = FALSE, data = full_df_no_invalid)
```

``` r
summary(lmm_model2_em_thr)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: D3TEM ~ B3TEMZ3 + thr_total + B1PRSEX + B1PAGE_M2 + B1PF7A +  
    ##     B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W +  
    ##     yr_lapsed + (1 | M2FAMNUM)
    ##    Data: full_df_no_invalid
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   2066.2   2166.2  -1012.1   2024.2      846 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.56475 -0.58225 -0.06345  0.50102  2.74169 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  M2FAMNUM (Intercept) 0.1798   0.4240  
    ##  Residual             0.4314   0.6568  
    ## Number of obs: 867, groups:  M2FAMNUM, 766
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)               0.262337   0.332099 866.958721   0.790   0.4298    
    ## B3TEMZ3                  -0.552038   0.032669 849.796766 -16.898  < 2e-16 ***
    ## thr_total                -0.007686   0.003225 836.205924  -2.383   0.0174 *  
    ## B1PRSEX2                  0.420349   0.060245 849.035072   6.977 6.06e-12 ***
    ## B1PAGE_M2                -0.209627   0.029131 770.681304  -7.196 1.47e-12 ***
    ## B1PF7A2                   0.001498   0.083839 855.656929   0.018   0.9857    
    ## B1PTSEI                   0.043384   0.027961 851.816433   1.552   0.1211    
    ## B1PA39former_smoker       0.056923   0.065357 855.950398   0.871   0.3840    
    ## B1PA39current_smoker     -0.140244   0.097903 866.191083  -1.432   0.1524    
    ## B4ALCOHformer_moderate    0.123614   0.106590 866.755640   1.160   0.2465    
    ## B4ALCOHformer_heavy       0.052245   0.118772 866.992620   0.440   0.6601    
    ## B4ALCOHcurrent_light      0.192083   0.137360 862.377159   1.398   0.1624    
    ## B4ALCOHcurrent_moderate   0.177825   0.088885 866.745416   2.001   0.0457 *  
    ## B4ALCOHcurrent_heavy     -0.019083   0.092441 864.006471  -0.206   0.8365    
    ## D1PB19-1                 -0.106298   0.089872 862.599477  -1.183   0.2372    
    ## D1PB191                   0.001169   0.132930 807.635797   0.009   0.9930    
    ## B4HMETMW                  0.054044   0.026897 841.675991   2.009   0.0448 *  
    ## B1SA11W1                 -0.009162   0.088466 866.941523  -0.104   0.9175    
    ## yr_lapsed                -0.047880   0.034370 866.507250  -1.393   0.1640    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Change in Executive Functioning

``` r
lmm_model2_ef_thr <- lmer(D3TEF ~ B3TEFZ3 + thr_total + B1PRSEX + B1PAGE_M2 + B1PF7A + B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W + yr_lapsed + (1|M2FAMNUM), REML = FALSE, data = full_df_no_invalid)
```

``` r
summary(lmm_model2_ef_thr)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: D3TEF ~ B3TEFZ3 + thr_total + B1PRSEX + B1PAGE_M2 + B1PF7A +  
    ##     B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W +  
    ##     yr_lapsed + (1 | M2FAMNUM)
    ##    Data: full_df_no_invalid
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    980.7   1080.7   -469.3    938.7      846 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6681 -0.5127  0.0314  0.5595  3.1957 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  M2FAMNUM (Intercept) 0.04622  0.215   
    ##  Residual             0.12816  0.358   
    ## Number of obs: 867, groups:  M2FAMNUM, 766
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)              6.088e-01  1.777e-01  8.669e+02   3.426  0.00064 ***
    ## B3TEFZ3                 -4.560e-01  1.915e-02  8.530e+02 -23.810  < 2e-16 ***
    ## thr_total                7.259e-04  1.723e-03  8.313e+02   0.421  0.67370    
    ## B1PRSEX2                -5.787e-02  3.065e-02  8.395e+02  -1.888  0.05936 .  
    ## B1PAGE_M2               -1.362e-01  1.624e-02  7.754e+02  -8.388 2.33e-16 ***
    ## B1PF7A2                 -3.603e-02  4.637e-02  8.517e+02  -0.777  0.43736    
    ## B1PTSEI                  4.137e-02  1.525e-02  8.569e+02   2.713  0.00680 ** 
    ## B1PA39former_smoker     -1.525e-02  3.494e-02  8.604e+02  -0.437  0.66258    
    ## B1PA39current_smoker    -3.638e-02  5.239e-02  8.669e+02  -0.695  0.48755    
    ## B4ALCOHformer_moderate   6.591e-02  5.698e-02  8.662e+02   1.157  0.24771    
    ## B4ALCOHformer_heavy      3.877e-02  6.356e-02  8.670e+02   0.610  0.54207    
    ## B4ALCOHcurrent_light    -4.749e-02  7.346e-02  8.644e+02  -0.647  0.51813    
    ## B4ALCOHcurrent_moderate  5.602e-02  4.756e-02  8.663e+02   1.178  0.23919    
    ## B4ALCOHcurrent_heavy     2.525e-02  4.943e-02  8.624e+02   0.511  0.60952    
    ## D1PB19-1                 6.516e-02  4.809e-02  8.637e+02   1.355  0.17578    
    ## D1PB191                  6.411e-02  7.123e-02  8.146e+02   0.900  0.36837    
    ## B4HMETMW                 1.753e-02  1.440e-02  8.442e+02   1.217  0.22397    
    ## B1SA11W1                -8.751e-02  4.732e-02  8.670e+02  -1.849  0.06475 .  
    ## yr_lapsed               -8.753e-02  1.838e-02  8.666e+02  -4.763 2.24e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

**deprivation** Change in Composite Scores

``` r
lmm_model2_cs_dep <- lmer(D3TCOMP ~ B3TCOMPZ3 + dep_total + B1PRSEX + B1PAGE_M2 + B1PF7A + B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W + yr_lapsed + (1|M2FAMNUM), REML = FALSE, data = full_df_no_invalid)
```

``` r
summary(lmm_model2_cs_dep)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: D3TCOMP ~ B3TCOMPZ3 + dep_total + B1PRSEX + B1PAGE_M2 + B1PF7A +  
    ##     B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W +  
    ##     yr_lapsed + (1 | M2FAMNUM)
    ##    Data: full_df_no_invalid
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    871.3    971.4   -414.7    829.3      846 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6175 -0.5633  0.0008  0.5451  3.7347 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  M2FAMNUM (Intercept) 0.04726  0.2174  
    ##  Residual             0.10693  0.3270  
    ## Number of obs: 867, groups:  M2FAMNUM, 766
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)               0.422163   0.167576 866.958018   2.519  0.01194 *  
    ## B3TCOMPZ3                -0.510797   0.017200 863.455333 -29.698  < 2e-16 ***
    ## dep_total                -0.004489   0.002298 852.097987  -1.953  0.05110 .  
    ## B1PRSEX2                  0.016155   0.028687 841.282427   0.563  0.57349    
    ## B1PAGE_M2                -0.139902   0.015159 772.985996  -9.229  < 2e-16 ***
    ## B1PF7A2                  -0.069491   0.043500 852.892588  -1.598  0.11052    
    ## B1PTSEI                   0.043598   0.014333 850.477372   3.042  0.00242 ** 
    ## B1PA39former_smoker       0.007449   0.032741 854.304577   0.228  0.82009    
    ## B1PA39current_smoker     -0.019791   0.049293 865.660007  -0.401  0.68815    
    ## B4ALCOHformer_moderate    0.093428   0.053411 866.991164   1.749  0.08061 .  
    ## B4ALCOHformer_heavy       0.036017   0.059666 866.589311   0.604  0.54624    
    ## B4ALCOHcurrent_light      0.049515   0.068975 860.336584   0.718  0.47304    
    ## B4ALCOHcurrent_moderate   0.115339   0.044693 866.999064   2.581  0.01002 *  
    ## B4ALCOHcurrent_heavy      0.021810   0.046458 864.976932   0.469  0.63886    
    ## D1PB19-1                 -0.017176   0.045137 861.254396  -0.381  0.70365    
    ## D1PB191                   0.004329   0.066608 793.887695   0.065  0.94820    
    ## B4HMETMW                  0.020971   0.013498 836.264867   1.554  0.12066    
    ## B1SA11W1                 -0.098681   0.043991 866.633213  -2.243  0.02514 *  
    ## yr_lapsed                -0.050413   0.017254 866.247325  -2.922  0.00357 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Change in Episodic Memory

``` r
lmm_model2_em_dep <- lmer(D3TEM ~ B3TEMZ3 + dep_total + B1PRSEX + B1PAGE_M2 + B1PF7A + B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W + yr_lapsed + (1|M2FAMNUM), REML = FALSE, data = full_df_no_invalid)
```

``` r
summary(lmm_model2_em_dep)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: D3TEM ~ B3TEMZ3 + dep_total + B1PRSEX + B1PAGE_M2 + B1PF7A +  
    ##     B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W +  
    ##     yr_lapsed + (1 | M2FAMNUM)
    ##    Data: full_df_no_invalid
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   2064.9   2165.0  -1011.5   2022.9      846 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.59478 -0.58037 -0.07277  0.50228  2.72441 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  M2FAMNUM (Intercept) 0.1758   0.4193  
    ##  Residual             0.4342   0.6589  
    ## Number of obs: 867, groups:  M2FAMNUM, 766
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)               0.297101   0.333134 866.902817   0.892   0.3727    
    ## B3TEMZ3                  -0.550560   0.032638 849.564497 -16.869  < 2e-16 ***
    ## dep_total                -0.012043   0.004566 852.815932  -2.638   0.0085 ** 
    ## B1PRSEX2                  0.410141   0.059734 850.664939   6.866 1.27e-11 ***
    ## B1PAGE_M2                -0.204350   0.028877 771.753352  -7.077 3.32e-12 ***
    ## B1PF7A2                   0.006670   0.083836 855.063825   0.080   0.9366    
    ## B1PTSEI                   0.042028   0.027964 853.297156   1.503   0.1332    
    ## B1PA39former_smoker       0.052855   0.065254 857.304043   0.810   0.4182    
    ## B1PA39current_smoker     -0.131285   0.098105 866.431686  -1.338   0.1812    
    ## B4ALCOHformer_moderate    0.117247   0.106315 866.812647   1.103   0.2704    
    ## B4ALCOHformer_heavy       0.055935   0.118746 866.990607   0.471   0.6377    
    ## B4ALCOHcurrent_light      0.207462   0.137394 863.378918   1.510   0.1314    
    ## B4ALCOHcurrent_moderate   0.183597   0.088933 866.709995   2.064   0.0393 *  
    ## B4ALCOHcurrent_heavy     -0.013358   0.092437 864.003798  -0.145   0.8851    
    ## D1PB19-1                 -0.106120   0.089816 862.665245  -1.182   0.2377    
    ## D1PB191                  -0.011522   0.132745 808.938594  -0.087   0.9309    
    ## B4HMETMW                  0.053970   0.026885 843.075632   2.007   0.0450 *  
    ## B1SA11W1                 -0.018451   0.087549 866.917054  -0.211   0.8331    
    ## yr_lapsed                -0.047171   0.034349 866.567399  -1.373   0.1700    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Change in Executive Functioning

``` r
lmm_model2_ef_dep <- lmer(D3TEF ~ B3TEFZ3 + dep_total + B1PRSEX + B1PAGE_M2 + B1PF7A + B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W + yr_lapsed + (1|M2FAMNUM), REML = FALSE, data = full_df_no_invalid)
```

``` r
summary(lmm_model2_ef_dep)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: D3TEF ~ B3TEFZ3 + dep_total + B1PRSEX + B1PAGE_M2 + B1PF7A +  
    ##     B1PTSEI + B1PA39 + B4ALCOH + D1PB19 + B4HMETMW + B1SA11W +  
    ##     yr_lapsed + (1 | M2FAMNUM)
    ##    Data: full_df_no_invalid
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    980.8   1080.9   -469.4    938.8      846 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6666 -0.5156  0.0320  0.5591  3.1851 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  M2FAMNUM (Intercept) 0.04622  0.215   
    ##  Residual             0.12819  0.358   
    ## Number of obs: 867, groups:  M2FAMNUM, 766
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)              6.155e-01  1.784e-01  8.668e+02   3.450 0.000587 ***
    ## B3TEFZ3                 -4.562e-01  1.916e-02  8.531e+02 -23.812  < 2e-16 ***
    ## dep_total                4.102e-04  2.445e-03  8.486e+02   0.168 0.866797    
    ## B1PRSEX2                -5.647e-02  3.044e-02  8.399e+02  -1.855 0.063906 .  
    ## B1PAGE_M2               -1.370e-01  1.612e-02  7.744e+02  -8.498  < 2e-16 ***
    ## B1PF7A2                 -3.582e-02  4.639e-02  8.512e+02  -0.772 0.440303    
    ## B1PTSEI                  4.125e-02  1.525e-02  8.571e+02   2.705 0.006975 ** 
    ## B1PA39former_smoker     -1.463e-02  3.491e-02  8.607e+02  -0.419 0.675292    
    ## B1PA39current_smoker    -3.532e-02  5.253e-02  8.669e+02  -0.672 0.501503    
    ## B4ALCOHformer_moderate   6.732e-02  5.688e-02  8.665e+02   1.184 0.236906    
    ## B4ALCOHformer_heavy      3.964e-02  6.360e-02  8.670e+02   0.623 0.533345    
    ## B4ALCOHcurrent_light    -4.803e-02  7.353e-02  8.647e+02  -0.653 0.513809    
    ## B4ALCOHcurrent_moderate  5.641e-02  4.763e-02  8.664e+02   1.184 0.236583    
    ## B4ALCOHcurrent_heavy     2.538e-02  4.947e-02  8.627e+02   0.513 0.607996    
    ## D1PB19-1                 6.499e-02  4.809e-02  8.635e+02   1.351 0.176926    
    ## D1PB191                  6.544e-02  7.116e-02  8.129e+02   0.920 0.358082    
    ## B4HMETMW                 1.751e-02  1.441e-02  8.443e+02   1.215 0.224552    
    ## B1SA11W1                -8.472e-02  4.687e-02  8.670e+02  -1.808 0.071009 .  
    ## yr_lapsed               -8.754e-02  1.838e-02  8.666e+02  -4.762 2.24e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
