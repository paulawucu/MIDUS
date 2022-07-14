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
         B1SA62J = ifelse(B1SA62J == 2, 0, 1)
  )
#write.csv(full_df_no_invalid, "./data/full_df.csv")


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
   mutate_each_(funs(scale(.)), c(6,10,25:30, 37))
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
-   education
-   (imputed) SES
-   changes in marital status
-   changes in stroke
-   smoking, drug use, alcohol consumption

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

![](investigations_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
plot(lmm2, main = "Change in Episodic Memory: resid vs. fitted")
```

![](investigations_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
plot(lmm3, main = "Change in Executive Function: resid vs. fitted")
```

![](investigations_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

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

## Multivariate Multilevel Regression Models

This method is also new to me, but I would like to want to explore the
possibilities of combining the three independent variables (Composite
scores, Episodic Memory, Executive Functioning) and see what are the
outcomes. Thus, here I introduce the concept of “multivariate multilevel
regression”, where multiple variables can be found on the LHS of the
equation (i.e. multiple independent variables).

For implementation, I used the library `brms`. Here is my
[reference](https://cran.r-project.org/web/packages/brms/vignettes/brms_multivariate.html)

``` r
# multivariate normal model
bform1 = bf(mvbind(D3TCOMP, D3TEM, D3TEF) ~ ctq_total + B3TCOMPZ3 + B3TEMZ3 + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + (1|p|M2FAMNUM)) + 
  set_rescor(TRUE) # we don't have missing values in predictors

fit1 = brm(bform1, data = full_df_no_invalid, chains = 3, cores = 4)
fit = add_criterion(fit1, "loo")
```

``` r
#saveRDS(fit, "./results/fit.rds")
fit = readRDS("./results/fit.rds")
sum_fit = summary(fit)
```

“p-value” in multivariate models’ results? Here is some very primitive
way of calculating the p-value (I will explore more).

``` r
sum_fit$fixed %>% 
  rownames_to_column() %>% 
  janitor::clean_names() %>% 
  mutate(p_val = 2*pnorm(-abs(estimate/est_error), mean = 0, sd = 1)) %>%
  mutate(sig = case_when(p_val < 0.001 ~ "***",
                         p_val < 0.01 ~ "**",
                         p_val < 0.05 ~ "*",
                         p_val < 0.1 ~ ".",
                         p_val >= 0.1 ~ "")) %>% 
  select(rowname, p_val, sig, everything())
```

    ##                            rowname        p_val sig      estimate   est_error
    ## 1                D3TCOMP_Intercept 2.526307e-03  ** -0.2353638089 0.077930583
    ## 2                  D3TEM_Intercept 3.306428e-02   * -0.3322727131 0.155901294
    ## 3                  D3TEF_Intercept 1.753800e-04 *** -0.3161855473 0.084269614
    ## 4                D3TCOMP_ctq_total 9.339860e-02   . -0.0017713866 0.001055819
    ## 5                D3TCOMP_B3TCOMPZ3 3.434017e-19 *** -0.7228967842 0.080736169
    ## 6                  D3TCOMP_B3TEMZ3 8.578900e-01      0.0053744423 0.030014637
    ## 7                  D3TCOMP_B3TEFZ3 2.054530e-03  **  0.2233442494 0.072461799
    ## 8                D3TCOMP_B1PAGE_M2 4.636804e-19 *** -0.1343070107 0.015055791
    ## 9                  D3TCOMP_B1PTSEI 1.826495e-01      0.0213984141 0.016057142
    ## 10                   D3TCOMP_B1PB1 2.976242e-03  **  0.0203503200 0.006851542
    ## 11                 D3TCOMP_B1PF7A2 9.516553e-03  ** -0.1061752512 0.040948176
    ## 12                D3TCOMP_D1PB19M1 6.471680e-01     -0.0210676495 0.046029385
    ## 13                 D3TCOMP_D1PB191 9.234111e-01     -0.0064158001 0.066735407
    ## 14                D3TCOMP_B1PRSEX2 7.691643e-02   .  0.0555899602 0.031426912
    ## 15     D3TCOMP_B1PA39former_smoker 7.906635e-01      0.0086385053 0.032542980
    ## 16    D3TCOMP_B1PA39current_smoker 8.976589e-01     -0.0064354088 0.050034571
    ## 17                D3TCOMP_B4HMETMW 1.281215e-01      0.0211613869 0.013907770
    ## 18                D3TCOMP_B1SA11W1 3.237252e-02   * -0.0962954522 0.045002492
    ## 19  D3TCOMP_B4ALCOHformer_moderate 1.479877e-01      0.0768355326 0.053111784
    ## 20     D3TCOMP_B4ALCOHformer_heavy 7.956704e-01      0.0157701967 0.060899511
    ## 21    D3TCOMP_B4ALCOHcurrent_light 8.256851e-01      0.0156822970 0.071205863
    ## 22 D3TCOMP_B4ALCOHcurrent_moderate 2.868330e-02   *  0.0988799613 0.045195823
    ## 23    D3TCOMP_B4ALCOHcurrent_heavy 8.987127e-01      0.0059569148 0.046798840
    ## 24                 D3TEM_ctq_total 1.436979e-02   * -0.0050415081 0.002059537
    ## 25                 D3TEM_B3TCOMPZ3 8.161332e-01      0.0385422673 0.165758045
    ## 26                   D3TEM_B3TEMZ3 1.813174e-22 *** -0.6045891614 0.061998024
    ## 27                   D3TEM_B3TEFZ3 4.540495e-01      0.1118093832 0.149341804
    ## 28                 D3TEM_B1PAGE_M2 3.841528e-08 *** -0.1690583328 0.030749137
    ## 29                   D3TEM_B1PTSEI 9.894500e-01      0.0004269643 0.032289889
    ## 30                     D3TEM_B1PB1 2.411435e-01      0.0161712981 0.013796465
    ## 31                   D3TEM_B1PF7A2 5.174442e-01      0.0521609191 0.080583530
    ## 32                  D3TEM_D1PB19M1 2.825847e-01     -0.0980599675 0.091258368
    ## 33                   D3TEM_D1PB191 9.681578e-01     -0.0054199279 0.135773678
    ## 34                  D3TEM_B1PRSEX2 4.891982e-13 ***  0.4533777200 0.062722884
    ## 35       D3TEM_B1PA39former_smoker 3.586126e-01      0.0601113557 0.065479921
    ## 36      D3TEM_B1PA39current_smoker 2.677765e-01     -0.1097913592 0.099071998
    ## 37                  D3TEM_B4HMETMW 6.635425e-02   .  0.0512022184 0.027887568
    ## 38                  D3TEM_B1SA11W1 9.389037e-01     -0.0066825863 0.087185532
    ## 39    D3TEM_B4ALCOHformer_moderate 3.293925e-01      0.1030966168 0.105703442
    ## 40       D3TEM_B4ALCOHformer_heavy 8.083661e-01      0.0287069416 0.118362333
    ## 41      D3TEM_B4ALCOHcurrent_light 2.063561e-01      0.1718411197 0.135988026
    ## 42   D3TEM_B4ALCOHcurrent_moderate 9.341795e-02   .  0.1485228492 0.088530972
    ## 43      D3TEM_B4ALCOHcurrent_heavy 6.960384e-01     -0.0354209853 0.090666408
    ## 44                 D3TEF_ctq_total 6.975250e-01      0.0004486804 0.001154418
    ## 45                 D3TEF_B3TCOMPZ3 6.882921e-01     -0.0357442275 0.089099086
    ## 46                   D3TEF_B3TEMZ3 5.727972e-01      0.0186002851 0.032982931
    ## 47                   D3TEF_B3TEFZ3 5.042881e-08 *** -0.4375142626 0.080280913
    ## 48                 D3TEF_B1PAGE_M2 5.784265e-16 *** -0.1330498623 0.016438539
    ## 49                   D3TEF_B1PTSEI 1.113335e-01      0.0279842777 0.017575547
    ## 50                     D3TEF_B1PB1 4.396941e-02   *  0.0150001973 0.007446549
    ## 51                   D3TEF_B1PF7A2 6.089280e-03  ** -0.1194286937 0.043540480
    ## 52                  D3TEF_D1PB19M1 2.240996e-01      0.0604092767 0.049690986
    ## 53                   D3TEF_D1PB191 3.967581e-01      0.0623493748 0.073575064
    ## 54                  D3TEF_B1PRSEX2 4.402568e-02   * -0.0669332675 0.033236534
    ## 55       D3TEF_B1PA39former_smoker 6.702227e-01     -0.0150605492 0.035366492
    ## 56      D3TEF_B1PA39current_smoker 5.607828e-01     -0.0318791999 0.054805474
    ## 57                  D3TEF_B4HMETMW 1.955925e-01      0.0189868425 0.014670592
    ## 58                  D3TEF_B1SA11W1 5.947240e-02   . -0.0933633632 0.049537917
    ## 59    D3TEF_B4ALCOHformer_moderate 2.589088e-01      0.0660697154 0.058521913
    ## 60       D3TEF_B4ALCOHformer_heavy 6.463397e-01      0.0310802770 0.067734713
    ## 61      D3TEF_B4ALCOHcurrent_light 4.833921e-01     -0.0542580553 0.077416700
    ## 62   D3TEF_B4ALCOHcurrent_moderate 2.445258e-01      0.0569434624 0.048931061
    ## 63      D3TEF_B4ALCOHcurrent_heavy 7.267603e-01      0.0178758627 0.051155987
    ##    l_95_percent_ci u_95_percent_ci      rhat  bulk_ess  tail_ess
    ## 1    -0.3868445085   -0.0851840410 1.0025516 1418.5187 1965.7418
    ## 2    -0.6379399445   -0.0188528174 1.0006016 1849.6252 1854.4627
    ## 3    -0.4788773248   -0.1505380853 1.0036426 1454.4122 2035.1499
    ## 4    -0.0038968415    0.0002908402 1.0002029 2834.6699 2565.5354
    ## 5    -0.8825137098   -0.5687637339 1.0010028  571.5610 1067.0410
    ## 6    -0.0529724825    0.0631048272 1.0031760  677.8950 1331.2487
    ## 7     0.0826995987    0.3670187391 1.0021691  612.2485 1144.3236
    ## 8    -0.1640534721   -0.1062973717 1.0005362 1478.0814 2016.2773
    ## 9    -0.0097373898    0.0531125087 1.0013847 1325.8060 1799.5427
    ## 10    0.0064741791    0.0332172500 1.0022661 1700.6468 1861.8368
    ## 11   -0.1850422233   -0.0258428042 1.0011562 1512.4140 1910.3766
    ## 12   -0.1118187556    0.0699774932 1.0075472 1199.4117 1759.7103
    ## 13   -0.1382187852    0.1237069144 1.0005003 1478.2985 2197.2758
    ## 14   -0.0062148040    0.1170988007 1.0001391 1466.7474 1921.5226
    ## 15   -0.0551935469    0.0717038725 1.0031590  875.0276 1436.2023
    ## 16   -0.1039356412    0.0893014397 1.0013595  940.8219 1367.9182
    ## 17   -0.0059427295    0.0483473890 1.0011538 1608.3831 1816.6643
    ## 18   -0.1844699931   -0.0107399822 1.0006455 1746.2215 1824.7351
    ## 19   -0.0267198539    0.1794099739 1.0028058  869.2644 1506.1998
    ## 20   -0.1067089488    0.1323013126 1.0014684  806.1402 1621.9641
    ## 21   -0.1274534144    0.1521711696 1.0021093 1000.4936 1582.9070
    ## 22    0.0104049902    0.1846458244 1.0024858  628.7646 1264.1200
    ## 23   -0.0842897349    0.0980203293 1.0028534  661.5278 1507.6225
    ## 24   -0.0090795507   -0.0010204362 0.9993518 2865.8540 2431.8816
    ## 25   -0.2803103920    0.3756887786 1.0023197  775.9178 1263.3638
    ## 26   -0.7258269624   -0.4853837224 1.0017066  901.7512 1468.9630
    ## 27   -0.1807829686    0.4061446514 1.0018206  806.2353 1177.7766
    ## 28   -0.2303746705   -0.1097410372 1.0001325 2204.9642 2398.6267
    ## 29   -0.0600826471    0.0639995134 0.9998737 1879.1938 2116.9354
    ## 30   -0.0115975798    0.0432208645 1.0003125 1752.6215 1798.3503
    ## 31   -0.1043960903    0.2074704868 1.0001394 2366.0080 2210.1158
    ## 32   -0.2721672178    0.0800913064 1.0038268 1771.4992 2065.8019
    ## 33   -0.2723241024    0.2548482315 1.0021144 2074.8116 2657.1259
    ## 34    0.3294790191    0.5793314349 0.9998441 1988.1505 1922.4937
    ## 35   -0.0623569232    0.1901909535 1.0005393 1558.9019 2121.1261
    ## 36   -0.3040520989    0.0848546966 0.9998371 1653.3373 2042.9880
    ## 37   -0.0044310402    0.1035686060 1.0022187 2357.8970 2344.1340
    ## 38   -0.1756195881    0.1633648394 1.0005799 2637.8141 2467.8506
    ## 39   -0.0957838310    0.3106821535 1.0010345 1159.2140 1865.6180
    ## 40   -0.1964395079    0.2642173443 1.0013565 1354.6035 1900.5565
    ## 41   -0.0942592662    0.4343869775 1.0005303 1441.8953 1699.3040
    ## 42   -0.0299655601    0.3155221167 1.0011088 1026.2811 1710.5029
    ## 43   -0.2169291493    0.1433313441 1.0003236 1045.7550 1795.6762
    ## 44   -0.0017560628    0.0026625040 1.0007567 2940.9527 2570.9000
    ## 45   -0.2116664308    0.1388277087 1.0007780  613.2384 1299.5007
    ## 46   -0.0463148621    0.0840633590 1.0010734  727.0812 1413.9460
    ## 47   -0.5938296147   -0.2752274161 1.0004319  648.4175 1329.6297
    ## 48   -0.1654845639   -0.1009618833 1.0000748 1744.8205 1859.3539
    ## 49   -0.0068565669    0.0625857662 1.0001281 1841.1399 1806.4089
    ## 50    0.0006120923    0.0294078137 1.0012802 1850.2850 2233.3094
    ## 51   -0.2038929667   -0.0349011580 1.0015901 1641.0224 1782.3408
    ## 52   -0.0365779665    0.1576742255 1.0046217 1501.2728 1925.1134
    ## 53   -0.0824747528    0.2049930385 1.0002557 1688.5758 2123.0496
    ## 54   -0.1316807880   -0.0021820705 1.0001482 1825.0096 2309.9386
    ## 55   -0.0847157240    0.0549548414 1.0005960 1079.8865 1589.8105
    ## 56   -0.1404179649    0.0738313060 1.0018553 1189.7750 1763.3088
    ## 57   -0.0094765944    0.0474161753 1.0017597 1945.8320 2310.4474
    ## 58   -0.1896096116    0.0018836438 1.0000170 2058.2997 1984.4168
    ## 59   -0.0510507504    0.1769101812 1.0025763  878.1562 1827.5946
    ## 60   -0.1041327480    0.1612053505 1.0010788  863.3733 1587.1740
    ## 61   -0.2021519890    0.0964239352 1.0010684 1092.0292 1837.6350
    ## 62   -0.0397673901    0.1528343485 1.0022164  687.0957  961.4746
    ## 63   -0.0814658296    0.1197456013 1.0032079  706.4619 1342.9127

May try
[MCMCglmm](http://cran.nexr.com/web/packages/MCMCglmm/vignettes/CourseNotes.pdf)
packages since `brms` takes a long time. Need to tune the prior
distribution for the function to work properly.

``` r
# settings
full_df_no_invalid_mcmc = as.data.frame(full_df_no_invalid) # need dataframe instead of tibble
family_set = c("gaussian", "gaussian", "gaussian")  # response variable categories

# fitting the model
fit_mcmc = MCMCglmm(cbind(D3TCOMP, D3TEM, D3TEF) ~ ctq_total + B3TCOMPZ3 + B3TEMZ3 + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH, random = ~us(trait):M2FAMNUM, rcov = ~us(trait):units, data = full_df_no_invalid_mcmc, family = family_set, verbose = FALSE)
```
