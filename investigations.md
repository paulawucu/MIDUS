investigations
================
Paula Wu
5/11/2022

**7.7 Updates** (all updates are located at the bottom of this
document): 1. Moderator effects of self-administered drugs are
investigated and reported. 2. I also explored another method called
“multivariate regression method”, which takes all three independent
variables as the outcome variables while fitting other dependent
variables as either predictors or covariates (details see below).

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

## Multivariate Regression Models

This method is also new to me, but I would like to want to explore the
possibilities of combining the three independent variables (Composite
scores, Episodic Memory, Executive Functioning) and see what are the
outcomes. Thus, here I introduce the concept of “multivariate
regression”, where multiple variables can be found on the LHS of the
equation.

For implementation, I used the library `brms`. Here is my
[reference](https://cran.r-project.org/web/packages/brms/vignettes/brms_multivariate.html)

``` r
# multivariate normal model
bform1 = bf(mvbind(D3TCOMP, D3TEM, D3TEF) ~ ctq_total + B3TCOMPZ3 + B3TEMZ3 + B3TEFZ3 + B1PAGE_M2 + B1PTSEI + B1PB1 + B1PF7A + D1PB19 + B1PRSEX + B1PA39 + B4HMETMW + B1SA11W + B4ALCOH + (1|p|M2FAMNUM)) + 
  set_rescor(TRUE) # we don't have missing values in predictors

fit1 = brm(bform1, data = full_df_no_invalid, chains = 3, cores = 4)
```

    ## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
    ## clang -mmacosx-version-min=10.13 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/Rcpp/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/unsupported"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/BH/include" -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/src/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppParallel/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DBOOST_NO_AUTO_PTR  -include '/Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/usr/local/include   -fPIC  -Wall -g -O2  -c foo.c -o foo.o
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Core:88:
    ## /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:1: error: unknown type name 'namespace'
    ## namespace Eigen {
    ## ^
    ## /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:16: error: expected ';' after top level declarator
    ## namespace Eigen {
    ##                ^
    ##                ;
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: 'complex' file not found
    ## #include <complex>
    ##          ^~~~~~~~~
    ## 3 errors generated.
    ## make: *** [foo.o] Error 1

``` r
fit = add_criterion(fit1, "loo")
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
    ## 1                D3TCOMP_Intercept 2.845193e-03  ** -0.2339104014 0.078388558
    ## 2                  D3TEM_Intercept 3.407572e-02   * -0.3294548917 0.155463786
    ## 3                  D3TEF_Intercept 2.246865e-04 *** -0.3164102021 0.085759412
    ## 4                D3TCOMP_ctq_total 7.945891e-02   . -0.0017798507 0.001014834
    ## 5                D3TCOMP_B3TCOMPZ3 2.283746e-18 *** -0.7297078846 0.083468463
    ## 6                  D3TCOMP_B3TEMZ3 8.094520e-01      0.0074477688 0.030886561
    ## 7                  D3TCOMP_B3TEFZ3 2.282133e-03  **  0.2306415940 0.075599737
    ## 8                D3TCOMP_B1PAGE_M2 1.428569e-17 *** -0.1333526405 0.015628253
    ## 9                  D3TCOMP_B1PTSEI 1.757704e-01      0.0215004646 0.015880473
    ## 10                   D3TCOMP_B1PB1 2.936000e-03  **  0.0202211259 0.006798479
    ## 11                 D3TCOMP_B1PF7A2 7.880707e-03  ** -0.1060164311 0.039898723
    ## 12                D3TCOMP_D1PB19M1 6.795080e-01     -0.0190515633 0.046114661
    ## 13                 D3TCOMP_D1PB191 9.316578e-01     -0.0059035865 0.068839090
    ## 14                D3TCOMP_B1PRSEX2 7.693085e-02   .  0.0548271186 0.030997165
    ## 15     D3TCOMP_B1PA39former_smoker 8.087188e-01      0.0078409038 0.032389826
    ## 16    D3TCOMP_B1PA39current_smoker 9.139607e-01     -0.0054153670 0.050121831
    ## 17                D3TCOMP_B4HMETMW 1.232728e-01      0.0208814789 0.013549013
    ## 18                D3TCOMP_B1SA11W1 3.194302e-02   * -0.0959407018 0.044725036
    ## 19  D3TCOMP_B4ALCOHformer_moderate 1.563166e-01      0.0782230526 0.055181114
    ## 20     D3TCOMP_B4ALCOHformer_heavy 8.263632e-01      0.0129668994 0.059110195
    ## 21    D3TCOMP_B4ALCOHcurrent_light 8.319082e-01      0.0152375313 0.071788832
    ## 22 D3TCOMP_B4ALCOHcurrent_moderate 2.980193e-02   *  0.0987120073 0.045432604
    ## 23    D3TCOMP_B4ALCOHcurrent_heavy 9.023354e-01      0.0057565971 0.046911537
    ## 24                 D3TEM_ctq_total 1.353100e-02   * -0.0050294287 0.002036636
    ## 25                 D3TEM_B3TCOMPZ3 8.635650e-01      0.0287272434 0.167176503
    ## 26                   D3TEM_B3TEMZ3 6.174926e-22 *** -0.6005550131 0.062385072
    ## 27                   D3TEM_B3TEFZ3 4.205560e-01      0.1218525525 0.151283754
    ## 28                 D3TEM_B1PAGE_M2 4.147255e-08 *** -0.1681164598 0.030653217
    ## 29                   D3TEM_B1PTSEI 9.537132e-01      0.0018047519 0.031092542
    ## 30                     D3TEM_B1PB1 2.274903e-01      0.0159685281 0.013231589
    ## 31                   D3TEM_B1PF7A2 4.752657e-01      0.0553184709 0.077483614
    ## 32                  D3TEM_D1PB19M1 3.081339e-01     -0.0928687886 0.091124172
    ## 33                   D3TEM_D1PB191 9.674842e-01     -0.0053547400 0.131360078
    ## 34                  D3TEM_B1PRSEX2 1.680873e-13 ***  0.4525963189 0.061393968
    ## 35       D3TEM_B1PA39former_smoker 3.227128e-01      0.0638355716 0.064552197
    ## 36      D3TEM_B1PA39current_smoker 2.733253e-01     -0.1069768652 0.097656807
    ## 37                  D3TEM_B4HMETMW 5.611875e-02   .  0.0510922158 0.026748278
    ## 38                  D3TEM_B1SA11W1 9.412204e-01     -0.0065797791 0.089234238
    ## 39    D3TEM_B4ALCOHformer_moderate 3.540975e-01      0.0997213788 0.107612517
    ## 40       D3TEM_B4ALCOHformer_heavy 8.697524e-01      0.0195681331 0.119337495
    ## 41      D3TEM_B4ALCOHcurrent_light 2.442437e-01      0.1677850279 0.144090128
    ## 42   D3TEM_B4ALCOHcurrent_moderate 1.094217e-01      0.1418369384 0.088603914
    ## 43      D3TEM_B4ALCOHcurrent_heavy 6.435700e-01     -0.0422834217 0.091381515
    ## 44                 D3TEF_ctq_total 6.864173e-01      0.0004472218 0.001107748
    ## 45                 D3TEF_B3TCOMPZ3 6.458784e-01     -0.0415425067 0.090408947
    ## 46                   D3TEF_B3TEMZ3 5.561289e-01      0.0201152672 0.034174698
    ## 47                   D3TEF_B3TEFZ3 1.046690e-07 *** -0.4312013599 0.081076882
    ## 48                 D3TEF_B1PAGE_M2 4.324565e-15 *** -0.1322982077 0.016863706
    ## 49                   D3TEF_B1PTSEI 1.199757e-01      0.0274628102 0.017662385
    ## 50                     D3TEF_B1PB1 4.721217e-02   *  0.0149663739 0.007542046
    ## 51                   D3TEF_B1PF7A2 6.074497e-03  ** -0.1196587229 0.043611654
    ## 52                  D3TEF_D1PB19M1 2.131364e-01      0.0614127039 0.049327978
    ## 53                   D3TEF_D1PB191 4.000969e-01      0.0636014399 0.075585695
    ## 54                  D3TEF_B1PRSEX2 4.911350e-02   * -0.0659931880 0.033539856
    ## 55       D3TEF_B1PA39former_smoker 6.282822e-01     -0.0169865958 0.035085681
    ## 56      D3TEF_B1PA39current_smoker 5.524907e-01     -0.0322586076 0.054304493
    ## 57                  D3TEF_B4HMETMW 2.211860e-01      0.0185256078 0.015142965
    ## 58                  D3TEF_B1SA11W1 5.538773e-02   . -0.0933144517 0.048707261
    ## 59    D3TEF_B4ALCOHformer_moderate 2.666843e-01      0.0678572212 0.061092422
    ## 60       D3TEF_B4ALCOHformer_heavy 6.535310e-01      0.0295509075 0.065835127
    ## 61      D3TEF_B4ALCOHcurrent_light 4.820748e-01     -0.0544865642 0.077509153
    ## 62   D3TEF_B4ALCOHcurrent_moderate 2.461485e-01      0.0581216437 0.050115442
    ## 63      D3TEF_B4ALCOHcurrent_heavy 7.096931e-01      0.0193874052 0.052079124
    ##    l_95_percent_ci u_95_percent_ci      rhat  bulk_ess tail_ess
    ## 1    -3.860800e-01   -0.0795740025 1.0012116 2249.3632 2341.992
    ## 2    -6.291004e-01   -0.0218210450 1.0008327 2841.9627 2488.170
    ## 3    -4.833034e-01   -0.1506559016 1.0012354 2604.1092 2448.943
    ## 4    -3.796933e-03    0.0001941245 0.9999404 3344.8725 2719.336
    ## 5    -8.919799e-01   -0.5679641346 1.0031372  710.2206 1087.156
    ## 6    -5.247329e-02    0.0696446565 1.0014711  813.5990 1651.797
    ## 7     8.349829e-02    0.3779445552 1.0023579  759.9366 1109.046
    ## 8    -1.644151e-01   -0.1034056380 1.0016249 2127.0283 2009.867
    ## 9    -9.847438e-03    0.0529356007 1.0038016 1898.0894 2126.518
    ## 10    7.174122e-03    0.0330554239 1.0000322 2598.1964 2687.848
    ## 11   -1.833019e-01   -0.0264922557 1.0016376 2406.9423 2134.029
    ## 12   -1.085408e-01    0.0688951321 1.0010415 1938.7413 2155.998
    ## 13   -1.386375e-01    0.1329012822 1.0038917 1918.4106 1956.860
    ## 14   -5.315170e-03    0.1147092534 1.0006233 2478.9695 2166.156
    ## 15   -5.674319e-02    0.0709516970 1.0001569 1391.9507 2342.152
    ## 16   -1.025367e-01    0.0898306719 1.0032214 1485.2778 2267.509
    ## 17   -5.257199e-03    0.0470802098 1.0005067 2599.9044 2125.672
    ## 18   -1.835077e-01   -0.0085023904 1.0009421 2405.6808 1989.499
    ## 19   -2.834802e-02    0.1818783982 0.9999115 1200.3908 1622.078
    ## 20   -1.019551e-01    0.1324547855 0.9995837 1201.6608 1725.136
    ## 21   -1.263785e-01    0.1509114649 1.0015026 1544.9083 1912.979
    ## 22    7.231266e-03    0.1856496703 1.0002890  968.7858 1507.603
    ## 23   -8.443258e-02    0.0933454043 0.9996510 1193.9823 2015.756
    ## 24   -9.054635e-03   -0.0010687525 1.0002499 3629.7272 2569.694
    ## 25   -3.028952e-01    0.3484421255 1.0028420 1015.3221 1663.814
    ## 26   -7.239021e-01   -0.4785584174 1.0024066 1055.8816 1867.205
    ## 27   -1.625920e-01    0.4127177782 1.0024235 1063.9283 1787.260
    ## 28   -2.290713e-01   -0.1082371233 1.0002801 3338.4087 2516.718
    ## 29   -5.916263e-02    0.0609109805 1.0011218 2456.6172 2500.747
    ## 30   -1.037560e-02    0.0411780582 0.9998846 3242.6297 2507.842
    ## 31   -9.640090e-02    0.2062078918 1.0007452 3564.7172 2713.389
    ## 32   -2.747504e-01    0.0852888177 1.0000421 3093.5565 2103.566
    ## 33   -2.536968e-01    0.2501918976 1.0001335 2899.2709 2496.672
    ## 34    3.316015e-01    0.5699951082 0.9994939 3217.1283 2354.105
    ## 35   -6.341476e-02    0.1862489437 1.0001555 2410.3080 2443.632
    ## 36   -3.017858e-01    0.0871188507 1.0017853 2921.3020 2305.762
    ## 37   -1.458286e-03    0.1053218905 1.0013122 3564.0580 2056.086
    ## 38   -1.827329e-01    0.1648690125 1.0004427 2945.5521 2293.065
    ## 39   -1.145019e-01    0.3093264999 1.0030419 1814.0635 2047.056
    ## 40   -2.197962e-01    0.2596959513 1.0026746 1936.5454 2380.892
    ## 41   -1.080074e-01    0.4513225123 1.0047948 1972.6156 2322.780
    ## 42   -3.059155e-02    0.3165580389 1.0034896 1441.6064 1807.556
    ## 43   -2.240122e-01    0.1401323749 1.0047621 1487.8330 2090.358
    ## 44   -1.739434e-03    0.0026364756 1.0004570 3450.7995 2584.894
    ## 45   -2.200895e-01    0.1356696043 1.0010213 1044.1125 1651.488
    ## 46   -4.492198e-02    0.0887903153 1.0005203 1288.2232 1832.821
    ## 47   -5.927704e-01   -0.2699564256 1.0010643 1014.5720 1398.972
    ## 48   -1.657690e-01   -0.1003309134 1.0019306 2525.3589 2260.988
    ## 49   -6.094109e-03    0.0614286899 1.0033711 2279.7544 2144.756
    ## 50    9.219127e-05    0.0296719940 1.0000096 3023.2061 2566.483
    ## 51   -2.025965e-01   -0.0302264515 1.0006707 2403.7725 1953.580
    ## 52   -3.889979e-02    0.1555845225 1.0001605 3071.2392 2204.278
    ## 53   -7.982794e-02    0.2151710297 1.0039951 2083.8846 1951.179
    ## 54   -1.308014e-01   -0.0024997939 0.9999488 2747.8548 2069.847
    ## 55   -8.418175e-02    0.0515675261 1.0003820 1493.7052 1971.461
    ## 56   -1.381430e-01    0.0739930320 1.0010032 1653.1809 1738.630
    ## 57   -1.172953e-02    0.0484066688 1.0005256 2794.9316 2040.227
    ## 58   -1.860024e-01    0.0028046632 1.0000285 2662.5927 2112.822
    ## 59   -5.359671e-02    0.1815968500 1.0009957 1625.5415 1944.228
    ## 60   -9.236472e-02    0.1601728411 1.0003255 1318.9443 2205.781
    ## 61   -2.015615e-01    0.0995596618 1.0001071 1924.1554 2111.383
    ## 62   -4.106690e-02    0.1550393450 1.0006954 1196.7345 1925.471
    ## 63   -8.415467e-02    0.1213510231 1.0004826 1353.0607 1775.581
