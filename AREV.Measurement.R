########################################################################
# Assessment of Reading Engagement & Values
# 
# We have some data from a set of items designed to measure reading
# values and reading engagement.
# 
# Many of the concepts overlap with those used by Reading Engagement researchers
# in other areas (outside cognitive science)
# 
# There are separate analyses of the measurement models for ARE and ARV in other
# scripts (ARE.Measurement.R and ARV.Measurement.R). Here I look at what
# structure might exist across the set of items.
# 
# Created: 2026-04-15
# Author: Serje Robidoux
# CHANGELOG:


# 0.0 Setup ####
include(readxl)
include(psych)
include(lavaan)
include(semPlot)

# 1.0 Data ####
xlData = read_excel('../270326WiL_HS_fulldata.xlsx', sheet="Data"
                    , na=c("NA", "ND"))
arevRaw = xlData %>% 
  select(ParticipantID, Grade, Age, starts_with("ARV_Item")
         , starts_with("ARE_Item")) %>% 
  mutate(ARE_Item15 = factor(ARE_Item15))

if (FALSE) {
  str(arevRaw)
  psych::describe(arevRaw %>% select(matches("AR[EV]"),-ARE_Item15))
  # on the whole these are pretty well - behaved Nice SDs, mostly good means
  # skews and kurtoses
  # Skewed Items (all positive):
  #   2,3 (8) all negative. - all have pretty high means (4.1, 4.0, 3.8)
  # Kurtosis: 2,3,-6,-9
  # Items 2 & 3 seem particularly non-normal, but not insanely.
  arvRaw %>% ggplot(aes(x=ARV_Item2)) + geom_histogram()
  arvRaw %>% ggplot(aes(x=ARV_Item3)) + geom_histogram()
  
  (cor(arvRaw %>% select(where(is.numeric), -Grade, -Age), use="pairwise.complete")-
      cor(arvRaw %>% select(where(is.numeric), -Grade, -Age), use="pairwise.complete"
          , method="spearman")) %>% 
    psych::describe()
  # None of the items seem to be overly sensitive to whether or not we use 
  # pearson or spearman.
  
}

## 1.1 Reverse coding ####
# Some items are clearly phrased with a reverse coding scheme.
# I want to code everything so that higher scores indicate "more/better engagement"

# I think this means Items: 2, 4, 5, 7, 9 Confirmed with AS
arevRaw = arevRaw %>% 
  mutate(across(c(ARE_Item2, ARE_Item4, ARE_Item5, ARE_Item7, ARE_Item9), ~ -.+2))

## 1.2 Scaled version of items
# Item SDs are all very similar, so I'm disinclined to scale them
arevZ = arevRaw %>% select(-ARE_Item15) %>% 
  mutate(across(matches("AR[EV]_"), ~scale(.)[,1]))

# 2.0 Item Analysis ####

## 2.1 Overall Test Cronbach's ####

(AREV.alpha = alpha(arevZ %>% select(contains("_Item"))))

# alpha of .88 - no really bad items, though ARE2, ARE5, ARE7, ARE9 do slightly
# harm the results. Lots of the ARE items have terrible r.drops
# AREV.alpha$item.stats %>% arrange(desc(r.drop)) %>% mutate(across(where(is.numeric), ~round(.,3)))
#                  n  raw.r  std.r  r.cor r.drop mean sd
# ARV_Item8      271  0.802  0.802  0.814  0.773    0  1
# ARV_Item7      274  0.785  0.785  0.795  0.752    0  1
# ARV_Item6      273  0.783  0.784  0.791  0.752    0  1
# ARV_Item5      272  0.716  0.714  0.719  0.673    0  1
# ARV_Item9      272  0.707  0.707  0.705  0.664    0  1
# ARV_Item2      272  0.705  0.704  0.712  0.662    0  1
# ARV_Item1      274  0.706  0.705  0.707  0.662    0  1
# ARV_Item10     275  0.700  0.698  0.692  0.653    0  1
# ARV_Item3      274  0.693  0.692  0.697  0.650    0  1
# ARV_Item4      272  0.662  0.662  0.664  0.615    0  1
# ARE_Item13     266  0.632  0.634  0.628  0.580    0  1
# ARE_Item12     275  0.583  0.582  0.568  0.526    0  1
# ARE_Item8      275  0.568  0.568  0.555  0.511    0  1
# ARE_Item11     275  0.564  0.565  0.550  0.508    0  1
# ARE_Item15_Num 266  0.513  0.516  0.488  0.454    0  1
# ARE_Item1      274  0.475  0.477  0.448  0.413    0  1
# ARE_Item10     273  0.469  0.468  0.430  0.403    0  1
# ### THINGS START TO LOOK BAD HERE
# ARE_Item3      268  0.402  0.403  0.370  0.335    0  1
# ARE_Item6      274  0.361  0.363  0.320  0.291    0  1
# ARE_Item4      275  0.332  0.333  0.290  0.261    0  1
# ARE_Item14     267  0.331  0.332  0.287  0.259    0  1
# ### THINGS ARE pretty ugly down here.
# ARE_Item2      274  0.167  0.166  0.110  0.088    0  1
# ARE_Item9      275  0.091  0.092  0.042  0.014    0  1
# ARE_Item5      272  0.083  0.083  0.028  0.005    0  1
# ARE_Item7      273 -0.079 -0.080 -0.143 -0.156    0  1

# These are the same items that had poor performance when looking at the ARE
# on it's own.

## 2.2 WiL structure ####

# 3.0 EFA ####

# According to the "p-value" test, we end up with 8 factors.
print((arev.fa=factanal(arevZ %>% select(contains("_Item")) %>% drop_na()
         , 8, rotation="promax")), cutoff=0)

# I'm using a promax rotation here because I am fine with correlations across the
# subscales, but would prefer if items loaded on very few factors.

#               Factor1 Factor2 Factor3 Factor4 Factor5 Factor6 Factor7 Factor8 Factor
# ARV_Item1       0.835  -0.044  -0.033   0.020   0.071   0.045  -0.007   0.094 1
# ARV_Item2       0.954   0.029  -0.098  -0.143   0.023  -0.057  -0.066  -0.002 1
# ARV_Item3       0.964   0.002  -0.195   0.026   0.023  -0.003  -0.017   0.058 1
# ARV_Item4       0.851  -0.085  -0.008  -0.037  -0.025   0.016   0.044   0.565 1
# ARV_Item5       0.677  -0.016   0.253  -0.041  -0.098   0.024   0.004   0.321 1
# ARV_Item6       0.265   0.222   0.614   0.054  -0.002  -0.114   0.006  -0.003 3
# ARV_Item7       0.708   0.071   0.247  -0.063   0.002  -0.057  -0.026   0.053 1
# ARV_Item8       0.819  -0.019   0.131  -0.016   0.078   0.005   0.008  -0.032 1
# ARV_Item9       0.219   0.045   0.677   0.067   0.008   0.011  -0.070   0.011 3
# ARV_Item10      0.512  -0.147   0.390   0.106  -0.023   0.139   0.122  -0.170 1
# ARE_Item1      -0.052   0.602   0.189  -0.316   0.059   0.062  -0.217  -0.072 2
# ARE_Item2       0.176   0.159  -0.101   0.231  -0.074  -0.032  -0.341  -0.044 -7
# ARE_Item3       0.022   0.343  -0.073   0.152  -0.005  -0.015   0.641   0.007 7
# ARE_Item4       0.191   0.086   0.017   0.434   0.046  -0.090  -0.113   0.078 4
# ARE_Item5      -0.007  -0.096  -0.014   0.672  -0.025  -0.013   0.198  -0.064 4
# ARE_Item6       0.001   0.006  -0.057  -0.005  -0.010   1.008  -0.008   0.011 6
# ARE_Item7       0.025  -0.207   0.149   0.159  -0.060   0.027  -0.357  -0.023 -7
# ARE_Item8      -0.109   0.721   0.034   0.055   0.069  -0.023   0.173   0.052 2
# ARE_Item9      -0.281   0.034   0.161   0.580   0.102   0.048  -0.098   0.001 4
# ARE_Item10      0.237   0.350  -0.166  -0.013   0.047   0.144   0.077  -0.004 2
# ARE_Item11      0.010   0.665   0.095   0.053  -0.224   0.075   0.361   0.033 7
# ARE_Item12      0.020   0.806   0.021  -0.033  -0.040  -0.106   0.109  -0.105 2
# ARE_Item13      0.076   0.089  -0.004   0.052   0.915  -0.013   0.036  -0.026 5
# ARE_Item14      0.050  -0.010   0.070  -0.090   0.375  -0.067   0.282   0.017 5
# ARE_Item15_Num -0.065   0.374   0.026   0.133   0.365   0.112  -0.056   0.050 5
# So by this, 
# F1 is Values other than "fun/Affect"
# F2 is Eng Items 1 8 10 12
# F3 is AffectValue
# F4 is Eng items 4 5 9 - distracted/avoidance
# F5 is Frequency
# F6 is Eng Item 6 - "read new words on your own"
# F7 is Eng Items -2, 3 -7 11 - messy
# F8 is SocialValues



# I think the basic story here is that values do separate from Engagement, so
# we should stick to separate models for them.