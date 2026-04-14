########################################################################
# Assessment of Reading Values
# 
# We have some data from a set of items designed to measure reading
# values.
# 
# This is the first time it has been used, so we need to do some basic
# item and measurement work.
# 
# Created: 2026-04-14
# Author: Serje Robidoux
# CHANGELOG:


# 0.0 Setup ####
include(readxl)
include(psych)
include(lavaan)
include(semPlot)

# 1.0 Data ####
xlData = read_excel('../270326WiL_HS_fulldata.xlsx', sheet="Data", na=c("NA", "ND"))
arvRaw = xlData %>% select(ParticipantID, Grade, Age, starts_with("ARV_Item"))

if (FALSE) {
  str(arvRaw)
  psych::describe(arvRaw %>% select(starts_with("ARV")))
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
# None of these items are reverse coded, that I can tell.


## 1.2 Scaled version of items
# Item SDs are all very similar, so I'm disinclined to scale them
arvZ = arvRaw %>% mutate(across(starts_with("ARV_"), ~scale(.)[,1]))

# 2.0 Item Analysis ####

## 2.1 Overall Test Cronbach's ####

(ARV15.alpha = alpha(arvZ %>% select(starts_with("ARV_Item"))))

# Outstanding. Alpha of .94, no weird items. Item 9 is the only item that is a
# bit weaker on r.drop, but it's really not an issue.


## 2.2 WiL structure ####
# near as I can tell, there aren't meant to be subscales here.
# subscaleItems = list(AM = c(13,14,15), AV=c(4, 8, 12), IN = c(1,5,9)
#                      , PE=c(2,6,10), VA = c(3, 7, 11))
# 
# subscaleAlphas = 
#   lapply(
#     subscaleItems
#     , function(x) alpha(arvZ %>% select(all_of(paste0("ARV_Item",x))))
#   )
# 
# lapply(subscaleAlphas, function(x) list(alpha=x$total, drop=x$alpha.drop))
# # FOR AM, item 14 is undermining the results
# # FOR AV: Item 4
# # IN: generally abysmal, but dropping Item 1 helps, 5 and 9 arv weird
# # PE: abysmal, dropping Item2 would help
# # VA: drop Item 7 
# # 
# # So all of the reverse coded Items arv messing with things. 2, 4, and 7 ruin
# # their subscales. 5 and 9 overwhelm item 1, so it's a bit odd but removing 1
# # might help.
# 
# ### 2.2.1 remove poor items ####
# revisedItems = list(AM = c(13, 15), AV=c(8, 12), IN = c(5, 9)
#                                     , PE=c(6, 10), VA = c(3, 11))
# 
# revisedAlphas = 
#   lapply(
#     subscaleItems
#     , function(x) alpha(arvZ %>% select(all_of(paste0("ARV_Item",x))))
#   )
# lapply(revisedAlphas, function(x) list(alpha=x$total, drop=x$item.stats))

# 3.0 EFA ####

# According to the "p-value" test, we end up with 4 factors.
factanal(arvZ %>% select(starts_with("ARV_Item")) %>% drop_na()
         , 4, rotation="promax")

# I'm using a promax rotation here because I am fine with correlations across the
# subscales, but would prefer if items loaded on very few factors.

#           Factor1 Factor2 Factor3 Factor4
# ARV_Item1   0.627                   0.162 #F1
# ARV_Item2   0.902                         #f1
# ARV_Item3   0.878                   0.126 #f1
# ARV_Item4   0.146                   0.857 #f4
# ARV_Item5           0.110   0.244   0.570 #f4
# ARV_Item6           0.571   0.351         #f2 (f3 weak)
# ARV_Item7   0.148           0.735         #f3
# ARV_Item8   0.431           0.568         #F1, F3
# ARV_Item9           1.121  -0.209         #F2!
# ARV_Item10  0.381   0.364   0.194  -0.110 #F1, F2

# So by this, 
# F1 is Items 1,2,3,(8), (10) # practical/independence
# F2 is Items 9, (6), (10) # "feelings"
# F3 is Items (6), (8) # (intrinsic?)
# F4 is Items 4, 5 # social value
# 
# Factor Correlations:
#         Factor1 Factor2 Factor3 Factor4
# Factor1   1.000   0.624   0.794   0.737
# Factor2   0.624   1.000   0.814   0.574
# Factor3   0.794   0.814   1.000   0.709
# Factor4   0.737   0.574   0.709   1.000
# 
# Hm. Much higher correlations than I'd usually like.

## 3.1 Varimax rotation ####
# Since we have very high correlations amongst the factors, this could produce
# quite different results.
factanal(arvZ %>% select(starts_with("ARV_Item")) %>% drop_na()
         , 4, rotation="varimax")

#           Factor1 Factor2 Factor3 Factor4
# ARV_Item1   0.665   0.305   0.393   0.140 
# ARV_Item2   0.790   0.277   0.292   0.111 
# ARV_Item3   0.776   0.207   0.374         
# ARV_Item4   0.445   0.247   0.786         
# ARV_Item5   0.402   0.399   0.624   0.206 
# ARV_Item6   0.287   0.711   0.237   0.242 
# ARV_Item7   0.517   0.460   0.387   0.426 
# ARV_Item8   0.640   0.448   0.309   0.360 
# ARV_Item9   0.199   0.911   0.224         
# ARV_Item10  0.491   0.533   0.192   0.179 

# yeah, the loadings are a lot less specific now - 
# F1 now gets everything but 6 & 9 - so it's kind of a general value (excl "fun")
# F2 is now the "fun" measure. 6& 9, + a bit of 7,8,10, 5,1
# F3 4 and 5 (social) with a bunch of other stuff.
# F4 7 8 (learning and thinking) +  a bit of other stuff.

## 3.3 Principal components ####
# Just a desperate attempt to find something sensible.

princomp(arvZ%>% select(starts_with("ARV_Item")) %>% drop_na()) %>% summary
# again, really only finds one component, maybe 2
princomp(arvZ%>% select(starts_with("ARV_Item")) %>% drop_na()) %>% loadings

# Yeah, "total value" and then "fun" vs other.

# 4.0 CFA ####
# Alright, let's hit the SEM and see if the there's a good case for the
# WiL structure.  I'll do this two ways, once with the reverse-coded, and once
# without.

## 4.1 Full set of items ARV ####

arv15model = '
AM =~ ARV_Item13 + ARV_Item14 + ARV_Item15
AV =~ ARV_Item4 + ARV_Item8 + ARV_Item12
IN =~ ARV_Item1 + ARV_Item5 + ARV_Item9
PE =~ ARV_Item2 + ARV_Item6 + ARV_Item10
VA =~ ARV_Item3 + ARV_Item7 + ARV_Item11

RE =~ AM + AV + IN + PE + VA

# Need to constrain the residual variance of Item 1 to be positive
# but left very small to recognize that this is what the model wanted (non-sig
# but negative)
ARV_Item1~~.01*ARV_Item1
'

ARV15.sem = sem(arv15model, arvZ, orthogonal = T, std.lv=T, missing="fiml")
semPaths(ARV15.sem, whatLabel="est", intercept=F)

# Ok, so there's a lot of funny stuff happening here.
# moderately ok fits, SRMR/RMSEA in the .09ish range, CFI, AGFI, TLI around .8

## 4.2 Remove Reverse-Coded items ARV10 ####

arv10model = '
AM =~ ARV_Item13 + ARV_Item14 + ARV_Item15
AV =~ ARV_Item8 + ARV_Item12
IN =~ ARV_Item1
PE =~ ARV_Item6 + ARV_Item10
VA =~ ARV_Item3 + ARV_Item11

RE =~ AM + AV + IN + PE + VA


'

ARV10.sem = sem(arv10model, arvZ, orthogonal = T, std.lv=T, missing="fiml"
                , meanstructure = F)
semPaths(ARV10.sem, whatLabel="est", intercepts = F)

# This fits quite well.
# SRMR, RMSEA in the .06 range, TLI, CFI, AGFI all above .9
 

## 4.3 Use the structure implied by the WiL structure based reliabilities ####

arvWiLmodel = '
AM =~ ARV_Item13 + ARV_Item15
IN =~ ARV_Item5 + ARV_Item9
PE =~ ARV_Item6 + ARV_Item10
VA =~ ARV_Item3 + ARV_Item11

# For some reason, the AV component really breaks here which is consistent with
# the other models  where it produces a nonsignificant set of weights for the
# items, but then has a massive weight from RE which is also non-significant
#AV =~ ARV_Item8 + ARV_Item12

RE =~ AM + IN + PE + VA# + AV 

# Need to constrain the residual variance of Item 5 to be positive
# but left very small to recognize that this is what the model wanted (non-sig
# but negative)
ARV_Item5~~.01*ARV_Item5
'

ARVWiL.sem = sem(arvWiLmodel, arvZ, std.lv=T, missing="fiml")
semPaths(ARVWiL.sem, whatLabel="est", intercepts = F)

# This fits quite well.
# SRMR, RMSEA in the .06 range, TLI, CFI, AGFI all above .9

## 4.4 what about the EFA model ####

## Each item is loaded om the factor where it was most strongly loaded,
## except Items 6, 8, 10 which are loaded on two factors each.
arvEFAmodel = '
F1 =~ ARV_Item1 + ARV_Item2 + ARV_Item3 + ARV_Item8 + ARV_Item10
F2 =~ ARV_Item6 + ARV_Item9 + ARV_Item10
F3 =~ ARV_Item7 + ARV_Item8 + ARV_Item6
F4 =~ ARV_Item4 + ARV_Item5

RE =~ F1 + F2 + F3 + F4
'

ARVEFA.sem = sem(arvEFAmodel, arvZ, std.lv = T, missing="fiml")
summary(ARVEFA.sem)
semPaths(ARVEFA.sem, whatLabel="est", intercept = F)

# Comparv that to the 15 item WiL structure
# anova(ARV10.sem, ARVEFA.sem)
cbind(ARV10=fitmeasures(ARV10.sem), ARVEFA=fitmeasures(ARVEFA.sem)) %>% round(3)

# So the EFA "promax" structure fits super well.

### 4.4.1 EFA with perfect separation ####
# how do things fit if each item is only allowed to load on a single factor

arvEFApure = '
F1 =~ ARV_Item1 + ARV_Item2 + ARV_Item3 + ARV_Item10
F2 =~ ARV_Item6 + ARV_Item9
F3 =~ ARV_Item7 + ARV_Item8
F4 =~ ARV_Item4 + ARV_Item5

RE =~ F1 + F2 + F3 + F4
'

ARVEFApure.sem = sem(arvEFApure, arvZ, std.lv = T, missing="fiml")
summary(ARVEFApure.sem)
semPaths(ARVEFApure.sem, whatLabel="est", intercept = F)

anova(ARVEFApure.sem, ARVEFA.sem)

# ok, os that does significantly damage the fit.
cbind(
  Pure=fitmeasures(ARVEFApure.sem, fit.measures = c("cfi", "tli", "agfi", "srmr", "rmsea"))
  , Flex=fitmeasures(ARVEFA.sem, fit.measures = c("cfi", "tli", "agfi", "srmr", "rmsea"))
  ) %>% round(3)

# RMSEA becomes unappy.