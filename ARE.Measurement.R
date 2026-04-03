########################################################################
# Assessment of Rading Engagement
# 
# We have some data from a set of items designed to measure reading
# engagement.
# 
# This is the first time it has been used, so we need to do some basic
# item and measurement work.
# 
# Created: 2026-04-02
# Author: Serje Robidoux
# CHANGELOG:


# 0.0 Setup ####
include(readxl)
include(psych)
include(lavaan)
include(semPlot)

# 1.0 Data ####
xlData = read_excel('270326WiL_HS_fulldata.xlsx', sheet="Data", na=c("NA", "ND"))
areRaw = xlData %>% select(ParticipantID, Grade, Age, starts_with("ARE_Item")) %>% 
  mutate(ARE_Item15 = factor(ARE_Item15))

if (FALSE) {
  str(areRaw)
  psych::describe(areRaw %>% select(starts_with("ARE"), -ARE_Item15))
  # Skewed Items (all positive):
  #   2, 13, 14, (4) (5)
  # Kurtosis: everything except 4,5,13
  #   all negative, except 2, and 14
  # Item 14 is alarmingly non-normal.
  areRaw %>% ggplot(aes(x=ARE_Item14)) + geom_histogram()
  # ok, there's no transformation that is going to make this normal.
  # Maybe we just accept that it's non-normal and treat it differently
  # Or that it's kind of a poisson distribution of some sort perhaps? Truncated?
  # Doesn't make a lot ot theoretical sense since there are only 7 days in a week
  # Could again be a mixture model. Like a "7-inflated" model. I guess that makes sense:
  # you have one population of kids who do this every single day as a routine, and another
  # that is not so much about routine but just does it more or less?
  # SOLVED: - this is about Shared Book Reading, which
  # is just something that 12-16 year olds don't do very much. I think we'd need
  # to know who they are reading with, because kids doing it a lot are likelty to be
  # badly in need of support, OR reading to their younger siblings (not at all
  # in need of support)

  (cor(areRaw %>% select(where(is.numeric), -Grade, -Age), use="pairwise.complete")-
  cor(areRaw %>% select(where(is.numeric), -Grade, -Age), use="pairwise.complete", method="spearman")) %>% psych::describe()
  # OK, so the correlation between Items 13 and 14 seems sensitive to this. Those are both the "frequency" measures.
  # They are interval scale though, but discrete.
  areRaw %>% ggplot(aes(x=ARE_Item13)) + geom_histogram()
  # Ok, well that's an odd one. Kind of normal-ish, but too many 0s. Zero-inflated normal?
  
# One thing is certain, I'm over thinking this stuff as far what is typically done.

}

## 1.1 Reverse coding ####
# Some items are clearly phrased with a reverse coding scheme.
# I want to code everything so that higher scores indicate "more/better engagement"

# I think this means Items: 2, 4, 5, 7, 9 Confirmed with AS
areRaw = areRaw %>% mutate(across(c(ARE_Item2, ARE_Item4, ARE_Item5, ARE_Item7, ARE_Item9), ~ -.+2))

## 1.2 Scaled version of items
areZ = areRaw %>% mutate(across(c(ARE_Item1:ARE_Item14, ARE_Item15_Num), ~scale(.)[,1])) %>% 
  select(-ARE_Item15) %>% rename(ARE_Item15=ARE_Item15_Num)

# 2.0 Item Analysis ####

## 2.1 Overall Test Cronbach's ####

(ARE15.alpha = alpha(areZ %>% select(starts_with("ARE_Item"))))

# Items 2, 4, 9, and 5 are wonky. Should they be reverse coded? Yup, but also
# 7, which is currently not showing up as wonky. Don't know what to make of that.
# Ok... so, now that I reverse code those items, the alpha gets worse, and 7
# is negatively correlated with the total. Also, 2, 4, 5, 7, and 9 have very 
# poor r.drops - worse than before the reverse coding.

# Were they entered as reverse coded? No. So the above analysis is correct.

# Overall alpha of .71, but there are several items that make things worse

### 2.1.1 What if we try a subset ####
# from the alpha.drop stuff, it looks like we might get slight improvement
# 2, 5, 7, 9 all (indvidually) reduce the fit.
# from the item.stats we get that those also have very poor r.drop.
# 
# This basically means that the reverse coded items just aren't working with the
# other items. (except 4 which just barely makes no difference, along with
# 6 and 14)

(ARE11.alpha = alpha(areZ %>% 
                       select(starts_with("ARE_Item"),
                              -ARE_Item2, -ARE_Item5, -ARE_Item7, -ARE_Item9)
                     )
)

# So the 11 item subscale gives us an overall measure with a reliability of .82
# Item 4 (the only reverse-coded item) is the only one that harms the alpha.
# It also has essentially no correlation with the total excluding it.

(ARE10.alpha = 
    alpha(areZ %>% 
            select(starts_with("ARE_Item")
                   ,-ARE_Item2, -ARE_Item5, -ARE_Item7, -ARE_Item9, -ARE_Item4)
    )
)

# This 10 item scale does a decent job if our goal is just to say "overall
# engagement, broadly defined"

## 2.2 WiL structure ####

subscaleItems = list(AM = c(13,14,15), AV=c(4, 8, 12), IN = c(1,5,9)
                     , PE=c(2,6,10), VA = c(3, 7, 11))

subscaleAlphas = 
  lapply(
    subscaleItems
    , function(x) alpha(areZ %>% select(all_of(paste0("ARE_Item",x))))
  )

lapply(subscaleAlphas, function(x) list(alpha=x$total, drop=x$alpha.drop))
# FOR AM, item 14 is undermining the results
# FOR AV: Item 4
# IN: generally abysmal, but dropping Item 1 helps, 5 and 9 are weird
# PE: abysmal, dropping Item2 would help
# VA: drop Item 7 
# 
# So all of the reverse coded Items are messing with things. 2, 4, and 7 ruin
# their subscales. 5 and 9 overwhelm item 1, so it's a bit odd but removing 1
# might help.

### 2.2.1 remove poor items ####
revisedItems = list(AM = c(13, 15), AV=c(8, 12), IN = c(5, 9)
                                    , PE=c(6, 10), VA = c(3, 11))

revisedAlphas = 
  lapply(
    subscaleItems
    , function(x) alpha(areZ %>% select(all_of(paste0("ARE_Item",x))))
  )
lapply(revisedAlphas, function(x) list(alpha=x$total, drop=x$item.stats))

# 3.0 EFA ####

# According to the "p-value" test, we end up with 4 factors.
factanal(areZ %>% select(starts_with("ARE_Item")) %>% drop_na()
         , 4, rotation="promax")

# I'm using a promax rotation here because I am fine with correlations across the
# subscales, but would prefer if items loaded on very few factors.

#               Factor1 Factor2 Factor3 Factor4
# ARE_Item1       0.664  -0.189   0.125  -0.198 # Mostly F1
# ARE_Item2       0.218   0.352  -0.121  -0.208 # F2, F1, F4 (all weak)
# ARE_Item3       0.241   0.112           0.701 # F4
# ARE_Item4       0.137   0.552                 # F2
# ARE_Item5      -0.139   0.601           0.235 # F2
# ARE_Item6       0.397  -0.194                 # F1 (weak)
# ARE_Item7      -0.122   0.239          -0.305 # F4 (weak)
# ARE_Item8       0.610   0.103   0.123   0.174 # F1
# ARE_Item9               0.643   0.107         # F2
# ARE_Item10      0.482                         # F1 (weak)
# ARE_Item11      0.678          -0.210   0.407 # F1, F4 (weak)
# ARE_Item12      0.714                         # F1
# ARE_Item13      0.137           0.923         # F3
# ARE_Item14             -0.115   0.442   0.194 # F3 (weakish)
# ARE_Item15_Num  0.393   0.187   0.327         # F1 F3 (both weak)

# So by this, 
# F1 is Items 1, (2), (6), 8, (10), (11), (15)
# F2 is Items (2), 4, 5, 9 (All the reverse coded items)
# F3 is Items 13, (14), (15)
# F4 is Items (2), 3, (7), (11)
# 
# Factor Correlations:
#   Factor1 Factor2 Factor3 Factor4
# Factor1  1.0000 -0.0868  0.5555   0.367
# Factor2 -0.0868  1.0000 -0.0215  -0.343
# Factor3  0.5555 -0.0215  1.0000   0.400
# Factor4  0.3674 -0.3431  0.4003   1.000

## 3.1 Varimax rotation ####
factanal(areZ %>% select(starts_with("ARE_Item")) %>% drop_na()
         , 4, rotation="varimax")

## 3.2 remove reverse coded ####
factanal(areZ %>% 
           select(starts_with("ARE_Item"), -ARE_Item2, -ARE_Item4
                  , -ARE_Item5, -ARE_Item7, -ARE_Item9) %>% 
           drop_na()
         , 3, rotation="promax")

# Unsurprisingly, this results in only 3 subscales.


## 3.3 Principal components ####
# Just a desperate attempt to find something sensible.

princomp(areZ%>% select(starts_with("ARE_Item")) %>% drop_na()) %>% summary
# again, no more than 4 components
princomp(areZ%>% select(starts_with("ARE_Item")) %>% drop_na()) %>% loadings

# 4.0 CFA ####
# Alright, let's hit the SEM and see if the there's a good case for the
# WiL structure.  I'll do this two ways, once with the reverse-coded, and once
# without.

## 4.1 Full set of items ARE15 ####

are15model = '
AM =~ ARE_Item13 + ARE_Item14 + ARE_Item15
AV =~ ARE_Item4 + ARE_Item8 + ARE_Item12
IN =~ ARE_Item1 + ARE_Item5 + ARE_Item9
PE =~ ARE_Item2 + ARE_Item6 + ARE_Item10
VA =~ ARE_Item3 + ARE_Item7 + ARE_Item11

RE =~ AM + AV + IN + PE + VA

# Need to constrain the residual variance of Item 1 to be positive
# but left very small to recognize that this is what the model wanted (non-sig
# but negative)
ARE_Item1~~.01*ARE_Item1
'

ARE15.sem = sem(are15model, areZ, orthogonal = T, std.lv=T, missing="fiml")
semPaths(ARE15.sem, whatLabel="est", intercept=F)

# Ok, so there's a lot of funny stuff happening here.
# moderately ok fits, SRMR/RMSEA in the .09ish range, CFI, AGFI, TLI around .8

## 4.2 Remove Reverse-Coded items ARE10 ####

are10model = '
AM =~ ARE_Item13 + ARE_Item14 + ARE_Item15
AV =~ ARE_Item8 + ARE_Item12
IN =~ ARE_Item1
PE =~ ARE_Item6 + ARE_Item10
VA =~ ARE_Item3 + ARE_Item11

RE =~ AM + AV + IN + PE + VA


'

ARE10.sem = sem(are10model, areZ, orthogonal = T, std.lv=T, missing="fiml"
                , meanstructure = F)
semPaths(ARE10.sem, whatLabel="est", intercepts = F)

# This fits quite well.
# SRMR, RMSEA in the .06 range, TLI, CFI, AGFI all above .9
 

## 4.3 Use the structure implied by the WiL structure based reliabilities ####

areWiLmodel = '
AM =~ ARE_Item13 + ARE_Item15
IN =~ ARE_Item5 + ARE_Item9
PE =~ ARE_Item6 + ARE_Item10
VA =~ ARE_Item3 + ARE_Item11

# For some reason, the AV component really breaks here which is consistent with
# the other models  where it produces a nonsignificant set of weights for the
# items, but then has a massive weight from RE which is also non-significant
#AV =~ ARE_Item8 + ARE_Item12

RE =~ AM + IN + PE + VA# + AV 

# Need to constrain the residual variance of Item 5 to be positive
# but left very small to recognize that this is what the model wanted (non-sig
# but negative)
ARE_Item5~~.01*ARE_Item5
'

AREWiL.sem = sem(areWiLmodel, areZ, std.lv=T, missing="fiml")
semPaths(AREWiL.sem, whatLabel="est", intercepts = F)

# This fits quite well.
# SRMR, RMSEA in the .06 range, TLI, CFI, AGFI all above .9

## 4.4 what about the EFA model ####
## This is based on the 10 item EFA
## Each item is loaded om the factor where it was most strongly loaded,
## except Items 6 & 15 which are loaded on two factors each.
areEFAmodel = '
F1 =~ ARE_Item1 + ARE_Item6 + ARE_Item8 + ARE_Item10 + ARE_Item12 + ARE_Item15
F2am =~ ARE_Item13 + ARE_Item14 + ARE_Item15
F3 =~ ARE_Item3+ARE_Item6+ARE_Item11

#RE =~ F1 + F2am + F3

# Need to constrain the residual variance of Item 13 to be positive
# but left very small to recognize that this is what the model wanted (non-sig
# but negative)
ARE_Item13~~.01*ARE_Item13
'

AREEFA.sem = sem(areEFAmodel, areZ, std.lv = T, missing="fiml")
summary(AREEFA.sem)
semPaths(AREEFA.sem, whatLabel="est", intercept = F)

# Compare that to the 15 item WiL structure
anova(ARE10.sem, AREEFA.sem)
cbind(ARE10=fitmeasures(ARE10.sem), AREEFA=fitmeasures(AREEFA.sem)) %>% round(3)

# So the EFA fits slightly better, but not that much really.
