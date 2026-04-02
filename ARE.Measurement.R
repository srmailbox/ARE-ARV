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

# I think this means Items: 2, 4, 5, 7, 9
areRaw = areRaw %>% mutate(across(c(ARE_Item2, ARE_Item4, ARE_Item5, ARE_Item7, ARE_Item9), ~ -.+2))

## 1.2 Scaled version of items
areZ = areRaw %>% mutate(across(c(ARE_Item1:ARE_Item14, ARE_Item15_Num), ~scale(.)[,1]))

# 2.0 Item Analysis ####

## 2.1 Cronbach's ####

alpha(areZ %>% select(starts_with("ARE_Item"), -ARE_Item15))

# Items 2, 4, 9, and 5 are wonky. Should they be reverse coded? Yup, but also
# 7, which is currently not showing up as wonky. Don't know what to make of that.
# Ok... so, now that I reverse code those items, the alpha gets worse, and 7
# is negatively correlated with the total. Also, 2, 4, 5, 7, and 9 have very 
# poor r.drops - worse than before the reverse coding.

# Were they entered as reverse coded? No. So the above analysis is correct.

# 3.0 EFA ####

# According to the "p-value" test, we end up with 4 factors.
factanal(areZ %>% select(starts_with("ARE_Item"), -ARE_Item15) %>% drop_na()
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

# 3.0.1 Varimax rotation ####
factanal(areZ %>% select(starts_with("ARE_Item"), -ARE_Item15) %>% drop_na()
         , 4, rotation="varimax")


## 3.1 Principal components ###
# Just a desperate attempt to find something sensible.

princomp(areZ%>% select(starts_with("ARE_Item"), -ARE_Item15) %>% drop_na()) %>% summary
# again, no more than 4 components
princomp(areZ%>% select(starts_with("ARE_Item"), -ARE_Item15) %>% drop_na()) %>% loadings
