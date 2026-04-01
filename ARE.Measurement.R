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
  
  (cor(areRaw %>% select(where(is.numeric), -Grade, -Age), use="pairwise.complete")-
  cor(areRaw %>% select(where(is.numeric), -Grade, -Age), use="pairwise.complete", method="spearman")) %>% psych::describe()
  # OK, so the correlation between Items 13 and 14 seems sensitive to this. Those are both the "frequency" measures.
  # They are interval scale though, but discrete.
  areRaw %>% ggplot(aes(x=ARE_Item13)) + geom_histogram()
  # Ok, well that's an odd one. Kind of normal-ish, but too many 0s. Zero-inflated normal?
  
# One thing is certain, I'm over thinking this stuff as far what is typically done.

}

areZ = areRaw %>% mutate(across(ARE_Item1:ARE_Item14, scale))



