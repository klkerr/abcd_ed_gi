#!/usr/bin/Rscript

library(psych)
library(effsize)

sink("abcd_ed_gi_descriptive_output.txt",split=TRUE)

print("Loading Rds files...")
abcd.ed.gi=readRDS("abcd_ed_gi.Rds")
abcd.ed.gi.m_crpbi=readRDS("abcd_ed_gi_mcrpbi.Rds")
abcd.ed.gi.f_crpbi=readRDS("abcd_ed_gi_fcrpbi.Rds")

abcd.ed.gi=abcd.ed.gi[abcd.ed.gi$gender=="F" | abcd.ed.gi$gender=="M",]

writeLines("\n######################")
writeLines("Demographics (Table 1)")
writeLines("######################")

writeLines("\nSex")
table(abcd.ed.gi$gender)

writeLines("\nRace_Ethnicity")
table(abcd.ed.gi$race_ethnicity.x)

writeLines("\nEducation")
table(abcd.ed.gi$high.educ.x)

writeLines("\nHousehold Income")
table(abcd.ed.gi$household.income.x)

writeLines("\nParent Marital Status")
table(abcd.ed.gi$married.or.livingtogether.x)

writeLines("\n#########################")
writeLines("Baseline Scores (Table 2)")
writeLines("#########################")

writeLines("\nBaseline Age")
describe(abcd.ed.gi$b.interview_age)

writeLines("\nBaseline BMI")
describe(abcd.ed.gi$b.bmi)

writeLines("\nBaseline GI Symptoms")
describe(abcd.ed.gi$b.gi_sum)

writeLines("\nBaseline CBCL Anxiety")
describe(abcd.ed.gi$b.cbcl_scr_dsm5_anxdisord_t)

writeLines("\nBaseline Maternal Acceptance")
describe(abcd.ed.gi.m_crpbi$b.crpbi_mother)

writeLines("\nBaseline Paternal Acceptance")
describe(abcd.ed.gi.f_crpbi$b.crpbi_father)

writeLines("\nBaseline ED Symptoms")
describe(abcd.ed.gi$b.ed_sum)


writeLines("\n#######################")
writeLines("Year 1 Scores (Table 2)")
writeLines("#######################")

writeLines("\nYear 1 Age")
describe(abcd.ed.gi$y1.interview_age)

writeLines("\nYear 1 BMI")
describe(abcd.ed.gi$y1.bmi)

writeLines("\nYear 1 GI Symptoms")
describe(abcd.ed.gi$y1.gi_sum)

writeLines("\nYear 1 CBCL Anxiety")
describe(abcd.ed.gi$y1.cbcl_scr_dsm5_anxdisord_t)

writeLines("\nYear 1 Maternal Acceptance")
describe(abcd.ed.gi.m_crpbi$y1.crpbi_mother)

writeLines("\nYear 1 Paternal Acceptance")
describe(abcd.ed.gi.f_crpbi$y1.crpbi_father)

writeLines("\nYear 1 ED Symptoms")
describe(abcd.ed.gi$y1.ed_sum)

detach("package:psych", unload=TRUE)
library(effsize)


writeLines("\n####################################")
writeLines("Baseline vs Year 1 t-tests (Table 2)")
writeLines("####################################")

writeLines("\nBaseline vs Y1 - Age")
t.test(abcd.ed.gi$b.interview_age,abcd.ed.gi$y1.interview_age)
cohen.d(abcd.ed.gi$b.interview_age,abcd.ed.gi$y1.interview_age,na.rm=TRUE)

writeLines("\nBaseline vs Y1 - BMI")
t.test(abcd.ed.gi$b.bmi,abcd.ed.gi$y1.bmi,paired=TRUE)
cohen.d(abcd.ed.gi$b.bmi,abcd.ed.gi$y1.bmi,na.rm=TRUE)

writeLines("\nBaseline vs Y1 - GI Symptoms")
t.test(abcd.ed.gi$y1.gi_sum,abcd.ed.gi$b.gi_sum,paired=TRUE)
cohen.d(abcd.ed.gi$y1.gi_sum,abcd.ed.gi$b.gi_sum,na.rm=TRUE)

writeLines("\nBaseline vs Y1 - CBCL Anxiety")
t.test(abcd.ed.gi$b.cbcl_scr_dsm5_anxdisord_t,abcd.ed.gi$y1.cbcl_scr_dsm5_anxdisord_t,paired=TRUE)
cohen.d(abcd.ed.gi$b.cbcl_scr_dsm5_anxdisord_t,abcd.ed.gi$y1.cbcl_scr_dsm5_anxdisord_t,na.rm=TRUE)

writeLines("\nBaseline vs Y1 - Maternal Acceptance")
t.test(abcd.ed.gi.m_crpbi$y1.crpbi_mother,abcd.ed.gi.m_crpbi$b.crpbi_mother,paired=TRUE)
cohen.d(abcd.ed.gi.m_crpbi$y1.crpbi_mother,abcd.ed.gi.m_crpbi$b.crpbi_mother,na.rm=TRUE)

writeLines("\nBaseline vs Y1 - Paternal Acceptance")
t.test(abcd.ed.gi.f_crpbi$y1.crpbi_father,abcd.ed.gi.f_crpbi$b.crpbi_father,paired=TRUE)
cohen.d(abcd.ed.gi.f_crpbi$y1.crpbi_father,abcd.ed.gi.f_crpbi$b.crpbi_father,na.rm=TRUE)

writeLines("\nBaseline vs Y1 - ED Symptoms")
t.test(abcd.ed.gi$y1.ed_sum,abcd.ed.gi$b.ed_sum,paired=TRUE)
cohen.d(abcd.ed.gi$y1.ed_sum,abcd.ed.gi$b.ed_sum,na.rm=TRUE)


writeLines("\n################################")
writeLines("Gendered Descriptives (Table S1)")
writeLines("################################")

girls.abcd.ed.gi=abcd.ed.gi[abcd.ed.gi$gender=="F",]
boys.abcd.ed.gi=abcd.ed.gi[abcd.ed.gi$gender=="M",]

girls.abcd.ed.gi.m_crpbi=abcd.ed.gi.m_crpbi[abcd.ed.gi.m_crpbi$gender=="F",]
boys.abcd.ed.gi.m_crpbi=abcd.ed.gi.m_crpbi[abcd.ed.gi.m_crpbi$gender=="M",]

girls.abcd.ed.gi.f_crpbi=abcd.ed.gi.f_crpbi[abcd.ed.gi.f_crpbi$gender=="F",]
boys.abcd.ed.gi.f_crpbi=abcd.ed.gi.f_crpbi[abcd.ed.gi.f_crpbi$gender=="M",]

print("Baseline Age - Girls")
describe(girls.abcd.ed.gi$b.interview_age)

print("Year 1 Age - Girls")
describe(girls.abcd.ed.gi$y1.interview_age)

print("Baseline BMI - Girls")
describe(girls.abcd.ed.gi$b.bmi)

print("Year 1 BMI - Girls")
describe(girls.abcd.ed.gi$y1.bmi)

print("Baseline GI Symptoms - Girls")
describe(girls.abcd.ed.gi$b.gi_sum)

print("Year 1 GI Symptoms - Girls")
describe(girls.abcd.ed.gi$y1.gi_sum)

print("Baseline Anxiety - Girls")
describe(girls.abcd.ed.gi$b.cbcl_scr_dsm5_anxdisord_t)

print("Year 1 Anxiety - Girls")
describe(girls.abcd.ed.gi$y1.cbcl_scr_dsm5_anxdisord_t)

print("Baseline ED Symptoms - Girls")
describe(girls.abcd.ed.gi$b.ed_sum)

print("Year 1 ED Symptoms - Girls")
describe(girls.abcd.ed.gi$y1.ed_sum)

print("Baseline Maternal Acceptance - Girls")
describe(girls.abcd.ed.gi.m_crpbi$b.crpbi_mother)

print("Year 1 Maternal Acceptance - Girls")
describe(girls.abcd.ed.gi.m_crpbi$y1.crpbi_mother)

print("Baseline Paternal Acceptance - Girls")
describe(girls.abcd.ed.gi.f_crpbi$b.crpbi_father)

print("Year 1 Paternal Acceptance - Girls")
describe(girls.abcd.ed.gi.f_crpbi$y1.crpbi_father)

print("Baseline Age - Boys")
describe(boys.abcd.ed.gi$b.interview_age)

print("Year 1 Age - Boys")
describe(boys.abcd.ed.gi$y1.interview_age)

print("Baseline BMI - Boys")
describe(boys.abcd.ed.gi$b.bmi)

print("Year 1 BMI - Boys")
describe(boys.abcd.ed.gi$y1.bmi)

print("Baseline GI Symptoms - Boys")
describe(boys.abcd.ed.gi$b.gi_sum)

print("Year 1 GI Symptoms - Boys")
describe(boys.abcd.ed.gi$y1.gi_sum)

print("Baseline ED Symptoms - Boys")
describe(boys.abcd.ed.gi$b.ed_sum)

print("Year 1 ED Symptoms - Boys")
describe(boys.abcd.ed.gi$y1.ed_sum)

print("Baseline Anxiety - Boys")
describe(boys.abcd.ed.gi$b.cbcl_scr_dsm5_anxdisord_t)

print("Year 1 Anxiety - Boys")
describe(boys.abcd.ed.gi$y1.cbcl_scr_dsm5_anxdisord_t)

print("Baseline Maternal Acceptance - Boys")
describe(boys.abcd.ed.gi.m_crpbi$b.crpbi_mother)

print("Year 1 Maternal Acceptance - Boys")
describe(boys.abcd.ed.gi.m_crpbi$y1.crpbi_mother)

print("Baseline Paternal Acceptance - Boys")
describe(boys.abcd.ed.gi.f_crpbi$b.crpbi_father)

print("Year 1 Paternal Acceptance - Boys")
describe(boys.abcd.ed.gi.f_crpbi$y1.crpbi_father)

sink()
