#!/usr/bin/Rscript

library(lme4)
library(lmerTest)

print("Loading Rds files...")
abcd.ed.gi=readRDS("abcd_ed_gi.Rds")
abcd.ed.gi.m_crpbi=readRDS("abcd_ed_gi_mcrpbi.Rds")
abcd.ed.gi.f_crpbi=readRDS("abcd_ed_gi_fcrpbi.Rds")

sink("abcd_ed_gi_covariate_models_output.txt",split=TRUE)

const.vars=c("subjectid","abcd_site.x","rel_family_id.x")

abcd.ed.gi=abcd.ed.gi[abcd.ed.gi$gender=="F" | abcd.ed.gi$gender=="M",]
abcd.ed.gi.m_crpbi = abcd.ed.gi.m_crpbi[abcd.ed.gi.m_crpbi$gender=="F" | abcd.ed.gi.m_crpbi$gender=="M",]
abcd.ed.gi.f_crpbi = abcd.ed.gi.f_crpbi[abcd.ed.gi.f_crpbi$gender=="F" | abcd.ed.gi.f_crpbi$gender=="M",]


## ED symptom prediction

writeLines("#########")
writeLines("ED ~ BMI")
writeLines("#########")


## BMI
data=abcd.ed.gi[c(const.vars,"b.bmi","y1.ed_sum")]
data=data[complete.cases(data),]

y1.ed_pred_b.bmi=lmer(y1.ed_sum ~ b.bmi + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.ed_pred_b.bmi)

writeLines("###################")
writeLines("ED ~ Race/ethnicity")
writeLines("###################")

## Race/ethnicity
data=abcd.ed.gi[c(const.vars,"race_ethnicity.x","y1.ed_sum")]
data=data[complete.cases(data),]

y1.ed_pred_b.race_ethnicity.x=lmer(y1.ed_sum ~ race_ethnicity.x + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.ed_pred_b.race_ethnicity.x)

writeLines("##############")
writeLines("ED ~ Education")
writeLines("##############")

## Education
data=abcd.ed.gi[c(const.vars,"high.educ.x","y1.ed_sum")]
data=data[complete.cases(data),]

y1.ed_pred_high.educ.x=lmer(y1.ed_sum ~ high.educ.x + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.ed_pred_high.educ.x)

writeLines("###################")
writeLines("ED ~ Marital status")
writeLines("###################")

## Marital status
data=abcd.ed.gi[c(const.vars,"married.or.livingtogether.x","y1.ed_sum")]
data=data[complete.cases(data),]

y1.ed_pred_married.or.livingtogether.x=lmer(y1.ed_sum ~ married.or.livingtogether.x + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.ed_pred_married.or.livingtogether.x)

writeLines("########")
writeLines("ED ~ Age")
writeLines("########")

## Age
data=abcd.ed.gi[c(const.vars,"y1.interview_age","y1.ed_sum")]
data=data[complete.cases(data),]

y1.ed_pred_y1.interview_age=lmer(y1.ed_sum ~ y1.interview_age + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.ed_pred_y1.interview_age)

writeLines("###########")
writeLines("ED ~ Income")
writeLines("###########")

## Household income
data=abcd.ed.gi[c(const.vars,"household.income.x","y1.ed_sum")]
data=data[complete.cases(data),]

y1.ed_pred_household.income.x=lmer(y1.ed_sum ~ household.income.x + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.ed_pred_household.income.x)

writeLines("###########")
writeLines("ED ~ Anxiety")
writeLines("###########")

## CBCL Anxiety
data=abcd.ed.gi[c(const.vars,"b.cbcl_scr_dsm5_anxdisord_t","y1.ed_sum")]
data=data[complete.cases(data),]

y1.ed_pred_b.cbcl_scr_dsm5_anxdisord_t=lmer(y1.ed_sum ~ b.cbcl_scr_dsm5_anxdisord_t + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.ed_pred_b.cbcl_scr_dsm5_anxdisord_t)

## GI symptom prediction

writeLines("########")
writeLines("GI ~ BMI")
writeLines("########")

## BMI
data=abcd.ed.gi[c(const.vars,"b.bmi","b.gi_sum")]
data=data[complete.cases(data),]

b.gi_pred_b.bmi=lmer(b.gi_sum ~ b.bmi + (1|abcd_site.x/rel_family_id.x),data=data)

summary(b.gi_pred_b.bmi)

writeLines("###################")
writeLines("GI ~ Race/ethnicity")
writeLines("###################")

## Race/ethnicity
data=abcd.ed.gi[c(const.vars,"race_ethnicity.x","b.gi_sum")]
data=data[complete.cases(data),]

b.gi_pred_b.race_ethnicity.x=lmer(b.gi_sum ~ race_ethnicity.x + (1|abcd_site.x/rel_family_id.x),data=data)

summary(b.gi_pred_b.race_ethnicity.x)

writeLines("##############")
writeLines("GI ~ Education")
writeLines("##############")

## Education
data=abcd.ed.gi[c(const.vars,"high.educ.x","b.gi_sum")]
data=data[complete.cases(data),]

b.gi_pred_high.educ.x=lmer(b.gi_sum ~ high.educ.x + (1|abcd_site.x/rel_family_id.x),data=data)

summary(b.gi_pred_high.educ.x)

writeLines("###################")
writeLines("GI ~ Marital status")
writeLines("###################")

## Marital status
data=abcd.ed.gi[c(const.vars,"married.or.livingtogether.x","b.gi_sum")]
data=data[complete.cases(data),]

b.gi_pred_married.or.livingtogether.x=lmer(b.gi_sum ~ married.or.livingtogether.x + (1|abcd_site.x/rel_family_id.x),data=data)

summary(b.gi_pred_married.or.livingtogether.x)

writeLines("########")
writeLines("GI ~ Age")
writeLines("########")

## Age
data=abcd.ed.gi[c(const.vars,"y1.interview_age","b.gi_sum")]
data=data[complete.cases(data),]

b.gi_pred_y1.interview_age=lmer(b.gi_sum ~ y1.interview_age + (1|abcd_site.x/rel_family_id.x),data=data)

summary(b.gi_pred_y1.interview_age)

writeLines("###########")
writeLines("GI ~ Income")
writeLines("###########")

## Household income
data=abcd.ed.gi[c(const.vars,"household.income.x","b.gi_sum")]
data=data[complete.cases(data),]

b.gi_pred_household.income.x=lmer(b.gi_sum ~ household.income.x + (1|abcd_site.x/rel_family_id.x),data=data)

summary(b.gi_pred_household.income.x)

writeLines("###########")
writeLines("GI ~ Anxiety")
writeLines("###########")

## CBCL Anxiety
data=abcd.ed.gi[c(const.vars,"b.cbcl_scr_dsm5_anxdisord_t","b.gi_sum")]
data=data[complete.cases(data),]

b.gi_pred_b.cbcl_scr_dsm5_anxdisord_t=lmer(b.gi_sum ~ b.cbcl_scr_dsm5_anxdisord_t + (1|abcd_site.x/rel_family_id.x),data=data)

summary(b.gi_pred_b.cbcl_scr_dsm5_anxdisord_t)


## Maternal acceptance prediction

writeLines("##################")
writeLines("Maternal Acc ~ BMI")
writeLines("##################")

## BMI
data=abcd.ed.gi.m_crpbi[c(const.vars,"b.bmi","b.crpbi_mother")]
data=data[complete.cases(data),]

y1.m_acc_pred_b.bmi=lmer(b.crpbi_mother ~ b.bmi + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.m_acc_pred_b.bmi)

writeLines("#############################")
writeLines("Maternal Acc ~ Race/ethnicity")
writeLines("#############################")

## Race/ethnicity
data=abcd.ed.gi.m_crpbi[c(const.vars,"race_ethnicity.x","b.crpbi_mother")]
data=data[complete.cases(data),]

y1.m_acc_pred_b.race_ethnicity.x=lmer(b.crpbi_mother ~ race_ethnicity.x + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.m_acc_pred_b.race_ethnicity.x)

writeLines("########################")
writeLines("Maternal Acc ~ Education")
writeLines("########################")

## Education
data=abcd.ed.gi.m_crpbi[c(const.vars,"high.educ.x","b.crpbi_mother")]
data=data[complete.cases(data),]

y1.m_acc_pred_high.educ.x=lmer(b.crpbi_mother ~ high.educ.x + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.m_acc_pred_high.educ.x)

writeLines("#############################")
writeLines("Maternal Acc ~ Marital status")
writeLines("#############################")

## Marital status
data=abcd.ed.gi.m_crpbi[c(const.vars,"married.or.livingtogether.x","b.crpbi_mother")]
data=data[complete.cases(data),]

y1.m_acc_pred_married.or.livingtogether.x=lmer(b.crpbi_mother ~ married.or.livingtogether.x + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.m_acc_pred_married.or.livingtogether.x)

writeLines("##################")
writeLines("Maternal Acc ~ Age")
writeLines("##################")

## Age
data=abcd.ed.gi.m_crpbi[c(const.vars,"y1.interview_age","b.crpbi_mother")]
data=data[complete.cases(data),]

y1.m_acc_pred_y1.interview_age=lmer(b.crpbi_mother ~ y1.interview_age + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.m_acc_pred_y1.interview_age)

writeLines("#####################")
writeLines("Maternal Acc ~ Income")
writeLines("#####################")

## Household income
data=abcd.ed.gi.m_crpbi[c(const.vars,"household.income.x","b.crpbi_mother")]
data=data[complete.cases(data),]

y1.m_acc_pred_household.income.x=lmer(b.crpbi_mother ~ household.income.x + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.m_acc_pred_household.income.x)

writeLines("######################")
writeLines("Maternal Acc ~ Anxiety")
writeLines("######################")

## CBCL Anxiety
data=abcd.ed.gi.m_crpbi[c(const.vars,"b.cbcl_scr_dsm5_anxdisord_t","b.crpbi_mother")]
data=data[complete.cases(data),]

y1.m_acc_pred_b.cbcl_scr_dsm5_anxdisord_t=lmer(b.crpbi_mother ~ b.cbcl_scr_dsm5_anxdisord_t + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.m_acc_pred_b.cbcl_scr_dsm5_anxdisord_t)


## Paternal acceptance prediction

writeLines("##################")
writeLines("Paternal Acc ~ BMI")
writeLines("##################")

## BMI
data=abcd.ed.gi.f_crpbi[c(const.vars,"b.bmi","b.crpbi_father")]
data=data[complete.cases(data),]

y1.f_acc_pred_b.bmi=lmer(b.crpbi_father ~ b.bmi + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.f_acc_pred_b.bmi)

writeLines("#############################")
writeLines("Paternal Acc ~ Race/ethnicity")
writeLines("#############################")

## Race/ethnicity
data=abcd.ed.gi.f_crpbi[c(const.vars,"race_ethnicity.x","b.crpbi_father")]
data=data[complete.cases(data),]

y1.f_acc_pred_b.race_ethnicity.x=lmer(b.crpbi_father ~ race_ethnicity.x + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.f_acc_pred_b.race_ethnicity.x)

writeLines("########################")
writeLines("Paternal Acc ~ Education")
writeLines("########################")

## Education
data=abcd.ed.gi.f_crpbi[c(const.vars,"high.educ.x","b.crpbi_father")]
data=data[complete.cases(data),]

y1.f_acc_pred_high.educ.x=lmer(b.crpbi_father ~ high.educ.x + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.f_acc_pred_high.educ.x)

writeLines("#############################")
writeLines("Paternal Acc ~ Marital status")
writeLines("#############################")

## Marital status
data=abcd.ed.gi.f_crpbi[c(const.vars,"married.or.livingtogether.x","b.crpbi_father")]
data=data[complete.cases(data),]

y1.f_acc_pred_married.or.livingtogether.x=lmer(b.crpbi_father ~ married.or.livingtogether.x + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.f_acc_pred_married.or.livingtogether.x)

writeLines("##################")
writeLines("Paternal Acc ~ Age")
writeLines("##################")

## Age
data=abcd.ed.gi.f_crpbi[c(const.vars,"y1.interview_age","b.crpbi_father")]
data=data[complete.cases(data),]

y1.f_acc_pred_y1.interview_age=lmer(b.crpbi_father ~ y1.interview_age + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.f_acc_pred_y1.interview_age)

writeLines("#####################")
writeLines("Paternal Acc ~ Income")
writeLines("#####################")

## Household income
data=abcd.ed.gi.f_crpbi[c(const.vars,"household.income.x","b.crpbi_father")]
data=data[complete.cases(data),]

y1.f_acc_pred_household.income.x=lmer(b.crpbi_father ~ household.income.x + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.f_acc_pred_household.income.x)

writeLines("######################")
writeLines("Paternal Acc ~ Anxiety")
writeLines("######################")

## CBCL Anxiety
data=abcd.ed.gi.f_crpbi[c(const.vars,"b.cbcl_scr_dsm5_anxdisord_t","b.crpbi_father")]
data=data[complete.cases(data),]

y1.f_acc_pred_b.cbcl_scr_dsm5_anxdisord_t=lmer(b.crpbi_father ~ b.cbcl_scr_dsm5_anxdisord_t + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.f_acc_pred_b.cbcl_scr_dsm5_anxdisord_t)

sink()
