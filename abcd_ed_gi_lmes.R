#!/usr/bin/Rscript

.libPaths(c("/media/cephfs/homefolders/librad.laureateinstitute.org/kkerr/rlib",.libPaths()))

library(psych)
library(lmerTest)
library(lme4)
library(dplyr)
library(MuMIn)
library(r2glmm)

print("Loading Rds files...")
abcd.ed.gi=readRDS("abcd_ed_gi.Rds")
abcd.ed.gi.m_crpbi=readRDS("abcd_ed_gi_mcrpbi.Rds")
abcd.ed.gi.f_crpbi=readRDS("abcd_ed_gi_fcrpbi.Rds")


sink("abcd_ed_gi_gam_output.txt",split=TRUE)

########################################
###########  Regression  ###############
########################################

const.vars=c("subjectid","abcd_site.x","rel_family_id.x","b.bmi","race_ethnicity.x","gender","high.educ.x","married.or.livingtogether.x","y1.interview_age","household.income.x")

#Center interaction terms
abcd.ed.gi$b.gi_sum.centered = scale(abcd.ed.gi$b.gi_sum,center=TRUE,scale=FALSE)
abcd.ed.gi.m_crpbi$b.gi_sum.centered = scale(abcd.ed.gi.m_crpbi$b.gi_sum,center=TRUE,scale=FALSE)
abcd.ed.gi.f_crpbi$b.gi_sum.centered = scale(abcd.ed.gi.f_crpbi$b.gi_sum,center=TRUE,scale=FALSE)

abcd.ed.gi.m_crpbi$b.crpbi_mother.centered = scale(abcd.ed.gi.m_crpbi$b.crpbi_mother,center=TRUE,scale=FALSE)
abcd.ed.gi.f_crpbi$b.crpbi_father.centered = scale(abcd.ed.gi.f_crpbi$b.crpbi_father,center=TRUE,scale=FALSE)

abcd.ed.gi$b.cbcl_scr_dsm5_anxdisord_t.centered = scale(abcd.ed.gi$b.cbcl_scr_dsm5_anxdisord_t,center=TRUE,scale=FALSE)

abcd.ed.gi=abcd.ed.gi[abcd.ed.gi$gender=="F" | abcd.ed.gi$gender=="M",]
abcd.ed.gi.m_crpbi = abcd.ed.gi.m_crpbi[abcd.ed.gi.m_crpbi$gender=="F" | abcd.ed.gi.m_crpbi$gender=="M",]
abcd.ed.gi.f_crpbi = abcd.ed.gi.f_crpbi[abcd.ed.gi.f_crpbi$gender=="F" | abcd.ed.gi.f_crpbi$gender=="M",]

#Make correlation matrices
cont.vars=c("b.bmi","y1.interview_age","b.gi_sum")
abcd.base=left_join(abcd.ed.gi,abcd.ed.gi.m_crpbi,by="subjectid")
abcd.base=left_join(abcd.base,abcd.ed.gi.f_crpbi,by="subjectid")

writeLines("#########################################")
writeLines("Whole sample correlation matrix (Table 3)")
writeLines("#########################################")

data=abcd.base[c(cont.vars,"b.cbcl_scr_dsm5_anxdisord_t","b.crpbi_mother","b.crpbi_father")]
cor(data,method="pearson",use="pairwise.complete.obs")

writeLines("###########################################")
writeLines("Paternal Acceptance LME (Table 4; Table S1)")
writeLines("###########################################")

#Paternal acceptance
data=abcd.ed.gi.f_crpbi[c(const.vars,"b.ed_sum","b.gi_sum.centered","y1.ed_sum","y1.interview_age","b.crpbi_father.centered","b.cbcl_scr_dsm5_anxdisord_t")]
data=data[complete.cases(data),]

y1.ed_pred_b.gi_b.fcrpbi=lmer(y1.ed_sum ~ b.ed_sum + b.gi_sum.centered + b.bmi + race_ethnicity.x + gender + high.educ.x + married.or.livingtogether.x + y1.interview_age + household.income.x + b.cbcl_scr_dsm5_anxdisord_t + b.crpbi_father.centered +
			gender * b.gi_sum.centered + b.crpbi_father.centered * b.gi_sum.centered + gender * b.crpbi_father.centered * b.gi_sum.centered + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.ed_pred_b.gi_b.fcrpbi)
r.squaredGLMM(y1.ed_pred_b.gi_b.fcrpbi)

#Maternal acceptance
data=abcd.ed.gi.m_crpbi[c(const.vars,"b.ed_sum","b.gi_sum.centered","y1.ed_sum","y1.interview_age","b.crpbi_mother.centered","b.gi_sum","b.crpbi_mother","b.cbcl_scr_dsm5_anxdisord_t")]
data=data[complete.cases(data),]

y1.ed_pred_b.gi_b.mcrpbi=lmer(y1.ed_sum ~ b.ed_sum + b.gi_sum.centered + b.bmi + race_ethnicity.x + gender + high.educ.x + married.or.livingtogether.x + y1.interview_age + household.income.x + b.cbcl_scr_dsm5_anxdisord_t + b.crpbi_mother.centered +
			gender * b.gi_sum.centered + b.crpbi_mother.centered * b.gi_sum.centered + gender * b.crpbi_mother.centered * b.gi_sum.centered + (1|abcd_site.x/rel_family_id.x),data=data)

writeLines("###########################################")
writeLines("Maternal Acceptance LME (Table 5; Table S2)")
writeLines("###########################################")

writeLines("Results for GI symptoms and maternal acceptance:")	
summary(y1.ed_pred_b.gi_b.mcrpbi)
r.squaredGLMM(y1.ed_pred_b.gi_b.mcrpbi)

writeLines("##################################################")
writeLines("Maternal acceptance LME results by participant sex")
writeLines("##################################################")
writeLines("\n##################")
writeLines("Females (Table S3)")
writeLines("##################")

girls.abcd.ed.gi.m_crpbi=abcd.ed.gi.m_crpbi[abcd.ed.gi.m_crpbi$gender=="F",]
boys.abcd.ed.gi.m_crpbi=abcd.ed.gi.m_crpbi[abcd.ed.gi.m_crpbi$gender=="M",]

data=girls.abcd.ed.gi.m_crpbi[c(const.vars,"b.ed_sum","b.gi_sum.centered","y1.ed_sum","y1.interview_age","b.crpbi_mother.centered","b.gi_sum","b.crpbi_mother","b.cbcl_scr_dsm5_anxdisord_t")]
data=data[complete.cases(data),]

g.y1.ed_pred_b.gi_b.mcrpbi=lmer(y1.ed_sum ~ b.ed_sum + b.gi_sum.centered + b.bmi + race_ethnicity.x + high.educ.x + married.or.livingtogether.x + y1.interview_age + household.income.x + b.cbcl_scr_dsm5_anxdisord_t + b.crpbi_mother.centered +
			b.crpbi_mother.centered * b.gi_sum.centered + (1|abcd_site.x/rel_family_id.x),data=data)

summary(g.y1.ed_pred_b.gi_b.mcrpbi)
r.squaredGLMM(g.y1.ed_pred_b.gi_b.mcrpbi)

writeLines("##################")
writeLines("Females (Table S4)")
writeLines("##################")

data=boys.abcd.ed.gi.m_crpbi[c(const.vars,"b.ed_sum","b.gi_sum.centered","y1.ed_sum","y1.interview_age","b.crpbi_mother.centered","b.gi_sum","b.crpbi_mother","b.cbcl_scr_dsm5_anxdisord_t")]
data=data[complete.cases(data),]

b.y1.ed_pred_b.gi_b.mcrpbi=lmer(y1.ed_sum ~ b.ed_sum + b.gi_sum.centered + b.bmi + race_ethnicity.x + high.educ.x + married.or.livingtogether.x + y1.interview_age + household.income.x + b.cbcl_scr_dsm5_anxdisord_t + b.crpbi_mother.centered +
			b.crpbi_mother.centered * b.gi_sum.centered + (1|abcd_site.x/rel_family_id.x),data=data)

summary(b.y1.ed_pred_b.gi_b.mcrpbi)
r.squaredGLMM(b.y1.ed_pred_b.gi_b.mcrpbi)


sink()
