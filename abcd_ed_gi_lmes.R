#!/usr/bin/Rscript

library(psych)
library(lmerTest)
library(lme4)
library(dplyr)
library(MuMIn)
library(r2glmm)
library(effectsize)

print("Loading Rds files...")
abcd.ed.gi=readRDS("abcd_ed_gi.Rds")
abcd.ed.gi.m_crpbi=readRDS("abcd_ed_gi_mcrpbi.Rds")
abcd.ed.gi.f_crpbi=readRDS("abcd_ed_gi_fcrpbi.Rds")


sink("abcd_ed_gi_gam_output.txt",split=TRUE)

########################################
###########  Regression  ###############
########################################

const.vars=c("subjectid","abcd_site.x","rel_family_id.x","race_ethnicity.x","gender","married.or.livingtogether.x","household.income.x")

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

writeLines("#########################################")
writeLines("Whole sample correlation matrix (Table 3)")
writeLines("#########################################")

#Make correlation matrices
cont.vars=c("b.bmi","b.interview_age","b.gi_sum")

data=abcd.ed.gi[c(cont.vars,"b.cbcl_scr_dsm5_anxdisord_t")]
corr.test(data, adjust = "none", use = "pairwise")

data=abcd.ed.gi.m_crpbi[c(cont.vars,"b.cbcl_scr_dsm5_anxdisord_t","b.crpbi_mother")]
corr.test(data, adjust = "none", use = "pairwise")

data=abcd.ed.gi.f_crpbi[c(cont.vars,"b.cbcl_scr_dsm5_anxdisord_t","b.crpbi_father")]
corr.test(data, adjust = "none", use = "pairwise")

abcd.m.temp = abcd.ed.gi.m_crpbi[c("subjectid","b.crpbi_mother")]
abcd.m.temp=abcd.m.temp[complete.cases(abcd.m.temp),]
abcd.f.temp = abcd.ed.gi.f_crpbi[c("subjectid","b.crpbi_father")]
abcd.f.temp=abcd.f.temp[complete.cases(abcd.f.temp),]
abcd.m.f.temp = merge(abcd.m.temp,abcd.f.temp,by="subjectid")
cor.test(abcd.m.f.temp$b.crpbi_mother,abcd.m.f.temp$b.crpbi_father,use="pairwise.complete.obs",method="pearson")

writeLines("###########################################")
writeLines("Paternal Acceptance LME (Table 4; Table S3)")
writeLines("###########################################")

#Paternal acceptance
data=abcd.ed.gi.f_crpbi[c(const.vars,"b.ed_sum","b.gi_sum.centered","y1.ed_sum","b.crpbi_father.centered","b.cbcl_scr_dsm5_anxdisord_t")]
data=data[complete.cases(data),]

y1.ed_pred_b.gi_b.fcrpbi=lmer(y1.ed_sum ~ b.ed_sum + b.gi_sum.centered + race_ethnicity.x + gender + married.or.livingtogether.x + household.income.x + b.cbcl_scr_dsm5_anxdisord_t + b.crpbi_father.centered +
				gender * b.crpbi_father.centered * b.gi_sum.centered + (1|abcd_site.x/rel_family_id.x),data=data)

summary(y1.ed_pred_b.gi_b.fcrpbi)
r.squaredGLMM(y1.ed_pred_b.gi_b.fcrpbi)
standardize(y1.ed_pred_b.gi_b.fcrpbi)

writeLines("###########################################")
writeLines("Maternal Acceptance LME (Table 5; Table S2)")
writeLines("###########################################")

#Maternal acceptance
data=abcd.ed.gi.m_crpbi[c(const.vars,"b.ed_sum","b.gi_sum.centered","y1.ed_sum","b.crpbi_mother.centered","b.cbcl_scr_dsm5_anxdisord_t")]
data=data[complete.cases(data),]

y1.ed_pred_b.gi_b.mcrpbi=lmer(y1.ed_sum ~ b.ed_sum + b.gi_sum.centered + race_ethnicity.x + gender + married.or.livingtogether.x + household.income.x + b.cbcl_scr_dsm5_anxdisord_t + b.crpbi_mother.centered +
				gender * b.crpbi_mother.centered * b.gi_sum.centered + (1|abcd_site.x/rel_family_id.x),data=data)

writeLines("Results for GI symptoms and maternal acceptance:")	
summary(y1.ed_pred_b.gi_b.mcrpbi)
r.squaredGLMM(y1.ed_pred_b.gi_b.mcrpbi)
standardize(y1.ed_pred_b.gi_b.mcrpbi)

writeLines("##################################################")
writeLines("Maternal acceptance LME results by participant sex")
writeLines("##################################################")
writeLines("\n##################")
writeLines("Females (Table S5)")
writeLines("##################")

girls.abcd.ed.gi.m_crpbi=abcd.ed.gi.m_crpbi[abcd.ed.gi.m_crpbi$gender=="F",]
boys.abcd.ed.gi.m_crpbi=abcd.ed.gi.m_crpbi[abcd.ed.gi.m_crpbi$gender=="M",]

data=girls.abcd.ed.gi.m_crpbi[c(const.vars,"b.ed_sum","b.gi_sum.centered","y1.ed_sum","b.crpbi_mother.centered","b.cbcl_scr_dsm5_anxdisord_t")]
data=data[complete.cases(data),]

g.y1.ed_pred_b.gi_b.mcrpbi=lmer(y1.ed_sum ~ b.ed_sum + b.gi_sum.centered + race_ethnicity.x + married.or.livingtogether.x + household.income.x + b.cbcl_scr_dsm5_anxdisord_t + b.crpbi_mother.centered +
			b.crpbi_mother.centered * b.gi_sum.centered + (1|abcd_site.x/rel_family_id.x),data=data)

summary(g.y1.ed_pred_b.gi_b.mcrpbi)
r.squaredGLMM(g.y1.ed_pred_b.gi_b.mcrpbi)
standardize(g.y1.ed_pred_b.gi_b.mcrpbi)

writeLines("##################")
writeLines("Males (Table S6)")
writeLines("##################")

data=boys.abcd.ed.gi.m_crpbi[c(const.vars,"b.ed_sum","b.gi_sum.centered","y1.ed_sum","b.crpbi_mother.centered","b.cbcl_scr_dsm5_anxdisord_t")]
data=data[complete.cases(data),]

b.y1.ed_pred_b.gi_b.mcrpbi=lmer(y1.ed_sum ~ b.ed_sum + b.gi_sum.centered + race_ethnicity.x + married.or.livingtogether.x + household.income.x + b.cbcl_scr_dsm5_anxdisord_t + b.crpbi_mother.centered +
			b.crpbi_mother.centered * b.gi_sum.centered + (1|abcd_site.x/rel_family_id.x),data=data)

summary(b.y1.ed_pred_b.gi_b.mcrpbi)
r.squaredGLMM(b.y1.ed_pred_b.gi_b.mcrpbi)
standardize(b.y1.ed_pred_b.gi_b.mcrpbi)


sink()
