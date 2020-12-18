#!/usr/bin/Rscript

library(ez)

sink("abcd_ed_gi_descriptive_anovas_output.txt",split=TRUE)

print("Loading Rds files...")
abcd.ed.gi=readRDS("abcd_ed_gi.Rds")
abcd.ed.gi.m_crpbi=readRDS("abcd_ed_gi_mcrpbi.Rds")
abcd.ed.gi.f_crpbi=readRDS("abcd_ed_gi_fcrpbi.Rds")

abcd.ed.gi=abcd.ed.gi[abcd.ed.gi$gender=="F" | abcd.ed.gi$gender=="M",]

writeLines("Interview Age")
data=abcd.ed.gi[c("b.interview_age","y1.interview_age","subjectid","gender")]
data=data[complete.cases(data),]
data=reshape(data,varying=c("b.interview_age","y1.interview_age"),v.names="interview_age",timevar="eventname",times=c("baseline","year1"),direction="long")

age_anova = ezANOVA(data,interview_age,subjectid,within=eventname,between=gender,observed=gender,return_aov=TRUE)
print(age_anova)


writeLines("BMI")
data=abcd.ed.gi[c("b.bmi","y1.bmi","subjectid","gender")]
data=data[complete.cases(data),]
data=reshape(data,varying=c("b.bmi","y1.bmi"),v.names="bmi",timevar="eventname",times=c("baseline","year1"),direction="long")

bmi_anova = ezANOVA(data,bmi,subjectid,within=eventname,between=gender,observed=gender,return_aov=TRUE)
print(bmi_anova)


writeLines("GI Symptoms")
data=abcd.ed.gi[c("b.gi_sum","y1.gi_sum","subjectid","gender")]
data=data[complete.cases(data),]
data=reshape(data,varying=c("b.gi_sum","y1.gi_sum"),v.names="gi_sum",timevar="eventname",times=c("baseline","year1"),direction="long")

gi_anova = ezANOVA(data,gi_sum,subjectid,within=eventname,between=gender,observed=gender,return_aov=TRUE)
print(gi_anova)


writeLines("CBCL Anxiety")
data=abcd.ed.gi[c("b.cbcl_scr_dsm5_anxdisord_t","y1.cbcl_scr_dsm5_anxdisord_t","subjectid","gender")]
data=data[complete.cases(data),]
data=reshape(data,varying=c("b.cbcl_scr_dsm5_anxdisord_t","y1.cbcl_scr_dsm5_anxdisord_t"),v.names="cbcl_anx_t",timevar="eventname",times=c("baseline","year1"),direction="long")

anx_anova = ezANOVA(data,cbcl_anx_t,subjectid,within=eventname,between=gender,observed=gender,return_aov=TRUE)
print(anx_anova)


writeLines("CRPBI-M")
data=abcd.ed.gi.m_crpbi[c("b.crpbi_mother","y1.crpbi_mother","subjectid","gender")]
data=data[complete.cases(data),]
data=reshape(data,varying=c("b.crpbi_mother","y1.crpbi_mother"),v.names="crpbi_mother",timevar="eventname",times=c("baseline","year1"),direction="long")

mcrpbi_anova = ezANOVA(data,crpbi_mother,subjectid,within=eventname,between=gender,observed=gender,return_aov=TRUE)
print(mcrpbi_anova)


writeLines("CRPBI-P")
data=abcd.ed.gi.f_crpbi[c("b.crpbi_father","y1.crpbi_father","subjectid","gender")]
data=data[complete.cases(data),]
data=reshape(data,varying=c("b.crpbi_father","y1.crpbi_father"),v.names="crpbi_father",timevar="eventname",times=c("baseline","year1"),direction="long")

pcrpbi_anova = ezANOVA(data,crpbi_father,subjectid,within=eventname,between=gender,observed=gender,return_aov=TRUE)
print(pcrpbi_anova)


writeLines("ED Symptoms")
data=abcd.ed.gi[c("b.ed_sum","y1.ed_sum","subjectid","gender")]
data=data[complete.cases(data),]
data=reshape(data,varying=c("b.ed_sum","y1.ed_sum"),v.names="ed_sum",timevar="eventname",times=c("baseline","year1"),direction="long")


ed_anova = ezANOVA(data,ed_sum,subjectid,within=eventname,between=gender,observed=gender,return_aov=TRUE)
print(ed_anova)
