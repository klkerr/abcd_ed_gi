#!/usr/bin/Rscript

library(dplyr)

print("Reading rds...")
abcd.all=readRDS("nda2.0.1.Rds")

abcd.base=abcd.all[abcd.all$eventname=="baseline_year_1_arm_1",]
abcd.oney=abcd.all[abcd.all$eventname=="1_year_follow_up_y_arm_1",]

print("Renaming baseline variables...")
abcd.base$b.cbcl_q56c_p=as.numeric(abcd.base$cbcl_q56c_p)
abcd.base$b.cbcl_q56f_p=as.numeric(abcd.base$cbcl_q56f_p)
abcd.base$b.cbcl_q56g_p=as.numeric(abcd.base$cbcl_q56g_p)
abcd.base$b.asr_q56c_p=as.numeric(abcd.base$asr_q56c_p)
abcd.base$b.asr_q56g_p=as.numeric(abcd.base$asr_q56g_p)
abcd.base$b.asr_q56f_p=as.numeric(abcd.base$asr_q56f_p)
abcd.base$b.bmi=abcd.base$anthro_bmi_calc
abcd.base$b.ksads_13_469_p=as.numeric(abcd.base$ksads_13_469_p)
abcd.base$b.ksads_13_470_p=as.numeric(abcd.base$ksads_13_470_p)
abcd.base$b.ksads_13_74_p=as.numeric(abcd.base$ksads_13_74_p)
abcd.base$b.ksads_13_66_p=as.numeric(abcd.base$ksads_13_66_p)
abcd.base$b.ksads_13_68_p=as.numeric(abcd.base$ksads_13_68_p)
abcd.base$b.ksads_13_70_p=as.numeric(abcd.base$ksads_13_70_p)
abcd.base$b.ksads_13_72_p=as.numeric(abcd.base$ksads_13_72_p)
abcd.base$b.interview_age=abcd.base$interview_age
abcd.base$b.cbcl_scr_dsm5_anxdisord_t=as.numeric(abcd.base$cbcl_scr_dsm5_anxdisord_t)

abcd.base$b.ksads_13_469_p[is.na(abcd.base$b.ksads_13_469_p)]=0
abcd.base$b.ksads_13_470_p[is.na(abcd.base$b.ksads_13_470_p)]=0
abcd.base$b.ksads_13_74_p[is.na(abcd.base$b.ksads_13_74_p)]=0
abcd.base$b.ksads_13_66_p[is.na(abcd.base$b.ksads_13_66_p)]=0
abcd.base$b.ksads_13_68_p[is.na(abcd.base$b.ksads_13_68_p)]=0
abcd.base$b.ksads_13_70_p[is.na(abcd.base$b.ksads_13_70_p)]=0
abcd.base$b.ksads_13_72_p[is.na(abcd.base$b.ksads_13_72_p)]=0

abcd.base$b.gi_sum=abcd.base$b.cbcl_q56c_p + abcd.base$b.cbcl_q56f_p + abcd.base$b.cbcl_q56g_p
abcd.base$b.ed_sum=abcd.base$b.ksads_13_469_p + 
	abcd.base$b.ksads_13_470_p + 
	abcd.base$b.ksads_13_74_p +
	abcd.base$b.ksads_13_66_p +
	abcd.base$b.ksads_13_68_p +
	abcd.base$b.ksads_13_70_p +
	abcd.base$b.ksads_13_72_p

abcd.base$b.p.gi_sum=abcd.base$b.asr_q56c_p + abcd.base$b.asr_q56g_p + abcd.base$b.asr_q56f_p

print("Renaming Year 1 variables...")
abcd.oney$y1.cbcl_q56c_p=as.numeric(abcd.oney$cbcl_q56c_p)
abcd.oney$y1.cbcl_q56f_p=as.numeric(abcd.oney$cbcl_q56f_p)
abcd.oney$y1.cbcl_q56g_p=as.numeric(abcd.oney$cbcl_q56g_p)
abcd.oney$y1.bmi=abcd.oney$anthro_bmi_calc
abcd.oney$y1.ksads_13_469_p=as.numeric(abcd.oney$ksads_13_469_p)
abcd.oney$y1.ksads_13_470_p=as.numeric(abcd.oney$ksads_13_470_p)
abcd.oney$y1.ksads_13_74_p=as.numeric(abcd.oney$ksads_13_74_p)
abcd.oney$y1.ksads_13_66_p=as.numeric(abcd.oney$ksads_13_66_p)
abcd.oney$y1.ksads_13_68_p=as.numeric(abcd.oney$ksads_13_68_p)
abcd.oney$y1.ksads_13_70_p=as.numeric(abcd.oney$ksads_13_70_p)
abcd.oney$y1.ksads_13_72_p=as.numeric(abcd.oney$ksads_13_72_p)
abcd.oney$y1.interview_age=abcd.oney$interview_age
abcd.oney$y1.cbcl_scr_dsm5_anxdisord_t=as.numeric(abcd.oney$cbcl_scr_dsm5_anxdisord_t)

print("Setting NAs to 0...")
abcd.oney$y1.ksads_13_469_p[is.na(abcd.oney$y1.ksads_13_469_p)]=0
abcd.oney$y1.ksads_13_470_p[is.na(abcd.oney$y1.ksads_13_470_p)]=0
abcd.oney$y1.ksads_13_74_p[is.na(abcd.oney$y1.ksads_13_74_p)]=0
abcd.oney$y1.ksads_13_66_p[is.na(abcd.oney$y1.ksads_13_66_p)]=0
abcd.oney$y1.ksads_13_68_p[is.na(abcd.oney$y1.ksads_13_68_p)]=0
abcd.oney$y1.ksads_13_70_p[is.na(abcd.oney$y1.ksads_13_70_p)]=0
abcd.oney$y1.ksads_13_72_p[is.na(abcd.oney$y1.ksads_13_72_p)]=0

print("Calculating sum scores...")
abcd.oney$y1.gi_sum=abcd.oney$y1.cbcl_q56c_p + abcd.oney$y1.cbcl_q56f_p + abcd.oney$y1.cbcl_q56g_p
abcd.oney$y1.ed_sum=abcd.oney$y1.ksads_13_469_p + 
	abcd.oney$y1.ksads_13_470_p + 
	abcd.oney$y1.ksads_13_74_p +
	abcd.oney$y1.ksads_13_66_p +
	abcd.oney$y1.ksads_13_68_p +
	abcd.oney$y1.ksads_13_70_p +
	abcd.oney$y1.ksads_13_72_p

#Primary caregiver = Mother 
print("Mother subset")
abcd.b.primMother=abcd.base[abcd.base$demo_prim_p=="Child's Biological Mother",]
abcd.y1.primMother=abcd.oney[abcd.oney$demo_prim_l=="Child's Biological Mother",]

#Primary caregiver = Father
print("Father primary subset")
abcd.b.primFather=abcd.base[abcd.base$demo_prim_p=="Child's Biological Father",]
abcd.y1.primFather=abcd.oney[abcd.oney$demo_prim_l=="Child's Biological Father",]

#Secondary caregiver = Mother
#Making sure there aren't duplicates from the first set
print("Secondary mother subset...")
abcd.b.secMother=abcd.base[abcd.base$demo_prim_p!="Child's Biological Mother",]
abcd.b.secMother=abcd.b.secMother[abcd.b.secMother$crpbi_acceptance_caregiver2=="mother",]
abcd.y1.secMother=abcd.oney[abcd.oney$demo_prim_l!="Child's Biological Mother",]
abcd.y1.secMother=abcd.y1.secMother[abcd.y1.secMother$crpbi_acceptance_caregiver2=="mother",]

#Secondary caregiver = Father
#Making sure there aren't duplicates from the first set
print("Father subset")
abcd.b.secFather=abcd.base[abcd.base$demo_prim_p!="Child's Biological Father",]
abcd.b.secFather=abcd.b.secFather[abcd.b.secFather$crpbi_acceptance_caregiver2=="father",]
abcd.y1.secFather=abcd.oney[abcd.oney$demo_prim_l!="Child's Biological Father",]
abcd.y1.secFather=abcd.y1.secFather[abcd.y1.secFather$crpbi_acceptance_caregiver2=="father",]

#Create 'crpbi_father' variable that includes CRPBI scores for father, regardless of primary or secondary caregiver
print("Father crpbi scores...")
abcd.b.primFather$b.crpbi_father=abcd.b.primFather$crpbi_ss_studycaregiver
abcd.b.secFather$b.crpbi_father=abcd.b.secFather$crpbi_y_ss_caregiver
abcd.y1.primFather$y1.crpbi_father=abcd.y1.primFather$crpbi_ss_studycaregiver
abcd.y1.secFather$y1.crpbi_father=abcd.y1.secFather$crpbi_y_ss_caregiver

#Combine primary and secondary father datasets into one
abcd.b.fathers=rbind(abcd.b.primFather,abcd.b.secFather)
abcd.y1.fathers=rbind(abcd.y1.primFather,abcd.y1.secFather)

#Create 'crpbi_mother' variable that includes CRPBI scores for mother, regardless of primary or secondary caregiver
print("Mother crpbi scores...")
abcd.b.primMother$b.crpbi_mother=abcd.b.primMother$crpbi_ss_studycaregiver
abcd.b.secMother$b.crpbi_mother=abcd.b.secMother$crpbi_y_ss_caregiver
abcd.y1.primMother$y1.crpbi_mother=abcd.y1.primMother$crpbi_ss_studycaregiver
abcd.y1.secMother$y1.crpbi_mother=abcd.y1.secMother$crpbi_y_ss_caregiver

#Combine primary and secondary mother datasets into one
abcd.b.mothers=rbind(abcd.b.primMother,abcd.b.secMother)
abcd.y1.mothers=rbind(abcd.y1.primMother,abcd.y1.secMother)

print("Subsetting data..")
const.vars=c("subjectid","rel_family_id","abcd_site","race_ethnicity","high.educ","married.or.livingtogether","household.income")

abcd.base.subset=abcd.base[c(const.vars,"b.cbcl_q56c_p","b.cbcl_q56f_p","b.cbcl_q56g_p","b.bmi","b.ksads_13_469_p","b.ksads_13_470_p","b.ksads_13_74_p","b.ksads_13_66_p","b.ksads_13_68_p","b.ksads_13_70_p","b.ksads_13_72_p","b.interview_age","b.gi_sum","b.ed_sum","b.cbcl_scr_dsm5_anxdisord_t","b.cbcl_q56f_p")]
abcd.oney.subset=abcd.oney[c(const.vars,"y1.bmi","y1.gi_sum","y1.ed_sum","y1.interview_age","gender","y1.cbcl_q56c_p","y1.cbcl_q56f_p","y1.cbcl_q56g_p","y1.ksads_13_469_p","y1.ksads_13_470_p","y1.ksads_13_74_p","y1.ksads_13_66_p","y1.ksads_13_68_p","y1.ksads_13_70_p","y1.ksads_13_72_p","y1.cbcl_scr_dsm5_anxdisord_t")]

abcd.b.mothers=abcd.b.mothers[c(const.vars,"b.bmi","b.gi_sum","b.ed_sum","b.interview_age","b.cbcl_q56c_p","b.cbcl_q56f_p","b.cbcl_q56g_p","b.ksads_13_469_p","b.ksads_13_470_p","b.ksads_13_74_p","b.ksads_13_66_p","b.ksads_13_68_p","b.ksads_13_70_p","b.ksads_13_72_p","b.crpbi_mother","b.cbcl_scr_dsm5_anxdisord_t")]
abcd.b.fathers=abcd.b.fathers[c(const.vars,"b.bmi","b.gi_sum","b.ed_sum","b.interview_age","b.cbcl_q56c_p","b.cbcl_q56f_p","b.cbcl_q56g_p","b.ksads_13_469_p","b.ksads_13_470_p","b.ksads_13_74_p","b.ksads_13_66_p","b.ksads_13_68_p","b.ksads_13_70_p","b.ksads_13_72_p","b.crpbi_father","b.cbcl_scr_dsm5_anxdisord_t")]

abcd.oney.mothers=abcd.y1.mothers[c(const.vars,"y1.bmi","y1.gi_sum","y1.ed_sum","y1.interview_age","gender","y1.cbcl_q56c_p","y1.cbcl_q56f_p","y1.cbcl_q56g_p","y1.ksads_13_469_p","y1.ksads_13_470_p","y1.ksads_13_74_p","y1.ksads_13_66_p","y1.ksads_13_68_p","y1.ksads_13_70_p","y1.ksads_13_72_p","y1.crpbi_mother","y1.cbcl_scr_dsm5_anxdisord_t")]
abcd.oney.fathers=abcd.y1.fathers[c(const.vars,"y1.bmi","y1.gi_sum","y1.ed_sum","y1.interview_age","gender","y1.cbcl_q56c_p","y1.cbcl_q56f_p","y1.cbcl_q56g_p","y1.ksads_13_469_p","y1.ksads_13_470_p","y1.ksads_13_74_p","y1.ksads_13_66_p","y1.ksads_13_68_p","y1.ksads_13_70_p","y1.ksads_13_72_p","y1.crpbi_father","y1.cbcl_scr_dsm5_anxdisord_t")]

print("Merging datasets...")
abcd.ed.gi=inner_join(abcd.base.subset,abcd.oney.subset,by="subjectid")
abcd.ed.gi.m_crpbi=inner_join(abcd.b.mothers,abcd.oney.mothers,by="subjectid")
abcd.ed.gi.f_crpbi=inner_join(abcd.b.fathers,abcd.oney.fathers,by="subjectid")

print("Saving Rds files...")
saveRDS(abcd.ed.gi,file="abcd_ed_gi.Rds")
saveRDS(abcd.ed.gi.m_crpbi,file="abcd_ed_gi_mcrpbi.Rds")
saveRDS(abcd.ed.gi.f_crpbi,file="abcd_ed_gi_fcrpbi.Rds")
