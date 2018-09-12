setwd('S:/lactrops/Retirement/Data/Machine Learning Prediction/Data')

library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)

# import the historical data of all employees (2013-2018)
rawdata=read.csv('AllEmployees_2013-2018.csv',stringsAsFactors=FALSE,header=TRUE, sep=",")

# add FISCAL_YEAR variable to rawdata
fy=read.csv('fy.csv',stringsAsFactors=FALSE,header=TRUE, sep=",")
rawdata =left_join(rawdata,fy[,c('PAYROLL_SEQUENCE','FISCAL_YEAR')],by='PAYROLL_SEQUENCE')

# For each employee, group the pay period's records into years
all_employee=rawdata %>%
  filter(CHECK_TYPE=='Regular') %>%
  select(FISCAL_YEAR,PAYROLL_SEQUENCE,EMPLOYEE_ID,DATE_OF_BIRTH,ORIGINAL_HIRE_DATE)

all_employee=all_employee %>%
  group_by(FISCAL_YEAR,EMPLOYEE_ID,DATE_OF_BIRTH,ORIGINAL_HIRE_DATE) %>%
  summarise(MAX_PP=max(PAYROLL_SEQUENCE)) # The MAX_PP is the last pay period a specific employee worked in that year

#######################################################################################################################
# Feature Engineerig 

# age and service year
all_employee$next_fy_startdate1='01-JUL'
all_employee$next_fy_startdate2=as.character(all_employee$FISCAL_YEAR-2000+1)
all_employee$next_fy_startdate=paste(all_employee$next_fy_startdate1,"-",all_employee$next_fy_startdate2,sep="")
all_employee$next_fy_startdate=dmy(all_employee$next_fy_startdate)
all_employee=all_employee[,-c(6,7)]

all_employee$DATE_OF_BIRTH=dmy(all_employee$DATE_OF_BIRTH)
year(all_employee$DATE_OF_BIRTH)=year(all_employee$DATE_OF_BIRTH) %% 100 +1900


all_employee$ORIGINAL_HIRE_DATE=dmy(all_employee$ORIGINAL_HIRE_DATE)
all_employee =all_employee %>%
  filter(!is.na(ORIGINAL_HIRE_DATE))

year(all_employee$ORIGINAL_HIRE_DATE[year(all_employee$ORIGINAL_HIRE_DATE)>2018]) = 
  year(all_employee$ORIGINAL_HIRE_DATE[year(all_employee$ORIGINAL_HIRE_DATE)>2018]) %% 100 +1900

# Age variable means before the beginning of next FY, specific employee would reach the value in his/her age variable. Same as service_year
all_employee$age=floor((all_employee$next_fy_startdate-all_employee$DATE_OF_BIRTH)/365)
all_employee$service_year=floor((all_employee$next_fy_startdate-all_employee$ORIGINAL_HIRE_DATE)/365)

# add categorical features 
all_employee=left_join(all_employee,unique(rawdata[,c('EMPLOYEE_ID','FMS_DEPARTMENT_TITLE','JOB_CLASS_TITLE','PAYROLL_SEQUENCE','PAY_GRADE',
                                                      'WORK_SCHEDULE','SALARY_STEP','RETIREMENT_PENSION_PLAN',
                                                      'ETHNICITY','GENDER','MARITAL_STATUS','JOB_STATUS','SEPARATION_REASON')]),
                       by=c('EMPLOYEE_ID','MAX_PP'='PAYROLL_SEQUENCE'))

# Create the target variable:retire(0-No, 1-Yes), and exclude those who leave the city but not because of retirement
all_employee=all_employee %>%
  mutate(retire = if_else(JOB_STATUS == 'Active',0,
                          if_else(SEPARATION_REASON == 'Retired/Pensioned',1,2))) %>%
  filter(retire != 2)

all_employee = all_employee[,-c(18,19)]

# Add numeric features : features related to salary
salary_summary = rawdata %>%
  filter(JOB_STATUS == 'Active' & CHECK_TYPE == 'Regular') %>%
  group_by(EMPLOYEE_ID,FISCAL_YEAR,JOB_CLASS_TITLE,SALARY_STEP) %>%
  summarise(RATE_PP=mean(BIWEEKLY_RATE),HW_HOURS_PP=mean(HW_TYPE_HOURS),OT_HOURS_PP
            =mean(OT_TYPE_HOURS),HW_TYPE_PAY=sum(HW_TYPE_PAY),OT_TYPE_PAY=sum(OT_TYPE_PAY)) %>%
  mutate(OT_TYPE_PAY_PERSENT=OT_TYPE_PAY/(OT_TYPE_PAY+HW_TYPE_PAY),
         RATE_PP_ESTIMATE=RATE_PP+RATE_PP*OT_TYPE_PAY/HW_TYPE_PAY)

all_employee=left_join(all_employee,salary_summary,by=c('EMPLOYEE_ID','FISCAL_YEAR','JOB_CLASS_TITLE','SALARY_STEP'))
all_employee=all_employee[,-c(22,23)]

# Add categorical features: mou and whether_supervisor
all_employee=left_join(all_employee,unique(rawdata[,c('EMPLOYEE_ID','MOU','PAYROLL_SEQUENCE')]),
                      by=c('EMPLOYEE_ID','MAX_PP'='PAYROLL_SEQUENCE'))

# Based on the name of MOU, we define employees belong to MOU12/13/16/17/19/20/22/25/32/36/39/63 are supervisors
# reference : http://cao.lacity.org/MOUs/
all_employee$whether_supervisor=0
all_employee[all_employee$MOU %in% c(12,13,16,17,19,20,22,25,32,36,39,63),'whether_supervisor']=1

# Add categorical features : highest_salary_step
salary_step=read_excel('JOB_CLASSES.xlsx',sheet = 1)
salary_step=salary_step %>% filter (!is.na(HIGHEST_STEP))

all_employee=left_join(all_employee,unique(rawdata[,c('EMPLOYEE_ID','PAYROLL_SEQUENCE','JOB_CLASS')]),
                       by=c('EMPLOYEE_ID','MAX_PP'='PAYROLL_SEQUENCE'))

# filter some special cases (same job_class_title & pay_grade, but have 2 highest steps basedon the JOb_group)
salary_step = salary_step %>% 
  filter(JOB_CLASS_TITLE !='MUNICIPAL POLICE OFFICER III') %>%
  filter(JOB_CLASS_TITLE != 'MACHINIST SUPERVISOR' | PAY_GRADE != '0') %>%
  filter(JOB_CLASS_TITLE != 'SUPT OF OPERATIONS I' | PAY_GRADE != '1') %>%
  filter(JOB_CLASS_TITLE != 'CONSTR INSPECTOR' | PAY_GRADE != 'Z') %>%
  filter(JOB_CLASS_TITLE != 'SR CONSTR INSPECTOR' | PAY_GRADE != 'A') %>%
  filter(JOB_CLASS_TITLE != 'SR CONSTR INSPECTOR' | PAY_GRADE != 'X')

# add highest salary step into the dataset
salary_step$JOB=as.numeric(salary_step$JOB)
all_employee=left_join(all_employee,unique(salary_step[,c('JOB','JOB_CLASS_TITLE','PAY_GRADE','HIGHEST_STEP')]),
                       by=c('JOB_CLASS'='JOB','JOB_CLASS_TITLE','PAY_GRADE'))

# fill in the NA in highest_step
all_employee[is.na(all_employee$HIGHEST_STEP) & all_employee$JOB_CLASS %in% c(3766,5113),'HIGHEST_STEP']=0
all_employee[is.na(all_employee$HIGHEST_STEP) & all_employee$JOB_CLASS %in% c(1790,1960,2340,3183,7209,7211,7213,7214),'HIGHEST_STEP']=15
all_employee[is.na(all_employee$HIGHEST_STEP) & all_employee$JOB_CLASS %in% c(3198,7294),'HIGHEST_STEP']=5
all_employee[is.na(all_employee$HIGHEST_STEP) & all_employee$JOB_CLASS==1576,'HIGHEST_STEP']=11
all_employee[is.na(all_employee$HIGHEST_STEP),'HIGHEST_STEP']=12

# add other variables related to highest_step
all_employee$dif_step=all_employee$HIGHEST_STEP-all_employee$SALARY_STEP

all_employee[all_employee$dif_step==0,'whether_highest']=1
all_employee[all_employee$dif_step!=0,'whether_highest']=0

# deal with dif_step < 0
all_employee[all_employee$dif_step<0 & all_employee$HIGHEST_STEP ==12,'HIGHEST_STEP']=15

all_employee[all_employee$dif_step<0 & all_employee$HIGHEST_STEP ==0 & all_employee$SALARY_STEP<=5,'HIGHEST_STEP']=5
all_employee[all_employee$dif_step<0 & all_employee$HIGHEST_STEP ==0 & all_employee$SALARY_STEP==6,'HIGHEST_STEP']=6
all_employee[all_employee$dif_step<0 & all_employee$HIGHEST_STEP ==0 & all_employee$SALARY_STEP<=8,'HIGHEST_STEP']=8
all_employee[all_employee$dif_step<0 & all_employee$HIGHEST_STEP ==0 & all_employee$SALARY_STEP<=12,'HIGHEST_STEP']=12

all_employee[all_employee$dif_step<0 & all_employee$HIGHEST_STEP ==2,'HIGHEST_STEP']=6

all_employee[all_employee$dif_step<0 & all_employee$HIGHEST_STEP ==6,'HIGHEST_STEP']=8

# redo the dif_step variable
all_employee$dif_step=all_employee$HIGHEST_STEP-all_employee$SALARY_STEP
all_employee[all_employee$dif_step==0,'whether_highest']=1
all_employee[all_employee$dif_step!=0,'whether_highest']=0


#####################################################################################################################
# Prepare the dataset for modeling

# filter the eligible employee in FY2013-FY2018 (train+test dataset for the model)
e_employee2=all_employee %>%
  filter(age>=55 & service_year >=10)

# Add numeric features: retire pension & retirement_factor
pension=e_employee2 %>%
  select(EMPLOYEE_ID,FISCAL_YEAR,MAX_PP,age,service_year)

pension$retirement_factor=1

pension[pension$age==55 & pension$service_year <30, 'retirement_factor'] =0.925
pension[pension$age==56 & pension$service_year <30, 'retirement_factor'] =0.94
pension[pension$age==57 & pension$service_year <30, 'retirement_factor'] =0.955
pension[pension$age==58 & pension$service_year <30, 'retirement_factor'] =0.97
pension[pension$age==59 & pension$service_year <30, 'retirement_factor'] =0.985

pension= left_join(pension,unique(rawdata[,c('EMPLOYEE_ID','PAYROLL_SEQUENCE','BIWEEKLY_RATE')]),
                   by=c('EMPLOYEE_ID','MAX_PP'='PAYROLL_SEQUENCE'))

pension$retirement_pension=pension$BIWEEKLY_RATE * 26/12 *pension$service_year * pension$retirement_factor * 0.0216

e_employee2=left_join(e_employee2,pension[,c('EMPLOYEE_ID','MAX_PP','retirement_factor','retirement_pension')],
                     by=c('EMPLOYEE_ID','MAX_PP'))

# Add numeric features: employee count and elighble employee persent for specific jobclass
ecount=all_employee %>%
  mutate(whether_eligible = if_else(age>=55 & service_year >= 10, 1,0)) %>%
  group_by(FISCAL_YEAR,FMS_DEPARTMENT_TITLE,JOB_CLASS_TITLE,whether_eligible) %>%
  summarise(ecount=n_distinct(EMPLOYEE_ID)) %>%
  spread(whether_eligible,ecount)

ecount[is.na(ecount)]=0
ecount$total_ecount=ecount$`1`+ecount$`0`
ecount$eligible_persent=ecount$`1`/ecount$total_ecount

e_employee2=left_join(e_employee2,ecount[,c('FISCAL_YEAR','FMS_DEPARTMENT_TITLE','JOB_CLASS_TITLE',
                                                   'eligible_persent','total_ecount')],
                     by=c('FISCAL_YEAR','FMS_DEPARTMENT_TITLE','JOB_CLASS_TITLE'))


# CLEAN the Dataset

# step1:fill in missing value
# For RATE_PP,HW_HOURS_PP,OT_HOURS_PP,OT_TYPE_PAY_PERSENT,RATE_PP_ESTIMATE, replace NAs with same JOBCLASS's avg value
salary_summary = salary_summary %>%
  group_by(JOB_CLASS_TITLE,SALARY_STEP) %>%
  summarise(RATE_PP2=mean(RATE_PP),HW_HOURS_PP2=mean(HW_HOURS_PP),OT_HOURS_PP2
            =mean(OT_HOURS_PP),HW_TYPE_PAY2=mean(HW_TYPE_PAY),OT_TYPE_PAY2=mean(OT_TYPE_PAY)) %>%
  mutate(OT_TYPE_PAY_PERSENT2=OT_TYPE_PAY2/(OT_TYPE_PAY2+HW_TYPE_PAY2),
         RATE_PP_ESTIMATE2=RATE_PP2+RATE_PP2*OT_TYPE_PAY2/HW_TYPE_PAY2)

e_employee2=left_join(e_employee2,salary_summary[,c('JOB_CLASS_TITLE','SALARY_STEP',
                                                  'RATE_PP2','HW_HOURS_PP2','OT_HOURS_PP2','OT_TYPE_PAY_PERSENT2','RATE_PP_ESTIMATE2')],
                     by=c('JOB_CLASS_TITLE','SALARY_STEP'))

e_employee2[is.na(e_employee2$RATE_PP),'RATE_PP']=e_employee2[is.na(e_employee2$RATE_PP),'RATE_PP2']
e_employee2[is.na(e_employee2$HW_HOURS_PP),'HW_HOURS_PP']=e_employee2[is.na(e_employee2$HW_HOURS_PP),'HW_HOURS_PP2']
e_employee2[is.na(e_employee2$OT_HOURS_PP),'OT_HOURS_PP']=e_employee2[is.na(e_employee2$OT_HOURS_PP),'OT_HOURS_PP2']
e_employee2[is.na(e_employee2$OT_TYPE_PAY_PERSENT),'OT_TYPE_PAY_PERSENT']=e_employee2[is.na(e_employee2$OT_TYPE_PAY_PERSENT),'OT_TYPE_PAY_PERSENT2']
e_employee2[is.na(e_employee2$RATE_PP_ESTIMATE),'RATE_PP_ESTIMATE']=e_employee2[is.na(e_employee2$RATE_PP_ESTIMATE),'RATE_PP_ESTIMATE2']

e_employee2=e_employee2[,-c(32:36)]
e_employee2=e_employee2 %>% filter(!is.na(RATE_PP))
e_employee2=e_employee2 %>% filter(!is.na( OT_TYPE_PAY_PERSENT))

# step2: encoding categorical variables
# work_schedule
e_employee2[e_employee2$WORK_SCHEDULE %in% c('N/A','Variable',' '),'WORK_SCHEDULE' ]='Other'
pattern=e_employee2 %>%
  group_by(WORK_SCHEDULE) %>%
  summarise(WORK_SCHEDULE_persent=mean(retire))
e_employee2=left_join(e_employee2,pattern,by='WORK_SCHEDULE')

# RETIREMENT_PENSION_PLAN
pattern=e_employee2 %>%
  group_by(RETIREMENT_PENSION_PLAN) %>%
  summarise(RETIREMENT_PENSION_PLAN_persent=mean(retire))
e_employee2=left_join(e_employee2,pattern,by='RETIREMENT_PENSION_PLAN')

#ETHNICITY
e_employee2[e_employee2$ETHNICITY %in% c('Unknown',''),'ETHNICITY' ]='Other'
pattern=e_employee2 %>%
  group_by(ETHNICITY) %>%
  summarise(ETHNICITY_persent=mean(retire))
e_employee2=left_join(e_employee2,pattern,by='ETHNICITY')

#GENDER
pattern=e_employee2 %>%
  group_by(GENDER) %>%
  summarise(GENDER_persent=mean(retire))
e_employee2=left_join(e_employee2,pattern,by='GENDER')

# MARITAL_STATUS
e_employee2[e_employee2$MARITAL_STATUS %in% c(''),'MARITAL_STATUS' ]='Unknown'
pattern=e_employee2 %>%
  group_by(MARITAL_STATUS) %>%
  summarise(MARITAL_STATUS_persent=mean(retire))
e_employee2=left_join(e_employee2,pattern,by='MARITAL_STATUS')

# MOU
pattern=e_employee2 %>%
  group_by(MOU) %>%
  summarise(MOU_persent=mean(retire))
e_employee2=left_join(e_employee2,pattern,by='MOU')

# HIGHEST_STEP
pattern=e_employee2 %>%
  group_by(HIGHEST_STEP) %>%
  summarise(HSTEP_persent=mean(retire))
e_employee2=left_join(e_employee2,pattern,by='HIGHEST_STEP')

# JOB_CLASS
pattern=e_employee2 %>%
  group_by(FMS_DEPARTMENT_TITLE,JOB_CLASS_TITLE) %>%
  summarise(jclass_persent=mean(retire))
e_employee2=left_join(e_employee2,pattern,by=c('FMS_DEPARTMENT_TITLE','JOB_CLASS_TITLE'))

# CURRENT_STEP
pattern=e_employee2 %>%
group_by(HIGHEST_STEP,SALARY_STEP) %>%
  summarise(CSTEP_persent=mean(retire))
e_employee2=left_join(e_employee2,pattern,by=c('HIGHEST_STEP','SALARY_STEP'))

#####################################################################################################################
# Prepare the dataset for prediction
# FY2019: A new year normally contains 2 parts : those who didn't retire in last year & those who become eligible in this year
e_employee_2019_1=all_employee %>%
  filter(FISCAL_YEAR==2018 & age>=55 & service_year >=10 & retire == 0 )

e_employee_2019_2=all_employee %>%
  filter(FISCAL_YEAR==2018 & retire == 0 ) %>%
  filter((age ==54 & service_year>=9) | (age >=55 & service_year ==9) )

e_employee_2019=rbind(e_employee_2019_1,e_employee_2019_2)


# some thing will change in the new year: age, service_year
e_employee_2019$FISCAL_YEAR=e_employee_2019$FISCAL_YEAR+1
e_employee_2019$age=e_employee_2019$age+1
e_employee_2019$service_year=e_employee_2019$service_year+1

# salary_step, since most of the employees who don't achieve highest step belong to 12/15 group, we will assume increase 1 step each year
e_employee_2019[e_employee_2019$whether_highest==0,'SALARY_STEP']=
  e_employee_2019[e_employee_2019$whether_highest==0, 'SALARY_STEP']+1

# update the dif_step and whether_highest
e_employee_2019$dif_step=e_employee_2019$HIGHEST_STEP-e_employee_2019$SALARY_STEP
e_employee_2019[e_employee_2019$dif_step==0,'whether_highest']=1
e_employee_2019[e_employee_2019$dif_step!=0,'whether_highest']=0

# for Rate_PP, we assume a 2% annual increase even the employee don't get promoted or increase salary_step
e_employee_2019$RATE_PP=e_employee_2019$RATE_PP * 1.02
e_employee_2019$RATE_PP_ESTIMATE=e_employee_2019$RATE_PP_ESTIMATE * 1.02

# retire pension

pension=e_employee_2019 %>%
  select(EMPLOYEE_ID,FISCAL_YEAR,MAX_PP,age,service_year)

pension$retirement_factor=1

pension[pension$age==55 & pension$service_year <30, 'retirement_factor'] =0.925
pension[pension$age==56 & pension$service_year <30, 'retirement_factor'] =0.94
pension[pension$age==57 & pension$service_year <30, 'retirement_factor'] =0.955
pension[pension$age==58 & pension$service_year <30, 'retirement_factor'] =0.97
pension[pension$age==59 & pension$service_year <30, 'retirement_factor'] =0.985

pension= left_join(pension,unique(rawdata[,c('EMPLOYEE_ID','PAYROLL_SEQUENCE','BIWEEKLY_RATE')]),
                   by=c('EMPLOYEE_ID','MAX_PP'='PAYROLL_SEQUENCE'))

pension$retirement_pension=pension$BIWEEKLY_RATE*1.02 * 26/12 *pension$service_year * pension$retirement_factor * 0.0216

e_employee_2019=left_join(e_employee_2019,pension[,c('EMPLOYEE_ID','MAX_PP','retirement_factor','retirement_pension')],
                      by=c('EMPLOYEE_ID','MAX_PP'))

# employee count and elighble employee persent for specific jobclass (Here we just assume no new employee, need to optimize)
ecount=all_employee %>%
  filter(FISCAL_YEAR==2018) %>%
  mutate(whether_eligible = if_else(age>=54 & service_year >= 9, 1,0)) %>%
  group_by(FISCAL_YEAR,FMS_DEPARTMENT_TITLE,JOB_CLASS_TITLE,whether_eligible) %>%
  summarise(ecount=n_distinct(EMPLOYEE_ID)) %>%
  spread(whether_eligible,ecount)

ecount[is.na(ecount)]=0
ecount$total_ecount=ecount$`1`+ecount$`0`
ecount$eligible_persent=ecount$`1`/ecount$total_ecount

e_employee_2019=left_join(e_employee_2019,ecount[,c('FMS_DEPARTMENT_TITLE','JOB_CLASS_TITLE',
                                           'eligible_persent','total_ecount')],
                      by=c('FMS_DEPARTMENT_TITLE','JOB_CLASS_TITLE'))

# check and fill in the NAs
summary(e_employee_2019)

# OT_TYPE_PAY_PERSENT & OT_TYPE_PAY_PERSENT has NAs
e_employee_2019=left_join(e_employee_2019,salary_summary[,c('JOB_CLASS_TITLE','SALARY_STEP',
                                                    'RATE_PP2','HW_HOURS_PP2','OT_HOURS_PP2','OT_TYPE_PAY_PERSENT2','RATE_PP_ESTIMATE2')],
                      by=c('JOB_CLASS_TITLE','SALARY_STEP'))

e_employee_2019[is.na(e_employee_2019$OT_TYPE_PAY_PERSENT),'OT_TYPE_PAY_PERSENT']=e_employee_2019[is.na(e_employee_2019$OT_TYPE_PAY_PERSENT),'OT_TYPE_PAY_PERSENT2']
e_employee_2019[is.na(e_employee_2019$RATE_PP_ESTIMATE),'RATE_PP_ESTIMATE']=e_employee_2019[is.na(e_employee_2019$RATE_PP_ESTIMATE),'RATE_PP_ESTIMATE2'] * 1.02

e_employee_2019=e_employee_2019[,-c(34:38)]
e_employee_2019=e_employee_2019 %>% filter(!is.na(OT_TYPE_PAY_PERSENT))

# remove uselesee columns
e_employee_2019=e_employee_2019[,-c(2:5,18,26)]


# step2: encoding categorical variables
# work_schedule
e_employee_2019[e_employee_2019$WORK_SCHEDULE %in% c('N/A','Variable',' '),'WORK_SCHEDULE' ]='Other'
pattern=e_employee2 %>%
  group_by(WORK_SCHEDULE) %>%
  summarise(WORK_SCHEDULE_persent=mean(retire))
e_employee_2019=left_join(e_employee_2019,pattern,by='WORK_SCHEDULE')

# RETIREMENT_PENSION_PLAN
pattern=e_employee2 %>%
  group_by(RETIREMENT_PENSION_PLAN) %>%
  summarise(RETIREMENT_PENSION_PLAN_persent=mean(retire))
e_employee_2019=left_join(e_employee_2019,pattern,by='RETIREMENT_PENSION_PLAN')

#ETHNICITY
e_employee_2019[e_employee_2019$ETHNICITY %in% c('Unknown',''),'ETHNICITY' ]='Other'
pattern=e_employee2 %>%
  group_by(ETHNICITY) %>%
  summarise(ETHNICITY_persent=mean(retire))
e_employee_2019=left_join(e_employee_2019,pattern,by='ETHNICITY')

#GENDER
pattern=e_employee2 %>%
  group_by(GENDER) %>%
  summarise(GENDER_persent=mean(retire))
e_employee_2019=left_join(e_employee_2019,pattern,by='GENDER')

# MARITAL_STATUS
e_employee_2019[e_employee_2019$MARITAL_STATUS %in% c(''),'MARITAL_STATUS' ]='Unknown'
pattern=e_employee2 %>%
  group_by(MARITAL_STATUS) %>%
  summarise(MARITAL_STATUS_persent=mean(retire))
e_employee_2019=left_join(e_employee_2019,pattern,by='MARITAL_STATUS')

# MOU
pattern=e_employee2 %>%
  group_by(MOU) %>%
  summarise(MOU_persent=mean(retire))
e_employee_2019=left_join(e_employee_2019,pattern,by='MOU')

# HIGHEST_STEP
pattern=e_employee2 %>%
  group_by(HIGHEST_STEP) %>%
  summarise(HSTEP_persent=mean(retire))
e_employee_2019=left_join(e_employee_2019,pattern,by='HIGHEST_STEP')

# JOB_CLASS
pattern=e_employee2 %>%
  group_by(FMS_DEPARTMENT_TITLE,JOB_CLASS_TITLE) %>%
  summarise(jclass_persent=mean(retire))
e_employee_2019=left_join(e_employee_2019,pattern,by=c('FMS_DEPARTMENT_TITLE','JOB_CLASS_TITLE'))

e_employee_2019[is.na(e_employee_2019$jclass_persent),'jclass_persent']=mean(pattern$jclass_persent)

# CURRENT_STEP
pattern=e_employee2 %>%
  group_by(HIGHEST_STEP,SALARY_STEP) %>%
  summarise(CSTEP_persent=mean(retire))
e_employee_2019=left_join(e_employee_2019,pattern,by=c('HIGHEST_STEP','SALARY_STEP'))


e_employee_2019=e_employee_2019[!is.na(e_employee_2019$CSTEP_persent),]


write.csv(e_employee_2019,'e_employee_2019.csv')
