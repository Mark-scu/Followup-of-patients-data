##Analysis for medication adherence on Praluent

#import data from Excel
pack <- c('readxl','tidyverse','nnet','ggplot2','ggstatsplot','rcompanion','car','forestplot','questionr')

for(i in pack){library(i,character.only = T)}



followup_data <- read_excel("C:/Users/ZHANG/Desktop/week3/患者随访数据.xlsx")
info_data <- read_excel("C:/Users/ZHANG/Desktop/week3/入组患者信息.xlsx")

#integrate the two sheets
all_info <- merge(followup_data, info_data, by="患者编号",all = TRUE)

info <- all_info %>% mutate(age=as.numeric(年龄)) %>% mutate(ldl_c_latest=as.numeric(
  最近一次低密度脂蛋白胆固醇)) %>% mutate(ldl_c_1st=as.numeric(首次注射降脂药物前的最近一次低密度脂蛋白胆固醇))
prep_data <- subset(info, select = c(患者编号, age, 性别,入院诊断,是否使用注射降脂药物,
                                     ldl_c_1st,患者是否有其他疾病,既往是否服用过口服降脂药物,
                                     既往是否使用过注射降脂药物,随访结果, 随访类型,
                                     随访状态,本次就诊是否已接受冠脉造影检查,本次就诊是否已接受心脏支架手术,
                                     ldl_c_latest,是否患有高血压,是否患有糖尿病,是否血脂异常,
                                     是否患有其他疾病,是否吸烟或曾吸烟))
#clean the environment panel
rm(all_info,followup_data,info_data,info,pack)

totalparticipants <- prep_data[!duplicated(prep_data$患者编号),]

##**************************************************************##

#cleaning data to determine the actual participants of this study(delete patients who didn't have the followup)
del1 <- which(prep_data$随访状态=='逾期')
prep_data <- prep_data[-del1,]
#patients from outside of the hospitals that failed to participate 
del2 <- which(prep_data$随访类型%in%c('一个周','一个月')&prep_data$随访结果%in%c('无效电话','拒接脱落','空号'))
prep_data <- prep_data[-del2,]
#patients can' contact for the whole period
one <- filter(prep_data,随访结果=='无人接听（停机、关机）'&随访类型=='一个月')
two <- filter(prep_data,随访结果=='无人接听（停机、关机）'&随访类型=='三个月')
three <- filter(prep_data,随访结果=='无人接听（停机、关机）'&随访类型=='六个月')
del3 <- merge(one,two,by=c('患者编号','性别','入院诊断')) %>%merge(three,by=c('患者编号','性别','入院诊断'))

prep_data <- subset(prep_data,!prep_data$患者编号%in%c(del3$患者编号))

rm(del1,del2,del3,one,two,three)

actualparticipants <- prep_data[!duplicated(prep_data$患者编号),]

##**************************************************************##

#Reassignment the variable
#gender
prep_data$性别[prep_data$性别=='男' ] <- 0
prep_data$性别[prep_data$性别=='女' ] <- 1

#diagnosis
prep_data$入院诊断[prep_data$入院诊断 %in% c('中风', '脑卒中', '缺血性卒中'
                                     ,'短暂性脑缺血发作')] <- 0
prep_data$入院诊断[prep_data$入院诊断 %in% c('冠心病', '多支病变',
                                     '动脉粥样硬化')] <- 1
prep_data$入院诊断[prep_data$入院诊断 %in% c('不稳定型心绞痛', '急性冠脉综合征',
                                     '心肌梗死')] <- 2
prep_data$入院诊断[prep_data$入院诊断 %in% c('血脂异常')] <- 3
prep_data$入院诊断[prep_data$入院诊断 %in% c('其他', '--')] <- 4

#combined disease
prep_data$患者是否有其他疾病 <- ifelse(prep_data$患者是否有其他疾病 =='否'|
                                prep_data$患者是否有其他疾病 =='--',0,1)

#high blood pressure?
prep_data$是否患有高血压 <- ifelse(prep_data$是否患有高血压 =='否'|
                              prep_data$是否患有高血压 == '--',0,1)
#diabets?
prep_data$是否患有糖尿病 <- ifelse(prep_data$是否患有糖尿病 == '否'|
                              prep_data$是否患有高血压 == '--',0,1)
#blood lipid disorder?
prep_data$是否血脂异常 <- ifelse(prep_data$是否血脂异常 =='否'|prep_data$是否血脂异常
                           =='--',0,1)
#other disease?
prep_data$是否患有其他疾病 <- ifelse(prep_data$是否患有其他疾病=='否'|
                               prep_data$是否患有其他疾病=='--',0,1)

#pcsk9i_history
prep_data$既往是否使用过注射降脂药物 <- ifelse(prep_data$既往是否使用过注射降脂药物=='否'|
                                    prep_data$既往是否使用过注射降脂药物=='--',0,1)

#oral drugs history
prep_data$既往是否服用过口服降脂药物 <- ifelse(prep_data$既往是否服用过口服降脂药物=='否'|
                                    prep_data$既往是否服用过口服降脂药物=='--',0,1)

#followup type
prep_data$随访类型[prep_data$随访类型=='一个月'] <- 1
prep_data$随访类型[prep_data$随访类型=='三个月'] <- 2
prep_data$随访类型[prep_data$随访类型=='六个月'] <- 3

#followup results
prep_data$随访结果[prep_data$随访结果=='正常随访'] <- 1
prep_data$随访结果[prep_data$随访结果=='停药'] <- 2
prep_data$随访结果[prep_data$随访结果=='未用药'] <- 3
prep_data$随访结果[prep_data$随访结果=='空号'|prep_data$随访结果=='无效电话'|
                 prep_data$随访结果=='拒接脱落'|prep_data$随访结果=='无人接听（停机、关机）'] <- 4

#have CAG OR NOT
prep_data$本次就诊是否已接受冠脉造影检查 <- ifelse(prep_data$本次就诊是否已接受冠脉造影检查
                                    %in% c('否','--'),0,1)

#have PCI or not
prep_data$本次就诊是否已接受心脏支架手术 <- ifelse(prep_data$本次就诊是否已接受心脏支架手术
                                    %in% c('否','--'),0,1)

#usage of PCSK9i？
prep_data$是否使用注射降脂药物 <- ifelse(prep_data$是否使用注射降脂药物=='否',0,1)

#smoking history
prep_data$是否吸烟或曾吸烟[prep_data$是否吸烟或曾吸烟=='否'] <- 0
prep_data$是否吸烟或曾吸烟[prep_data$是否吸烟或曾吸烟=='是'] <- 1

#LDL-C target achievement
prep_data <- prep_data %>% mutate(riskfactor=as.numeric(prep_data$是否患有高血压)+
                                    as.numeric(prep_data$是否患有糖尿病)+
                                    ifelse(prep_data$ldl_c_1st>=4.9,1,0)+
                                    as.numeric(prep_data$本次就诊是否已接受心脏支架手术)+
                                    as.numeric(prep_data$是否吸烟或曾吸烟)) %>% 
  mutate(reduction=(ldl_c_1st-ldl_c_latest)/ldl_c_1st) 
prep_data$target4 <-ifelse(prep_data$ldl_c_latest<1.4&prep_data$reduction>0.5,1,0)
prep_data$target8 <-ifelse(prep_data$ldl_c_latest<1.8&prep_data$reduction>0.5,1,0)

#************************************************************************************#

##descriptive analysis for patients who don't use pcsk9i during the study
zeropcsk9i <- filter(prep_data,随访结果==3&随访类型%in%c(1,2,3))
#gender
sum(as.numeric(zeropcsk9i$性别))
#age
hist(zeropcsk9i$age)
shapiro.test(zeropcsk9i$age)
#the median age and quartile of age
zeropcsk9i %>% summarise(avg=median(age),q25=quantile(age,probs=0.25),q75=quantile(age,probs=0.75))
#num of diagnosis type
nrow(zeropcsk9i[zeropcsk9i$入院诊断==0,])
nrow(zeropcsk9i[zeropcsk9i$入院诊断==1,])
nrow(zeropcsk9i[zeropcsk9i$入院诊断==2,])
nrow(zeropcsk9i[zeropcsk9i$入院诊断==3,])
nrow(zeropcsk9i[zeropcsk9i$入院诊断==4,])

#num of combined disease
sum(zeropcsk9i$患者是否有其他疾病)
#num of high blood pressure
sum(zeropcsk9i$是否患有高血压)
#num of diabets
sum(zeropcsk9i$是否患有糖尿病)
#num of blood lipid disorder
sum(zeropcsk9i$是否血脂异常)
#num of other disease
sum(zeropcsk9i$是否患有其他疾病)
#num of PCI
sum(zeropcsk9i$本次就诊是否已接受心脏支架手术)
#num of CAG
sum(zeropcsk9i$本次就诊是否已接受冠脉造影检查)
#oral drugs history
sum(zeropcsk9i$既往是否服用过口服降脂药物)
#check the normality of first LDL-C
hist(zeropcsk9i$ldl_c_1st)
shapiro.test(zeropcsk9i$ldl_c_1st)
sum(is.na(zeropcsk9i$ldl_c_1st))
#the median LDL-C and quartile of it
zeropcsk9i %>% summarise(avg=median(ldl_c_1st,na.rm = TRUE),q25=quantile(ldl_c_1st,probs=0.25,na.rm = TRUE),
                         q75=quantile(ldl_c_1st,probs=0.75,na.rm = TRUE))
#check the normality of last LDL-C
hist(zeropcsk9i$ldl_c_latest)
shapiro.test(zeropcsk9i$ldl_c_latest)
sum(is.na(zeropcsk9i$ldl_c_latest))
#the median LDL-C and quartile of it
zeropcsk9i %>% reframe(avg=median(ldl_c_latest,na.rm = TRUE),q25=quantile(ldl_c_latest,probs=0.25,na.rm = TRUE),
                         q75=quantile(ldl_c_latest,na.rm = TRUE))
#num of smokers
sum(as.numeric(na.omit(zeropcsk9i$是否吸烟或曾吸烟)))
sum(is.na(zeropcsk9i$是否吸烟或曾吸烟))

##**************************************************************************************8*##

##descriptive analysis for patients that completed 3 followup and use pcsk9i constantly
sixthmonth <- filter(prep_data,是否使用注射降脂药物==1&随访类型==3&随访结果==1)
thirdmonth <- filter(prep_data,是否使用注射降脂药物==1&随访类型==2&随访结果==1)
firstmonth <- filter(prep_data,是否使用注射降脂药物==1&随访类型==1&随访结果==1)
threefull <- merge(thirdmonth,firstmonth,by=c('患者编号','性别','入院诊断',
                                              '本次就诊是否已接受心脏支架手术',
                                              '本次就诊是否已接受冠脉造影检查',
                                              '是否患有其他疾病','是否血脂异常',
                                              '是否患有糖尿病','既往是否服用过口服降脂药物',
                                              '是否患有高血压','患者是否有其他疾病',
                                              'age','既往是否使用过注射降脂药物')) %>%merge(sixthmonth,by=c(
                                                '患者编号','性别','入院诊断','本次就诊是否已接受心脏支架手术',
                                                '是否患有其他疾病','是否血脂异常','是否患有糖尿病',
                                                '本次就诊是否已接受冠脉造影检查',
                                                '既往是否使用过注射降脂药物','既往是否服用过口服降脂药物',
                                                '是否患有高血压','患者是否有其他疾病','age'))

##patients characteristics of the group using pcsk9i constantly
#gender
sum(as.numeric(threefull$性别))
#age
shapiro.test(threefull$age)
threefull %>% summarise(avg=median(age),q25=quantile(age,probs = 0.25),q75=quantile(age,probs = 0.75))
#diagnosis type
nrow(threefull[threefull$入院诊断==0,])
nrow(threefull[threefull$入院诊断==1,])
nrow(threefull[threefull$入院诊断==2,])
nrow(threefull[threefull$入院诊断==3,])
nrow(threefull[threefull$入院诊断==4,])

#num of combined disease
sum(threefull$患者是否有其他疾病)
#num of high blood pressure
sum(threefull$是否患有高血压)
#num of diabets
sum(threefull$是否患有糖尿病)
#num of blood lipid disorder
sum(threefull$是否血脂异常)
#num of other disease
sum(threefull$是否患有其他疾病)
#num of PCI
sum(threefull$本次就诊是否已接受心脏支架手术)
#num of CAG
sum(threefull$本次就诊是否已接受冠脉造影检查)
#oral drugs history
sum(threefull$既往是否服用过口服降脂药物)
#pcsk9i history use
sum(threefull$既往是否使用过注射降脂药物)
#check the normality of first LDL-C
hist(threefull$ldl_c_1st)
shapiro.test(threefull$ldl_c_1st)
#the median LDL-C and quartile of it
threefull %>% summarise(avg=median(ldl_c_1st),q25=quantile(ldl_c_1st,probs=0.25),
                        q75=quantile(ldl_c_1st,probs=0.75))
#check the normality of last LDL-C
hist(threefull$ldl_c_latest)
shapiro.test(threefull$ldl_c_latest)
threefull %>% summarise(avg=median(ldl_c_latest,na.rm=TRUE),q25=quantile(ldl_c_latest,probs=0.25,na.rm = TRUE),
                        q75=quantile(ldl_c_latest,na.rm = TRUE))
#num of smokers
sum(as.numeric(na.omit(threefull$是否吸烟或曾吸烟)))
sum(is.na(threefull$是否吸烟或曾吸烟))

#****************************************************************************************************#

##descriptive analysis for patients that used psc9i in 1st followup failed to contact in 2nd and 3rd
no2nd <- filter(prep_data,随访类型==2&随访结果%in%c(2,4))
no3rd <- filter(prep_data,随访类型==3&随访结果%in%c(2,4))
only1 <- merge(no2nd,no3rd,by=c('患者编号','性别','入院诊断','本次就诊是否已接受心脏支架手术',
                                '本次就诊是否已接受冠脉造影检查','是否患有其他疾病','是否血脂异常',
                                '是否患有糖尿病','既往是否服用过口服降脂药物','是否患有高血压',
                                '患者是否有其他疾病','age','既往是否使用过注射降脂药物')) %>%merge(
                                  firstmonth,by=c('患者编号','性别','入院诊断',
                                                  '本次就诊是否已接受心脏支架手术',
                                                  '本次就诊是否已接受冠脉造影检查',
                                                  '是否患有其他疾病','是否血脂异常',
                                                  '是否患有糖尿病','既往是否服用过口服降脂药物',
                                                  '是否患有高血压','患者是否有其他疾病',
                                                  'age','既往是否使用过注射降脂药物'))

#gender
sum(as.numeric(only1$性别))
#age
hist(only1$age)
shapiro.test(only1$age)
#the median age and quartile of age
only1 %>% summarise(avg=median(age),q25=quantile(age,probs=0.25),q75=quantile(age,probs=0.75))
#num of diagnosis type
nrow(only1[only1$入院诊断==0,])
nrow(only1[only1$入院诊断==1,])
nrow(only1[only1$入院诊断==2,])
nrow(only1[only1$入院诊断==3,])
nrow(only1[only1$入院诊断==4,])

#num of combined disease
sum(only1$患者是否有其他疾病)
#num of high blood pressure
sum(only1$是否患有高血压)
#num of diabets
sum(only1$是否患有糖尿病)
#num of blood lipid disorder
sum(only1$是否血脂异常)
#num of other disease
sum(only1$是否患有其他疾病)
#num of PCI
sum(only1$本次就诊是否已接受心脏支架手术)
#num of CAG
sum(only1$本次就诊是否已接受冠脉造影检查)
#oral drugs history
sum(only1$既往是否服用过口服降脂药物)
#pcsk9i history use
sum(only1$既往是否使用过注射降脂药物)
#check the normality of first LDL-C
hist(only1$ldl_c_1st)
shapiro.test(only1$ldl_c_1st)
#the median LDL-C and quartile of it
only1 %>% summarise(avg=median(ldl_c_1st,na.rm = TRUE),q25=quantile(ldl_c_1st,probs=0.25,na.rm = TRUE),
                    q75=quantile(ldl_c_1st,probs=0.75,na.rm = TRUE))
#check the normality of last LDL-C
hist(only1$ldl_c_latest)
shapiro.test(only1$ldl_c_latest)
sum(is.na(only1$ldl_c_latest))
#the median LDL-C and quartile of it
only1 %>% reframe(avg=median(ldl_c_latest,na.rm = TRUE),q25=quantile(ldl_c_latest,probs=0.25,na.rm = TRUE),
                  q75=quantile(ldl_c_latest,na.rm = TRUE))
#num of smokers
sum(as.numeric(na.omit(only1$是否吸烟或曾吸烟)))
sum(is.na(only1$是否吸烟或曾吸烟))

#****************************************************************************************************#
##descriptive analysis for patients that used psc9i in 1st&2nd followup failed to contact in 3rd
onetwono3 <- merge(firstmonth,thirdmonth,by=c('患者编号','性别','入院诊断','本次就诊是否已接受心脏支架手术',
                                              '本次就诊是否已接受冠脉造影检查','是否患有其他疾病','是否血脂异常',
                                              '是否患有糖尿病','既往是否服用过口服降脂药物','是否患有高血压',
                                              '患者是否有其他疾病','age','既往是否使用过注射降脂药物')) %>% 
  merge(no3rd,by=c('患者编号','性别','入院诊断','本次就诊是否已接受心脏支架手术',
                   '本次就诊是否已接受冠脉造影检查','是否患有其他疾病','是否血脂异常',
                   '是否患有糖尿病','既往是否服用过口服降脂药物','是否患有高血压',
                   '患者是否有其他疾病','age','既往是否使用过注射降脂药物'))
#gender
sum(as.numeric(onetwono3$性别))
#age
hist(onetwono3$age)
shapiro.test(onetwono3$age)
#the median age and quartile of age
onetwono3 %>% summarise(avg=median(age),q25=quantile(age,probs=0.25),q75=quantile(age,probs=0.75))
#num of diagnosis type
nrow(onetwono3[onetwono3$入院诊断==0,])
nrow(onetwono3[onetwono3$入院诊断==1,])
nrow(onetwono3[onetwono3$入院诊断==2,])
nrow(onetwono3[onetwono3$入院诊断==3,])
nrow(onetwono3[onetwono3$入院诊断==4,])

#num of combined disease
sum(onetwono3$患者是否有其他疾病)
#num of high blood pressure
sum(onetwono3$是否患有高血压)
#num of diabets
sum(onetwono3$是否患有糖尿病)
#num of blood lipid disorder
sum(onetwono3$是否血脂异常)
#num of other disease
sum(onetwono3$是否患有其他疾病)
#num of PCI
sum(onetwono3$本次就诊是否已接受心脏支架手术)
#num of CAG
sum(onetwono3$本次就诊是否已接受冠脉造影检查)
#oral drugs history
sum(onetwono3$既往是否服用过口服降脂药物)
#pcsk9i history use
sum(onetwono3$既往是否使用过注射降脂药物)
#check the normality of first LDL-C
hist(onetwono3$ldl_c_1st)
shapiro.test(onetwono3$ldl_c_1st)
#the median LDL-C and quartile of it
onetwono3 %>% summarise(avg=median(ldl_c_1st,na.rm = TRUE),q25=quantile(ldl_c_1st,probs=0.25,na.rm = TRUE),
                        q75=quantile(ldl_c_1st,probs=0.75,na.rm = TRUE))
#check the normality of last LDL-C
hist(onetwono3$ldl_c_latest)
shapiro.test(onetwono3$ldl_c_latest)
sum(is.na(onetwono3$ldl_c_latest))
#the median LDL-C and quartile of it
onetwono3 %>% reframe(avg=median(ldl_c_latest,na.rm = TRUE),q25=quantile(ldl_c_latest,probs=0.25,na.rm = TRUE),
                      q75=quantile(ldl_c_latest,na.rm = TRUE))
#num of smokers
sum(as.numeric(na.omit(onetwono3$是否吸烟或曾吸烟)))
sum(is.na(onetwono3$是否吸烟或曾吸烟))

rm(firstmonth,no2nd,no3rd,sixthmonth,thirdmonth)

#********************************************************************************#

ldlcontrol <-  prep_data[!duplicated(prep_data$患者编号),] %>% filter(患者编号%in%c(zeropcsk9i$患者编号,
                                                                            only1$患者编号,onetwono3$患者编号,threefull$患者编号))
##high ascvd risk people
high_risk_people <- filter(ldlcontrol,入院诊断%in%c(0,1,2))
##ultra high risk people
ultra_high_risk_people <- filter(high_risk_people,riskfactor>=2)


#********************************************************************************#

# Chi-square test of the 4 groups above(未用pcsk9i的患者、第一个月正常随访但后续随访失败、第1、3个月正常随访但第6个月随访失败、随访并且使用了六个月pcsk9i的患者)
#gender
group1 <- matrix(c(359,90,49,78,550,152,97,146),nrow = 4,ncol = 2)
chisq.test(group1)
gender <- data.frame(sex=c(zeropcsk9i$性别,only1$性别,onetwono3$性别,threefull$性别)
                     ,groups=c(rep(0,909),rep(1,242),rep(2,146),rep(3,224)))
ggbarstats(data = gender,x=sex,y=groups,title = "Gender distribution of four groups",
           legend.title = "Gender",palette = 'Set2')
#diagnosis type
group2 <- matrix(c(36,6,4,2,627,157,98,158,215,69,41,61,31,10,3,3),nrow = 4,ncol = 4)
chisq.test(group2)
diagnosistype <- data.frame(type=c(zeropcsk9i$入院诊断,only1$入院诊断,onetwono3$入院诊断,
                                   threefull$入院诊断),groups=c(rep(0,909),rep(1,242),rep(2,146),rep(3,224)))
ggbarstats(data = diagnosistype,x=type,y=groups,title = "Type of Diagnosis in four groups",
           legend.title = "Type of Diagnosis",palette = 'Set2')
#combined disease
group3 <- matrix(c(797,218,133,207,112,24,13,17),nrow = 4,ncol = 2)
chisq.test(group3)
combinedisease <- data.frame(type=c(zeropcsk9i$患者是否有其他疾病,only1$患者是否有其他疾病,
                                    onetwono3$患者是否有其他疾病,threefull$患者是否有其他疾病),
                             groups=c(rep(0,909),rep(1,242),rep(2,146),rep(3,224)))
ggbarstats(data = combinedisease,x=type,y=groups,title = "Combined disease in four groups",
           legend.title = "Combined disease",palette = 'Set2')
#smokers
group10<- matrix(c(27,3,0,4,522,201,21,96),nrow = 4,ncol = 2)
fisher.test(group10)
smokers <- data.frame(smokinghistory=c(na.omit(zeropcsk9i$是否吸烟或曾吸烟),na.omit(only1$是否吸烟或曾吸烟),
                                    na.omit(onetwono3$是否吸烟或曾吸烟),na.omit(threefull$是否吸烟或曾吸烟))
                             ,groups=c(rep(0,549),rep(1,204),rep(2,21),rep(3,100)))
ggbarstats(data = smokers,x=smokinghistory,y=groups,title = "Distribution of Smokers in four groups"
           ,legend.title = "Smoking history",palette = 'Set3')


#******************************************#

#PCI
group6<- matrix(c(215,105,50,88,694,137,96,136),nrow = 4,ncol = 2)
chisq.test(group6)
pairwiseNominalIndependence(group6, chisq = TRUE,gtest  = FALSE,cramer = FALSE,fisher = FALSE)
PCI <- data.frame(pci=c(zeropcsk9i$本次就诊是否已接受心脏支架手术,only1$本次就诊是否已接受心脏支架手术,
                        onetwono3$本次就诊是否已接受心脏支架手术,threefull$本次就诊是否已接受心脏支架手术)
                  ,groups=c(rep(0,909),rep(1,242),rep(2,146),rep(3,224)))
ggbarstats(data = PCI,x=pci,y=groups,title = "pci history in four groups",legend.title = "pci history",palette = 'Set3')
#CAG
group7<- matrix(c(605,183,112,184,304,59,34,40),nrow = 4,ncol = 2)
chisq.test(group7)
pairwiseNominalIndependence(group7, chisq = TRUE,gtest  = FALSE,cramer = FALSE,fisher = FALSE)
CAG <- data.frame(cag=c(zeropcsk9i$本次就诊是否已接受冠脉造影检查,only1$本次就诊是否已接受冠脉造影检查,
                        onetwono3$本次就诊是否已接受冠脉造影检查,threefull$本次就诊是否已接受冠脉造影检查)
                  ,groups=c(rep(0,909),rep(1,242),rep(2,146),rep(3,224)))
ggbarstats(data = CAG,x=cag,y=groups,title = "CAG history in four groups",legend.title = "CAG history")
#oral drug history
group8<- matrix(c(441,165,113,150,468,77,33,74),nrow = 4,ncol = 2)
chisq.test(group8)
pairwiseNominalIndependence(group8, chisq = TRUE,gtest  = FALSE,cramer = FALSE,fisher = FALSE)
oraldrughistory <- data.frame(oral=c(zeropcsk9i$既往是否服用过口服降脂药物,only1$既往是否服用过口服降脂药物,
                        onetwono3$既往是否服用过口服降脂药物,threefull$既往是否服用过口服降脂药物)
                  ,groups=c(rep(0,909),rep(1,242),rep(2,146),rep(3,224)))
ggbarstats(data = oraldrughistory,x=oral,y=groups,title = "Oral drug history in four groups",legend.title = "Oral drug history")
#pcsk9i history
group9 <- matrix(c(67,54,85,175,92,139),nrow = 3,ncol = 2)
chisq.test(group9)
pairwiseNominalIndependence(group9, chisq = TRUE,gtest  = FALSE,cramer = FALSE,fisher = FALSE)
pcsk9 <- data.frame(hisofpcsk9=c(only1$既往是否使用过注射降脂药物,onetwono3$既往是否使用过注射降脂药物
                           ,threefull$既往是否使用过注射降脂药物),groups=c(rep(1,242),rep(2,146),rep(3,224)))
ggbarstats(data = pcsk9,x=hisofpcsk9,y=groups,title = "pcsk9i history in three groups",legend.title = "pcsk9i history",palette = 'Set1')

rm(group6,group7,group8,group9,group10)
#***********************************************************************************************#

#KW test for continuous variable
#age
compar_age <- data.frame(number=c(zeropcsk9i$age,only1$age,onetwono3$age,threefull$age),
                         group=c(rep(0,909),rep(1,242),rep(2,146),rep(3,224)))
boxplot(compar_age$number~compar_age$group,xlab = "groups",ylab = "Age")
leveneTest(compar_age$number,compar_age$group,center=median)
summary(aov(number~group,compar_age))
pairwise.t.test(compar_age$number,compar_age$group,p.adjust.method = "bonf",pool.sd = FALSE)

#first LDL-C
compar_ldl1st <- data.frame(number=c(zeropcsk9i$ldl_c_1st,only1$ldl_c_1st
                                     ,onetwono3$ldl_c_1st,threefull$ldl_c_1st),
                            group=c(rep(0,909),rep(1,242),rep(2,146),rep(3,224)))
boxplot(compar_ldl1st$number~compar_ldl1st$group,xlab = "groups",ylab = "LDL-C baseline")
leveneTest(compar_ldl1st$number,compar_ldl1st$group,center=median)
kruskal.test(number~group,compar_ldl1st)
ggplot(compar_ldl1st, aes(sample = number, colour = factor(group))) +
  stat_qq_line()+stat_qq()+ylim(0,9)
#last LDL-C
compar_ldllatest <- data.frame(number=c(zeropcsk9i$ldl_c_latest,only1$ldl_c_latest
                                     ,onetwono3$ldl_c_latest,threefull$ldl_c_latest),
                            group=c(rep(0,909),rep(1,242),rep(2,146),rep(3,224)))
leveneTest(compar_ldllatest$number,compar_ldllatest$group,center=median)
kruskal.test(number~group,compar_ldllatest)
pairwise.wilcox.test(compar_ldllatest$number,compar_ldllatest$group,p.adjust.method = "bonferroni")
ggplot(compar_ldllatest, aes(sample = number, colour = factor(group))) +
  stat_qq_line()+stat_qq()




## Control rate of LDL-C using 1.4mmol/L as target value

sum(zeropcsk9i$target4,na.rm = TRUE)
sum(only1$target4,na.rm = TRUE)
sum(onetwono3$target4,na.rm = TRUE)
sum(threefull$target4,na.rm = TRUE)
length(na.omit(zeropcsk9i$target4))
length(na.omit(only1$target4))
length(na.omit(onetwono3$target4))
length(na.omit(threefull$target4))

group10 <- matrix(c(15,24,5,19,59,71,14,66-19),nrow = 4,ncol = 2)
fisher.test(group10)
ultra_high <- data.frame(target=c(rep(1,15),rep(0,59),rep(1,24),rep(0,71),rep(1,5),
                                  rep(0,14),rep(1,19),rep(0,47)),groups=c(rep(0,74),
                                                                          rep(1,95),rep(2,19),rep(3,66)))
ggbarstats(data = ultra_high,x=target,y=groups,title = "Control rate of LDLC-C  in four groups (<1.4mmol/L)",legend.title = "status of target")

## Control rate of LDL-C using 1.8mmol/L as target value

sum(zeropcsk9i$target8,na.rm = TRUE)
sum(only1$target8,na.rm = TRUE)
sum(onetwono3$target8,na.rm = TRUE)
sum(threefull$target8,na.rm = TRUE)
length(na.omit(zeropcsk9i$target8))
length(na.omit(only1$target8))
length(na.omit(onetwono3$target8))
length(na.omit(threefull$target8))

group11 <- matrix(c(17,30,9,26,74-17,65,10,40),nrow = 4,ncol = 2)
chisq.test(group11)
high_risk <- data.frame(target=c(rep(1,17),rep(0,74-17),rep(1,30),rep(0,65),rep(1,9),
                                 rep(0,10),rep(1,26),rep(0,40)),groups=c(rep(0,74),
                                                                         rep(1,95),rep(2,19),rep(3,66)))
ggbarstats(data = high_risk,x=target,y=groups,title = "Control rate of LDLC-C  in four groups (<1.8mmol/L)",legend.title = "status")


##Logistic regression

a <- zeropcsk9i[,c('age','本次就诊是否已接受冠脉造影检查','本次就诊是否已接受心脏支架手术',
                   '既往是否服用过口服降脂药物')]
a$group <- 0
b <- only1[,c('age','本次就诊是否已接受冠脉造影检查','本次就诊是否已接受心脏支架手术',
              '既往是否服用过口服降脂药物')]
b$group <- 1
c <- onetwono3[,c('age','本次就诊是否已接受冠脉造影检查','本次就诊是否已接受心脏支架手术',
                  '既往是否服用过口服降脂药物')]
c$group <- 2
d <- threefull[,c('age','本次就诊是否已接受冠脉造影检查','本次就诊是否已接受心脏支架手术',
                  '既往是否服用过口服降脂药物')]
d$group <- 3

abcd<- rbind(a,b,c,d)
fit <- multinom(group~abcd$age+as.factor(abcd$本次就诊是否已接受冠脉造影检查)
                +as.factor(abcd$本次就诊是否已接受心脏支架手术)
                +as.factor(abcd$既往是否服用过口服降脂药物),data=abcd)
fit.result <- summary(fit)
Anova(fit)
OR <- round(odds.ratio(fit),2)
rowname <- matrix(c('1-age','1-cag','1-pci','1-oral','2-age','2-cag','2-pci',
                    '2-oral','3-age','3-cag','3-pci','3-oral'),nrow = 12,ncol = 1)
map <- forestplot(labeltext=cbind(rowname,OR$p[c(-1,-6,-12)]),mean=OR$OR[c(-1,-6,-12)],
                  lower=OR$`2.5 %`[c(-1,-6,-12)],upper=OR$`97.5 %`[c(-1,-6,-12)],
                  zero=1,boxsize=0.2,lineheight = unit(7,'mm'),colgap=unit(2,'mm'),
                  lwd.zero=1.5,lwd.ci=2,lty.ci = "solid",title = "Forestplot",
                  txt_gp = fpTxtGp(ticks = gpar(cex = 0.85),xlab  = gpar(cex = 0.8),
                                   cex = 0.9),line.margin = 0.08,xlab="OR",lwd.xaxis =1,graph.pos=2)  

  
