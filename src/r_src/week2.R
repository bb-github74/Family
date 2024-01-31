setwd("/Users/bat/Documents/Family/hap835/src1")

library(tidyverse)

df2 = readRDS('../data1/afterw1_df1.rds')

created_cols = c("Age1839","Age4064","hasIns","Child",
                 "HHSize","Incom","Sex","Marital","hasEduc","Empl",
                 "Seatbelt","isSmoker","persDoc","doesPA","isMSA",
                 "didFluShot", "mRace")

########################## a #######################################
stats_table = df2 %>% # group_by(Incom) %>% count()
  select(all_of(created_cols)) %>% 
  #names()
  group_by(hasIns) %>% 
  summarize(
    
    frequency = n(),
    
    # age between 18 and 39
    mean_age1839 = mean(Age1839, na.rm = TRUE),
    sd_age1839 = sd(Age1839, na.rm = TRUE),
    
    # age between 40 and 64
    mean_age4064 = mean(Age4064, na.rm = TRUE),
    sd_age4064 = sd(Age4064, na.rm = TRUE),
    
    # gender: female or not female
    mean_sex = mean(Sex, na.rm = TRUE),
    sd_sex = sd(Sex, na.rm = TRUE),
    
    # income
    mean_Inc = mean(Incom, na.rm = TRUE),
    sd_Incom = sd(Incom, na.rm = TRUE),
    
    # marital status: married or not married
    mean_mRace = mean(mRace, na.rm = TRUE),
    sd_mRace = sd(mRace, na.rm = TRUE),
    
    # has college degree
    mean_hasEdu = mean(hasEduc, na.rm = TRUE),
    sd_hasEdu = sd(hasEduc, na.rm = TRUE),
    
    # whether has employment
    mean_Empl = mean(Empl, na.rm = TRUE),
    sd_Empl = sd(Empl, na.rm = TRUE),
    
    # whether smoker
    mean_isSmoker = mean(isSmoker, na.rm = TRUE),
    sd_isSmoker = sd(isSmoker, na.rm = TRUE))

stats_table %>% t() %>% write.csv('w2_stats.csv')
############################# b #######################################
# function to calculate pooled standard deviation: 
# equation is standardized difference = (m1 - m2) / pooled sd

pooled_sd <- function(nA, nB, sd1, sd2){
  # Group A
  degFreedom1 = nA - 1
  var1 = sd1^2
  g1 = degFreedom1*var1
  
  # Group B
  degFreedom2 = nB - 1
  var2 = sd2^2
  g2 = degFreedom2*var2
  
  # pooled sd
  psd = sqrt( (g1+g2) / (nA+nB-2) )
  return(psd)
}

sd_diff <- function(m1,m2,psd){
  val = (m1-m2)/psd
  return(val)
}
############################## b ####################################
n0 = stats_table$frequency[1] # 45049
n1 = stats_table$frequency[2] # 245631

psd_age1839 = pooled_sd(n0,n1,
                        stats_table$sd_age1839[1],
                        stats_table$sd_age1839[2]) #4322
sd_age1839 = sd_diff(stats_table$mean_age1839[1],
                     stats_table$mean_age1839[2],
                     psd_age1839)
sd_age1839 # 256
#++++++++++++++++++++++++++++++++++++++++++++++++age4064++++++++++++++++++++++++
psd_age4064 = pooled_sd(n0,n1, 
                        stats_table$sd_age4064[1],
                        stats_table$sd_age4064[2]) # .4322

sd_age4064 = sd_diff(stats_table$mean_age4064[1],
                     stats_table$mean_age4064[2],
                     psd_age4064)
sd_age4064   #-.255

#++++++++++++++++++++++++++++++++++++++++++++++++sex++++++++++++++++++++++++
psd_sex = pooled_sd(n0,n1,
                    stats_table$sd_sex[1],
                    stats_table$sd_sex[2]) # .4878

sd_sex = sd_diff(stats_table$mean_sex[1],stats_table$mean_sex[2],psd_sex) #-.0396
#++++++++++++++++++++++++++++++++++++++++++++++++sex++++++++++++++++++++++++

psd_inc = pooled_sd(n0,n1,
                    stats_table$sd_Incom[1],
                    stats_table$sd_Incom[2]) #2.01

sd_inc = sd_diff(stats_table$mean_Inc[1],stats_table$mean_Inc[2],psd_inc) #-1.887
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
psd_race = pooled_sd(n0,n1,
                    stats_table$sd_mRace[1],
                    stats_table$sd_mRace[2]) #.3529

sd_race = sd_diff(stats_table$mean_mRace[1],
                  stats_table$mean_mRace[2],
                  psd_race) 
#-.1659
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
psd_edu = pooled_sd(n0,n1,
                     stats_table$sd_hasEdu[1],
                     stats_table$sd_hasEdu[2]) #.4752

sd_edu = sd_diff(stats_table$mean_hasEdu[1],
                 stats_table$mean_hasEdu[2],
                 psd_edu) #-.5359
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
psd_emp = pooled_sd(n0,n1,
                     stats_table$sd_Empl[1],
                     stats_table$sd_Empl[2]) #.4196

sd_emp = sd_diff(stats_table$mean_Empl[1],
                 stats_table$mean_Empl[2],
                 psd_emp) # 0.014
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
psd_smo = pooled_sd(n0,n1,
                     stats_table$sd_isSmoker[1],
                     stats_table$sd_isSmoker[2]) #.39265

sd_smo = sd_diff(stats_table$mean_isSmoker[1],
                 stats_table$mean_isSmoker[2],
                 psd_smo) # .3721

# Creating standartized difference table
sd_table = data.frame(
          feature = c("age1839", "age4064", "Sex","hasInc","Race","hasEdu","hasEmpl","isSmoker"),
          value = c(sd_age1839,sd_age4064,sd_sex,sd_inc,sd_race,sd_edu,sd_emp,sd_smo))

sd_table = sd_table %>% 
  mutate('sdGreaterPoint25' = ifelse(value > .25, 1,0))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(glmnet)
# drop NA
model1 = df2 %>% 
  select(all_of(created_cols)) %>%
  select(!persDoc) %>% 
  filter(complete.cases(.))

# to see label ratio
model1 %>% 
  group_by(hasIns) %>% count() 
  # 0 = 22826 1 = 83192

# run logistic regression
log_model = glm(hasIns ~ ., family = 'binomial', data = model1)

# print summary
log_model %>% summary() # Age4064 has colinearity

model1 %>% 
  group_by(Age1839, Age4064) %>% count() # might consider drop either one

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Predicting probabilities
model1$insur_fitted = predict(log_model, newdata = model1, type = "response")

# to review the created variable
model1 %>% 
  select(insur_fitted, hasIns)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ftd_table = model1 %>% 
  select(hasIns, insur_fitted) %>% 
  group_by(hasIns) %>% 
  summarise(
    mu = mean(insur_fitted),
    sigma = sd(insur_fitted),
    minim = min(insur_fitted),
    maxim = max(insur_fitted)
  )

sd_table  
ftd_table

n00 = 22915
n11 = 83325

ftd_e = pooled_sd(n00,n11,
                    ftd_table$sigma[1],
                    ftd_table$sigma[2]) #.1441619

ftd_diff = sd_diff(ftd_table$mu[1],
                 ftd_table$mu[2],
                 ftd_e) #-.9444059 

# e) standardized difference is not above .25
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
model1 %>% 
  select(hasIns, insur_fitted) %>%
  ggplot(aes(x=insur_fitted, fill=as.factor(hasIns)))+
  geom_histogram(bins = 50, position = 'identity', alpha=0.5)+
  scale_fill_manual(values=c('red','green'), name='Insurance')+
  labs(title = "Histogram by Group",
       x = "Value", 
       y = "Density")


model1 %>% 
  select(hasIns, insur_fitted) %>%
  ggplot(aes(x=insur_fitted, fill=as.factor(hasIns)))+
  geom_histogram(aes(y = stat(density)), bins = 50, alpha=.5, position = 'identity')+
  geom_density(alpha=.2)+
  scale_fill_manual(values = c('blue','red'), name = 'Insurance')+
  labs(title = "Histogram and KDE by Group",
       x = "Value", 
       y = "Density")


