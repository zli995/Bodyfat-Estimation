library(knitr)
library(kableExtra)
library(tidyverse)
library(reshape)
library(corrplot)
library(leaps)
##All the code is discussed and determined by the whole group
#Following is written by Augustine Tang
dat = read.csv('BodyFat.csv')[,-1]


#data preparing------------------------
#Draw boxplot for each variable
isoutlier = function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

k = nrow(dat)
dat_1 = dat %>%
  scale(.) %>%
  as.data.frame(.) %>%
  melt(.) %>%
  group_by(variable) %>%
  mutate(outlier = ifelse(isoutlier(value), value, as.numeric(NA)))


for(i in 1:nrow(dat_1)){ 
  if(!is.na(dat_1$outlier[i])) 
    dat_1$outlier[i] = i %% k
}

p = ggplot(data = dat_1,aes(x = variable, y = value))+ 
  geom_boxplot(aes(fill = variable))+
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.8)+
  scale_y_continuous(breaks=NULL) +   
  theme(axis.text.y = element_blank()) +  
  theme(axis.ticks.y = element_blank()) +   
  theme(panel.border = element_blank()) +
  labs(x = "Variable", y = "")
windows(12,6)
p

outlier.index = as.factor(dat_1$outlier[!is.na(dat_1$outlier)])
p = ggplot(data = data.frame(outlier.index))+
  geom_histogram(aes(outlier.index), fill="#619CFF", stat="count")+
  labs(x = "Outlier Index", y = "Count")+
  theme(panel.grid.major=element_blank())
windows(12,6)
p


#We can see some are potential outliers. 
data = dat[-c(31, 39, 41, 42, 86, 216),]

#Check consistency
#Find difference between given bodyfat and calculated bodyfat
df = data.frame(Real = data$BODYFAT, Fit = 495 / data$DENSITY - 450)
is_outlier2 = function(x) {
  return(x < mean(x) - 2 * sd(x) | x > mean(x) + 2 * sd(x))
}
df_1 = df %>%
  mutate(dif = (Fit - Real)) %>%
  mutate(outlier = ifelse(is_outlier2(dif), 1,  as.numeric(NA)))

for(i in 1:nrow(df_1)){ 
  if(!is.na(df_1$outlier[i])) 
    df_1$outlier[i] = i %% nrow(df_1)
}
df$outlier = df_1$outlier
p = ggplot(data = df, aes(x = Real, y = Fit))+
  geom_point(color = '#00BFC4')+
  geom_point(data = df[!is.na(df$outlier),], color = '#F8766D')+
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.2, color = '#F8766D')+
  labs(x = 'given bodyfat', y = 'calculated bodyfat')
windows(5,5)
p
#Delete some observation with conflict between given bodyfat and calculated bodyfat
data = data[-c(44, 72, 91, 177),]

#Find difference between given BMI and calculated BMI
df = data.frame(Real = 703 * data$WEIGHT / data$HEIGHT ^ 2, Fit = data$ADIPOSITY)

df_1 = df %>%
  mutate(dif = (Fit - Real)) %>%
  mutate(outlier = ifelse(is_outlier2(dif), 1,  as.numeric(NA)))

for(i in 1:nrow(df_1)){ 
  if(!is.na(df_1$outlier[i])) 
    df_1$outlier[i] = i %% nrow(df_1)
}
df$outlier = df_1$outlier
p = ggplot(data = df, aes(x = Real, y = Fit))+
  geom_point(color = '#00BFC4')+
  geom_point(data = df[!is.na(df$outlier),], color = '#F8766D')+
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.2, color = '#F8766D')+
  labs(x = 'given BMI', y = 'calculated BMI')
windows(5,5)
p
#Delete some observation with conflict between given bodyfat and calculated bodyfat
data = data[-c(155, 211),]


#Following is written by Zixiang Xu
#Variable selection----------------------------------
#Ineraction detection
data = data[,-2]
glm0 <- glm(BODYFAT~1, data=data)
glm.twoway <- glm(BODYFAT~ .*., data=data) 
step(glm0, scope = list(upper=glm.twoway), 
     direction = "both", k=log(87))
#seems no interaction
#model selection by all possible subsets
library(leaps)
myleaps = regsubsets(BODYFAT~.,data=data, nbest = 1, nvmax = 15)
myleaps.summary = summary(myleaps)
bettertable = cbind(myleaps.summary$which, myleaps.summary$rsq, myleaps.summary$rss, 
                    myleaps.summary$adjr2, myleaps.summary$cp, myleaps.summary$bic)
dimnames(bettertable)[[2]] = c(dimnames(myleaps.summary$which)[[2]], 
                               "rsq", "rss", "adjr2", "cp", "bic") 
show(bettertable)
windows(10,4)
par(mfrow=c(1,3), pty="s") 
plot(myleaps, scale = "adjr2")
plot(myleaps, scale = "Cp")
plot(myleaps, scale = "bic")

#possible model 1 by Cp
model1 = lm(BODYFAT~WEIGHT +ABDOMEN + THIGH +FOREARM +WRIST, data = data)
summary(model1)

#Following is written by Zeyu Li 
#remove insignificant term 
model1 = lm(BODYFAT~WEIGHT +ABDOMEN +FOREARM +WRIST, data = data)
summary(model1)

#model1 validation 
windows(10,10)
par(mfrow = c(2,2))
plot(model1, which  = 1)
plot(model1, which  = 2)
plot(model1, which  = 3)
plot(model1, which  = 4)

#possible model 2 by BIC
model2 = lm(BODYFAT~WEIGHT +ABDOMEN +WRIST, data = data)
#model1 validation 
windows(10,10)
par(mfrow = c(2,2))
plot(model2, which  = 1)
plot(model2, which  = 2)
plot(model2, which  = 3)
plot(model2, which  = 4)

#comparison
anova(model2, model1)

#reject H0, use model 1
summary(model1)

