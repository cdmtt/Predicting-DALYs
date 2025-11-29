#Supplement to paper: 
#Comparison of approaches for predicting disability-adjusted life years to inform policy evaluation: A case study on the burden of stomach cancer attributable to sodium intake in Japan
#Author: Constanza De Matteu (cdmmo@food.dtu.dk)


#library
library(dplyr)
library(tseries)
library(forecast)
library(ggplot2)
library(cowplot)




# CRA ---------------------------------------------------------------------

#IHME risk curve for stomach cancer and diets high in sodium is extracted
#source: https://vizhub.healthdata.org/burden-of-proof/

ihme1 <- read.csv("C:/Users/cdmmo/Downloads/download (1).csv")

#piecewise function 
#source: https://doi.org/10.1101/2025.08.27.25333764 (Jacob et al.,[preprint])

piecewise_function <- function(x_coords, y_coords) {
  function(z) {
    idx <- findInterval(z, x_coords, rightmost.closed = TRUE)
    ifelse(idx == 0, y_coords[1], 
           ifelse(idx > length(y_coords), y_coords[length(y_coords)], y_coords[idx])) #last point is continuous
  }
}


# Create the function
f <- piecewise_function(ihme1$Risk, ihme1$Log.relative.risk.of.outcome) #actual function of the line

f_lower <- piecewise_function(ihme1$Risk, ihme1$Log.relative.risk.of.outcome..lower.fixed...random.effects.uncertainty.)
f_upper <- piecewise_function(ihme1$Risk, ihme1$Log.relative.risk.of.outcome..upper.fixed...random.effects.uncertainty.)



plot(NULL, xlim = c(min(ihme1$Risk) - 1, max(ihme1$Risk) + 1), ylim = range(ihme1$Log.relative.risk.of.outcome) + c(-1, 1), 
     xlab = "x", ylab = "f(x)", main = "Piecewise Constant Function", type = "n")

# Draw the constant segments (on the plot)
for (i in 1:(length(ihme1$Risk) - 1)) {
  segments(ihme1$Risk[i], ihme1$Log.relative.risk.of.outcome[i], ihme1$Risk[i + 1], ihme1$Log.relative.risk.of.outcome[i], col = "blue", lwd = 2)  # Horizontal line
  points(ihme1$Risk[i], ihme1$Log.relative.risk.of.outcome[i], pch = 16, col = "blue")                   # Solid point at start
  points(ihme1$Risk[i + 1], ihme1$Log.relative.risk.of.outcome[i], pch = 1, col = "blue")                # Open circle at end
  segments(ihme1$Risk[length(ihme1$Risk)], ihme1$Log.relative.risk.of.outcome[length(ihme1$Log.relative.risk.of.outcome)], max(ihme1$Risk) + 2, ihme1$Log.relative.risk.of.outcome[length(ihme1$Log.relative.risk.of.outcome)], col = "blue", lwd = 2)
  points(ihme1$Risk[length(ihme1$Risk)], ihme1$Log.relative.risk.of.outcome[length(ihme1$Log.relative.risk.of.outcome)], pch = 16, col = "blue")
}


## draw lower

plot(NULL, xlim = c(min(ihme1$Risk) - 1, max(ihme1$Risk) + 1), ylim = range(ihme1$Log.relative.risk.of.outcome..lower.fixed...random.effects.uncertainty.) + c(-1, 1), 
     xlab = "x", ylab = "f(x)", main = "Piecewise Constant Function - lower", type = "n")

# Draw the constant segments (on the plot)
for (i in 1:(length(ihme1$Risk) - 1)) {
  segments(ihme1$Risk[i], ihme1$Log.relative.risk.of.outcome..lower.fixed...random.effects.uncertainty.[i], ihme1$Risk[i + 1], ihme1$Log.relative.risk.of.outcome..lower.fixed...random.effects.uncertainty.[i], col = "red", lwd = 2)  # Horizontal line
  points(ihme1$Risk[i], ihme1$Log.relative.risk.of.outcome..lower.fixed...random.effects.uncertainty.[i], pch = 16, col = "red")                   # Solid point at start
  points(ihme1$Risk[i + 1], ihme1$Log.relative.risk.of.outcome..lower.fixed...random.effects.uncertainty.[i], pch = 1, col = "red")                # Open circle at end
  segments(ihme1$Risk[length(ihme1$Risk)], ihme1$Log.relative.risk.of.outcome..lower.fixed...random.effects.uncertainty.[length(ihme1$Log.relative.risk.of.outcome..lower.fixed...random.effects.uncertainty.)], max(ihme1$Risk) + 2, ihme1$Log.relative.risk.of.outcome..lower.fixed...random.effects.uncertainty.[length(ihme1$Log.relative.risk.of.outcome..lower.fixed...random.effects.uncertainty.)], col = "red", lwd = 2)
  points(ihme1$Risk[length(ihme1$Risk)], ihme1$Log.relative.risk.of.outcome..lower.fixed...random.effects.uncertainty.[length(ihme1$Log.relative.risk.of.outcome..lower.fixed...random.effects.uncertainty.)], pch = 16, col = "red")
}

##draw upper 
plot(NULL, xlim = c(min(ihme1$Risk) - 1, max(ihme1$Risk) + 1), ylim = range(ihme1$Log.relative.risk.of.outcome..upper.fixed...random.effects.uncertainty. ) + c(-1, 1), 
     xlab = "x", ylab = "f(x)", main = "Piecewise Constant Function - upper", type = "n")

# Draw the constant segments (on the plot)
for (i in 1:(length(ihme1$Risk) - 1)) {
  segments(ihme1$Risk[i], ihme1$Log.relative.risk.of.outcome..upper.fixed...random.effects.uncertainty.[i], ihme1$Risk[i + 1], ihme1$Log.relative.risk.of.outcome..upper.fixed...random.effects.uncertainty.[i], col = "green", lwd = 2)  # Horizontal line
  points(ihme1$Risk[i], ihme1$Log.relative.risk.of.outcome..upper.fixed...random.effects.uncertainty.[i], pch = 16, col = "green")                   # Solid point at start
  points(ihme1$Risk[i + 1], ihme1$Log.relative.risk.of.outcome..upper.fixed...random.effects.uncertainty.[i], pch = 1, col = "green")                # Open circle at end
  segments(ihme1$Risk[length(ihme1$Risk)], ihme1$Log.relative.risk.of.outcome..upper.fixed...random.effects.uncertainty.[length(ihme1$Log.relative.risk.of.outcome..upper.fixed...random.effects.uncertainty.)], max(ihme1$Risk) + 2, ihme1$Log.relative.risk.of.outcome..upper.fixed...random.effects.uncertainty.[length(ihme1$Log.relative.risk.of.outcome..upper.fixed...random.effects.uncertainty.)], col = "green", lwd = 2)
  points(ihme1$Risk[length(ihme1$Risk)], ihme1$Log.relative.risk.of.outcome..upper.fixed...random.effects.uncertainty.[length(ihme1$Log.relative.risk.of.outcome..upper.fixed...random.effects.uncertainty.)], pch = 16, col = "green")
}

#####
#### converting salt intakes to sodium intakes 
#intakes from the report on the National Health Survey, Japan 2022
#conversion factor from WHO

sodium_all <- 9.7/2.542
sodium_males <- 10.5/2.542
sodium_females <- 9/2.542

sodium_70_m <- 10.9/2.542
sodium_70_f <- 9.4/2.542

salt15to49_m <- (9.9+10.7+10.1)/3
sodium_15to49_m <- salt15to49_m/2.542

salt15to49_f <- (7.9+8.8+8.6)/3
sodium_15to49_f <- salt15to49_f/2.542

salt50to69_m <- (10.5+10.6)/2
sodium_50to69_m <- salt50to69_m/2.542

salt50to69_f <- (8.6+9.4)/2
sodium_50to69_f <- salt50to69_f/2.542

###

rr_total <- exp(f(sodium_all))  #convert logRR
rr_m <- exp(f(sodium_males))
rr_f <- exp(f(sodium_females))

rr_s1_total <- exp(f(sodium_all/2))
rr_s1_m <- exp(f(sodium_males/2))
rr_s1_f <- exp(f(sodium_females/2))

###lower and upper bound 

rr_total_lower <- exp(f_lower(sodium_all))  
rr_total_upper <- exp(f_upper(sodium_all))

rr_s1_total_lower <- exp(f_lower(sodium_all/2)) 
rr_s1_total_upper <- exp(f_upper(sodium_all/2))



#####
rr_m <- exp(f(sodium_males))
rr_f <- exp(f(sodium_females))

###

#15-49
#rr_1549 <- exp(f(sodium_ ))
rr_1549_m <- exp(f(sodium_15to49_m))
rr_1549_f <- exp(f(sodium_15to49_f))

#rr_s1_1549 <-
rr_s1_1549_m <- exp(f(sodium_15to49_m/2))
rr_s1_1549_f <- exp(f(sodium_15to49_f/2))

#50-69
#rr_5069 <- exp(f(sodium_ ))
rr_5069_m <- exp(f(sodium_50to69_m))
rr_5069_f <- exp(f(sodium_50to69_f))

#rr_s1_5069 <-
rr_s1_5069_m <- exp(f(sodium_50to69_m/2))
rr_s1_5069_f <- exp(f(sodium_50to69_f/2))

#70
#rr_70 <- exp(f(sodium_ ))
rr_70_m <- exp(f(sodium_70_m))
rr_70_f <- exp(f(sodium_70_f))

#rr_s1_1549 <-
rr_s1_70_m <- exp(f(sodium_70_m/2))
rr_s1_70_f <- exp(f(sodium_70_f/2))

######
####PIF
#alt-ref/ref

pif_total <- (rr_s1_total - rr_total)/rr_total
#[1] -0.1591025
pif_males <- (rr_s1_m - rr_m)/rr_m
#[1] -0.1221714
pif_females <- (rr_s1_f - rr_f)/rr_f
#-0.2255832

####
pif_lower_total <-
  pif_upper_total <- 
  
  ###
  
  pif_1549m <- (rr_s1_1549_m - rr_1549_m)/rr_1549_m
#-0.1399448
pif_1549f <- (rr_s1_1549_f - rr_1549_f)/rr_1549_f
#-0.2501336

pif_5069m <- (rr_s1_5069_m - rr_5069_m)/rr_5069_m
#-0.1221714
pif_5069f <- (rr_s1_5069_f - rr_5069_f)/rr_5069_f
#-0.2255832

pif_70m <- (rr_s1_70_m - rr_70_m)/rr_70_m 
#-0.1058779
pif_70f <- (rr_s1_70_f - rr_70_f)/rr_70_f 
#-0.179745


##disease envelope GBD 2021
#source: https://vizhub.healthdata.org/gbd-results/
gbd21 <- read.csv("C:/Users/cdmmo/OneDrive - Danmarks Tekniske Universitet/uTokyo/IHME-GBD_2021_stomach-cancer.csv")
summary.factor(gbd21$age_name)

##data subsets
sc <- gbd21 %>% filter(gbd21$age_name == "Age-standardized") 
sc_male <- sc %>% filter(sc$sex_name == "Male")
sc_female <- sc %>% filter(sc$sex_name == "Female")

sc_1549 <- gbd21 %>% filter(gbd21$age_name == "15-49 years") 
sc_1549male <- sc_1549 %>% filter(sc_1549$sex_name == "Male")
sc_1549female <- sc_1549 %>% filter(sc_1549$sex_name == "Female")

sc_5069 <- gbd21 %>% filter(gbd21$age_name == "50-69 years") 
sc_5069male <- sc_5069 %>% filter(sc_5069$sex_name == "Male")
sc_5069female <- sc_5069 %>% filter(sc_5069$sex_name == "Female")

sc_70 <- gbd21 %>% filter(gbd21$age_name == "70+ years") 
sc_5069male <- sc_5069 %>% filter(sc_5069$sex_name == "Male")
sc_5069female <- sc_5069 %>% filter(sc_5069$sex_name == "Female")

###########
###incidence 

inc_rate_total <- sc[9,14]
total_inc <- pif_total * inc_rate_total
#-4.064029

inc_rate_male <- sc_male[3,14]
#inc_rate_ub_male <- sc_male[3,15]
#inc_rate_lb_male <- sc_male[3,16]

inc_rate_female <- sc_female[3,14]
#inc_rate_ub_female <- sc_female[3,15]
#inc_rate_lb_female <- sc_female[3,16]

male_inc <- pif_males * inc_rate_male
#[1]  -4.737002

#male_inc_ub <- pif_males * inc_rate_ub_male
#male_inc_lb <- pif_males * inc_rate_lb_male


female_inc <- pif_females * inc_rate_female
# -3.306262

#female_inc_ub <- pif_females * inc_rate_ub_female
#female_inc_lb <- pif_females * inc_rate_lb_female

####15-49
inc_rate_1549m <- sc_1549male[3,14]
m1549_inc <- pif_1549m * inc_rate_1549m
# -0.8990839

inc_rate_1549f <- sc_1549female[3,14]
f1549_inc <- pif_1549f * inc_rate_1549f

#50-69
inc_rate_5069m <- sc_5069male[3,14]
m5069_inc <- pif_5069m * inc_rate_5069m
# -10.99378

inc_rate_5069f <- sc_5069female[3,14]
f5069_inc <- pif_5069f * inc_rate_5069f
#-7.036204

#70
inc_rate_70m <- sc_70[7,14]
m70_inc <- pif_70m * inc_rate_70m
# -42.43012

inc_rate_70f <- sc_70[8,14]
f70_inc <- pif_70f * inc_rate_70f
#-29.34204


##############
#daly calculation

yld <- sc[3,14]
yll <- sc[6,14]

yld_total <- yld * pif_total
#-1.278951
yll_total <- yll * pif_total
#-41.70616

daly_total <- yld_total + yll_total
#-42.98511

#male
yld_m  <- sc_male[1,14]
#yld_ub_m <- sc_male[1,15]
#yld_lb_m <- sc_male[1,16]

yll_m <- sc_male[2,14]
#yll_ub_m <- sc_male[2,15]
#yll_lb_m <- sc_male[2,16]

yld_males <- yld_m * pif_males
#-1.482658

yll_males <- yll_m * pif_males
#-47.29386

daly_males <- yld_males + yll_males
#-48.77651

#female
yld_f  <- sc_female[1,14]
#yld_ub_f <- sc_female[1,15]
#yld_lb_f <- sc_female[1,16]

yll_f <- sc_female[2,14]
#yll_ub_f <- sc_female[2,15]
#yll_lb_f <- sc_female[2,16]

yld_females <- yld_f * pif_females
#-1.032925

yll_females <- yll_f * pif_females
# -35.13511

daly_females <- yld_females + yll_females
#-36.16804

##1549

yld_1549m <- sc_1549male[1,14]
yll_1549m <- sc_1549male[2,14]

yld_1549f <- sc_1549female[1,14]
yll_1549f <- sc_1549female[2,14]

#males
yld_males1549 <- yld_1549m * pif_1549m
#-0.363334
yll_males1549 <- yll_1549m * pif_1549m
#-13.66311
daly_males1549 <- yld_males1549 + yll_males1549
#-14.02645

#females
yld_females1549 <- yld_1549f * pif_1549f
#-0.5339222
yll_females1549 <- yll_1549f * pif_1549f
#-22.21629
daly_females1549 <- yld_females1549 + yll_females1549
#-22.75021

##50-69
yld_5069m <- sc_5069male[1,14]
yll_5069m <- sc_5069male[2,14]

yld_5069f <- sc_5069female[1,14]
yll_5069f <- sc_5069female[2,14]


yld_males5069 <- yld_5069m * pif_5069m
#-3.847667
yll_males5069 <- yll_5069m * pif_5069m
#-131.6128
daly_males5069 <- yld_males5069 + yll_males5069
#-135.4605

#females
yld_females5069 <- yld_5069f * pif_5069f
#-2.44999
yll_females5069 <- yll_5069f * pif_5069f
#-86.612
daly_females5069 <- yld_females5069 + yll_females5069
# -89.06199


##70
yld_70_m <- sc_70[1,14]
yll_70_m <- sc_70[4,14]

yld_70_f <- sc_70[2,14]
yll_70_f <- sc_70[5,14]


yld_males70 <- yld_70_m * pif_70m
#-11.80638
yll_males70 <- yll_70_m * pif_70m
#-345.8632

daly_males70 <- yld_males70 + yll_males70
#-357.6696


yld_females70 <- yld_70_f * pif_70f
#-7.423256
yll_females70 <- yll_70_f * pif_70f
#-227.9792

daly_females70 <- yld_females70 + yll_females70
#-235.4024



# ARIMA -------------------------------------------------------------------

#data extracted from GBD Results tool

#upload subset with "age standardised" variables 
#source: https://vizhub.healthdata.org/gbd-results/

sc <- read.csv("C:/Users/cdmmo/OneDrive - Danmarks Tekniske Universitet/uTokyo/data salt/sc_data_perage.csv")  

#subset based on sex
sc_b <- sc %>% filter(sex_name %in% "Both")
sc_m <- sc %>% filter(sex_name %in% "Male")
sc_f <- sc %>% filter(sex_name %in% "Female")

#subset based on age
sc_b1549 <- sc_b %>% filter(age_name %in% "15-49 years")
sc_b5069 <- sc_b %>% filter(age_name %in% "50-69 years")
sc_b70 <- sc_b %>% filter(age_name %in% "70+ years")

sc_m1549 <- sc_m %>% filter(age_name %in% "15-49 years")
sc_m5069 <- sc_m %>% filter(age_name %in% "50-69 years")
sc_m70 <- sc_m %>% filter(age_name %in% "70+ years")

sc_f1549 <- sc_f %>% filter(age_name %in% "15-49 years")
sc_f5069 <- sc_f %>% filter(age_name %in% "50-69 years")
sc_f70 <- sc_f %>% filter(age_name %in% "70+ years")


# data check --------------------------------------------------------------

#visuals

#1549
p1_1549 = ggplot(sc_b1549, aes(x = year, y = na)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  ylab("Summary exposure value, diet high in sodium")+ ylim(c(0,70))+
  xlab("Year")+xlim(c(1990,2021))+theme_minimal()+theme(legend.title=element_blank()) +
  ggtitle("Sex combined, age 15-49")
p1_1549

p1m_1549 = ggplot(sc_m1549, aes(x = year, y = na)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  ylab("Summary exposure value, diet high in sodium")+ ylim(c(0,70))+
  xlab("Year")+xlim(c(1990,2021))+theme_minimal()+theme(legend.title=element_blank()) +
  ggtitle("Males, age 15-49")
p1m_1549

p1f_1549 = ggplot(sc_f1549, aes(x = year, y = na)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  ylab("Summary exposure value, diet high in sodium")+ ylim(c(0,70))+
  xlab("Year")+xlim(c(1990,2021))+theme_minimal()+theme(legend.title=element_blank()) +
  ggtitle("Females, age 15-49")
p1f_1549

#50-69
p1_5069 = ggplot(sc_b5069, aes(x = year, y = na)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  ylab("Summary exposure value, diet high in sodium")+ ylim(c(0,100))+
  xlab("Year")+xlim(c(1990,2021))+theme_minimal()+theme(legend.title=element_blank()) +
  ggtitle("Sex combined, age 50-69")
p1_5069

p1m_5069 = ggplot(sc_m5069, aes(x = year, y = na)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  ylab("Summary exposure value, diet high in sodium")+ ylim(c(0,100))+
  xlab("Year")+xlim(c(1990,2021))+theme_minimal()+theme(legend.title=element_blank()) +
  ggtitle("Males, age 50-69")
p1m_5069

p1f_5069 = ggplot(sc_f5069, aes(x = year, y = na)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  ylab("Summary exposure value, diet high in sodium")+ ylim(c(0,100))+
  xlab("Year")+xlim(c(1990,2021))+theme_minimal()+theme(legend.title=element_blank()) +
  ggtitle("Females, age 50-69")
p1f_5069

#70
p1_70 = ggplot(sc_b70, aes(x = year, y = na)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  ylab("Summary exposure value, diet high in sodium")+ ylim(c(0,100))+
  xlab("Year")+xlim(c(1990,2021))+theme_minimal()+theme(legend.title=element_blank()) +
  ggtitle("Sex combined, age 70 and 70+")
p1_70

p1m_70 = ggplot(sc_m70, aes(x = year, y = na)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  ylab("Summary exposure value, diet high in sodium")+ ylim(c(0,100))+
  xlab("Year")+xlim(c(1990,2021))+theme_minimal()+theme(legend.title=element_blank()) +
  ggtitle("Males, age 70 and 70+")
p1m_70

p1f_70 = ggplot(sc_f70, aes(x = year, y = na)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  ylab("Summary exposure value, diet high in sodium")+ ylim(c(0,100))+
  xlab("Year")+xlim(c(1990,2021))+theme_minimal()+theme(legend.title=element_blank()) +
  ggtitle("Females, age 70 and 70+")
p1f_70

#####

#stationary check

adf.test(sc_b1549$at_daly, alternative = "stationary")
adf.test(log(sc_b1549$at_daly), alternative = "stationary")


dif_atdaly <- diff(sc_b1549$at_daly, differences=1)
adf.test(dif_atdaly)

dif_atdaly2 <- diff(sc_b1549$at_daly, differences=2)
adf.test(dif_atdaly2)
###stationary


adf.test(sc_b5069$at_daly, alternative = "stationary")
adf.test(log(sc_b5069 $at_daly), alternative = "stationary")


dif_atdaly <- diff(sc_b5069$at_daly, differences=1)
adf.test(dif_atdaly)

dif_atdaly2 <- diff(sc_b5069$at_daly, differences=2)
adf.test(dif_atdaly2)
###stationary


adf.test(sc_b70$at_daly, alternative = "stationary")
adf.test(log(sc_b70$at_daly), alternative = "stationary")


dif_atdaly <- diff(sc_b70$at_daly, differences=1)
adf.test(dif_atdaly)

dif_atdaly2 <- diff(sc_b70$at_daly, differences=2)
adf.test(dif_atdaly2)
###stationary


##correlation check

cor_atdaly_b1549 <- matrix(
  c(
    paste(round(cor(sc_b1549$na, sc_b1549$at_daly), 2)," (",round(cor.test(sc_b1549$na, sc_b1549$at_daly)$p.value,3),")",sep=""),
    paste(round(cor(sc_m1549$na, sc_m1549$at_daly), 2)," (",round(cor.test(sc_m1549$na, sc_m1549$at_daly)$p.value,3),")",sep=""),          
    paste(round(cor(sc_f1549$na, sc_f1549$at_daly), 2)," (",round(cor.test(sc_f1549$na, sc_f1549$at_daly)$p.value,3),")",sep="")
  ),ncol=3,byrow=T)

#    [,1]       [,2]       [,3]      
#[1,] "0.98 (0)" "0.99 (0)" "0.97 (0)"

cor_atdaly_b5069 <- matrix(
  c(
    paste(round(cor(sc_b5069$na, sc_b5069$at_daly), 2)," (",round(cor.test(sc_b5069$na, sc_b5069$at_daly)$p.value,3),")",sep=""),
    paste(round(cor(sc_m5069$na, sc_m5069$at_daly), 2)," (",round(cor.test(sc_m5069$na, sc_m5069$at_daly)$p.value,3),")",sep=""),          
    paste(round(cor(sc_f5069$na, sc_f5069$at_daly), 2)," (",round(cor.test(sc_f5069$na, sc_f5069$at_daly)$p.value,3),")",sep="")
  ),ncol=3,byrow=T)

#     [,1]       [,2]       [,3]      
#[1,] "0.96 (0)" "0.96 (0)" "0.96 (0)"

cor_atdaly_b70 <- matrix(
  c(
    paste(round(cor(sc_b70$na, sc_b70$at_daly), 2)," (",round(cor.test(sc_b70$na, sc_b70$at_daly)$p.value,3),")",sep=""),
    paste(round(cor(sc_m70$na, sc_m70$at_daly), 2)," (",round(cor.test(sc_m70$na, sc_m70$at_daly)$p.value,3),")",sep=""),          
    paste(round(cor(sc_f70$na, sc_f70$at_daly), 2)," (",round(cor.test(sc_f70$na, sc_f70$at_daly)$p.value,3),")",sep="")
  ),ncol=3,byrow=T)
#     [,1]       [,2]       [,3]      
#[1,] "0.96 (0)" "0.96 (0)" "0.96 (0)"





########
####Descriptive

###Table1
# only attributable daly 

table1_1549 <- matrix(nrow = 21, ncol = 5)
colnames(table1_1549) <- c("year", "sex", #"total daly (rate)",
                           "attributable daly (rate)", "SEV - Diet high in sodium (%)", #"SEV - smoking (%)",
                           "SDI (%)")
table1_1549[,1] <- rep(c("1990", "1995", "2000", "2005", "2010", "2015", "2021"), each = 3) 
table1_1549[,2] <- rep(c("Both","Male", "Female"), times = 7) 

c4 <- cbind(
  rbind(
    sc_b1549[sc_b1549$year == 1990, "at_daly"],
    sc_m1549[sc_m1549$year == 1990, "at_daly"],
    sc_f1549[sc_f1549$year == 1990, "at_daly"],
    
    sc_b1549[sc_b1549$year == 1995, "at_daly"],
    sc_m1549[sc_m1549$year == 1995, "at_daly"],
    sc_f1549[sc_f1549$year == 1995, "at_daly"],
    
    sc_b1549[sc_b1549$year == 2000, "at_daly"],
    sc_m1549[sc_m1549$year == 2000, "at_daly"],
    sc_f1549[sc_f1549$year == 2000, "at_daly"],
    
    sc_b1549[sc_b1549$year == 2005, "at_daly"],
    sc_m1549[sc_m1549$year == 2005, "at_daly"],
    sc_f1549[sc_f1549$year == 2005, "at_daly"],
    
    sc_b1549[sc_b1549$year == 2010, "at_daly"],
    sc_m1549[sc_m1549$year == 2010, "at_daly"],
    sc_f1549[sc_f1549$year == 2010, "at_daly"],
    
    sc_b1549[sc_b1549$year == 2015, "at_daly"],
    sc_m1549[sc_m1549$year == 2015, "at_daly"],
    sc_f1549[sc_f1549$year == 2015, "at_daly"],
    
    sc_b1549[sc_b1549$year == 2021, "at_daly"],
    sc_m1549[sc_m1549$year == 2021, "at_daly"],
    sc_f1549[sc_f1549$year == 2021, "at_daly"]
  )
)

c5 <- cbind(
  rbind(
    sc_b1549[sc_b1549$year == 1990, "na"],
    sc_m1549[sc_m1549$year == 1990, "na"],
    sc_f1549[sc_f1549$year == 1990, "na"],
    
    sc_b1549[sc_b1549$year == 1995, "na"],
    sc_m1549[sc_m1549$year == 1995, "na"],
    sc_f1549[sc_f1549$year == 1995, "na"],
    
    sc_b1549[sc_b1549$year == 2000, "na"],
    sc_m1549[sc_m1549$year == 2000, "na"],
    sc_f1549[sc_f1549$year == 2000, "na"],
    
    sc_b1549[sc_b1549$year == 2005, "na"],
    sc_m1549[sc_m1549$year == 2005, "na"],
    sc_f1549[sc_f1549$year == 2005, "na"],
    
    sc_b1549[sc_b1549$year == 2010, "na"],
    sc_m1549[sc_m1549$year == 2010, "na"],
    sc_f1549[sc_f1549$year == 2010, "na"],
    
    sc_b1549[sc_b1549$year == 2015, "na"],
    sc_m1549[sc_m1549$year == 2015, "na"],
    sc_f1549[sc_f1549$year == 2015, "na"],
    
    sc_b1549[sc_b1549$year == 2021, "na"],
    sc_m1549[sc_m1549$year == 2021, "na"],
    sc_f1549[sc_f1549$year == 2021, "na"]
  )
)

c7 <- cbind(
  rbind(
    sc_b1549[sc_b1549$year == 1990, "sdi"],
    sc_m1549[sc_m1549$year == 1990, "sdi"],
    sc_f1549[sc_f1549$year == 1990, "sdi"],
    
    sc_b1549[sc_b1549$year == 1995, "sdi"],
    sc_m1549[sc_m1549$year == 1995, "sdi"],
    sc_f1549[sc_f1549$year == 1995, "sdi"],
    
    sc_b1549[sc_b1549$year == 2000, "sdi"],
    sc_m1549[sc_m1549$year == 2000, "sdi"],
    sc_f1549[sc_f1549$year == 2000, "sdi"],
    
    sc_b1549[sc_b1549$year == 2005, "sdi"],
    sc_m1549[sc_m1549$year == 2005, "sdi"],
    sc_f1549[sc_f1549$year == 2005, "sdi"],
    
    sc_b1549[sc_b1549$year == 2010, "sdi"],
    sc_m1549[sc_m1549$year == 2010, "sdi"],
    sc_f1549[sc_f1549$year == 2010, "sdi"],
    
    sc_b1549[sc_b1549$year == 2015, "sdi"],
    sc_m1549[sc_m1549$year == 2015, "sdi"],
    sc_f1549[sc_f1549$year == 2015, "sdi"],
    
    sc_b1549[sc_b1549$year == 2021, "sdi"],
    sc_m1549[sc_m1549$year == 2021, "sdi"],
    sc_f1549[sc_f1549$year == 2021, "sdi"]
  )
)

table1_1549[,3] <- c4
table1_1549[,4] <- c5
table1_1549[,5] <- c7

###same for other age groups

#################

############### ARIMA


####### 

###########sdi

#1549
sdi_b1549 = auto.arima(
  sc_b1549$sdi,    
  ic="aic",       
  stepwise=F,     
  approximation=F,   
  max.p=10,       ### p is AR terms      
  max.q=10,        ### q is MA terms 
  max.order=20,
  parallel=T,     
  num.cores=1     
)

sdi_m1549 = auto.arima(
  sc_m1549$sdi,    
  ic="aic",       
  stepwise=F,     
  approximation=F,   
  max.p=10,         
  max.q=10,        
  max.order=20,
  parallel=T,     
  num.cores=1     
)


sdi_f1549 = auto.arima(
  sc_f1549$sdi,     
  ic="aic",      
  stepwise=F,     
  approximation=F,   
  max.p=10,       
  max.q=10,        
  max.order=20,
  parallel=T,     
  num.cores=1     
)


#5069

sdi_b5069 = auto.arima(
  sc_b5069$sdi,   
  ic="aic",       
  stepwise=F,     
  approximation=F,   
  max.p=10,       
  max.q=10,       
  max.order=20,
  parallel=T,      
  num.cores=1     
)

sdi_m5069 = auto.arima(
  sc_m5069$sdi,    
  ic="aic",       
  stepwise=F,     
  approximation=F,  
  max.p=10,            
  max.q=10,        
  max.order=20,
  parallel=T,     
  num.cores=1     
)


sdi_f5069 = auto.arima(
  sc_f5069$sdi,    
  ic="aic",       
  stepwise=F,     
  approximation=F,   
  max.p=10,       
  max.q=10,       
  max.order=20,
  parallel=T,      
  num.cores=1     
)

#70

sdi_b70 = auto.arima(
  sc_b70$sdi,    
  ic="aic",       
  stepwise=F,     
  approximation=F,  
  max.p=10,           
  max.q=10,        
  max.order=20,
  parallel=T,     
  num.cores=1     
)

sdi_m70 = auto.arima(
  sc_m70$sdi,    
  ic="aic",       
  stepwise=F,     
  approximation=F,   
  max.p=10,       
  max.q=10,        
  max.order=20,
  parallel=T,      
  num.cores=1     
)


sdi_f70 = auto.arima(
  sc_f70$sdi,    
  ic="aic",      
  stepwise=F,     
  approximation=F,   
  max.p=10,           
  max.q=10,        
  max.order=20,
  parallel=T,      
  num.cores=1     
)

################SEV diet high in sodium

#1549
na_b1549 = auto.arima(
  sc_b1549$na,    
  ic="aic",       
  stepwise=F,    
  approximation=F,   
  max.p=10,       
  max.q=10,       
  max.order=20,
  parallel=T,      
  num.cores=1    
)

na_m1549 = auto.arima(
  sc_m1549$na,    
  ic="aic",       
  stepwise=F,     
  approximation=F,   
  max.p=10,           
  max.q=10,        
  max.order=20,
  parallel=T,      
  num.cores=1     
)


na_f1549 = auto.arima(
  sc_f1549$na,    
  ic="aic",       
  stepwise=F,     
  approximation=F,   
  max.p=10,       
  max.q=10,        
  max.order=20,
  parallel=T,     
  num.cores=1     
)

#5069

na_b5069 = auto.arima(
  sc_b5069$na,    
  ic="aic",       
  stepwise=F,    
  approximation=F,   
  max.p=10,        
  max.q=10,       
  max.order=20,
  parallel=T,      
  num.cores=1     
)

na_m5069 = auto.arima(
  sc_m5069$na,    
  ic="aic",       
  stepwise=F,     
  approximation=F,   
  max.p=10,       
  max.q=10,        
  max.order=20,
  parallel=T,     
  num.cores=1     
)


na_f5069 = auto.arima(
  sc_f5069$na,    
  ic="aic",       
  stepwise=F,     
  approximation=F,    
  max.p=10,           
  max.q=10,        
  max.order=20,
  parallel=T,      
  num.cores=1     
)


#70
na_b70 = auto.arima(
  sc_b70$na,   
  ic="aic",       
  stepwise=F,    
  approximation=F,   
  max.p=10,       
  max.q=10,        
  max.order=20,
  parallel=T,      
  num.cores=1     
)

na_m70 = auto.arima(
  sc_m70$na,   
  ic="aic",       
  stepwise=F,     
  approximation=F,   
  max.p=10,          
  max.q=10,       
  max.order=20,
  parallel=T,       
  num.cores=1     
)


na_f70 = auto.arima(
  sc_f70$na,    
  ic="aic",       
  stepwise=F,     
  approximation=F,   
  max.p=10,       
  max.q=10,       
  max.order=20,
  parallel=T,      
  num.cores=1     
)

##predict until 2050

#1549
p_sdi_b1549=c(sc_b1549$sdi,forecast(sdi_b1549,h=29)$mean)  
p_sdi_m1549=c(sc_m1549$sdi,forecast(sdi_m1549,h=29)$mean)
p_sdi_f1549=c(sc_f1549$sdi,forecast(sdi_f1549,h=29)$mean)

#5069
p_sdi_b5069=c(sc_b5069$sdi,forecast(sdi_b5069,h=29)$mean) 
p_sdi_m5069=c(sc_m5069$sdi,forecast(sdi_m5069,h=29)$mean)
p_sdi_f5069=c(sc_f5069$sdi,forecast(sdi_f5069,h=29)$mean)

#70
p_sdi_b70=c(sc_b70$sdi,forecast(sdi_b70,h=29)$mean)  
p_sdi_m70=c(sc_m70$sdi,forecast(sdi_m70,h=29)$mean)
p_sdi_f70=c(sc_f70$sdi,forecast(sdi_f70,h=29)$mean)


#Current trend (baseline)
#1549
p_na_b1549=c(sc_b1549$na,forecast(na_b1549,h=29)$mean) 
p_na_m1549=c(sc_m1549$na,forecast(na_m1549,h=29)$mean) 
p_na_f1549=c(sc_f1549$na,forecast(na_f1549,h=29)$mean) 


#5069
p_na_b5069=c(sc_b5069$na,forecast(na_b5069,h=29)$mean) 
p_na_m5069=c(sc_m5069$na,forecast(na_m5069,h=29)$mean) 
p_na_f5069=c(sc_f5069$na,forecast(na_f5069,h=29)$mean) 

#70
p_na_b70=c(sc_b70$na,forecast(na_b70,h=29)$mean) 
p_na_m70=c(sc_m70$na,forecast(na_m70,h=29)$mean) 
p_na_f70=c(sc_f70$na,forecast(na_f70,h=29)$mean) 


# 50% reduction until 2050

#1549
s1_na_b1549 = c(sc_b1549$na, rev(seq(from = sc_b1549$na[32]*0.5, to = sc_b1549$na[32],length.out=29)))
s1_na_m1549 = c(sc_m1549$na, rev(seq(from = sc_m1549$na[32]*0.5, to = sc_m1549$na[32],length.out=29)))
s1_na_f1549 = c(sc_f1549$na, rev(seq(from = sc_f1549$na[32]*0.5, to = sc_f1549$na[32],length.out=29)))

#5069
s1_na_b5069 = c(sc_b5069$na, rev(seq(from = sc_b5069$na[32]*0.5, to = sc_b5069$na[32],length.out=29)))
s1_na_m5069 = c(sc_m5069$na, rev(seq(from = sc_m5069$na[32]*0.5, to = sc_m5069$na[32],length.out=29)))
s1_na_f5069 = c(sc_f5069$na, rev(seq(from = sc_f5069$na[32]*0.5, to = sc_f5069$na[32],length.out=29)))

#70
s1_na_b70 = c(sc_b70$na, rev(seq(from = sc_b70$na[32]*0.5, to = sc_b70$na[32],length.out=29)))
s1_na_m70 = c(sc_m70$na, rev(seq(from = sc_m70$na[32]*0.5, to = sc_m70$na[32],length.out=29)))
s1_na_f70 = c(sc_f70$na, rev(seq(from = sc_f70$na[32]*0.5, to = sc_f70$na[32],length.out=29)))


### prepare xreg for prediction by scenarios (attributable DALY)

#current trend

#1549
xreg0_attb1549 = as.matrix(cbind(
  sdi=p_sdi_b1549[33:61],
  na=p_na_b1549[33:61]
))

xreg0_attm1549 = as.matrix(cbind(
  sdi=p_sdi_m1549[33:61],
  na=p_na_m1549[33:61]
))

xreg0_attf1549 = as.matrix(cbind(
  sdi=p_sdi_f1549[33:61],
  na=p_na_f1549[33:61]
))

#5069
xreg0_attb5069 = as.matrix(cbind(
  sdi=p_sdi_b5069[33:61],
  na=p_na_b5069[33:61]
))

xreg0_attm5069 = as.matrix(cbind(
  sdi=p_sdi_m5069[33:61],
  na=p_na_m5069[33:61]
))

xreg0_attf5069 = as.matrix(cbind(
  sdi=p_sdi_f5069[33:61],
  na=p_na_f5069[33:61]
))

#70
xreg0_attb70 = as.matrix(cbind(
  sdi=p_sdi_b70[33:61],
  na=p_na_b70[33:61]
))

xreg0_attm70 = as.matrix(cbind(
  sdi=p_sdi_m70[33:61],
  na=p_na_m70[33:61]
))

xreg0_attf70 = as.matrix(cbind(
  sdi=p_sdi_f70[33:61],
  na=p_na_f70[33:61]
))

#S1

#1549
xreg1_attb1549 = as.matrix(cbind(
  sdi=p_sdi_b1549[33:61],
  na=s1_na_b1549[33:61]
))

xreg1_attm1549 = as.matrix(cbind(
  sdi=p_sdi_m1549[33:61],
  na=s1_na_m1549[33:61]
))

xreg1_attf1549 = as.matrix(cbind(
  sdi=p_sdi_f1549[33:61],
  na=s1_na_f1549[33:61]
))

#5069
xreg1_attb5069 = as.matrix(cbind(
  sdi=p_sdi_b5069[33:61],
  na=s1_na_b5069[33:61]
))

xreg1_attm5069 = as.matrix(cbind(
  sdi=p_sdi_m5069[33:61],
  na=s1_na_m5069[33:61]
))

xreg1_attf5069 = as.matrix(cbind(
  sdi=p_sdi_f5069[33:61],
  na=s1_na_f5069[33:61]
))

#70
xreg1_attb70 = as.matrix(cbind(
  sdi=p_sdi_b70[33:61],
  na=s1_na_b70[33:61]
))

xreg1_attm70 = as.matrix(cbind(
  sdi=p_sdi_m70[33:61],
  na=s1_na_m70[33:61]
))

xreg1_attf70 = as.matrix(cbind(
  sdi=p_sdi_f70[33:61],
  na=s1_na_f70[33:61]
))




######
####ARIMAX

#attributable DALY

#1549
res_sc_attb1549 = auto.arima(
  log(sc_b1549$at_daly),
  xreg = as.matrix(sc_b1549[1:32,c("sdi","na")]),
  ic="aic",
  stepwise=F,
  approximation=F,
  max.p=10, 
  max.q=10,
  max.order=20,
  parallel=T,
  num.cores=1 
)

res_sc_attm1549 = auto.arima(
  log(sc_m1549$at_daly),
  xreg = as.matrix(sc_m1549[1:32,c("sdi","na")]),
  ic="aic",
  stepwise=F,
  approximation=F,
  max.p=10, 
  max.q=10,
  max.order=20,
  parallel=T,
  num.cores=1 
)

res_sc_attf1549 = auto.arima(
  log(sc_f1549$at_daly),
  xreg = as.matrix(sc_f1549[1:32,c("sdi","na")]),
  ic="aic",
  stepwise=F,
  approximation=F,
  max.p=10, 
  max.q=10,
  max.order=20,
  parallel=T,
  num.cores=1 
)

#5069
res_sc_attb5069 = auto.arima(
  log(sc_b5069$at_daly),
  xreg = as.matrix(sc_b5069[1:32,c("sdi","na")]),
  ic="aic",
  stepwise=F,
  approximation=F,
  max.p=10, 
  max.q=10,
  max.order=20,
  parallel=T,
  num.cores=1 
)

res_sc_attm5069 = auto.arima(
  log(sc_m5069$at_daly),
  xreg = as.matrix(sc_m5069[1:32,c("sdi","na")]),
  ic="aic",
  stepwise=F,
  approximation=F,
  max.p=10, 
  max.q=10,
  max.order=20,
  parallel=T,
  num.cores=1 
)

res_sc_attf5069 = auto.arima(
  log(sc_f5069$at_daly),
  xreg = as.matrix(sc_f5069[1:32,c("sdi","na")]),
  ic="aic",
  stepwise=F,
  approximation=F,
  max.p=10, 
  max.q=10,
  max.order=20,
  parallel=T,
  num.cores=1 
)


#70
res_sc_attb70 = auto.arima(
  log(sc_b70$at_daly),
  xreg = as.matrix(sc_b70[1:32,c("sdi","na")]),
  ic="aic",
  stepwise=F,
  approximation=F,
  max.p=10, 
  max.q=10,
  max.order=20,
  parallel=T,
  num.cores=1 
)

res_sc_attm70 = auto.arima(
  log(sc_m70$at_daly),
  xreg = as.matrix(sc_m70[1:32,c("sdi","na")]),
  ic="aic",
  stepwise=F,
  approximation=F,
  max.p=10, 
  max.q=10,
  max.order=20,
  parallel=T,
  num.cores=1 
)

res_sc_attf70 = auto.arima(
  log(sc_f70$at_daly),
  xreg = as.matrix(sc_f70[1:32,c("sdi","na")]),
  ic="aic",
  stepwise=F,
  approximation=F,
  max.p=10, 
  max.q=10,
  max.order=20,
  parallel=T,
  num.cores=1 
)


##attributable daly

#1549
sc_attb1549_forecast0 = forecast(res_sc_attb1549, h=29,xreg = xreg0_attb1549)
sc_attb1549_forecast1 = forecast(res_sc_attb1549, h=29,xreg = xreg1_attb1549)


#5069
sc_attb5069_forecast0 = forecast(res_sc_attb5069, h=29,xreg = xreg0_attb5069)
sc_attb5069_forecast1 = forecast(res_sc_attb5069, h=29,xreg = xreg1_attb5069)


#70
sc_attb70_forecast0 = forecast(res_sc_attb70, h=29,xreg = xreg0_attb70)
sc_attb70_forecast1 = forecast(res_sc_attb70, h=29,xreg = xreg1_attb70)


#1549
sc_attb1549_output = data.frame(rbind(data.frame(
  year = c(1990:2050),
  pred = "Reference",
  value = exp(c(sc_attb1549_forecast0$x, sc_attb1549_forecast0$mean)),
  ll = exp(c(sc_attb1549_forecast0$x, sc_attb1549_forecast0$lower[,2])),
  ul = exp(c(sc_attb1549_forecast0$x, sc_attb1549_forecast0$upper[,2]))
),
data.frame(
  year = c(1990:2050),
  pred = "Scenario 1",
  value = exp(c(sc_attb1549_forecast1 $x, sc_attb1549_forecast1 $mean)),
  ll = exp(c(sc_attb1549_forecast1 $x, sc_attb1549_forecast1$lower[,2])),
  ul = exp(c(sc_attb1549_forecast1 $x, sc_attb1549_forecast1$upper[,2]))
)
))

#5069
sc_attb5069_output = data.frame(rbind(data.frame(
  year = c(1990:2050),
  pred = "Reference",
  value = exp(c(sc_attb5069_forecast0$x, sc_attb5069_forecast0$mean)),
  ll = exp(c(sc_attb5069_forecast0$x, sc_attb5069_forecast0$lower[,2])),
  ul = exp(c(sc_attb5069_forecast0$x, sc_attb5069_forecast0$upper[,2]))
),
data.frame(
  year = c(1990:2050),
  pred = "Scenario 1",
  value = exp(c(sc_attb5069_forecast1 $x, sc_attb5069_forecast1 $mean)),
  ll = exp(c(sc_attb5069_forecast1 $x, sc_attb5069_forecast1$lower[,2])),
  ul = exp(c(sc_attb5069_forecast1 $x, sc_attb5069_forecast1$upper[,2]))
)
))

#70
sc_attb70_output = data.frame(rbind(data.frame(
  year = c(1990:2050),
  pred = "Reference",
  value = exp(c(sc_attb70_forecast0$x, sc_attb70_forecast0$mean)),
  ll = exp(c(sc_attb70_forecast0$x, sc_attb70_forecast0$lower[,2])),
  ul = exp(c(sc_attb70_forecast0$x, sc_attb70_forecast0$upper[,2]))
),
data.frame(
  year = c(1990:2050),
  pred = "Scenario 1",
  value = exp(c(sc_attb70_forecast1 $x, sc_attb70_forecast1 $mean)),
  ll = exp(c(sc_attb70_forecast1 $x, sc_attb70_forecast1$lower[,2])),
  ul = exp(c(sc_attb70_forecast1 $x, sc_attb70_forecast1$upper[,2]))
)
))

########males

#1549
sc_attm1549_forecast0 = forecast(res_sc_attm1549, h=29,xreg = xreg0_attm1549)
sc_attm1549_forecast1 = forecast(res_sc_attm1549, h=29,xreg = xreg1_attm1549)

#5069
sc_attm5069_forecast0 = forecast(res_sc_attm5069, h=29,xreg = xreg0_attm5069)
sc_attm5069_forecast1 = forecast(res_sc_attm5069, h=29,xreg = xreg1_attm5069)

#70
sc_attm70_forecast0 = forecast(res_sc_attm70, h=29,xreg = xreg0_attm70)
sc_attm70_forecast1 = forecast(res_sc_attm70, h=29,xreg = xreg1_attm70)


#1549
sc_attm1549_output = data.frame(rbind(data.frame(
  year = c(1990:2050),
  pred = "Reference",
  value = exp(c(sc_attm1549_forecast0$x, sc_attm1549_forecast0$mean)),
  ll = exp(c(sc_attm1549_forecast0$x, sc_attm1549_forecast0$lower[,2])),
  ul = exp(c(sc_attm1549_forecast0$x, sc_attm1549_forecast0$upper[,2]))
),
data.frame(
  year = c(1990:2050),
  pred = "Scenario 1",
  value = exp(c(sc_attm1549_forecast1 $x, sc_attm1549_forecast1$mean)),
  ll = exp(c(sc_attm1549_forecast1 $x, sc_attm1549_forecast1$lower[,2])),
  ul = exp(c(sc_attm1549_forecast1 $x, sc_attm1549_forecast1$upper[,2]))
)
))

#5069
sc_attm5069_output = data.frame(rbind(data.frame(
  year = c(1990:2050),
  pred = "Reference",
  value = exp(c(sc_attm5069_forecast0$x, sc_attm5069_forecast0$mean)),
  ll = exp(c(sc_attm5069_forecast0$x, sc_attm5069_forecast0$lower[,2])),
  ul = exp(c(sc_attm5069_forecast0$x, sc_attm5069_forecast0$upper[,2]))
),
data.frame(
  year = c(1990:2050),
  pred = "Scenario 1",
  value = exp(c(sc_attm5069_forecast1 $x, sc_attm5069_forecast1$mean)),
  ll = exp(c(sc_attm5069_forecast1 $x, sc_attm5069_forecast1$lower[,2])),
  ul = exp(c(sc_attm5069_forecast1 $x, sc_attm5069_forecast1$upper[,2]))
)
))

#70
sc_attm70_output = data.frame(rbind(data.frame(
  year = c(1990:2050),
  pred = "Reference",
  value = exp(c(sc_attm70_forecast0$x, sc_attm70_forecast0$mean)),
  ll = exp(c(sc_attm70_forecast0$x, sc_attm70_forecast0$lower[,2])),
  ul = exp(c(sc_attm70_forecast0$x, sc_attm70_forecast0$upper[,2]))
),
data.frame(
  year = c(1990:2050),
  pred = "Scenario 1",
  value = exp(c(sc_attm70_forecast1 $x, sc_attm70_forecast1$mean)),
  ll = exp(c(sc_attm70_forecast1 $x, sc_attm70_forecast1$lower[,2])),
  ul = exp(c(sc_attm70_forecast1 $x, sc_attm70_forecast1$upper[,2]))
)
))

#########females

#1549
sc_attf1549_forecast0 = forecast(res_sc_attf1549, h=29,xreg = xreg0_attf1549)
sc_attf1549_forecast1 = forecast(res_sc_attf1549, h=29,xreg = xreg1_attf1549)

#5069
sc_attf5069_forecast0 = forecast(res_sc_attf5069, h=29,xreg = xreg0_attf5069)
sc_attf5069_forecast1 = forecast(res_sc_attf5069, h=29,xreg = xreg1_attf5069)


#70
sc_attf70_forecast0 = forecast(res_sc_attf70, h=29,xreg = xreg0_attf70)
sc_attf70_forecast1 = forecast(res_sc_attf70, h=29,xreg = xreg1_attf70)


#1549
sc_attf1549_output = data.frame(rbind(data.frame(
  year = c(1990:2050),
  pred = "Reference",
  value = exp(c(sc_attf1549_forecast0$x, sc_attf1549_forecast0$mean)),
  ll = exp(c(sc_attf1549_forecast0$x, sc_attf1549_forecast0$lower[,2])),
  ul = exp(c(sc_attf1549_forecast0$x, sc_attf1549_forecast0$upper[,2]))
),
data.frame(
  year = c(1990:2050),
  pred = "Scenario 1",
  value = exp(c(sc_attf1549_forecast1$x, sc_attf1549_forecast1 $mean)),
  ll = exp(c(sc_attf1549_forecast1 $x, sc_attf1549_forecast1$lower[,2])),
  ul = exp(c(sc_attf1549_forecast1 $x, sc_attf1549_forecast1$upper[,2]))
)
))

#5069
sc_attf5069_output = data.frame(rbind(data.frame(
  year = c(1990:2050),
  pred = "Reference",
  value = exp(c(sc_attf5069_forecast0$x, sc_attf5069_forecast0$mean)),
  ll = exp(c(sc_attf5069_forecast0$x, sc_attf5069_forecast0$lower[,2])),
  ul = exp(c(sc_attf5069_forecast0$x, sc_attf5069_forecast0$upper[,2]))
),
data.frame(
  year = c(1990:2050),
  pred = "Scenario 1",
  value = exp(c(sc_attf5069_forecast1$x, sc_attf5069_forecast1 $mean)),
  ll = exp(c(sc_attf5069_forecast1 $x, sc_attf5069_forecast1$lower[,2])),
  ul = exp(c(sc_attf5069_forecast1 $x, sc_attf5069_forecast1$upper[,2]))
)
))

#70
sc_attf70_output = data.frame(rbind(data.frame(
  year = c(1990:2050),
  pred = "Reference",
  value = exp(c(sc_attf70_forecast0$x, sc_attf70_forecast0$mean)),
  ll = exp(c(sc_attf70_forecast0$x, sc_attf70_forecast0$lower[,2])),
  ul = exp(c(sc_attf70_forecast0$x, sc_attf70_forecast0$upper[,2]))
),
data.frame(
  year = c(1990:2050),
  pred = "Scenario 1",
  value = exp(c(sc_attf70_forecast1$x, sc_attf70_forecast1 $mean)),
  ll = exp(c(sc_attf70_forecast1 $x, sc_attf70_forecast1$lower[,2])),
  ul = exp(c(sc_attf70_forecast1 $x, sc_attf70_forecast1$upper[,2]))
)
))


# Results plot --------------------------------------------------------

#sex combined

sc_attplot1549 = ggplot(sc_attb1549_output)+
  geom_ribbon(aes(x=year,ymin=ll,ymax=ul,fill=pred),alpha=0.2)+
  geom_line(aes(x=year, y= value, color=pred), linewidth=0.3)+
  geom_line(data=sc_attb1549_output[1:32,],aes(x=year, y= value, linetype=factor(50,labels=c("Observed"))),linewidth=0.3)+
  ylab("Attributable DALYs rate")+ylim(c(0,35))+
  xlab("Year")+
  scale_x_continuous(breaks = seq(1990, 2050, by = 10), limits = c(1990, 2050)) +
  ggtitle("Age 15-49 years, sex combined")+
  theme_minimal()+theme(legend.title=element_blank())
sc_attplot1549

#ggsave("sc_attplot1549.png", sc_attplot1549, bg = "white", width = 14, height = 7, dpi = 300)


sc_attplot5069 = ggplot(sc_attb5069_output)+
  geom_ribbon(aes(x=year,ymin=ll,ymax=ul,fill=pred),alpha=0.2)+
  geom_line(aes(x=year, y= value, color=pred), linewidth=0.3)+
  geom_line(data=sc_attb5069_output[1:32,],aes(x=year, y= value, linetype=factor(50,labels=c("Observed"))),linewidth=0.3)+
  ylab("Attributable DALYs rate")+ylim(c(0,200))+
  xlab("Year")+
  scale_x_continuous(breaks = seq(1990, 2050, by = 10), limits = c(1990, 2050)) +
  ggtitle("Age 50-69 years, sex combined")+
  theme_minimal()+theme(legend.title=element_blank())
sc_attplot5069

#ggsave("sc_attplot5069.png", sc_attplot5069, bg = "white", width = 14, height = 7, dpi = 300)

sc_attplot70 = ggplot(sc_attb70_output)+
  geom_ribbon(aes(x=year,ymin=ll,ymax=ul,fill=pred),alpha=0.2)+
  geom_line(aes(x=year, y= value, color=pred), linewidth=0.3)+
  geom_line(data=sc_attb70_output[1:32,],aes(x=year, y= value, linetype=factor(50,labels=c("Observed"))),linewidth=0.3)+
  ylab("Attributable DALYs rate")+ylim(c(0,400))+
  xlab("Year")+
  scale_x_continuous(breaks = seq(1990, 2050, by = 10), limits = c(1990, 2050)) +
  ggtitle("Age 70+ years, sex combined")+
  theme_minimal()+theme(legend.title=element_blank())
sc_attplot70

#ggsave("sc_attplot70.png", sc_attplot70, bg = "white", width = 14, height = 7, dpi = 300)

##males

sc_attplotm1549 = ggplot(sc_attm1549_output)+
  geom_ribbon(aes(x=year,ymin=ll,ymax=ul,fill=pred),alpha=0.2)+
  geom_line(aes(x=year, y= value, color=pred), linewidth=0.3)+
  geom_line(data=sc_attm1549_output[1:32,],aes(x=year, y= value, linetype=factor(50,labels=c("Observed"))),linewidth=0.3)+
  ylab("Attributable DALYs rate")+ylim(c(0,35))+
  xlab("Year")+
  scale_x_continuous(breaks = seq(1990, 2050, by = 10), limits = c(1990, 2050)) +
  ggtitle("Males aged 15-49 years")+
  theme_minimal()+theme(legend.title=element_blank())
sc_attplotm1549

#ggsave("sc_attplotm1549.png", sc_attplotm1549, bg = "white", width = 14, height = 7, dpi = 300)

sc_attplotm5069 = ggplot(sc_attm5069_output)+
  geom_ribbon(aes(x=year,ymin=ll,ymax=ul,fill=pred),alpha=0.2)+
  geom_line(aes(x=year, y= value, color=pred), linewidth=0.3)+
  geom_line(data=sc_attm5069_output[1:32,],aes(x=year, y= value, linetype=factor(50,labels=c("Observed"))),linewidth=0.3)+
  ylab("Attributable DALYs rate")+ylim(c(0,300))+
  xlab("Year")+
  scale_x_continuous(breaks = seq(1990, 2050, by = 10), limits = c(1990, 2050)) +
  ggtitle("Males aged 50-69 years")+
  theme_minimal()+theme(legend.title=element_blank())
sc_attplotm5069

#ggsave("sc_attplotm5069.png", sc_attplotm5069, bg = "white", width = 14, height = 7, dpi = 300)


sc_attplotm70 = ggplot(sc_attm70_output)+
  geom_ribbon(aes(x=year,ymin=ll,ymax=ul,fill=pred),alpha=0.2)+
  geom_line(aes(x=year, y= value, color=pred), linewidth=0.3)+
  geom_line(data=sc_attm70_output[1:32,],aes(x=year, y= value, linetype=factor(50,labels=c("Observed"))),linewidth=0.3)+
  ylab("Attributable DALYs rate")+ylim(c(0,650))+
  xlab("Year")+
  scale_x_continuous(breaks = seq(1990, 2050, by = 10), limits = c(1990, 2050)) +
  ggtitle("Males aged 70 and 70+ years")+
  theme_minimal()+theme(legend.title=element_blank())
sc_attplotm70

#ggsave("sc_attplotm70.png", sc_attplotm70, bg = "white", width = 14, height = 7, dpi = 300)


##females

sc_attplotf1549 = ggplot(sc_attf1549_output)+
  geom_ribbon(aes(x=year,ymin=ll,ymax=ul,fill=pred),alpha=0.2)+
  geom_line(aes(x=year, y= value, color=pred), linewidth=0.3)+
  geom_line(data=sc_attf1549_output[1:32,],aes(x=year, y= value, linetype=factor(50,labels=c("Observed"))),linewidth=0.3)+
  ylab("Attributable DALYs rate")+ylim(c(0,35))+
  xlab("Year")+
  scale_x_continuous(breaks = seq(1990, 2050, by = 10), limits = c(1990, 2050)) +
  ggtitle("Females aged 15-49 years")+
  theme_minimal()+theme(legend.title=element_blank())
sc_attplotf1549

#ggsave("sc_attplotf1549.png", sc_attplotf1549, bg = "white", width = 14, height = 7, dpi = 300)


sc_attplotf5069 = ggplot(sc_attf5069_output)+
  geom_ribbon(aes(x=year,ymin=ll,ymax=ul,fill=pred),alpha=0.2)+
  geom_line(aes(x=year, y= value, color=pred), linewidth=0.3)+
  geom_line(data=sc_attf5069_output[1:32,],aes(x=year, y= value, linetype=factor(50,labels=c("Observed"))),linewidth=0.3)+
  ylab("Attributable DALYs rate")+ylim(c(0,150))+
  xlab("Year")+
  scale_x_continuous(breaks = seq(1990, 2050, by = 10), limits = c(1990, 2050)) +
  ggtitle("Females aged 50-69 years")+
  theme_minimal()+theme(legend.title=element_blank())
sc_attplotf5069

#ggsave("sc_attplotf5069.png", sc_attplotf5069, bg = "white", width = 14, height = 7, dpi = 300)


sc_attplotf70 = ggplot(sc_attf70_output)+
  geom_ribbon(aes(x=year,ymin=ll,ymax=ul,fill=pred),alpha=0.2)+
  geom_line(aes(x=year, y= value, color=pred), linewidth=0.3)+
  geom_line(data=sc_attf70_output[1:32,],aes(x=year, y= value, linetype=factor(50,labels=c("Observed"))),linewidth=0.3)+
  ylab("Attributable DALYs rate")+ylim(c(0,300))+
  xlab("Year")+
  scale_x_continuous(breaks = seq(1990, 2050, by = 10), limits = c(1990, 2050)) +
  ggtitle("Females aged 70 and 70+")+
  theme_minimal()+theme(legend.title=element_blank())
sc_attplotf70

#ggsave("sc_attplotf70.png", sc_attplotf70, bg = "white", width = 14, height = 7, dpi = 300)



