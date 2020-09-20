library(Lahman)
library(dplyr)
library(ggplot2)

?Teams

Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(R_per_game = R/G, AB_per_game = AB/G) %>%
  ggplot(aes(R_per_game,AB_per_game))+
  geom_point(alpha=0.5)
  

Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(wins_per_game = W/G, FE_per_game = E/G) %>%
  ggplot(aes(wins_per_game,-FE_per_game))+
  geom_point(alpha=0.5)

Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(wins_per_game = W/G, FE_per_game = E/G) %>%
  ggplot(aes(wins_per_game,FE_per_game))+
  geom_smooth(alpha=0.5)


Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(X3B_per_game = X3B/G, X2B_per_game = X2B/G) %>%
  ggplot(aes(X3B_per_game,X2B_per_game))+
  geom_point(alpha=0.5)


install.packages("HistData")
library(HistData)
?HistData

data("GaltonFamilies")

colnames(GaltonFamilies)
set.seed(1983)


galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) + xlim(60,80) + ylim(60, 80) + geom_abline()


GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1)

rho <- mean(scale(x)*scale(y))

galton_heights %>% summarize(r = cor(father, son))


library(Lahman)

Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(R_per_game = R/G, AB_per_game = AB/G) %>%
  summarise(r= cor(R_per_game,AB_per_game))

Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(wins_per_game = W/G, FE_per_game = E/G) %>%
  summarise(r= cor(wins_per_game,FE_per_game))


Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(X3B_per_game = X3B/G, X2B_per_game = X2B/G) %>%
  summarise(r= cor(X2B_per_game,X3B_per_game))



galton_heights %>% mutate(father_strata = factor(round(father))) %>% 
  ggplot(aes(father_strata, son)) + 
  geom_boxplot() + 
  geom_point()



galton_heights %>% mutate(father_strata = round(father)) %>% 
  group_by(father_strata) %>% summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father_strata, son_conditional_avg)) + 
  geom_point()



# calculate values to plot regression line on original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)

# add regression line to plot
galton_heights %>% 
  ggplot(aes(father, son)) + 
  geom_point(alpha = 0.5) +
  geom_abline(slope = r * s_y/s_x, intercept = mu_y - r * s_y/s_x * mu_x) 



#When a pair of random variables are approximated by the bivariate normal distribution, scatterplots look like ovals. They can be thin (high correlation) or circle-shaped (no correlation).
galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) + #QQ plots with an abline  => Normal distribution
  facet_wrap( ~ z_father)



set.seed(1989) #if you are using R 3.6 or later
set.seed(1989, sample.kind="Rounding")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     #Sample one row from each family
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mean(female_heights$mother)
sd(female_heights$mother)
mean(female_heights$daughter)
sd(female_heights$daughter)

1/(sd(female_heights$daughter)/sd(female_heights$mother))

female_heights  %>% summarize(r = cor(mother,daughter))


reg <- function(x,y){
  mu_x <- mean(x)
  mu_y <- mean(y)
  s_x <- sd(x)
  s_y <- sd(y)
  r <- cor(x,y)
  r_slope = r * s_y/s_x
  r_intercept = mu_y - r * s_y/s_x * mu_x
  print ( r_slope)
  print ( r_intercept)
  return(r_slope,r_intercept)
}

reg(female_heights$mother,female_heights$daughter)

mu_x <- mean(female_heights$mother)
mu_y <- mean(female_heights$daughter)
s_x <- sd(female_heights$mother)
s_y <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)

mu_x
s_x
s_y
r

r*s_y/s_x


s_y/s_x
r**2 

female_heights %>% filter(mother == 60)%>% pull(daughter) %>% mean()

exp_val <- function(x,y,z_y){
  mu_x <- mean(x)
  mu_y <- mean(y)
  s_x <- sd(x)
  s_y <- sd(y)
  r <- cor(x,y)
  r_slope <- r * s_y/s_x
  r_intercept <- mu_y - r * s_y/s_x * mu_x
  
  return(r_slope*z_y + r_intercept )
}


exp_val(female_heights$mother,female_heights$daughter,60)




# calculate correlation between HR, BB and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))



# stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)
# scatterplot for each HR stratum
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)
# calculate slope of regression line after stratifying by HR
dat %>%  
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))


dat %>%  
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))

lm(son ~ father, data = galton_heights)

?lm
#lm(son~father) gives the intercept and the slope and intercept of the regression line.

#0.493 is the slope. i.e, for every unit increase in the fathers height, sons height will increase by 0.493 units


summary_stats <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
  summarize(avg_HR = mean(HR_per_game),
            s_HR = sd(HR_per_game),
            avg_R = mean(R_per_game),
            s_R = sd(R_per_game),
            r = cor(HR_per_game, R_per_game))

reg_line <- summary_stats %>% summarize(slope = r*s_R/s_HR,
                                        intercept = avg_R - slope*avg_HR)
p <- Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
p + geom_abline(intercept = reg_line$intercept, slope = reg_line$slope)


#We can acieve the same line by the following code

p + geom_smooth(method = "lm")

# the code is useful to predict the Runs per game based on the number of HRs (Home runs)

#We want the intercept term for our model to be more interpretable, so we run the same model as before but now we subtract the mean of fathers’ heights from each individual father’s height to create a new variable centered at zero.

galton_heights <- galton_heights %>%
  mutate(father_centered=father - mean(father))


lm(son ~ father_centered, data = galton_heights)

#By replacing x_i with mean(x)-x_i, we have an equation which tells us directly that 
# Height of a son with father of average height (x_i = mean(x)) will be the intercept (69.144 inches)



#RSS = Residual sum of squares. Least square prediction is based on minimizing this value

rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)


Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G,BB_per_game = BB / G)%>%
  lm(R_per_game ~ HR_per_game + BB_per_game, data=.)



#We run a Monte Carlo simulation where we repeatedly take samples of N = 100 from the Galton heights data and compute the regression slope coefficients for each sample:


B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

head(lse)
hist(lse$beta_1)

lse %>% ggplot(aes(.$beta_0))+
  geom_histogram()



galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

#-----Another way to plot with confidence intervals
model <- lm(son ~ father, data = galton_heights)
# predictions <- predict(model) gives no confidence intervals


predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

#as.tibble not working as expected..removed

data <- predictions %>% bind_cols(father = galton_heights$father)

head(data)
ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))



#---- 

set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)


female_heights %>% head()


female_heights %>% lm(mother~daughter,data=.) -> model_d
predictions_d <- predict(model_d, interval = c("confidence"), level = 0.95)

predictions_d <- predictions_d %>% bind_cols(mother = female_heights$mother)

head(predictions_d)

#------- AMking the team



library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_02 %>% head()

bat_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

x<-bat_01 %>% group_by(playerID) %>% summarize(mean_singles = mean(singles), mean_bb = mean(bb))

head(x)
x%>% filter(mean_singles>0.2) %>% nrow()
x%>% filter(mean_bb>0.2) %>% nrow()

bat_01 %>% group_by(playerID) %>% 
  summarize(mean_singles = mean(singles), mean_bb = mean(bb)) %>%
  inner_join(bat_02, by="playerID") -> joined

head(joined)
cor(joined$singles,joined$mean_singles)

cor(joined$bb,joined$mean_bb)

joined %>% ggplot(aes(mean_singles,singles))+
  geom_point(alpha=0.3)

joined %>% ggplot(aes(mean_bb,bb))+
  geom_point(alpha=0.3)

#Both the distributions seem to be oval shaped, an indication of them being bi-variate normal

#Fit a linear model to predict 2002 singles given 1999-2001 mean_singles

joined %>% lm(singles ~ mean_singles, data = .)

joined %>% lm(bb ~ mean_bb, data = .)



#--------------- Tibbles are modern data frames


#some functions don't know what to do with tibbles and cooerce them into dataframes
#the do() function returns a dataframe from a tibble and can be used to bridge the gap between 
#the tidyverse tibbles and dinosauR functions
install.packages("broom")
library(broom)
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}


dat %>% 
  group_by(HR) %>% 
  do(get_slope(.))




dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 


dat %>% head()

dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR") 

set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

#This is the best example of data wrangling with both, gather and unite




galton %>% filter(pair=="father_daughter") %>% nrow
galton %>% filter(pair=="mother_son") %>% nrow

galton %>% group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = T)) %>% 
  filter(term == "parentHeight")


cors <- galton %>% 
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight))

cors

#I'm lost now. Re studying to understand the lm function


fit <- lm(son ~ father, data = galton_heights)
fit


mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

galton_heights %>% 
  ggplot(aes(father, son)) + 
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = b_1, slope = m_1, col = "blue")

# fit is thus directly calculating m_1 and b_1

galton %>% group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = T)) %>% 
  filter(term == "parentHeight" & p.value < .05)%>%
  mutate(range = abs(conf.high - conf.low)) %>%
  select(pair,estimate, conf.low, conf.high) %>%
  ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()+
  coord_flip()+
  expand_limits(x = -0.5)


Teams %>% filter(yearID == 1971)  %>% lm(R~BB+HR, data=.) %>% tidy()

Teams %>% filter(yearID %in% 1961:2018)%>% 
  group_by(yearID) %>%
  do(tidy( lm(R~BB+HR, data=.),conf.int=TRUE)) %>%
  filter(term=="BB") %>%
  select(yearID,estimate)%>%
  ggplot(aes(yearID,estimate)) +
  geom_point()+
  geom_abline()

Teams %>% filter(yearID %in% 1961:2018)%>% 
  group_by(yearID) %>%
  do(tidy( lm(R~BB+HR, data=.),conf.int=TRUE)) %>%
  filter(term=="BB") %>%
  select(yearID,estimate)  %>%
  lm(estimate~yearID,data=.) %>%
  tidy()

library(tidyverse)
library(broom)
library(Lahman)


Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)


Teams_small %>%
  mutate(R_pg = R/G) %>%
  lm(avg_attendance ~ R_pg , data =.) %>%
  tidy()

Teams_small %>%
  mutate(HR_pg = HR/G) %>%
  lm(avg_attendance ~ HR_pg , data =.) %>%
  tidy()




Teams_small %>%
  lm(avg_attendance ~ W , data =.) %>%
  tidy()



Teams_small %>%
  lm(avg_attendance ~ yearID , data =.) %>%
  tidy()




  cor(Teams_small$W , Teams_small$R)

  cor(Teams_small$W , Teams_small$HR)
  
  
Teams_small %>%
  mutate(HR_pg = HR/G) %>%
  lm(HR_pg ~ W , data =.) %>%
  tidy()


Teams_small %>%
summarize(r = cor(R / G, W )) %>% pull(r)

Teams_small %>%
  summarize(r = cor(HR / G, W )) %>% pull(r)


Teams_small %>%
  mutate(Win_strata = round(W/10, 0)) %>%
  filter(Win_strata >= 5 & Win_strata <=10) %>%
  filter(Win_strata == 8) %>%
  nrow()

Teams_small %>%
  mutate(Win_strata = round(W/10, 0),R_pg = R/G) %>%
  filter(Win_strata >= 5 & Win_strata <=10) %>%
  group_by(Win_strata)%>%
  do(tidy(lm(avg_attendance~R_pg, data = .)))%>%
  filter(term == "R_pg") %>%
  arrange(-estimate)
  


Teams_small %>%
  mutate(Win_strata = round(W/10, 0),HR_pg = HR/G) %>%
  filter(Win_strata >= 5 & Win_strata <=10) %>%
  group_by(Win_strata)%>%
  do(tidy(lm(avg_attendance~HR_pg, data = .)))%>%
  filter(term == "HR_pg") %>%
  arrange(-estimate)


Teams_small %>%
  mutate(Win_strata = round(W/10, 0),HR_pg = HR/G,R_pg = R/G) %>%
  filter(Win_strata >= 5 & Win_strata <=10) %>%
  group_by(Win_strata)%>%
  do(tidy(lm(avg_attendance~HR_pg+W, data = .)))%>%
  arrange(-estimate)



Teams_small %>% 
  mutate(HR_pg = HR/G,R_pg = R/G) %>%
  lm(avg_attendance ~ R_pg + HR_pg + W + yearID, data = .) -> model



topredict<-data.frame(R_pg=5,HR_pg=1.2,W=80,yearID=1960)

predict(model, newdata = topredict)

Teams %>%
  filter(yearID==2002)%>%
  mutate(HR_pg = HR/G,R_pg = R/G) %>%
  mutate(Att_hat = predict(model, newdata = .)) %>%
  summarise(correlation = cor(Att_hat,attendance)) %>% 
  pull(correlation)


Teams %>%
  filter(yearID==2002)%>%
  mutate(HR_pg = HR/G,R_pg = R/G) %>%
  mutate(Att_hat = predict(model, newdata = .)) ->temp




####Correlation is not Causation: Confounding


N <- 25
g <- 1000000
sim_data <- tibble(group = rep(1:g, each=N), 
                   x = rnorm(N * g), 
                   y = rnorm(N * g))


head(sim_data)


library(dslabs)


?admissions


data("research_funding_rates")
research_funding_rates

research_funding_rates %>% 
  select(awards_men,awards_women,applications_men,applications_women) %>%
  gather(key, value) %>%
  separate(key,c("Status","Sex")) %>%
  group_by(Status,Sex) %>%
  summarise(n=sum(value)) %>%
  spread(Status,n) %>%
  mutate(not_awarded = applications - awards,
         perc_awarded = (1 - (not_awarded/applications))*100 ,
         perc_not_awarded = 100 - perc_awarded) 
 




#This didn't work as my table is not no award vs award.

#It is application and award
research_funding_rates %>% 
  select(awards_men,awards_women,applications_men,applications_women) %>%
  gather(key, value) %>%
  separate(key,c("Status","Sex")) %>%
  group_by(Status,Sex) %>%
  summarise(n=sum(value)) %>%
  spread(Status,n) %>%
  select(-Sex) %>%
  chisq.test() %>%
  tidy()

#This was painful and also useless. The chi-sq test gives the same result 
#regardless of transpose

research_funding_rates %>% 
  select(awards_men,awards_women,applications_men,applications_women) %>%
  gather(key, value) %>%
  separate(key,c("Status","Sex")) %>%
  group_by(Status,Sex) %>%
  summarise(n=sum(value)) %>%
  spread(Status,n) %>%
  .[,-1] %>%
  t()%>%
  `colnames<-`(c("men", "women"))


#this was useless

research_funding_rates %>% 
  select(awards_men,awards_women,applications_men,applications_women) %>%
  gather(key, value) %>%
  separate(key,c("Status","Sex")) %>%
  group_by(Status,Sex) %>%
  summarise(n=sum(value)) %>%
  spread(Status,n) %>%
  summarise(rate = sum(awards)/sum(applications)) %>%
  pull(rate) ->rate

tbt_1


tbt_1 %>% mutate(no = applications - awards) %>%
  select(-applications,-Sex) %>%
  chisq.test()


#The correct answer then is :
research_funding_rates %>% 
  select(awards_men,awards_women,applications_men,applications_women) %>%
  gather(key, value) %>%
  separate(key,c("Status","Sex")) %>%
  group_by(Status,Sex) %>%
  summarise(n=sum(value)) %>%
  spread(Status,n) %>%
  mutate(no = applications - awards) %>%
  select(-applications,-Sex) %>%
  chisq.test()


  


dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat

dat %>%
  ggplot(aes(success,discipline,colour=gender,size = applications))+
  geom_point()
  


research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  ggplot(aes(success,discipline,colour=gender,size = applications))+
  geom_point()
 


#Selecting a team by linear programming


library(reshape2)
library(lpSolve)

players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 

head(players)
