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
