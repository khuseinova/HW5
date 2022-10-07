#HW5
#Kseniia Huseinova
#10/06/2022


#Minghao Deng, Joe Holland


attach(acs2017_ny)
use_varb <- (AGE >= 25) & (AGE <= 55) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35)
dat_use <- subset(acs2017_ny,use_varb) # 
detach()
attach(dat_use)

model_temp1 <- lm(INCWAGE ~ AGE + female + AfAm + Asian + Amindian + race_oth + Hispanic + educ_hs + educ_somecoll + educ_college + educ_advdeg)
summary(model_temp1)
plot(model_temp1)
require(stargazer)
stargazer(model_temp1, type = "text")

install.packages("stargazer")
require(stargazer)
stargazer(model_temp1, type = "text")

install.packages("AER")
library(AER)
require(AER)

NNobs <- length(INCWAGE)
set.seed(12345)
graph_obs <- (runif(NNobs) < 0.1) 
data_graph <-subset(data_use,graph_obs)  

plot(INCWAGE ~ jitter(AGE, factor = 15), pch = 20, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = data_graph)
plot(INCWAGE ~ jitter(AGE, factor = 6), pch = 20, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = data_graph)

install.packages("yhat")
require(yhat)

to_be_predicted2 <- data.frame(AGE = 25:55, veteran = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 0)
to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)

detach()

#We tried to do our first lm regression by incorporate pretty much everything first. There are few larger categories being considered here: \
#1. Commute method: a base case is commute_other, and estimates are compared with commute_other (for example, estimates=5000 would mean people using this commute method will likely have $5000 more in terms of INCWAGE, this will be the same for all other categories with a base case) \
#2. Race where Hispanic origin is considered base case. \
#3. Residential area where Staten Island is considered base case. \
#4. Sex, where male = 0. \
#5. FAMSIZE and number of child. \
#6. Rent and OWNCOST. \
#7. educational status, where no high school degree is considered base case. \


huge_lm <- lm(INCWAGE ~ AGE + Commute_bus + Commute_car + Commute_rail + Commute_subway + has_AnyHealthIns + white + AfAm + Amindian + Asian + race_oth + in_Nassau + in_Westchester + in_Queens + in_Bronx + in_Brooklyn + in_Manhattan + female + NCHILD + FAMSIZE + RENT + OWNCOST + educ_hs + educ_somecoll + educ_college + educ_advdeg)
summary(huge_lm)

#Most notably here are races, where all of them have very high standard errors with estimates sometimes smaller than standard errors. For all of them, we will always assume $H_0 = 0, H_1 \ne 0$. As given in the linear regression above, for White, we have p-value = 0.57, for AfAM, we have p-value = 0.1, for Amindian, we have p-value = 0.77, for Asian, we have p-value = 0.15, for race_oth, we have p-value = 0.06, all of which are higher than a/2, which is 0.025. Therefore, we failed to reject null hypothesis for all of these values. \
#It's notably interesting, because racial inequalities are often talked about in a lot of ways, but as shown here, there's no significance for a 95% confidence intervals. In fact, our smallest one is race_oth and there's no significance for a 90% confidence interval. For AfAm, which is often under represented, p-value = 0.1, which means we have significance only at an 80% confidence interval, which is hard to justify statistically speaking. \
#With this being said, it's a good idea to exclude those from the linear regression.


huge_lm2 <- lm(INCWAGE ~ AGE + Commute_bus + Commute_car + Commute_rail + Commute_subway + has_AnyHealthIns + in_Nassau + in_Westchester + in_Queens + in_Bronx + in_Brooklyn + in_Manhattan + female + NCHILD + FAMSIZE + RENT + OWNCOST + educ_hs + educ_somecoll + educ_college + educ_advdeg)
summary(huge_lm2)

#The next thing we are going to look at is in_Bronx. We are going to use the same hypothesis testing as before where $H_0 = 0, H_1 \ne 0$. It's worth nothing that if we are doing a one-tail testing with 95% confidence interval, in_Bronx would be significant, and other residential areas appear to affect the INCWAGE significantly. However, if we take one residential areas out, I should take the rest out for consistency. \
#There are also certain overlaps between residential areas and commute methods as well as residential areas and rent/owncost that is causing the significance. For example, Manhattan simply has the highest ownership cost out of all areas, and that naturally makes people living there making more, because they are able to sustain the level of rent. It also makes sense for people living in Manhattan to not own a car because of the maintenance cost and how useless a car is in Manhattan. We can take those out now and see what's left. \


huge_lm3 <- lm(INCWAGE ~ AGE + Commute_bus + Commute_car + Commute_rail + Commute_subway + has_AnyHealthIns + female + NCHILD + FAMSIZE + RENT + OWNCOST + educ_hs + educ_somecoll + educ_college + educ_advdeg)
summary(huge_lm3)


#As shown above, everything now is very significant (p-value is <2e-16). This concludes our final linear regression. There are few important things, which we noticed: \
#1. Being a female would result in less wages. This might be explained in one of two ways: first, females are unequally treated; second females often give birth to child at their best ages, which affect their abilities to climb up the ladder. While both could be true, politicians certainly believes solely on the first cause. \
#2. College degree is extremely important and results in 20K difference in wages from someone without high school degree and almost 15K from someone with a high school degree. Advanced degree, however, doesn't seem to be affecting the wages as much as college degree. \
#3. While rent and ownership cost do matter, the effect is so small that they are actually not correlating with wages in any significant way. This is surprising to find out. \
#4. Family size contributes negatively to the wages, but number of child contributes positively to the wages. It's understandable that when the family size is bigger, there are more things to take care of and therefore less time to make money. Positive contribution from number of children could be explained in one of two ways: first, only people who can afford to raise more than one child would give birth to new children; second, increasing number of children makes people work... harder?