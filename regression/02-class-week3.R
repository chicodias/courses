library(DAAG)
data(allbacks)


book_mlr = lm(weight ~ volume + cover, data= allbacks)
summary(book_mlr)

## video 2 - adjusted R square

states <- read.csv("http://d396qusza40orc.cloudfront.net/statistics/lec_resources/states.csv")

pov_slr <- lm(poverty ~ female_house, data = states)
summary(pov_slr)

# adicionando uma variavel ao modelo
pov_mlr <- lm(poverty ~ female_house + white, data = states)
summary(pov_mlr)
anova(pov_mlr)

# inference with MLR

cognitive <- read.csv("http://bit.ly/dasi_cognitive")


# full model
cog_full <- lm(kid_score ~ mom_hs + mom_iq + mom_work + mom_age, data = cognitive)
summary(cog_full)

# calculando p-valor
pt(2.201, df = 429, lower.tail = F) *2

# calculando intervalo de confianca t*
qt(0.025, df = 429)

# Inter = obs +- t*slope 

# final model
cog_final <- lm(kid_score ~ mom_hs + mom_iq + mom_work, data = cognitive)
summary(cog_final)

# model diagnostics

plot(cog_final$residuals ~ cognitive$mom_iq)

# distribuido aleatoriamente em torno do 0
hist(cog_final$residuals)
qqnorm(cog_final$residuals)
qqline(cog_final$residuals)

# variabilidade constante dos residuos
plot(cog_final$residuals ~ cog_final$fitted.values)

plot(abs(cog_final$residuals) ~ cog_final$fitted.values)

# independent residuals
plot(cog_final$residuals)

