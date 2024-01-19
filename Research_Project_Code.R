library(zoo)
library(readxl)
library(dynlm)
library(lmtest)
library(stargazer)
library(urca)
library(stats)
library(strucchange)
library(ARDL)
library(tidyr)
library(ggplot2)
library(dplyr)

data <- read_excel("/Users/wikst/Desktop/Y3/Research Project/ALL_DATA.xlsx",
                   sheet = "All", range = ("J1:N29"))

colnames(data) [2] <- "ep"
colnames(data) [3] <- "cpi"
colnames(data) [4] <- "re"
colnames(data) [5] <- "gdpc"

#plot grid
plotting_data <- data[c("Year", "ep", "re", "cpi", "gdpc")]

pd_long <- plotting_data %>%
  pivot_longer(cols = -Year, names_to = "variable", values_to = "value")

custom_labels <- c(
  re   = "Share of Renewable Energyy\n(% of Total Electricity Production)",
  ep   = "Electricity Prices\n(€/kWh)",
  cpi  = "Consumer Price Index\n(% Change)",
  gdpc = "GDP per Capita\n(Current USD Thousands)"
)

pd_long <- pd_long %>%
  mutate(
    title = case_when(
      variable == "re"   ~ "Share of Electricity Production from Renewables",
      variable == "ep"   ~ "Electricity Consumer Prices",
      variable == "cpi"  ~ "Consumer Price Index (Inflation)",
      variable == "gdpc" ~ "GDP per Capita",
      TRUE ~ variable
    ),
    y_label = case_when(
      variable == "re"   ~ "% of total Electricity Production",
      variable == "ep"   ~ "€/kWh",
      variable == "cpi"  ~ "% Change",
      variable == "gdpc" ~ "Current USD",
      TRUE ~ ""
    )
  )

p <- ggplot(pd_long, aes(x = Year, y = value)) +
  geom_line() +
  facet_wrap(~variable, ncol = 2, scales = "free_y", labeller = labeller(variable = custom_labels)) +
  theme_minimal() +
  labs(x = "Year", y = "") +
  theme(strip.text.x = element_text(face = "bold"))

print(p)


data$t <- c(1:28)
tsdata <- ts(data)


# Stationarity
adf_ep <- ur.df(data$ep, type="none")
summary(adf_ep)
kpss1 <- ur.kpss(data$ep)
summary(kpss1)

adf_cpi <- ur.df(data$cpi, type="none")
summary(adf_cpi)
kpss2 <- ur.kpss(data$cpi)
summary(kpss2)

adf_re <- ur.df(data$re, type="none")
summary(adf_re)
kpss3 <- ur.kpss(data$re)
summary(kpss3)

adf_gdpc <- ur.df(data$gdpc, type="none")
summary(adf_gdpc)
kpss4 <- ur.kpss(data$gdpc)
summary(kpss4)

#Addressing stationarity
data$epdiff <- c(NA,diff(data$ep))
adf_epdiff <- ur.df(na.omit(data$epdiff), type="none")
summary(adf_epdiff)
plot(data$epdiff)
kpss1.2 <- ur.kpss(data$epdiff)
summary(kpss1.2)

data$cpidiff <- c(NA,diff(data$cpi))
adf_cpidiff <- ur.df(na.omit(data$cpidiff), type="none")
summary(adf_cpidiff)
plot(data$cpidiff)
kpss2.2 <- ur.kpss(data$cpidiff)
summary(kpss2.2)

data$rediff <- c(NA,diff(data$re))
adf_rediff <- ur.df(na.omit(data$rediff), type="none")
summary(adf_rediff)
plot(data$rediff)
kpss3.2 <- ur.kpss(data$rediff)
summary(kpss3.2)

data$gdpcdiff <- c(NA,diff(data$gdpc))
adf_gdpcdiff <- ur.df(na.omit(data$gdpcdiff), type="none")
summary(adf_gdpcdiff)
plot(data$gdpcdiff)
kpss4.2 <- ur.kpss(data$gdpcdiff)
summary(kpss4.2)


#Trends
model_ep <- dynlm(ep~t, data=data)
e_quad <- lm(ep ~ t + I(t^2), data=data)
e_cubic <- lm(ep ~ t + I(t^2) + I(t^3), data=data)
summary(model_ep)
summary(e_quad)
summary(e_cubic)

model_cpi <- dynlm(cpi~t, data=data)
c_quad <- lm(cpi ~ t + I(t^2), data=data)
c_cubic <- lm(cpi ~ t + I(t^2) + I(t^3), data=data)
summary(model_cpi)
summary(c_quad)
summary(c_cubic)

model_re <- dynlm(re~t, data=data)
re_quad <- lm(re ~ t + I(t^2), data=data)
re_cubic <- lm(re ~ t + I(t^2) + I(t^3), data=data)
summary(model_re)
summary(re_quad)
summary(re_cubic)

model_gdpc <- dynlm(gdpc~t, data=data)
gdp_quad <- lm(gdpc ~ t + I(t^2), data=data)
gdp_cubic <- lm(gdpc ~ t + I(t^2) + I(t^3), data=data)
summary(model_gdpc)
summary(gdp_quad)
summary(gdp_cubic)

text_model_all <- stargazer(model_ep, model_cpi, model_re, model_gdpc, type = "text")


#Contemporaneous (only tests)
testmodel <- dynlm(ep ~ re + cpi + gdpc, data=data)
testmodel_star <- stargazer(testmodel, type = "text")
testmodel2 <- dynlm(epdiff ~ rediff + cpidiff + gdpcdiff, data=data)
testmodel2_star <- stargazer(testmodel2, type = "text")
testmodel3 <- dynlm(ep ~ re, data=data)
testmodel3_star <- stargazer(testmodel3, type = "text")


#Structural Breaks
eppoints <- breakpoints(data$ep ~ 1)
summary(eppoints)
eppoints$breakpoints
#suggests 3: 8(2002), 13(2007), 24(2018)
plot(eppoints)

cpipoints <- breakpoints(data$cpi ~ 1)
summary(cpipoints)
cpipoints$breakpoints
#suggest 0

repoints <- breakpoints(data$re ~ 1)
summary(repoints)
repoints$breakpoints
#suggests 2: 17(2011), 22(2016)

gdpcpoints <- breakpoints(data$gdpc ~ 1)
summary(gdpcpoints)
gdpcpoints$breakpoints
#suggests 2: 8(2002), 12(2006)

#dummies
data$d2002 <- ifelse(data$t >= 8 & data$t < 13, 1, 0)
data$d2007 <- ifelse(data$t >= 13 & data$t < 24, 1, 0)
data$d2018 <- ifelse(data$t >= 24, 1, 0)

data$d2022 <- ifelse(data$t >= 28, 1, 0)


#Lag selection
ardl1 <- auto_ardl(ep ~ re + cpi + gdpc, selection="BIC", data = data, max_order = 3)
ardl1
ardl2 <- auto_ardl(epdiff ~ rediff + cpidiff + gdpcdiff, selection="BIC", data = data, max_order = 3)
ardl2


#Lagged models
model1 <- ardl(ep ~ re + cpi + gdpc, data=data, order=c(1,1,0,0))
summary(model1)
dwtest(model1)

##### The official model #####
model2 <- ardl(epdiff ~ rediff + cpidiff + gdpcdiff, data=data, order=c(1,1,0,0))
summary(model2)
dwtest(model2)
##### ##### #####

#testing
model7 <- dynlm(ep ~ re + cpi + gdpc + d2002 + d2007 +d2018, data=data)
textmodel7 <- stargazer(model7, type = "text")

model8 <- dynlm(epdiff ~ rediff + cpidiff + gdpcdiff + d2002 + d2007 + d2018, data=data)
textmodel8 <- stargazer(model8, type = "text")
dwtest(model8)

model9 <- ardl(epdiff ~ rediff + cpidiff + gdpcdiff + d2002 + d2007 + d2018, 
               data=data, order=c(1,1,0,0,0,0,0))
summary(model9)
dwtest(model9)

model10 <- dynlm(ep ~ L(ep) + re + L(re) + cpi + gdpc + d2002+d2007+d2018, data=tsdata)
textmodel10 <- stargazer(model10, type = "text")
dwtest(model10)

model11 <- dynlm(ep ~ L(ep) + re + L(re) + cpi + gdpc + d2022, data=tsdata)
textmodel11 <- stargazer(model11, type = "text")
dwtest(model11)


#for table 1
model2_diff <- dynlm(epdiff ~ L(epdiff) + rediff + L(rediff) + cpidiff + gdpcdiff, data=tsdata)
textmodel2.2 <- stargazer(model2_diff, type = "text", digits = 5)
dwtest(model2_diff)

epresiduals <- residuals(model_ep)
data$epres <- epresiduals
residadf <- ur.df(epresiduals)
summary(residadf)

model2_res <- dynlm(epres ~ L(epres) + rediff + L(rediff) + cpidiff + gdpcdiff, data=tsdata)
textmodel2_res <- stargazer(model2_res, type = "text", digits = 5)
dwtest(model2_res)

model2_dt <- dynlm(ep ~ L(ep) + rediff + L(rediff) + cpidiff + gdpcdiff + t, data=tsdata)
textmodel2_dt <- stargazer(model2_dt, type = "text", digits = 5)
dwtest(model2_dt)

textmodel2_all <- stargazer(model2_diff, model2_res, model2_dt, type = "text", digits = 5)