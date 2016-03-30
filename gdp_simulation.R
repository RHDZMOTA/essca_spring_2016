
# level of disarmament % - runif
# ratio

# 1 Determine GDP
# 2 Determine level of military exp.
# 3 Calculate militart exp.
# 4 Determine level of disarmament
# 5 Calculate extra resources
# 6 Determine transfer level
# 7 Calculate transfer
# 8 Determine saving level
# 9 Calculate saving (domestic inv.)
# 10 Calculate consumption

library(tibble)
library(ggplot2)
library(tidyr)

GDP <- 1205
military_level <- 0.1 #jitter
military_expenditure1 <- GDP * military_level
disarmament_level <- runif(1)
extra_resources <- military_expenditure1 * disarmament_level
military_expenditure2 <- military_expenditure1 - extra_resources
transfer_level <- runif(1)
transfer_resources <- transfer_level * extra_resources
saving_level <- runif(1)
saving_resources <- saving_level * (GDP - military_expenditure1)
saving_resources_extra <- saving_level * (1 - transfer_level) * extra_resources
consumption_resources <-  (1 - saving_level) * (GDP - military_expenditure1)
consumption_resources_extra <- extra_resources - saving_resources_extra - transfer_resources

GDP == military_expenditure1 + saving_resources + consumption_resources
GDP == military_expenditure2 + transfer_resources + saving_resources + saving_resources_extra +
  consumption_resources + consumption_resources_extra

n <- 1000
gdp_seed <- 1200
gdp_seed2 <- 195
GDP <- rep(gdp_seed, n) + floor(10 * runif(n, -1, 1))
GDP2 <- rep(gdp_seed2, n) + floor(5 * runif(n, -1, 1))
military_level <- runif(n, 0, .30)
military_level2 <- runif(n, 0, .30)
military_expenditure1 <- GDP * military_level
military_expenditure12 <- GDP2 * military_level2
disarmament_level <- runif(n)
disarmament_level2 <- runif(n)
extra_resources <- military_expenditure1 * disarmament_level
extra_resources2 <- military_expenditure12 * disarmament_level2
military_expenditure2 <- military_expenditure1 - extra_resources
military_expenditure22 <- military_expenditure12 - extra_resources2
transfer_level <- runif(n)
transfer_resources <- transfer_level * extra_resources
foreign_investment <- transfer_resources
saving_level <- runif(n)
saving_level2 <- runif(n, 0, 0.15)
saving_resources <- saving_level * (GDP - military_expenditure1)
saving_resources2 <- saving_level2 * (GDP2 - military_expenditure12)
saving_resources_extra <- saving_level * (1 - transfer_level) * extra_resources
saving_resources_extra2 <- saving_level2 * extra_resources2
consumption_resources <-  (1 - saving_level) * (GDP - military_expenditure1)
consumption_resources2 <-  (1 - saving_level2) * (GDP2 - military_expenditure12)
consumption_resources_extra <- extra_resources - saving_resources_extra - transfer_resources
consumption_resources_extra2 <- extra_resources2 - saving_resources_extra2
total_productive_investment2 <- saving_resources2 + saving_resources_extra2 + foreign_investment

gr <- rep(5, n) + floor(10 * runif(n, 0, 1))
gr2 <- rep(2, n) + floor(1 * runif(n, 0, 1))
t <- rep(10, n)

if(n == 1){
  GDP == military_expenditure1 + saving_resources + consumption_resources
  GDP == military_expenditure2 + transfer_resources + saving_resources + saving_resources_extra +
    consumption_resources + consumption_resources_extra
  
  GDP2 == military_expenditure12 + saving_resources2 + consumption_resources2
  GDP2 == military_expenditure22 + saving_resources2 + saving_resources_extra2 +
    consumption_resources2 + consumption_resources_extra2
}
capital_output <- function(X) 100 * X[1] / (X[2] * X[3]) # I, GNP, AG <- actual growth
saving_rate    <- function(X) X[1]/X[2] # I, GNP
growth_rate1   <- function(X) X[1] * exp(X[4] * X[2] / X[3]) # y0, i, b, t
growth_rate2   <- function(X){
  # H0, i1, i2, b1, b2, y1, y2, t
  H0 <- X[1]; i1 <- X[2]; i2 <- X[3];
  b1 <- X[4]; b2 <- X[5]; y1 <- X[6]; y2 <- X[7];
  t <- X[8]
  lambd1 <- i1 / b1
  lambd2 <- i2 / b2
  gr <- H0 * exp(lambd1 * t) / (b2*(lambd1 - lambd2)) +
    (y2 - H0 / (b2 * (lambd1 - lambd2))) * exp(lambd2 * t)
  return(gr)
}
lgrowth_rate2  <- function(H0, i1, i2, b1, b2, y1, y2, t){
  lambd1 <- i1 / b1
  lambd2 <- i2 / b2
  gr <- - H0 * exp(lambd1 * t) / (b2*(lambd1 - lambd2)) +
    (y2 + H0 / (b2 * (lambd1 - lambd2))) * exp(lambd2 * t)
  return(gr)
}
growth_rate    <- function(x) ((x[2] / x[3]) ^ (1/x[1]) - 1) * 100 # t, future, present
avg_disarm <- apply(cbind(disarmament_level, disarmament_level2), 1, mean)
i1 <- apply(cbind(saving_resources+saving_resources_extra, GDP), 1, saving_rate)
i2 <- apply(cbind(saving_resources2 + saving_resources_extra2, GDP2), 1, saving_rate)
b1 <- apply(cbind(saving_resources+saving_resources_extra, GDP, gr), 1, capital_output)
b2 <- apply(cbind(saving_resources2 + saving_resources_extra2, GDP2, gr2), 1, capital_output)
gdp_g <- apply(cbind(GDP, i1, b1, t), 1, growth_rate1)
gdp2_g <- apply(cbind(foreign_investment, i1, i2, b1, b2, GDP, GDP2, t), 1, growth_rate2)
gr_1 <- apply(cbind(t,gdp_g, GDP), 1, growth_rate)
gr_2 <- apply(cbind(t,gdp2_g, GDP2), 1, growth_rate)
# disarmament

dataset <- data_frame(Average_level_disarmament = avg_disarm,
                      Transfer_level = transfer_level,
                      Transfer = foreign_investment,
                      Developed = gr_1,
                      Underdeveloped = gr_2)

dataset <- gather(dataset, key = Area, value = Growth, Developed, Underdeveloped)
big_transfers <- mean(dataset$Transfer) + sd(dataset$Transfer)
small_transfers <- mean(dataset$Transfer) - 2*sd(dataset$Transfer)
# [dataset$Transfer > big_transfers,]

ggplot(dataset, aes(y = Growth, x = Average_level_disarmament)) + 
  geom_point(aes(size = Transfer)) +
  geom_smooth(data = dataset,
              aes(y = Growth, x = Average_level_disarmament, color = Area),
              method = "lm", se = F) + 
  theme_light()

ggplot(dataset, aes(y = Growth, x = Average_level_disarmament)) + 
  geom_point(aes(size = Transfer)) + theme_light() +
  ggtitle("Effects of the level of disarmament and transfer on economic growth") +
  xlab("Disarmament level") + ylab("Growth (%)")

ggplot(dataset, aes(y = Growth, x = Average_level_disarmament)) + 
  geom_point(aes(size = Transfer)) + theme_light() +
  geom_smooth(data = dataset,
              aes(y = Growth, x = Average_level_disarmament, color = Area),
              method = "lm", se = F) +
  ggtitle("Effects of the level of disarmament and transfer on economic growth") +
  xlab("Disarmament level") + ylab("Growth (%)")
  
  ggplot(dataset, aes(y = Growth, x = Transfer)) + 
    geom_point(aes(color = Area), alpha = 0.5) + theme_light() +
    geom_smooth(data = dataset,
                aes(y = Growth, x = Transfer, color = Area),
                method = "lm", se = F) +
    scale_color_manual(name="",values = c("black","dark red")) + 
    ggtitle("Effects of the transfer's magnitude on the economic growth") +
    xlab("Transfer ($ bil. usd)") + ylab("Growth (%)")
  
  ggplot(dataset[dataset$Area == "Developed",
                 c("Growth", "Transfer")], aes(y = Growth, x = Transfer)) +
    geom_point(alpha = 0.5) +
    xlim(0, 100) + ylim(0, 80) +
    ggtitle("Group 1: transfer's magnitude and growth") +
    theme_light()
  
