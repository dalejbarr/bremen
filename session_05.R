dat <- readRDS("FAN.rds")
dat1 <- subset(dat, Day == 2)

dat1 <- transform(dat1,
                  V1 = (Cond == "same voice") -
                      mean(Cond == "same voice"),
                  V2 = (Cond == "same gender, different voice") -
                      mean(Cond == "same gender, different voice"))

dat <- transform(dat,
                  V1 = (Cond == "same voice") -
                    mean(Cond == "same voice"),
                  V2 = (Cond == "same gender, different voice") -
                    mean(Cond == "same gender, different voice"),
                 Day = Day - mean(Day))

library("lme4")
mod_full <- glmer(Accuracy ~ Day * (V1 + V2) +
                    (Day * (V1 + V2) | SessionID) +
                    (Day * (V1 + V2) | ItemID) +
                    (1 | SessionID:ItemID),
                  dat, binomial,
                  control = glmerControl(optimizer = "bobyqa"))

mod_full_nrc <- glmer(Accuracy ~ Day * (V1 + V2) +
                    (Day * (V1 + V2) || SessionID) +
                    (Day * (V1 + V2) || ItemID) +
                    (1 | SessionID:ItemID),
                  dat, binomial,
                  control = glmerControl(optimizer = "bobyqa"))


mod_test <- glmer(Accuracy ~ V1 + V2 +
                 (V1 + V2 | SessionID) +
                 (V1 + V2 | ItemID) +
               (1 | SessionID:ItemID),
             data = dat1, family = binomial(link = logit),
             control = glmerControl(optimizer = "bobyqa"))

summary(mod)

mod2 <- update(mod, . ~ . - V1 - V2)

anova(mod, mod2)

pmean <- aggregate(Accuracy ~ Cond, dat, mean)

params <- fixef(mod)[-1]

mx <- matrix(c(-1/3,  2/3, -1/3,
               -1/3, -1/3,  2/3), ncol = 2)

df1 <- data.frame(Cond = c("different gender, different voice",
                   "same voice",
                   "same gender, different voice"),
           logit = as.numeric(mx %*% params + fixef(mod)[1]))
df1$pmod = 1 / (1 + exp(-df1$logit))

merge(df1, pmean)

c(params["V1"], OR = exp(params["V1"]))

