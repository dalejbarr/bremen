dat <- readRDS("FAN.rds")
dat1 <- subset(dat, Day == 2)

dat1 <- transform(dat1,
                  V1 = (Cond == "same voice") -
                      mean(Cond == "same voice"),
                  V2 = (Cond == "same gender, different voice") -
                      mean(Cond == "same gender, different voice"))

library("lme4")

mod <- glmer(Accuracy ~ V1 + V2 +
                 (V1 + V2 | SessionID) +
                 (V1 + V2 | ItemID),
             data = dat1, family = binomial(link = logit),
             control = glmerControl(optimizer = "bobyqa"))

summary(mod)

mod2 <- update(mod, . ~ . - V1 - V2)

anova(mod, mod2)

pmean <- aggregate(Accuracy ~ Cond, dat, mean)

int <- fixef(mod)
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
