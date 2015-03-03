fan <- readRDS("FAN.rds")

head(fan)

# have a look at the design
xtabs(~Day + Cond + SessionID + ItemID, fan)
xtabs(~Day + Cond + SessionID, fan)
xtabs(~Day + Cond + ItemID, fan)

par(mfrow = c(1, 3))
with(fan, hist(RT))
with(fan, hist(log(RT)))

cutoff <- with(fan, quantile(RT, .975))
fan$RTt <- with(fan, ifelse(RT > cutoff, cutoff, RT))

with(fan,
     interaction.plot(factor(Day), factor(Cond), RTt))

fan$T <- with(fan, Day - mean(Day))

fan$V1 <- with(fan, 
               (Cond == "same voice") - mean(Cond == "same voice"))

fan$V2 <- with(fan,
               (Cond == "same gender, different voice") -
                   mean(Cond == "same gender, different voice"))

library("lme4")

## doesn't converge
mod <- lmer(RTt ~ T * (V1 + V2) +
                (T * (V1 + V2) | SessionID) +
                (T * (V1 + V2) | ItemID) +
                (1 | SessionID:ItemID),
            fan, REML = FALSE)

## increase iterations from 10000 (default) to 20000
mod_ii <- lmer(RTt ~ T * (V1 + V2) +
                (T * (V1 + V2) | SessionID) +
                (T * (V1 + V2) | ItemID) +
                (1 | SessionID:ItemID),
               fan, REML = FALSE,
               control = lmerControl(optCtrl = list(maxfun = 20000)))

## fit a "diagonal" model
## (constraint covariances to zero)
mod_diag <- lmer(RTt ~ T * (V1 + V2) +
                     (T * (V1 + V2) || SessionID) +
                     (T * (V1 + V2) || ItemID) +
                     (1 | SessionID:ItemID),
            fan, REML = FALSE)

summary(mod_diag)

mod_diag2 <- lmer(RTt ~ T * (V1 + V2) +
                      (1 | SessionID) +
                      (0 + T | SessionID) +
                      (0 + T:V1 | SessionID) +
                      (1 | ItemID) +
                      (0 + V1 | ItemID) +
                      (0 + V2 | ItemID) +
                      (1 | SessionID:ItemID),
            fan, REML = FALSE)

## test 2x3 interaction
mod_diag2_no_ix <- update(mod_diag2, . ~ . - T:V1 - T:V2)

## test main effect of voice
mod_diag2_no_voice <- update(mod_diag2, . ~ . - V1 - V2)

## test main effect of day
mod_diag2_no_day <- update(mod_diag2, . ~ . -T)

anova(mod_diag2, mod_diag2_no_ix)
anova(mod_diag2, mod_diag2_no_voice)
anova(mod_diag2, mod_diag2_no_day)
