library("lme4")

dat <- readRDS("urefs.rds")

aggregate(WC ~ Novelty + Addressee + Feedback,
          dat, mean)

aggregate(WC ~ Novelty + Addressee, dat, mean)

hist(dat$WC)
hist(log(dat$WC))

dat2 <- transform(dat, N = ifelse(Novelty == "New", 1, 0),
                          A = ifelse(Addressee == "New", 1, 0),
                          F = ifelse(Feedback == "Yes", 1, 0))
dat2c <- transform(dat2,
                         Nc = N - mean(N),
                         Ac = A - mean(A),
                         Fc = F - mean(F))                          

mod <- glmer(WC ~ Ac * Nc * Fc +
               (1 + Nc * Fc | SessionID) +
               (1 + Ac * Nc | ItemID),
             dat2c, poisson,
             control = glmerControl(optimizer = "bobyqa"))

mod3 <- glmer(WC ~ Ac * Nc * Fc +
               (1 + Nc * Fc | SessionID) +
               (1 + Ac * Nc | ItemID),
             dat2c, poisson,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 20000)))


mod2 <- glmer(WC ~ Ac * Nc * Fc +
                (1 + Nc * Fc | SessionID) +
                (1 + Ac + Ac:Nc | ItemID),
              dat2c, poisson,
              control = glmerControl(optimizer = "bobyqa"))

mod_diag <- glmer(WC ~ Ac * Nc * Fc +
               (1 + Nc * Fc || SessionID) +
               (1 + Ac * Nc || ItemID),
             dat2c, poisson)

mod_diag_noA <- update(mod_diag, . ~ . -Ac)
mod_diag_noN <- update(mod_diag, . ~ . -Nc)
mod_diag_noF <- update(mod_diag, . ~ . -Fc)
mod_diag_noAN <- update(mod_diag, . ~ . -Ac:Nc)
mod_diag_noAF <- update(mod_diag, . ~ . -Ac:Fc)
mod_diag_noNF <- update(mod_diag, . ~ . -Nc:Fc)
mod_diag_noANF <- update(mod_diag, . ~ . -Ac:Nc:Fc)

anova(mod_diag, mod_diag_noA)
anova(mod_diag, mod_diag_noN)
anova(mod_diag, mod_diag_noF)
anova(mod_diag, mod_diag_noAN)
anova(mod_diag, mod_diag_noAF)
anova(mod_diag, mod_diag_noNF)
anova(mod_diag, mod_diag_noANF)
