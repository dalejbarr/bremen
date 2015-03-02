dat <- readRDS("kbbb.rds")
# calculate latencies
dat$tfix <- 1000*((dat$targfix - dat$crit) / 60)

hist(dat$tfix)

# truncate outliers at 97.5th percentile of distribution
cutoff.tfix <- quantile(dat$tfix, probs=.975, na.rm=TRUE)
dat$tfixTrunc <- ifelse(dat$tfix>cutoff.tfix, cutoff.tfix, dat$tfix)

hist(dat$tfixTrunc)

# aggregate
aggregate(tfixTrunc ~ cond, dat, mean)

# re-create data: t-test
dat.subj <- aggregate(tfixTrunc ~ SubjID + cond, dat, mean)
dat.subj <- dat.subj[order(dat.subj$SubjID, dat.subj$cond), ]

dat.t <- t.test(subset(dat.subj, cond=="C")$tfixTrunc,
                subset(dat.subj, cond=="E")$tfixTrunc, paired=TRUE)
print(dat.t)

# linear mixed model
# create deviation-coded predictor
dat$D <- dat$cond == "E"
dat$C2 <- dat$D - mean(dat$D)

library("lme4")
mod1 <- lmer(tfixTrunc ~ C2 +
                 (1 + C2 | SubjID) + 
                 (1 + C2 | object),
             data=dat,
             subset = complete.cases(dat),
             REML=FALSE)

print(summary(mod1), correlation = FALSE)

paramest <- fixef(mod1)
stderrs <- sqrt(diag(vcov(mod1)))
tstats <- paramest / stderrs
pvals <- 2 * (1 - pnorm(abs(tstats)))

data.frame(b = paramest, se = stderrs, t = tstats, p = pvals)

mod2 <- update(mod1, . ~ . -C2)

anova(mod1, mod2)

chi2 <- deviance(mod2) - deviance(mod1)
pchi <- pchisq(chi2, 1, lower.tail = FALSE)

c(chisq = chi2, p = pchi)

blups <- ranef(mod1)
blups$SubjID$C2 + fixef(mod1)[2] # every subject shows effect
blups$object$C2 + fixef(mod1)[2] # every item shows effect

library("pbkrtest")

mod_kr <- KRmodcomp(mod1, mod2)

summary(mod_kr)

mod_pb <- PBmodcomp(mod1, mod2)

summary(mod_pb)
