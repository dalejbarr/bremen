library("MASS") # needed for mvrnorm

set.seed(11709)

nsubj <- 100
nitem <- 50 # must be an even number

########################################
## create the data structures
subj <- data.frame(subject_id = 1:nsubj)

item <- data.frame(item_id = 1:nitem,
                   cond = rep(1:2, each = nitem / 2))

trial <- expand.grid(subject_id = 1:nsubj,
                      item_id = 1:nitem)

########################################
## define parameters for data generation
## first, fixed effects
mu <- 800 # grand mean
eff <- 80 # 80 ms difference
effc <- c(-.5, .5) # deviation codes
res_sd <- 200 # residual (standard deviation)
iri <- 80 # by-item random intercept sd
sri <- 100 # by-subject random intercept sd
srs <- 40 # by-subject random slope sd
rcor <- .2 # correlation between intercept and slope

## define item random effects variance
## and sample items
item$iri <- rnorm(nitem, mean = 0, sd = iri)

## view the expected mean for each item
## for a typical subject (random effs = 0)
head(cbind(mu, eff[item$cond], item$iri,
           mu + eff[item$cond] + item$iri), 3)

## define subject random effects variance
## variance co-variance matrix
svcov <- matrix(c(sri^2, 
                  rcor * sri * srs,
                  rcor * sri * srs,
                  srs^2), nrow = 2)

## sample subjects
srfx <- mvrnorm(nsubj, mu = c(0, 0), Sigma = svcov)

subj$sri <- srfx[, 1]
subj$srs <- srfx[, 2]

head(subj, 3)

dat <- merge(merge(subj, trial), item)
dat <- dat[order(dat$subject_id, dat$item_id), ] # sort
dat$err <- rnorm(nrow(dat), sd = res_sd) # trial-level noise
dat$Y <- with(dat,
    mu + sri + iri + (eff + srs) * effc[cond] + err)

head(dat)

library("dplyr")
dat %>%
    mutate(mu = mu, eff = eff, x = effc[cond]) %>%
    select(sid = subject_id, iid = item_id, c = cond, 
           Y, mu, sri, iri, eff, srs, x, err) %>%
    filter(sid < 5) %>%
    group_by(sid, c) %>%
    slice(1:2) %>% ungroup()

library("lme4")
dat$c <- dat$cond - mean(dat$cond)
mod <- lmer(Y ~ c + (1 + c | subject_id) + (1 | item_id),
            dat, REML = FALSE)

summary(mod)
