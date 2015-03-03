library("MASS") # needed for mvrnorm

set.seed(11709)

nsubj <- 200
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
mu <- 1.5 # grand mean
eff <- .2   # difference
effc <- c(-.5, .5) # deviation codes
iri <- .8 # by-item random intercept sd
sri <- 1 # by-subject random intercept sd
srs <- .1 # by-subject random slope sd
rcor <- -.4 # correlation between intercept and slope

## define item random effects variance
## and sample items
item$iri <- rnorm(nitem, mean = 0, sd = iri)

## view the expected mean for each item
## for a typical subject (random effs = 0)
head(cbind(mu, effc[item$cond] * eff, item$iri,
           mu + effc[item$cond] * eff + item$iri), 3)

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
dat$eta <- with(dat,
    mu + sri + iri + (eff + srs) * effc[cond])

head(dat)

dat$p <- sapply(dat$eta, function(x) 1 / (1 + exp(-x)))

dat$Y <- sapply(dat$p, function(x) {
  sample(c(0, 1), 1, prob = c(1 - x, x))
})

library("lme4")

dat$c <- dat$cond - mean(dat$cond)

mod <- glmer(Y ~ c + 
              (1 + c | subject_id) + 
              (1 | item_id),
            dat, 
            family = binomial(link = "logit"),
            control = glmerControl(optimizer = "Nelder_Mead"))

summary(mod)

