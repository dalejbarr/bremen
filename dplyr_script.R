library("dplyr")

fan <- readRDS("FAN.rds")

## dplyr verbs:
##    select 
##    filter
##    mutate
##    arrange

select(fan, SessionID, Cond, Accuracy)
select(fan, -Cond)
fan[, c("SessionID", "Cond", "Accuracy")]

filter(fan, SessionID == 1)
filter(fan, ItemID == 1)
filter(fan, ItemID == 1 & Day == 1)
filter(fan, (SessionID == 1) | (ItemID == 1) )

mutate(fan, RTsecs = RT / 1000,
        D1 = (Cond == "same voice"))

arrange(fan, RT)
arrange(fan, desc(RT))

## dplyr (magrittr) pipes
##   x <- f()
##   y <- g(x, opt1 = TRUE)
##   z <- h(y, opt2 = TRUE)
##   h(g(f(x), opt1 = TRUE), opt2 = TRUE)
## y <- x %>% f() %>% g(opt1 = TRUE) %>% h(opt2 = TRUE)
## gloss: "..and then.."

fan %>% 
  filter(SessionID == 1) %>% 
  select(-Cond) %>%
  arrange(SessionID, ItemID)

fan2 <- fan %>% mutate(RTsecs = RT / 1000)

## data summaries
##    group_by
##    summarize
fan %>% 
  group_by(SessionID, Cond) %>% 
  summarize(mRT = mean(RT))

fan %>% group_by(SessionID, Cond) %>% 
  summarize(mRT = mean(RT),
            sd = sd(RT),
            Nobs = n()) %>%
  as.data.frame()

head(fan, 10)

fan2 <- fan %>% 
  group_by(SessionID) %>%
  mutate(ord = row_number()) %>% 
  ungroup()

## dplyr joins:
##    inner_join
##    left_join
subj <- fan %>% 
  select(SessionID) %>% 
  distinct() %>%
  arrange(SessionID) %>%
  mutate(Gender = rep(c("M", "F"), each = 10))

fan3 <- inner_join(fan, subj, "SessionID")