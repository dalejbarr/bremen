library(lme4)

crefs <- readRDS("crefs.rds")
head(crefs, 3)

crefs_mit <- subset(crefs, ModInTrain) 

with(crefs_mit, aggregate(Modifier ~ Novelty+Addressee+Feedback, FUN=mean))

xtabs(~Addressee+SessionID, crefs)
xtabs(~Addressee+ItemID, crefs)

crefs_mit2 <- transform(crefs_mit, N=ifelse(Novelty=="New",1,0),
                    A=ifelse(Addressee=="New",1,0),
                    F=ifelse(Feedback=="Yes",1,0))
crefs_mit2c <- transform(crefs_mit2,
                     Nc=N-mean(N),
                     Ac=A-mean(A),
                     Fc=F-mean(F))                          

head(crefs_mit2c)

# takes a long time but doesn't converge
m1 <- glmer(Modifier ~ Ac*Nc*Fc +
            (1 + Nc*Fc | SessionID) + (1 + Ac*Nc | ItemID),
            crefs_mit2c, family=binomial(link="logit"),
            control=glmerControl(optimizer="bobyqa"))

# no-random-correlations model
m2 <- glmer(Modifier ~ Ac*Nc*Fc + 
            (1 + Nc*Fc || SessionID) + (1 + Ac*Nc || ItemID),
            crefs_mit2c, family=binomial(link="logit"),
            control=glmerControl(optimizer="bobyqa")) # converges

m3 <- glmer(Modifier ~ Ac*Nc*Fc +
                (1 | SessionID) +
                (0 + Nc | SessionID) +
                (1 | ItemID) +
                (0 + Ac | ItemID) +
                (0 + Nc | ItemID),
            crefs_mit2c, family=binomial(link="logit"),
            control=glmerControl(optimizer="bobyqa")) # converges

m3_noA <- update(m3, . ~ . - Ac) 
m3_noN <- update(m3, . ~ . - Nc) 
m3_noF <- update(m3, . ~ . - Fc) 
m3_noAN <- update(m3, . ~ . - Ac:Nc)
m3_noAF <- update(m3, . ~ . - Ac:Fc)
m3_noNF <- update(m3, . ~ . - Nc:Fc)
m3_noANF <- update(m3, . ~ . - Ac:Nc:Fc) 

anova(m3, m3_noA)
anova(m3, m3_noN)
anova(m3, m3_noF)
anova(m3, m3_noAN)
anova(m3, m3_noAF)
anova(m3, m3_noNF)
anova(m3, m3_noANF)


