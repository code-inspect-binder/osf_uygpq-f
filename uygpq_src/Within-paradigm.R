library(lme4)
library(lmerTest)

#Models with Two-stakes paradigm

#Decision
m.p1_decision <- glmer(decision ~ 
                        centredAge +
                        gender +
                        stakeSize +
                        prevRecodedDecision +
                        centredCEMSAngerCoping +
                        centredCEMSSadnessCoping +
                        centredCEMSAnxietyCoping +
                        centredCEMSAngerInhibition +
                        centredCEMSSadnessInhibition +
                        centredCEMSAnxietyInhibition +
                        centredCEMSAngerDysreg +
                        centredCEMSSadnessDysreg +
                        centredCEMSAnxietyDysreg +
                        (1 | participantNumber) + 
                        (1 | item), 
                      data = p1Data, 
                      family = binomial(link = logit), 
                      control = glmerControl(optimizer = "bobyqa",
                                             optCtrl = list(maxfun = 200000)))

#summary of model, including beta coefficients  
summary(m.p1_decision)

#generates 95% confidence intervals for each beta coefficient
m.p1_decision.CI <- round(confint(m.p1_decision, parm = "beta_"), 3)

#Response time
m.p1_time <- lmer(logDecisionTime ~ 
                     centredAge +
                     gender +
                     stakeSize +
                     prevRecodedDecision +
                     recodedDecision +
                     centredCEMSAngerCoping +
                     centredCEMSSadnessCoping +
                     centredCEMSAnxietyCoping +
                     centredCEMSAngerInhibition +
                     centredCEMSSadnessInhibition +
                     centredCEMSAnxietyInhibition +
                     centredCEMSAngerDysreg +
                     centredCEMSSadnessDysreg +
                     centredCEMSAnxietyDysreg +
                     recodedDecision:stakeSize + 
                     recodedDecision:prevRecodedDecision +
                     recodedDecision:centredAge +
                     (1 | participantNumber) + 
                     (1 | item), 
                   data = p1Data, 
                   control = lmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 200000)))
  
summary(m.p1_time)

m.p1_time.CI <- round(confint(m.p1_time, parm = "beta_"), 3)

#Negative valence rating
m.p1_feeling <- lmer(feeling ~ 
                       centredAge +
                       gender +
                       stakeSize +
                       prevRecodedDecision +
                       recodedDecision +
                       centredCEMSAngerCoping +
                       centredCEMSSadnessCoping +
                       centredCEMSAnxietyCoping +
                       centredCEMSAngerInhibition +
                       centredCEMSSadnessInhibition +
                       centredCEMSAnxietyInhibition +
                       centredCEMSAngerDysreg +
                       centredCEMSSadnessDysreg +
                       centredCEMSAnxietyDysreg +
                       recodedDecision:stakeSize + 
                       recodedDecision:prevRecodedDecision +
                       recodedDecision:centredAge +
                       (1 | participantNumber) + 
                       (1 | item), 
                     data = p1Data, 
                     control = lmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 200000)))
  
summary(m.p1_feeling)

m.p1_feeling.CI <- round(confint(m.p1_feeling, parm = "beta_"), 3)


#Models with Four-stakes paradigm

#Decision
m.p2_decision <- glmer(decision ~ 
                        centredAge +
                        gender +
                        stakeSize +
                        prevRecodedDecision +
                        centredCEMSAngerCoping +
                        centredCEMSSadnessCoping +
                        centredCEMSAnxietyCoping +
                        centredCEMSAngerInhibition +
                        centredCEMSSadnessInhibition +
                        centredCEMSAnxietyInhibition +
                        centredCEMSAngerDysreg +
                        centredCEMSSadnessDysreg +
                        centredCEMSAnxietyDysreg +
                        (1 | participantNumber) + 
                        (1 | item), 
                      data = p2Data, 
                      family = binomial(link = logit), 
                      control = glmerControl(optimizer = "bobyqa",
                                             optCtrl = list(maxfun = 200000)))
  
summary(m.p2_decision)

m.p2_decision.CI <- round(confint(m.p2_decision, parm = "beta_"), 3)

#Response time
m.p2_time <- lmer(logDecisionTime ~ 
                   centredAge +
                   gender +
                   stakeSize +
                   prevRecodedDecision +
                   recodedDecision +
                   centredCEMSAngerCoping +
                   centredCEMSSadnessCoping +
                   centredCEMSAnxietyCoping +
                   centredCEMSAngerInhibition +
                   centredCEMSSadnessInhibition +
                   centredCEMSAnxietyInhibition +
                   centredCEMSAngerDysreg +
                   centredCEMSSadnessDysreg +
                   centredCEMSAnxietyDysreg +
                   recodedDecision:stakeSize + 
                   recodedDecision:prevRecodedDecision +
                   recodedDecision:centredAge +
                   (1 | participantNumber) + 
                   (1 | item), 
                 data = p2Data, 
                 control = lmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 200000)))
  
summary(m.p2_time)

m.p2_time.CI <- round(confint(m.p2_time, parm = "beta_"), 3)

#Negative valence rating
m.p2_feeling <- lmer(feeling ~ 
                       centredAge +
                       gender +
                       stakeSize +
                       prevRecodedDecision +
                       recodedDecision +
                       centredCEMSAngerCoping +
                       centredCEMSSadnessCoping +
                       centredCEMSAnxietyCoping +
                       centredCEMSAngerInhibition +
                       centredCEMSSadnessInhibition +
                       centredCEMSAnxietyInhibition +
                       centredCEMSAngerDysreg +
                       centredCEMSSadnessDysreg +
                       centredCEMSAnxietyDysreg +
                       recodedDecision:stakeSize + 
                       recodedDecision:prevRecodedDecision +
                       recodedDecision:centredAge +
                       (1 | participantNumber) + 
                       (1 | item), 
                     data = p2Data, 
                     control = lmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 200000)))

summary(m.p2_feeling)

m.p2_feeling.CI <- round(confint(m.p2_feeling, parm = "beta_"), 3)

#Models with Comments paradigm

#Decision
m.p3_decision <- glmer(decision ~ 
                         centredAge +
                         gender +
                         stakeSize +
                         commentNature +
                         prevRecodedDecision +
                         centredCEMSAngerCoping +
                         centredCEMSSadnessCoping +
                         centredCEMSAnxietyCoping +
                         centredCEMSAngerInhibition +
                         centredCEMSSadnessInhibition +
                         centredCEMSAnxietyInhibition +
                         centredCEMSAngerDysreg +
                         centredCEMSSadnessDysreg +
                         centredCEMSAnxietyDysreg +
                         (1 | participantNumber) + 
                         (1 | item), 
                       data = p3Data, 
                       family = binomial(link = logit), 
                       control = glmerControl(optimizer = "bobyqa",
                                              optCtrl = list(maxfun = 200000)))
  
summary(m.p3_decision)

m.p3_decision.CI <- round(confint(m.p3_decision, parm = "beta_"), 3)

#Response time
m.p3_time <- lmer(logDecisionTime ~ 
                    centredAge +
                    gender +
                    stakeSize +
                    commentNature +
                    prevRecodedDecision +
                    recodedDecision +
                    centredCEMSAngerCoping +
                    centredCEMSSadnessCoping +
                    centredCEMSAnxietyCoping +
                    centredCEMSAngerInhibition +
                    centredCEMSSadnessInhibition +
                    centredCEMSAnxietyInhibition +
                    centredCEMSAngerDysreg +
                    centredCEMSSadnessDysreg +
                    centredCEMSAnxietyDysreg +
                    recodedDecision:stakeSize + 
                    recodedDecision:prevRecodedDecision +
                    recodedDecision:centredAge +
                    recodedDecision:commentNature +
                    (1 | participantNumber) + 
                    (1 | item), 
                  data = p3Data, 
                  control = lmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 200000)))

summary(m.p3_time)

m.p3_time.CI <- round(confint(m.p3_time, parm = "beta_"), 3)

#Negative valence rating
m.p3_feeling <- lmer(feeling ~ 
                       centredAge +
                       gender +
                       stakeSize +
                       commentNature +
                       prevRecodedDecision +
                       recodedDecision +
                       centredCEMSAngerCoping +
                       centredCEMSSadnessCoping +
                       centredCEMSAnxietyCoping +
                       centredCEMSAngerInhibition +
                       centredCEMSSadnessInhibition +
                       centredCEMSAnxietyInhibition +
                       centredCEMSAngerDysreg +
                       centredCEMSSadnessDysreg +
                       centredCEMSAnxietyDysreg +
                       recodedDecision:stakeSize + 
                       recodedDecision:prevRecodedDecision +
                       recodedDecision:centredAge +
                       recodedDecision:commentNature +
                       (1 | participantNumber) + 
                       (1 | item), 
                     data = p3Data, 
                     control = lmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 200000)))
  
summary(m.p3_feeling)

m.p3_feeling.CI <- round(confint(m.p3_feeling, parm = "beta_"), 3)