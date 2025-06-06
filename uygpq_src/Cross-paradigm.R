library(lme4)
library(lmerTest)

#models with stakesContrastData

#Decision
m.stakes_decision <- glmer(decision ~ 
                             centredAge +
                             gender +
                             fairness +
                             stakes +
                             fairness:stakes +
                             (1 | participantNumber) + 
                             (1 | item), 
                          data = stakesContrastData, 
                          family = binomial(link = logit), 
                          control = glmerControl(optimizer = "bobyqa",
                                                 optCtrl = list(maxfun = 200000)))

#summary of model, including beta coefficients
summary(m.stakes_decision)

#generates 95% confidence intervals for each beta coefficient
m.stakes_decision.CI <- round(confint(m.stakes_decision, parm = "beta_"), 3)

#Response time
m.stakes_time <- lmer(logDecisionTime ~ 
                        centredAge +
                        gender +
                        fairness +
                        recodedDecision +
                        stakes +
                        fairness:stakes +
                        fairness:recodedDecision +
                        recodedDecision:stakes +
                        fairness:recodedDecision:stakes +
                        (1 | participantNumber) + 
                        (1 | item), 
                      data = stakesContrastData, 
                      control = lmerControl(optimizer = "bobyqa",
                                             optCtrl = list(maxfun = 200000)))

summary(m.stakes_time)

m.stakes_time.CI <- round(confint(m.stakes_time, parm = "beta_"), 3)

#Negative valence rating
m.stakes_feeling <- lmer(feeling ~ 
                            centredAge +
                            gender +
                            fairness +
                            recodedDecision +
                            stakes +
                            fairness:stakes +
                            fairness:recodedDecision +
                            recodedDecision:stakes +
                            fairness:recodedDecision:stakes +
                            (1 | participantNumber) + 
                            (1 | item), 
                          data = stakesContrastData, 
                          control = lmerControl(optimizer = "bobyqa",
                                                 optCtrl = list(maxfun = 200000)))

summary(m.stakes_feeling)

m.stakes_feeling.CI <- round(confint(m.stakes_feeling, parm = "beta_"), 3)

#models with commentsContrastData

#Decision
m.comments_decision <- glmer(decision ~ 
                               gender +
                               fairness +
                               comments +
                               fairness:comments +
                               (1 | participantNumber) + 
                               (1 | item), 
                             data = commentsContrastData, 
                             family = binomial(link = logit), 
                             control = glmerControl(optimizer = "bobyqa",
                                                    optCtrl = list(maxfun = 200000)))

summary(m.comments_decision)

m.comments_decision.CI <- round(confint(m.comments_decision, parm = "beta_"), 3)

#Response time
m.comments_time <- lmer(logDecisionTime ~ 
                           gender +
                           fairness +
                           recodedDecision +
                           comments +
                           fairness:comments +
                           fairness:recodedDecision +
                           recodedDecision:comments +
                           fairness:recodedDecision:comments +
                           (1 | participantNumber) + 
                           (1 | item), 
                         data = commentsContrastData, 
                         control = lmerControl(optimizer = "bobyqa",
                                                optCtrl = list(maxfun = 200000)))

summary(m.comments_time)

m.comments_time.CI <- round(confint(m.comments_time, parm = "beta_"), 3)

#Negative valence rating
m.comments_feeling <- lmer(feeling ~ 
                             gender +
                             fairness +
                             recodedDecision +
                             comments +
                             fairness:comments +
                             fairness:recodedDecision +
                             recodedDecision:comments +
                             fairness:recodedDecision:comments +
                             (1 | participantNumber) + 
                             (1 | item), 
                           data = commentsContrastData, 
                           control = lmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 200000)))


summary(m.comments_feeling)

m.comments_feeling.CI <- round(confint(m.comments_feeling, parm = "beta_"), 3)