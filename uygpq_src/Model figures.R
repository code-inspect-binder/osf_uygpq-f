library(dplyr)
library(ggplot2)
library(ggsignif)
library(patchwork)

#Contrasting Two- vs Four-stakes paradigms

#Decision
#generates table of beta coefficients and associated statistics
coef.stakes_decision <- data.frame(cbind(summary(m.stakes_decision)$coefficients, m.stakes_decision.CI)) %>%
  tibble::rownames_to_column('Variable') %>%
  rename(SE = Std..Error) %>%
  rename(Z = z.value) %>%
  rename(P = Pr...z..) %>%
  rename(CI_L = X2.5..) %>%
  rename(CI_U = X97.5..) %>%       
  mutate(Variable = factor(Variable, 
                           levels = c('fairnessunfair:recodedDecisionreject:stakes4', 
                                      'recodedDecisionreject:stakes4',
                                      'fairnessunfair:stakes4', 
                                      'fairnessunfair:recodedDecisionreject',
                                      'stakes4',
                                      'recodedDecisionreject',
                                      'fairnessunfair',
                                      'genderfemale',
                                      'centredAge',
                                      '(Intercept)'
                                      )))

#figure 1(a)
plot.coef.stakes_decision <- ggplot(data=coef.stakes_decision,
                                    aes(x=Estimate, 
                                        y=Variable)) +
  geom_vline(xintercept = 0, color = 'grey') +
  annotate("rect", 
           xmin = -Inf, 
           xmax = 0, 
           ymin = -Inf, 
           ymax = Inf, 
           fill = "grey", 
           alpha = 0.25) +
  geom_point(size = 2) +
  geom_text(aes(label= format(round(Estimate, digits = 2), nsmall = 2)),
                vjust = -0.5, size = 5) +
  geom_errorbar(aes(xmin = CI_L,
                    xmax = CI_U),
                width = 0.2) +
  geom_signif(stat="identity",
              data=data.frame(x = c(-2.82, 7.18),
                              y = c(7, 10), 
                              annotation =c ("***", "***")), 
              group = c(1:2),
              aes(x = x,
                  xend = x, 
                  y = y, 
                  yend = y, 
                  annotation = annotation, 
                  group = group,
                  textsize = 5)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y=element_blank(),
        axis.text = element_text(size=15),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 15)) +
  scale_y_discrete(drop = FALSE,
                   labels = c('Unfair x Reject x 4 stakes',
                              'Reject x 4 stakes',
                              'Unfair x 4 stakes',
                              'Unfair x Reject',
                              '4 stakes',
                              'Reject',
                              'Unfair',
                              'Female',
                              'Age',
                              'Intercept')) + 
  labs(title = 'a')

#Response time
coef.stakes_time <- data.frame(cbind(summary(m.stakes_time)$coefficients, m.stakes_time.CI)) %>%
  tibble::rownames_to_column('Variable') %>%
  rename(SE = Std..Error) %>%
  rename(Tvalue = t.value) %>%
  rename(P = Pr...t..) %>%
  rename(CI_L = X2.5..) %>%
  rename(CI_U = X97.5..) %>%  
  mutate(Variable = factor(Variable, 
                           levels = c('fairnessunfair:recodedDecisionreject:stakes4', 
                                      'recodedDecisionreject:stakes4',
                                      'fairnessunfair:stakes4', 
                                      'fairnessunfair:recodedDecisionreject',
                                      'stakes4',
                                      'recodedDecisionreject',
                                      'fairnessunfair',
                                      'genderfemale',
                                      'centredAge',
                                      '(Intercept)'
                           )))

#Figure 1(b)
plot.coef.stakes_time <- ggplot(data=coef.stakes_time,
                                    aes(x=Estimate, 
                                        y=Variable)) +  
  geom_vline(xintercept = 0, color = 'grey') +
  annotate("rect", 
           xmin = -Inf, 
           xmax = 0, 
           ymin = -Inf, 
           ymax = Inf, 
           fill = "grey", 
           alpha = 0.25) +
  geom_point(size = 2) +
  geom_text(aes(label= format(round(Estimate, digits = 2), nsmall = 2)),
            vjust = -0.5, size = 5) +
  geom_errorbar(aes(xmin = CI_L,
                    xmax = CI_U),
                width = 0.2)  +
  geom_signif(stat="identity",
              data=data.frame(x = c(-0.06, 1.97),
                              y = c(4, 10), 
                              annotation =c ("*", "***")), 
              group = c(1:2),
              aes(x = x,
                  xend = x, 
                  y = y, 
                  yend = y, 
                  annotation = annotation, 
                  group = group,
                  textsize = 5)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size=15),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 15)) +
  scale_y_discrete(drop = FALSE) + 
  labs(title = 'b')

#Negative valence rating
coef.stakes_feeling <- data.frame(cbind(summary(m.stakes_feeling)$coefficients, m.stakes_feeling.CI)) %>%
  tibble::rownames_to_column('Variable') %>%
  rename(SE = Std..Error) %>%
  rename(Tvalue = t.value) %>%
  rename(P = Pr...t..) %>%
  rename(CI_L = X2.5..) %>%
  rename(CI_U = X97.5..) %>%  
  mutate(Variable = factor(Variable, 
                           levels = c('fairnessunfair:recodedDecisionreject:stakes4', 
                                      'recodedDecisionreject:stakes4', 
                                      'fairnessunfair:stakes4',
                                      'fairnessunfair:recodedDecisionreject',
                                      'stakes4',
                                      'recodedDecisionreject',
                                      'fairnessunfair',
                                      'genderfemale',
                                      'centredAge',
                                      '(Intercept)'
                           )))

#Figure 1(c)
plot.coef.stakes_feeling <- ggplot(data=coef.stakes_feeling,
                                aes(x=Estimate, 
                                    y=Variable)) +  
  geom_vline(xintercept = 0, color = 'grey') +
  annotate("rect", 
           xmin = -Inf, 
           xmax = 0, 
           ymin = -Inf, 
           ymax = Inf, 
           fill = "grey", 
           alpha = 0.25) +
  geom_point(size = 2) +
  geom_text(aes(label= format(round(Estimate, digits = 2), nsmall = 2)),
            vjust = -0.5, size = 5) +
  geom_errorbar(aes(xmin = CI_L,
                    xmax = CI_U),
                width = 0.2) +
  geom_signif(stat="identity",
              data=data.frame(x = c(-0.35, 1.53, 1.76, 1.55),
                              y = c(1, 6, 7, 10), 
                              annotation =c ("*", "***", "***", "***")), 
              group = c(1:4),
              aes(x = x,
                  xend = x, 
                  y = y, 
                  yend = y, 
                  annotation = annotation, 
                  group = group,
                  textsize = 5)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size=15),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 15)) +
  scale_y_discrete(drop = FALSE) + 
  labs(title = 'c')

#Figure 1
plot.coef.stakes_decision + plot.coef.stakes_time + plot.coef.stakes_feeling

#Contrasting Two-stakes vs Comments paradigms

#Decision
coef.comments_decision <- data.frame(cbind(summary(m.comments_decision)$coefficients, m.comments_decision.CI)) %>%
  tibble::rownames_to_column('Variable') %>%
  rename(SE = Std..Error) %>%
  rename(Z = z.value) %>%
  rename(P = Pr...z..) %>%
  rename(CI_L = X2.5..) %>%
  rename(CI_U = X97.5..) %>%  
  mutate(Variable = factor(Variable, 
                           levels = c('fairnessunfair:recodedDecisionreject:commentswith comments', 
                                      'recodedDecisionreject:commentswith comments',
                                      'fairnessunfair:commentswith comments', 
                                      'fairnessunfair:recodedDecisionreject',
                                      'commentswith comments',
                                      'recodedDecisionreject',
                                      'fairnessunfair',
                                      'genderfemale',
                                      '(Intercept)'
                           )))

#Figure 2(a)
plot.coef.comments_decision <- ggplot(data=coef.comments_decision,
                                    aes(x=Estimate, 
                                        y=Variable)) +  
  geom_vline(xintercept = 0, color = 'grey') +
  annotate("rect", 
           xmin = -Inf, 
           xmax = 0, 
           ymin = -Inf, 
           ymax = Inf, 
           fill = "grey", 
           alpha = 0.25) +
  geom_point(size = 2) +
  geom_text(aes(label= format(round(Estimate, digits = 2), nsmall = 2)),
            vjust = -0.5, size = 5) +
  geom_errorbar(aes(xmin = CI_L,
                    xmax = CI_U),
                width = 0.2) +
  geom_signif(stat="identity",
              data=data.frame(x = c(0.36, -2.89, 6.29),
                              y = c(3, 7, 9), 
                              annotation =c ("*","***", "***")), 
              group = c(1:3),
              aes(x = x,
                  xend = x, 
                  y = y, 
                  yend = y, 
                  annotation = annotation, 
                  group = group,
                  textsize = 5)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y=element_blank(),
        axis.text = element_text(size=15),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 15)) +
  scale_y_discrete(drop = FALSE,
                   labels = c('Unfair x Reject x Comments',
                              'Reject x Comments',
                              'Unfair x Comments',
                              'Unfair x Reject',
                              'Comments',
                              'Reject',
                              'Unfair',
                              'Female',
                              'Intercept')) + 
  labs(title = 'a')

#Response time
coef.comments_time <- data.frame(cbind(summary(m.comments_time)$coefficients, m.comments_time.CI)) %>%
  tibble::rownames_to_column('Variable') %>%
  rename(SE = Std..Error) %>%
  rename(Tvalue = t.value) %>%
  rename(P = Pr...t..) %>%
  rename(CI_L = X2.5..) %>%
  rename(CI_U = X97.5..) %>%  
  mutate(Variable = factor(Variable, 
                           levels = c('fairnessunfair:recodedDecisionreject:commentswith comments', 
                                      'recodedDecisionreject:commentswith comments', 
                                      'fairnessunfair:commentswith comments',
                                      'fairnessunfair:recodedDecisionreject',
                                      'commentswith comments',
                                      'recodedDecisionreject',
                                      'fairnessunfair',
                                      'genderfemale',
                                      '(Intercept)'
                           )))

#Figure 2(b)
plot.coef.comments_time <- ggplot(data=coef.comments_time,
                                aes(x=Estimate, 
                                    y=Variable)) +  
  geom_vline(xintercept = 0, color = 'grey') +
  annotate("rect", 
           xmin = -Inf, 
           xmax = 0, 
           ymin = -Inf, 
           ymax = Inf, 
           fill = "grey", 
           alpha = 0.25) +
  geom_point(size = 2) +
  geom_text(aes(label= format(round(Estimate, digits = 2), nsmall = 2)),
            vjust = -0.5, size = 5) +
  geom_errorbar(aes(xmin = CI_L,
                    xmax = CI_U),
                width = 0.2)  +
  geom_signif(stat="identity",
              data=data.frame(x = c(0.05, 2.02),
                              y = c(4, 9), 
                              annotation =c ("*", "***")), 
              group = c(1:2),
              aes(x = x,
                  xend = x, 
                  y = y, 
                  yend = y, 
                  annotation = annotation, 
                  group = group,
                  textsize = 5)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size=15),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 15)) +
  scale_y_discrete(drop = FALSE) + 
  labs(title = 'b')

#Negative valence rating
coef.comments_feeling <- data.frame(cbind(summary(m.comments_feeling)$coefficients, m.comments_feeling.CI)) %>%
  tibble::rownames_to_column('Variable') %>%
  rename(SE = Std..Error) %>%
  rename(Tvalue = t.value) %>%
  rename(P = Pr...t..) %>%
  rename(CI_L = X2.5..) %>%
  rename(CI_U = X97.5..) %>%  
  mutate(Variable = factor(Variable, 
                           levels = c('fairnessunfair:recodedDecisionreject:commentswith comments', 
                                      'recodedDecisionreject:commentswith comments',
                                      'fairnessunfair:commentswith comments', 
                                      'fairnessunfair:recodedDecisionreject',
                                      'commentswith comments',
                                      'recodedDecisionreject',
                                      'fairnessunfair',
                                      'genderfemale',
                                      '(Intercept)'
                           )))
#Figure 2(c)
plot.coef.comments_feeling <- ggplot(data=coef.comments_feeling,
                                   aes(x=Estimate, 
                                       y=Variable)) +  
  geom_vline(xintercept = 0, color = 'grey') +
  annotate("rect", 
           xmin = -Inf, 
           xmax = 0, 
           ymin = -Inf, 
           ymax = Inf, 
           fill = "grey", 
           alpha = 0.25) +
  geom_point(size = 2) +
  geom_text(aes(label= format(round(Estimate, digits = 2), nsmall = 2)),
            vjust = -0.5, size = 5) +
  geom_errorbar(aes(xmin = CI_L,
                    xmax = CI_U),
                width = 0.2) +
  geom_signif(stat="identity",
              data=data.frame(x = c(0.13, 1.73, 1.6, 0.65, 1.52),
                              y = c(3, 6, 7, 8, 9), 
                              annotation =c ("*", "***", "***", "*", "***")), 
              group = c(1:5),
              aes(x = x,
                  xend = x, 
                  y = y, 
                  yend = y, 
                  annotation = annotation, 
                  group = group,
                  textsize = 5)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size=15),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 15)) +
  scale_y_discrete(drop = FALSE) + 
  labs(title = 'c')

#Figure 2
plot.coef.comments_decision + plot.coef.comments_time + plot.coef.comments_feeling

#Two-stakes paradigm

#Decision
coef.p1_decision <- data.frame(cbind(summary(m.p1_decision)$coefficients, m.p1_decision.CI)) %>%
  tibble::rownames_to_column('Variable') %>%
  rename(SE = Std..Error) %>%
  rename(Z = z.value) %>%
  rename(P = Pr...z..) %>%
  rename(CI_L = X2.5..) %>%
  rename(CI_U = X97.5..) %>%  
  mutate(Variable = factor(Variable, 
                           levels = c('centredCEMSAnxietyDysreg',
                                      'centredCEMSSadnessDysreg',
                                      'centredCEMSAngerDysreg',
                                      'centredCEMSAnxietyInhibition',
                                      'centredCEMSSadnessInhibition',
                                      'centredCEMSAngerInhibition',
                                      'centredCEMSAnxietyCoping',
                                      'centredCEMSSadnessCoping',
                                      'centredCEMSAngerCoping',
                                      'commentNaturenegative',
                                      'stakeSize0.1',
                                      'stakeSize1',
                                      'stakeSize10',
                                      'prevRecodedDecisionreject',
                                      'genderfemale',
                                      'centredAge',
                                      '(Intercept)'
                           )))
#Figure 3(a)
plot.coef.p1_decision <- ggplot(data=coef.p1_decision,
                                      aes(x=Estimate, 
                                          y=Variable)) +  
  geom_vline(xintercept = 0, color = 'grey') +
  annotate("rect", 
           xmin = -Inf, 
           xmax = 0, 
           ymin = -Inf, 
           ymax = Inf, 
           fill = "grey", 
           alpha = 0.25) +
  geom_point(size = 2) +
  geom_text(aes(label= format(round(Estimate, digits = 2), nsmall = 2)),
            vjust = -0.5, size = 5) +
  geom_errorbar(aes(xmin = CI_L,
                    xmax = CI_U),
                width = 0.2) +
  geom_signif(stat="identity",
              data=data.frame(x = c(-0.14),
                              y = c(14), 
                              annotation =c ("***")), 
              group = c(1),
              aes(x = x,
                  xend = x, 
                  y = y, 
                  yend = y, 
                  annotation = annotation, 
                  group = group,
                  textsize = 5)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y=element_blank(),
        axis.text = element_text(size=15),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 15)) +
  scale_y_discrete(drop = FALSE,
                   labels = c('Anxiety dysregulation',
                              'Sadness dysregulation',
                              'Anger dysregulation',
                              'Anxiety inhibition',
                              'Sadness inhibition',
                              'Anger inhibition',
                              'Anxiety coping',
                              'Sadness coping',
                              'Anger coping',
                              'Negative comment',
                              '£0.10 stake',
                              '£1 stake',
                              '£10 stake',
                              'Reject previous offer',
                              'Female',
                              'Age',
                              'Intercept')) + 
  labs(title = 'a')

#Response time
coef.p1_time <- data.frame(cbind(summary(m.p1_time)$coefficients, m.p1_time.CI)) %>%
  tibble::rownames_to_column('Variable') %>%
  rename(SE = Std..Error) %>%
  rename(Tvalue = t.value) %>%
  rename(P = Pr...t..) %>%
  rename(CI_L = X2.5..) %>%
  rename(CI_U = X97.5..) %>%  
  mutate(Variable = factor(Variable, 
                           levels = c('commentNaturenegative:recodedDecisionreject',
                                      'stakeSize0.1:recodedDecisionreject',
                                      'stakeSize1:recodedDecisionreject',
                                      'stakeSize10:recodedDecisionreject',
                                      'prevRecodedDecisionreject:recodedDecisionreject',
                                      'centredAge:recodedDecisionreject',
                                      'centredCEMSAnxietyDysreg',
                                      'centredCEMSSadnessDysreg',
                                      'centredCEMSAngerDysreg',
                                      'centredCEMSAnxietyInhibition',
                                      'centredCEMSSadnessInhibition',
                                      'centredCEMSAngerInhibition',
                                      'centredCEMSAnxietyCoping',
                                      'centredCEMSSadnessCoping',
                                      'centredCEMSAngerCoping',
                                      'commentNaturenegative',
                                      'stakeSize0.1',
                                      'stakeSize1',
                                      'stakeSize10',
                                      'recodedDecisionreject',
                                      'prevRecodedDecisionreject',
                                      'genderfemale',
                                      'centredAge',
                                      '(Intercept)'
                           )))

#Figure 4(a)
plot.coef.p1_time <- ggplot(data=coef.p1_time,
                                aes(x=Estimate, 
                                    y=Variable)) +  
  geom_vline(xintercept = 0, color = 'grey') +
  annotate("rect", 
           xmin = -Inf, 
           xmax = 0, 
           ymin = -Inf, 
           ymax = Inf, 
           fill = "grey", 
           alpha = 0.25) +
  geom_point(size = 2) +
  geom_text(aes(label= format(round(Estimate, digits = 2), nsmall = 2)),
            vjust = -0.5, size = 5) +
  geom_errorbar(aes(xmin = CI_L,
                    xmax = CI_U),
                width = 0.2) +
  geom_signif(stat="identity",
              data=data.frame(x = c(0.22, 0.85, 2.3),
                              y = c(5, 9, 24), 
                              annotation =c ("*", "**", "***")), 
              group = c(1:3),
              aes(x = x,
                  xend = x, 
                  y = y, 
                  yend = y, 
                  annotation = annotation, 
                  group = group,
                  textsize = 5)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y=element_blank(),
        axis.text = element_text(size=15),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 15)) +
  scale_y_discrete(drop = FALSE,
                   labels = c('Reject current x Negative comment',
                              'Reject current x £0.10 stake',
                              'Reject current x £1 stake',
                              'Reject current x £10 stake',
                              'Reject current x Reject previous',
                              'Reject current x Age',
                              'Anxiety dysregulation',
                              'Sadness dysregulation',
                              'Anger dysregulation',
                              'Anxiety inhibition',
                              'Sadness inhibition',
                              'Anger inhibition',
                              'Anxiety coping',
                              'Sadness coping',
                              'Anger coping',
                              'Negative comment',
                              '£0.10 stake',
                              '£1 stake',
                              '£10 stake',
                              'Reject current offer',
                              'Reject previous offer',
                              'Female',
                              'Age',
                              'Intercept')) + 
  labs(title = 'a')

#Negative valence rating
coef.p1_feeling <- data.frame(cbind(summary(m.p1_feeling)$coefficients, m.p1_feeling.CI)) %>%
  tibble::rownames_to_column('Variable') %>%
  rename(SE = Std..Error) %>%
  rename(Tvalue = t.value) %>%
  rename(P = Pr...t..) %>%
  rename(CI_L = X2.5..) %>%
  rename(CI_U = X97.5..) %>%  
  mutate(Variable = factor(Variable, 
                           levels = c('commentNaturenegative:recodedDecisionreject',
                                      'stakeSize0.1:recodedDecisionreject',
                                      'stakeSize1:recodedDecisionreject',
                                      'stakeSize10:recodedDecisionreject',
                                      'prevRecodedDecisionreject:recodedDecisionreject',
                                      'centredAge:recodedDecisionreject',
                                      'centredCEMSAnxietyDysreg',
                                      'centredCEMSSadnessDysreg',
                                      'centredCEMSAngerDysreg',
                                      'centredCEMSAnxietyInhibition',
                                      'centredCEMSSadnessInhibition',
                                      'centredCEMSAngerInhibition',
                                      'centredCEMSAnxietyCoping',
                                      'centredCEMSSadnessCoping',
                                      'centredCEMSAngerCoping',
                                      'commentNaturenegative',
                                      'stakeSize0.1',
                                      'stakeSize1',
                                      'stakeSize10',
                                      'recodedDecisionreject',
                                      'prevRecodedDecisionreject',
                                      'genderfemale',
                                      'centredAge',
                                      '(Intercept)'
                           )))
#Figure 5(a)
plot.coef.p1_feeling <- ggplot(data=coef.p1_feeling,
                            aes(x=Estimate, 
                                y=Variable)) +  
  geom_vline(xintercept = 0, color = 'grey') +
  annotate("rect", 
           xmin = -Inf, 
           xmax = 0, 
           ymin = -Inf, 
           ymax = Inf, 
           fill = "grey", 
           alpha = 0.25) +
  geom_point(size = 2) +
  geom_text(aes(label= format(round(Estimate, digits = 2), nsmall = 2)),
            vjust = -0.5, size = 5) +
  geom_errorbar(aes(xmin = CI_L,
                    xmax = CI_U),
                width = 0.2) +
  geom_signif(stat="identity",
              data=data.frame(x = c(0.20, -0.28, 1.78, 1.17, 2.9),
                              y = c(5, 8, 20, 21, 24), 
                              annotation =c ("*", "*", "***", "***", "***")), 
              group = c(1:5),
              aes(x = x,
                  xend = x, 
                  y = y, 
                  yend = y, 
                  annotation = annotation, 
                  group = group,
                  textsize = 5)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        xis.title.y=element_blank(),
        axis.text = element_text(size=15),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 15)) +
  scale_y_discrete(drop = FALSE,
                   labels = c('Reject current x Negative comment',
                              'Reject current x £0.10 stake',
                              'Reject current x £1 stake',
                              'Reject current x £10 stake',
                              'Reject current x Reject previous',
                              'Reject current x Age',
                              'Anxiety dysregulation',
                              'Sadness dysregulation',
                              'Anger dysregulation',
                              'Anxiety inhibition',
                              'Sadness inhibition',
                              'Anger inhibition',
                              'Anxiety coping',
                              'Sadness coping',
                              'Anger coping',
                              'Negative comment',
                              '£0.10 stake',
                              '£1 stake',
                              '£10 stake',
                              'Reject current offer',
                              'Reject previous offer',
                              'Female',
                              'Age',
                              'Intercept')) + 
  labs(title = 'a')

#Four-stakes paradigm

#Decision
coef.p2_decision <- data.frame(cbind(summary(m.p2_decision)$coefficients, m.p2_decision.CI)) %>%
  tibble::rownames_to_column('Variable') %>%
  rename(SE = Std..Error) %>%
  rename(Z = z.value) %>%
  rename(P = Pr...z..) %>%
  rename(CI_L = X2.5..) %>%
  rename(CI_U = X97.5..) %>%  
  mutate(Variable = factor(Variable, 
                           levels = c('centredCEMSAnxietyDysreg',
                                      'centredCEMSSadnessDysreg',
                                      'centredCEMSAngerDysreg',
                                      'centredCEMSAnxietyInhibition',
                                      'centredCEMSSadnessInhibition',
                                      'centredCEMSAngerInhibition',
                                      'centredCEMSAnxietyCoping',
                                      'centredCEMSSadnessCoping',
                                      'centredCEMSAngerCoping',
                                      'commentNaturenegative',
                                      'stakeSize0.1',
                                      'stakeSize1',
                                      'stakeSize10',
                                      'prevRecodedDecisionreject',
                                      'genderfemale',
                                      'centredAge',
                                      '(Intercept)'
                           )))

#Figure 3(b)
plot.coef.p2_decision <- ggplot(data=coef.p2_decision,
                                aes(x=Estimate, 
                                    y=Variable)) +  
  geom_vline(xintercept = 0, color = 'grey') +
  annotate("rect", 
           xmin = -Inf, 
           xmax = 0, 
           ymin = -Inf, 
           ymax = Inf, 
           fill = "grey", 
           alpha = 0.25) +
  geom_point(size = 2) +
  geom_text(aes(label= format(round(Estimate, digits = 2), nsmall = 2)),
            vjust = -0.5, size = 5) +
  geom_errorbar(aes(xmin = CI_L,
                    xmax = CI_U),
                width = 0.2) +
  geom_signif(stat="identity",
              data=data.frame(x = c(-0.62, 0.04, -0.24, 0.20),
                              y = c(3, 11, 12, 14), 
                              annotation =c ("*", "***", "***", "***")), 
              group = c(1:4),
              aes(x = x,
                  xend = x, 
                  y = y, 
                  yend = y, 
                  annotation = annotation, 
                  group = group,
                  textsize = 5)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y=element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size=15),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 15)) +
  scale_y_discrete(drop = FALSE) + 
  labs(title = 'b')

#Response time
coef.p2_time <- data.frame(cbind(summary(m.p2_time)$coefficients, m.p2_time.CI)) %>%
  tibble::rownames_to_column('Variable') %>%
  rename(SE = Std..Error) %>%
  rename(Tvalue = t.value) %>%
  rename(P = Pr...t..) %>%
  rename(CI_L = X2.5..) %>%
  rename(CI_U = X97.5..) %>%  
  mutate(Variable = factor(Variable, 
                           levels = c('commentNaturenegative:recodedDecisionreject',
                                      'stakeSize0.1:recodedDecisionreject',
                                      'stakeSize1:recodedDecisionreject',
                                      'stakeSize10:recodedDecisionreject',
                                      'prevRecodedDecisionreject:recodedDecisionreject',
                                      'centredAge:recodedDecisionreject',
                                      'centredCEMSAnxietyDysreg',
                                      'centredCEMSSadnessDysreg',
                                      'centredCEMSAngerDysreg',
                                      'centredCEMSAnxietyInhibition',
                                      'centredCEMSSadnessInhibition',
                                      'centredCEMSAngerInhibition',
                                      'centredCEMSAnxietyCoping',
                                      'centredCEMSSadnessCoping',
                                      'centredCEMSAngerCoping',
                                      'commentNaturenegative',
                                      'stakeSize0.1',
                                      'stakeSize1',
                                      'stakeSize10',
                                      'recodedDecisionreject',
                                      'prevRecodedDecisionreject',
                                      'genderfemale',
                                      'centredAge',
                                      '(Intercept)'
                           )))

#Figure 4(b)
plot.coef.p2_time <- ggplot(data=coef.p2_time,
                            aes(x=Estimate, 
                                y=Variable)) +  
  geom_vline(xintercept = 0, color = 'grey') +
  annotate("rect", 
           xmin = -Inf, 
           xmax = 0, 
           ymin = -Inf, 
           ymax = Inf, 
           fill = "grey", 
           alpha = 0.25) +
  geom_point(size = 2) +
  geom_text(aes(label= format(round(Estimate, digits = 2), nsmall = 2)),
            vjust = -0.5, size = 5) +
  geom_errorbar(aes(xmin = CI_L,
                    xmax = CI_U),
                width = 0.2) +
  geom_signif(stat="identity",
              data=data.frame(x = c(0.21, 2.06),
                              y = c(5, 24), 
                              annotation =c ("**", "***")), 
              group = c(1:2),
              aes(x = x,
                  xend = x, 
                  y = y, 
                  yend = y, 
                  annotation = annotation, 
                  group = group,
                  textsize = 5)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y=element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size=15),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 15)) +
  scale_y_discrete(drop = FALSE) + 
  labs(title = 'b')

#Negative valence rating
coef.p2_feeling <- data.frame(cbind(summary(m.p2_feeling)$coefficients, m.p2_feeling.CI)) %>%
  tibble::rownames_to_column('Variable') %>%
  rename(SE = Std..Error) %>%
  rename(Tvalue = t.value) %>%
  rename(P = Pr...t..) %>%
  rename(CI_L = X2.5..) %>%
  rename(CI_U = X97.5..) %>%  
  mutate(Variable = factor(Variable, 
                           levels = c('commentNaturenegative:recodedDecisionreject',
                                      'stakeSize0.1:recodedDecisionreject',
                                      'stakeSize1:recodedDecisionreject',
                                      'stakeSize10:recodedDecisionreject',
                                      'prevRecodedDecisionreject:recodedDecisionreject',
                                      'centredAge:recodedDecisionreject',
                                      'centredCEMSAnxietyDysreg',
                                      'centredCEMSSadnessDysreg',
                                      'centredCEMSAngerDysreg',
                                      'centredCEMSAnxietyInhibition',
                                      'centredCEMSSadnessInhibition',
                                      'centredCEMSAngerInhibition',
                                      'centredCEMSAnxietyCoping',
                                      'centredCEMSSadnessCoping',
                                      'centredCEMSAngerCoping',
                                      'commentNaturenegative',
                                      'stakeSize0.1',
                                      'stakeSize1',
                                      'stakeSize10',
                                      'recodedDecisionreject',
                                      'prevRecodedDecisionreject',
                                      'genderfemale',
                                      'centredAge',
                                      '(Intercept)'
                           )))

#Figure 5(b)
plot.coef.p2_feeling <- ggplot(data=coef.p2_feeling,
                            aes(x=Estimate, 
                                y=Variable)) +  
  geom_vline(xintercept = 0, color = 'grey') +
  annotate("rect", 
           xmin = -Inf, 
           xmax = 0, 
           ymin = -Inf, 
           ymax = Inf, 
           fill = "grey", 
           alpha = 0.25) +
  geom_point(size = 2) +
  geom_text(aes(label= format(round(Estimate, digits = 2), nsmall = 2)),
            vjust = -0.5, size = 5) +
  geom_errorbar(aes(xmin = CI_L,
                    xmax = CI_U),
                width = 0.2) +
  geom_signif(stat="identity",
              data=data.frame(x = c(0.45, 1.01, 0.34, 0.26, 1.25, 0.77, 3.15),
                              y = c(6, 9, 17, 18, 20, 21, 24), 
                              annotation =c ("***", "**", "***", "***", "***", "***", "***")), 
              group = c(1:7),
              aes(x = x,
                  xend = x, 
                  y = y, 
                  yend = y, 
                  annotation = annotation, 
                  group = group,
                  textsize = 5)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y=element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size=15),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 15)) +
  scale_y_discrete(drop = FALSE) + 
  labs(title = 'b')

#Comments paradigm

#Decision
coef.p3_decision <- data.frame(cbind(summary(m.p3_decision)$coefficients, m.p3_decision.CI)) %>%
  tibble::rownames_to_column('Variable') %>%
  rename(SE = Std..Error) %>%
  rename(Z = z.value) %>%
  rename(P = Pr...z..) %>%
  rename(CI_L = X2.5..) %>%
  rename(CI_U = X97.5..) %>%  
  mutate(Variable = factor(Variable, 
                           levels = c('centredCEMSAnxietyDysreg',
                                      'centredCEMSSadnessDysreg',
                                      'centredCEMSAngerDysreg',
                                      'centredCEMSAnxietyInhibition',
                                      'centredCEMSSadnessInhibition',
                                      'centredCEMSAngerInhibition',
                                      'centredCEMSAnxietyCoping',
                                      'centredCEMSSadnessCoping',
                                      'centredCEMSAngerCoping',
                                      'commentNaturenegative',
                                      'stakeSize0.1',
                                      'stakeSize1',
                                      'stakeSize10',
                                      'prevRecodedDecisionreject',
                                      'genderfemale',
                                      'centredAge',
                                      '(Intercept)'
                           )))

#Figure 3(c)
plot.coef.p3_decision <- ggplot(data=coef.p3_decision,
                                aes(x=Estimate, 
                                    y=Variable)) +  
  geom_vline(xintercept = 0, color = 'grey') +
  annotate("rect", 
           xmin = -Inf, 
           xmax = 0, 
           ymin = -Inf, 
           ymax = Inf, 
           fill = "grey", 
           alpha = 0.25) +
  geom_point(size = 2) +
  geom_text(aes(label= format(round(Estimate, digits = 2), nsmall = 2)),
            vjust = -0.5, size = 5) +
  geom_errorbar(aes(xmin = CI_L,
                    xmax = CI_U),
                width = 0.2) +
  geom_signif(stat="identity",
              data=data.frame(x = c(1.83, 1.84, -0.73, 2.05, 0.15, 0.24, 0.18, 0.33),
                              y = c(1, 4, 5, 9, 10, 12, 14, 16), 
                              annotation =c ("*", "*", "**", "*", "***", "***", "***", "*")), 
              group = c(1:8),
              aes(x = x,
                  xend = x, 
                  y = y, 
                  yend = y, 
                  annotation = annotation, 
                  group = group,
                  textsize = 5)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y=element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size=15),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 15)) +
  scale_y_discrete(drop = FALSE) + 
  labs(title = 'c')

#Response time
coef.p3_time <- data.frame(cbind(summary(m.p3_time)$coefficients, m.p3_time.CI)) %>%
  tibble::rownames_to_column('Variable') %>%
  rename(SE = Std..Error) %>%
  rename(Tvalue = t.value) %>%
  rename(P = Pr...t..) %>%
  rename(CI_L = X2.5..) %>%
  rename(CI_U = X97.5..) %>%  
  mutate(Variable = factor(Variable, 
                           levels = c('commentNaturenegative:recodedDecisionreject',
                                      'stakeSize0.1:recodedDecisionreject',
                                      'stakeSize1:recodedDecisionreject',
                                      'stakeSize10:recodedDecisionreject',
                                      'prevRecodedDecisionreject:recodedDecisionreject',
                                      'centredAge:recodedDecisionreject',
                                      'centredCEMSAnxietyDysreg',
                                      'centredCEMSSadnessDysreg',
                                      'centredCEMSAngerDysreg',
                                      'centredCEMSAnxietyInhibition',
                                      'centredCEMSSadnessInhibition',
                                      'centredCEMSAngerInhibition',
                                      'centredCEMSAnxietyCoping',
                                      'centredCEMSSadnessCoping',
                                      'centredCEMSAngerCoping',
                                      'commentNaturenegative',
                                      'stakeSize0.1',
                                      'stakeSize1',
                                      'stakeSize10',
                                      'recodedDecisionreject',
                                      'prevRecodedDecisionreject',
                                      'genderfemale',
                                      'centredAge',
                                      '(Intercept)'
                           )))

#Figure 4(c)
plot.coef.p3_time <- ggplot(data=coef.p3_time,
                            aes(x=Estimate, 
                                y=Variable)) +  
  geom_vline(xintercept = 0, color = 'grey') +
  annotate("rect", 
           xmin = -Inf, 
           xmax = 0, 
           ymin = -Inf, 
           ymax = Inf, 
           fill = "grey", 
           alpha = 0.25) +
  geom_point(size = 2) +
  geom_text(aes(label= format(round(Estimate, digits = 2), nsmall = 2)),
            vjust = -0.5, size = 5) +
  geom_errorbar(aes(xmin = CI_L,
                    xmax = CI_U),
                width = 0.2) +
  geom_signif(stat="identity",
              data=data.frame(x = c(0.19, 0.19, 0.29, 0.51, 0.48, 0.28, 2.06),
                              y = c(1, 5, 6, 20, 21, 23, 24), 
                              annotation =c ("**", "***", "*", "*", "*", "***", "***")), 
              group = c(1:7),
              aes(x = x,
                  xend = x, 
                  y = y, 
                  yend = y, 
                  annotation = annotation, 
                  group = group,
                  textsize = 5)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size=15),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 15)) +
  scale_y_discrete(drop = FALSE) + 
  labs(title = 'c')

#Negative valence rating
coef.p3_feeling <- data.frame(cbind(summary(m.p3_feeling)$coefficients, m.p3_feeling.CI)) %>%
  tibble::rownames_to_column('Variable') %>%
  rename(SE = Std..Error) %>%
  rename(Tvalue = t.value) %>%
  rename(P = Pr...t..) %>%
  rename(CI_L = X2.5..) %>%
  rename(CI_U = X97.5..) %>%  
  mutate(Variable = factor(Variable, 
                           levels = c('commentNaturenegative:recodedDecisionreject',
                                     'stakeSize0.1:recodedDecisionreject',
                                     'stakeSize1:recodedDecisionreject',
                                     'stakeSize10:recodedDecisionreject',
                                     'prevRecodedDecisionreject:recodedDecisionreject',
                                     'centredAge:recodedDecisionreject',
                                     'centredCEMSAnxietyDysreg',
                                     'centredCEMSSadnessDysreg',
                                     'centredCEMSAngerDysreg',
                                     'centredCEMSAnxietyInhibition',
                                     'centredCEMSSadnessInhibition',
                                     'centredCEMSAngerInhibition',
                                     'centredCEMSAnxietyCoping',
                                     'centredCEMSSadnessCoping',
                                     'centredCEMSAngerCoping',
                                     'commentNaturenegative',
                                     'stakeSize0.1',
                                     'stakeSize1',
                                     'stakeSize10',
                                     'recodedDecisionreject',
                                     'prevRecodedDecisionreject',
                                     'genderfemale',
                                     'centredAge',
                                     '(Intercept)'
                           )))

#Figure 5(c)
plot.coef.p3_feeling <- ggplot(data=coef.p3_feeling,
                               aes(x=Estimate, 
                                   y=Variable)) +  
  geom_vline(xintercept = 0, color = 'grey') +
  annotate("rect", 
           xmin = -Inf, 
           xmax = 0, 
           ymin = -Inf, 
           ymax = Inf, 
           fill = "grey", 
           alpha = 0.25) +
  geom_point(size = 2) +
  geom_text(aes(label= format(round(Estimate, digits = 2), nsmall = 2)),
            vjust = -0.5, size = 5) +
  geom_errorbar(aes(xmin = CI_L,
                    xmax = CI_U),
                width = 0.2) +
  geom_signif(stat="identity",
              data=data.frame(x = c(0.15, 0.36, 1.73, 0.89, 0.6, 2.79),
                              y = c(5, 6, 20, 21, 23, 24), 
                              annotation =c ("**", "*", "***", "***", "*", "***")), 
              group = c(1:6),
              aes(x = x,
                  xend = x, 
                  y = y, 
                  yend = y, 
                  annotation = annotation, 
                  group = group,
                  textsize = 5)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y=element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size=15),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 15)) +
  scale_y_discrete(drop = FALSE) + 
  labs(title = 'c')

#Figure 3
plot.coef.p1_decision + plot.coef.p2_decision + plot.coef.p3_decision

#Figure 4
plot.coef.p1_time + plot.coef.p2_time + plot.coef.p3_time

#Figure 5
plot.coef.p1_feeling + plot.coef.p2_feeling + plot.coef.p3_feeling
