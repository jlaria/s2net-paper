# evaluate 
library(tidyr)
library(dplyr)

df = results[,-3] %>% 
  group_by(method, n_target, n_vars, scenario) %>% summarise(Test = mean(test.auc)*100) %>% 
  unite(scenario, scenario,  n_vars, n_target, sep = ",") %>% spread(key = c("scenario"), value = Test)
df

df.Box = results %>% select(N, method, n_target, n_vars, test.auc, scenario) %>% 
  filter(n_target == 250, scenario == "unlucky") %>% 
  spread(method, test.auc) %>% select(-N, -n_target, -n_vars, -scenario) %>% as.matrix()

library(PMCMR)
# This test say whether one of the methods is different
friedman.test(df.Box)
# This test test the differences between methods (2 by 2) 
posthoc.friedman.conover.test(df.Box)
# Paired t.test to compare
t.test(x = df.Box[,1], y = df.Box[, 3], alternative = "greater", paired = T)

df1 = as.matrix(df[,-1])
df1 = t(t(df1)/df1[2,]-1)
df[-2,-1] = round(df1[-2,]*100, 1)
df 
  

# plot data =================================================================================

library(s2net)
library(reshape2)
library(ggplot2)
library(ggpubr)

data = simulate_groups(50, 50, 200, response = "logit")
data = s2Data(data$xL, data$yL, data$xU)
d1 = melt(data$xL)
d2 = melt(data$xU)
gg1 = ggplot() + geom_line(data = d1, aes(x = Var2, y = value, group = Var1, color = "source"), alpha = 0.06) +
  geom_line(data = d2, aes(x = Var2, y = value, group = Var1, color = "target"), alpha = 0.02) + 
  theme_bw() + 
  scale_color_manual(values = c("black", "red")) +
  labs(x = "variable", y = "value", color = "type") + 
  theme(legend.position = "top")
gg1
# data = simulate(p = 200, nS.train = 50, nT.train = 200)

pc = stats::prcomp(rbind(data$xL, data$xU))
R = pc$rotation[,1:2]
Xl = scale(data$xL, center = pc$center, scale = pc$scale)
Xl = Xl %*% R

Xu = scale(data$xU, center = pc$center, scale = pc$scale)
Xu = Xu %*% R

df = rbind(data.frame(Xl, type = "source"), data.frame(Xu, type = "target"))
colnames(df) = c("PC1", "PC2", "type")
gg2 = ggplot(df)+
  geom_point(ggplot2::aes(PC1, PC2, color = type, shape = type)) +
  labs(x = "PC1", y = "PC2") + 
  scale_color_manual(values = c("black", "red")) +
  theme_bw()
gg2

ggarrange(gg2, gg1, common.legend = T)
