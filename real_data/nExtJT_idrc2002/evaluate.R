# evaluate 
library(tidyr)
library(dplyr)

levels(results$method) -> lev
lev[3] = "s²net"
levels(results$method) <- lev


df = results %>% group_by(method) %>% summarise(MSE = round(mean(test.loss),4), sd = round(sd(test.loss), 3))
df

df.Box = results %>% select(N, method, test.loss) %>% 
  spread(method, test.loss) %>% select(-N) %>% as.matrix()




library(PMCMR)
# This test say whether one of the methods is different
friedman.test(df.Box)
# This test test the differences between methods (2 by 2) 
posthoc.friedman.conover.test(df.Box)
# Paired t.test to compare
t.test(x = df.Box[,1], y = df.Box[, 3], alternative = "greater", paired = T)

library(ggplot2)  
library(gridExtra)

g1 = results %>% filter(method %in% c("baseline", "glmnet", "s²net", "JT")) %>%
  ggplot() + 
  geom_boxplot(aes( x = method, y = test.loss))+
  scale_y_continuous(limits = c(0.03, 0.15))+
  theme_minimal()+
  labs(x = "", y = "Test MSE")+
  coord_flip()

ggplot(results) + 
  geom_density(aes(x = test.loss, group = method, color = method, linetype = method))+
  scale_x_continuous(limits = c(0, 0.3))+
  theme_bw()+
  annotation_custom(tableGrob(df, rows=NULL), 
                    xmin=0.15, xmax=0.3, ymin=12, ymax=23)+
  annotation_custom(ggplotGrob(g1), xmin = 0.15, xmax = 0.3, ymin = 0, ymax = 12)+
  labs(x = "Test MSE")
