# evaluate 
library(tidyr)
library(dplyr)


levels(results$method) -> lev
lev[3] = "s²net"
levels(results$method) <- lev


df = results %>% group_by(method) %>% summarise(ACC = round(mean(test.acc), 4), sd = round(sd(test.acc), 3)) 
df

df.Box = results %>% select(N, method, test.loss) %>% 
  spread(method, test.loss) %>% select(-N) %>% as.matrix()




library(PMCMR)
# This test say whether one of the methods is different
friedman.test(df.Box)
# This test test the differences between methods (2 by 2) 
posthoc.friedman.conover.test(df.Box)
posthoc.friedman.nemenyi.test(df.Box)
# Paired t.test to compare
t.test(x = df.Box[,1], y = df.Box[, 3], alternative = "greater", paired = T)


library(ggplot2)  
library(gridExtra)
library(ggpubr)

g1 = results %>% filter(method %in% c("baseline", "glmnet", "s²net")) %>%
  ggplot() + 
  geom_boxplot(aes( x = method, y = test.acc))+
  scale_y_continuous(limits = c(0.5, 0.95))+
  theme_classic2()+
  labs(x = "", y = "Test ACC")+
  coord_flip()

ggplot(results) + 
  geom_density(aes(x = test.acc, group = method, color = method, linetype = method))+
  scale_x_continuous(limits = c(0.3, 0.95))+
  theme_bw()+
  annotation_custom(tableGrob(df, rows=NULL), 
                    xmin=0.3, xmax=0.5, ymin=0, ymax=6)+
  annotation_custom(ggplotGrob(g1), xmin = 0.3, xmax = 0.7, ymin = 7, ymax = 12)+
  labs(x = "Test ACC")


