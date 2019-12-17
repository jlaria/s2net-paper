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
posthoc.friedman.nemenyi.test(df.Box)
# Paired t.test to compare
t.test(x = df.Box[,1], y = df.Box[, 3], alternative = "greater", paired = T)


library(ggplot2)  
library(gridExtra)

g1 = results %>% filter(method %in% c("baseline", "glmnet", "s²net", "JT")) %>%
  ggplot() + 
  geom_boxplot(aes( x = method, y = test.loss))+
  scale_y_continuous(limits = c(0.25, 0.4))+
  theme_classic()+
  labs(x = "", y = "Test MSE")+
  coord_flip()

ggplot(results) + 
  geom_density(aes(x = test.loss, group = method, color = method, linetype = method))+
  scale_x_continuous(limits = c(0.1, 0.5))+
  theme_bw()+
  annotation_custom(tableGrob(df, rows=NULL), 
                    xmin=0.3, xmax=0.5, ymin=10, ymax=30)+
  annotation_custom(ggplotGrob(g1), xmin = 0.1, xmax = 0.25, ymin = 10, ymax = 30)+
  labs(x = "Test MSE")


