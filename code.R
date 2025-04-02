## -----------------------------------------------------------------------------------------
#| echo: false
#| message: false
#| warning: false

knitr::opts_chunk$set(fig.align = 'center')
library(tidyverse)


## flowchart TB
##   subgraph One Replication
##   A[Factor A] --> C{All combinations}
##   B[Factor B] --> C
##   C --> D[Randomize Order]
##   D --> F[Experiment]
##   end

## -----------------------------------------------------------------------------------------
#| fig-align: center
#| fig-cap: "Calculation of p-value under an F distribution."
#| echo: false

num.df = 10
den.df = 50
f.alpha = 0.05

tibble(x = seq(0, 4, l = 100)) %>% 
ggplot(mapping = aes(x = x)) + 
  stat_function(fun = \(x)(df(ifelse(x > qf(f.alpha, 
                                            num.df, 
                                            den.df, lower.tail = F), x, NA), num.df, den.df)), 
                geom = 'area', fill = 'red') + 
  stat_function(fun = \(x)(df(x, num.df, den.df)), geom = 'line') + 
  theme_bw() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  labs(x = 'X', y = expression(f[x]),
       subtitle = 'P(F > F-statistic)',
       title = expression(F(df[term], df[error]))) + 
  theme(aspect.ratio = 1/2,
        axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 0),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())


## -----------------------------------------------------------------------------------------
#| echo: true
agriculture <- read.csv('data/example1-agriculture.csv')
head(agriculture)


## -----------------------------------------------------------------------------------------
#| echo: true

with(agriculture, interaction.plot(brand, fertilizer, height))
with(agriculture, interaction.plot(fertilizer, brand, height))


## -----------------------------------------------------------------------------------------
#| echo: true


model1 <- lm(height ~ fertilizer*brand,
             data = agriculture)
model1

# aov(height ~ fertilizer*brand,
#              data = agriculture)


## -----------------------------------------------------------------------------------------
#| echo: true


summary(model1)


## -----------------------------------------------------------------------------------------
#| warning: false
#| echo: true

anova(model1)

library(car)
Anova(model1, type = 3)


## -----------------------------------------------------------------------------------------
#| echo: true


library(emmeans)
em1 <- emmeans(model1, ~brand*fertilizer)
em1

emmeans(model1, ~fertilizer|brand)


## -----------------------------------------------------------------------------------------
#| echo: true

pairs(em1)
pairs(em1, infer = c(T,T))


## -----------------------------------------------------------------------------------------
#| layout-ncol: 2
#| layout-nrow: 2
#| echo: true

plot(model1)


## -----------------------------------------------------------------------------------------
bacteria <- read.csv('data/example2-bacteria.csv')
head(bacteria)


## -----------------------------------------------------------------------------------------
#| echo: true
with(bacteria, interaction.plot(ph, salt, logcfu))
with(bacteria, interaction.plot(salt, ph, logcfu))


## -----------------------------------------------------------------------------------------
#| echo: true
model2a <- lm(logcfu ~ factor(ph)*factor(salt), 
              data = bacteria)


## -----------------------------------------------------------------------------------------
#| echo: true
model2b <- lm(logcfu ~ (ph + I(ph^2))*(salt + I(salt^2)),
              data = bacteria)


## -----------------------------------------------------------------------------------------
summary(model2b)


## -----------------------------------------------------------------------------------------
anova(model2b)


## -----------------------------------------------------------------------------------------
em2 <- emmeans(model2b, ~ph*salt)
em2


## -----------------------------------------------------------------------------------------
em2b <- emmeans(model2b, ~ph*salt,
                at = list(ph = c(6, 7, 8),
                          salt = c(15, 20, 25)))
em2b


## -----------------------------------------------------------------------------------------
pairs(em2b)


## -----------------------------------------------------------------------------------------
#| layout-ncol: 2
#| layout-nrow: 2


plot(model2b)


## -----------------------------------------------------------------------------------------
#| eval: false
# options(contrasts = c("contr.sum", "contr.poly"))

