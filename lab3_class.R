#install.packages('modelsummary')
library(tidyverse)
library(modelsummary)

set.seed(13)
xvar <- runif(n = 100, min = 0, max = 50)
error <- rnorm(n=100, mean = 0, sd = 30)

yvar <- 25 - 1.2*xvar + error

fake <- tibble(xvar, yvar)

plot(fake$xvar, fake$yvar)
cov(fake$xvar, fake$yvar)
cor(fake$xvar, fake$yvar)
cor.test(fake$xvar, fake$yvar)

m1 <- lm(yvar ~ xvar, data = fake)
summary(m1)

modelsummary(m1, title = "OLS Results", 
                 notes = "Warning: Fake data.")

plot1 <- ggplot(data = fake, mapping = aes(x = xvar, y = yvar)) +
            geom_point(color = "steelblue", size = 3, alpha = .3) +
            geom_smooth(method = "lm", color = "black", alpha = .3)
plot1 + geom_smooth(method = "loess", fill = "orange", color = "orange", alpha = .2) +
        labs(title = "Scatterplot with Regression Line",
             x = "X",
             y = "Y") +
        theme_minimal()
