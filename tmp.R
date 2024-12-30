library(dplyr)
library(ggplot2)

# pls write simple code to demostrate data visualization based mtcars dataset

## 1. point plot of mpg vs cyl
ggplot(data = mtcars, aes(x = cyl, y = mpg)) +
  geom_point() +
  labs(title = "Point plot of mpg vs cyl", x = "cyl", y = "mpg")
