library(tidyverse)
?mpg
mpg
print(mpg, n=15)
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy, color = class, size = cyl, alpha = class,
                           shape = drv))
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy)) + 
               facet_wrap(~class, nrow=2)

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~class)


ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv~cyl)
