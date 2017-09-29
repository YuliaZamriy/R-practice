rm(list=ls())
setwd("C:/Users/yzamriy/Documents/Tools and Methodology/DS/Powerlifting/CSV")

library(tidyverse)
library(ggplot2)


g <- ggplot(data = mpg)
    g + geom_point(mapping = aes(x = displ, y = hwy, size = class))
    g + geom_point(mapping = aes(x = displ, y = hwy, shape = class))
    g + geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
    g + geom_point(mapping = aes(x = displ, y = hwy), color = "blue")
    g + geom_point(mapping = aes(x = displ, y = hwy, color = cty))
    g + geom_point(mapping = aes(x = displ, y = hwy, size = cty))
    g + geom_point(mapping = aes(x = displ, y = hwy, shape = cty)) # A continuous variable can not be mapped to shape
    g + geom_point(mapping = aes(x = displ, y = hwy, size = cty, color = cty))
    g + geom_point(mapping = aes(x = displ, y = hwy, size = displ, color = cty))
    g + geom_point(mapping = aes(x = displ, y = hwy), shape = 21, stroke = 2)
    g + geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))
    
    g + geom_point(mapping = aes(x = displ, y = hwy)) +
        facet_wrap(~class, nrow = 2)
    g + geom_point(mapping = aes(x = displ, y = hwy)) +
        facet_grid(drv~cyl)
    g + geom_point(mapping = aes(x = displ, y = hwy)) +
        facet_grid(.~cyl)
    g + geom_point(mapping = aes(x = displ, y = hwy)) +
        facet_grid(.~cty)
    
    g + geom_point(mapping = aes(x = displ, y = hwy))
    g + geom_smooth(mapping = aes(x = displ, y = hwy))

    g + geom_smooth(mapping = aes(x = displ, y = hwy))
    g + geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
    g + geom_smooth(mapping = aes(x = displ, y = hwy, color = drv), show.legend = FALSE)
    
    g + geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv)) +
        geom_point(mapping = aes(x = displ, y = hwy, color = drv))

g <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy))
    g + geom_point() +
        geom_smooth()
    g + geom_point(mapping = aes(color = class)) +
        geom_smooth()
    g + geom_point(mapping = aes(color = class)) +
        geom_smooth(
            data = filter(mpg, class == 'subcompact'), 
            se = FALSE
        )
    
g <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv))
    g + geom_point() +
        geom_smooth(se = FALSE)

g <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy))
    g + geom_point(size = 4) +
        geom_smooth(se = FALSE)
    g + geom_point(size = 4) +
        geom_smooth(mapping = aes(group = drv),
                    se = FALSE,
                    show.legend = FALSE)

g <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv))
    g + geom_point(size = 4) +
        geom_smooth(se = FALSE)

g <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy))
    g + geom_point(mapping = aes(color = drv),
                   size = 4) +
        geom_smooth(se = FALSE)
    
g <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy))
    g + geom_point(mapping = aes(color = drv),
                   size = 4) +
        geom_smooth(mapping = aes(linetype = drv),
                    se = FALSE)

g <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv))
    g + geom_point(size = 4)
    
g <- ggplot(data = diamonds)
    g + geom_bar(mapping = aes(x = cut))
    g + stat_count(mapping = aes(x = cut))
    
demo <- tribble(
    ~a,      ~b,
    "bar_1", 20,
    "bar_2", 30,
    "bar_3", 40
)    

ggplot(data = demo) +
    geom_bar(
        mapping = aes(x = a, y = b), stat = "identity"
    )
ggplot(data = demo) +
    geom_col(mapping = aes(x = a, y = b))

g <- ggplot(data = diamonds)
g + geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))
    g + geom_bar(mapping = aes(x = cut, y = ..prop..))
    g + geom_bar(mapping = aes(x = cut, fill = color, y = ..prop..))
    
    g + stat_summary(
        mapping = aes(x = cut, y = depth),
        fun.ymin = min,
        fun.ymax = max,
        fun.y = median
        )
    g + geom_pointrange(mapping = aes(x = cut, y = depth),
                        stat = "summary",
                        fun.ymin = min,
                        fun.ymax = max,
                        fun.y = median)
    
    g + geom_bar(mapping = aes(x = cut, color = cut))
    g + geom_bar(mapping = aes(x = cut, fill = cut))
    g + geom_bar(mapping = aes(x = cut, fill = clarity))
    
ggplot(data = diamonds, mapping = aes(x = cut, color = clarity)) +
    geom_bar(fill = NA, position = "identity")

g <- ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity))
    g + geom_bar(alpha = 1/5, position = "identity")
    g + geom_bar(position = "fill")
    g + geom_bar(position = "dodge")

g <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy))
    g + geom_point()
    g + geom_point(position = 'jitter')
    g + geom_jitter()

g <- ggplot(data = mpg, mapping = aes(x = cty, y = hwy))
    g + geom_point(color = 'blue') + geom_jitter(color = 'red')
    g + geom_count()

g <- ggplot(data = mpg, mapping = aes(x = class, y = hwy))
    g + geom_boxplot()
    g + geom_boxplot() + geom_jitter(width = 0.2)
    g + geom_boxplot() + coord_flip()
    g + geom_boxplot(notch = TRUE)
    g + geom_boxplot(varwidth = TRUE)
    g + geom_boxplot(aes(colour = drv))

install.packages("maps")    
install.packages("mapproj")    
library(maps)
library(mapproj)
nz <- map_data("nz")
z <- ggplot(nz, aes(long, lat, group = group))
    z + geom_polygon(fill = 'white', color = 'black')
    z + geom_polygon(fill = 'white', color = 'black') + coord_quickmap()
    z + geom_polygon(fill = 'white', color = 'black') + coord_map()
    
bar <- ggplot(data = diamonds) +
        geom_bar(mapping = aes(x = cut, fill = cut),
                 show.legend = FALSE,
                 width = 1) +
        theme(aspect.ratio = 1) +
        labs(x = NULL, y = NULL)
bar + coord_flip()
bar + coord_polar()

g <- ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity))
    g + geom_bar() +
        coord_polar()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
    geom_point() +
    geom_abline() +
    coord_fixed()

