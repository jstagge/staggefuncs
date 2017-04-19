# The palette with grey:
cbPalette_8 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette_8 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

### Colorbline palette for 4 or fewer colors
pal_cb_4_few <- c("#4477AA", "#EE6677", "#228833", "#CCBB44")

### Colorbline palette for 5 or more colors
pal_cb_5_more <- c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377", "#BBBBBB", "black")



cols <- 8

x <- rep(seq(1,20),cols)
variable <- paste0("Name ", rep(1:cols, each=20))
value <- rnorm(20*cols) + rep(seq(0,2,2/(cols-1)), each=20)

df <- data.frame(x, variable, value )

d <- ggplot(df, aes(x=x, y=value, group=variable, colour=variable ) ) + 
            geom_line(size=1) + theme_bw()
d + scale_colour_manual(values=pal_cb_5_more)





