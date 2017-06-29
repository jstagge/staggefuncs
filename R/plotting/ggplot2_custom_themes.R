theme_pub_majgrid <- function(base_size = 10) {
  theme( 
    axis.line =          element_line(),
    axis.text.x =        element_text(size = base_size * 0.8, lineheight = 0.9, vjust = 1, colour="black"),
    axis.text.y =        element_text(size = base_size * 0.8, lineheight = 0.9, hjust = 1, colour="black"),
    axis.ticks =         element_line(colour = "black", size = 0.2),
    axis.title.x =       element_text(size = base_size, vjust = -0.5, hjust=0.5),
    axis.title.y =       element_text(size = base_size, angle = 90, vjust = 0.2),
    axis.ticks.length =  unit(0.15, "cm"),
    axis.ticks.margin =  unit(0.1, "cm"),

    legend.background =  element_rect(colour = NA),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         element_rect(colour = NA),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = base_size * 0.8),
    legend.text.align =  NULL,
    legend.title =       element_text(size = base_size * 0.8, face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,

    panel.background =   element_blank(),
    panel.border =       element_blank(),
    panel.grid.major =   element_line(colour = "grey90", size = 0.2),
    panel.grid.minor =   element_blank(),
	#panel.grid.minor = element_line(colour = "grey98", size = 0.5),
    panel.margin =       unit(0.25, "lines"),
	
    strip.background =   element_rect(fill = NA, colour = NA),
    strip.text.x =       element_text(size = base_size * 0.8),
    strip.text.y =       element_text(size = base_size * 0.8, angle = -90),

    plot.background =    element_blank(),
    plot.title =         element_text(size = base_size * 1.2, face="bold"),
    plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines"),

    complete = FALSE
  )
}



theme_pub_grid <- function(base_size = 10) {
  theme( 
    axis.line =          element_line(),
    axis.text.x =        element_text(size = base_size * 0.8, lineheight = 0.9, vjust = 1,colour = "black"),
    axis.text.y =        element_text(size = base_size * 0.8, lineheight = 0.9, hjust = 1,colour = "black"),
    axis.ticks =         element_line(colour = "black", size = 0.2),
    axis.title.x =       element_text(size = base_size, vjust = -0.5, hjust=0.5),
    axis.title.y =       element_text(size = base_size, angle = 90, vjust = 0.2),
    axis.ticks.length =  unit(0.15, "cm"),
    axis.ticks.margin =  unit(0.1, "cm"),

    legend.background =  element_rect(colour = NA),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         element_rect(colour = NA),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = base_size * 0.8),
    legend.text.align =  NULL,
    legend.title =       element_text(size = base_size * 0.8, face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,

    panel.background =   element_blank(),
    panel.border =       element_blank(),
    panel.grid.major =   element_line(colour = "grey90", size = 0.2),
    #panel.grid.minor =   element_blank(),
	panel.grid.minor = element_line(colour = "grey98", size = 0.5),
    panel.margin =       unit(0.25, "lines"),
		
    strip.background =   element_rect(fill = NA, colour = NA),
    strip.text.x =       element_text(size = base_size * 0.8),
    strip.text.y =       element_text(size = base_size * 0.8, angle = -90),

    plot.background =    element_blank(),
    plot.title =         element_text(size = base_size * 1.2, face="bold"),
    plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines"),

    complete = FALSE
  )
}


theme_pub_nogrid <- function(base_size = 10) {
  theme( 
    axis.line =          element_line(),
    axis.text.x =        element_text(size = base_size * 0.8, lineheight = 0.9, vjust = 1,colour = "black"),
    axis.text.y =        element_text(size = base_size * 0.8, lineheight = 0.9, hjust = 1,colour = "black"),
    axis.ticks =         element_line(colour = "black", size = 0.2),
    axis.title.x =       element_text(size = base_size, vjust = -0.5, hjust=0.5),
    axis.title.y =       element_text(size = base_size, angle = 90, vjust = 0.2),
    axis.ticks.length =  unit(0.15, "cm"),
    axis.ticks.margin =  unit(0.1, "cm"),

    legend.background =  element_rect(colour = NA),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         element_rect(colour = NA),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = base_size * 0.8),
    legend.text.align =  NULL,
    legend.title =       element_text(size = base_size * 0.8, face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,

    panel.background =   element_blank(),
    panel.border =       element_blank(),
    panel.grid.major =   element_blank(),
    panel.grid.minor =   element_blank(),
	#panel.grid.minor = element_line(colour = "grey98", size = 0.5),
    panel.margin =       unit(0.05, "lines"),
		
    strip.background =   element_rect(fill = NA, colour = NA),
    strip.text.x =       element_text(size = base_size * 0.8),
    strip.text.y =       element_text(size = base_size * 0.8, angle = -90),

    plot.background =    element_blank(),
    plot.title =         element_text(size = base_size * 1.2, face="bold"),
    plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines"),

    complete = FALSE
  )
}

#, base_family="GillSans"

theme_pub_map <- function(base_size = 10) {
  theme( 
    axis.line =          element_line(),
    axis.text.x =        element_text(size = base_size * 0.8, lineheight = 0.9, vjust = 1,colour = "black"),
    axis.text.y =        element_text(size = base_size * 0.8, lineheight = 0.9, hjust = 1,colour = "black"),
    axis.ticks =         element_line(colour = "black", size = 0.2),
    axis.title.x =       element_blank(),
    axis.title.y =       element_blank(),
    axis.ticks.length =  unit(0.15, "cm"),
    axis.ticks.margin =  unit(0.1, "cm"),

    legend.background =  element_rect(colour = NA),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         element_rect(colour = NA),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = base_size * 0.8),
    legend.text.align =  NULL,
    legend.title =       element_text(size = base_size * 0.8, face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,

    panel.background =   element_blank(),
    panel.border =       element_rect(colour = "black", fill=NA),
    panel.grid.major =   element_line(colour = "grey50", size = 0.25,linetype="dotted" ),
    panel.grid.minor =   element_blank(),
		
    strip.background =   element_rect(fill = NA, colour = NA),
    strip.text.x =       element_text(size = base_size * 0.8),
    strip.text.y =       element_text(size = base_size * 0.8, angle = -90),

    plot.background =    element_blank(),
    plot.title =         element_text(size = base_size * 1.2, face="bold"),
    plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines"),

    complete = FALSE
  )
}

   axis.text.x =        element_text(size = base_size * 0.8, lineheight = 0.9, vjust = 1,colour = "black", margin=unit(0.1, "cm")),
    axis.text.y =        element_text(size = base_size * 0.8, lineheight = 0.9, hjust = 1,colour = "black", margin=unit(0.1, "cm")),

element_text(margin=margin(5,5,10,5,"pt")

element_text(size = base_size * 0.8, lineheight = 0.9, vjust = 1,colour = "black", margin=margin(0.1,0.1,0.1,0.1,"cm")),

theme_pub_map_grey <- function(base_size = 10) {
  theme( 
    axis.line =          element_line(),
    axis.text.x =        element_text(size = base_size * 0.8, lineheight = 0.9, vjust = 1,colour = "black", margin=margin(0.1,0.1,0.1,0.1,"cm")),
    axis.text.y =        element_text(size = base_size * 0.8, lineheight = 0.9, hjust = 1,colour = "black", margin=margin(0.1,0.1,0.1,0.1,"cm")),
    axis.ticks =         element_line(colour = "black", size = 0.2),
    axis.title.x =       element_blank(),
    axis.title.y =       element_blank(),
    axis.ticks.length =  unit(0.15, "cm"),

    legend.background =  element_rect(colour = NA),
    legend.margin =      margin(0.2,0.2,0.2,0.2, unit="cm"),
    legend.key =         element_rect(colour = NA),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = base_size * 0.8),
    legend.text.align =  NULL,
    legend.title =       element_text(size = base_size * 0.8, face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,

    panel.background =   element_rect(fill = "grey95", colour=NA),
    panel.border =       element_rect(colour = "black", fill=NA),
    panel.grid.major =   element_line(colour = "grey50", size = 0.25,linetype="dotted" ),
    panel.grid.minor =   element_blank(),
		
    strip.background =   element_rect(fill = NA, colour = NA),
    strip.text.x =       element_text(size = base_size * 0.8),
    strip.text.y =       element_text(size = base_size * 0.8, angle = -90),

    plot.background =    element_blank(),
    plot.title =         element_text(size = base_size * 1.2, face="bold"),
    plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines"),

    complete = FALSE
  )
}
