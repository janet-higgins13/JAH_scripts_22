library("rgl")

data <- read.csv("../no_adm_1605_IndicaForPlot.csv")

x  <- data$V1
y  <- data$V4
z  <- data$V5

##rgl_init(): A custom function to initialize RGL device
rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}

#Use c(-max, max)
#Let’s define a helper function to calculate the axis limits:
  
lim <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}


# the background color; the second color is the color of tick marks
rgl_add_axes <- function(x, y, z, axis.col = "grey",
                         xlab = "", ylab="", zlab="", show.plane = TRUE, 
                         show.bbox = FALSE, bbox.col = c("#333377","black"))
{ 
  
  
  # Add axes
  xlim <- lim(x); ylim <- lim(y); zlim <- lim(z)
  rgl.lines(xlim, c(0, 0), c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), ylim, c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), c(0, 0), zlim, color = axis.col)
  
  # Add a point at the end of each axes to specify the direction
  axes <- rbind(c(xlim[2], 0, 0), c(0, ylim[2], 0), c(0, 0, zlim[2]))
  rgl.points(axes, color = axis.col, size = 3)
  
  # Add axis labels
  rgl.texts(axes, text = c(xlab, ylab, zlab), color = axis.col,adj = c(0.5, -0.8), size = 2)
  
}




groups <- data$plot_Subpopulation
table(groups)
groups
I1      I2      I3      I4      I5      Im notViet 
145      90      37      62      42      48    1181 

levs <- levels(groups)
group.col <- c("blue","darkorchid","forestgreen","darkorange","red","grey90","tan")
# Plot observations
rgl_init()
#rgl.points(x, y, z, color = group.col[as.numeric(groups)], size = 3.0) # Scatter plot
rgl.spheres(x, y, z, r = 0.8,color = group.col[as.numeric(groups)]) 
rgl_add_axes(x, y, z, show.bbox = FALSE)
# Compute ellipse for each group
for (i in 1:length(levs)) {
  group <- levs[i]
  selected <- groups == group
  xx <- x[selected]; yy <- y[selected]; zz <- z[selected]
  ellips <- ellipse3d(cov(cbind(xx,yy,zz)), 
                      centre=c(mean(xx), mean(yy), mean(zz)), level = 0.95) 
  shade3d(ellips, col = group.col[i], alpha = 0.1, lit = FALSE) 
}
aspect3d(1,1,1)

parent <- currentSubscene3d()

legend3d("topright",legend= c("I1","I2","I3","I4","I5","Im","not Vietnam"),
         col=c("blue","darkorchid","forestgreen","darkorange","red","grey90","tan"),cex=1.5,pch=16,
         box.lty=1, box.col="black")

useSubscene3d(parent)

rgl.snapshot(filename = "plot_subpops_PC145.png")

writeWebGL(dir = "webGL", filename = "Subpops_index_PC145.html")





groups <- data$plot_K15_new
levs <- levels(groups)
group.col <- c("darkred","darkred","brown","tan1","magenta","royalblue","cyan","green","navy","grey99")
# Plot observations
rgl_init()
rgl.spheres(x, y, z, r = 0.8,color = group.col[as.numeric(groups)]) 
rgl_add_axes(x, y, z, show.bbox = FALSE)
#rgl.bg(color = "black") # Setup the background color
# Compute ellipse for each group
for (i in 1:length(levs)) {
  group <- levs[i]
  selected <- groups == group
  xx <- x[selected]; yy <- y[selected]; zz <- z[selected]
  ellips <- ellipse3d(cov(cbind(xx,yy,zz)), 
                      centre=c(mean(xx), mean(yy), mean(zz)), level = 0.95) 
  shade3d(ellips, col = group.col[i], alpha = 0.1, lit = FALSE) 
}
aspect3d(1,1,1)

parent <- currentSubscene3d()

legend3d("topright",legend= c("Ind-1A","Ind-1B.1","Ind-1B.2","Ind-2","Ind-3.1","Ind-3.2","Ind-3.3","Ind-3.4","Ind-adm"),
         col=c("darkred","brown","tan1","magenta","royalblue","cyan","green","navy","grey"),cex=1.5,pch=16,
         box.lty=1, box.col="black")

useSubscene3d(parent)


rgl.snapshot(filename = "bruno_K15_PC145.png")

writeWebGL(dir = "webGL", filename = "bruno_K15_PC145_index.html")




groups <- data$plot_K15_IRRI 
new  XI-1A XI-1B1 XI-1B2  XI-2A  XI-2B  XI-3A XI-3B1 XI-3B2 XI-adm 
377    195     64     63    137    101    180    259     87    142
levs <- levels(groups)
group.col <- c("gold","darkred","brown","tan1","magenta","orchid","royalblue","darkturquoise","blue4","grey99")
# Plot observations
rgl_init()
rgl.spheres(x, y, z, r = 0.8,color = group.col[as.numeric(groups)]) 
rgl_add_axes(x, y, z, show.bbox = FALSE)
#rgl.bg(color = "black") # Setup the background color
# Compute ellipse for each group
for (i in 1:length(levs)) {
  group <- levs[i]
  selected <- groups == group
  xx <- x[selected]; yy <- y[selected]; zz <- z[selected]
  ellips <- ellipse3d(cov(cbind(xx,yy,zz)), 
                      centre=c(mean(xx), mean(yy), mean(zz)), level = 0.95) 
  shade3d(ellips, col = group.col[i], alpha = 0.1, lit = FALSE) 
}
aspect3d(1,1,1)


parent <- currentSubscene3d()

legend3d("topright",legend= c("New","XI-1A","XI-1B1","XI-1B2","XI-2A","XI-2B","XI-3A","XI-3B1","XI-3B2","XI-adm"),
         col=c("gold","darkred","brown","tan1","magenta","orchid","royalblue","darkturquoise","blue4","grey"),cex=1.5,pch=16,
         box.lty=1, box.col="black")
useSubscene3d(parent)

rgl.snapshot(filename = "IRRI_K15_PC145.png")

writeWebGL(dir = "webGL", filename = "IRRI_K15__PC145_index.html")



groups <- data$plot_Subpopulation
# show axes colours
rgl_init()
#rgl.spheres(x, y, z, r = 0.8, color = "yellow") 
rgl.spheres(x, y, z, r = 0.8,color = group.col[as.numeric(groups)]) 
# Add x, y, and z Axes
rgl.lines(c(min(x), max(x)), c(0, 0), c(0, 0), color = "black")
rgl.lines(c(0, 0), c(min(y),max(y)), c(0, 0), color = "red")
rgl.lines(c(0, 0), c(0, 0), c(min(z),max(z)), color = "green")

rgl.snapshot(filename = "subpops_xyz_PC145_ind.png")
writeWebGL(filename = "subpops_xyz_PC145_jap.html")
