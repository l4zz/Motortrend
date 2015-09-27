# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

library(ggplot2)
# library(GGally)
data(mtcars)
?mtcars

# mpg --> Miles/US gallon
# cyl --> nbr cylinders
# disp --> Displacement (cu. in.)
# hp
# drat
# wt --> weight (lb/1000)
# qsec --> 1/4 mile time
# vs --> engine configuration (v/straight)
# am --> Transmission (0 automatic, 1 manual)
# gear --> nbr forward gears
# carb --> nbr carburetors

cars <- mtcars
cars$transmission <- "automatic"
cars[cars$am == 1,"transmission"] = "manual"
cars$transmission <- as.factor(cars$transmission)
# conversion to Litres (1 cu.in == 0.016 l)
cars$disp <- cars$disp * 0.016
cars$vs_ <- "V"
cars[cars$vs == 1,"vs_"] = "S"
cars$vs_ <- as.factor(cars$vs_)

hist(cars$disp)
head(cars)
# Quantifying the difference between manual and automatic
# H0: mu_mpg(manual) = mu_mgp(automatic)
t.test(cars[cars$transmission=="manual", "mpg"],
       cars[cars$transmission=="automatic", "mpg"],
       two.tailed=TRUE,
       paired=FALSE)
# exploratory plots
p1 <- ggplot(cars, aes(x=disp,y=mpg)) + geom_point(aes(colour=transmission))
p1 <- p1 + ggtitle("MPG variation with engine size")+ labs(x="Engine size (litres)", y="MPG (US gallon)")
p2 <- ggplot(cars, aes(x=carb,y=mpg)) + geom_point(aes(colour=transmission))
p2 <- p2 + ggtitle("MPG variation with the number of carburetors") + labs(x="Number of carburetors", y="MPG (US gallon)")
p4 <- ggplot(cars, aes(x=vs_,y=mpg)) + geom_boxplot(aes(colour=transmission))
p4 <- p4 + ggtitle("MPG distribution by engine configuration") + labs(x="V-shaped engine / Straight engine", y="MPG (US gallon)")
p5 <- ggplot(cars, aes(x=transmission, y=mpg)) + geom_boxplot(aes(colour=transmission))
p5 <- p5 + ggtitle("MPG distribution by transmission type") + labs(x="Transmission type", y="MPG (US gallon)")
p6 <- ggplot(cars, aes(x=wt, y=mpg)) + geom_point(aes(colour=transmission))
p6 <- p6 + ggtitle("MPG distribution by weight") + labs(x="Weight (lb/1000)", y="MPG (US gallon)")
p7 <- ggplot(cars, aes(x=as.factor(cyl), y=mpg)) + geom_boxplot(aes(colour=transmission))
p7 <- p7 + ggtitle("MPG distribution by number of cylinders") + labs(x="Number of cylinders", y="MPG (US gallon)")
multiplot(p5, p1, p2, p6, p4, p7, cols=2)

# stats models
lm <- lm(mpg ~ transmission, data=cars)
summary(lm)

lma0 <- lm(mpg ~ wt + transmission, data=cars)
# this is the coef for manual transmission, holding wt as fixed
a0coef <- round(summary(lma0)$coef[3],3)
a0rsqr <- round(summary(lma0)$r.squared,2)
lma1 <- lm(mpg ~ carb + transmission, data=cars)
# this is the coef for manual transmission, holding carb as fixed
a1coef <- round(summary(lma1)$coef[3],3)
a1rsqr <- round(summary(lma1)$r.squared,2)
lma2 <- lm(mpg ~ disp + transmission, data=cars)
# this is the coef for manual transmission, holding disp as fixed
a2coef <- round(summary(lma2)$coef[3],3)
a2rsqr <- round(summary(lma2)$r.squared,2)
top <- c("Coefficient", "R^2")
side <- c("wt", "carb", "disp")
var0 <- c(a0coef, a0rsqr)
var1 <- c(a1coef, a1rsqr)
var2 <- c(a2coef, a2rsqr)
df <- data.frame()
df <- rbind(var0, var1, var2)
colnames(df) <- top
rownames(df) <- side
df

lmb <- lm(mpg ~ wt + disp + carb + transmission, data=cars)
summary(lmb)
summary(lmb)$call
summary(lmb)$coef
round(summary(lmb)$r.squared,2)*100

# testing models
anova(lm, lma0, lmb) # test H0: there is little difference between the models

# diagnostics
par(mfrow=c(2,2))
plot(lmb)



