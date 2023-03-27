set.seed(20160229)

my_data = data.frame(
    y=c(rnorm(1000), rnorm(1000, 0.5), rnorm(1000, 1), rnorm(1000, 1.5)),
    x=c(rep('a', 2000), rep('b', 2000)),
    m=c(rep('i', 1000), rep('j', 2000), rep('i', 1000))
)

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
  data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
  grp <- data[1, "group"]
  newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
  newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
  newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])

  if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
    stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
      1))
    quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
    aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
    aesthetics$alpha <- rep(1, nrow(quantiles))
    both <- cbind(quantiles, aesthetics)
    quantile_grob <- GeomPath$draw_panel(both, ...)
    ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
  }
  else {
    ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
  }
})

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

ggplot(my_data, aes(x, y, fill = m)) + geom_split_violin()

#----------------

library(vioplot)
par(mfrow=c(2,1))
mu<-2
si<-0.6
bimodal<-c(rnorm(1000,-mu,si),rnorm(1000,mu,si)) 
uniform<-runif(2000,-4,4)
normal<-rnorm(2000,0,3)
vioplot(bimodal,uniform,normal)
boxplot(bimodal,uniform,normal)

# add to an existing plot
x <- rnorm(100)
y <- rnorm(100)
plot(x, y, xlim=c(-5,5), ylim=c(-5,5))
vioplot(x, col="tomato", horizontal=TRUE, at=-4, add=TRUE,lty=2, rectCol="gray")
vioplot(y, col="cyan", horizontal=FALSE, at=-4, add=TRUE,lty=2)

#---------------
violin <- read.csv("/Users/fd252/Dropbox/NAU/Research3/surveyPUC/violin/violin.csv")

violin <- read.csv("/Users/fd252/Dropbox/NAU/Research3/survey/surveyPUC/violin/violin.csv")

violin <- read.csv("violinMilestones.csv")

beanplot(y~ label, names=c("labels"), ll = 0.04, data = violin, main = "", ylab = "AUC Metric", side = "both",  border = NA, col = list("black", c("grey", "white")))
legend(cex = 0.75, "bottomleft", fill = c("black", "grey"), legend = c("Control", "Treatment"))



> violin%>%
+ group_by(label)%>% 
+ summarise(Mean=mean(y), Max=max(y), Min=min(y), Median=median(y), Std=sd(y))
`summarise()` ungrouping output (override with `.groups` argument)
# A tibble: 2 x 6
  label  Mean   Max   Min Median   Std
  <chr> <dbl> <int> <int>  <int> <dbl>
1 API    4.76     8     0      5  2.57
2 Type   4.10     7     0      4  1.92


