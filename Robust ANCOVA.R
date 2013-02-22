#dependancies (library)
library(ggplot2)
library(Hmisc)
library(MASS)
library(psych)
library(gridExtra)
library(outliers)
library(car)
library(nlme)
library(reshape)
library(compute.es)
library(effects)
library(multcomp)
library(pastecs)
library(WRS)
library(car)
library(nlme)
library(reshape)
library(ggplot2)

options(contrasts=c("contr.sum","contr.poly"))
#' A theme with grey background and white gridlines.
#'
#' @param base_size base font size
#' @param base_family base font family
#' @aliases theme_gray theme_grey
#' @export theme_gray theme_grey
theme_grey <- function(base_size = 12, base_family = "") {
  theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line =               element_line(colour = "black", size = 0.5, linetype = 1,
                            lineend = "butt"),
    rect =               element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text =               element_text(family = base_family, face = "plain",
                            colour = "black", size = base_size,
                            hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    axis.text =          element_text(size = rel(0.8), colour = "black"),
    strip.text =         element_text(size = rel(0.8)),

    axis.line =          element_line(colour = "black"),
    axis.text.x =        element_text(vjust = 1),
    axis.text.y =        element_text(hjust = 1),
    axis.ticks =         element_line(colour = "black"),
    axis.title.x =       element_text(),
    axis.title.y =       element_text(angle = 90),
    axis.ticks.length =  unit(0.15, "cm"),
    axis.ticks.margin =  unit(0.1, "cm"),

    legend.background =  element_rect(colour = NA),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         element_rect(fill = "grey95", colour = "white"),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.8)),
    legend.text.align =  NULL,
    legend.title =       element_text(size = rel(0.8), face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,

    panel.background =   element_blank(),
    panel.border =       element_blank(),
    panel.grid.major =   element_blank(),
    panel.grid.minor =   element_blank(),
    panel.margin =       unit(0.25, "lines"),

    strip.background =   element_rect(fill = "grey80", colour = NA),
    strip.text.x =       element_text(),
    strip.text.y =       element_text(angle = -90),

    plot.background =    element_rect(colour = "white"),
    plot.title =         element_text(size = rel(1.2)),
    plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines"),

    complete = TRUE
  )
}
theme_minimal <- theme_grey
setwd("C:/Users/John/Documents/My Dropbox/CAMI")
CAMI <- read.csv("John-CAMI.csv", header = TRUE)
#Descriptive Statistics

by(CAMI$AvgTotalInternal, CAMI$TOD, stat.desc)
by(CAMI$TimeofInterview, CAMI$TOD, stat.desc)

#Levene's test
leveneTest(CAMI$TimeofInterview, CAMI$TOD, center = median)
contrasts(CAMI$TOD)<-contr.poly(2)
camiModel <-aov (AvgTotalInternal ~ TimeofInterview + TOD, data = CAMI)
Anova(camiModel, type = "III")

#the adjusted means are the means where the covariate has been taken into account

adjustedMeans <- effect("TOD", camiModel, se=TRUE)
summary(adjustedMeans)
adjustedMeans$se

#to see the planned contrasts, use the summary.lm function

summary.lm(camiModel)

#Post-hoc tests
postHocs <-glht(camiModel, linfct = mcp(TOD="Tukey"))
summary(postHocs)
confint(postHocs)

#Testing for homogeneity of regression slopes using an interaction
hoRS <-aov (AvgTotalInternal ~ TimeofInterview*TOD, data = CAMI)
Anova(hoRS, type = "III")
summary(hoRS)
#splitting data (preparing for modeling)
morning <- subset(CAMI, TOD == "Morning Type")
evening <- subset (CAMI, TOD == "Evening Type")

#the following creates vectors for the covariates and dependent measures for each of the levels

covGrp1 <-morning$TimeofInterview
dvGrp1  <-morning$AvgTotalInternal
covGrp2 <-evening$TimeofInterview
dvGrp2  <-evening$AvgTotalInternal

ancova(covGrp1, dvGrp1, covGrp2, dvGrp2)
ancboot(covGrp1, dvGrp1, covGrp2, dvGrp2,tr = .2, nboot=2000)
