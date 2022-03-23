library(readstata13)
library(ggplot2)
library(plyr)
library(ggridges)
library(ggpubr)
library(gridExtra)
theme_set(theme_pubr())
gallupData <- read.dta13("urban-rural-replication.dta")
attach(gallupData)

## For our sample, the median distance from a large city of this size is fifteen miles. 
exp(median(cityDistanceLogged, na.rm=T))
# [1] 15

## For population density, the median value for the sample is approximately eight hundred residents per square mile.
median(PopDens101000s, na.rm = T)
# [1] 0.793551

## All of the respondents living in communities with densities over ten thousand people per square mile live in either New York City or the adjacent suburbs of New Jersey.
table(State[PopDens101000s > 10])
# AA   AE   AK   AL   AP   AR   AS   AZ   CA   CO   CT   DC   DE   FL   FM   GA   GU   HI   IA   ID   IL   IN   KS 
# 0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0 
# KY   LA   MA   MD   ME   MH   MI   MN   MO   MP   MS   MT   NC   ND   NE   NH   NJ   NM   NV   NY   OH   OK   OR 
# 0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0  210    0    0 2318    0    0    0 
# PA   PR   PW   RI   SC   SD   TN   TX   UT   VA   VI   VT   WA   WI   WV   WY 
# 0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0 

## Seventy-five percent of the respondents live within 45 miles of a city of 100,000 and ninety-five percent of the sample lives within one-hundred five miles of such a place.  
exp(quantile(cityDistanceLogged, .75, na.rm = T))
# 75% 
# 45 
exp(quantile(cityDistanceLogged, .95, na.rm = T))
# 95% 
# 105 

## The median distance from a city (of at least one hundred thousand) is 20 miles for a Republican, 17 miles for an independent, and 12 miles for a Democrat. 
exp(median(cityDistanceLogged[pid3=="Rep"], na.rm = T))
# [1] 20
exp(median(cityDistanceLogged[pid3=="Ind"], na.rm = T))
# [1] 17
exp(median(cityDistanceLogged[pid3=="Dem"], na.rm = T))
# [1] 12

##  While the median population density for Democratic respondents is 1,197 people per square mile, the median population density is less than half that for Republicans at about 585 people per square mile. Meanwhile, independents fall between the two at 738 people per square mile.
median(PopDens101000s[pid3=="Dem"], na.rm = T)
# [1] 1.197358
median(PopDens101000s[pid3=="Rep"], na.rm = T)
# [1] 0.585149
median(PopDens101000s[pid3=="Ind"], na.rm = T)
# [1] 0.7376639

## Based on comparisons using t-tests, each of these values are significantly different from each other.
t.test(PopDens101000s[pid3=="Dem"], PopDens101000s[pid3=="Rep"])
t.test(PopDens101000s[pid3=="Dem"], PopDens101000s[pid3=="Ind"])
t.test(PopDens101000s[pid3=="Ind"], PopDens101000s[pid3=="Rep"])

t.test(cityDistanceLogged[pid3=="Dem"], cityDistanceLogged[pid3=="Rep"])
t.test(cityDistanceLogged[pid3=="Dem"], cityDistanceLogged[pid3=="Ind"])
t.test(cityDistanceLogged[pid3=="Ind"], cityDistanceLogged[pid3=="Rep"])


detach(gallupData)
## Generate the Histograms
gallupData$pid3 <- car::recode(gallupData$pid5, "1='Democrats';2='Democrats';3='independents';4='Republicans';5='Republicans'")

makeHist <- function(theData, theSubtitle1, theSubtitle2) {
  theDataAll <- theData
  theData <- na.omit(theDataAll[, c("cityDistanceLogged", "pid3")])
  cdat <-
    ddply(theData,
          .(pid3),
          summarise,
          distance.median = median(cityDistanceLogged, na.rm = T))
  p1 <- ggplot(theData,
               aes(x = `cityDistanceLogged`, y = `pid3`)) +
    xlab("Distance from City (logged scale)") +
    ylab("") +
    geom_density_ridges(aes(fill = pid3)) +
    theme(legend.position = "none") +
    (scale_x_continuous(
      breaks = c(0, 2, 4, 6),
      labels = c(round(exp(0)), round(exp(2)), round(exp(4)), round(exp(6))),
      limits = c(0, 6.25)
    ))
  
  p1data <- ggplot_build(p1)$data
  p1data <- p1data[[1]]
  demMed1 <- which.min(abs(cdat[1, 2] - p1data$x[p1data$group == 1]))
  indMed1 <- which.min(abs(cdat[2, 2] - p1data$x[p1data$group == 2]))
  gopMed1 <- which.min(abs(cdat[3, 2] - p1data$x[p1data$group == 3]))
  
  yMaxDem1 <- p1data$ymax[p1data$group == 1][demMed1]
  yMaxInd1 <- p1data$ymax[p1data$group == 2][indMed1]
  yMaxGop1 <- p1data$ymax[p1data$group == 3][gopMed1]
  
  yMinInd1 <- p1data$ymax[p1data$group == 1][indMed1]
  yMinGop1 <- p1data$ymax[p1data$group == 2][gopMed1]
  
  p1 <-  p1 +  scale_fill_manual(values = gray.colors(3)) + #c("#0571b0", "#c2a5cf", "#ca0020")) +
    labs(title = theSubtitle1) +
    annotate(
      "text",
      label = paste(round(exp(cdat$distance.median), 2), "mi"),
      size = 4,
      x = cdat$distance.median + .05,
      y = c(yMaxDem1, yMaxInd1, yMaxGop1) - .5,
      hjust = 0
    ) +
    geom_segment(aes(
      x = cdat$distance.median[1],
      y = 1,
      xend = cdat$distance.median[1],
      yend = yMaxDem1
    )) +
    geom_segment(
      aes(
        x = cdat$distance.median[2],
        y = yMinInd1,
        xend = cdat$distance.median[2],
        yend = yMaxInd1
      )
    ) +
    geom_segment(
      aes(
        x = cdat$distance.median[3],
        y = yMinGop1,
        xend = cdat$distance.median[3],
        yend = yMaxGop1
      )
    )
  
  ## Density
  theData <- na.omit(theDataAll[, c("PopDens10Logged", "pid3")])
  cdat2 <-
    ddply(theData,
          .(pid3),
          summarise,
          density.median = median(PopDens10Logged, na.rm = T))
  p2 <- ggplot(theData,
               aes(x = `PopDens10Logged`, y = `pid3`)) +
    xlab("Population Density (logged scale)") +
    ylab("") +
    geom_density_ridges(aes(fill = pid3)) +
    theme(legend.position = "none") +
    (scale_x_continuous(
      breaks = c(-2, 0, 2,4,6,8,10,12),
      labels = c(round(exp(-2),1), round(c(exp(0), exp(2),exp(4),exp(6),exp(8),exp(10),exp(12)), 0)),  
      limits = c(2.25, max(gallupData$PopDens10Logged, na.rm =T)))
    )
  
  pdata <- ggplot_build(p2)$data
  pdata <- pdata[[1]]
  demMed <- which.min(abs(cdat2[1, 2] - pdata$x[pdata$group == 1]))
  indMed <- which.min(abs(cdat2[2, 2] - pdata$x[pdata$group == 2]))
  gopMed <- which.min(abs(cdat2[3, 2] - pdata$x[pdata$group == 3]))
  
  yMaxDem <- pdata$ymax[pdata$group == 1][demMed]
  yMaxInd <- pdata$ymax[pdata$group == 2][indMed]
  yMaxGop <- pdata$ymax[pdata$group == 3][gopMed]
  
  yMinInd <- pdata$ymax[pdata$group == 1][indMed]
  yMinGop <- pdata$ymax[pdata$group == 2][gopMed]
  
  
  p2 <-  p2 +  scale_fill_manual(values = gray.colors(3)) + #c("#0571b0", "#c2a5cf", "#ca0020")) +
    labs(title = theSubtitle2) +
    annotate(
      "text",
      label = paste(round(exp(cdat2$density.median)), "ppl / sq mi"),
      size = 4,
      x = cdat2$density.median + .05,
      y = c(yMaxDem, yMaxInd, yMaxGop) - .5,
      hjust = 0
    ) +
    geom_segment(aes(
      x = cdat2$density.median[1],
      y = 1,
      xend = cdat2$density.median[1],
      yend = yMaxDem
    )) +
    geom_segment(
      aes(
        x = cdat2$density.median[2],
        y = yMinInd,
        xend = cdat2$density.median[2],
        yend = yMaxInd
      )
    ) +
    geom_segment(
      aes(
        x = cdat2$density.median[3],
        y = yMinGop,
        xend = cdat2$density.median[3],
        yend = yMaxGop
      )
    )
  grid.arrange(p1, p2, nrow=1, widths = c(.25,.25))  
}  


## Party
makeHist(theData = gallupData,
         theSubtitle1 = "Distance from City and Partisanship\nAll Respondents",
         theSubtitle2 = "Population Density and Partisanship\nAll Respondents")
## Race
makeHist(theData = subset(gallupData, black ==1),
         theSubtitle1 = "Distance from City and Partisanship\nBlack",
         theSubtitle2 = "Population Density and Partisanship\nBlack")
makeHist(theData = subset(gallupData, hispanic ==1),
         theSubtitle1 = "Hispanic",
         theSubtitle2 = "Hispanic")
makeHist(theData = subset(gallupData, white ==1),
         theSubtitle1 = "White",
         theSubtitle2 = "White")
makeHist(theData = subset(gallupData, asian ==1),
         theSubtitle = "Asian",
         theSubtitle2 = "Asian")

## Income
makeHist(theData = subset(gallupData, incomeLt20 ==1 | income20t30==1),
         theSubtitle1 = "Distance from City and Partisanship\nLess Than $29,999",
         theSubtitle2 = "Population Density and Partisanship\nLess Than $29,999")
makeHist(theData = subset(gallupData, income30t50==1 ),
         theSubtitle1 = "$30,000 to $49,999",
         theSubtitle2 = "$30,000 to $49,999")
makeHist(theData = subset(gallupData, income50t75 ==1),
         theSubtitle1 = "$50,000 to $74,999",
         theSubtitle2 = "$50,000 to $74,999")
makeHist(theData = subset(gallupData, income75t100 ==1),
         theSubtitle1 = "$75,000 to $99,999",
         theSubtitle2 = "$75,000 to $99,999")
makeHist(theData = subset(gallupData, income100t150 ==1 | incomeMt150==1),
         theSubtitle1 = "more than $100,000",
         theSubtitle2 = "more than $100,000")
makeHist(theData = subset(gallupData, hsOrLess ==1),
         theSubtitle1 = "Distance from City and Partisanship\nHigh School or Less",
         theSubtitle2 = "Population Density and Partisanship\nHigh School or Less")
makeHist(theData = subset(gallupData, someCollege ==1),
         theSubtitle1 = "Some College",
         theSubtitle2 = "Some College")
makeHist(theData = subset(gallupData, baDegree == 1),
         theSubtitle1 = "College Grad",
         theSubtitle2 = "College Grad")
makeHist(theData = subset(gallupData, postGrad == 1),
         theSubtitle1 = "Post Grad",
         theSubtitle2 = "Post Grad")

## Church
makeHist(theData = subset(gallupData, relAttOnceAWeek ==1 | relAttAlmostEveryWeek==1),        
         theSubtitle1 = "Distance from City and Partisanship\nWeekly / Almost Weekly Religious Attendance",
         theSubtitle2 = "Population Density and Partisanship\nWeekly / Almost Weekly Religious Attendance")
makeHist(theData = subset(gallupData, relAttOnceAMonth==1),
         theSubtitle1 = "Monthly Religious Attendance",
         theSubtitle2 = "Monthly Religious Attendance")
makeHist(theData = subset(gallupData, relAttSeldom ==1 | relAttNever==1),
         theSubtitle1 = "Never / Seldom Religious Attendance",
         theSubtitle2 = "Never / Seldom Religious Attendance")


## Age
makeHist(theData = subset(gallupData, under30==1),         
         theSubtitle1 = "Distance from City and Partisanship\nUnder 30 Years Old",
         theSubtitle2 = "Population Density and Partisanship\nUnder 30 Years Old")
makeHist(theData = subset(gallupData, senior==0 & under30==0),
         theSubtitle1 = "Over 30 under 65 Years Old",
         theSubtitle2 = "Over 30 under 65 Years Old")
makeHist(theData = subset(gallupData, senior==1),
         theSubtitle1 = "Over 65 Years Old",
         theSubtitle2 = "Over 65 Years Old")
