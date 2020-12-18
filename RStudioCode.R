#https://www.keepcalmandtravel.com/difference-camper-trailer-caravan-camper-van-aussie-road-trip/
# https://docs.microsoft.com/en-us/machine-learning-server/r-reference/revoscaler/rximport
# Charge the image as an R object with the "JPEG" package

library(png)
my_image <- readPNG("CaravanPic.png")

# Set up a plot area with no plot
plot(0:705, type='n', main="", xlab="x", ylab="y")

# Get the plot information so the image will fill the plot box, and draw it
lim <- par()
rasterImage(my_image, 
            xleft=0, xright=705, 
            ybottom=0, ytop=526)
grid()

lines(
  x=c(0, 200, 400, 600, 800, 1000), 
  y=c(0, 0, 0, 0, 0, 0), 
  type="b", lwd=5, col="black")

# Import, reference, get variable info, summary by levels -----------------

# Import Data as XDF
rxImport(inData = "TrainingData.csv", outFile = "TrainingData.xdf", overwrite=TRUE)

#Variable to refence data file
insur_xdf <- RxXdfData('TrainingData.xdf')
#Variable info
rxGetInfo(insur_xdf, getVarInfo = TRUE, numRows = 1)
#END---
#Customer Subtype, Avg age Counts
rxSummary( ~ factor(MOSTYPE, levels = 1:41) + 
             factor(MGEMLEEF, levels = 1:6), insur_xdf)
# Q1: Age:Customer Main Type; Factor F(X)
#105 3          8          1          51
#40-50 years, 8 Family with grown ups
rxCube( ~ F(MGEMLEEF):F(MOSHOOFD):F(CARAVAN), insur_xdf)


# Factor CrossTabs, transforms --------------------------------------------
# Q2---
#Number of houses:Avg size household
rxCrossTabs( ~ NumHouses:SizeHousehold:F(CARAVAN), insur_xdf,
             transforms = list(
               NumHouses = ifelse(MAANTHUI <= 1, 0, 1),
               SizeHousehold = MGEMOMV,
               NumHouses = factor(NumHouses, labels = c('One', 'multiple')),
               SizeHousehold = factor(SizeHousehold))
             )


# Histograms --------------------------------------------------------------
# Q3---Number of houses, numBreaks = 10
rxHistogram( ~ MAANTHUI, insur_xdf, histType = "Counts", xTitle="Number of Houses")

rxCube( ~ F(CARAVAN), insur_xdf)
# Q4 MRELSA Living together
#rxHistogram( ~ MRELSA, insur_xdf, histType = "Counts", rowSelection = (CARAVAN == '1'), xTitle = "Living Together Categories")
# Q4 MRELOV Other relation
#rxHistogram( ~ MRELOV, insur_xdf, histType = "Counts", rowSelection = (CARAVAN == '1'), xTitle = "Other Relation Categories")

# Q4---MRELGE Married (Married easier to see, understand than Living together and other relation)
rxHistogram( ~ MRELGE, insur_xdf, histType = "Counts", rowSelection = (CARAVAN == '1'), xTitle = "Married % Categories of those with Caravan Policies")

# Q5---MFALLEEN Singles: Very few on zip codes with high percentage of single
rxHistogram( ~ MFALLEEN, insur_xdf, histType = "Counts", rowSelection = (CARAVAN == '1'), xTitle = "Single % Categories of those with Caravan Policies")

#Those with caravan policies live in about equal number of zip codes with children than those without
# Q5---MFGEKIND Household without children
#rxHistogram( ~ MFGEKIND, insur_xdf, histType = "Counts", rowSelection = (CARAVAN == '1'), xTitle = "Without Children % Categories of those with Caravan Policies")

# Q5---MFWEKIND Household with children
#rxHistogram( ~ MFWEKIND, insur_xdf, histType = "Counts", rowSelection = (CARAVAN == '1'), xTitle = "With Children % Categories of those with Caravan Policies")

# Q6
# = cut(tip_percent,breaks = c(-10, 5, 10, 15, 20, 25, 100))

rxHistogram( ~ high_education, insur_xdf, histType = "Counts",
             rowSelection = (CARAVAN == '1'),
             transforms = list(
               high_education = MOPLHOOG))

#Medium and low education is spread out thoroughout the categories
# rxHistogram( ~ medium_education, insur_xdf, histType = "Counts",
#              rowSelection = (CARAVAN == '1'),
#              transforms = list(
#                medium_education = MOPLMIDD))

# rxHistogram( ~ low_education, insur_xdf, histType = "Counts",
#              rowSelection = (CARAVAN == '1'),
#              transforms = list(
#                low_education = MOPLLAAG))

# Q7
# MBERHOOG High status | MOSHOOFD Customer main type
# rxHistogram( ~ high_status | F(MOSHOOFD), insur_xdf, title = "Customer main types", histType = "Percent",
#              rowSelection = (CARAVAN == '1'),
#                             transforms = list(
#                             high_status = MBERHOOG))

rxHistogram( ~ Entrepreneur | F(MOSHOOFD), insur_xdf, title = "Customer main types", histType = "Percent",
             rowSelection = (CARAVAN == '1'),
             transforms = list(
               Entrepreneur = MBERZELF))

rxHistogram( ~  Farmer | F(MOSHOOFD), insur_xdf, title = "Customer main types", histType = "Percent",
             rowSelection = (CARAVAN == '1'),
             transforms = list(
               Farmer = MBERBOER))

# rxHistogram( ~ middle_management | F(MOSHOOFD), insur_xdf, title = "Customer main types", histType = "Percent",
#              rowSelection = (CARAVAN == '1'),
#              transforms = list(
#                middle_management = MBERMIDD))

# rxHistogram( ~ skilled_labourers | F(MOSHOOFD), insur_xdf, title = "Customer main types", histType = "Percent",
#              rowSelection = (CARAVAN == '1'),
#              transforms = list(
#                skilled_labourers = MBERARBG))
# 
# rxHistogram( ~ Unskilled_labourers | F(MOSHOOFD), insur_xdf, title = "Customer main types", histType = "Percent",
#              rowSelection = (CARAVAN == '1'),
#              transforms = list(
#                Unskilled_labourers = MBERARBO))


            