#!/usr/bin/env Rscript

#May need to install two packages
#install.packages("tidyr","plm")
library(tidyr)
library(plm)
#
#Need to set the working directory to wherever you download the UCR dataset
ucr.data <- "~/web/larceny_theft_states_2000_2014.csv"

vYears <- c(2000:2016)

vURLs <- c(
  "https://www.bjs.gov/content/pub/sheets/ppus00.zip",
  "https://www.bjs.gov/content/pub/sheets/ppus01.zip",
  "https://www.bjs.gov/content/pub/sheets/ppus02.zip",
  "https://www.bjs.gov/content/pub/sheets/ppus03.zip",
  "https://www.bjs.gov/content/pub/sheets/ppus04.zip",
  "https://www.bjs.gov/content/pub/sheets/ppus05.zip",
  "https://www.bjs.gov/content/pub/sheets/ppus06.zip",
  "https://www.bjs.gov/content/pub/sheets/ppus07st.zip",
  "https://www.bjs.gov/content/pub/sheets/ppus08.zip",
  "https://www.bjs.gov/content/pub/sheets/ppus09.zip",
  "https://www.bjs.gov/content/pub/sheets/ppus10.zip",
  "https://www.bjs.gov/content/pub/sheets/ppus11.zip",
  "https://www.bjs.gov/content/pub/sheets/ppus12.zip",
  "https://www.bjs.gov/content/pub/sheets/ppus13.zip",
  "https://www.bjs.gov/content/pub/sheets/ppus14.zip",
  "https://www.bjs.gov/content/pub/sheets/ppus15.zip",
  "https://www.bjs.gov/content/pub/sheets/ppus16.zip"
)
vFiles <- c(
  "pp0003.csv",
  "ppus0102.csv",
  "ppus0202.csv",
  "ppus0302.csv",
  "ppus0402.csv",
  "ppus05t02.csv",
  "ppus06t01.csv",
  "ppus07st02.csv",
  "ppus08at02.csv",
  "ppus09at02.csv",
  "ppus10at02.csv",
  "ppus11at02.csv",
  "ppus12at02.csv",
  "ppus13at02.csv",
  "ppus14at04.csv",
  "ppus15at2.csv",
  "ppus16at02.csv"
)
vSkips <- c(
  21,
  20,
  16,
  16,
  18,
  15,
  16,
  17,
  17,
  15,
  15,
  16,
  16,
  16,
  15,
  15,
  15
)
vRows <- c(
  57,
  57,
  54,
  54,
  54,
  54,
  54,
  54,
  54,
  51,
  51,
  51,
  51,
  51,
  51,
  51,
  51
)

cleanState <- function(dirty.state){
  test <- tolower(dirty.state)
  if (grepl("alabama", test)) {res <- "Alabama"}
  else if (grepl("alaska", test)) {res <- "Alaska"}
  else if (grepl("arizona", test)) {res <- "Arizona"}
  else if (grepl("arkansas", test)) {res <- "Arkansas"}
  else if (grepl("california", test)) {res <- "California"}
  else if (grepl("colorado", test)) {res <- "Colorado"}
  else if (grepl("connecticut", test)) {res <- "Connecticut"}
  else if (grepl("delaware", test)) {res <- "Deleware"}
  else if (grepl("district.*of.*columbia", test)) {res <- "District of Columbia"}
  else if (grepl("florida", test)) {res <- "Florida"}
  else if (grepl("georgia", test)) {res <- "Georgia"}
  else if (grepl("hawaii", test)) {res <- "Hawaii"}
  else if (grepl("idaho", test)) {res <- "Idaho"}
  else if (grepl("illinois", test)) {res <- "Illinois"}
  else if (grepl("indiana", test)) {res <- "Indiana"}
  else if (grepl("iowa", test)) {res <- "Iowa"}
  else if (grepl("kansas", test)) {res <- "Kansas"}
  else if (grepl("kentucky", test)) {res <- "Kentucky"}
  else if (grepl("louisiana", test)) {res <- "Louisiana"}
  else if (grepl("maine", test)) {res <- "Maine"}
  else if (grepl("maryland", test)) {res <- "Maryland"}
  else if (grepl("massachusetts", test)) {res <- "Massachusetts"}
  else if (grepl("michigan", test)) {res <- "Michigan"}
  else if (grepl("minnesota", test)) {res <- "Minnesota"}
  else if (grepl("mississippi", test)) {res <- "Mississippi"}
  else if (grepl("missouri", test)) {res <- "Missouri"}
  else if (grepl("montana", test)) {res <- "Montana"}
  else if (grepl("nebraska", test)) {res <- "Nebraska"}
  else if (grepl("nevada", test)) {res <- "Nevada"}
  else if (grepl("new.*hampshire", test)) {res <- "New Hampshire"}
  else if (grepl("new.*jersey", test)) {res <- "New Jersey"}
  else if (grepl("new.*mexico", test)) {res <- "New Mexico"}
  else if (grepl("new.*york", test)) {res <- "New York"}
  else if (grepl("north.*carolina", test)) {res <- "North Carolina"}
  else if (grepl("north.*dakota", test)) {res <- "North Dakota"}
  else if (grepl("ohio", test)) {res <- "Ohio"}
  else if (grepl("oklahoma", test)) {res <- "Oklahoma"}
  else if (grepl("oregon", test)) {res <- "Oregon"}
  else if (grepl("pennsylvania", test)) {res <- "Pennsylvania"}
  else if (grepl("rhode.*island", test)) {res <- "Rhode Island"}
  else if (grepl("south.*carolina", test)) {res <- "South Carolina"}
  else if (grepl("south.*dakota", test)) {res <- "South Dakota"}
  else if (grepl("tennessee", test)) {res <- "Tennessee"}
  else if (grepl("texas", test)) {res <- "Texas"}
  else if (grepl("utah", test)) {res <- "Utah"}
  else if (grepl("vermont", test)) {res <- "Vermont"}
  else if (grepl("west.*virginia", test)) {res <- "West Virginia"}
  else if (grepl("virginia", test)) {res <- "Virginia"}
  else if (grepl("washington", test)) {res <- "Washington"}
  else if (grepl("wisconsin", test)) {res <- "Wisconsin"}
  else if (grepl("wyoming", test)) {res <- "Wyoming"}
  else {res <- ""}
  return(res)
}
cleanStates <- function(dirty.states){
  return(lapply(dirty.states, cleanState))
}

setClass("num.w.commas")
setAs("character", "num.w.commas", function(from) suppressWarnings(as.numeric(gsub(",", "", from))))
setClass("state.name")
setAs("character", "state.name", function(from) as.character(cleanStates(from)))

lCols <- list()
lCols[[1]] <- c("state.name", rep("NULL", 12), "num.w.commas")
lCols[[2]] <-  c("state.name", rep("NULL", 12), "num.w.commas")
lCols[[3]] <-  c("state.name", rep("NULL", 6), "num.w.commas")
lCols[[4]] <-  c("state.name", rep("NULL", 6), "num.w.commas")
lCols[[5]] <-  c("state.name", rep("NULL", 6), "num.w.commas")
lCols[[6]] <-  c("state.name", rep("NULL", 6), "num.w.commas")
lCols[[7]] <-  c("NULL", "state.name", rep("NULL", 9), "num.w.commas")
lCols[[8]] <-  c("NULL", "state.name", rep("NULL", 9), "num.w.commas")
lCols[[9]] <-  c("NULL", "state.name", rep("NULL", 9), "num.w.commas")
lCols[[10]] <-  c(rep("NULL", 2), "state.name", rep("NULL", 9), "num.w.commas")
lCols[[11]] <-  c(rep("NULL", 2), "state.name", rep("NULL", 9), "num.w.commas")
lCols[[12]] <-  c("NULL", "state.name", rep("NULL", 12), "num.w.commas")
lCols[[13]] <-  c("NULL", "state.name", rep("NULL", 9), "num.w.commas")
lCols[[14]] <-  c("NULL", "state.name", rep("NULL", 9), "num.w.commas")
lCols[[15]] <-  c("NULL", "state.name", rep("NULL", 13), "num.w.commas")
lCols[[16]] <-  c("NULL", "state.name", rep("NULL", 12), "num.w.commas")
lCols[[17]] <-  c("NULL", "state.name", rep("NULL", 12), "num.w.commas")

lData <- list()

for (i in c(1:length(vYears))) {
  tmp <- tempfile()
  download.file(vURLs[i], tmp)
  df <- read.csv(unz(tmp, vFiles[i]), sep=",", skip=vSkips[i], nrows=vRows[i], header=FALSE,
                 na.string="..", colClasses=lCols[[i]])
  unlink(tmp)
  names(df) <- c("State", "ProbationPer100k")
  df <- df[!(df$State==""), ]
  df$Year <- vYears[i]
  df <- df[,c(1,3,2)]
  lData[[i]] <- df
}

df <- lData[[1]]

for (i in c(2:length(vYears))) {
  df <- rbind(df, lData[[i]])
}

ucr.df.wide <- read.csv(ucr.data, sep=",", skip=4, nrows=15, header=TRUE)
vWide <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware",
           "District.of.Columbia","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa",
           "Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan",
           "Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New.Hampshire",
           "New.Jersey","New.Mexico","New.York","North.Carolina","North.Dakota","Ohio","Oklahoma",
           "Oregon","Pennsylvania","Rhode.Island","South.Carolina","South.Dakota","Tennessee","Texas",
           "Utah","Vermont","Virginia","Washington","West.Virginia","Wisconsin","Wyoming")
ucr.df <- gather_(ucr.df.wide, "State", "LarcenyTheftPer100k", vWide)
ucr.df$State <- cleanStates(ucr.df$State)
df <- merge(df, ucr.df, by=c("State", "Year"))

ols <- lm(df$ProbationPer100k ~ df$LarcenyTheftPer100k)
summary(ols)

fixed.effects <- plm(ProbationPer100k ~ LarcenyTheftPer100k,
                     data=df, index=c("State", "Year"), model="within")
summary(fixed.effects)

pFtest(fixed.effects, ols)

random.effects <- plm(ProbationPer100k ~ LarcenyTheftPer100k,
                      data=df, index=c("State", "Year"), model="random")
summary(random.effects)

phtest(fixed.effects, random.effects)

fixef(fixed.effects)
