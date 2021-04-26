#camera trap data

wants <- c("ggplot2", "devtools", "here")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

install.packages("devtools")

library(devtools)

install.packages("usethis")

library(devtools)
devtools::install_github("arcaravaggi/remBoot")
'force = TRUE'

library(remBoot)
library(ggplot2)
library(here)

CamDat<-read.csv(here("Data", "Jackal_CameraTrap_50.csv"))


head(CamDat)

tm <- 33600
v <- 12.00

output<- rem(CamDat, tm, v)
output

surveyarea<- 22915
surveyarea

popEst<- output*surveyarea
popEst


#or we divide the distance travlled per day (263.909) by the number of jackals (22) which makes the total distance travlled by a singular jackal so therefore 

tm <- 33600
v <- 11.9959

output<- rem(CamDat, tm, v)
output

surveyarea<- 22915
surveyarea

popEst<- output*surveyarea
popEst

