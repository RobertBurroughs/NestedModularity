---
  title: "Nested Morphological Modularity in Rodent Molars Matches Evolutionary Incorporation of Developmental Modules"
author: "R. W. Burroughs"
---
  ## R Markdown
  
library(Morpho)
library(geomorph)

all.lm.data<-"/Users/robertburroughs 1/Desktop/Extant_Fossils_New_LMs_fcsv"

foss.lm.data<-"/Users/robertburroughs 1/Desktop/Fossils_New_LMs_fcsv"

rec.lm.data<-"/Users/robertburroughs 1/Desktop/extant_fcsv"

#file names

all.file.names<-list.files(all.lm.data)

foss.file.names<-list.files(foss.lm.data)

rec.lm.data<-"/Users/robertburroughs 1/Desktop/extant_fcsv"

#set this function to work for your specific .fcsv's, because different ways of saving produce differently formatted files
correct.way.to.read <-function(folder,filename){
  #skip first 3 rows because they aren't helpful, confuse the computer. 
  #make sure it doesn't think that the first row is a header
  my.table<-read.csv(paste(folder,filename,sep="/"), colClasses = c("numeric", "numeric", "numeric", "NULL"),header=FALSE, skip=3)
  return(my.table)
}

#If you set column 4 as "NULL" you will drop the Z coordinates

#same as earlier function, for this particular fcsv file type you want 2nd thru 4th columns
files2landmarks<-function(filename,folder,correct.number.of.rows){
  input.raw<-correct.way.to.read(filename=filename, folder)
  if (nrow(input.raw)!=correct.number.of.rows){
    cat(paste("ERROR: file",filename,"does not have the correct number of points.\n"))
    cat(paste("It should have",correct.number.of.rows,"but it has", nrow(input.raw),"\n"))
  }
  #I don't know why it needs to be as.matrix() but it wouldn't work without the function
  just.coords<-as.numeric(as.matrix(input.raw[,c(2:4)]))
  return(just.coords)
}

#read in first specimen just to figure out number of coordinates (=number of rows)
#all specimens

all.lms.raw<-correct.way.to.read(filename=all.file.names[1], folder=all.lm.data)

all.correct.number.of.rows<-nrow(all.lms.raw)

files2landmarks(all.file.names[1],folder=all.lm.data,correct.number.of.rows = all.correct.number.of.rows)

all.lms.set<-array(data=NA,
                   dim=c(nrow(all.lms.raw),3,length(all.file.names)))

#fossils
foss.lms.raw<-correct.way.to.read(filename=foss.file.names[1], folder=foss.lm.data)
foss.correct.number.of.rows<-nrow(foss.lms.raw)

files2landmarks(foss.file.names[1],folder=foss.lm.data,foss.correct.number.of.rows)

foss.lms.set<-array(data=NA,
                    dim=c(nrow(foss.lms.raw),3,length(foss.file.names)))

#recent
rec.lms.raw<-correct.way.to.read(filename=rec.file.names[1], folder=rec.lm.data)
rec.correct.number.of.rows<-nrow(rec.lms.raw)

files2landmarks(rec.file.names[1],folder=rec.lm.data,correct.number.of.rows)

rec.lms.set<-array(data=NA,
                   dim=c(nrow(rec.lms.raw),3,length(rec.file.names)))

#cycle through reading files with a for-loop
#NOTE: Loop will produce ERROR if a file is missing a landmark [ex: TTU-403255-r]
#Note: You have to change "all" to "foss" to get this to populate the fossil data above
for (i in 1:length(all.file.names)){
  all.lms.set[,,i]<-files2landmarks(all.file.names[i],folder=all.lm.data, all.correct.number.of.rows)
}

plot3d(all.lms.set[,,1])

#LandMark Subset for Modularity Analysis

land.gps_am3<-rep('x', 24)

land.gps_am3[1]<-'a'; land.gps_am3[9:10]<-'a';land.gps_am3[21:24]<-'a'

land.gps_am3[2:8]<-'p';land.gps_am3[11:20]<-'p'

land.gps_am3

#Subset LMs for Vrel/GPA analyses

all.am<-all.lms.set[which(land.gps_am3=="a"),,]

all.pm<-all.lms.set[which(land.gps_am3=="p"),,]

foss.am<-foss.lms.set[which(land.gps_am3=="a"),,]

foss.pm<-foss.lms.set[which(land.gps_am3=="p"),,]

#Generalized Procrusted Analysis of landmarks in geomorph framework, gpagen.                         
#GPAs for all specimens with LMs subset

gpa.all <- gpagen(all.lms.set)

gpa.all.am<- gpagen(all.am)

gpa.all.pm <-gpagen(all.pm)

#Fossil Specimens with LMs subset

gpa.foss <-gpagen(foss.lms.set)

gpa.foss.am <-gpagen(foss.am)

gpa.foss.pm <-gpagen(foss.pm)

#Recent Specimens (no need to subset this is only for CR Modularity Analysis)

gpa.rec <-gpagen(rec.lms.set)

#Data Table with Attribute Info

data.table.all<-read.csv(file.choose())

#Subset Each Independent Variable from data.table.extant

Fossil.Extant <-data.table.all$Fossil.Extant

age<-data.table.all$Age

#Conversion to Geomorph.Data.Frame

gdf.all <-geomorph.data.frame(gpa.all, locality = locality.all, Fossil.Extant = Fossil.Extant, tri_num = tri_num, age = age)

#PCA

pca.all <-gm.prcomp(gpa.all$coords)

pca.all

plot.pca.all <-plot(pca.all)

pca.foss <-gm.prcomp(gpa.foss$coords)

pca.foss

plot.pca.foss <-plot(pca.foss)

#Add Color to Plots

color.palette.foss.ext<-c("magenta","blue")

color.index.foss.ext<-as.factor(gdf.all$Fossil.Extant)

#plot PCA with color and shapes

plot.pca.rec.foss <-plot(pca.all, pch = c(18,1)[color.index.foss.ext], col=color.palette.foss.ext[color.index.foss.ext])
legend("topright",legend = levels(color.index.foss.ext), col=color.palette.foss.ext, pch = c(1,18)[color.index.foss.ext])

#Plot To Reference Specimens - mean shape changes across PCs 1 and 2 between oldest fossil mean and recent mean

is.fossil<-which(gdf.all$Fossil.Extant=="F")
is.recent<-which(gdf.all$Fossil.Extant=="E")

foss.ref <-mshape(gdf.all$coords[,,is.fossil])

rec.ref <-mshape(mshape(gdf.all$coords[,,is.recent]))

tooth.links <- define.links(rec.ref, ptsize = 1, links = NULL) 

plot.foss.ref <-plotRefToTarget(rec.ref, foss.ref, mag=2, method = "TPS", links = tooth.links)
     
plot.rec.ref <-plotRefToTarget(rec.ref, gpa.all$coords[,,1], mag=2, method = "TPS", links = tooth.links)
     
#ProcrustesANOVA ALL Specimens
fit_age <-procD.lm(coords ~ age, data=gdf.all, iter=999, turbo=TRUE)

summary(fit_age)

fit_tri <-procD.lm(coords ~ tri_num, data=gdf.all, iter=999, turbo=TRUE)

summary(fit_tri)

plot(fit_tri)

fit_rec_foss <-procD.lm(coords ~ Fossil.Extant, data=gdf.all, iter=999, turbo=TRUE)

summary(fit_rec_foss)

plot(fit_rec_foss)

fit_tri <-procD.lm(coords ~ tri_num, data=gdf.all, iter=999, turbo=TRUE)

summary(fit_tri)

plot(fit_tri)

#test if more complex age model is better

fit.time.simple<-procD.lm(gdf.all$coords ~ age)
fit.time.complex<-procD.lm(coords ~ Fossil.Extant / age, data = gdf.all, iter=999, turbo=TRUE) 

#logically speaking, age nests within fossil.modern because no value of age will ever be in *both* the fossil and modern categories
anova(fit.time.simple,fit.time.complex)

#Note: Now we use Recent and Fossil datasets separately

#Integration.VREL test. Calculates relative eigenvalue from covariance matrix. Then Z-corrects it for effective size
Vrel_Rec_AM <- integration.Vrel(gpa.rec.am$coords)

Vrel_Rec_PM <-integration.Vrel(gpa.rec.pm$coords)

Vrel_Rec_All <-integration.Vrel(gpa.rec$coords)

Vrel_Foss <-integration.Vrel(gpa.foss$coords)

Vrel_Foss_AM <-integration.Vrel(gpa.foss.am$coords)

Vrel_Foss_PM <-integration.Vrel(gpa.foss.pm$coords)

#Compare.ZVRel compares Z-corrected Vrel scores as a measure for the difference in amount of integration between datasets (requires two or more Vrel values)
comp_ZVRel_AM_PM_Rec_one_tail <-compare.ZVrel(Vrel_Rec_AM, Vrel_Rec_PM, Vrel_Rec_All, two.tailed = FALSE)

comp_ZVRel_AM_PM_Rec_one_tail

#Fossils Vrel

comp_ZVRel_AM_PM_Foss_one_tail <-compare.ZVrel(Vrel_Foss_AM, Vrel_Foss_PM, Vrel_Foss, two.tailed = FALSE)

comp_ZVRel_AM_PM_Foss_one_tail

#ModularityAnalysis - We have to do this for the separate recent and fossil datasets, because significant shape difference between Fossils and Recent

#Recent

m_test <-modularity.test(gpa.rec$coords, land.gps_am3, CI=TRUE, iter=99)

CInterval<-m_test$CInterval

CInterval

CRBoot <-m_test$CR.boot

CRBoot

summary(m_test)

plot(m_test)

#Fossils
m_test_foss <-modularity.test(gpa.foss$coords, land.gps_am3, CI=TRUE, iter=99)

CInterval_foss<-m_test_foss$CInterval

CInterval_foss

CRBoot_foss <-m_test_foss$CR.boot

CRBoot_foss

summary(m_test_foss)

plot(m_test_foss)

#End