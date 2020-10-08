
#####MARKDOWN SCRIPT START#####
## Manage Packages
rm(list = ls()) # clear global environment
graphics.off() # close all graphics
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(DataExplorer,fpp2,ggplot2,scales,dplyr,pROC,jtools,forcats) #maybe not jtools


####PUT IN PACKRAT AND CHECKPOINT PROBABLY
##get rid of data summary and project script


#Intial Setup
options(scipen=999)
df<-read.csv("AmesHousing.csv") #82 columns

plot_missing(df)

#pool
df$Pool.QC<-as.factor(df$Pool.QC)
summary(df$Pool.QC)
summary(df$Pool.Area)
df$pool<-!is.na(df$Pool.QC)
df$pool<-as.factor(df$pool)
summary(df$pool)
#remove orignal pool variables since 99.56% of the values were missing. Transformed this variable into an indicator on whether or not the house has a pool instead to retain any useful information
#could drop this later on, as well as shed

#misc feature
df$Misc.Feature<-as.factor(df$Misc.Feature)
summary(df$Misc.Feature)
df$shed<-recode_factor(df$Misc.Feature,
       "Elev"="No","Gar2"="No", "Othr"="No","TenC"="No", "Shed"="Yes")
df$shed[is.na(df$shed)]<-"No"
summary(df$shed)
#since 96.38% of this data was missing, this variable as been transformed into a shed indicator instead before being dropped
#shed was selected since it was the most prevalent 


#alley 
df$Alley[is.na(df$Alley)]<-"No"
df$Alley<-as.factor(df$Alley)
summary(df$Alley)
df$Alley<-recode_factor(df$Alley,
                       "Grvl"="Yes","Pave"="Yes")
summary(df$Alley)
#indicator of whether there is an alley for the house or not, translated due to 93.24% missing rate


#fence
df$Fence[is.na(df$Fence)]<-"No"
df$Fence<-as.factor(df$Fence)
df$Fence<-recode_factor(df$Fence,
                       "GdPrv"="Yes","GdWo"="Yes", "MnPrv"="Yes","MnWw"="Yes")
summary(df$Fence)
#indicator of whether or not house has a fence due to 80.48% missing rate


#fireplace
summary(df$Fireplaces)
df$Fireplace.Qu<-as.factor(df$Fireplace.Qu)
summary(df$Fireplace.Qu)
#due to high degree of missing values in Fireplace.Qu (48.53%) and similar information stored in Fireplaces value, this column is dropped

#lot
summary(df$Lot.Frontage)
df$M_LotFrontage<-as.factor(ifelse(is.na(df$Lot.Frontage),1,0))
summary(df$M_LotFrontage)
df$Lot.Frontage[is.na(df$Lot.Frontage)]<-median(df$Lot.Frontage, na.rm=TRUE)
#lot area has no NAs and no 0 values. Since lot frontage is the width of a lot, this value also cannot be 0 or NA. As a result, the median value is imputed for the NA's present that likely represent a data collection error
#M_LotFrontage is variable used to signify the values that were originally NAs

#Garages--done
summary(df$Garage.Area)
df$Garage.Area<-ifelse(is.na(df$Garage.Area),0, df$Garage.Area)
summary(df$Garage.Cars)
df$Garage.Cars<-ifelse(is.na(df$Garage.Cars),0, df$Garage.Cars)


df$Garage.Cond[df$Garage.Cond==""]<-NA
df$Garage.Cond[is.na(df$Garage.Cond)]<-"NoGarage"
df$Garage.Cond<-as.factor(df$Garage.Cond)
summary(df$Garage.Cond)

df$Garage.Finish[df$Garage.Finish==""]<-NA
df$Garage.Finish[is.na(df$Garage.Finish)]<-"NoGarage"
df$Garage.Finish<-as.factor(df$Garage.Finish)
summary(df$Garage.Finish)

df$Garage.Qual[df$Garage.Qual==""]<-NA
df$Garage.Qual[is.na(df$Garage.Qual)]<-"NoGarage"
df$Garage.Qual<-as.factor(df$Garage.Qual)
summary(df$Garage.Qual)

df$Garage.Type[df$Garage.Type==""]<-NA
df$Garage.Type[is.na(df$Garage.Type)]<-"NoGarage"
df$Garage.Type<-as.factor(df$Garage.Type)
summary(df$Garage.Type)


df$Garage<-as.factor(ifelse(is.na(df$Garage.Yr.Blt),"No","Yes"))
summary(df$Garage)
df$Garage.Yr.Blt[is.na(df$Garage.Yr.Blt)]<-median(df$Garage.Yr.Blt, na.rm = TRUE)
#max value of 2207, after looking at data this is clearly meant to be 2007
df$Garage.Yr.Blt[df$Garage.Yr.Blt == 2207] <- 2007
summary(df$Garage.Yr.Blt)


#Basement
df$Bsmt.Cond[df$Bsmt.Cond==""]<-NA
df$Bsmt.Cond[is.na(df$Bsmt.Cond)]<-"NoBsmt"
df$Bsmt.Cond<-as.factor(df$Bsmt.Cond)
summary(df$Bsmt.Cond)

df$Bsmt.Qual[df$Bsmt.Qual==""]<-NA
df$Bsmt.Qual[is.na(df$Bsmt.Qual)]<-"NoBsmt"
df$Bsmt.Qual<-as.factor(df$Bsmt.Qual)
summary(df$Bsmt.Qual)

df$Bsmt.Exposure[df$Bsmt.Exposure==""]<-NA
df$Bsmt.Exposure[is.na(df$Bsmt.Exposure)]<-"NoBsmt"
df$Bsmt.Exposure<-as.factor(df$Bsmt.Exposure)
summary(df$Bsmt.Exposure)

df$BsmtFin.Type.1[df$BsmtFin.Type.1==""]<-NA
df$BsmtFin.Type.1[is.na(df$BsmtFin.Type.1)]<-"NoBsmt"
df$BsmtFin.Type.1<-as.factor(df$BsmtFin.Type.1)
summary(df$BsmtFin.Type.1)

df$BsmtFin.Type.2[df$BsmtFin.Type.2==""]<-NA
df$BsmtFin.Type.2[is.na(df$BsmtFin.Type.2)]<-"NoBsmt"
df$BsmtFin.Type.2<-as.factor(df$BsmtFin.Type.2)
summary(df$BsmtFin.Type.2)

summary(df$Bsmt.Full.Bath)
df$Bsmt.Full.Bath<-ifelse(is.na(df$Bsmt.Full.Bath),0, df$Bsmt.Full.Bath)
summary(df$Bsmt.Half.Bath)
df$Bsmt.Half.Bath<-ifelse(is.na(df$Bsmt.Half.Bath),0, df$Bsmt.Half.Bath)

summary(df$Bsmt.Unf.SF)
df$Bsmt.Unf.SF<-ifelse(is.na(df$Bsmt.Unf.SF),0, df$Bsmt.Unf.SF)

summary(df$Total.Bsmt.SF)
df$Total.Bsmt.SF<-ifelse(is.na(df$Total.Bsmt.SF),0, df$Total.Bsmt.SF)

summary(df$BsmtFin.SF.2)
df$BsmtFin.SF.1<-ifelse(is.na(df$BsmtFin.SF.1),0, df$BsmtFin.SF.1)
df$BsmtFin.SF.2<-ifelse(is.na(df$BsmtFin.SF.2),0, df$BsmtFin.SF.2)

#other
summary(df$Mas.Vnr.Area)
df$Mas.Vnr.Area<-ifelse(is.na(df$Mas.Vnr.Area),0, df$Mas.Vnr.Area)


#condense location into larger groups in order to validate model
summary(df$Neighborhood)
#df$Neighborhood<-recode_factor(df$Neighborhood,"GrnHill"="Other","Landmrk"="Other", "Greens"="Other")



#Var removal----move this below the cleaning section
#pid dropped unless needed for geography
df.clean<-select(df, -Pool.QC, -Pool.Area, -Misc.Feature,-Fireplace.Qu, -PID)

#cleaning
plot_missing(df.clean)

#change all character columns into factors
df.clean[sapply(df.clean, is.character)] <- lapply(df.clean[sapply(df.clean, is.character)], 
                                       as.factor)
str(df.clean)
summary(df.clean)

#doesn't make sense to use, way to many variables
library(DataExplorer)
plot_correlation(df)




#Notes:

#find specific row
which(is.na(df$Garage.Cars))

df[2237,]

#Subsets
inv<-subset(df, df$Garage.Qual=="NoGarage")
inv$Garage.Yr.Blt[max(df$Garage.Yr.Blt)]<-2007

summary(df.clean$SalePrice)



#visualizations

#good plot for average sale price by neighborhood 
df.clean %>%
  ggplot(aes(SalePrice, fct_reorder(Neighborhood,SalePrice))) +
  stat_summary(fun.y = "mean", geom="point") +
  theme(text = element_text(6),
        axis.text.y = element_text(6)) +
  scale_y_discrete(name=element_blank()) +
  scale_x_continuous(name="Average Price")+
  labs(title="Average Price by Neighborhood",
       caption="Data source: Ames, Iowa Assessor's Office") +
  theme_bw() +
  theme(axis.text.y=element_text(size=9))


#count of homes by neighborhood
df.clean %>%
  group_by(Neighborhood) %>%
  summarise(cnt = n()) %>%
  ggplot(aes(cnt,fct_reorder(Neighborhood,cnt))) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+ 
  scale_y_discrete(name=element_blank()) +
  scale_x_continuous(name="Number of Homes")+
  labs(title="Number of Houses Sold by Neighborhood") 



#problem solutions 
set.seed(101)


#Stepwise model with neighborhood
full.1<-lm(SalePrice~., data=df.clean, family="binomial")
null.1<-lm(SalePrice~1, data=df.clean, family="binomial")
step.1<-step(null.1, list(lower=formula(null.1), upper=formula(full.1)), data=df.clean, direction="both", trace=0)
#adjusted Rsquared = 0.9175, significant p-value

#without location, compare to see the impact
df.noLocation<-select(df.clean, -Neighborhood)
full.2<-lm(SalePrice~., data=df.noLocation, family="binomial")
null.2<-lm(SalePrice~1, data=df.noLocation, family="binomial")
step.2<-step(null.2, list(lower=formula(null.2), upper=formula(full.2)), data=df.noLocation, direction="both", trace=0)
#adjusted Rsquared = 0.9061, signifcant p-value


#final model visuals and model comparison visuals
install.packages("jtools")
library(jtools)
summ(step.1)
summ(step.2)
summary(step.1)

plot(step.1, which=c(1))
#while there does appear to be a slight pattern here, it does not appear that constant variance is serious issue for this model

plot(step.1, which=c(2))
#Data appears to be roughly normally distributed

plot(step.1, which=c(5))
#2181 and 2182 are influential points and should be removed 

remove<-c(2181,2182)
df.final<-df.clean[-remove,]


#Final stepwise model
full.3<-lm(SalePrice~., data=df.final, family="binomial")
null.3<-lm(SalePrice~1, data=df.final, family="binomial")
step.3<-step(null.3, list(lower=formula(null.3), upper=formula(full.3)), data=df.final, direction="both", trace=0)
summ(step.3)
summary(step.3)
#adjusted Rsquared = 0.9344, significant p-



#Markdown problem: pandoc error 11

#solution on github:

# Download pandoc 2.7.1 built with ghc-8.6.4, and instruct
# RStudio + rmarkdown to use it.

local({
  
  # The directory where Pandoc will be extracted. Feel free
  # to adjust this path as appropriate.
  dir <- "C:/Users/DPDav/OneDrive/Desktop/Documents/Current Classes/ISA 616/ISA616_Project"
  # The version of Pandoc to be installed.
  version <- "2.7.1"
  
  # Create and move to the requested directory.
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  owd <- setwd(dir)
  on.exit(setwd(owd), add = TRUE)
  
  # Construct path to pandoc.
  root <- "https://s3.amazonaws.com/rstudio-buildtools"
  suffix <- sprintf("pandoc-%s-windows-x86_64.zip", version)
  url <- file.path(root, "pandoc-rstudio", version, suffix)
  
  # Download and extract pandoc.
  file <- basename(url)
  utils::download.file(url, destfile = file)
  utils::unzip(file)
  unlink(file)
  
  # Write .Renviron to update the version of Pandoc used.
  entry <- paste("RSTUDIO_PANDOC", shQuote(path.expand(dir)), sep = " = ")
  contents <- if (file.exists("~/.Renviron")) readLines("~/.Renviron")
  filtered <- grep("^RSTUDIO_PANDOC", contents, value = TRUE, invert = TRUE)
  amended <- union(filtered, entry)
  writeLines(amended, "~/.Renviron")
  
  # Report change to the user.
  writeLines("Updated .Renviron:\n")
  writeLines(amended)
  writeLines("\nPlease restart RStudio for these changes to take effect.")
  
})
