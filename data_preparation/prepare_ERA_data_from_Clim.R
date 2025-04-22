### Settings
PROCESS_CLIMATE_FILES <- TRUE  # Flag to process climate from ERA.Compiled

research_crop <- "Maize"
research_target <- "Crop Yield"
exclusion_buffer <- 10000
M.ORIGIN <- "1900-01-01"
cores <- max(1, parallel::detectCores() - 1) # N cores for parallel processing
ID <- "Site.Key"


### Load packages
if(!require("pacman", character.only = TRUE)){
  install.packages("pacman",dependencies = T)
}
source("R/ERAWide2Long_mod.R")
source("R/ERA_convert_units.R")
source("R/map_to_usda_texture.R")
if(!require("ERAg", character.only = TRUE)){
  devtools::install_github("https://github.com/EiA2030/ERAg",dependencies = T)
}
pacman::p_load(data.table,miceadds,dplyr,ERAgON,tidyverse)


#####
### Calculate Climate data
if (PROCESS_CLIMATE_FILES == TRUE){
  ### Load packages
  if(!require("pacman", character.only = TRUE)){
    install.packages("pacman",dependencies = T)
  }
  required.packages <- c("data.table", "doSNOW", "ERAg", "ERAgON", "ncdf4", 
                         "raster", "sp", "terra", "arrow", "ggplot2", "circular",
                         "zoo", "pbapply", "dismo", "miceadds")
  p_load(char=required.packages,install = T,character.only = T)
  source('R/local_CalcClimate.R')
  
  ### Get fields' description
  table_output <- knitr::kable(ERAg::ERACompiledFields[
    ,c("Field.Name", "Description")])
  write.table(table_output, file = "era_fields_table.txt", row.names = FALSE, 
              col.names = FALSE, quote = FALSE)
 
  
  ERA <- ERAg::ERA.Compiled[Buffer<exclusion_buffer]
  
  ### Create directories
  # Create a directory for the climate analysis
  AnalysisDir<-"ERA Climate Analysis/"
  if(!dir.exists(AnalysisDir)){
    dir.create(AnalysisDir)
  }
  # Create a directory for strange or incorrect ERA values to investigate
  ErrorDir<-paste0(AnalysisDir,"Potential Errors/")
  if(!dir.exists(ErrorDir)){
    dir.create(ErrorDir)
  }
  # Create a directory for crop specific information
  CROP_dir<-paste0(AnalysisDir,"Crops/") 
  if(!dir.exists(CROP_dir)){
    dir.create(CROP_dir)
  }
  
  
  ### Prepare ERA dataset
  ERA[Season.Start==1 & Season.End==1,Season:="1"
      ][Season.Start==2 & Season.End==2,Season:="2"
        ][Season.Start==1 & Season.End==2,Season:="1&2"
          ][M.Year=="",M.Year:=NA
            ][Season=="",Season:=NA]
  
  ERA[,Harvest.Start:=as.Date(Harvest.Start,"%d.%m.%Y")
      ][,Harvest.End:=as.Date(Harvest.End,"%d.%m.%Y")
        ][,Plant.Start:=as.Date(Plant.Start,"%d.%m.%Y")
          ][,Plant.End:=as.Date(Plant.End,"%d.%m.%Y")]
  
  Physical<-data.table(ERAgON::ERA_Physical)
  
  # Add DEM altitude to ERA
  ERA[!is.na(Latitude),Altitude.DEM:=Physical[match(unlist(
    ERA[!is.na(Latitude),..ID]),unlist(Physical[,..ID])) , Altitude.mean]]
  
  ERA[,Altitude:=Elevation][is.na(Altitude)|Altitude=="",Altitude:=Altitude.DEM]
  
  
  # Subset data
  ERA.Yields <- ERA[Out.SubInd == research_target &
                      (M.Year.Start == M.Year.End &
                         (Season %in% c(1,2)|is.na(Season))) &
                      Product %in%  research_crop &
                      !grepl(" x |-",Product)]
  
  
  ### Prepare POWER.CHIRPS data
  unique_site_keys <- unique(ERA.Yields$Site.Key)
  
  POWER.CHIRPS <- arrow::read_parquet("../databases/ERA/POWER.CHIRPS.parquet")
  POWER.CHIRPS <- POWER.CHIRPS[Site.Key %in% unique_site_keys]
  
  ### Add EcoCrop data
  ERA.Yields <- cbind(ERA.Yields,ERAgON::AddEcoCrop(Products = ERA.Yields[,Product]))
  
  ### Calculate ETo if absent
  if(POWER.CHIRPS[,is.null(ETo)]){
    POWER.CHIRPS[,ETo:=ERAgON::PETcalc(Tmin=Temp.Min,
                                       Tmax=Temp.Max,
                                       SRad=Solar.Rad,
                                       Wind=WindSpeed,
                                       Rain=Rain,
                                       Pressure=Pressure,
                                       Humid=Humid,
                                       YearDay=Day,
                                       Latitude=Latitude,
                                       Altitude=Altitude)[,1]]}
  # Ensure fields are correct classes
  POWER.CHIRPS[,Site.Key :=as.factor(Site.Key )
               ][,Year:=as.factor(Year)
                 ][,Day:=as.factor(Day)
                   ][,Altitude:=as.integer(Altitude)]
  
  ### Estimate unreporting planting dates
  ERA.Yields<-EstPDayData(DATA=ERA.Yields)$DATA
  round(sum(!is.na(ERA.Yields$Data.PS.Date))/sum(is.na(ERA.Yields$Plant.Start))*100,2)
  
  ### Estimate season lengths where harvest dates are missing
  ERA.Yields<-EstSLenData(DATA=ERA.Yields)
  round(sum(!is.na(ERA.Yields$SLen))/sum(is.na(ERA.Yields$Data.SLen))*100,2)
  
  ### Refine EcoCrop cycle lengths
  # Estimate season length from date allowing 7 days uncertainty in planting and harvest dates
  A<-ERA.Yields
  A<-unique(A[!(is.na(Plant.Start)|is.na(Plant.End)|is.na(Harvest.End)|is.na(Harvest.End))
              ][,PLANT.AVG:=Plant.Start+(Plant.End-Plant.Start)/2
                ][,HARVEST.AVG:=Harvest.Start+(Harvest.End-Harvest.Start)/2
                  ][!(((Plant.End-Plant.Start)>7)|((Harvest.End-Harvest.Start)>7))
                    ][,SLEN:=round(HARVEST.AVG-PLANT.AVG,0)
                      ][!SLEN<30,c("Product",..ID,"Code","Variety","M.Year","PLANT.AVG","SLEN","Country")])
  
  B<-A[,list(SLEN.mean=mean(SLEN),SLEN.med=median(SLEN),N=.N),by=c("Product")
       ][,SLEN.mean:=round(as.numeric(SLEN.mean),0)
         ][,SLEN.med:=round(as.numeric(SLEN.med),0)]
  
  B[,SLEN.EcoC:=round(apply(ERA.Yields[match(B$Product,ERA.Yields$Product),c("cycle_min","cycle_max")],1,mean),0)]
  
  
  ### Replace EcoCrop values where we have >=5 observation from our data
  B<-B[N>=5]
  # Save estimate crop length datasets
  fwrite(B,paste0(CROP_dir,"Crop Season Lengths Estimated From Data.csv"))
  
  
  ### Add estimate season lengths to ERA where season length values are missing
  N<-match(ERA.Yields$Product,B$Product)
  N1<-which(!is.na(N))
  ERA.Yields$cycle_min[N1]<-B$SLEN.med[N[N1]]
  ERA.Yields$cycle_max[N1]<-B$SLEN.med[N[N1]]
  
  
  ### Refine uncertain planting dates using rainfall data
  ERA.Yields<-EstPDayRain(Data=ERA.Yields, 
                          ID=ID, 
                          Rain.Data = POWER.CHIRPS, # You could also use POWER.CHIRPS here
                          Rain.Data.Name = "CHIRPS", 
                          Rain.Field ="Rain", 
                          # DaysBefore: When searching for rainfall events in-between the uncertain planting start/end dates
                          # supplied in ERA.Yieds extend the planting start date backwards by 2 days
                          DaysBefore = 2,
                          #  MultiplyWin: a proportion that changes the size of the difference between plant start and plant 
                          # end, 1 = no change
                          MultiplyWin = 1, 
                          # Window: add two addition temporal periods beyond the initial temporal window of 14 days
                          Window = c(14,14), 
                          # Widths: We need to set a threshold for the rainfall amount in mm that triggers planting within each 
                          # window in this case there are 3 windows 1 + the 2 extra windows specified in the `Window` argument
                          Widths = c(2,3,3), 
                          # Rain.Thresholds: We need to set a threshold for the rainfall amount in mm that triggers planting in each 
                          # window if `Widths[1]=2` and `Rain.Thresholds[1]=30` then 30mm needs to fall over 2 days within the initial
                          # window of plant start to plant end dates (as modified by DaysBefore and  MultiplyWin arguments) for 
                          # planting to occur. If the threshold is not met then function iteratively goes to `Window[1]`, `Widths[2]` and 
                          # `Rain.Thresholds[2]`.
                          Rain.Thresholds = c(30,20,15), 
                          # Uncertainty.Min/Uncertainty.Max: refine planting dates with uncertainty of between 7-90 days
                          Uncertainty.Min = 7, 
                          Uncertainty.Max = 90, 
                          Add.Values = T, 
                          Use.Data.Dates = T  
  )
  
  
  ### Eliminate observations that led to errors 
  # ERA.Yields <- ERA.Yields[Site.Key != "11.8060 13.1990 B500"]
  # POWER.CHIRPS <- POWER.CHIRPS[Site.Key != "11.8060 13.1990 B500"]
  
  ### Calculate Climate Variables
  Climate <- local_CalcClimate(DATA=ERA.Yields,
                         ID=ID,
                         CLIMATE=POWER.CHIRPS,
                         Rain.Data.Name="CHIRPS", 
                         Temp.Data.Name="POWER",
                         # Rain.Windows: In estimation of planting dates for long-term climate calculation we will look for
                         # rainfall events in 4 windows: 6 weeks before planting (the first element supplied is always before
                         # planting set to 0 if you do not want to look before planting), then in three consecutive windows
                         # of 4, 2 and 2 weeks
                         Rain.Windows = c(6*7,4*7,2*7,2*7),  
                         # Widths: To estimate planting dates rainfall is summed in a scanning window for each of the windows 
                         # specified in Rain.Windows; we have 4 windows so need to supply 4 values in days to this argument
                         Widths = c(3,3,2,2),  
                         # Rain.Threshold: We need to set a threshold for the rainfall amount in mm that triggers planting in 
                         # each combination of `Rain.Window` x `Width`, again 4 values. If Rain.Window = 4*7 & Width = 3 and 
                         # Rain.Threshold = 30 then the function looks within a 21 window for cumulative rainfall of 30mm over 
                         # 3 days
                         Rain.Threshold = c(30,30,20,15), 
                         # Win.Start: When calculating climate from planting.dates and harvest.dates Win.Start adjust the 
                         # beginning date of the calculation window. -3 means the window begins 3 days before planting
                         Win.Start=-3,
                         # Max.LT.Avg: Calculations of long-term climate only use years up to and including 2010
                         Max.LT.Avg=2010, 
                         Do.LT.Avg=T, 
                         Do.BioClim=T, 
                         # Windows: By default this function looks at two calculation windows `Data` and `EcoCrop`. Additional 
                         # windows can specified the `Windows` argument. In the line below the additional window will called 
                         # `Plant.1-30` in the `W.Name` field of output data.tables and it begins 1 day after planting and ends 
                         # 30 days after planting. This window explores the post-planting climate, a critical period for crop 
                         # establishment and success
                         Windows=data.table(Name="Plant.1-30",Start=1,End=30), 
                         SaveDir=AnalysisDir,
                         ErrorDir=NA,
                         ROUND=3)
}
#####

### Changing ID column
ID <- "ID"

### Set working directory
ClimDir <- "ERA Climate Analysis/Analysis/422814143322303020152010130-3POWCHIFALSE0.6/"
if(!dir.exists(ClimDir)){
  dir.create(ClimDir)
}


### Load Data
ClimateA<-load(paste0(ClimDir,"ClimStatsA.RData"))
ERA<-load(paste0(ClimDir,"Data.RData"))
Soils<-ERAgON::ERA_ISDA


### Prepare ERA data
setDT(DATA)
DATA <- data.table::copy(DATA)

climate_ids <- unique(Seasonal$Observed[,ID])

DATA <- DATA[Product == research_crop &
               !grepl("\\.\\.", Site.ID) &
               Irrigation.C == FALSE & Irrigation.T == FALSE &
               Site.Type %in% c("Station", "Station.Farm", "Farm", "Farm & Station") &
               (is.na(Mulch.Code) | Mulch.Code != "b26") &
               Code != "EO0006" &
               Site.Key %in% climate_ids &
               nchar(M.Year) < 7 &
               !(T1 == "b26" | T2 == "b26" | T3 == "b26" | T4 == "b26" | 
                   T5 == "b26" | T6 == "b26" | T7 == "b26" | T8 == "b26")]


  

era_ids <- unique(DATA[,ID])


### Basic check
high_yields <- DATA[MeanT > 10000, ]
# View(high_yields)
print(DATA [Code %in% high_yields$Code, MeanT] )  # This seems fine
hist(DATA[, MeanT])


### Prepare Soils data
Soils <- Soils[Site.Key %in% era_ids, ]

Soils[grepl("0..200cm",Variable),Depth.Upper:=0
      ][grepl("0..200cm",Variable),Depth.Lower:=200
        ][,Variable:=gsub("_m_30m_0..200cm","",Variable)]

Soils[grepl("0..20cm",Variable),Depth.Upper:=0
      ][grepl("0..20cm",Variable),Depth.Lower:=20
        ][,Variable:=gsub("_m_30m_0..20cm","",Variable)]

Soils[grepl("20..50cm",Variable),Depth.Upper:=20
      ][grepl("20..50cm",Variable),Depth.Lower:=50
        ][,Variable:=gsub("_m_30m_20..50cm","",Variable)]

Soils[,unique(Variable)]

Soils[,Weight:=Depth.Lower-Depth.Upper]

Soils<-unique(Soils[,list(Median=weighted.mean(Median,Weight),
                          Mean=weighted.mean(Mean,Weight),
                          SD=weighted.mean(SD,Weight),
                          Mode=Mode[Weight==max(Weight)]),
                    by=list(Variable,Site.Key)])

Soils<-dcast(Soils,Site.Key~Variable,value.var = c("Median","Mean","SD","Mode"))


### Prepare Long Term and Observed Climate data
Clim.Data <- Seasonal[["Observed"]]
Clim.Data <- Clim.Data[ID %in% era_ids, ]

# Concatenate location, crop codes and planting date fields
Clim.Data[,MCode:=paste(ID,EU,PD.Used,sep="|")]
DATA[,MCode:=paste(Site.Key,EU,P.Date.Merge,sep="|")]

LTAvg.Clim.Data<-Seasonal[["LongTerm"]][["LT.Clim.Avg"]]
LTAvg.Clim.Data <- LTAvg.Clim.Data [ID %in% era_ids]

# Concatenate location, crop codes and season fields
LTAvg.Clim.Data[,MCode.LT:=paste(ID,EU,Season)]
Clim.Data[,MCode.LT:=paste(ID,EU,Season)]


### Merge Climate & Soil Data with ERA
ERA.Clim.Soil<-lapply(Clim.Data[,unique(W.Name)],FUN=function(X){
  
  # Subset seasonal climate data to calculation window X
  CData<-Clim.Data[W.Name==X]
  
  # Subset long-term climate averages to calculation window X
  LT.Clim.Data<-LTAvg.Clim.Data[W.Name==X]
  
  # Subset long-term climate averages for mean and median variables
  LT.Clim.Data.Mean<-LT.Clim.Data[Variable=="Mean"]
  LT.Clim.Data.Median<-LT.Clim.Data[Variable=="Median"]
  
  # Make an object containing the column names of climate variables for which we wish to calculate deviance
  Cols<-colnames(CData[,!c("Tmax.sd","Tmean.sd","PD.Used","W.Start","W.End","W.Name","ID","M.Year","Season","MCode","MCode.LT","EU","ETo.NA")])
  
  # Subtract long term average climate from the observed seasonal climate
  Mean.Diff<-CData[,..Cols]-LT.Clim.Data.Mean[match(CData[,MCode.LT],LT.Clim.Data.Mean[,MCode.LT])][,..Cols]
  Median.Diff<-CData[,..Cols]-LT.Clim.Data.Median[match(CData[,MCode.LT],LT.Clim.Data.Median[,MCode.LT])][,..Cols]
  
  # Rename columns to reflect they are now deviance from long-term mean or median values
  colnames(Mean.Diff)<-paste0(colnames(Mean.Diff),".Dev.Mean")
  colnames(Median.Diff)<-paste0(colnames(Median.Diff),".Dev.Median")
  
  # Join seasonal climate with deviance from long-term average tables
  CData<-cbind(CData,Mean.Diff,Median.Diff)
  
  # Match soils data to ERA using Site.Key field
  SoilData<-Soils[match(DATA[,Site.Key],Site.Key)]
  
  # Join ERA, Climate and Soils datasets removing any duplicated columns
  cbind(DATA, 
        CData[match(DATA$MCode,MCode),!c("MCode","EU","ID")], 
        SoilData[,!"Site.Key"])
})

# Name list levels
names(ERA.Clim.Soil)<-unique(Clim.Data$W.Name)

data_ERA.Clim.Soil <- ERAWide2Long_mod(ERA.Clim.Soil$Data)
EcoCrop_ERA.Clim.Soil <- ERAWide2Long_mod(ERA.Clim.Soil$EcoCrop)
Plant1_30_ERA.Clim.Soil <- ERAWide2Long_mod(ERA.Clim.Soil$`Plant.1-30`)
PreSowing_ERA.Clim.Soil <- ERAWide2Long_mod(ERA.Clim.Soil$PreSowing)

unique_cols <- !duplicated(names(data_ERA.Clim.Soil))
data_ERA.Clim.Soil <- data_ERA.Clim.Soil[, ..unique_cols]

unique_cols <- !duplicated(names(EcoCrop_ERA.Clim.Soil))
EcoCrop_ERA.Clim.Soil <- EcoCrop_ERA.Clim.Soil[, ..unique_cols]

unique_cols <- !duplicated(names(Plant1_30_ERA.Clim.Soil))
Plant1_30_ERA.Clim.Soil <- Plant1_30_ERA.Clim.Soil[, ..unique_cols]

unique_cols <- !duplicated(names(PreSowing_ERA.Clim.Soil))
PreSowing_ERA.Clim.Soil <- PreSowing_ERA.Clim.Soil[, ..unique_cols]

data_ERA.Clim.Soil <- unique(data_ERA.Clim.Soil)
EcoCrop_ERA.Clim.Soil <- unique(EcoCrop_ERA.Clim.Soil)
Plant1_30_ERA.Clim.Soil <- unique(Plant1_30_ERA.Clim.Soil)
PreSowing_ERA.Clim.Soil <- unique(PreSowing_ERA.Clim.Soil)


### Eliminate NAs from the predictors
datasets = list(data_ERA.Clim.Soil, EcoCrop_ERA.Clim.Soil, 
                Plant1_30_ERA.Clim.Soil, PreSowing_ERA.Clim.Soil)
datasets_names = list("data_ERA_Clim_Soil", "EcoCrop_ERA_Clim_Soil", 
                      "Plant1_30_ERA_Clim_Soil", "PreSowing_ERA_Clim_Soil")

setnames(PreSowing_ERA.Clim.Soil, "Rain.sum", "Presowing.Rain.sum")

for (i in 1:length(datasets)){
  dataset <- datasets[[i]]
  dataset_name <- datasets_names[[i]]
  dataset <- dataset[, .SD, .SDcols = unique(names(dataset))]
  na_counts <- data.frame(
    Column = colnames(dataset),
    NA_Count = sapply(dataset, function(x) sum(is.na(x)))
  )
  # # Checks
  # na_counts <- data.frame(
  #   Column = colnames(dataset),
  #   NA_Count = sapply(dataset, function(x) sum(is.na(x))))
  # row.names(na_counts) <- NULL
  
  # Drop observations with no climate data
  if (dataset_name != "PreSowing_ERA_Clim_Soil"){
    setkey(dataset, Site.Key, TID, Code, T.Descrip, MeanT)
    setkey(PreSowing_ERA.Clim.Soil, Site.Key, TID, Code, T.Descrip, MeanT)
    copy <- copy(dataset)
    copy[PreSowing_ERA.Clim.Soil, Presowing.Rain.sum := i.Presowing.Rain.sum]
    dataset <- copy
    predictors <- colnames(dataset)[c(128:160, 161:210, 211:283)]
    predictors <- append(predictors, colnames(dataset)[length(colnames(dataset))])  
  } else{
    predictors <- colnames(dataset)[c(128:160, 161:210, 211:283)]
  }
  
  dataset_noNA <- dataset[!is.na(get(predictors[1])) & 
                            rowSums(is.na(dataset[, ..predictors])) == 0]
  
  # Convert to data.frame
  dataset_noNA_df <- as.data.frame(dataset_noNA)
  dataset_noNA_df <- dataset_noNA_df %>%
    mutate(Soil.Texture.USDA = sapply(Soil.Texture, map_to_usda_texture))
  
  dataset_noNA_df <- ERA_convert_units(dataset_noNA_df)
  
  hist(dataset_noNA_df$MeanT, main=dataset_name)
  
  # # Checks
  low_yields<- dataset_noNA_df %>% filter(MeanT<100) %>% dplyr::select(MeanT)
  hist(low_yields$MeanT, main=paste(dataset_name, "Low yields"))
  
  if (dataset_name != "PreSowing_ERA_Clim_Soil"){
  dataset_noNA_df <- dataset_noNA_df %>%
    filter(!MeanT < 100,
           Rain.sum > 100)
  }
  dataset_noNA_df <- dataset_noNA_df %>%
    rename(yield = MeanT)
  
  write_csv(dataset_noNA_df, paste0("outputs/era/psw_", dataset_name, ".csv"))
  saveRDS(dataset_noNA_df, file=paste0("outputs/era/psw_", dataset_name, ".rds"))
}

### Data checks
### Additional Checks
count(dataset_noNA_df %>% filter(yield < 1000) %>% dplyr::select(yield))
low_yield_codes <- unique(dataset_noNA_df %>% filter(yield < 1000) %>% dplyr::select(Code))

low_yield_exp <- dataset_noNA_df %>%
  filter(Code %in% low_yield_codes$Code) %>%
  dplyr::select(Code, yield, TorC)

# These observations may be fine, they were Mg/ha.
consistent_low_yield_exp <- dataset_noNA_df %>%
  group_by(Code) %>%
  filter(all(yield < 1000)) %>%
  ungroup() %>%
  dplyr::select(Code, yield, TorC, OriUnits)

DATA %>% filter(Code == "NN0297") %>% dplyr::select(MeanT, MeanC, Code, Units)
