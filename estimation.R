# please change here --------------------------------------------
dirname = "~/Dropbox/saVAST_egg"


# packages ------------------------------------------------------
##=====if needed=====##
#install.packages("dplyr")
#install.packages("devtools")

##=====necessary=====##
#install.packages("TMB")
#install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)

#require(devtools)
#devtools::install_github("nwfsc-assess/geostatistical_delta-GLMM") 
#devtools::install_github("james-thorson/VAST")

##if you get some errors and cannot download the VAST, please try the below code.
##i confirmed that the VAST can be installed in the latest version of R (version 3.6.1) and RStudio (Version 1.2.1335).
#devtools::install_github("james-thorson/VAST", INSTALL_opts = c("--no-multiarch --no-test-load"))


require(dplyr)
require(TMB)
require(VAST)


# estimation ----------------------------------------------------
setwd(dir = dirname)
data = read.csv("df_egg_saba2.csv", fileEncoding = "CP932")
head(data)
sakana = c("chub", "spotted")[2]
df = filter(data, area_no < 4, between(month, 1, 6)) %>% select(-area, -area_no, -mean_SST, -mean_salinity)
df = df %>% mutate(time = paste(df$year, df$month, sep="_"))
summary(df)

#####3 Settings#####
#Version = get_latest_version( package="VAST" )
Version = "VAST_v4_2_0"

###3.1 Spatial settings###
Method = c("Grid", "Mesh", "Spherical_mesh")[2]
Kmeans_Config = list("randomseed" = 1, "nstart" = 100, "iter.max" = 1000)
grid_size_km = 0.5
n_x = 100

###3.2 Model settings###
FieldConfig = c(Omega1 = 1, Epsilon1 = 1, Omega2 = 1, Epsilon2 = 1)
RhoConfig = c(Beta1 = 0, Beta2 = 0, Epsilon1 = 0, Epsilon2 = 0) #0: fixed, 1: independent, 2:RW, 3:constant, 4:AR
OverdispersionConfig = c("Eta1" = 1, "Eta2" = 1) #overdispersion
ObsModel = c(PosDist = 1, Link = 0) #[1] = 1(lognormal) or 2(gamma), [2] = 0
Options = c(SD_site_density = 0, SD_site_logdensity = 0,
            Calculate_Range = 1, Calculate_evenness = 0, 
            Calculate_effective_area = 1, Calculate_Cov_SE = 0, 
            Calculate_Synchrony = 0, Calculate_Coherence = 0)

###3.3 Stratification for results###
strata.limits = data.frame(STRATA = "All_areas")

###3.4 Derived objects###
Region = "other"
#Species_set = unique(df$Month)

###3.5 Save settings###
DateFile = paste0(getwd(), "/vast", Sys.Date(), "_lnorm_log", n_x, sakana, "fixed")
dir.create(DateFile)
Record = list(Version = Version, Method = Method, grid_size_km = grid_size_km, n_x = n_x, 
              FieldConfig = FieldConfig, RhoConfig = RhoConfig, OverdispersionConfig = OverdispersionConfig, 
              ObsModel = ObsModel, Kmeans_Config = Kmeans_Config, Region = Region,
              strata.limits = strata.limits) 
setwd(dir = DateFile)
save(Record, file = file.path(DateFile, "Record.RData")) 
capture.output(Record, file = paste0(DateFile, "/Record.txt"))

#####4. Prepare the data#####
###4.1 Data-frame for catch-rate data###
df = df[, c("year", "month", "lon", "lat", "chub", "time", "spotted")]
colnames(df) = c("Year", "Month", "Lon", "Lat", "Chub", "Time", "Catch_KG")
summary(df)
Data_Geostat = df

#####4.2 Extrapolation grid#####
Extrapolation_List = FishStatsUtils::make_extrapolation_info(
  Regio = "other", #zone range in Japan is 51:56
  strata.limits = strata.limits, 
  observations_LL = Data_Geostat[, c("Lat", "Lon")], 
)

###4.3 derived objects for spatio-temporal estimation###
Spatial_List = FishStatsUtils::make_spatial_info(
  n_x = n_x,
  Lon = Data_Geostat[, "Lon"], 
  Lat = Data_Geostat[, "Lat"], 
  Extrapolation_List = Extrapolation_List, 
  Method = Method,
  grid_size_km = grid_size_km,
  randomseed = Kmeans_Config[["randomseed"]], 
  nstart = Kmeans_Config[["nstart"]], 
  iter.max = Kmeans_Config[["iter.max"]], 
  #fine_scale = TRUE,
  DirPath = DateFile,
  Save_Results = TRUE)

Data_Geostat = cbind(Data_Geostat, knot_i = Spatial_List[["knot_i"]])
setwd(dir = DateFile)
write.csv(Data_Geostat, "Data_Geostat.csv", fileEncoding = "CP932")

#####5 Build and run model#####
TmbData = make_data(
  Version = Version, 
  FieldConfig = FieldConfig, 
  OverdispersionConfig = OverdispersionConfig, 
  RhoConfig = RhoConfig, 
  ObsModel = ObsModel, 
  c_iz = rep(0,nrow(Data_Geostat)),
  #c_iz = as.matrix(Data_Geostat[, "Month"] - min(Data_Geostat[, "Month"])),
  b_i = Data_Geostat[, "Catch_KG"], 
  a_i = rep(1, nrow(Data_Geostat)), 
  s_i = Data_Geostat[, "knot_i"] - 1, 
  t_iz = Data_Geostat[, "Year"], 
  Q_ik = matrix(log(Data_Geostat[, "Chub"]+0.1), ncol = 1),
  v_i = matrix(Data_Geostat[, "Time"], ncol = 1),
  a_xl = Spatial_List$a_xl, 
  MeshList = Spatial_List$MeshList, 
  GridList = Spatial_List$GridList, 
  Method = Spatial_List$Method, 
  Aniso = TRUE,
  spatial_list = Spatial_List,
  Options = c(SD_site_density = 1, 
              SD_site_longensity = 1, 
              Calculate_Range = 1, 
              Calculate_evenness = 1, 
              Calculate_effective_area = 1)
)

TmbList = VAST::make_model(TmbData = TmbData,
                           RunDir = DateFile,
                           Version = Version,
                           RhoConfig = RhoConfig,
                           loc_x = Spatial_List$loc_x,
                           Method = Spatial_List$Method)

Obj = TmbList[["Obj"]]
Opt = TMBhelper::Optimize(obj = Obj, 
                          lower = TmbList[["Lower"]], 
                          upper = TmbList[["Upper"]],
                          getsd = TRUE, 
                          savedir = DateFile, 
                          bias.correct = TRUE)

Report = Obj$report()
Save = list("Opt" = Opt, 
            "Report" = Report, 
            "ParHat" = Obj$env$parList(Opt$par),
            "TmbData" = TmbData)
save(Save, file = paste0(DateFile,"/Save.RData"))

setwd(DateFile)
plot_data(Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, Data_Geostat=Data_Geostat, PlotDir=DateFile )
pander::pandoc.table( Opt$diagnostics[,c('Param','Lower','MLE','Upper','final_gradient')] ) 
Enc_prob = plot_encounter_diagnostic( Report=Report, Data_Geostat=Data_Geostat, DirName=DateFile)
Q = plot_quantile_diagnostic( TmbData=TmbData, Report=Report, FileName_PP="Posterior_Predictive",
                              FileName_Phist="Posterior_Predictive-Histogram", 
                              FileName_QQ="Q-Q_plot", FileName_Qhist="Q-Q_hist", DateFile=DateFile )
MapDetails_List = make_map_info( "Region"=Region, "spatial_list"=Spatial_List, "Extrapolation_List"=Extrapolation_List )
Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
Years2Include = which( Year_Set %in% sort(unique(Data_Geostat[,'Year'])))
plot_residuals(Lat_i=Data_Geostat[,'Lat'], Lon_i=Data_Geostat[,'Lon'], TmbData=TmbData, Report=Report, Q=Q, savedir=DateFile, MappingDetails=MapDetails_List[["MappingDetails"]], PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8)
plot_anisotropy( FileName=paste0(DateFile,"Aniso.png"), Report=Report, TmbData=TmbData )

Dens_xt = plot_maps(plot_set=c(3), MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALS, pch = 19)
Dens_DF = cbind( "Density"=as.vector(Dens_xt), "Year"=Year_Set[col(Dens_xt)], "E_km"=Spatial_List$MeshList$loc_x[row(Dens_xt),'E_km'], "N_km"=Spatial_List$MeshList$loc_x[row(Dens_xt),'N_km'] )

plot_maps(plot_set=c(3), MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, category_names=levels(Data_Geostat[,'Month']))

pander::pandoc.table( Dens_DF[1:6,], digits=3 )
Index = plot_biomass_index( DirName=DateFile, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, Years2Include=Years2Include, use_biascorr=TRUE, category_names=levels(Data_Geostat[,"Month"]))
pander::pandoc.table( Index$Table[,c("Year","Fleet","Estimate_metric_tons","SD_log","SD_mt")] ) 
plot_range_index(Report=Report, TmbData=TmbData, Sdreport=Opt[["SD"]], Znames=colnames(TmbData$Z_xm), PlotDir=DateFile, Year_Set=Year_Set)
