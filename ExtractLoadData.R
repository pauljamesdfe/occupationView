####
# Title: LSIP dashboard data load
# Author: Paul James
# Date: 30th Aug 2022
# Last updated: 28th Mar 2023
# Aim: Loads the original data sources used within the dashboard.
# Use: To update any datasets, delete the relevant file within the relevant folder, and paste in the new data file. Rerun this file and then run TransformData.R
# Sources: See the dashboard page Data sources for links to the datasets
# Notes: Ensure there is *only* the file(s) that will be used in each of the numbered folders in ./Data/
# Running time: ~20mins
# NB 2.2.2 has a clause to ignore the latest partial year of data (since we only work with full years). You may need to amend this.
# NB 2.1.1 and 2.1.4 are currently importing the last 4 populated datsets and so are hardcoded (following data issues). check if there is new data and then you can change to LATEST dat format
###

# Load libraries ----
library(openxlsx) # use read.xlsx, read.csv
library(sf) # use st_read
library(tidyverse) # use map_df, mutate
library(nomisr) # use nomis api
library(data.table) # use %like%
library(readxl)# read xls

# 1.Geography and mapping tables ----
## 1.1 LEPs 2020 and LA%20LSIP lookup ----
folder <- "1-1_GeogLkup"
sheetNum <- "LAD21 - LSIP - LEP21"
I_LEP2020 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 1.2 Missing LAD-LEP lookup----
# This happens because we have some old LADs in the ILR (and other) data that have since been made inactive. These do not feature in the most recent LAD-LEP matching. We have manually mapped these LADs to the latest LEPS (2021)
folder <- "1-2_LEPmissing"
sheetNum <- 2
I_missingLAD <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 1.3 MCA lookup ----
folder <- "1-3_MCA_lookup"
sheetNum <- 1
I_mcalookup <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 1.4 LA 2011 to 2021 lookup ----
folder <- "1-4_LaLookup"
I_LaLookup <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

## 1.5 LEP boundary----
folder <- "1-5_LEPBoundary"
I_mapLEP <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
  stringsAsFactors = F
)

## 1.6 LA boundary----
folder <- "1-6_LABoundary"
I_mapLA <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
  stringsAsFactors = F
)

## 1.7 MCA boundary----
folder <- "1-7_MCABoundary"
I_mapMCA <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
  stringsAsFactors = F
)

## 1.8 Region boundary----
folder <- "D_regionBoundary"
I_mapRegion <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
                        stringsAsFactors = F
)%>%
  sf::st_transform('+proj=longlat +datum=WGS84')

# 2. Datasets ----
## 2.1 Nomis datasets ----
# list of all the geogs we need (excluding the user defined which we add later)
geogUseAps <- nomis_get_metadata(id = "NM_17_1", concept = "geography", type = "type") %>%
  filter(description.en %in% c("local enterprise partnerships (as of April 2021)", "local authorities: district / unitary (as of April 2021)", "countries"))

# Get data and filter function for APS
extractNomis <- function(tableID, dates, cells) {
  bind_rows(
    # user defined pt 1 (do in two parts as falls over all together)
    nomis_get_data(
      id = tableID, date = dates, geography =
        "MAKE|Brighton%20and%20Hove%2C%20East%20Sussex%2C%20West%20Sussex%20LSIP|1811939621;1811939622;1811939560;1811939623;1811939624;1811939576;1811939577;1811939625;1811939578;1811939626;1811939579;1811939580;1811939627,MAKE|Buckinghamshire%20LSIP|1811939575,MAKE|Cambridgeshire%20and%20Peterborough%20LSIP|1811939479;1811939480;1811939481;1811939482;1811939476;1811939483,MAKE|Cambridgeshire%20and%20Peterborough%20MCA|1811939479;1811939480;1811939481;1811939482;1811939476;1811939483,MAKE|Cheshire%20and%20Warrington%20LSIP|1811939345;1811939346;1811939348,MAKE|Cornwall%20and%20the%20Isles%20of%20Scilly%20LSIP|1811939631;1811939632,MAKE|Cumbria%20LSIP|1811939349;1811939350;1811939351;1811939352;1811939353;1811939354,MAKE|Derbyshire%20and%20Nottinghamshire%20LSIP|1811939407;1811939436;1811939437;1811939408;1811939438;1811939409;1811939403;1811939410;1811939411;1811939439;1811939412;1811939440;1811939441;1811939413;1811939405;1811939442;1811939414,MAKE|Dorset%20LSIP|1811939649;1811939655,MAKE|Enterprise%20M3%20LEP%20(including%20all%20of%20Surrey)%20LSIP|1811939581;1811939582;1811939610;1811939611;1811939612;1811939586;1811939613;1811939614;1811939615;1811939589;1811939616;1811939617;1811939618;1811939590;1811939619;1811939591;1811939620,MAKE|Essex%2C%20Southend-on-Sea%20and%20Thurrock%20LSIP|1811939484;1811939485;1811939486;1811939487;1811939488;1811939489;1811939490;1811939491;1811939492;1811939493;1811939477;1811939494;1811939478;1811939495,MAKE|Gloucestershire%20LSIP|1811939656;1811939657;1811939658;1811939659;1811939660;1811939661,MAKE|Greater%20Lincolnshire%20LSIP|1811939422;1811939423;1811939424;1811939425;1811939406;1811939426;1811939427;1811939428;1811939384;1811939385,MAKE|Greater%20London%20LSIP|1811939540;1811939541;1811939542;1811939543;1811939544;1811939526;1811939527;1811939545;1811939546;1811939547;1811939548;1811939528;1811939529;1811939530;1811939549;1811939550;1811939551;1811939552;1811939531;1811939532;1811939553;1811939533;1811939534;1811939554;1811939535;1811939555;1811939556;1811939536;1811939557;1811939537;1811939558;1811939538;1811939539,MAKE|Greater%20Manchester%20LSIP|1811939355;1811939356;1811939357;1811939358;1811939359;1811939360;1811939361;1811939362;1811939363;1811939364,MAKE|Greater%20Manchester%20MCA|1811939355;1811939356;1811939357;1811939358;1811939359;1811939360;1811939361;1811939362;1811939363;1811939364,MAKE|Heart%20of%20the%20South-West%20LSIP|1811939640;1811939641;1811939662;1811939642;1811939643;1811939634;1811939663;1811939667;1811939644;1811939664;1811939645;1811939638;1811939646;1811939647,MAKE|Hertfordshire%20LSIP|1811939496;1811939497;1811939499;1811939500;1811939501;1811939503;1811939505;1811939506;1811939507;1811939509,MAKE|Hull%20and%20East%20Yorkshire%20LSIP|1811939382;1811939383,MAKE|Kent%20and%20Medway%20LSIP|1811939592;1811939593;1811939594;1811939595;1811939599;1811939596;1811939597;1811939562;1811939598;1811939601;1811939602;1811939603;1811939604,MAKE|Lancashire%20LSIP|1811939343;1811939344;1811939365;1811939366;1811939367;1811939368;1811939369;1811939370;1811939371;1811939372;1811939373;1811939374;1811939375;1811939376,MAKE|Leicester%20and%20Leicestershire%20LSIP|1811939415;1811939416;1811939417;1811939418;1811939404;1811939419;1811939420;1811939421,MAKE|Liverpool%20City%20Region%20LSIP|1811939347;1811939377;1811939378;1811939379;1811939380;1811939381,MAKE|Liverpool%20City%20Region%20MCA|1811939347;1811939377;1811939378;1811939379;1811939380;1811939381,MAKE|Norfolk%20and%20Suffolk%20LSIP|1811939517;1811939510;1811939511;1811939524;1811939512;1811939519;1811939513;1811939520;1811939514;1811939515;1811939516;1811939525",
      cell = cells
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, CELL_NAME, OBS_VALUE, MEASURES_NAME),
    # user defined pt 2
    nomis_get_data(
      id = tableID, date = dates, geography =
        "MAKE|North%20East%20LSIP|1811939330;1811939338;1811939341;1811939342,MAKE|North%20East%20MCA|1811939330;1811939338;1811939341;1811939342,MAKE|North%20of%20Tyne%20LSIP|1811939339;1811939340;1811939334,MAKE|North%20of%20Tyne%20MCA|1811939339;1811939340;1811939334,MAKE|Oxfordshire%20LSIP|1811939605;1811939606;1811939607;1811939608;1811939609,MAKE|Solent%20LSIP|1811939583;1811939584;1811939585;1811939587;1811939561;1811939588;1811939564;1811939567,MAKE|South%20East%20Midlands%20LSIP|1811939473;1811939474;1811939475;1811939768;1811939769;1811939563,MAKE|South%20Yorkshire%20LSIP|1811939394;1811939395;1811939396;1811939397,MAKE|South%20Yorkshire%20MCA|1811939394;1811939395;1811939396;1811939397,MAKE|Stoke-on-Trent%20and%20Staffordshire%20LSIP|1811939447;1811939448;1811939449;1811939450;1811939451;1811939452;1811939453;1811939445;1811939454,MAKE|Swindon%20and%20Wiltshire%20LSIP|1811939637;1811939639,MAKE|Tees%20Valley%20LSIP|1811939329;1811939331;1811939332;1811939335;1811939336,MAKE|Tees%20Valley%20MCA|1811939329;1811939331;1811939332;1811939335;1811939336,MAKE|Thames%20Valley%20Berkshire%20LSIP|1811939559;1811939565;1811939566;1811939568;1811939569;1811939570,MAKE|The%20Marches%20LSIP|1811939443;1811939444;1811939446,MAKE|West%20Midlands%20and%20Warwickshire%20LSIP|1811939460;1811939461;1811939462;1811939455;1811939456;1811939457;1811939463;1811939464;1811939458;1811939465;1811939459;1811939466,MAKE|West%20Midlands%20MCA|1811939460;1811939461;1811939462;1811939463;1811939464;1811939465;1811939466,MAKE|West%20of%20England%20and%20North%20Somerset%20LSIP|1811939628;1811939630;1811939633;1811939636,MAKE|West%20of%20England%20MCA|1811939628;1811939630;1811939636,MAKE|West%20Yorkshire%20LSIP|1811939398;1811939399;1811939400;1811939401;1811939402,MAKE|West%20Yorkshire%20MCA|1811939398;1811939399;1811939400;1811939401;1811939402,MAKE|Worcestershire%20LSIP|1811939467;1811939468;1811939469;1811939470;1811939471;1811939472,MAKE|York%20and%20North%20Yorkshire%20LSIP|1811939387;1811939388;1811939389;1811939390;1811939391;1811939392;1811939393;1811939386",
      cell = cells
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, CELL_NAME, OBS_VALUE, MEASURES_NAME),
    # other geogs
    nomis_get_data(
      id = tableID, date = dates, geography = geogUseAps$id,
      cell = cells
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, CELL_NAME, OBS_VALUE, MEASURES_NAME)
  ) %>%
    filter(MEASURES_NAME == "Value") %>%
    select(-MEASURES_NAME)
}

# list all the APS cells available
cellsListAps <- nomis_get_metadata(id = "NM_17_1", concept = "CELL")

### 2.1.1 Employment by occupation ----
# Query data
# Geography: England, LEPs, regions, LADs (as of April 2021)
# Date: 12 months to Dec 2021-2022. Older data is not availabel at SOC2020
# Cell: T09a Employment by occupation (SOC2010) sub-major group and full-time/part-time; All people/ All people
# find cells we want
# cellsUseAps_empOcc <- cellsListAps %>% filter(description.en %like% "T09a:" & description.en %like% "All people - ")
# # get data
# I_empOcc <-
#   extractNomis("NM_17_1", "2017-12,2018-12,2019-12,2020-12,2021-12", cellsUseAps_empOcc$id) %>%
#   filter(str_sub(CELL_NAME, -12, -1) == "All people )") # ignore part time

#Get last 5 years of employment count by 1) soc2020, 2) sex
I_empOcc2010<-nomis_get_data(
  id = "NM_168_1"
  , date = "latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest"
  , geography = "2092957699,2013265921...2013265932"
  , C_SEX = "0...2"#cellsUseAps_empOcc$id
  , c_occpuk11h_0="0,1000001,100000,10000,1,2,10001,3...5,10002,6...12,10003,13,10004,14,15,10005,16...18,10006,19,20,10007,21,100001,10008,22,23,10009,24...28,10010,29,30,10011,31...36,2000000,200000,20000,37...41,20001,42...48,20002,49...54,20003,55,56,20004,57,2000001,20005,58...66,20006,67...70,20007,71,72,200002,20008,73...80,200003,20009,81...83,20010,84...89,20011,90...95,20012,96...99,20013,100,101,20014,102...104,20015,105...107,3000000,300000,30000,108...114,30001,115,116,30002,117,118,300001,30003,119...123,30004,124...128,300003,30005,129...134,300004,30006,135...141,30007,142,143,30008,144...146,300005,30009,147...149,30010,150,30011,151...159,30012,160...165,30013,166,30014,167...172,4000000,400000,40000,173...175,40001,176...180,40002,181...186,40003,187,188,40004,189,190,400001,40005,191...197,5000000,500000,50000,198...202,500001,50001,203...208,50002,209...213,50003,214...219,50004,220...224,50005,225,500005,50006,226...232,50007,233...235,50008,236,500009,50009,237...241,50010,242...244,50011,245...250,50012,251...254,6000000,600000,60000,255...259,60001,260...262,60002,263...270,600002,60003,271...275,60004,276,277,60005,278,279,60006,280,7000000,700000,70000,281...285,70001,286...291,70002,292,700002,70003,293...297,70004,298,8000000,800000,80000,299...307,80001,308...315,80002,316...322,80003,323...326,800002,80004,327...331,80005,332...335,80006,336...340,9000000,900000,90000,341...343,90001,344,90002,345...347,900002,90003,348,349,90004,350...356,90005,357...360,90006,361,362,90007,363,90008,364...369"
)

#Get last 2 years of employment count by 1) soc2020, 2) sex
I_empOcc2020<-nomis_get_data(
  id = "NM_218_1"
  , date = "latestMINUS4,latest"
  , geography = "2092957699,2013265921...2013265932"
  , C_SEX = "0...2"
  , jtype="0"
  , ftpt="0"
  , etype="0"
  , soc2020_full="0,1000001,100000,10000,1,2,10001,3...5,10002,6...13,10003,14,10004,15,10005,16...18,10006,19,20,100001,10007,21,22,10008,23...27,10009,28...30,10010,31...33,10011,34...42,2000000,200000,10012,43...48,10013,49...56,10014,57...64,10015,65,66,10016,67,68,10017,69,70,200001,10018,71,72,10019,73...79,10020,80...86,10021,87,10022,88...94,200002,10023,95...102,10024,103...107,200003,10025,108...110,10026,111...113,10027,114...119,10028,120,10029,121...125,10030,126...130,10031,131,132,10032,133...135,10033,136...139,3000000,300000,10034,140...146,10035,147,10036,148...150,300001,10037,151...155,10038,156...160,10039,161,162,10040,163,300002,10041,164...168,300003,10042,169...175,10043,176...178,10044,179...181,300004,10045,182,183,10046,184,10047,185...188,10048,189...193,10049,194...200,10050,201,10051,202...205,10052,206,207,4000000,400000,10053,208...210,10054,211...215,10055,216...221,10056,222...224,10057,225...227,400001,10058,228...234,5000000,500000,10059,235...239,500001,10060,240...243,10061,244...248,10062,249...254,10063,255...261,10064,262,500002,10065,263...270,10066,271...273,10067,274,500003,10068,275...278,10069,279...281,10070,282...287,10071,288...291,6000000,600000,10072,292...297,10073,298,299,10074,300...307,600001,10075,308...312,10076,313,314,10077,315,316,10078,317,10079,318,600002,10080,319,320,7000000,700000,10081,321...325,10082,326...331,10083,332,333,700001,10084,334...338,10085,339,8000000,800000,10086,340...345,10087,346,10088,347...352,10089,353...359,10090,360...363,10091,364,800001,10092,365...370,10093,371...373,10094,374...378,9000000,900000,10095,379...381,10096,382,383,10097,384...386,900001,10098,387,388,10099,389...395,10100,396...398,10101,399,400,10102,401...404,10103,405...412"
)
### 2.2.1 Ashe earnings data ----
# https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/occupation4digitsoc2010ashetable14
 folder <- "B_AsheEarnings22"
 sheet <- "All"
 I_ashe17 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[1]), sheet = sheet)
 I_ashe18 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[2]), sheet = sheet)
 I_ashe19 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[3]), sheet = sheet)
 I_ashe20 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[4]), sheet = sheet)
 I_ashe21 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[5]), sheet = sheet)
 I_ashe22 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[6]), sheet = sheet)

 sheet <- "Male"
 I_asheMale17 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[1]), sheet = sheet)
 I_asheMale18 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[2]), sheet = sheet)
 I_asheMale19 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[3]), sheet = sheet)
 I_asheMale20 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[4]), sheet = sheet)
 I_asheMale21 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[5]), sheet = sheet)
 I_asheMale22 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[6]), sheet = sheet)

 sheet <- "Female"
 I_asheFemale17 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[1]), sheet = sheet)
 I_asheFemale18 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[2]), sheet = sheet)
 I_asheFemale19 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[3]), sheet = sheet)
 I_asheFemale20 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[4]), sheet = sheet)
 I_asheFemale21 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[5]), sheet = sheet)
 I_asheFemale22 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[6]), sheet = sheet)
 
 ### 2.2.2 Soc - stem lookup ----
 folder <- "C_stemLookup"
 I_stemLookup <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))
 
  
### 2.1.2 Employment level and rate ------------
# Geog and date as above
# Cell: T01 Economic activity by age Aged 16-64/ All people
# find cells we want
# cellsUseAps_emp <- cellsListAps %>% filter(description.en %like% "T01:" & description.en %like% "Aged 16-64" & description.en %like% "All People")
# # get data
# I_emp <- extractNomis("NM_17_1", "latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest", cellsUseAps_emp$id)

### 2.1.3 UK Business Count----
# Enterprise by employment size and industry
# Query data
# Geography: England, LEPs, regions, LADs (as of April 2022)
# Date: 12 months to Dec 2018-2022
# Cell: UK Business Counts - enterprises by industry and employment size band
# Enterprise by employment size and industry
# I_entIndSize <-
#   bind_rows(
#     # user defined pt 1 (do in two parts as falls over all together)
#     nomis_get_data("NM_142_1",
#       geography =
#         "MAKE|Brighton%20and%20Hove%2C%20East%20Sussex%2C%20West%20Sussex%20LSIP|1811939621;1811939622;1811939560;1811939623;1811939624;1811939576;1811939577;1811939625;1811939578;1811939626;1811939579;1811939580;1811939627,MAKE|Buckinghamshire%20LSIP|1811939575,MAKE|Cambridgeshire%20and%20Peterborough%20LSIP|1811939479;1811939480;1811939481;1811939482;1811939476;1811939483,MAKE|Cambridgeshire%20and%20Peterborough%20MCA|1811939479;1811939480;1811939481;1811939482;1811939476;1811939483,MAKE|Cheshire%20and%20Warrington%20LSIP|1811939345;1811939346;1811939348,MAKE|Cornwall%20and%20the%20Isles%20of%20Scilly%20LSIP|1811939631;1811939632,MAKE|Cumbria%20LSIP|1811939349;1811939350;1811939351;1811939352;1811939353;1811939354,MAKE|Derbyshire%20and%20Nottinghamshire%20LSIP|1811939407;1811939436;1811939437;1811939408;1811939438;1811939409;1811939403;1811939410;1811939411;1811939439;1811939412;1811939440;1811939441;1811939413;1811939405;1811939442;1811939414,MAKE|Dorset%20LSIP|1811939649;1811939655,MAKE|Enterprise%20M3%20LEP%20(including%20all%20of%20Surrey)%20LSIP|1811939581;1811939582;1811939610;1811939611;1811939612;1811939586;1811939613;1811939614;1811939615;1811939589;1811939616;1811939617;1811939618;1811939590;1811939619;1811939591;1811939620,MAKE|Essex%2C%20Southend-on-Sea%20and%20Thurrock%20LSIP|1811939484;1811939485;1811939486;1811939487;1811939488;1811939489;1811939490;1811939491;1811939492;1811939493;1811939477;1811939494;1811939478;1811939495,MAKE|Gloucestershire%20LSIP|1811939656;1811939657;1811939658;1811939659;1811939660;1811939661,MAKE|Greater%20Lincolnshire%20LSIP|1811939422;1811939423;1811939424;1811939425;1811939406;1811939426;1811939427;1811939428;1811939384;1811939385,MAKE|Greater%20London%20LSIP|1811939540;1811939541;1811939542;1811939543;1811939544;1811939526;1811939527;1811939545;1811939546;1811939547;1811939548;1811939528;1811939529;1811939530;1811939549;1811939550;1811939551;1811939552;1811939531;1811939532;1811939553;1811939533;1811939534;1811939554;1811939535;1811939555;1811939556;1811939536;1811939557;1811939537;1811939558;1811939538;1811939539,MAKE|Greater%20Manchester%20LSIP|1811939355;1811939356;1811939357;1811939358;1811939359;1811939360;1811939361;1811939362;1811939363;1811939364,MAKE|Greater%20Manchester%20MCA|1811939355;1811939356;1811939357;1811939358;1811939359;1811939360;1811939361;1811939362;1811939363;1811939364,MAKE|Heart%20of%20the%20South-West%20LSIP|1811939640;1811939641;1811939662;1811939642;1811939643;1811939634;1811939663;1811939667;1811939644;1811939664;1811939645;1811939638;1811939646;1811939647,MAKE|Hertfordshire%20LSIP|1811939496;1811939497;1811939499;1811939500;1811939501;1811939503;1811939505;1811939506;1811939507;1811939509,MAKE|Hull%20and%20East%20Yorkshire%20LSIP|1811939382;1811939383,MAKE|Kent%20and%20Medway%20LSIP|1811939592;1811939593;1811939594;1811939595;1811939599;1811939596;1811939597;1811939562;1811939598;1811939601;1811939602;1811939603;1811939604,MAKE|Lancashire%20LSIP|1811939343;1811939344;1811939365;1811939366;1811939367;1811939368;1811939369;1811939370;1811939371;1811939372;1811939373;1811939374;1811939375;1811939376,MAKE|Leicester%20and%20Leicestershire%20LSIP|1811939415;1811939416;1811939417;1811939418;1811939404;1811939419;1811939420;1811939421,MAKE|Liverpool%20City%20Region%20LSIP|1811939347;1811939377;1811939378;1811939379;1811939380;1811939381,MAKE|Liverpool%20City%20Region%20MCA|1811939347;1811939377;1811939378;1811939379;1811939380;1811939381,MAKE|Norfolk%20and%20Suffolk%20LSIP|1811939517;1811939510;1811939511;1811939524;1811939512;1811939519;1811939513;1811939520;1811939514;1811939515;1811939516;1811939525",
#       industry = 37748736, date = "latestMINUS4-latest", employment_sizeband = "0,10,20,30,40", industry = "163577857...163577874", legal_status = "0", measures = "20100"
#     ) %>%
#       select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, INDUSTRY_NAME, EMPLOYMENT_SIZEBAND_NAME, OBS_VALUE),
#     # user defined pt 2
#     nomis_get_data("NM_142_1",
#       geography =
#         "MAKE|North%20East%20LSIP|1811939330;1811939338;1811939341;1811939342,MAKE|North%20East%20MCA|1811939330;1811939338;1811939341;1811939342,MAKE|North%20of%20Tyne%20LSIP|1811939339;1811939340;1811939334,MAKE|North%20of%20Tyne%20MCA|1811939339;1811939340;1811939334,MAKE|Oxfordshire%20LSIP|1811939605;1811939606;1811939607;1811939608;1811939609,MAKE|Solent%20LSIP|1811939583;1811939584;1811939585;1811939587;1811939561;1811939588;1811939564;1811939567,MAKE|South%20East%20Midlands%20LSIP|1811939473;1811939474;1811939475;1811939768;1811939769;1811939563,MAKE|South%20Yorkshire%20LSIP|1811939394;1811939395;1811939396;1811939397,MAKE|South%20Yorkshire%20MCA|1811939394;1811939395;1811939396;1811939397,MAKE|Stoke-on-Trent%20and%20Staffordshire%20LSIP|1811939447;1811939448;1811939449;1811939450;1811939451;1811939452;1811939453;1811939445;1811939454,MAKE|Swindon%20and%20Wiltshire%20LSIP|1811939637;1811939639,MAKE|Tees%20Valley%20LSIP|1811939329;1811939331;1811939332;1811939335;1811939336,MAKE|Tees%20Valley%20MCA|1811939329;1811939331;1811939332;1811939335;1811939336,MAKE|Thames%20Valley%20Berkshire%20LSIP|1811939559;1811939565;1811939566;1811939568;1811939569;1811939570,MAKE|The%20Marches%20LSIP|1811939443;1811939444;1811939446,MAKE|West%20Midlands%20and%20Warwickshire%20LSIP|1811939460;1811939461;1811939462;1811939455;1811939456;1811939457;1811939463;1811939464;1811939458;1811939465;1811939459;1811939466,MAKE|West%20Midlands%20MCA|1811939460;1811939461;1811939462;1811939463;1811939464;1811939465;1811939466,MAKE|West%20of%20England%20and%20North%20Somerset%20LSIP|1811939628;1811939630;1811939633;1811939636,MAKE|West%20of%20England%20MCA|1811939628;1811939630;1811939636,MAKE|West%20Yorkshire%20LSIP|1811939398;1811939399;1811939400;1811939401;1811939402,MAKE|West%20Yorkshire%20MCA|1811939398;1811939399;1811939400;1811939401;1811939402,MAKE|Worcestershire%20LSIP|1811939467;1811939468;1811939469;1811939470;1811939471;1811939472,MAKE|York%20and%20North%20Yorkshire%20LSIP|1811939387;1811939388;1811939389;1811939390;1811939391;1811939392;1811939393;1811939386",
#       industry = 37748736, date = "latestMINUS4-latest", employment_sizeband = "0,10,20,30,40", industry = "163577857...163577874", legal_status = "0", measures = "20100"
#     ) %>%
#       select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, INDUSTRY_NAME, EMPLOYMENT_SIZEBAND_NAME, OBS_VALUE),
#     # other geogs
#     nomis_get_data("NM_142_1", geography = geogUseAps$id, industry = 37748736, date = "latestMINUS4-latest", employment_sizeband = "0,10,20,30,40", industry = "163577857...163577874", legal_status = "0", measures = "20100") %>%
#       select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, INDUSTRY_NAME, EMPLOYMENT_SIZEBAND_NAME, OBS_VALUE),
#   )
# # Ignore total
# I_entInd <- I_entIndSize %>% filter(INDUSTRY_NAME != "Total")
# # Just by size
# I_entSize <- I_entIndSize %>% filter(INDUSTRY_NAME == "Total")

### 2.1.4 Skill by age gender ------------
# Geog and date as above
# Cell: T19	Qualification by age and gender - NVQ. All people aged 16-64. only updated every Jan-Dec
# find cells we want
# cellsUseAps_qual <- cellsListAps %>% filter(description.en %like% "T19:" & description.en %like% "Total")
# # get data
# I_qualAgeGender <- extractNomis("NM_17_1", "2017-12,2018-12,2019-12,2020-12,2021-12", cellsUseAps_qual$id)

### 2.1.5 Employment by industry------------
# Geog and date as above
# Cell: T13a	Employment by industry (SIC 2007) and flexibility
# find cells we want
# cellsUseAps_Ind <- cellsListAps %>% filter(description.en %like% "T13a:" & description.en %like% "All people")
# # get data
# I_empInd <- extractNomis("NM_17_1", "latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest", cellsUseAps_Ind$id)

## 2.2 EES datasets----
### 2.2.1 Achievements by SSAt1, LAD, gender, level------------
# folder <- "2-7_ILRachSSA"
# I_FeSsa <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

### 2.2.2 Achievements/starts/part by LAD and provision, level and age------------
## Download "Further education and skills geography - detailed summary " from https://explore-education-statistics.service.gov.uk/data-catalogue/further-education-and-skills/2021-22
# folder <- "2-8_ILRach"
# I_FeProvLevelAge <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder)))) %>%
#   filter(time_period != 202223) # ignore since only a partial year

### 2.2.3 KS4 destinations----
# National pupil database
# folder <- "2-9_KS4destin"
# I_KS4 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))
# 
# ### 2.2.4 KS5 destinations----
# folder <- "2-10_KS5destin"
# I_KS5 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

# ## 2.3 ONS datasets ----
# ### 2.3.1 Business demography, UK ----
# # Number of enterprise births, deaths and active
# # Geography: England and LADS (as of April 2021)
# folder <- "2-11_bussdemo"
# firstRow <- 4
# 
# # births
# sheet <- "Table 1.1a"
# I_births_ONS1618 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
# sheet <- "Table 1.1b"
# I_births_ONS19 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
# sheet <- "Table 1.1c"
# I_births_ONS20 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
# sheet <- "Table 1.1d"
# I_births_ONS21 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
# 
# # deaths
# sheet <- "Table 2.1a"
# I_deaths_ONS1618 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
# sheet <- "Table 2.1b"
# I_deaths_ONS19 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
# sheet <- "Table 2.1c"
# I_deaths_ONS20 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
# sheet <- "Table 2.1d"
# I_deaths_ONS21 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
# 
# # active
# sheet <- "Table 3.1a"
# I_active_ONS1618 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
# sheet <- "Table 3.1b"
# I_active_ONS19 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
# sheet <- "Table 3.1c"
# I_active_ONS20 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
# sheet <- "Table 3.1d"
# I_active_ONS21 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)

### 2.3.2 ONS job adverts by profession ----
folder <- "2-12_OnsProf"
sheet <- "Table 10"
I_OnsProfDetailEng <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 11"
I_OnsProfDetailRegion <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 12"
I_OnsProfDetailLA <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 13"
I_OnsProfDetailLep <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 14"
I_OnsProfDetailLsip <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 15"
I_OnsProfDetailMca <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)

sheet <- "Table 4"
I_OnsProfEng <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 5"
I_OnsProfRegion <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 6"
I_OnsProfLA <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 7"
I_OnsProfLep <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 8"
I_OnsProfLsip <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 9"
I_OnsProfMca <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)

## 2.4 Skills imperative----
# These take a long time to run ~20mins
folder <- "2-13_skillsImperative2035"
dir_path <- paste0("./Data/", folder, "/")
skillsImpFileList <- list.files(dir_path)

read_dir <- function(dir_path, file_name, sheet_name, row_nos) {
  read.xlsx(paste0(dir_path, file_name), sheet = sheet_name, skipEmptyRows = T, rows = row_nos) %>%
    mutate(file_name = file_name)
}

# Industry future
# I_wfIndF2 <-
#   skillsImpFileList %>%
#   map_df(~ read_dir(dir_path, .x, "Ind F2", 4:38))
# Occupation future
I_wfOccF2 <-
  skillsImpFileList %>%
  map_df(~ read_dir(dir_path, .x, "Occ F2", 4:49))
# Qualification future
# I_wfQualF1 <-
#   skillsImpFileList %>%
#   map_df(~ read_dir(dir_path, .x, "Qual F1", 4:14))
# Area names
I_wfAreaName <-
  skillsImpFileList %>%
  map_df(~ read_dir(dir_path, .x, "Info", 2:5)) %>%
  filter(grepl("name", Scenario, fixed = TRUE))

# 3 Dashboard text----
## 3.1 Data sources ----
folder <- "3-1_DataTable"
sheetNum <- 1
I_DataTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

# ## 3.2 Data notes and caveats ----
# folder <- "3-2_dataText"
# sheetNum <- 1
# I_DataText <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 3.3 FE Interventions table (not currently used)----
# folder <- "3-3_FeInterventions"
# sheetNum <- 1
# I_InterventionTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

# ## 3.4 Load FE sources and tools tables ----
# folder <- "3-4_FeSources"
# sheetNum <- "Tools"
# I_ToolsTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)
# sheetNum <- "Sources"
# I_SourcesTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)
