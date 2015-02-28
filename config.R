#General libraries
library(reconstructr) #Session reconstruction
library(WMUtils) #Global querying
library(data.table) #Easy groupwise manipulation
library(lubridate) #Date/time handling
library(rgeoip) #Geolocation
library(magrittr) #Piping
library(mwutils) #Timestamp handling and revert detection.
library(openssl) #Username hashing
library(boot) #Permutation testing
library(perm) #Permutation testing

#Plotting libraries
library(ggplot2) #Find me a researcher who has not used this and you will have found me a researcher who has not lived.
library(scales) #For percentage scales.
library(maptools) #Mapping dependency
library(rgeos) #Mapping dependency
library(rworldmap) #SpatialPolygonDataFrame creation.
library(RColorBrewer) #Colour scale definitions.

#Config variables for geolocation
geo_city_path <- "/usr/local/share/GeoIP/GeoIP2-City.mmdb"
geo_con_path <- "/usr/local/share/GeoIP/GeoIP2-Connection-Type.mmdb"

#Config variables for automata detection
bot_usernames <- c("OctraBot","ZiadBot","Happy05dzBot","1999franbot","AlphamaBot","AlphamaBot2","ShitiBot",
                   "EmausBot","Hoangdat bot","AlphamaBot4","XLinkBot","BryanBot","HangsnaBot2","AVdiscuBOT","JBot",
                   "StubCreationBot","Yjs5497 bot","IanraBot","MerlBot","RotlinkBot","Dinobot-br","Jembot","DvtBot",
                   "Fikarumsrobot","H2Bot","BanwolBot","ThitxongkhoiAWB","Rotlink","CommonsDelinker", "Ops Monitor (WMF)")
bot_agents <- "((Py(thon)?)?wiki(pedia)?bot|MediaWiki|Wiki\\.java|libcurl|(Synch|Abbott|Wartungslisten|Octra)bot|libwww-perl)"

#Options
options(scipen = 500)
options(q = "no")
