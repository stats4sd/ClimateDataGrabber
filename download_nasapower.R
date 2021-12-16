# Constants for ET0 (Cobaner et al., 2017)
# Solar constant
Gsc = 0.0820 # (MJ m-2 min-1)
# Radiation adjustment coefficient (Samani, 2004)
kRs = 0.17

download_nasapower <- function(Start,End,longitude,latitude,ID=NA,map=TRUE){
  
  get_power(community = "ag",dates = c(Start,End),
            lonlat = c(longitude,latitude),
            temporal_api = "daily",pars = c("T2M_MIN","T2M_MAX","RH2M","PRECTOTCORR")) %>% 
    
    
    # Converting units or adding variables
    mutate(ID=ID,Tmean = (T2M_MAX+T2M_MIN)/2, # Mean temp. (degC)
           # Nasapower does not provide VPD values
           # However, it is possible to estimate it with Temp and RH.
           es = 0.6108 * exp((17.27*Tmean) / (Tmean+237.3)),
           ea = es * (RH2M / 100),
           # vapour Pressure deficit (kPa)
           VPD = es - ea,
           # Data for ET0
           lat_rad = LAT*0.0174533,
           dr = 1 + 0.033*cos((2*pi/365)*DOY),
           Sd = 0.409*sin((2*pi/365)*DOY - 1.39),
           ws = acos(-tan(lat_rad)*tan(Sd)),
           Ra = (24*60)/(pi) * Gsc * dr * (ws*sin(lat_rad)*sin(Sd)+
                                             cos(lat_rad)*sin(ws)),
           ET0_HS = 0.0135 * kRs * (Ra / 2.45) * (sqrt(T2M_MAX-T2M_MIN)) * (Tmean + 17.8)
    ) %>% dplyr::select(-es,-ea,-lat_rad,-dr,-Sd,-ws,-Ra)%>%
    dplyr::select(ID,longitude=LON,latitude=LAT,YEAR:DOY,Date=YYYYMMDD,tmin=T2M_MIN,
                tmax=T2M_MAX,tmean=Tmean,prec=PRECTOTCORR,rh=RH2M,vpd=VPD,et0_hs=ET0_HS)->outdata
  if(map==TRUE){
  outdata %>%
    slice(1) %>%
  leaflet() %>%
    setView(zoom=6,lng =longitude ,lat = latitude) %>%
    addCircleMarkers(~longitude, ~latitude,popup = ~ID) %>%
    addProviderTiles("OpenStreetMap.Mapnik") %>%
    print()
  }
  return(outdata)
}





