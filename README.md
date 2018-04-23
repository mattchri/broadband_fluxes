# broadband_fluxes
Look up table and code

Total Solar Irradiance data was obatined from the Goddard Earth Sciences Data and Information website
http://disc.sci.gsfc.nasa.gov/SORCE/

;Inputs: Uses two datasets (ascii format)
1) composite_d25_07_0310a.dat (VIRGO AND SOHO data from 1978 to 2003)
   ftp://ftp.ngdc.noaa.gov/STP/SOLAR_DATA/SOLAR_IRRADIANCE/composite_d25_07_0310a.dat

2) sorce_tsi_L3_c24h_latest.txt  (SORCE data from 2003 to 2017)
   http://lasp.colorado.edu/home/sorce/data/tsi-data/#data_files
   TIM Daily

3 sorce_ssi_L3_c24h_0000nm_2413nm_20030301_20171128.txt
   http://lasp.colorado.edu/data/sorce/ssi_data/composite/sorce_ssi_latest.txt.zip
   SIM

The data downloaded comes from:
Yeo et al. (2014), "Reconstruction of total and spectral solar irradiance from 1974 to 2013 based on KPVT, SoHO/MDI and SDO/HMI observations

To make reconstructed bias corrected earth-sun distance correced filled missing product see: TSI_MK_LUT.PRO
this program reads the two ascii files SOHO AND SORCE and produces tsi.nc


