PRO TSI_MK_LUT,TSI_PATH=TSI_PATH,Plots=Plots
  
;Generate top of atmosphere incoming total solar irradiance (TSI) for
;1-au and true TSI adjusted for earth-sun distance variations as a function of
;the julian day and year.
;
;Inputs: Uses two datasets (ascii format)
;1) composite_d25_07_0310a.dat (VIRGO AND SOHO data from 1978 to 2003)
;   ftp://ftp.ngdc.noaa.gov/STP/SOLAR_DATA/SOLAR_IRRADIANCE/composite_d25_07_0310a.dat
;
;2) sorce_tsi_L3_c24h_latest.txt  (SORCE data from 2003 to 2017)
;   http://lasp.colorado.edu/home/sorce/data/tsi-data/#data_files
;   TIM Daily
;
;3 sorce_ssi_L3_c24h_0000nm_2413nm_20030301_20171128.txt
;   http://lasp.colorado.edu/data/sorce/ssi_data/composite/sorce_ssi_latest.txt.zip
;   SIM
;
;Comments:
;There is a significant bias between the two datasets. The mean bias
;during the overlap period during 2003 is 4.711 +/- 0.055 W/m2.
;
;Output: TSI is reconstructed from these datasets and missing values
;are filled using bilinear interpolation. The output file is in netcdf format.

;DEFINE INPUT DATA PATH
IF KEYWORD_SET(TSI_PATH) EQ 0 THEN TSI_PATH='/group_workspaces/cems2/nceo_generic/cloud_ecv/data_in/bbflux_luts/tsi/'


;--------------------------------------------------
; FETCH: VIRGO & SOHO DATA (1978 -- 2003)
;--------------------------------------------------
TSI_FILE = TSI_PATH+'composite_d25_07_0310a.dat'
OPENR,1,TSI_FILE
JUNK=STRARR(1)
FOR I=0,42 DO READF,1,JUNK

all_TMP = DBLARR(5,100000)
CT=0L
WHILE EOF(1) EQ 0 DO BEGIN
 
 ;READ DATA
 READF,1,JUNK
 YEAR = STRMID(JUNK,1,2)*1. + 1900.
 IF YEAR LT 1970. THEN YEAR = YEAR+100.
 MONTH = STRMID(JUNK,3,2)
 MDAY = STRMID(JUNK,5,2)
 YDAY = JULDAY(MONTH,MDAY,YEAR)-(JULDAY(1,0,YEAR))
 TSI  = STRMID(JUNK,18,9)*1D
 
 TMP = DBLARR(5)
 ;FILL TEMP ARRAY
 TMP(0) = YEAR
 TMP(1) = MONTH
 TMP(2) = MDAY
 TMP(3) = YDAY
 TMP(4) = TSI
 
 ;FILL All Array
 all_TMP(*,CT) = TMP 

 CT++
ENDWHILE
CLOSE,1
d25_TMP = all_TMP(*,0:CT-1)





;--------------------------------------------------
; FETCH: SORCE DATA (2003 -- 2017)
;--------------------------------------------------
TSI_FILE = TSI_PATH+'sorce_tsi_L3_c24h_latest.txt'
OPENR,1,TSI_FILE
JUNK=STRARR(1)
READF,1,JUNK
WHILE STRPOS(JUNK,'***DATA RECORDS***') EQ -1 DO BEGIN
 READF,1,JUNK
ENDWHILE
;FOR I=0,119 DO READF,1,JUNK
print,junk

all_TMP = DBLARR(6,100000)
CT=0L
WHILE EOF(1) EQ 0 DO BEGIN
 
 ;READ DATA
 READF,1,JUNK
 YEAR  = STRMID(JUNK,0,4)*1.
 MONTH = STRMID(JUNK,4,2)
 MDAY = STRMID(JUNK,6,2)
 YDAY = JULDAY(MONTH,MDAY,YEAR)-(JULDAY(1,0,YEAR))
 TSI  = strmid(junk,47,9)*1.
 TSI_true = strmid(junk,97,9)*1.
 
 TMP = DBLARR(6)
 ;FILL TEMP ARRAY
 TMP(0) = YEAR
 TMP(1) = MONTH
 TMP(2) = MDAY
 TMP(3) = YDAY
 TMP(4) = TSI
 TMP(5) = TSI_TRUE
 
 ;FILL All Array
 all_TMP(*,CT) = TMP 

 CT++
ENDWHILE
CLOSE,1
SORCE_TMP = all_TMP(*,0:CT-1)


 ;PLOT SoHo & SORCE DATASETS
 IF KEYWORD_SET(Plots) EQ 1 THEN BEGIN
  id1=where(d25_tmp(4,*) gt 0.);SOHO GOOD INDEX (red)
  id2=where(sorce_tmp(4,*) gt 0.);SORCE GOOD INDEX (blue)

  MULTI_COLORBAR
  n=greek_letters()
  !p.thick=2 & !p.charsize=1.5 & !p.charthick=4 & !x.thick=4 & !y.thick=4 & !p.thick=2 & !p.font=0
  ps_Tstr  = TSI_PATH +'tsi_1au_datasets.eps'
  ps_open_v2,file=ps_Tstr,/encapsulated,/color,xsize=8,ysize=4,/por

   plot,d25_tmp(0,id1)+d25_tmp(3,id1)/365.,d25_tmp(4,id1),$
    xtitle='time',ytitle='total solar irradiance (W/m!e2!n)',$
    yrange=[1358,1372],xrange=[1978,2018],xstyle=1,/nodata

    oplot,d25_tmp(0,id1)+d25_tmp(3,id1)/365.,d25_tmp(4,id1),color=2,psym=1,symsize=.1
    oplot,sorce_tmp(0,id2)+sorce_tmp(3,id2)/365.,sorce_tmp(4,id2),color=3,psym=1,symsize=.1

    xyouts,.36,.82,' SOHO (mean = ' +num_dimf(mean(d25_tmp(4,id1)))+')',/norm,charsize=1.,color=2
    xyouts,.36,.76,'SORCE (mean = '+num_dimf(mean(SORCE_tmp(4,id2)))+')',/norm,charsize=1.,color=3
   ps_close,/noprint
 ENDIF


;-------------------------------------------------------
;Obtain indices for datasets during the OVERLAP period
;-------------------------------------------------------
jnt=FLTARR(2,5000) ;[0th is the SOHO INDEX, 1ST IS THE SORCE INDEX]
CT=0L
FOR II=0,N_ELEMENTS(d25_tmp(0,*))-1 DO BEGIN
FOR JJ=0,N_ELEMENTS(sorce_tmp(0,*))-1 DO BEGIN
 IF d25_tmp(0,II) EQ SORCE_TMP(0,JJ) THEN BEGIN
 IF d25_tmp(3,II) EQ SORCE_TMP(3,JJ) THEN BEGIN
  IF d25_tmp(4,II) GT 0. THEN BEGIN
  IF SORCE_TMP(4,JJ) GT 0. THEN BEGIN
   jnt(0,CT)=II
   jnt(1,CT)=JJ
   CT++
  ENDIF
  ENDIF
 ENDIF
 ENDIF
ENDFOR
ENDFOR
JNT=jnt(*,0:CT-1) ;joint index

 ;Determine the mean bias between the two datasets
 diff = (d25_tmp(4,jnt(0,*))-SORCE_TMP(4,jnt(1,*)))
 bias = MEAN(diff)
 sig  = STDDEV(diff)
 PRINT,'BIAS (MEAN, STD)',bias,sig
  ;Correct SoHo dataset based on BIAS CORRECTION w.r.t. SORCE
  d25_tmp_corr = d25_tmp
  d25_tmp_corr(4,*) = d25_tmp_corr(4,*)-bias

 ;Plot bias corrected SOHO data (pink)
 IF KEYWORD_SET(Plots) EQ 1 THEN BEGIN
 ps_Tstr  = TSI_PATH +'tsi_1au_bias_correction.eps'
 ps_open_v2,file=ps_Tstr,/encapsulated,/color,xsize=8,ysize=4,/por

  plot,d25_tmp(0,id1)+d25_tmp(3,id1)/365.,d25_tmp(4,id1),$
   xtitle='time',ytitle='total solar irradiance (W/m!e2!n)',$
   yrange=[MEAN(d25_tmp(4,id1))-5.,MEAN(d25_tmp(4,id1))+5.],xrange=[1978,2018],xstyle=1,/nodata,title='BIAS CORRECTED'

  oplot,d25_tmp(0,id1)+d25_tmp(3,id1)/365.,d25_tmp(4,id1),color=2
  oplot,sorce_tmp(0,id2)+sorce_tmp(3,id2)/365.,sorce_tmp(4,id2),color=3

  oplot,d25_tmp_corr(0,id1)+d25_tmp_corr(3,id1)/365.,d25_tmp_corr(4,id1)
  
  xyouts,.25,.82,'SOHO BIAS (MEAN, SIGMA) = '+NUM_DIMF(BIAS)+','+NUM_DIMF(SIG)+' W/m!e2!n',/NORM
 ps_close,/noprint
 ENDIF



;-------------------------------------------------------
; CREATE TSI LUT
;-------------------------------------------------------
;start date - 1978/11/17
;end date   - 2017/11/27
stDay = JULDAY(11,17,1978)
;edDay = JULDAY(11,27,2017)
edDay = JULDAY(MONTH*1.,MDAY*1.,YEAR*1.) - 1D

nT = edDay-stDay+1   ;number of julian days in time series
nT = long(NT[0])
YEAR  = DBLARR(nT)   ;year
JDAY  = DBLARR(nT)   ;julian day
tsi_1au = DBLARR(nT) ;
tsi_1au(*)=-999.D
FOR I=0,nT-1 DO BEGIN
 CALDAT,stDay+I,mm,dd,yy
 YEAR(I) = yy
 JDAY(I) = ( JULDAY(mm,dd,yy) - JULDAY(1,0,yy) )
 
 ID1=WHERE(d25_tmp_corr(0,*) EQ YEAR(I) AND d25_tmp_corr(3,*) EQ JDAY(I) AND d25_tmp_corr(4,*) GT 0,IDCT1)
 ID2=WHERE(SORCE_TMP(0,*) EQ YEAR(I) AND SORCE_TMP(3,*) EQ JDAY(I) AND SORCE_TMP(4,*) GT 0,IDCT2)

 ;COMPOSITE NO SORCE
 IF IDCT1 EQ 1 AND IDCT2 EQ 0 THEN BEGIN
  IF d25_tmp_corr(4,ID1(0)) GT 0 THEN tsi_1au(I) = d25_tmp_corr(4,ID1(0))
 ENDIF 

 ;SORCE NO SOHO
 IF IDCT1 EQ 0 AND IDCT2 EQ 1 THEN BEGIN
  IF SORCE_TMP(4,ID2(0)) GT 0 THEN tsi_1au(I) = SORCE_TMP(4,ID2(0))
 ENDIF 

 ;BOTH EXIST - AVERAGE BOTH
 IF IDCT1 EQ 1 AND IDCT2 EQ 1 THEN BEGIN
  tsi_1au(I) = MEAN([d25_tmp_corr(4,ID1(0)),SORCE_TMP(4,ID2(0))])
 ENDIF 

ENDFOR


;-------------------------------------------------------
;FILL IN MISSING VALUES/DAYS BY INTERPOLATION
;-------------------------------------------------------
FILL_MISSING, TSI_1AU, -999.D
Y=TSI_1AU

IF KEYWORD_SET(Plots) EQ 1 THEN BEGIN
 id1=where(d25_tmp(4,*) gt 0.);SOHO GOOD INDEX (red)
 id2=where(sorce_tmp(4,*) gt 0.);SORCE GOOD INDEX (blue)
 ps_Tstr  = TSI_PATH+'tsi_1au_correction.eps'
 ps_open_v2,file=ps_Tstr,/encapsulated,/color,xsize=8,ysize=4,/por

 plot,d25_tmp(0,id1)+d25_tmp(3,id1)/365.,d25_tmp(4,id1),$
  xtitle='time',ytitle='total solar irradiance (W/m!e2!n)',$
  yrange=[1358,1365],xrange=[1978,2018],xstyle=1,/nodata,title='BIAS-CORRECTED MISSING-FILLED'

   oplot,year+jday/365.,Y

 d25_tmp_corr = d25_tmp
 d25_tmp_corr(4,*) = d25_tmp_corr(4,*)-bias
 oplot,d25_tmp_corr(0,id1)+d25_tmp_corr(3,id1)/365.,d25_tmp_corr(4,id1)
 ps_close,/noprint
ENDIF



;1-AU TSI
TSI_1AU  = Y*1D

;GET TRUE TSI ADUSTED USING EARTH-SUN DISTANCE
;Forula derived from Fu Liou textbook page 49
t = (2D * !pi * JDAY) / 365D
;sfac = 1.000110D + 0.034221D*cos(t) + 0.001280D*sin(t) + 0.000719D*cos(2*t) + 0.000077*sin(2*t)

;Amplitude is too large; get better fit using this formula
sfac = 1.000110D + 0.03341D*cos(t) + 0.001280D*sin(t) + 0.000719D*cos(2*t) + 0.000077*sin(2*t)
TSI_TRUE_EARTH = Y* sFac

;REPLACE TSI_TRUE_EARTH WITH MEASUREMENTS FROM SORCE WHERE THEY EXIST
;(off at most by 1 W/m2 due to equation) Better to use theoretical
;value as trends will be strongly affected otherwise.
;;FOR K=0,N_ELEMENTS(YEAR)-1 DO BEGIN
;; JUNK=WHERE( SORCE_TMP[0,*] EQ YEAR[K] AND SORCE_TMP[3,*] EQ JDAY[K] AND SORCE_TMP[5,*] GT 0.,JUNKCT)
;; IF JUNKCT EQ 1 THEN TSI_TRUE_EARTH[K] = SORCE_TMP[5,JUNK(0)]
;;ENDFOR



;PLOT RECONSTRUCTED TSI CORRECTED FOR EARTH-SUN DISTANCE
IF KEYWORD_SET(Plots) EQ 1 THEN BEGIN
ps_Tstr  = TSI_PATH +'tsi_true_earth.eps'
ps_open_v2,file=ps_Tstr,/encapsulated,/color,xsize=8,ysize=4,/por

 plot,YEAR+JDAY/365.,TSI_TRUE_EARTH,$
  xtitle='time',ytitle='total solar irradiance (W/m!e2!n)',$
  yrange=[1315,1412],xrange=[1978,2018],xstyle=1,/nodata,title='True Earth Sun-Distance Correction',ystyle=1


   oplot,YEAR+JDAY/365.,TSI_TRUE_EARTH
   id=where(sorce_tmp(5,*) gt 0.)
   oplot,SORCE_TMP(0,id)+SORCE_TMP(3,id)/365.,SORCE_TMP(5,id),COLOR=3,psym=1,symsize=.1

ps_close,/noprint
ENDIF


;Put data into structure
TSI = [ $
{data:TSI_1AU,vname:'tsi_1au',long:'total solar irradiance at 1 au',units:'w/m2'},$
{data:TSI_TRUE_EARTH,vname:'tsi_true_earth',long:'total solar irradiance at earth distance',units:'w/m2'},$
{data:YEAR,vname:'year',long:'year start date - 1978/11/17 to 2015/8/19',units:'1'},$
{data:JDAY,vname:'jday',long:'julian day of calendar year',units:'1'}]

ncname = TSI_PATH + 'TSI.nc'
mk_ncdf,struct=TSI,ncname=ncname

END
