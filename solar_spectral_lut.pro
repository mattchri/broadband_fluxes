; Compute coefficients for BUGSrad & Fu-Liou spectral channels
; based on observations from SoHo & SORCE
;
; BUGSrad has 6 spectral channels + PAR
;  ch1: 200  - 689 nm
;  ch2: 689  - 1299
;  ch3: 1299 - 1905
;  ch4: 1905 - 2500
;  ch5: 2500 - 3509
;  ch6: 3509 - 4000
;  par: 400  - 700
;
; Fu-Liou has 18 spectral channels
;  ch1:  175.4  - 224.7
;  ch2:  224.7  - 243.9
;  ch3:  243.9  - 285.7
;  ch4:  285.7  - 298.5
;  ch5:  298.5  - 322.5
;  ch6:  322.5  - 357.5
;  ch7:  357.5  - 437.5
;  ch8:  437.5  - 497.5
;  ch9:  497.5  - 595
;  ch10: 595    - 690
;  ch11: 690    - 794
;  ch12: 794    - 889
;  ch13: 889    - 1042
;  ch14: 1042   - 1410
;  ch15: 1410   - 1904.8
;  ch16: 1904.8 - 2500
;  ch17: 2500   - 3508.8
;  ch18: 3508.8 - 4000

FUNCTION SOLAR_SPECTRAL_LUT, SPEC_LAMBDA, SPEC_DATA, TSI_TRUE_EARTH, PLOT_IT=PLOT_IT

;BUGSrad BANDS
bN = 6
lambda_bugsrad = [ $
[200,689]  , $
[689,1299] , $
[1299,1905], $
[1905,2500], $
[2500,3509], $
[3509,4000] ]

;Fu-Liou Bands
fN = 18
lambda_fulio   = [ $
[175.4  , 224.7],$
[224.7  , 243.9 ],$
[243.9  , 285.7 ],$
[285.7  , 298.5 ],$
[298.5  , 322.5 ],$
[322.5  , 357.5 ],$
[357.5  , 437.5 ],$
[437.5  , 497.5 ],$
[497.5  , 595   ],$
[595    , 690   ],$
[ 690    , 794  ],$
[ 794    , 889  ],$
[ 889    , 1042 ],$
[1042   , 1410  ],$
[1410   , 1904.8],$
[1904.8 , 2500  ],$
[2500   , 3508.8],$
[3508.8 , 4000] ]

;Extra bands
special_STR = ['UVB','UVA','PAR']
sN = N_ELEMENTS(special_STR)
lambda_special = [ $
[280,315],$
[315,400],$
[400,700] ]


; Compute solid angle
 ;based on sun-earth distance - approximate solution
 ;doy = JULDAY(month,calday,year)-JULDAY(1,1,year) +1
 ;t = (2D * !pi * doy) / 365D
 ;sfac = 1.000110D + 0.034221D*cos(t) + 0.001280D*sin(t) + 0.000719D*cos(2*t) + 0.000077*sin(2*t)
 ;AU_DIST = 149597870700. ;m
 ;Rsun = 695700000. ;m
 ;Dsun = sfac * AU_DIST
 ;Gsolid_angle = !pi*(atan(Rsun/Dsun))^2.


 ;based on planck body radiation - exact solution
 Tx = FINDGEN(100000)+0.5
 TB = PLANCK_FUNCTION( Tx/1000., 5777. ) ;output: W/(m^2 sr m), x (um), t (K); T for BUGSrad
 solid_angle = TSI_TRUE_EARTH / INT_TABULATED( Tx, TB)
 TB = TB * solid_angle
 ;FOR KK=0,bN-1 DO PRINT,INT_TABULATED( Tx(WHERE(tX GE LAMBDA_BUGSRAD[0,KK] AND tX LE LAMBDA_BUGSRAD[1,KK])), TB(WHERE(tX GE LAMBDA_BUGSRAD[0,KK] AND tX LE LAMBDA_BUGSRAD[1,KK])) )



 ;Calculate weights for each band
 X = SPEC_LAMBDA
 B = SPEC_DATA
 BF = solar_spectral_lut_fill_data_gap(X,B,Tx,yREPLACE=TB)
  JUNK=WHERE(BF LT 0.,JUNKCT)
  IF JUNKCT GT 0 THEN BF[JUNK] = 0D0

;Correct so that area under curve of fitted data matches the TSI
 BF = BF * ( TSI_TRUE_EARTH / INT_TABULATED( Tx, BF ) )

; spectral weights
spectral_weights = BF / TB
 JUNK=WHERE(FINITE(spectral_weights) EQ 0,JUNKCT)
 IF JUNKCT GT 0 THEN SPECTRAL_WEIGHTS(JUNK) = 0.


; BUGSrad Weights
bugsrad_Spectral_Weights = DBLARR(bN)
FOR I=0,bN-1 DO BEGIN
 JUNK=WHERE(tX GE lambda_bugsrad[0,I] AND tX LT lambda_bugsrad[1,I],JUNKCT)
 bugsrad_Spectral_Weights[I] = INT_TABULATED( Tx[JUNK], BF[JUNK] ) / INT_TABULATED( Tx[JUNK], TB[JUNK])
ENDFOR

; Fu Liou Weights
fulio_Spectral_Weights = DBLARR(fN)
FOR I=0,fN-1 DO BEGIN
 JUNK=WHERE(tX GE lambda_fulio[0,I] AND tX LT lambda_fulio[1,I],JUNKCT)
 fulio_Spectral_Weights[I] = INT_TABULATED( Tx[JUNK], BF[JUNK] ) / INT_TABULATED( Tx[JUNK], TB[JUNK])
ENDFOR

; Extra weights
special_Spectral_Weights = DBLARR(sN)
FOR I=0,sN-1 DO BEGIN
 JUNK=WHERE(tX GE lambda_special[0,I] AND tX LT lambda_special[1,I],JUNKCT)
 special_Spectral_Weights[I] = INT_TABULATED( Tx[JUNK], BF[JUNK] ) / INT_TABULATED( Tx[JUNK], TB[JUNK])
ENDFOR








; Plot data
IF KEYWORD_SET(PLOT_IT) EQ 1 THEN BEGIN
MULTI_COLORBAR
WINDOW,0,XSIZE=600,YSIZE=600
plot,X,B,xtitle='wavelength (nm)',ytitle='spectral irradiance (W/m^2/nm)',/nodata,xrange=[0,4200],xstyle=1

yrng=!y.(7)

;FOR I=0,fN-1 DO BEGIN
; JUNK=WHERE(tX GE lambda_fulio[0,I] AND tX LT lambda_fulio[1,I],JUNKCT)
; col=11
; POLYFILL,[lambda_fulio[0,I],lambda_fulio[1,I],lambda_fulio[1,I],lambda_fulio[0,I]],[yrng[0],yrng[0],yrng[1],yrng[1]],color=col
;ENDFOR
;FOR I=0,fN-1 DO PLOTS,[lambda_fulio[0,I],lambda_fulio[0,I]],[yrng[0],yrng[1]],color=0
;FOR I=0,fN-1 DO PLOTS,[lambda_fulio[1,I],lambda_fulio[1,I]],[yrng[0],yrng[1]],color=0


;FOR I=0,sN-1 DO BEGIN
I=2
 JUNK=WHERE(tX GE lambda_special[0,I] AND tX LT lambda_special[1,I],JUNKCT)
 POLYFILL,[lambda_special[0,I],lambda_special[1,I],lambda_special[1,I],lambda_special[0,I]],[yrng[0],yrng[0],yrng[1],yrng[1]],color=i+2
;ENDFOR
PLOTS,[lambda_special[0,I],lambda_special[0,I]],[yrng[0],yrng[1]],color=0
PLOTS,[lambda_special[1,I],lambda_special[1,I]],[yrng[0],yrng[1]],color=0
;FOR I=0,sN-1 DO PLOTS,[lambda_special[0,I],lambda_special[0,I]],[yrng[0],yrng[1]],color=0
;FOR I=0,sN-1 DO PLOTS,[lambda_special[1,I],lambda_special[1,I]],[yrng[0],yrng[1]],color=0

oplot,tx,tb,color=3,linestyle=2,thick=2
oplot,tx,bf,color=0,thick=2


WINDOW,1,XSIZE=600,YSIZE=600
multi_colorbar
plot,X,B,xtitle='wavelength (nm)',ytitle='spectral irradiance (W/m^2/nm)',/nodata,xrange=[0,4200],xstyle=1
yrng=!y.(7)

FOR I=0,bN-1 DO BEGIN
 JUNK=WHERE(tX GE lambda_bugsrad[0,I] AND tX LT lambda_bugsrad[1,I],JUNKCT)
 col=11
 if I EQ 6 then col=5
 POLYFILL,[lambda_bugsrad[0,I],lambda_bugsrad[1,I],lambda_bugsrad[1,I],lambda_bugsrad[0,I]],[yrng[0],yrng[0],yrng[1],yrng[1]],color=col
ENDFOR
FOR I=0,bN-1 DO PLOTS,[lambda_bugsrad[0,I],lambda_bugsrad[0,I]],[yrng[0],yrng[1]],color=0
FOR I=0,bN-1 DO PLOTS,[lambda_bugsrad[1,I],lambda_bugsrad[1,I]],[yrng[0],yrng[1]],color=0
 oplot,tx,tb,color=3,linestyle=2
 oplot,tx,bf,color=7


TSI_PATH='/group_workspaces/cems/cloud_ecv/mchristensen/orac/data/tsi/'
;PLOT RECONSTRUCTED TSI CORRECTED FOR EARTH-SUN DISTANCE
  MULTI_COLORBAR
  n=greek_letters()
  !p.thick=2 & !p.charsize=1.5 & !p.charthick=4 & !x.thick=4 & !y.thick=4 & !p.thick=2 & !p.font=0
ps_Tstr  = TSI_PATH +'bugsrad.eps'
ps_open_v2,file=ps_Tstr,/encapsulated,/color,xsize=6,ysize=4,/por

plot,X,B,xtitle='wavelength (nm)',ytitle='spectral irradiance (W/m!e2!n/nm)',/nodata,xrange=[100,4200],xstyle=1,/xlog,yrange=[0,2.75],ystyle=1
yrng=!Y.(7)
FOR I=0,bN-1 DO  POLYFILL,[lambda_bugsrad[0,I],lambda_bugsrad[1,I],lambda_bugsrad[1,I],lambda_bugsrad[0,I]],[yrng[0],yrng[0],yrng[1],yrng[1]],color=11
FOR I=0,bN-1 DO PLOTS,[lambda_bugsrad[1,I],lambda_bugsrad[1,I]],[yrng[0],yrng[1]],color=0
 oplot,tx,tb,color=3,linestyle=2
 oplot,tx,bf,color=7
plots,[.25,.3],[.78,.78],color=3,linestyle=2,/norm,thick=6
plots,[.25,.3],[.86,.86],color=7,/norm,thick=6
xyouts,.31,.85,'extraterrestrial solar spectral irrdiance!c(SoHo+SORCE Observations)',/norm,charsize=1
xyouts,.31,.77,'blackbody spectrum !c(T!dsun!n=5777 K; So=1370.22 W/m!e2!n)',/norm,charsize=1
plot,X,B,xtitle='wavelength (nm)',ytitle='spectral irradiance (W/m!e2!n/nm)',/nodata,xrange=[100,4200],xstyle=1,/xlog,yrange=[0,2.75],/noerase,ystyle=1
ps_close,/noprint
CONVERT_IMG_TO_PNG,ps_tstr
ENDIF

;Output structure array
hVar={bugsrad_spectral_weights:bugsrad_spectral_weights, bugsrad_spectral_range:lambda_bugsrad, $
      fulio_spectral_weights:fulio_spectral_weights, fulio_spectral_range:lambda_fulio, $
      par_weight:special_spectral_weights[2], par_spectral_range:[400,700], $
      uvb_weight:special_spectral_weights[0], uvb_spectral_range:[280,315], $
      uva_weight:special_spectral_weights[1], uva_spectral_range:[315,400]}

RETURN,hVar
END
