; TOP-LEVEL LUT TO GENERATE SOLAR SPECTRAL DATA AS
; INPUTS TO CC4CL BROADBAND FLUX CODE.
;
; Author
; Matt Christensen RAL-STFC 2017
PRO PROCESS_SOLAR_SPECTRAL_LUT
;wget ftp://ftp.ngdc.noaa.gov/STP/SOLAR_DATA/SOLAR_IRRADIANCE/composite_d25_07_0310a.dat
;wget http://lasp.colorado.edu/data/sorce/tsi_data/daily/sorce_tsi_L3_c24h_latest.txt
;wget wget http://lasp.colorado.edu/data/sorce/ssi_data/composite/sorce_ssi_latest.txt.zip
;unzip sorce_ssi_latest.txt.zip

outPath='./'

; Process Total Solar Irradiance Data
; SoHo & Sorce
; Create continuous TSI product from observations
TSI_MK_LUT,TSI_PATH=outPath

; Read TSI DATA
STAT=READ_NCDF(outPath+'TSI.nc',TSI)

;Read RAW Spectral data
SPEC=read_lasp_ascii_file(outPath+'sorce_ssi_L3_c24h_0000nm_2413nm_20030301_20190913.txt')

; Assign Quantities
SPEC_DATA = SPEC.(6)
X_LAMBDA  = ( (SPEC.(2)) + (SPEC.(3)) )/2.
s=string( format='(I8)',(SPEC.(0))[*] )


; Fetch lambda
JUNK=WHERE(s eq '20080320')
xLambda=X_LAMBDA[JUNK]

; Fetch timestamp
YYYY=STRMID(s,0,4)
MM=STRMID(s,4,2)
DD=STRMID(s,6,2)
N = N_ELEMENTS(s)

nDays = JULDAY(DD[N-1],MM[N-1],YYYY[N-1]) - JULDAY(DD[0],MM[0],YYYY[0])
dID   = LINDGEN(nDays) + JULDAY(DD[0],MM[0],YYYY[0])
SID   = MAKE_ARRAY( nDays, 2, /LONG, VALUE=-1)

SDATA = MAKE_ARRAY( nDays, 985, /FLOAT, VALUE=-99999.)
XDATA = MAKE_ARRAY( nDays, 985, /FLOAT, VALUE=-99999.)
NDATA = MAKE_ARRAY( nDays, /INT)

jd=SPEC.(1)
hist_res=histogram(jd,binsize=1,reverse_indices=ri)
histN = n_elements(hist_res)
FOR j=0,histN-1 DO BEGIN
PRINT,J
 if ri[j+1] gt ri[j] then begin ;check for indices
  indices = [ri[ri[j]:ri[j+1]-1]]
  num     = n_elements(indices)

  IF num GT 0 THEN BEGIN
   JUNK=WHERE(SPEC_DATA[indices] GT 0.,JUNKCT)

   IF JUNKCT GT 0 THEN BEGIN 
   TMPi = WHERE(dID EQ jd[indices(0)],tmpCT)
   IF tmpCT EQ 1 THEN BEGIN

    SID[TMPi,0] = indices[0]
    SID[TMPi,1] = indices[num-1]

   FOR K=0,num-1 DO SDATA[TMPi, WHERE( xLambda EQ X_LAMBDA[indices(K)])] = SPEC_DATA[indices(K)]

    ;SDATA[TMPi,0:num-1] = SPEC_DATA[indices]
    XDATA[TMPi,0:num-1] = X_LAMBDA[indices]
    NDATA[TMPi] = num

   ENDIF
   ENDIF
  ENDIF

 endif
ENDFOR


;-------------------------------------------
; Climatology
;-------------------------------------------

;Full record - from all days with full continous data
SDATA_CLIM = FLTARR(985)
FOR J=0,984 DO BEGIN
 JUNK=WHERE( NDATA EQ 985 AND SDATA[*,J] GT 0.,JUNKCT)
 IF JUNKCT GT 0 THEN BEGIN
  SDATA_CLIM[J] = MEAN( SDATA[JUNK, J] )
 ENDIF
ENDFOR

;Each year
CALDAT,DID,MM,DD,YYYY
DOY=JULDAY(MM,DD,YYYY)-JULDAY(1,1,YYYY)+1
;;YEARS = FINDGEN(15)+2003
;;SDATA_YEARLY_CLIM = FLTARR(15,985)
;;FOR I=0,14 DO BEGIN
;;  JUNK = WHERE( YYYY EQ YEARS[I],JUNKCT)
;;  IF JUNKCT GT 1 THEN BEGIN
;;   TMPDAT=SDATA[ JUNK, *]
;;   FOR J=0,984 DO BEGIN
;;     JUNK=WHERE(TMPDAT(*,J) GT 0,JUNKCT)
;;     IF JUNKCT GT 0 THEN SDATA_YEARLY_CLIM[I,J]=MEAN( TMPDAT[JUNK,J] )
;;   ENDFOR
;; ENDIF
;;ENDFOR

;Each day - replace with climatology if day does not exist
CALDAT,JULDAY(1,1,TSI.YEAR)+TSI.JDAY+1,TSI_MM,TSI_DD,TSI_YYYY
NDAYS = N_ELEMENTS(TSI.JDAY)
SPECTRAL_DATA = MAKE_ARRAY( NDAYS, 985 )
FOR I=0,N_ELEMENTS(TSI.JDAY)-1 DO BEGIN

 ;ALL DATA INITIALLY IS ASSIGNED TO CLIMATOLOGY
 SPECTRAL_DATA[I,*] = SDATA_CLIM

 ;SPECTRAL DATA IS NOT AVAILABLE UNTIL SORCE OBSERVATIONS IN 2003
 IF TSI.YEAR[I] GE 2003 THEN BEGIN
  JUNK=WHERE( YYYY EQ TSI.YEAR[I] AND DOY EQ TSI.JDAY[I] AND NDATA EQ 985,COUNT)
  IF COUNT EQ 1 THEN BEGIN
   JUNK2=WHERE( SDATA[JUNK(0),*] GT 0.,COUNT2)
    IF COUNT2 EQ 985 THEN SPECTRAL_DATA[I,*] = SDATA[JUNK(0),*]
  ENDIF
 ENDIF
ENDFOR

; Compute solar flux based on this wavelength range
S0 = FLTARR(NDAYS)
FOR I=0,NDAYS-1 DO S0[I] = INT_TABULATED( xLambda, REFORM(SPECTRAL_DATA[I,*]))

; Couple of the channels are flawed screen based on values having
; solar constant less than 1320 W/m2
JUNK=WHERE( S0 LT 1320.,COUNT)
FOR Ict=0,COUNT-1 DO SPECTRAL_DATA(JUNK[Ict],*) = SDATA_CLIM



; PROCESS ALL DAYS IN TSI FILE
BUGSRAD_SPECTRAL_WEIGHTS = DBLARR( NDAYS, 6 )
FULIO_SPECTRAL_WEIGHTS = DBLARR( NDAYS, 18 )
PAR_WEIGHT = DBLARR( NDAYS )
UVB_WEIGHT = DBLARR( NDAYS )
UVA_WEIGHT = DBLARR( NDAYS )
FOR K=0,NDAYS-1 DO BEGIN

; FIND CORRESPONDING SPECTRAL DATA
SPEC_DATA = REFORM( SPECTRAL_DATA[K,*] )

; FIND CORRESPONDING TSI FOR THE GIVEN DAY AND YEAR
TSI_TRUE_EARTH = (REFORM( TSI.TSI_TRUE_EARTH( K ) ))[0]

outData = SOLAR_SPECTRAL_LUT( xLAMBDA, SPEC_DATA, TSI_TRUE_EARTH);, /PLOT_IT)
;outData = SOLAR_SPECTRAL_LUT( xLAMBDA, SPEC_DATA, TSI_TRUE_EARTH, /PLOT_IT)

 ;FILL ARRAYS
 BUGSRAD_SPECTRAL_WEIGHTS[K,*] = outData.BUGSRAD_SPECTRAL_WEIGHTS
 FULIO_SPECTRAL_WEIGHTS[K,*] = outData.FULIO_SPECTRAL_WEIGHTS

 PAR_WEIGHT[K] = (outData.PAR_WEIGHT + (outData.PAR_WEIGHT-1.0)*0.5) ;correct for path length through atmosphere
 UVB_WEIGHT[K] = outData.UVB_WEIGHT
 UVA_WEIGHT[K] = outData.UVA_WEIGHT

PRINT,TSI.YEAR[K],TSI.JDAY[K],PAR_WEIGHT[K]
ENDFOR


;Put data into structure
TSI2 = [ $
{data:DOUBLE(TSI.TSI_1AU),vname:'tsi_1au',long:'total solar irradiance at 1 au',units:'w/m2'},$
{data:DOUBLE(TSI.TSI_TRUE_EARTH),vname:'tsi_true_earth',long:'total solar irradiance at earth distance',units:'w/m2'},$
{data:DOUBLE(TSI.YEAR),vname:'year',long:'year start date - 1978/11/17 to 2015/8/19',units:'1'},$
{data:DOUBLE(TSI.JDAY),vname:'jday',long:'julian day of calendar year',units:'1'},$
{data:PAR_WEIGHT,vname:'par_weight',long:'solar weight derived from SORCE radiances spanning 400-700 nm',units:'1'},$]
{data:UVB_WEIGHT,vname:'uvb_weight',long:'solar weight derived from SORCE radiances spanning 280-315 nm',units:'1'},$]
{data:UVA_WEIGHT,vname:'uva_weight',long:'solar weight derived from SORCE radiances spanning 315-400 nm',units:'1'}]

;Make NETCDF FILE
ncname = outPath + 'TSI_spec.nc'
mk_ncdf,struct=TSI2,ncname=ncname


END
