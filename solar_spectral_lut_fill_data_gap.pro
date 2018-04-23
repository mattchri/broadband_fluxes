function solar_spectral_lut_fill_data_gap,xIn,yIn,xOut,yREPLACE=yREPLACE

;Produce continuous dataset (fill using planck)
;xIn - x-axis position of yIn data point
;yIn - y-axis position of yIn data point
;xOut - new x-axis spacing array
;yOut - interpolated values from yIn to xOut

;OPTIONAL
;yREPLACE: REPLACEMENT VALUE FOR MISSING INTERPOLATION
;must have same spacing as xOUT

nX = N_ELEMENTS(xOut)
yOUT = MAKE_ARRAY(nX,/DOUBLE, VALUE=-99999.)
FOR I=0,nX-1 DO BEGIN
 JUNK=WHERE( xIN GT xOUT[I]-50 AND xIN LT xOUT[I]+50, JUNKCT)
 ;Interpolate data
 IF JUNKCT GT 1 THEN BEGIN
  tmpY = yIN[JUNK]
  tmpX = xIN[JUNK]
   Yout[I] = INTERPOL( tmpY, tmpX, xOUT[I] )
 ENDIF

 IF JUNKCT LE 1 THEN BEGIN
 ;Use theoretical value
  yOut[I] = yREPLACE[I]
 ENDIF
ENDFOR

return,yOUT
end
