pro multi_colorbar,color_table=color_table

;GET RAINBOW COLORS
loadct,33 ;blue-to-red
IF KEYWORD_SET(color_table) EQ 1 THEN BEGIN
 IF color_table eq 'rainbow' THEN loadct,13
 IF color_table eq 'HAZE' THEN loadct,16
 IF color_table eq 'HUE'  THEN loadct,20
 IF color_table eq 'redtemp' THEN loadct,3
 IF color_table eq 'bluewhite' THEN loadct,1
 IF color_table eq 'diff' THEN getct,70,/grey,/rev
ENDIF

tvlct,r,b,g,/get
trr=fltarr(200)
tbb=fltarr(200)
tgg=fltarr(200)

for i=0,199 do begin
trr(i)=r(fix((257./200.)*i))
tbb(i)=b(fix((257./200.)*i))
tgg(i)=g(fix((257./200.)*i))
endfor

trr=fix(trr)
tbb=fix(tbb)
tgg=fix(tgg)

rr=fltarr(256)
gg=fltarr(256)
bb=fltarr(256)

;pre-assigned colors (0-19)
rr[0:19] = [0,1, 1,0,0, 1,1,0, 1.,.6, .4,.7,.6,0,0,.6,.6,0,.6,.3]*255.
gg[0:19] = [0,1,0,1,0,1,0,1,.7,0.,.4,.7,0,.6,0,.6,0,.6,.4,0.]*255.
bb[0:19] = [0,1,0,0,1,0,1,1,.2,.8,.4,.7,0,0,.6,0,.6,.6,.1,.6]*255.


;rainbow
rr[20:219]=trr
bb[20:219]=tbb
gg[20:219]=tgg

;black-white (220-255)
rr[220:255] = findgen(36)/35. * 255.
gg[220:255] = findgen(36)/35. * 255.
bb[220:255] = findgen(36)/35. * 255.

IF KEYWORD_SET(color_table) EQ 1 THEN BEGIN
 IF COLOR_TABLE EQ 'reverse_gray' then begin
rr[220:255] = reverse( findgen(36)/35. * 255. )
gg[220:255] = reverse( findgen(36)/35. * 255. )
bb[220:255] = reverse( findgen(36)/35. * 255. )
 ENDIF
ENDIF

;IF KEYWORD_SET(color_table) EQ 1 THEN BEGIN
; IF color_table eq 'redtemp' then begin
;  INDS=REVERSE( INDGEN(200)+20 )
;  rr[20:219]=rr[INDS]
;  gg[20:219]=gg[INDS]
;  bb[20:219]=bb[INDS]
; ENDIF
;ENDIF


tvlct,rr,bb,gg

return
end
