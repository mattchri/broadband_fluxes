;==========================================================================
;+
;	function GREEK_LETTERS
;
;	Description: based on the Vector Font Tables
;       To read the table note that each row increases by:
;       4,6,10,12,14,16,20,22,24,26,30,32,34,36 and multiple by 10
;then add the column number
;note, the website is wrong
;http://www.harrisgeospatial.com/docs/Using_Hershey_Vector_Fon.html;
;it indicates 32,48,64,80,96... these values are wrong!!!! I
;don't know why but use the above format instead of these values.)
; 
;	Use
; 
;	Parameters
; 
;	Keywords
; $Id$
;-
;==========================================================================
FUNCTION greek_letters

!p.font=0

h=hash()

thisLetter = "104B
h('delta') = '!9' + String(thisLetter) + '!X'

thisLetter = "155B
h('mu') = '!9' + String(thisLetter) + '!X'

thisLetter = "164B
h('tau') = '!9' + String(thisLetter) + '!X'

thisLetter = "162B
h('sigma') = '!9s' + '!X'

thisLetter = "162B
h('rho') = '!9' + String(thisLetter) + '!X'

thisLetter = "141B
h('alpha') = '!9' + String(thisLetter) + '!X'

h('degree') = "!Eo!N"

thisLetter = "167B
h('omega') = '!9' + String(thisLetter) + '!X'

thisLetter = "261B
h('plusminus') = '!9' + String(thisLetter) + '!X'

thisLetter = "305B
h('angstrom') = '!3' + String(thisLetter) + '!X'

thisLetter = "366B
h('oe') = '!3' + String(thisLetter) + '!X'

thisLetter = "143B
h('chi') = '!9' + String(thisLetter) + '!X'

greekL = h.toStruct()

return,greekL
end
