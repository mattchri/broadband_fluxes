function planck_function, x, t, wn=wn, fq=fq, wl=wl
;+
; function planck_function, x, t, wn=wn, wl=wl, fq=fq
;
; Calculates the value of the plank function at the given wavelengths,
; wavenumbers or frequencies and temperatures.
; RETURNS
; The spectral radiance for each wl/wn/fq and temperature combination
; supplied
; Returned value will have dimension as n_x n_t, where n_x and n_t are
; the number of elements in x and t respectively.
; Units returned values will either be:
; W / (m^2 sr m)  - wavelength based
; W / (m^2 sr Hz) - frequency based
; W cm / (m^2 sr) - wavenumber based
; INPUT PARAMETERS
; x   The wavelengths in microns (default), wavenumbers in cm-1 (if wn
;     keyword is set) or frequencies in Hz (if fq keyword is set) at 
;     which the radiance is required
; t   The BB temperatures in Kelvin at which the radiance is required
; KEYWORDS
; /wn Calculation will assume x are wavenumbers in cm-1
; /fq Calculation will assume x are frequencies in Hz
; /wl Calculation will assume x are wavelengths in microns (this is 
;     the default)
; NOTE: If the calculation is done as a function of wavelength, the 
;       spectral radiances will be returned per unit wavelength (m). 
;       If frequency is used, radiances will be returned per unit 
;       frequency (Hz).
;       If wavenumber is used, radiances will be returned per unit 
;       wavenumber (cm-1).
;       THESE ARE ALL DIFFERENT NUMBERS
; HISTORY
; 18/07/07 G Thomas Original
; 19/07/07 G Thomas Correction to expression to produce spectral 
;                   radiance as a function of wavenumber
; BUGS
; None known
;-

; Define some constants
  c = 2.99792458d+08            ; Speed of light m/s
  h = 6.62606896d-34            ; Planck's constant J.s
  k = 1.38065042d-23            ; Blotzmann's constant J/K

  n_x = n_elements(x)
  n_t = n_elements(t)
  rad = dblarr(n_x,n_t)

  if keyword_set(wn) then begin
     wn = x * 100d0             ; wn in SI units
     for i=0,n_t-1 do begin     ; Assume less t's than x's
        rad[*,i] = (2d0 * h * wn^3 * c^2) / $
                   (exp((h * wn * c) / (k * t[i])) - 1d0)
     endfor
     if n_elements(rad) eq 1 then rad = rad[0]
     return, rad * 100d0       ; Return wn to cm-1 units
  endif else if keyword_set(fq) then begin
     fq = x
     for i=0,n_t-1 do begin
        rad[*,i] = (2d0 * h * fq^3) / $
                   (c^2 * (exp((h * fq) / (k * t[i])) - 1d0))
     endfor
     if n_elements(rad) eq 1 then rad = rad[0]
     return, rad
  endif else begin
     wl = x * 1d-6              ; wavelength in SI units
     for i=0,n_t-1 do begin
        rad[*,i] = (2d0 * h * c^2) / $
                   (wl^5 * (exp((h * c) / (wl* k * t[i])) - 1d0))
     endfor
     if n_elements(rad) eq 1 then rad = rad[0]
     return, rad
  endelse
end
