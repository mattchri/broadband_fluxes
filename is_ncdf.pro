;+
;
; (C) Cooperative Institute for Meteorological Satellite Studies, 1999
;
; NAME:
;       is_ncdf
;
; PURPOSE:
;       Fucntion to check if a file is a NetCDF file.
;
; CATEGORY:
;       NCDF
;
; LANGUAGE:
;       IDL v5
;
; CALLING SEQUENCE:
;       result = is_ncdf( file )
;
; INPUTS:
;       file:    Name of the input file to test.
;
; KEYWORD PARAMETERS:
;       None.
;
; OUTPUTS:
;       Function returns -1 if the file is NOT in NetCDF format or if
;                           any errors occur with input processing,
;                         1 if the file IS in NetCDF format
;
; CALLS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       None known.
;
; PROCEDURE:
;       Function uses the CATCH procedure to handle the error that
;         may occur using NCDF_OPEN to open a non-NetCDF format file.
;
; EXAMPLE:
;       IDL> PRINT, is_ncdf( 'blah' )
;       % IS_NCDF: blah is not a NetCDF format file.
;             -1
;
;       IDL> PRINT, is_ncdf( '980711C2_RSE.nc' )
;              1
;
; MODIFICATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 19-Apr-1999
;                       paul.vandelst@ssec.wisc.edu
;       Gareth Thomas, AOPP, 25-Apr-2012
;                       Added tests for the existance and readabilty of the
;                       file
;       Gareth Thomas, RAL Space, 7-Nov-2013
;                       Altered the error catching so that any error
;                       messages from the NetCDF DLM will be printed.
;
;-

FUNCTION is_ncdf, file


;------------------------------------------------------------------------------
;                             -- RCS Id keyword --
;------------------------------------------------------------------------------

  rcs_Id = '$Id: is_ncdf.pro,v 1.2 1999/04/19 16:16:59 paulv Exp $'



;------------------------------------------------------------------------------
;                               -- Check input --
;------------------------------------------------------------------------------

  n_arguments = 1
  IF ( N_PARAMS() NE n_arguments) THEN BEGIN
    MESSAGE, 'Must supply an input filename', /INFO
    RETURN, -1
  ENDIF


; ---------------------------------
; Check if file argument is defined
; ---------------------------------

  IF ( N_ELEMENTS( file ) EQ 0 ) THEN BEGIN
    MESSAGE, 'File argument not defined!', /INFO
    RETURN, -1
  ENDIF

  IF ( STRLEN( file ) EQ 0 ) THEN BEGIN
    MESSAGE, 'File name string is zero length!', /INFO
    RETURN, -1
  ENDIF

;-----------------------------------------------------
; Check that the specified file exists and is readable
;-----------------------------------------------------
  if file_test(file) eq 0 then begin
     message, 'File does not exist', /info
     return, -1
  endif else if file_test(file,/read,/regular) eq 0 then begin
     message, 'File is not readable, or is not a regular file', /info
     return, -1
  endif


;------------------------------------------------------------------------------
;                     -- Establish CATCH condition --
;------------------------------------------------------------------------------

  CATCH, error_status

  IF ( error_status NE 0 ) THEN BEGIN
    print, 'Error status: ', strtrim(error_status,2)
    print, !ERROR_STATE.MSG
    CATCH, /CANCEL
    RETURN, -1
  ENDIF



;------------------------------------------------------------------------------
;                        -- Open the input file --
;------------------------------------------------------------------------------

  ncdf_id = NCDF_OPEN( file )

  IF ( ncdf_id EQ -1 ) THEN BEGIN
    MESSAGE, 'Error opening file ' + file, /INFO
    RETURN, -1
  ENDIF



;------------------------------------------------------------------------------
;                         -- No errors occurred --
;------------------------------------------------------------------------------

; ------------------
; Turn off the CATCH
; ------------------

  CATCH, /CANCEL


; ---------------------
; Close the NetCDF file
; ---------------------

  NCDF_CLOSE, ncdf_id


; --------------
; Return success
; --------------

  RETURN, 1

END
