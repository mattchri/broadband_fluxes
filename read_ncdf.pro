;+
;
; NAME:
;       read_ncdf
;
; PURPOSE:
;       Function to read variable and attribute data from NetCDF 
;       format files.
;
; CATEGORY:
;       NCDF
;
; LANGUAGE:
;       IDL v5+
;
; CALLING SEQUENCE:
; result = read_ncdf( ncdf_file, $                               ; Input
;                     data, $                                    ; Output
;                     variable_list         = variable_list, $   ; Input keyword
;                     count                 = count, $           ; Input keyword
;                     offset                = offset, $          ; Input keyword
;                     stride                = stride, $          ; Input keyword
;                     attributes            = attributes, $      ; Input keyword
;                     variable_attributes   = variable_attributes, $
;                                                                ; Input keyword
;                     global_attributes     = global_attributes, $
;                                                                ; Input keyword
;                     no_var_byte_to_string = no_var_byte_to_string, $
;                                                                ; Input keyword
;                     no_att_byte_to_string = no_att_byte_to_string, $
;                                                                ; Input keyword
;                     store_name_case       = store_name_case, $ ; Input keyword
;                     apply_scale_offset    = apply_scale_offset, $ Depreciated
;                     no_scale_offset       = no_scale_offset, $ ; Input keyword
;                                                                ; Input keyword
;                     quiet                 = quiet, $           ; Depreciated
;                     show_dimensions       = show_dimensions )  ; Input keyword
;
; INPUTS:
;       ncdf_file:     The name of the NetCDF file to read
;
; INPUT KEYWORD PARAMETERS:
;       no_data                 Set this keyword to only read dimensions
;                               and attributes from file (ie. no actual
;                               data values will be returned). If used
;                               without attributes keywords, only dimesions 
;                               will be returned.
;                               If /global_attributes or /attributes are set
;                               then global attributes are returned
;                               If /variable_attributes or /attributes are
;                               set, then substructures containing
;                               attributes of each variable will be included
;       variable_list:          A string array of variable name to read from
;                               the NetCDF file. If not specified, ALL the
;                               variables are read.
;       count:                  Set this keyword to a vector containing the
;                               number of points in each dimension that are
;                               required for a variable read. It is a 1-based
;                               vector and defaults to match the size of all
;                               dimensions so that all data is read.
;       offset:                 Set this keyword to a vector containing the
;                               starting index position for each dimension of
;                               the variable required. It is a 0-based
;                               vector and defaults to zero for every dimension
;                               so that all data is read.
;       stride:                 Set this keyword to a vector containing the
;                               strides, or sampling intervals, between accessed
;                               values of the required variable. It is a 1-based
;                               vector and defaults to one for every dimension
;                               so that all data is read.
;       attributes:             Set this keyword to return all attribute data
;                               (both variable and global). Equivalent to
;                               setting both the variable_attributes &
;                               global_attributes keywords.
;       variable_attributes:    Set this keyword to return variable
;                               attribute data. Using this keyword modified the
;                               the form of the output structure. See the 
;                               OUTPUTS description below.
;       global_attributes:      Set this keyword to return global
;                               attribute data.
;       no_var_byte_to_string:  Set this keyword to prevent the
;                               conversion of BYTE variable data
;                               to STRING type. (IDL 5.2 and earlier only)
;       no_att_byte_to_string:  Set this keyword to prevent the
;                               conversion of BYTE attribute data
;                               to STRING type. (IDL 5.2 and earlier only)
;       store_name_case:        If set, the function will add a field to all
;                               output variables which contains the
;                               case-specific variable names and 
;                               global attribute names (note, all variables
;                               AND attributes will be returned as structures)
;       tag_attribute:          If set, data sub-structures will include 
;                               the tag "attribute", which will be set to
;                               1 for global attributes, and 0 otherwise
;       apply_scale_offset:     Depreciated - now default behaviour.
;       no_scale_offset:        If set, the function will not apply any 
;                               scale_factor and/or add_offset attributes.
;                               I.e. data will be returned in the form it is
;                               stored in the file.
;       quiet:                  Depreciated - now default behaviour.
;       show_dimensions:        Set this keyword to show information about
;                               the dimensions of the file (old default).
;       notrap:                 Setting this keyword prevents IDL from
;                               catching errors. Execution will just
;                               stop where the problem occurs
;
; OUTPUTS:
;       data:          The data structure containing the file data
;                      requested.
;
;                      OUTPUT DATA STRUCTURE FORM
;                      --------------------------
;                      o The file dimensions are always returned,
;
;                          data.dim1
;                              .dim2
;                              .dim3
;                            .....
;                              .dimN
;
;                      o If variable data is read in, they are present in
;                        the output structure like so:
;
;                          data.var1
;                              .var2
;                              .var3
;                            .....
;                              .varN
;  
;                      o If variable attributes are also requested, the variable
;                        portion of the output structure has the form:
;  
;                          data.var1.DATA
;                                   .att1
;                                   .att2
;                                 .....
;                                   .attN
;                              .var2.DATA
;                                   .att1
;                                   .att2
;                                 .....
;                                   .attN
;                            .....
;                              .varN.DATA
;                                   .att1
;                                   .att2
;                                 .....
;                                   .attN
;
;                        where the capitalised tag DATA is the actual tag name
;                        used for the variable data.
;
;                      o If global attributes are requested, they are present 
;                        in the output structure like so:
;  
;                          data.gatt1
;                              .gatt2
;                              .gatt3
;                            .....
;                              .gattN
;
;
; FUNCTION RESULT:
;       Error_Status: The return value is an integer defining the error status.
;                     The error codes are defined in the error_codes.pro file.
;                     If == SUCCESS the netCDF data read was successful.
;                        == FAILURE an unrecoverable error occurred.
;                     UNITS:      N/A
;                     TYPE:       INTEGER
;                     DIMENSION:  Scalar
;
; CALLS:
;       is_ncdf:   Function to determine if a file is NetCDF format.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       None.
;
; PROCEDURE:
;       As each variable/attribute is read in, it is appended to the
;       output data structure.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 23-Sep-1999
;                       paul.vandelst@ssec.wisc.edu
;       Modified by:    Gareth Thomas, 06-Jun-2008
;                   Hardwired path to error_codes.pro for EODG path
;                       Gareth Thomas, 24-Jun-2008
;                   Dimension names now have "d_" put on the front when
;                   written to the output structure. This avoids an
;                   error if a variable shares a name with a dimension.
;                       Gareth Thomas, 19-Aug-2008
;                   Added the NO_DATA keyword
;                       Gareth Thomas, 19-Apr-2011
;                   Added the ATTRIBUTES keyword
;                       Gareth Thomas, 12-Jul-2012
;                   Added the store_name_case keyword
;                       Gareth Thomas, 15-Feb-2013
;                   Added apply_scale_offset keyword
;                       Gareth Thomas, 19-Apr-2013
;                   Changed so that "/quiet" and "/apply_scale_offset"
;                   are now the default behavior (depreciating these
;                   keywords).
;                   Added a new no_scale_offset keyword to override this
;                   behavior (messages can be switched on with /verbose)
;                       Gareth Thomas, 28-May-2014
;                   Changed so that offset, count & stride keywords
;                   are only passed to ncdf_varget if at least one of
;                   them has been passed to read_ncdf() (it is
;                   possible this _might_ prevent unnecessary I/O)
;                   Also added code to deal with an apparent bug in
;                   the IDL NetCDF DLM which can cause the code to
;                   hang at 100% CPU load when the stride keyword is
;                   used 
;                       Gareth Thomas, 18-Mar-2015
;                   Added tag_attribute keyword
;                       Gareth Thomas, 15-Jul-2015
;                   Added support for files which contain scale and
;                   offset attributes defined as arrays. It is assumed
;                   that these slice the data arrays in their first
;                   dimension (as seen when read into IDL).
;                       Gareth Thomas, 19-Jul-2016
;                   Added support for /variable_attributes to the
;                   /no_data keyword
;                       Gareth Thomas, 21-Jul-2016
;                   Bug-fix: _fillvalues are no-longer affected by 
;                   scale/offset
;
; Original version: (C) Paul van Delst, 1999, 2006
;
;-

;===============================================================================
;
; Function to check the validity of the COUNT, OFFSET, and STRIDE vector
; keywords.
;
; Adapted from Liam Gumley's NC_READ.PRO
;
;===============================================================================

FUNCTION check_vectors, ncdf_id, $
                        variable_info, $
                        count, $
                        offset, $
                        stride, $
                        count_vector, $
                        offset_vector, $
                        stride_vector, $
                        variable_dimensions



;------------------------------------------------------------------------------
;                         -- SET UP ERROR HANDLER --
;------------------------------------------------------------------------------
;----------------------------------------------------------------------------
;                         -- DEFINE ERROR CODES --                           
;----------------------------------------------------------------------------
  SUCCESS_USE =  1L
  SUCCESS_DEF =  0L
  FAILURE     = -1L
;----------------------------------------------------------------------------
;                      -- DEFINE TRUE/FALSE EXPLICITLY --                    
;----------------------------------------------------------------------------
  FALSE = 0
  TRUE  = 1

  CATCH, Error_Status
  
  IF ( Error_Status NE 0 ) THEN BEGIN
     CATCH, /CANCEL
     MESSAGE, !ERROR_STATE.MSG, /CONTINUE
     RETURN, FAILURE
  ENDIF


;------------------------------------------------------------------------------
;                         -- Get the variable dimensions --
;------------------------------------------------------------------------------

; Create the variable dimension array
  variable_dimensions = LONARR( variable_info.NDIMS )

; Loop over the dimensions
  FOR i = 0, variable_info.NDIMS - 1 DO BEGIN

;   Get the dimension name and size
    NCDF_DIMINQ, ncdf_id, $                 ; Input
                 variable_info.DIM[ i ], $  ; Input
                 dimension_name, $          ; Output
                 dimension_size             ; Output

;   Save the dimension size
    variable_dimensions[ i ] = dimension_size

  ENDFOR



;------------------------------------------------------------------------------
;    -- Check for COUNT, OFFSET, and STRIDE vectors of the wrong length --
;------------------------------------------------------------------------------

; Count vector
  IF ( N_ELEMENTS( count ) NE 0 AND $
       N_ELEMENTS( count ) NE variable_info.NDIMS ) THEN BEGIN
    MESSAGE, 'COUNT vector must have ' + $
             STRTRIM( variable_info.NDIMS, 2 ) + $
             ' dimensions for ' + $
             STRUPCASE( variable_info.NAME ) + $
             ' variable', /NONAME, /NOPRINT
  ENDIF

; Offset vector
  IF ( N_ELEMENTS( offset ) NE 0 AND $
       N_ELEMENTS( offset ) NE variable_info.NDIMS ) THEN BEGIN
    MESSAGE, 'OFFSET vector must have ' + $
             STRTRIM( variable_info.NDIMS, 2 ) + $
             ' dimensions for ' + $
             STRUPCASE( variable_info.NAME ) + $
             ' variable', /NONAME, /NOPRINT
  ENDIF

; Stride vector
  IF ( N_ELEMENTS( stride ) NE 0 AND $
       N_ELEMENTS( stride ) NE variable_info.NDIMS ) THEN BEGIN
    MESSAGE, 'STRIDE vector must have ' + $
             STRTRIM( variable_info.NDIMS, 2 ) + $
             ' dimensions for ' + $
             STRUPCASE( variable_info.NAME ) + $
             ' variable', /NONAME, /NOPRINT
  ENDIF



;------------------------------------------------------------------------------
;  -- Check for definition and range of COUNT, OFFSET, and STRIDE vectors --
;------------------------------------------------------------------------------

; Define the default return value - Success and count/offset/stride
;                                   keywords not required
  retval = SUCCESS_DEF

; Offset vector
; Is it defined?
  IF ( N_ELEMENTS( offset ) EQ 0 ) THEN BEGIN
    offset_vector = REPLICATE( 0L, variable_info.NDIMS )
    retval = SUCCESS_USE
 ENDIF ELSE offset_vector = LONG( offset )
; Is it valid?
  offset_vector = ( offset_vector < ( variable_dimensions - 1L ) ) > $
                  REPLICATE( 0L, variable_info.NDIMS )

; Stride vector
; Is it defined?
  IF ( N_ELEMENTS( stride ) EQ 0 ) THEN BEGIN
    stride_vector = REPLICATE( 1L, variable_info.NDIMS )
    retval = SUCCESS_USE
  ENDIF ELSE stride_vector = LONG(stride)
; Is it valid?
  stride_vector = ( stride_vector < ( variable_dimensions - offset_vector ) ) > $
                  REPLICATE( 1L, variable_info.NDIMS )

; Count vector
; Is it defined?
  IF ( N_ELEMENTS( count ) EQ 0 ) THEN BEGIN
    count_vector = ( variable_dimensions - offset_vector ) / stride_vector
    retval = SUCCESS_USE
  ENDIF  ELSE count_vector = LONG( count )
; Is it valid?
  count_vector = ( count_vector < ( ( variable_dimensions - offset_vector ) / stride_vector ) ) > $
                 REPLICATE( 1L, variable_info.NDIMS )



;------------------------------------------------------------------------------
;                                  -- Done --
;------------------------------------------------------------------------------

  CATCH, /CANCEL
  RETURN, retval

END


;===============================================================================
;
; Function to convert BYTE/CHAR values to a STRING data type.
;
; Need this because all string netCDF data types were returned as BYTE arrays.
; For pre IDL v5.3 the returned data type string is "BYTE"
; For IDL v5.3 the returned data type string is "CHAR"
;
;
;===============================================================================

FUNCTION convert_string, data_type, input_string, $
                         no_convert = no_convert

  ; Set the IDL version with the bug fix
  fixed_IDL_version = 5.3

  ; If the data type is CHAR, then we have a string and convert it
  IF ( STRUPCASE( data_type ) EQ 'CHAR' ) THEN $
    RETURN, STRING( input_string )

  ; The data type is not CHAR. Maybe it needs converting, maybe not.
  IF ( FLOAT( !VERSION.RELEASE ) LT fixed_IDL_version AND $
       STRUPCASE( data_type ) EQ 'BYTE'               AND $
       ( NOT KEYWORD_SET( no_convert ) )                  ) THEN $
    RETURN, STRING( input_string )

  ; Don't do anything
  RETURN, input_string

END

;===============================================================================
;
; Main function
;
;===============================================================================

FUNCTION Read_NCDF, ncdf_file, $         ; Input
                    data, $              ; Output

                    ; -- Which variables to read keyword
                    no_data = no_data, $                ; Input keyword
                    variable_list = variable_list, $    ; Input keyword

                    ; -- How to read the variables keywords
                    count  = count, $    ; Input keyword
                    offset = offset, $   ; Input keyword
                    stride = stride, $   ; Input keyword

                    ; -- Attribute keywords
                    attributes          = attributes, $             ; Input keyword
                    variable_attributes = variable_attributes, $    ; Input keyword
                    global_attributes   = global_attributes, $      ; Input keyword

                    store_name_case     = store_name_case, $        ; Input keyword
                    tag_attribute      = tag_attribute, $         ; Input keyword
                    
                    ; -- Conversion keywords
                    no_var_byte_to_string = no_var_byte_to_string, $   ; Input keyword
                    no_att_byte_to_string = no_att_byte_to_string, $   ; Input keyword
                    apply_scale_offset    = apply_scale_offset,    $   ; Depreciated
                    no_scale_offset       = no_scale_offset,       $   ; Input keyword

                    ; -- Shhhhh
                    quiet = quiet, $                     ; Depreciated
                    ; -- Speak to me
                    show_dimensions = show_dimensions, $ ; Input keyword
                    ; -- Don't catch errors - halt at the problem
                    notrap=notrap

;------------------------------------------------------------------------------
;                         -- SET UP ERROR HANDLER --
;------------------------------------------------------------------------------
  SUCCESS =  1L
  FAILURE = -1L
  FALSE = 0
  TRUE  = 1

  if ~keyword_set(notrap) then begin
     CATCH, Error_Status
     
     IF ( Error_Status NE 0 ) THEN BEGIN
        CATCH, /CANCEL
        MESSAGE, !ERROR_STATE.MSG, /CONTINUE
        IF ( N_ELEMENTS( NCDF_Id ) NE 0 ) THEN NCDF_CLOSE, ncdf_id
        RETURN, FAILURE
     ENDIF    
  endif



;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;                            -- Check input --
;------------------------------------------------------------------------------

  n_arguments = 2
  IF ( N_PARAMS() LT n_arguments ) THEN BEGIN
    MESSAGE, 'Invlaid number of arguments', /NONAME, /NOPRINT
  ENDIF

; Check that required arguments are defined
  IF ( N_ELEMENTS( ncdf_file ) EQ 0 ) THEN BEGIN
    MESSAGE, 'Input NCDF_FILE argument not defined!', /NONAME, /NOPRINT
  ENDIF

; Check that file argument is a string
  IF ( SIZE( ncdf_file, /TNAME ) NE 'STRING' ) THEN BEGIN
    MESSAGE, 'Input NCDF_FILE argument must be a string', /NONAME, /NOPRINT
  ENDIF

; Check variable_list keyword. If the variable_list
; keyword is NOT set, the default action is to read
; ALL the data in the NetCDF file
  IF ( KEYWORD_SET( variable_list ) ) THEN $
    all_variables = 0 $
  ELSE $
    all_variables = 1



;------------------------------------------------------------------------------
;                   -- Make sure that file is in NetCDF format --
;------------------------------------------------------------------------------

  result = is_ncdf( ncdf_file )

  IF ( result NE SUCCESS ) THEN BEGIN
    MESSAGE, ncdf_file + ' is not a NetCDF format file', /NONAME, /NOPRINT
  ENDIF



;------------------------------------------------------------------------------
;                        -- Open the netCDF data file --
;------------------------------------------------------------------------------

  ncdf_id = NCDF_OPEN( ncdf_file, /NOWRITE )

  IF ( ncdf_id EQ -1 ) THEN BEGIN
    MESSAGE, 'Error opening file ' + ncdf_file, /NONAME, /NOPRINT
  ENDIF



;------------------------------------------------------------------------------
;                   -- Print out some dimension information --
;------------------------------------------------------------------------------

  ncdf_file_info = NCDF_INQUIRE( ncdf_id )

  IF ( KEYWORD_SET( show_dimensions ) ) THEN BEGIN

    PRINT, FORMAT = '( 10x,"Number of dimensions        : ",i7 )', $
                    ncdf_file_info.NDIMS
    PRINT, FORMAT = '( 10x,"Number of variables         : ",i7 )', $
                    ncdf_file_info.NVARS
    PRINT, FORMAT = '( 10x,"Number of global attributes : ",i7 )', $
                    ncdf_file_info.NGATTS
    PRINT, FORMAT = '( 10x,"ID of unlimited dimension   : ",i7 )', $
                    ncdf_file_info.RECDIM

  ENDIF
  
;------------------------------------------------------------------------------
;                       -- Get the dimension information --
;------------------------------------------------------------------------------

  FOR i=0, ncdf_file_info.NDIMS-1 DO BEGIN

;   Get dimension info
    NCDF_DIMINQ, ncdf_id, i, DimName, DimSize
    DimName = idl_validname(DimName,/convert_all)

    DimName='d_'+DimName
;   Load into structure
    IF ( i EQ 0 ) THEN $
      data = CREATE_STRUCT( DimName, DimSize ) $
    ELSE $
      data = CREATE_STRUCT( data, DimName, DimSize )

  ENDFOR

;------------------------------------------------------------------------------
;                     -- Read the global attributes? --
;------------------------------------------------------------------------------

  IF ( KEYWORD_SET( global_attributes ) OR KEYWORD_SET( attributes ) ) THEN BEGIN

;   Determine the number of global attributes
    n_global_attributes = ncdf_file_info.NGATTS

;   Are there any global attributes to read?
    IF ( n_global_attributes GT 0 ) THEN BEGIN

;   Add the number of attributes to the output structure
      data = create_struct( data, 'd_nattributes', n_global_attributes )

;     loop over global attributes
      FOR i = 0, n_global_attributes - 1 DO BEGIN

;       Get global attribute name
        attribute_name = NCDF_ATTNAME( ncdf_id, $  ; Input
                                       i, $        ; Input
                                       /GLOBAL )   ; Input keyword
;       Get global attribute value
        NCDF_ATTGET, ncdf_id, $         ; Input
                     attribute_name, $  ; Input
                     attribute, $       ; Output
                     /GLOBAL            ; Input keyword

;       Get global attribute info
        attribute_info = NCDF_ATTINQ( ncdf_id, $         ; Input
                                      attribute_name, $  ; Input
                                      /GLOBAL )          ; Input keyword

;       If necessary and required, convert BYTE/CHAR attribute to STRING
        attribute = convert_string( attribute_info.datatype, $
                                    attribute, $
                                    no_convert = no_att_byte_to_string )
        
;       If required, turn the attribute variable, that contains both the 
;       case sensitive name, and/or attribute flag, as well as attribute data
        attvar = attribute
        if keyword_set(store_name_case) then attvar = {name : attribute_name, $
                                                       data : attribute }
        if keyword_set(tag_attribute) then begin
           if keyword_set(store_name_case) $
           then attvar = create_struct(attvar, 'attribute', 1) $
           else attvar = {data : attribute, attribute : 1}
        endif
;       Make sure Attribute name is a legal IDL variable name
        attribute_name = idl_validname(attribute_name,/convert_all)


;       Append to structure
        data = CREATE_STRUCT( data, $                ; Input
                              attribute_name, $      ; Input
                              attvar )               ; Input

      ENDFOR      ; Loop over global attributes

;   No global attributes to read
    ENDIF ELSE BEGIN
      MESSAGE, 'No global attributes to read!', /INFO
    ENDELSE     ; n_global_attributes > 0 IF statement

  ENDIF

;------------------------------------------------------------------------------
;                            -- No data required?--
;------------------------------------------------------------------------------

  IF KEYWORD_SET(NO_DATA) and ~KEYWORD_SET(VARIABLE_ATTRIBUTES) and $
     ~KEYWORD_SET(ATTRIBUTES) THEN BEGIN

;    If no data is required, return here
     NCDF_CLOSE, ncdf_id
     CATCH, /CANCEL
     RETURN, SUCCESS
     
  ENDIF

;------------------------------------------------------------------------------
;                             -- Read the data --
;------------------------------------------------------------------------------

; Set the number of variables to read and
; initialise the valid variable counter
  IF ( all_variables EQ 0 ) THEN $
    n_variables = N_ELEMENTS( variable_list ) $
  ELSE $
    n_variables = ncdf_file_info.NVARS
  
; Are there any variables to read?
  IF ( n_variables GT 0 ) THEN BEGIN

;   Flag for IDL NetCDF DLM bug work-around warning:
    IDL_BUG_WARNING=0
;   Loop over variables
    FOR i = 0, n_variables - 1 DO BEGIN
;     Reset the local values of scale and offset
       scale_factor = 0.0
       add_offset   = 0.0

;     Get the variable ID
      IF ( all_variables EQ 0 ) THEN BEGIN

;       Only getting requested data so
;       set the current variable name
        variable_name = variable_list[ i ]

;       Get the current variable ID
        variable_id = NCDF_VARID( ncdf_id, $       ; Input
                                  variable_name )  ; Input

;       Is the current variable present in the NetCDF file?
        IF ( variable_id LT 0 ) THEN BEGIN
          MESSAGE, 'Variable ' + variable_name + ' not present in ' + ncdf_file, /NONAME, /NOPRINT
        ENDIF

      ENDIF ELSE BEGIN

;       Getting all data. Use loop counter as variable ID
        variable_id = i

      ENDELSE

;     Get the variable info
      variable_info = NCDF_VARINQ( ncdf_id, $     ; Input
                                   variable_id )  ; Input

;     Make sure we have the variable name
      variable_name = variable_info.NAME

;      print, 'Reading variable name: ',variable_name

      IF ~KEYWORD_SET(NO_DATA) THEN BEGIN
;     Does the current variable have dimensions?
      IF ( variable_info.NDIMS EQ 0 ) THEN BEGIN

;       No. It is scalar. Simply read it.
        NCDF_VARGET, ncdf_id, $          ; Input
                     variable_id, $      ; Input
                     variable_data       ; Output

      ENDIF ELSE BEGIN

;       Yes. Check COUNT, OFFSET, and STRIDE vectors for this variable.
        result = check_vectors( ncdf_id, $         ; Input
                                variable_info, $   ; Input
                                count, $           ; Input
                                offset, $          ; Input
                                stride, $          ; Input
                                count_vector, $    ; Output
                                offset_vector, $   ; Output
                                stride_vector, $   ; Output
                                variable_dimensions ) ; Output
        IF ( result EQ FAILURE ) THEN BEGIN
          MESSAGE, 'COUNT, OFFSET, and/or STRIDE vector check failed.', /NONAME, /NOPRINT
        ENDIF

;       Read the variable data
;       Check what check_vectors has returned - if it is "TRUE" then
;       we use the count, offset and stride vectors, otherwise we just 
;       do a default read
        if result eq FALSE then begin
           NCDF_VARGET, ncdf_id, $  ; Input
                        variable_id, $ ; Input
                        variable_data  ; Output
        endif else begin

;          NB. There appears to be a bug in the IDL implementation of
;          NCDF_VARGET that causes the NetCDF DLM to hang at 100% CPU
;          under some circumstances if the stride keyword is used. As
;          far as I can tell, it occurs if the variable has it's
;          last (in IDL ordering) dimension = 1. Here I've added a
;          check for this situation. GT 2014-05-28
           if variable_info.ndims gt 1 and $
              variable_dimensions[variable_info.ndims-1] eq 1 then begin
              if keyword_set(stride) and IDL_BUG_warning eq 0 then begin
                 message,/info,'IDL bug prevents setting a stride value for arrays ending with a dimension of 1!'
                 message,/info,'Attempting to read all elements (respecting offset)'
                 IDL_BUG_warning = 1
              endif
              count_vector = variable_dimensions - offset_vector
              NCDF_VARGET, ncdf_id, $                     ; Input
                           variable_id, $                 ; Input
                           variable_data, $               ; Output
                           count  = count_vector, $       ; Input keyword
                           offset = offset_vector         ; Input keyword
           endif else NCDF_VARGET, ncdf_id, $             ; Input
                                   variable_id, $         ; Input
                                   variable_data, $       ; Output
                                   count  = count_vector, $ ; Input keyword
                                   offset = offset_vector, $ ; Input keyword
                                   stride = stride_vector    ; Input keyword
;          end of IDL NetCDF DLM bug work-around        
        endelse ; count/offset/stride needed IF statement

      ENDELSE                    ; Scalar or array? Variable dimension IF statement


;     If necessary and required, convert BYTE/CHAR variable data to STRING
      variable_data = convert_string( variable_info.datatype, $
                                      variable_data, $
                                      no_convert = no_var_byte_to_string )

      ENDIF                        ; No data check
      
;     Wipe any existing variable_str variable
      variable_str = 0

;     If required, put the actual variable name into the structure
      if keyword_set(store_name_case) then $
         variable_str = CREATE_STRUCT( 'name', variable_name )

;     Determine the number of variable attributes
      n_variable_attributes = variable_info.NATTS

;     Retrieve the variable attributes if required
      IF ( ( KEYWORD_SET( variable_attributes ) OR $
             KEYWORD_SET( attributes ) ) AND $
           n_variable_attributes GT 0              ) THEN BEGIN

;       loop over current variable's attribute
        scaleoffread = 0
        FOR j = 0, n_variable_attributes - 1 DO BEGIN

;         Get the current attribute name
          attribute_name = NCDF_ATTNAME( ncdf_id, $       ; Input
                                         variable_id, $   ; Input
                                         j )              ; Input

;         Get the current attribute value
          NCDF_ATTGET, ncdf_id, $         ; Input
                       variable_id, $     ; Input
                       attribute_name, $  ; Input
                       attribute          ; Output

;         Get the current attribute info
          attribute_info = NCDF_ATTINQ( ncdf_id, $        ; Input
                                        variable_id, $    ; Input
                                        attribute_name )  ; Input

;         Check for, and store scale and offset values in local variables
          case strlowcase(attribute_name) of
             'scale_factor' : begin
                NCDF_ATTGET, ncdf_id, $           ; Input
                             variable_id, $       ; Input
                             attribute_name, $    ; Input
                             scale_factor         ; Output
                scaleoffread = scaleoffread + 1
             end
             'add_offset'   : begin
                NCDF_ATTGET, ncdf_id, $           ; Input
                             variable_id, $       ; Input
                             attribute_name, $    ; Input
                             add_offset           ; Output
                scaleoffread = scaleoffread +1
             end
             '_fillvalue'   : begin
                NCDF_ATTGET, ncdf_id, $           ; Input
                             variable_id, $       ; Input
                             attribute_name, $    ; Input
                             _fillvalue           ; Output
             end
             else :
          endcase

;         If necessary and required, convert BYTE attribute to STRING
          attribute = convert_string( attribute_info.datatype, $
                                      attribute, $
                                      no_convert = no_att_byte_to_string )

;         Make sure Attribute name is a legal IDL variable name
          attribute_name = idl_validname(attribute_name,/convert_all)

;         If the apply_scale_offset keyword is set, apply the values
;         to the appropriate attributes
          if ~keyword_set(no_scale_offset) then begin
             case attribute_info.datatype of
                'SHORT' : attribute = float(attribute)
                'INT'   : attribute = float(attribute)
                'LONG'  : attribute = double(attribute)
                else    : goto, skip_conversion
             endcase
             
             if scaleoffread gt 0 then begin
                if scale_factor[0] ne 0.0 then $
                   attribute = attribute * scale_factor
                attribute = attribute + add_offset
             endif
             skip_conversion:
          endif else scaleoffread = 0

;         Add current attribute to variable structure
          sz = size(variable_str)
          if sz[sz[0]+1] eq 8 then $
             variable_str = CREATE_STRUCT( variable_str, $   ; Input
                                           attribute_name, $ ; Input
                                           attribute ) $     ; Input
          else variable_str = CREATE_STRUCT( attribute_name, $ ; Input
                                             attribute )     ; Input

        ENDFOR      ; Loop over current variable attributes

      ENDIF $       ; Get attributes IF statement
      ELSE IF ~KEYWORD_SET(NO_DATA) THEN BEGIN
;        If the Apply_scale_offset keyword has been set, read the
;        appropriate values
         if ( ~keyword_set(no_scale_offset) and $
              ( n_variable_attributes gt 0 ) ) then begin
            scaleoffread = 0
            for j=0, n_variable_attributes-1 do begin
               attribute_name = NCDF_ATTNAME( ncdf_id, $     ; Input
                                              variable_id, $ ; Input
                                              j )            ; Input
               case strlowcase(attribute_name) of
                  'scale_factor' : begin
                     NCDF_ATTGET, ncdf_id, $        ; Input
                                  variable_id, $    ; Input
                                  attribute_name, $ ; Input
                                  scale_factor      ; Output
                     scaleoffread = scaleoffread + 1
                  end
                  'add_offset'   : begin
                     NCDF_ATTGET, ncdf_id, $        ; Input
                                  variable_id, $    ; Input
                                  attribute_name, $ ; Input
                                  add_offset        ; Output
                     scaleoffread = scaleoffread +1
                  end
                  '_fillvalue'   : begin
                     NCDF_ATTGET, ncdf_id, $        ; Input
                                  variable_id, $    ; Input
                                  attribute_name, $ ; Input
                                  _fillvalue        ; Output
                     scaleoffread = scaleoffread +1
                  end
                  else :
               endcase
               if scaleoffread eq 3 then break
            endfor
         endif else scaleoffread = 0

      ENDIF ; End of non-attribute reading checks

;     Now, if scale and/or offset has been found, convert the data to 
;     a floating point type and apply the values
      IF ~KEYWORD_SET(NO_DATA) THEN BEGIN
         if scaleoffread gt 0 then begin
;        First find which values in the data array have the fill
;        value, so that we can ensure they keep the _fillvalue on
;        output
         if n_elements(_fillvalue) gt 0 then $
            is_fill = where(variable_data eq _fillvalue) $
         else is_fill = -1
         case variable_info.datatype of
            'SHORT' : variable_data = float(variable_data)
            'INT'   : variable_data = float(variable_data)
            'LONG'  : variable_data = double(variable_data)
            else :
         endcase
;        Some data has multiple scale and offsets for different parts
;        of a data array. Assume that this is done in the final (first
;        in IDL) index of the array....
         noffset = n_elements(add_offset)
         if noffset gt 1 then begin
            ddim = size(variable_data, /dimensions)
            if ddim[0] ne noffset then message, $
               "Can't make sense of scale/offset arrays. Re-run with NO_SCALE_OFFSET keyword"
            case variable_info.ndims of
               1 : begin
                  if scale_factor[0] ne 0.0 then $
                     variable_data = variable_data * scale_factor
                  variable_data = variable_data + add_offset
               end
               2 : for di=0,noffset-1 do begin
                  if scale_factor[di] ne 0.0 then $
                     variable_data[di,*] = variable_data[di,*] * scale_factor[i]
                  variable_data[di,*] = variable_data[di,*] + add_offset[i]
               end
               3 : for di=0,noffset-1 do begin
                  if scale_factor[di] ne 0.0 then $
                     variable_data[di,*,*] = variable_data[di,*,*] * scale_factor[i]
                  variable_data[di,*,*] = variable_data[di,*,*] + add_offset[i]
               end
               4 : for di=0,noffset-1 do begin
                  if scale_factor[di] ne 0.0 then $
                     variable_data[di,*,*,*] = variable_data[di,*,*,*] * scale_factor[di]
                  variable_data[di,*,*,*] = variable_data[di,*,*,*] + add_offset[di]
               end
               5 : for di=0,noffset-1 do begin
                  if scale_factor[di] ne 0.0 then $
                     variable_data[di,*,*,*,*] = variable_data[di,*,*,*,*] * scale_factor[di]
                  variable_data[di,*,*,*,*] = variable_data[di,*,*,*,*] + add_offset[di]
               end
               else: message, 'Only arrays of up to 5 dimensions are supported for multi-dimension scale/offset values'
            endcase
         endif else begin
            if scale_factor[0] ne 0.0 then $
               variable_data = variable_data * scale_factor
            variable_data = variable_data + add_offset
         endelse
;        Reset fill value data with the correct, unscaled, value
         if is_fill[0] ge 0 then variable_data[is_fill] = _fillvalue
      endif
      ENDIF ; End of check for no_data keyword

;     The data is in its final form now. If the data structure exists,
;     add the data to it and redefine variable data to be the structure
      sz = size(variable_str)
      IF ~KEYWORD_SET(NO_DATA) THEN BEGIN
      if sz[sz[0]+1] eq 8 then begin
         variable_data = create_struct('data', variable_data, variable_str)
;       Also add the attribtue flag (false) to the variable structure
;       if the keyword is set
        if keyword_set(tag_attribute) then $
           variable_data = CREATE_STRUCT( variable_data, $   ; Input
                                         'attribute', 0 )
      endif else if keyword_set(tag_attribute) then $
        variable_data = CREATE_STRUCT( 'data', variable_data, $
                                      'attribute', 0 )

      ENDIF ELSE BEGIN
      if sz[sz[0]+1] eq 8 then variable_data = variable_str $
      else if keyword_set(tag_attribute) then $
         variable_data = CREATE_STRUCT( 'attribute', 0 )
      ENDELSE
;     Make sure variable name is a legal IDL variable name
      variable_name = idl_validname(variable_name,/convert_all)
;     Append data to return structure
;     Because NetCDF files are case sensitive, unlike IDL variable
;     names, its possible to get two variables in the same file with
;     the same name - like t (time) and T (temperature) in ERA_interim
;     NetCDF profile data. Add a hack to append a number to the second
;     occurance of the same name...
      variable_name_new = variable_name
      replicator = 2
      while where(tag_names(data) eq variable_name_new) ge 0 do begin
         variable_name_new = variable_name + strtrim(replicator,2)
         replicator = replicator+1
      endwhile
      data = CREATE_STRUCT( data, $                       ; Input
                            variable_name_new, $          ; Input
                            TEMPORARY( variable_data ) )  ; Input

    ENDFOR      ; Loop over requested variables

; No data to read
  ENDIF ELSE BEGIN

    MESSAGE, 'No variables to read!', /INFO

  ENDELSE     ; n_variables > 0 IF statement



;------------------------------------------------------------------------------
;                       -- Close the NetCDF data file --
;------------------------------------------------------------------------------

  NCDF_CLOSE, ncdf_id



;------------------------------------------------------------------------------
;                                  -- Done --
;------------------------------------------------------------------------------

  CATCH, /CANCEL
  RETURN, SUCCESS


END ; FUNCTION Read_NCDF
