;
;  read_lasp_ascii_file.pro
;
;    USAGE:  data = read_lasp_ascii_file(filename)
;
;    Also includes  create_var.pro  (first function)
;
;    Updated  2007-03-20 by Tom Woods to include on_ioerror (error checking)
;

function create_var, type, value=value, dimensions=dimensions
;
; Create a variable of the given type and dimensions.  The dimensions
; argument may be absent (in which case a scalar is returned), a scalar
; (in which case a 1-dimensional array is returned) or an array of length
; n (in which case an n-dimensional array is returned).
;
  compile_opt idl2
;
; B. Knapp, 2007-02-23
;
  if n_elements(value) eq 0 then value=0
  case type of
       0: message,'Cannot create an undefined variable'
       1: var = byte(value)
       2: var = fix(value)
       3: var = long(value)
       4: var = float(value)
       5: var = double(value)
       6: var = complex(value)
       7: var = ''
       8: message,'Cannot create a struct; use create_struct'
       9: var = dcomplex(value)
      10: var = ptr_new()
      11: var = obj_new()
      12: var = uint(value)
      13: var = ulong(value)
      14: var = long64(value)
      15: var = ulong64(value)
    else: message, string(type, "('Unrecognized variable type:',i3)")
  endcase
;
  if n_elements(dimensions) gt 0 then $
    return, replicate(var, dimensions) $
  else $
    return, var
end


function read_lasp_ascii_file, filename
;
; Returns an array of structures representing the data from the given 
; LASP-formatted ASCII file
;
; B. Knapp, 2007-03-07
;
; Display usage?
  if n_elements(filename) eq 0 then begin
    print, ''
    print, ' Returns an array of structures, corresponding to the data'
    print, ' and data definitions contained in the named LASP-formatted '
    print, ' ASCII data file.'
    print, ''    
    print, ' data = read_lasp_ascii_file(filename)'
    return, ''
  endif
;
; IDL types
  type = 'Und I1  I2  I4  R4  R8  C4  StrnStruC8  Ptr Obj UI2 UI4 I8  UI8'
;
  record = ''
  
  on_ioerror, err_open
;
; Do we have a zipped file?
  ext = strmid(filename,strpos(filename,'.',/reverse_search)+1)
  if ext eq 'gz' then $
    openr, lun, filename, /get_lun, /compress $
  else $
    openr, lun, filename, /get_lun

  on_ioerror, err_read

;
; First, obtain the structure tags and assemble a create_struct() command
  while strpos(record, '***DATA DEFINITIONS***') lt 0 do readf, lun, record
  tokens = strsplit(record, ';, =',/extract)
  n_tags = long(tokens[3])
  tags = strarr(n_tags)
  vals = ''
  format = '('
  for i=0,n_tags-1 do begin
    readf, lun, record
    tokens = strsplit(record,';, ',/extract)
    tags[i] = tokens[0]
    if vals eq '' then begin
      vals = 'create_var('+string(strpos(type,tokens[1])/4,"(i2)")+')'
      format = format+tokens[2]
    endif else begin
      vals = vals+',create_var('+string(strpos(type,tokens[1])/4,"(i2)")+')'
      format = format+','+tokens[2]
    endelse
  endfor
  format = format+')'
;
; Create the structure
  cmd = 'str=create_struct(tags,'+vals+')'
  if not execute(cmd) then message,'Cannot create structure with: '+cmd
;
; Read the data
  while strpos(record, '***DATA RECORDS***') lt 0 do readf, lun, record
  tokens = strsplit(record, ';, =',/extract)
  n_data = long(tokens[3])
  data = replicate(str, n_data)
  readf,lun,data,format=format
  close, lun
  free_lun, lun
;
  on_ioerror, NULL
  return, data

err_open:
  on_ioerror, NULL
  print, 'ERROR: read_lasp_ascii_file() could not OPEN the file: ', filename
  return, -1

err_read:
  on_ioerror, NULL
  close, lun
  free_lun, lun
  print, 'ERROR: read_lasp_ascii_file() had READ error for file: ', filename
  print, '    ', !ERR_STRING
  return, -1

end
