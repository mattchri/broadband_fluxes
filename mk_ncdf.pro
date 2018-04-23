pro mk_ncdf,struct=struct,ncname=ncname

nVars = n_elements(struct)

dat=STRUCT(0).data
sz=size(dat)
xdim_name = 'nx'
xdim=sz(1)

;open file
ncid=ncdf_create(ncname, /CLOBBER, /NETCDF4_FORMAT)

;Define dimensions
xId=ncdf_dimdef(ncid, xdim_name, xdim)

;Dimensions for each variable
dims = [xId]

vID=lonarr(nVars)
FOR I=0,nVars-1 DO BEGIN
 vID(I)=ncdf_vardef(ncid, struct(I).vname, dims, GZIP=9)
  ncdf_attput,ncid, vID(I),'long_name',struct(I).long
  ncdf_attput,ncid, vID(I),'units',struct(I).units
ENDFOR

;END define mode
ncdf_control, ncid, /endef

;Write data to defined variables
FOR I=0,nVars-1 DO BEGIN
 ncdf_varput, ncid, vID(I), struct(I).data
ENDFOR

;close netcdf file
ncdf_close,ncid

print,'CREATED: ',NCNAME

end
