;+
; NAME: ssg_ana_mag
;
; PURPOSE: Put magnetic information into ssg database
;
; CATEGORY: ssg analysis
;
; CALLING SEQUENCE: ssg_ana_mag
;
; DESCRIPTION:
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:  
;   Common blocks are ugly.  Consider using package-specific system
;   variables.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
; $Id: ssg_ana_mag.pro,v 1.1 2015/02/17 23:01:04 jpmorgen Exp $
;
; $Log: ssg_ana_mag.pro,v $
; Revision 1.1  2015/02/17 23:01:04  jpmorgen
; Initial revision
;
;-

;; Grab Carey's maginfo verbatim to do magnetic field
;; calculations --> I don't know how valid this is for non-Io
;; satellites, pardicularly , but it can always be updated
pro maginfo,sys3l,lshell=l,bvector=b,bmag=bmag,latitude=eqplat,quiet=qp,$
    blatitude=blat,dice=dice,$
    dpoff=dpoff,dptilt=dptilt,dpmnt=dpmnt,iorad=ior,oldmagparams=oldmp
;
; Computes various bits of Jovian-magnetic-field-related information
; at Io's position, given its System III longitude. --RCWj 2/98
;
; Required input: System III longitude(s) of Io (scalar or vector)
;
; Input keywords (all optional):
;   DPTILT	2 elements: dipole tilt and longitude of NP tilt direction
;   DPOFF	3 elements: offset of dipole center from Jovian center
;   DPMNT	Dipole moment (nT * Rj^3)
;   IORAD	Io orbital radius (assumed constant and spin-equatorial)
;   QUIET	If set (or if input is a vector), results are not printed
;   OLDMAGPARAMS If set, don't recalculate purely magnetic-model-related
;               parameters. (Must leave clear on first call.)
;
; Output keywords (all optional):
;   BMAG	Magnitude of magnetic field (nT)
;   BVECTOR	Vector magnetic field (nT)
;   LSHELL	L-shell (greatest distance of field line from dipole center)
;   LATITUDE	Iocentric latitude of point on prime meridian where magnetic
;               field is tangent to surface (assumed spherical with 0 radius)
;   BLATITUDE	Magnetic latitude of Io
;   DICE	Distance from Io to the Centrifugal Equator
;		~             ~         ~           ~
; All angles are in degrees; all distances are in jovian radii; CS of DPOFF and
; BVECTOR has +z along spin north pole, +x along System III longitude 0.
;
common magparams,dpt,dpo,dpm,dprot
on_error,2

if not keyword_set(oldmp) then begin
  if n_elements(dpoff) eq 3 then dpo = dpoff else dpo = [-0.085,-0.051,-0.010] 
  if n_elements(dptilt) eq 2 then dpt = dptilt else dpt = [9.6,202.]
  if n_elements(dpmnt) eq 1 then dpm = dpmnt else dpm = 4.23e5
  cdpt = cos(dpt(0)/!radeg) & sdpt =  sin(dpt(0)/!radeg)
  cdtl = cos(dpt(1)/!radeg) & sdtl = -sin(dpt(1)/!radeg)
  dprot = fltarr(3,3)
  dprot(0,0) = cdtl * cdpt & dprot(1,0) = sdtl * cdpt & dprot(2,0) =-sdpt
  dprot(0,1) =-sdtl        & dprot(1,1) = cdtl
  dprot(0,2) = cdtl * sdpt & dprot(1,2) = sdtl * sdpt & dprot(2,2) = cdpt
endif

ns3l = n_elements(sys3l)
if ns3l lt 1 then message,'System III longitude(s) missing.'
if ns3l gt 1 then qp = 1
if n_elements(ior) ne 1 then ior = 5.91
cs3l = cos(sys3l /!radeg) & ss3l = -sin(sys3l /!radeg)
x1 = ior*cs3l - dpo(0)
y1 = ior*ss3l - dpo(1)
z1 = -dpo(2)
x2 = x1*dprot(0,0) + y1*dprot(1,0) + z1*dprot(2,0)
y2 = x1*dprot(0,1) + y1*dprot(1,1) + z1*dprot(2,1)
z2 = x1*dprot(0,2) + y1*dprot(1,2) + z1*dprot(2,2)
rad = sqrt(x2*x2+y2*y2+z2*z2)
bcom = 3. * dpm  / (rad^5>1.e-5)
bbx = bcom * x2*z2 
bby = bcom * y2*z2 
bbz = bcom *(z2*z2 - rad*rad/3.)
l = rad^3 / (x2*x2 + y2*y2)
bx = bbx*dprot(0,0) + bby*dprot(0,1) + bbz*dprot(0,2)
by = bbx*dprot(1,0) + bby*dprot(1,1) + bbz*dprot(1,2)
bz = bbx*dprot(2,0) + bby*dprot(2,1) + bbz*dprot(2,2)
bmag = sqrt(bx*bx+by*by+bz*bz) & b = transpose([[bx],[by],[bz]])
eqplat = !radeg * atan((ss3l*by+cs3l*bx)/bz)
blat = !radeg * asin(z2/rad)
tanb = z2 / sqrt(rad*rad - z2*z2)
tanax2 = (3. - sqrt(9.-8.*tanb*tanb)) / 2. / tanb
dice = rad * tanax2 / sqrt(1.+tanax2*tanax2)

if keyword_set(qp) then return
print,fix(bmag),fix(bx),fix(by),fix(bz),$
  format="('Magnetic field is',I5,' nT  (',I4,',',I4,',',I5,')')"
print,l,eqplat,format="('L-shell is',F5.2,'; equipotential Iocentric"+$
                       " latitude is',F6.1)"
print,blat,dice,format="('Magnetic latitude is',F6.2,'; distance to"+$
                       " centrifugal equator is',F6.2)"
end


pro ssg_ana_mag, $
   begin_nday=begin_nday, $
   end_nday=end_nday

  init = {ssg_sysvar}

  ;; The begin_nday and end_nday should match the ssg_phi_req values
  ;; (if any)
  if N_elements(begin_nday) eq 0 then $
     begin_nday = 0d
  ;; Hopefully this is sufficiently beyond the end of the MMP lifetime
  if N_elements(end_nday) eq 0 then $
     end_nday = 1d9

  ;; Just in case
  dbclose
  ;; Open our database in writable mode
  adbname = 'io_oi_analyze'
  oldpriv=!priv
  !priv = 2
  dbopen, adbname, 1

  ;; Get entries for all objects in our nday range
  entries = dbfind(string("nday>", begin_nday), $
                   dbfind(string("nday<", end_nday)), count=ecount)

  if ecount eq 0 then begin
     message, /CONTINUE, 'WARNING: no objects in reduction database between ' + string(nday_range[0]) + ' and ' + string(nday_range[1])
     dbclose
     return
  endif

  dbext, entries, 'nday, obj_code, long_3', $
         ndays, obj_codes, long_3s

  ;; Now populate the columns for each object in the database.
  ;; Columns exist for all ndays so as to make cross-correlations
  ;; easy.

  ;; Put together some mnemonics to make accessing the columns as a
  ;; function of object easy
  obj_names = ['Jup', 'Io', 'Eur', 'Gan', 'Cal']

  ;; Prepare the lat_B, d_CE, b_field, lshell, and lat_tan_B columns.
  ;; These correspond to the moon that is actually being observed (if
  ;; any).  Initialize to NAN and fill in below.  Note that Jupiter
  ;; values will end up being NAN
  lat_B = make_array(N_elements(long_3s), value=!values.f_nan)
  d_CE = lat_B
  b_field = lat_B
  lshell  = lat_B
  lat_tan_B=lat_B

  ;; Loop through each set of columns
  Rj = 69911d ;; km
  for ig=1,4 do begin
     ;; Set io_rad, the radius of the moon to calculate the magnetic
     ;; field parameters.  Note that these are semi-major axes.  Carey
     ;; used a slightly smaller radius than I see for any Io perijove.
     case ig of
        1: iorad = 421.6E3 / Rj ;; Io
        2: iorad = 670.9E3 / Rj ;; Europa
        3: iorad = 1070.4E3 / Rj;; Ganymede
        4: iorad = 1882.7E3 / Rj;; Callisto
     endcase
     maginfo, long_3s, $
              iorad=iorad, $
              blatitude=m_lat_B, $
              dice=m_d_CE, $
              lshell=m_lshell, $
              bmag=m_b_field, $
              latitude=m_lat_tan_B

     ;; Find all the observations of the Galilean satellite we are
     ;; currently considering
     g_idx = where(obj_codes eq ig, count)
     ;; If any observations are found, copy the columns to our generic handles
     if count gt 0 then begin
        lat_B[g_idx]     = m_lat_B[g_idx]
        d_CE[g_idx]   = m_d_CE[g_idx]
        lshell[g_idx]    = m_lshell[g_idx]
        b_field[g_idx]   = m_b_field[g_idx]
        lat_tan_B[g_idx] = m_lat_tan_B[g_idx]
     endif

     ;; Write moon-specific phase arrays into the database
     dbupdate, entries, obj_names[ig]+'_lat_B, ' + obj_names[ig]+'_d_CE, ' + $
               obj_names[ig]+'_lshell, ' + obj_names[ig]+'_b_field, ' + obj_names[ig]+'_lat_tan_B', $
               float(m_lat_B), float(m_d_CE), float(m_lshell), float(m_b_field), float(m_lat_tan_B)

  endfor ;; Each set of satellite columns
  
  ;; Now write the generic handles we have accumulated
  dbupdate, entries, 'lat_B, d_CE, lshell, b_field, lat_tan_B', $
            float(lat_B), float(d_CE), float(lshell), float(b_field), float(lat_tan_B)

  message, /INFORMATIONAL, 'Updated magnetic info in ' + adbname

  dbclose
  !priv=oldpriv


end
