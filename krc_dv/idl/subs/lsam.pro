function lsam, arg, myn, aud, rev=rev,pbs=pbs
;_Titl  LSAM  Convert Julian day to Mars season L_s and back. Allison and McEwen
; arg   in.  May be scaler or array[N]
;              Julian day OR offset from J2000, UTC. Switches at 1,000,000.
;            If /rev, then  L_s aerographic longitude of the sun, in degrees. 
;                  should be within 0 to 360.  May be scaler or array
; myn   both  Integ.  Modern Mars Year number (27 starts 2004mar06)
;             Input if /rev is set;  else is output   
; aud   out_ fltarr(N,4)  [Heliocentric AU, sub-solar latitude(ocentric,deg),
;                            Equation of time(deg), obliquity(deg)] 
; rev   in_  Flag or fltarr(5) Convert Ls and MY to date. ERROR +.03:.16  day
;             This is the primary relation given by A&M.
;            If n_elem ge 5 these are coeffs of empirical correction for closure
;            else, if rev[0] ge 5 then uses the default coefficients below
;                                 else, no correction
; func. out. flt [N]    Aerographic longitude of the sun, in degrees.
;               If rev set: days from J2000.0
;_Desc 
; Ref1: M. Allison and M. McEwen.' A post-Pathfinder evaluation of areocentric 
; solar coordinates with improved timing recipes for Mars seasonal/diurnal 
; climate studies'. Plan. Space Sci., 2000, v=48, pages = {215-235},
; Ref2:  http://www.giss.nasa.gov/tools/mars24/help/algorithm.html
;
; Ref1 counts Mars years from 1874, which are 42 MY larger than current
; practice amoung planetary scientists (0=1953may24)
; Ref1 MJD          is days from 2400000.5 (p 221.5b), here called dj4
;      DeltaT_J2000 is TT days from j2000.0=2451545.0, here called djm
;_Hist 2012apr08 Hugh Kieffer Derived from: Ref1 and Ref2
; 2012apr10 HK Add option for REV to include a correction; reduces MAR 
; of closure from 0.0956 to 0.0028 days
;rec=[0.09571092 ,0.0034577459,0.0040696292,0.029028188,-0.00040623528]
; above for MJD -10000:+11000, below for -5500:+9490 ~1985.0 to 2026.0
rec=[-0.0043336633,0.0043630568,0.0039601861,0.029025116,-0.00042300026]
; Correction developed in qlsam.pro @22 to 25
; 2013jul12 HK  Return  aud  as [N,4]
; 2-14oct29 HK Add keyword PBS
;_End                 .comp lsam

dj2000=2451545.D0               ; JD of epoch J2000
dj4m=51544.5D0                  ; days offset from dj4 to djm
smja=1.523679             ; semi-major axis in AU, Table 2. last digit from Ref2
meanmo=0.52402075D0             ; mean motion, degrees/day. Constant in Table 2
; Perturbations from the other planets. Table 5 A
; digits for A at Ref2
A5=[71,57,39,37,21,20,18]*.0001  ; 
tau=[2.2353,2.7543,1.1177,15.7866,2.1354,2.4694,32.8493]
phi=[49.409,168.173,191.837,21.736,15.704,95.528,49.095]
; for i=0,6 do print,i,a5[i],tau[i],phi[i],form='(i1,f8.3,f8.4,f8.3)'

dor = keyword_set(rev) ; do reverse
if dor then begin  ; input is Ls and My
    lsub=arg                    ; Ls in degrees
    dels=lsub-250.99864         ; delta Ls used in Eq 14.  251.0
    rels=!dtor*dels             ; " in radians
    dj4=51507.5+1.90826*dels -20.42*sin(rels)+0.72*sin(2.*rels) ; Eq. 14
    brak=686.9726+0.0043*cos(rels)-0.0003*cos(2.*rels) ; in brackets in Eq 14
    ny=myn+42                   ; orbits of mars since 1874.0
    dj4=dj4+brak*(ny-66)        ; last part of Eq. 14
    djtt=dj4-dj4m               ; days from J2000 TT
    tcen=djtt/36525.            ; julian centuries from J2000
    tcor=64.184 + Tcen*(59. +tcen*(- 51.2+tcen*(-67.1-tcen*16.4))); TT-UTC
    djm=djtt-tcor/86400.        ; convert correction to days and apply
    out=djm                     ;  out is UTC
endif else begin ; input was days from j2000 or full JD
    if arg[0] lt 1.e6 then djm=arg else djm=arg-dj2000 ; in UTC
    tcen=djm/36525.             ; julian centuries from J2000
    tcor=64.184 + Tcen*(59. +tcen*(- 51.2+tcen*(-67.1-tcen*16.4))); TT-UTC
    djtt=djm+tcor/86400.         ; convert correction to days and apply. get TT
    dj4=djtt+dj4m                ; A+M's MJD
endelse

; things that require djtt
nin=n_elements(arg)
pbs=dblarr(nin)
for i=0,nin-1 do begin 
    q=0.985626*djtt[i]           ; first term within cos in Eq 18
    pbsi= a5*cos(!dtor*(q/tau + phi))   ; summed in Eq 18
    pbs[i]=total(pbsi)          ; eq. 18
endfor
manom=19.3870D0+meanmo*djtt ; Mean anomoly M, Table 2 and Eq. 16
mrad=!dtor*manom                ; M in radians

; eoc is true anomaly - mean anomaly; in degrees. Analytic form for small ecc.
eoc=(10.691+3.e-7*djtt)*sin(mrad) $ ; equation of center: in brackets in eq 20
  + 0.623*sin(2.*mrad) +0.050 *sin(3.*mrad) $ ; and all but first term
  + 0.005*sin(4.*mrad) +0.0005*sin(5.*mrad)  ; in Eq. 19, exclude PBS

if dor then begin               ; apply my refinements to Eq. 14
    j=n_elements(rev)
    if j ge 5 then rec=rev ; use input coeffs
    if j lt 5 and rev[0] lt 5 then fd=0. else  fd=rec[0] +rec[1]*sin(rels) $
          +rec[2]*sin(2.*rels) +rec[3]*sin(3.*rels) +rec[4]*sin(4*rels) 
    out=out -(pbs/meanmo +fd)
endif else begin                ; -------------- calc Ls and MY
    afms=270.3863+ 0.52403840D0*djtt ; Eq. 17
    lsub=afms+ eoc+pbs              ; Eq. 19 LS in degrees
    lsub=ZERO360(lsub)
    out=lsub
    marstyear=686.971          ; mean mars tropical year in terrestrial days
;  marsysol=668.5991  mars siderial year in sols
    ny=floor((dj4-5668.690)/marstyear) ; full Mars years from 1874
    myn=ny-42                   ; climate MY
endelse

; need djtt, manom, lsub
if arg_present(aud) then begin  ; extra items
    ecc=0.09340+2.477e-9*djtt    ; eccentricity, Table 2
    e2=ecc^2                    ; powers of eccentricity
    fe2=e2/2.                   ; 1/2 e^2
    fe3=3.*ecc*e2/8.            ; 3/8 e^3
    fe4=e2*e2/3.                ; 1/3 e^4
    roa=1.+fe2-(ecc-fe3)*cos(mrad) $ ; R/a. Eq 10
    -(fe2-fe4)*cos(2.*mrad) -fe3*cos(3.*mrad) -fe4*cos(4.*mrad) 
    lrad=!dtor*lsub             ; L_s in radians
    eot=2.861*sin (2.*lrad) $   ; Eq 20 Equation of Time in degrees
      -.071*sin(4.*lrad) +.002*sin(6.*lrad)-eoc 
    obli=25.1919+3.45E-7*djtt    ; obliquity  Abstract and table 4
    sdec=obli*sin(lrad)         ; sub-solar declination
    aud=[smja*roa,sdec,eot,obli]
    if nin gt 1 then aud=reform(aud,nin,4)
endif
if !dbug ge 4 then stop
return,out

end

; dj4 51549=2000jan6 00:00 UTC ; so here DJ4 is JD=2400000.5
; 5668.690 ; DJ4 of 1874 vernal equinox
; 2453070.2 ; start of consensus MY 27, 69 MY after 1874
; print,(2453070.2-(2400000.5+5668.690))/marsyear  68.9998

;djh=  -19.3870D0/meanmo; dj of peri from Eq 16
; lsp=270.3863+ 0.52403840D0*djh ; Ls at perihelion ignoring pbs. Eq 19 and 17 
;     250.99864
