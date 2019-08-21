function krclayer, fd,id,ld, ggg, year=year,v2=v2, flab=flab, glab=glab
;_Titl  KRCLAYER  Compute and print KRC layer depth table from KRCCOM values
; fd     in.  KRCCOM floats
; id     in.  KRCCOM integers
; ld     in.  KRCCOM logicals
; ggg   out.  fltarr(n1,2) diffusivity and convergence factor (no time doubling)
; year   in_  flt  Length of a year in days (not sols)
; v2     in_  flag. Old version: Treat FLAY as virtual layer
; flab  out_  strarr(7)  Short labels for columns of function
; glab  out_  strarr(2)  Short labels for columns of ggg
; func. out. fltarr (N1,7) of:  0=thickness in local scale  1=thickness in m
; 2=Center depth in Top scale   3=Center depth in m   4=Mass above layer center 
; 5=Mass above layer bottom     6=Depth to bottom in local scale
;_Desc
; stability requirements. delt_t < blay^2/(2*diffu)
; stability factor: blay^2/(2*delt_t*diffi)
;_Calls  none
;_Hist 2008apr13 Hugh Kieffer Convert Fortran from tday.f and tdisk.f
; 2014feb13 HK Omit obsolete layer indices. Add total mass above bottom of layer
; 2014may02 HK Add ggg output
; 2016jan12 HK Add year input. Adopt to v3.2.3 definition of flay
; 2016mar05 HK Output of same type as input. E.g. , double for KRC version 3+
;   and lowercase all code but called routines
;_End      .comp krclayer

if not keyword_set(year) then year=686.9929  ; default is Mars

flab=['Thick,scaled','Thick,m','Center,scaled','center,m' $
,'MassAboveCen','MassAboveBot','BotDeptLocal']

glab=['diffusivity','convergence']

; indices here are 1 less than in KRC helplist
n1    =id[0]
n2    =id[1]
ic2   =id[7]
n24   =id[5]
skrc  =fd[2]
cond2 =fd[3]
dens2 =fd[4]
period=fd[5]
spht  =fd[6]
dens  =fd[7]
spht2 =fd[15]
rlay  =fd[32]
flay  =fd[33]
convf =fd[34]
local =ld[14]

cond=skrc*skrc/(dens*spht)      ; surface conductivity
type=size(cond,/type) 
if type le 4 then zero=0. else zero=0.d0

ti2=sqrt(cond2*dens2*spht2)     ; lower thermal inertia
persec = period * 86400.        ; get solar period in seconds
;dtim=persec/n2                  ; size of one time step
diffu=cond /(dens*spht)         ; surface diffusivity
diff2=cond2/(dens2*spht2)       ; lower diffusivity
scal1=sqrt(diffu*persec/!pi)    ; surficial diurnal skin depth
scal2=sqrt(diff2*persec/!pi)    ; lower diurnal skin depth
;help,n1,n2,ic2,local,skrc,cond2,dens2,period,spht,dens,spht2,rlay,flay,convf

diffi=replicate(diffu,n1) ; upper diffusivity
if ic2 gt 1 and ic2 lt n1 then diffi[ic2-1:*]=diff2 ; lower

if keyword_set(v2) then vlay=flay else vlay=flay/rlay ; virtual layer
yy=vlay*rlay^indgen(n1) ; factors in layer thickness

if local then blay=yy*sqrt (diffi   *persec/!pi )  $ ; layer thickness
         else blay=yy*sqrt (diffi[0]*persec/!pi )
x=make_array(n1, type=type)     ; to hold depths
x[0]=-blay[0]/2.                ; x is depth to layer center, [cm]
for i=1,n1-1 do x[i]= x[i-1]+ (blay[i]+blay[i-1])/2.

yrfac=sqrt(year/period)         ; factor: annual/diurnal skin-depth
; print layer depth table
q2=dens*spht
fmt1='('' Conductiv.='',E10.3,''  Dens*Cp='',E10.3,''  Diffu.='',E10.3,''  Scale='',E10.3)'
fmt2='(''Beginning at layer  '',i2,''  At  '',f8.4,'' m.   Inertia=  '',f8.1)'
print,cond,q2,diffu,scal1, form=fmt1
if ic2 gt 2  and ic2 lt n1-1 then begin 
    q2=dens2*spht2
    q6=x[ic2-2]+blay[ic2-2]/2.    ; depth to top of 2nd material
    print,ic2,q6,ti2,form=fmt2    ; IC2 announcement
    print,cond2,q2,diff2,scal2, form=fmt1 ; lower layer properties
  ENDIF
print,'      ___THICKNESS____    _____CENTER_DEPTH________   ____BOTTOM______ Annual'
print,'LAYER LocScale   meter  TopScale    meter    kg/m^2    kg/m^2 LocScale   Local'
print,'out:      0        1        2        3         4         5       6'
fmt='(I4,F9.4,F9.4,g10.4,f9.4,2G10.3,f9.2,f7.2)'
q6=zero                         ; mass above center of layer
rhop=zero                       ; density of prior layer                       
rho=zero                        ; density of current layer
scale=scal1                     ; top layer properties
sumd=zero                       ; thermal scales above bottom of layer
burd=zero                       ; mass burden above bottom of the layer
out=fltarr(n1,7)                ; array to be returned
for i=0,n1-1 do begin
    if i eq 1 then rho=dens
    if i eq ic2-1 then begin ; start lower material
        rho=dens2
        scale=scal2
    endif
    q2 = blay[i]/scale          ; thickness in units of local scale
    q4 = x[i]/scal1             ; center-depth in units of surface scale
    if i ge 1 then begin 
        q6=q6+0.5*(blay[i-1]*rhop+blay[i]*rho) ; layer-center columnar mass
        sumd=sumd+q2
        burd=burd+blay[i]*rho
    endif
    print,i+1,q2,blay[i],q4,x[i],q6,burd,sumd,sumd/yrfac,form=fmt
    rhop=rho                    ; will be density of prior layer
    out[i,*]=[q2,blay[i],q4,x[i],q6,burd,sumd]
endfor
;        print,'Bottom layers for time doubling:  ',N1K[0:kkk], form='(a,10I5)'

ggg=reform([diffi,blay^2/((2.*persec/n2)*diffi)],n1,2.,/over)

; Compute the layer indices used for Type 52; set by   tdisk.f
Print,' Type 52 storage: layers 2 to maximum of',n24-2

if !dbug then stop
return,out
end
