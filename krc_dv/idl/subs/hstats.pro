function hstats, zz,  kode, prt=prt, lab=lab
;_Titl HSTATS Selectable statistics
; zz	in. Numerical array
; kode	in_ Strarr of items requested. Default is all
;		Valid are (case independant) 
    val=['M','S','I','X','MA','SA','IA','XA','N']
;   of values as input: N=number m=mean   S=StdDev  i=minimum,  x=maximum
;   for Absolute values, append an 'a'
; prt   in_ String or flag or integer; print formatted results
;        If a flag ('/prt') or positive int, or string whose first
;              character is not '-' print headings and values
;        If negative integer or first character is '-', with omit column headins
;        If a string, print this as ID at end of first line of values    
; lab	out_ Strarr of titles for the statistics
; func.	out. Fltarr of the requested values
;            If zz array has 0 items, StdDev will be -1.
;_Calls  MEAN_STD
;_Hist 2004jun22 Hugh Kieffer Original Version
; 2006jan24 HK Fix bug for length 1, and make robust for 0,1 or n items 
; 2006oct18 HK Add option 'N' 
; 2010apr24 HK Add keyword  prt
; 2019oct14 HK Revise print to be only first line if no negative values
; 2019oct21 HK Fix prt to display id, and option to omit column headings 
;_End

tit=['Mean','StdDev','Min','Max' $ ; max 7 chars
,'MeanAbs','StDvAbs','MinAbs','MaxAbs','Num']

siz=size(zz) & type=siz[siz[0]+1]
nz=siz[siz[0]+2]
if type ge 7 then message,'Input array must be numerical'

siz=size(prt,/type) ; type of prt

nneq=0
if nz lt 2 then begin ; message,'Require at least 2 values'
    if nz lt 1 then ff=[0.,-1.,0.,0.,   0.-1.,0.,0.,0] else $
      ff=[zz,0.,zz,zz,  zz,0.,zz,zz,1]
    if zz[0] lt 0 then nneg=1
endif else begin        ; have 2+ items, simplist to compute all valid statistics
    mout=MEAN_STD(zz,std=sout)
    iout=min(zz,max=xout)
    ii=where(zz lt 0,nneg)
    azz=abs(zz)
    amout=MEAN_STD(azz,std=asout)
    aiout=min(azz,max=axout)
    ff=[mout,sout,iout,xout,amout,asout,aiout,axout,nz]
endelse

if siz eq 7 then begin  ; keyword prt was set as an ID
  if strmid(prt,0,1) eq '-' then begin 
    q=strmid(prt,1) ; drop the leading '-'
    dop=0 ; no heading
  endif else begin
    q=prt
    dop=1
  endelse
endif else if siz gt 0 then begin ; prt was a flag or integer
  q=''                            ; null id
  dop=prt gt 0 ; headings only if positive
endif else begin ; no print
  q='' ; no id
  dop=0 ; no headings
endelse

if siz gt 0 then begin ; print values
  if dop then print,'     Number           Mean       StdDev      Minimum      Maximum'
    fmt='(a3,i8,2x,4g13.6,2x,a)'
    print,'',round(ff[8]),ff[0:3],'signed '+q,form=fmt
    if nneg gt 0 then print,'neg',nneg,ff[4:7],'absolute',form=fmt
endif

if n_params() gt 1 then begin ; kode was present
    nk=n_elements(kode) & out=fltarr(nk)
    lab=strarr(nk)
    kode=strupcase(kode)        ; convert all to upper-case
    for k=0,nk-1 do begin 
        i=where(val eq kode[k]) & i=i[0]
        if i lt 0 or i gt 8 then message,'Invalid kode element='+kode[k]
        out[k]=ff[i]
        lab[k]=tit[i]
    endfor
endif else begin 
    out=ff
    lab=tit
endelse
return,out
end
