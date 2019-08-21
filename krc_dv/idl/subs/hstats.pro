function hstats, zz,  kode, prt=prt, lab=lab
;_Titl HSTATS Selectable statistics
; zz	in.	Numerical array
; kode	in_	Strarr of items requested. Default is all
;		Valid are (case independant) 
    val=['M','S','I','X','MA','SA','IA','XA','N']
;   of values as input: N=number m=mean   S=StdDev  i=minimum,  x=maximum
;   for of Absolute values, append an 'a'
; prt   in_     String If present, will print formatted table with title
; lab	out_	Strarr of titles for the statistics
; func.	out.	Fltarr of the requested values
;     f array has 0 items, StdDev will be -1.
;_Calls  MEAN_STD
;_Hist 2004jun22 Hugh Kieffer Original Version
; 2006jan24 HK Fix bug for length 1, and make robust for 0,1 or n items 
; 2006oct18 HK Add option 'N' 
; 2010apr24 HK Add keyword  prt
;_End

tit=['Mean','StdDev','Min','Max' $ ; max 7 chars
,'MeanAbs','StDvAbs','MinAbs','MaxAbs','Num']

siz=size(zz) & type=siz[siz[0]+1]
nz=siz[siz[0]+2]
if type ge 7 then message,'Input array must be numerical'

if nz lt 2 then begin ; message,'Require at least 2 values'
    if nz lt 1 then ff=[0.,-1.,0.,0.,   0.-1.,0.,0.,0] else $
      ff=[zz,0.,zz,zz,  zz,0.,zz,zz,1]
endif else begin        ; have 2+ items, simplist to compute all valid statistics
    mout=MEAN_STD(zz,std=sout)
    iout=min(zz,max=xout)
    azz=abs(zz)
    amout=MEAN_STD(azz,std=asout)
    aiout=min(azz,max=axout)
    ff=[mout,sout,iout,xout,amout,asout,aiout,axout,nz]
endelse

if keyword_set(prt) then begin 
    print,'                   Mean       StdDev      Minimum      Maximum'
;          1234567890    5.78381e-07  2.06981e-05 -9.15527e-05  9.15527e-05 
    print,prt,ff[0:3],'signed',form='(a10,2x,4g13.6,2x,a)'
    print,'N=',round(ff[8]),ff[4:7],'absolute',form='(a2,i8,2x,4g13.6,2x,a)'
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
