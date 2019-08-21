PRO clot,yyy,txt,xx=xx,locc=locc,xran=xran,yran=yran,titl=titl,oplot=oplot $
,ssiz=ssiz,abso=abso,yr2=yr2,bw=bw,ksym=ksym,tsiz=tsiz, omit=omit, wait=wait $
, lack=lack,_extra=e
;_Titl  CLOT  Color plot of related curves
; yyy   in. array(n,m=numCurves [,p=NumPlots])  Data to plot
; txt   in_ strarr(m)  Labels for curves. Default is 0-based count
; xx    in_[out] array (n)  Abcissa positions. Default is 0,1,2...
;        xx(n) is expected if lack is present. BEWARE: If xx is present 
;          and short, it will be replaced with findgen for the longest curve.
; locc  in_ fltarr(4)  Loc. for Guide in NORMAL units ala CURVEGUIDE.
;           fltarr(5)  [xloc,off,size,thick,over] for LABEL_CURVE  
;              If absent or less then 4 use [0.15,0.93,-0.03,0.06]
;              If neither txt or locc, then no legend or curve labels
; xran  in_ fltarr(2)  X plot limits:  Default is automatic. Ignored for oplot
; yran  in_ fltarr(2)  Y plot limits:  Default: full range. Ignored for oplot
;            If values are equal,uses full range of dataset
;            If [0] gt [1], normalizes each curve onto 0:1 
; titl  in_ strarr(3)  Lables for x,y axes and top. Ignored for oplot
; oplot in_ flag.      If set, overplots: ranges and titl ignored 

;   obsolete:
;                        Offset in SETCOLOR line index -=none
;                      Number of curves set by yyy
;       oplot modes:
;       -9 or less: more data; use same colors, no addition to legend
;       -0.1:6: alt. values. use line type -n mod 6, show within existing legend
;       +n: more curves, offset line index, add more items to legend
;       100+n: n=line style from common kkl and  new set of labels
;       200+n: line style = IDL standard: n mod 6 ; no new labels

; oplot as of 2017aug09  Use +/- 0.1 for zero
;     line: |oplot|/100  0,+=fixed;  -=from common with this offset
;              if # bw >1 , then monochrome and cycle through bw
;   symbol: set by ksym, (- will omit any line)
;    color: from common with offset of |oplot| mod 100
;   legend: only if locc specified; at least x should be offset.
;            words; only if txt specified 
; ssiz  in_ fltarr(n,m [[,p]] ) Size of each symbol (but not right 2nd plot)
;             2nd dimesion may be sub-multiple of m
;             If 3rd dimension absent, will apply to each plot 
; abso  in_ flag  If set, plots absolute value, adding symbol 8 where negative
; yr2   in_ fltarr(2)  Yrange for 2nd plot on right half in one call.
;                          Ignored if values are equal
; bw    in_ Intarr(m)  Line type. If m>1, does in black and white
;                     If scalar, uses this line type, Default is 0=solid
; ksym  in_ Int        Symbol to use as well as line. Will use abs-value
;                      If negative, will suppress the line
; tsiz  in_ Float or fltarr(2)  Character-size for curve guide/legend. 
;          if (2), [1] is character thickness for CURVEGUIDE. Default=IDL default
; omit  in_ Int    Omit every n'th line
; wait  in_ Float  Wait time in seconds; -=indefinite.  Ignored unless yyy 3D
;              -2 = STOP after each plot  -3= and ask for exit  Default=1.
; lack  in_ intarr(V) last index for each of the V curve-sets

; Responds to !dbug : ge 8: stop at entry.    ge 7: stop before return
;_Desc
; If in  V curve-sets mode, it is possible for some sets to have no data,
;    in which case routine dummys in 2 identical points
;_Calls  CURVEGUIDE  LABEL_CURVE  SCALELIN  ST0
;_Hist 2009dec08 Hugh Kieffer
; 2010mar16 HK Add _Extra keyword
 ;2010apr09-11 HK Enable overplot, add keyword ksym.  Done only for color code
; 2010jul14 HK Allow auto scaling of all curves
; 2010dec10 HK Fix bug that could overwrite input xx
; 2011dec31 HK Add oplot 100+n mode
; 2012jan18 HK Add keyword  tsiz
; 2012feb06 HK Add keyword abso
; 2013apr01 HK Make default X value Long so will handle >32767 properly
; 2013may12 HK Include _extra in  all data plot commands
; 2014feb03 HK Add keyword yr2
; 2015jun27 HK Add 3D movie capability
; 2015jul12 HK Add keyword omit
; 2015oct11 HK Apply charactersize to LABEL_CURVE also
; 2016ocd28 HK Add 200+n mode
; 2017jan17 HK Revise wait to be always available; allows caller do loops
; 2017jul30 HK Make legend automatic if txt present
; 2017aug10 HK Complete change of oplot value meaning
; 2017sep07 HK Fix line style for color
; 2017oct29 HK Add keyword  ssiz  for variable symbol sizes; optional line types
; 2017nov07 HK Add ability of different length sets
; 2017nov09 HK If txt is too few, internally add curve numbers with warning
; 2017nov10 HK Allow a single curve.
; 2018may22 HK If ylog, and any negative values, force absolute Y mode
;_Lien
; 2018may22 change can cause failure if -extra is used but does not contain ylog
;       Problen is n_elements will abort on a structure tag that does not exist.
;  Ugly solution, enter the following 3 commands:  doy=0   .skip   .con
;  Or, always define ylog, even if 0.
common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,scex1
;_End                 .comp clot

if !dbug ge 8 then stop
; help,yyy,txt,xx,locc,xran,yran,titl,oplot,bw,ksym,oplot

dov=keyword_set(lack) ; variable number of items

siz=size(yyy)

dop = keyword_set(oplot) ; overplot Flag

if dop then doy=!y.type else begin      ; get from system variable
  doy=0B                              ; flag that we are doing ylog
  i=n_elements(e)                     ; are there any extra keywords?
  if i gt 0 then doy=n_elements(e.ylog) ; yes: check if ylog was a keyword
  if doy gt 0 then doy=e.ylog ne 0      ; if yes, then set DOY true for any non-zero value
endelse

if dov then begin
  ny=n_elements(lack)
  nx=siz[1]                     ; redone inside Y loop
  nox= n_elements(xx) lt n_elements(yyy)     ; full XX not provided
  if nox then begin             ; must find longest curve
    ii=lack-shift(lack,+1)      ; length of each curev, first is wrap
    ii[0]=lack[0]               ; repair wrap
    i=max(ii)                   ; greatest length
    xx=findgen(i)               ; X array to use, overrides input
  endif
endif else begin                ; uniform sets
  nx=siz[1]
  if siz[0] ge 2 then ny=siz[2] else ny=1
  if n_elements(xx) lt nx then xv=findgen(nx) else xv=xx[0:nx-1]; x values
endelse

dom = siz[0] eq 3 ; Will do movie
if dom then begin 
  nump=siz[3] 
  if not keyword_set(wait) then wait=1.
endif else begin 
  nump=1                        ; number of plots
  if not keyword_set(wait) then wait=0.
endelse

kok=n_elements(locc)

nlin=n_elements(bw)
doc = nlin lt 2                 ; do color

if dop then idop=round(oplot) else idop=0 ; line index offset
loff=-1 & koff=0 ; line and color offsets
if nlin eq 1 then loff=bw ; optional line type

npa=n_params() ; will be 2 if txt is present
if npa ge 2 then begin; check size of txt
  tut=txt
  j=n_elements(tut)
  if j lt ny then begin
    message,'Warning, fewer labels than curves',/con
    tut=[tut,strtrim(j+indgen(ny-j),2)]
  endif
endif else begin
  if dop then tut=replicate ('',ny)  $ ; text only if specified, else null
         else tut=strtrim(indgen(ny),2) ; generate curve numbers
endelse

dol=kok ge 5                    ; plot a line
if dop then begin ; this is overplot
  j=abs(idop)
  loff=j/100                    ; line or its offset
  koff=j-100*loff              ; color offset
  if oplot lt 0 then loff=-loff-1 ; -(offset+1) or + line
  dog = kok gt 0 ;  add to legend only if locc ssecified
  if kok lt 4 then loc2=0 else loc2=locc ; legend location
endif else begin ; new plot
  if kok lt 4 then loc2=[0.15,0.93,-0.03,0.06] else loc2=locc ; legend location
  dog=npa ge 2 or kok gt 0                                    ; do some Guide
endelse                    ; put label on each curve

if dov and dom then begin
  message,'both variable size and movie not allowed, Returning',/con
  return
 endif 

if siz[0] lt 2 and not dop and not dov then begin 
   help,yyy,nx,ny
   message,'Seems to be single curve',/con
   i=1 ; dummy
 endif

doa = keyword_set(abso)         ; plot absolute value
if not keyword_set(tsiz) then tsiz=0 
csiz=tsiz[0] ; character size for legend
if n_elements(tsiz) lt 2 then thik=0 else thik=tsiz[1] ; character thickness

if not keyword_set(omit) then omit=0
if n_elements(yr2) lt 2 then yr2=[0,0]       ; dual plot, Y magnified on right
dod = yr2[0] ne yr2[1] ; ignore if values the same
;---
if dol then dog=0               ; if label on curves, do not to legend
if not keyword_set(ksym) then ksym=0 
ksya=abs(ksym)<8                  ; ensure not beyond psym valid range

if n_elements(xran) lt 2 then begin ; set X range
    if dov then xa=min(xx,max=xb) else xa=min(xv,max=xb)
    xran=[xa,xb]
 end

don=0 ; set to not auto-scale each
if n_elements(yran) lt 2 then yran=[-1.,-1.]; Yran absent; set to auto-scale
if yran[0] eq yran[1] then begin            ; equal, use full dataset rance 
   ya=min(yyy,max=yb,/nan)
   if ya gt 0 then doa=0B       ; there are no negative values
   if doa then ya=min(abs(yyy),max=yb,/nan)
endif else if yran[0] gt yran[1] then begin ; min>max, normalize onto 0,1
   don=1                                    ; set the normalization flag
   ya=0. & yb=1. 
endif else begin
  ya=yran[0] & yb=yran[1]       ; use the input values
endelse

if ya gt 0. then doy=0B else $        ; no need for sign reversal
  if doy then ya=min(abs(yyy))        ; reset yrange minimum
doa=doa or doy                        ; must do abs value
yrn=[ya,yb]                     ; yrange to use
;help,doy,ya,doa & print,yrn 

xmarg=[10,3]                    ; the default xmargin
if dod then begin               ; must double X plot range
   dx=(xb-xa)/(nx-1.)           ; delta X value
   xa2=xb+dx                    ; gap to right half
   xran[1]=xa2+(xb-xa)          ; new top of xrange
   xd=xv+(xa2-xa)               ; right side
   fff=SCALELIN(yr2,vv=yrn)
   xmarg=[10,8]
endif

if not keyword_set(titl) then titl=['Count','Constant scale','CLOT']
jn=0                            ; default is no extra symbol for negative data

siz=size(ssiz) & dos = siz[1] eq nx  ; individual symbol size
if dos then begin 
  ms2=siz[2]
  if ny mod siz[2] ne 0 then Message,'Warning, symbol sizes not submultiple',/con
  if siz[0] eq 3 then ms3=siz[3]-1  else ms3=0 ; maximum 3rd dim to use 
endif
; linn=[0,2,3,4,5,1]

; Summary of flags defined above
; doa   use absolute values
; doc   do color
; dod   right side magnified
; dog   add to legend
; dol   plot a line
; dom   do a movie
; don   normalize each curve
; dop   overplot
; dos   individual symbol size
; dov   variable number of items
; nox   dov true but inadequate xx provided

for jp=0,nump-1 do begin ; each plot of movie  vvvvvvvvvvvvvvvvv
  if dom then titl[2]='CLOT: page '+strtrim(jp,2)
  if not dop then plot,xran,xran,xran=xran,yran=yrn,/nodata $
    ,xtit=titl[0],ytit=titl[1],title=titl[2],xmargin=xmarg,_extra=e

  if dod then axis,yaxis=1,yrange=yr2,ystyle=1,ticklen=-.02 ; right scale

  if doa then begin ; doing absolute values
    plots,.82,.02,psym=8,/norm,_extra=e
    xyouts,.83,.015,' indicates was negative',/norm
  endif
  kd2=-1                        ; last of a set, used only if dov
  for k=0,ny-1 do begin         ; each curve on this plot
    k2=koff+k                   ; set color index 
    if dov then begin           ; variable length curves
      if k eq 0 then kd1=0 else kd1=lack[k-1]+1 ; first index of this set
      kd2=lack[k]               ; last index of this set
      if kd2 ge kd1 then begin; normal, some data
        yy=yyy[kd1:kd2]       ; Y values for this curve
        if nox then xv=xx[0:kd2-kd1] else xv=xx[kd1:kd2]        ; X " " "
      endif else begin ; no data, dummy in replicate point
        yy=yyy[[kd1,kd1]]       ; replicate 
        if nox then xv=[0,0] else xv=xx[[kd1,kd1]]        ; X " " "
      endelse
    endif else begin
      yy=yyy[*,k,jp]            ; one curve
    endelse

    if dos then sv=(ssiz[*,k mod ms2,jp<ms3]> 0.1)<6 ; sizes of symbols
    if doa or doy then begin           ; doing absolute values
      ii=where(yy lt 0.,jn)     ; all negative points
      yy=abs(yy)
    endif
    if idop ge 1 or not dop then gtex=tut[k] else gtex='' 

    if dod then yd=(yy-fff[0])*fff[1] ;  re-scale for right plot
 
    if don then begin ; auto-scale each curve
      ya=min(yy,max=yb)
      if ya ne yb then yy= (yy-ya)/(yb-ya) ; scale onto [0,1]
      gtex=gtex+ST0([ya,yb])               ; range as text
    endif

    if doc then begin ; ----------------- doing color
      clr=kkc[k2 mod kcc[2]]
      if loff ge 0 then lint=loff else lint=kkl[k2 mod kcc[3]]
      j=lint                     ; for use by  CURVEGUIDE
      if ksym ge 0 then begin ; plot data line
; print,k,k2,clr,lint
        if omit eq 0 then oplot,xv,yy,line=lint,color=clr,_extra=e $ ; one curve
        else for i=0,nx-omit,omit do $ ; omit every omit'th line
          plots,xv[i:i-1+omit],yy[i:i-1+omit],line=lint,color=clr,_extra=e
      endif
      if ksya ne 0 then begin  ; add symbol
        if dos eq 0 then oplot,xv,yy,psym=ksya,color=clr,_extra=e else $
          for i=0,nx-1 do plots,xv[i],yy[i],psym=ksya,color=clr,symsize=sv[i]
      endif
    if jn gt 0 then for i=0,jn-1 do plots,xv[ii],yy[ii],color=clr,psym=8,_extra=e
      if dod then begin ; plot again on the right
        if ksym ge 0 then oplot,xd,yd,line=lint,color=clr,_extra=e 
        if ksya ne 0 then  oplot,xd,yd,psym=ksya,color=clr,_extra=e
    if jn gt 0 then for i=0,jn-1 do plots,xd[ii],yd[ii],color=clr,psym=8,_extra=e
       endif
      if dog then CURVEGUIDE,k,gtex,lint,locc=loc2,color=clr,ksym=ksya $
                             ,charsize=csiz,charthick=thik
      if dol then LABEL_CURVE, gtex,xv,yy,loc2[0],off=loc2[1],size=loc2[2] $
                       ,thick=loc2[3],over=loc2[4],color=clr,size=csiz
    endif else begin ; ------------------- monochrome
      lint=((bw[k2 mod nlin])>0) mod 6 ; ensure valid line
      oplot,xv,yy,line=lint
      if ksya ne 0 then begin oplot,xv,yy,psym=ksya,_extra=e ; add symbol 
        if dos eq 0 then oplot,xv,yy,psym=ksya,_extra=e else $
          for i=0,nx-1 do plots,xv[i],yy[i],psym=ksya,symsize=sv[i]
      endif
      if jn gt 0 then for i=0,jn-1 do plots,xv[ii],yy[ii],psym=8,_extra=e 
      if dog then CURVEGUIDE,k,gtex,lint,locc=loc2,ksym=ksya $
                             ,charsize=csiz,charthick=thik
      if dol then LABEL_CURVE,gtex,xv,yy,loc2[0],off=loc2[1],size=loc2[2] $
                              ,thick=loc2[3],over=loc2[4]
      if dod then begin         ; plot again on the right
        oplot,xd,yd,line=lint
        if ksya ne 0 then  oplot,xd,yd,psym=ksya,_extra=e ; add symbol 
        if jn gt 0 then for i=0,jn-1 do plots,xd[ii],yd[ii],psym=8,_extra=e 
        if dol then LABEL_CURVE,gtex,xd,yd,loc2[0],off=loc2[1],size=loc2[2] $
                                ,thick=loc2[3],over=loc2[4]
      endif
    endelse          ; -------------------------
  endfor

  if wait gt 0 then wait,wait else if wait lt 0 then begin 
    if wait le -2 then STOP else begin  
      print,'Any key to go'     ; wait for user
      i=get_kbrd(1)
    endelse
  endif ; if wait is 0, does nothing

endfor                          ; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

if !dbug ge 7 then stop
; help,yyy,txt,xv,locc,xran,yran,titl,oplot,bw,ksym,oplot
; help, dom,nump,npa,idop,dop,koff,dog,loc2,ksy,k2,lint,j

return
end
