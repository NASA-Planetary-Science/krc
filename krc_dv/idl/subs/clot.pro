PRO clot,yyy,txt,xx=xx,locc=locc,xran=xran,yran=yran,titl=titl,oplot=oplot $
,ssiz=ssiz,abso=abso,yr2=yr2,bw=bw,ksym=ksym,tsiz=tsiz, omit=omit, wait=wait $
, lack=lack,pgt=pgt,_extra=uex
;_Titl  CLOT  Color plot of related curves
; yyy   in. array(n,m=numCurves [,p=NumPlots])  Data to plot
; txt   in_ strarr(m)  Labels for curves. Default is 0-based count
; xx    in_[out] array (n)  Abcissa positions. Default is 0,1,2...
;        xx(n) is expected if lack is present. BEWARE: If xx is present 
;          and short, it will be replaced with findgen for the longest curve.
; locc  in_ fltarr(4)  Loc. for Guide in NORMAL units ala CURVEGUIDE.
;           fltarr(5)  [xloc,off,size,thick,over] for LABEL_CURVE  
;              If absent or less then 4 use [0.15,0.92,-0.03,0.06]
;              If neither txt or locc, then no legend or curve labels
; xran  in_ fltarr(2)  X plot limits:  Default is automatic. Ignored for oplot
; yran  in_ fltarr(2)  Y plot limits:  Default: full range. Ignored for oplot
;            If values are equal,uses full range of dataset
;            If [0] gt [1], normalizes each curve onto 0:1 
; titl  in_ strarr(3)  Lables for x,y axes and top. Ignored for oplot
; oplot in_ flag.      If set, overplots: ranges and titl ignored 
;                 Use +/- 0.1 for zero
;     line: |oplot|/100  0,+=fixed;  -=from common with this offset
;              if # bw >1 , then monochrome and cycle through bw
;   symbol: set by ksym, (- will omit any line)
;    color: from common with offset of |oplot| mod 100
;   legend: only if locc specified; at least x should be offset.
;            words; only if txt specified 
;   obsolete 2017aug09:
;   o                    Offset in SETCOLOR line index -=none
;   o                  Number of curves set by yyy
;   o   oplot modes:
;   o   -9 or less: more data; use same colors, no addition to legend
;   o   -0.1:6: alt. values. use line type -n mod 6, show within existing legend
;   o   +n: more curves, offset line index, add more items to legend
;   o   100+n: n=line style from common kkl and  new set of labels
;   o   200+n: line style = IDL standard: n mod 6 ; no new labels
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
;         if (2), [1] is character thickness for CURVEGUIDE. Default=2.
; omit  in_ Int    Omit every n'th line
; wait  in_ Float  Wait time in seconds; -=indefinite.  Ignored unless yyy 3D
;              -2 = STOP after each plot  -3= and ask for exit  Default=1.
; lack  in_ intarr(V) last index for each of the V curve-sets in 1-D yyy
; pgt   in  strarr(p) Top titles for pages of a movie, Ignored unless yyy is 3D
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
;  Find that this can cause failure if -extra is used but does not contain ylog
;    Problem is n_elements will abort on a structure tag that does not exist.
; 2018jul6 HK Resolve may22 problem; call tag_names to find if ylog present
; 2019jan07 HK Add keyword  pgt
; 2019may02 HK Change  tsiz  default from 0 to 2.
; 2019may09 HK Position of 'indicates was negative' depends upon tsiz
; 2019jun09 HK Fix Yrange when log and largest values are negative
; 2019jun25 HK Copy xx so that it cannot be altered.
; 2019sep01 HK Fix tiny bug; duplicate keyword in call to LABEL_CURVE 
; 2020feb23 HK Fix bug; when xx not provided, using lack and first is longest
common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,scex1
;_End                 .comp clot

if !dbug ge 8 then stop
; help,yyy,txt,xx,locc,xran,yran,titl,oplot,bw,ksym,oplot


dov=keyword_set(lack) ; variable number of items

sizy=size(yyy) & nu1y=sizy[1]  ; number of Y in each curve (if 2+ D array)
numy=sizy[sizy[0]+2]           ; n_elements yyy
sizx=size(xx) & numx=sizx[sizx[0]+2] ; sizx may be all 0's
if numx gt 0 then wx=xx ; make a working copy

dop = keyword_set(oplot) ; overplot Flag
; doy is flag for Ylog

if dop then doy=!y.type else begin      ; get from system variable
  doy=0B                  ; flag that we are doing ylog, default is off
  i=n_elements(uex)         ; are there any extra keywords?
  if i gt 0 then begin
    tn=tag_names(uex)             ; get all the tag names in _extra
    j=where(tn eq 'YLOG') & j=j[0] ; check if ylog is present
    if j ge 0 then doy=uex.ylog ne 0 ; set DOY true for any non-zero value
  endif
endelse

if dov then begin
  nyc=n_elements(lack)          ; number of curves
  nx=sizy[1]                    ; num X needed. redone inside Y loop
  nox= numx lt numy     ; full XX not provided
  if nox then begin             ; must find longest curve
    ii=lack-shift(lack,+1)      ; length of each curev, first is wrap
    ii[0]=lack[0]+1             ; repair wrap
    i=max(ii)                   ; greatest length
    wx=findgen(i)               ; X array to use
  endif
endif else begin                ; uniform sets
  nx=sizy[1]
  if sizy[0] ge 2 then nyc=sizy[2] else nyc=1
  if numx lt nx then xv=findgen(nx) else xv=wx[0:nx-1]; x values
endelse

dom = sizy[0] eq 3 ; Will do movie
if dom then begin 
  nump=sizy[3] 
  if not keyword_set(wait) then wait=1.
  topti='CLOT: page '+strtrim(1+indgen(nump),2) ; default page titles
  i=n_elements(pgt)                             ; look for page titles
  if i gt 0 then topti[0:((i<nump)-1)]=pgt      ; use titles input
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
  if j lt nyc then begin
    message,'Warning, fewer labels than curves',/con
    tut=[tut,strtrim(j+indgen(nyc-j),2)]
  endif
endif else begin
  if dop then tut=replicate ('',nyc)  $ ; text only if specified, else null
         else tut=strtrim(indgen(nyc),2) ; generate curve numbers
endelse

dol=kok ge 5                    ; plot a line
if dop then begin ; this is overplot
  j=abs(idop)
  loff=j/100                    ; line or its offset
  koff=j mod 100                ; color offset
  if oplot lt 0 then loff=-loff-1 ; -(offset+1) or + line
  dog = kok gt 0 ;  add to legend only if locc specified
  if kok lt 4 then loc2=0 else loc2=locc ; legend location
endif else begin ; new plot
  if kok lt 4 then loc2=[0.15,0.92,-0.03,0.06] else loc2=locc ; legend location
  dog=npa ge 2 or kok gt 0                                    ; do some Guide
endelse                    ; put label on each curve

if dov and dom then begin
  message,'both variable size and movie not allowed, Returning',/con
  return
 endif 

if sizy[0] lt 2 and not dop and not dov then begin 
   help,yyy,nx,nyc
   message,'Seems to be single curve',/con
   i=1 ; dummy
 endif

doa = keyword_set(abso)         ; plot absolute value
if not keyword_set(tsiz) then tsiz=2.
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
    if dov then xa=min(wx,max=xb) else xa=min(xv,max=xb)
    xran=[xa,xb]
 end

doa=doa or doy                        ; must check absolute value
don=0 ; set to not auto-scale each
if n_elements(yran) lt 2 then yran=[-1.,-1.]; Yran absent; set to auto-scale
yay=min(yyy,max=yby,/nan) & ya=yay & yb=yby                ; range of yyy
if yran[0] eq yran[1] then begin            ; equal, use full dataset range 
  if doa then ya=min(abs(yyy),max=yb)
endif else if yran[0] gt yran[1] then begin ; min>max, normalize onto 0,1
  don=1                                     ; set the normalization flag
  ya=0. & yb=1. 
endif else begin
  ya=yran[0] & yb=yran[1]       ; use the input values
endelse
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

sizz=size(ssiz) & dos = sizz[1] eq nx  ; individual symbol size
if dos then begin 
  ms2=sizz[2]
  if nyc mod sizz[2] ne 0 then Message,'Warning, symbol sizes not submultiple',/con
  if sizz[0] eq 3 then ms3=sizz[3]-1  else ms3=0 ; maximum 3rd dim to use 
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
; doi   add notification of triangle for negative
; dop   overplot
; dos   individual symbol size
; dov   variable number of items
; doy   doing ylog
; nox   dov true but inadequate xx provided

for jp=0,nump-1 do begin ; each plot of movie  vvvvvvvvvvvvvvvvv
  if dom then titl[2]=topti[jp]
  if not dop then plot,xran,xran,xran=xran,yran=yrn,/nodata $
    ,xtit=titl[0],ytit=titl[1],title=titl[2],xmargin=xmarg,_extra=uex

  if dod then axis,yaxis=1,yrange=yr2,ystyle=1,ticklen=-.02 ; right scale

  if doa and yay lt 0 then begin ; doing absolute values
    xa=.95-.1*tsiz ; want last char near edge of plot
    plots, xa,.021,psym=8,/norm,_extra=uex
;    plots, xa,.015+.006*tsiz,psym=8,/norm,_extra=uex,symsize=tsiz
    xyouts,xa,.015,'  indicates was negative',/norm  ,charsize=tsiz
  endif
  kd2=-1                        ; last of a set, used only if dov
  for k=0,nyc-1 do begin         ; each curve on this plot
    k2=koff+k                   ; set color index 
    if dov then begin           ; variable length curves
      if k eq 0 then kd1=0 else kd1=lack[k-1]+1 ; first index of this set
      kd2=lack[k]               ; last index of this set
      if kd2 ge kd1 then begin; normal, some data
        yy=yyy[kd1:kd2]       ; Y values for this curve
        if nox then xv=wx[0:kd2-kd1] else xv=wx[kd1:kd2]        ; X " " "
      endif else begin ; no data, dummy in replicate point
        yy=yyy[[kd1,kd1]]       ; replicate 
        if nox then xv=[0,0] else xv=wx[[kd1,kd1]]        ; X " " "
      endelse
    endif else begin
      yy=yyy[*,k,jp]            ; one curve
    endelse

    if dos then sv=(ssiz[*,k mod ms2,jp<ms3]> 0.1)<6 ; sizes of symbols
    if doa then begin           ; doing absolute values
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
        if omit eq 0 then oplot,xv,yy,line=lint,color=clr,_extra=uex $ ; one curve
        else for i=0,nx-omit,omit do $ ; omit every omit'th line
          plots,xv[i:i-1+omit],yy[i:i-1+omit],line=lint,color=clr,_extra=uex
      endif
      if ksya ne 0 then begin  ; add symbol
        if dos eq 0 then oplot,xv,yy,psym=ksya,color=clr,_extra=uex else $
          for i=0,nx-1 do plots,xv[i],yy[i],psym=ksya,color=clr,symsize=sv[i]
      endif
    if jn gt 0 then for i=0,jn-1 do plots,xv[ii],yy[ii],color=clr,psym=8,_extra=uex
      if dod then begin ; plot again on the right
        if ksym ge 0 then oplot,xd,yd,line=lint,color=clr,_extra=uex 
        if ksya ne 0 then  oplot,xd,yd,psym=ksya,color=clr,_extra=uex
    if jn gt 0 then for i=0,jn-1 do plots,xd[ii],yd[ii],color=clr,psym=8,_extra=uex
       endif
      if dog then CURVEGUIDE,k,gtex,lint,locc=loc2,color=clr,ksym=ksya $
                             ,charsize=csiz,charthick=thik
      if dol then LABEL_CURVE, gtex,xv,yy,loc2[0],off=loc2[1],size=loc2[2] $
                       ,thick=loc2[3],over=loc2[4],color=clr
    endif else begin ; ------------------- monochrome
      lint=((bw[k2 mod nlin])>0) mod 6 ; ensure valid line
      oplot,xv,yy,line=lint
      if ksya ne 0 then begin oplot,xv,yy,psym=ksya,_extra=uex ; add symbol 
        if dos eq 0 then oplot,xv,yy,psym=ksya,_extra=uex else $
          for i=0,nx-1 do plots,xv[i],yy[i],psym=ksya,symsize=sv[i]
      endif
      if jn gt 0 then for i=0,jn-1 do plots,xv[ii],yy[ii],psym=8,_extra=uex 
      if dog then CURVEGUIDE,k,gtex,lint,locc=loc2,ksym=ksya $
                             ,charsize=csiz,charthick=thik
      if dol then LABEL_CURVE,gtex,xv,yy,loc2[0],off=loc2[1],size=loc2[2] $
                              ,thick=loc2[3],over=loc2[4]
      if dod then begin         ; plot again on the right
        oplot,xd,yd,line=lint
        if ksya ne 0 then  oplot,xd,yd,psym=ksya,_extra=uex ; add symbol 
        if jn gt 0 then for i=0,jn-1 do plots,xd[ii],yd[ii],psym=8,_extra=uex 
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
