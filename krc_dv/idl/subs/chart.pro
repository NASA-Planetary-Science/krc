PRO chart, yyy, title=title,parti=parti,xtit=xtit,range=range,dlin=dlin,psy=psy $
,clip=clip,fmt=fmt,evod=evod,xin=xin,marg=marg,lax=lax, kim=kim, tloc=tloc $
,csize=csize,cthick=cthick,cclr=cclr,oplot=oplot,clr=clr, rangu=rangu
;_Titl  CHART  Strip-chart plot of several variables
; yyy	in.	array(N_data,M_parameters)
; titl	in_	string: Text title for plot
; parti	in_	strarr(M): Text title for each strip
; xtit	in_	Title for abcissa, Default='Count'
; range	in_	Plot range for each variable, flt[2,1 or m]
; 		 If fewer than m pairs provided, uses last for rest
;		  so a single pair will be used for all.
;                If supplied with oplot, shoule be identical to first call
; dlin	in_	Integer: Line style for strip separation. use -1 for solid
;			Default is none.
; psy   in_     Integer plot symbol, default is line unless evod is set.
; clip	in_	Only used if range present; If set, constrains plot to +N strips
; fmt	in_	Format for range printout, ignored if a single range used.
;		  Default is 'g12.5'  
; evod	in_	Intarr(6) Symbols or lines and colors for even, odd, both points
;		[0=even symbol/line, following the PSYMLINE convention,-9=none
;		[  1=even color as % of maximum
;		[2,3  same for odd points
;		[4,5  same for line of both points.
; xin	in_	array(N) of X-axis values. Default is count.
; marg  in_     Fractional margin for x axis. Default=.005
; lax   in_     Fraction X position of left end of strip labels, Default=
; kim   in_     intarr(M) Number of item  to plot for each paramter 
; tloc  in_     Flt or fltarr. Y proportional location of label for each strip. 
;                Default=0.4  May have 1 or M values.
; csize	in_	Charsize for each strip
; cthick in_	Charthick for each strip
; cclr  in_     Color for strip labels
; oplot	in_	Integer, If set, will over-plot set of lines with this line_style
; 		  auto-range will be that of new data, which will be displayed
; 		  The following ignored: parti,xtit,dlin
; clr   in_     Integer, line Color [not the index of !binc ]
; rangu out_    Fltarr(2,M)  Range used for each strip. Use as range for oplot
;_Desc
; allocates 1/m of Y-space for each parameter
;_Hist  1999apr28 Hugh Kieffer
; 1999aug30 HHK adjust title location
; 2000feb10 HHK add options for:  xtit, range, clip, fmt
; 2001jan07 HHK Accomodate NAN's
; 2001feb19 HHK include  evod  option
; 2001apr18 HHK add  xin  option, force Y calculations to floating
; 2001may27 HHK Avoid plotting at precision roundoff limit
; 2001jau16 HK  Add  csize  and  cthick  keywords
; 2002mar13 HK  Align Range after panel titles
; 2004may26 HK  Use magnitude of clip. Minor changes in part titles
; 2004Oct20 HK  Add  dlin  option
; 2005jan23 HK  Add  overplot  option. Minimal coding changes
; 2008dec12 HK  Add  psy  option
; 2009dec03 HK  Fix bug that plotted NAN's when clipped
; 2010apr15 HK  Add  kim   keyword. (for histograms of different length)
;                  may not work with  evod  option 
; 2010sep04 HK  Accomodate xrange for non-monotonic data (indicated by psy ne 0)
; 2013may16 HK  Add  tloc  keyword
; 2015may31 HK  Add display of ranges used for an overplot 
; 2015jun07 HK  Add keyword rangu
; 2015aug10 HK  Add cclr keyword
; 2016dec14 HK  Fix X position of text if oplot and csize
; 2017mar29 HK  Incorporate common SETCOLOR_COM2
; 2017nov13 HK  Revise how range formatted and displayed
; 2018mar21 HK  Add  marg  and  lax  keywords
;_End          .comp chart

ssy=size(yyy) & nd=ssy[1]
mp = ssy(2)                     ; number of panels
type=ssy[ssy[0]+1]              ; type of input array
; reasonable minimum fractional plot range based on type
;    undef. byte  int    long   float double complex
prec=[ 1.,  .02, 1.e-4, 1.e-8, 1.e-6, 1.e-12, 1.e-6] ; types 0:6

c1 = not keyword_set(oplot) ; this is the first call, not an overplot
if c1 then dine=0 else dine=oplot ; line style for the data, unless evod

if not keyword_set(title) then title=''    ; top title
if not keyword_set(marg) then marg=.005 ; x-axis plot fractional margin
if not keyword_set(csize) then csize=1.    ; default charsize for xyouts
if not keyword_set(cthick) then cthick=1.  ; default charthick  for xyouts
if not keyword_set(cclr) then cclr=!P.color  ; default color for xyouts
if not keyword_set(xtit) then xtit='Count' ; default X-axis label
if not keyword_set(tloc) then tloc=replicate(0.4,mp) ; strip label Y location
if not keyword_set(psy) then psy=0         ; no symbols
if not keyword_set(clr) then begin 
    kok= !D.name eq 'X'        ; color ok for this device. false for B&W printer
    if kok then clr=!P.color else clr=0 ; color or white
endif
if keyword_set(range) then nr=n_elements(range)/2 else nr=-1 ; # of ranges
dof= keyword_set(fmt) ; format provided
if dof then fmt='(2'+fmt+')' else fmt='(2g12.5)'; range format
if n_elements(xin) eq nd then xax=xin else xax=findgen(nd)
if keyword_set(clip) then klip=clip>1.0 else klip=0. ; limit to its strip

if n_elements(kim) ge mp then nnk=kim-1 else nnk=replicate(nd-1,mp) ; # to plot

do2= keyword_set(evod)          ; will make separate curves for even and odd
sran1=''
if keyword_set(parti) then begin ; individual titles
  ntt=n_elements(parti)          ; # panel titles
  len=strlen(parti)              ; their lengths
  maxlen=max(len,min=minlen)     ; range of lengths
  blank=string(replicate(32B,maxlen-minlen+1))
endif else begin                ; no individual titles
  ntt=-1 
  if nr ge 1 then sran1='Range: '
endelse
if do2 then begin
  percol=0.01*!P.color            ; 1% of maximum color
  eve=xax[2*indgen(nd/2)]         ; all the even (0-based) points
  odd=xax[1+2*indgen((nd-1)/2)]   ; all the odd (0-based) points
  evsym=PSYMLINE(evod[0],evlin)   ; decode into two items
  if evod[0] lt -8 then evsym=0   ; no plot
  evcol=round(evod[1]*percol)     ; set color
  odsym=PSYMLINE(evod[2],odlin)   ; decode into two items
  if evod[2] lt -8 then odsym=0   ; no plot
  odcol=round(evod[3]*Percol)     ; set color
  if evod[4] ge 0 then begin      ; line for all points
    bolin= evod[4] mod 6
    bocol=round(evod[5]*Percol) ; set color
  endif
endif

if nr lt 1 then ytit='Each normalized onto [0,1]' $
else if nr eq 1 then ytit='Each normalized to: '+ST0(range) $
else ytit='Preset range for each strip'

;;;stop
xa=min(xax,max=xb) & xp=marg*(xb-xa)
xran=[xa-xp,xb+xp]                  ; plotting X range
xa=xax[0] & xb=xax[nd-1]            ; Left and Right end X values
if psy ne 0 then xa=min(xax,max=xb) ; if symbol, allow random order
if c1 then PLOT,xax,yyy(*,0),xrange=xran,yrange=[0.,mp],/nodata $
	,xstyle=1,ystyle=1,xtit=xtit,ytit=ytit,tit='Chart:  '+title

if c1 then xp=0.03 else xp=1.-0.15*csize
if keyword_set(lax) then xp=lax
xloc=xran[0]+xp*(xran[1]-xran[0])	; labeling X fractional position
rangu=fltarr(2,mp)
for k=0,mp-1 do begin           ; each panel
  ky=float(k)                   ; need floating to insure flat arithmatic
  yy=float(yyy[*,k])            ; "
;    j=where (finite(yy),ngood)
  jj=where (finite(yy) eq 0, nbad)
  if nbad eq nd then sran='all NaN' else begin
    j=nnk[k]                    ; number to plot for this line
    if j lt nd-1 then begin     ; option to plot fewer
      yy=yy[0:j]
      xp=xax[0:j]
    endif else xp=xax
    if nr ge 0 then begin                       ;  range provided
      kr=k < (nr-1)                             ; index of range to use
      ymin=range[0,kr] & ymax=range[1,kr]       ; reset range
    endif else ymin=min(yy,max=ymax,/nan)       ; auto-scale
    rangu[*,k]=[ymin,ymax]                      ; save the range used
    if ymax ne ymin then begin                  ; have changing data
      ymax = ymax > ((1.+prec[type])*ymin)      ; avoid range near roundoff
      y=(yy-ymin)/(ymax-ymin)                   ; scale to range
      if klip gt 0 then y= (y>0.)<klip          ; limit to its strip
      if nbad gt 0 then y[jj]=!values.f_nan     ; reinstate NANs
      if do2 then begin                         ; indicate odd and even points
        if evsym ne 0 then OPLOT,eve,y[eve]+ky,psym=evsym,color=evcol
        if evlin ge 0 then OPLOT,eve,y[eve]+ky,line=evlin,color=evcol
        if odsym ne 0 then OPLOT,odd,y[odd]+ky,psym=odsym,color=odcol
        if odlin ge 0 then OPLOT,odd,y[odd]+ky,line=odlin,color=odcol
        if evod[4] ge 0 then OPLOT,xp,   y+ky,line=bolin,color=bocol
      endif else if psy eq 0 then OPLOT,xp,y+ky,line=dine,color=clr $ ; data
      else OPLOT,xp,y+ky,psym=psy,color=clr
    endif
  if dof then sran=string(ymin,ymax,format=fmt) else sran=ST0([ymin,ymax])
  endelse
  if k lt ntt then sout=parti[k]+strmid(blank,0,maxlen-len[k]+1)  $
              else sout=sran1
  if nr ne 1 then sout=sout+sran
 if c1 then XYOUTS,xloc,k+tloc[k],sout,charsize=csize,charthick=cthick,color=cclr
  if not c1 and nr lt 0 then $
    XYOUTS,xloc,k+tloc[k],sran,charsize=csize,charthick=cthick,color=cclr
endfor

;option for pstrip separators
if c1 and keyword_set(dlin) then for k=1,mp-1 do plots,[xa,xb],[k,k],line=dlin

return
end
