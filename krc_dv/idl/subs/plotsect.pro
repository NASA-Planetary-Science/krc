PRO plotsect, lab, yf,  xv=xv, lins=lins, xp=xp,yr=yr,_extra=tra
;_Titl  PLOTSECT Plot section lines and titles when several things on one abcissa
; lab  in. strarr(N)    Section labels
; yf   in. float        Fraction of way up for labels.
; xv   in_ fltarr(N-1)  X values of the separations. Default is uniform
; NOPE cs   in_ float        Character size of labels. Default is 1.5
; lins in_ integer      Line style, Default is 1=dotted. -1 is solid, -2 is none
; xp   in_ intarr(2)    Delta of X axis from data range. OBSOLETE 2017dec13
;                          commonly [-1,1] to avoid symbol plotting on edges
; yr   in_ fltarr(2)    Fraction of way up for the vertical lines.
;                           Default is entire plot
; _extra in  Any keyword accepted by xyouts, e.g., orientation
;_Hist  2013sep01 Hugh Kieffer  When can't find any prior version
; 2014may08 HK  Accomodate Y -log axis
; 2014mar01 HK  Fix bug in xv index
; 2015jun07 HK  Add keyword lin
; 2015aug13 HK  Allow lines to be omitted. ; 2015oct07 HK  Add keyword yr
; 2016jan19 HK  Constrain Y positions to be within data range, 
;               Y inputs always fractions
; 2016sep13 HK Remove keyword cs and add _extra
; 2017dec13 HK Inset first and last labels from edges
;_End             .comp plotsect

nlab=n_elements(lab)            ; number of section
if not keyword_set(lins) then lins=1
linu=lins
if lins eq -1 then linu=0       ; solid line

pxr=!x.crange                   ; primary x-range
pyr=!y.crange
 
if n_elements(yr) eq 2 then ylen=yr else ylen=[0.,1.]

ya=yf<.97                       ; ensure within plot
yy=[ya,ylen]                    ; as fraction of plot Y
yy=(yy>0.)<1.                   ; ensure all on plot
yy=pyr[0]+yy*(pyr[1]-pyr[0])    ; convert Y values in data units
if !y.type eq 1 then yy=10.^yy  ; Y is log
ya=yy[0] 
ylen=yy[1:2]

if n_elements(xp) eq 2 then pxr=pxr-xp ; recover ends of data 
if n_elements(xv) lt (nlab-1)>1 then begin;  not enough provided
   if pxr[0] eq 0 and pxr[1]-round(pxr[1]) eq 0 then begin ; integral indices
      nt=pxr[1]+1                                          ; number of intervals
      dx=nt/nlab
      xx=indgen(nlab+1)*dx -.5 ; generate extra at each end
   endif else xx=pxr[0]+(findgen(nlab+1))*((pxr[1]-pxr[0])/float(nlab))
endif else xx=[pxr[0],xv[0:nlab-2],pxr[1]]
xc=(xx+shift(xx,-1))/2.        ; center of each section

if not keyword_set(cs) then cs=1.5
cs=(cs>.5)<6.                   ; reasonable limits

if lins ge 0 then for j=1,nlab-1 do plots,[1.,1.]*xx[j],ylen,line=linu ; separate sections
alii=replicate(0.5,nlab)
alii[0]=-0.1 & alii[nlab-1]=1.1
for j=0,nlab-1 do xyouts,xc[j],ya,lab[j],align=alii[j],_extra=tra ; each lable

if !dbug ge 7 then stop
return
end


