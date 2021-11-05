PRO plotsect, lab, yf,  xv=xv, lins=lins, xp=xp,yr=yr,scol=scol,_extra=uex
;_Titl  PLOTSECT  Plot section lines and labels when several things along abscissa
; lab  in. strarr(N)    Section labels
; yf   in. float        Fraction of way up for labels.
; xv   in_ fltarr(N-1)  X values of the separations. Default is uniform to ends
;                        Negative scalar will place label in center of each section
; lins in_ integer      Line style, Default is 1=dotted. -1 is solid, -2 is none
; xp   in_ intarr(2)    Delta of X axis from data range. Default=[0,0]
;                         e.g.,[-2,0] leaves room on left for legend
; yr   in_ fltarr(2)    Fraction of way up for the vertical lines.
;                           Default is entire plot
; scol in_ integer      Color of the seperator lines
; _extra in  Any keyword accepted by xyouts, e.g., orientation, which must be
; 'orient'.  
;_Desc
; The location of the separation lines sets the positions of the labels.
; Virtual (not plotted) lines at the end of data range. Intermediate lines
; set by xv if it is large enough, else uniform.
; Labels are at the center of each interval, except end ones will be inset if
; xp is not set.     
;_Hist  2013sep01 Hugh Kieffer  When can't find any prior version
; 2014may08 HK  Accomodate Y -log axis
; 2014mar01 HK  Fix bug in xv index
; 2015jun07 HK  Add keyword lin
; 2015aug13 HK  Allow lines to be omitted. ; 2015oct07 HK  Add keyword yr
; 2016jan19 HK  Constrain Y positions to be within data range, 
;               Y inputs always fractions
; 2016sep13 HK Remove keyword cs and add _extra
; 2017dec13 HK Inset first and last labels from edges
; 2018jul20 HK Improve inset of first and last labels from edges
; 2018aug22 HK Make robust if called with 1 label
; 2019mar25 HK Fix bug that had no solid line
; 2019may02 HK Add  xv=-scalar  option
; 2020mar04 HK Add keyword scol
;_End             .comp plotsect

nlab=n_elements(lab)            ; number of section

if not keyword_set(lins) then lins=1                  ; default
if n_elements(xp) eq 2 then xu=xp else xu=[0.,0.]     ; default
if n_elements(yr) eq 2 then ylen=yr else ylen=[0.,1.] ; default
if not keyword_set(scol) then scol=255                  ; default

linu=lins>0                     ; -1 is solid line

pxr=!x.crange -xu               ; primary x-range for data
pyr=!y.crange

ya=yf<.97                       ; ensure within plot
yy=[ya,ylen]                    ; as fraction of plot Y
yy=(yy>0.)<1.                   ; ensure all on plot
yy=pyr[0]+yy*(pyr[1]-pyr[0])    ; convert Y values in data units
if !y.type eq 1 then yy=10.^yy  ; Y is log
ya=yy[0] 
ylen=yy[1:2]

rx=pxr[1]-pxr[0]                  ; total data range

; xc is location of labels
; xx[1:nlab-1] is location of divider lines

if keyword_set(xv) then begin
  if n_elements(xv) ge (nlab-1) then xx=[pxr[0],xv[0:nlab-2],pxr[1]] $
    else message,'Expect xv to be at least nlab-1. Using default',/con 
endif else begin
  dint=max(abs(pxr-floor(pxr))) eq 0 ; both limits are integers
  if dint then begin
    dx=(rx+1)/nlab  & xo=.5
  endif else begin 
    dx=rx/float(nlab) & xo=0.
  endelse
  xx=(pxr[0]-xo)+findgen(nlab+1)*dx ; divisors, including virtual ends
endelse
xc=(xx+shift(xx,-1))/2. ; middle of each interval, last is wrap

; if nlab eq 1 then xc=(pxr[0]+pxr[1])/2. ; put single label in the center

if lins ge -1 and nlab gt 1 then $
  for j=1,nlab-1 do plots,[1.,1.]*xx[j],ylen,line=linu,color=scol ; section dividers

for j=0,nlab-1 do xyouts,xc[j],ya,lab[j],align=0.5,_extra=uex ; each label

if !dbug ge 7 then stop
return
end


