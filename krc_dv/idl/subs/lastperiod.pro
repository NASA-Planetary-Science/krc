function lastperiod, lsv,leny,jy2, per=per, verb=verb
;_Titl  LASTPERIOD  Find the last period in generally increase "season"
; lsv   in.  fltarr(N) Season or other up-ramping periodic item
; leny  out. integer   Number of seasons in a year, may be an estimate
; jy2   out. integer   First index of the final [partial] year
; per   in_  float     Length of a period; default is 360.
; verb  in_  integer   If set, prints results. If ge 2, also print 2 other itmes
;                         If ge 4, then stops before return 
; func. out. integer   First index of the last [up to] complete year coverage
;_His  Hugh Kieffer  Address all possibilities of KRC files 
;_End      .comp lastperiod  

if not keyword_set(per) then per=360.
if per le 0. then message,'Must have positive period'
if not keyword_set(verb) then verb=0

nsea=n_elements(lsv)

xx=shift(lsv,-1)-lsv    ; large negative at the last of each year, last is wrap
ii=where(xx[0:nsea-2] lt -per/2.,j) ; locations of last-of-year

if j gt 1 then begin              ; have more than a full year
   leny=ii[j-1]-ii[j-2]           ; seasons in the last complete year
   dels=per/leny                 ; average delta LS
   lgo= per-lsv[nsea-1] le 1.1*dels ; last year is [virtually] complete
   jy2=nsea-leny               ; first season of the final [partial] year
   jy1= nsea-1-leny            ; first season of the complete year coverage
endif else if j eq 1 then begin ;  one Ls=0 crossing
; could have single or few points on either side of jump
   jy2=ii[0]+1                  ; start of the last partial year
   if jy2 lt nsea/2 then dels = (lsv[nsea-1]-lsv[jy2])/(nsea-1-jy2) $
      else dels=(lsv[jy2-1]-lsv[0])/(jy2-1); average Delta Ls in longer segment
;   yb=(nsea-1.)*dels/per               ; fractional number of years  
   jj=where(lsv[0:jy2-1] lt lsv[nsea-1],i) ; look for overlap in Ls
   if i lt 1 then begin ; no overlap, less than one year coverage
      jy1=0 
      leny=round(per/dels)      ; estimate
   endif else begin 
      jy1=i                     ; first of final year coverage
      leny=nsea-i
   endelse
endif else begin ; 0 jumps, have partial that could be complete
   dels = (lsv[nsea-1]-lsv[0])/(nsea-1)
   jy1=0 & jy2=0
   leny=round(360./dels) ; estimate of year length
endelse

if verb gt 0 then print,'LASTPERIOD:  leny,jy1, jy2 =',leny,jy1,jy2
if verb ge 2 then print,'j,dels=',j,dels
if verb ge 4 then stop

return,jy1
 end
