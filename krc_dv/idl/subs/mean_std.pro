function mean_std,xx,wei=wei,nan=nan,double=double,both=both, STD=std
;_Titl  MEAN_STD   Mean and standard deviation of a vector
; xx	in.     Array of any dimensionality
; wei	in_	Weight for each point, Default= uniform
; nan	in_	If set, treats NAN's as missing data
; double in_    Flag: Do totals in double precision
; both  in_     Flag: Return both mean and StdDev in function.
; std	out_	Standard deviation;  set to -1. if too few points
; func. out.    Mean.  It is possible that this is not finite
;           If  both  is set, then function is [Mean,StdDev]
;_Hist 98jul05 Hugh Kieffer 99feb01 more robust
; 2000feb06 HHK add weight option
; 2001jan05 HHK add NAN option
; 2001dec17 HK Fix bug of undefined std when input is all NANs
; 2005jun22 HK Fix StdDev bug, Did not handle n=2
; 2009sep27 HK Add keyword double
; 2014jan08 HK Add comment that mean (and std) can be not finite in perverse
;    cases. E.g., total exceeds valid numerical range 
; 2014nov01 HK Ensure use of double if input is double  
; 2015jun04 HK Add option for both return values in the function, Redo the logic
; 2018jun14 HK FIx bug that did not compute std when /both set.
;_Desc
; Weight is squared for weighting in the variance sum
;_End             .comp  mean_std

siz=size(xx) & type=siz[siz[0]+1]
nin=siz[siz[0]+2]
db=type eq 5 or type eq 9 or keyword_set(double) ; do totals in double precision
if keyword_set(wei) then if n_elements(wei) ne nin then message,'wei must be same size as xx'

unif = not keyword_set(wei) ; uniform weighting

if keyword_set( nan ) then begin
    igood = where( finite(xx) ne 0, ngood) ; number that are not NAN's
  endif else ngood=nin          ; else, all are good, proceed normally

if ngood lt 2 then begin; rare case: too few for statistics
  std=-1. 
  if ngood lt 1 then mean=!values.F_NAN else begin 
    if keyword_set(nan) then mean=xx[igood] else mean=xx
  endelse
endif else begin               ; N now 3 or more, full statistics

  if ngood lt nin then begin    ; some good, Use recursive call, also does stdDev
    if unif then mean=MEAN_STD(xx[igood],STD=std,doub=db) else $
      mean=MEAN_STD(xx[igood],wei=wei[igood],STD=std,doub=db) ; recursive
       
  endif else if unif then mean=total(xx,doub=db)/float(nin) else $
                mean = total(wei*xx,doub=db)/total(wei,doub=db)

  if ngood eq nin and (arg_present(std) or keyword_set(both) ) then begin ; do standard deviation
    if unif then var=total((xx-mean)^2,doub=db) /(nin-1.) else $ ; variance
    var=(total((wei*(xx-mean))^2,doub=db) / total(wei^2,doub=db)) $
             *(float(nin)/(nin-1.))
    std=sqrt(var)               ; standard deviation
  endif
endelse

if keyword_set(both) then return,[mean,std] else return,mean
end
