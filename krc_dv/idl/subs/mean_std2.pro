function mean_std2, xin,one=one, wei=wei,snan=snan, STD=std,rng=rng,double=double
;_Titl  MEAN_STD2  Mean and standard deviation of 2-D array
; xin	in.	Array; statistics across 2nd element
; one	in_	   if set, do statistics along first dimension
;			default is along second dimension
; wei	in_	Weight for each point, Default= uniform
; 	 if present, must be same size as xin OR 1-D of the length of a vector.
; snan  in_     Flag, if set, StdDev of 1 point will be NAN; default is -1.
; std	in or out_  Out=Standard deviation vector; =-1. if formal error
;	If ,/std on input, then the output function includes mean and StdDev
; rng   out_   fldarr(n,2) Min and max across statst dimension
; func. out.	Vector of means; scalar <0 if formal error
; 	   If ,/std set on input, the fltarr[*,2] where  0]=mean & 1]=StdDev
;_Calls  MEAN_STD to do the statistics
;_Hist 99feb01 Hugh Kieffer, designed for spectra. 99feb09 HHK add /one option
; and allow larger number of dimensions if they have size of 1
; this routine robust against vectors and array sizes of 1
; 2000feb13 HHK renamed older version MEAN_STD3, which can still handle some
;  3 and 1 dimensional arrays; but makes a copy of the input array.
; MEAN_STD2 presumes input array is 2-D, and now accepts weights.
; 2001jan05  HHK   will treat NAN's as missing data by calls to MEAN_STD
; 2001feb05 HHK  Add option to output arr[*,2]
; 2011novo8 HK  Add  snan  keyword. 
; 2014nov02 HK Add  double  keyword Ensure use of double if input is double 
; 2018jan12 HK Add rng keyword 
;_End                     .comp mean_std2

ssx=size(xin) & type=ssx[ssx[0]+1]			; get array size
if ssx[0] ne 2 then message,'Requires 2-D array [see MEAN_STD3]' ; formal error
db=type eq 5 or type eq 9 or keyword_set(double) ; do totals in double precision
if keyword_set(one) then begin
    kone=1B                     ; treat 2-nd dimension as vectors of data
    k=ssx[2]                    ; number of vectors to process
    l=ssx[1]                    ; length of each vector
endif else begin
    kone=0B
    k=ssx[1]  & l=ssx[2]   
endelse

if db then mean=dblarr(k) else mean=fltarr(k); array to hold results
if db then rng=dblarr(k,2) else rng=fltarr(k,2); array to hold range

both=0B ; if true, then put both mean and std into output function
if keyword_set(std) or arg_present(std) then begin ; must do Std Dev
    lstd=-1B                    ; set true
    if db then sdev=dblarr(k) else sdev=fltarr(k) ; to hold Standard Deviation
    if not arg_present(std) then both=1B
endif else lstd=0B              ; set false
unan=keyword_set(snan)          ; use NAN for StdDev of 1 point

ww=0                            ; default in case wei is not set
eachw = 0                  ; do not look for weights separately for each vector
if keyword_set (wei) then begin
    ssw=size(wei)
    if ssw[0] eq 2 then begin   ; need 2-D weight vector same size as data
        if (ssw[1] ne ssx[1] or ssw[2] ne ssx[2])  $
          then message,'Size of wei does not agree'
        eachw=-1                ; turn on each weight is separate
    endif else begin            ; need 1-D weight vector same length as data
        if ssw[1] ne l then message,'Size of wei does not agree'
        ww=wei                  ; weight vector to use for all data vectors
    endelse
endif

for i=0L,k-1 do begin ; each output index
    if kone then xx=xin[*,i] else xx=reform(xin[i,*]); vector to process
    if eachw then begin
        if kone then ww=wei[*,i] else ww=reform(wei[i,*])
      endif
    rng[i,0]=min(xx,max=yb) & rng[i,1]=yb ; range for this set
    if lstd then begin
        mean[i]=MEAN_STD(xx,wei=ww,std=st, /nan,double=double) ; look for NAN's
        if unan and st lt 0. then st=!values.F_NAN
        sdev[i]=st               ; transfer standard deviation
    endif else mean[i]=MEAN_STD(xx,wei=ww, /nan,double=double)
endfor
if lstd and not both then std=sdev; transfer Standard Deviation
if both then return,reform([mean,sdev],k,2) else return,mean

end
