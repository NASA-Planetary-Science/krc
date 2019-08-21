function month, in, up=up
;_Titl  MONTH  Converts 3-character month to/from integer 1:12
; in	in.	Month of the year, Integer or 3(+) letter string
;		  Scalar or vector
; up	in_	Number of uppercase characters out. Ignored for string input.
;			Default is 0, 1 yield  e.g., 'Jan' out
;		 	3:   e.g., 'JAN' out
; func.	out.	Converted month of the year, default is 3 lower-case letters
;_Desc
; If integer input, values outsied 1:12 yield '???'
; If string in, processes first 3 letters
;    if month is not recognizeable, returns -1.	
;_Hist  Hugh Kieffer 99feb24
; 2000jun05  HK add string-->int capability
; 2001sep17  HK action controlled by type of input, invoke '???'
;_End

mmm=['??-','jan','feb','mar','apr','may','jun','jul','aug','sep' $
,'oct','nov','dec','??+']

siz=SIZE(in)
type=siz[siz[0]+1]              ; input word type
n =siz[siz[0]+2]                ; number of input elements

if type eq 7 then begin           ; convert from string to integer
    ss=STRMID(STRLOWCASE(in),0,3) ; get first 3 characters as lowercase
    out=intarr(n)               ;  create integer output array
    for i=0,n-1 do begin ; each month
        j=where(mmm eq ss[i])   ; 
        if j[0] lt 0 then out[i]=-1 else out[i]=j[0] ; convert to 1:12
    endfor 
    if n le 1 then out=out[0]   ; case of scalar in, scalar out
endif else if type le 3 then begin ;  convert from integer to string
    if not keyword_set(up) then up=0
    out=mmm[((in <13) > 0)]       ; get as all lowercase
;;    out=mmm((in-1) mod 12)      ; get as all lowercase
    if up eq 1 then out=STRUPCASE(STRMID(out,0,1))+ STRMID(out,1,2)
    if up eq 3 then out=STRUPCASE(out) 
endif else Message,'Invalid input type'
 
return,out
end
	
