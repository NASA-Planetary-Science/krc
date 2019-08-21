function st0, vin , nojoin=nojoin
;_Titl  ST0  Make minimal string for numbers, or string arrays
; vin	 in.   scalar or vector of numbers, all the same type
; nojoin in_ Flag or string. Controls joining and separator
;   If set as flag (not a string), output has same number of elements as input,
;		i.e., no join and no padding
;   If set as a string,  e.g., '_', that will be separator and items are joined
;   If not set, return single string with blank padding. (2 for strings)
; func	 out.  String of one value, or vector of them with one blank between
;_Lims Not designed for more than 99 items in a vector
;_Calls  DELAST0
;_Hist 99jul02 Hugh Kieffer  Revised to remove float trailing zeros 
; 1999aug30 HK fix to handle byte values
; 1999sep15 HK revise to return single string for integer arrays
; 1999dec30 HK minor fix, string input not treated as float.
; 2001nov01 HK Add nojoin keyword, recode.
; 2002aug05 HK Extract DELAST0 into separate file
; 2010jul13 HK Add ability to set nojoin as a separator.
;_End

vv=vin
n= N_ELEMENTS(vv)
type=SIZE(vv,/type)
flt = type ge 4 and type le 6 ; float or double precision
if type eq 1 then vv=fix(vin)	; must convert byte to integer

j=SIZE(nojoin,/type) ; =0 if not set, =2 if set, =7 if string
if j eq 2 then begin            ; nojoin set as flag
    ss=strarr(n)                ; output array
endif else begin
    if type eq 7 then b='  ' else b=' ' ; to separate items in output
    if j eq 7 then b=nojoin ; use the provided character
    ss=b; 
endelse

for i=0,n-1 do begin
    vvi=vv[i]
    if flt then q=DELAST0(STRTRIM(STRING(vvi),2)) $
      else if type eq 7 then q=STRTRIM(vvi,2) $
      else  q=STRTRIM(STRING(vvi),2)
    if i eq n-1 then b=''       ; no trailing separator
    if j eq 2 then ss[i]=q else ss=ss+q+b  
endfor
;stop
return,ss
end
