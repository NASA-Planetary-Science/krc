function vec2code, vin, fmt=fmt
;_Titl  VEC2CODE  Convert vector of values to code string
; vin	 in.   Scalar or vector of numbers, all the same type
; fmt	 in_   String. Format to use for all values. e.g. ,'g6.4'
; func	 out.  String. The values formated for inclusion as IDL code
;               e.g.:    =[1.1,3.1415, 77.] 
;_Calls  DELAST0
;_Hist 2003aug16 Hugh Kieffer
; 2007jan30 HK Add optional format specification for floats
;_End

vv=vin
n= n_elements(vv)
type=size(vv,/type)
flt = type ge 4 and type le 6 ; float or double precision
if type eq 1 then vv=fix(vin)	; must convert byte to integer

dof=  keyword_set(fmt)
if dof then form='('+fmt+')'

if type eq 7 then begin 
    ss='=['''
    b=''','''
    b2=''']'
endif else begin 
    ss='=['
    b =','                      ; to separate items in output                 
    b2=']' 
endelse

for i=0,n-1 do begin
    vvi=vv[i]
    if flt then if dof then q=string(vvi, format=form) $ 
               else q=DELAST0(strtrim(string(vvi),2)) $
         else if type eq 7 then q=strtrim(vvi,2) $
         else  q=strtrim(string(vvi),2)
    ss=ss+q
    if i lt n-1 then ss=ss+b else ss=ss+b2
endfor
return,ss
end
