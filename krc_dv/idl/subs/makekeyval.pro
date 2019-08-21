FUNCTION  makekeyval, key,val,fmt=fmt,sep=sep,tex=tex
;_Title  MAKEKEYVAL  Make keyword=value string for file header
;_Args
; key	in. Strarr of keywords
; val 	in. Values array, may be any type except structure or complex.
; sep   in. string Character to separate key=val sets. Default is a blank
;             Not used if  tex  is set.
; fmt	in_ String. Array of formats; e.g. 'G12.2'; <8 characters
;		Ignored for string values
;		if contains an  'I', values are treated as integer
;		If value is real, but integral value, printed as integer
; tex   in  Int. If set, then will add characters for LaTeX formatting
;                Value is number of  \  spaces to insert between items
; func.	out. Composite string
;_Desc
;  There will be one blank at start and end of string, with multiple
;  keyword=value (no internal blanks) sets separated by 1 blank.
;  String values will have any internal blanks removed!
;_Hist  HughKieffer  98dec29
; 2002feb13 HK Allow string values
; 2003jun20 HK Process floats with DELAST0 to delete trailing zeros
; 2008dec30 HK Add LaTeX option
; 2011oct09 HK Add  sep  option
;_End

n=n_elements (key)
siz=size(val)
type=siz[siz[0]+1]
if siz[siz[0]+2] ne n then message,'Size mismatch'
dotex=keyword_set(tex)
if dotex then begin             ; format for LaTeX
    bbb='$ '                    ; start with leading blank
    space=' \ '                 ; add space between items
    if tex gt 1 then for i=2,tex do space=space+'\ '
endif else begin                ; plain formatting
    bbb=' '                     ; start with leading blank
    if keyword_set(sep) then space=sep else  space=' ' ; blank between items
endelse
for j=0,n-1 do begin		; each item
  rv=val(j)                     ; move into equivalenced area
  keyj=strtrim(key(j),2)	; trim both ends
  rem0=0B                       ; unset the "remove trailing 0's" flag
  if type eq 7 then ss=strcompress(val(j),/REMOVE_ALL) $
  else begin                    ; numeric
    if keyword_set(fmt) then form='('+fmt(j)+')' else begin
      del=.5
      if abs(rv) lt 1111 then del=rv-fix(rv) ; will be zero for positive integers
      if del eq 0. then form='(i10)' else begin
          form='(g12.5)'        ; default formats
          rem0=1B
      endelse
    endelse
    ss=string(val(j),format=form)
    if rem0 then ss=DELAST0(ss)
    ss=strtrim(ss,2)		; trim both ends
    endelse
    bbb=bbb+keyj+'='+ss    ; add to output string
    if j lt n-1 then bbb=bbb+space
endfor
if dotex then bbb=bbb+' $'
return,bbb
end
