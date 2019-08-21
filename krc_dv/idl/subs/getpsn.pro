PRO getpsn, desc, var,labs=lab,align=align,use=use
;_Titl  GETPSN  Interactive input any elements of a string array, with prompt
; desc  in.	Prompt text
; var	both. 	String array to modify
; lab	in_	Labels for full prompts
; align	in_	If set, will align the columns for prompts. No affect on output.
; use	in_	Intarr of items to show and allow changes. Default is all 
;_Hist  99mar26 Hugh Kieffer   Derived from getpan
; 1999jul06 HK remove leading blank
; 2001apr17 HK Add align option
; 2001dec20 HK Add explanation of how to get null or blank, and -1 9 option.
; 20205jul13 Hk Add  use  option
;_End

n=n_elements(var)               ; # items in array
act=bytarr(n)                   ; logical "active" flags
if keyword_set(use) then act[use]=1 else act[*]=1 
on_ioerror,bad                  ; avoid failing out of this routine
idx=1  & test='a'               ; define types
print,'Input item # and its new value. [-1 1 for current list, -2 2 for quit].'
print, 'No quotes.  First space deleted, so <space><CR> yields the null string'
print,' -1 9 will show blanks or null'
show:
print,desc                      ; Callers description
if arg_present (lab) then begin ; print with labels
    if keyword_set (align) then begin ; print aligned
        tf=max(STRLEN(lab))
        fmt='(i3,A,A,T'+strtrim(tf+5,2)+',A,A)'
        for i=0,n-1 do if act[i] then print, i,' ',lab(i),' = ',var(i),format=fmt
    endif else for i=0,n-1 do if act[i] then print, i,' ',lab(i),'   = ',var(i)
endif  else for i=0,n-1 do if act[i] then print, i,' ',var(i) ; no labels
if test eq '9' then for i=0,n-1 do if act[i] then  print, i,' = >'+var(i)+'<' ; show non-print
get:
read,idx,test,prompt='Enter index and new value> '
i=strlen(test)                  ; # characters after end of index
test=strmid(test,1)             ; delete the leading blank
if i lt 3 then print,'Took >'+test+'<' ; show possible null or blank
if idx lt -1 then return	; all done
if idx eq -1 then goto,show	; display current values
if idx ge n     then begin & print,'invalid index' & goto,get & end
if not act[idx] then begin & print,'inactive index' & goto,get & end
var(idx)=test			; update value
goto, get			; prompt foe another change

bad:
print,'Read error occured; check and try again'
goto, get
end
