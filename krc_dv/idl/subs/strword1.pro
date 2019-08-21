function strword1, ss,len=len
;_Titl STRWORD1 Extract first word from a string or strarr
; ss	in	String or String array
; len   in_ Integer. length into which to right-adjust the string.
;       Default is that output string is the length of the first word.
;       If this set, then longer words trimmed to retia first len characters
; func.	out.	String or String array of the first word of each item in ss
;_Desc
; If no interior blanks, returns the string (exterior blanks removed)
;_Hist 2002mar21 Hugh Kieffer
; 2009mar4 HK Add len option
;_End

ww=['',' ','  ','   ','    ','     ','      ','       ','        '] ; whites
last=n_elements(ww)-1           ; last valid index for above

if not keyword_set(len) then len=0
dolen = len gt 0                ; logical flag

n=n_elements(ss)                ; size of array
qq=strtrim(ss,2)                ; remove any leading blanks or trailing blanks
ib=strpos(qq,' ')               ; location of first blank
if dolen then ib=ib<len         ; trim if long
out=strarr(n)                   ; create output array
for i=0,n-1 do begin            ; for each element
    j=ib[i]                     ; location of first blank
    if j ge 0 then q=strmid(ss[i],0,j) else q=ss[i]
    if dolen and j lt len then q=ww[(len-j)<last]+q 
    out[i]=q
endfor
return,out
end
