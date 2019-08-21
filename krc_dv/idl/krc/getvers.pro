function getvers, sss, nnn
;_Titl  GETVERS  Get version number v.N.N.N from a string
; sss in. string. Should have version number after 'v' in first word
;            version is set of integers separated by periods. e.g., 2.1.3
; nnn out. intarr  The integers of the version: e.g., [2,1,3]
; func. out. string, version number as a string, e.g., '2.1.3'
;_Desc.  If no proper version is found, the returns 'v0' and nnn=[0,0,0]
;_Hist 2013jun10 Hugh Kieffer
;_End

buf=strtrim(sss,1)            ; remove any leading blank 
j=strpos(buf,' ')             ; find after end of first word
i=strpos(buf,'v')             ; last character before numbers
if i lt 0 or i gt j then begin 
   message,'No v in first word',/con
   nnn =[0,0,0]                 ; benign default
   out='v0'                     ; benign default
endif else begin 

   out=strmid(buf,i+1,j-i-1)    ; extract the version as string
   nnn=fix(str_sep(out,'.'))    ; get the integers
endelse
return,out
end
