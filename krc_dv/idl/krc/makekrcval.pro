function makekrcval, krccom,kist, vals
;_Titl  MAKEKRCVAL  Make string of selected KRC inputs: Key=val
; krccom in.  KRC KRCCOM structure
; kist	 in.  Intarr of 1-based indices to treat: 
;			+100 for .fd   +200 =.id   +300=.ld
; vals   out_ Fltarr of the values for items in kist
; func.	 out. String. of label=value. Null if kist is invalid
;_Calls DEFINEKRC  DELAST0  STRWORD1
;_Hist 2002aug05 Hugh Kieffer
; 2015dec07 HK Add argument vals, minor comments typos
; 2016feb17 HK Recode to call STRWORD1 in only one place
;_End                   .comp makekrcval
qq=DEFINEKRC('KRC',params,labkf,labki,labkl,idmin,idmax)
nin=n_elements(kist)

out=''                          ; null string to become results
vals=fltarr(nin)                  ; to hold values
for j=0,nin-1 do begin            ; each requested item
  i=kist[j]
  if i gt 300 then begin               ; logical value
    k=i-301                            ; index within the type
    slab= labkl[k]                     ; full description for this item
    fv=krccom.ld[k]                    ; get the value
    if fv ne 0 then qq='T' else qq='F' ; convert value to string
  endif else if i gt 200 then begin  ; integer value
    k=i-201
    slab=labki[k]
    v=krccom.id[k]
    fv=float(v)
    qq=strtrim(v,2) ; just the integer
  endif else if i gt 100 then begin ; real value
    k=i-101
    slab= labkf[k]
    fv=float(krccom.fd[k])  ; in case it was double with many 0's before a digit
    qq=DELAST0(strtrim(fv,2))   ; remove trailing zeros
  endif
  out=out+STRWORD1(slab)+'='+qq+' '
  vals[j]=fv
endfor
return,out
end
