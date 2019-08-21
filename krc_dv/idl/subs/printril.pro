PRO printril ,kode, fval,fclab, ival,iclab,lval, lclab
;_Titl PRINTRIL Print real, integer and logical items
; kode	in.	Integer. Print control code. +1=floats +2=integers +4=logicals
; fval	in.	Fltarr of values
; ival	in.	Intarr of values
; lval	in.	Intarr of Logical values
; fclab	in.	Short labels for the float   items
; iclab	in.	Short labels for the integer items
; lclab	in.	Short labels for the logical items
;_Desc
; Uses the _lab arrays for lenght, so the _val arrays must be at least this long
;_Calls None
;_Hist 2002mar02 Hugh Kieffer Mostly extracted from READKRC
; 2002mar21 HK Make _Lab input, and immune to undefined arrays
;_End

ko1=kode mod 2 eq 1             ; 1 was added
ko2=ishft(kode,-1) mod 2 eq 1   ; 2 was added
ko4=ishft(kode,-2) mod 2 eq 1   ; 4 was added

if ko1 then begin 
    n=n_elements(fclab)-1
    if n lt 1 or n_elements(fval) lt n then  $
      Print,'PRINTRIL: Some Float undefined' $
    else begin
        last=n-1
        for i=0,last,8 do begin
            i2=(i+7) < last
            print,fclab[i:i2],format='(8A10)'
            print,fval[i:i2],format='(8f10.2)'
        endfor
    endelse
endif

if ko2 then begin
    n=n_elements(iclab)-1
    if n lt 1 or n_elements(ival) lt n then  $
      Print,'PRINTRIL: Some Int. undefined' $
    else begin
        last=n-1
        for i=0,last,8 do begin
            i2=(i+7) < last
            print,iclab[i:i2],format='(8A10)'
            print,ival[i:i2],format='(8I10)'
        endfor
    endelse
endif

if ko4 then begin 
    n=n_elements(lclab)-1
    if n lt 1 or n_elements(lval) lt n then  $
      Print,'PRINTRIL: Some Logical undefined' $
    else begin
        for i=0,last,10 do begin
            i2=(i+9) < last
            q=replicate('F',i2-i+1)
            jj=where(Lval[i:i2] ne 0,nj)
            if nj gt 0 then q[jj]='T'
            print,Lclab[i:i2],format='(10A7)'
            print,q,format='(10A7)'
        endfor
    endelse
endif

return
end
