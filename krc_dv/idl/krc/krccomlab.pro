PRO krccomlab ,kode,fcom,icom,lcom, fclab,iclab,lclab, uuu,  idx=idx
;_Titl  KRCCOMLAB  Print KRC common input items
; kode	in.	Integer. Print control code. +1=floats +2=integers +4=logicals
; fcom	in.	KRCCOM floating values
; icom	in.	KRCCOM integer  values
; lcom	in.	KRCCOM logical  values
; fclab	in.	Short (one word) labels for the float   items in KRCCOM
; iclab	in.	Short labels for the integer items in KRCCOM
; lclab	in.	Short labels for the logical items in KRCCOM
; uuu   in_     fltarr(nlat,[lat,elev],case). If present, will print
; idx   in_     flag. If+, will print 0-based index of items in arrays
;                  if abs=1, omits computed real items, if 2, includes them
;                  Default is -1
;_Desc
; Uses the _lab arrays for length, so the _com arrays must be at least this long
;_Calls  none
;_Hist 2002mar02 Hugh Kieffer Mostly extracted from READKRC
; 2002mar21 HK Make _Lab input, and immune to undefined arrays
; 2004jul24 HK. Correct error where in   n  was one too small
; 2010sep05 HK Add keyword idx
; 2011aug06 HK Add argument  uuu, more options for idx
; 2014mar07 HK Fix missinf "last" for logicals
;_End                .comp krccomlab

ko1=kode mod 2 eq 1             ; 1 was added
ko2=ishft(kode,-1) mod 2 eq 1   ; 2 was added
ko4=ishft(kode,-2) mod 2 eq 1   ; 4 was added
if not keyword_set(idx) then idx=-1
dix=idx gt 0 ; print indices

if ko1 then begin               ; print floats
    n=n_elements(fclab)
    if n lt 1 or n_elements(fcom) lt n then  $
      Print,'KRCCOMLAB: Some Float undefined' $
    else begin
        last=n-1
        if abs(idx) eq 1 then last=last-32 ; omit computed floats
        for i=0,last,8 do begin
            i2=(i+7) < last
            print,fclab[i:i2],format='(8A10)'
            if dix then print,i+indgen(i2-i+1),format='(8I10)'
            print,fcom[i:i2],format='(8f10.2)'
        endfor
    endelse
endif

if ko2 then begin               ; print integers
    n=n_elements(iclab)
    if n lt 1 or n_elements(icom) lt n then  $
      Print,'KRCCOMLAB: Some Int. undefined' $
    else begin
        last=n-1
        for i=0,last,8 do begin
            i2=(i+7) < last
            print,iclab[i:i2],format='(8A10)'
            if dix then print,i+indgen(i2-i+1),format='(8I10)'
            print,icom[i:i2],format='(8I10)'
        endfor
    endelse
endif

if ko4 then begin               ; print logicals
    n=n_elements(lclab)
    if n lt 1 or n_elements(lcom) lt n then  $
      Print,'KRCCOMLAB: Some Logical undefined' $
    else begin
        last=n-1
        for i=0,last,10 do begin
            i2=(i+9) < last
            print,Lclab[i:i2],format='(10A7)'
            if dix then print,i+indgen(i2-i+1),format='(10I7)'
            print,Lcom[i:i2],format='(10I7)'
        endfor
    endelse
endif

if n_params() ge 8 then begin 
print,'LATITUDES: in 10F7.2  _____7 _____7 _____7 _____7 _____7 _____7 _____7'
 print,uuu[*,0,0],form='(10f7.2)'
print,'Elevations: in 10F7.2 _____7 _____7 _____7 _____7 _____7 _____7 _____7'
print,uuu[*,1,0],form='(10f7.2)'
endif
if !dbug then stop
return
end
