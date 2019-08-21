function zero360, aa,rad=rad
;_Titl  ZERO360  Brings angles into 0:360 degree range
;_args
; aa	in. scalar or vector of angles
; rad	in_   If set as flag (thus == 1), will treat angles as being in radians
;              if other value, uses that as 1/2 period. Default is 360 degrees. 
; rad	in_opt.	If set, will treat angles as being in radians
; func	out. output array; OK to override input. double prec. if needed
;_Desc Coding chosen to avoid any operations (and hence possible loss 
;  of precision) for values already within the desired output range.
;_Hist 99may13 Hugh Kieffer   derived from pm180
; 2014dec01 HK use floor instead of fix, remove tests for negative. 
;             Allow  rad  to be any half period
;_End

type=size(aa,/type)	; get type of input
if keyword_set(rad) then begin
    if abs(rad) eq 1 then half=!dpi else half=rad ; set angle units
endif else  half=180.D 
if type ne 5 then half=float(half)
cir=2.*half

ii = floor(aa/cir)		; number of full circles
bb=aa-cir*ii			; subtract them
return,bb
end
