function scalelin, xx, vv=vv, first=first, out=out 
;_Titl  SCALELIN  compute factors for linear scaling onto the interval -1:1
; xx	in. 	array (or the extremes) of variable to be scaled
; vv    in_     Output interval, Default is -1:+1
; first in_     If set, scaled=(xx*func[1])-func[0]   xx=(out+f[0])/f[1]
;           Default is  scaled=(xx-func[0])*func[1]   xx=out/f[1] + f[0]
; out	out_	input array scaled onto vv
; func.	out	fltarr(2) = [offset and multiplier] such that:
;		   scaled xx has same range as vv
;_Hist 99jan28  Hugh Kieffer
; 1999jul15 HK Add  out  keyword
; 2009may16 HK Add option for scaling onto any range
; 2012oct05 HK Add option for multiply before offset
; 2018feb18 HK comment typos only
;_End

if n_elements(xx) lt 2 then message,'Must have >1 values' ; impossible
if not keyword_set(vv) then vv=[-1.,1.] ; output range
if n_elements(vv) lt 2 then message,'vv Must have 2 values'
if vv[0] eq vv[1]  then message,'vv values must be different'
xu=max(xx,min=xl)		; get X extremes

if xu eq xl then begin
	fac=1. & off=0.		; degenerate case of no range
 endif else begin
     fac=(vv[1]-vv[0])/(xu-xl)  ; scale to a total range of 2.
     off=xl -vv[0]/fac          ; find the offset
 endelse

if arg_present(out) then out=(xx-off)*fac	; rescale input

if keyword_set(first) then off=off*fac ; multiply before offset

return,[off,fac]
end
