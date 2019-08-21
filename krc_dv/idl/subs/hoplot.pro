PRO hoplot,xx,yy,k, _extra=e 
;_Titl  HOPLOT  Transfer to OPLOT that accomodates combined psym and linestyle
; xx	in.	X value array.
; yy	in.	Y value array.
; k	in.	Integer scalar for Psym and Line.  PSYMLINE convention.
; _extra in_    Can pass any valid arguments EXCEPT psym and linestyle to OPLOT
;_Desc
; Decodes k using the PSYMLINE conventions, and plots one curve
;_Hist  2001apr15 Hugh Kieffer
;_End

psym=PSYMLINE(k,pline)          ; decode k
if pline ge 0 then OPLOT,xx,yy,linestyle=pline,_extra=e
if psym  ne 0 then OPLOT,xx,yy,psym=psym,_extra=e

return
end
