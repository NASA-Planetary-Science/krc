function linatan,xx, x1,y1, xfac,xlin, cln=cln
;_Titl  LINATAN  Transform with range 0:1, linear at small values, arctan log at large
; xx    in.  float or fltarr(N). Independent values
; x1, y1 in. float Independent, Dependent value at one  point
; xfac  in.  float Factor in X for change of 1/4. MUST be positive
; xlin  in.  float. Value below which relation is linear, MUST be < x1
; cln   out. float or fltarr(N). Simple e^- tau model, linear at small P
; func. out. float or fltarr(N). Dependent values, within zero to 1.
;_Desc  Use the arctan relation , which has a total range of -\+ pi/2.
;  Reduce range to -/+ 1/2, and add 1/2
;      y=0.5+atan [ln(x/xmid)/ln(xfac)] /pi
; derive: x1/xmid= exp[ln(xfac) * tan(pi(y1-.5)) ]
;            xmid= x1/( exp[ln(xfac) * tan(pi(y1-.5)) ] )
;                = x1/xfac^ tan(pi(y1-.5))
;_Hist  Hugh Kieffer 2014jan26  As a crude model of Mars atm gas IR opacity
;_End         .comp linatan

nx = n_elements(xx) ; size of input
out=fltarr(nx)      ; create output

afac=alog(xfac)                   ; term needed in several places
xmid= x1/( xfac^ tan(!pi*(y1-.5)) ) ; point of steepest change
ylin= 0.5+atan(alog(xlin/xmid)/afac)/!pi

for j=0,nx-1 do begin           ; each input point
   xin=xx[j]
   if xin gt xlin then begin ; use atan log relation
      y= 0.5+atan(alog(xin/xmid)/afac)/!pi
   endif else begin
      y=xin*ylin/xlin           ; linear
   endelse
   out[j]=y
endfor

if arg_present(cln) then begin ; Y=1.-exp(-P/Pa) ==1.-exp(P/(-Pa))
; Pa=X1/ln(1.-y1)
   mpa=x1/alog(1.-y1)           ; one free parameter to match a point.
   cln=1.-exp(xx/mpa)          ; simpler crude band model, linear at small P
endif
if nx eq 1 then out=out[0]      ; make a scalar

return, out
end
