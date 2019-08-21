PRO curveguide,k,text,m, set=set, locc=locc, ksym=ksym, _extra=e
;_Titl  CURVEGUIDE  Put guide to curves or symbols onto a  plot
; k	in.  integer  Count within set of items on the guide
; text	in.  string to print as guide
; m	in.  integer  line index     OR index into SETCOLOR scheme
;            >5= no line             unless \set
;            if negative, warning, but no action
; set   in.  If set, m is index into kkc and kkl in Common 
; locc	in_  fltarr(4), location and size of Guide; in NORMAL units. 
;		  Should not be changes within a guide block
;		  Defaults are in the first line of code: upper left corner
;		0: X location for left end of guide (left end of line).
;		1: Y location of first guide
;		2: Delta Y between guides
;		3: Lenght of line   
; ksym   in_   integer  Psym index, none if <1
; _extra in_ 	 to transfer    color  DO NOT Use for line of psm.

common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,scex1
;_Desc
; Works in concert with SETCOLOR
;_Lims
; Uses normalized location, so will not work well with multi-plots on one page
;_Hist 2001apr22 Hugh Kieffer derive from curve_guide; no use of common
; 2003aug13 HK treat locc[2] in positive sense, not negative.
; 2003nov18 HK Increase separation between right symbol and name
; 2006apr24 HK Accomodate new version of PSYMLINE with no action
; 2009jul27 HK Add the  set  keyword, which option does not include symbols
; 2010apr12 HK Drop use of PSYMLIN, transfer psym via new keyword ksym
; 2012sep07 HK Single symbol if no line
;_End


;; locl=['Normalized Guide locations: X ',' "  Y location of first guide' $
;; ,' " Delta-Y between guides',' "  Length of line']
;,'X loc   \/ @--','Y loc   | Curve-','Delta Y | Guide for','LineLen /\ --' $

if m lt 0 then begin 
    message,'CURVEGUIDE  no longer uses PSYMLINE',/con
    return                      ; no action
endif

if not keyword_set(locc) then locc=[0.15,0.92,-0.04,0.08]
if not keyword_set(ksym) then ksym=0 ; Default no symbol 

xloc=locc[0]                    ; normalized X location
xlen=locc[3]                    ; Length of line 
yloc=locc[1]+k*locc[2]           ; Y location this time, normalized 

if keyword_set(set) then begin 
    clr =kkc[m mod kcc[2]]  ; color
    line=kkl[m mod kcc[3]] ; line type
    PLOTS,xloc+[0.,xlen],[yloc,yloc],/norm,line=line,color=clr,_extra=e
endif else begin
    if m    le 5 then PLOTS,xloc+[0.,xlen],[yloc,yloc],/norm,line=m,_extra=e
    if ksym gt 0 then begin 
        if m gt  5 then PLOTS,xloc+xlen,yloc ,/norm,psym=ksym,_extra=e $
        else PLOTS,xloc+[0.,xlen],[yloc,yloc],/norm,psym=ksym,_extra=e
    endif
endelse           
XYOUTS,xloc+xlen+.015,yloc-.006,text,/norm,_extra=e ; text

return
end
