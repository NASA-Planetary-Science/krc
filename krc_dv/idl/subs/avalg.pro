function avalg, aaa,vv,op,dim=dim
;_Titl  AVALG  Array Vector algebra; +-*/=<> operations
; aaa	in.	Array of dimensionality 2 to 4 (could recode for more)
; vv	in.	Vector to be applied to aaa. If this is a scalar, 
;		then aaa may be any dimensionality
; op	in.	Operation as a 1-or 2 character string, Valid are:
;		'*'  out=aaa*vv
;		'+'  out=aaa+vv
;		'/'  out=aaa/vv
;		'-'  out=aaa-vv
;		'v/' out=vv/aaa		NOTE inversion !
;		'v-' out=vv-aaa		NOTE inversion !
;               '='  out=vv
;               '>'  out=aaa>vv
;               '<'  out=aaa<vv
; dim	in_	Dimension of array to which vector is applied.
;		  Default is the first that matches size of vv
; func.	out	Result of algebra same size as aaa.  -integer if formal error.
;_Desc
; This routine coded as with M0 logic; I.e., no change in array dimensions.
;   For example of  M1 (minus 1 dimension) logic, see m1mean .
; Because it is easy for user to change to make -vv or 1/vv,
;   the v- and v/ operations are inverted to allow more generality.
;_Hist  99may24  Hugh Kieffer
; 2001mar09 HK change op from keyword to argument
; 2001may08 HK Add the vx operations, and redefine the - and / operations
; 2007jan30 HK Allow for aaa to be a vector and vv a scalar. Purpose here is to
; avoid test and branches in calling programs 
; 2012mar12 HK  Add > and < operations
; 2013jan11 HK Make all indices long so can treat dimensions > 32767
; 2015oct04 HK Issues warning if dimension match ambiguous
;   Tested for speed in TIMINGD: relative to explicit code,
;   for large arrays slower by factor of 2, for 1000 elements factor is 2.5 to 6.
;_End                      .comp avalg

maxd=4                          ; max number of input dimensions coded for
kerr=0                          ; set error index to  none
ssa=size(aaa) & dima=ssa[0]     ; dimensionality of aaa
if dima lt 1  or dima gt maxd then goto,error1
ssv=size(vv)  & if ssv[0] gt 1 then goto,error2
vlen=n_elements(vv)             ; length of arg.2 (the vector)
if vlen eq 1 then begin         ; simple operation for entire array
    vc=vv[0]
	case op of		; do one row or column of array
	 '+': bbb=aaa+vc
	 '*': bbb=aaa*vc
	 '-': bbb=aaa-vc
	 '/': bbb=aaa/vc
	 'v-': bbb=vc-aaa
	 'v/': bbb=vc/aaa
         '=': begin & bbb=aaa & bbb[*]=vc &  end
	 '<': bbb=aaa<vc
	 '>': bbb=aaa>vc
	 else: goto, error5
	endcase

endif else begin ; normal situation

if dima eq 1 then dimu=1 else begin
    if not keyword_set(dim) then begin ; find first dimension match
        j=where(ssa[1:dima] eq vlen,i1) ; all dimensions that match
        if i1 gt 1 then message,'WARNING: dimension match ambiguous',/con
        dim=j[0]+1              ; will be 0 if no match
    endif 
    dimu=dim
endelse
if dimu lt 1 or dimu gt dima+1 then goto,error3	; insure dimension valid
if ssa[dimu] ne vlen then goto, error4	; sizes don't match
bbb=make_array(size=ssa)		; make output array matching input

; make list of all dimensions except the one aligned with vector
lastt=lonarr(maxd) 
if dima gt 1 then begin
    move=indgen(dima-1)+1       ; values = 1,2,...
    ssm=[ssa[0:dimu-1],ssa[dimu+1:*]] ; & print,ssm
    lastt[move]=ssm[move]-1     ; & print,lastt ; indexes on loops here
endif
for i3=0L,lastt[3] do begin
  for i2=0L,lastt[2] do begin
    for i1=0L,lastt[1] do begin

        case dimu of             ; extract vector to process
        1: xx=aaa[*,i1,i2,i3]
        2: xx=aaa[i1,*,i2,i3]
        3: xx=aaa[i1,i2,*,i3]
        4: xx=aaa[i1,i2,i3,*]
        endcase

	case op of		; do one row or column of array
	 '+':  yy=xx+vv
	 '*':  yy=xx*vv
	 '-':  yy=xx-vv
	 '/':  yy=xx/vv
	 'v-': yy=vv-xx
	 'v/': yy=vv/xx
         '=':  yy=vv
	 '<':  yy=xx<vv
	 '>':  yy=xx>vv
	 else: goto, error5
	endcase

        case dimu of             ; insert result into output array
        1: bbb[*,i1,i2,i3]=yy
        2: bbb[i1,*,i2,i3]=yy
        3: bbb[i1,i2,*,i3]=yy
        4: bbb[i1,i2,i3,*]=yy
        endcase
;stop
    endfor
  endfor
endfor

endelse
;; stop
return,bbb

; error section, stop with message [ if .skip .con , will return scalar code]
error5: kerr=kerr+1
error4: kerr=kerr+1
error3: kerr=kerr+1
error2: kerr=kerr+1
error1: kerr=kerr+1
ermes=[' ','Array # dim. <2 or too large','Vector # dim. not 1' $
 ,'No dimension matches','Dimensions dont match','Invalid operation code'] 
errstr='ERROR # '+strtrim(kerr,2)+': '+ermes[kerr]
message,errstr
return,-kerr

end
