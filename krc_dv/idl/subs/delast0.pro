function delast0,s
;_Title  DELAST0  Delete trailing 0's past the decimal point
; s	in.	a string representing a floating scaler
; func	out.	same string with any trailing zeros removed
;_Hist 2002aug05 Extract from ST0.pro; delete commented debug statements
;_End

ss=s
jp=STRPOS(ss,'.')	; should never be negative
je=STRPOS(ss,'e')	; may be -1
if je ge 0 then ss=STRMID(s,jp+1,je-jp-1) else ss=STRMID(s,jp+1)
again:
	J=strlen(ss)
	l=RSTRPOS(ss,'0')	; find last 0
	if l ne j-1 then goto,done ; if it is not at the end, quit
	ss=STRMID(ss,0,j-1)	; delete last 0
	if j le 1 then goto, done	; none left
	goto, again

done:
ss=STRMID(s,0,jp+1)+ss
if je gt 0 then ss=ss+STRMID(s,je)
return,ss
end
