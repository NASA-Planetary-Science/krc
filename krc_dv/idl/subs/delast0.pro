function delast0,s
;_Title  DELAST0  Delete trailing 0's past the decimal point
; s	in.	a string representing a floating scaler
; func	out.	same string with any trailing zeros removed
;_Hist 2002aug05 Hugh Kieffer Extract from ST0.pro; delete commented debug statements
; 2019feb20 HK Change IDL routine calls to lower case, replace obsolete routine RSTRPOS
;_End

ss=s
jp=strpos(ss,'.')	; should never be negative
je=strpos(ss,'e')	; may be -1
if je ge 0 then ss=strmid(s,jp+1,je-jp-1) else ss=strmid(s,jp+1)
again:
	J=strlen(ss)
	l=strpos(ss,'0',/reverse_search)	; find last 0
	if l ne j-1 then goto,done ; if it is not at the end, quit
	ss=strmid(ss,0,j-1)	; delete last 0
	if j le 1 then goto, done	; none left
	goto, again

done:
ss=strmid(s,0,jp+1)+ss
if je gt 0 then ss=ss+strmid(s,je)
return,ss
end
