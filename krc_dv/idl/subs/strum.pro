function strum, ss,xx, join=join,quiet=quiet, rem=rem, nix=nix
;_Titl  STRUM  Separate or concatonate strings into one using separator.
; ss	in.	String with items to be unjoined or Strings to be joined.
; xx	in. 	Single character used as separator.
; join	in_ 	If set, joins strings; else, separates them.
;               If == -2 then, extracts existing joined string from ss
; quiet	in_	If set, no message if separation character not found.
; rem	in_	If set, will try to remove a STRUM entry from sss
; nix	out.	Integer count of number of strings that contained an xx
;		Used only if join is set. 
; func.	out.	Strings that have been unjoined or joined.
; 		If rem was set, string cleaned of that STRUM
;		If error, returns 'ERROR'
;_Desc
; Double separation character is used at both ends of cat-string. 
; The separation character should NOT occur within any string element.
; This methodology allows blanks within string elements.
; E.g., if xx is '|' then a joined string might be:
;    '||one|this is second string|three||'
;_Hist  2001apr02 Hugh Kieffer
; 2001oct07 HK Add warning if separation character occurs within a string
; 2004aug17 HK Add error print and return
; 2006jan02 HK Add warning if separator pair occurs more than twice in input.
; 2007mar23 HK Add   quiet  option
; 2007apr03 HK Add    rem   option
; 2012nov12 HK Add option to return the joined string with no change
;_End

verb= not keyword_set(quiet)
ierr=0 ; error count
n=N_ELEMENTS(ss) & if n lt 1 then goto,err1
if n_elements(xx) ne 1 then goto,err2
if strlen(xx) ne 1 then goto,err2
x2=xx+xx                         ; form end expression
if not keyword_set(join) then join=0

if join eq -2 then begin        ; extract joined string as is
    I=STRPOS(ss,x2)             ; look for start
    if i lt 0 then goto,err4    ; this strum not in the input string
    j=STRPOS(ss,x2,i+2)         ; look for end
    if j lt 0 then goto,err5 
    out=STRMID(ss,i,j-i+2)      ; extract existing joined items
endif else if keyword_set(rem)then begin ; look for prior STRUM to remove
    I=STRPOS(ss,x2)              ; look for start
    if i lt 0 then return,ss    ; this strum not in the input string
    j=STRPOS(ss,x2,i+2)         ; look for end
    if j lt 0 then goto,err5
    out=STRMID(ss,0,i)+ STRMID(ss,j+2) ; split the string
endif else if join ne 0 then begin ; join strings
    nix=0                       ; count of strings that contain the separator 
    for i=0,n-1 do if strpos(ss[i],xx) ge 0 then nix=nix+1
    out=x2+ss[0]                ; double to start
    if n gt 1 then for i=1,n-1 do out=out+xx+ss[i] ; interior
    out=out+x2                  ; add double character at end
    if nix gt 0 then goto,err3
endif else begin                ; separated strings
    I=STRPOS(ss,x2)             ; look for start
    if i lt 0 then goto,err4
    j=STRPOS(ss,x2,i+2)         ; look for end
    if j lt 0 then goto,err5
    out=STR_SEP(STRMID(ss,i+2,j-i-2),xx) ; split the string
    k=STRPOS(ss,x2,j+2)
    if k ge 0 then goto,err6
endelse

done: return,out

;------------- Error section --------------------------------------------

err6:ierr=ierr+1
err5:ierr=ierr+1
err4:ierr=ierr+1
err3:ierr=ierr+1
err2:ierr=ierr+1
err1:ierr=ierr+1
errs=['null','String not defined','Separator not a single character' $
,'Separator was found in input strings','No ends found' $
,'No 2nd end found','WARNING, separator_pair appears more than twice']

if verb or ierr eq 6 then begin 
    print,'STRUM: error: ',errs[ierr]
    help,xx,ss,join,nix
    help,/trace
    print, ' .con will return "ERROR"'
    stop
endif
if ierr lt 6 then out='ERROR'
goto,done

end
