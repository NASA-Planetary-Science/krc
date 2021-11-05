function strum, sin,xx, join=join, box=box, quiet=quiet, rem=rem, nix=nix
;_Titl  STRUM  Separate or concatonate strings into one using separator.
; sin	in.	String containing items to be unjoined or Strings to be joined.
; xx	in. 	Single character used as separator.
; join	in_ 	If set or +, joins strings; else, separates them.
;               If == -2 then, isolates existing joined string from ss
;                  <= -3 will try to also include 'xxx=' before the STRUM
; box   in_     String that occurs exactly twice and is otherwise unique in sin,
;                which encases a joined strum array. This allows multiple 
;                routines to share the  xx  character for different strum sets. 
; quiet	in_	If set, no message if separation character not found.
; rem	in_	If set, will remove a STRUM entry from ss
;             If join <== -3 then will try to also remove 'xxx=' before the STRUM
; nix	out.	Integer count of number of strings that contained an xx
;		Used only if join is set.
; func.	out. Strings that have been unjoined or joined.
;       If rem was set, returns string cleaned of that STRUM
;       If  join  is negative and rem not set, returns the extracted joined string
;       If error, returns 'ERROR'
;_Desc
; Double separation character is used at both ends of cat-string. 
; The separation character should NOT occur within any string element.
; This methodology allows blanks within string elements.
; E.g., if xx is '|' then a joined string might be:
;    '||one|this is second string|three||'
; Warning; Caller is responsible for uniqness of  xx  and  box .
;_Calls  None beyond standard IDL
;_Hist  2001apr02 Hugh Kieffer
; 2001oct07 HK Add warning if separation character occurs within a string
; 2004aug17 HK Add error print and return
; 2006jan02 HK Add warning if separator pair occurs more than twice in input.
; 2007mar23 HK Add   quiet  option
; 2007apr03 HK Add    rem   option
; 2012nov12 HK Add option to return the joined string with no change
; 2018aug20 HK No error msg if seperator missing. Change IDL routine calls to
; lower-case
; 2019nov17 HK Option to also remove the keyword identifying the strum string
; 2020feb10 HK Clarify the join and rem keywords, simplify the code
; 2020apr05 HK Add the  box  keyword. For logging parameter arrays for diff. tasks
;_Lien  No test on length of  box  for join 
;_End

ierr=0 ; error count
n=n_elements(sin) & if n lt 1 then goto,err1
if n_elements(xx) ne 1 then goto,err2
if strlen(xx) ne 1 then goto,err2
x2=xx+xx                         ; form end expression

if not keyword_set(join) then join=0
if not keyword_set(rem)  then rem=0
out='' ; in case no vector for this xx

if keyword_set(box) and join eq 0 then begin ; find the box
    j=strlen(box)
    i1=strpos(sin,box)
    i2=strpos(sin,box,/reverse_search)
    i3=i2-i1-j                           ; characters between the two box ends
    if i1 lt 0 or i2 lt 0 or i3 lt 4 then goto,err7 ; valid STRUM has 4+ char
    ss=strmid(sin,i1+j,i3) ; excluding the box ends
    i=strpos(ss,box) ; check inside
    if i ge 0 then goto,err7 ; found another box
  endif else ss=sin ; copy the whole input string

if join le -2 or rem then begin ; isolate joined string as is
  i=strpos(ss,x2)               ; look for start
  if i lt 0 then goto,err4      ; this strum not in the input string
  j=strpos(ss,x2,i+2)           ; look for end
  if j lt 0 then goto,err5 
  if join lt -2 then begin                ; look for leading word
    i3=strpos(ss,'=',i,/reverse_search)   ; find = before strum
    i=strpos(ss,' ',i3,/reverse_search)   ; find blank before the =
  endif
  if rem then out=strmid(ss,0,i)+strmid(ss,j+2) $ ; before and after the strum
         else out=strmid(ss,i,j-i+2)              ; extract existing joined items

endif else if join ne 0 then begin ;  set and +,  join strings

  nix=0                         ; count of strings that contain the separator 
  for i=0,n-1 do if strpos(ss[i],xx) ge 0 then nix=nix+1
  out=x2+ss[0]                                     ; double to start
  if n gt 1 then for i=1,n-1 do out=out+xx+ss[i]   ; interior
  out=out+x2                                       ; add double character at end
  if keyword_set(box) then out=box+out+box         ; encase in box
  if nix gt 0 then goto,err3

endif else begin                ; =0, separate strings

  I=strpos(ss,x2)               ; look for start
  if i lt 0 then goto,err4
  j=strpos(ss,x2,i+2)           ; look for end
  if j lt 0 then goto,err5
  out=str_sep(strmid(ss,i+2,j-i-2),xx) ; split the string
  k=strpos(ss,x2,j+2)
  if k ge 0 then goto,err6

endelse

done: return,out

;------------- Error section --------------------------------------------

err8:ierr=ierr+1
err7:ierr=ierr+1
err6:ierr=ierr+1
err5:ierr=ierr+1
err4:ierr=ierr+1
err3:ierr=ierr+1
err2:ierr=ierr+1
err1:ierr=ierr+1
errs=['String not defined','Separator not a single character' $
,'Separator was found in input strings','No ends found' $
,'No 2nd end found','WARNING, separator_pair appears more than twice' $
,'Not exactly 2 boxs']

if ierr eq 6  or not keyword_set(quiet) then begin 
    print,'STRUM: error: ',errs[ierr-1]
    help,sin,ss,xx,join,ierr
    help,/trace
    print, ' .con will return "ERROR" +  '
    stop
endif
out='ERROR'+out
goto,done

end
