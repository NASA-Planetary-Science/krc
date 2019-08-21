PRO getpints, desc, var,low,high, fmt=fmt
;_Titl  GETPINTS  Interactive change of a list and its length; 
; desc  in.	String: Description to be used as prompt
; var	both.	Array of parameter values: interger, real or string 
;		  If it is undefined upon entry, will create single entry =  1  
; low 	in_	Integer/float: minimum allowed value
; high	in_	Integer/float: max allowed value. If <= low, no limits imposed.
; fmt   in_     Print format to use; e.g., 'I8',  'f6.1' 
;               Defaults: integer='I6', real='g10.3', string='a'
;_Desc
; Actions in response to the prompted input  
;	N should be an integer within the length of the list
;	I should be a value valid for its intended function 
;	x may be anything, its value is ignored
;  N I  Change value of N'th item to I
;         If N is outside current list, no change is made
; +N I  Insert value I before current item N
;         If N is beyond current list, item is appended at the end
; dN x  Delete item N from the list
;         If N is beyond current list, last item is removed
; -1 x  Display current list (to be same as GETP family)
; -2 x  Exit (to be same as GETP family)
;_Hist 2005sep16  Hugh Kieffer  adopted from getpset.pro
; 2006jan05 HK Activate limits
; 2008oct19 HK Allow +large index to append
; 2016sep20 HK Allow real and string values as well as integer
;_End                               .comp getpints        

on_ioerror,bad 
kerr=-2   ; define now in case of I/O error
siz=size(var)
if siz[0] gt 1 then message,'Invalid 2+ D array' $
else if siz[siz[0]+2] eq 0 then begin
  var=[1]                         ; define an integer array
  siz=size(var)
endif
kt=siz[2] ; word type

if n_params() lt 4 or kt eq 7 then begin ; set to no limit test
  low=1
  high=0
endif

if      kt lt 4 then formv='I6' $      ; integer 
else if kt lt 7 then formv='(g10.3)' $ ; real
                else formv='a'         ; string
if n_elements(fmt) eq 1 then formv=fmt ; over-ride defaults

buf=''                          ; define types of input items
c1=['-','+','d','D']            ; valid non-numeric first characters

Print,'GETPINTS: Enter a set of items for: ',desc
print,' N V = replace item N with value V'
print,'+N V = insert new value before current N'
print,'dN x = delete item N'
print,'-1 x = print current list     -2 x = return to caller'

guide:
numv=n_elements(var)             ; length of current list
print,'Index  Value'
for i=0,numv-1 do print,i,var[i], format='(i4,3x,'+formv+')' ; may get I/O error

get: kerr=-1                    ; set to no error 
read,buf,prompt='Enter index and value  > '
b2=strtrim(buf,2)               ; remove exterior blanks
b2=strcompress(b2)              ; compress interior blanks to one
bb=str_sep(b2,' ')              ; separate the parts
if n_elements(bb) ne 2 then goto, halt1

ls=bb[0]                        ; decode action
l1=strmid(ls,0,1)               ; first character
kode=where(c1 eq l1)  & kode=kode[0] ; look for valid control character
if kode ge 0 then begin         ; had some control character
    l2=strmid(ls,1)             ; rest of first word 
    if kode eq 0 then begin     ; was a '-'
        if l2 eq '2' then return else goto,guide
    endif else begin            ; rest of first word should be location
        loc=fix(l2)             ; location in list
    endelse
endif else loc=fix(ls)
;*************************************************
; Now, kode=-1 means first word was simple index, do a replace
;            1 means insert
;            2 or more means delete
;     loc is the integer location in the list
;**********************************************
;; help,kode,loc,numv
vs=bb[1]                        ; second word is the value
if kt eq 1 then val=byte(fix(vs))  $ ; to avoid a byte for each character
     else if kt lt 7 then  val=fix(vs,type=kt) $  ; converts to indicated type
     else val=vs                                  ; keep as string

if kode le -1 then begin        ; simple replace
    if loc ge numv then goto, halt2
;    if nv gt 1 then print,'Warning: only the first value used.'
    if high gt low then $       ; test value for validity
      if val lt low or val gt high then goto,halt3
    var[loc]=val
endif else if kode eq 1 then begin      ; insert before loc
    if high gt low then $       ; test value for validity
      if val lt low or val gt high then goto,halt3
    loc=loc<numv                ; if large location, append to list
    if loc gt 0 then new=[var[0:loc-1],val] else new=val ; part up to  new
    if loc lt numv then new=[new,var[loc:*]] ; part after new
    var=new
endif else begin                ; delete
    if      loc eq 0       then var=var[1:*] $ ; delete first
    else if loc ge (numv-1) then var=var[0:numv-2] $ ; delete last
    else var=[var[0:loc-1],var[loc+1:*]] ; delete interior
endelse
if kode gt 0 then goto,guide    ; redisplay the list if alignment changed
goto,get                        ; do another change

; ERROR Section
halt3: kerr=kerr+1              ; 3 values out of bounds
halt2: kerr=kerr+1              ; 2 index out of bounds
halt1: kerr=kerr+1              ; 1 expect precisely two words
bad:   kerr=kerr+1              ; 0 I/O error
if kerr lt 0 then begin
  print,'ERROR:   format must be bad' 
  help,var,fmt
  print,'Will return with no changes'
  return
endif
serr=['Read error occured; check and try again' $
   , 'expect precisely two words','index out of bounds','value out of bounds']
if kerr ge 0 then print,'ERROR: ',serr[kerr]
if kerr eq 0 then print,'  format is > ',formv
if kerr eq 3 then print,'Valid range is',low,high
goto, guide

end
