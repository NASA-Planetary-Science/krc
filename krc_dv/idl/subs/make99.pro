pro make99, id,prior, hold, maxk=maxk,all=all,get=get,ofile=ofile,comf=comf $
,quiet=quiet
;_Titl MAKE99 Make/print list of user options for a program
; id	in.	String name of target program. usually should be  ptitl  . 
; CALLER SHOULD NOT MODIFY the next 2 arguments:
; prior both. 	Strarr(2) of prior id and string(all). Reads source if changed.
; hold	both.	Strarr of the guide. Stored between calls 
; maxk	in_	Maximum number of kons to store. Default=200; last digit 7=debug
; all	in_	Two meanings: If an integer, then bit-encoded
;		  Completeness: +1 = include ;[ keys.
;	  	                +2 = include ;- keys 
;			        +4 = separate output line for each
;			        +8 = List kons expanded into LaTeX
;                              +16 = Use 2nd lines for full descriptions
;                                    if they start with a ";^"
;	    		Except for +8, these take effect only when the source 
;	    		  file is being read.
;                       If +2 and +4 and not +8, then Print each kon in LaTeX
;		If an Intarr, then this is list of kons to be explained
; get	in_	If set, specific processing for GETP family
; ofile	in_     String name of file for output. must be at least 2 characters
; comf	in_	String name of common-functions file to include
;               or, if flag integer, will use kon99
; quiet in_     Flag, little output to terminal
;_Desc
; Parses source file for an IDL program built to Kieffers standard 'kon' 
; skeleton. Builds an action guide as a string array (one per program) and
; prints that guide to the screen. String array is passed back so that the
; source file does not have to be read again for repeated guide requests.
; Skips lines in the source file before the KON case statement or after the 99:
; Note: the Kon values must begin in the first or second column to distinguish
;   them from other possible interior numeric case statements; those should be 
;   indented so that the colon is >7 spaces in.
;
; Requires that there is only one space between the words  "case kon of".
; Requires that there are no spaces in "kons=[" . Can handle up to 2-line array.
; "begin" and "&" on kon line are not printed 
; Code for first character after the first semi-colon
; ;o = obsolete, do not print ever
; ;- = do not print unless bit2 set
; ;+ = append to current output line text before ;= or ;<   unless bit4, 
;		then use   what follows ;= if that symbol pair exists
; 		or prepend what follows ;< if that symbol pair exists
; ;n = Always use the next line for the comment.
; ;< = (not after ;+) include executable on this line in the guide
; ;[ = include only if bit1 set, then within [ ] unless using LaTeX format;
;       indicates this kon is not normally directly used
;
; For ;+ lines, fuller comments can be made by using additional keys
;   ;=   following material replaces the short form
;   ;<   following material prepends the short form
;
; Proper form of a kon target line is: nnn: <code>  ; Text or ;+ Text
; A kons line should have the form:    nnn: kons=[nn,nn,...,nn] 
;
; Processing sequence
;  Processes the source file whenever  prior  changes.
;   Always makes a  kon  list to check for duplicates
; Style depends upon bit-coded  all.  Primary modes are:
;   Default: Produces a real-time compact guide
;    all=15: One line for each kon
;    all=31: Extended description uses optional 2nd comment line in source file.
;_Liens
; No way to get values of parameter arrays
;_Calls none except normal IDL
;_Requires: Environment variable  IDLTOP
;_Hist 2002jan30 Hugh Kieffer
; 2002jun17 HK Add a space after \at in output
; 2002jul15 HK Require no more than 1 blank before a kon value
; 2003apr29 HK Add capability to look for a use second comment line
; 2003may08 HK Clean up some logic. Better documentation
; 2004      HK Add option for  all  to be a integer array of kons
; 2004aug17 HK Make GETP special processing require a set keyword.
; 2004nov13 HK Improve processing of  ;+ and following line
; 2006oct05 HK Prepend '/' to search string to avoid ambiquity
; 2007mar03 HK Add ability to print all Kon once in LaTeX
; 2007mar30 HK Fix to avoid reconstruction when expanding all kons
; 2007aug16 HK Clarify test for KON99 to cover KON91
; 2008aug20 HK For distributions, allow top IDL level to have no .pro files
; 2010feb24 HK Fix  comf  to utilize string entry
; 2010sep21 HK Minor comment changes
; 2013nov15 HK Ensure link is defined 
; 2015jul24 HK Add quiet option, usefull for KONLOOPFLOW calls
; 2016jun20 HK Increase maxk default from 200
; 2016nov12 HK Address existing output file of same name
; 2018mar01 HK Avoid duplicate processing of  comf
; 2018sep07 HK Handle the rare case that lowest kon is duplicated
; 2018oct31 HK If I/O error, print current directory, [can't find ./subs/kon99.pro
;_END        .comp make99

; help, id,prior, hold, maxk,all,get,ofile,comf,quiet & print,'prior=',prior

maxd=5                        ; maximum directory descent
;^^^^^^^^^^^^^  firm-code
on_ioerror, bad

errs='' & link=-1               ; ensure defined
if not keyword_set(maxk) then maxk=250 
if not keyword_set(quiet) then quiet=0B 
if n_elements(prior) lt 2 then prior=[';','0'] ; impossible idl routine name
pin=prior[0]                                   ; use only for debugging
dbug=(maxk mod 10) eq 7
doget = keyword_set(get) ; do the special processing to GETP family
nall=n_elements(all)
if nall eq 0 then all=0  ; default is no special actions
if nall le 1 then begin  ; was a bit-encoded control
    bit1=      all     mod 2 eq 1 ;  +1 = include ;[ lines
    bit2=ishft(all,-1) mod 2 eq 1 ;  +2 = include ;- lines
    bit4=ishft(all,-2) mod 2 eq 1 ;  +4 = one item per line (ignore ;+)
    bit8=ishft(all,-3) mod 2 eq 1 ;  +8 = expand the kons lines
    bit6=ishft(all,-4) mod 2 eq 1 ; +16 = Use 2nd comment line if there
    sall=strtrim(all,2)         ; string copy for comparison between calls
endif else begin  ; was a list of kons to be printed
    bit1=0 & bit2=0 & bit4=0 & bit6=0
    bit8=1B
    sall=prior[1]               ; avoid doing construction again
endelse
;;print,'bits',bit1,bit2,bit4,bit8,bit6
buf='' 
if id ne prior[0] or (not bit8 and (sall ne prior[1]) ) then begin 
;;print,'Construction: id,sall=',id,sall
prior=[id,sall]

; ---------------------- start of construction ----------------------
; ------------------------------------------------------------------------
; ------------------------------------------------------------------------
buf2=''
sqo=''''                        ; single quote
tab=string(9B)                  ; horizontal Tab character
hold=strarr(maxk)
kkon=lonarr(maxk)
kh=-1                           ; count of output guide lines
kv=-1                            ; count of kon  values
kd=-1  ; current level below idltop
; Find source file; search progressively lower directories.
; Use of any explicit letters in mask requires the proper level of /* .
; Here, start at top IDL level, and find all .pro files at that level
; and look for a match. If no match, go to next lower. Continue until
; either a match, or no .pro files found at all.
; To work with Distributions, allow for the case where the top IDL level
; may have no .pro files.
top=getenv('IDLTOP') ; top='/home/hkieffer/idl/'
m1='*.pro'                      ; highest IDL source level
m=m1                            ; start there
again: kd=kd+1                  ; increment directory level
q=findfile(top+m,count=nq)
;;print,'m,nq=',m,nq
if nq eq 0 and m ne m1 then begin
    print,' Could not find source for ',id
    print,'IDLTOP and dir level=',idltop,kd
    goto,error
end
i=where( strpos(q,'/'+id+'.pro') ge 0,ni) ; index location of the sought file
if ni lt 1 then begin
    m='*/'+m ; move one level lower in directory tree
    goto,again
end
if ni gt 1 then errs=errs+'Multiple matches>: '+q[i]+' <'

; have match ; open souce code file
srcfile=q[i[0]]
ncomf=n_elements(comf) & kcomf=0 ; code for later multiple additional files

opensource: 
;;print,'file=',srcfile
openr,lun,srcfile,/get_lun
;;print,'lun=',lun
; read down to the top of the kon section 
link=0                          ; count of lines in the source file
readf,lun,buf & link=link+1
skip:  ; read through code until find start of case statement
if eof(lun) then begin
    print,' MAKE99: did not find kon start for ',srcfile
    goto,error
end 
readf,lun,buf & link=link+1
;;print,buf
if strpos(buf,'case kon of') lt 0 then goto,skip

; look for lines that are kon starts
;      if hit 99;, exit the loop
; print that line, parse if possible
next: ; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
readf,lun,buf & link=link+1
if eof(lun) then begin
    errs=errs+'Did not find 99 in '+srcfile
    goto,nomore
end 
use2 = 0B                       ; have not read 2nd line yet
redo: ; come here if buf2 read, but was not a pure comment line
;;print,' buf=',buf
i1=strpos(buf,'=KON99')         ; look for end of case for files using KON9x
if i1 gt 0 then begin ; reached the  else  portion of the case statement
;    if not keyword_set(comf) then goto,nomore
    if kcomf ge ncomf then goto,nomore
    if size(comf,/type ) eq 7 then srcfile=comf else $
                srcfile='./subs/kon99.pro'  ; Lien, relative file
    close,lun ; close the inout file
    kcomf=kcomf+1 ; avoid endless loop
    goto,opensource ; and switch to KON99.pro
endif
i1=strpos(buf,':')              ; : required for a kon line
if i1 lt 0 or i1 gt 6 then goto,next
;;if dbug then print,'i1,2,3=',i1,i2,i3
i2=strpos(buf,';')              ; search for comment character
if i2 ge 0 and i2 lt i1 then goto,next ; : was in a comment
i3=strpos(buf,'[')              ; search for beginning of dimension
if i3 ge 0 and i3 lt i1 then goto,next ; : was in a array index
i3=strpos(buf,sqo)             ; search for start of string
if i3 ge 0 and i3 lt i1 then goto,next ; : was in a string
if dbug then print,' buf=',buf

; Have a potential  kon  value
s1=strmid(buf,0,i1)             ; get kon number from front of string
j1=strpos(s1,tab)
if j1 ge 0 then goto,next       ; had early tab; not allowed for a kon line
skon=strtrim(s1,2)              ; kon value as a string
if dbug then print,'skon=',skon
j=strlen(skon)                  ; number of digits in kon
;help,s1,i1,j1,j
if i1-j gt 2 then goto,next     ; too many leading blanks, probably was a case 
bb=byte(skon)                   ; interpret  kon  as Byte array
if bb[0] lt 45 or bb[0] gt 57 then goto,next ; first character not a digit
; have a kon line. process it...........................................
; j1 and j2 will be the limits of text retained in guide
; j2=79                           ; 80'th column in source file
kl=long(skon)                   ; convert to long integer
if kl eq 99 then goto,nomore    ; kon=99 is beginning of guide section
if kv ge maxk-1 then goto,full   ; no more room for storage
kv=kv+1                           ; increment number of kon's found
kkon[kv]=kl                      ; save the kon number

; Process the rest of the  kon  line

if i2 ge 0 then begin         ; have a comment, look for 1-byte key after the ;
    key=strmid(buf,i2+1,1) 
    if key ne ' ' then i2=i2+1  ; offset start so as to not print key in guide
;    j1=i2+1                     ; after the :
endif else begin
    key='none'
;    j1=i1+1
endelse

if dbug then print,'skon,kl,i2,key=',skon,kl,i2,' >',key,'<'

if key eq 'o' then goto,next   ; skip obsolete section
if key eq '-' and not bit2 then goto,next 
if key eq '[' and not bit1 then goto,next ; skip the [ key

if bit6 or key eq 'n' or key eq 'N' then begin ; look for 2nd line
; will use next line, for coding simplicity, let all the tests execute and fail
    readf,lun,buf2 & link=link+1
    i1=strpos(buf2,';^')
    if i1 ge 0 and i1 lt 3 then begin ; if it is a comment only
        out=strmid(buf2,i1+2) ; use it
        goto,store
    endif else begin 
        use2=1B                 ; flag that this buffer must be processed
        if not bit6 then print,'Missing n line; buf=',buf
    endelse
endif

if bit6 and key eq'-' then begin ; use this comment
    out=strmid(buf,i2+1)        ; use it
    goto, store
endif 

; special treatment of GETP family.  2004aug17 make optional
ubuf=strupcase(buf)
i6 = strpos(ubuf,'GETP')
if doget and i6 gt 0 then begin     ; was a GETP__ system call, extract its description
    i7=strpos(buf,sqo,i6)       ; limits description
    i8=strpos(buf,sqo,i7+1)     ; 
    desc=strmid(buf,i7+1,i8-i7-1) ; description
    i9=strpos(buf,',',i8+1)
    i10=strpos(buf,',',i9+1)
    var=strmid(buf,i9+1,i10-i9-1) ; variable name
    out=' GET '+desc+': '+var
    if bit6 then begin         ; append description
        i1=strpos(buf,';',i10+1)
        out=out+'  '+strmid(buf,i1+2)
        endif
        use2=0B
    goto,store
endif

; check for kons array
i6 = strpos(ubuf,'KONS=[')
if i6 ge 0 then begin           ; extend auto-sequence list
    desc=strmid(buf,i2+1)       ; save the description
    out=''                      ; create null string
    j1=i6-1                      ; starting place on kons= line
addtolist:
    i8=strpos(buf,']')         ; end of list (assumed not >2 lines)
    if i8 ge 0 then begin       ; end of list is on this line
        out=out+strmid(buf,j1,i8-j1+1)
    endif else begin            ; list must continue on next
        i9=strpos(buf,'$')     ; end of list (assumed not >2 lines)
        if i9 lt 0 then message,'no end to   kons=   list'
        out=out+strlowcase(strmid(buf,j1,i9-j1-1)) ; Insure kons= is lower-case
        if use2 then begin
            buf=buf2
            use2=0B
        endif else begin
            readf,lun,buf & link=link+1 ; next line
        endelse
        j1=0 ; start next line at the beginning 
        goto, addtolist
    endelse
    out=out+desc
    goto,store
endif

if i2 ge 0 and key ne '<' then j1=i2+1 else begin
    j1=i1+1                     ; include executable
    i4=strpos(ubuf,' BEGIN ')   ; bypass "begin" and possible "&"
    if i4 ge 0 then begin       ; by replaceing location of :
        i5=strpos(buf,'& ')
        if i5 ge i4+1 and i5 le i4+3 then j1=i5+2 else j1=i4+7
    endif
    out=strtrim(strmid(buf,j1,i2-2-j1)) ; only the executable
    goto,store
endelse

out=strmid(buf,j1)     ; all line after the start

store:
if key eq '+' then begin   ; may append to prior line
    i7=strpos(buf,';=',i2) ; start of full comment 
    i8=strpos(buf,';<',i2) ; start of prepend comment
    if bit4 then begin          ; full output
        if i7 gt 0 then out=strmid(buf,i7+2) else $ ; use all after ;=
        if i8 gt 0 then out=strmid(buf,i8+2)+strmid(buf,j1,i8-j1) ; prepend whats after ;<
    endif else begin            ; append output
        if i7 gt 0 then out=strmid(buf,j1,i7-j1) else $ ;| use whats before 
        if i8 gt 0 then out=strmid(buf,j1,i8-j1)        ;| 2nd control pair
        hold[kh]=hold[kh]+'   '+skon+':'+out 
        goto,next               ; read the next line
    endelse
endif

q=skon+':'+out
if key eq '[' and not bit8 then q='[ '+q+' ]' ; encase guide line in [ ]
if dbug then print,'key,q= >',key,'< ',q
kh=kh+1
hold[kh]=q                      ; store as a line in quide
if not use2 then goto,next      ; read the next line
buf=buf2                        ; move second line buffer into primary line buffer
goto,redo                       ; process this
                       ;.............................................

full: Message,'   WARNING   Out of storage room   WARNING ',/con,/info
nomore:
hold=hold[0:kh] ; prune the holding array
kkon=kkon[0:kv]
; test for duplicate kon cases
ii=sort(kkon)
uu=uniq(kkon[ii])
if uu[0] ne 0 then begin ; rare case not caught by 'del gt 1' test
  print,'Duplicate:',kkon[ii[0]]
  kv=kv-1                       ; decrement required number of uniq
endif
if n_elements(uu) ne kv+1 then begin
    errs=errs+'  Some duplicate kon .... '
    del=uu-shift(uu,1)          ; increment in unique list
    iu=where(del gt 1)          ; where list had duplicates
    print,'Duplicates=',kkon[ii[uu[iu]]] ; the duplicate kon value
endif
free_lun,lun
link=7000 ; indicate done reading

endif  ; -------------------- end of construction -----------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

; print the holding array
n= n_elements(hold) & k2=n-1
if keyword_set(ofile) then ffile=ofile else ffile=''
dot = strlen(ffile) le 1 and not quiet ; output to terminal
dof = strlen(ffile) gt 1 ; output to file
if dof then begin 
  ffile=ffile+strtrim(all[0],2) ; make unique for way called
fagin:  openw,lun,ffile,error=operr,/get_lun
  if operr ne 0 then begin
    message,' file open ERROR',/con 
    print,'ERROR#,file,message:',operr,' , ',ffile,' , ',strmessage(operr)
    cd,current=cwd 
    print,'Current dir =',cwd,' Fix Dir or Remove existing file' 
    read,buf,prompt='CR=tryAgain or append; n=noFile s=stop > '
    if buf eq 's' then stop ; .con will add an s
    if buf ne 'n' then begin  ; append buf to file and try again
      ffile=ffile+buf
      goto,fagin
    endif
    dot=1B
    dof=0B
  endif
endif

if dot then print,'..====================== ',id,' =======================..' 
if dof then printf,lun,'..====================== ',id,' ===================..'
pat1=' '                        ; for line preceeding kons list
pat2='\qi \at '                 ; to begin  each kon line
sspace=['','....','...','..','.','',''] ; alignment, normal index is 1:5
; prepare index of kon values

ii=strpos(hold,':')             ; look for guide lines probably starting with kon

if not bit8 then begin  ; print all kon once
    if bit2 and bit4 then begin ; in LaTeX format
        for k=0,k2 do begin
            i=ii[k]             ; location of ':'
            text=strmid(hold[k],ii[k]+2)
            q=strmid(hold[k],0, ii[k])+sspace[i]+' '+text ; align output
; changes required for LaTeX processing
            i=strpos(q,' &')
            if i ge 0 then q=strmid(q,0,i+1)+'\&'+strmid(q,i+2)
            i=strpos(q,'<')
            if i ge 0 then q=strmid(q,0,i)+'$<$'+strmid(q,i+1)
            i=strpos(q,'>')
            if i ge 0 then q=strmid(q,0,i)+'$>$'+strmid(q,i+1)
            i=strpos(q,'_')
            if i ge 0 then q=strmid(q,0,i)+'\_'+strmid(q,i+1)
            if dot then print,pat2,q 
            if dof then printf,lun,pat2,q
        endfor 
    endif else begin            ; in stored format
        if dot then for k=0,k2 do print,hold[k] ; print guide 
        if dof then for k=0,k2 do printf,lun,hold[k]
    endelse                     ; format
endif else begin ; print all kons. Should have done preceeding call with all=3
skon=strarr(n)                  ; make list of kon as strings
for k=0,k2 do skon[k]=strmid(hold[k],0,ii[k])
if nall lt 2 then begin 
  for k=0,k2 do begin           ; each line in guide
    if strpos(hold[k],'kons=') gt 0 then begin ; a kons line
        qq=hold[k]              ; copy line and remove the expected <
        i=strpos(qq,'<')
        if i gt 0 then qq=strmid(qq,0,i)+strmid(qq,i)
        if dot then print,pat1 
        if dof then printf,lun,pat1 ; before kons line
        i=strpos(qq,' &')       ; make any & compatible with LaTeX
        if i ge 0 then qq=strmid(qq,0,i+1)+' \&'+strmid(qq,i+2)
        if dot then print,qq 
        if dof then printf,lun,qq ; kons line
        i=strpos(hold[k],'[')   ; find start of kons list
        j=strpos(hold[k],']')   ; ended by ]
        jend=j
        if j lt 0 then j=strpos(hold[k],'$') ;or by $ (incomplete)
        qq=strmid(hold[k],i+1,j-i-1) ; extract all kon in kons
        ss=strcompress(qq,/remove_all)
        qq=str_sep(ss,',') ;         qq=strsplit(ss,',',/extract)

        nk=n_elements(qq)       ; number of kon destinations in this line

        if nk gt 0 then for j=0,nk-1 do begin ; for each kon in list
            q=qq[j]             ; one kon from the list
            i=where(skon eq q,ni) & i=i[0] ; find kon in main list
            if ni eq 1 then text=strmid(hold[i],ii[i]+2) $ ; OK
              else if q eq 'konstd' then text='  Standard sequence. See above.' $
              else text='......... missing .........' ; missing
            i=strlen(q)         ; number of characters in this kon number
            q=q+sspace[i]+' '+text & q=q[0] ; align output
; changes required for LaTeX processing
            i=strpos(q,' &')
            if i ge 0 then q=strmid(q,0,i+1)+'\&'+strmid(q,i+2)
            i=strpos(q,'<')
            if i ge 0 then q=strmid(q,0,i)+'$<$'+strmid(q,i+1)
            i=strpos(q,'>')
            if i ge 0 then q=strmid(q,0,i)+'$>$'+strmid(q,i+1)
            i=strpos(q,'_')
            if i ge 0 then q=strmid(q,0,i)+'\_'+strmid(q,i+1)
            if dot then print,pat2,q else printf,lun,pat2,q
        endfor 

    endif                       ; j
  endfor
endif else begin                ; nall >=2
    qq=strtrim(all,2)           ; convert the kons array into string\
    for j=0,nall-1 do begin     ; for each kon in list
            q=qq[j]             ; one kon from the list
            i=where(skon eq q,ni) & i=i[0] ; find kon in main list
            if ni eq 1 then text=strmid(hold[i],ii[i]+2) $ ; OK
              else if q eq 'konstd' then text='  Standard sequence. See above.' $
              else text='......... missing .........' ; missing
            i=strlen(q)         ; number of characters in this kon number
            q=q+sspace[i]+' '+text & q=q[0] ; align output
            if dot then print,q 
            if dof then printf,lun,q
        endfor
    endelse                     ; nall >=2
;;stop
endelse                         ; print all kons

if dof then begin 
    free_lun,lun
    print,' OUTPUT is in >>>>>>>>> ',ffile
endif
if dbug then stop
if strlen(errs) lt 2 then return

error: j=1
print,'>>>>>>>>  MAKE99 error: ',errs
read,j,prompt=' enter 0 to stop, else 1 to return > '
if j eq 0 then stop
return

bad: j=1
print,'MAKE99 read error: line,err_string=',link,!err_string
cd,current=curd                 ; get current directory
print,'Current directory is: ',curd
read,j,prompt='Enter 0 to stop, else 1 to return > '
if j eq 0 then stop
return

end
