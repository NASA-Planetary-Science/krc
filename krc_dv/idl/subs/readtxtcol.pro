function readtxtcol, name, sp=sp, nskip=nskip,ncol=ncol,mrow=mrow,fill=fill $
,quiet=quiet,cend=cend,ilun=ilun, top=top
;_Titl READTXTCOL Reads a columnar file of ASCII [space,comma,tab]-delimited text
; name	in.  File name
; sp	In.  String for separater type. Valid are W=white, C=comma, T=tab
;		  D=comma, process Double quotes first, then 
;                 replace any interior commas with colons
;		  0[zero] = no separation done     :=:
;		  Default is white space
; nskip	in_  Number of comment lines to skip; -=search for C_END. Default=0
; ncol	in_  Number of columns to read. Default = all sep. by  delimiter
;              If sp='0' ncol is number of characters required to retain line
; mrow	in_  Max number of lines to read (after skip). Default=1000
;               Negative value will be treated as average number of bytes/line
;                and program will compute lines needed for the file.
; fill	in_  String: If set, will fill missing columns with this value with 
;                 leading blank. Else too few cols stops. 
;                 Default is to skip completely blank lines
; quiet	in_  flag. If set, will not report rows with too few columns
; cend  in_  string  Pattern for the last line of comments. Default = C_END
; ilun both  integer, Logical unit to continue reading. Out is the LUN used
;             If in =0 (default) or negative, will open, read and close     
;             If in is 1:99, will get new LUN, open file, read and not close
;                            and return the open lun number 
;             If in is ge 100, assumes file is open. Will read and not close
; top	out_ Strarr(*) each of the initial lines "skipped"
;                If nskip is negative, At most 50 lines will be retained
; func	out. StrArr(row,col). Scalar -1 if IO error before any good lines.
; !dbug: ge 8= help, print nskip and all buffers.  ge 9= stop refore return
;_Desc
; Data may be proceeded by comment lines.
; Blank or too-short lines are skipped
; NOTE, trailing blank lines can cause a fatal error. 
;  The EOF must be in a row containing the number of columns being read.
;_Calls  none
;_Lim
nreq=1                        ; characters required for line not to be skipped
mtop=50                       ; maximum top lines saved when nskip negative 
;_Hist  2002mar11 Hugh Kieffer  Derived from readcol
; 2004mar02 HK make work also for comma or tab de-limited columns 
;		And ad option to return the skipped lines
; 2005jan18 HK Add code to skip blank lines
; 2005mar26 HK Allow rows to have extra words
; 2005jul30 HK Allow no-separation option
; 2006jan29 HK Move nr= early to avoid it being undefined on IOERROR
; 2007aug16 HK Add test to prune xx only if needed
; 2009apr13 HK Add computation of estimated number of rows in file
; 2009sep01 HK Add : as separator, for qgrep routine index 
; 2009dec17 HK Set min non-blank line length to  nreq   rather than ncol 
; 2010jan19 HK Change blank output to value to blank + value of fill
; 2012mar03 HK For sp='0', negative ncol will retain blank lines
; 2013jul23 HK Allow retention of "top" lines even if nskip is negative
; 2016mar08 HK Add the keywords cend and ilun
; 2017feb01 HK Correct actions with ilun that left LUN assigned
; 2017apr04 HK fix typo in above action
;_End                 .comp readtxtcol

if !dbug ge 8 then  help,name,sp,nskip,ncol,mrow,fill,quiet,cend,ilun,top

if not keyword_set(ilun) then ilun=0
if not keyword_set(cend) then cend='C_END'
jlun=ilun                       ; will become the unit to use
if jlun lt 100 then begin 
  openr, jlun, name,error=operr,/get_lun ; get a free logical unit & open file
  if operr ne 0 then begin
    print,'READTXTCOL open ERROR # and file=',operr $
          ,' ',name,' ',strmessage(operr)
    return,operr < 0          ; lun has not been assigned, don't need to free it
  endif
endif
nr = -1L			; index of successful rows
ON_IOERROR, BAD

if not keyword_set(nskip) then nskip=0
if not keyword_set(mrow) then mrow=1000
maxrow=mrow
if mrow lt 0 then begin 
    stat=fstat(jlun)            ; file information
    maxrow=round(stat.size/(-mrow))   ; estimated numnber of rows in file
endif

verb=not keyword_set(quiet)
if not keyword_set(sp) then sp='W' ; default is white space
valid=['W','C','D','T'       ,'0',':']
sepp= [' ',',',',',string(9B),'' ,':']   ; separator
kode=where(valid eq strupcase(sp)) & kode=kode[0] ; separation kode
if kode lt 0 then message,'invalid separater'

dof=keyword_set(fill)           ; fill empty columns
if dof then mpt=' '+strtrim(fill,2)        ; word to use for empty columns

savetop=arg_present(top)  ; if true, return leading comment lines
if savetop then begin 
   i=nskip
   if i lt 1 then i=mtop          ; default for negative nskip
   top=strarr(i)
endif
nm1=long(maxrow-1)		; last allowed read
buf = 'dum'
if !dbug ge 8 then help,nskip
if nskip gt 0 then for i=0,nskip-1 do begin 
    readf, format='(A)',jlun,buf
    if !dbug ge 9 then print,'SKIP',i,' ',buf
    if savetop then top[i]=buf
endfor
kskip=0
if nskip lt 0 then repeat begin 
    readf,format='(A)',jlun,buf 
    if savetop and (kskip lt mtop) then top[kskip]=buf ; save up to mtop lines
    kskip=kskip+1
    i=strpos(buf,cend)
    if !dbug ge 9 then print,'B',kskip,i,' ',buf
endrep until i eq 0 or i eq 1 ; Found C_END (or input cend) at start of line
if savetop and nskip lt 1 then top=top[0:kskip-1]

if !dbug ge 8 then print,'C- ',buf
if verb and kskip gt 0 then help,kskip

if kode eq 4 then begin  ; do no separation
    if keyword_set (ncol) then nreg=0 ; retain blank lines
    ncol=1 
    findcal=0B 
    xx=strarr(maxrow)
endif else begin        ; some separation 
    sep=sepp[kode]      
    findcol= not keyword_set(ncol) ; detect number of columns
    if findcol then ncol=1 else xx=strarr(maxrow,ncol) ; create output array
endelse

point_lun,-jlun,pos             ; remember the position after header

;_________________________________________________________________
while not (eof(jlun) or (nr ge nm1)) do begin ; each line of table
    readf,jlun, buf             ; read additional lines
    if !dbug ge 8 then print,'D- ',buf
    if kode eq 0 then buf=strtrim(strcompress(buf),2) ; reduce each white space to one blank
    if strlen(buf) lt nreq then goto,empty ; skip [nearly] blank lines
    if kode eq 4 then begin     ; do no separation
       nr = nr+1                ; increment row count
       xx[nr]=buf		; store one row
       goto,empty               ; next line
   endif
   if kode eq 2 then begin      ; remove double-quote pairs
       qq=buf                   ; and convert interior coma to colons
       q2=''                    ; will append each part
dagain:
       i=strpos(buf,'"')
       if i ge 0 then begin 
           j=strpos(buf,'"',i+1)
           if j lt 0 then message,'Unpaired "'
           q=strmid(buf,i+1,j-i-1)
           qb=byte(q)
           ii=where(qb eq 44B,qn) ; look for comma's
           if qn gt 0 then begin 
               qb[ii]=58B       ; replace with colon
               q=string(qb)
           endif
           if i eq 0 then buf=q+strmid(buf,j+1) $
                     else buf=strmid(buf,0,i)+q+strmid(buf,j+1)
;           print,buf & stop
           goto,dagain
       endif
    endif  ; ---- kode eq 2
   pp=str_sep(buf,sep)          ; divide string at each separator
   if findcol then begin
       ncol=n_elements(pp)      ; count the number of pieces
       xx=strarr(maxrow,ncol)
       findcol=0B               ; do not do this again
   endif 
   n=n_elements(pp) < ncol
   if n lt ncol then begin
       if dof then begin 
           pp=[pp,replicate(mpt,ncol-n)]
           if verb then begin
       q='Cols expected, found, Rows_done= '+ST0([ncol,n,nr])
               print,'Warning: '+q 
               print,'Buf=',buf
           endif
           n=ncol
       endif else begin
       q='Cols expected, found, Rows_done= '+ST0([ncol,n,nr])
           message,'Quitting; '+q,/con
           print,buf & stop
           goto, done
       endelse
    endif ; ----- n lt ncol

   nr = nr+1                    ; increment row count
   j=n<ncol                     ; ignore extra words
   xx[nr,*]=pp[0:j-1]		; store one row
empty:
endwhile ; ____________________________________________________________
done:
if nr lt maxrow-1 then xx=xx[0:nr,*] ; extract only the part defined

finish: if !dbug ge 9 then stop
if ilun lt 7 then free_lun, jlun  $           ; will close file first
else if ilun lt 100 then ilun=jlun ; return new lun assigned. leave file open
return,xx

bad: print,'READTXTCOL read error,nr,err_string=',nr,!err_string
if nr lt 0 then xx=-1
goto, finish

end
