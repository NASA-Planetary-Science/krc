pro bin5, code, name,heds,aa, verb=verb,note=note,quiet=quiet,idx=idx,arc=arc $
,date=date,exc=exc
;_Titl  BIN5  Write/Read numeric binary files with 'standard' header
; code	in.	String. valid are 'Write' or 'Read', case insensitive;
;		  Only the first character required.
;                 If the second character is 'b' (case insensitive), then the
;                 standard extension .bin5' will be appended to the file name.
; name	in.	Full path file name. [optionally without the  .bin5]
; heds	in/out	String. Text for header in the file; any length.
; aa	in/out	Numeric array, any size; NOT structures or strings.
;		For Read, if open error occurs, this will be the error number.
; verb	in_	Integer. Verbosity: If set, will print file name before file
; 	      action. If =2, for Read, will also print file date and any subset.
;	      >= 3, will request confirmation. If no conf., aa is unaltered.
;        For write, =4 asks for confirm only if file already exist, 5 never asks 
;             +10  will print debug values and stop before return
; note  in_	String. Notification of what the array represents. Ignored if
;                verb is not set.  WRITE only
; quiet	in_ 	If set, will not report swap or missing file.  READ only
; idx   in_     lonarr(2) 1st & last index of outer (right-most) index to read
; arc	in/out_	String. File byte architecture
;	     for Read; will report the byte order in the file
;	     for Write, can override the "Native" byte order. (Not recommended)
;            Valid are in Lims section below
; exc   out_    Integer: -=error  0=no action  1=request executed successfully 
; date	out_	String of creation date & time. READ only
;
;_USAGE example
; bin5,'W', file_name, descriptive_text_for_file, numeric_array_to_write
;_bin5,'R', file_name, descriptive_text_from_file, numeric_array_from_file
;_bin5,'Rb', file_name without extension,  header, numeric_array_from_file
;_Desc 
; The header is ASCII and its length is always a multiple of 512-bytes. 
; The first part contains the dimensions of the array as an IDL  SIZE vector,
; followed by the header size, then the date and time. This required 
; section ends with  >>.  After this comes the user-supplied text. 
; The header is blank-filled up to the last 10 or 13 bytes, which are always 
; the byte-architecture used to write the file followed by 'C_END'.
;
; There is a corresponding FORTRAN program  binf5.f 
; If there is an error opening the file, return is a scalar of  operr.
; Will print warning if non-numeric file is read. 
; This routine does byte-swapping on Read if and only if needed.
; Includes warning and required response if new file name is "dangerous"
;
; With advent of 64-bit machines, the architecture word could be 6 [or more?]
; characters long, whereas the prior design of this routine accommodated
; only 5. Rather than use a 5-byte version of the these longer words,
; the space allocated to the architecture word has been increased to 8 bytes.
; For backward compatibility, the extra code has been included to check for 
; 5-byte architecture names.
;_Calls none but normal IDL
;_Lims 
;       big   big  |  lit    lit lit lit    lit
ctype='sparc bigend litend alpha ppc x86 x86_64 ' ; all the types handled,
;            All leading archs must be of same type
postyp0= 12 ; byte position in  types  between last type 1 and first type 2  
; blank would yield position of -1, which is equivalent to type 1 
;_Calls  None beyond RSI
;_Hist  97apr27 Hugh Kieffer  Began development of bin5r and bin5w
; 2004aug03 HK Combine bin5r and bin5w
; 2007apr14 HK Add x86_64 architecture, and verb=4 will stop before return
;              and allow second character of code to control extension
; 2007nov25 HK Add return keyword exc to indicate response to verb=3 query
; 2008mar31 HK Add type: ppc.  And use 1 & 2 for types, rather than 0 & 1.
; 2009sep09 HK Add debug printouts.
; 2010sep17 HK Add keyword idx 
; 2011dec20 HK Fix small bug in setting  exc
; 2012mar10 HK Add print of header for write: verb ge 3
; 2012Jul13 HK Rename endian for clarity:  here--> enhere   file--> enfile
; 2012jul22 HK Some reorganization for clarity in anticipation of bin8
; 2013jan30 HK Make endedness generic by detecting cpu endian
;               Retain the older words in ctype for backward compatibility
; 2013feb20 HK Revise extraction of word type to avoid problem of embedded nulls
; 2013mar01 HK Add notification for write if file exists
; 2015mar10 HK Revise write logic to remove looping for header size
;_End

valid=[1,2,3,4,5,6,9,12,13,14,15] ; all the numeric types
headlen = 512                   ; default (minimum) total length of header
barc=8  & bend=5                ; bytes reserved for architecture and 'C_END'
barc1=5                  ; pre-2007apr number of bytes reserved for architecture
;^^^^^^^^ firmcode

kode =strupcase(strmid(code,0,1)) ; action requested
fame=name                       ; copy input name
if strupcase(strmid(code,1,1)) eq 'B' then fame=fame+'.bin5'; autoextension

if not KEYWORD_SET(verb) then kwv=0 else kwv=verb
dbug= kwv ge 10                 ; set flag to stop at end
kwv=kwv mod 10                  ; move back in to normal range

if not KEYWORD_SET(note) then note=''
headend=barc+bend            ; Number of bytes reserves for architecture+'C_END'
qq=' '                          ; define type
exc=-1                          ; set success flag false
enhere= byte(1L, 0, 1) ne 1     ; extract the first byte of a Long word
enhere=enhere[0]                ; true if CPU is big-endian
if enhere then arch='bigend' else arch='litend'
enfile=enhere                   ; default is no-swap

if kode eq 'R' then goto, read
if kode ne 'W' then message,'Invalid code' ; formal error, no recovery

;=====================================WRITE=================================

siza=size(aa) & md=siza[0]      ; number of dimensions
jt=siza[md+1]                   ; file type code
i=WHERE(valid eq jt,k)          ; check that type is numeric
if k ne 1 then message,'Invalid array type of '+string(jt) ; will halt here

; Look for dangerous characters
; Note, idl (or Linux) will not allow a null file name
danger=[' ','$','//','''']      ; dangerous characters in a file name, last is '
j=0                             ; error count
for i=0,N_ELEMENTS(danger)-1 do begin
    k=STRPOS(fame,danger(i))    ; is this character in the users file name?
    if k ge 0 then begin        ;  yup
        j=j+1                   ; increment error count
        print,'DANGER: found -->'+danger(i)+'<-- in file name at location ',k
    endif
endfor
if j ne 0 then begin            ; at least one dangerous character found
    print,'Requested file name is -->'+fame+'<--'
    ss='I want it'              ; intentionally difficult string
    read,qq,prompt=' Enter precisely -->'+ss+'<-- to proceed: '
    if qq ne ss then begin       ; compare response
        print,'No confirmation, BIN5 quitting'
        exc=0
        goto, done 
    endif else print,'OK, you asked for it!'
endif
if kwv ge 1 then print,'Will write '+note+' file: ',fame,' Size= ',strtrim(siza,2)

j=0
if kwv ge 3 and kwv lt 5 then begin  ; check for pre-existence
    ss=findfile(fame,count=j)
    if j ne 0 then print,'FILE EXISTS'
endif

if kwv eq 3 or (kwv eq 4 and j eq 1) then begin ; ask for confirmation
    print,'bin5 HEAD=',heds
    read,prompt='Enter 19 to confirm ',qq 
    if qq ne '19' then  begin
        print,'No confirmation, BIN5 quitting'
        exc=0
        goto, done 
    endif
endif

OPENW,iod,fame,error=operr,/get_lun	; open the binary output file
if operr ne 0 then begin
	print,'BIN5-W: open ERROR # and file=',operr,fame,strmessage(operr)
        goto, done              ; release iod before return
endif

; Can forecast exactly header length needed
jin=STRLEN(heds)                ; length of input header
hh = ''                         ; construct ASCII header of SIZE and headlen
for i=0,md+2 do hh=hh+' '+strtrim(string(siza[i]),2) ; add dimensions 
jid=STRLEN(hh) ; length of IDL dimensions
date=' <<IDL_SIZE + headlen '+SYSTIME(0)+' >> '
jda=STRLEN(date) ; length of date block
jed=jin+jda+jid+headend  ; total bytes needed except for head_size
; Now everything except the string for the header size is defined
; That will be 4 for jed up to 512-4=' 512' , 5 for jed up to 9728-5 
; and 6 up to 99840-6
if jed le 508 then j=4 else if jed le 9723 then j=5 else j=6
nblocks= 1+ (jed+j-1)/512 	;   compute number of 512-byte blocks required
headlen=512*nblocks             ;   total header length
hh=hh+' '+strtrim(string(headlen),2)+date+heds ; all header text but arch+C_END
j=headlen-STRLEN(hh) ; additional length needed, 
if j lt 0 then message,'Algorithm failure'
;; if j gt 0 then for i=1,j do hh=hh+' '	; append blanks 
if j gt 0 then hh=hh+string(replicate(32B,j))	; append blanks 
if dbug then begin 
   help,jin,jed,nblocks,j
   print,'headTot=',strlen(hh)
endif
   
if keyword_set(arc) then begin  ; request to change type
    i=STRPOS(ctype,arc)         ; look for match to valid type
    if i ge 0 then begin        ; only if user request was valid
        enfile= i lt postyp0    ; true if bigendian
        arch=arc                ; replace type in header
    endif
endif

STRPUT,hh,arch,headlen-headend	; write hardware architecture in fixed location
STRPUT,hh,'C_END',headlen-bend	; write standard "end" to header
bb=byte(hh)                     ; vonvert header to byte 
; help,bb

if enhere eq enfile then WRITEU,iod, bb,aa  $ ; write the header & array
                else WRITEU,iod, bb,SWAP_ENDIAN(aa)
if kwv ne 0  then print,'BIN5 Wrote file ',fame
exc=1                           ; successfull completion
goto,done

read: ;=========================== READ===========================

kwq=KEYWORD_SET(quiet)

GET_LUN,iod			; get a logical unit
OPENR, iod,fame,error=operr	; open the binary file
if operr ne 0 then begin
    if not kwq then begin       ; Default error reporting
        print,'BIN5-R: open ERROR. file--->',fame
        print,' Error # and message=',operr,' ',strmessage(operr)
    endif
    aa=operr
    goto,done
endif

bb=bytarr(headlen)              ; create a dummy buffer for reading header
READU,iod, bb                   ; read first 512-bytes as byte array
hh=string(bb)                   ; convert this to a string
i1=STRPOS(hh,'<<')              ; locate the end of the dimensions
idim=STRMID(hh,0,i1-1)          ; extract the dimensions part
idi=STRCOMPRESS(strtrim(idim,2)) ; remove extra white space
idd=long(STR_SEP(idi,' '))      ; separate into its parts, make integer
n=n_elements(idd) 
ndim=idd[0]                     ; number of dimensions in array
siza=idd[0:ndim+2]              ; SIZE of array

j=siza[siza[0]+1]               ; array type
i=WHERE(valid eq j,k)           ; check that type is numeric
if k ne 1 then Print,'BIN5-R WARNING; Invalid array type of '+string(j)

; can accomodate headers > 512 only if headlen was appended to dimensions
if n eq ndim+4 then begin	; header length was written in header
    hlen=idd[n-1]		; total length of header
    if hlen gt 512 then begin	; should read the rest of the header
        more=hlen-512           ; amount still to be read
        cc=bytarr(more)         ; create array to hold rest of header
        READU,iod, cc           ; read rest of header as a byte array
        bb=[bb,cc]              ; add to first part of header
        headlen=hlen            ; update the header length
        hh=string(bb)           ; convert entire header to a string
    endif
endif
;determine if byte order in file is different than on this cpu
i2=headlen-headend
arc=string(bb[i2:i2+barc-1]) ; extract architecture word
arc=strtrim(arc,2)
;arc=strtrim(STRMID(hh,headlen-headend,barc),2) ; Older version
i=STRPOS(ctype,arc)             ; look for match to valid types
if i lt 0 then begin            ; was not there, try old style 5-byte
;    headend=barc1+bend
;    arc=STRMID(hh,headlen-headend,barc1) ; look for 5-byte architecture word
    arc=strtrim(string(bb[i2:i2+barc1-1]),2) ; extract architecture word
    i=STRPOS(ctype,arc)         ; and a match in the valid set
endif
if i lt 0 then message,'Unrecognized architecture '+arc,/con
enfile=i lt postyp0

i2=STRPOS(hh,'>>') 		; find end of date
i1=i2-21                        ; skip past 'IDL_SIZE + headlen'
date=STRMID(hh,i1,20)           ; extract date/time
heds=STRMID(hh,i2+2,headlen-headend-3-i2) ; extract range that may contain text
heds=STRTRIM(heds)		; and trim trailing blanks

if kwv ge 1 then print,'Will Read '+note+' file: ',fame,' Size= ',strtrim(siza,2)
if kwv ge 2 then print, '  File date= ', date $
	  ,'   Head_length= ',strtrim(strlen(heds),2)
if kwv ge 3 then begin ; ask for confirmation
    read,prompt='Enter 19 to confirm ',qq 
    if qq ne '19' then  begin
        print,'no confirmation, BIN5-R quitting' 
        exc=0  
        goto,done
    endif
endif

if keyword_set(idx) then begin ; request of smaller last index range
    if n_elements(idx) lt 2 then begin ; error
        message,'IDX must have 2 values',/con
        goto,done
    end
    i1=idx[0]>0 ; first row to get
    i2=idx[1]<(siza[ndim]-1) ; last row to get
    if i1 gt i2 then begin  ; error
        message,'IDX [0] must be le [2]'
        goto,done
    end
    if i1 ne 0 then begin       ; must skip first part of array
        siz=siza
        siz[ndim]=i1-1
        aa=MAKE_ARRAY(size=siz) ; create the array
        READU,iod, aa           ; read the part to be skipped
    endif
    siza[ndim]=i2-i1+1          ; size of region to be read 
if kwv ge 2 then print,'Subset to range; ',i1,i2
endif

aa=MAKE_ARRAY(size=siza)        ; create the array
READU,iod, aa			; read the data array
if enhere ne enfile then begin 
    if (not kwq) and kwv then print,'BIN5 R: swapping array of size= ',idi
    aa=SWAP_ENDIAN(aa)
endif
exc=1                           ; successfull completion

done: ;===================== DONE =====================

if dbug then begin
    if code eq 'R' then begin
        help,arc,idd & for i=0,n-1 do print,i,' >',idd(i),'<'
        print,'5r headlen=',headlen
    endif
    print,'here and file=',enhere,enfile
    stop
endif

if N_ELEMENTS(iod) ge 1 then FREE_LUN,iod ; close the file and free up the logical unit
return
end
