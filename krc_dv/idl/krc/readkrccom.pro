function readkrccom, arg1,khold
;_Titl READKRCCOM  Read a KRCCOM structure from a type 5x bin5 file 
; arg1  in. Either String or integer
;           If File name, will open KRC type 5x file and return the front values
;           If negative integer, closes current file and releases I/O unit
;           If positive integer, will return krccom for that case (1-based)
; khold  both. lonarr(5)      Storage of values needed between calls:
;           DO NOT MODIFY   But is reusable with  multiple  khold  arrays 
;             [0] is lun, the logical unit of the bin5 file  0=none open
;             [1] is number of 4-byte words in a case
;             [2] is number of cases in the file
;             [3] is n4krc == number of 4-byte words in  KRCCOM
;             [4] is two-digit KRC version number
;           If arg1 is string, will open new unit
;           Else expects valid open lun. Caller should do the final  free_lun,
; func.	out. If arg1 is string, intarr modified "front" words of KRC file +ncase
;              [0] is the number of 4-byte words in KRCCOM
;              [1] is 1-based index of the dimension with prepend index 
;              [2] is the number of prepended 'planes'
;              [3] is number of seasons
;              [4] is number of cases
;            If arg1 is non-neg integer, Structure KRCCOM for the requested case
;            If an error occurs, returns negative integer
;             -1= Failure to open file     -2= Unexpected IDX in file 
;             -3= Invalid case requested   -4= arg1 of invalid type
;_Desc
; Because KRCCOM contains items of different binary types, only way for IDL to
; access these is to read from file as a structure. 
; Read the file using a structure for KRCCOM; spacing forward as needed
; Code so that could use with more than 1 file at a time; thus, all storage
; must be in arguments, requiring recalc. on each call; time trivial.
;_Use
; Call initially,   q=READKRCCOM('filename',khold) where  khold  may be undefined
; To get a case   kcom=READKRCCOM(Icase,khold)  where khold is unchanged
; If done with file: q=READKRCCOM(-1,khold) or free_lun,khold[0]
;_Calls   DEFINEKRC  GETVERS
;_Hist 2004jul22 Hugh Kieffer
; 2008oct22 HK Complete replacement, including meaning of arguments.
; 2010jan14 HK Add action to free logical unit  Jan 28 add nwkrc to khold
; 2014mar16:28 HK Accomodate R*8 version of KRC
; 2016jun22 HK Khold[4] now 2-dig version number, was DP flag. line adjustments
; 2017mar30 HK Account for possible longer header
;_End       .comp readkrccom

k=size(arg1,/type)            ; word type of first argument 
if k eq 7 then begin          ; open file ;=========================

  if n_elements(khold) lt 3 then lun=0 else lun=khold[0] ; Is lun assigned ?
  if lun eq 0 then get_lun,lun else close,lun            ; Get new lun or reuse

  openr, lun,arg1,error=operr   ; open the binary file
  if operr ne 0 then begin
    print,'READKRCCOM: open ERROR. file--->',arg1
    print,' Error # and message=',operr,' ',strmessage(operr)
    return,-1
  endif

; get bin5 array size
  hlen=512                  ; default size of bin5 header in bytes
  bb=bytarr(hlen)           ; default header
  readu,lun,bb              ; read past the default bin5 header

; code to find header size extracted from  bin5r.pro
  hh=string(bb[0:128])         ; plenty long to include IDL size definition
  i1=strpos(hh,'<<')           ; locate the end of the dimensions
  idim=strmid(hh,0,i1-1)       ; extract the dimensions part
  idi=strcompress(idim)        ; remove extra white space
  idd=long(str_sep(idi,' '))   ; separate into its parts, the FIRST WILL BE NULL
  idd=idd[1:*]                 ; drop the leading null
;n=n_elements(idd)               ; # items in bin5 size specification
  ndim=idd[0]                   ; number of dimensions in array
  ncase=idd[ndim]               ; number of cases
  ityp=idd[ndim+1]              ; word type
  ilen=idd[ndim+3]              ; actual header length
  if ilen gt hlen then begin ; may never occur for type 52
    bb=bytarr(ilen-hlen)        ; additional header
    readu,lun,bb                ; read past it
    print,ilen-hlen,' extended header is',string(bb)
  endif
  dodp=ityp eq 5                ; file is double precision
                                ; KRC files never have  >512 byte header.
  i1=strpos(hh,'>>')            ; locate before version
  vern=GETVERS(strmid(hh,i1+2),ii) ; look for version number
  vrs=(10*ii[0]+ii[1])>20  ; create two-digit version number
; get the first 4 words of array; needed by user but not needed here.
  if dodp then front=dblarr(4) else front=fltarr(4)
  readu,lun,front               ; first 4 words of the array
  front=fix(front)              ; convert to integer
  idx=front[1]                  ; index of dimension with extra planes
;if idx eq 29 then front[1:2]=[4,2] ;  <<<< TEMPORARY Tue Jan 26 22:04:27 PST 2010
;  to read some older-style file
;idx=front[1]
  if idx ne ndim-1 then begin 
    print,'hh=',hh
    print,'front=',front
    message,'Unexpected IDX in file '+arg1,/con
    print,'.com to continue' & stop
    return,-2
  endif
  if dodp then front[0]=2*front[0] ; convert # real words to # 4-byte words
  out=[front,ncase]           ; output the first 4 words and the number of cases
; compute number of real words in a case , which is always the last dimension
  mmm=idd[1]                                           ; size of first dimension
  if ndim gt 1 then for i=2,ndim-1 do mmm=mmm*idd[i]   ; size of last dimension
  if dodp then mmm=2*mmm                               ; number of 4-byte words
  khold=[long(lun),mmm,ncase,front[0],vrs] ; ensure long array; mmm could be big some time

endif else if k lt 4 then begin ; arg is integer case number ===============

  lun=khold[0]                ; current logical unit
  if arg1 lt 0 then begin     ; close the current file and free the logical unit
    free_lun,lun
    khold[0]=0 
    return,0
  endif else if arg1 eq 0 or arg1 gt khold[2] then begin 
    print,'krccom: Invalid case: requested, max = ',arg1,khold[2]
    return,-3 
  endif

  dodp=khold[4] ge 31                  ; flag for R*8 words
  if dodp then begin
    ifro=8*4       ; bytes: 4 KRC indices leading each case
; ifro bytes at start of each case. Defined by tdisk.f
;   nbkrc=khold[3]*8                      ; # bytes in krccom
  endif else begin
    ifro=4*4
;   nbkrc=khold[3]*4                        ; # bytes in krccom
  endelse
  j=512L+(arg1-1)*4*khold[1]+ifro ; bytes before krccom of requested case 
  point_lun,lun,j              ; set pointer to just before krccom for this case
  if dodp then i=8 else i=4
  out=DEFINEKRC('KRC',vrs=khold[4])   ; define the structure
  readu,lun,out                 ; read the structure from file

endif else begin                ; unexpected type =====================
  message,'arg1 of invalid type',/con
  return,-4
endelse

done: if !dbug then stop
return,out
end
