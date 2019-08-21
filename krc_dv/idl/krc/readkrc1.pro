function readkrc1, file,fcom,icom,lcom, lsubs,ttou, vrs=vrs,ktype=ktype $
   ,maxs=maxs,verb=verb, dates=dates ,ddd=ddd,lab=lab,desc=desc
;_Titl   READKRC1  Read KRC direct access files
; file	in.	String. file path name
; fcom	out	KRCCOM floating values, first season See KRCCOMLAB.pro
; icom	out	KRCCOM integer  values, first season
; lcom	out	KRCCOM logical  values, first season
; lsubs	out	fltarr(seasons) L_sub_s. For type -1 computed here assuming Mars
; ttou	out	fldarr(hour,lat,season, 1 to 3]) Temperatures defined 
;                 0]= Surface kinetic
;                 1]= Planetary bolometric   2]= Atmosphere kinetic 
; vrs   in_     Integer: 2-digit KRC version that wrote the file. Default=34
;                 Logic here changes at: 
;       20: by 2012nov22 had  MAXN5 =161
;       31: 2014mar16  V3.1.1 double-precision version. Order in KRCCOM changed
;          321 is last that had  MAXN5 in common
;       34: 2016may20  V3.4.1 Types -1,2,3 redefined  new -2 was old -1
;              prior: logical record contained a season of 1:3 T-sets
;              34+:   1:3 logical records per season
; ktype	in_	Integer: KRC disk file kode  K4OUT. Default is 0
;     -1,2,3: TSF[,TSP,[TA]]  With the first record being KRCCOM+filler
;      0: KRCCOM+LATCOM
;     +n: Not handled here! (n<50)= KRCCOM+DAYCOM  n=50+, see readkrc5-.pro 
; maxs	in_	Max number of seasons to read, Default = all in file.
;                if negative , will return only that 1-based season
; verb	in_	If set, will be verbose
; dates out_    Fltarr(seasons) DJUL at each season; Type -n computed here 
;----- Next 3 are defined only for ktype =0
; ddd	out_	fltarr[ lats, seasons, 6 items]   Items are:
;  	            0] DTM4   = rms temperature change on last day
;  	            1] TST4   = equilibrium temperature
;  	            2] TTS4   = mean surface temperature for each latitude
;  	            3] TTB4   = mean bottom temperature
;  	            4] FROST4 = frost amount kg/m^2.
;  	            5] AFRO4  = frost albedo.
; lab	out_   strarr(6) Short labels for the items in ddd
; desc	out_   strarr(6) Descriptions " " "
; func.	out.   fltarr (nlat,2)  0]=latitude  1]=elevation used
; 	     If error occurs, will return -1.
; !dbug ge  stops: 1=before return 2=after sizes 3=after each record
;_Calls  DEFINEKRC
;_Lien
; function may not be correct for files before version 34
;_Hist 99dec06 Hugh Kieffer  00jan09 HHK add fclab
; 2002mar02 HK Reorder arguments, many no longer keywords. 
;   Remove common parameter print; now in  KRCCOMLAB
;   Add ktype argument to handle two file types
; 2002jul20 HK Revise to new common size
; 2010apr16 HK Update from readkrc.pro, add !dbug
; 2010sep04 HK Allow negative maxs to return single season
; 2013aug30 HK Assume DJUL date for distinction of KRC Versions
; 2014jan29 HK Add keyword dates. Clarify when this and Ls are calculated here
; 2014apr25 HK Use ktype=-2 to indicate old krc that did not have MAXN5 is in 232
; 2016may24 HK Add keyword VRS to accomodate KRC version 341 and several older
;   For type -n, use the logic from TFAR8.f to detect -1,-2 or -3
;_End                .comp readkrc1   

; help,file,fcom,icom,lcom,lsubs,ttou,vrs,ktype,maxs,verb,dates,ddd,lab,desc

dv2=5000. ; 2013-09-09 12:00   switch date between assumed Version 1 and 2
;^^^^^^^^^^^ firmcode

if not keyword_set(vrs) then vrs=34
if not keyword_set(ktype) then ktype=0
if vrs lt 31 then nbyt=4 else nbyt=8 ; set real word length
if vrs lt 34 and ktype lt 0 then ktype=-2 ; old type -1
krc1=DEFINEKRC('KRC',param,  vrs=vrs,nword=nwkrc) ; define structure == krccom
; definition of  KRCCOM
     maxn1 =param[ 0] ;   30
     maxn2 =param[ 1] ; 1536
     maxn3 =param[ 2] ;   16
     maxn4 =param[ 3] ;   37
     maxn5 =param[ 4] ;  161  unsure when this dropped from  KRCCOM
     i=1              ; however, DEFINE KRC no longer has MAXN5
     maxn6 =param[ 5-i] ;    6
     maxnh =param[ 6-i] ;   48 or 96
    maxbot =param[ 7-i] ;    6
     numfd =param[ 8-i] ;   96
     numid =param[ 9-i] ;   40
     numld =param[10-i] ;   20
    numtit =param[11-i] ;   20
    numday =param[12-i] ;    5
     nwkrc =param[13-i] ;  ---- 255  Num words ( of Nbytes) in KRCCOM

bd=bytarr((numtit+numday)*4) ; length of TITLE + DAYTIM
vrb=keyword_set(verb)           ; verbose flag
operr=0
openr,lun1,file,error=operr,/get_lun     ; ,/F77_UNFORMATTED
if operr ne 0 then begin
    print,' readkrc1: open ERROR. file--->',file
    print,' Error # and message=',operr,' ',strmessage(operr)
    return,-1
endif
status=fstat(lun1)
len=status.size ; length of the file in bytes. 
;--------------------------------------------------------------------
id=lonarr(numid) & ld=lonarr(numld)
;--------------------------------------------------------------------

jpr=1                           ; logical records per season, default
mint=2                          ; T-sets per season, default
if ktype lt 0 then begin        ; type -n
  nwtot=maxnh*maxn4             ; words in a record
  if vrs lt 34 then begin    ; old type -1
    nwtot=2*nwtot               ; two T-set/record
  endif else begin           ; one T-set/record
    mint=(1>(-ktype))<3                
    jpr=mint                           
  endelse
  ihead=1                        ; prepended records
endif else begin                ; type 0
  nwlat= (9+ 3*maxn1 + 2*maxnh) *maxn4 ; size of this latcom in  words
  nwtot=nwkrc+nwlat
  ihead=0
endelse

nrecl=nbyt*nwtot                ; expected record length in bytes
frec=float(len)/float(nrecl)    ; should equal an integer
numr=len/nrecl                  ; number of records
if len mod nrecl ne 0 then print,'ALERT, non-integral records'
if vrb then print,'File length and # records=',len,frec,numr
nread=numr-ihead                 ; number of seasonRecords in the file
nsea=nread/jpr         ; seasons expected based on file size, holds for 0 and -n
if not keyword_set(maxs) then maxs=nsea

; Read the first krccom, holds for types -n, 0 and +<50
readu,lun1,krc1
nhour=krc1.id[5] & lasth=nhour-1      ; N24
nlat=krc1.id[3]  & lastlat=nlat-1     ; N4
k4out=krc1.id[16]                     ; K4OUT should contain file type
if vrb then print,'nhour & nlat=',nhour,nlat
fout=reform([krc1.alat[0:lastlat],krc1.elev[0:lastlat]],nlat,2) ; lat and elevation
fcom=krc1.fd                    ; return first season
icom=krc1.id                    ;  "
lcom=krc1.ld                    ; "
ftype=icom[17-1] ; K4OUT in the file
if vrb then help,vrs,ktype,ftype,mint,jpr,ihead,nbyt,nwtot,nrecl,len,numr,nread,nsea
if vrs lt 34 and ftype eq -1 then ftype = -2 ; override old .tm1 type
if ftype ne ktype then begin
 message,'Type in file not as requested',/con
 help,vrs,ktype,ftype,mint 
 if mint ne ktype then stop
 endif

n5=krc1.id[5-1] & jdisk=krc1.id[12-1] ; number and first output season
djul=krc1.fd[41-1] & deljul=krc1.fd[42-1] ; first and delta date
nsx=n5-jdisk+1 ; number of seasons expected based on KRCCOM
nread=(maxs<nsx)<nread ; number of seasons to read
Print,'nread,nsea,nsx,maxs=',nread,nsea,nsx,maxs
if nsx ne nread then message,'NumSeas disagree',/con
if !dbug ge 2 then stop
; dates and lsubs will be overwritten below for Type 0
if djul gt dv2 then begin                                 ; assume version 1
  dates=(djul-11545.)+ deljul*(findgen(nread)+(jdisk-1))  ; days from J2000
  lsubs=float(L_S(dates))       ; compute Ls (returns dblarr)
  print,'READKRC1: Assuming -2440000 dates'
endif else begin                ; assume version 2
  dates=djul+ deljul*(findgen(nread)+(jdisk-1))
;   lsubs=LSMARS(1,dates)        ; uses MJD
;   lsubs=lsubs[*,0]             ; drop the A.U and sub-solar latitude
  lsubs=LSAM(dates,myn,aud)     ; uses MJD
endelse

;;ts=fltarr(nhour,nlat,nread)     ; Create arrays just the right size for Tsur
;;tp=fltarr(nhour,nlat,nread)     ;  and Tplan even if read only one

  if nbyt eq 8 then begin 
    ttou=dblarr(nhour,nlat,mint,nsea) ; output array
    trec=dblarr(maxnh,maxn4,mint)      ; array for one logical record
  endif else begin
    ttou=fltarr(nhour,nlat,mint,nsea)
    trec=fltarr(maxnh,maxn4,mint)
  endelse
if ktype lt 0 then begin ; ==============================================
  point_lun,lun1,nrecl   ; skip rest of first record.

  if  vrs lt 34 then begin ; old type -1; TSF and TPF only
    for k=0,nsea-1 do begin                   ; read the seasons
      readu,lun1,trec                         ; read one record
      ttou[*,*,*,k]=trec[0:lasth,0:lastlat,*] ; transfer defined temperatures
    endfor
 endif else begin ; version 34x or later
;   t-set=fldarr(maxnh,maxn4)  ; final hourly temperature; surf, plan or atm
  for k=0,nsea-1 do begin                     ; read the seasons
      readu,lun1,trec                         ; read one record
    for j=0,mint-1 do begin                   ; each kind of temperature
      ttou[*,*,j,k]=trec[0:lasth,0:lastlat,j] ; transfer defined temperatures
    endfor
    if !dbug ge 3 then begin 
       CLOT,reform(ttou[*,*,*,k],nhour*nlat,mint),['Ts','Tp','Ta'],locc=1 $
,titl=['Hour * latitude','Temperature','0_Season= '+strtrim(k,2)] 
       print,' s to stop, else continue'
       q=get_kbrd(1)
       if q eq 's' then stop
    endif
  endfor
endelse

endif else if ktype eq 0 then begin  ; =======================================
   plab=['DTM4','TST4','TTS4','TTB4','FROST4','AFRO4']
   mpar=n_elements(plab)        ; number of parameters to be extracted
   if arg_present(lab) then lab=plab
   if arg_present(desc) then desc=[' rms Temp. change on last day' $
       ,'equilibrium temperature' $
       ,'mean surface temperature','mean bottom temperature' $
       ,'frost amount kg/m^2','frost albedo']
   ddd=fltarr(nlat,nread,mpar)       ; to hold latcom extracts
   latc=DEFINEKRC('LAT',nword=nword,vrs=vrs) ; define structure == latcom

  point_lun,lun1,0  ; start file again
   for k=0,nread-1 do begin ; each record=season
      readu,lun1,krc1 ; read krccom 
      dates[k]=krc1.fd[40]      ; 41-1 extract DJUL
      lsubs[k]=krc1.fd[44]      ; 45-1 and SUBS
      readu,lun1,latc           ; read latcom
         ddd[*,k,0]=latc.dtm4[0:lastlat] ; | where seasonal frosts
         ddd[*,k,1]=latc.tst4[0:lastlat]
         ddd[*,k,2]=latc.tts4[0:lastlat]
         ddd[*,k,3]=latc.ttb4[0:lastlat]
         ddd[*,k,4]=latc.frost4[0:lastlat]
         ddd[*,k,5]=latc.afro4[0:lastlat]
      ttou[*,*,0,k]=latc.tsf[0:lasth,0:lastlat] ; surface kinetic temperature
      ttou[*,*,1,k]=latc.tpf[0:lasth,0:lastlat] ; Planetary temp
   endfor
endif else begin  ; =======================================
    Message,'Not coded for positive KTYPE',/continue
    fout=-1
endelse          ; =======================================

free_lun,lun1
i= nread -nsx ; 
if vrb and i gt 0 then print,'Additional unread records in file =',i
if !dbug ge 1 then stop
return,fout
end
