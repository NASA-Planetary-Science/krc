function readkrc1, file,fcom,icom,lcom, lsubs,ttou, vern=vern,ktype=ktype $
   ,maxs=maxs,verb=verb, dates=dates, rtime=rtime, ddd=ddd,lab=lab,desc=desc
;_Titl   READKRC1  Read KRC direct access files
; file	in.	String. file path name
; fcom	out	KRCCOM floating values, first season See KRCCOMLAB.pro
; icom	out	KRCCOM integer  values, first season
; lcom	out	KRCCOM logical  values, first season
; lsubs	out	fltarr(seasons) L_sub_s. For type -1 computed here assuming Mars
; ttou	out	fldarr(hour, lat, 1 to 3, season]) Temperatures defined 
;                 0]= Surface kinetic
;                 1]= Planetary bolometric   2]= Atmosphere kinetic 
; vern both_     Integer: 3-digit KRC version that wrote the file. 
;        in_  Default=342
;                 Logic here changes at: 
;       200: by 2012nov22 had  MAXN5 =161
;       311: 2014mar16  V3.1.1 double-precision version. Order in KRCCOM changed
;          321 is last that had  MAXN5 in common
;       341: 2016may20  V3.4.1 Types -1,2,3 redefined  new -2 was old -1
;              prior: logical record contained a season of 1:3 T-sets
;              341+:   1:3 logical records per season
;        361+ Version number in the file. Option for R*4 seasonal records
;       out_ will be reset only if value in file is 361:400
; ktype	in_	Integer: KRC disk file kode  K4OUT. Default is 0
;     -1,2,3: TSF[,TSP,[TA]]  With the first record being KRCCOM+filler
;      0: KRCCOM+LATCOM
;     +n: Not handled here! (n<50)= KRCCOM+DAYCOM  n=50+, see readkrc5-.pro 
; maxs	in_	Max number of seasons to read, Default = all in file.
;                if negative , will return only that 1-based season
; verb	in_	If set, will be verbose
; dates out_    Fltarr(seasons) DJUL at each season; Type -n computed here 
; rtime out_  String  Runtime from DAYTIM in KRCCOM
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
; 2018oct26 HK Handle both R*8 and R*4 seasonal records, use 3-digit vers number
; 2019nov11 HK Add keyword rtime, update vern if it is in file
;_End                .comp readkrc1   

; help,file,fcom,icom,lcom,lsubs,ttou,vern,ktype,maxs,verb,dates,ddd,lab,desc

dv2=5000. ; 2013-09-09 12:00  assumed date of switch between Version 1 and 2
;^^^^^^^^^^^ firmcode

if not keyword_set(vern) then vern=342
if not keyword_set(ktype) then ktype=0
if vern lt 342 and ktype lt 0 then ktype=-2 ; old type -1
krc1=DEFINEKRC('KRC',param,  vern=vern,nword=nwkrc) ; define structure == krccom
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
     nwkrc =param[13-i] ;  ---- 255  Num words in KRCCOM
    maxn4e =param[13]   ; 38
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

mint=1                          ; logical records per season, default
if ktype lt 0 then begin        ; type -n
  nwtot=maxnh*maxn4             ; words in a temperature-set array
  if vern lt 340 then begin     ; old type -1
    mint=2                      ; T-sets per season, default
    nwtot=2*nwtot               ; two T-set/record
  endif else begin              ; one T-set/record
    mint=(1>(-ktype))<3         ; records / season          
  endelse
  ihead=1                       ; prepended records
endif else begin                ; type 0
  nwlat= (8+ 3*maxn1 + 2*maxnh)*maxn4 +maxn4e/2 ; size of latcom in  words
  nwtot=nwkrc+nwlat
  ihead=0
endelse

; Read the first krccom, holds for types -n, 0 and +<50
readu,lun1,krc1                 ;### READ KRCCOM structure
fcom=krc1.fd                    ; return KRCCOM from file
icom=krc1.id                    ;  "
lcom=krc1.ld                    ;  "
rtime=string(krc1.daytim)       ; " run date/time

nhour=icom[5] & lasth=nhour-1   ; N24
nlat =icom[3] & lastlat=nlat-1  ; N4
if vrb then print,'nhour & nlat=',nhour,nlat
fout=reform([krc1.alat[0:lastlat],krc1.elev[0:lastlat]],nlat,2) ; lat and elevation

ftype=icom[17-1] ; K4OUT in the file; expect 0 or negative
kver=vern        ; default is to use input version, or the firm-code default)
k =icom[24]      ; ID25 is version number for 361+
if k ge 361 and k lt 400 then begin
  kver=k                        ; Use the version in the file
  vern=k                        ; reset the keyword
endif
if kver lt 340 and ftype eq -1 then ftype = -2 ; override old .tm1 type
if ftype ne ktype then begin
 message,'Type in file not as requested',/con
 help,kver,ktype,ftype,mint 
; if mint ne ktype then stop
endif

if kver lt 310 then nbyt=4 else nbyt=8 ; set real word length for first record
; Version 361 and later, option for R*4 seasons; ID24 =4 is R*4, else R*8
if kver ge 360 and icom[23] eq 4 then nbyt=4 
nrecl=nbyt*nwtot                ; expected record length in bytes
frec=float(len)/float(nrecl)    ; should equal an integer unless mixed R*4/R*8
numr=len/nrecl                  ; number of records in file
; if len mod nrecl ne 0 then print,'ALERT, non-integral records',frec
if vrb then print,'File length and # records=',len,frec,numr

; for type -n
fsea=(len-ihead*nrecl)/(float(nwtot)*nbyt); number of seasonal arrays in file
nread=round(fsea)
if fsea-nread ne 0 then print,'ALERT, non-integral season records',fsea

nsea=nread/mint   ; seasons expected based on file size, holds for 0 and -n
if not keyword_set(maxs) then maxs=nsea
if vrb then help,file,vern,ktype,kver,ftype,mint,ihead,nwtot,nbyt,nrecl,len,frec,numr,fsea,nread,nsea

n5=icom[5-1] & jdisk=icom[12-1] ; number and first output season
djul=krc1.fd[41-1] & deljul=krc1.fd[42-1] ; first and delta date
nsx=n5-jdisk+1 ; number of seasons expected based on KRCCOM
nread=(maxs<nsx)<nread ; number of seasons to read
if vrb then Print,'nread,nsea,nsx,maxs,fsea=',nread,nsea,nsx,maxs,fsea
if nsx ne nread then message,'NumSeas disagree',/con
if !dbug ge 2 then stop
; dates and lsubs will be overwritten below for Type 0
if kver lt 200 and djul gt dv2 then begin                  ; assume version 1
  dates=(djul-11545.)+ deljul*(findgen(nread)+(jdisk-1))  ; days from J2000
  lsubs=float(L_S(dates))       ; compute Ls (returns dblarr)
  print,'READKRC1: Assuming -2440000 dates'
endif else begin                ; assume version 2+
  dates=djul+ deljul*(findgen(nread)+(jdisk-1))
;   lsubs=LSMARS(1,dates)        ; uses MJD
;   lsubs=lsubs[*,0]             ; drop the A.U and sub-solar latitude
  lsubs=LSAM(dates,myn,aud)     ; MARS ONLY    uses MJD
endelse

if nbyt eq 8 then begin 
  trec=dblarr(maxnh,maxn4,mint)     ; file array for one season
  ttou=dblarr(nhour,nlat,mint,nsea) ; output array
endif else begin
  trec=fltarr(maxnh,maxn4,mint)
  ttou=fltarr(nhour,nlat,mint,nsea)
endelse

if ktype lt 0 then begin ; ====================== tmx ========================
  point_lun,lun1,nrecl   ; skip rest of first record.

  if  kver lt 340 then begin ; old type -1; TSF and TPF only
    for k=0,nsea-1 do begin                   ; read the seasons
      readu,lun1,trec                         ;### READ ONE SEASON
      ttou[*,*,*,k]=trec[0:lasth,0:lastlat,*] ; transfer defined temperatures
    endfor
 endif else begin ; version 34x or later
;   t-set=fldarr(maxnh,maxn4)  ; final hourly temperature; surf, plan or atm
  for k=0,nsea-1 do begin                     ; loop over seasons
    readu,lun1,trec                           ;### READ ONE SEASON 
    for j=0,mint-1 do  ttou[*,*,j,k]=trec[0:lasth,0:lastlat,j] ; each kind of T
    if !dbug ge 3 then begin 
       CLOT,reform(ttou[*,*,*,k],nhour*nlat,mint),['Ts','Tp','Ta'],locc=1 $
,titl=['Hour * latitude','Temperature','0_Season= '+strtrim(k,2)] 
       print,' s to stop, else continue'
       q=get_kbrd(1)
       if q eq 's' then stop
    endif
  endfor
endelse

endif else if ktype eq 0 then begin  ; ============ type 0 ====================
   plab=['DTM4','TST4','TTS4','TTB4','FROST4','AFRO4']
   mpar=n_elements(plab)        ; number of parameters to be extracted
   if arg_present(lab) then lab=plab
   if arg_present(desc) then desc=[' rms Temp. change on last day' $
       ,'equilibrium temperature' $
       ,'mean surface temperature','mean bottom temperature' $
       ,'frost amount kg/m^2','frost albedo']
   ddd=fltarr(nlat,nread,mpar)       ; to hold latcom extracts
   latc=DEFINEKRC('LAT',nword=nword,vern=kver) ; define structure == latcom

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
