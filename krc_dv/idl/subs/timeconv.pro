function timeconv, arg1,  mjd=mjd, fin=fin, form=form
;_Titl  TIMECONV get/convert date and time between many formats
; arg1  in.  Time as a number or a string or absent to get current time
;              (may be array, [N,2] for fin=-2,  or [N] for all others)
;           The value of any 1-character spacer (T,:,-) is ignored upon input,
;             so any character in the right place will do.
;   If the input is numerical scalar, assumes it is Mjd is < 171424 
;   If the input is numerical and dimensionality is 2 or more, and the last 
;     dimension is 2, then assumes the input is type -2.
; mjd   in_ Integer  Bit-encoded: +1 for input, +2 for output
;                If set, for real uses Modified Julian Date; offset from 2000.0
;                For no arg in (current time), default is computer local time;
;                 set +1 for UTC time
; fin   in_ integer  Input  format: default -1 or 0.  Required if arg1 is string
; form  in_ integer  Output format: default -1 or 0.  SEE codes below..
; func.	out. Time as a number or a string: will be array if input was.
;              If form=-2, will be fltarr(nin,2) 
;_Lien If fin is not set, cannot distinguish between single date in form -2
;              and two dates in form -1. Will treat as former. 
;    Does not check if input and output forms specified are identical.
; If 2 digits of year, assumes between 1970 and 2070
;_Desc. Converts from input to master to output. 
;       Master form is double-precision full Julian date
;   For numerical or null input, default output is code=0
;   For string input, default output is double precision MJD
; Use
; Forms, both input and output
;   real:  fjd  Full Julian date  default 
;          MJD (from j2000.0) ; set by /mjd 
;          Either of the above can be as: double, or float day and fraction
;   long:   (input only) seconds since 1 January 1970 UTC.  fin ignored 
;  string:  See non-negative codes below  
; Codes for fin and form:
;          -3:  Seconds since  1970 January 1 zero UTC 
;          -2:  fldarr([N],2) day and fraction of a day
;          -1:  double precision (default for string input). Input may be float.
;           0:  yyyy-mm-ddThh:mm:ss ISO 8601 format;   -,T,: ignored on input
;                or yy-mm-dd     T and all after may be absent on input
;           1:  yyyymonddThh:mm:ss 
;           2:  yyyymonddThhmmss    was isotime 12
;           3:  yyyymmddThh:mm:ss   was isotime 3 as 2 words
;           4:  yyyydddhhmmss.x  most sig. order, Possibly confusing
;           5:  mm/dd/yyyy-hh:mm:ss  COMMON SPREADSHEET form
;           6:  Uhhmmssyyyyddd  Landsat; e.g., LO800U0831172017131LGN00_B1.h5
;                Input with leading directory and/or trailing extension allowed
;           7:  dd-mon-yy[yy]Thh:mm:ss[.f]  COMMON COMMERCIAL form
;           8:  date from .bin5 header , << ... mmm dd hh:mm:ss yyyy ...>>
;           9:  dd/mm/yyyy-hh:mm:ss  PLIEADES spreadsheet
;          10:  monddThh:mm    this and following redundant after a year
;          11:  ddmonhh:mm     Short but Possibly confusing
;          14:  MonddThhmm     redundant after a year  (was isotime 14)
;          15:  yyMonddThhmm   time to minute within century
; Notes:
; systime()  yields e.g. 'Mon Feb 25 09:07:57 2019' in computer time zone
; systime(1) yields e.g. DOUBLE,  Seconds since 1 January 1970 GMT,
;                      resolution effectively about 1 microsec.
; In current era, numerical precision of full Julian date is about 12 microsec.
;
; Replaces 10 Older routines
; b5date2jd: arg1= date from header, but no extra colons. fin=8 
; ddpp:      printed fjd-2450000.d0. Routine discarded
; dmyhms2jd: arg1=dmy+' '+hms, fin=7
; iso2jd:    fin/form= 0  Here does not output alternate separators 
;               or seconds format
; isotime:   omit arg1, was first arg is now form= ; sin is automatic on type
;            mjd=2, No consideration of timezone here. Does not do 'ddmonhhmm'
; jd2datetime:  fin=-1, form=0 .   Here, milliseconds are automatic
; mdy2mjd:   fin=5
; ymd2mjd:   call with fin=0, form=0
; ymd2j2:    word=string(year,mon,day,form='(i4,i2,i2)', use fin=0,/mjd,form=-2
;              then mjd=round(func[1])    Was an unreliable routine.
; ydoy2mjd:  fin=4
;_Hist 2019feb26:mar26 Hugh Kieffer  Replace the above 10 routines:
; 2019may13 HK Add format 15
; 2019may29 HK Current local time based on global env  DELUTCSEC set by INIT
; 2019sep15 HK Fix year bug in input form 15. Modify debug print
;_End                         .comp timeconv

ptitl='timeconv'

mmm=['??-','jan','feb','mar','apr','may','jun','jul','aug','sep' $
,'oct','nov','dec','??+']

cpuz=-8.          ; standard time zone this computer is on
dj0=1721424.0    ; julday(1,1,1,12)  0 AD, switch between MJD and full
dj2=2451545.d0   ; julday(1,1,2000,12,0,0.) 2000jan01 noon GMT
dj7=2440587.5d0  ; julday(1,1,1970, 0,0,0.) 1 January 1970  0 GMT
daysec=86400.d0  ; seconds in a day
;^^^^^^^^^^^ firmcode
; print,dj2-dj7   10957.500

; Objective is to minimize the code.

; if input arg1 is absent, get system time in seconds, convert to FJD
; if input is other string, convert it into integers for year, month, day, hour,
;    minute and int/real seconds, then convert to FJD
; If input is real, convert to FJD (if needed)
; Now have master form, which is FJD
; If output is numeric, convert to desired form
; If output is string, use CALDAT to get parts
;    Construct the desired form

if not keyword_set(mjd) then mjd=0 ; encoded MJD flag
domi=mjd mod 2 eq 1 ; apply to input
domo=mjd ge 2      ; apply to output

siz=size(arg1)  ; size and dimension of input
ndim=siz[0]     ; dimensionality

if siz[siz[0]+2] lt 1 then begin ; no defined argument, make it now
  x=systime(1) ; current UT in seconds past 1970.0 GMT, double
  fj1=double(getenv('DELUTCSEC')) ; computer time-zone delta seconds from UTC
  if fj1 eq 0 then fj1=3600.*cpuz ; firmcode default
  if not domi then x=x+fj1 ; use local time
  arg1=long(x)       ; time in seconds past 1970, as integer
  siz=size(arg1)        ; size and dimension of input
  ndim=siz[0]           ; dimensionality
endif
 
type=siz[ndim+1] ; input word type
nin =siz[ndim+2] ; n_elements input
if keyword_set(fin) then kin=fin else begin
  if type ne 7 then begin                ; input format
    if mjd eq 0 then domi= arg1[0] lt dj0 ; check on MJD from value
    kin=-1                                ; default for double
  endif else kin=0                        ; default for string
endelse

din2= kin eq -2 or (type lt 7 and type ge 4 and ndim gt 1 and siz[ndim] eq 2) ; flag: input is two items each
if din2 then nout=nin/2 else nout=nin          ; 

if keyword_set(form) then kou=form else kou=0 ; output format
if kou ge 0 then outt=strarr(nout) else  $  ; set output array
if kou eq -2 then outt=fltarr(nout,2) else outt=dblarr(nout)

for kk=0,nout-1 do begin ; V=V=V=V=V=V=V=V=V=V= date loop V=V=V=V=V=V=V=V=V=V=
if type le 3 or type ge 12 then begin ; integer, treat as Seconds since 1 January 1970  0 GMT
  fjd=dj7+double(arg1[kk])/daysec ; convert seconds to days
endif else if type lt 7 then begin ; real number
  if din2 then begin  ; input is split pairs
    if nout eq 1 then aaa=double(arg1[0])+double(arg1[1]) $ ; may have been [2]
                 else aaa=double(arg1[kk,0])+double(arg1[kk,1]) ; must be [n,2]
  endif else aaa=double(arg1[kk])
  if domi then fjd=aaa+dj2 else fjd=aaa   ; full JD
endif else begin ; ________________________ string input __________________
  aaa=arg1[kk]
  lin=strlen(aaa)               ; length of word
  mon='-1' ; 3-letter Mon or will be 'doy'. Default is flag to use cmo
  cmo='-1' ; 2-digit month as string. Default is flag to use mon
  cy='-1'  ; flag year as null
  cs='00'  ; default seconds
;  c2=''    ; year in century, only for 15
  case kin of ; which string form
;                    aaa='yyyy-mm-ddThh:mm:ss' or  aaa='yy-mm-dd'
    0: begin & i=strpos(aaa,'-') ; 2 or 4 character year
      cy=strmid(aaa,0,i) & cmo=strmid(aaa,i+1,2) & cd=strmid(aaa,i+4,2)
      ch=strmid(aaa,i+7,2) & cm=strmid(aaa,i+10,2) & cs=strmid(aaa,i+13)
      end
;                    aaa='yyyymonddThh:mm:ss' 
    1: begin & cy=strmid(aaa,0,4) & mon=strmid(aaa,4,3) & cd=strmid(aaa,7,2)
        ch=strmid(aaa,10,2) & cm=strmid(aaa,13,2) & cs=strmid(aaa,16) & end 
;                    aaa='yyyymonddThhmmss'  was 12
    2: begin & cy=strmid(aaa,0,4) & mon=strmid(aaa,4,3) & cd=strmid(aaa,7,2)
    ch=strmid(aaa,10,2) & cm=strmid(aaa,12,2) & cs=strmid(aaa,14) & end
;                   aaa='yyyymmddThh:mm:ss' 
    3: begin & cy=strmid(aaa,0,4) & cmo=strmid(aaa,4,2) & cd=strmid(aaa,6,2)
    ch=strmid(aaa,9,2) & cm=strmid(aaa,12,2) & cs=strmid(aaa,15) & end
 ;                  aaa='yyyydddhhmmss.x'
    4: begin & cy=strmid(aaa,0,4) & mon='doy'& cd=strmid(aaa,4,3) 
    ch=strmid(aaa,7,2) & cm=strmid(aaa,9,2) & cs=strmid(aaa,11) & end
;                    aaa='mm/dd/yyyy-hh:mm:ss'  common spreadsheet 
    5: begin & cy=strmid(aaa,6,4) & cmo=strmid(aaa,0,2) & cd=strmid(aaa,3,2)
    ch=strmid(aaa,11,2) & cm=strmid(aaa,14,2) & cs=strmid(aaa,17) & end
;            aaa='/work1/dk18/blah/LO800U0831172017131LGN00_B6.h5   Landsat
;            or aaa='hhmmssyyyyddd'
    6: begin 
      if lin lt 13 then message,'name too short'
      if lin eq 13 then tim=aaa else begin
        i=strpos(aaa,'/',/reverse_search) ; last directory mark
        k=strpos(aaa,'U',i)               ; always preceeds time
        if k lt 0 or lin-k lt 14 then message,'timeDate improper'
        tim=strmid(aaa,k+1,13)  ; hhmmssyyyyddd
      endelse
      cy=strmid(tim,6,4) & mon='doy' & cd=strmid(tim,10,3) ; cd is doy
      ch=strmid(tim,0,2) & cm=strmid(tim,2,2) & cs=strmid(tim,4,2) & end
;             aaa='dd-mon-yyyyThh:mm:ss.f'  Year may be yy, T may be any char.
      7: begin &  i=strpos(aaa,':') ; 2 or 4 character year
        if i lt 14 then j=2 else j=4 ; length of year
      cy=strmid(aaa,7,j) & mon=strmid(aaa,3,3) & cd=strmid(aaa,0,2)
      ch=strmid(aaa,i-2,2) & cm=strmid(aaa,i+1,2) & cs=strmid(aaa,i+4) & end
;            aaa='Mon Feb 25 07:45:49 2019' with any lead of tail without colon
    8: begin &  i=strpos(aaa,':') 
        cy=strmid(aaa,i+7,4) & mon=strmid(aaa,i-9,3) & cd=strmid(aaa,i-5,2)
        ch=strmid(aaa,i-2,2) & cm=strmid(aaa,i+1,2) & cs=strmid(aaa,i+4) & end
;                    aaa='dd/mm/yyyy-hh:mm:ss'  Plieades spreadsheet 
    9: begin & cy=strmid(aaa,6,4) & cd=strmid(aaa,0,2) & cmo=strmid(aaa,3,2)
    ch=strmid(aaa,11,2) & cm=strmid(aaa,14,2) & cs=strmid(aaa,17) & end
;                    aaa='monddThh:mm'    redundant after a year
    10: begin & mon=strmid(aaa,0,3) & cd=strmid(aaa,3,2)
    ch=strmid(aaa,6,2) & cm=strmid(aaa,9,2) & end
;                    aaa='ddmonhh:mm' 
    11: begin & mon=strmid(aaa,2,3) & cd=strmid(aaa,0,2)
    ch=strmid(aaa,5,2) & cm=strmid(aaa,8,2) & end
;                   aaa='monddThhmm'     redundant after a year 
    14: begin & mon=strmid(aaa,0,3) & cd=strmid(aaa,3,2)
    ch=strmid(aaa,6,2) & cm=strmid(aaa,8,2) & end
    15: begin & cy=strmid(aaa,0,2) & mon=strmid(aaa,2,3) & cd=strmid(aaa,5,2)
    ch=strmid(aaa,8,2) & cm=strmid(aaa,10,2) & end
    else : message,'Invalid fin'
  endcase
 if !dbug then  help,cy,c2,mon,cmo,cd,ch,cm,cs

; convert words to integers

  if cmo ne '-1' then imo=fix(cmo) else begin ; integer month was set, use it
    if mon eq '-1' then stop    ; no month,  logical flaw
    if mon ne 'day' then begin 
      jj= where (strlowcase(mmm) eq strlowcase(mon) )  ; convert mon to integer
      imo=jj[0]
    endif
  endelse
  id=fix(cd)                             ; mon, day
  ih=fix(ch) & im=fix(cm) & fs=float(cs) ; hour, min, second
  if cy eq '-1' then begin     ; was no year, find time within last year
    x= systime(1)              ; now
    fjd=dj7+x/daysec           ; fjd now
    caldat,fjd, jmo,jd,iy      ;  CALDAT  get iy=current year.  jmo,jd dummies
    try=julday(imo,id,iy,ih,im,fs)   ; fjd  if it were in current year
    if try gt fjd then iy=iy-1       ; decrement year
  endif else begin                   ; had 2 or 4 digit year
    i=strlen(cy)                     ; how many digits
    if i lt 4 then begin        ; extend 2-dig to 4-dig year
      j=fix(cy)
      if j lt 70 then cy='20'+cy else cy='19'+cy ; pick century
    endif 
    iy=fix(cy)                  ; convert 4-dig to integer
  endelse
  if !dbug then help,iy,imo,id,ih,im,fs

; There is no case of doy that does not have the year
  if mon eq 'doy' then begin  ; convert doy to date
    day1= julday(1,1,iy,ih,im,fs)         ; jan,1,year,hh,mm,ss
    fjd=day1+double(id-1)                 ; add additional days of year
  endif else fjd=julday(imo,id,iy,ih,im,fs) ; JULDAY  full Julian day as double

endelse ; _________________ end ___ string input __________________
; now have full Julian date.
if fjd gt 2816788.d0 then message,'beyond year 2999'
; To allow debugging, output integers and words named differently than input

if kou lt 0 then begin ;_____________ output numeric  ____________
  if kou le -3 then bbb=(fjd-dj7)*daysec else begin 
    if domo then bbb=fjd-dj2 else bbb=fjd  ; MJD option
    if kou le -2 then begin  ; float day and fraction
      lbb=floor(bbb)
      bb1=float(lbb)
      bb2=float(bbb-double(lbb)) ; fraction of a day
      bbb=[bb1,bb2]              ; 2-element array
    endif
  endelse
endif else begin ; _________________________ convert to string  ____________
  caldat,fjd, jmo,jd,jy,jh,jm,fs ; all long except fs is double
  by=string(jy,form='(i4)')  & y2=strmid(by,2,2) ; year in century
  bmo=string(jmo,form='(i02)') & mob=mmm[jmo]
  muc=strupcase(strmid(mob,0,1))+strmid(mob,1,2) ; First letter capital
  bd=string(jd,form='(i02)')
  bh=string(jh,form='(i02)')
  bm=string(jm,form='(i02)')  
  bs=string(round(fs),form='(i02)') ; integer, required if bs is not last
  jfs=floor(fs)   ; print msec if it does not round to zero +/-1
  msec=round(1000.*(fs-jfs))
  if msec lt 2 or msec gt 998 then ms=bs else ms=bs+'.'+string(msec,form='(i03)')
; help,by,y2,bmo,mob,muc,bd,bh,bm,msec,bs
  if kou eq 4 or kou eq 6 then begin        ; construct day-of-year
    fj1=julday(1,1,jy,jh,jm,fs) 
    kd=round(fjd-fj1+1.)
    doy=string(kd,form='(i03)') 
  endif
  if !dbug ge 5 then  begin 
    print,jy,jmo,-77,jd,jh,jm,fs,form='(6i5,f7.3,f20.6)'
    print,by,bmo,mob,bd,bh,bm,bs,msec,ms, form='(7a5,i5,a5)'
  endif

  case kou of   ; make the string
    0: bbb=by+'-'+bmo+'-'+bd+'T'+bh+':'+bm+':'+ms
    1: bbb=by+mob+bd+'T'+bh+':'+bm+':'+ms
    2: bbb=by+mob+bd+'T'+bh+bm+ms
    3: bbb=by+bmo+bd+'T'+bh+':'+bm+':'+ms
    4: bbb=by+doy+bh+bm+ms
    5: bbb=bmo+'/'+bd+'/'+by+'-'+bh+':'+bm+':'+ms
    6: bbb=bh+bm+bs+by+doy
    7: bbb=bd+'-'+mob+'-'+by+'T'+bh+':'+bm+':'+ms
    8: bbb=mob+' '+bd+' '+bh+':'+bm+':'+bs+' '+by
    9: bbb=bd+'/'+bmo+'/'+by+'-'+bh+':'+bm+':'+ms
    10: bbb=mob+bd+'T'+bh+':'+bm
    11: bbb=bd+mob+bh+':'+bm
    14: bbb=muc+bd+'T'+bh+bm
    15: bbb=y2+muc+bd+'T'+bh+bm
    else: message,'Invalid form'
  endcase
endelse ; ______________________________________________________

if !dbug then begin 
  if kk eq 0 then begin 
    help,arg1,mjd,fin,form,kin,kou,aaa,cy,c2,mon,cmo,cd,ch,cm,cs,iy,imo,id,ih,im,fs,fjd,din2,bbb,outt
    stop
  endif
  print,arg1[kk],' kin,kou=',kin,kou,' din2,mjd=',din2,mjd

  if !dbug ge 5 then begin 
    print,['yyyy','mon','cmo','cd','ch','cm','cf'], form='(7a5)'
    print,cy,mon,cmo,cd,ch,cm,cs, form='(7a5)'
    print,iy,-77,imo,id,ih,im,fs,fjd, form='(6i5,f7.3,f20.6)'
  endif
  stop
endif
if kou eq -2 then outt[kk,*]=bbb else outt[kk]=bbb   ; store one date
endfor ; =A=A=A=A=A=A=A=A=A=A  date loop  =A=A=A=A=A=A=A=A=A=A

if nout eq 1 then if kou ne -2 then outt=outt[0] else outt=reform(outt[0,*])
return,outt
end
