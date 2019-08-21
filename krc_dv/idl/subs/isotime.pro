function isotime, form, tz=tz
;_Titl  ISOTIME  Current date-time now as a string or MJD to .0001
; form in_ Integer. Desired format. Default is 0
;           0:  yyyy-mm-ddThh:mm:ss ISO 8601 (lunar standard) format
;           1:  yyyymondd hh:mm:ss as one word
;           2:  yyyymondd hh:mm:ss as 2 words
;           3:  yyyymmdd  hh:mm:ss as 2 words
;           4:  monddThh:mm        as one word
;           5:  ddmonhh:mm         as one word  Short but POSSIBLY CONFUSING
;          12:  yyyymondd hhmmss   as 2 words 
;          14:  monddThhmm         as one word
;          15:  ddmonhhmm          as one word  Short but POSSIBLY CONFUSING 
;      else= :  MJD to 4 decimal places as one word   Don't use tz 
; tz  in_  Integer in range -11:11, local standard time zone for label
;           Or 'Z' for zero=UTC=Zulu (actually, any string)
;           If present, a timezone will be appended to the last word 
;           otherwise the computer native zone will be used with no notice.
; func.	out. string or strarr:  Current date and time [and time-zone]
;
;_Usage  string=ISOTIME()   Or, e.g for PST, str=ISOTIME(form=3,tz=-8)
;_Calls  MONTH    IDL: SYSTIME  STR_SEP
;_Liens  
;        Time-zone of computer clock is firm-coded to Pacific = UTC-8
;        Does not correct for date wraping out of a month for zone correction
;_Hist 2001oct25   Hugh Kieffer   Original version
; 2008may09 HK incorporate strcompress to fix small error
; 2011sep29 HK include keywords form and  tz , makes  catime.pro  obsolete
;              and account for USA Daylight Savings Time = DST
; 2011nov18 HK Make  form  an optional argument, add forms 4,5,6
; 2015may12 HK Add two forms with no colons; add 10 to index. 
; 2017jun21 HK Add fomr 14
;_End            .comp isotime

cpuz=-8                         ; standard time zone this computer is on
;^^^^^^ firm code

dd=SYSTIME(0)                   ; get the system date/time
ss=STR_SEP(strcompress(dd),' ') ; weekday, mon, day-of-month, time, year 
mm=string(MONTH(ss[1]),format='(i2.2)') ; convert 3-character month to integer
day=ss[2]                       ; day as 1- or 2-character string
;day=strmid(dd,8,2)              ; day as 2-character string 1 may be blank
if strlen(day) lt 2 then day='0'+day ; day as 2-character no-blank string
doz=keyword_set(tz)

if n_params() lt 1 then form=0
if doz then begin               ; incorporate time zone
    j=size(tz,/type)
    if j gt 3 then zi=0 else zi=tz ; time zone as an integer
    shr=string(abs(zi),form='(I2.2)')  ; hours of offset
    if zi lt 0 then ltz='-'+shr else ltz='+'+shr ; as a string
    iday=fix(day)               ; day as integer
    hr=strmid(ss[3],0,2)        ; hour as 2-character string
    ihr= fix(hr)                ; hour as an integer
    mcs=strmid(ss[3],2)         ; :mm:ss
    dst=0                       ; offset from standard time
 ; USA DST starts 2nd Sunday in March to first Sunday in November 
    if mm gt 3 and mm lt 11 then dst=1 else begin ; this month is all DST
        week=['Sun','Mon','Tue','Wed','Thu','Fri','Sat'] ; days of week
        dow=where(week eq ss[1]) & dow=dow[0] ; day of week
        j=iday-dow              ; zero-based days after first Sunday
        if (mm eq 3 and j ge 7) or (mm eq 11 and j lt 0) then dst=1   
    endelse

    delz=zi-(cpuz+dst)          ; account for timezone computer is in
    ihr=ihr+delz                ; hour including the offset
    if ihr lt 0  then begin     ; prior day
        ihr=ihr+24
        iday=iday-1
    endif else if ihr ge 24 then begin ; next day
        ihr=ihr-24
        iday=iday+1
    endif
    day=string(iday,form='(I2.2)')
    hr =string(ihr,form='(I2.2)')
    hcs=hr+mcs                  ; hh:mm:ss; hcs included colons
endif else hcs=ss[3] 
phr=strmid(hcs,0,2) ; pure hour 
pmn=strmid(hcs,3,2) ; pure minute
psc=strmid(hcs,6,2) ; pure seconds
hhmm=phr+pmn        ; hhmm
hms=hhmm+psc        ; hhmmss
case form of
    0:    out= ss[4]+'-'+mm+'-'+day+'T'+hcs ; ISO one-word, the default
    1:    out= ss[4]+ss[1]+day+' '+hcs  ; yyyymondd hh:mm:ss
    2:    out=[ss[4]+ss[1]+day,    hcs] ; " as two words
    3:    out=[ss[4]+mm   +day,    hcs] ; yyymmdd   hh:mm:ss as 2 words
    4:    out=ss[1]+day+'T'+strmid(hcs,0,5) ; monddThh:mm
    5:    out= ss[2]+ss[1]+strmid(hcs,0,5) ;ddmonhh:mm POSSIBLY CONFUSING 
    12:   out=[ss[4]+ss[1]+day, hms] ; yyyymondd hhmmss  as 2 words
    14:   out=ss[1]+day+'T'+hhmm ; monddThhmm
    15:   out= ss[2]+ss[1]+hhmm  ; ddmonhhmm POSSIBLY CONFUSING 
   else:   begin ; requested MJD
    dd=SYSTIME(1)               ; seconds from 1970jan01, double precision
;base=julday(1,1,1970.,0,0,0) ; full JD of computer base time
;b2=julday(1,1,2000.,12,0,0)  ; full JD of J2000
;boff=b2-base
    mjd=dd/86400.d0 - 10957.5d0 ; MJD as double
    out=string(mjd,form='(f9.4)') ; OK until 2027may 19
    end
endcase

if doz then begin ; append time zone
    n=n_elements(out) & n=n-1 ; index of last word of output
    out[n]=out[n]+ltz+':00'
endif

;return,reform(out) ; make scaler if only one element
return,out ; make scaler if only one element

end
