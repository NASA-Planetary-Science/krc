function readkrc53, finame,ts,tp,tmin,tmax,frost, log=log,jword=jword
;_Titl  READKRC53  Read KRC type 53 bin5 file
; finame in.  String of file name
; ts    out.  fltarr (24 hours,nseas,ncase), surface kinetic temperature 
; tp    out.  fltarr (24 hours,nseas,ncase), planetary bolometric temperature 
; tmin  out.  fltarr (nlayers,nseas,ncase), Minimum temp. of subsurface layers 
; tmax  out.  fltarr (nlayers,nseas,ncase), Maximum temp. of subsurface layers
; frost out.  fltarr (nseas,ncase), Amount of CO2 frost.  at midnight???
; log   in_.  If set, will print a log of changes between cases.
; jword out_  Lonarr(3) of location of krccom as needed by  readkrccom
; func. out.  Fltarr of L-sub-S corresponding to season for the first case.
;             negative integer if a formal error occurs
;_Desc
;_Calls DEFINEKRC  KRCHANGE
; Utility: BIN5read 
;_Lims: Requires that there are 24 hours and 30 layers dimensioned
;   Assumes that delta-season is uniform
;_Hist 2003jun17 Hugh Kieffer  Derive from parts of kbinplot.pro
; 2005may23 HK  A little cleanup
; 2006mar25 HK Modify to use KRCHANGE rather than finding changes here
; 2008oct04 HK Remove depth argument, add jword
;_Lien  value of wpc not tested
;_End

BIN5,'R',finame,text,aaa,/quiet
ssa=size(aaa) & print,'readkrc53 read size = ', ssa
if ssa[0] lt 2 then return,-1	; error opening file
if ssa[1] ne 133 then return,-2 ; required for type 53 file

;The first [2] "seasons" contain the bits of KRCCOM. Thus, Real values 
; (but not Integers or byte=string) can be retrieved by knowing their location. 
; Meaning of first index for the remaining Seasons.
; 0 = Frost amount
; 1:24 = Tsurf
; 25:48 = Tplan
; 49:78 = Tmin
; 79:108 = Tmax
; 109:132 = Asolh
i=where(aaa[1,2:*,0] gt 70.,ns) ; TS for case 0: number of seasons computed
i=where(aaa[1,3,*] gt 70.,ncase) ; TS at season 0;  number of cases computed
i=where(aaa[49:78,3,0] gt 1.,nlay) ;Tmin at seas & case 0; # of layers computed
print,'# seasons, cases, layers =',ns,ncase,nlay
j2=nlay-1                       ; index ranges on layers
wpc=ssa[1]*ssa[2]               ; words per case
jword=[0,wpc,ncase] ; Offset beyond bin5 header, Spacing,# cases 
frost=reform(aaa[    0,   2:ns+1,0:ncase-1])
ts=   aaa[ 1:24,   2:ns+1,0:ncase-1]
tp=   aaa[25:48,   2:ns+1,0:ncase-1]
tmin= aaa[49:49+j2,2:ns+1,0:ncase-1]
tmax= aaa[79:79+j2,2:ns+1,0:ncase-1]

openr,lun,finame,/get_lun       ; reopen KRC output file
bb=bytarr(512)                  ; size of bin5 header
kcom1=DEFINEKRC('KRC',labkf,labki,labkl,idmin,idmax)
readu,lun,bb                    ; read past the bin5 header
readu,lun,kcom1                 ; read the first KRCCOM
free_lun,lun                    ; close the file and release the logical unit

djul=kcom1.fd[40]               ; first date
deljul=kcom1.fd[41]             ; days between seasons
n5=kcom1.id[4]                  ; number of seasons
jdisk=kcom1.id[11]              ; first seasons with disk output

yy=djul+deljul*(jdisk-1+findgen(n5+1-jdisk)) ; KRC dates
yy=2440000.D0+yy                ; Full Julian Date
ls=float(L_S(yy,kode=0))        ; L_s at KRC seasons

if keyword_set(log) then begin ; print log of changes between cases
    sss=KRCHANGE(finame,jword)
    print,'Case=  1 had: '+sss[0]
    for j=1,ncase-1 do  print,'Case=',j+1,' changed: '+sss[j] 
endif                           ; ncase

return,ls
end
