function readkrc56, finame, ts,tp,tlay,tlx,tsx,tltit,tstit
;_Titl  READKRC56  Read KRC type 56 bin5 file
; finame in.  String of file name
; ts    out.  fltarr (hours,lats,nseas,ncase), surface kinetic temperature 
; tp    out.  fltarr (hours,lats,nseas,ncase), planetary bolometric temperat.
; tlay  out.  fltarr (nlayers,lats,nseas,ncase), Midnight temp. subsurface layer
; tlx   out.  fltarr (3,nlat,nseas,ncase)   'FROST4','HEATMM','TTA4'
; tsx   out.  fltarr (nseas,5,ncase)  'DJU5','SUBS','TAUD','SUMF','PZREF'
; tltit out.  strarr ID's for first dimension of tlx
; tstit out.  strarr ID's for first dimension of tsx
;        See tdisk.f section 56 for definitions 
; func. out.  Fltarr of L-sub-S corresponding to Jul.Day for the first case.
;                  Returns -1 if file read error.
; Responds to global dbug
;_Desc
; front+krccom is stored at the beginning of each case
;_Calls   BIN5-read
;_Hist 2008oct01 Hugh Kieffer  Derive from readkrc53.pro
; 2009mar02 HK update to new tdisk.f
;_End
stype='56'                      ; This type
ndim=4                          ; [items, lat,x+season,case] # dim. expected
tstit=['DJU5','SUBS','PZREF','TAUD','SUMF'] ; seasonal items in prefix
tltit=['FROST4','HEATMM','TTA4'] ; extra items for each latitude
;^^^^^^^^^^ specific to this type
BIN5,'R',finame,head,aaa 
siz=size(aaa) & print,'readkrc'+stype+' read size = ', siz
if siz[0] eq ndim-1 then begin ; was a single case
    aaa=reform(aaa,siz[1],siz[2],siz[3],1,/over) ;<<< depends on ndim
    siz=size(aaa) 
endif else if siz[0] ne ndim then return,-1	; error opening file
print,'BIN5 head=',head
front=fix(aaa[0:3])             ; tdisk sizes stored as floats
nwkrc=front[0]                  ; # words in KRCCOM
idx  =front[1]                  ; 1-based index of dimension with extra values
ndx  =front[2]                  ; Number of those extra
if idx ne ndim-1 then return,-2      ; prefix not where expected
;^^^^^^^^^^^^ generic for all type 5x
;Type 56:  aaa is: [vectors&items, latitudes, NDX+ seasons, cases]
m1   =siz[1]                    ; 2*N24+N1+3 
nlat =siz[2]                    ; number of latitudes
nseas=siz[ndim-1]-ndx           ; # seasons
ncase=siz[ndim]                 ; each is a case
ppp=reform(aaa[*,*,0:ndx-1,*],m1*nlat*ndx,ncase,/over); extract prefix

nlx=n_elements(tltit)           ; number of extra items for each latitude
nsx=n_elements(tstit)           ; number of extra items for each season
i1=nwkrc+4                      ; start of season array
i2=i1-1+nsx*nseas               ; end " " "
tsx=reform(ppp[i1:i2,*],nseas,nsx,ncase,/over) ; make [seas,item,case]
;; chart,tsx[*,*,0],parti=tstit
aaa=aaa[*,*,ndx:*,*] ; omit the first season[s] containing the bits of KRCCOM
siz=size(aaa) & print,'minus leading season = ', siz
; Reasonable range for number of layers is 10 to 30. Because the difference is
; less than 24, should be able to figure out nhour under the assumption that it
; is a multiple of 24
q=float(m1-20-3)/(2.*24)        ; multiplier of 24 to get nhour
j=round(q)                      ; nearest integer
q=abs(q-j)                      ; test for being close
if q gt 0.25 then message,'Roundoff to get NHOUR large='+string(q)
nhour=24*j                      ; number of "hours"
nlay=m1-2*nhour-nlx             ; number of layers
i1=nhour-1                      ; index needed in separation of first dimension
i2=2*nhour                      ; "
i3=i2+nlay                      ; " index of last of layers
ts  =aaa[    0:i1,  *,*,*]    ; [hour,lat,season,case] 
tp  =aaa[nhour:i2-1,*,*,*]    ; [hour,lat,season,case]
tlay=aaa[   i2:i3-1,*,*,*]    ; [layer,lat,season,cas
tlx =aaa[   i3:*,   *,*,*]    ; [lat item,lat,season,case]
out=reform(tsx[*,1,0])          ; L-sub-s for first case
;; help,ppp,tsx,ts,tp,tlay,tlx
if !dbug then stop
return,out
end
