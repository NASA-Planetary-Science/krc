function readkrc54, finame,ttt,itemt, t55=t55
;_Titl  READKRC54  Read KRC type 54 or type 55 bin5 file
; finame in.  String of file name
; ttt    out.  fltarr (nseas,n_item,[nlat,] ncase)  2nd index is variable items
; itemt  out.  Strarr of titles to items in ttt
; t55    in_   Flag. If set, expects Type 55 file, else Type 54
; func.  out.  Fltarr of L-sub-S corresponding to Jul.Day for the first case.
;                  Returns -1 if file read error.
; Responds to global dbug
;_Desc
; front+krccom is stored at the beginning of each case
;_Lims
;_Calls   BIN5-read
;_Hist 2009mar07 HK Derive from readkrc56.pro
;_End

do5=keyword_set(t55) ; True=Type 55;   false =Type 54
tstit=['DJU5'] ; seasonal items in prefix
itemt=['TsurfH1','TsurfH3','TsurfH13','spare','Tbolo1','Tbolo13'$
,'HeatFlow','FrostMass','Tbot'] ; type 55 file items
if do5 then begin 
    stype='55'                  ; This type
    ndim=3                      ; [seasons,x+items,case]  # dim. expected
endif else begin 
    stype='54'                  ; This type
    ndim=4                      ; [seasons, items, x+latitudes, cases]
    itemt=itemt[[0,2,6,7,8]]     ; fewer items that type 55
endelse
;^^^^^^^^^^ specific to this type

BIN5,'R',finame,head,aaa 
siz=size(aaa) & print,'readkrc'+stype+' read size = ', siz
if siz[0] eq ndim-1 then begin 
    if do5 then aaa=reform(aaa,siz[1],siz[2],1,/over) $ ;<<<  depends on ndim
           else aaa=reform(aaa,siz[1],siz[2],siz[3],1,/over) ;<<<
    siz=size(aaa) 
endif else if siz[0] ne ndim then return,-1	; error opening file
print,'BIN5 head=',head
front=fix(aaa[0:3])             ; tdisk sizes stored as floats
nwkrc=front[0]                  ; # words in KRCCOM
idx  =front[1]                  ; 1-based index of dimension with extra values
ndx  =front[2]                  ; Number of those extra
if idx ne ndim-1 then return,-2 ; prefix not where expected
;^^^^^^^^^^^^ generic for all type 5x
nseas=siz[1]                    ; # seasons
ncase=siz[ndim]                 ; each is a case
if do5 then begin
    nit=siz[ndim-1]-ndx         ; # items
    mx=nseas                    ; size of virtual space
    ppp=reform(aaa[*,0:ndx-1,*],mx*ndx,ncase,/over) ; extract prefix
    print,'#  seasons, items, cases=',nseas,nit,ncase 
endif else begin
    nit=siz[2]                  ; # items
    nlat=siz[ndim-1]-ndx        ; # lats
    mx=nseas*nit
    ppp=reform(aaa[*,*,0:ndx-1,*],mx*ndx,ncase,/over) ; extract prefix
    print,'#  seasons, items, latitudes, cases=',nseas,nit,nlat,ncase 
endelse

nsx=n_elements(tstit)           ; number of extra items for each season
nlx=n_elements(itemt)           ; number items for each latitude
if nit ne nlx then message,'Size mismatch'
i1=nwkrc+4                      ; start of season array
i2=i1-1+nsx*nseas               ; end " " "
tsx=reform(ppp[i1:i2,*],nseas,nsx,ncase,/over) ; make [seas,p.item,case]
dju5=tsx[*,0]                   ; PORB convention, base 2440000.
lsubs=float(L_S(dju5-11545.))   ; return is double, convert to single 
if do5 then ttt=aaa[*,ndx:*,*] else ttt= aaa[*,*,ndx:*,*] ; without prefix

if !dbug then stop
return,lsubs
end
