;_Titl examt52 Read KRC style 52 files to test low I capability
;_Desc. Reads  KRC type 52
; ttt,tth =[hour,item,lat,season,case] KRC type 52 array 6 items (last is spare)
; ccc,cch=[hour,item,case] ; KRC specific season and latitude, 5 items
; yyy = [Hour,item]  KRC specific case from either ccc or cch
;_Desc
;_Calls  CHART  CLOT  CURVEGUIDE  DEFINEKRC  GETP  GETPAN  GETPINTS  GETPSN
;  HISTFAST  HOPLOT  HSTATS  KON91 KRCHANGE  KRCCOMLAB  MAKEKEYVAL  
;  MEAN_STD2  PAUSE  PRINTJCOLS  PRINTRIL  READKRC52  READKRCCOM  
;  ST0 STRWORD1  VEC2CODE
; Addition calls via kon99:  COLOR24BIT  DELAST0  GRAPH  MAKE99  
;    SETCOLOR  SETWIND  SUBTITLE  TOOTHB
;_Hist 2012mar03 Hugh Kieffer Look at asteroid KRC run output
; 2013jul26 HK COmpare V1 and V2 outputs
; 2015sep14 HK Make sequences for testing Pluto atm runs.

 common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,scex1
;_End                   .rnew examt52

;   .rnew examt52

ptitl='examt52' & prior=['q','q']
dir=getenv('MYHOME')      ; get current home
cpu=getenv('HOST')
solib=getenv('SOLIB')           ; location of shared object library

labf=['DIR for type 52','file','extension','2nd file','spare' ] 
parf=['/work1/krc/','RQ36p','.t52','testib','---']
parf=['/work1/krc/mars/','VH1','.t52','testib','---']
parf=['/work1/krc/mars/','masterA','.t52','master222','---'] ; V1, V2
parf=['/work1/krc/mars/','InsERD1','.t52','---','---'] ; InSight ERD run
parf=['/work1/krc/Tim/','PlutoN2','.t52','---','---'] ; Pluto run


;if dir eq '/u/hkieffer/' then parf[0]='/work/hkieffer/krc/test/' ; Mac
;if cpu ne 'hulk' then parf[[0,5]]='/Users/hugh/home/krc/output/' ; ASU

labi=['Background: 0 or 255','Flag: DJUL is oldstyle' $
,'Jpeg Quality','j4=t54 index','jl=lat index','js=season "','jc=case "' $
,'Linestyle @332','NOPE1= color=case','kode for krccomlab' $
,'Which Ames model','j2=Which t52 index','@38 Use parti','NOPE 1=use Ls for X' $
,'NOPE 10*clip for Chart','@71 print']
pari=[371,0,  80, 2, 0,12,0,2,1,7,2,1,1,0,10,0]

labr=['Tsurf Min.',' " Max','Heat_up Min.',' " Max' $
,'Mass_frost Min.',' " Max','Lsubs Min.',' " Max' $
,'@33,34,38 Tmin  | >=Tmax',' "   "    Tmax  | = auto.' $
,'@33 line thick',' @34 "','@72 "','@52 Tdelta']
parr= [140.,170.,-2, 5., 0., 1000., 0.,360., 180.,280.,1.,1.0,1.0,6.]

labg=['@722 K min',' " Xmax',' " Ymin',' " Ymax'  $
,' Case. Guide X normalized',' " Y 1 norm.',' " Delta Y',' " Line len.' $
,' Item. Guide X normalized',' " Y 1 norm.',' " Delta Y',' " Line len.' ]
Parg=[0., 24., -40., 25.,   0.7, 0.3, -0.03, 0.06,   0.15, 0.32, -0.03, 0.08]

lab52=['Hour','item','Lat.','Season','Case'] ; dimensions of type52

stid=strtrim(indgen(50),2) ; 0-based decimal

kist=[101,103,109,115,117,119,207]; KRCCOM items in n*100+i format

clrr=[255,254,100,200,150,60] & nclr=n_elements(clrr); cases in color
labc=['Base = Case 1','Case 2','case 3','Case 4 & _h','Case 5 & GCM','Case 6']
labl=['Tsurf & 1', 'Tbolo & 2','Tatm & 3','4 & _h',   '5 & GCM',     '6=spare']
thkk=replicate(1.,6)
linn=[0,2,3,4,5,1] & nlin=n_elements(linn)

nitm=5 & litm=nitm-1            ; number of items to retain
hsk=['M','S','I','X','MA','N']  ;  kodes for desired stats
hfmt=['f6.3','f5.3','f8.3','f7.3','f7.3','i6'] ;  blanks will be trimmed
laba=['Time','Tg','T-bol','T-MW','V-FLUX','IR-FLUX']; Ames column titles
des52=['Surface kinetic','TOA Bolometric','Atmosphere','Down-going Vis' $
,'Down-going IR'] ; labels
dj2000=2451545.D0               ; JD of epoch J2000
k24=dj2000-2440000.              ; Offset for KRC before 2013
;===============================================================================
kite=' ' & komit=0   &  paw=-1 ; type definitions
prior=['1','1']                 ;impossible values, to initiate MAKE99
text='dum_text' & ytext='dum_ytext' & text2='dum_up' ; subtitle place holders
lkon=0B
kons=[854,20,21,22,29,251,252,23,255,31,32,27]  ; required for definitions
kon=123 ;  & goto,dokon           ; do them immediately

;===============================================================================
ask: ;---------------------------------------- interactive parameter change
konl=kon
if lkon then begin              ; auto-sequence
    kkon=kkon+1                 ; increment to next item
    kon=kons(kkon)              ; get action from list
    print,'Doing -------------->',kon ; Notice of current action to monitor
    if kkon eq lastkon then lkon=0B ; turn off further auto-processing
endif else begin
    sureask: lkon=0B                 ; forced request for action
    READ, kon, prompt=ptitl+' Enter selection: 99=help 0=stop 123=auto> '
endelse
dokon: kitel=kite               ; remember prior action to use in subtitle
kite=ptitl+'@'+strtrim(kon,2)   ; follows date in subtitle      fils[2]+' '+
nkc=kcc[2] & nkl=kcc[3]         ; in case SETCOLOR was called
case kon of ;...................................................................
; DO NOT USE those defined in KON91::
; -1 -3 -9 100:3 121 122 8 80 85 87 88 801:4 808 850:860 880:899  
; 9 99 991 992 994 995

 0: stop 

110: kons=[854,20,21,22,29,251,252,23,255,431,432,434] ; first file

111: kons=[21,22,29,251,252]    ; reread first file

112: kons=[26,263,266,252,255,27] ; second file

; kons=[26,252,45,51,-1,52,-1,54,-1,61,-1,62,-1,63,-1,64] ; compare years

113: parf=['/work1/krc/mars/','v21ls','.t52','Vwebls','---'] ; V1, Vweb 2013jul27

114: parf=['/work1/krc/mars/','Vwebls','.t52','v22ls','---'] ; V1, V2

;115: begin & parf[1]='syl1a' & end ; Piqueux
115: begin & kons=[20,21,22,29,251,252,23,232,255 $  ; InSight
,431,432,460,461,-1,462,-1,4622,-1,463,-1,464,-1,233,-1,465]
 parf[1]='InsHP3'
end

116: begin & parf[1]='testic' ; set to shallow ice
parf[3]='testid' & parr[13]=28. & end

117: kons=[20,21,22,29,251,252,570,571,572] ; PLuto tests

; 117: clrr=replicate(kcc[9],6 ) ; set all colors to foreground
;..................................................................

123: begin & lkon=1b & kkon=-1  ;- start auto-script 
lastkon=n_elements(kons)-1 & end ; last preset command

11: GETPSN,'File names',parf,labs=labf,/align ;- modify File names parf
14: GETPAN,'pari',pari,-1,999,labs=labi ;- modify integers pari
15: GETPAN,'parr',parr,0.,0.,labs=labr ;- modify floats parr
16: GETPAN,'Plot',parg,0.,0.,labs=labg  ;- modify Plot parg
152: print,'Parr',VEC2CODE(parr) ; vect2code: parr
162: print,'Parg',VEC2CODE(parg) ;+ parg

166: GETPAN,'PsymLinestyle <-9=none: linn',linn,-1,5,labs=labl ;- modify lines
167: GETPAN,'!binc Colors: clrr',clrr,0,255,labs=labc ;- modify colors
168: GETPAN,'Line thick: thkk',thkk,0.,4.,labs=labl ;- modify thicknesses

18: begin & help,ifile,ttt,tth,uuu,vvv,ccc,cch,ccctit,cchtit,yyy,yyytit,ggg,g24 ; Help:
if n_elements(cased) gt 1 then PRINTJCOLS,cased,1,len=65 & end

188: if n_elements(itemv) lt 1 then print,'Need to do @252' else begin ;+ contents
  help,ttt & print,'(hour,item,latitude,season,case)' & print,'itemt = ',tit52
  help,ddd & print,'(layer,item,latitude,season,case)' & print,'itemd = ',itemd
  help,ggg & print,'(item,latitude,season,case)' & print,'itemg = ',itemg
  help,uuu & print,'(nlat,item,case)' & print,'itemu = ',itemu
  help,vvv & print,'(season,item,case)' & print,'itemv = ',itemv
help,kcom,/struct & end

19: begin & i=7 ; Print input portion of selected KRCCOM arrays
GETP,'+1=floats +2=integers +4=logicals',i,0,7
 PRINTRIL ,i,krccom.fd,clabf, krccom.id,clabi,krccom.ld,clabl
end

20: begin                      ; Get KRCCOM structure and definitions
krcstu=DEFINEKRC('KRC',param,labkf,labki,labkl,idmin,idmax) ; full descriptions
fclab=STRWORD1(labkf) ; first word = parameter name   
iclab=STRWORD1(labki)
lclab=STRWORD1(labkl)
numkf=n_elements(labKf) & lastf=numkf-1 ; number defined for input
numki=n_elements(labKi) & lasti=numki-1
numkl=n_elements(labKl) & lastl=numkl-1
end

21: begin & ifile=parf[0]+parf[1]+parf[2]; open file to determine locations of krccom
front=READKRCCOM(ifile,khold) 
print,'khold=',khold
kcom1=READKRCCOM(1,khold) ; get first case
end

221: GETPINTS,'KRCCOM Items',kist,100,320 ; Change KRCCOM List

22: begin ; Get KRC changes
cased=KRCHANGE(khold,/log,list=kist) ; ,kcom1=kcom1)
;print,parf[1]+' Base case= ',cased[0]
cased[0]=parf[1]+' Base' & end

225: cased=ST0([50,200,800],/nojoin); firmcode cases

23: KRCCOMLAB, pari[9],kcom.fd,kcom.id,kcom.ld,fclab,iclab,lclab ;+ Print krccom

232: begin ; Read atm dust and ice in one bin5
BIN5,'Rb','/work1/mars/opacity/THEMIS1yearDustIce',headtaud,tauds,exc=exc,/verb
sizc=size(tauds)                ; size for climate
if exc ne 1 or sizc[0] ne 3 then goto,halt
nucx=sizc[1] & nucy=sizc[2]
dells=360./ nucx & dellat=180./ nucy
lstau=dells*indgen(nucx) +dells/2. ; Ls at each point
end

233: begin & alat=uuu[jl,0,0] ; Extract Tau's for current latitude
fy=(alat+90.)/dellat+.5 ; real index into bins
i=fix(fy) & q=fy-i
taud=(1.-q)*tauds[*,i,0]+ q*tauds[*,i+1,0] ; dust opacity
taui=(1.-q)*tauds[*,i,1]+ q*tauds[*,i+1,1] ; ice opacity
ya=min([taud,taui],max=yb)
plot,lstau,taud,yran=[0.,yb],xtit='Ls',ytit='Opacity at latitude='+ST0(alat) $
,title=' TES and THEMIS opacities averages over several Mars Years'
oplot,lstau,taui,line=2
end

251: begin   & filet=parf[1]    ; Set file 1 name
ifile=parf[0]+filet+parf[2] & end

252: begin  ; Open/Read/Close type 52 file
kcom=READKRC52(ifile,ttt,uuu,vvv,tit52,itemu,itemv,ddd,ggg,itemd,itemg,vern=vern)
help,ttt,uuu,vvv,ddd,ggg,vern,kcom
siz=size(kcom)
if siz[siz[0]+1] ne 8 then goto,halt ; must get a structure
sizt=size(ttt) & nhour=sizt[1] & nlat=sizt[3] & nseas=sizt[4] & ncase=sizt[5]
print,'Nseas, nlat, ncase=',nseas,nlat,ncase
alat=uuu[0:nlat-1,0,0]          ; latitudes for first case
slat=ST0(alat,/nojoin)          ; lats as string
titc=tit52[0:litm]
djmm=vvv[*,0,0]                 ; DJUL
lsv =vvv[*,1,0]                 ; LSUBS
dfl0=(djmm-151.269) mod 686.99161 ; days from Ls=0
tsur=reform(ttt[*,0,0,*,0])       ; Tsurf [hour, season]
hourk=(findgen(nhour)+1)*(24./nhour) ; hours of output
end

255:  begin ; Print date,Ls,sdec  REQ 23 ,252
print,'file and version= ',ifile,'  ',vern 
ii=[40,44,43,42] & print,'kcom: ',MAKEKEYVAL(fclab[ii],kcom.fd[ii])
print,'vvv: date, LS,        ',vvv[0,0:1,0],form='(a,4g12.5)'
q=kcom.fd[40] ; DJUL
ql=LSAM(q,myr,aud); ls and sdec
print,'LSAM: mjd,Ls,DAU,SDec=',q, ql,aud[0:1],form='(a,4g12.5)'
q=q-k24 ; now maybe MJD
ql=LSAM(q,myr,aud) ; ls and sdec
print,'LSAM: mjd,Ls,DAU,SDec=',q, ql,aud[0:1],form='(a,4g12.5)'
end

26: begin & tth=ttt & uuh=uuu & fileh=filet & kcomh=kcom & caseh=cased ; tth=ttt etc.
ifh=ifile & vvh=vvv & lsubh=lsv & ddh=ddd & ggh=ggg & verh=vern & tsh=tsur & end

261: begin & kfile='~/krc/tes/krc.prt'  ; read onePoint output
i=315 & j=359 & mrow=43; j-i+1+2
sss=READTXTCOL(kfile,nskip=-1,ncol=12,mrow=mrow)
ls1=float(sss[*,1]) & ts1=float(sss[*,10])
plot,ls1,ts1,xtit='L3  color=full',ytit='Tsurf',title=kfile
oplot,lsv[i:j],ttt[12,0,0,i:j,0],color=254
end

263: begin   & filet=parf[3]; Read Second type 52 file     DO 26 first
ifile=parf[0]+filet+parf[2] & end

266: help,ifh,ifile,lsubh,lsv $ ; Help latest and hold
,tth,ttt,uuh,uuu,vvh,vvv,ddh,ddd,ggh,ggg,vern

268: begin ; print Ts min/max 
print,'   J   K     Lat.   Elev.  Tmin    Tmax   Case'
;         0   1    0.00   -1.00   191.1   278.7  ALBEDO=0.2 INERTIA=150. 
for j=0,nlat-1 do begin ; each lat/elev 
   for k=0,ncase-1 do begin   ; each case
      xa=min(ttt[*,0,j,*,k],max=xb) 
      print,j,k,uuu[j,*,k],xa,xb,cased[k],form='(2i4,2f8.2,2f8.1,2x,a)' 
endfor & endfor
end

27: begin & mjd=djmm; check Ls agains LSAM
if pari[1] then mjd=djmm-k24 ; adjust older version to j2000
lsam=LSAM(mjd,myn,aud)
plot,mjd,PM180(lsv-lsam),xtit='MJD  '+ifile,ytit='Ls from vvv-LSAM'
end

271: plot,djmm,PM180(lsv-lsubh),xtit='MJD  '+ifile,ytit='lsv-lsabh'; plot LS-LSH REQ 26

28: begin & qd=vvv[*,0]-vvh[*,0] ; delta date  REQ 26
xa=min(qd,max=xb)
 print,'DJul-djul_H  Range=',xa,xb
plot,qd-qd[0] & end

281: begin & th=reform(tth[*,0,0,*,0]) ; Delta Ts  REQ 26 
plot,tsur-tsh,xtit='index  hour*season',ytit='Delta Ts', titl=ifile+'-'+ifh
end

282: begin ; Thigh/Ls  REQ 26
plot,lsv,tsur[12,*],xtit='Ls from vvv',ytit='Tc:H=13',psym=4 
oplot,lsubh,tsh[12,*],psym=6
end

283: begin & dv=reform(ttt[*,3,0,*,0]) ; plot noon down Vis REQ 26
 dvh=reform(tth[*,3,0,*,0])
ya=min([dv[11,*],dvh[11,*]],max=yb)
 plot,dv[11,*],yran=[ya,yb] ; l noon
oplot,dvh[11,*],line=2 
end

29: q= READKRCCOM(-1,khold)     ; close the unit

41:  KRCCOMLAB, pari[9] $ ; difference 2 KRCCOM's
,kcom.fd-kcomh.fd,kcom.id-kcomh.id,kcom.ld-kcomh.ld,fclab,iclab,lclab

42: begin ; CHART difference between KRC models
qq=ccc[*,*,*]-cch[*,*,*]
q2=transpose(qq[*,0:litm,*],[0,2,1])
q3=reform(q2,24*ncase,nitm)
CHART,q3,parti=titc,xtit='Hour * Case',title='Difference @ '+selec
for i=0,ncase-1 do print,'case',i,' = ',cased[i]
end

;432     ttt [hour,item,lat,season,case]
;432     y4  [hour,item,    season,case]  lat=jl=pari[4]
;432     ccc [hour,item,           case]  season=js=pari[5]
;461 y3=Tsur [hour,         season,case]
;462      c2 [hour,                case]
;463      yy [hour,         season,case]  item=itm
;463      yyd[season,2,case]  min/max of item= itm

431: begin & xa=1.1 & read,xa,prompt='Ls desired? > ' ; find season index
q=min(abs(lsv-xa),js) & pari[5]=js & end

432: begin ; ttt --> ccc Extract specific latitude and season
if sizt[0] ne 5 then goto,halt
jl=(pari[4]>0)<(nlat-1) ; lat index
if jl ne pari[4] then print,'WARNING: Lat index constrained'
js=(pari[5]>0)<(nseas-1) ; season index
if js ne pari[5] then print,'WARNING: Season index constrained'
;selec=MAKEKEYVAL([itemu,itemv],[reform(uuu[jl,*,0]),reform(vvv[js,*])])
selec=MAKEKEYVAL([itemu],reform(uuu[jl,*,0]))
y4=reform(ttt[*,0:litm,jl,*,*],/over) ; y4 is [hour,item,season,case]
y4tit=filet+' '+MAKEKEYVAL([itemu],reform(uuu[jl,*,0]))
ccc=reform(y4[*,*,js,*],/over) ; ccc is [hour,item,case]
ccctit=y4tit+' Ls='+string(lsv[js],form='(f5.1)')
print,ccctit & end

433: begin & sizh=size(tth) ;+ tth --> ccc
if sizh[0] ne 5 then goto,halt
jlh=(pari[4]>0)<(sizh[3]-1)
jsh=(pari[5]>0)<(sizh[4]-1)
if jlh ne pari[4] then print,'WARNING: Lat index constrained'
if jsh ne pari[5] then print,'WARNING: Season index constrained'
seleh=MAKEKEYVAL([itemu,itemv],[reform(uuh[jlh,*]),reform(vvh[jsh,*])])
cchtit=fileh+MAKEKEYVAL(['Lat','L_s'],[uuh[jlh,0],vvh[jsh,1]])
cch=reform(tth[*,0:litm,jlh,jsh,*],/over) 
print,seleh & end

434: begin & sizc=size(ccc) ; ccc ---> yyy Extract single case
if sizc[0] eq 2 then ncase=1 else if sizc[0] eq 3 then ncase=sizc[3] $
 else goto,halt
jc=(pari[6]>0)<(ncase-1) 
if jc ne pari[6] then print,'WARNING: Case index constrained'
casey=' '+cased[jc]
yyy=ccc[*,*,jc] & yyytit=ccctit & end ; case description

435: begin & sizc=size(cch) ;+ from cch
if sizc[0] eq 2 then ncase=1 else if sizc[0] eq 3 then ncase=sizc[3] $
 else goto,halt
jc=(pari[6]>0)<(ncase-1) 
if jc ne pari[6] then print,'WARNING: Case index constrained'
casey=' '+caseh[jc]
yyy=cch[*,*,jc]   & yyytit=cchtit & end ; case description

436: begin   & nkc=kcc[2] & nkl=kcc[3]      ; Plot day T's
topt='KRC Diurnal temperatures: '+yyytit+casey
if parr[9] gt parr[8] then yran=parr[8:9] else begin  
    ya=min(yyy,max=yb)
    yran=[ya,yb]
endelse
k33=0                           ; count of overplots that may come
plot,lsv,lsv,xran=[0.,25.],yran=yran,/nodata,color=kcc[8] $ 
,xtit='Hour',ytit='Temperature',titl=topt
thk=parr[10]
for  i=0,2 do begin 
    clr=!binc[clrr[i]]
    HOPLOT,xx,yyy[*,i],linn[i],color=clr,thick=thk
    CURVEGUIDE,i,des52[i],linn[i],locc=parg[8:11],color=clr,thick=thk
endfor
CURVEGUIDE,k33,yyytit,linn[0],locc=parg[4:7],color=!binc[clrr[0]]
print,'title,kon,k33 = ',yyytit,kon,k33
end

437: begin & k33=k33+1 & thk=parr[11];+ Oplot yyy 
for  i=0,2 do HOPLOT,xx,yyy[*,i],linn[i],color=!binc[clrr[i]],thick=thk
CURVEGUIDE,k33,yyytit,linn[0],locc=parg[4:7],color=!binc[clrr[0]],thick=thk
print,'title,kon,k33 = ',yyytit,kon,k33
end
;---------------------------------------------------------------------
441: begin & jc=pari[6] ; Plot Tk for 5 firm-code latitudes
ii=[2,5,9,13,16] ; latitudes
ni=n_elements(ii)
qd=reform(ttt[*,0,ii,*,jc])
qc=qd[*,*,*,0]
qb=transpose(qc,[0,2,1])
qb=reform(qb,nhour*nseas,ni,/over)
CLOT,qb,slat[ii],locc=[.33,.92,-.03,.08] $
,titl=['hour*season','Surface T', parf[1]+' Case='+strtrim(jc,2)+': '+cased[jc]]
end

442: begin & jc=pari[6] ;+ oplot
qd=reform(ttt[*,0,ii,*,jc])
qc=qd[*,*,*,0]
qb=transpose(qc,[0,2,1])
qb=reform(qb,nhour*nseas,ni,/over)
CLOT,qb,slat,locc=[.33,.92,-.03,.08],oplot=-2
end

447: begin  ; read fort79
 sss=readtxtcol('~/krc/tes/fort.79', nskip=0,ncol=5,mrow=2000)
siz=size(sss)
fff=float(sss)
n2=kcom.id[1]                  ; times per day
nj=siz[1]/n2                    ; number of whole days
CHART,fff,parti=['HaVis','TotSurf','DIRECT','DIFFUSE','BOUNCE']
pause,-1
ffr=reform(fff[*,1],n2,nj) & help,ffr
CLOT,ffr,locc=1
oplot,fff[0:n2-1,1],line=2
ya=min(ffr[*,4]-ffr[*,0],max=yb)
print,'Day1,case2-day1; min,max=',ya,yb
end

451: for k=0, ncase-1 do begin ; Oplot all other cases by color
    if k ne jc then begin
        k33=k33+1               ; then oplot all other cases 
        clrk=kkc[k33 mod nkc]   ; color
        for  i=0,2 do  hoplot,xx,ccc[*,i,k],linn[i],color=clrk
        CURVEGUIDE,k33,'Case '+strtrim(k+1,2),0,locc=parg[4:7],color=clrk
    endif
endfor

452: begin  ; Plot Seasonal T's
plot,ttt[*,0,jl,*,jc],/nodata,xtit='Hour & season' $ ; orange
,ytit='Temperature',titl=filet+' Lat index=',jl,color=kcc[8]
for i=0,2 do oplot,ttt[*,i,jl,*,jc],color=!binc[clrr[i]]
end

453: begin & print,ccctit ; Print table for GCM comparison
fmt='(i3,5f10.2)'
print,'Fluxes in W/m^2'
print,'hour Surf_kin TOA_bright T_atm_kin  DownVis    DownIR'
for n=0,nhour-1 do print,n+1,ccc[n,*,jc],format=fmt
end

454: begin & j2=pari[11]; Chart trend for any item/lat
qq=reform(ttt[*,j2,jl,*,*],nhour,nlat,nseas,ncase,/over) ; extract 1 item
q=reform(qq,nhour*nseas,ncase,/over)
qq=tit52[j2]+' @ Lat & Elev = '+ST0(uuu[jl,*])
if parr[9] gt parr[8] then yran=parr[8:9] else yran=0
if pari[12] eq 0 then parti=0 else parti=cased
CHART,q,xtit='hour * season',titl=qq,range=yran,dlin=1,parti=parti
qq=MEAN_STD2(q,/one) & print,'Means = ',qq
end

455: begin & sizc=size(ttt) ; movie through all of ttt T's
for k=0,sizc[5]-1 do begin
    for j=0,sizc[3]-1 do begin 
        plot,ttt[*,0,j,k1:*,k],titl=slat[j]+' '+cased[k]
        PAUSE,-1
     endfor & endfor  & end

460: cases=['I=250. I=500 below .12m, ','uniform I=250','Siegler: I=77 I=500 below .061 m','uniform I=77','I=250/500 Climate Tau','I=250 Climate Tau','I=77/500 Climate Tau','I=77 Clmate Tau'] ; Firmcode case titles (for HP3)

461: begin & cased=cases ; season plot for one lat,
y3=reform(y4[*,0,*,*]) ; y3 is Tsur[hour,season,case]
y2=reform(y3,nhour*nseas,ncase)
CLOT,y2,cases,locc=1,titl=['hour*season','Tsurf',y4tit ]
end

462: begin & itm=0 & read,itm,prompt='Item index for ttt? > '  ; one lat,one season REQ 461
itm=(itm>0)<4  ; ccc is [hour,item,case]
c2=reform(ccc[*,itm,*])  ; c2 is [hour,case]
xx=(findgen(nhour)+1.)*(24./nhour) ; KRC hours
CLOT,c2,cases,xx=xx,locc=1,titl=['hour',tit52[itm],ccctit ]
end

4622: begin & i=ncase/2 ; Delta due to climate versus hour index
 d2=c2[*,i:*]-c2[*,0:i-1]
CLOT,d2,cased[0:ncase/2-1],xx=hourk,locc=[.4,.5,-.03,.1] $
,titl=['hour',tit52[itm]+'  Delta due to Climate Tau',ccctit]
end

463: begin  ; seasonal min/max for all cases  REQ 462
yy=reform(y4[*,itm,*,*]) ; yy is [hour,season,case]
yyd=fltarr(nseas,2,ncase)
for k=0,ncase-1 do begin
   qq=yy[*,*,k]
   for j=0,nseas-1 do begin
      ya=min(qq[*,j],max=yb) ; diurnal min/max
      yyd[j,*,k]=[ya,yb]
   endfor
endfor 
q=['Min T. ','Max T. ']+cased
qq=transpose(yyd,[0,1,2])
CLOT,reform(qq,nseas,2*ncase),[cased,cased],locc=1 $
,titl=['season index','Min and Max for '+tit52[itm],y4tit]
end

464: begin ; annual min.max for one case REQ 463
read,j,k,prompt='2 Case indices? x - for only one > '
j=(j>0)<(ncase-1)
if k lt 0 then begin            ; only one case
   qq=yyd[*,*,j]
   q=['Min T. ','Max T. ']+cased[j]
endif else begin
   k=k<(ncase-1)
   qq=yyd[*,*,[j,k]] ; yyd[season,2,case]
   qq=reform(qq,nseas,4)
   q=['Min T. ','Max T. ']+cased[j]
   q2=['Min T. ','Max T. ']+cased[k]
   q=[q,q2]
endelse
CLOT,qq,q,xx=lsv,locc=1  $
,titl=['season Ls','Min and Max for  '+tit52[itm],y4tit]
end

465: begin & i=ncase/2; Climate effect on T @hour vrs season  REQ 463
dd=yy[*,*,i:*]-yy[*,*,0:i-1]
read, i,prompt='Hour index? > ' & i=(i>0)<(nhour-1) 
dd1=reform(dd[i,*,*]) & help,dd1
CLOT,dd1,cased[0:ncase/2-1],locc=[.15,.5,-.03,.1] $
,titl=['season index',tit52[itm]+' at hour='+ST0(hourk[i]),y4tit+'  Delta due to Climate Tau' ]
end
 
47: begin & j=3  ; Extreme T comparison REQUIRES 33
krcs=fltarr(3,j,ncase)
kdel=fltarr(3,j,ncase)
kmg =fltarr(3,j,ncase)
qa='                   Surface Kin.       TOA Bolometric        Atmosphere '
qb='    Case         min   max  mean     min   max  mean     min   max  mean'
;      TR=0.5      8.4  3.5 -8.5    5.9 -0.3-11.4   22.5 14.0 -8.5
fmt='(a,t15,3f6.1,2x,3f6.1,2x,3f6.1)'
k2=pari[10]                     ; Ames model index
for k=0,ncase-1 do begin 
    for i2=0,j-1 do krcs[*,i2,k]=HSTATS(cch[*,i2,k],hsk)
    for i2=0,j-1 do kdel[*,i2,k]=HSTATS(cch[*,i2,k]-ccc[*,i2,k] ,hsk)
    for i2=0,j-1 do  kmg[*,i2,k]=HSTATS(cch[*,i2,k]-g24[*,i2+1,k2],hsk)
endfor 
Print,'Absolute T' & print,qa & print,qb
for k=0,ncase-1 do print,caseh[k],krcs[*,*,k],format=fmt
Print,'CCh-ccc' & print,qa & print,qb
for k=0,ncase-1 do print,cased[k],kdel[*,*,k],format=fmt
print,' Relative to GCM case',k2 & print,qa & print,qb
for k=0,ncase-1 do print,caseh[k],kmg[*,*,k],format=fmt
print,' GCM cases' & print,qa & print,qb
for k=0,kames-1 do print,fames[k],gcms[*,0:2,k],format=fmt
end

471: begin  ; Stats on 6th item
ndjt=reform(ttt[0,5,*,*,*]) & ndjh=reform(tth[0,5,*,*,*])
dtmt=reform(ttt[1,5,*,*,*]) & dtmh=reform(tth[1,5,*,*,*])
q=HSTATS(ndjh,hsk,lab=lab) 
print,'NDJh ',MAKEKEYVAL(lab,q,fmt=hfmt)
q=HSTATS(ndjt,hsk,lab=lab) 
print,'NDJt ',MAKEKEYVAL(lab,q,fmt=hfmt)
q=HSTATS(dtmh,hsk,lab=lab) 
print,'DTMh ',MAKEKEYVAL(lab,q,fmt=hfmt)
q=HSTATS(dtmt,hsk,lab=lab) 
print,'DTMt ',MAKEKEYVAL(lab,q,fmt=hfmt)
histfast,dtmt-dtmh
end

472: begin & siz=size(ddd); Plot diurnal magnitude for each layer, one Lat REQ 432
nlay=siz[1]
tmin=reform(ddd[*,0,jl,*,*],nlay,nseas*ncase) ; [layer,season*case]
tmax=reform(ddd[*,1,jl,*,*],nlay,nseas*ncase) ; [layer,season*case]
CLOT,transpose(tmax-tmin),/ylog,'layer'+string(indgen(nlay)+2,form='(i2)') $
,locc=1,titl=['season * case','Tmin-tmax',y4tit]
end

48: begin & kons=[20,21,252,461] ; KONS for Vicki Hamilton  INCOMPLETE?
end

481: begin ; CLOT Ts for first lat, all seasons
n24=kcom.id[5] ; output times per sol
CLOT,reform(ttt[*,0,0,*,0]),string(lsv),xx=hourk,locc=[.15,.93,-.023,.05] $
,titl=['Hour','Tsurf','TEsts for Vicki Hamilton using KRC version '+vern]
end

482: begin &  mrow=81; read and plot VH 2006 values
sss=READTXTCOL('/home/hkieffer/krc/vh/gts_ti150_2006.prt' $
,nskip=9,ncol=12,mrow=mrow)
ii=[1,3,10,11] ; cols desired
fff=float(sss[*,ii])
delt=fltarr(mrow)
end

51: begin & qq=ttt-tth & k1=nseas/2 ; look at file2-file1: Hist
tmhy=tmhy[*,0,*,k1:*,*]         ; delta Tsurf for the last year
print,'Tsurf of File2 - File1, last year'
q=HSTATS(tmhy,hsk,lab=lab) 
print,MAKEKEYVAL(lab,q,fmt=hfmt)
HISTFAST,tmhy & end

52: begin  & ii=where(abs(tmhy) gt parr[13]);+ then CHART wild
ll=LOC2INDEX(ii,tmhy) 
q1=tth[ll[0,*],ll[1,*],ll[2,*],ll[3,*],ll[4,*]]
q2=ttt[ll[0,*],ll[1,*],ll[2,*],ll[3,*],ll[4,*]]
q3=tth[(ll[0,*]+1)<23,ll[1,*],ll[2,*],ll[3,*],ll[4,*]]
q4=ttt[(ll[0,*]+1)<23,ll[1,*],ll[2,*],ll[3,*],ll[4,*]]
q=[ll,q1,q2]
CHART,transpose(q),parti=[lab52,'Tf1','Tf2'],dlin=1
end

524: begin &  q=[ll,q1,q2,q3,q4] ;; Or Chart 4
 CHART,transpose(q),parti=[lab52,'Tf1','Tf2','Slope1','Slope2'],dlin=1
end

53: print,fix(ll)               ;+ then print

54: begin  & ij=where(abs(tmhy) le parr[13]) ;+ then HIST -wild 
print,'Tsurf of File2 - File1, omitting values > '+strtrim(parr[13],2)
q=HSTATS(tmhy[ij],hsk,lab=lab) 
print,MAKEKEYVAL(lab,q,fmt=hfmt)
HISTFAST,tmhy[ij] & end

56: begin  ; plot season effect to check ExoPlanet
ts=reform(ttt[*,0,*,*,1]) ; no atmospher case
tst=transpose(ts,[0,2,1]) ; [hour,season,lat]
tsr=reform(tst,nhour*nseas,nlat)
ii=[0,4,9,14,18]
CLOT,tsr[*,ii],slat[ii],locc=[.5,.95,-.02,.07],titl=['hour*season','ts','@56 '+ifile]
end
 562: begin ; Midday Ts for all lats  REQ 56
tsm=reform(tst[(nhour-1)/2,*,*]) ; [season,lat]
CLOT,tsm,slat,locc=[.5,.95,-.02,.07],titl=['season','Tsurf at midday','@56 '+ifile]
end

563: begin  & tsn=reform(ttt[(nhour+2)/2,0,*,*,0]) ; with atm
CLOT,transpose(tsn),slat,locc=[.5,.95,-.02,.07],titl=['season','ts at midday','@56 '+ifile]
end

;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

570: thslci=transpose(ttt,[0,3,2,4,1])

571: begin  ; CLOT one ttt:  hour*seas for each Lat.
i=0 & j=0
read,i,j,prompt='Input ttt indexes for case and item > '
i=(i>0)<(ncase-1) & j=(j>0)<4
tx=reform(thslci[*,*,*,i,j])  ; [hour, season, latitude]
CLOT,reform(tx,nhour*nseas,nlat),slat,locc=1 $
,titl=['Hour * season',tit52[j],cased[i]]
if nseas lt 49 then PLOTSECT,stid[1:nseas],.1
end

572: begin  ; CLOT one ggg:  seas for each Lat.
i=0 & j=0
read,i,j,prompt='Input ggg indexes for case and item > '
i=(i>0)<(ncase-1) & j=(j>0)<5
tx=reform(ggg[j,*,*,i]); [season, latitude]
CLOT,transpose(tx),slat,locc=1,titl=['season',itemg[j],cased[i]]
end

61: begin & k1=nseas/2          ; look at change in last year file1
q1=tth[*,0,*,k1:*,*]-tth[*,0,*,0:k1-1,*]
plot,q1,psym=3,xtit='Index in hour,lat,season,case' $
,ytit='T change in last year: File1'
end

62: begin & HISTFAST,q1         ;+then HISTFAST
print,'T change in last year: File 1'
q=HSTATS(q1,hsk,lab=lab) 
print,MAKEKEYVAL(lab,q,fmt=hfmt) & end

63: begin & k1=nseas/2          ; look at change in last year file2
q2=ttt[*,0,*,k1:*,*]-ttt[*,0,*,0:k1-1,*]
plot,q2,psym=3,xtit='Index in hour,lat,season,case' $
,ytit='T change in last year: File 2'
end

64: begin & HISTFAST,q2         ;+then HISTFAST
print,'T change in last year: File 2'
q=HSTATS(q2,hsk,lab=lab) 
print,MAKEKEYVAL(lab,q,fmt=hfmt) & end

65: begin ; print subset of Tsurf  REQ 301
q2=reform(ttt[*,0,*,*,*]) ; [hour,lat,season,case]
jj=[7,24,38] ; seasons
qq=q2[*,ii,jj,*] ; selected latitudes and seasons
sizz=size(qq)
qq=reform(qq,2,24,sizz[2],sizz[3],sizz[4]) 
qq=reform(qq[1,*,*,*,*])        ; 24 hours
openw,lun,'t52.tex',/get_lun
print,'lats=',slat[ii]
print,'seasons=',lsv[jj]
fmt='(99f6.1)'
for k=0,sizz[4]-1 do begin  ; cases
    for j=0,sizz[3]-1 do begin  ; latitudes
        for i=0,sizz[2]-1 do printf,lun,qq[*,i,j,k],form=fmt
endfor & endfor 
free_lun,lun & end

66: BIN5,'Wb',parf[1],'Tkinetic[hour,latitude,season,case]' $ ;+ bin5 W Tkin
+' cases are I=' + STRUM(cased,'|',/join),q2,/verb


71: begin ;  Plot diurnal and annual averages
 cased[ncase-1]='base with climate'
t4=reform( ttt[*,[0,3],0,*,*]) ; ;tsur and downvis [
 t3=total(t4,1)/nhour &  help,t3 ; diurnal averages
 CLOT,reform(t3[0,*,*]),cased,locc=[.2,.9,-.025,.08],xx=dfl0 $
,titl=['Days from Ls=0','Tsurf diurnal average',ifile]
t2=total(t3,2)/nseas
print,'    Tsurf      Vdown  Annual average'
  for i=0,ncase-1 do print, t2[*,i],cased[i], form='(2f10.3,2x,a)'
end

73: begin &  k33=k33+1 & k2=pari[10] & x2=g24[*,0]         ;+ Oplot 24 version
    j=4
    clrk=kkc[k33 mod nkc]       ; color
    for i=1,3 do oplot,x2,g24[*,i,k2],color=clrk,line=linn[i-1],psym=j
    CURVEGUIDE,k33,'Ames GCM/24 '+strtrim(k2,2),j+10,locc=parg[8:11],color=clrk
    end

74: CHART,g24[*,*,1],parti=laba,xtit='Hour-1',title='AMES Model' ; chart GCM24

75: begin  & k2=pari[10] & geps=1.0 ; Compute & plot GCM diurnal tau
; ts4*emt+ta4(1-emt)=tb4  where emt= exp(-tau)
ta4=g24[*,3,k2]^4 & hour=g24[*,0,k2]
emtau=(g24[*,2,k2]^4-ta4)/(geps*g24[*,1,k2]^4-ta4)
plot,hour,emtau,color=kcc[8] & stop
tau=-alog((emtau>5.e-3)<1.) & plot,hour,tau & end

76: begin & dir='/home/hkieffer/krc/vh/'; check Vi Hamilton oneP runs
mrow=84
sss=READTXTCOL(dir+'gts_ti150_2006.prt', nskip=7,ncol=12,mrow=mrow)
tt1=float(sss[*,10:11])
ss2=READTXTCOL(dir+'gts_ti150_2013.prt', nskip=7,ncol=12,mrow=mrow)
tt2=float(ss2[*,10:11])
ss3=READTXTCOL(dir+'150.prt', nskip=-1,ncol=12,mrow=mrow)
tt3=float(ss3[*,10:11])
plot,tt1,psym=1,ytitl='I=150.  Left=Tsur, right=Tplan: Kelvin' $
,xtit='Season Index:   Left=Tsur, right=Tplan   H=5.7 6.7 12.7 13.7'  $
,titl='+=2006   diamond=2013  square=Ver2  *=V2.2.1 Full'
oplot,tt2,psym=4                ; Diamond
oplot,tt3,psym=6                ; square
qq=float(ss2[*,1])              ; all the Ls
qq=reform(qq,4,21)              ; one of each
qv=reform(qq[0,*])
qd=lsmars(2,qv)                 ; convert to MJD and 5 othr items
qm=qd[*,0]                      ; only the MJD
qs=qm-shift(qm,+1)              ; delta MJD; first is smaller
print,MEAN_STD(qs[2:*],std=std),std ;  10.2405     0.101031
print,WHEN2START(155.9,122,10.24)
parf[1]='VH2'
end

 77: begin; ; after 76,251, 252,
; with N24=36, [9,18] are 6.67 and 12.67
; want to match starting with second Ls
; lsv[0:19] matches VH seasons 1:20
tv=ttt[*,0:1,0,0:19,0] ;  Ts and Tp I=150
tv=reform(tv) ; [hour,item,season]
tvh=tv[[9,18],*,*]              ; select 2 hours that match onePoint runs
ix=5+4*indgen(20)               ; index for Tsur at 6.67
oplot,ix,tvh[0,0,*],psym=2      ; Ts at 6.6
oplot,ix+2,tvh[1,0,*],psym=2    ; Ts at 13.6
oplot,ix+84,tvh[0,1,*],psym=2   ; Tp at 6.6
oplot,ix+86,tvh[1,1,*],psym=2   ; Tp at 13.6
xyouts,20,240,'asterisk is V2.2.1 Full Krc at 6.7 and 13.7',chars=1.5
end

else: begin & KON91,ptitl,prior,hold,kon,kons,kitel ;=KON99 < needed by MAKE99
      if kon eq 99 then begin 
          print,'11: files: parf= ',parf
          print,'14: Integs: pari= ',ST0(pari)
          print,'15: Floats: parr= ',ST0(parr)
          print,'16:   Plot: parg= ',ST0(parg)
          print,'166: PLines linn= ',ST0(linn)
          print,'167: Colors clrr= ',ST0(clrr)
          print,'168: Thick  thkk= ',ST0(thkk)
      endif & end
endcase
goto, ask

halt:  print,'SOME ERROR CONDITION at kon=',kon,'.  Any key to Go'
i=get_kbrd(1) & goto,sureask
end
