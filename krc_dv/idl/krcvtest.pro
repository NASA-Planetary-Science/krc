pro krcvtest, oldd=oldd,oldv=oldv, newd=newd,newv=newv
;_Titl KRCVTEST  Check consistency of KRC within and between versions
dold='/work1/krc/test/'; oldd  in_ string Directory for the older version. <<Default
vold='V222' ; oldv  in_ string  File stem for the older version. <<Default
; newd  in_ string  Directory for the newer version. Default is global PROJDAT
vnew='V232' ; newv  in_ string  File stem for the newer version. <<Default
;_Desc. Reads  KRC type 52, 0 , -1  
;_Calls  CHART  CLOT  DEFINEKRC  GETP  GETPAN  GETPINTS  GETPSN
;  HISTFAST  HSTATS  KON91  KRCHANGE  KRCCOMLAB  LASTPERIOD  LSAM  MAKEKEYVAL  
;  MEAN_STD2  PAUSE  PLOTSECT  PM180  PRINTJCOLS  PRINTRIL  READKRC1  
;  READKRC52  READKRCCOM  READTXTCOL   SETCOLOR   ST0 STRWORD1  VEC2CODE
; Addition calls via kon91:  COLOR24BIT  DELAST0  GRAPH  MAKE99  
;    SETWIND  SUBTITLE  TOOTHB
;_Hist 2013sep03 Hugh Kieffer
; 2014feb27 HK Turn into procedure with path's as argument
 common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,scex1
;_End        .comp krcvtest    krcvtest,newv='V232'

ptitl='krcvtest' & prior=['q','q']
homedir=getenv('MYHOME')        ; get current home
hcpu=getenv('HOST')             ; get current CPU name
;solib=getenv('SOLIB')           ; location of shared object library
krcdat=getenv('PROJDAT')        ; KRC data files
krcsrc=getenv('PROJSRC')        ; KRC input files
idltop=getenv('IDLTOP')         ; expected top of current IDL run directory
CD,current=curd                 ; get current directory

; If testing KRC distro IDL
if hcpu eq 'hulk3' and curd eq '/work1/build/idl/' then krcdat='/work1/build/run/'

if not keyword_set(oldd) then oldd=dold
if not keyword_set(oldv) then oldv=vold
if not keyword_set(newd) then newd=krcdat
if not keyword_set(newv) then newv=vnew

;if homedir eq '/u/hkieffer/' then krcdat='/work/hkieffer/krc/test/' ; Mac

labf=['VerA=new DIR ',' " case file',' " multi-type stem',' " OnePoint [.prt]' $
,' " DIR for prt','VerB=prior DIR',' " case file',' " multi-type stem' $
,' " OnePoint [.prt]',' " DIR for prt','DIR for IDL output' $
,'Output onePoint set','Report file stem', ' " .ext'] 
parf=[newd,newv+'test1',newv+'test2',newv+'Mone','/home/hkieffer/krc/tes/' $
,oldd,oldv+'test1',oldv+'test2',oldv+'Mone',newd,idltop,'grid.one',newv $
,'Report.txt'] 

parf0=parf ; remember the initial values

labi=['Flag: DJUL is oldstyle','@46 # seasons','@46 N hours','@522 item index' $
,'@71 rows in OnePoint','@77 max output rows','SPARE season/year','+Case','-Case'$
,'@64x first depth index',' " last','@64x first lat. index',' " last' $
,'@642.646 ref layers'] 
pari=[0,20,6,1,1152,2000,40, 0,3, 0,5,2,8,23]

labp=['@641 Yplot Min',' " max: = =auto, < =sigma',' Ymag min',' " max' $
,'X loc   \/ @--','Y loc   | Curve-','Delta Y | Guide for','LineLen /\',' " plotsect Y______' $
,'@642 Yplot Min',' " max: = =auto, < =sigma',' Ymag min',' " max' $
,'X loc   \/ @--','Y loc   | Curve-','Delta Y | Guide for','LineLen /\',' " plotsect Y______' $
,'@643  Yplot Min',' " max: = =auto, < =sigma','- Ymag min','- " max' $
,'X loc   \/ @--','Y loc   | Curve-','Delta Y | Guide for','LineLen /',' " plotsect Y______' $
,'@644  Yplot Min',' " max: = =auto, < =sigma','- Ymag min','- " max' $
,'X loc   \/ @--','Y loc   | Curve-','Delta Y | Guide for','LineLen /',' " plotsect Y______' ]
parp=[-3.,3.,-0.1,0.1,0.37,0.7,0.025,0.06,0.1,-1.,1.,0.2,0.2,0.4,0.7,0.025,0.06,0.2,0.,0.,0.,0.,0.41,0.15,0.025,0.06,0.05,0.,0.,0.,0.,0.4,0.12,0.025,0.06,0.9]

labr=['@663,664,672 Yplot Min',' " max: = =auto, < =sigma',' Ymag min',' " max' $
,'X loc   \/ @--','Y loc   | Curve-','Delta Y | Guide for','LineLen /\_______' $
,'@72 Yplot Min',' " max: = =auto, < =sigma',' Ymag min',' " max' $
,'X loc   \/ @--','Y loc   | Curve-','Delta Y | Guide for','LineLen /\_______' $
,'@68,682  Yplot Min',' " max: = =auto, < =sigma',' Ymag min',' " max'$
,'X loc   \/ @--','Y loc   | Curve-','Delta Y | Guide for','LineLen /\_______' ]
parr=[1.,1.,0.0,1.0, .8,.94,-.025,.06,   7.,1.,-.5,.5, .1,.93,-.025,.08 $
   , -1,0.5,-.5,0.5, .5,.60,-.025,.06  ]

lab52=['Hour','item','Lat.','Season','Case'] ; dimensions of type52

kist=[101,103,109,115,117,119,207]; KRCCOM items in n*100+i format

clrr=[255,254,100,200,150,60] & nclr=n_elements(clrr); cases in color
labc=['Base = Case 1','Case 2','case 3','Case 4 & _h','Case 5 & GCM','Case 6']
labl=['Tsurf & 1', 'Tbolo & 2','Tatm & 3','4 & _h',   '5 & GCM',     '6=spare']
thkk=replicate(1.,6)
linn=[0,2,3,4,5,1] & nlin=n_elements(linn)

str0=string(indgen(16),form='(i2)')
str1=string(indgen(16)+1,form='(i2)')

hsk=['M','S','I','X','MA','N']  ;  kodes for desired stats
hfmt=['f6.3','f5.3','f8.3','f7.3','f7.3','i6'] ;  blanks will be trimmed
id52=['Tsur','Tplan','Tatm','DownVis','DownIR']
des52=['Surface kinetic','TOA Bolometric','Atmosphere','Down-going Vis' $
,'Down-going IR'] ; labels
dj2000=2451545.D0               ; JD of epoch J2000
k24=dj2000-2440000.              ; Offset for KRC before 2013

i=1 & j=1 & it1=0  & luf=0 & ifh='-none-' & pres=600. ; insurance
;===============================================================================
kite=' ' & komit=0   &  paw=-1 ; type definitions
prior=['1','1']                 ;impossible values, to initiate MAKE99
text='dum_text' & ytext='dum_ytext' & text2='dum_up' ; subtitle place holders
lkon=0B
kons=[850,88,851,869,20]  ; required for definitions;
kon=123 & goto,dokon           ; do them immediately

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

 0: stop  ;- Stop

-1: begin & print,'Any key to GO' & qi=get_kbrd(1) & end ;  Wait

110: parf=parf0 ; Reset names to default

111: kons=[200,202,207,21,22,29,252,253] ; Read VerA group 1 cases

112: kons=[41,-1,411,-1,42,43,-1,44,-1,45,-1,46] ; Test cases

113: kons=[200,203,207,252,50,51,18] ; Read 3 types for Ver A

114: kons=[511,-1,52,-1,53,-1,55] ; Test between types

115: kons=[26,201,202,207,252] ; Save current t52 and Read VerB cases

116: kons=[61,-1,62,63] ; Compare versions

118: kons=[432,43,435,-1,44,-1,445] ; look at effect of atm

131: kons=[77,411,43,-1,44,-1,45,26,203,207,252,50,51,511,-1,52,53,-1,55] ; Test one version

132: kons=[26,201,207,252,67,-1,68,78] ; compare 2 versions AFTER 131

133: kons=[200,203,207,252,77,671,673,-1,664,-1,672]; Long runs A

134: kons=[26,201,207,252,671,-1,68,-1,682,78]; Long runs B-A
;..................................................................

123: begin & lkon=1b & kkon=-1  ;- Start auto-script 
lastkon=n_elements(kons)-1 & end ; last preset command

11: GETPSN,'File names',parf,labs=labf,/align ;- Modify File names parf
12: GETPAN,'pari',pari,0,0,labs=labi ;- Modify integers pari
15: GETPAN,'parp',parp,0.,0.,labs=labp ;- Modify positions parp
157:  print,'parp',VEC2CODE(parp) ; Print current parp as code
16: GETPAN,'parr',parr,0.,0.,labs=labr ;- Modify floats parr
167: print,'parr',VEC2CODE(parr) ; Print current parr as code

18: begin & help,ifile,ttt,ifh,tth,uuu,vvv,tsz,tsm ; Help, and print cases
if n_elements(cased) gt 1 then PRINTJCOLS,cased,1,len=65 & end

188: if n_elements(itemv) lt 1 then print,'Need to do @252' else begin ;+ Contents
  help,ttt & print,'(hour,item,latitude,season,case)' & print,'itemt = ',itemt
  help,ddd & print,'(layer,item,latitude,season,case)' & print,'itemd = ',itemd
  help,ggg & print,'(item,latitude,season,case)' & print,'itemg = ',itemg
  help,uuu & print,'(nlat,item,case)' & print,'itemu = ',itemu
  help,vvv & print,'(season,item,case)' & print,'itemv = ',itemv
print,'KRCCOM is in kcom:' & help,kcom,/struct & end

19: begin & i=7 ; Print input portion of selected KRCCOM arrays REQ 20,21
GETP,'+1=floats +2=integers +4=logicals',i,0,7
 PRINTRIL ,i,kcom1.fd,fclab, kcom1.id,iclab,kcom1.ld,lclab
end

; 222222222222222222222222222222222 set names and read files
200: Begin & iver=0 & verr='A' & end ; Set to VerA 
201: Begin & iver=1 & verr='B' & end ; Set to VerB

202: begin & igrp=1 & verg='1' & end ; Set to case group 1
203: begin & igrp=2 & verg='2' & end ; Set to case group 2

207: begin & i=5*iver  ; Set input file stem
stem=parf[i+igrp]         ; core of the KRC file name
fame=parf[i]+stem         ; input file name without extension
sfile=verr+verg
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

21: begin  ; Open file to determine locations of krccom
front=READKRCCOM(fame+'.t52',khold) 
if n_elements(front) lt 5 then goto,halt
print,'khold=',khold
kcom1=READKRCCOM(1,khold) ; get first case
end

221: GETPINTS,'KRCCOM Items',kist,100,320 ; Change KRCCOM List

22: begin & if n_elements(front) lt 5 then goto,halt ; Get KRC changes
cased=KRCHANGE(khold,/log,list=kist) ; ,kcom1=kcom1)
;print,parf[1]+' Base case= ',cased[0]
cased[0]=parf[1]+' Base' & end

23: KRCCOMLAB, pari[9],kcom.fd,kcom.id,kcom.ld,fclab,iclab,lclab ;+ Print krccom

232: KRCCOMLAB, pari[9] $ ; Difference 2 KRCCOM's  REQ 26
,kcom.fd-kcomh.fd,kcom.id-kcomh.id,kcom.ld-kcomh.ld,fclab,iclab,lclab

252: begin & ifile=fame+'.t52' ; Open/Read/Close type 52 file
kcom=READKRC52(ifile,ttt,uuu,vvv,itemt,itemu,itemv,ddd,ggg,itemd,itemg,vern=vern)
help,ttt,uuu,vvv,ddd,ggg,vern,kcom
siz=size(kcom)
if siz[siz[0]+1] ne 8 then goto,halt ; must get a structure
sizt=size(ttt) & nhour=sizt[1] & niti=sizt[2]
nlat=sizt[3] & nsea=sizt[4] & ncase=sizt[5]
print,'Nseas, nlat, ncase=',nsea,nlat,ncase
alat=uuu[0:nlat-1,0,0]          ; latitudes for first case
slat=ST0(alat,/nojoin)          ; lats as string
sour=ST0((24./nhour)*(findgen(nhour)+1),/nojoin)
scase='Case '+str1[0:ncase-1]   ; 1-based case numbers
djmm=vvv[*,0,0]                 ; DJUL
lsv =vvv[*,1,0]                 ; LSUBS
; dfl0=(djmm-151.269) mod 686.99161 ; days from Ls=0 for Mars
tsur=reform(ttt[*,0,0,*,0])          ; Tsurf [hour, season]
jy1=LASTPERIOD (lsv,leny,jy2,verb=1) ; get season coverage 
n3=kcom.id[2]                        ; N3
jdisk=kcom.id[11]                    ; JDISK
nyr=nsea/leny                        ; number of full years 
end

253: begin                      ; specific case names
if stem eq 'V224str1' then scase= ' Pt='+['546','200','100','50','10','5','2','none']
if stem eq 'V224str2' then scase= ' Pt='+['10000','5000','2000','1000','500','200','100','50','20','10','5','2','none']
if stem eq 'V224str5' then scase= ' Pt='+['10000','5000','2000','1000','500','200','100','50','20','10','5','2', '1.01','none']
if strpos(stem,'test1') ge 0 then scase=['AtmTconFcon','AtmTdepFcon','AtmTconFvar','noAtmTcon','noAtmTvar','noAtmTuni']
if stem eq 'try2' or stem eq 'try8' or stem eq 'try5' then scase=['29','18','19','20','21','22','23','25','27']
end

26: begin & siz=size(ttt) ; hold current set. tth=ttt etc. 
if siz[0] ne 5 then goto,halt
tth=ttt & uuh=uuu & kcomh=kcom & caseh=cased & sizh=sizt & scash=scase
ifh=ifile & vvh=vvv & lsh=lsv & ddh=ddd & ggh=ggg & verh=vern & tsh=tsur 
lenh=leny & end

261: begin & nyr=6 ; extract 23 layer 6 year from multi-N1 10 year 
i=leny*nyr ; last to keep
nsea=i+1 & j=2 ; j is case for n1=23
ncase=1
ttt=reform(ttt[*,*,*,0:i,j],nhour,niti,nlat,nsea,ncase,/over)
ddd=ddd[*,*,*,0:i,j]
ggg=ggg[*,*,0:i,j]
uuu=uuu[*,*,j]
;vvv=vvv[0:i,*,j]
lsv=lsv[0:i] & djmm=djmm[0:i]
scase=scase[j]
tsur=reform(ttt[*,0,0,*,0])          ; Tsurf [hour, season]
jy1=LASTPERIOD (lsv,leny,jy2,verb=1) ; get season coverage
print,'Nseas, nlat, ncase=',nsea,nlat,ncase
end

266: help,ifh,ifile,lsh,lsv $ ; Help latest and hold
,tth,ttt,uuh,uuu,vvh,vvv,ddh,ddd,ggh,ggg,vern

27: ltab=KRCLAYER(kcom.fd,kcom.id,kcom.ld) ; Print layer table
; print,reverse(ly[klay]),form='(9f6.2)'  After 641

29: q= READKRCCOM(-1,khold)     ; Close the KRC unit

; 44444444444444444444444444444444 test between cases
; Ver 222 cases
; 0: With atmosphere, properties constant with T
; 1: With atmosphere, properties T-dependent
; 2: With atmosphere, soil properties constant with T, frost properties variable
; 3: No atmosphere, properties constant with T
; 4: No atmosphere, properties T-dependent
; 5: No atmosphere, properties T-dependent, but constant

41: begin                       ; Test Ls  Requires more than one case
yy=reform(vvv[*,1,*]) ; Ls [season,case]
siz=size(yy) & if siz[0] ne 2 then goto,halt ; only one case
ya=MEAN_STD2(yy,std=yb) ; statistics across cases
xa=min(yb,max=xb) ; range of changes between cases
print,'Range of change of Ls between cases:',xa,xb
plot,yy[*,0],xtit='Season index [0-based]',ytit='Ls for case 0', title=ifile
end

411: begin & mjd=djmm           ; Check Ls against LSAM REQ 252
if pari[1] then mjd=djmm-k24 ; adjust older version to j2000
lsam=LSAM(mjd,myn,aud)
plot,mjd,PM180(lsv-lsam),xtit='MJD  '+ifile,ytit='Ls from vvv-LSAM'
xa=MEAN_STD(PM180(lsv-lsam),std=xb)
if luf ne 0 then printf,luf,'@411 Ls t52-A&M: Ave and StdDev',xa,xb,form='(a9,2f8.3)'
end

42: begin                       ; Confirm convergence days 
ndj4=reform(ggg[0,*,*,*])
xa=min(ndj4,max=xb)
print,' NDJ4: min,max=',xa,xb
plot,ndj4, psym=1,xtit='Lat * season * case', ytit='Number of convergence days'
jj=histogram(ndj4,omin=i1,omax=i2)
Print,'Distribution of NDJ4'
for i=i1,i2 do print,i,jj[i-i1]
end

431: it1=0 ; Set to Tsur

432: read,it1,prompt='First index in ttt 0=Ts 2=Ta etc > ' ; Set to any item in ttt

43: begin & qt=itemt[it1]                     ; Plot hourly Ts near equator for 2 seasons
i1=jy1
t1=reform(ttt[*,it1,nlat/2,i1:*,*]) ;[hour,season,case]  at equator
q=min(abs(lsv[i1:*]-251.),i1) ; find index nearest perihelion, which is at Ls=251
q=min(abs(lsv[i1:*]-71.),i2)  ;    "    "     "      aphelion, which is at Ls=71
ii=[i1,i2] 
t1=t1[*,ii,*] ; only two seasons
t2=transpose(t1,[0,2,1]) ; [hour,case,season]
t3=reform(t2,nhour,2*ncase) ; [hour, case*season]
q=[scase+(' Seas '+strtrim(ii[0],2)),scase+(' Seas '+strtrim(ii[1],2))]
CLOT,t3,q,titl=['Hour index',qt+' at equator. Last Year',ifile],locc=[.45,.5,-.03,.08]
qs=string(lsv[ii],form='(f5.1)')
Print,'Seasons and Ls=',ii, lsv[ii]
end

433: begin & read,i,j,prompt='Season and lat index > ' ; Plot hourly one lat,season  REQ 43 
t2=reform(ttt[*,it1,j,i,*]) ; [hour,case]
CLOT,t2,scase,titl=['Hour index.   Season='+strtrim(i,2)+'  Ls='+strtrim(lsv[i],2) $
,qt+' at lat='+slat[j],ifile],locc=[.45,.5,-.03,.08]
end

435: begin & print,' pre-dawn and Midday values for ',qt ; Print midday REQ 43
j=nhour/2 & k=nhour/6
for i=0,2*ncase-1 do print, i, t3[k,i],t3[j,i],q[i],form='(i3,2g12.5,2x,a)' & end

436: begin & print,' Midday values for ',qt ; Plot midday REQ 43
plot,t3[j,*],xtit='case',ytit= ' Midday values for '+qt,psym=-4 
xa=(2*ncase-1)/2. & oplot,[xa,xa],!y.crange,line=1 ; season dividing line
xyouts,.25,.5,'Ls= '+qs[0],/norm,chars=2.
xyouts,.75,.5,'Ls= '+qs[1],/norm,chars=2.& end

44: begin  & qt=itemt[it1]  ; Display central latitude seasonal behaviour
t1=reform(ttt[nhour/2,it1,*,*,*]) ;[lat,season,case] Tsur at noon equator
t2=transpose(t1,[1,2,0])  ;[season,case,lat] 
t3=reform(t2,nsea*ncase,nlat)
CLOT,t3,slat,loc=[.38,.45,-.03,.08],titl=['season*case',qt+' at noon',ifile]
PLOTSECT,scase,.21,cs=1.5
PLOTSECT,str0[1:ncase],.16,cs=1.5
end

445:begin & read,j,prompt='Lat index > '   ; CLOT one latitude REQ 43 then 44
j=(j>0)<(nlat-1)
CLOT,t2[*,*,j],scase,loc=1,titl=['Season Index',qt+' near Noon','Lat. index '+strtrim(j,2)]
print,'Lat= ',slat[j], alat[j] 
i=nsea/2 & print,'Values at season: index and Ls=', i,lsv[i]
for k=0,ncase-1 do print,k,t2[i,k,j],'  ',scase[k]  & end

45: begin & k1=pari[7]<(ncase-1) ; Difference two cases
 k2=pari[8]<(ncase-1) & if k1 eq k2 then k2=k1-1
cdtit=scase[k1]+'-'+scase[k2]
t1=ttt[*,*,*,*,k1]-ttt[*,*,*,*,k2] ; Prop=constant - Prop=uniform
j=n_elements(t1)                   ; t1 is [hour,item,lat,season]
print,'@45  ',cdtit
print,'Item in ttt Mean     Std    mean_ABS_std'
;         Tsurf   0.259   1.236   0.977   0.801
if luf ne 0 then printf,luf,'@45  ',cdtit
if luf ne 0 then printf,luf,'Item in ttt Mean     Std    mean_ABS_std'
for i=0,4 do begin              ; each item in ttt
   xx=(t1[*,i,*,*])    ; all hours, lats and seasons
   xa=MEAN_STD(xx,std=xb)    
   ya=MEAN_STD(abs(xx),std=yb)
   print,itemt[i],xa,xb,ya,yb,form='(a9,4f8.3)'
   if luf ne 0 then printf,luf,itemt[i],xa,xb,ya,yb,form='(a9,4f8.3)'
   if xa gt 1.E-6 then begin 
      HISTFAST,xx,xlab='Delta '+itemt[i]+'  '+cdtit
      PAUSE,-1
   endif
endfor
end

46: begin  ; Plot Tsur, DownVis difference of two cases, AFTER 45 
nsp=pari[1] & nhp=pari[2]       ; Number of seasons and hours to plot
; For NoAtm cases, Tplan, Tatm, DownIR, FROST4 ,AFRO4 are meaningless
jj=[0,3]                        ; Tsur and DownVis
t2=t1[*,jj,*,*,*] ; two items valid with/without atm ; [ hours, 2 items,lat,season]
t2=transpose(t2,[0,3,2,1])      ; [hour,season,lat,item]
ii=(nsea/nsp)*indgen(nsp)      ; subset nsp seasons
print,'Index of Seasons plotted:',ii
t2=t2[*,ii,*,*]  
ii=(nhour/nhp)*indgen(nhp)      ; subset nhp hours
t2=t2[ii,*,*,*]
print,'Hours plotted:',ii
;t3=reform(t2,nhour*nsea*nlat,2) ; [hour*season*lat,item]
t3=reform(t2,nhp*nsp*nlat,2) ; [hour*season*lat,item]
CHART,t3,psy=3,parti=itemt[jj],dlin=2,xtit='hour * season * latitude' $
,titl=cdtit+' '+ifile
PLOTSECT,slat,.51, cs=2.
end

47: begin ; Estimate Atm Radiative time
; krccom values are for the last latitude at the end of the first season
; for the first case
beta=kcom.fd[86-1] & sigsb=kcom.fd[95-1]
pres=kcom.fd[72-1]
tatmj=kcom.fd[76-1]
grav=kcom.fd[47-1]
atmcp=kcom.fd[48-1]
radtime=atmcp*(pres/grav)/(beta*sigsb*tatmj^3) ; radiation time in secs
raday=radtime/(2.71828*86400.); relaxation time in days
print,'Radiation time,sec=',radtime,'  Relaxation in days=',raday
end

472: print,3182.48/(27.9546-alog(pres)) ; T of P for CO2 SET PRES


; 55555555555555555555555555555555 test between file types
; Read type 52, -1 and 0 for the same model

50:  begin & fun0=READKRC1(fame+'.t0',fcoz,icoz,lcoz, lsz $  ; Read type 0
,tsz,tpz,  ktype=0,/verb,ddd=dd0,lab=labd,desc=desc)
siz=size(fun0) & if siz[0] ne 2 then goto,halt
slat0=string(fun0[*,0],form='(f6.1)')
end

51: begin & funm=READKRC1(fame+'.tm1',fcom,icom,lcom, lsm $ ; Read type -1
,tsm,tpm,  ktype=-1,/verb)
siz=size(funm) & if siz[0] ne 2 then goto,halt
slatm=string(funm[*,0],form='(f6.1)')
end

511: begin & nread=n_elements(lsz); Compare Ls in Type 0 file with LSAM 
djul=fcoz[41-1] & deljul=fcoz[42-1] ; ASSUMED to be MJD. False before Version 2
jdisk=icoz[12-1]
mjd=djul+deljul*(findgen(nread)+(jdisk-1)) ; days from J2000
lsam=LSAM(mjd,myn,aud)
plot,mjd,PM180(lsz-lsam),xtit='MJD  '+ifile,ytit='Ls from Type0-LSAM'
end

52: begin & siz=size(dd0) ; Plot delta of each ddd item
if siz[0] ne 3 then goto,halt
SETCOLOR,init=862 ; 17 colors
for j=0,siz[3]-1 do begin ; each item
   CLOT,transpose(dd0[*,*,j]),slat0,loc=[.35,.20,.025,.08] $
        ,titl=['season index.  ',desc[j], stem+'.t0 '+labd[j]]
   PAUSE,-1
endfor & end

522: begin & j=(pari[3]>0)<5 ; Plot one dd0 item
   CLOT,transpose(dd0[*,*,j]),slat0,loc=[.35,.20,.025,.08] $
        ,titl=['season index.  ',desc[j], stem+'.t0 '+labd[j]]
end

53: begin & help,lsv,lsm,lsz  ; Check Ls between types
plot,lsv,xtit='Season index  __=52 +=t0  diamond=t-1  & 200+100*diff' $
,ytit='Ls',title=stem
oplot,lsm,psym=4
oplot,lsz,psym=1
oplot,200.+100*(lsm-lsv),psym=4
oplot,200.+100*(lsz-lsv),psym=1
xa=MEAN_STD(lsm-lsv,std=xb)
ya=MEAN_STD(lsz-lsv,std=yb)
if luf ne 0 then printf,luf,'Ls  t0-t52: Ave and StDev',xa,xb,form='(a,2f8.3)'
if luf ne 0 then printf,luf,'Ls tm1-t52: Ave and StDev',ya,yb,form='(a,2f8.3)'
end

55: begin & fmt='(a10,4f12.5)'; Check Ts and Tp for equivalence between types
qq='@55   What        Mean      StdDev     Minimum     Maximum'
print, qq & if luf ne 0 then printf,luf,qq
qt=tsz-tsm
aa=HSTATS(qt,['M','S','I','X']) & print,'Ts 0--1',aa,form=fmt
if luf ne 0 then printf,luf,'Ts 0--1',aa,form=fmt
qt=tpz-tpm
aa=HSTATS(qt,['M','S','I','X']) & print,'Tp 0--1',aa,form=fmt
if luf ne 0 then printf,luf,'Tp 0--1',aa,form=fmt
qt=ttt[*,0,*,*,0]-tsm ; type 52 for atm,Tconst
aa=HSTATS(qt,['M','S','I','X']) & print,'Ts 52--1',aa,form=fmt
if luf ne 0 then printf,luf,'Ts 52--1',aa,form=fmt
qt=ttt[*,1,*,*,0]-tpm
aa=HSTATS(qt,['M','S','I','X']) & print,'Tp 52--1',aa,form=fmt
if luf ne 0 then printf,luf,'Tp 52--1',aa,form=fmt
ii=[1,3,4] ; items in ggg
jj=[0,4,5] ; corresponding items in dd0
for i=0,2 do begin 
   qt=dd0[*,*,jj[i]]- ggg[ii[i],*,*,0]
   aa=HSTATS(qt,['M','S','I','X']) & print,itemg[ii[i]],aa ,form=fmt
   if luf ne 0 then printf,luf,itemg[ii[i]],aa ,form=fmt
endfor
end

56: begin & siz=size(tsm) & if siz[0] ne 3 then goto,halt ; Store Type 0,-1
siz=size(tsz) & if siz[0] ne 3 then goto,halt
lsmh=lsm & tsmh=tsm & tpmh=tpm ;
lszh=lsz & tszh=tsz & tpzh=tpz & dd0h=dd0 & end

57: begin & fmt='(a10,4f12.5)' ; Compare Versions for Type 0 and -1
print,'   What          Mean      StdDev     Minimum     Maximum'
qt=lsz-lszh
aa=HSTATS(qt,['M','S','I','X']) & print,'Ls 0',aa,form=fmt
qt=tsz-tszh
aa=HSTATS(qt,['M','S','I','X']) & print,'Ts 0',aa,form=fmt
qt=tpz-tpzh
aa=HSTATS(qt,['M','S','I','X']) & print,'Tp 0',aa,form=fmt
qt=dd0-dd0h
aa=HSTATS(qt,['M','S','I','X']) & print,'ddd 0',aa,form=fmt
qt=lsm-lsmh
aa=HSTATS(qt,['M','S','I','X']) & print,'Ls -1',aa,form=fmt
qt=tsm-tsmh
aa=HSTATS(qt,['M','S','I','X']) & print,'Ts -1',aa,form=fmt
qt=tpm-tpmh
aa=HSTATS(qt,['M','S','I','X']) & print,'Tp -1',aa,form=fmt
end 

; 66666666666666666666666666666666 tests between versions
61: begin & yy=PM180(lsv-lsh); Plot LS-LSH
qt=max(abs(yy)) & print,'Maximum difference in Ls is:',qt
if qt ne 0 then plot,djmm,yy,xtit='MJD  '+ifile,ytit='lsv-lsh' $
,titl=ifile+' - '+ifh
end

62: begin & i=nhour/2 & j=nlat/2 ; Plot Tsur noon equator
xx=tth[i,0,j,*,0] ; noon equator case zero 
yy=ttt[i,0,j,*,0] ; noon equator case zero  Ver B
ya=min([xx,yy],max=yb) ; total range in T
plot,xx,yran=[ya,yb],xtit='Season index  Dashed in VerB' $
,ytit='Tsur near noon equator'
oplot,yy,line=2,color=99
oplot,280.+100*(yy-xx),psym=3
end

63: begin  & fmt='(a10,4f12.5)'; Stats on VerB-VerA
if total(abs(sizt-sizh)) ne 0 then goto,halt ; some dimension different
print,'xxx-xxh           Mean      StdDev     Minimum     Maximum'
qt=ttt-tth 
for j=0,niti-1 do begin & aa=HSTATS(qt[*,j,*,*,*],['M','S','I','X']) 
print,itemt[j],aa,form=fmt & endfor
qt=ddd-ddh 
for j=0,1 do begin & aa=HSTATS(qt[*,j,*,*,*],['M','S','I','X']) 
print,itemd[j],aa,form=fmt & endfor
qt=ggg-ggh
for j=0,5 do begin & aa=HSTATS(qt[j,*,*,*],['M','S','I','X']) 
print,itemg[j],aa,form=fmt & endfor
qt=vvv-vvh
for j=0,4 do begin & aa=HSTATS(qt[*,j,*],['M','S','I','X']) 
print,itemv[j],aa,form=fmt & endfor
qt=uuu-uuh
for j=0,1 do begin & aa=HSTATS(qt[*,j,*],['M','S','I','X']) 
print,itemu[j],aa,form=fmt & endfor
ndj4=fix(reform(ggg[0,*,*,*])) ; [lat,season,case] number of days to convergence
ndj4h=fix(reform(ggh[0,*,*,*]))
del4=ndj4-ndj4h                 ; change in number of iteration days
ii=where(del4 eq 0.,ni)         ; days the same within [lat,season,case]
i=nlat*nsea*ncase-ni ; number not the same
print,'Num lat*seas*case with NDJ4 same/diff=',ni,i
if i le 0 then print, 'All NDJ4 the same' else begin
   tr=reform(ttt,nhour,niti,nlat*nsea*ncase) ; [hour,item,lat*seas*case]
   tw=reform(tth,nhour,niti,nlat*nsea*ncase)
   qq=tr[*,*,ii]-tw[*,*,ii]     ; [hour,item, only same convg days)
   for k=0,niti-1 do begin 
      qt=qq[*,k,*]              ; one item
      aa=HSTATS(qt,['M','S','I','X']) & print,id52[k],aa,form=fmt
   endfor
endelse
end

;64x  file MUST have multiple cases, expected to be different N1
; i is case=nlay index
; j is lat index

641: begin & siz=size(ddd) ;  Convergence at surface as function of N1
if siz[5] lt 2 then goto,halt
klay=[30,27,23,22,21,20]     ; FIRMCODE number of layers in deep cases
nk=n_elements(klay)
i1=pari[9]>0 & i2=pari[10]<(nk-1) ; subset of depth = cases
klay=klay[i1:i2]                ; depth set to use
nk=n_elements(klay)
slay=ST0(klay,/nojoin)          ; layers as strings
i3=min(klay)-2                   ; index of shallowest bottom layer in ddd
j1=pari[11]>0 & j2=pari[12]<(nlat-1) ; subset of latitudes
klat=j2-j1+1                         ; number of subset latitudes
skat=slat[j1:j2]
lasy=nyr*leny-1                 ; last of full years
ddj=fltarr(J2-j1+1,nsea,nk) ; [latitude,season,case]
ddk=fltarr(J2-j1+1,nsea,nk)
for i=i1,i2 do ddk[*,*,i-i1]=ddd[klay[i]-2,0,j1:j2,*,i] ; [latitude,season,case]
for i=i1,i2 do ddj[*,*,i-i1]=ddd[i3,0,j1:j2,*,i] ; [latitude,season,case]
swig=ddd[0,1,*,*,*]-ddd[0,0,*,*,*]               ; diurnal swing in top layer 
swig=reform(swig,/over)                          ; [lat,season,case]
swig=transpose(swig,[1,2,0])                     ; [season,case,lat]
swig1=swig[0:lasy,*,*]                           ; get integral number of years
swir=reform(swig1[*,i1:i2,j1:j2],leny,nyr,nk,klat); [season,year,case,lat]
for i=0,i2-i1 do for j=0,j2-j1 do  $
swir[*,*,i,j]=AVALG(swir[*,*,i,j],swir[*,nyr-1,i,j],'-',dim=1)
;swig2=reform(swig,nsea*ncase,nlat) ;  [season*case,lat]
tsav=reform(total(ttt[*,0,j1:j2,*,i1:i2],1))/nhour ; Tsur ave. [lat,season,case]
tsy=reform(tsav[*,0:lasy,*],klat,leny,nyr,nk) ; [lat,season,case]
tsy=transpose(tsy,[1,2,3,0])      ; [season, year,case,latitude]
tsr=fltarr(leny,nyr,nk,klat)
for i=0,i2-i1 do for j=0,j2-j1 do  $
  tsr[*,*,i,j]=AVALG(tsy[*,*,i,j],tsy[*,nyr-1,i,j],'-',dim=1)
;tsry=total(abs(tsr),1)/leny ; MAR  [seas-in-year,lat] 
;tsr2=reform(tsr,leny*nyr,nlat)    ; [season,lat.]
; help,ttt,tsav,tsr,tsry,tsr2,ddd,ddj,ddk,ddr
CLOT,reform(tsr,leny*nyr*nk,klat),skat,yran=parp[0:1],yr2=parp[2:3] $
,locc=parp[4:7], titl=$
['season*ncase','TsurAve-last year',fame]
if parp[2] eq parp[3] then PLOTSECT,slay,parp[8] $
                      else PLOTSECT,[slay,slay],parp[8]
end

642: begin & k=pari[13]  ; Last year for all cases
i=where(klay eq k) & i=i[0]
if i lt 0 then goto,halt ; invalid number of layers
tsay=fltarr(leny,ncase,klat)
; ref is [season,{last year},{ref case},latitude]
for j=0,j2-j1 do  $ ; for case for lat 
tsay[*,*,j]=AVALG(reform(tsy[*,nyr-1,*,j]),tsy[*,nyr-1,i,j],'-',dim=1)
CLOT,reform(tsay,leny*ncase,klat),skat,yran=parp[9:10],yr2=parp[11:12] $
,locc=parp[13:16], titl= $
['season of last year * case','TsurAve -last year for N1 ='+strtrim(k,2),fame]
if parp[11] eq parp[12] then PLOTSECT,slay,parp[17] else PLOTSECT,[slay,slay],parp[17]
 end

643: begin  ; Convergence at specific depth REQ 641
ddr=reform(ddj,klat,nsea*nk) ; [lat,seas*case]
CLOT,transpose(ddr),skat,yran=parp[18:19],locc=parp[22:25] $
,titl=['season * case','Tmin at same depth',fame]
PLOTSECT,slay,parp[26]
end

644: begin ; Convergence at bottom REQ 641
ddr=reform(ddk,klat,nsea*nk)
CLOT,transpose(ddr),skat,yran=parp[27:28],locc=parp[31:34] $
,titl=['season * case','Tmin at bottom',fame]
PLOTSECT,slay,parp[35]
end

645: begin ; Convergence of top layer diurnal swing REQ 641
CLOT,reform(swir,leny*nyr*nk,klat),skat,yran=parp[0:1],yr2=parp[2:3] $
,locc=parp[4:7], titl=$
['season*ncase','Diurnal amplitude - last year',fame]
if parp[2] eq parp[3] then PLOTSECT,slay,parp[8] $
                     else PLOTSECT,[slay,slay],parp[8]
end

646: CLOT,reform(swir[*,*,i,*],leny*nyr,klat) $ ; One case REQ 642 
,skat,yran=parp[9:10], locc=[.5,.6,.025,.06] $
,titl=['season * year   N1='+strtrim(k,2),'Diurnal amplitude - last year',fame]

;___________________________

663: begin & ymin=parr[0]  & ymax=parr[1]; Check on last year of global-sol run
i=1 & j=1 & read,i,j,prompt='initial season and delta years > '
i1=i>0
i2=i1+j*leny
if i2 gt nsea-1 then begin
   print,'Latter time too late'
   goto,halt
endif
print,'Ls used:',lsv[[i1,i2]]   ; Ls of above seasons,
print,'  dates:',vvv[[i1,i2],0,0] 
yy=reform(ddd[*,0,*,i1,0]- ddd[*,0,*,i2,0]) ; [layer,lat]
xa=MEAN_STD(yy, std=xb)
if ymin eq ymax then ymin=min(yy,max=ymax) $ ; use full range
else if ymin gt ymax then begin               ; +/-  pari[1] StdDev
   ya=abs(ymax)*xb                            ; N-sigma
   ymin=xa-ya & ymax=xa+ya
endif                           ; else ymax is gt ymin and will use these
CLOT,yy,slat,locc=[.15,.15,.02,.06],yran=[ymin,ymax],yr2=parr[2:3] $
,titl=['KRC layer -2;  Ls='+strtrim(lsv[i1],2) $
,' Delta Tmin over '+strtrim(j,2)+' year','File: '+fame]
end

664: begin & ymin=parr[0]  & ymax=parr[1] ; maximum Tmin layer diff. from final season
ii=leny*indgen(nyr+1) & print,lsv[ii]; all are 350.661
nk=n_elements(ii) ; ddd is (layer,item,latitude,season,case)
yy=fltarr(nk,nlat) ; [season,lat] 
xx=reform(ddd[*,0,*,ii[nk-1],0]) ; tmin[layer,latitude] at reference date
for i=0,nk-1 do begin ; every year
   k=ii[i]            ; season in that year
   for j=0,nlat-1 do yy[i,j] =max(abs(ddd[*,0,j,k,0]-xx[*,j])) ; max over layers
endfor
xa=MEAN_STD(yy, std=xb)
if ymin eq ymax then ymin=min(yy,max=ymax) $ ; use full range
else if ymin gt ymax then begin               ; +/-  pari[1] StdDev
   ya=abs(ymax)*xb                            ; N-sigma
   ymin=xa-ya & ymax=xa+ya
endif                           ; else ymax is gt ymin and will use these
CLOT,yy,slat,locc=[.3,.4,.02,.06],yran=[ymin,ymax],yr2=parr[2:3] $
,titl=['recorded years',' Maximum of any layer Tmin from final year',fame] 
end

665: begin & siz=size(ddd)      ; Plot Tmin at bottom over season
j=siz[1]-1                      ; 0based index of the lowest saved layer
i1=0 & i2=0 & read,i,i2,prompt='First and last season index to plot > '
i1=i1>0 & i2=i2<(nsea-1)            ; insurance
print,'Season index range is',i1,i2
yy=reform(ddd[j,0,*,i1:i2,0]) ; tmin_bot [lat,season]
q='season index after '+strtrim(i,2)
CLOT,transpose(yy),slat,locc=[.4,.3,.025,.06] $
,titl=[q,'Tmin of lowest layer',fame]
tsav=reform(total(ttt[*,0,*,i1:i2,0],1))/nhour ; Surface temp average
ya=MEAN_STD2(yy,std=yb)
xa=MEAN_STD2(tsav,std=xb)
 print,xb/yb
tsr=reform(tsav[*,0:nyr*leny-1],nlat,leny,nyr)
tsr=transpose(tsr,[1,2,0])      ; [season, year,latitude]
for j=0,nlat-1 do tsr[*,*,j]=AVALG(tsr[*,*,j],tsr[*,nyr-1,j],'-',dim=1)
tsry=total(abs(tsr),1)/leny ; MAR  [ 
tsr2=reform(tsr,leny*nyr,nlat)    ; [season,lat.]
end

666: begin ; CLOT bottom T for one lat, all seasons.
read,i,prompt='Latitude index > '
zz=reform(ddd[j,0,i,*,0]) ; tmin_bot [season]
nyr=nsea/leny ; full years
zz=reform(zz[0:nyr*leny-1],leny,nyr)
print,'Ls of first point: ',lsv[0]
CLOT,zz,str0[0:nyr-1],locc=[.7,.9,-.025,.06] ,titl=['Season index within a year. Start at Ls '+string(lsv[0]),'Tmin of lowest layer',fame+'  Lat.='+slat[i]]
zzr=AVALG(zz,zz[*,nyr-1],dim=1)
end

667: CLOT,transpose(tsav),slat $ ; Plot Tsur_average REQ 665
,locc=[.4,.3,.025,.06],titl=[q,'Diurnal average of Tsur',fame]

668: CLOT,reform(tsav[i,0:nyr*leny-1],leny,nyr); CLOT Tsur_ave one lat, REQ 665,666

669: CLOT,tsry,slat,locc=[.4,.5,.025,.06] $ ; Plot Tsur diurnal avg, year MAR REQ 665
,yr2=[0.,.2],titl=['Recorded year. right half magnified' $
,'MAR of Tsur diurnal average for each year  - finale year',fame] 

671: begin ; Plot final Midnight Tsur
tt0=reform(ttt[23,0,*,nsea-leny:nsea-1,0]) ; midnight tsur 
CLOT,transpose(tt0),slat,locc=[.15,.3,.025,.06] $
,titl=['season index in last recorded year','Midnight Tsurf',fame] & end

672: begin ; Plot difference from last season. List NDJ4
tt0=reform(ttt[23,0,*,*,0]) ; midnight tsur 
ii=leny*indgen(nyr+1) & print,lsv[ii]; all are 350.661
tt1=AVALG(tt0,tt0[*,ii[nyr]],'-')
tt2=tt1[*,ii]
xa=MEAN_STD(tt2, std=xb)
print,'Ave and StDev of Tsur at midnight, end of all years',xa,xb
if luf then printf,luf,'Ave and StDev of Tsur at midnight, end of all years',xa,xb
ymin=parr[0] & ymax=parr[1]                   ; plot limits
if ymin eq ymax then ymin=min(tt2,max=ymax) $ ; use full range
else if ymin gt ymax then begin               ; +/-  pari[1] StdDev
   ya=abs(ymax)*xb                            ; N-sigma
   ymin=xa-ya & ymax=xa+ya
endif                           ; else ymax is gt ymin and will use these
CLOT,transpose(tt2),slat,locc=[.2,.3,.025,.06],yran=[ymin,ymax] $ ; ,yr2=parr[2:3] $
,titl=['At end of recorded year','Midnight Tsurf Relative to last year',fame]
end

673: begin                      ; List NDJ4  FOLLOW 672
yy=reform(ggg[0,*,*,0]) & help,yy
if n3 gt 3 then begin
   print,'Idx  Lat',1+indgen(n3),form='(a8,20i4)'
   if luf then printf,luf,'@672: Distribution of NDJ4 = days of iteration'
   if luf then printf,luf,'Idx  Lat',1+indgen(n3),form='(a8,20i4)'
   for j=0,nlat-1 do begin 
      jj=histogram(yy[j,*],bin=1,min=1,max=n3) 
      print,j,slat[j],jj,format='(i3,a5,20i4)' 
      if luf then printf,luf,j,slat[j],jj,format='(i3,a5,20i4)' 
   endfor
   endif & end

68: begin                       ; Compare skip with everySol 
j=1  ; Delta years from JDISK in skip to everySol
th0=reform(tth[23,0,*,*,0]) ; midnight tsur  skip = 18-sol skip
tt0=reform(ttt[23,0,*,*,0]) ; [lat,season] midnight tsur  everySol
iih=lenh*(indgen(nyr+1)+j)  ; year end in Skip run
iii=leny*indgen(nyr+1)  ; every sol that matches last of 18
print,iih & print,iii
print,lsh[iih] & print,lsv[iii]
print,vvh[iih,0,0] & print, djmm[iii] 
i=iii[4]                        ; end of the last complete year
yh=th0[*,iih]                   ; [lat,year]  skip, at end of each year
th1 =AVALG(yh,tt0[*,i],'-')     ; delta same Ls
thm1=AVALG(yh,tt0[*,i-1],'-')   ; one sol earlier
thp1=AVALG(yh,tt0[*,i+1],'-')   ; one sol later
qq=strtrim(indgen(nyr+1)+round(jdisk/float(leny)),2) ;  years into run
CLOT,th1,qq,yran=parr[16:17],locc=parr[20:23],titl=['Latitude index' $
,'Midnight Tsurf Relative to last year',fame]
plots,[0,18],[0,0],line=1
CLOT,thm1,oplot=-1,line=1 & xyouts,.6,.20,'dots: 1 sol earlier',/norm,chars=2.
CLOT,thp1,oplot=-1,line=2 & xyouts,.6,.25,'dash: 1 sol later',/norm,chars=2.
j1=pari[11]>0 & j2=pari[12]<(nlat-1) ; subset of latitudes
xa=total(abs(th1[j1:j2,nyr]))/13 ; j1:j2 is lats to avoid polar 
ya=total(abs(thm1[j1:j2,nyr]))/13 ; MAR for the last year, Skip-everySol
yb=total(abs(thp1[j1:j2,nyr]))/13
q=string('@68: MAR over lats ',slat[[j1,j2]],' -,0,+ :',ya,xa,yb,form='(4a,3f6.3)')
print,q
if luf then printf,luf,q
end 

682: begin & yt=tt0[*,iii]; Difference at each year end FOLLOW 68
yy=yh-yt ; skip-every-sol  at end of each year
;yy[*,0]=0. ; first not at same season
CLOT,yy,qq,yran=parr[16:17],locc=parr[20:23],titl=['Latitude index' $
,'Midnight Tsurf: Skip Run - every_sol',fame]
end

; 77777777777777777777777777777777 Test one-point mode
71: begin & kfile=parf[4]+parf[3]+'.prt' ; Test one-point mode
mrow=pari[4] ; Most rows to read
sss=READTXTCOL(kfile,nskip=-1,ncol=12,mrow=mrow)
siz=size(sss) & if siz[0] ne 2 then goto,halt
numA = siz[1]
if numa ge mrow then print,'There may be more than the rows read=',numa
ppa=float(sss[*,1:11])          ; convert all numeric to float
tppa=ppa[*,9:10]                ; just the 2 output columns
kfile=parf[9]+parf[8]+'.prt' ;------------------------------- 2nd file 
sss=READTXTCOL(kfile,nskip=-1,ncol=12,mrow=mrow)
siz=size(sss) & if siz[0] ne 2 then goto,halt
if siz[1] ne numa then goto,halt ; require number of rows read be the same
print,'OnePoint rows read=',numa
ppb=float(sss[*,1:11])  
tppb=ppb[*,9:10]
qq=tppb-tppa                    ; difference: File B -A
xa=min(qq,max=xb)
print,'Range of OnePoint T differences',xa,xb
ya=MEAN_STD(qq,std=yb)
print,'    B-A  Mean and stdDev=',ya,yb
ya=MEAN_STD(abs(qq),std=yb)
print,'abs(B-A) Mean and stdDev=',ya,yb
xa=MEAN_STD(qq[*,1]-qq[*,0],std=xb)
print,'Delta (Tp-Ts) Mean and stdDev=',xa,xb
end

72: begin ; Check annual trends
if nsea mod leny ne 0 then goto,halt 
read,k,prompt='Hour index > ' & k=(k>0)<(nhour-1)
read,i,prompt='Reference case > ' & i=(i>0)<(ncase-1)
t12=reform(ttt[k,0,*,*,*]); hottest hour     [lat,season,case]
t12=transpose(t12,[1,2,0]) ;  [season,case,lat]
t12=reform(t12,nspy,nyr*ncase,nlat,/over) ; [season,year*case,lat]
t12r=   fltarr(nspy,nyr*ncase,nlat) ; [season,year*case,lat]
i1=nyr*i+nyr-1                      ; index of last year of case i
for j=0,nlat-1 do begin
   xx=t12[*,i1,j]                ; last year of first case
   t12r[*,*,j]=AVALG(t12[*,*,j],xx,'-')
   endfor
t12r=reform(t12r,nsea,ncase,nlat,/over) 
again72: read,j,prompt='Lat index, -=done > '
if j lt 0 then goto,ask         ; done
j=j<(nlat-1)
yy=t12r[*,*,j]
ymin=parr[8] & ymax=parr[9]                   ; plot limits
if ymin eq ymax then ymin=min(tt2,max=ymax) $ ; use full range
else if ymin gt ymax then begin               ; +/-  pari[1] StdDev
   xa=MEAN_STD(yy, std=xb)
   ya=abs(ymax)*xb              ; N-sigma
   ymin=xa-ya & ymax=xa+ya
endif
CLOT,t12r,scase,locc=parr[12:15],yran=[ymin,ymax] $
,titl=['Season index:  '+strtrim(nspy,2) $
+' /year' ,'Ts relative to last year for first case',stem+':  hour='+sour[k] $
+' Lat='+slat[j]]
plots, !x.crange,[0.,0.],line=1    
goto,again72     
end

73: begin ; generate pressure input series
ppp=[10000.,5000,2000,1000,500,200,100,50,20,10,5,2,1.01, 0.5]
qq=10.^(.5*findgen(13)) ;very wide P range
x1=555. & y1=0.11             ; one point on relation
xfu=5.                        ; factor for 1/2 of full change
xlin=100.                     ; independent value below which relation is linear
ccc=LINATAN(ppp,x1,y1,5.,100.,cln=cln) ; crude band model , downwelling coef.
for j=0,n_elements(ppp)-1 do begin 
   x=ppp[j]                     ; P total
   print,'1 12 ',ppp[j],' ''PTOTAL''  / ' ; firm-code list
   print,'1  9 ',ccc[j],' ''CABR''  / '  ; crube band model
   print,'1 17 ',ppp[j]*0.3/546.,' ''TAUD''  / ' ; linear with P
   print,'0/   end of case'
endfor
print,'0 /  End of Run'  
end

74: begin & kmax=pari[5] ; Generate a large grid .one file
 ofile=parf[0]+parf[11]
openw,lun,ofile,/get_lun
printf,lun,'Generated in krcvtest.pro @ 77'
fmt=       '(i2 ,f6.1, f6.1, f6.2,f5.1,f5.2,  f7.1,f5.2,f5.1,f5.0,i7)'
;                     F6.1,F6.1,F6.2,F5.1,F5.2,F7.1,F5.2,F5.1,F5.0,A20)
printf,lun,'C_END  Ls   Lat  Hour Elev  Alb Inerti Opac Slop Azim'     
;            11   0.0 -10.0 20.00  1.0 0.20  400.0 0.20 15.0  90.
k=0
for c8 = 0.,16.,15.  do begin      ; Slop  2
    c9 = 45.  ; Azim
for c1 = 0.,359., 70. do begin     ; Ls    6
for c2 = -40.,41.,40. do begin     ; lat   3 
for c3 = 7., 14., 6.5 do begin     ; Hour  2
for c4 = -3.,0,2.     do begin     ; Elev  2
for c5 = 0.20, 0.35, 0.1 do begin ; Albedo 2
for c6 = 100., 800., 500. do begin ; Inertia 2
for c7 = 0.2, 0.31,0.1 do begin  ; Opac    2
   k=k+1 
   printf,lun,11,c1,c2,c3,c4,c5,c6,c7,c8,c9,k,form=fmt 
   if k ge kmax then goto,done77
endfor & endfor & endfor & endfor & endfor & endfor & endfor & endfor
done77: free_lun,lun
print,'Last index=',K
end

75: begin  ; SHOWBYTES for start of parf[0+11]
ofile=parf[0]+parf[11]
openr,lun,ofile,/get_lun
aaa=bytarr(300)
readu,lun,aaa
SHOWBYTES,aaa
stop
point_lun,lun,0
end

76: begin ; Find most extreme season for start
; must have skip model loaded
q=LSUBS(djmm[0:37],aud) ; first recorded year+1
chart,aud ; look at Subsolar declination and heliocentric distance
xx=cos(!dtor*aud[*,1])*(aud[*,0]/1.52371)^2 ; combine the effects
plot,xx ; look for min as hottest in south
print,lsv[29],djmm[29] ; hottest in south ; 264.485      671.179
; start at 
end

; grip.one shows no extra bytes, LF at end of each line
77: begin & openw,luf,parf[12]+parf[13],/get_lun ; Open report file
printf,luf,ptitl+' Report '+isotime(1) ; yyyymondd hh:mm:ss as one word
printf,luf,'Last read= ',fame
printf,luf,'Held file= ',ifh
end 

78: begin & free_lun,luf ; Close report file 
luf=0
print,'Report file is  ',parf[12]+parf[13]
end

else: begin & KON91,ptitl,prior,hold,kon,kons,kitel ;=KON99 < needed by MAKE99
      if kon eq 99 then begin 
          print,'11: files: parf= ',parf
          print,'12: Integs: pari= ',ST0(pari)
          print,'15: Floats: parp= ',ST0(parp)
          print,'16: Floats: parr= ',ST0(parr)
      endif & end
endcase
goto, ask

halt:  print,'SOME ERROR CONDITION at kon=',kon,'.  Any key to Go'
i=get_kbrd(1) & goto,sureask
end
