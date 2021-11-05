;_Titl  KV3  Check consistency of KRC within and between versions
;_Desc. Reads  KRC type 52, 0 , -1 
; Strive for consistency in subtraction of similar item: ttt-tth = A-B
;_Calls via mycalltree  2016 Jun 20 20:24:13
;  ABRAD  AVALG  BIN5  CALL91  CHART  CLOT  CUMSUM  DEFINEKRC
;  FLAYER  FREELUN  GETPAN  GETPINTS  GETPSN  HISTFAST  HSTATS  KOFTDEL  KON99
;  KRCCOMLAB  KRCHANGE  KRCINDIFF  KRCLAYER  KRCLISTNAME  KRCLISTVAL  KRCSIMPLE
;  KRCSIMPLOT  LASTPERIOD  LINATAN  LSAM  LSUBS  LSUBSGEN  MEAN_STD  MEAN_STD2
;  NOWHERE  NUMGEOMLAY  PAUSE  PLOTSECT  PM180  PRINTJCOLS  QKRCSIMP  QUILT3
;  READKRC1  READKRC52  READKRCCOM  READTUN  READTXTCOL  SETCOLOR
;  SHOWBYTES  SIGSB  ST0  STRWORD1  TTTMOD  TVFAST  VALUES  VEC2CODE
; Addition calls via kon91:  COLOR24BIT  DELAST0  GRAPH  MAKE99  
;    SETWIND  SUBTITLE  TOOTHB
;_Hist 2013sep03 Hugh Kieffer
; 2014feb27 HK Turn into procedure with path's as argument
; 2014apr14 HK Make program for krcvtest procedure, include lots from hp3.pro. 
; 2014jun03 HK Incorporate READTUN 
; 2016jan06 Transfer KRCSIMPLE treatment and Mars circular tests from gcmcomp to
;  here
; 2016may25 HK Accomodate version 34 styles of direct-access files
; 2017aug15  tsm and tsz never defined! was for type -1 type 0 
; 2017dec to 2018 feb  Treat eclipse and  TFINE output
; 2018oct HK More for fff, replace all ucase with caset
; 2020apr06 HK Revise file some file defaults and selection @114

 common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,scex1
;_End        .rnew kv3

;        1         2         3         4         5         6         7         8
;2345678901234567890123456789012345678901234567890123456789012345678901234567890
;2345678901234567890123456789012345678901234567890123456789012345678901234567890;2345678901234567890123456789012345678901234567890123456789012345678901234567890123456
; 166 colm for 2cdole emacs
ptitl='kv3' & prior=['q','q']
homedir=getenv('MYHOME')        ; get current home
hcpu=getenv('HOST')             ; get current CPU name
;solib=getenv('SOLIB')           ; location of shared object library
krcdat=getenv('PROJDAT')        ; KRC data files
krcsrc=getenv('PROJSRC')        ; KRC input files
idltop=getenv('IDLTOP')         ; expected top of current IDL run directory
cd,current=curd                 ; get current directory

; If testing KRC distro IDL
if hcpu eq 'hulk4' and curd eq '/work1/build/idl/' then krcdat='/work1/build/run/'
;if homedir eq '/u/hkieffer/' then krcdat='/work/hkieffer/krc/test/' ; Mac

qq='/home/hkieffer/krc/tes/out/'  & q='V331' ; placeholders
labf=['VerA=new DIR   200',' " case file   202',' " multi stem  203' $
,' " OnePoint [.prt]',' " DIR for prt' $
,'VerB=prior DIR 201',' " case file   202',' " multi stem  203' $
,' " OnePoint [.prt]',' " DIR for prt','DIR for IDL output' $
,'Output onePoint set','Report file stem', ' " .ext',' TUN Directory' $
,' " file',' " extension',' DirectAccess ext' ] 
parf=[qq,q+'test1',q+'test2',q+'Mone','/home/hkieffer/krc/tes/' $
,qq,q+'test1',q+'test2',q+'Mone',qq,idltop,'grid.one',q $
,'Report.txt','/work1/krc/mars/','InSi102','.tab','.t0'] 
parf0=parf ; remember the initial values

labi=['3-digit file version','# seasons @411 46 744','N hours @46' $ ; 0:2
,'item index @522','rows in OnePoint @71','max output rows @732' $ ; 3:5
,'single Lat index ->jlat, set to jeq' $ ; 6
,'CASE index: 0-based, first ->jc',' " " last.  <=all' $ ; 7:8
,'DEPTH index \ first '    ,'   " last   / @64x, 783' $ ; 9:10 @641 784 785 786
,'LATITUDE  index,first ','   " last <=all' $ ; 11:12 @561 641 68
,'ref. layers @642',' Lines in fort. @752','TUN index @74 743' $ ; 13:15
,'Hour index ->ihour, set to ihot'  $ ; 16
,'SEASON index first or only ->jsea','   " last or <=all' $ ; 17:18 @561
,'year index','2nd case index' $ ; 19:20
,'item index <-> J5','verbosity flag' $ ;21:22 
,'-n file type @50','leading hour gap @ 782','item index ->it1' $
,'fall= keep all t52 cases'] 
pari=[34,20,6,1,1152,2000,3,0,-1, 0,5,0,-1,23,5760,2,19,0,-1,6,4,0,1,-2,-14,0,0]

labj=['1= QUILT patch ID @56x 0=none','Index of main',' " add' $
,'1000*fraction to add','QUILT mag @ 574',' " " 563,4,5' $
,'-=do all +=which one item','@567 Min T for compare','QUILT Flag: grid DN' $
,' "  prt: +1=Zrange +2=axes meanings','flag: reduce hours to 24 @57' $
,' " Abs(delta) @56,563']
parj=[0,0,3,100, 3,5,-1,160,-90,3,0,0]

labk='List of - cases, -1, then + cases'
park=[0,1,-1,6,7]

labg=['Zones INT','RLAY','FLAY','desired Total D depth','Period,days' $
,'N1=nlay+1 INT','N2 INT','@53 z2',' " f2',' " cond Surf',' " " deep 0' $
,' " " deep 1',' " denn surf',' " " deep','" which cond. INT' $
,'Use for zones: rlay',' " " flay']
parg=[20,1.12, .12,130.,1.0275,44,1536, 10.,.95,0.01,0.02,0.1,1000.,1750.,1 $
,1.12, .12]

labp=['@641 Yplot Min',' " max: = =auto, < =sigma',' Ymag min',' " max' $
,'X loc   \/ @--','Y loc   | Curve-','Delta Y | Guide for','LineLen /\',' " plotsect Y______' $
,'@642 Yplot Min',' " max: = =auto, < =sigma',' Ymag min',' " max' $
,'X loc   \/ @--','Y loc   | Curve-','Delta Y | Guide for','LineLen /\',' " plotsect Y______' $
,'@643  Yplot Min',' " max: = =auto, < =sigma','- Ymag min','- " max' $
,'X loc   \/ @571a','Y loc   | Curve-','Delta Y | Guide for','LineLen /',' " plotsect Y______' $
,'@644  Yplot Min',' " max: = =auto, < =sigma','- Ymag min','- " max' $
,'X loc   \/ @571b','Y loc   | Curve-','Delta Y | Guide for','LineLen /',' " plotsect Y______' ]
parp=[-3.,3.,-0.1,0.1,0.37,0.7,0.025,0.06,0.1,-1.,1.,0.2,0.2,0.4,0.7,0.025,0.06,0.2,0.,0.,0.,0., .07,.93,-.025,.03,.1, 0.,0.,0.,0.,0.4,0.12,0.025,0.06,0.9]

labr=['@663,664,672 Yplot Min',' " max: = =auto, < =sigma',' Ymag min',' " max' $
,'X loc   \/ @--','Y loc   | Curve-','Delta Y | Guide for','LineLen /\_______' $
,'@72 Yplot Min',' " max: = =auto, < =sigma',' Ymag min',' " max' $
,'X loc   \/ @--','Y loc   | Curve-','Delta Y | Guide for','LineLen /\_______' $
,'@68,682  Yplot Min',' " max: = =auto, < =sigma',' Ymag min',' " max'$
,'X loc   \/ @--','Y loc   | Curve-','Delta Y | Guide for','LineLen /\_______'  $
,'@50 min lat',' " max lat']
parr=[1.,1.,0.0,1.0, .8,.94,-.025,.06,   7.,1.,-1.,1, .05,.47,-.025,.04 $
   , -1,0.5,-.5,0.5, .5,.60,-.025,.06, -46.,46.  ]

labu=['com.fd','com.elev','Tiny for log','Tsurf','Tplan','Tatm','DownVIS','DownIR' $
,'Tmin','Tmax','NDJ4','DTM4','TTA4','FROST4','AFRO4','HEATMM' $
,'Lat','Elev','DJU5','SUBS','PZREF','TAUD','SUMF']
paru0=[1.e-6,1.e-6,1.e-7, .001,.002,.002,.0001,.0001 , .001,.001 $
, .01,.001,.001,.001,.001,.0001, .001,.001,  .01, .01, .01,.01,.01 ]
paru=paru0

; ver,          dir,               soly,        19 lats,  other  [4,*]   Optional changes
;  0             1                    2             3         4
parff=[ $
 '232','/work2/KRC/232c/run/out/','V232test1b','V232test2b','232v3t'   $ ;0 232
,'241','/work2/KRC/241/run/out/' ,'V241test1' ,'V241test2' ,''         $ ;1 241
,'321','/work2/KRC/321/run/out/' ,'VerTest'   ,'VerTest2'  ,'321v3t'   $ ;2 321
,'331','/work2/KRC/331/run/out/' ,'V331test1' ,'V331Test2' ,''         $ ;3 331
,'341','/work1/krc/test/'        ,'341aTest'  ,'341aTest2' ,'candi341' $ ;4 341
,'342','/home/hkieffer/krc/tes/out/','V342Test','V342Test2','342v3t'   $ ;5 342
,'355','/home/hkieffer/krc/tes/out/','-'       ,'-'        ,'355Test'  $ ;6 355
,'361','/home/hkieffer/krc/tes/out/','-'       ,'-'        ,'361Test'  $ ;7 361
,'364','/work/work2/KRC/364/run/out/','-'      ,'-'        ,'363Test'  ] ;8 364
   
i= n_elements(parff) & npf=i/5 & parff=reform(parff,5,npf,/over)
j1=npf-1 & j2=npf-2 ; first of 3 lines for insurance
parf[0:1]=parff[[1,4],j1] ; new file
parf[5:6]=parff[[1,4],j2] ; older base file
; parff[4,2]='321NoAtmb'

lab52=['Hour','item','Lat.','Season','Case'] ; dimensions of type52

clrr=[255,254,100,200,150,60] & nclr=n_elements(clrr); cases in color
thkk=replicate(1.,6)
linn=[0,2,3,4,5,1] & nlin=n_elements(linn)
;sti2=string(indgen(99),form='(I02)') ; 0-based, always 2 characters
sti2=string(indgen(99),form='(I2)') ; 0-based, always 2 characters
stid=strtrim(indgen(99),2) ; 0-based, no blanks or leading 0

hkode=['M','S','I','X','MA','XA'] ; statistics desired
zz=HSTATS(paru,hkode,lab=hlab)  ; get labels
hfmt='(a8,6f11.5)'
htit=string('Item',hlab,form='(a8,6a11)')
;hfmt=['f6.3','f5.3','f8.3','f7.3','f7.3','i6'] ;  blanks will be trimmed
des52=['Surface kinetic','TOA Bolometric','Atmosphere','Down-going Vis' $
,'Down-going IR'] ; labels
dj2000=2451545.D0                ; JD of epoch J2000
k24=dj2000-2440000.              ; Offset for KRC before 2013
solsec= 88775.224D0              ; Mars mean solar day in seconds
myear=686.9929D0                 ; mars year in Days
daysec=86400.d0                  ; Day in seconds
SIGSB = 5.67051D-8               ; Stephan Boltzman constant
get52='' ; file requested by KRC35
;;kist=[101,103,109,115,117,119,207]; @221 KRCCOM items in n*100+i format
;;kist=[201,202,208,207,133,134]; processed at 22
;;kiform=['f6.3','f7.1','f6.3','f7.1','f6.3','f6.3','i3'] 
kist  =[201, 202, 208, 207,  133,   134] & cid=[0,1,2,3,4,5]; processed at 22
kiform=['i4','i6','i4','i6','f6.3','f6.3'] 
ucase=['Nominal','NoAtm','N1=22','KofT','KofTfixed'] ; IDs for a specific run
ncase=1 & kr52file='q' & FREELUN & fhold=lonarr(4) & sflag=0 ; insurance
cased=['noneYet'] & prior=['q','q'] & kins=[-1] & irr=0  & nsea=2; insurance 
i=1 & j=1 & it1=0  & luf=0 & ifh='-none-' & steh=ifh & pres=600. ; insurance
nhour=24 & nlat=5 & krct=replicate(-1.,50) & jlat=0 & ihour=0 & it1=0 ; insurance
kite=' ' & komit=0   &  paw=-1 ; type definitions
prior=['1','1']                 ; impossible values, to initiate MAKE99
lkon=0B
kons=[851,880,861,206,256]  ; required for definitions;
kon=123 & goto,dokon            ; do them immediately

;===============================================================================
ask: ;---------------------------------------- interactive parameter change
konl=kon
if lkon then begin              ; auto-sequence
    kkon=kkon+1                 ; increment to next item
    kon=kons(kkon)              ; get action from list
    print,ptitl,' -------------->',kon ; Notice of current action to monitor
    if kkon eq lastkon then lkon=0B ; turn off further auto-processing
endif else begin
    sureask: lkon=0B                 ; forced request for action
    READ, kon, prompt=ptitl+' Enter selection: 99=help 0=stop 123=auto> '
endelse
dokon: kitel=kite               ; remember prior action to use in subtitle
kite=ptitl+'@'+strtrim(kon,2)+' '   ; follows date in subtitle      fils[2]+' '+
nkc=kcc[2] & nkl=kcc[3]         ; in case SETCOLOR was called
case kon of ;...................................................................
; DO NOT USE those defined in KON91::
; -1 -3 -9 100:3 121 122 8 80 85 87 88 801:4 808 850:860 880:899  
; 9 99 991 992 994 995

 0: stop  ;- Stop

-1: begin & print,'Any key to GO' & qi=get_kbrd(1) & end ; Wait
-4: if CALL91(ptitl,'kv3-4') then stop ; CALL91

110: parf=parf0 ; Reset names to default

;111: kons=[206,252,23,22,18,188] ; Read one file
111: kons=[200,202,207,252,23,22,18,188,212,228,12] ; Read Ver A

112: kons=[41,-1,42,43,-1,44,-1,45,-1,46] ; Test cases

113: kons=[200,203,207,252,50,51,18] ; Read 3 types for Ver A

114: begin & k=npf-1 ; Select version and run names from firm-code
print,' i ver           top dir                  2=soly  3=19_lats   4=vTest'
;     ' 8 364      /work/work2/KRC/364/run/           -          -   363Test
for i=0,k do print,i,parff[*,i],form='(i2,a4,a30,a12,a11,a10)'
read,i,prompt='Column: 2 to 4 > '
i=(i>2)<4 ; ensure valid
j1=0 & j2=0 & read,j1,j2,prompt='i or Version number: new  base > '
if j1 gt 100 then begin
q=strtrim(j1,2) & parf[0]='/work2/KRC/'+q+'/run/out/' ; new file
parf[1]=q+'Test' & endif else parf[0:1]=parff[[1,i],j1]
if j2 gt 100 then begin 
q=strtrim(j2,2) & parf[5]='/work2/KRC/'+q+'/run/out/' ; older base file
parf[6]=q+'Test' & endif else parf[5:6]=parff[[1,i],j2] 
if i eq 3 then pari[11:12]=[4,15] else pari[11:12]=[0,4]; latitude range
end

115: kons=[201,202,207,252,22,26,200,207,252,22,254,222,12] ; ttt=A, tth=B
; read B1=Parf[5+6];t52, save as -h, then read A1=parf[0+1]

;116: kons=[61,-1,62,63] ; Compare versions
116: kons=[233,55,56,561,562,564,565,61,622,-1,63] ; Compare versions After 115

117: kons=[432,43,435,-1,44,-1,445] ; look at effect of atm

118: begin & read,i,prompt='Which file set? > '  ; Select file set. Before 115

; 119: kons=[501,-1,52,-1,53,-1,55] ; Test between types
case i of  ; . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    0: begin & kons=[11,206,252,22,212,213,228,779,781,782] ; Compare KRC with SIMPLE 2016
    parf[0:2]=['/work1/krc/test/','MarsCir','J']
    kist=[133,134,135,201,202]  ; RLAY, FLAY, convg,Nlay, N2
    kiform=['f6.3','f6.3','f6.2','I4','I6'] & cid=1
    print,'@206 Using Parf  0+1+2 +.t52'
    end
    1: begin & kons=[11,206,252,22,6] ; KRC multi-year vrs depth 2016
    parf[0:2]=['/work1/krc/test/','Mars','D']
    kist=[133,134,135,201,202]  ; RLAY, FLAY, convg,Nlay, N2
    kiform=['f6.3','f6.3','f6.2','I4','I6'] & cid=1
    print,'@206 Using Parf  0+1+2 +.t52'
    end
    2: begin & parf[9]='/home/hkieffer/krc/robin/18may15/' ; Robin 343 vrs 355 
    parf[[0,5]]=parf[9]+'out/'
    parf[1]='343sk0100a008t002' & parf[6]='35Bsk0100a008t002'
    end
    3: begin & parf[9]='/home/hkieffer/krc/robin/18may28/' ; Robin 343 vrs 355 
    parf[[0,5]]=parf[9]+'out/'
 ;   parf[1]='343sk0100a008t002' & parf[6]='355sk0100a008t002'
    parf[1]='343i3' & parf[6]='355i3'
    parr[24:25]=[-46.,46]
    end
    4: begin & parf[9]='/home/hkieffer/krc/robin/18jun06/zip/' ; Robins 343 vrs 355 
    parf[[0,5]]=parf[9]
 ;   parf[1]='343sk0100a008t002' & parf[6]='355sk0100a008t002'
    parf[1]='343i3' & parf[6]='355i3'
    parr[24:25]=[-46.,46]
  end

    5: begin & kons=[201,202,207,252,22,26,200,207,252,22]; Bennu Ec vrs EE
      parf[0:1]=['/work1/krc/beam/','BEnA/BEnA02A00']
      parf[5:6]=['/work1/krc/beam/','BEnE/BEnE02E00']
    end

    50: begin  ; My 343 vrs 355  then 115 123 116 123
    parf[0:2]=['/home/hkieffer/krc/robin/18may28/out/','343i3',''] 
     parf[5]=parf[0] & parf[6]='356ki3'
    parr[24:25]=[-99.,99.]
    end
    51: begin  ; My 343f vrs 355f  then 115 123 116 123
    parf[0:2]=['/home/hkieffer/krc/robin/18may28/out/','343fi3',''] 
    parf[5]=parf[0] & parf[6]='355fi3'
    parr[24:25]=[-46.,46]
    end
    53: begin & kons=[201,202,207,252, 206,50, 18,182,51,512]  ; My .tm2 vrs .t52
    parf[5:7]=['/home/hkieffer/krc/robin/18may28/out/','343i3',''] ; 343 .t52 
    parf[0]=parf[5] & parf[1]='343i6' & parf[2]='' & parf[17]='.tm2'  ; tm2 I60
    pari[0]=343 & pari[23]=-2 ;  version and file type
    parr[24:25]=[-99.,99.]      ; keep all lats
    end

    55: begin ; for fort.5x files
    parf[0:2]=['/home/hkieffer/krc/robin/18may28/out/','3431',''] 
    parf[5]=parf[0] & parf[6]='3561'
    parr[24:25]=[-46.,46] & pari[0]=351 & end

    63: begin & kons=[201,202,207,252, 206,50, 18,182,51,512] ; Robins .tm2 vrs .t52
    parf[5:7]=['/home/hkieffer/krc/robin/18jun06/zip/','343i3','']
    parf[0]=parf[5] & parf[1]='343i6' &  parf[17]='.tm2'
    pari[0]=351 & pari[23]=-2 ;  version and file type
    parr[24:25]=[-99.,99.] ; keep all lats
    end
    65: begin & parf[1]='355i6'& parf[6]='355i3'  ; " " 355, REQ 118:53
    pari[0]=355 & end
  
;parf[[1,2,6,7]]=['V4','K4','V48','K48']
;parf[[0,1]]=['/work1/krc/mars/','InSiN1']
;parf[0:2]=['/work1/krc/beam/','Beam','BaF00'] ; beaming
;parf[0:2]=['/work1/krc/beam/','Beam','BaF00'] ; test v341

;parf[0]='/work/work2/KRC/342/run/out/'   ; 2016dec05 set
;parf[1]='VerTest34rlf' ;
;parf[5]='/work/work2/KRC/321/run/out/'
;parf[6]='VerTest321rlf' ;

;parf[0]='~/krc/robin/'   ; 2016dec08 replicateset
;parf[1]='rlf342' ;
;parf[5]='~/krc/robin/'
;parf[6]='rlf321' ;

;parf[0]='~/krc/tes/out/'   ; 2018ajna30   355 vrs 344
;parf[1]='354Test' ;
;parf[5]='/work2/KRC/344/run/out/'
;parf[6]='354Test' ; 
;ucase=['base','Tdep','vFrost','noAtm','lunar','lunarTd','lunarTc','Slope','Far']
;ifilp='354-344' ; custom run name
  else: print,'invalid set number'
endcase & end

131: kons=[77,411,43,-1,44,-1,45,26,203,207,252,50,51,512,514,-1,55] ; Test one version

132: kons=[26,201,207,252,61,550,63,545,-1,546,613] ; read 2nd  version AFTER 131 or 111

133: kons=[200,203,207,252,28,671,673,-1,664,-1,672]; Long runs A

134: kons=[26,201,207,252,671,-1,68,-1,682,29]; Long runs B-A

135: begin & kons=[11,12,206,252,22,781,782,0,6] ; KRC annual runs
parf[0:2]=['/work1/krc/test/','Mars','D']
pari[7:8]=[0,8]
pari[11]=2 & pari[24]=-14  ; extreme latitude
kist=[133,134,135,201,202] ; RLAY, FLAY, convg,Nlay, N2
kiform=['f6.3','f6.3','f6.2','I4','I6'] & cid=1
print,'@206 Using Parf  0+1+2 +.t52'
end

136: begin & kons=[200,202,207,22,252,253,862,48,-1 $ ; Thin runs, 1-lat
,482, -1, 26,201,202,207,252,253,482,-1,48]
parf[0:1]=['/work1/krc/test/','thin4']
;;kkk=[114,113,112,111,104,208,207,206,201,202,203,300] 
;;kkk=[114,113,112,111,202,206,203,204,205,207,208,209,210,211,300]
kkk=[114,113,112,111,202,206,203,204,205,207,208,209,210,211,300]
;;kkd=[0,1,2,5,6,11] 
kkd=[0,1,14,13] 
parf[5:6]=['/work1/krc/test/','thin6'] & end
; may10: 5 lat skip, R4 and R8. Then 1;at soly R4 and R8

137: kons=[484,485,486,-1,487,488,489] ; Thin combine and plot

138: begin & kons=[11,206,252,22,212,213,228,45,781,782] ; test version 3.3
parf[0:2]=['/work1/krc/test/','mtime','B']
pari[11]=1 ; set lat to -30
kist=[133,134,201,135,202] ; RLAY, FLAY,Nlay, convg, N2  First 3 REQ @ 228
kiform=['f6.3','f6.3','I4','f6.2','I6']
cid=[0,1,2,3,4] ; items of kist for curve labels 
end

139: begin & kons=[11,206,252,22,212,213,228,27,781,782,-1,784] ; test version 3.3
parf[0:2]=['/work1/krc/test/','test33','C']
pari[11]=1 ; set lat to -30
;         RLAY   FLAY Nlay LkofT LZONE PTOTAL ARC2/P IIB  IC2 First 3 REQ @ 228
kist=     [133,   134, 201, 310,  315,    112,  121, 207, 208] ;
kiform=['f6.3','f6.3','i4','i3', 'i3', 'f7.2','f7.0','i6','i4']
cid=[2,3,8,7] ;cid=[7,8,2,3] ; items of kist for curve labels  IIB IC2 N1 KofT
heatflow=1.  ;<<< value for MarsCirN,P
end

141: begin & kons=[200,202,207,252,22,26,201,207,252,22,550,233,561,562,563,564,565,  61,-1,62,63] ; candi; 115+116
parf[0:1]=['/home/hkieffer/krc/candout/','formal']
parf[5:6]=['/home/hkieffer/krc/candout/','candi']
end

142: begin ; set to Robin august tests
parf[0:1]=['/home/hkieffer/krc/robin/','V241_test1']
parf[5:6]=['/home/hkieffer/krc/robin/','V321_test1'] & end

144: begin & kons=[200,203,207,50,512,-1,513,-1,515,-1,517,-1  $; Robin verify OBS
,200,202,207,22,252,254,26,201,202,207,252,54] 
pari[0]=100
parf[0:2]=['/work1/build/run/output/','robin1','robin1']
parf[5:7]=['/home/hkieffer/krc/tes/out/','robin233','robin233']
end

;145: kons=[514,515,-1,517,-1,518] ; " for Tplan REQ 132

146: parf[[0,1,5,6]]=['/work2/KRC/321/run/out/','VerTest' $ ; 321 vrs 341
,'/work1/krc/test/','v341aTest']

147: begin & kons=[200,202,207,252,203,207,50,51,512,514]  ; t52 vs tm1
parf[0]='/work1/krc/test/' ; data directory
parf[1]='v341aTest2' ; case:  [.t52]
parf[2]='v341aFlat2'; multi: [.tm3] 
end

148: begin & kons=[12,206,252,22,462,-4,465]  ; V34 deltas
parf[0:2]=['/home/hkieffer/krc/tes/out/','V34','a']
pari[[25,6,17]]=[0,2,21] ; tsur,equator,Ls~180
ucase=['Mars','heatflow=40','homog','Eslip','Wdune','WduneFFF','P=1.1,flat','NoAtmLamb','LomSee','Minn.8','Keihm'] ;V34b
ucase=['Mars','heatflow=40','homog','Wdune','Eslip','EslipFflat','EslipFdune','P=1.1,flat','NoAtmLamb','LomSee','Minn.8','Keihm'] ; V34a
idel=[1,0, 2,0, 5,4, 6,4, 7,8, 9,8, 10,8, 11,8]
parr[4:7]=[.42,.5,-.03,.03] ; locc for 462
parr[12:15]=[.45,.4,-.03,.03] ; locc for 465
end

149: begin & kons=[12,206,252,22,462,-4,466]  ; V34 inertia set
parf[0:2]=['/home/hkieffer/krc/tes/out/','V34','i']
ucase=string((25.*sqrt(2.)^indgen(14)),form='(f6.1)')
pari[[25,6,17]]=[0,2,21] ; tsur,equator,Ls~180
parr[4:7]=[.45,.50,-.03,.03] ; locc for 462
parr[20:23]=[.1,.50,-.03,.03] ; locc for 466
end

152: begin ; 354B set
parf[0]='~/krc/tes/out/'   ; 2017oct04   354 vrs 344
parf[1]='354B' ;
parf[5]='/work2/KRC/344/run/out/'
parf[6]='354B' ; 
ucase=['Base','Tdep','Tcon','Tiny','TinyTd','TinyTc','NoAtm','NoAtmTd','NoAtmTc']
end

;..................................................................

123: begin & lkon=1b & kkon=-1  ;- Start auto-script 
lastkon=n_elements(kons)-1 & end ; last preset command

11: GETPSN,'File names',parf,labs=labf,/align ; Modify parf: File names
12: begin & GETPAN,'pari',pari,0,0,labs=labi ; Modify pari: integers 
jlat=pari[6]<(nlat-1) & ihour=pari[16]<(nhour-1) & it1=(pari[25]>0)<4 
jsea=pari[17]<(nsea-1) & jc=pari[7]<(ncase-1) & end
125: GETPAN,'parj',parj,0,0,labs=labj ; Modify parj: integers
13:  GETPINTS,ptitl+' Actions: kons',kons,0,0,fmt='I6'; Modify actions kons
140: GETPINTS,labk,park,0,50 ; Modify park: comparison cases 
14: Begin ; Modify paru: tolerances
read,i,prompt='paru=tolerances: 0=initial, 1=allSame 2=std > '
if i le 0 then paru=paru0 else if i eq 1 then begin
    xa=0.  & read,xa,prompt=' Tolerance for all > ' & paru[*]=xa 
  endif else GETPAN,'paru: tolerances',paru,0.,0.,labs=labu 
end
15: GETPAN,'parp',parp,0.,0.,labs=labp ; Modify parp: positions
157: print,'parp',VEC2CODE(parp) ; Print current parp as code
16: GETPAN,'parr',parr,0.,0.,labs=labr ; Modify parr: floats
167: print,'parr',VEC2CODE(parr) ; Print current parr as code
17: GETPAN,'parg',parg,0.,0.,labs=labg ; Modify parq: floats

18: begin & help,ifile,ttt,ifh,tth,uuu,vvv,dfile ; Help, and print cases
if n_elements(caset) gt 1 then PRINTJCOLS,caset,1,len=65 & end

182: help,dfile,fcz,icz,lcz,lsz,ttou,vernz,ktyz,pari[22] $ ; Type mx help
,jdates,gg1,labd,desc,fout,djul,jdisk,mjd

188: if n_elements(itemv) lt 1 then print,'Need to do @252' else begin ; Contents
  help,ttt & print,'(hour,item,latitude,season,case)' & print,'itemt = ',itemt
  help,ddd & print,'(layer,item,latitude,season,case)' & print,'itemd = ',itemd
  help,ggg & print,'(item,latitude,season,case)' & print,'itemg = ',itemg
  help,uuu & print,'(nlat,item,case)' & print,'itemu = ',itemu
  help,vvv & print,'(season,item,case)' & print,'itemv = ',itemv
print,'KRCCOM is in kcom:' & help,kcom,/struct & end

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv in work vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;355: begin ; Version 355 test
;end

; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ in work ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

;  set names and read files
200: Begin & iver=0 & verr='A' & end ; Set to VerA  parf[0]
201: Begin & iver=1 & verr='B' & end ; Set to VerB  parf[5]

202: begin & igrp=1 & verg='1' & end ; Set to case group 1  parf[1 or 6]
203: begin & igrp=2 & verg='2' & end ; Set to case group 2  parf[2 or 7]

206: begin & path=parf[0]; file=parf[0+1+2] +.t52
  verr=parf[2] & stem=parf[1]+verr  & sfile='A12' & end 

207: begin & i=5*iver  ; Set input file stem
path=parf[i]           ; input file dir
stem=parf[i+igrp]         ; core of the KRC file name
sfile=verr+verg
; help,iver,i,verr,path,stem,sfile
end

211: GETPINTS,'KRCCOM Items',kist,101,320 ; Change KRCCOM List=kist
212: kiname=KRCLISTNAME(kist,/prt) ; Get KRCCOM names for kist
213: kivals=KRCLISTVAL(kcom,kist,/prt) ; Get KRCCOM values for kist

22: begin  ; Get KRC changes and values for kist
front=READKRCCOM(path+stem+'.t52',khold) ; get khold and front
if n_elements(front) lt 5 then goto,halt
print,'khold=',khold
vern=khold[4]                    ; 2-digit KRC version
ucase=KRCHANGE(khold,list=kist,kcom1=kcom,lout=lout,/log)
ucase[0]=parf[1]+' Base'
caset=stid[1:ncase]+'_'+ucase
q=READKRCCOM(-1,khold) ; close and free logical unit
i=where(kist eq 201) & i=i[0] ; location of N1
if i ge 0 then kn1=round(reform(lout[i,*]))-1 $     ; number of physical layers
          else message,'no N1 in list',/con ; not in list
i=where(kist eq 202) & i=i[0] ; location of N2
if i ge 0 then kn2=round(reform(lout[i,*])) $     ; number of time steps
          else message,'no N2 in list',/con ; not in list
i=where(kist eq 208) & i=i[0] ; location of IC2
if i ge 0 then ic2=round(reform(lout[i,*])) $     ; number of time steps
          else message,'no IC2 in list',/con ; not in list
i=where(kist eq 207) & i=i[0] ; location of IIB
if i ge 0 then begin 
  iib=round(reform(lout[i,*]))  ; lower boundary: 
  hflow=0.001*iib & ii=where(iib le 0,j) & if j gt 0 then hflow[ii]=0.
  mxhf=max(hflow)
endif else begin 
  message,'no IIB in list',/con ; not in list
  mxhf=-1.  ; fflag that it is not reliable
  endelse 
i=where(kist eq 133) & i=i[0] ; location of RLAY
if i ge 0 then rlayy=reform(lout[i,*]) $     ; RLAY
          else message,'no RLAY in list',/con ; not in list
i=where(kist eq 134) & i=i[0] ; location of FLAY
if i ge 0 then flayy=reform(lout[i,*]) $     ; FLAY
          else message,'no FLAY in list',/con ; not in list
end 

222:  begin & print,'ncase=',ncase; Interactive case names 
print,'caset:',caset
print,'ucase:',ucase
q='' & read,q, prompt='c or u or n=number ? > '
q=strlowcase(q)
if q eq 'n' then scase=replicate('',ncase)  $
else if q eq 'u' then begin ; use as many as are available
  i=n_elements(ucase)
  if i lt ncase then scase=[ucase,replicate('case',ncase-i)] $
  else if i ge ncase then scase=ucase[0:i-1] $
  else scase=ucase
endif else scase=strmid(caset,3) ; drop the nn_
GETPSN,'case names',scase ;
caset=sti2[1:ncase]+'_'+scase  ; prepend 1-based case number
end

225: begin & www=fltarr(kn1[0],4,ncase) ; read all layer tables ??
q=parf[4]+parf[1]+parf[2]+'.prt'
ilun=1 ; set to open file and leave open
for j=0,ncase-1 do begin & $
sss=READTXTCOL(q,sp='W',nskip=-1,ncol=10,mrow=kn1[j]+1,fill='0',cend='LAYER',ilun=ilun) & $
www[0:kn1[j]-1,*,j]=float(sss[1:*,[2,5,6,7]]) & endfor
free_lun,ilun ; close file and free LUN
blay=float(sss[*,2]) & ktt=float(sss[*,5]) ; layer thickness in m, conductivity
denn=float(sss[*,6]) & ctt=float(sss[*,7]) ; layer density, Specfic heat
help,blay,ktt,denn,ctt
end

228: begin & krcf=[-1.,kcom.fd]; For SIMPLE and  MarsCir runs REQ 22
rlay=rlayy > 1.0001    ; layer ratio
deep=flayy*(rlay^kn1-1.)/(rlay-1.)
dtim=daysec/reform(lout[4,*])         ; time-step in seconds
rcu=krcf[7]*krcf[8]                   ; SPHT*DENS of upper material
condu=krcf[3]^2/rcu                   ; Conductivity of upper material
persec=krcf[6]*daysec                 ; period in seconds
Diffu=condu/rcu                 ; 3.733E-08 ;<<< ffirmcode for MarsCir runs
Scale= sqrt(diffu*persec/!pi)   ;    3.204E-02 ;<<< firmcode 
sconvg=(scale*flayy)^2/(2.*dtim*diffu) ; safety factor
cat=strarr(ncase) & sdat=strarr(ncase)
k1fmt=kiform[0]+',' & for i=1,n_elements(kist)-1 do k1fmt=k1fmt+kiform[i]+','
for i=0,ncase-1 do cat[i]=string(i,lout[*,i],deep[i],form='(i2,'+k1fmt+'f8.1)')
j=n_elements(cid) & if j lt 2 then cid=indgen(j) ; replace default
k2fmt=kiform[cid[0]]+',' & for i=1,j-1 do k2fmt=k2fmt+kiform[cid[i]]+','
for i=0,ncase-1 do sdat[i]=string(i,lout[cid,i],deep[i],form='(i2,'+k2fmt+'f7.2)')
end 

23: begin & i=7 ; Print input portion of selected KRCCOM arrays REQ 252
read,i,prompt='+1=floats +2=integers +4=logicals > ' & i=(i>0)<7
KRCCOMLAB ,i,kcom.fd,kcom.id,kcom.ld, fclab,iclab,lclab ; , uuu,  idx=idx
end

233: jj=KRCINDIFF(kcom,kcomh, fclab,iclab,lclab); KRCINDIFF changes REQ 26 

252: begin & ifile=path+stem+'.t52' ; Open/Read/Close type 52 file
if strlen(get52) gt 2 then ifile=get52+'.t52'
kcom=READKRC52(ifile,ttt,uuu,vvv,itemt,itemu,itemv,ddd,ggg,itemd,itemg $
,fall=pari[26],sver=sver)
help,ttt,uuu,vvv,ddd,ggg,sver,kcom
siz=size(kcom)
if siz[siz[0]+1] ne 8 then goto,halt ; must get a structure
sizt=size(ttt) & nhour=sizt[1] & niti=sizt[2]
nlat=sizt[3] & nsea=sizt[4] & ncase=sizt[5]
siz=size(ddd) & nlay=siz[1] ; number of real layers 
print,'Nseas, nlat, ncase=',nsea,nlat,ncase
tlow=reform(ddd[0,0,*,*,*]) ; minimum T of surface layer [lat,seas,case]
nloc=n_elements(tlow)  ; nlat*nsea*ncase
alat=uuu[0:nlat-1,0,0]          ; latitudes for first case
slat=ST0(alat,/nojoin)          ; lats as string
q=min(abs(alat),jeq) & pari[6]=jeq & jlat=jeq ; index of latitude closest to equator
xxh=(findgen(nhour)+1)*(24./nhour) ; hours
q=min(abs(xxh-13.),ihot) & pari[16]=ihot & ihour= ihot; idx of hour closest to 13
sour=ST0(xxh,/nojoin)
caset='Case_'+stid[1:ncase] ; 1-based case numbers
djmm=vvv[*,0,0]                 ; DJUL
lsv =vvv[*,1,0]                 ; LSUBS           VALID ONLY FOR MARS
sls=string(lsv,format='(f5.1)')  ; as string
; dfl0=(djmm-151.269) mod 686.99161 ; days from Ls=0 for Mars
fsix=findgen(nsea*nhour)/float(nhour); fraction season index
jy1=LASTPERIOD (lsv,leny,jy2,verb=1) ; get season coverage 
nfy=nsea/leny                      ; number of full years
q=(kcom.id[11]-1)/float(leny) & ky0=round(q) & print,'spinup years:',q,ky0
ks0=nsea-nfy*leny ; index of starting season the yield integral remaining years
ks1=(nsea-leny)>0 & ks2=nsea-1 ; season index range for the last year
print,'Ls for ks1,ks2=',lsv[[ks1,ks2]]
n3=kcom.id[2]                        ; N3
jdisk=kcom.id[11]                    ; JDISK
kiner=kcom.fd[2] & kcp=kcom.fd[6] & kden=kcom.fd[7]
kcond=kiner^2/(kden*kcp) 
print,'I,rho,Cp,K=',kiner,kden,kcp,kcond
dav=total(ddd,2)/2.; Average of min and max [Layer,latitude,season,case]
n4=nlat & m4=indgen(n4) ; insurance
end
;help,lsv,jy1,leny,jy2,nfy,ks1,ks2

253: begin ; Read times and Generate generic Ls
;; geometry matrix is not in .t52, so must read the .inp, or go to source tables
fff=file_search('~/krc/tes/'+stem+'.inp',count=j)
if j ne 1 then fff=file_search('~/krc/tes/*.inp',count=j)
for i=0,j-1 do print,i,fff[i]
read,i,prompt=' inp index > '
i=(i<0)<(j-1) ; ensure valid
print,'.inp file= ',fff[i]
  qqq=READTXTCOL(fff[i],sp='0',nskip=0,ncol=1,fill='7.7')
j=n_elements(qqq)
i1=-1
i=-1
repeat begin & i=i+1 & i1=strpos(qqq[i],'=RUNTIME') & endrep until i1 gt 0 or i ge j
if i1 lt 1 then message,'geom matrix not found'
print,qqq[i]
porb=dblarr(30)
for j=0,5 do porb[5*j:5*j+4]=double(strsplit(qqq[i+1+j],/extract))
;  SMA  ECC INC_deg LNODE_deg ARGP_deg tjp cmass
ele7=porb[[7,6,4,3,5,15,1]-1]; SMA ECC INC_deg LNODE_deg ARGP_deg TJP cmass
print,'Body index is ',fix(porb[0])
ele7[6]=1.                      ; central mass in M_Sun
raDec=porb[[11,10]-1] ; RA andDec in radians in J2000 equ.
ppp=LSUBSGEN(0,ele7,!radeg*radec) ; RaDec in degrees
gls=LSUBSGEN(1,ppp,vvv[*,0,0]); arg 3 is DJU5
plot,lsv,xtit='disk season index',ytit='Ls' & oplot,gls[*,0],psym=1
print,'WRONG ? : offset for mars'
end

254: begin & ucase=stid[1:ncase] ; specific case names
if stem eq 'InSiN1' then ucase=['Deep','Base','2Material','Tau=.2','KofT','Climate']
if stem eq 'V224str1' then ucase= ' Pt='+['546','200','100','50','10','5','2','none']
if stem eq 'V224str2' then ucase= ' Pt='+['10000','5000','2000','1000','500','200','100','50','20','10','5','2','none']
if stem eq 'V224str5' then ucase= ' Pt='+['10000','5000','2000','1000','500','200','100','50','20','10','5','2', '1.01','none']
if strpos(stem,'test1') ge 0 then ucase=['AtmTconFcon','AtmTdepFcon','AtmTconFvar','noAtmTcon','noAtmTvar','noAtmTuni']
if stem eq 'try2' or stem eq 'try8' or stem eq 'try5' then ucase=['29','18','19','20','21','22','23','25','27']
if strpos(stem,'thin7') ge 0 then ucase=['1536','384','768','3072','6144','12288','24576'] ; thin7
if strpos(stem,'thin8') ge 0 then ucase=['1536deep','1536thin','1536','384','768','3072','6144','1536atm'] ; thin7
if strpos(stem,'InSiN') ge 0 then ucase=['Deep','22Layers','DualMater','Tau=.2','KofT','Climate']
if strpos(stem,'test342f') ge 0 then ucase=['tinyFlat','NoFlat','No10self','No10tiny','No10no']
if strpos(stem,'PhoA') ge 0 then ucase=['Mars','solar5','Solar7','Phobos','lunar','rare0','rare.95']
if strpos(stem,'Test') ge 0 then ucase=['MarsBase','LKofT','varFrost','noAtm','Kheim','LkofT','Tdep=0','MarsAtmSLope','AtmSLope+fff']
if strpos(stem,'t361') ge 0 then ucase=['Base:o8','slope0.1:o4','slope0.1+fff','slope30+fff','noAtm:o8','noAtmSlope0.1:o4','noAtmSlope0.1+fff8','noAtmSlope30+fff8','noAtmSlope30+fff4']
i=n_elements(ucase)
caset=sti2[1:i]+'_'+ucase ; may be short
end
 
;V34a
;ucase=['Mars','heatflow=40','P=1.1,noHeat','naAtmLamb','LomSee','Minn.8','Keihm:Alb.12','Lamb:A=.12','MarsEdune','Wdune','WduneFFF'];
;,'Keihm.25'

; V34b ucase=['Mars','heatflow=40','homog','MarsEdune','Wdune','WduneFFF','P=1.1,flat','NoAtmLamb','LomSee','Minn.8','Keihm'];

255: begin & what=['KRC','DAY','LAT','FIL'] ; test DEFINEKRC
if pari[0] ge 300 then i=8 else i=0        ; which precision
  for k=0,2 do begin 
   qq=DEFINEKRC(what[k],param,labkf,labki,labkl,idmin,idmax,nword=nword,pid=pid)
    help,qq,/struc & nword,4*nword
    PAUSE,-1
  endfor 
end

256: begin & ; DEFINEKRC for current precision REQ 22 
krcstu=DEFINEKRC('KRC',param,labkf,labki,labkl,idmin,idmax,vern=vern) 
fclab=STRWORD1(labkf) ; first word = parameter name   
iclab=STRWORD1(labki)
lclab=STRWORD1(labkl)
end

257: begin  ; SHOWBYTES for start of parf[0+11]
ofile=parf[0]+parf[11]
openr,lun,ofile,/get_lun
aaa=bytarr(300)
readu,lun,aaa
SHOWBYTES,aaa
stop
point_lun,lun,0
end

26: begin & siz=size(ttt) ; hold current set. tth=ttt etc. 
if siz[0] ne 5 then goto,halt
tth=ttt & uuh=uuu & kcomh=kcom & caseh=caset & sizh=sizt
ifh=ifile & vvh=vvv & lsh=lsv & ddh=ddd & ggh=ggg & verh=sver & tloh=tlow 
dah=dav & lenh=leny & vrh=vern & steh=stem 
tth=reform(tth,siz[1],siz[2],siz[3],siz[4],siz[5],/over)  & end

261: begin & nyr=6 ; extract 23 layer 6 year from multi-N1 10 year 
i=leny*nyr ; last to keep
nsea=i+1 & j=2 ; j is case for n1=23
ncase=1
ttt=reform(ttt[*,*,*,0:i,j],nhour,niti,nlat,nsea,ncase,/over)
ddd=ddd[*,*,*,0:i,j]
ggg=ggg[*,*,0:i,j]
uuu=uuu[*,*,j]
;vvv=vvv[0:i,*,j]
lsvt=lsv[0:i] & djmm=djmm[0:i]
q=caset[j]
tsur0=reform(ttt[*,0,0,*,0])          ; Tsurf [hour, season]
jy1=LASTPERIOD (lsvt,leny,jy2,verb=1) ; get season coverage
print,'Nseas, nlat, ncase=',nsea,nlat,ncase
print,' WARNING: ttt,ddd,ggg,uuu pruned' & q=get_kbrd(1)
end

;262: begin & if n_elements(jj) gt ncase-1 then goto,halt
;ttt=ttt[*,*,*,*,jj] & ddd=ddd[*,*,*,*,jj] & ggg=ggg[*,*,*,jj] 
;uuu=uuu[*,*,jj] & vvv=vvv[*,*,jj] & caset=caset[jj] & end

266: help,ifh,ifile,lsh,lsv $ ; Help latest and hold
,tth,ttt,uuh,uuu,vvh,vvv,ddh,ddd,ggh,ggg,sver

27: zzz=KRCLAYER(kcom.fd,kcom.id,kcom.ld,flab=zlab) ; Print layer table
; 0]= KRC 1    ; print,reverse(ly[klay]),form='(9f6.2)'  After 641
; CHART,zzz[0:8,*],parti=['Thick,local','thick,m','Center,D_up','Center,m' $
; ,'MassAboveCenter','MassAboveBottom','DepthToBot,local']

275: begin ; CLOT T/z  REQ 252 27
k=0 & read,k,prompt='Case index? > '
k=(k>0)<(ncase-1)
yy=reform(ddd[*,*,*,nsea-1,k])
ya=min(yy,max=yb)
zm=kayers[*,3] & zm[0]=.2*zm[1] ; fake surface depth
CLOT,reform(yy[*,0,*]),slat,xx=zm,/xlog,yran=[ya,yb],locc=[.7,.93,-.03,.04] $
,titl=['Depth, m','T range',kite+' '+caset[k]]
CLOT,reform(yy[*,1,*]),oplot=200
end


; grip.one shows no extra bytes, LF at end of each line
28: begin & openw,luf,parf[12]+parf[13],/get_lun ; Open report file
printf,luf,ptitl+' Report '+isotime(1) ; yyyymondd hh:mm:ss as one word
printf,luf,'Last read= ',path+stem
printf,luf,'Held file= ',ifh
end 

29: begin & free_lun,luf ; Close report file 
luf=0
print,'Report file is  ',parf[12]+parf[13]
end

; 4_4_4_4_4_4_4_4_4_4_4_4_4_4_4_4_ test between cases
; Ver 222 cases
; 0: With atmosphere, properties constant with T
; 1: With atmosphere, properties T-dependent
; 2: With atmosphere, soil properties constant with T, frost properties variable
; 3: No atmosphere, properties constant with T
; 4: No atmosphere, properties T-dependent
; 5: No atmosphere, properties T-dependent, but constant

40: begin ; replicate tday convergence
fac5= 5.67051e-08
fac45=4.*fac5
fac7=parp[0]
ABRAD = parp[1]           ; surface absorbed radiation
; t2=parp[2] ; top layer last time
tstart=parp[3] ; prior tsur
ggt = parp[2]
;ttj2=  215. ;  1342942875.71509
print,' k       sheatf        power         delt        tsur         adeln' 
;       0 -1.21452e+10 -8.29296e+28 -2.74924e+08  8.24771e+08     0.333333
t1=140.
t3=10.
nt=18. & nk=7
zz=fltarr(nk,nt,2) ; T and delt
yy=fltarr(nk,nt) ; ideal factor
tu=t1+findgen(nt)*t3 ; top layer T
for i=0,nt-1  do begin
   ttj2=tu[i]
   tsur=tstart      ; prior
   print,tsur-ttj2
   for k=0,nk-1 do begin 
      ts3=tsur^3                         ; bare ground
      sheatf= fac7*(ttj2-tsur)           ; gradient heat flow
      power = abrad +sheatf - fac5*tsur*ts3 ; unbalanced flux
      delt = power / (fac7+fac45*ts3)       ; Newton estimate
      zz[k,i,*]=[tsur,delt]
      tsur=tsur+delt
      adel=abs(delt)
;            if (adel.lt.ggt) goto 242 ; satisfies convergence test
      adeln=adel/tsur
      print,k,sheatf,power,delt,tsur,adel
;            if (adeln.gt.0.8) goto 340 ; probably blowing up numerically
;      if adeln gt 0.1 then  tsur=tsur-0.7*delt ;reduce increment,help stability
   endfor 
yy[*,i]=(tsur-zz[*,i,0])/zz[*,i,1] ; ideal factor to multiply DELT
endfor
CLOT,yy,ST0(tu,/nojoin),locc=[.7,.3,.03,.06],titl=['iteration','Ideal factor',''] 
PAUSE,-1
aa=zz[*,*,1]/zz[*,*,0] ; Delta/T
CLOT,abs(aa)> parp[4],ST0(tu,/nojoin),locc=[.7,.3,.03,.06],/ylog $
,titl=['iteration','Abs(delta T)/T',kite]
end

401: begin ; new solution REQ 40
doa=[87.2,123.4,220]
yy=fltarr(3,nt)
for k=0,2 do yy[k,*]= (tu/doa[k])^4+tu-tstart
CLOT,yy,ST0(tu,/nojoin)
end

402: begin ; CLOT DownVis then Tsur for all cases
yy=reform(ttt[*,*,jlat,jsea,*]) ; [hour,item,case]
titl=['hour','DownVis',stem+': lat='+slat[jlat]+' sLs='+sls[jsea]]
CLOT,reform(yy[*,3,*]),caset,locc=[.1,.9,-.025,.03],xx=xxh,titl=titl
  if CALL91(ptitl,'kv402') then stop 
titl[1]='Surface T'
CLOT,reform(yy[*,0,*]),caset,locc=[.1,.9,-.025,.03],xx=xxh,titl=titl
end

41: begin                       ; Test Ls across multiple cases
yy=reform(vvv[*,1,*]) ; Ls [season,case]
siz=size(yy) & if siz[0] ne 2 then goto,halt ; only one case
ya=MEAN_STD2(yy,std=yb) ; statistics across cases
xa=min(yb,max=xb) ; range of changes between cases
print,'Range of change of Ls between cases:',xa,xb
plot,yy[*,0],xtit='Season index [0-based]',ytit='Ls for case 0', title=kite+ifile
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

43: begin & qt=itemt[it1]           ; Plot hourly Tx near equator for 2 seasons
i1=jy1
t1=reform(ttt[*,it1,jeq,i1:*,*]) ;[hour,season,case]  at equator
q=min(abs(lsv[i1:*]-251.),i1) ; find index nearest perihelion, which is at Ls=251
q=min(abs(lsv[i1:*]-71.),i2)  ;    "    "     "      aphelion, which is at Ls=71
ii=[i1,i2] 
t1=t1[*,ii,*] ; only two seasons
t2=transpose(t1,[0,2,1]) ; [hour,case,season]
t3=reform(t2,nhour,2*ncase) ; [hour, case*season]
q=[caset+(' Seas '+strtrim(ii[0],2)),caset+(' Seas '+strtrim(ii[1],2))]
CLOT,t3,q,titl=['Hour index',qt+' near equator. Last Year' $
,kite+stem],locc=[.45,.5,-.03,.08]
qs=sls[ii]
Print,'Seasons and Ls=',ii, lsv[ii]
end

433: begin & i=0 & j=0; Plot hourly one lat,season  REQ 43
read,i,j,prompt='Season and lat index > '  
t2=reform(ttt[*,it1,j,i,*]) ; [hour,case]
CLOT,t2,caset,titl=['Hour index.   Season='+strtrim(i,2)+'  Ls='+sls[i] $
,qt+' at lat='+slat[j],kite+ifile],locc=[.45,.5,-.03,.08]
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
t1=reform(ttt[nhour/2,it1,*,*,*]) ;[lat,season,case] Tsur at noon 
t2=transpose(t1,[1,2,0])  ;[season,case,lat] 
t3=reform(t2,nsea*ncase,nlat)
CLOT,t3,slat,loc=[.38,.45,-.03,.08],titl=['season*case',qt+' at noon',kite+ifile]
PLOTSECT,stid[1:ncase],.16,cs=1.5
PLOTSECT,scase,.21,cs=1.5, orient=90.,align=0.
end

445: begin & read,j,prompt='Lat index > '   ; CLOT one latitude REQ 43 then 44
j=(j>0)<(nlat-1)
CLOT,t2[*,*,j],caset,loc=1,titl=['Season Index',qt+' near Noon',kite+'Lat. index '+strtrim(j,2)]
print,'Lat= ',slat[j], alat[j] 
i=nsea/2 & print,'Values at season: index and Ls=', i,lsv[i]
for k=0,ncase-1 do print,k,t2[i,k,j],'  ',caset[k]  & end

45: begin & jc=pari[7]<(ncase-1) ; Difference two cases
 k2=pari[20]<(ncase-1) & if jc eq k2 then k2=(jc-1)>0
cdtit=stid[jc]+'-'+stid[k2]
dtt=ttt[*,*,*,*,jc]-ttt[*,*,*,*,k2] ; Prop=constant - Prop=uniform
j=n_elements(dtt)                   ; dtt is [hour,item,lat,season]
print,'@45  ',cdtit
print,'Item in ttt Mean     Std    mean_ABS_std'
;         Tsurf   0.259   1.236   0.977   0.801
if luf ne 0 then printf,luf,'@45  ',cdtit
if luf ne 0 then printf,luf,'Item in ttt Mean     Std    mean_ABS_std'
for i=0,4 do begin              ; each item in ttt
   xx=(dtt[*,i,*,*])    ; all hours, lats and seasons
   xa=MEAN_STD(xx,std=xb)    
   ya=MEAN_STD(abs(xx),std=yb)
   print,itemt[i],xa,xb,ya,yb,form='(a9,4f8.3)'
   if luf ne 0 then printf,luf,itemt[i],xa,xb,ya,yb,form='(a9,4f8.3)'
   if ya gt 1.E-6 then begin 
      HISTFAST,xx,xlab='Delta '+itemt[i]+'  '+cdtit
      PAUSE,-1
   endif
endfor
end

452: begin  ; check zone sums ?
period=kcom.fd[5] & rlay=kcom.fd[32]  & flay=kcom.fd[33] ; items 6,33 and 34
n1=kcom.id[0] & kn1=kcom.id[1] 
ydz=.0219 & yden=1600. & ycond=.03864 & ysph=647.   ; values from  zone file
sum0=fltarr(n1+1) & qa=1. & sum0[0]=QA ; start with 1
for j=1,n1 do begin & qa=qa*rlay & sum0[j]=sum0[j-1]+qa & endfor
n0=alog(1.+sum0*(rlay-1.))/alog(rlay) -1. ; start with 1
print,'0 sums residue',max(abs(n0-findgen(n1+1)))
sum1=fltarr(n1+1) & qa=rlay & sum1[0]=qa ; start with 1
for j=1,n1 do begin & qa=qa*rlay & sum1[j]=sum1[j-1]+qa & endfor
ns1=alog( (sum1*(rlay-1.))/rlay +1)/alog(rlay) ; start with rlay  
print,'1 Sums residue',max(abs(ns1-(1+findgen(n1+1))))
; above confirms the algebra is correct..

  persec = period * 86400.      ; get solar period in seconds
  dtim=persec/kn2                ; size of one time step
  qa=flay/rlay
  dify=ycond/(yden*ysph)               ; local diffusivity
  csfac=ydz/sqrt(2.*dtim*dify)         ; convergence safety factor
  dscal=sqrt(dify*persec/!dpi)         ; local diurnal scale
  ddz=ydz/dscal                        ; zone thickness in d units
  fac3=ddz/(qa*rlay)                   ; zone thickness / first-layer
  q1=alog((1.+(rlay-1.)*fac3)/rlay)/alog(rlay)
  qq=alog(1.+(rlay-1.)*fac3)/alog(rlay)
  ii=round(qq)>1                ; truncate to integer number of layers
  help,dify,dscal,ddz,qq,ii,sum0[ii-1]
end

46: begin  ; Plot Tsur, DownVis difference of two cases, AFTER 45 
nsp=pari[1] & nhp=pari[2]       ; Number of seasons and hours to plot
; For NoAtm cases, Tplan, Tatm, DownIR, FROST4 ,AFRO4 are meaningless
jj=[0,3]                        ; Tsur and DownVis
t2=dtt[*,jj,*,*,*] ; two items valid with/without atm ; [ hours, 2 items,lat,season]
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
,titl=kite+cdtit+' '+ifile
PLOTSECT,slat,.51, cs=2.
end

462: begin & fff=reform(ttt[*,it1,jlat,jsea,*]) ; fff= ttt[Hour,case] REQ 252
if n_elements(caset) ne ncase then goto,halt
topt=' Lat='+slat[jlat]+' Ls='+sls[jsea]
titl=['hour',itemt[it1],ifile+topt] 
CLOT,fff,caset,locc=parr[4:7],xx=xxh,titl=titl,tsiz=2.
end

463: begin  & ii=[0,1,2,3,4] ; First 5 sases REQ 462
CLOT,fff[*,ii],caset[ii],locc=1,xx=xxh,titl=titl
end

464: begin  &  ii=[5,6,7,8,9]; 2nd 5 sases REQ 462
CLOT,fff[*,ii],caset[ii],locc=1,xx=xxh,titl=titl
yy=AVALG(fff[*,ii],fff[*,6],'-')
CLOT,yy,stid[ii]+'-'+stid[6],locc=1,xx=xxh,titl=titl
end

465: begin & j=n_elements(idel) & k=j/2; V34b deltas REQ 462 
ii=reform(idel,2,k)
yy=fltarr(nhour,k) & qq=strarr(k)
for i=0,k-1 do yy[*,i]=fff[*,ii[0,i]]-fff[*,ii[1,i]]
for i=0,k-1 do qq[i]=stid[ii[0,i]]+' - '+stid[ii[1,i]]
CLOT,yy,qq,locc=parr[12:15],xx=xxh,titl=titl,tsiz=2.
end

466: begin   ; dT/dI  REQ 462 
ffd=shift(fff,[0,-1])-fff  
fdd=fltarr(nhour+1,ncase-1) ; will drop last case as it is wrap
fdd[0,*]=ffd[nhour-1,0:ncase-2]  ; wrap midnight early
fdd[1:*,*]=ffd[*,0:ncase-2]
CLOT,fdd,stid[0:ncase-2]+' '+stid[0:ncase-2],xx=[0.,xxh] $ 
,locc=parr[20:23],titl=titl,tsiz=2.,xran=[0.,24]
plots,[0,24],[0.,0.], line=1
xyouts,17.,-5.,'Delta T for sqrt(2) * I',/data,charsize=2
;  if CALL91(ptitl,'kv466') then stop 
end

467: begin & siz=size(ddd) ;  dff=[tmin/max,layer,case]  REQ 252
dff=reform(ddd[*,*,jlat,jsea,*]) ; [tmin/max,layer,case]
dcd=zzz[1:*,3]                   ; depth to center of layer
i2=7 -2; input spec for material change, -2 for drop virtual and 0-based
xa=zzz[i2,3]+zzz[i2,1]/2. ; bottom of above material
yy=dff[*,*,0] & ya=min(yy,max=yb)
plot,dcd,yy[*,0],yran=[ya,yb],/xlog,xtit='Depth in m',ytit='Diurnal extreme temperatures',titl=ifile+topt, psym=-4,charsize=2.
oplot,dcd,yy[*,1], psym=-1 ; Tmax
plots,[xa,xa],[ya,yb],line=2 ; material boundary
; CLOT,dff[*,*,0],['Tmin','Tmax'],xx=dcd,/xlog,locc=1 $
; ,titl=['Depth to layer center, m','Temperature',kite]
  if CALL91(ptitl,'kv490') then stop 
plot,dcd,dff[*,0,1]-dff[*,0,0],xtit='Depth in m',ytit='Change in minimum T with heatflow',psym=-4,charsize=2.
yy=dff[*,0,1]-dff[*,0,0] ; change in Tmin [layer]
j=siz[1]-1 ; last valid layer
zb=(yy[j]-yy[9])/(dcd[j]-dcd[9]) ; change in thermal gradient  K/m
iib=40 ; from @22 print, in kcom.id[6] for the proper case.
hf=0.001*iib ; heatflow in W/m^2
q=kcom.fd[3]; lower material conductivity
za=hf/q
print,'Thermal gradient: expected and found ',za,zb, 1.-zb/za
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

474: FROST4,ttt,ggg,slat,caset  ; Only for fort.73 run; e.g. 355g2

475: FROST5,ttt,ggg,slat,caset,lsv, tth,ggh; FORST4

480: begin & rlay=1.2 ; Calc FLAY set
flay=[.18,0.0533/2^indgen(5)]
n2=1536L*[1,4L^indgen(5)]
print,'FLAY:',flay & print,'N2  :',n2 
nlay=22+alog(flay[0]/flay)/alog(rlay)
print,'N1  :',nlay & end

48: begin & locc=[.37,.53,-.025,.05]; examine cases at last season
qq=reform(ttt[*,0,jeq,nsea-1,*]) & nq=ncase             & q=stid[1:nq]+' '+caset
CLOT,qq,q,locc=locc,xx=xxh,titl=['Hour','Tsurf equator',kite+ifile]
end

482: begin ; Last-first season (one year)
print,'Ls pair',lsv[[0,nsea-1]]
tdd=reform(ttt[*,0,jeq,nsea-1,*]-ttt[*,0,jeq,0,*])
CLOT,tdd,stid[1:nq]+' '+caset,locc=locc,xx=xxh $
,titl=['Hour','Tsurf equator: Change in last year',kite+ifile]
end

483:  begin & qq=reform(tth[*,0,jeq,nsea-1,*]) ; CLOT tth  REQ 26
 nq=n_elements(caseh) & q=stid[1:nq]+' '+caseh
CLOT,qq,q,locc=locc,xx=xxh,titl=['Hour','Tsurf equator',kite+ifile] & end

484: begin  & nadd=parj[0] ; Print cases available
fmtc='(i3,2x,a)'
nch=n_elements(caseh)
print,'100+ is tth'
for i=0,nch-1 do print,i+1,caseh[i],form=fmtc
print,'200+ is tth'
for i=0,ncase-1 do print,i+1,caset[i],form=fmtc
end

485: begin & k=nsea-1  ; Edit and make combines of 2 runs
; 30x must follow indices parj[1:2]
GETPINTS,' Combines in tt+ 100=h 200=t 300=a',kkk,0,0
j1=nch -1         ; max case index for tth
j2=ncase-1
ncc=n_elements(kkk)
casec=strarr(ncc)
siz=size(ttt) & siz[2]=ncc  ; construct combines
ttc=make_array(size=[2,siz[1],ncc,siz[siz[0]+1],99]) ; to hold all combines
for i=0,ncc-1 do begin
   k=kkk[i] & kh=k/100 & ki=k-kh*100-1
   case kh of
      1: begin & if ki gt j1 then begin; from ttt
            print,'Index too big, reset to ',j1
            ki=j1
         endif
         casec[i]= caseh[ki] 
         ttc[*,i]=tth[*,0,0,nsea-1,ki] & end
      2: begin & if ki gt j2 then begin
            print,'Index too big, reset to ',j2
            ki=j2
         endif
         casec[i]= caset[ki]    ; from tth
         ttc[*,i]=ttt[*,0,0,nsea-1,ki] & end
      3: begin                  ; algebra. Currently only one valid.
         i1=parj[1] & i2=parj[2] & frac=0.001*parj[3] ; linear combo
         if i1 ge i or i2 ge i then message,'Forward index'
         casec[i]=casec[i1]+'+'+string(frac,form='(f6.3)')+'*'+casec[i2]
         ttc[*,i]=(1.-frac)*ttc[*,i1]+frac*ttc[*,i2] ; add 1
      end
      else: print,'invalid combo index'
; help,i,k,kh,ki,ttc,j1,j2,i1,i2
   endcase 
   print,i,' ', casec[i]
endfor & print,'kkk',VEC2CODE(kkk) & end

486: CLOT,ttc,stid[0:ncc-1]+' '+casec $ ; Plot combine REQ 485
,locc=[.35,.45,-.025,.05],xx=xxh ,titl=['Hour','Tsurf equator',kite+ifile]

487: begin ; Edit deltas
GETPINTS,' Deltas in ttc, first is base',kkd,0,ncc-1
ncd=n_elements(kkd)
cased=strarr(ncd)
for i=0,ncd-1 do begin 
cased[i]= casec[kkd[i]] & print,i,' ', cased[i]
endfor & print,'kkd',VEC2CODE(kkd) &  end

488: begin & siz=size(ttc) & siz[2]=ncd-1  ; Construct deltas REQ 485
dref=cased[0]
yy=ttc[*,kkd[0]] ; reference
tdd=make_array(size=siz) ; to hold all deltas
for i=0,ncd-2 do tdd[*,i]=ttc[*,kkd[i+1]]-yy
end

489: begin & CLOT,tdd,stid[1:ncd-1]+' '+cased[1:*],locc=locc $ ; Plot deltas
,xx=xxh,titl=['Hour','Tsurf equator: Delta from Ref.',kite+'Ref: '+dref]
plots,[0.,24.],[0.,0.],line=1 & end

; 5_5_5_5_5_5_5_5_5_5_5_5_5_5_5_5_ test between file types
; Read type -1 and 0 for the same model

50: begin & dfile=path+stem+parf[17] ; Read type 0 or -n
vernz=pari[0]/10       ; first 2 of 3-digit version that wrote the file
ktyz=pari[23] ; file type
fout=READKRC1(dfile,fcz,icz,lcz, lsz,ttou, vern=vernz,ktype=ktyz $
   ,verb=pari[22],  dates=jdates, ddd=gg1,lab=labd,desc=desc)
siz=size(fout) & if siz[0] ne 2 then goto,halt
slat0=string(fout[*,0],form='(f6.1)')
siz=size(ttou) & nzh=siz[1] & nzl=siz[2] & mint=siz[3] & nzs=siz[4] ; temperatures
nread=n_elements(lsz)
djul=fcz[41-1] & deljul=fcz[42-1] ; ASSUMED to be MJD. False before Version 2
jdisk=icz[12-1]
mjd=djul+deljul*(findgen(nread)+(jdisk-1)) ; days from J2000
end

51: begin & help,lsv,lsz  ; Check Ls between types REQ 252, 50
; lsv,djmm from t52, lsz,mjd from tmn --> lsam,
vernz=pari[0]/10       ; first 2 of 3-digit version that wrote the file
if vernz ge 21 then qq=mjd else qq=mjd-k24 ; adjust tmx JulDay of older versions
print,'MJD tmx-t52: Ave and StDev',MEAN_STD(qq-djmm,/both),form='(a,2f10.5)'
lsam=LSAM(qq,myn,aud) ; Allison and McEwen version of Ls for type 52
plot,PM180(lsv-lsz),xtit='index  + is t52-A&M ',ytit=' Ls:  52-tmx'
oplot,PM180(lsv-lsam), psym=1
print,'Ls  AM-t52: Ave and StDev',MEAN_STD(PM180(lsam-lsv),/both),form='(a,2f8.3)'
print,'Ls tmn-t52: Ave and StDev',MEAN_STD(PM180(lsz-lsv),/both),form='(a,2f8.3)'
end

512: begin   ; compare t52 and tmx  REQ 252, 50
help,ttt,ttou ; assumes tm1 was for first case.
k=0 & read,k,prompt=' 0-based case of .tmx file > '
k=(k>0)<(ncase-1) ; insurance 
for i=0,mint-1 do begin & 
  qq=ttt[*,i,*,*,k]-ttou[*,*,i,*]
  za=MEAN_STD(qq,std=zb)        ; tmx-t52 DJUL 
  print,'t52-tmz,all hour,lat,season: ',itemt[i],za,zb,form='(2a,2g13.5)'
  if abs(za)>zb gt 1.e-6 then begin 
    HISTFAST,qq & PAUSE,-1
  endif
endfor
end

514: begin & siz=size(gg1) ; Plot delta of each ddd item  REQ 50
if siz[0] ne 3 then goto,halt
SETCOLOR,init=862 ; 17 colors
for j=0,siz[3]-1 do begin ; each item
   CLOT,transpose(gg1[*,*,j]),slat0,loc=[.35,.20,.025,.08] $
        ,titl=['season index.  ',desc[j], kite+stem+'.t0 '+labd[j]]
   PAUSE,-1
endfor & end

516: begin & j=(pari[3]>0)<5 ; Plot one gg1 item  REQ 50
   CLOT,transpose(gg1[*,*,j]),slat0,loc=[.35,.20,.025,.08] $
        ,titl=['season index.  ',desc[j], stem+'.t0 '+labd[j]]
 end

532: begin & cp=600. ; Find constants in lunar-like depth profiles
nzon=round(parg[0]) & rlay=parg[1] & flay=parg[2] & dtot=parg[3]
persec=parg[4]*86400. & nlay=round(parg[5])-1 & n2=parg[6]
; v=vf(z+c3)/(z+c4)   basic relation  Grott10 Eq.2  and 3
z2=parg[7] & f2=parg[8]; given at 10 meters, f2==v(10)/v_inf=.95
v0=parg[9] & vf=parg[10:11]; conductivity known values at z=0 and z=infinity:
i2=fix(parg[14]) ; index of cond to use
c2=z2*(1.-f2)/(f2-v0/vf) ; result of algebra: conductivity
c1=c2*v0/vf
print,'c2=',c2 & print,'c1=',c1
r0=parg[12] & rf=parg[13]  ; density at surface and infinity 3 lines below Eq (3)
c4=z2*(1.-f2)/(f2-r0/rf) ; result of algebra: density
c3=c4*r0/rf
print,'c4=',c4 & print,'c3=',c3
glay=rlay^indgen(nzon)                    ; geometric series starting at 1.0
delg=glay/2.+shift(glay,-1)/2.            ; advance between centers
zz=glay[0]/2.+[0.,CUMSUM(delg[0:nzon-2])] ; cumulative normalized depth
flam=z2/zz[nzon-1]                        ; first layer in m
help,flam
blay=flam*glay                  ; layer thickness in m
zz=flam*zz                      ; layer centers, in m 
cond=fltarr(nzon,2)
for i=0,1 do cond[*,i]= vf[i]*(zz+c1[i])/(zz+c2[i])
plot,cond[*,0],zz,xran=[0.,0.1] & oplot,cond[*,1],zz
PAUSE,-1
denn=rf*(zz+c3)/(zz+c4)
plot,denn,zz
dss=sqrt((cond[*,i2]/(denn*cp))*(persec/!pi)) ; scale at each layer
dbot=CUMSUM(blay/dss)
zlab=' Thick,m    denn    conduc.  SpecHt zone   cDep,m  bDep,D   scale'  
;    '  0.1476 1039.69    0.02003  600.00    2   0.0738   4.90   0.0301
print,zlab & fmt='(f8.4,f8.2,g11.4,f8.2,i5,f9.3,f7.2,f9.4)'
for i=0,nzon-1 do print,blay[i],denn[i],cond[i,i2],cp, i+1,zz[i] $
,dbot[i],dss[i],form=fmt
end

533: begin & ; below, start building KRC zones REQ 532
nlay=round(parg[5])-1 & rlay=parg[1] & flay=parg[2] & dtot=parg[3]
diffi=v0/(r0*cp) ; surface diffusivity
dscale=sqrt(diffi*persec/!pi) ; scale for each conductivity
bmin= sqrt(2.*diffi*persec/n2) ; minimum stable layer thickness, m
fmin=bmin/dscale               ; min first layer in D units
help,diffi,dscale,bmin,fmin
flaz=flay>fmin ; thick enough to converge
gbot=rlay^nlay/(rlay-1.) ; normalized total depth
dbot=flay*gbot ; total depth in D units
help,gbot,flay,dbot,dtot
; bsaf=sqrt(2.*diffi[i2]*persec/n2)
; rcp= 1.e6 ; typical volumne specific heat
yy=NUMGEOMLAY(dtot,jint=nlay,flay=[flaz,.12,.1,.08,.05])
print,'resetting @16: 15 and 16',yy[0],flaz
parg[15]=yy[0]
parg[16]=flaz
end

534: begin & ds=dscale ; Revised table REQ 533
k0=vf[i2] & cc1=c1[i2] & cc2=c2[i2] ; conductivity coefficients
nlay=round(parg[5])-1 & flaz=parg[16] & rlaz=parg[15]
deld=flaz/rlaz                      ; thickness of virtual layer in D
zd=-deld/2.                      ; center depth of virtual  layer, D
delm=abs(zd)*ds                      ; 1/2 of this (virtual) layer in m
zpri=-delm                      ; center depth of virtual layer, m
dbot=0.                      ; bottom of virtual layer in D
; help,ds,deld,zd,delm,zpri,dbot
print,zlab
for i=0,nlay-1 do begin
  deld=rlaz*deld                ; layer thickness in D
  z=zpri+delm*(1.+rlaz)   ; estimate for center of this layer, m
  con=k0*(z+cc1)/(z+cc2)    ; estimates to compute scale
  den=rf*(z+c3)/(z+c4)    ; ""
  difi=con/(den*cp )            ; local diffusivity
  ds=sqrt(difi*persec/!pi)      ; local scale 
  delz=0.5*deld*ds                  ; 1/2 of this layer in m
  z=zpri+delm+delz              ; center of this layer
  con=k0*(z+cc1)/(z+cc2)          ; conductivity here
  den=rf*(z+c3)/(z+c4)          ; density here
  difi=con/(den*cp )            ; local diffusivity
  ds=sqrt(difi*persec/!pi)      ; local scale 
  delz=0.5*deld*ds                  ; 1/2 of this layer in m, revised
  z=zpri+delm+delz              ; center of this layer in m , revised
  dbot=dbot+deld                ; bottom of this layer in D
  print,delm+delz,den,con,cp, i+1,z,dbot,ds,form=fmt
; help,deld,delm,delz
  zpri=z
  delm=delz                     ; 1/2 of this layer in m, save for next loop
endfor
if dbot gt dtot then begin 
flaz=flaz*(dbot/dtot)  & parg[16]=flaz
endif else begin
rlaz=NUMGEOMLAY(dtot,jint=nlay,flay=flaz) & parg[15]=rlaz
endelse
print,'rlaz,flaz,n1=',rlaz,flaz,nlay+1
end

535: begin ;  KRC35  TFINE and fff analysis REQ 252
KRC35,ifile,ttt,caset,ddd,alat, get52
if get52 ne '' then begin 
  pari[26]=1                        ; return even bad cases
  kons=[252,254,535] & kon=123 & goto,dokon
endif
end 

536: begin & rlaz=parg[15] & flaz=parg[16] ; Replicate some TDAY code
dbot=7.68
ydz=0.2788
ddz=7.658
fa1=fltarr(50) &  dtj=fltarr(50) ; 
qa=1.                            ; thickness of normalized first layer
fa1[1]=qa              ; Construct sum to bottom of 1-based index of
dtj[1]=1.                        ; physical layer, So [0] is meaningless
for j=2,49 do begin & qa=qa*rlaz & fa1[j]=qa & dtj[j]=dtj[j-1]+qa & endfor
fac8=FLAYER(dbot/flaz,rlaz) ; ideal number of layers so far 
fac82=flaz*rlaz^(fac8-0.5)  ; goal thickness in d units of next layer
fac9=ddz/fac82              ; normalized sum for zone thickness
qq=FLAYER(fac9,rlaz)        ; number of layers needed for this zone
ii=round(qq)
qc=ydz/dtj(ii) 
help,dbot,rlaz,flaz,ddz,fac8,fac82,fac9,qq,ii,qc 
end

54: begin ; BUG: check Robin runs. REQ [old,new] tth,ttt, tsm,tsz, ggh,ggg
ttm=reform(tth[*,0,*,*,0]) ; old Ts[hour,lat,seas] for TYpe 52
tt2=reform(ttt[*,0,*,*,0]) ; new " 
HISTFAST,ttm-tsm & PAUSE,-1 ; Type 52 - Type -1 , Old Tsur.
HISTFAST,tt2-tsz & end       ; " " "new " 

541: begin   ; CLOT delt Down- near equator
ddm=reform(dtt[*,*,jeq,*,0])  ; Delta at equat. [hour,item,season]
qq=ddm[*,3:4,*] & qq=transpose(qq,[0,2,1]) & qq=reform(qq,nhour*nsea,2,/over)
xtit='hour * season'
CLOT,qq,'Delta '+itemt[3:4],locc=1,titl=[xtit,' Delta Down',kite+vtit] 
down=reform(tth[*,3:4,9,*,0]) &  down=transpose(down,[0,2,1])
down=reform(down,nhour*nsea,2,/over) & PAUSE,-1
CLOT,qq/(down>20.),'Ratio '+itemt[3:4],locc=1,titl=[xtit,'Ratio Down',kite+vtit] 
 end

542: begin & ddma=reform(ddm[*,2,*]) ; Plot delta Ta REQ 541
plot,ddma, xtit=xtit,ytit='Delta Tatm' & end

543: CLOT,ddma,sls,locc=[.4,.93,-.02,.06] $ ; CLOT delta Ta REQ 542
,titl=['Hour','Delta Ta',kite+vtit]

544: begin & ii=10*indgen(4) +1 ;  4*10 spaced seasons
taeq=ttt[*,2,9,ii,0]          ; Ta [hour, season] new                  
taeh=tth[*,2,9,ii,0] & print,'Ls =',lsv[ii]
plot,taeq,xtit=xtit+'/10   Dashed=old',ytit=' T atm' 
oplot,taeh,line=2                   
end

545: begin & i=pari[11] & j=pari[12]          ; CHART delta ggg
if j lt i then begin & i=0 & j=nlat-1 & endif ; lat index range
qq=dgg[*,i:j,*]               ; for range of latitudes
print,'Lats used:',slat[i:j]
dd=transpose(qq,[2,1,0]) & dd=reform(dd,nsea*(j-i+1),6,/over)
CHART,dd,parti='Delta '+itemg,xtit='season * latitude',dlin=1,titl=kite+vtit
end

546: begin &   ; stats of all TTT items REQ dtt
Print,'ttt-tth, all hour,lat,season,case'
;    '  -0.356    1.734  Tplan
print,'   Mean   StdDev   what'
for i=0,4 do print,MEAN_STD(dtt[*,i,*,*,*],std=yb),yb,itemt[i] $
 ,form='(2f10.5,2x,a)'
end

547: begin & q='fort.74' ; Process fort.74
sss=READTXTCOL('/work1/build/run/'+q,nskip=0,ncol=5)
v199=float(sss[*,1:*]) & flab=['DJU5','SUBS','DAU','SDEC']
sss=READTXTCOL('~/krc/tes/'+q,nskip=0,ncol=5)
siz=size(sss) & n=siz[1]
v233=float(sss[*,1:*])          ;   DJU5,SUBS,DAU,SDEC
qq=v233-v199 & qq[*,1]=PM180(qq[*,1])
xa=total(qq[*,0])/n & print,'Average DJU5 offset',xa
qq[*,0]=qq[*,0]-xa
CHART,qq,parti='Del '+flab,dlin=1 
las=fltarr(n,3)
for i=0,n-1 do begin & q=LSUBS(v233[i,0],dau) & las[i,*]=[q,dau] & endfor
d233=v233[*,1:3]-las & d233[*,0]=PM180(d233[*,0])
PAUSE,-1  & CHART,d233,parti='Del '+flab[1:3] & wset,2
PAUSE,-1  & CHART,v199[*,1:3]-las & wset,0
; if only Ls offset, at Ls=0, del Ls = !radeg del_sdec/obliq
PAUSE,-1  &  plot,las[*,0],d233[*,2]
end

548: begin &  deldj=0.4330591 ; slide time REQ 547
deriv=shift(v233,[-1,0])-v233 & deriv[*,1]=PM180(deriv[*,1])
CHART,deriv[0:n-2,1:3],parti='Deriv. '+flab[1:3],dlin=1
v29=v233+deldj*deriv
d29=v29[*,1:3]-las & d29[*,0]=PM180(d29[*,0])
CHART,d29[0:n-2,*],parti='adj '+flab[1:3] & wset,2
; corrects much of DAU, but sdec worse.
end

549: begin & fmt='(a10,4f12.5)'; BUG Check Ts and Tp for equivalence between types
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
jj=[0,4,5] ; corresponding items in gg1
for i=0,2 do begin 
   qt=gg1[*,*,jj[i]]- ggg[ii[i],*,*,0]
   aa=HSTATS(qt,['M','S','I','X']) & print,itemg[ii[i]],aa ,form=fmt
   if luf ne 0 then printf,luf,itemg[ii[i]],aa ,form=fmt
endfor
end

55: begin & dtt=ttt-tth & dgg=ggg-ggh  & deldd=ddd-ddh & end ; Delta .t52

;---------------------------- compare two versions of t52 ------------
550: begin ; Constrict latitude, prepare for t52 differences REQ 252 26 252
ii=where(alat ge parr[24] and alat le parr[25],j)
print,'Will leave ',j,' latitudes'
if j lt 1 then goto,halt
nlat=j & uuu=uuu[ii,*,*]
alat=uuu[0:nlat-1,0,0]          ; latitudes for first case
slat=ST0(alat,/nojoin)          ; lats as string
q=min(abs(alat),jeq) & pari[6]=jeq & jlat=jeq ; 
ttt=ttt[*,*,ii,*,*] & sizt=size(ttt)
ddd=ddd[*,*,ii,*,*]
ggg=ggg[*,ii,*,*]
tlow=reform(ddd[0,0,*,*,*]) ; minimum T of surface layer [lat,seas,case]
nloc=n_elements(tlow)  ; nlat*nsea*ncase
ndj4=fix(reform(ggg[0,*,*,*])) ; [lat,season,case] number of days to convergence
dtit=sver                      ; general title
n4=nlat & m4=indgen(n4) ; insurance
; If have held arrays, winnoew them the same
siz=size(tth)
if siz[0] eq 5 then begin 
  k=siz[3]
  hlat=uuh[0:k-1,0,0]        ; latitudes for first case
  ii=where(hlat ge parr[24] and hlat le parr[25],j)
  print,'Will leave ',j,' latitudes'
  if j ne nlat then goto,halt
  uuh=uuh[ii,*,*]
  hlat=uuh[*,0,0]        ; latitudes for first case
  if total(abs(alat-hlat)) gt 0 then goto,halt
  tth=tth[*,*,ii,*,*] & sizh=size(tth)
  ddh=ddh[*,*,ii,*,*]
  ggh=ggh[*,ii,*,*]
  dtit=sver+' - '+verh          ; general title
; Mask for NDJ4 the same
  ndj4h=fix(reform(ggh[0,*,*,*]))
  del4=ndj4-ndj4h               ; change in number of iteration days
  m4=where(del4 eq 0.,n4)       ; mask: days the same within [lat,season,case]
  print,'Num lat*seas*case with NDJ4 same/diff=',n4,nloc-n4
  dtt=ttt-tth & dgg=ggg-ggh  & deldd=ddd-ddh
endif
end

551: begin ; Quilt [last season] REQ 561
if ndq eq 4 then begin 
q3=reform(qy[*,*,sizq[3]-1,*]) ; last season
q3q=QUILT3(q3,['hour','lat','case'],/mod3,prt=3,mag=5) 
endif & end

552: begin     ; Differences for first case, last season
dtsa=reform(ttt[*,0:1,*,nsea-1,0]-tth[*,0:1,*,nsea-1,0])
CLOT,reform(dtsa[*,0,*]),slat,locc=[.1,.95,-.022,.03],titl=['hour index','Tsurf',kite]
if CALL91(kite,'S') then stop   ; <><><><><><><><>
CLOT,reform(dtsa[*,1,*]),slat,titl=['hour index','Tatm',kite]
end

56: begin & wart='' ; Select array and item
read,wart,prompt='Which array: t d g u or v > '
j5=0 & read,j5,prompt=' Which item, 0-based > ' 
j5=j5>0                         ; insurance
pari[21]=j5
print,'     IDL     KRC'
for i=0,ncase-1 do print,i,'  ',caset[i]
Print,'Check @12 11:12 latitude range, 17:18 season range' & end

561: begin ; Prepare the base and difference  REQ 252,26,252  56
;k1=pari[7] & k2=pari[8]  ;|  k1=k2=match one case    k2>k1= diff 2 cases of now
;                         ;|  k2<k1=match all cases   k1<0= diff all now to k2 
; To avoid complexity of vanishing dimensions of 1, require that any 
; sub-selection of latitude or season leave dimension of at least 2
; ARRSUB does arg1-arg2
wlab=['hour','lat','seas','case','layer'] ; dimensions if using ttt array
parq=pari[[21,7,8,11,12,17,18]] ; indices needed by ARRSUB
j5=parq[0]
case wart of    ; qy=ARRSUB is always arg1-arg2
  't': begin & u0=3 & xlab=itemt[j5]   ; ttt: item is index 2
    qy=ARRSUB(ttt,tth,wlab,parq,sdim,qw,qb,qp,ku=ku)
    qt=dtt[*,*,ku[3]:ku[4],ku[5]:ku[6],ku[1]:ku[2]]
    print,htit
    for j=0,niti-1 do begin & aa=HSTATS(qt[*,j,*,*,*],hkode) 
    print,itemt[j],aa,form=hfmt & endfor
  end
  'd': begin & u0=8 & xlab=itemd[j5]    ; ddd: item is index 2
  qy=ARRSUB(ddd,ddh,wlab,parq,sdim,qw,qb,qp) & end
  'g': begin & u0=10 &  xlab=itemg[j5]  ; ggg: item is index 1
    wlin=wlab[1:4]
  qy=ARRSUB(ggg,ggh,wlin,parq,sdim,qw,qb,qp) & end
; qb and qp not yet implimented for next 2 arrays
  'u': begin & u0=16 & xlab=itemu[j5]        ; uuu: item is index 2
    wlin=wlab[[2,1,4]]
  qy=ARRSUB(uuu,uuh,wlin,parq,sdim,qw,qb,qp) & end
  'v': begin & u0=18 & xlab=itemv[j5]       ; vvv: item is index 2
    wlin=wlab[[3,1,4]]
  qy=ARRSUB(vvv,vvh,wlin,parq,sdim,qw,qb,qp) & end
  else: goto,halt
; sdim out index dimensions
endcase
if n_elements(qy) eq 1 then begin 
  print,'ARRSUB error ',qy & goto,halt & endif
j5=parq[0] & k1=parq[1] & k2=parq[2] & j1=parq[3] & j2=parq[4] 
s1=parq[5] & s2=parq[6] ; actual values used
if k1 ge 0 then qtit=stem+' - '+ steh  $
           else qtit=stem+': case '+strtrim(k2,2)+' - '+strtrim(k1,2)
help,qy,qb,qp  ; qy=delta  qp=+  qb=-
sizq=size(qy) & ndq=sizq[0] ; dimensionality of qy
mlat=sizq[2] & msea=sizq[3] & mcase=sizq[4]
;sdim=sdim[0:ndq-1]        ; case dimension may be gone
ztit=string(xlab,qw,sdim,form='(a,1x,a,4a6)')
print,ztit
if ndq ge 3 then print,' quilt before any other display' else dd=qy
zz=HSTATS(qy,/prt)     ; prints table
if parj[11] then begin
  qy=abs(qy)
  sdim[0]='Abs: '+sdim[0]
endif & end

; see 55x

562: begin & tiny=paru[2] ; Stats versus latitude  REQ ttt and 561
if sizq[0] lt 3 then goto,halt
; assumes qy is [hour,lat,seas,case] and 'siz' is good
qt=total(qy,1)/sizq[1]         ; average [lat,seas,case]
xa=total(abs(qy),1)/sizq[1]         ; average abs [lat,seas,case]
xb=total(xa,2)/sizq[3]  ; average abs [lat,case]
ztit=qtit+':  '+xlab+'. '+qw
print,'mean delta Abs. (each case) last row and column are average'
print,ztit & print,'Idx',[slat[ku[3]:ku[4]],'Average','in mK'],form='(a3,9a8)'
if sizq[0] eq 3 then begin     ; only one case 
  za=MEAN_STD2(qt,std=zb) ; stats for DiurnalAve for each latitude
  ya=min([za,zb],max=yb)
  plot,za,yran=[ya,yb], xtit='Lat index',ztit=ztit $
       ,ytit='Mean[solid] and Std[dash] change'
  oplot,zb,line=2
  print,'Mean= ',float(za)
  print,'StDev=',float(zb)
endif else begin                ; multiple cases
  za=fltarr(mlat+1,mcase+1,2)   ; add average in both dimensions
  for i=0,mcase-1 do za[0:mlat-1,i,*]=MEAN_STD2(qt[*,*,i],/std)
  za[*,mcase,*]=total(za[*,0:mcase-1,*],2)/mcase 
  za[mlat,*,*]=total(za[0:mlat-1,*,*],1)/mlat 
  fmt='(i3,37f8.1)'
  for i=0,mcase do print,i+1,1000.*za[*,i,0],form=fmt
  print,'StDev=' 
  for i=0,mcase do print,i+1,1000.*za[*,i,1],form=fmt
  CLOT,xb,caset[k1:k2],/ylog ,tsiz=2.,titl=['Latitude index' $
                      ,'Mean absolute change of surface temperature',ztit]
endelse
end 

563: begin & dell=ggg-ggh & k=10 ; ggg REQ 561
; =nhour*nlat*nsea*ncase ; number of items for each type
print,htit +'  0]='+labu[k]
j1=0 & j2=5 & j=parj[6]  ; parj[6] - means do all items
if j ge 0 then begin & j1=(j1>j)<j2 & j2=j1 & end; do one valid item
for j=j1,j2 do begin ; each type
   qq=dell[j,*,*,*] ; ggg-ggh [latitude,season,case]
   zz=HSTATS(qq,hkode)
   print,itemg[j],zz,form=hfmt
   if zz[4] gt paru[k+j] then begin ; MeanAbs > tolerance
      qq=reform(qq,/over)
     if parj[11] then qq=abs(qq)
     prt=parj[9] & if j eq j1 then prt=prt+2
      if ncase lt 2 then TVFAST,transpose(qq),mag=3,win=2  $
      else out=QUILT3(qq,['latitude','season','case'],mag=parj[5] $
                      ,mod3=parj[0],grid=parj[8],prt=prt)
      if CALL91(ptitl,'kv570') then stop
 endif
endfor & end
; transpose qq so that season increases to the right.

564: begin & dell=uuu-uuh & k=16  ; uuu
print,htit+'  0]='+labu[k]
j1=0 & j2=1 & j=parj[6]
if j ge 0 then begin & j1=(j1>j)<j2 & j2=j1 & end; do one valid item
for j=j1,j2 do begin ; each type
   qq=dell[*,j,*]
   zz=HSTATS(qq,hkode)
   print,itemu[j],zz,form=hfmt
   if zz[4] gt paru[k+j] then begin 
      qq=reform(qq,/over)
      if ncase lt 2 then plot,qq,xtit='lat index',ytit=itemu[j] $
      else TVFAST,transpose(qq),mag=parj[5],win=2
      print,'Any key to go' & i=GET_KBRD(1) ; wait
 endif
endfor & end

565: begin & dell=vvv-vvh & k=18 ; vvv
print,htit+'  0]='+labu[k]
j1=0 & j2=4 & j=parj[6]
if j ge 0 then begin & j1=(j1>j)<j2 & j2=j1 & end; do one valid item
for j=j1,j2 do begin ; each type
   qq=dell[*,j,*]
   zz=HSTATS(qq,hkode)
   print,itemv[j],zz,form=hfmt
   if zz[4] gt paru[k+j] then begin 
     if ncase lt 2 then plot,qq,xtit='lat index',ytit=itemv[j] $
     else TVFAST,transpose(reform(qq)),mag=parj[5],win=2
     print,'Any key to go' & i=GET_KBRD(1) ; wait
 endif
endfor & end

566: begin & j5=0 & k=0 ; HISTFAST now-h of any one item, all season and case REQ 550
read,j5,k,prompt='1=t 2=d 3=g 4=u 5=v  and item therein > '
case j5 of
   1: begin &  k=(k>0)<4
      qq= dtt[*,k,*,*,*]
      n1=nhour & xlab=itemt[k] & end
   2: begin &  k=(k>0)<1
      qq= ddd[*,k,*,*,*]-ddh[*,k,*,*,*]
      n1=nlay & xlab=itemd[k] & end
   3: begin &  k=(k>0)<5
      qq= ggg[k,*,*,*]-ggh[k,*,*,*]
      n1=1 & xlab=itemg[k] & end
   4: begin &  k=(k>0)<1
      qq= uuu[*,k,*]-uuh[*,k,*]
      n1=0 & xlab=itemu[k] & end
   5: begin &  k=(k>0)<4
      qq= vvv[*,k,*]-vvv[*,k,*]
      n1=0 & xlab=itemv[k] & end
else: goto,halt
endcase
HISTFAST,reform(qq,/over),xlab=sver+' - '+verh+ ' Delta of '+ xlab
end

567: begin & if n1 lt 1 then goto,halt ; Avoid frost temperatures REQ 566
     qq=reform(qq,n1,nloc,/over)
     ii=where(tlow gt parj[7],j)
if j lt 5 then goto,halt ; too few warm
print,' T test:',parj[7],'  NumLocs: total, warm, cold, N4diff:',nloc,j,nloc-j $
,nloc-n4,form='(a,f6.1,a, 4i7)'
print,htit
zz=HSTATS(qq,hkode)
print,xlab,zz,form=hfmt
zz=HSTATS(qq[*,ii],hkode)
print,'warm',zz,form=hfmt
zz=HSTATS(qq[*,m4],hkode)
print,'NDJ4=',zz,form=hfmt
HISTFAST,qq[*,ii],xlab=sver+' - '+verh+ ' Delta of '+ xlab $
+ ' where above '+strtrim(parj[7],2)
end

568: begin  ; CLOT correlation of T differences REQ 550
qq=reform(dtt,nhour,niti,nloc)
qt=transpose(qq,[0,2,1])
qt=reform(qt,nhour*nloc,niti,/over)
CLOT,qt,itemt,locc=1,yran=parp[2:3],psym=3,titl=['Latitude * season * case' $
,'Candidate-Formal',kite+dtit]
end

569: begin  ;  only warm REQ 550
qq=reform(dtt,nhour,niti,nloc)
ii=where(tlow gt parj[7],j)   ; keep only warm locations
qc=qq[*,*,ii]
plot,qc[*,0,*],qc[*,2,*],psym=3,xtit=itemt[0]+' warm',ytit=itemt[2] $
,title=kite+dtit
end

570: begin ; CLOT Ts and insolation changes REQ 26   
; firmcode the wild cases and the attenuation factor
zz=reform(ttt[*,0,jlat,nsea-1,*]) & yy=reform(tth[*,0,jlat,nsea-1,*])
ya=min([yy,zz],max=yb)
q=['Hour index.    Dashed is held  Lat='+slat[jlat],'Surface temperature' $
,kite+stem +' held='+steh+' Last season']
CLOT,zz,caset,locc=1,yran=[ya,yb],titl=q
CLOT,yy,oplot=-2
  if CALL91(ptitl,'kv570') then stop  ; <><><><><><><><>
q[1]='Surface temperature: now-hold'
qq=zz-yy
CLOT,qq,caset,locc=1,titl=q
  if CALL91(ptitl,'kv570d') then stop  ; <><><><><><><><>
zz=reform(ttt[*,3,jlat,jsea,*]) & yy=reform(tth[*,3,jlat,jsea,*])
q[1]='Insolation: now-hold.  Ls='+sls[jsea]
CLOT,zz-yy,caset,locc=1,titl=q
  if CALL91(ptitl,'kv570q') then stop  ; <><><><><><><><>
end

57: begin &  siz=size(qy); One ttt every hour REQ 561
; xlab=item title  qy=data[hour,lat,seas,case]  
; qw=title of data ranges  qtit=title of files
nh=nhour
if parj[10] eq 0 then qq=qy else begin ; reduce to 24 hours
  j=nhour/24 & ii=j*indgen(24)+1 ; every hour
  qq=qy[ii,*,*,*]                ; item [24,lat,seas,case]
  nh=24
endelse
ya=parr[10] & yb=parr[11]    ; @16
if yb lt ya then ya=min(qy,max=yb) 
qt='case '+stid[1:ncase]
siq=size(qq)
if siq[0] lt siz[0] then dummy=ONE_MORE_D(qq,1,/rep)
q4=transpose(qq,[0,2,3,1]) ;[hour,season,case,lat]
yy=reform(q4,nh*siz[3]*siz[4],siz[2])
titl=['hour * season * case',xlab,qtit+' : '+qw ]
CLOT,yy,slat[parq[3]:parq[4]],locc=parp[22:25],ksym=-3,yran=[ya,yb],titl=titl
PLOTSECT,qt,parp[26]
end

571: begin & siz=size(qq) ; Attenuate any large diversions REQ 57 
xa=max(abs([ya,yb]))
qr=reform(qq,siz[1]*siz[2]*siz[3],siz[4])
; za=MEAN_STD2(qr,std=zb,/one)
za=MINMAX2(abs(qr), zb,/one) ; zb in maximum of each case
jj=where(zb gt xa ,j) ; all cases with range outside the @57 plot
if j gt 0 then begin
  q=qt ; copy the legend titles
  for i=0,j-1 do begin & k=jj[i] & xb=ceil(zb[k]/xa) & qr[*,k]=qr[*,k]/xb $; attenuate the wild cases
   & q[k]=qt[k]+' /'+strtrim(xb,2) & endfor
qr=reform(qr,siz[1],nlat,nsea,ncase,/over)
q4=transpose(qr,[0,2,3,1]) ;[hour,season,case,lat]
yy=reform(q4,siz[1]*nsea*ncase,nlat)
CLOT,yy,slat,locc=parp[22:25],ksym=-3,yran=[ya,yb],titl=titl   ; locc in @15
PLOTSECT,q,parp[26]
endif & end

572: begin  ; Tave(layer) difference  SPEC 311
; firmcode the wild cases and the attenuation factor
q=['Season index: sols from Ls=0 .   Lat='+slat[jlat] $
,'layer temperature',kite+ifile +' - '+ifh]
q1=q[1]
qa='KRC layer '+stid[2:nlay+1]  ; curve titles
qt='case '+stid[1:ncase]        ; case titles
tave=reform(dav[*,jlat,*,*])    ; [layer,season,case]
tavh=reform(dah[*,jlat,*,*])
yy=transpose(tave,[1,2,0])
yy=reform(yy,nsea*ncase,nlay,/overw)
CLOT,yy,qa,locc=1,titl=q
PLOTSECT,qt,.1
  if CALL91(ptitl,'kv571a') then stop
qq=tave-tavh
for i=1,4,3 do qq[*,*,i]=qq[*,*,i]/40. ; attenuate the wild cases
for i=1,4,3 do qt[i]=qt[i]+' /40.'
yy=transpose(qq,[1,2,0])
yy=reform(yy,nsea*ncase,nlay,/overw) 
  q[1]=q1+' now-hold'
CLOT,yy,qa,locc=[.25,.93,-.025,.03],titl=q
PLOTSECT,qt,.05
  if CALL91(ptitl,'kv571') then stop
end

573: begin & qz=reform(qy[*,jlat,*,*]) ;  Delta Tsur, 1 lat, 3 seas REQ 561
yb=max(qz[*,[0,12,31],*])
CLOT,reform(qz[*,0,i5]),caset[i5],locc=[.7,.93,-.03,.03],xx=xxh,yran=[0.,yb] $
,titl=['hour','Delta Tsur from Lambertian',kite+stem+' Lat='+slat[jlat]]
CLOT,reform(qz[*,12,i5]),xx=xxh,oplot=-2
CLOT,reform(qz[*,31,i5]),xx=xxh,oplot=-1
print,lsv[[0,12,31]]
end

574: begin & print,ztit  ; QUILT3 and make dd REQ 561
hdim=sdim ; titles: if 4D, will be modified by QUILT3
qq=qy     ; insurance, as QUILT3 can reform arg1
dd=QUILT3(qq,sdim,ar=-1,mag=parj[4],mod3=parj[0],grid=parj[8],prt=parj[9])
for j=1,2 do begin
  i=where(wlab[j] eq sdim) & i=i[0] ;
  if i eq 1 then print,'Latitude range: '+slat[j1]+' '+slat[j2] 
  if i eq 2 then print,'Season range: '+sls[s1]+' '+sls[s2]
endfor
siz=size(qy)
if siz[0] eq 4 then begin ; also average over hour
  if CALL91(kite,'nm') then stop ; <><><><><><><><>
  qq=total(qy,1)/nhour
  dd=QUILT3(qq,sdim[1:*],ar=-1,mag=parj[5],mod3=parj[0],grid=parj[8],prt=parj[9])
endif
end
;------------------------------------------

575: begin & siz=size(tsm) & if siz[0] ne 3 then goto,halt ; Store Type 0,-1
siz=size(tsz) & if siz[0] ne 3 then goto,halt
lsmh=lsm & tsmh=tsm & tpmh=tpm ;
lszh=lsz & tszh=tsz & tpzh=tpz & gg1h=gg1 & end

576: begin ; CLOT seas*2h*case,lat REQ 561 [884]
qshcl=transpose(qy[[nhour/2,nhour-1],*,*,*],[2,0,3,1])
qshcl=reform(qshcl,nsea*2*ncase,nlat,/over) ; 
CLOT,qshcl>1.e-4,slat,locc=[.73,.10,.02,.00],tsiz=1.5,/ylog,/abso,ksym=-1 $
,titl=['season * [noon,midnight] * case','Delta '+xlab,kite+qtit]
PLOTSECT,caset,.03,cs=2. 
if CALL91(kite,'nm') then stop   ; <><><><><><><><>
plot, ttt[*,0,9,23,2],yran=[140,325],ytit='Tsurf',titl=kite+qtit $
,xtit='hour  Line is Ls=90, dash is Ls=272. Color is v356'
oplot,ttt[*,0,9,62,2],line=2    
oplot,tth[*,0,9,23,2],color=200 
oplot,tth[*,0,9,62,2],line=2,color=200  
if CALL91(kite,'l9') then stop   ; <><><><><><><><>  
plot, ttt[24,0,9,*,2],yran=[140,325],ytit='Tsurf',titl=kite+qtit $
,xtit='season  Line is noon, dash is midnight. Color is v356'
oplot,tth[24,0,9,*,2],line=2       
oplot,tth[47,0,9,*,2],line=2,color=200
oplot,ttt[47,0,9,*,2],color=200 
end

580: begin & fmt='(a10,4f12.5)' ; Compare Versions for Type 0 and -1
print,'   What          Mean      StdDev     Minimum     Maximum'
qt=lsz-lszh
aa=HSTATS(qt,['M','S','I','X']) & print,'Ls 0',aa,form=fmt
qt=tsz-tszh
aa=HSTATS(qt,['M','S','I','X']) & print,'Ts 0',aa,form=fmt
qt=tpz-tpzh
aa=HSTATS(qt,['M','S','I','X']) & print,'Tp 0',aa,form=fmt
qt=gg1-gg1h
aa=HSTATS(qt,['M','S','I','X']) & print,'ddd 0',aa,form=fmt
qt=lsm-lsmh
aa=HSTATS(qt,['M','S','I','X']) & print,'Ls -1',aa,form=fmt
qt=tsm-tsmh
aa=HSTATS(qt,['M','S','I','X']) & print,'Ts -1',aa,form=fmt
qt=tpm-tpmh
aa=HSTATS(qt,['M','S','I','X']) & print,'Tp -1',aa,form=fmt
end 

58: begin ; get annual heat flow
ucase=['base','Taud=.1','I=100','Ice@8','Ice@4']
delz= 0.0112-  0.0035 ; @27 center depth Layer 3-2 
zcon= 3.864E-02 ; @27 conductivity
tave=total(ddd,2)/2. ; 
hf= (zcon/delz)*reform(tave[1,*,*,*]-tave[0,*,*,*])
hf=transpose(hf,[1,2,0]) ; [seas,case,lat],
tmax=reform(ddd[0,1,*,*,*]) ; Tmax[lat,seas,case]
tmax=transpose(tmax,[1,2,0]) ; Tmax[seas,case,lat]
; slat=ST0([-88.75, -85.00, -80.00, -75.00, -70.00],/nojoin)
CLOT,reform(tmax,nsea*ncase,nlat),slat,locc=1
end

581: CLOT,reform(hf,nsea*ncase,nlat)>0.,slat,locc=1 $ ; CLOT heatflow
,titl=['season * case', 'Upward heatflow',kite+stem]

;59: begin ; concat .tm1 into .t52
;for i=0,18 do begin 

; 6_6_6_6_6_6_6_6_6_6_6_6_6_6_6_6_ tests between versions
6: q=TTTMOD([i], ttt,ddd,ggg,uuu,vvv,lsv,sdat, tth,ggh,lsh $ ; TTTMOD specials
, kcom=kcom,stem=stem,krct=krct)

61: begin & yy=PM180(lsv-lsh); Plot LS-LSH
vtit=ifile+' - '+ifh
qt=max(abs(yy)) & print,'Maximum difference in Ls is:',qt
if qt ne 0 then plot,djmm,yy,xtit='MJD  '+ifile,ytit='lsv-lsh' $
,titl=vtit
end

613: begin & tvtab='Tsur'       ; Prepare many latitudes for quilt.
tdd=reform(ttt[*,0,*,*,0]-tth[*,0,*,*,0],/over) ;  Delta Tsurf
tseq= reform(tdd[*,jeq,*])                        ; equator [hour,season]
CLOT,tseq,sls,locc=[.1,.95,-.02,.03],titl=['Hour index','Delta Tsur at equator',kite]
end

615: begin ; KRC  average daily temperature
tsav=reform(total(ttt[*,0,*,*,*],1))/nhour ; Tsur ave. [lat,season,case]
;qq=reform(tsav[1,ks1:ks2,*]); middle lat, last year
if nlat eq 1 then qq=tsav else qq=reform(tsav[1,*,*]); middle lat, last year
xx=lsv & i=nsea-1 & if xx[i] lt 180. then  xx[i]=xx[i]+360.
CLOT,qq,caset,xx=xx,locc=[.2,.9,-.03,.06],titl=['Season: Ls' $
,'Diurnal average surface temperature K',kite+parf[11]]
ya=min(qq[*,0],max=yb) ; min/max of nominal case
yswig=yb-ya ; annual variation of daily averag Tsur
twig=ddd[0,1,jlat,ks1:ks2,0]-ddd[0,0,jlat,ks1:ks2,0]; diurnal swing of top layer, nominal case
dan=total(twig)/(ks2-ks1+1) ; average diurnal swing of top layer
print,'average diurnal swing of top layer',dan
end

616: begin & ii=leny*indgen(nfy+1) ; one sol/year
; assumes have N*year+1 seasons
tty=reform(ttt[*,0,0,ii,*])
for k=0,ncase-1 do begin 
    yy=AVALG(reform(tty[*,0:3,k]),reform(tty[*,4,k]),'-')
    CLOT,yy,'Ls=0, year'+string(3+indgen(4),form='(i2)'), locc=1 $
         ,xx=xxh, titl=['Hour','Difference from final date',kite+caset[k]]
    if k lt ncase-1 then PAUSE,-1
endfor & end

62: begin  ; Tsurf for 1 hour & lat, Ver A and B case 0
xx=reform(tth[ihour,0,jlat,*,0]) ; noon equator case zero 
yy=reform(ttt[ihour,0,jlat,*,0]) ; noon equator case zero  Ver B
ya=min([xx,yy],max=yb) ; total range in T
plot,xx,yran=[ya,yb],ytit='Tsur near noon equator' $
,xtit='Season index  Dashed is VerB, diamond is 100*diff, + is 100*Ta diff.' $
,title =qtit+':  Hour='+sour[ihour]+'  Lat='+slat[jlat]
oplot,yy,line=2,color=99            ; ver B
oplot,[0,nsea],[280.,280.] , line=1 ; Delta=0 line
oplot,280.+100*(yy-xx),psym=4       ; 100* Delta Tsur 
oplot,280.+100.*(ttt[ihour,2,jlat,*,0]-tth[ihour,2,jlat,*,0]), psym=1, color=99; 100* Delta Ta
end

; Or, req 561 with one lat, all seasons, all cases
622: begin & qt=itemt[it1]; Clot Tx for 1 hour & lat, Ver A and B, all cases
if it1 eq 0 then j=2 else j=0  ; 2nd item is Tatm or Tsurf
yy=reform(ttt[ihour,it1,jlat,*,*])  ; [season,case] Ver A
xx=reform(tth[ihour,it1,jlat,*,*])  ; [season,case] Ver B
qa=reform(ttt[ihour,  j,jlat,*,*])  ; " "  Ver A
qq=reform(tth[ihour,  j,jlat,*,*])  ; " "  Ver B
ya=min([xx,yy,qa,qq],max=yb) ; total range in Tx
zz=yy-xx                    ; A-B [season,case]
ff=SCALELIN(zz,vv=[ya,yb]) ; Delta [season,case] scale onto abs. temperature
t0=ff[0] ;  scaled=(xx-func[0])*func[1]  
print,t0,' =ZeroDelta. and  Y mag factor=',ff[1]
ink= abs(ff[1]) lt 5. and t0 lt ya or t0 gt yb ; plot in absolute K
if ink then q='' else q='  Diff Scaled by =' +strtrim(ff[1],2)
CLOT,xx,caset,locc=-1,yran=[ya,yb], titl=['Season index.  Ver. A   line=' $
+qt+'   long dash='+itemt[j]+'  symbols are Ver. B' $ ; x title
,q+'  + or X  is '+qt+'   Box or diamond is '+itemt[j] $ ; Y title
,'A-B= '+qtit+':  Hour='+sour[ihour]+'  Lat='+slat[jlat]] ; Top title
CLOT,qa,oplot=500  ; Ver A 2nd as longdash
if ink then begin   ; ff[1] is magnification    plot absolutes
  CLOT,yy,oplot=100,ksym=-1    ; item 1
  CLOT,qa,oplot=100,ksym=-4    ; item 2
endif else begin ; plot differences
  CLOT,(  zz   -t0)*ff[1],oplot=100,ksym=-7
  CLOT,((qa-qq)-t0)*ff[1],oplot=100,ksym=-6 
  oplot,[0,nsea],[t0,t0],line=1 ; Delta=0 line
endelse 
end

63: begin  ; Stats on VerB-VerA (briefer than 56x) REQ 550
if total(abs(sizt[0:5]-sizh[0:5])) ne 0 then goto,halt ; some dimension different
help,ifh,ifile & print,htit
for j=0,niti-1 do begin & aa=HSTATS(dtt[*,j,*,*,*],hkode) 
print,itemt[j],aa,form=hfmt & endfor
for j=0,1 do begin & aa=HSTATS(deldd[*,j,*,*,*],hkode) 
print,itemd[j],aa,form=hfmt & endfor
for j=0,5 do begin & aa=HSTATS(dgg[j,*,*,*],hkode) 
print,itemg[j],aa,form=hfmt & endfor
qt=vvv-vvh
for j=0,4 do begin & aa=HSTATS(qt[*,j,*],hkode) 
print,itemv[j],aa,form=hfmt & endfor
qt=uuu-uuh
for j=0,1 do begin & aa=HSTATS(qt[*,j,*],hkode) 
print,itemu[j],aa,form=hfmt & endfor
; do subsets of ttt to remove poor conditions
qq=reform(dtt,nhour,niti,nloc) ; [hout,item, location]
if n4 eq nloc then print, 'All NDJ4 the same' else begin
   print,'Excluding seasons when convergence days differed'
   for k=0,niti-1 do begin 
      qt=qq[*,k,m4]              ; one item
      aa=HSTATS(qt,hkode) & print,itemt[k],aa,form=hfmt
   endfor
endelse
print, 'Excluding seasons when either surface diurnal minimum was below',parj[7]
ii=where(tlow<tloh gt parj[7],j)   ; keep only warm locations
for k=0,niti-1 do begin 
  qt=qq[*,k,ii]                 ; one item
  aa=HSTATS(qt,hkode) & print,itemt[k],aa,form=hfmt
endfor
end

631: begin ; Compare everySol with skip
i=n_elements(lsh) & j=n_elements(lsv)
if i gt j then begin ; lsv is skip
   k=round(float(i)/j)          ; sols per season
   n=i/k                        ; number of seasons  available
   jj=k*indgen(n)
   q=lsv[jj]-lsh[0:n-1]
   print,min(q,max=yb),yb
   qq=ttt[*,*,*,jj,*]-tth[*,*,*,0:n-1,*] 
endif else begin
   stop
endelse
For j=0,2 do for i=0,4 do print,MEAN_STD(qq[*,i,j,*,*],std=yb),yb,itemt[i] $
,slat[j],form='(2f9.4,a8,a5)'
q=reform(qq[*,0,2,*,*]) ; [hour,seas,case]
plot,q,psym=3
end

632: begin; Compare layer results 
tdel=(deldd[*,1,*,*,*]+ deldd[*,0,*,*,*])/2 ; delta average 
tdswi= deldd[*,1,*,*,*]- deldd[*,0,*,*,*] ; delta swing
qq=transpose(reform(tdel),[2,1,3,0])
qq=reform(qq,nsea*ncase*nlat,nlay)
locc=[.5,.93,-.025,.06]
titl=['season * latitude * case','Delta Layer T average',kite+ifile+' - '+ifh]
CLOT,qq,string(indgen(nlay)+1,form='(i2)'),locc=locc,titl=titl
PLOTSECT,caset,.21,cs=1.5 & end

633: begin & qq=transpose(reform(tdswi),[2,1,3,0]) ; CLOT delta swing REQ 632
qq=transpose(reform(tdel),[2,1,3,0])
qq=reform(qq,nsea*ncase*nlat,nlay)
titl[1]='Delta Layer T swing'
CLOT,qq,string(indgen(nlay)+1,form='(i2)'),locc=locc,titl=titl
PLOTSECT,caset,.21,cs=1.5 & end

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
lasy=nfy*leny-1                 ; last of full years
ddj=fltarr(J2-j1+1,nsea,nk) ; [latitude,season,case]
ddk=fltarr(J2-j1+1,nsea,nk)
for i=i1,i2 do ddk[*,*,i-i1]=ddd[klay[i]-2,0,j1:j2,*,i] ; [latitude,season,case]
for i=i1,i2 do ddj[*,*,i-i1]=ddd[i3,0,j1:j2,*,i] ; [latitude,season,case]
swig=ddd[0,1,*,*,*]-ddd[0,0,*,*,*]               ; diurnal swing in top layer 
swig=reform(swig,/over)                          ; [lat,season,case]
swig=transpose(swig,[1,2,0])                     ; [season,case,lat]
swig1=swig[0:lasy,*,*]                           ; get integral number of years
swir=reform(swig1[*,i1:i2,j1:j2],leny,nfy,nk,klat); [season,year,case,lat]
for i=0,i2-i1 do for j=0,j2-j1 do  $
swir[*,*,i,j]=AVALG(swir[*,*,i,j],swir[*,nfy-1,i,j],'-',dim=1)
;swig2=reform(swig,nsea*ncase,nlat) ;  [season*case,lat]
tsav=reform(total(ttt[*,0,j1:j2,*,i1:i2],1))/nhour ; Tsur ave. [lat,season,case]
tsy=reform(tsav[*,0:lasy,*],klat,leny,nfy,nk) ; [lat,season,case]
tsy=transpose(tsy,[1,2,3,0])      ; [season, year,case,latitude]
tsr=fltarr(leny,nfy,nk,klat)
for i=0,i2-i1 do for j=0,j2-j1 do  $
  tsr[*,*,i,j]=AVALG(tsy[*,*,i,j],tsy[*,nfy-1,i,j],'-',dim=1)
;tsry=total(abs(tsr),1)/leny ; MAR  [seas-in-year,lat] 
;tsr2=reform(tsr,leny*nfy,nlat)    ; [season,lat.]
; help,ttt,tsav,tsr,tsry,tsr2,ddd,ddj,ddk,ddr
CLOT,reform(tsr,leny*nfy*nk,klat),skat,yran=parp[0:1],yr2=parp[2:3] $
,locc=parp[4:7], titl=$
['season*ncase','TsurAve-last year',kite+stem]
if parp[2] eq parp[3] then PLOTSECT,slay,parp[8] $
                      else PLOTSECT,[slay,slay],parp[8]
end

642: begin & k=pari[13]  ; Last year for all cases
i=where(klay eq k) & i=i[0]
if i lt 0 then goto,halt ; invalid number of layers
tsay=fltarr(leny,ncase,klat)
; ref is [season,{last year},{ref case},latitude]
for j=0,j2-j1 do  $ ; for case for lat 
tsay[*,*,j]=AVALG(reform(tsy[*,nfy-1,*,j]),tsy[*,nfy-1,i,j],'-',dim=1)
CLOT,reform(tsay,leny*ncase,klat),skat,yran=parp[9:10],yr2=parp[11:12] $
,locc=parp[13:16], titl= $
['season of last year * case','TsurAve -last year for N1 ='+strtrim(k,2),stem]
if parp[11] eq parp[12] then PLOTSECT,slay,parp[17] else PLOTSECT,[slay,slay],parp[17]
 end

643: begin  ; Convergence at specific depth REQ 641
ddr=reform(ddj,klat,nsea*nk) ; [lat,seas*case]
CLOT,transpose(ddr),skat,yran=parp[18:19],locc=parp[22:25] $
,titl=['season * case','Tmin at same depth',kite+stem]
PLOTSECT,slay,parp[26]
end

644: begin ; Convergence at bottom REQ 641
ddr=reform(ddk,klat,nsea*nk)
CLOT,transpose(ddr),skat,yran=parp[27:28],locc=parp[31:34] $
,titl=['season * case','Tmin at bottom',kite+stem]
PLOTSECT,slay,parp[35]
end

645: begin ; Convergence of top layer diurnal swing REQ 641
CLOT,reform(swir,leny*nfy*nk,klat),skat,yran=parp[0:1],yr2=parp[2:3] $
,locc=parp[4:7], titl=$
['season*ncase','Diurnal amplitude - last year',kite+stem]
if parp[2] eq parp[3] then PLOTSECT,slay,parp[8] $
                     else PLOTSECT,[slay,slay],parp[8]
end

646: CLOT,reform(swir[*,*,i,*],leny*nfy,klat) $ ; One case REQ 642 
,skat,yran=parp[9:10], locc=[.5,.6,.025,.06] $
,titl=['season * year   N1='+strtrim(k,2),'Diurnal amplitude - last year',kite+stem]

647: begin & j=nlat-1 ; last day of lat 2 for each case 
qq=reform(ttt[*,0,j,nsea-1,*])
 CLOT,qq,locc=1
print,qq[nhour/2,*] ; noon on last
end

648: begin ; Plot matrix   REQ copy from krc .prt
plot,-tria,psym=-1  & oplot,-tric,psym=-4
print,'Any key to continue' & i=get_kbrd(1)
plot,frh,psym=-4
end

65: begin ; read fort88 for current ttt size
n2=kcom.id[1] ; time-steps
j=n2*ncase ; number of detail lines, Ntime*nlat*ncase
ssid=['JJ','COSI','COLL','QA','QI','DIRECT','DIFFUSE','BOUNCE','HALB','ALBJ']
k=n_elements(ssid)
sss=READTXTCOL('~/krc/tes/fort.88',nskip=0,ncol=k,mrow=j)
sss=reform(sss,n2,ncase,k,/overw)
cosi=float(sss[*,*,1])
halb=float(sss[*,*,8])
albj=float(sss[*,*,9])
xx=cosi[*,0]
titl=['Cosine incidence flat','HALB','fort.88']
zout=halb  ; HALB for one latitude and season, all cases
CLOT,zout,caset,locc=[.5,.9,-.03,.03],xx=xx $
,xran=[0.,1.],titl=titl, yran=[0.,1.]
; oplot,[0,1.],[0.,0.],line=1
  if CALL91(ptitl,'kvHALB') then stop
q=max(xx,i) ; find incidence angle closest to 0
qq=reform(zout[i,*])  & qm=MEAN_STD(qq,std=std); HALB there
print,qm,std
; 651
titl[1]='ALBJ'
CLOT,albj,caset,locc=[.5,.9,-.03,.03],xx=xx,xran=[0.,1.],titl=titl
  if CALL91(ptitl,'kvALBJ') then stop 
titl=['time step','HALB','fort.88,  cases']
CLOT,halb,titl=titl
PAUSE,-1 & i=400
Plot,albj[i,*],xtit='case', ytit='ALBJ at timestep='+strtrim(i,2),psym=-4
oplot,[0,n2],[0.,0.],line=1
PAUSE,-1 
titl[1]='ALBJ: =HALB after [0:1] limit'
CLOT,albj,titl=titl
PAUSE,-1 & i=580
Plot,albj[i,*],xtit='lat * case', ytit='ALBJ at timestep='+strtrim(i,2),psym=-4
i5=[0,2,5,6,4,3] & zb=max(zout[*,i5]) & help,zb
i6=[0,1,2,3,4,6]
end

651: CLOT,zout[*,i5],caset[i5],locc=[.5,.9,-.03,.03] $ ; Compare to HEMIALB REQ 65
,xx=xx,xran=[0.,1.],yran=[0.,zb<1. ],titl=['Cosine incidence flat' $
,'HALB [dashed from hemialb','fort.88  stem='+stem],ksym=3

66: HEMIALB,outx,outy,outt  ; HEMIALB

661: CLOT,outy[*,i6],outt[i6],locc=1,xx=outx $ ; CLOT hemialb REG 66
, titl=['outx','outy','Hemialb return'], yran=[0.,5.]

662:  CLOT,qm*outy[*,i6],outt[i6],xx=outx,oplot=-2 ; oplot hemialb REQ 651 66
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
,titl=['KRC layer -2;  Ls='+sls[i1] $
,' Delta Tmin over '+strtrim(j,2)+' year',kite+'File: '+stem]
end

664: begin & ymin=parr[0]  & ymax=parr[1] ; maximum Tmin layer diff. from final season
ii=leny*indgen(nfy+1) & print,lsv[ii]; all are 350.661
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
,titl=['recorded years',' Maximum of any layer Tmin from final year',kite+stem] 
end

665: begin & siz=size(ddd)      ; Plot Tmin at bottom over season
j=siz[1]-1                      ; 0based index of the lowest saved layer
i1=0 & i2=0 & read,i,i2,prompt='First and last season index to plot > '
i1=i1>0 & i2=i2<(nsea-1)            ; insurance
print,'Season index range is',i1,i2
yy=reform(ddd[j,0,*,i1:i2,0]) ; tmin_bot [lat,season]
q='season index after '+strtrim(i,2)
CLOT,transpose(yy),slat,locc=[.4,.3,.025,.06] $
,titl=[q,'Tmin of lowest layer',kite+stem]
tsav=reform(total(ttt[*,0,*,i1:i2,0],1))/nhour ; Surface temp average
ya=MEAN_STD2(yy,std=yb)
xa=MEAN_STD2(tsav,std=xb)
 print,xb/yb
tsr=reform(tsav[*,0:nfy*leny-1],nlat,leny,nfy)
tsr=transpose(tsr,[1,2,0])      ; [season, year,latitude]
for j=0,nlat-1 do tsr[*,*,j]=AVALG(tsr[*,*,j],tsr[*,nfy-1,j],'-',dim=1)
tsry=total(abs(tsr),1)/leny ; MAR  [ 
tsr2=reform(tsr,leny*nfy,nlat)    ; [season,lat.]
end

666: begin ; CLOT bottom T for one lat, all seasons.
read,i,prompt='Latitude index > '
zz=reform(ddd[j,0,i,*,0]) ; tmin_bot [season]
nfy=nsea/leny ; full years
zz=reform(zz[0:nfy*leny-1],leny,nfy)
print,'Ls of first point: ',lsv[0]
CLOT,zz,stid[0:nfy-1],locc=[.7,.9,-.025,.06] ,titl=['Season index within a year. Start at Ls '+sls[0],'Tmin of lowest layer',path+stem+'  Lat.='+slat[i]]
zzr=AVALG(zz,zz[*,nfy-1],dim=1)
end

667: CLOT,transpose(tsav),slat $ ; Plot Tsur_average REQ 665
,locc=[.4,.3,.025,.06],titl=[q,'Diurnal average of Tsur',kite+stem]

668: CLOT,reform(tsav[i,0:nfy*leny-1],leny,nfy); CLOT Tsur_ave one lat, REQ 665,666

669: CLOT,tsry,slat,locc=[.4,.5,.025,.06] $ ; Plot Tsur diurnal avg, year MAR REQ 665
,yr2=[0.,.2],titl=['Recorded year. right half magnified' $
,'MAR of Tsur diurnal average for each year  - finale year',kite+stem] 

671: begin ; Plot final Midnight Tsur
tt0=reform(ttt[23,0,*,nsea-leny:nsea-1,0]) ; midnight tsur 
CLOT,transpose(tt0),slat,locc=[.15,.3,.025,.06] $
,titl=['season index in last recorded year','Midnight Tsurf',kite+stem] & end

672: begin ; Plot difference from last season. List NDJ4
tt0=reform(ttt[23,0,*,*,0]) ; midnight tsur 
ii=leny*indgen(nfy+1) & print,lsv[ii]; all are 350.661
tt1=AVALG(tt0,tt0[*,ii[nfy]],'-')
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
,titl=['At end of recorded year','Midnight Tsurf Relative to last year',kite+stem]
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

674: begin ; comparisons REQ 26  Surface T
tlim=reform(ddd[0,*,*,*,*]) ; top layer diurnal limits [min/max,lat,seas,case]
tlih=reform(ddh[0,*,*,*,*]) 
xtit='latitude * season * case'
plot,tlim,xtit=xtit+' color=held',ytit='Top layer extreme temperatures',psym=3
oplot,tlih,psym=3,color=99
end

675: begin                      ;  Diurnal averages
tda=total(ttt,1)/nhour ; diurnal average
tdh=total(tth,1)/nhour
qq=tda-tdh
dd=transpose(qq,[1,2,3,0])
q3=reform(dd,nlat*nsea*ncase,5)
CLOT,q3,itemt,locc=1,ksym=-3 $
,titl=['lat * season * case','ttt: Diurnal average.  new-held',kite+'held='+ifh]
end

676: begin ; Atmosphere extremes, average REQ dtt
alim=fltarr(nlat,nsea,ncase,4)
for k=0,ncase-1 do for j=0,nsea-1 do for i=0,nlat-1 do begin 
   y1=min(ttt[*,2,i,j,k],max=y2) ; extremes of Ta 
   x1=min(tth[*,2,i,j,k],max=x2) ; " " held
   alim[i,j,k,*]=[x1,x2,y1,y2]
endfor ; & endfor & endfor
ali3=reform(alim,nlat*nsea*ncase,4)
CLOT,ali3,['held min','held max','New min','new max'],locc=1,ksym=-3 $
,titl=['latitude * season * case','Tatm extremes',kite+'held='+ifh]
PAUSE,-1
dts=dtt[*,0,*,*,*]               ; delta Ts 
dta=dtt[*,2,*,*,*]               ;  delta Ta
xtit='hour * latitude * season * case'
plot,ttt[*,0,*,*,*],xtit=xtit,psym=3,ytit='Ts'
PAUSE,-1
plot,dts,psym=3,xtit=xtit,ytit='Delta Ts'
end

677: begin ; Find most extreme season for start
; must have skip model loaded
q=LSUBS(djmm[0:37],aud) ; first recorded year+1
chart,aud ; look at Subsolar declination and heliocentric distance
xx=cos(!dtor*aud[*,1])*(aud[*,0]/1.52371)^2 ; combine the effects
plot,xx ; look for min as hottest in south
print,lsv[29],djmm[29] ; hottest in south ; 264.485      671.179
; start at 
end

68: begin                       ; Compare skip with everySol 
j=1  ; Delta years from JDISK in skip to everySol
th0=reform(tth[23,0,*,*,0]) ; midnight tsur  skip = 18-sol skip
tt0=reform(ttt[23,0,*,*,0]) ; [lat,season] midnight tsur  everySol
iih=lenh*(indgen(nfy+1)+j)  ; year end in Skip run
iii=leny*indgen(nfy+1)  ; every sol that matches last of 18
print,iih & print,iii
print,lsh[iih] & print,lsv[iii]
print,vvh[iih,0,0] & print, djmm[iii] 
i=iii[4]                        ; end of the last complete year
yh=th0[*,iih]                   ; [lat,year]  skip, at end of each year
th1 =AVALG(yh,tt0[*,i],'-')     ; delta same Ls
thm1=AVALG(yh,tt0[*,i-1],'-')   ; one sol earlier
thp1=AVALG(yh,tt0[*,i+1],'-')   ; one sol later
qq=strtrim(indgen(nfy+1)+round(jdisk/float(leny)),2) ;  years into run
CLOT,th1,qq,yran=parr[16:17],locc=parr[20:23],titl=['Latitude index' $
,'Midnight Tsurf Relative to last year',kite+stem]
plots,[0,18],[0,0],line=1
CLOT,thm1,oplot=-1,line=1 & xyouts,.6,.20,'dots: 1 sol earlier',/norm,chars=2.
CLOT,thp1,oplot=-1,line=2 & xyouts,.6,.25,'dash: 1 sol later',/norm,chars=2.
j1=pari[11]>0 & j2=pari[12]<(nlat-1) ; subset of latitudes
xa=total(abs(th1[j1:j2,nfy]))/13 ; j1:j2 is lats to avoid polar 
ya=total(abs(thm1[j1:j2,nfy]))/13 ; MAR for the last year, Skip-everySol
yb=total(abs(thp1[j1:j2,nfy]))/13
q=string('@68: MAR over lats ',slat[[j1,j2]],' -,0,+ :',ya,xa,yb,form='(4a,3f6.3)')
print,q
if luf then printf,luf,q
end 

682: begin & yt=tt0[*,iii]; Difference at each year end FOLLOW 68
yy=yh-yt ; skip-every-sol  at end of each year
;yy[*,0]=0. ; first not at same season
CLOT,yy,qq,yran=parr[16:17],locc=parr[20:23],titl=['Latitude index' $
,'Midnight Tsurf: Skip Run - every_sol',kite+stem]
end

 69: begin ; Polar slope test 2017sep28
path='/home/hkieffer/krc/tes/out/' & stem='pps'
print,'do 252   then 69 '
size=size(ttt)
if size[0] ne 5 then begin 
  kons=[252,69] & kon=123 & goto,dokon
endif
hhot=round(.54*nhour) ; H=13, hot time of day
lhot=12 ; warm season, just after solstice
lqx=22  ; fall equinox
sss=strarr(nlat,ncase) ; [nlat, nseas
for i=0,nlat-1 do sss[i,*]=slat[i]
scas=['0','10','20','30','40'] ; firm code slopes
for i=0,ncase-1 do sss[*,i]=sss[*,i]+scas[i]
tdd=reform(ttt[*,0,*,lhot,*],nhour,nlat*ncase);  diurnal Ts for all lat and case
titl=['hour index','Tsurf','legend is lat.slope to north']
locc=[.38,.95,-.03,.06] ; set legend location
CLOT,tdd,sss,locc=locc,titl=titl,tsiz=[2,2.]
if CALL91(kite,kitel) then stop ; <><><><>stop
tdd=reform(ttt[*,0,*,lqx,*],nhour,nlat*ncase)
titl[2]='Fall equinox Ls=177.6'
CLOT,tdd,sss,locc=locc,titl=titl,tsiz=[2,2.]
if CALL91(kite,kitel) then stop ; <><><><>
yy3=reform(ttt[*,0,*,*,*])
yy4=reform(yy3[*,1,*,3]);  70.30
i=0 & j=20
titl[2]='70.30 through spring and summer, Legend is Ls'
CLOT,yy4[*,i:j],sls[i:j],locc=locc,titl=titl,tsiz=[2,2.]
if CALL91(kite,kitel) then stop ; <><><><>
yy3=transpose(yy3,[0,1,3,2])
yy3=reform(yy3,nhour,nlat*ncase,nsea,/overw)
i=10 & j=20
titl[2]='Movie over Ls range'+ST0(sls[[i,j]])
CLOT,yy3[*,*,i:j],titl=titl
end


; 7_7_7_7_7_7_7_7_7_7_7_7_7_7_7_7_ Test one-point mode
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
t12=reform(t12,nspy,nfy*ncase,nlat,/over) ; [season,year*case,lat]
t12r=   fltarr(nspy,nfy*ncase,nlat) ; [season,year*case,lat]
i1=nfy*i+nfy-1                      ; index of last year of case i
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
CLOT,t12r,caset,locc=parr[12:15],yran=[ymin,ymax] $
,titl=['Season index:  '+strtrim(nspy,2) $
+' /year' ,'Ts relative to last year for first case',kite+stem+':  hour='+sour[k] $
+' Lat='+slat[j]]
plots, !x.crange,[0.,0.],line=1    
goto,again72     
end

731: begin ; generate pressure input series
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

732: begin & kmax=pari[5] ; Generate a large grid .one file
 ofile=parf[10]+parf[11]
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

74: begin & fam2=parf[14]+parf[15]+parf[16] ; read TUN file REQ 252
fff=READTUN(fam2,pari[15],fid,mruca) & help,fff ;(item,hour,lat,season[,case])
siz=size(fff)
if (siz[2] ne sizt[1]) or (siz[4] ne sizt[4]) then goto,halt ; hours,seasons must match
nif=siz[1] ; expected: tun=1, >20 , Tun=2, 5
print,'id:',fid
fid=[itemt,fid] & nff=n_elements(fid)
if pari[15] eq 2 and nif ne 5 then goto,halt
end

741: begin ; CLOT TUN and ttt for one lat,seas,case  REQ 74 
   jc=(pari[7]>0)<(ncase-1)     ; case index
print,'Change @12, Index for lat and case=',jlat,jc  
ff=fltarr(nhour,nff)
xa=0. & read,xa,prompt='Desired Ls > '
q=min(abs(lsv-xa),j) & print, 'Fetched Ls=',lsv[j]
qq='Ls = '+string(lsv[j], form='(f5.1)')
k=where(mruca[1,*] eq jc) & k=k[0]
if k lt 0 then goto,halt ; case not in TUN file
ff[*,0:4]=ttt[*,*,jlat,j,jc] ; ttt is [hour,item,lat.season,case]
ff[*,5:*]=transpose(reform(fff[*,*,jlat,j,jc]))
CLOT,ff,String(indgen(nff),form='(i2)')+' '+fid,xx=xxh , locc=1,titl=['Hour' $
,'Temperature and Flux [W/m^2]',kite+fam2+' '+sdat[jc]]
end

742: CLOT,transpose(ff[*,5:*]) $ ; CLOT layer T. REQ 741 and TUN=1
,string(xxh,form='(f4.1)'),locc=[.7,.93,-.025,.06] $
,titl=['Layer index, KRC-2','Temperature at '+qq,kite+fam2+' '+sdat[jc]]

743: begin & yy=2+indgen(nif); Plot T(z,H) REQ 741
if pari[15] ne 1 or nif lt 7 then goto,halt ; do not have layers 
; fff is Tk [item:layer,hour,lat,season[,case]) ; ff is [hour,item:layer]
xa=min(ff[*,5:*],max=xb)
plot,ff[0,5:*],yy, xran=[xa,xb],xtit='Soil kinetic temperature', Ytit='KRC layer'
for i=6,5+nif-1 do oplot,ff[i,5:*],yy
end

744: begin   ; Seasonal soil temps REQ 741
qq='Hour='+string(xxh[ihour], form='(f4.1)')
nsp=pari[1]  ; Number of seasons to plot
ii=(nsea/nsp)*indgen(nsp)      ; subset nsp seasons
print,'Index of Seasons plotted:',ii
gg=reform(fff[*,ihour,jlat,*,jc],/over) ; [ layer, season]
gg=gg[*,ii] ; spaced seasons
CLOT,gg,string(lsv[ii],form='(f5.1)'),locc=[.7,.93,-.025,.06] $
,titl=['Layer index, KRC-2','Temperature at '+qq ,kite+fam2+' '+sdat[jc]]
end

752: begin & dir='~/krc/tes/fort.' ; Check fort79,88
f1=['79','88']
mrow=pari[14] & ncol=8
sk=READTXTCOL(dir+f1[0],nskip=0,ncol=ncol, mrow=mrow)
fk=float(sk)
;chart,fk
sk=READTXTCOL(dir+f1[1],nskip=0,ncol=ncol, mrow=mrow)
f8=float(sk)
xx=fltarr(ncol)
for i=0,ncol-1 do xx[i]=max(fk[*,i])
qq=['JJ','COSI','COLL','QA=ADGR','QI=ASOL','DIRECT','DIFFUSE','BOUNCE'] $
+' '+ST0(xx,/nojoin)
CHART,f8-fk,xtit='jj * case',parti=qq
PLOTSECT,caset,0.1
end

753: begin &  j=nlat-1 ; Check downvis, REQ 26
dvc=reform(ttt[*,3,j,*,*]) ;0:1]) ; std and no-atm cases, 0 lat, all seasons
dvh=reform(tth[*,3,j,*,*]) ;0:1])
qq=dvh-dvc ; & i=n_elements(qq) & if i gt 200000 then psym=1 else psym=0
plot,qq,xtit='Hours * seasons * cases', ytit='DownVis: ttt-tth', psym=1  ; no-atm are the same
PLOTSECT,caset,0.1
end

755: begin ; Process LQ3 from krc.prt ; REQ 252
sk=READTXTCOL('~/krc/tes/LQ3',nskip=-1,ncol=7)
siz=size(sk) & mrow=siz[1]
ii=where(sk[*,3] eq '1',j) & j4=mrow/j ; Nlats
ff=double(sk[*,4:6])
CHART,ff,parti=['TATMAVE','PRES','OPACITY'],xtit='lat * case * run',dlin=1
;ii=where(ff[*,2] ge .0001)      ; Atm cases
;ya=mean_std2(ff[ii,*],std=yb,/one)
; ii=where(ff[*,2] lt .0001)      ; NoAtm cases
nrun=j/ncase
help,sk,mrow,j,j4,ncase,nrun
f4=reform(ff,j4,ncase,nrun,3) ; [lat,case,run, item]
f2=reform(f4[j4-1,*,*,*])     ; [case,run, item] last lat only
f3=transpose(f2,[1,0,2])  ; [run, case, item]
print,f3 
f2=reform(f3,nrun*ncase,3)
ii=where(f2[*,2]  ge .0001,i)      ; Atm cases
xa=total(f2[ii,2])/i
ii=where(f2[*,2]  lt .0001,i)      ; Atm cases
f2[ii,2]=xa
PAUSE,-1
CHART,f2,parti=['TATMAVE','PRES','OPACITY'],xtit='run * case ',dlin=1
PLOTSECT,caset,0.1
end

756: begin  & ik=6  ; Plot diurnal diff REQ 75
j=nsea/ik
 ii=j*indgen(ik)
 dd=qq[*,0,2,ii,*] ; Tsur at last Lat: [hour, n*seas, case]
 q4=reform(dd,nhour,ik*ncase)
q=strarr(ik,ncase)
for i=0,ik-1 do q[i,*]=sls[ii[i]]
for i=0,ncase-1 do q[*,i]=q[*,i]+(' '+caset[i])
q[*,0]=sls[ii]
CLOT,q4,q,locc=[.6,.93,-.02,.06],titl=['hour index','Delta at several seasons' $
,kite+ifile+' - '+ifh]
print, 'Value at start of day'
for i=0,ncase-1 do print,i,q4[0,ik*i ],caset[i], format='(i3,f9.5,2x,a)'
end

;757: begin ; Compare everysol with skip.
;print,'2do'
;end


760: begin ; Robins 321:342 verify, REQ tth=my 343 run and ttt=321
; parf[[0,1,5,6]]=['~/krc/tes/out/','thin9','~/krc/robin/','rlf321']
ii=[3,6,9,12,15] & print,slat[ii]
t5t=ttt[*,*,ii,*,0] ; 5 lats in common
t5h=tth[*,*,*,*,2] ; 343 run closest to 321
d5t=t5t-t5h   &  dd3=d5t[*,*,1:3,*]; equiv 341 run & +/- 30 lat range
print,htit
for j=0,niti-1 do begin & aa=HSTATS(d5t[*,j,*,*],hkode)  & $
   print,itemt[j],aa,form=hfmt & endfor
for j=0,niti-1 do begin & aa=HSTATS(dd3[*,j,*,*],hkode)  & $
   print,itemt[j],aa,form=hfmt & endfor
tq=reform(tth[*,0,2,39,*])
titl=['Hour','Delta T surface, K.','Re: Robins 321:342 verify']
CLOT,tq,sdat+'  '+caset,locc=[.05,.95,-.03,.03],titl=titl
  if CALL91(ptitl,'R343') then stop 
tz=AVALG(tq,tq[*,8],'-')
qq=caset+' -case 8'
CLOT,tz,qq,locc=[.05,.95,-.03,.03],titl=titl 
  if CALL91(ptitl,'del343') then stop 
tx=reform(ttt[*,0,9,39,*]) ; v321 Tsur equator
tz[*,0]=tq[*,0]-tq[*,1]  & qq[0]='N2=1538- 1536' ; 1536-1538
tz[*,1]=tq[*,2]-tx & qq[1]='V321-V343equiv' ; version 343 - version 321 same thicknesses
;tz[*,2]=0. & qq[2]='zero'
tz[*,2]=tz[*,0]+tz[*,1] & qq[2]='Sum of 0 and 1'
qq=sti2[0:8]+': '+qq
CLOT,tz,qq,locc=[.05,.95,-.03,.03],titl=titl 
tt5=ttt[*,*,[3,6,9,12,15],*,0] ; 5 lats in common
end

761: begin & print,htit ; Subset of latitudes
j1=pari[11]>0 & j2=pari[12]<(nlat-1) ; subset of latitudes
d5t=dtt[*,*,j1:j2,*]
for j=0,niti-1 do begin & aa=HSTATS(d5t[*,j,*,*],hkode)  & $
   print,itemt[j],aa,form=hfmt & endfor
 print,kite,' Lat range used:', slat[[j1,j2]]
end

; ----------------------- KofT -----------------------
770: begin & kons=[252,22,771,772,773,774] ; Compute and plot KofT delta
parf[1:2]=['kot5',''] & end ; optionally 772 and 773

771: begin  ; firmode cases and colors
if n_elements(cased) ne 10 then goto,halt
txt=['noTvar','none','k','k_and_C','C']
cased=strarr(2,5) & cased[0,*]=txt & cased[1,*]=txt+'_IC=7'
cased=reform(cased,2*5,/over)
;kink=[90,170,254, 130,185,40]
k869=[1,2,3,8,4,5]
end

772: qq=KOFTDEL(reform(ttt[*,0,*,*,*]),cased,uuu[*,0,0],vvv[*,1,0],cato,plo=0) ; KOFTDEL
;qq is (hour,[mean,std],case,mater,lat]   lats are [0,-40]

773: begin & siz=size(qq) ; Plot KofTdelt, REQ 772
nh=siz[1] & nc=siz[3]
xx=findgen(nh)*(24./float(nh)) & yran=[-1.5,1.5]
locc=[.45,.93,-.03,.08]
txt=['k only','k and C','C only']
txt=[txt,txt+' 2-mater.']
ii=[1,2,3] & nc=3                      ; skip the null case 
ya=reform(qq[*,0,ii,*,0],nh,nc*2); mean [hour,case*mater] , first lat only
yb=reform(qq[*,1,ii,*,0],nh,nc*2); stdDev[hour,case*mater]
CLOT,ya,txt,xx=xx,locc=locc,yrange=yran $
,titl=['Hour','Delta T from no T-dependance',kite+'Tests of T-dependence']
  if CALL91(ptitl,'kv773') then stop 
CLOT,ya+yb,txt,xx=xx,locc=locc,yrange=yran,oplot=-1 ;+ M+StDev REQ 773
;CLOT,ya-yb,txt,xx=xx,locc=locc,yrange=yran,oplot=-1 ;+ M-StDev REQ 773
end

774: begin & xa=0. ; Plot of number of layers required
read,xa,prompt='Total depth scaled > '
yy=NUMGEOMLAY(xa)
end

775: begin; Compare to Simple 
tks=reform(ttt[*,0,jeq,*,0]) ;  Tsur close to equator
tsimp=KRCSIMPLE(paru,ttm,tbot,asol, lab=lab)
help,tsimp,ttm,tbot,asol
siz=size(tsimp) & nsh=siz[1] & n5=siz[2] ; Number of "hours" and seasons
help,tks,tsimp,ttm,tbot
ddel=tks[*,nsea-1]-tsimp[*,n5-1]
dowv=reform(ttt[*,3,2,0,0])
i=n_elements(asol)/nsh
asol=reform(asol,i,48) & bsol=reform(asol[i-1,*]) ; at same points as KRC
plot,dowv,xtit='Hour index.   line-KRC  +=simple',ytit='DownVis'
oplot,bsol,psym=1 
  if CALL91(ptitl,'kv777') then stop 
xa=min(dowv-bsol,max=xb) 
print,'Range of KRC-KRCSim DownVIs',xa,xb,' noon=',max(dowv)
ii=where (bsol gt 0.) & qq=dowv[ii]/bsol[ii]
plot,ii,qq-1.,xtit='Hour index of '+strtrim(nsh,2) $
,ytit='(KRC DOWNVIS / KRCsimple asol)-1'
help,tks,tsimp
i=(nsea<n5)-1 ; fewer seasons
ytit='Surface T, KRC - simple'
plot,tks[*,0:i]-tsimp[*,0:i],xtit='hour * season',ytit=ytit
  if CALL91(ptitl,'kv777b') then stop 
CLOT,tks[*,0:i]-tsimp[*,0:i],sti2[1:i+1],locc=[.15,.95,-.017,.02] $
,titl=['Hour index',ytit,kite] 
end

776: begin & maxr=20 ; Test many runs of KRCSIMPLE
tsurs=QKRCSIMP(maxr,ppp)
end

777: begin  ; Read SIMPLE save file
fil1='' & read,fil1,prompt='SIMPLE save file name > '
fff=file_search('/work1/krc/test/simple'+fil1+'.sav',count=j)
if j ne 1 then goto,halt
restore,file=fff[0],/verb
help,sss,ppp
  siz=size(sss) & kk=siz[2]-1
;CLOT,sss,sti2[0:kk],locc=1,titl=['hour index','Tsurf','QKRCSIMP']
;  if CALL91(ptitl,'779a') then stop 
kiss=KRCSIMPLOT(sss,ppp,verb=1,rrr=tref,lref=lref)
kiss=kiss>0 ; in case tref used and kiss still -
end

778: begin  & tref=sss[*,kiss] ; set tref to SIMPLE
lref='SIMPLE '+fil1+':'+strtrim(kiss,2) & end

779: begin &  BIN5,'Rb','spenc',head,ssurf ; compare Spencer model
tjs=reform(ssurf[0,*]) & tjs=shift(tjs,-1)
ya=min([tref,tjs],max=yb)
plot,tref,yran=[ya,yb],xtit='Line=KRC. +=Spencer',ytit='Tsurf',titl=kite+lref+' '+head
oplot,tjs,psym=1
qq=total(abs(tjs-tref))/48. & print,'MAR=',qq
end

780: begin; select a KRC case as the reference
tss=reform(ttt[*,0,jeq,nsea-1,*]) ; Tsur close to equator
read,irr,prompt=' ref model index > '
irr=(irr>0)<(ncase-1) 
tref=tss[*,irr] & lref='KCR '+stem+':'+strtrim(irr,2)
end

781: begin ; Try for KRC run times in the .prt file  REQ 252, 212,228
fff=file_search('~/krc/tes/'+stem+'.prt',count=j)
if j ne 1 then fff=file_search('~/krc/tes/krc.prt',count=j)
fok= j eq 1                     ; have a good file
if not fok then message,'did not find .prt',/con else begin 
  spawn,'fgrep  DTIME '+fff[0]+' > ~/krc/tes/time'
  qqq=READTXTCOL('~/krc/tes/time',nskip=0,ncol=8)
  krct=float(qqq[*,7])
  ii=where(krct gt .001,j)      ; valid cases
  if j ne ncase then fok=0B     ; file had wrong number of good times
endelse
if fok then krct=krct[ii] else begin ; match
  message,'KRC run times not available',/con ; some mis-match
  krct=0.77+findgen(ncase)
endelse                         ;--------------- done getting run times
print,' i',kiname,'Deep','Sconv','secs',form='(a2,19a7)'
fmt='(i2,'+k1fmt+'f7.2,f7.2,f7.2)'
for i=0,ncase-1 do print,i,lout[*,i],deep[i],sconvg[i],krct[i],form=fmt
;CLOT,tss & pause,-1  
nlac=fix(lout[3,*])             ; NLAY for each case
end

782: begin  & xran=[pari[24],nhour-1] ; Tdel; Accuracy vrs time  REQ 781
tss=reform(ttt[*,0,jlat,nsea-1,*]) ; Tsur in last year [hour,case]
read,irr,prompt='Ref model index -=keep > '
if irr ge 0 then begin & tref=tss[*,irr] & lref='KRC '+stem+':'+strtrim(irr,2) & end
ttd=AVALG(tss,tref,'-')
mar=total(abs(ttd),1)/nhour  ; MAR of delta T
xa=min(ttd,max=xb) & print,'Tdel range:',xa,xb
CLOT,ttd,sdat,loc=[.06,.94,-.02,.03],xran=xran,titl=['hour index' $
   ,'KRC Tsurf at '+slat[jlat]+' at last season - reference',kite+'KRC  '+stem+' rel to '+lref]
plots,[0,nhour-1],[0.,0],line=1 
  if CALL91(ptitl,'kv782') then stop 
; simpt=[165.52, 83.94,333.64,672.40] ; SIMPLE run X
;print,' i',kiname,'    Deep  Sconv   secs    MAR   Tdel'
print,' i',kiname,'Deep','Sconv','secs','MAR','Tdel',form='(a2,19a7)'
;print,' i  RLAY   FLAY    CVG NLAY  Ntime    Deep  Sconv   secs    MAR   Tdel'
fmt='(i2,'+k1fmt+'f7.2,f7.2,f7.3,f7.3,f7.3)'
for i=0,ncase-1 do print,i,lout[*,i],deep[i],sconvg[i],krct[i],mar[i],ttd[14,i],form=fmt
ii=NOWHERE(ncase,irr) ; all cases except the reference
xa=min(krct[ii],max=xb) & xc=xb-xa & xran=[xa,xb]*[.99,1.05] ; +xc*[-.03,+.05]
ya=min(mar[ii], max=yb) & yc=yb-ya & yran=[ya,yb]*[.92,1.09] ; =[ya,yb]+yc*[-.05,+.05]
yran[0]=yran[0]>1.e-3 & xran[0]=xran[0]>0.005
plot,krct[ii],mar[ii],psym=3,xran=xran,yran=yran,/ylog,/xlog $
  ,xtit='run time, seconds',ytit='MAR compared to high-accuracy at '+slat[jlat] $
  ,titl=kite+'KRC  '+stem+' rel to '+lref
plots,[.1,10.],[1.,.01], line=1
; plots,[.1,10.],[.2,.002], line=5
for i=0,ncase-2 do xyouts,krct[ii[i]], mar[ii[i]],stid[ii[i]],/data
if 2 gt 3 then begin 
  ii=[0,1,5,8,11,14,17,20] ; cases to plot
  CLOT,ttd[*,ii],sdat[ii],loc=[.06,.94,-.02,.02],titl=['hour index' $
  ,'KRC Tsurf- SIMPLE',kite+'KRC  '+stem+' rel to SIMPLE '+fil1+':'+strtrim(i,2)]
  plots,[0,nhour-1],[0.,0],line=1
endif
end

783: begin & clr=!binc[254]; add second file to time/MAR plot REQ 781 then 11 252 22
tss=reform(ttt[*,0,jeq,nsea-1,*]) ; Tsur close to equator
fff=file_search('~/krc/tes/'+stem+'.prt',count=j)
if j ne 1 then goto, halt
spawn,'fgrep  DTIME '+fff[0]+' > ~/krc/tes/time'
qqq=READTXTCOL('~/krc/tes/time',nskip=0,ncol=8)
krct=float(qqq[*,7])
ii=where(krct gt .001,j)        ; valid cases
krct=krct[ii]
ttd=AVALG(tss,tref,'-')
mar=total(abs(ttd),1)/nhour  ; MAR of delta T
oplot,krct,mar,psym=4,color=clr ; red
for i=0,ncase-1 do xyouts,krct[i]*1.02, mar[i],stid[i],/data,color=clr
end

784: begin & print,kiname[cid]  ; Calc heatflow REQ 252
i1=pari[9] ; first layer to plot
q=reform(ddd[*,*,*,nsea-nfy*leny:*,*],nlay,2,nlat,leny,nfy,ncase)
qq=total(q,2)/2. ; avg. Tmin and Tmax [layer,lat,season,year,case]
qa=total(qq,3)/leny       ; average over season [layer,lat,year,case] [0 is KRC 2
ii= where(qa le 10.,j)    ; layers that were not defined, thus still zero
yy=qa-shift(qa,[1,0,0,0]) ; T increase with depth. first is wrap. [0] is KRC 1.5
df=yy                     ; make HF array the correct type and size
blay=zzz[*,1]             ; layer thickness in m.  [0] is KRC 1
; help,qq,qa,yy,blay,lout,zzz
; yy [lay,lat,year,case] Need to process by case with proper cond.
; HOWEVER  Blay also changes with IC2.. scales as sqrt(cond)
; MUST assume that zzz= KRCLAYER(kcom.fd  uses homogenous case
for k=0,ncase-1 do begin  
  xx=replicate(kcond,nlay) & i=ic2[k]  ; change layer
  if i gt 2 and i lt nlay then xx[i-1:*]=kcom.fd[3]; lower mat. cond
  codd=xx/blay                  ; k/thickness   [0] is KRC 1
; There may be residual jump neartIC2 unless deal with 1/2 layers there
  dodd=(codd+shift(codd,-1))/2. ; [0] is KRC 1.5 last is wrap
  df[*,*,*,k] = AVALG(reform(yy[*,*,*,k]),dodd,'*') 
;       HF.  [0] is KRC 1.5 [layer,lat,year,case], both ends  wrap
endfor 
if j gt 0 then df[ii]=!VALUES.F_NAN ; ensure they don't affect plot range
;dy=(yy+shift(yy,1))/2.          ; [0] is KRC 1   first 2 are wrap
;hf=AVALG(dy,codd,'*')      ; HF.  [0] is KRC 1   [layer,lat,year,case]
i=(nlay-i1-1)*nfy ; number for first dimension of zz below
zz=reform(df[i1:nlay-2,jlat,*,*],i,ncase) ;[lay*year,case]
; help,codd,df,dy,hf,zz
;CLOT,zz,stid[0:ncase-1]+string(zz[i-1,*],form='(f8.4)'),locc=1 $
CLOT,zz,sdat,locc=[.6,.7,-.03,.06],tsiz=[2.,2.],titl=['KRC layer-'+stid[i1+1]+'.5 * year','Heatflow for latitude '+slat[jlat],kite+stem]
PLOTSECT,stid[ky0+1:ky0+nfy],.16 ; convert to 1-based year
plots,[0,i],[1.,1.]*mxhf,line=1
if 2 gt 3 then begin                     ; ONLY by hand 
  za=reform(total(ddd[*,*,jlat,*,*],2)/2.) ; tave[layer,season*year,case]
  zb=za-shift(za,[1,0,0]) ; delta T with layer [layer,seas*year,case] first=wrap
for j=1,6 do begin & CLOT,reform(zb[nlay-j,*,*]),sdat,locc=1 & PAUSE,-1 & endfor
  zb=reform(zb,nlay,leny,nfy,ncase,/overw)
  zc=total(zb,2)/leny           ; average over season [layer,year,case]
  zd=reform(zc[i1:nlay-1,*,*],(nlay-i1)*nfy,ncase); [lay*year,case]
  CLOT,zd,sdat,locc=1,titl=['KRC layer-'+stid[i1+1]+'.5 * year' $
      ,'dT/dLayer for latitude '+slat[jlat],kite+stem]
  j=nlay-i1                     ; number of layers to plot
  y3=reform(yy[*,jlat,*,*]) & yd=y3[i1:nlay-1,*,*]
; zd is identical to yd
  ye=yd                         ; make array the proper size and type
  For k=0,ncase-1 do begin & fac=rlayy[k]^indgen(j) & ye[*,*,k]=AVALG(yd[*,*,k],fac,'/',dim=1) & endfor
  CLOT,reform(yd,j*nfy,ncase,/overw) & PAUSE,-1
  CLOT,yd,yran=[-.1,.1] & PAUSE,-1
  titl=['KRC layer-'+stid[i1+1]+'.5 * year','dT/dz, scaled. May vary between' $
        +' cases',kite+stem+'  Latitude='+slat[jlat]]
  CLOT,reform(ye,j*nfy,ncase,/overw),caset,locc=1,titl=titl & PAUSE,-1
  CLOT,ye,caset,locc=1,titl=titl,yran=[-.1,.1] & PAUSE,-1
  ndj4=round(reform(ggg[0,jlat,*,*])) ; NDJ4: convergence days [season*year,case]
  ndj4=reform(ndj4,leny,nfy,ncase,/over) ; [season,year,case]
  nj4= reform(ndj4[*,nfy-1,*])           ; last year, [season,case]
endif
end

785: begin ; Compare delta T with expectation  REQ 784
i1=pari[9] & i2=pari[10] ; range of layers to treat
GETPINTS,labk,park,-1,ncase-1
ii=where(park lt 0) & nk=ii[0] ; number of comparisons
ii=park[0:nk-1] & ih=park[nk+1:*] ; indices of reference and 'hot" cases
if n_elements(ih) ne nk then goto, halt
qq=stid[ih]+'-'+stid[ii]+': '+caset[ih]+' <-> '+caset[ii]
emis=kcom.fd[1]            ; emissivity
tsa=total(ttt[*,0,jlat,*,*],1)/nhour ; diurnal mean surface T
tsa=reform(tsa,/over)             ;  [season,case]
jj=(ks0-1)+(indgen(nfy)+1)*leny ; last season of each year
tsa=tsa[jj,*]
deltsa=tsa[*,ih]-tsa[*,ii] ; do the case subtraction
CLOT,deltsa,qq,locc=1,titl=['year on disk','Delta diurnal average Tsur',kite+stem]
ya=mxhf/(4.*emis*sigsb*tsa[nfy-1,0]^3) ; for reference case ??WRONG
plots,[0,nfy-1],[1.,1.]*ya,line=1
if CALL91(ptitl,'kv785s') then stop 
qc=reform(qa[*,jlat,*,*]) ; remove latitude index [layer,year,case]
qc=qc[*,*,ih]-qc[*,*,ii] ; hot case - ref case [layer,year,casePair]
CLOT,reform(qc[i1:i2,*,*],(i2-i1+1)*nfy,nk),qq,locc=1 $
,titl=['layers'+stid[i1]+':'+stid[12]+' * year' $
,'Delta T_ave between cases for latitude '+slat[jlat],kite+stem]
PLOTSECT,stid[ky0+1:ky0+nfy],.3  ; KRC years
delthf=zzz[1:*,3]*(mxhf/kcond) ; predicted delta T due to heatflow vrs depth
; using conductivity at top of case 0
oplot,delthf[i1:i2],psym=1      ; plot over the first yesr
oplot,(nfy-1)*(i2-i1+1)+indgen(i2-i1),delthf[i1:i2],psym=1 ; and the last year
if CALL91(ptitl,'kv785a') then stop 
CLOT,reform(qc[i1:i2,nfy-1,*]),qq,locc=1 $
,titl=['layers'+string([i1,12],form='(2i3)')+' on last year' $
,'T_ave: odd case - even case',kite+stem]
oplot,delthf[i1:i2],psym=1    ; plot predoction over the last year
if CALL91(ptitl,'kv785') then stop 
CLOT,reform(qc[*,*,0])>.001,stid[ky0+1:ky0+nfy],locc=1,/ylog,titl=['layers' $
,'T_ave: with - without heat-flow','Successive years '+ kite+stem]
oplot,delthf[*],psym=-1
;if CALL91(ptitl,'kv785b') then stop 
;yb=AVALG(reform(qc[i1:i2,*,*]),delthf[i1:i2],'-')
;ya=min(
;CLOT,yb[*,*,0],stid,locc=1
;if nk gt 1 then for i=1,nk-1 do CLOT,yb[*,*,i], oplot=-9
end

786: begin & k=pari[20]  ; Detailed heatflow for one case thru last year 
print,caset[k],sdat[k]
i=ic2[k]         ; KRC first layer of lower material
j=kn1[k]         ; number of physical layers
qq=reform(ddd[*,*,jlat,nsea-leny:nsea-1,k]) 
tave=total(qq,2)/2. ; Tave [layer,season] last year, one case
CLOT,tave,stid[1:nlay],locc=[.4,.93,-.018,.03],titl=['layer','Tave',kite+stem] 
if CALL91(ptitl,'kv7846a') then stop 
; Code follows equations in dv3.tex
blm=zzz[1:*,1] ; layer thickness in m assuming upper material
xx=replicate(kcond,j)             ; each physical layer
if i gt 2 and i lt nlay then begin
  xx[i-1:*]=kcom.fd[3]
  blm[i-1:*]=blm[i-1:*]*sqrt(kcom.fd[3]/kcond) ; for lower material 
endif
help,tave,blm,delt,xx           ; see V33UG section on heatflow relations
fac=shift(xx,-1)*blm/(xx*shift(blm,-1)) ; k_+ B_i / k_i * B_+ ; H5. Last is wrap
tp=AVALG(tave,1./(1.+fac),'*')+AVALG(shift(tave,[-1,0]),fac/(1.+fac),'*')
tdel= shift(tave,[-1,0])-tave ; T_+-T_i [layer,season] 
zm=zzz[1:*,3]                 ; physical layer center depth, m
dzm=shift(zm,-1)-zm           ; layer separation, m; last is wrap
gradt=AVALG(tdel,dzm,'/')     ; T gradient: delta T / delta z
hfc=AVALG(tdel,xx/dzm,'*')    ; rough HF, Last is wrap
yy=xx*2.*fac/(blm*(1.+fac))      ; first 2 factors in Eq. H6 
hfa=AVALG(tdel,yy,'*')        ; HF following last relevant equation in V33UG
hfb=AVALG(tp-tave,2.*xx/blm,'*') ; HF using T_p; tave==T_i
i1=pari[9] & i2=pari[10]        ; range of layers to plot
CLOT,hfa[i1:i2,*],stid,locc=[.5,.93,-.016,.03],titl=['layer starting at KRC 2+'+strtrim(i1,2),'Heat flow, W/m2. Latitude:'+slat[jlat],kite+stem+' Season in last year. 0Case '+stid[k]]
q=(hflow[k]-hfb[*,leny-2])*((leny-1.)/leny)/(hfb[*,leny-2]-hfb[*,0])
print,'years to go',q[i1:i2], form='(a,99f6.1)'
end

788: begin  ; Clot T min, Tmax vrs depth
k1=pari[8]<(ncase-1) & k2=pari[7]<(ncase-1)
if k1 gt k2 then goto,halt
xx=reform(ddd[*,*,jlat,nsea-leny:nsea-1,k1:k2]);  [layer,min/max,season[,case]
zz=reform(xx[0,*,*,*])
ii=where(zzz[*,2] ge 1.) & i=ii[0]
yy=reform(total(xx[i,*,*,0],2)/2.) ; Tave near D=1 [season]
q=min(yy,j1)  & q=max(yy,j2)   ; cold and hot season
k=(k2-k1+1) ; number of cases retained
qq=reform(xx[*,*,[j1,j2],*],nlay,4*k) ;[layer, min/max,cold/hot [,case]]
q=strarr(4,k) & q[0,*]='Min:cold ' & q[1,*]='Max:cold '  
q[2,*]='Min:Hot  ' & q[3,*]='Max:Hot  ' 
for i=0,k-1 do q[*,i]= q[*,i]+ stid[k1+i]+' '+ caset[k1+i]
CLOT,qq,reform(q,4*k), locc=[.2,.93,-.02,.03],titl=['Physical layer index' $
,'Minimum and maximum temperature at Lat='+slat[jlat],kite+stem+' ls='+sls[j1]+' '+sls[j2]]
end

789: begin     ; CLOT heatflow for one lat  REQ 784
jyr=0>pari[19] <(nfy-1) ; which year
; df is [layer,latitude,year,case]
CLOT,reform(df[11:nlay-2,jlat,jyr,*],nlay-12,ncase),stid,locc=1 $
,titl=['KRC layer-13.5','Heatflow for latitude '+slat[jlat],kite+stem+' disk year index'+strtrim(jyr,2)]
end

790: Begin  ; CLOT delta heatflow   REQ 785
k1=pari[7]<(ncase-1) & k2=pari[8]<(ncase-1) ; - and + case
print,caset[k2]+'   minus' & print,caset[k1]
cdf=df[*,*,jyr,k2]-df[*,*,jyr,k1]; change for no atm. [0] is KRC 1.5
chf=hf[*,*,jyr,k2]-hf[*,*,jyr,k1]; change for no atm [0] is KRC 1 
; help,ddd,q,qq,qa,yy,blay,xx,ic2,codd,dy,hf,zz
CLOT,cdf[1:nlay-2,*], slat,locc=1,titl=['KRC layer-2.5','Heatflow: case ' $
+strtrim(k2,2)+' minus '+strtrim(k1,2),kite+stem+' disk year '+stid[jyr] ]
CLOT,chf[2:*,*],oplot=-9,ksym=-4 ; KRC layer - 3
end
;    if CALL91(ptitl,'kv784') then stop 

else: ire9=KON99(ptitl,prior,hold,m9,v9,kon,kons,kitel, dd,bbb,log,avv)
endcase
goto, ask

halt:  print,'SOME ERROR CONDITION at kon=',kon,'.  Any key to Go'
i=get_kbrd(1) & goto,sureask
end
