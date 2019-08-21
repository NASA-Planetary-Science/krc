;_Titl  QDEDing2  Test Delta-Eddington FORTRAN code
;_Desc
;_Calls  Fortan DEDING2.  CHART  GETPAN
;_Hist 2002jul13 Hugh Kieffer Adopt from dedmain.pro
;_Lims
;_End
ptitl='qdeding2'
solib=getenv('SOLIB')
prior=['1','1'] 

labr=['Omega low','G  "','Albedo "','CosIncid "','Tau "'$
,'Index of variable','Var. High','Number of vals'] ;5:7
parr=[.8, .8, .0, 0.1, 1.,    3, 1. ,20 ]

blab=['Bond','Coll@Bot','IsoTop','AssymTop','IsoBot','AssymBot' $ ; from deding2
,'BotDown','Trans','AtmHeat','TopUp','TopDownTest'] ; derived
nbb=n_elements(blab)

kite=' ' & lkon=0B & kon=0
kons=[99] ; basic sequence
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
kip=kite+'    '                 ; for possible use by HISTFAST
case kon of ;...................................................................

  0: stop
-1: begin & print,'Any key to GO' & qi=get_kbrd(1) & end ; pause
11: parr=[ .8, .8,  .0,0.1,  1.,3., 1.,20 ]; mu as variable
12: parr=[0.8,0.85,0.8,0.2, .01,4.,100.,20.]; Tau as variable
16: GETPAN,'Floats',parr,0.,0.,lab=labr ;-

123: begin & lastkon=N_ELEMENTS(kons)-1 ;- Auto-sequence
	lkon=1b & kkon=-1 & end		; start auto-script

18: PRINTJCOLS,blab,2,len=15 ; Guide
4: begin ; RUN
k=round(parr[5]) ; index of variable term
vlo=parr[k]                     ; low value of variable
vhi=parr[6]                     ;  " high value
nk=round(parr[7])               ; number to do
if k eq 4 then begin ; do tau as log
    vlo=alog(vlo)
    vhi=alog(vhi)
endif
vdel=(vhi-vlo)/((nk-1)>1)       ; variable delta
vv=vlo+findgen(nk)*vdel         ; abcissa
bbb=fltarr(nk,nbb)               ; to hold outputs and computed
omega=parr[0]
cosa=parr[1]
albsurf=parr[2]
cosi=parr[3]
tau=parr[4]
bond=1. & coll=1. & ri=fltarr(2,2) ; define outputs 
tt=2./3.                        ; two-thirds 
for i=0,nk-1 do begin
    v=vv[i]
    case k of
        0: omega=v
        1: cosa=v
        2: albsurf=v
        3: cosi=v
        4: tau=exp(v)
    endcase
    dum= CALL_EXTERNAL( solib,'deding2',omega,cosa,albsurf,cosi,tau,  $
                        bond,coll,ri)
    if not finite(bond) then bond=.99999
    mu0=cosi                    ; cosine of incidence angle
    topup  =!pi*(ri[0,0]-tt*ri[1,0]) ; diffuse flux up    at top of atm.
    botdown=!pi*(ri[0,1]+tt*ri[1,1]) ; diffuse flux down at bottom of atm.
    trans=botdown+cosi*coll     ; total flux reaching the surface
    atmheat=mu0-topup -(1.-albsurf)*trans ; atmospheric heating
    absorp=atmheat/mu0
    topdown=!pi*(ri[0,0]+tt*ri[1,0]); diffuse flux down at top of atm. =0.
    bbb[i,*]=[bond,coll,reform(ri,4),botdown,trans,absorp,topup,topdown]
;              0     1    2 3 4 5      6       7      8       9    10
;bbb[*,2] =I0(0)  3]=I1(0)  4]=I0(tau)   5]=I1(tau) 
endfor
tit= MAKEKEYVAL(['SingScat','G','AlbSurf','mu_0','Tau'],parr[0:4])
end

6: CHART,bbb,title='DEDING2 test'+tit,parti=blab,csize=1.5 $ ; Chart
,xtit=labr[k]+string(vlo,vhi)

7: if k ge 3 and k le 4 then begin ; JWW Fig 1,2 (variable incl) or 3, var. tau)
if k eq 3 then begin
    xx=vv
    plot,xx,bbb[*,0],yrange=[0.,1.],/nodata $ 
      ,xtit='mu_0  +,trian=AtmAbs sqr=Bond  x,diamond=trans',ytit='t,r,a' $
      ,title=titl ; x=botdown
endif else begin
    xx=exp(vv)
    plot,xx,bbb[*,0],yrange=[0.,1.],/nodata,/xlog $ 
      ,xtit='tau  +,trian=AtmAbs sqr=Bond  x,diamond=trans',ytit='t,r,a' $
      ,title=titl ; x=botdown
endelse
oplot,xx,bbb[*,0],psym=-6 ; square=reflectivity=Bond
oplot,xx,bbb[*,8],psym=-1 ; +=absorption=AbsAtm/cosi
t2=!pi*(bbb[*,4]+tt*bbb[*,5])/cosi + bbb[*,1] ; Diffuse + Direct
oplot,xx,t2,psym=-7 ; x=tranmission
endif else goto,halt

8: GRAPH,8,hard                 ;- new output device
80: GRAPH,0,hard                ;- restart output device
85: SETCOLOR                    ;-
88: SUBTITLE,id=ptitl           ;-
9: GRAPH,9,hard                 ;- close plot device
99: begin ; Action guide.
MAKE99,ptitl,prior,hold         ; make guide to this program
print,'GRAPH: 8=new 80=restart 85=setcolor 88=SubTit 9=plot'
print,'123: auto-sequence is',ST0(kons)
print,'16: Floats: parr=',ST0(parr)
  end
  else: print,'Invalid entry'
endcase
goto, ask

halt:  print,'SOME ERROR CONDITION OCCURED.  Any key to Go'
i=get_kbrd(1) & goto,sureask

end
