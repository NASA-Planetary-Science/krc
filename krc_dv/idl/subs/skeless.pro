;_Titl skeless   IDL skeleton, uses KON99 or 91. No looping and hints
;_Calls  Utility: KON99  GETPAN  GETPSN  ST0
; Check KON99 for actions already assigned
common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,scex1
;_Hist 2015jul25 HK RTemove loops and hints from KON99
;_End

ptitl='skeless' ;<<<
;solib=getenv('SOLIB')           ; location of shared object library
!except=2                       ; report all math errors
ON_IOERROR, BAD                 ; if I/O error occurs

;vvvvvvvvvvvv Begin modifiable parameters vvvvvvvvv

labf=['PROJDAT top DIR for Large Data','PROJSRC " for source and data']; Strings
parf=['replaced','replaced']

labi=['spare','---'] ; integer Params
pari=[-7,-7] 

labr=['spare'] ; float parameters
parr=[ 7.7] 

;^^^^^^^^^^^^^^^^^^^^^^^ constants and inputs
;m9=[0,2,1, 99,0, -1,-1,0, 10,3,5,17,0, 80,1,0, 4,-777,2,0,0] ; | defaults, 91,92
;v9=[-20.,150.,10.,2., -77.,-77.,-77., -7777., 1,1]           ; | retain changes

prior=['1','1']                 ; impossible values, to initiate MAKE99
kite=' ' & i=0                  ; type definitions

;print,'[ Enter 131 to start with lucent.inp as control file ]'
;print,'  Enter 123 for auto-sequence'

kons=[850,851,861] ; sequence for initiate colors. May augment
kon=123 & goto,dokon ; do auto-initiation

;===============================================================================
ask: ;------------ top of action loop ------------ interactive parameter change
konl=kon
if lkon then begin                    ; auto-sequence
  kkon=kkon+1                         ; increment to next item
  kon=kons(kkon)                      ; get action from list
  print,'Doing -------------->',kon   ; Notice 
  if kkon eq lastkon then lkon=0B     ; turn off further auto-processing
endif else begin
  sureask: lkon=0B               ; forced request for action
  READ, kon, prompt=ptitl+' Enter selection: 99=help 0=stop 123=auto> '
endelse
dokon: kitel=kite               ; remember prior action to use in subtitle
kite=ptitl+'@'+strtrim(kon,2)   ; follows date in subtitle      fils[2]+' '+
nkc=kcc[2] & nkl=kcc[3]         ; in case SETCOLOR was called
case kon of ;...................................................................
; DO NOT USE those defined in this skeleton or in KON99:

-2: return                      ;<
0: stop     ;- 

;;; 1x == Modify values; Guides; commitment ....................................
11: GETPSN,'File names',parf,lab=labf,/align ;- modify any of a set of strings
12: GETPAN,'Integer values',pari,0,0,labs=labi ;- modify any of a set
16: GETPAN,'Float values',parr,0.,0.,labs=labr ;-  " " , no limits
18: help,dd             ;<<< Guides and help

123: begin & lkon=1b & kkon=-1  ;- Start auto-script 
lastkon=n_elements(kons)-1 & end ; last preset command 

; INSERT with new actions

else: begin 
;  ire9=KON99(ptitl,prior,hold,m9,v9,kon,kons,kitel, dd,bbb,log,avv)
;  if ire9 eq 1 then goto,sureask & if ire9 eq 2 then goto,halt
;^v^v^v^v EITHER the above two OR the one below ^v^v^v^v^v^v^v^v^v^v^v^v
  KON91,ptitl,prior,hold,kon,kons,kitel ;=KON99 < needed by MAKE99
  if kon eq 99 then begin 
    print,'11: files: parf= '
    print,'12: Subset: pari= ',ST0(pari)
    print,'16: Floats: parr= ',ST0(parr)
  endif & end
endcase
goto, ask

bad:  print,'System I/O error'
halt: print,'SOME ERROR CONDITION at kon=',kon,'  Any key to Go.'
i=get_kbrd(1) & goto,sureask

end
