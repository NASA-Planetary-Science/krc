PRO kirin, src=src, dat=dat, ver=ver, prn=prn, diss=diss
;_Titl  KIRIN  Krc Initialize Running Idl eNvironement
;_Arguments     All have defaults, some are machine dependant.
; src  in_ string  Full path to directory containing KRC source (small) files
; dat  in_ string  Full path to directory containing KRC binary (large) files
; ver  in_ string  Version of KRC
; prn  in_ string or strarr(2)  Name of printer. Optional 2nd name is for color.
; diss in_ Intarr(2) Maximum display size [X,Y] 
;_Desc
; Because of different response for different shells, this version ASSUMES that 
; if the host if not hulk3, it is at ASU.
; For new installation, edit the lines with <<<
;_Calls  SETCOLOR
; 2013oct01 Hugh Kieffer  Derive from init.pro, adding many keywords
; 2013nov15 HK Ensure idltop ends with a /.  Firm-code test for stop
; 2014feb01 HK Increase   TVFAST_COM, safe  to 6 items for compatibility
;_End

common TVFAST_COM, safe ; intarr(6) [X,Y Display pixel limits, backing] for TVFAST
;                 then 3:5 same, used by SETWIND

cpu=getenv('HOST')              ; get current cpu, Will return null is undefined
if strlen(cpu) eq 0 then cpu='undefi' ; Force in case HOST is not defined

myhome=getenv('HOME')+'/'       ; Get home directory

cd,current=curd                 ; get current directory

; some common defaults
idltop=curd                     ; Top of IDL source tree
i=strlen(idltop)                ; string length
q=strmid(idltop,i-1,1)          ; last character
if q ne '/' then idltop=idltop+'/' ; ensure that it ends with a /

retain=2     ; Default backing store option; set to IDL provides. See IDL manual
win=[1280,1000]                 ; Common display size
vera='krcCurrent'               ; version of KRC
if keyword_set(ver) then vera=ver ; "
if curd eq '/work/work1/build/idl' then cpu='test' ; Special code for a test build on H3

print,'CPU is: ',cpu
case cpu of
  'hulk3': begin               ; CR primary computer
      solib=idltop+'externals/ftnwrap64.so' ; shared object library
      prjdat='/work/work1/krc/test/'      ; Project large files
      prjsrc=myhome+'krc/tes/'  ; Project other files
      mybw='HP_Laserjet_3330'   ; B&W printer
      myclr='q'                 ; Color printer
      outid='Kieffer'           ; ID to appear in plot subtitles
;      spice='/work/work1/SPICE/icy/lib/icy.dlm'
      end
  'hkieffer': begin      ; MAC laptop
      solib=idltop+'externals/ftnwrap64.so' ; shared object library
;;      myhome='/Users/hkieffer/'
   end
   'test': begin ; test build of distro
      capk='/work1/build/'       ; /K/: top of the build area
      idltop=capk+'idl/'
      solib=idltop+'extern/ftnwrap64.so' ; 
      krcdist=capK              ; top of KRC
      prjdat=krcdist+'big/'     ; Project large files
      prjsrc=krcdist+'run/'     ; Project other files
      outid=vera                
      mybw='HP_Laserjet_3330'   ; B&W printer
      myclr='q'                 ; Color printer 
  end
  else: begin              ; assumes ASU test environemnt
     idltop='/mars/common/rsi/idl71/bin/idl/' ; <<< IDL execution 
     solib=idltop+'externals/ftnwrap64.so'      ; shared object library
     krcdist='/mars/u/saadat/krc/krcDist222b/'; <<< top of KRC
     prjdat=krcdist+'krcdevtestprotocall/new/' ;<<< Project large files
     prjsrc=krcdist+'/run.org/'                ;<<< Project other files
     outid=vera                               
     mybw='q'                   ;<<< B&W printer
     myclr='q'                  ;<<< Color printer 
     end
endcase

if keyword_set(src) then prjsrc=src
if keyword_set(dat) then prjdat=dat          ; over-ride defaults
if keyword_set(prn) then mybw=prn[0]         ; B&W [and color] printer
if n_elements(prn) ge 2 then myclr=prn[1]    ; color printer 
if n_elements(diss) ge 2 then win=diss[0:1] ; display size

wset,0                          ; set to window 0
setenv,'MYHOME='+myhome         ; my home path
setenv,'IDLTOP='+idltop         ; top of my IDL source tree
setenv,'SOLIB='+solib           ; shared object library
setenv,'MYBW='+mybw             ; black&white printer
setenv,'MYCLR='+myclr           ; color printer
;setenv,'SPECDIR='+specdir       ; Ice spectra model computation
; Project areas
setenv,'PROJDAT='+prjdat        ; top of Project large data files
setenv,'PROJSRC='+prjsrc        ; top of Project source files [other than IDL code]

!x.style=1 & !y.style=1         ; set plots to default to use exact range

defsysv,'!idltop',exists=i      ; path to top of IDL run area
if i eq 0 then begin            ; no, so define it
    kok=9                       ; for use in next call to set read_only
    defsysv,'!idltop',idltop,kok; make it permanent
endif

defsysv,'!dbug',exists=i        ; debug flag
if i eq 0 then begin            ; no, so define it
    defsysv,'!dbug',0B          ; allow it to be modified
endif

defsysv,'!outid',exists=i       ; User ID for IDL plots
if i eq 0 then begin            ; no, so define it
    kok=9                       ; for use in next call to set read_only
    defsysv,'!outid',outid,kok  ; make it permanent
endif

window,0,retain=retain          ; Open default window with backing store option
safe=[win,retain, 800,640,retain] ; Init display size and backing store in Common
SETCOLOR,init=0                 ; Start the colors. Will define !binc

a=!dtor*30. & c=cos(a) &  s=sin(a)          ;| Define psym=8 as
usersym,[0.,c,-c,0.]*1.4,[-1.,s,s,-1.]*1.4  ;| inverted triangle

print,'env:  MYHOME= ',myhome,'   !outid = ',!outid
print,'IDLTOP=!idltop= ',!idltop
print,'PROJSRC=',prjsrc
print,'PROJDAT=',prjdat
print,'Printer names: MYBW=',mybw,'  MYCLR=',myclr
print,'Monitor size=',win

if 2 gt 3 then stop  ;<<< Modify this NEVER test if want to stop here
return
end
