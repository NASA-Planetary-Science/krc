PRO printsimple,fin,fout,hour,verb=verb
;_Titl  PRINTSIMPLE  Print table of values for KRC website simple mode
; fin  in. string  Pathname of KRC type 52 file
; fout in. string  Pathname for output table
; hour in_ float   Desired hour.  Default=13. Not needed for Hour mode.
; verb in_ Flag, bit encoded +1: Print some information 
;                +2: Stop before return   
;                +4: If  fin  is season, plot diurnal curve at 4 seasons.
;_Use
; in IDL:   printsimple,<krcoutput.t52>,<plot input>  [,verb=1]
;
;_Calls  READKRC52  which calls   BIN5  READKRCCOM calls DEFINEKRC calls BYTEPAD
; If verb is set,  CLOT  may call:  CURVEGUIDE  LABEL_CURVE  SCALELIN
;  LABEL_CURVE calls RNDEX  RTERP1
;_Desc
; Purpose, Run at ASU to suppport the web simple mode. Follows in time a KRC run
; using either the simph or simps input file, reads the .t52 output file and
; creates a file of plain text table used to create the web plot. 

; Expects .t52 file with one latitude and one case.
;   Either 670 seasons, or 
;
; Determines which mode is desired from the number of seasons in the .t52 file
; Output file: Both modes have 5 columns, first is a 0-based line index
; Col.2 is the abcissa ( X-axis), which will be hour or Ls
; Col.3 is Surface kinetic temperature
; Col.4 is Planetary Bolometric T.
; Col.5 is Atmosphere temperature
; For seasonal mode, produces line for every season in the .t52 file
; For hourly mode, produces line for every hour in the .t52 file

;_Hist 2014feb05 Hugh Kieffer Initial version for Ken Rios
; 2014mar06 HK Add comments. Print hour or Ls that is output
;_End
help,verb
if n_elements(verb) lt 1 then verb=0 ; set verbosity
help,verb

ss=findfile(fin,count=j) ; check that input file exists
if j ne 1 then begin
   message,fin+'   file not found' ,/con
   goto,done
endif

kcom=READKRC52(fin,ttt,uuu,vvv,itemt,itemu,itemv,ddd,ggg,itemd,itemg,vern=vern)
siz=size(kcom)
if siz[siz[0]+1] ne 8 then begin  ; kcom must be a structure
   message,fin+'   file read failed' ,/con
   goto,done
endif
sizt=size(ttt) & nhour=sizt[1] & niti=sizt[2]
nlat=sizt[3] & nsea=sizt[4] & ncase=sizt[5]

hh=(24./nhour)*(findgen(nhour)+1) ; hours
ls=vvv[*,1,0]                     ; seasons
if nsea lt 10 then begin ; ------------ Diurnal plot mode ------------
   xx=hh                ; abscissa is hour
   nx=nhour
   yyy=ttt[*,0:2,0,0,0]         ; [hour,  [Tsur,Tplan,Tatm]
   xtit='Hour'
   yval=ls[0]                   ; Lsubs at output
endif else begin ;------------ season plot mode------------
   xx=ls; abscissa is Ls
   nx=nsea
   if not keyword_set(hour) then hour=13. ; requested time-of-day
   q=min(abs(hh-hour),j)         ; get location of closest hour
   yyy=transpose(reform(ttt[j,0:2,0,*,0])) ; [season,3]
   xtit='Ls, degrees'
   yval=hh[j]                   ; hour output
endelse


openw,lun,fout,/get_lun,error=operr ; open the output file
if operr ne 0 then begin
   print,'PRINTSIMPLE:: open ERROR # and file=',operr,fout,strmessage(operr)
   goto,done
endif

; write the output lines
for i=0,nx-1 do printf,lun,i,xx[i],yyy[i,*], form='(i4,f6.1,3f8.3)'
free_lun,lun ; close file and free the unit

if verb ne 0 then begin ; skip unless verbose mode requested
   print,'yval',yval
   CLOT,yyy,['Tsur','Tplan','Tatm'],xx=xx,locc=[.4,.4,-.03,.06] $
        ,titl=[xtit,'Kelvin','PRINTSIMPLE: '+fin]
   lsv=vvv[0,1,0]
   alat=uuu [0,0,0]
   elev=uuu [0,1,0]
   print,'Lat., elev, =', alat,elev
   print,'Num. Seasons, ls[0],nx =', nsea,lsv,nx

   if nsea gt 40 and ishft(verb,-2) mod 2 then begin ; +4
      zz=fltarr(nhour,4) ; to hold diurnal curves
      sls=strarr(4)
      for i=0,3 do begin 
         ls=i*90.
         q=min(abs(xx-ls),j)    ; get location of closest hour
         sls[i]=string(xx[j],form='(f7.1)')
         zz[*,i]=ttt[*,0,0,j,0]
      endfor
      print,'any key to go' &  j=GET_KBRD(1)
      CLOT,zz,sls,xx=hh,locc=[.5,.4,-.03,.06] $
           ,titl=['Hour','Tsur at 4 seasons, Kelvin','PRINTSIMPLE: '+fin]
   endif
   if ishft(verb,-1) mod 2 then stop ; +2
endif

done: 
return
end
