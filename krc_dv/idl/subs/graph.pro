PRO graph, kodin,hard, psfile=psfile,port=port
;_Titl  GRAPH  Interface to graphics devices
; kodin	 in. integer control kode:
;     80    = restart output file empty
;     8,81  = set to B&W/color hardcopy and open output file
;       81: coded directly in KON91,99
;     87,9  = close output file, set to X-term   9= and spawn to printer
;  +100 = stop before return
; hard   in_ OBSOLETE: for backward compatibility
; psfile in_ OBSOLETE PostScript output file stem; will append '.eps' if absent;
;	Default is 'idlout' & prompt: <CR> yields no change.
; port in_ Int     1=portrait  0=landscape(default); -= will ask 
;_Desc
; Required capabilitities:
; 8,81  Direct following plot output to a file: B&W (default) or color
;     allow setting to landscape  or portrait. (default)
;   If destined for B&W, then call setcolor to set all lines black
;   and reset with the file closed.
;   If prior state was monitor Save that state so that it can be recovered
;   For 81, !binc must be insgen(256), else !binc=COLOR24BIT(kok,lc=-2) 
; 9 If there was a plot, send file to printer
;      If prior state was monitor, Restore the monitor color state
;      Direct folow plots to monitor.
; NORMAL USAGE:
; first call with kodin = -1 to initialize and set to plot on screen
; when ready to plot to hardcopy, call with kodin = 8 or 81
; after plot calls are done, call with kodin=7 or 9 to send plot to printer
;_Call  SETCOLOR

;_Hist  99jun11 Hugh Kieffer complete rewrite, largely keeping default actions
; 2001apr20 HK remove prompt for file name, rather use 98: option
; 2002apr05 HK auto-init to landscape color terminal, Delete many call options
; 2002aug25 HK Add backing-store option to use windows system 
; 2010oct25 HK Major rewrite to be compatible with only:
;   color monitor, B&W printer, color printer (incomplete)
;   Delete 2nd argument 'hard' 
;   Remove all setting of colors from here.
; 2012jun15 HK Complete the color implementation. Drop use of psfile
;_End

common GRAPH_SAVE, outfile,orient,bwf,devh,winc	; local memory
;      as of 2012jun16, not used by any other idl/subs/*.pro
; outfile = output file name
; NOPE orient = orientation of hardcopy plot, 1 or 21 -> landscape, else portrait
; bwf  Flag; 1=B&W output device
; devh = defined hardcopy output device, 0=undefined
; winc lonarr(256) store !binc while doing color eps

common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,hink

wasx =!d.name eq 'X'  ; device when entering this was the monitor

if n_elements(bwf) lt 1 then begin 
    bwf=0                       ; ensure defined
endif

kode=kodin mod 100

;if kode eq 8 then begin ; restart the output file


if kode eq 8 or kode eq 80 or kode eq 81 then begin ; direct to print file
;    if keyword_set (psfile) then outfile=psfile else outfile='idlout.eps'
    outfile='idl.eps'
    if wasx then winc=!binc ; save the 256 colors

    if not keyword_set(port) then port=0
    if port lt 0 then begin   ; set orientation
        read,port,prompt='orientation?: 1=portrait else=landscape > '
        if port ne 0 then port=1
    endif

    if kode eq 81 then begin    ; go to color file and printer 
        devh=getenv('MYCLR') 
        outfile='cidl.eps'      ; indicate color file
        bwf=0                   ; unset B&W flag
;'        hink=kink               ; save prior color set
;'        kink=indgen(256)        ; "
        if wasx then winc=!binc
        !binc=indgen(256)       ; use byte index rather that 24bit color
        for j=0,kcc[10]-1 do kkc[j]=kink[j mod kcc[6]] ; color/line set 
        set_plot,'PS'           ; use the Postscript device
        if port then device,file=outfile,/color,bits=8 $
                else device,file=outfile,/color,bits=8,/landscape
    endif else begin            ; go to B&W file and printer
        devh=getenv('MYBW')     ; get printer name
        SETCOLOR,init=857       ; save current colors and set to all black
        bwf=1                   ; set B&W flag
        set_plot,'PS'
	if port then device,file=outfile,/portrait $
	        else device,file=outfile,/landscape
    endelse

endif else begin ; redirect to monitor

    if not wasx then begin
        DEVICE,/close_file      ; may not close X device
        if kode eq 9 then begin ; Option to print it
            SPAWN,'lpr -P'+devh+' '+outfile ; print the file
            print,'Now: mv '+outfile
        endif
        !binc=winc              ; restore 256 colors
    endif
    SET_PLOT,'X'
    DEVICE,retain=1
;'    SETCOLOR,init=858           ; reset to prior color scheme
    bwf=0                       ; unset B&W flag
endelse

if kodin ge 100 then stop
return
end
