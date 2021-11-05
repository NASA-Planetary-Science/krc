PRO setcolor,arg1, init=init 
;_Titl  SETCOLOR  Set or modify colors, lines, plot-symbols, #plots/page
; arg1 or init	in_ Controls type of action.  See Guide at 88: near end of code
;_Use 851:883 described by 888=Guide    899=window Guide
; -8:2 described after 66: below. Print by call init=1 then @99 
;_Vars
common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,hink
;        vvWASv before 2001mar01
; kcc[0] lctab	Last color-table loaded	                 Leave alone
; kcc[1] ctab	Color table user wants to use;-8:40      May reset before call
; kcc[2] nkc	# of color levels defined in kkc.     User may reset before call
;   after an 85x call, this will be total number of distinct lines/colors
;   but the number defined will always be at least 14
; kcc[3] nkl	# of line types defined in kkl    Reset only if you redefine kkl
; kcc[4] nkp 	# of symbols defined in kkp.      Reset only if you redefine kkp
; kcc[5] bwline Color for a Black&white line.	         Leave alone
; kcc[6] 	Number of unique colors defined
; kcc[7] fkc	Low percentage of colors not used.   OBSOLETE
; kcc[8] bclr   Color for call to PLOTS for borders and titles
; kcc[9] pclr   Color for high-contrast line to monitor. Leave alone
; kcc[10] minl  Number of color/lines defined. Leave alone
; linecol intarr[1=inactive or 8] Colors for lines over B&W images.
; kkc	intarr	Array of colors.	[ These 3 are refreshed at each call  ]
; kkl	intarr	Array of linesymbols.	[ You may reset values, but not number]
; kkp	intarr	Array of psyms.		[  lasts only until next call  
;--- user generally won't need to look beyond here
; fix-kkc,-kkl,-kkp intarr Preset colors, lines, symbols 
; kink  intarr  Current indices into !binc
; hink  intarr  Holds prior kink when called with 857. 
;                   Also used by kon91 (as scex1) to store instrument bands
; System Variable  !binc  intarr  lines + curretn pseudo-color indexs
;_Desc
; User may reset the items in common to get specific color/line/symbol style
; Read the instruction for each item below
; Default linestyle sequence uses dots last 
; As of 2009jul26, Hulk printer gray levels are indgen(256): 0]=black 255=white
;         Monitor white levels:  i* (1+256+256L^2 )
;_Useage hints;
;	For more than  5 or 8 colored lines,   line= kkl[(k/kcc[2]) mod kcc[3]]
; TESTING: plot,findgen(16)   then:  setcolor,init=85x  
; for i=0,15 do oplot,i+.1*findgen(9),line=kkl[i mod kcc[3]],color=kkc[i mod
; kcc[2]; for k=4,9 do begin & setcolor,init=850+k & for i=0,n_elements(kink)-1
; do xyouts,k,i,kink[i],color=kkc[i] & endfor
;_Desc
; For B&W printers: N (to >6) distinctive lines, gray-scale image
; For color monitor: same as Printer, Plus:  M distinctive colored lines 
;                colored paths across B&W image
; Routine is getting complex, >90% of use is for curves. Want some way to 
; handle them to Monitor or printer with no changes outside this routine.
; Plotting routines that want to be compatible with both monitor and printer
; should use the kkl and kkc arrays in common. This routine will always define
; these to be at least  minl  long, though some schemes will involve duplicates.
; Thus, plotting routines will not need to modulo the curve index with nkc or 
; nkl unless that index exceeds minl.
; After any plot to printer, must re-establish color scheme
;_Calls  CLOC  COLOR24BIT  GETP  GETPAN  GETPINTS  TOOTHB  ST0 
;_Lim
minl=50                         ; minimum number of lines always defined
; ROLO needs 32, OLI 3pix*14fpm needs 42
;_Hist  1999apr18  Hugh Kieffer
; 2012nov10-14 HK Make compatible with color eps, use DECOMP0SE=0, 
; + !binc=kink+indgen    Allow optional first argument instead of keyword
; + Delete history and older version of common, see 2012jun24 version 
; 2015oct02 HK Add 865 for strong colors on black
; 2015dec29 HK revise 860 colors; brightest first. 
; 2016jan18 HK Increase minl from 42 to 50
; 2016feb22 HK Include hink and kink in initiation, rather than test them for
; size
; 2018jun29 HK Add 884 as 37 progressive colors without black or white
;_END         .comp setcolor

; common COLORS, Rorig,gorig,borig,rcur,gcur,bcur
; colt=reform([Rorig,gorig,borig,rcur,gcur,bcur],256,6)
; CLOT,colt,['Rorig','Gorig','Borig','Rcur','Gcur','Bcur'], locc=1
;;;help ; see who called me 

defsysv,'!binc',exists=i        ; check if !binc  is defined
if i eq 0 then defsysv,'!binc',indgen(256) ; define to  allow later changes
i=n_params()           ; set types
if not keyword_set(init) then inj=0 else inj=init
if i ge 1 then inj=arg1 ; 
if inj ge 3 and inj lt 850 then begin
    message,'SETCOLOR called with invalid value='+string(inj),/con
    inj=0                    ; special to handle simple code in calling programs
endif
cblack=0 & cwhite=16777215      ; normal monitor values works for DECOMPP= 0 or 1

if inj eq 2 or inj eq 850 or n_elements(kcc) lt 11 then begin  ; initialization
    kcc=[-1,39,0,0,0,1,1,20,0,255,minl] ; will force color table set
    set_plot,'X'                ; set to monitor
    device, get_visual_depth=vdp ; or, if !p.color gt 40000
    if vdp eq 24 then begin
        lll=COLOR24BIT(r,lc=-2,/put) ; my default, r==dummy  Will load CT
;        TVLCT,reform(bgr[*,2]),reform(bgr[*,1]),reform(bgr[*,0])
        kcc[0:1]=-2             ; remember what was loaded
    endif 
    !binc=indgen(256)           ; unmodified Color Table
    linecol=0                   ; no low-index line colors assigned
  ; undocumented IDL: psym -7 to -1  connects symbols with solid line
  ;   linesymbol <0 yields solid, >5 yields long dash.  -1 used by moviespec
    fixkkc=[23,61,111,151,190,235,247,255] ; 8 fairly distinct colors
    kkc=fixkkc
    fixkkl=[0,2,3,5,4,1]        ; preset lines
    kkl=fixkkl
    fixkkp=[6,4,5,1, 7,2,3]     ; preset psymbols
    kkp=fixkkp
    !p.multi = [0,0,0,0,0]	; 1/page
    !p.background=cblack        ; black background, white border
    !P.color=cwhite
    kcc[8]=cwhite               ; bclr, borders & titles
    kcc[9]=255 & kcc[5]=cwhite 
    kink=fixkkc                 ; insurance
    hink=fixkkc                 ; insurance
endif

if inj ge 851 then begin 
; Set up line color/style scheme
; Basic approach is to define a number of color/style combos
; and recyle thru them if necessary
; If caller needs more than this, could either
; start repeating, or make the lines wider.
  case inj of
    851: begin  & !p.background=cblack  ; black background, white border
        !p.color=cwhite
        kcc[8]=cwhite           ; bclr, borders & titles
    	kcc[9]=255 & kcc[5]=cwhite  & end   ; lines
    852: begin & !p.background=cwhite; white background, black border and labels
        !p.color=cblack
        kcc[8]=cblack           ; bclr, borders & titles
    	kcc[9]=0 & kcc[5]=cblack & end   ; lines
    853: begin & !p.background=cwhite; printer preview to monitor
        !p.color=cblack
        j=155                   ; gray level, of 255
        k=long(j)+256L*j+256L^2*j ; 24-bit color word
        g=[cblack,k]            ; two levels of gray
        i=6                     ; # of different linetypes
        kkc=lonarr(minl)
        kkl=intarr(minl)
        for j=0,minl-1 do begin ; each defined index
            kkc[j]=g[j/6 mod 2] ; grayness
            kkl[j]=fixkkl[j mod 6] ; line type cycles fastest
        endfor & end
    854: kink=[45,120,170,254]; 4 on either
    855: j=1                   ; use kink in common
    856: kink=[15,44,120,170,220,254] ; 6 on either
;    857: begin & if kcc[6] gt 2 then hink=kink ; all black, insurance for printer
;           kink=replicate(cblack,6) & end
;    858: if n_elements(hink) gt 2 then kink=hink ; recover colors saved @857,863
    857: begin & hink=kink ; all black, insurance for printer
           kink=replicate(cblack,20) & end
    858: if n_elements(hink) gt 2 then kink=hink ; recover colors saved @857,863
    859: GETPINTS,'kink==!binc index set',kink,0,255
    860: kink=[255,130,197,209,254,40,225,15,65,110] ; 10 on black
    861: kink=[15,33,40,50,85,112,130,150,170,195,209,221,254,255] ; 14 on black
    862: kink=[13,30,32,40,50,60,85,114,130,143,170,197,209,221,232,254,255] ;17
    863: begin & if kcc[6] gt 2 then hink=kink ; black and gray
            kink=[cblack,15]  & end   
    864: kink=[15,90,160,254] ; 4 for white
    865: kink=[255,140,190,254,209,40,226]; up to 7 for black
    866: kink=[15,40,85,185,254,130] ; 6 for white
    868: kink=[15,40,85,185,254,130,225,50] ; 6+2 for white
;    869: kink=[0,20,40,50,85,120,130,175,225,254] ; 10=max good on white
    869: begin & kink=[0,1,2,3,4,5,6,7,8,9,10] ; 11 on white Pub
; set colors independent of which color table current
;black red blu grn blu brn pnl org pnk vil gray 
  b=[0,  0,255,  0,230, 10,255,  0,118,211,130] ; from xwincolors @704,79
  g=[0,  0,  0,200,200,150,  0,140, 16,  0,130]
  r=[0,255,  0,  0,  0,185,255,255,205,148,130]
  tvlct, r,g,b                  ;override the low end of current color table
  device, decomposed = 0 
  linecol=11                     ; number of overridden low colors
end
876: kink=[30,32,40,50,60,85,125,143,170,197,209,221,232,254,255] ; 15 on black
877: kink=[1,20,32,36,40,60,85,110,133,147,152,180,215,225,254] ; 15 on white

878: kink=[33,120,190,254,45,210,230,255,140,15] ; OLI bands on black | LDCM
879: kink=[45 ,90,170,254,60,220,230,  1,130,15] ; OLI bands on white | order
880: device, decomposed = 0 ; color is index into current table
881: device, decomposed = 1 ; color is composite RGB value e.g., !binc
882: TOOTHB                 ; default toothbar for all 256 colors
883: CLOC,kink,[.55,.93,-.022,.08] ; display defined current line colors
884: kink=[11,19,26,32,37,40,47,54,60,65,70,80,100,106,112,120,127,135 $ ; 37 cols
,142,150,155,160,166,175,182,187,192,200,204,208,215,223,228,234,240,246,254]
;885,886,887,889 spare
888: begin & print   ; Guide 
print,'851=black background  852=white background  853=printer preview' 
print,'854,6=4,6 for BorW  855=use kink in common  857=all black for printer'
print,'858=recover colors saved @857  859=modif. kink       882:TOOTHB'
print,'860,1,2=10,14,17 colors for black  863=B&gray        883:line display'
print,'864,6,8,9=4,6,8,11reset colors for white 865=7 black 888:color Guide'
print,'876,7,8,9 OLI scan:blk/wht  bands on black,white     899:window Guide'
print,'880/1=DECOMPOSED=0/1: color is index/24bit        884=37 colors'
        end
    else: begin & Message,' invalid 85x call',/cont
        kon=13 ; invalid, will cause print of Help
        goto,dokon
    end
  endcase
  if inj ge 880 and inj ne 884 then return   ; no change to kink or !binc
  if inj ge 854 then begin      ; fill out the common arrays
      i=n_elements(kink)>1      ; # of different colors
      if inj eq 857 then i=1    ; all colors are black
      kcc[6]=i                  ; move into common
      kkc=lonarr(minl)
      kkl=intarr(minl)
      for j=0,minl-1 do begin   ; each defined index
          kkc[j]=kink[j mod i]  ; color cycles fastest 
          kkl[j]=fixkkl[(j/i) mod 6] ; line type cycles slower, might repeat
      endfor
  endif                         ;^^^  inj ge 854
  goto,done
endif ;^^^ inj gt 850

; Gets here for inj lt 880
; Normally does nothing above this for init=2

refresh:        ; set color according to device and options. kcc[2] == # colors
kok= !D.name eq 'X'             ; color ok for this device
kkp=fixkkp                      ; insure it is set
if kok then begin  ; to the monitor
    kcc[5]=cblack 
    kcc[8]=kcc[9]        ; reset color for calls that plot border
endif else begin  ; to the printer, presume Black on white background
    g=[cblack,128]              ; two levels of gray
    i=6                         ; # of different linetypes
    kkc=lonarr(minl)
    kkl=intarr(minl)
    for j=0,minl-1 do begin     ; each defined index
        kkc[j]=g[j/6 mod 2]     ; grayness, step every 6 lines
        kkl[j]=fixkkl[j mod 6]  ; line type cycles fastest
    endfor
    kcc[5]=1                    ; PS, 1=black
    kcc[8]=0                    ; ensure color is 0 for calls that plot border
endelse
if kcc[1] ne kcc[0] then begin & LOADCT, kcc[1] & kcc[0]=kcc[1] & end
if inj lt 1 or inj gt 2 then goto,done; no prompting

kon=1 
ask: ;---------------------------------------- interactive parameter change
read, kon, prompt='SETCOLOR: 0=return 3=ctab 5=scheme 7=# 99=UseGuide> '
dokon: 
case kon of
 -1: stop
 0: goto,done                   ; return
1: !p.multi = [0,0,0,0,0]	; 1/page
2: !p.multi = [0,1,2,0,0]	; 2 vertical
3: begin & i=kcc[1] & GETP,'Color table',i,0,40 & kcc[1]=i & goto,refresh & end
4: !p.multi = [0,2,2,0,0]
6: !p.multi = [0,2,3,0,0]
7: begin & lowc =[-1, 0,  2,1,1,  0, 0, 0, 0,  0,6]
           highc=[40,40,255,6,9,255,22,85,50,255,60]
labc=['Current color-table      | Change ONLY' $
,'<< color table desired.  | those with << ' $
,'# of colors defined.     | here or before call' $
,'# of line types defined. MUST redefine fixkkl' $
,'# of symbols defined. MUST redefine fixkkp','Black&white line ' $
,'# of unique colors defined','<< Low % to not use (not used here)' $
,'Color for call to PLOTS for borders and titles' $
,'!binc index for monitor white','Number of color/lines defined']
     GETPAN,'Constants in Common',kcc,lowc,highc,labs=labc
     goto,refresh & end
9: !p.multi = [0,3,3,0,0]
62: TOOTHB,-2,VV=VV ; modify location/size of toothbar
64: TOOTHB,-4,VV=VV ; plot
66: TOOTHB ; plot default toothed color bar on current window
else: begin ; especially if =99
print,'Calling: SETCOLOR,init=  '
print,'869: override low end of CT with line colors'
print,'-1: set to have no reserved line colors'
print,' 0: minimum initialization/refresh, no prompts'
print,' 1: or /init,  minimum reinitializes; and prompts'
print,'		Rainbow_&_white, 6 linestyles, 7 symbols'
print,' 2: full refresh (update current plot device and arrays) and prompt'
print,' 3:850 same as init=0     888=Guide to 851:889'
print,'    Minimum number of color/line set=',minl,'  modulo for larger'
print,'    Code: plot,...  ,color=kkc[j mod kkc[2]],line=kkl[j mod kkc[3]]'
print,' ----- Here: ---'
print,'-1=stop  0=done'
print,' 1,2,4,6,9: # plots/page=12469. !p.multi=',!p.multi,format='(a,5i3)'
print,' 3: set color table   =',kcc[1]
print,' 7: set constants in COMMON: kcc=',ST0(kcc)
print,'66: overplot default toothed color bar  62=modify   64=plot '
  end
endcase
goto, ask

done:
i=n_elements(kink)>1            ; # of different colors
!binc[0:i-1]=kink               ; same line colors as kink
!binc[i:255]=i+indgen(256-i)    ; then pseudo-colors
kcc[2]=n_elements(kkc)          ; always insure sizes are right
kcc[3]=n_elements(kkl)
kcc[4]=n_elements(kkp)
if !dbug ge 7 then stop
return
end
