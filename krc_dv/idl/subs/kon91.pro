Pro kon91, ptitl,prior,hold,kon,kons,kitel, maxk=maxk,get=get
;_Titl  KON91  Common minimal functionality in the kon case statement
; ptitl	in.	String name of calling program.
; CALLER SHOULD NOT MODIFY the next 2 arguments:
; prior both. 	Strarr of prior id and string(all). Reads source if changed.
; hold	both.	Strarr of the guide. Stored between calls
; kon	in.	Integer. The case statement item
; kons	in.	Intarr. Auto-sequence of kon
; kitel	in.	String.	Prior action by calling program
; maxk  in_     Integer. Max # of kon entries.  Sent to MAKE99. Default there
; get   in_     Integer  Flag to expand GETP_   Sent to MAKE99. Default there
;_Desc
; Normally do not stop in this routine. Thus arguments in calls to lower 
; routines will be lost unless passed in the above argument list
; Easiest to leave section before  ask: calling to setcolor in main routine
; because of surrogates for elements of kcc[]
;_Usage  The following actions are reserved here:
; -8 -3 -1 100:3 121:2 340 344 8 80 801:4 808:810 81 82 850:860 87 88
;    9 99 990:5 
;	992 To generate a line in storage for each kon.
;		Then 994 produces a full listing of all
;	991 To expand only the current kon
;_Calls  GETPINTS  GRAPH  MAKE99  SETCOLOR  SETWIND  ST0  SUBTITLE  
; graph calls setcolor
; setcolor calls COLOR24BIT  GETP  GETPAN  TOOTHB  st0 
; st0 calls DELAST0
;_Hist 2007aug14 Hugh Kieffer Adopt from KON99 by eliminating all
;image function and call options
; 2007sep06 HK Activate calls to setcolor
; 2011jul13:15 HK Add .png output. Direct-code for .pjg output instead of TV2JPG
;          Direct-code for .eps output instead of TV2LP 
; 2012jan24 HK Incorporate call to SETWIND
; 2012mar29 HK Add action 808   
; 2012may14 HK Add action 810   
;_Liens If caller uses   common SETCOLOR_COM2  he will have to detect kon in
;  range 850:882 to refresh any variable dependant upon that common.
;_End            .comp kon91

common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,scex1

kite=ptitl+'@'+strtrim(kon,2)   ; follows date in subtitle      fils[2]+' '+
if kon ge 850 and kon le 899 then begin ; set to set of high-contrast colors
    if kon le 889 then SETCOLOR,init=kon  else SETWIND,kon
    return 
end 
case kon of ;...................................................................

-8: stop                     ;- Stop in KON91 --------

-3: i=1                         ;-  -------- KON91  null

-1: begin & print,'Any key to GO' & qi=get_kbrd(1) & end ;- Wait
100: wset,0                     ; wset: 0
101: erase                      ;+ Clear the default window
102: wset,2                     ;+ wset: 2
103: begin & window,3,retain=2,xsize=640,ysize=512,xpos=2560+640,ypos=1064; Window 3 for output
  wset,3 & end
                   
122: GETPINTS,'Action sequence',kons,0,0 ;- Modify  kons  sequence
121: kons=-3 ;- + Reset to null

340: loadct,0                   ; Load gray scale

344: lll=COLOR24BIT(r,lc=-2,/put) ; Load my color table

; gcmplot @79 generates a color test pattern

801: begin ;- Write positive .eps file  Expect color on white
; wasdev=!D.NAME                  ; current device
; white background. To show all lines will need to convert everything that is 
; not equal to !P.background, which is 255 in all 3 panes, or total of 765
image=tvrd(true=3); read image in current window
; add all 3 panes, any non-white becomes 764, then 127
image=byte((fix(total(image,3))>764)-637) ; either 127 or 128
set_plot,'ps'                   ; set output device to postscript
device,filename='idl.eps',bits_per_pixel=1,/port ; define filename, 1-bit 
tv, image ; write negative image to ps device. All colors -> black
device,/close
set_plot,'x' ; , wasdev  Return plotting to original device
if !P.background eq 0 then print,'WARNING, will yield black background' 
print,'Created 1-bit PS file.   mv idl.eps   or  epstopdf idl.eps  Then mv' 
end

802: begin & write_png,'idl.png',transpose(tvrd(true=3),[2,0,1]) ;- Write .png
print,'Created PNG file >> mv idl.png' & end

803: begin & write_jpeg,'idl.jpg',true=3,tvrd(true=3),quality=90 ;- Write .jpg
print,'Created qual=90 JPEG file >> mv idl.jpg' & end

804: begin ;- Reverse Black and white image to 1-bit .eps file
; wasdev=!D.NAME                  ; current device 
image=tvrd() ; black=0, white=255
set_plot,'ps'                   ; set output device to postscript
device,filename='idlr.eps',bits_per_pixel=1,/port ; define filename, 1-bit 
tv,not image ; write negative image to ps device. 
device,/close
set_plot,'x' ; , wasdev  Return plotting to original device
if !P.background ne 0 then print,'WARNING, may yield black background' 
print,'Created 1-bit PS file.   mv idlr.eps   or epstopdf idlr.eps  Then mv' 
end

; available: 805,806,807
808: xyouts,0.,.983,strtrim(kitel,2),/norm ; add action to top-left of plot

809: Begin  ;- Message to move file, and wait.
Print,'Move the file   Then any key to GO' & qi=get_kbrd(1) & end

810: begin & kkc=scex1 ; Set colors to instrument bands (scex1)
kink=scex1 & i=n_elements(scex1) & kcc[2]=i 
kkl=replicate(0,i)  & kcc[3]=i & end  ; REQ that scex1 loaded

80: CEPS0,-1                    ; Set figure type
81: CEPS1                       ;+ Before plot
82: CEPS2,cepsid                ;+ After plot
88: SUBTITLE,id=kitel          ;- Add subtitle to plot

; next set becoming obsolete
;81: GRAPH,81,hard               ;- Start Graph to color file and printer
;80: GRAPH,0,hard               ;- Reset plot output device
8:  GRAPH,8,hard                ;- Start Graph to file and printer
87: GRAPH,7,hard                ;- Close plot device, no spawn of plot
9:  GRAPH,9,hard                ;- End a plot, save and print the file

99: MAKE99,ptitl,prior,hold,maxk=maxk,get=get; make guide to calling program

990: begin ; KON91 action guide
print,'-8=StopInKON91  -3=null  -1=pause    0=Stop    888=setcolorGuide'
print,'100=wset,0  101=erase  102=wset,2  103=window for output   899=wind'
print,'121=kons=-3  122=Edit Kons  801/2/3/4 output to eps/png/jpg/-eps'
print,'340/4=load grey/myColor 808=actionlabel 809=Warning: mv  810=BandClr'
print,'80/81/82=type/start/endFig  8=newPS 87=close 88=subtitle 9=plotPS'
print,'MAKE99:  991=Expand kons  992/995=1-line each  994=expand all'
print,'123: Do kons=',ST0(kons)
end  

991: if n_elements(kons) ge 2 then $ ; Expand current kons in MAKE99
     MAKE99,ptitl,prior,hold,all=kons,maxk=maxk,get=get ; Expand current Kons 
992: MAKE99,ptitl,prior,hold,all=23  ,maxk=maxk,get=get ; Full 1 line each
994: MAKE99,ptitl,prior,hold,all=8   ,maxk=maxk,get=get ; Expand all Kons
995: MAKE99,ptitl,prior,hold,all=23  ,maxk=maxk,get=get,comf='./subs/kon91.pro'; And kon91

else: print,'KON91: Invalid entry'
 
endcase

return
end
