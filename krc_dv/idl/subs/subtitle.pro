pro subtitle, id=id,text=text,up=up,ytex=ytex, size=size,time=time, user=user
;_Titl  SUBTITLE  Add subtitle time and text to existing plot
; id	in_	Program name. Default is blank
; text	in_	Additional text on time line. Default is blank
; up	in_	Additional text for line above time line. Default is none.
; ytex	in_	Text up along left border. Default is none.
; size	in_	Character size. Default=1.
; time  in_	String for date & time. Default is current time
; user  in_     String name of person making the plot. Default is !outid
;_Hist  99apr15  Hugh Kieffer
; 2000feb08 HHK add a blank after time
; 2000feb26 HHK put date&time (yyyymmmdd time) all the way to left. Add size
; 2000mar12 HHK Add ytex
; 2000sep28 HHK add time option
; 2003nov16 HHK add option to change the User
; 2011oct02 HK Replace call to CATIME with ISOTIME
;_End

if not keyword_set(id)   then id=''
if not keyword_set(text) then text=''
if not keyword_set(size) then size=1.0
if keyword_set(time) then nowis=time else $
    nowis=ISOTIME(1)         ; get date/time now as yyyymondd hh:mm:ss
if not keyword_set(user)   then user=!outid
xyouts,0.,0.,nowis+' '+user+' '+id+' '+text,charsize=size,/device

if !D.name eq 'X' then  yloc=15. else yloc=400. ; y location of upper line
yloc=yloc*size
if keyword_set(up)   then  xyouts,0.,     yloc    ,up,charsize=size,/device
if keyword_set(ytex) then  xyouts,.5*yloc,1.4*yloc,ytex $
  ,charsize=size,/device, orient=90.

return
end
