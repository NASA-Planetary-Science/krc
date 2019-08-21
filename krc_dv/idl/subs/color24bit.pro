 function color24bit,kkk,lc=lc, plt=plt, brf=brf, put=put, bgr=bgr
;_Title COLOR24BIT Generate 256 longwords to emulate nice 8-bit color table  
; kkk  both_  Intarr(4n or 4,n) way points on color worm
;                [0=blue  [1=green  [2=red   [3=location, must increase
; lc    in_  Integer color table index. Use -1 to get 0.  Default is current
;              -2 Hughs 2011 revision of IDL 39
;              -3 IDL table 39 
;              -4 for grey rainbow pink
;              -5 trial
;              -6 Hughs 2009 revision of IDL 39
;              -7 use kkk input
;              -8 two cycles
; plt   in_  Flag. If set, will plot each color vector
; brf   in_  Real  Brightness factor
; put   in_  Flag. If set, and lc<0, will load the load table
; func. out. Lonarr(256) of 24-bit values for ture color
;; common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
;_Calls  GETPAN
;_Usage
;  Call this routine, then plot with color=out(i).  0 <= i <= 255
;  Responds to  !dbug
;_Hist  2009mar12 HK Major revision, old version saved as .2008jul23
; 2011apr27 HK Fix error that dropped kkk3]. Revise lc=-2 (older put at -6)
; 2011jul18 HK Modify case -2 for dark green. Add case -8 with two cycles
; 2012may25 HK Slight modification of -8.  Add  put  option
; 2012nov10-14 HK Assume default is DECOMP=0.  Add keyword  bgr 
;_End

if not keyword_set(lc) then lc=0
if not keyword_set(brf) then brf=1.0
brf=(0.1>brf)<1.0                 ; insurance

case lc of
-2: kkk=[0,0, 0,   0, $ ; grey,b,g,r,white 2011apr28
 80, 80, 80,   1, $ ; dark grey
130,130,130,  10, $ ; mid grey
210,210,210,  30, $ ; bright grey  
255,  0,255,  40, $ ; bright pink
255,  0,  0,  90, $ ; blue
180,130,  0, 115, $ ; b+g dim
255,255,  0, 140, $ ; b+g bright
  0,130,  0, 165, $ ; green dim
  0,255,  0, 190, $ ; green bright
  0,255,255, 210, $ ; g+r 
  0,  0,255, 254, $ ; red
255,255,255, 255 ] ; white
;130,130,130    3, $ ; deep red-purple
; 32,165,218, 21, $ ; goldenrod

-3: kkk=[0,0, 0,   0, $ ; IDL table 39
 70,  0, 70,  14, $ ; dark purple
120,  0, 90,  25, $ ; purple peak red
200,  0, 70,  41, $ ; purple
255,  0,  0,  57, $ ; blue
255,255,  0, 102, $ ; b+g
  0,255,  0, 146, $ ; green
  0,255,255, 191, $ ; g+r 
  0,  0,255, 235, $ ; red
  0,  0,255, 254, $ ; red
255,255,255, 255 ] ; white

-4: $; if total is monotonic, then can never have same color twice!
kkk=[0,0, 0,   0, $ ; black.   Points along color worm
 80, 80, 80,  20, $ ; dark grey
120,  0,120,  40, $ ; purple
255,  0,  0,  50, $ ; blue
170,170,170,  60, $ ; grey
255,255,  0, 100, $ ; cyan
  0,255,  0, 145, $ ; green
  0,255,255, 180, $ ; yellow
  0,120,255, 190, $ ; orange
127,  0,191, 200, $ ; bright purple
255,  0,255, 220, $ ; heavy pink
  0,  0,255, 240, $ ; red
255,191,255, 249, $ ; bright pink
255,255,255, 255  ] ; white


-5: kkk=[0,0, 0,   0, $ ; black.   Points along color worm
 80, 80, 80,  1,  $ ; dark grey
 70,  0, 70,  2,  $ ; dark purple
120,  0, 90,  25, $ ; purple
255,  0,  0,  50, $ ; blue
255,255,  0, 100, $ ; cyan
  0,255,  0, 145, $ ; green
  0,255,255, 180, $ ; yellow
127,  0,191, 200, $ ; bright purple
255,  0,255, 220, $ ; heavy pink
  0,  0,255, 240, $ ; red
255,191,255, 249, $ ; bright pink
255,255,255, 255  ] ; white

-6: $        ; Construct blue,green,red byte tables for segmented color-worm
kkk=[0,0, 0,   0, $ ; Hughs revision of IDL table 39, & new 236:254
 80, 80, 80,   1, $ ; dark grey
160,160,160,   2, $ ; dark grey
130,  0, 50,   3, $ ; deep red-purple
255,  0,255,  13, $ ; bright magenta
 50,  0,130,  23, $ ; deep blue-purple
 70,  0, 70,  30, $ ; dark purple as in IDL39
120,  0, 90,  40, $ ; purple 
200,  0, 70,  51, $ ; purple
255,  0,  0,  61, $ ; blue
255,255,  0, 111, $ ; b+g
  0,255,  0, 151, $ ; green
  0,255,255, 191, $ ; g+r 
  0,  0,255, 235, $ ; red
255,255,255, 255 ] ; white

-7: begin & j=n_elements(kkk) ; user inputs kkk
        k=j mod 4
        if k ne 0 or j lt 8 then message,'Invalid kkk'
    end

-8: begin & kkk=[0,0, 0,   0, $ ; two cycles 2011jul18
 80, 80, 80,   1, $ ; dark grey  INdex used
130,130,130,  12, $ ; mid grey    2012may25 change from -1
210,210,210,  30, $ ; bright grey    1/2    2012may25 change from -1
130,  0,130,  -1, $ ; pink
130,  0,  0,  -1, $ ; blue
130,130,  0,  -1, $ ; b+g
  0,130,  0,  -1, $ ; green
  0,130,130,  -1, $ ; g+r 
  0,  0,130,  -1, $ ; red  ^^^DIM   BRIGHT
255,  0,255,  -1, $ ; pink
255,  0,  0,  -1, $ ; blue
255,255,  0,  -1, $ ; b+g
  0,255,  0,  -1, $ ; bright green
  0,255,255,  -1, $ ; g+r 
  0,  0,255,  -1, $ ; red   2012may25 change from 254
255,255,255, 255 ] ; white
; uniform space all set that were to -1
    nkk=n_elements(kkk) & n=nkk/4
    kkk=reform(kkk,4,n,/over)
jj=reform(kkk[3,*])
ii=where(jj lt 0,i) & i1=ii[0] & i2=ii[i-1]
j1=jj(i1-1) ; last low defined
j2=jj[i2+1] ; first hi defined
ii=j1+round((findgen(i)+1)*((j2-j1)/(float(i+1))))
kkk[3,i1:i2]=ii
 end

else: begin  ; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    if lc ne 0 then begin       ; load a new IDL color table
        lc=(lc>0)<40            ; handle -1 input, ensure valid
        loadct,lc               ; load a specific predefined color table
    endif                       ; use current table
    tvlct,v1,v2,v3,/get         ; get the red,green,blue profiles
    end
endcase

if lc lt -1 then begin ; construct 3 color vectors from way points 
    nkk=n_elements(kkk) & npk=nkk/4
    kkk=reform(kkk,4,npk,/over)
    mm=reform(kkk[3,*])         ; locations
    ccc=kkk[0:2,*] *brf         ; colors triples
    ii=mm-shift(mm,+1)          ; delta to next location
    if min(ii[1:*]) le 0 then message,'Locations not monotomic'
    bgr=bytarr(256,3)
    for k=0,npk-2 do begin      ; advance to next ending point
        bb1=ccc[*,k]   & j1=mm[k] ; prior end point
        bb2=ccc[*,k+1] & j2=mm[k+1] ; ending point
        denom=float(j2-j1)
        for j=j1,j2 do begin    ; each location along worm
            x=(j-j1)/denom      ; fraction along segment
            bgr[j,*]=round((1.-x)*bb1+x*bb2) ; linear interpolation
        endfor
    endfor
    v3=bgr[*,0] & v2=bgr[*,1] & v1=bgr[*,2] ; blue,green,red
    if keyword_set(put) then tvlct,v1,v2,v3 ; Load the color table
endif

;if n_elements(bgr) ne 3*256 then bgr=reform([v3,v2,v1],256,3)
ctab=long(v1)+256L*v2+256L^2*v3 ; generate the corresponding 24-bit integer

if keyword_set(plt) then begin
    q=min(abs(ctab-255*256L^2),ib) ; loc of blue
    q=min(abs(ctab-255*256L  ),ig) ; loc of green
    q=min(abs(ctab-255       ),ir) ; loc of red
    DEVICE, DECOMPOSED = 1 ; set to true color
    plot,v3,yran=[-5,260],/nodata $
      ,titl='...__=v1=red  __ __=v2=green  - - -=v3=blue  ...=average'
    oplot,v3,line=2, color=ctab[ib]
    oplot,v2,line=5, color=ctab[ig]
    oplot,v1,line=4, color=ctab[ir]
    oplot,(fix(v1)+v2+v3)/3,psym=3 ; white for total
    for i=0,255 do $            ; quick colorbar
        XYOUTS,i-1,20,'|',color=ctab[i],charth=3,charsize=2
    DEVICE, DECOMPOSED = 0 ; set to pseudo-color

endif

if !dbug then stop

return,ctab
end
