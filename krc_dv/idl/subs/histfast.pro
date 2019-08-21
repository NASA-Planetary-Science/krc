pro histfast,aa,sigin=sigin,bisin=bisin,xlab=xlab,linear=linear,sub=sub $
              ,by=by,noplot=noplot,wt=wt, h1=h1,i1=i1,r1=r1, used=used
;_Titl  HISTFAST  Robust, easy histogram plot, with statistics, opt row weights
; aa	in.	Array to be treated
; sigin	in_ 	Number of std. deviations for final plot. neg. --> Default=4.
;	    OR  [min,max] plot limits for X
; bisin	in_	Binsize for final plot. 0 --> Default is to yield ~100 bins
; xlab	in_	Label for x axis, Default= 'X value'
; linear in_	Force plot to be linear
;		   Default is linear if not very peaked, else log
; sub	in_	Subtitle text to follow  SUBTITLE + HistFast. Default is none
;		 If this present and not a string, no subtitle at all.
; by	in_	If set, and data range <255, will use 0:255 for X
; noplot in_	If set, does print instead of plot; if =2, no print either.
; wt	in_	Vector of weights for each row. size must equal dim.2 of aa
; h1	out_	The histogram
; i1	out_	Lonarr(4), [nbin, #lo, #hi, nbad, ntot]    Used for the Histogram
; r1	out_	Fltarr(7), [hist_min, _max,binsize,mean,StdDev, data_Min,_Max]
;	            True X values of bins are: r1[0]+r1[2]*findgen(i1[0]) 
; Next keyword is obsolete, replaced by i1 and r1
; used	out_	[min, max, binsize, #lo, #hi, nbad,mean,StdDev,nbin]  used for
;		  0    1      2      3    4     5    6    7     8     histogram
; i1lab=['nbin','#lo','#hi','nbad','ntot']  ; Labels for histfast results
; r1lab=['histMin','histMax','binSize','Mean','StdDev','dataMin','dataMax']

;_Calls  MEAN_STD  SUBTITLE  IDL: histogram  sort
;_History  2000feb27  Hugh Kieffer   Modify from histfast>hf, add sub key
; 2000mar27 HHK use binwidth of 1 for <256 items, add by and dbug keywords
;			 and add used, h1 and noplot keywords.
; 2000jun26 HHK add mean,StdDev to used output.
; 2000dec21 HHK Change from overwriting NAN's with mean; now if non-finite
;			data occur, will make copy of only the finite data.
; 2001apr14 HHK Allow sigin to have 2 items as limits, and fix annotation 
;		to scale with !p.multi
; 2001jun22 HK Make test for a2-a2 use Long to avoid Int extremes yielding -1
; 2001oct06 HK Accomodate IDL word types 12-15
; 2001dec11 HK Add =2 option to noplot, Fix return if all values the same.
; 2001dec18 HK Accomodate rare case of all input NAN
; 2003may14 HK Add  i1 and  r1; and small documentation changes
; 2009mar06 HK add data_Min,_Max to r1. Replace keyword dbug with !dbug
; 2010apr14 HK add  nin  as last item to i1
; 2010oct08 HK Tweak text formats
; 2011mar22 HK Stop before return only if !dbug ge 7
; 2011nov09 HK Fix small bug that allowed long(huge) 
; 2014apr12 HK Deal with pathologic case of all same but SD is roundoff
; 2015jan09 HK Do not test integer arrays for finite. And test range as longs
; 2016dec11 HK Fix bug that left a1 undefined when some NAN's present
; 2017sep12 HK If invalid array, return with message rather than stop
;_End

;on_error,2 ; to handle the "Array has a corrupted descriptor: H1" upon return

sizea=SIZE(aa) & wordtype = sizea[sizea[0]+1] ; size of input array
isint=wordtype le 3 or wordtype ge 12; some form of integer
nin=sizea[sizea[0]+2]             ; number of input elements
if nin lt 1 or wordtype eq 7 then begin 
  message,'Invalid input: number='+strtrim(nin,2)+'  type=' $
  +strtrim(wordtype,2),/con
  h1=fltarr(2) & i1=intarr(4) & r1=fltarr(7) ; ensure defined, but nuts
  r1[2]=1.                                   ; binsize not zero
  return
 endif

if keyword_set(xlab) then xtit=xlab else xtit='X value'

; check for non-finite points
if isint then nbad=0 else ibad=where(finite(aa) eq 0,nbad) 
if nbad gt 0 then begin
    if nin-nbad gt 0 then begin ; some finite
        ff=aa[where(finite(aa))] ; create array of only finite points
        mean=MEAN_STD(ff,std=sd) ; only finite elements
        a1=min(ff,max=a2)
        if finite(mean) eq 0 then begin
           print,'HISTFAST: ',xtit,' mean is not finite' 
           print,'Min and max of finite=',a1,a2
           b1=mean & b2=a2 & bsize=-1. & j1=0 & j2=0 & nbin=1 
           message,'; may need to swap_endien.',/con
           goto,done 
        end
;;    aa[bad]=mean
    endif else begin            ; no finite
        sd=-1.
        mean=!VALUES.F_NAN
    endelse
 endif else begin               ; all finite
   a2=max(aa,min=a1) ; extremes of input
   if isint then a2=long(a2) 
   if a2-a1 gt 0 then begin 
      mean=MEAN_STD(aa,std=sd)
      if sd le 0. then begin 
         message,'HISTFAST: numerical limit',/con
         sd=1.e-32              ; tiny number
      endif
   endif else sd=0. ; & use as flag 
endelse
if sd le 0. then begin ; constant or no finite
   print,'HISTFAST: ',xtit,':     All',nin,' items =',a1 
   b1=a1 & b2=a2 & bsize=-1. & j1=0 & j2=0 & nbin=1 ; define for used=
   mean=a1
goto,done & end

; Now, pick limits to exclude far outliers
j=n_elements(sigin)
case j of
    0: sigin=4                  ; default
    1: if sigin lt 0.5 then sigin = 4. ;  unreasonable yields default
    else: begin                 ; 2-items input, used them as limits
        b1=sigin[0]
        b2=sigin[1]
    end
endcase
if j lt 2 then begin            ; set limits based on StdDev
    b1= a1 > (mean-sigin*sd)
    b2= a2 < (mean+sigin*sd)
endif 

nbin=100 > round(sqrt(nin/100.)) ; rough # bins
bsize=(b2-b1)/nbin		; # bins used is 1 larger than this
if isint then if (long(a2)-long(a1)) le 255 then begin
    bsize=1
    if keyword_set(by) then begin
        b1=0 & b2=255 
    endif else begin
        b1=a1 & b2=a2
    endelse
endif
if keyword_set(bisin) then if bisin gt 0. then bsize=bisin ; use only if >0
if isint then bsize=fix(bsize) > 1

nw=n_elements(wt)
if nw eq 0 then begin           ; unweighted
    ytit='Bin count'
    if nbad eq 0 then begin     ; use original array
        h1=HISTOGRAM(aa,min=b1,max=b2,binsize=bsize)
    endif else begin            ; use only finite data
        h1=HISTOGRAM(ff,min=b1,max=b2,binsize=bsize)
    endelse
endif else begin                ; weighted sums 
    if nw ne sizea[2] then message,'size of wt and aa[2] do not agree'
    ytit='Bin count, weighted for rows'
    h1=fltarr(nbin)             ; to hold sums
    for j=0,nw-1 do begin       ; each row can have different weight
        wtj=wt[j]
        ii=round((aa[*,j]-b1)/bsize)   ; vector of bin indexes
        for i=0,nbin-1 do begin
            q=where(ii eq i,nq)
            h1[i]=h1[i]+nq*wtj
        endfor
    endfor
endelse
j=where (aa lt b1,j1)           ; # non-counted outliers
j=where (aa gt b2,j2)           ; "

nbin=n_elements(h1)

if keyword_set(noplot) then begin ; must be after above code
    if noplot ne 2 then begin
        print,'Histfast: #points, Mean, S.D.=',nin,mean,sd,'  Extremes=',a1,a2
        if nbad gt 0 then print,'  Number non-finite=',nbad
    endif
    goto,done
endif

y2=max(h1)
xx= b1+bsize*findgen(nbin)      ; true value X-axis
; if there are only a few high points, use log_y
ii=SORT(h1)                     ; index of low to high
ymid=h1[ii[round(nbin/2)]] >1	; could catch a zero
; display results, with printed statistics & outlier counts

yfac=0.94-.04*findgen(4)          ; normalized location of annotations
xloc=b1+0.04*bsize*(nbin-1)
j=(!p.multi[2]>1)<4             ; # plot rows per page
csize=[1.2, 1.,0.65,0.5,0.35]
csize=csize[j]

toptit='Histfast: #points & bins='+string(nin,nbin)
if y2/ymid lt 10 or y2 lt 80 or keyword_set(linear) then begin 	; linear Y scale
	PLOT,xx,h1,yrange=[0.,y2],xstyle=1,ystyle=1 $
		,xtitle=xtit,ytitl=ytit,title=toptit
        yloc=yfac*y2
endif else begin		; log Y scale
	PLOT,xx,(float(h1) > 0.8),yrange=[0.8,y2],xstyle=1,ystyle=1,/ylog $
		,xtitle=xtit,ytitl=ytit,title=toptit
        yloc=exp(yfac*alog(y2))
endelse
if j1 gt 0 then PLOTS,xx[0],j1,psym=4,symsize=2.*csize
if j2 gt 0 then PLOTS,xx[nbin-1],j2,psym=4,symsize=2.*csize


XYOUTS,xloc,yloc[0],'Mean & S.D.='+string(mean,sd,form='(2g11.4)'),charsize=csize
XYOUTS,xloc,yloc[1],'Extremes='+string(a1,a2,form='(2g11.4)'),charsize=csize
if j1+j2 gt 0 then XYOUTS,xloc,yloc[2],'Number of unplotted outliers=' $
  +strcompress(string(j1,j2)),charsize=csize
if nbad gt 0 then XYOUTS,xloc,yloc[3], 'Number non-finite ='+string(nbad) $
  ,charsize=csize
if not keyword_set(sub) then SUBTITLE,text='HistFast' else if $
   size(sub,/type) eq 7 then SUBTITLE,text='HistFast '+sub
done:
if arg_present(used) then begin
    used=[b1,b2,bsize,j1,j2,nbad,mean,sd]
    print,' keyword  USED is obsolete'
endif
i1=[nbin,j1,j2,nbad,nin]
r1=[b1,b2,bsize,mean,sd,a1,a2]
if !dbug ge 7 then stop
return
end
