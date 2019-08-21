PRO printjcols,ss,jin,down=down,right=right,align=align,oct=oct	,len=len
;_Titl  PRINTJCOLS  print j columns of strings, indexed
;_Args 
; ss	in.   Vector of strings to print
; jin	in.   Integer, Number of Columns to print
; down  in_   If set, indices increment down. Default is across
; right in_   If set, string appears on the right of  =. Default is on the left
; align in_   If set, left-adjust ss
; oct	in_   If set, index count is in octal
; len	both_ Integer. Length of string to print, Default is 12, (must be < 75)
;_Desc
; A printed column requires 6 places after the label, except last needs only 4
;_Hist  98aug31  Hugh Kieffer Add the len parameter
;   98nov23 HK  Fix name with final s
; 2005sep10 HK Add the keywords  down, right. align
; 2005nov27 HK Fix bugs with indexing and   right  .  Clarify documentation
;_End

if keyword_set(right) then j2=1 else j2=0 ; set index of string position
if keyword_set(len) then w=len else w=12 ; set word length
dow =keyword_set(down)           ; increment down rather than across
ali =keyword_set(align)          ; left-adjust input strings
doct=keyword_set(oct)          ; left-adjust input strings
nin=n_elements(ss)              ; number of items to print

maxcol=fix (82/(w+6))           ; # cols that will fit in 80 characters
ncol=jin< maxcol                ; # cols to use
if ncol lt 1 then begin         ; force at least one column
	  ncol=1  & w=60 
end 
nrow=(nin-1)/ncol +1            ; # of rows required
ii=intarr(ncol) 
; construct the output format in real time
fm1 = 'fmt=''(' + strtrim(string(ncol),2)+'(' ; leading part
fms = 'a'+strtrim(string(w),2)    ; string part
fmeq= ',''''='''',' ; equal sign.Need the multiple ' to get 1 into final string
fmi = 'a3'                       ; part for the index
fm2 = ',2x))'' '                   ; last part
if j2 eq 0 then com=fm1+fms+fmeq+fmi+fm2 $ ; string on the left
           else com=fm1+fmi+fmeq+fms+fm2 ; string on the right
;;  print,'com=   ',com
ok=execute (com)
;;  help,ok,fmt 
;;  stop
j1=1-j2                         ; set relative position of input string items
jj=nrow*indgen(ncol)            ; offsets, if needed for down
blank=''
wm1=w-1 ; last position in output string of input string
for i=0,w do blank=blank+' '    ; make a long blank word

for i=0,nrow-1 do begin		; do each row
    if dow then begin           ; increment down columns
        ii=i+jj
        if ii[ncol-1] gt nin-1 then ii=ii[0:ncol-2] ; delete last column
        k=n_elements(ii)        ; number columns in this row
    endif else begin            ; increment across rows
        i1=i*ncol
	i2=(i1+ncol-1) < (nin-1) 	; last for this row
        k=i2-i1+1		; # in this row
        ii=indgen(k)+i1          ; index of items in this row
    endelse
    s=strarr(2,k)
    if ali then begin
        bb=strtrim(ss[ii],2)    ; remove all external blanks
        kk=strlen(bb)
        for j=0,k-1 do if kk[j] lt w then bb[j]=bb[j]+strmid(blank,0,w-kk[j])
        s[j2,*]=bb
;            if i eq 24 then stop
    endif else begin
        s[j2,*]=ss[ii]
    endelse
    if doct then qq=DEC2OCT(ii) else qq=strtrim(string (ii),1)
    s[j1,*]=qq
    print,s,format=fmt
endfor
;; stop
return
end
