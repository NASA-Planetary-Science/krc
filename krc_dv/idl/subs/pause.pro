PRO pause, delay,text=text,n=n
;_Titl  PAUSE  Pause a fixed time or until key hit
; delay	in.	Delay in seconds. If negative, will wait for response
;                 or if le -2, will stop
; text	in_	If present, will use as prompt. Default is: 'Any key to go' ;
;        if first character is ':' or blank, will append to default
; n	out_	If present, will return n as a single-digit (0:9) integer
;		If delay is non-negative, n will be 0
;	    If user hits non-numeric key, will return integer 0, with message.
;_Desc
; Does nothing if delay=0.
;_Hist 2000apr20 Hugh Kieffer
; 2000may08 HK add text and n options
; 2010oct16 HK Allow stop if delay is -2
; 2015oct30 HK If first character of text is blank or :, appends it to 
;  the default message. And fix problem that input n would be added to text. 
;_End

def='Any key to go'
if keyword_set(text) then begin 
  q=strmid(text,0,1)
  if q eq ' ' or q eq ':' then def=def+text else def=text
endif 
if arg_present(n) then def=def+'. Expect integer'
if delay gt 0 then begin ; pause specific amount of time
    WAIT,delay 
    n=0                         ; set it in case keyword is present
endif else if delay le -2 then STOP $ ;stop
else if delay lt 0  then begin ; wait for user
    print,def
    i=GET_KBRD(1)
    if arg_present(n) then begin
        j=byte(i) & j=j[0]      ; get scalar ASCII code
        if j lt 48 or j gt 57 then begin ; outside decimal range
            print,'Expected an integer 0:9   Will return 0'
            n=0
        endif else n=fix(i)
    endif
endif                           ; if delay was 0, return immediately
return
end
