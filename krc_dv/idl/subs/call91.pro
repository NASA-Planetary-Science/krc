function call91, kitel,textin,nop
;_Titl  CALL91  Quick access to the functions of KON91
; kitel in.   String. Prior action by calling program; passed to KON91
; textin  in_   String to append here
; nop   in_   Flag, do NOT add ID kitel+text in upper left corner
; func. out.  Flag: set true if caller should stop
;_Calls  Utility: KON91
; Check KON99 for actions already assigned
;_Hist 2015oct01 Hugh Kieffer  To allow plots to files 
; 2017dec07 HK Add keyword  lab
;_End                .comp call91

ptitl='kon91'     ; reasonable taget for a source file. Should never be needed
prior=['1','1']   ; impossible values, to initiate MAKE99 if called
kon=0             ; type definition
kons=[-1,-1]      ; dummy
;===============================================================================
q=kitel
if keyword_set(textin) then begin
  if strmid(textin,0,1) eq '@' then text=textin else text=':'+textin
  q=kitel+text ; location of call to this routine
  print,'<<call91>> ',q
endif
if not keyword_set(nop) then xyouts,0.01,0.98,q,/normal,charsize=1.5

ask: ;------------ top of action loop ------------ interactive parameter change

READ, kon, prompt=' KON91 select: 1or-2=done 0=stopInCall > '
if abs(kon) le 2 then if kon eq 0 then return,1B else return,0B

KON91,ptitl,prior,hold,kon,kons,kitel ;=KON99 < needed by MAKE99
goto, ask

end
