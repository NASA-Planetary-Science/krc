PRO getp, name, var,low,high
;_Titl  GETP  modify single numeric value; with prompt and limit tests
;_Args
; name  in.   String, Parameter name
; var   both. numeric scalar parameter value
; low   in.   Minimum allowed value
; high  in.   Max allowed value. If le low, no test applied.
;_Desc
; entering a 77 will cause no change
;_Hist  98jul13  HHKieffer
; 2001aug27 HK upgrade comments
;_End


on_ioerror,bad
get: 
test=var
READ,test,prompt=name+': now='+string(test)+' > '	
if fix(test) eq 77 then return

if low lt high then begin ; there is a valid range
  if (test lt low) or (test gt high) then begin
     print,'Allowed range is',+string(low)+string(high)
     goto, get
  endif
endif

var=test
return

bad:
print,'Read error occured; check and try again'
goto, get
end
