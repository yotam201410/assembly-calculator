IDEAL
MODEL small
STACK 100h
DATASEG
; --------------------------
startmessage db "welcome to the calculator.$"
resultmessage db "the result is:$"
asknumber db "please enter a number lower then 32,767.$"
askaction db "enter action: + , - , * , / , % , ^ , R , ! , L or C to clear and esc to leave.$"
moreExplanation db " + addition (gets two numbers) num1+num2",10,13
				db " - subtraction (gets two numbers) num1-num2",10,13
				db " * multipaction (gets two numbers) num1*num2",10,13
				db " / devision (gets two numbers) num1/num2",10,13
				db " % modulo (gets two numbers) num1%num2",10,13
				db " ^ power (gets two numbers) num1^num2",10,13
				db " R root (gets two numbers) num1^(1/num2)",10,13
				db " ! factorial (gets one number) !num1",10,13
				db " L log (gets two numbers the first the number and the second the base)",10,13
				db " ln(num1)/ln(num2)",10,13
numerror db "the number has to be positive and under 32767. returning you to the start$"
exceededNumLimit db "over 32767. returning you to the start.$"
devideByZeroError db "cant devide by zero. returning you to the start.$"
logbaseerror db "you cant have a base of 1 or 0. returning you to the start.$"
lognumerror db "you cant get the log of 0. returning you to the start.$"
action db ?
strnum1 db 8 dup(?)
strnum2 db 8 dup(?)
num2 dw ?
num1 dw ?
numstr db '$$$$$$'
min_counter dw 0
help_var dw ?
counter dw ?
; --------------------------
CODESEG
;---------------------------------------------------------------------------      
; Convert number to a string of digits for print. Requires in DATASEG:
; numstr db '$$$$$'. Proc dollars fills numstr with $ signs and proc
; number2str receive a number in ax and create the string in numstr.

proc dollars        ;No parms. Fills area in memory with "$" 
	push cx  
	push di
	mov  cx, 6
	mov  di, offset numstr
	dollars_loop:
	mov  bl, '$'
	mov  [ di ], bl
	inc  di
	loop dollars_loop
	pop  di
	pop  cx	
	ret
endp dollars
;---------------------------------------------------------------------------
proc number2string  
	push cx
	push ax
	push dx
	push bx
	call dollars  ;FILL STRING WITH $.
	mov  bx, 10   ;DIGITS ARE EXTRACTED DIVIDING BY 10.
	mov  cx, 0    ;COUNTER FOR EXTRACTED DIGITS.
;EXTRACT DIGITS               
	cycle1:       
	mov  dx, 0    ;NECESSARY TO DIVIDE BY BX.       
	div  bx       ;DX:AX / 10 = AX:QUOTIENT DX:REMAINDER.
	push dx       ;PRESERVE DIGIT EXTRACTED FOR LATER.
	inc  cx       ;INCREASE COUNTER FOR EVERY DIGIT EXTRACTED.
	cmp  ax, 0    ;IF NUMBER IS NOT YET ZERO, LOOP.
	jne  cycle1   ; 
;NOW RETRIEVE PUSHED DIGITS.
	mov si, offset numstr
	cycle2:       
	pop  dx           
	add  dl, 48   ;CONVERT DIGIT TO CHARACTER. 48 is '0' in ASCII 
	mov  [ si ], dl
	inc  si
	loop cycle2  
	pop bx
	pop dx
	pop ax
	pop cx
	ret
endp number2string

proc NewLine
	push ax
	push dx
	mov dl,10
    mov ah,2
    int 21h
    mov dl,13
    mov ah,2
    int 21h
	pop dx
	pop ax
	ret
endp NewLine

proc PrintMinus
	push ax
	push dx
    mov dl,'-'
    mov ah,2
    int 21h
	pop dx
	pop ax
	ret
endp PrintMinus

proc PrintString ;getting the offset of the string in dx
	push ax
    mov ah, 9h
    int 21h
	pop ax
	ret
endp PrintString

proc InputAction;getting an action and putting it in the action var 
	push ax
	mov ah, 1h
    int 21h
    mov dl,al
	pop ax
	mov [action], dl
	ret
endp InputAction

proc InputString;putting a string of 5 chars in the start index that was given dx
	push ax
    mov bx,dx
    mov [byte ptr bx], 6
    mov ah, 0ah
    int 21h
	pop ax
	ret
endp InputString

proc MakeNumber;gets the offset of the string in dx & the number will be in ax
	push bx
	push cx
	push dx
	mov bx,dx;bx= the memory position of the start of the stirng (maximum number of letters)
	inc bx; changing bx to be how much numebrs there are actual in it
	xor cx,cx
	add cl, [bx];putting the amount of chars in the number
	xor ax,ax
	inc bx;changing bx to the start of the number
	Adding:
	mov dx,10
	mul dx
	xor dx,dx
	add dl,[bx]
	sub dl,'0'
	add ax, dx
	inc bx
	loop Adding
	pop dx
	pop cx
	pop dx
	ret
endp MakeNumber

proc Absulte; gets a in number in ax and return the absulte number in ax
	cmp ax,0
	jl Minus
	ret
	Minus:
	not ax
	inc ax
	ret
endp Absulte

proc PrintResultMessage
	mov dx, offset resultmessage
	call PrintString
	call NewLine
	ret
endp

start:
	mov ax, @data
	mov ds, ax
; -------------------------
	;printing the start message
	mov dx, offset startmessage
	call PrintString
	call  NewLine
	mov dx, offset moreExplanation
	call PrintString
	call NewLine
	;asking for the first number
	FirstNum:
		mov dx, offset asknumber
		call PrintString
		call NewLine
		;getting first number
		mov dx,offset strnum1
		call InputString
		call  NewLine
		;changing it from string in to an integer, dx = start of the strnum1
		call MakeNumber
		mov [num1],ax
		cmp ax,32767
		jbe AskAgain
		mov dx, offset exceededNumLimit
		call PrintString
		call NewLine
		jmp FirstNum
	;asking second number
	AskAgain:
		;asking for the action
		mov dx,offset askaction
		call PrintString
		call  NewLine
		;getting the action
		call InputAction
		call NewLine
		cmp [action],'C'
		je FirstNum
		cmp [action], 27
		je Convinance
		cmp [action],'!'
		je Convinance
		secondNum:
		mov dx, offset asknumber
		call PrintString
		call NewLine
		;getting second number
		mov dx,offset strnum2
		call InputString
		call  NewLine
		;changing it from string in to an integer, dx = start of the strnum2
		call MakeNumber
		mov [num2],ax
		cmp ax,32767
		jbe secondNumNotOver
		mov dx, offset exceededNumLimit
		call PrintString
		call NewLine
		jmp secondNum
	;going to what action it is
	secondNumNotOver:
		cmp [action], '+'
		je Addition
		jmp Convinance

Addition:
	call PrintResultMessage
	;add the numbers
	mov ax, [num1]
	add ax, [num2]
	cmp ax,32767
	jbe notOver
	mov dx, offset exceededNumLimit
	call PrintString
	call NewLine
	jmp FirstNum
	notOver:
		;move the result to the first num
		mov [num1],ax
		;print the result and go back to the start
		call number2string
		mov dx,offset numstr
		call PrintString
		call NewLine
		jmp AskAgain

Convinance:
	cmp [action],'-'
	je Subtraction
	cmp [action],'*'
	je Multipication
	jmp Convinance2

Subtraction:
	call PrintResultMessage
	mov ax, [num2]
	;check if the result is a minus number
	cmp ax, [num1]
	jg SubMinus
	;print the result if its positive or 0
	sub [num1],ax
	mov ax, [num1]
	call number2string
	mov dx,offset numstr
	call PrintString
	call NewLine
	jmp AskAgain
	;print the result of the minus number but the num1 becomes the ubsolote value of the result
	SubMinus:
		sub ax,[num1]
		mov [num1],ax
		call PrintMinus
		call number2string
		mov dx,offset numstr
		call PrintString
		call NewLine
	jmp AskAgain

Multipication:
	call PrintResultMessage
	;multiplaying the numbers
	xor dx,dx
	mov ax, [num1]
	mul [num2]
	mov [num1],ax
	;checking if the result is over 32767 is so printing an error
	cmp ax,32767
	jbe CheckNoExceededNumLimitMultipication
	mathErorrMul:
	mov dx, offset exceededNumLimit
	call PrintString
	call NewLine
	jmp FirstNum
	CheckNoExceededNumLimitMultipication:
		cmp dx,0
		je NoExceededNumLimitMultipication
		jmp mathErorrMul
	NoExceededNumLimitMultipication:
		call number2string
		mov dx,offset numstr
		call PrintString
		call NewLine
		jmp AskAgain

Convinance2:
	cmp [action],'/'
	je Division
	cmp [action],'^'
	je Pow
	jmp Convinance3

Division:
	call PrintResultMessage
	xor ax,ax
	;checking if the derivative is 0 if so printing an error
	cmp ax,[num2]
	je devideZero
	mov ax,[num1]
	xor dx,dx
	;dividing the number
	div [num2]
	mov [num1],ax
	mov ax, [num1]
	;printing the number and jumping the the start
	call number2string
	xor ax, ax
	mov dx,offset numstr
	call PrintString
	call NewLine
	jmp AskAgain
	devideZero:
		mov dx,offset devideByZeroError
		call PrintString
		call NewLine
		jmp FirstNum

Pow:
	call PrintResultMessage
	push cx
	push ax
	cmp [num1] ,0
	jne noZeroPowZero
	cmp [num2],0
	jne noZeroPowZero
	mov ax,1
	mov [num1],ax
	jmp continuePow
	;getting ready to loop by having at cx the pow and at ax the number
	noZeroPowZero:
		mov cx, [num2]
		xor dx,dx
		mov ax,1
		MulLoop:
		
			mul [num1]
			;checking if the the pow gets over the limit of the caculation power
			cmp dx,0
			jne exitPowExceededNumLimit
		loop MulLoop
		mov [num1],ax 
		;checking if the the pow gets over the limit of the caculation power
		cmp dx,0
		je continuePow
		exitPowExceededNumLimit: ; printing an error msg if the number is bigger then 32767 and jumping to the start
			mov dx, offset exceededNumLimit
			call PrintString
			call NewLine
			jmp FirstNum
		checkContinuePow:
			cmp ax, 32767
			jbe continuePow
			jmp exitPowExceededNumLimit
		continuePow: ;printing the result and jumping to the start
			call number2string
			mov dx, offset numstr
			call PrintString
			call NewLine
			pop cx
			pop ax
			jmp AskAgain
Convinance3:
cmp [action],'R'
je Root
jmp Convinance4

Root:;bx = og_num, ax=num2, cx = i,y , dx = min_num, min_counter = min_counter
;ax being the multiplied number, cx being the first and second loop indexes, dx being the min number
	call PrintResultMessage
	push ax
	push cx
	push bx
	push dx
	xor ax,ax
	xor cx,cx
	xor dx,dx
	xor bx,bx
	mov [counter],0
	mov dx, 32767
	mov cx, [num2]
	cmp cx,0
	jne noRootError
	rootError:
		mov dx, offset devideByZeroError
		call PrintString
		call NewLine
		pop dx
		pop bx
		pop cx
		pop ax
		jmp FirstNum
	noRootError:
		mov cx, [num1]
		cmp cx,0
		je RootZero
		FindingTheRoot:
			mov ax,1 ;readieng the ax to multipication
			push cx;saving cx
			mov bx, [counter]
			mov cx, [num2];preapering cx to start mul loop
			mov [help_var],dx;saving dx 
			xor dx,dx
			PowLoop:
				mul bx
			loop PowLoop
			;checking if the result is more then the number and check if the result is closer to the num then the last result if so exit else keep looking for the closest number
			mov cx, [num1]
			sub cx,ax
			mov ax,cx
			call Absulte
			pop cx
			mov dx,[help_var]
			cmp ax,dx
			jg exitRootLoop
			mov [min_counter],cx
			mov dx,ax
			inc [counter]
		loop FindingTheRoot
		exitRootLoop:
			;printing the result
			mov ax, [min_counter]
			sub ax, [num1]
			call Absulte
			mov [num1],ax
			call number2string
			mov dx,offset numstr
			call PrintString
			call NewLine
			pop dx
			pop bx
			pop cx
			pop ax
			jmp AskAgain
	RootZero:
		mov ax,cx
		mov [num1],ax
		call number2string
		mov dx,offset numstr
		call PrintString
		call NewLine
	pop dx
	pop bx
	pop cx
	pop ax
	jmp AskAgain

Convinance4:
cmp [action],'%'
je Modulo
cmp [action],'!'
je Factorial
jmp Convinance5

Modulo:
	call PrintResultMessage
	mov ax,[num1]
	mov cx,[num2]
	;checking that we wouldnt divide by 0
	cmp cx,0
	je devideByZeroErrorM
	;divide
	xor dx,dx
	div cx
	;printing the result
	mov [num1],dx
	mov ax, [num1]
	call number2string
	mov dx,offset numstr
	call PrintString
	call NewLine
	jmp AskAgain
	devideByZeroErrorM: ;printing the error if we divide by 0
		mov dx,offset devideByZeroError
		call PrintString
		call NewLine
		jmp FirstNum

Factorial:
	call PrintResultMessage
	;prepering the loop
	mov cx,[num1]
	mov ax, 1
	mov bx,1
	xor dx,dx
	cmp cx,8
	je exitExceededNumLimitFractorial
	;loop
	multiplaying:
		mul bx ;multiplaying and adding 
		add bx,1
		cmp dx, 0;leaving the loop if the result excceds 32767
		jne exitExceededNumLimitFractorial
		cmp ax,32767
		jg exitExceededNumLimitFractorial
	loop multiplaying
	mov [num1],ax
	;checking if we didnt exceed 32767
	cmp dx, 0
	je checkFactorialError
	;printing the error message
	exitExceededNumLimitFractorial:
		mov dx, offset exceededNumLimit
		call PrintString
		call NewLine
		jmp FirstNum
	;printing the result
	checkFactorialError:
		cmp ax, 32767
		jbe printNumFractorial
		jmp exitExceededNumLimitFractorial
	printNumFractorial:
		call number2string
		mov dx,offset numstr
		call PrintString
		call NewLine
		jmp AskAgain

Convinance5:
	cmp [action],"L"
	je Log
	jmp exit
	
Log:; minnum =help_var, num2 = ax, i=cx
	call PrintResultMessage
	cmp [num2],1
	jbe logbaseerrorl
	cmp [num1],0
	je lognumerrorl
	mov cx,1
	cmp [num1],1
	je foundLog
	mov [help_var],32767
	mov ax,1
	findingTheLog:
		push cx
		MultipicationLog:
			mul [num2]
		loop MultipicationLog
		pop cx
		sub ax, [num1]
		call Absulte
		cmp ax, [help_var]
		jg foundLog
		mov [help_var],ax
		mov ax,1
		inc cx
		cmp cx, [num1]
		jne findingTheLog
	foundLog:
		dec cx
		mov ax, cx
		mov [num1],ax
		call number2string
		mov dx,offset numstr
		call PrintString
		call NewLine
		jmp AskAgain
	logBaseErrorL:
		mov dx,offset logbaseerror
		call PrintString
		call NewLine
		jmp FirstNum
	logNumErrorl:
		mov dx,offset lognumerror
		call PrintString
		call NewLine
		jmp FirstNum

exit:
	mov ax, 4c00h
	int 21h

END start