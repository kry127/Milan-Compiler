; consts usage https://www.tutorialspoint.com/assembly_programming/assembly_constants.htm
buf_size equ 64 ; console buffer size
STDIN equ -10 ; weird constants from official WinAPI documentation
STDOUT equ -11
STDERR equ -12

; tell assembler what cpu type to use
cpu 386

; Tell assembler to generate 32 bit code
bits 32

; Import stuff
; well, with help of this: https://forum.nasm.us/index.php?topic=1473.0
extern _MessageBoxA@16  ;import from user32.dll
extern _ExitProcess@4  ;from kernel32
extern _ReadConsoleA@20 ; пять параметров, все скорее всего DWORD = 4 байта
extern _GetStdHandle@4 ; для получения STDIN, STDOUT и STDERR

; Data segment
section	.data use32
	title db 'Assembler', 0x00  ;title of msgbox
	message db 'Printed character is: "X", read buffer size:    .', 0x00  ;string to be printed
	;len equ $ - msg     ;length of the string

; Uninitialized Data Segment
section .bss use32
	;https://www.tutorialspoint.com/assembly_programming/assembly_variables.htm
	int1: RESD 1 ;reserve one doubleword
	int2: RESD 1 ;reserve one doubleword
	int3: RESD 1 ;reserve one doubleword
	
	console_stdin RESD 1 ; console stdin handle
	console_stdout RESD 1 ; console stdout handle
	console_buf RESB 64 ; console buffer
	console_buf_size_read RESD 1 ; console actually read to buffer

; Code segment
section	.text
	global _start     ;must be declared for linker (ld)
	
_start:	            ;tells linker entry point
	; Step 1: get STDIN handle
	; return value usage: https://stackoverflow.com/questions/6171172/return-value-of-a-c-function-to-asm
	push dword STDIN ; pass parameter: we would like to get STDIN resource handle
	call _GetStdHandle@4 ; call the WINAPI function
	mov [console_stdin],eax ; the return value expected to appear in eax, copy it into variable
	
	; если функция вернула -1, то что-то не так...
	cmp eax,-1
	je get_handle_err
	
	; Step 2: read from console first digit
	; Пример кода взят отсюда: https://eax.me/winapi-console-app/
	push dword 0 ; pInputControl = NULL
	push dword console_buf_size_read ; lpNumberOfCharsRead
	push dword buf_size ; nNumberOfCharsToRead, одного символа для нас хватит
	push dword console_buf ; lpBuffer, место, куда сложится прочитанный символ
	push dword [console_stdin] ; соответственно, передаём хэндл консоли на чтение
	call _ReadConsoleA@20 ; вызываем чтение с консоли
	;jmp throw_return_value ; probe string. Type "echo %errorlevel%" in batch file
	
	;если функция вернула ноль, то что-то не так...
	cmp eax,0
	jz input_err ; jump to input_err if return value is zero
	
	; Step3: making output message
	mov byte al,[console_buf]
	mov byte [message + 23],al
	
	mov word ax, [console_buf_size_read] ; little endian
	mov word dx, [console_buf_size_read+2]
	mov bx, 10
	mov ecx,3 ;вместимость позиций: 3 шт.
_step3_num_outputer: ; делим до посинения
	div bx ; постоянно делим на десятку
	add dl,'0' ; добавляем код символа '0' для вывода
	mov byte [message + ecx + 44],dl ; ax -- quotient, dx -- remainder
	mov dx,0 ; обнуляем остаток для дальнейшего целочисленного деления
	cmp ax, 0 ; http://www.penguin.cz/~literakl/intel/j.html
	je _step4; проверим, что в остатке не ноль (jump if ax zero)
	dec ecx ; следующая позиция для печати
	jnz _step3_num_outputer ; если есть ещё позиции для печати, повторить процесс деления
	
_step4:
	; Step 4: output text to message box
	push dword 0 ; style of message box
	push dword title  ; address of title of message box
	push dword message  ; address of text in message box
	push dword 0 ; handle owner window
	call _MessageBoxA@16 ; call function
	
	; quit program
	push dword 0 ; set return value
	call  _ExitProcess@4 ; call function
get_handle_err:
	push dword 1 ; set error return value
	call  _ExitProcess@4 ; call function
input_err:
	push dword 2 ; set error return value
	call  _ExitProcess@4 ; call function
throw_return_value:
	push dword eax
	call  _ExitProcess@4

   