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
extern _WriteConsoleA@20 ; также пять параметров
extern _GetStdHandle@4 ; для получения STDIN, STDOUT и STDERR

; Data segment
section	.data use32
	title db 'Assembler', 0x00  ;title of msgbox
	message db 'Printed character is: "X"', 0x00  ;string to be printed
	;len equ $ - msg     ;length of the string
	console_buf_size_read dd 0x00 ; console actually read to buffer
	console_buf_index dd 0x00 ; buffer index for getc

; Uninitialized Data Segment
section .bss use32
	;https://www.tutorialspoint.com/assembly_programming/assembly_variables.htm
	int1: RESD 1 ;reserve one doubleword
	int2: RESD 1 ;reserve one doubleword
	int3: RESD 1 ;reserve one doubleword
	
	console_stdin RESD 1 ; console stdin handle
	console_stdout RESD 1 ; console stdout handle
	console_buf RESB buf_size ; console buffer
	console_write_buf RESB 1; буффер записи (будет только один байт для putc)
	console_write_buf_written RESD 1; сколько записано байт

; Code segment
section	.text
	global _start     ;must be declared for linker (ld)
	
; функция, реализующая приём одного символа со стандартного потока ввода
getc:
	mov ebx, [console_buf_index]
	cmp ebx, [console_buf_size_read]
	jnz _getc_ret ; maybe jne?
	
	mov dword [console_buf_index], 0 ; refresh buffer counter
	push dword 0 ; pInputControl = NULL
	push dword console_buf_size_read ; lpNumberOfCharsRead
	push dword buf_size ; nNumberOfCharsToRead, читаем значения в буфер заготовленного размера
	push dword console_buf ; lpBuffer, место, куда сложится прочитанный символ
	push dword [console_stdin] ; соответственно, передаём хэндл консоли на чтение
	call _ReadConsoleA@20 ; вызываем чтение с консоли
	;jmp throw_return_value ; probe string. Type "echo %errorlevel%" in batch file
	cmp eax,0;если функция вернула ноль, то что-то не так...
	jz input_err ; jump to input_err if return value is zero
_getc_ret:
	mov eax, 0
	mov ebx, [console_buf_index]
	mov al, [console_buf + ebx]
	inc ebx
	mov [console_buf_index], ebx
	ret
	
; функция, реализующая вывод одного символа в стандартный поток вывода
; TODO1 -- буффер-аккумулятор? (наверное, нет)
; TODO2 -- прямое обращение к стеку через esp, ebp вместо pop?
putc:
	; all about magic:
	; https://www.nasm.us/doc/nasmdoc9.html
	; https://stackoverflow.com/questions/14185381/ebp-esp-and-stack-frame-in-assembly-with-nasm
	push ebp ;magic (saving old value of ebp)
	mov ebp, esp ;also magic (new value of base pointer is current pointer)
	; [ebp] -- old ebp
	; [ebp + 4] -- return adress
	; [ebp + 8] -- first parameter
	; ... etc
	mov eax, [ebp + 8] ; принимаем входной параметр (это char)
	mov byte [console_write_buf], al
	push dword 0 ; должен быть NULL по документации
	push dword console_write_buf_written ; количество записанных символов
	push dword 1 ; записать ОДИН символ
	push dword console_write_buf ; вот этот символ
	push dword [console_stdout] ; соответственно, передаём хэндл консоли на запись
	call _WriteConsoleA@20 ; вызываем запись в консоль
	cmp eax,0;если функция вернула ноль, то что-то не так...
	jz input_err ; jump to input_err if return value is zero
	pop ebp ;third time magic
	; чистим стек вызова от всяких параметров
	add     esp, byte 8      ; добавляем (количество параметров + 1) * 4, где затирается адрес вызова
	jmp [esp - 8] ; basicly ret (see below:) (т.е. вызываем [esp - (количество параметров + 1) * 4]) 
	;push dword [esp - 8]
	;ret
	
	
_start:	            ;tells linker entry point
	; Init variables
	; for _getc function
	mov dword [console_buf_index], 0
	mov dword [console_buf_size_read], 0
	
	; Step 1: get STDIN и STDOUT handle
	; return value usage: https://stackoverflow.com/questions/6171172/return-value-of-a-c-function-to-asm
	push dword STDIN ; pass parameter: we would like to get STDIN resource handle
	call _GetStdHandle@4 ; call the WINAPI function
	mov [console_stdin],eax ; the return value expected to appear in eax, copy it into variable
	cmp eax,-1 ; если функция вернула -1, то что-то не так...
	je get_handle_err
	push dword STDOUT
	call _GetStdHandle@4
	mov [console_stdout],eax
	cmp eax,-1
	je get_handle_err
	
_step3:
	; Step3: making output message
	call getc
	cmp al, 10
	jz quit_program
	mov byte [message + 23],al
	
	push eax
	call putc
	
	push dword 0 ; style of message box
	push dword title  ; address of title of message box
	push dword message  ; address of text in message box
	push dword 0 ; handle owner window
	call _MessageBoxA@16 ; call function
	jmp _step3
	

quit_program:
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

   