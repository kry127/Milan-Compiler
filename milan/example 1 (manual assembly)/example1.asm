; preambule for windows .i386
; consts usage https://www.tutorialspoint.com/assembly_programming/assembly_constants.htm
; Этот код -- родоначальник сниппета "core_snippet.asm". Здесь имеются все комментарии по используемым частям кода
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

section	.data use32
	console_buf_size_read dd 0x00 ; console actually read to buffer
	console_buf_index dd 0x00 ; buffer index for @getc
	; first affection of source code (constant table)
	string1 db 'The result is ', 0x00

section .bss use32
	console_stdin RESD 1 ; console stdin handle
	console_stdout RESD 1 ; console stdout handle
	console_buf RESB buf_size ; console buffer
	console_write_buf RESB 1; буффер записи (будет только один байт для @putc)
	console_write_buf_written RESD 1; сколько записано байт
	;second affection of source code (variable table)
	A: RESD 1 ; line 2
	B: RESD 1 ; line 3
	C: RESD 1 ; line 4

section	.text
	global _start
	
; функция, реализующая приём одного символа со стандартного потока ввода
@getc:
	mov ebx, [console_buf_index]
	cmp ebx, [console_buf_size_read]
	jnz _@getc_ret ; maybe jne?
	
	mov dword [console_buf_index], 0 ; refresh buffer counter
	push dword 0 ; pInputcontrol = NULL
	push dword console_buf_size_read ; lpNumberOfCharsRead
	push dword buf_size ; nNumberOfCharsToRead, читаем значения в буфер заготовленного размера
	push dword console_buf ; lpBuffer, место, куда сложится прочитанный символ
	push dword [console_stdin] ; соответственно, передаём хэндл консоли на чтение
	call _ReadConsoleA@20 ; вызываем чтение с консоли
	;jmp throw_return_value ; probe string. Type "echo %errorlevel%" in batch file
	cmp eax,0;если функция вернула ноль, то что-то не так...
	jz input_err ; jump to input_err if return value is zero
_@getc_ret:
	mov eax, 0
	mov ebx, [console_buf_index]
	mov al, [console_buf + ebx]
	inc ebx
	mov [console_buf_index], ebx
	ret
	
; функция, реализующая вывод одного символа в стандартный поток вывода
; TODO1 -- буффер-аккумулятор? (наверное, нет)
; TODO2 -- прямое обращение к стеку через esp, ebp вместо pop?
@putc:
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
	jz output_err ; jump to input_err if return value is zero
	pop ebp ;third time magic
	; чистим стек вызова от всяких параметров
	add     esp, byte 8      ; добавляем (количество параметров + 1) * 4, где затирается адрес вызова
	jmp [esp - 8] ; basicly ret (see below:) (т.е. вызываем [esp - (количество параметров + 1) * 4]) 
	;push dword [esp - 8]
	;ret

; печатает строковую константу
@print_str:
	push ebp
	mov ebp, esp
	mov ebx, [ebp + 8] ; принимаем указатель на строку
	mov eax, 0 ; здесь принимаются символы на печать
@print_str_while_begin:
	cmp byte [ebx],0
	jz @print_str_while_end
	mov byte al, [ebx]
	push eax
	call @putc ; печатаем символ
	inc ebx
	jmp @print_str_while_begin
@print_str_while_end:
	pop ebp
	add esp, byte 8
	jmp [esp - 8]
	
; печатает знаковое число
@print_int32:
	push ebp
	mov ebp, esp
	mov eax, [ebp + 8] ; принимаем целое число на печать
	mov ebx, eax ; сохраняем его также в регистре ebx для тестирования
	and eax, 0x80000000 ; проверяем знак числа
	jz @print_int32u ; если оно положительное, пропускаем замену знака
	neg ebx ; меняем знак у числа
	push dword '-'
	call @putc ; печатаем знак "минус" (стоит учесть, что там не стоит менять регистр ebx)
@print_int32u:
	push ebx ; отправляем на печать без изменения знака
	call @print_uint32
	pop ebp
	add esp, byte 8
	jmp [esp - 8]
	
	
; печатает беззнаковое число
@print_uint32:
	push ebp
	mov ebp, esp
	mov eax, [ebp + 8] ; принимаем целое число на печать
	mov ebx, 10 ; здесь принимаются символы на печать
	mov ecx, 0; обнуляем счётчик
	push dword 0 ; добавляем символ конца строки в стек (необходимо для печати)
@print_int_dowhile_begin:
	mov edx, 0 ; обнуляем dx
	div ebx ; делим на 10, в dl будет остаток
	add byte dl, '0' ; добавляем код символа '0'
	push edx ; помещаем в стек
	inc ecx; в стеке на один символ больше для печати
	cmp dword eax,0 ; смотрим, осталось ли что-то от делимого
	jnz @print_int_dowhile_begin
	; вывод символов из стека
@print_int_putc_loop:
	mov eax, [esp] ; читаем текущее значение, которое получит функция @putc
	cmp eax, 0
	jz @print_int_putc_loop_end
	call @putc ; печатаем символ (функция возьмёт параметр из стека сама! :)
	jmp @print_int_putc_loop ; повторяем до конца строки
@print_int_putc_loop_end:
	pop eax ; извлекаем символ конца строки из стека
	pop ebp
	add esp, byte 8
	jmp [esp - 8]
	
@print_newline:
	push dword 13
	call @putc
	push dword 10
	call @putc
	ret

begin: ; this is a keyword for MILAN, so should be OK. Use of @ for entry point is prohibited by GoLink =(
	; Init variables
	; for _@getc function
	mov dword [console_buf_index], 0
	mov dword [console_buf_size_read], 0
	; get STDIN и STDOUT handles
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
	
; here starts work of compiler! :)
	mov dword [A],4 ; line 2
	mov dword [B],3 ; line 3
	; calculate line 5 expression
	mov dword eax,[A]
	push eax ; {A}
	push dword 5 ; {A,5}
	pop ebx ; {A}
	pop eax ; {}
	add eax, ebx
	push eax ; {A+5}
	mov dword eax,[B]
	push eax ; {A+5,B}
	push dword 7 ; {A+5,B,7}
	pop ebx ; {A+5,B}
	pop eax ; {A+5}
	sub eax, ebx
	push eax ; {A+5,B-7}
	pop ebx ; {A+5}, ebx <- B-7
	pop eax ; {} eax <- A+5
	mul ebx ; eax <- (A+5)(B-7)
	push eax ; {(A+5)*(B-7)}
	; assign value to variable C
	pop eax
	mov dword [C], eax
	push eax
	; end of expression evaluation
	pop eax ; ends always with this to clear stack
	; line 6
	push dword string1
	call @print_str ; печатаем нуль-терминированную строку
	mov dword eax, [C]
	push eax
	call @print_int32 ; печатаем число
	call @print_newline ; новая строка
	
; postamble
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
output_err:
	push dword 3 ; set error return value
	call  _ExitProcess@4 ; call function
throw_return_value:
	push dword eax
	call  _ExitProcess@4
