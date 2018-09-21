;
; code snippet for Windows 7 x86, instruction set .i386
; used in MILAN language compiler
@buf_size equ 64
@STDIN equ -10
@STDOUT equ -11
@STDERR equ -12

cpu 386

bits 32

extern _MessageBoxA@16  ; user32.dll
extern _ExitProcess@4  ; kernel32
extern _ReadConsoleA@20
extern _WriteConsoleA@20
extern _GetStdHandle@4

section	.data use32
	@console_buf_size_read dd 0x00
	@console_buf_index dd 0x00
	; table of constants begins here
	; (insert)
	string1 db 'The result is ', 0x00
	; table of constants ends here

section .bss use32
	@console_STDIN RESD 1
	@console_STDOUT RESD 1
	@console_buf RESB @buf_size
	@console_write_buf RESB 1
	@console_write_buf_written RESD 1
	; table of variables begins here
	; (insert)
	A: RESD 1 ; line 2
	B: RESD 1 ; line 3
	C: RESD 1 ; line 4
	; table of variables ends here

section	.text
	global begin
	
@getc:
	mov ebx, [@console_buf_index]
	cmp ebx, [@console_buf_size_read]
	jnz _@getc_ret
	
	mov dword [@console_buf_index], 0
	push dword 0
	push dword @console_buf_size_read
	push dword @buf_size
	push dword @console_buf
	push dword [@console_STDIN]
	call _ReadConsoleA@20
	cmp eax,0
	jz @input_err
_@getc_ret:
	mov eax, 0
	mov ebx, [@console_buf_index]
	mov al, [@console_buf + ebx]
	inc ebx
	mov [@console_buf_index], ebx
	ret
	
@putc:
	push ebp
	mov ebp, esp
	mov eax, [ebp + 8]
	mov byte [@console_write_buf], al
	push dword 0
	push dword @console_write_buf_written
	push dword 1
	push dword @console_write_buf
	push dword [@console_STDOUT]
	call _WriteConsoleA@20
	cmp eax,0
	jz @output_err
	pop ebp
	add esp, byte 8
	jmp [esp - 8]

@print_str:
	push ebp
	mov ebp, esp
	mov ebx, [ebp + 8]
	mov eax, 0
@print_str_while_begin:
	cmp byte [ebx],0
	jz @print_str_while_end
	mov byte al, [ebx]
	push eax
	call @putc
	inc ebx
	jmp @print_str_while_begin
@print_str_while_end:
	pop ebp
	add esp, byte 8
	jmp [esp - 8]
	
@print_int32:
	push ebp
	mov ebp, esp
	mov eax, [ebp + 8]
	mov ebx, eax
	and eax, 0x80000000
	jz @print_int32u
	neg ebx
	push dword '-'
	call @putc
@print_int32u:
	push ebx
	call @print_uint32
	pop ebp
	add esp, byte 8
	jmp [esp - 8]
	
	
@print_uint32:
	push ebp
	mov ebp, esp
	mov eax, [ebp + 8]
	mov ebx, 10
	mov ecx, 0
	push dword 0
@print_int_dowhile_begin:
	mov edx, 0
	div ebx
	add byte dl, '0'
	push edx
	inc ecx
	cmp dword eax, 0
	jnz @print_int_dowhile_begin
@print_int_putc_loop:
	mov eax, [esp]
	cmp eax, 0
	jz @print_int_putc_loop_end
	call @putc
	jmp @print_int_putc_loop
@print_int_putc_loop_end:
	pop eax
	pop ebp
	add esp, byte 8
	jmp [esp - 8]
	
@print_newline:
	push dword 13
	call @putc
	push dword 10
	call @putc
	ret

begin:
	mov dword [@console_buf_index], 0
	mov dword [@console_buf_size_read], 0
	push dword @STDIN
	call _GetStdHandle@4
	mov [@console_STDIN],eax
	cmp eax, -1
	je @get_handle_err
	push dword @STDOUT
	call _GetStdHandle@4
	mov [@console_STDOUT],eax
	cmp eax,-1
	je @get_handle_err
	
	; your program goes here
	; (insert)
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
	push dword 0
	jmp @quit_program
	; your program ends here
	
	push dword -1
@quit_program:
	call  _ExitProcess@4
@get_handle_err:
	push dword 1
	call  _ExitProcess@4
@input_err:
	push dword 2
	call  _ExitProcess@4
@output_err:
	push dword 3
	call  _ExitProcess@4
@throw_return_value:
	push dword eax
	call  _ExitProcess@4
	