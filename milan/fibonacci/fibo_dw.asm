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
const0 db 'Fibonacci number #', 0x00
const1 db ': ', 0x00
	; table of constants ends here

section .bss use32
	@console_STDIN RESD 1
	@console_STDOUT RESD 1
	@console_buf RESB @buf_size
	@console_write_buf RESB 1
	@console_write_buf_written RESD 1
	; table of variables begins here
var0: RESD 1
var1: RESD 1
var2: RESB 1
var3: RESD 1
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
	
	
@println_str:
	push ebp
	mov ebp, esp
	push dword [ebp + 8]
	call @print_str
	call @print_newline
	pop ebp
	add esp, byte 8
	jmp [esp - 8]
	
@println_int32:
	push ebp
	mov ebp, esp
	push dword [ebp + 8]
	call @print_int32
	call @print_newline
	pop ebp
	add esp, byte 8
	jmp [esp - 8]
	
@println_int32u:
	push ebp
	mov ebp, esp
	push dword [ebp + 8]
	call @print_int32u
	call @print_newline
	pop ebp
	add esp, byte 8
	jmp [esp - 8]

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
push dword 0
pop eax
mov dword [var0], eax
push dword 1
pop eax
mov dword [var1], eax
push dword 0
pop eax
and eax, 000000FFh
push eax
pop eax
mov byte [var2], al
push dword 1
pop eax
mov dword [var3], eax
push dword const0
call @print_str
push eax
pop eax
mov dword eax,[var3]
push eax
call @print_int32
push eax
pop eax
push dword const1
call @print_str
push eax
pop eax
mov dword eax,[var1]
push eax
call @println_int32
push eax
pop eax
jmp0:
push dword 1
mov dword eax,[var3]
push eax
pop ebx
pop eax
add eax,ebx
push eax
pop eax
mov dword [var3], eax
push eax
pop eax
push dword const0
call @print_str
push eax
pop eax
mov dword eax,[var3]
push eax
call @print_int32
push eax
pop eax
push dword const1
call @print_str
push eax
pop eax
mov eax,0
mov byte al,[var2]
push eax
push dword 0
pop ebx
pop eax
push eax
pop eax
cmp eax, ebx
jz jmp1
push dword 0
jmp jmp2
jmp1:
push dword 1
jmp2:
pop eax
cmp eax,0
jz jmp3
mov dword eax,[var0]
push eax
mov dword eax,[var1]
push eax
pop ebx
pop eax
add eax,ebx
push eax
pop eax
mov dword [var0], eax
push eax
call @println_int32
push eax
pop eax
jmp jmp4
jmp3:
mov dword eax,[var0]
push eax
mov dword eax,[var1]
push eax
pop ebx
pop eax
add eax,ebx
push eax
pop eax
mov dword [var1], eax
push eax
call @println_int32
push eax
pop eax
jmp4:
push dword 1
mov eax,0
mov byte al,[var2]
push eax
pop eax
push eax
pop ebx
pop eax
sub eax,ebx
push eax
pop eax
mov byte [var2], al
push eax
pop eax
mov dword eax,[var0]
push eax
push dword 10000
pop ebx
pop eax
cmp eax, ebx
jl jmp5
push dword 0
jmp jmp6
jmp5:
push dword 1
jmp6:
mov dword eax,[var1]
push eax
push dword 10000
pop ebx
pop eax
cmp eax, ebx
jl jmp7
push dword 0
jmp jmp8
jmp7:
push dword 1
jmp8:
pop ebx
pop eax
and eax,ebx
push eax
pop eax
cmp eax,0
jnz jmp0
	; your program ends here
	
	push dword 0
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
	