;
; code snippet for Windows 7 x86, instruction set .i386
; used in MILAN language compiler

; this code is manual assembled from snippet (that is also manually assembled :)
; see block ";your program goes here", everything else is snippet part
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
const0 db 'Enter A:', 0x00
const1 db 'Enter B:', 0x00
const2 db 'Enter C:', 0x00
const10 db 'A=', 0x00
const11 db 'B=', 0x00
const12 db 'C=', 0x00
const13 db 'const=', 0x00
	; table of constants ends here

section .bss use32
	@console_STDIN RESD 1
	@console_STDOUT RESD 1
	@console_pre_buf RESB @buf_size
	@console_buf RESB @buf_size
	@console_write_buf RESB 1
	@console_write_buf_written RESD 1
	; table of variables begins here
var0: RESD 1
var1: RESD 1
var2: RESB 1
var3: RESD 1
var4: RESD 1
	; table of variables ends here

section	.text
	global begin
	
@ungetc:
	mov ecx, [@console_buf_index]
	dec ecx
	mov edx, ecx
	add edx, @console_buf
	cmp edx, @console_pre_buf
	jl @input_err
	mov [@console_buf_index], ecx
	ret
	
@getc:
	mov ebx, [@console_buf_index]
	cmp ebx, [@console_buf_size_read]
	jnz _@getc_ret
	
	mov ecx, @buf_size
@getc_pre_buf_cpy:
	mov edx, [@console_buf + ecx - 1]
	mov [@console_pre_buf + ecx - 1], edx
	loop @getc_pre_buf_cpy
	
	
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
	jz @print_int32_as_unsigned
	neg ebx
	push dword '-'
	call @putc
@print_int32_as_unsigned:
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
	
@println_uint32:
	push ebp
	mov ebp, esp
	push dword [ebp + 8]
	call @print_uint32
	call @print_newline
	pop ebp
	add esp, byte 8
	jmp [esp - 8]

@skip_whitespace:
	call @getc
	cmp al, 21h ; two 16-symbol rows of special symbols, and 20h -> ' '
	jl @skip_whitespace
	call @ungetc
	ret
	
@scan_int32:
	call @skip_whitespace
	call @getc
	cmp al, '-'
	jnz @scan_int32_as_unsigned
	call @scan_int32u
	neg eax
	ret
@scan_int32_as_unsigned:
	call @ungetc
	call @scan_int32u
	ret
	
@scan_int32u:
	mov edx, 0
@scan_int32u_loop:
	push edx
	call @getc
	pop edx
	sub al, '0'
	jl @scan_int32u_ret
	cmp al, '9'
	jg @scan_int32u_ret
	mov ebx, edx
	shl edx, 3
	shl ebx, 1
	add edx, ebx
	add edx, eax
	jmp @scan_int32u_loop
@scan_int32u_ret:
	push edx
	call @ungetc
	pop eax
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
	jmp program
func1:
	; takes one parameter "a"
	; stack now looks like:
	; esp     : return_adress
	; esp + 4 : a
	; ...
	push ebp ; save base pointer (esp:=esp-4, mov [esp], ebp)
	; stack now looks like:
	; esp     : ebp
	; esp + 4 : return_adress
	; esp + 8 : a
	; ...
	mov ebp, esp ; refresh base pointer for func 1
	; stack now looks like:
	; esp,ebp : ebp (old)
	; esp + 4 : return_adress
	; esp + 8 : a
	; ...

	; func1 body
	; just call func2 with param
	
	mov eax, const0 ; ask user to enter first parameter
	push eax
	call @print_str
	call @scan_int32 ; scan parameter from console
	push eax ; push user input as a parameter to next function
	call func2 ; call next function with entered param
	
	; func1 body end
	; assuming, stack is left uncorrupted by function body

	pop ebp ; restore base pointer old value
	; stack now looks like:
	; esp     : return_adress
	; esp + 4 : a
	; ...
	; we shoud clear parameter "a", so we need to do esp := esp + 8
	add esp, 8
	; stack now looks like:
	; esp - 8 : return_adress
	; esp - 4 : a
	; ...
	; now we should jump by the address [stack pointer - 8] in order to return
	jmp [esp - 8]

func2:
	; do the same thing as with func1
	push ebp
	mov ebp,esp
	mov eax, const1 ; ask user to enter second parameter
	push eax
	call @print_str
	call @scan_int32 ; scan parameter from console
	push eax ; push user input as a parameter to next function
	call func3 ; call next function with entered param
	pop ebp
	add esp, 8
	jmp [esp - 8]

func3:
	push ebp
	mov ebp,esp
	mov eax, const2 ; ask user to enter third parameter
	push eax
	call @print_str
	call @scan_int32 ; scan parameter from console
	push eax ; push user input as a parameter to next function
	call test_func ; call next function with entered param
	pop ebp
	add esp, 8
	jmp [esp - 8]

test_func:
	push ebp
	mov ebp,esp
	; test_func body
	; this is where magic shoud happen.
	; We should gain access to all pushed parameters here and print them!

	; func1 params
	; call stack: [test_func func3 func2 func1]
	; let's get first one
	push const13
	call @print_str
	mov ebx,ebp ; mov current base pointer
	; this is base pointer of test_func. We shoud do three jumps
	mov ecx, 3
test_func_loop1:
	mov ebx,[ebx]
	loop test_func_loop1
	; then, we can access first parameter (it is first one, index = 0)
	mov eax, [ebx + 8 + 0*4]
	push ebx
	push eax
	call @println_int32 ; print parameter
	pop ebx
	; then, we can access second parameter (it is first one, index = 1)
	mov eax, [ebx + 8 + 1*4]
	push eax
	push const13
	call @print_str
	call @println_int32 ; print parameter

	; OK, let's get func2 params
	; call stack: [test_func func3 func2 func1]
	mov ebx,ebp ; move current base pointer
	; this is base pointer of test_func. We shoud do two jumps
	mov ecx, 2
test_func_loop2:
	mov ebx,[ebx]
	loop test_func_loop2
	; OK, access the parameter, then print
	mov eax, [ebx + 8 + 0*4]
	push eax
	push const10
	call @print_str
	call @println_int32 ; print parameter
	
	; func3 param print
	; call stack: [test_func func3 func2 func1]
	mov ebx,[ebp] ; get old basepointer (that easy)
	mov eax, [ebx + 8 + 0*4] ; get param value
	push eax
	push const11
	call @print_str
	call @println_int32 ; print

	; test_func param print
	push dword [ebp + 8 + 0*4] ; even easier
	push const12
	call @print_str
	call @println_int32 ; print

	; test_func body end
	pop ebp
	add esp, 8
	jmp [esp - 8]

program:
	push 8 ;push some const to func1 call 
	push 12 ;push another const to func1 call 
	call func1 ; call function
	; your program ends here
	
	push dword 0
	call @quit_program
@quit_program:
	push ebp
	mov ebp, esp
	push dword [ebp + 8]
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
	