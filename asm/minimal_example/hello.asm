; tell assembler what cpu type to use
cpu 386

; Tell assembler to generate 32 bit code
bits 32

; Import stuff
extern _MessageBoxA@16  ;import from user32.dll
extern _ExitProcess@4  ;from kernel32

; Data segment
section	.data use32
	title db 'Assembler', 0x00  ;title of msgbox
	message db 'Hello, world!', 0x00  ;string to be printed
	;len equ $ - msg     ;length of the string

; Uninitialized Data Segment
section .bss use32

; Code segment
section	.text
	global _start     ;must be declared for linker (ld)
	
_start:	            ;tells linker entry point
	; push parameters onto stack
	push dword 0 ; style of message box
	push dword title  ; address of title of message box
	push dword message  ; address of text in message box
	push dword 0 ; handle owner window
	call _MessageBoxA@16 ; call function
	
	; quit program
	push dword 0 ; set return value
	call  _ExitProcess@4 ; call function
   