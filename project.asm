IDEAL
MODEL small
STACK 100h
DATASEG
	PlayerMsg db ? ;A buffer for a string given by the user
	message db 130 dup (?)
	base64Alphabet db 64 dup (?)
	encodedString db 256 dup (?)
CODESEG

	;write a function that takes a combintaion of characters from the user
proc getStringFromUser
		;put first character in PlayerMsg second position and go forward.
		;First position will be the length of the message including the enter and break (10,13,'$')
		;we are doing this to make sure the user isn't putting more than 255 characters in the message
	
		;run until we get enter or get to 255 characters
endp getStringFromUser



proc createBase64Alphabet
	;puts in base64Alphabet each char of the alphabet
	push si
	push ax
	
	xor si, si
	;add capital letters
	mov al, 'A' ;65
	mov ah, 'Z' ;90
CapitalLetterLoop:
	mov [byte ptr base64Alphabet+si], al
	inc al
	inc si
	
	cmp al,ah
	ja CapitalLetterLoop
	
	;add lowercase letters
	mov al, 'a' ;97
	mov ah, 'z' ;122
	
LowercaseLettersLoop:
	mov [byte ptr base64Alphabet+si], al
	inc al
	inc si
	cmp al,ah
	jbe LowercaseLettersLoop
	
	;add numbers
	xor al, al
	add al, 30h
	mov ah, 9
	add ah, 30h ;put the ascii values of the numbers in ah, al
NumberLoop:
	mov [byte ptr base64Alphabet+si], al
	inc al
	inc si
	cmp ah, al
	jbe NumberLoop
	
	;add + and /
	inc si
	mov [base64Alphabet+si], '+' ;43
	inc si
	mov [base64Alphabet+si], '/' ;47
	

	pop ax
	pop si
	ret
endp createBase64Alphabet









proc encode3BytesBase64
	;get 3 bytes from the stack, first word is 2 first bytes and the third one is the index in the string and the other byte
	;put in the right index in encodedString the encoded 3 bytes (4 characters, 24 bits)
	;assume that all chars are full so you don't have to complete the last one
	
	
	;first put the 6 bits into encodedString, then shift into that register the second character, do it for 3 and 4 too.
	
	push bp, sp
	mov bp, sp
	
	
	push ax
	push bx 
	push cx
	push dx
	push si
	
	
	mov ax, [bp+6] ;first 2 bytes
	mov bx, [bp+4] ;bh: index, bl: byte
	
	mov dh, ah ;we'll do the calculations on dh
	
	and dh, 11111100b ;get first character
	shr dh, 2 ;now dh is the encoded char
	
	mov cl, bh
	xor ch, ch
	mov si, cx
	;now si is the index
	xor dl, dl
	mov bx, dx
	mov dh, [byte ptr base64Alphabet+bx] ;dh is the actual character
	mov [byte ptr encodedString+si], dh
	
	;finished first character
	mov dh, ah
	shr dh, 6 
	;dh has the first 2 bits of the second byte
	
	
	mov dl, al ;byte 2
	shr dl, 4
	add dh, dl ;dh is now the second encoded byte
	
	
	inc si;index
	
	xor dl, dl
	mov bx, dx
	mov dh, [byte ptr base64Alphabet+bx] ;dh is the actual character
	mov [byte ptr encodedString+si], dh

	;finished second character
	
	mov dh, ah
	and dh, 00001111b ;get first 4 bits of third character
	shl dh, 2 ;move the bits to their place
	mov dl, bl
	shr dl, 6 ;get the last 2 bits
	
	add dh, dl ;dh is the full character
	
	xor dl, dl
	mov bx, dx
	mov dh, [byte ptr base64Alphabet+bx]
	
	inc si
	
	mov [byte ptr encodedString+si], dh
	
	;finished third character
	
	mov dh, bl
	and dh, 00111111b ;dh is the last character
	xor dl, dl
	mov bx, dx
	
	mov dh, [byte ptr base64Alphabet+bx]
	inc si
	mov [byte ptr encodedString+si], dh
	
	;finished function
	;cleanup
	
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 4
endp encode3BytesBase64





proc encodeEndBase64
	;encodes the last 3 digits of the message
	;get the first 2 characters in the top word in the stack and the index and last char at the bottom, where the index is at the high segment
	push bp
	mov bp, sp
	
	mov ax, [bp+4] ;index and last char
	mov bx, [bp+6] ;2 chars
	
	mov di, ah ;index
endp encodeEndBase64




start:
	mov ax, @data
	mov ds, ax
	

	call createBase64Alphabet
	
	
	
	
exit:
	mov ax, 4c00h
	int 21h
END start
