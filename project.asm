IDEAL
MODEL small
STACK 100h
DATASEG
	PlayerMsg db ? ;A buffer for a string given by the user
	message db 192 dup (?)
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




proc encodeBase64
	;encode the string stored in message and put the encoded version in encodedString
	;loop on every 3 bytes and send them to encode3BytesBase64, then for the last 3 (if not 3) encode manually
	;10,13,'$' will be appended to the message
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	
	
	
	
	
	mov bl, [byte ptr message+1] ;total characters in message
	
	mov al, bl
	xor ah, ah ;ax is total characters in message
	mov bh, 3
	div bh ;ah is the remaining bytes (if there are not 3)
	
	cmp ah, 0
	je noExtraBytes
	
	dec al
noExtraBytes:
	;now loop for al times (number of 3 consistant bytes)
	;stop at al
	
	xor si, si ;index of the message
	xor dh, dh ;index of encodedString
encodeBase64Loop:
	
	
	mov ch, [byte ptr message+si]
	inc si
	mov cl, [byte ptr message+si]
	inc si
	push cx
	
	;index is dh
	mov dl, [byte ptr message+si]
	inc si
	
	push dx
	
	call encode3BytesBase64
	
	add dh, 4
	
	cmp dh, al
	jl encodeBase64Loop
	
	
	
	;end loop
	
	cmp sh, 0
	je endEncodeBase64 ;if there are no more bytes to encode
	
	
	mov dl, dh
	xor dh, dh
	mov di, dx
	;di is encodedString index
	;si is message index
		


	;encode last 3 bytes




	;encode first byte (exists no matter what)
	
	mov bl, [byte ptr message+si]
	inc si
	
	shr bl, 2
	and bl, 00111111b
	;bl is the right 6 bits
	xor bh, bh
	mov bl, [byte ptr base64Alphabet+bx]
	;bl is the encoded character
	mov [byte ptr encodedString+di], bx
	inc di
	
	;finished first byte
	
	
	cmp ah, 1
	je bytesRemaining2Base64Encode
	
	;add last character and '==' for missing bytes
	dec si
	mov bl, [byte ptr message+si]
	;si is in the last byte (assuming that there are no more)
	
	and bl, 00000011b
	shl bl, 4
	and bl, 00110000b
	
	xor bh, bh
	mov bl, [byte ptr base64Alphabet+bx]
	
	mov [byte ptr encodedString+di], bx
	
	;append 2 '=='
	mov [byte ptr encodedString+di], '='
	inc di
	mov [byte ptr encodedString+di], '='
	inc di
	
	
	;finished case when 2 bytes are missing
jmp endEncodeBase64




;case when 1 byte is missing
bytesRemaining2Base64Encode:
	
	
	mov bl, [byte ptr message+si]
	
	shr bl, 2
	and bl, 00111111b
	;bl is the right 6 bits
	xor bh, bh
	mov bl, [byte ptr base64Alphabet+bx]
	;bl is the encoded character
	mov [byte ptr encodedString+di], bx
	inc di
	
	
	;finished first byte
	
	mov bl, [byte ptr message+si]
	inc si
	
	and bl, 00000011b
	shl bl, 4
	mov bh, [byte ptr message+si]
	
	and bh, 11110000b
	shr bh, 4
	add bl, bh
	;bl is the full charcter
	
	xor bh, bh
	mov bl, [byte ptr base64Alphabet+bx]
	;bl is the encoded character
	mov [byte ptr encodedString+di], bx
	inc di
	
	;finished second character
	
	mov bl, [byte ptr message+si]
	and bl, 00001111b
	shl bl, 2
	and bl, 00111100b
	
	xor bh, bh
	mov bl, [byte ptr base64Alphabet+bx]
	;bl is the encoded character
	mov [byte ptr encodedString+di], bx
	inc di
	
	;now append '='
	mov [byte ptr encodedString+di], '='
	
	
	
endEncodeBase64:
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp encodeBase64





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




start:
	mov ax, @data
	mov ds, ax
	

	call createBase64Alphabet
	
	
	
	
exit:
	mov ax, 4c00h
	int 21h
END start
