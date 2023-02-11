IDEAL
MODEL small
STACK 100h
DATASEG
	PlayerMsg db ? ;A buffer for a string given by the user
	;message db 192 dup (?)
	
	message db 17,8,'IMA SCHA'
	charIndex db ?
	
	base64Alphabet db 64 dup (?)
	encodedString db 256 dup (?)
	decodedMsg db 256 dup (?)
CODESEG

	;write a function that takes a combintaion of characters from the user
proc getStringFromUser
		;put first character in PlayerMsg second position and go forward.
		;First position will be the length of the message including the enter and break (10,13,'$')
		;we are doing this to make sure the user isn't putting more than 255 characters in the message
	
		;run until we get enter or get to 189 characters
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
	jbe CapitalLetterLoop
	
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
	cmp al, ah
	jbe NumberLoop
	
	;add + and /
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
	
	dec al

	;now loop for al times (number of 3 consistant bytes)
	;stop at al
	
	xor si, si ;index of the message
	mov si, 2 ;start of message
	xor dh, dh ;index of loop
	xor bh, bh ;index of encodedString
encodeBase64Loop:
	
	
	mov ch, [byte ptr message+si]
	inc si
	mov cl, [byte ptr message+si]
	inc si
	push cx
	
	;index is ch
	mov bl, [byte ptr message+si]
	inc si
	
	push bx
	

	call encode3BytesBase64
	
	
	add bh, 4
	inc dh
	
	cmp dh, al
	jle encodeBase64Loop
	
	
	
	;end loop
	
	cmp ah, 0
	je endEncodeBase64pause ;if there are no more bytes to encode
	
	
	mov dl, dh
	add dl, dl
	add dl, dl

	;mul by 4
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
	mov [byte ptr encodedString+di], bl
	inc di
	
	;finished first byte
	
	
	cmp ah, 2
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
	
	mov [byte ptr encodedString+di], bl
	inc di
	;append 2 '=='
	mov [byte ptr encodedString+di], '='
	inc di
	mov [byte ptr encodedString+di], '='
	inc di
	
	
	;finished case when 2 bytes are missing
jmp endEncodeBase64





endEncodeBase64pause:
	cmp ah, 0
	je endEncodeBase64 ;if there are no more bytes to encode
	
	
	

;case when 1 byte is missing
bytesRemaining2Base64Encode:

	dec si
	mov bl, [byte ptr message+si]
	inc si
	and bl, 00000011b
	shl bl, 4
	mov bh, [byte ptr message+si]
	inc si
	shr bh, 4
	
	add bl, bh
	
	;bl is the right 6 bits
	xor bh, bh
	mov bl, [byte ptr base64Alphabet+bx]
	;bl is the encoded character
	mov [byte ptr encodedString+di], bl
	inc di
	
	
	;finished first byte
	dec si
	mov bl, [byte ptr message+si]
	inc si
	
	and bl, 00001111b
	shl bl, 2
	and bl, 00111100b
	;bl is the full charcter
	
	xor bh, bh
	mov bl, [byte ptr base64Alphabet+bx]
	;bl is the encoded character
	mov [byte ptr encodedString+di], bl
	inc di
	
	;finished second character
	

	
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
	
	push bp
	mov bp, sp
	
	
	push ax
	push bx 
	push cx
	push dx
	push si
	push di
	
	mov ax, [bp+6] ;first 2 bytes
	mov bx, [bp+4] ;bh: index, bl: byte
	
	mov dh, ah ;we'll do the calculations on dh
	
	and dh, 11111100b ;get first character
	shr dh, 2 ;now dh is the encoded char
	
	mov cl, bh
	xor ch, ch
	mov si, cx
	;now si is the index
	mov dl, dh
	xor dh, dh
	;dl is the 6 bits to encode
	mov di, dx
	mov dh, [byte ptr base64Alphabet+di] ;dh is the actual character
	mov [byte ptr encodedString+si], dh
	
	
	;finished first character
	mov dh, ah
	and dh, 00000011b
	shl dh, 4
	;dh has the first 2 bits of the second byte
	
	
	mov dl, al ;byte 2
	shr dl, 4
	and dl, 00001111b
	add dh, dl ;dh is now the second encoded byte
	
	
	inc si;index
	
	mov dl, dh
	xor dh, dh
	
	mov di, dx
	mov dh, [byte ptr base64Alphabet+di] ;dh is the actual character
	mov [byte ptr encodedString+si], dh

	;finished second character
	
	mov dh, al
	and dh, 00001111b ;get first 4 bits of third character
	shl dh, 2 ;move the bits to their place
	mov dl, bl
	shr dl, 6 ;get the last 2 bits
	
	add dh, dl ;dh is the full character
	
	mov dl, dh
	xor dh, dh
	
	mov di, dx
	mov dh, [byte ptr base64Alphabet+di]
	
	inc si
	
	mov [byte ptr encodedString+si], dh
	
	;finished third character
	
	mov dh, bl
	and dh, 00111111b ;dh is the last character
	
	mov dl, dh
	xor dh, dh
	
	mov di, dx
	
	mov dh, [byte ptr base64Alphabet+di]
	inc si
	mov [byte ptr encodedString+si], dh
	
	;finished function
	;cleanup
	
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 4
endp encode3BytesBase64




proc decodeBase64
    ;get encoded message in the variable message and decode it to the variable decodedMsg

    ;for each four characters in decoded message, decode them into 3 bytes.

	push ax
	push bx
	push cx
	push dx
	push si
	push di
    
	

    ;loop on every 4 characters in message

    mov al, [byte ptr message+1]
	xor ah, ah
    ;ax is the number of characters in message
	
	mov bl, 4
	div bl
	dec al
	;iterate for al
	
	;si is index in message (the message to decode)
	;di is index in decodedMsg
	xor si, si
	add si, 2
	xor di, di
	
decodeBase64Loop:
	mov bx, di
	mov cx, si
	mov bh, cl
	
	
	push bx
	
	call decode3BytesBase64
	
	add di, 3
	add si, 4

	
	dec al
	cmp al, 0
	jne decodeBase64Loop
	
	
	;end loop
	xor cl, cl
	cmp ah, 0
	je endDecodeBase64Stop1
	;if there are no characters left
	inc cl
	
	
	;we've got 4 characters left in message
	
	add si, 2
	cmp [byte ptr message+si], '='
	
	jne Not2EqualSigns
	
	;we've got == at the end
	
	sub si, 2
	mov dl, [byte ptr message+si]
	shl dl, 2
	inc si
	mov dh, [byte ptr message+si]
	and dh, 00110000b
	shr dh, 4
	add dl, dh
	;dl is the full byte
	mov [byte ptr decodedMsg+di], dl
	
	dec di
	dec si
	;end function for ==
	jmp endDecodeBase64
	
	
Not2EqualSigns:
	add si, 3
	cmp [byte ptr message+si], '='
	jne not1EqualSign
	
	;we've got = at the end so 2 bytes to decode
	
	sub si, 3
	mov dl, [byte ptr message+si]
	shl dl, 2
	inc si
	mov dh, [byte ptr message+si]
	and dh, 00110000b
	shr dh, 4
	add dl, dh
	;dl is the full byte
	mov [byte ptr decodedMsg+di], dl
	
	inc di
	mov dl, [byte ptr message+si]
	
	
	
	endDecodeBase64Stop1:
	cmp cl, 0
	je endDecodeBase64Stop2
	
	
	
	
	shl dl, 4
	
	inc si
	mov dh, [byte ptr message+si]
	and dh, 00111100b
	shr dh, 4
	add dl, dh
	mov [byte ptr decodedMsg+di], dl
	inc di
	
	jmp endDecodeBase64
	

	



not1EqualSign:
	;we've got no = at the end so 3 bytes to decode
	
	sub si, 3
	mov dl, [byte ptr message+si]
	shl dl, 2
	inc si
	mov dh, [byte ptr message+si]
	and dh, 00110000b
	shr dh, 4
	add dl, dh
	;dl is the full byte
	mov [byte ptr decodedMsg+di], dl
	
	
endDecodeBase64Stop2:
	cmp cl, 0
	je endDecodeBase64
	
	inc di
	mov dl, [byte ptr message+si]
	shl dl, 4
	
	inc si
	mov dh, [byte ptr message+si]
	and dh, 00111100b
	shr dh, 4
	add dl, dh
	mov [byte ptr decodedMsg+di], dl
	inc di
	
	mov dl, [byte ptr message+si]
	and dl, 00000011b
	shl dl, 6
	
	inc si
	mov dh, [byte ptr message + si]
	and dh, 00111111b
	add dl, dh
	
	mov [byte ptr message+di], dl
	
	
	
	
	;cleanup
endDecodeBase64:
	
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	
	
	ret
endp decodeBase64



proc decode3BytesBase64
    ;get index in decodedMsg via stack (the low part), and an index in message (the high part)
    ;every byte in message is a byte with 00 at it's start

    push bp
    mov bp, sp

    push ax
    push bx
    push si
    push di

    mov ax, [bp+4]
    mov bl, ah
    xor bh, bh
    mov si, bx

    xor ah, ah
    mov di, ax
    ;si is index in message
    ;di is index in decodedMsg

    ;now we'll decode the first byte
   
    mov al, [byte ptr message + si]
    shl al, 2
    mov ah, [byte ptr message + si + 1]
    inc si
    shr ah, 4
    add al, ah
    ;al is the byte to move through base64Alphabet

    xor ah, ah
    push ax
    call redoCharArrayBase64
    ;charIndex is the encoded first byte

    mov al, [byte ptr charIndex]
    mov [byte ptr decodedMsg + di], al
    inc di

    ;finshed first byte



    mov al, [byte ptr message + si]
    shl al, 4
    mov ah, [byte ptr message + si + 1]
    inc si
    shr ah, 2
    add al, ah
    ;al is the byte to encode

    xor ah, ah
    push ax
    call redoCharArrayBase64
    mov al, [byte ptr charIndex]
    mov [byte ptr decodedMsg+di], al
    inc di

    ;finished second byte



    mov al, [byte ptr message + si]
    shl al, 6

    mov ah, [byte ptr message + si + 1]
    add al, ah
    ;al is the full byte to decode

    xor ah, ah
    push ax
    call redoCharArrayBase64
    mov al, [byte ptr charIndex]
    mov [byte ptr decodedMsg + di], al

    ;cleanup

    pop di
    pop si
    pop bx
    pop ax
    pop bp

    ret 2
endp decode3BytesBase64



proc redoCharArrayBase64
    ;get a byte from the stack (low part) and return its index in base64Alphabet
    ;charIndex variable is a byte and we'll put in it the index

    push bp
    mov bp, sp

    push si
    push ax


    mov ax, [bp + 4]
    ;al is the byte to find it's index
    xor si, si ;index in base64Alphabet

redoCharArrayBase64Loop:

    cmp al, [byte ptr base64Alphabet+si]
    je rightIndex

    inc si
    jmp redoCharArrayBase64Loop

rightIndex:
    ;end loop

    mov ax, si
    mov [charIndex], al

    ;cleanup

    pop ax
    pop si
    pop bp

    ret 2
endp redoCharArrayBase64




start:
	mov ax, @data
	mov ds, ax
	

	call createBase64Alphabet
	
	call decodeBase64
	

	
	
	
exit:
	mov ax, 4c00h
	int 21h
END start
