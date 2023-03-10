IDEAL
MODEL small
STACK 100h
DATASEG
	
	
	mainMenuTitle	db 10,10,'               Hello!',10,10,'               In this assembly project, you can encode',10,'               and decode strings through the base64'
					db 'algorithm.',10,10
					db '               Enjoy!',10,10,10,10,10,'$'
					
					
	mainMenuNav	db '               ','Press E/e for encode',10,10,10
				db '               ','Press D/d for decode',10,10,10
				db '               ','Press I/i for an introduction to the algorithm','$'
				
				
	IntroText	db 10,10,'               ','Introduction Page',10,10,10,10,10
				db '               ','Press D/d for an explenation about the decode algorithm',10,10,10
				db '               ','Press E/e for an explenation about the encode algorithm',10,10,10
				db '               ','Press M/m to go back to the Main Menu','$'
				
				
	IntroEncodeText	db 10,10,'               ','Given a String of characters in the base64 alphabet,',10
					db '               ','ABCDEFGHIJKLMNOPQRSTUVWXYZ',10
					db '               ','abcdefghijklmnopqrstuvwxyz0123456789+/',10
					db '               ','We will encode it as follows:',10,10,10
					db '               ','1. Split the stream of bits to chunks of 6 bits each.',10,10
					db '               ','2. For every block, with binary value 63 at max,',10
					db '               ','convert it to its index at the alphabet. so for each 3 bytes,',10
					db '               ','4 characters will be the output.',10,10
					db '               ','3. At the end, if there is 1 or 2 bytes left,',10
					db '               ','we have 16 or 8 bits remaining. So, add 0s at their end ',10
					db '               ','to complete them to 6 bit blocks.',10,10
					db '               ','4. For each byte missing at the end, append an =.',10,10,10,10
					db '               ','Press I/i to go back to the introduction Page.','$'
					
					
	IntroDecodeText db 10,10,'               ','Given a String of characters, decode it as follows:',10,10,10
					db '               ','1. Divide the stream into 4 character long blocks.',10,10
					db '               ','2. For each block, revert its characters back through',10
					db '               ','the alphabet, and then weve got a stream of 24 bits.',10,10
					db '               ','3. Each 8 bit sequence is our encoded letter.',10,10
					db '               ','4. At the last block, if there 1 or 2 =,',10
					db '               ','decode 3 minus the number of =',10,10,10,10
					db '               ','Press I/i to go back to the introduction Page.','$'
					
					
	DecodeText 	db 10,10,'               ','Write a string you will like to decode (max 56 characters)',10,10
				db '               ','Each character not in the alphabet will not be registered.',10,10
				db '               ','Press Enter to submit string and backspace to delete a char',10,10
				db '               ','Press ! to go back to the main menu',10,10,10,10
				db '               ','$'
				
				
	InvalidStringDecodeText	db 10,10,'               ','You submitted an invalid String.',10,10
							db '               ','Try to write a valid string next time :)',10,10,10,10
							db '               ','Press M/m to go back to the Main Menu',10,10
							db '               ','Press D/d to resubmit a string','$'
							
							
	ValidDecodeMsgText1	db 10,10,'               ','Your decoded message is',10,10,10
						db '               ','$'


	ValidDecodeMsgText2 db 10,10,10,10,'               ','Press M/m to go back to the main menu',10,10
						db '               ','Press D/d to decode another message','$'
							
	
	EncodeText	db 10,10,'               ','Write a string you will like to encode (max 42 characters)',10,10
				db '               ','Each character not between 32 and 126 (included) ascii value',10,10
				db '               ','will not be registered.',10,10
				db '               ','Press Enter to submit the string and backspace to delete a char',10,10
				db '               ','Press ESCAPE to go back to the main menu',10,10,10,10
				db '               ','$'
							
							
	PostEncodeText1	db 10,10,'               ','Your encoded string is',10,10,10,'               $'
	
	
	PostEncodeText2 db 10,10,'               ','Press E/e to encode another string',10,10
					db '               ','Press M/m to go back to the main menu','$'
							
							
	message db 59 dup (?) ;last char is $
	charIndex db ?
	
	base64Alphabet db 64 dup (?)
	encodedString db 56 dup (?)
	decodedMsg db 56 dup (?)
	jumpTo db ? ;E if EncodeMenu, D if DecodeMenu, I if InrtoMenu, M if MainMenu
	
CODESEG



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
	
	
	
	
	
	
	
	;now loop for al times (number of 3 consistant bytes)
	;stop at al
	
	xor si, si ;index of the message
	mov si, 2 ;start of message
	xor dh, dh ;index of loop
	xor bh, bh ;index of encodedString
	
	
	cmp al, 0
	je EndEncodeLoop
	
	
	dec al
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
	
	
EndEncodeLoop:
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
	je endEncodeBase64Semi ;if there are no more bytes to encode
	
	
	

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
	inc di
	
	
	
	
	
endEncodeBase64:
	mov [byte ptr encodedString+di], '$' ;if we didnt skip on the last level
	jmp endEndEncodeBase64
	
	
endEncodeBase64Semi:
	inc al
	add al, al
	add al, al
	xor ah, ah
	mov di, ax
	mov [byte ptr encodedString+di], '$'
	
	
endEndEncodeBase64:
	
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
	
	xor dh, dh
	xor dl, dl
	
	cmp al, 0
	jne not1IterationDecode
	inc dh ;dh is 1 if we don't need to run more than once
	
not1IterationDecode:
	;iterate for al
	
	;si is index in message (the message to decode)
	;di is index in decodedMsg
	xor si, si
	add si, 2
	xor di, di
	
	cmp al, 0
	jne decodeBase64Loop
	
	add si, 3
	cmp [byte ptr message+si], '='
	je exitDecodeBase64Loop	;if there are less than 3 character in the decoded message


	sub si, 3
	
decodeBase64Loop:
	mov bx, di
	mov cx, si
	mov bh, cl
	
	
	push bx
	
	call decode3BytesBase64
	
	add di, 3
	add si, 4
	
	cmp dh, 1
	je exitDecodeBase64Loop

	
	dec al
	cmp al, 0
	ja decodeBase64Loop

exitDecodeBase64Loop:

	cmp [byte ptr message+si], '=' ;this can't happen if we ran in the loop cuz it can only be in the last 2 bytes
	jne moreThan3CharsDecode
	sub si, 3
	dec dh
	
moreThan3CharsDecode:
	
	;end loop
	xor cl, cl
	inc cl
	
	cmp dh, 1
	jne not1IterationDecode2
	
	dec cl
	jmp endDecodeBase64Stop1
	
not1IterationDecode2:
	
	
	;we've got 4 characters left in message
	
	add si, 2
	cmp [byte ptr message+si], '='
	
	jne Not2EqualSigns
	
	;we've got == at the end
	
	sub si, 2
	mov dl, [byte ptr message+si]
	xor dh, dh
	push dx
	call redoCharArrayBase64
	mov dl, [charIndex]
	shl dl, 2
	
	inc si
	mov bl, [byte ptr message+si]
	xor bh, bh
	push bx
	call redoCharArrayBase64
	mov dh, [charIndex]
	shr dh, 4
	add dl, dh
	mov [byte ptr decodedMsg+di], dl
	inc di
	

	dec si
	;end function for ==
	jmp endDecodeBase64
	
	
Not2EqualSigns:
	inc si
	cmp [byte ptr message+si], '='
	jne not1EqualSign
	
	;we've got = at the end so 2 bytes to decode
	
	sub si, 3
	mov dl, [byte ptr message+si]
	xor dh, dh
	push dx
	call redoCharArrayBase64
	mov dl, [charIndex]
	shl dl, 2
	inc si
	
	mov bl, [byte ptr message+si]
	xor bh,bh
	push bx
	call redoCharArrayBase64
	mov dh, [charIndex]
	and dh, 00110000b
	shr dh, 4
	add dl, dh
	mov [byte ptr decodedMsg+di], dl
	inc di
	
	;finished first byte
	
	
endDecodeBase64Stop1:
	cmp cl, 0
	je endDecodeBase64Stop2
	
	
	
	mov bl, [byte ptr message+si]
	inc si
	xor bh,bh
	push bx
	call redoCharArrayBase64
	mov bh, [charIndex]
	and bh, 00001111b
	shl bh, 4
	
	mov dl, [byte ptr message+si]
	xor dh, dh
	push dx
	call redoCharArrayBase64
	mov dl, [charIndex]
	
	and dl, 00111100b
	shr dl, 2
	add dl, bh
	
	mov [byte ptr decodedMsg+di], dl
	inc di
	

	jmp endDecodeBase64
	


not1EqualSign:
	;we've got no = at the end so 3 bytes to decode
	
	sub si, 3
	mov dl, [byte ptr message+si]
	xor dh, dh
	push dx
	call redoCharArrayBase64
	mov dl, [charIndex]
	shl dl, 2
	
	
endDecodeBase64Stop2:
	cmp cl, 0
	je endDecodeBase64Stop3

	
	inc si
	mov bl, [byte ptr message+si]
	xor bh, bh
	push bx
	call redoCharArrayBase64
	mov dh, [charIndex]
	shr dh, 4
	add dl, dh
	mov [byte ptr decodedMsg+di], dl
	inc di
	
	mov bl, [byte ptr message+si]
	inc si
	xor bh,bh
	push bx
	call redoCharArrayBase64
	mov bh, [charIndex]
	and bh, 00001111b
	shl bh, 4
	
	
endDecodeBase64Stop3:
	cmp cl, 0
	je endDecodeBase64
	
	
	mov dl, [byte ptr message+si]
	xor dh, dh
	push dx
	call redoCharArrayBase64
	mov dl, [charIndex]
	
	and dl, 00111100b
	shr dl, 2
	add dl, bh
	
	mov [byte ptr decodedMsg+di], dl
	inc di
	
	mov dl, [byte ptr message+si]
	xor dh,dh
	push dx
	call redoCharArrayBase64
	mov dl, [charIndex]
	and dl, 00000011b
	shl dl, 6
	inc si
	mov bl, [byte ptr message+si]
	xor bh, bh
	push bx
	call redoCharArrayBase64
	mov bl, [charIndex]
	and bl, 00111111b
	add dl, bl
	mov [byte ptr decodedMsg+di], dl
	inc di
	

	
	;cleanup
endDecodeBase64:
	mov [byte ptr decodedMsg+di], '$'
	
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
   
    mov al, [byte ptr message+si]
	inc si
	xor ah, ah

	push ax
	call redoCharArrayBase64
	
	mov al, [charIndex] ;al is the first bits of the first byte
	shl al, 2
	
	
	mov bl, [byte ptr message+si]
	xor bh, bh
	
	push bx
	call redoCharArrayBase64
	
	mov bl, [charIndex]
	shr bl, 4
	
	add al, bl
	mov [byte ptr decodedMsg+di], al
	inc di
	
	;finished first byte
	
	mov al, [byte ptr message+si]
	inc si
	xor ah, ah
	push ax
	call redoCharArrayBase64
	
	mov al, [charIndex]
	shl al, 4
	mov bl, [byte ptr message+si]
	xor bh, bh
	push bx
	call redoCharArrayBase64
	
	mov bl, [charIndex]
	shr bl, 2
	and bl, 00001111b
	add al, bl
	mov [byte ptr decodedMsg+di], al
	inc di
	
	;finished second byte
	
	mov al, [byte ptr message+si]
	inc si
	xor ah, ah
	push ax
	call redoCharArrayBase64
	
	mov al, [charIndex]
	shl al, 6
	mov bl, [byte ptr message+si]
	xor bh, bh
	push bx
	call redoCharArrayBase64
	
	mov bl, [charIndex]
	add al, bl
	mov [byte ptr decodedMsg+di], al
	
	;finished third byte
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



proc checkInAlphabet
	;get a char via stack (low part) and make ah 1 if it is in base64Alphabet and 0 if it's not
	
	push bp
	mov bp, sp
	push si
	push ax
	
	mov al, [bp + 4]
	xor si,si

checkInAlphabetLoop:
	cmp [byte ptr base64Alphabet+si], al
	je FoundCharInAlphabet
	
	inc si
	cmp si, 64
	jne checkInAlphabetLoop
	
	pop ax
	xor ah, ah
	jmp endCheckInAlphabet

FoundCharInAlphabet:
	pop ax
	mov ah, 1

endCheckInAlphabet:
	pop si
	pop bp
	ret 2
endp checkInAlphabet



proc checkValidDecode
	;checks if the message in decodedMsg contains chars not in the normal ASCII charset
	;if its invalid put 0 in di and if it is put 1
	;use checkInAlphabet
	push si
	push dx
	push ax
	
	xor di, di
	xor si, si
	xor dh, dh
	;si is the index in decodedMsg
	
checkValidDecodeLoop:

	mov dl, [byte ptr decodedMsg+si]
	
	cmp dl, 32
	jb InvalidCharDecode
	
	cmp dl, 126
	ja InvalidCharDecode
	
	jmp ValidCharDecodeFunc
	
	
	
InvalidCharDecode:
	;invalid char
	xor cl, cl
	jmp endCheckValidDecodeLoop
	
ValidCharDecodeFunc:
	inc si
	cmp [byte ptr decodedMsg+si], '$'
	jne checkValidDecodeLoop
	
	mov di, 1
	
endCheckValidDecodeLoop:
	pop ax
	pop dx
	pop si
	
	ret
endp checkValidDecode




start:
	mov ax, @data
	mov ds, ax
	

	call createBase64Alphabet
	
	
	
	;Initialize program
	
	
	
	;Main Menu
MainMenu:
	mov [jumpTo], 0
	xor ch, ch
	xor cl, cl
	
	mov ah, 0 ;text mode
	mov al, 2
	int 10h
	
	;print main menu
	mov ah, 9
	mov dx, offset mainMenuTitle
	int 21h
	
	
	mov ah, 9
	mov dx, offset mainMenuNav
	int 21h
	
	
	
MainMenuLoop:
	
	;check for I D or E
	
	mov ah, 0h
	int 16h
	
	cmp al, 'I'
	je InrtoMenu
	cmp al, 'i'
	je InrtoMenu
	
	
	cmp al, 'E'
	je EncodeMenuStop1
	cmp al, 'e'
	je EncodeMenuStop1
	
	jmp NotEMainMenu
	
EncodeMenuStop1:
	mov [jumpTo], 'E'
	jmp EncodeMenuStop2
	
	
NotEMainMenu:
	
	cmp al, 'D'
	je DecodeMenuStop1
	cmp al, 'd'
	je DecodeMenuStop1

	
	jmp MainMenuLoop
	
	
DecodeMenuStop1:
	mov cl, 1
	jmp DecodeMenuStop2
	
	
MainMenuStop1:
	cmp ch, 1
	je MainMenu
	
	
	
InrtoMenu:

	mov ax, 3
	int 10h
	
	mov ah, 9
	mov dx, offset IntroText
	int 21h
	
IntroMenuLoop:

	mov ah, 0h
	int 16h
	
	
	cmp al, 'd'
	je IntroDecode
	
	cmp al, 'D'
	je IntroDecode
	
	cmp al, 'e'
	je IntroEncode
	
	cmp al, 'E'
	je IntroEncode
	
	cmp al, 'M'
	je MainMenu
	
	cmp al, 'm'
	je MainMenu
	
	jmp IntroMenuLoop
	
DecodeMenuStop2:
	cmp cl, 1
	je DecodeMenuStop5
	
	
EncodeMenuStop2:
	cmp [jumpTo], 'E'
	je EncodeMenu
	
	
IntroEncode:
	mov ax, 3
	int 10h
	
	mov ah, 9
	mov dx, offset IntroEncodeText
	int 21h

IntroEncodeLoop:

	mov ah, 0h
	int 16h
	
	
	cmp al, 'I'
	je InrtoMenu
	cmp al, 'i'
	je InrtoMenu
	
	jmp IntroEncodeLoop
	
	
	
IntroDecode:
	mov ax, 3
	int 10h
	
	mov ah, 9
	mov dx, offset IntroDecodeText
	int 21h
	
	
IntroDecodeLoop:
	mov ah, 0h
	int 16h
	
	
	cmp al, 'I'
	je InrtoMenu
	cmp al, 'i'
	je InrtoMenu
	
	jmp IntroDecodeLoop
	
	
	
	
	
MainMenuStop2:
	cmp ch, 1
	je MainMenuStop1
	
	







DecodeMenuStop5:
	cmp cl, 1
	je DecodeMenuStop6


EncodeMenu:

	mov ax, 3
	int 10h
	
	mov ah, 9
	mov dx, offset EncodeText
	int 21h
	
	mov si, 2
	mov dl, [byte ptr message+1]
	xor [byte ptr message+1], dl ; xor length
	
	mov [byte ptr message], 42
	
EncodeMenuLoop:
	
	mov ah, 0
	int 16h
	
	cmp al, 27
	jne NotGoToMainEncode
	
	mov ch, 1
	jmp MainMenuStop2
	
NotGoToMainEncode:
	
	;check for enter
	cmp al, 0dh
	je EncodeMessage
	
	;check for bs
	cmp al, 8
	jne NotDeleteCharEncode
	
	
	cmp [byte ptr message+1], 0
	je EncodeMenuLoop
	
	dec si
	dec [byte ptr message+1]
	
	
	jmp PrintEncode
	
	
EncodeMenuStop4:
	cmp [jumpTo], 'E'
	je EncodeMenu
	
	;dead space
DecodeMenuStop6:
	cmp cl, 1
	je DecodeMenu
	
NotDeleteCharEncode:
	
	;check for valid character
	
	cmp al, 32
	jb EncodeMenuLoop
	
	cmp al, 126
	ja EncodeMenuLoop
	
;valid char:
	;check if max chars
	cmp [byte ptr message+1], 42
	je EncodeMenuLoop

	inc [byte ptr message+1]
	mov [byte ptr message+si], al
	inc si
	

	
PrintEncode:
;print message
	mov ax, 3
	int 10h
	
	mov ah, 9
	mov dx, offset EncodeText
	int 21h
	
	mov [byte ptr message+si], '$'
	
	mov dx, offset message
	add dx, 2
	mov ah, 9h
	int 21h
	
	jmp EncodeMenuLoop
	
	
	
EncodeMessage:
	cmp [byte ptr message+1], 0
	je EncodeMenuLoop


	call encodeBase64
	
	
	mov ax, 3
	int 10h
	
	mov ah, 9
	mov dx, offset PostEncodeText1
	int 21h
	
	mov ah, 9
	mov dx, offset encodedString
	int 21h
	
	mov ah, 9
	mov dx, offset PostEncodeText2
	int 21h
	
PostEncodeLoop:
	mov ah, 0
	int 16h
	
	cmp al, 'e'
	je EncodeMenuStop3
	
	cmp al, 'E'
	je EncodeMenuStop3
	
	cmp al, 'M'
	je PostEncodeMainMenu
	
	cmp al, 'm'
	je PostEncodeMainMenu
	
	jmp PostEncodeLoop
	
PostEncodeMainMenu:
	mov ch, 1
MainMenuStop6:
	cmp ch, 1
	jmp MainMenuStop2
	
	;dead space


EncodeMenuStop3:
	mov [jumpTo], 'E'
	jmp EncodeMenuStop4

	
	


DecodeMenu:

	mov ax, 3
	int 10h
	
	mov ah, 9
	mov dx, offset DecodeText
	int 21h
	
	mov si, 2
	;si is the index in message
	mov dl, [byte ptr message+1]
	xor [byte ptr message+1], dl
	
	mov [byte ptr message], 56
DecodeMenuLoop:
	mov [jumpTo], 0

	mov ah, 0h
	int 16h
	
	cmp al, '!'
	jne NotGoToMain1
	
	mov ch, 1
	jmp MainMenuStop2
	
NotGoToMain1:
	xor ah, ah
	push ax
	call checkInAlphabet
	
	
	;if ah is 0 then avoid the char
	cmp ah, 1
	je ValidCharDecode
	
	cmp al, '='
	je ValidCharDecode
	
	;check for enter
	
	cmp al, 0dh ;enter
	je DecodeMessage
	
	;check for backspace

	cmp al, 8 ;backspace
	jne DecodeMenuLoop
	
	cmp [byte ptr message+1], 0
	je DecodeMenuLoop ;if there are no chars in message
	
	dec si
	dec [byte ptr message+1]
	
	
	;print message
	mov ax, 3
	int 10h
	
	mov ah, 9
	mov dx, offset DecodeText
	int 21h
	
	mov [byte ptr message+si], '$'
	
	mov dx, offset message
	add dx, 2
	int 21h
	
	
	
	jmp DecodeMenuLoop
	
MainMenuStop4:
	cmp ch, 1
	je MainMenuStop6
	
	;end porgram for backspace
	
DecodeMenuStop4:
	cmp [jumpTo], 'D'
	je DecodeMenu
	
	
	;char is valid
	
DecodeMenuLoopStop1:
	cmp [jumpTo], 'X'
	je DecodeMenuLoop

notBsDecode:
ValidCharDecode:
	;if not backspace (8 in ascii)
	cmp [byte ptr message+1], 56
	je DecodeMenuLoop
	

	inc [byte ptr message+1]
	mov [byte ptr message+si], al
	inc si
	
	;print message
	mov ax, 3
	int 10h
	
	mov ah, 9
	mov dx, offset DecodeText
	int 21h
	
	
	mov [byte ptr message+si], '$'
	
	
	mov dx, offset message
	add dx, 2
	mov ah, 9h
	int 21h
	
	jmp DecodeMenuLoop
	
	
MainMenuStop3:
	cmp ch, 1
	je MainMenuStop4
	
DecodeMessage:
	
	cmp [byte ptr message+1], 0
	jne NotDecodeMenuLoopStop1
	
	mov [jumpTo], 'X'
	jmp DecodeMenuLoopStop1
	
NotDecodeMenuLoopStop1:

	mov al, [byte ptr message+1]
	mov dl, 4
	div dl
	cmp ah, 0
	jne InvalidStringDecode
	
	
	
	call decodeBase64
	
	;check for exceptions
	call checkValidDecode
	cmp di, 0
	je InvalidStringDecode
	
	
	
	
	
	;valid string
	;print decoded string
	
	mov ax, 3
	int 10h
	
	mov ah, 9
	mov dx, offset ValidDecodeMsgText1
	int 21h
	
	mov ah, 9
	mov dx, offset decodedMsg
	int 21h
	
	mov ah, 9
	mov dx, offset ValidDecodeMsgText2
	int 21h
	
	
	
ValidStringDecodeLoop:
	mov ah, 0h
	int 16h
	
	
	cmp al, 'd'
	je DecodeMenuStop3

	cmp al, 'D'
	je DecodeMenuStop3
	
	cmp al, 'M'
	je MainMenuStop5
	
	cmp al, 'm'
	je MainMenuStop5
	
	jmp ValidStringDecodeLoop
	
	
MainMenuStop5:
	mov ch, 1
	jmp MainMenuStop4
	
	
	
	
	
	
InvalidStringDecode:
	
	mov ax, 3
	int 10h
	;clear screen
	
	
	mov ah, 9
	mov dx, offset InvalidStringDecodeText
	int 21h
	
InvalidStringDecodeLoop:
	mov ah, 0h
	int 16h
	
	
	cmp al, 'd'
	je DecodeMenuStop3

	cmp al, 'D'
	je DecodeMenuStop3
	
	jmp NotDecodeMenuStop3
	
DecodeMenuStop3:
	mov [jumpTo], 'D'
	jmp DecodeMenuStop4
	
NotDecodeMenuStop3:
	
	
	cmp al, 'm'
	je InvalidStringDecodeBackToMain
	
	cmp al, 'M'
	je InvalidStringDecodeBackToMain
	
InvalidStringDecodeBackToMain:
	mov ch, 1
	jmp MainMenuStop3
	
	
	
	jmp InvalidStringDecodeLoop
	
	
exit:
	mov ax, 4c00h
	int 21h
END start
