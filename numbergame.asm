data segment
    time db 0   ;variable used when checking if the time has changed
    
    posX db ?
    posY db ?
    
    width dw 320         ; screen width
    height dw 200        ; screen height
    
    fieldWidth db 120    ; field width
    fieldHeight db 165   ; field height
    cursorPosX dw 90     ; cursor position (used when drawing a field)
    cursorPosY dw 20
    
    address dw ?
    
    scoreMessage db "Score: $"  ; score message
    scoreStr db "0         $"   ; holds score string
    score dw 0                  ; holds score int
    clrScore db "  $"           ; used for clearing score 
    
    playerNumber db "  $" ; random generated number a player can move (as a string)
    pNum dw 0             ; same but as a number (used when checking if it's even)
    
    clrNumber db "  $"  ; used for clearing a number 
    direction db "$"
    
    pNumPosX db 18 ; starting position of a movable number
    pNumPosY db 21     

    number db "  $" ; random generated number (this is a placeholder for it)
    num dw 0
    
    numPosX db 12   ; starting position of a number that will be falling down
    numPosY db 3 
    
    numMinX db 12    ; closest a player number can get to the left wall
    numMaxX db 24    ; closest a player number can get to the right wall
    
    pNumEven dw 0
    numEven dw 0
    numEvenStr db '  '
    pNumEvenStr db '  '
    
    endMessage db "Game Over!$"
    endScore   db "Final score: $"
data ends

stck segment stack
    dw 128 dup(0)
stck ends
         
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; reading character without it showing on screen
; after reading key, c needs to be reset
readKey macro c
    local keyPressed, readKeyEnd
    
    push ax
    mov ah, 1
    int 16h
    jnz keyPressed

    jmp readKeyEnd
    
    keyPressed:
    mov ah, 0
    int 16h
    mov c, al
         
    readKeyEnd:
    pop ax
endm

; checks if number is even
; a - number getting checked
; b - variable holding true or false
macro isEven a, b
    local even, endIsEven
    
    test a, 1       ; checks if number is even
    je even         ; if it is jumps to label 'even'
    
    mov b, 0        ; if not sets to false
    jmp endIsEven   ; jumps to end
    
    even:
    mov b, 1        ; sets to true
    
    endIsEven:    
endm

; End of program
macro programEnd 
    mov ax, 4c02h
    int 21h
endm
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Code segment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

code segment 

; string to int conversion
strToInt proc
    push ax 
    push bx 
    push cx 
    push dx 
    push si
    mov bp, sp
    mov bx, [bp+14]
    mov ax, 0
    mov cx, 0
    mov si, 10
loop1:
    mov cl, [bx]
    cmp cl, '$'
    je end1
    mul si
    sub cx, 48
    add ax, cx
    inc bx
    jmp loop1
end1:
    mov bx, [bp+12]
    mov [bx], ax
    pop si 
    pop dx 
    pop cx 
    pop bx 
    pop ax
    ret 4
strToInt endp

; int to string conversion
intToStr proc
    push ax 
    push bx 
    push cx 
    push dx 
    push si
    mov bp, sp
    mov ax, [bp+14]
    mov dl, '$'
    push dx
    mov si, 10
loop2:
    mov dx, 0
    div si
    add dx, 48
    push dx
    cmp ax, 0
    jne loop2
    mov bx, [bp+12]
loop2a:
    pop dx
    mov [bx], dl
    inc bx
    cmp dl, '$'
    jne loop2a
    pop si 
    pop dx 
    pop cx  
    pop bx 
    pop ax
    ret 4
intToStr endp

; prints a string
; dh - row
; dl - col
; bx - string address
; bh - color
print proc
    ; set cursor position
    push bx
    mov ah, 2
    mov al, bh
    xor bh, bh
    int 10h

    ; print string
    mov ah, 9
    pop dx
    int 21h

    ret
print endp

; draw the field of the game
drawField proc
    ; position of the first pixel
    mov ah, 0ch
    mov al, 7             ; color of the pixel
    mov cx, cursorPosX    ; column
    mov dx, cursorPosY    ; row
    int 10h
    
    ; loop for creating a top line of a rectangle
    mov bl, fieldWidth
    topline:
    int 10h
    inc cx
    dec bl
    jnz topline
    
    ; loop for creating a right line of a rectangle
    mov bl, fieldHeight
    rightline:
    int 10h
    inc dx
    dec bl
    jnz rightline
    
    ; loop for creating a bottom line of a rectangle
    mov bl, fieldWidth
    bottomline:
    int 10h
    dec cx
    dec bl
    jnz bottomline
    
    ; loop for creating a left line of a rectangle
    mov bl, fieldHeight
    leftline:
    int 10h
    dec dx
    dec bl
    jnz leftline
    
    ret
drawField endp

; clears previous position of the number moved by player
; ch - number Y pos
; cl - number X pos
clearNumber proc
    mov dh, ch
    mov dl, cl
    mov bx, offset clrNumber ; replaces current number with an empty string
    call print
    ret
clearNumber endp

; clears the previous position of score
clearScore proc
    mov dh, 1
    mov dl, 8
    mov bx, offset clrScore
    call print
    ret 
clearScore endp


; generates a random number from 0 - 9 using system time
; stores it in dl
; bx - range of numbers
randGen proc 
    loopRandGen:
    mov ah, 00h   ; interrupts to get system time        
    int 1ah       ; CX:DX now hold number of clock ticks since midnight      

    mov ax, dx
    xor dx, dx
    mov cx, bx    
    div cx       ; here dx contains the remainder of the division - from 0 to 9

    add dl, '0'  ; to ascii from '0' to '9'
      
    ret      
randGen endp

; generates a random X position of a number
genRandomX proc
    mov bx, 7    ; there can be 7 positions
    call randGen ; calls a random num gen for numbers 0 - 6
    
    cmp dl, '0'
    je is0 
    cmp dl, '1'
    je is1
    cmp dl, '2'
    je is2
    cmp dl, '3'
    je is3
    cmp dl, '4'
    je is4    
    cmp dl, '5'
    je is5    
    cmp dl, '6'
    je is6
    
    ; if it somehow gets to here end the procedure 
    jmp end 
     
    is0:             ; if it's 0 stay on starting position (12 - far left of field)
    mov numPosX, 12
    jmp end           
    is1:             ; if it's 1 move 2 pixels to the right
    mov numPosX, 14
    jmp end
    is2:
    mov numPosX, 16
    jmp end
    is3:
    mov numPosX, 18
    jmp end
    is4:
    mov numPosX, 20
    jmp end
    is5:
    mov numPosX, 22
    jmp end
    is6:
    mov numPosX, 24
    
    end:
    ret
genRandomX endp

; creates and prints a number that will be falling from the top on a random x pos
createNum proc
    mov bx, 10
    call randGen         ; generates a random number 0 - 9
    mov [number], dl
    
    call randGen
    mov [number + 1], dl ; and then once again, "number" now has 2 digits
              
    call genRandomX      ; gets a random x pos
    
    call printNumber     ; prints a number on screen     
    ret    
createNum endp

; printing a number
printNumber proc
    mov dh, numPosY
    mov dl, numPosX
    mov bx, offset number
    call print
    ret
printNumber endp

; printing a players movable number
printPlayerNumber proc
    mov dh, pNumPosY
    mov dl, pNumPosX
    mov bx, offset playerNumber
    call print
    ret
printPlayerNumber endp

; printing score
printScore proc
    mov dh, 1
    mov dl, 8
    mov bx, offset scoreStr
    call print
    ret
printScore endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

start: 
    ; Setting the segment registers
    ASSUME cs: code, ss:stck
    mov ax, data
    mov ds, ax 
    
input:
    ; entering graphical mode
    mov ah, 0
    mov al, 13h
    int 10h
    
    keyWait:            ; game won't start until a player presses 's'
	readKey direction   ; this is intended only for readjusting the emu window
	cmp direction, 's'  
	je continueProgram
	
	jmp keyWait
	
    continueProgram:   
    ; printing a score message
    mov dh, 1
    mov dl, 1
    mov bx, offset scoreMessage
    call print
    call printScore  
    
    ; drawing the field
    call drawField
    
newNumbers:    
    ; calling randGen two times because we want a 2 decimal number
    mov bx, 10
    call randGen
    mov [playerNumber], dl       ; putting first random generated number in a first reserved position
    
    call randGen
    mov [playerNumber + 1], dl   ; putting second random num in a second reserved position
    
    push offset playerNumber
    push offset pNum
    call strToInt
    
    isEven pNum, pNumEven 
    
    call printPlayerNumber
    
    mov [numPosY], 3  ; resetting numPosY so it doesn't start from the bottom after the first loop
    call createNum
    
    push offset number
    push offset num
    call strToInt
    
    isEven num, numEven
    
checkTime:            ;time checking loop
	mov ah, 2Ch       ;get the system time
	int 21h    		  ;CH = hour CL = minute DH = second DL = 1/100 seconds
	
	cmp dl, time  	  ; is the current time equal to the previous one?
	je checkTime      ;if it is the same, check again 
	
	mov time, dl      ; if it's not the same, time becomes current time
	
moveNumber:
    mov ch, numPosY
    mov cl, numPosX
    call clearNumber    ; clears number from the screen
    
    mov al, pNumPosY
    cmp numPosY, al     ; compares y pos of both numbers
    je collect          ; if they're the same go to collect label
    
    add numPosY, 2
    call printNumber	  

loopMoving:
    readKey direction
    
    cmp direction, 'a'  ; reads the key that's been pressed
    je left
    cmp direction, 'd'
    je right
    jmp checkTime     
      
    left:               ; if we're trying to go left
    mov direction, 0    ; resetting direction
    mov al, numMinX     ; putting variable in a register because you can't compare two variables
    cmp al, pNumPosX    ; compare current x pos of number with min x pos
    je continueLoop     ; if numposX != minPosX don't move left
    jmp continueLeft    ; if numposX != minPosX jump to continueLeft label
    
    continueLeft:       ; move left
    mov ch, pNumPosY
    mov cl, pNumPosX
    call clearNumber    ; clear the last pos of number 
    sub pNumPosX, 2     ; sub x with 2 (go left 2 pixels)
    jmp continue      
   
    right:              ; if we're trying to go right  
    mov direction, 0
    mov al, numMaxX
    cmp al, pNumPosX
    je continueLoop
    jmp continueRight
    
    continueRight:      ; move right
    mov ch, pNumPosY
    mov cl, pNumPosX
    call clearNumber
    add pNumPosX, 2
    jmp continue
    
    continue:           ; this prints the number in a new position
    call printPlayerNumber 
    
    continueLoop:       ; after everything is finished, continues to check time
    jmp checkTime
    
collect:
    ; check if they're the right parity
    mov ax, pNumEven
    cmp numEven, ax
    je collectTrue   ; if they are, jump to collectTrue
    
    collectFalse:    ; if they're not...
    mov al, pNumPosX ; compare their x positions
    cmp numPosX, al  ; if they're the same end the game
    je endPoint
    
    jmp continueCollect ; if they're not continue

    collectTrue:
    mov al, pNumPosX     
    cmp numPosX, al     ; compare their x positions
    je continueCollect  ; if they're the same continue
    
    jmp endPoint        ; if not, end
    
    continueCollect:
    add score, 10
    call clearScore
   
    push score
    push offset scoreStr
    call intToStr
    
    call printScore
    
    mov ch, pNumPosY
    mov cl, pNumPosX
    call clearNumber    ; clears both numbers from the screen
    
    mov ch, numPosY
    mov cl, numPosX
    call clearNumber
    
    jmp newNumbers      ; jump to newNumbers (continue loop / create new numbers)
    
endPoint:
    mov dh, 6
    mov dl, 14
    mov bx, offset endMessage     ; print end message
    call print
    mov dh, 9
    mov dl, 13
    mov bx, offset endScore       ; print final score
    call print
    mov dh, 11
    mov dl, 13
    mov bx, offset scoreStr       ; final score
    call print
    programEnd                    ; end the game
                                  
ends
end start