; 7 positions

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
    
    scoreMessage db "Score: $"
    
    score db "$"
    strS db "        "
    S dw 0
    
    playerNumber db "  $" ; random generated number a player can move
    clrNumber db "  $"  ; used for clearing a number 
    
    pNumPosX db 18 ; starting position of a movable number
    pNumPosY db 21 
    direction db "$"

    number db "  $" ; random generated number (this is a placeholder for it)

    numPosX db 12   ; starting position of a number that will be falling down
    numPosY db 3 
    
    numMinX db 12    ; closest a player number can get to the left wall
    numMaxX db 24    ; closest a player number can get to the right wall
    temp db ?
    numTemp db ?
    
    endMessage db "You lose! final score: $"
data ends

stck segment stack
    dw 128 dup(0)
stck ends
         
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

macro readChar c
    push ax
    mov ah, 01
    int 21h
    mov c, al
    pop ax
endm

; reading character without it showing on screen
readKey macro c
    push ax
    mov ah, 08
    int 21h
    mov c, al
    pop ax
endm

; Reading a character without saving
macro keypress 
    push ax
    mov ah, 08
    int 21h
    pop ax
endm

; writing one char on screen
write macro c
    push ax 
    push dx
    mov ah, 02
    mov dl, c
    int 21h
    pop dx 
    pop ax
endm

; setting current position to (x, y)
macro setXY x y
     push ax
     push dx
     mov posX, x
     mov posY, y
     
     mov dx, width
     shl dx, 1
     mov ax, dx
     mov ah, posY
     mul ah
     mov dl, posX  
     shl dl, 1
     add ax, dx
   
     mov address, ax
     pop dx
     pop ax
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
    
; New line
newLine proc
    push ax
    push bx
    push cx
    push dx
    mov ah,03
    mov bh,0
    int 10h
    inc dh
    mov dl,0
    mov ah,02
    int 10h
    pop dx
    pop cx
    pop bx
    pop ax
    ret
newLine endp 

; Reading a string
; String address is a parameter on stack
readString proc
    push ax
    push bx
    push cx
    push dx
    push si
    mov bp, sp
    mov dx, [bp+12]
    mov bx, dx
    mov ax, [bp+14]
    mov byte [bx] ,al
    mov ah, 0Ah
    int 21h
    mov si, dx     
    mov cl, [si+1] 
    mov ch, 0
copy:
    mov al, [si+2]
    mov [si], al
    inc si
    loop copy     
    mov [si], '$'
    pop si  
    pop dx
    pop cx
    pop bx
    pop ax
    ret 4
readString endp

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
intToString proc
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
intToString endp

; prints a string
; dh - row
; dl - col
; bx - string address
print proc
    ; set cursor position
    push bx
    mov ah, 2
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
clearNumber proc
    mov dh, pNumPosY
    mov dl, pNumPosX
    mov bx, offset clrNumber ; replaces current number with an empty string
    call print
    ret
clearNumber endp


; generates a random number from 0 - 9 using system time
; stores it in dl
; bx - range of numbers
randGen proc
    push bx ; might not be needed ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    mov ah, 00h   ; interrupts to get system time        
    int 1ah       ; CX:DX now hold number of clock ticks since midnight      

    mov ax, dx
    xor dx, dx
    mov cx, bx    
    div cx       ; here dx contains the remainder of the division - from 0 to 9

    add dl, '0'  ; to ascii from '0' to '9'   
    pop bx
    ret      
randGen endp

genRandomX proc
    mov bx, 7
    call randGen
    
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
    
    ; if it gets to here somehow call the function again
    ;call genRandomX
    jmp end 
     
    is0:
    jmp end
    is1:
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

createNum proc
    mov bx, 10
    call randGen
    mov [number], dl
    
    call randGen
    mov [number + 1], dl 
              
    call genRandomX
    
    call printNumber          
    ret    
createNum endp

printNumber proc
    ; printing a number
    mov dh, numPosY
    mov dl, numPosX
    mov bx, offset number
    call print
    ret
printNumber endp

printPlayerNumber proc
    ; printing a players movable number
    mov dh, pNumPosY
    mov dl, pNumPosX
;    mov ah, 02h  ; SetCursorPosition
    mov bx, offset playerNumber
    call print
    ret
printPlayerNumber endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

start: 
    ; Setting segment registers
    ASSUME cs: code, ss:stck
    mov ax, data
    mov ds, ax 
    
input:
    ; entering graphical mode
    mov ah, 0
    mov al, 13h
    int 10h
    
    ; printing a score message
    mov dh, 1
    mov dl, 1
    mov bx, offset scoreMessage
    call print
    
    ; drawing the field
;    call drawField
    
    ; calling randGen two times because we want a 2 decimal number
    mov bx, 10
    call randGen
    mov [playerNumber], dl       ; putting first random generated number in a first reserved position
    
    call randGen
    mov [playerNumber + 1], dl   ; putting second random num in a second reserved position
    
    call printPlayerNumber 
    call createNum
    
checkTime:            ;time checking loop
	mov ah, 2Ch       ;get the system time
	int 21h    		  ;CH = hour CL = minute DH = second DL = 1/100 seconds
	
	cmp dl, time  	  ; is the current time equal to the previous one?
	je checkTime      ;if it is the same, check again 
	
	mov time, dl      ; if it's not the same, time becomes current time  

loopMoving:
    readKey direction
    
    cmp direction, 'a'
    je left
    cmp direction, 'd'
    je right
    jmp endPoint     
      
    left:             ; if we're trying to go left
    mov al, numMinX   ; putting variable in a register because you can't compare two variables
    cmp al, pNumPosX  ; compare current x pos of number with min x pos
    je continueLoop   ; if numposX != minPosX don't move left
    jmp continueLeft  ; if numposX != minPosX jump to continueLeft label
    
    continueLeft:     ; move left
    call clearNumber  ; clear the last pos of number 
    sub pNumPosX, 2   ; sub x with 2 (go left 2 pixels)
    jmp continue      
   
    right:            ; if we're trying to go right  
    mov al, numMaxX
    cmp al, pNumPosX
    je continueLoop
    jmp continueRight
    
    continueRight:    ; move right
    call clearNumber
    add pNumPosX, 2
    jmp continue
    
    continue:         ; this prints the number in a new position
    call printPlayerNumber 
    
    continueLoop:     ; after everything is finished, continues to check time
    jmp checkTime
    
endPoint:
    programEnd
ends
end start