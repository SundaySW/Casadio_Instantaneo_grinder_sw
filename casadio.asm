list p=16f628a
include <p16F628a.inc>
__CONFIG _FOSC_INTOSCIO & _WDTE_OFF & _PWRTE_OFF & _MCLRE_OFF & _BOREN_ON & _LVP_OFF & _CPD_OFF & _CP_OFF
 
;*** Variables ******************************************
variable    eeprom_adr = 0x0
variable    stage = 0x1

;*** Constants ******************************************
REG1	EQU 0x21
REG2	EQU 0x22
REG3	EQU 0x23
REG4	EQU 0xC1	    ;Bank1 eeprom buffer
REG5	EQU 0x51	    ;pause reg
REG6	EQU 0x74
DELAYst1   EQU 0x24
DELAYst2   EQU 0x25
DELAYst3   EQU 0x26
DELAYst4   EQU 0x27
REG11   EQU 0x28
REG12   EQU 0x29 
REG9	EQU 0x2A
REGA	EQU 0x2B

bit1	EQU 0x3
bit2	EQU 0x4
bit3	EQU 0x6
bit4	EQU 0x7
	
SEG1	EQU 0x40	    ;display segments data storage to show
SEG2	EQU 0x41
SEG3	EQU 0x42
SEG4	EQU 0x43
TIMER2	EQU 0x46	    ;for btn lag
TIMER3	EQU 0x47	    ;for setting time for start btns
TIMER4	EQU 0x48
TIMER5	EQU 0x50
BTNFLAGS    EQU 0x49	
	;BTNFLAGS,0	any button was pressed and still operating
	;BTNFLAGS,1	for save procedure for btn1 time setting
	;BTNFLAGS,2	for save procedure for btn2 time setting
	;BTNFLAGS,3	if btn2 was pressed (not used frag)
	;BTNFLAGS,4	if btn2 was pressed for main counter inc
TMRFLAGS    EQU 0x55
	;TMRFLAGS,2	TIMER2 working
	;TMRFLAGS,3	TIMER3 working
	;TMRFLAGS,5	TIMER5 working
EADR	EQU	0x4B
W_ISR   EQU     H'70'	    ; For context saving in INTs.
S_ISR   EQU     H'71'
P_ISR   EQU     H'72'
F_ISR   EQU     H'73'
 		
;==========================
; INIT
;==========================
    ORG     0x0
    GOTO    START
;==========================
; Interrupt
;==========================
    ORG     0004
 ; Save
	bcf	STATUS,RP0
        movwf   W_ISR                   ; Save W to W_ISR
        swapf   STATUS, W               ; Use SWAPF instruction so status bits don't change
        movwf   S_ISR                   ; Save Status to S_ISR
        clrf    STATUS                  ; Switch to Bank 0
        movf    PCLATH, W               ; Move PCLATH to W register
        movwf   P_ISR                   ; Save PCLATH to P_ISR
        clrf    PCLATH                  ; Force page 0
        movf    FSR, W                  ; Move FSR to W register
        movwf   F_ISR                   ; Save FSR to F_ISR
; Interrupt content
	btfsc	TMRFLAGS,2
	goto	tmr2
	btfsc	TMRFLAGS,3
	goto	tmr3
	btfsc	TMRFLAGS,5
	goto	tmr5
	goto	continueISR
tmr5
	decfsz  TIMER5,F
	goto	continueISR
	call	finish_timer5
	goto	continueISR
tmr3
	incf	TIMER3,F
	btfss	TIMER3,3
	goto	continueISR
	call	finish_timer3
	goto	continueISR
tmr2
	call	minusing
	movf    SEG1,F
	btfsc   STATUS,Z
	call	finish_timer4
	
; restore
continueISR
        bcf     PIR1, TMR1IE            ; Clear TMR1 interrupt flag while still in Bank 0        
        movf    F_ISR, W                ; MOVE F_ISR to W
        movwf   FSR                     ; Restore FSR
        movf    P_ISR, W                ; MOVE P_ISR to W
        movwf   PCLATH                  ; Restore PCLATH
        swapf   S_ISR, W                ; Undo previous SWAPF, place result in W
        movwf   STATUS                  ; Restore STATUS
        swapf   W_ISR, F                ; Use SWAPF instruction so status bits don't change
        swapf   W_ISR, W                ; Undo previous SWAPF and restore W register
    retfie				; Return From Interrupt
    
;********************************************************
;MACROSes
;********************************************************
;Increment for deñ format through 4 digits
plus	macro	stage
    while(stage < 5)
	incf    SEG#v(stage),F
	bcf	STATUS,C
	movlw   0xF6	;247 Dec
	addwf   SEG#v(stage),W
	btfss   STATUS,C
	return
	movlw   0x0
	movwf   SEG#v(stage)
	stage += 1
    endw
endm
;******************************************************** 
;Decrement for deñ format through 4 digits
minus	macro	stage
	if(stage == 1)
	    bcf	    STATUS,Z
	    movf    SEG3,F
	    btfss   STATUS,Z
	    decf    SEG3,F
	    btfss   STATUS,Z
	    return
	    movlw   0x9
	    movwf   SEG3
	    bcf	    STATUS,Z
	    movf    SEG4,F
	    btfss   STATUS,Z
	    decf    SEG4,F
	else
	    bcf	    STATUS,Z
	    movf    SEG1,F
	    btfss   STATUS,Z
	    decf    SEG1,F
	    btfss   STATUS,Z
	    return
	    bcf	    STATUS,Z
	    movf    SEG2,F
	    btfsc   STATUS,Z
	    return
	    decf    SEG2,F
	    movlw   0x9
	    movwf   SEG1
	endif
endm
	minusing
	    stage = 2
	    minus	stage
	return
;********************************************************
load_from_eeprom macro eeprom_adr, stage
    while(stage < 5)
	call	eeprom_pause
	eeprom_adr += 1
	movlw   eeprom_adr
	call    Read
	movwf   SEG#v(stage)
	stage += 1
    endw
endm
;******************************************************** 
write_to_eeprom macro eeprom_adr, stage
    while(stage < 5)
	movf	SEG#v(stage),W
	banksel REG4
	movwf	REG4
	eeprom_adr += 1
	movlw   eeprom_adr
	call    Write
	call    eeprom_pause	;this current pic bug on write, must be a pause
	stage	+=1
    endw
endm
;******************************************************** 
;one cycle of toggling 4 transistors showing data in SEG regs
load_disp2 macro    stage
    while(stage < 5)
	bcf	PORTB,bit#v(stage)
	movf    SEG#v(stage),W
	call	TABLE
	movwf   PORTA
	call    displ_pause
	bsf	PORTB,bit#v(stage)
	movlw	0xFF
	movwf	PORTA
	stage	+=1
    endw
endm

;********************************************************
TABLE
	addwf	PCL,F	    ;Install counter PC=PC+W
	retlw	0xA0		;0
	retlw	0xD9		;1
	retlw	0x44		;2
	retlw	0x50		;3
	retlw	0x19		;4
	retlw	0x32		;5
	retlw	0x2		;6
	retlw	0xD8		;7
	retlw	0x20		;8
	retlw	0x30		;9
	retlw	0x7F		;-  (A)
	retlw	0x6F		;r  (B)
	retlw	0xE3		;u  (C)
	retlw	0x6B		;n  (D)
	retlw	0xFF		;off(E)
	retlw	0x61		;d  (F)
	retlw	0x32		;S  (10)
;********************************************************
Write		
	bsf	STATUS,RP0
	movwf	EEADR		;W to Adress
	movf	REG4,W		;REG4 to W
	movwf	EEDATA		;W to Data
	bsf	EECON1,WREN	;EE Read
	bcf	INTCON,GIE
	movlw	0x55
	movwf	EECON2		;Write 55h
	movlw	0xAA
	movwf	EECON2		;Write AAh
	bsf	EECON1,WR	;Set WR bit
	bsf	INTCON,GIE	;Enable INTs.
	bcf	STATUS,RP0
	return
;********************************************************
Read
	bsf	STATUS,RP0	;Bank 1
	movwf	EEADR		;Adress to Read
	bsf	EECON1,RD	;EE Read
	movf	EEDATA,W	;W = EEDATA
	movwf	REG4		;Reg4 = W
	bcf	STATUS,RP0
	return
;********************************************************
;********************************************************
START
    bcf	    STATUS, RP1
    bsf	    STATUS, RP0
    clrf    TRISA
    movlw   B'00000000'     
    movwf   TRISB
    bcf	    STATUS, RP0
    movlw   0xD8
    movwf   PORTB	;turn off all 4 transistors
    
    movlw   0xA9
    movwf   TIMER2
    movlw   0x0
    movwf   TIMER3
    movlw   0x0
    movwf   TIMER4
    movlw   0x5
    movwf   TIMER5
    movlw   0x0
    movwf   BTNFLAGS
    movlw   0x0
    movwf   TMRFLAGS

    movlw   0xE	
    movwf   SEG1
    movlw   0xF    
    movwf   SEG2
    movlw   0xD    
    movwf   SEG3    
    movlw   0x10
    movwf   SEG4
;=======================================================
;TURN OFF
;=======================================================
;COMPARATOR OFF
    banksel CMCON
    movlw   0x7
    movwf   CMCON
;=======================================================
;=======================================================
;TMR1 SET-UP
    bsf	    STATUS,RP0
    bsf     PIE1, TMR1IE	    ;enable Timer1 overflow int
    bcf	    STATUS,RP0
    bsf     INTCON, GIE             ; Enable Global Interrupts
    bsf     INTCON, PEIE            ; Enable Peripheral Interrupts
    bcf	    PIR1, TMR1IF
    movlw   B'00100100'		    ;TMR1 1:4
    movwf   T1CON
    bsf	    T1CON,TMR1ON
;=======================================================

;Show logo on start
    bsf	    TMRFLAGS,5
show		
    stage = 1
    load_disp2	stage
    decfsz  TIMER2,F
    goto    show
    call    pauseSTART
    btfss   TMRFLAGS,5
    goto    main
    goto    show
;main loop after start, toggling display transistors    
main
    call    pauseSTART
    eeprom_adr = 0x11
    stage = 1
    load_from_eeprom eeprom_adr, stage	    ;load main counter digits to regs
loop
    btfsc   TMRFLAGS,2
    goto    lp
    decfsz  TIMER2,F
    goto    lp
    call    check_btn
lp
    stage = 1
    load_disp2	stage			    ;toggling transistors
    goto    loop
;check if any of 4 btns is pressed    
check_btn
    movlw   b'11011000'
    andwf   PORTB,F	    ;must turn transistors off to toggle TRISA
    bsf	    STATUS,RP0	
    movlw   0xF
    movwf   TRISA	    ;put PORTA on input
    bcf	    STATUS,RP0
    call    delay_small	    ;little pause should be for propper work
	btfss   PORTA,0
	call    plus_
	btfss   PORTA,2
	call    min_
	btfss   PORTA,1
	call    btn1_
	btfss   PORTA,3
	call    btn2_
    bsf	    STATUS,RP0
    clrf    TRISA
    bcf	    STATUS,RP0
    movlw   0xF
    movwf   TIMER2	    ;resore timer
return

;============BTN+
plus_
	btfsc	TMRFLAGS,2	;check if motor running
	return
	btfsc	BTNFLAGS,0
	goto	increm		;if already setting time for 1 btn
	goto	load_btn1_time	;goto init
increm
	clrf	TIMER3		
	stage = 3
	plus stage
	return
load_btn1_time
	bsf	BTNFLAGS,0
	bsf	BTNFLAGS,1
	clrf	TIMER3
	eeprom_adr = 0x21
	stage = 1
	load_from_eeprom eeprom_adr, stage
	bsf	TMRFLAGS,3
	bsf	T1CON,TMR1ON
    return
;============BTN-
min_
	btfsc	TMRFLAGS,2
	return
	btfsc	BTNFLAGS,0
	goto	decrem
	goto	load_btn2_time
decrem
	clrf	TIMER3
	stage = 1
	minus stage
	return
load_btn2_time
	bsf	BTNFLAGS,0
	bsf	BTNFLAGS,2
	clrf	TIMER3
	eeprom_adr = 0x31
	stage = 1
	load_from_eeprom eeprom_adr, stage
	bsf	TMRFLAGS,3
	bsf	T1CON,TMR1ON
    return
;============BTN1
btn1_
    btfsc   BTNFLAGS,0
    return
    bsf	    BTNFLAGS,0
    bsf	    BTNFLAGS,3
    bsf	    TMRFLAGS,2
    bsf	    PORTB,0	    ;turn 
    bsf	    PORTB,5	    ; motor ON
    	movlw   0x24	    ;load time in disp regs
	call    Read
	movwf   SEG1
	movlw   0x25
	nop
	call    Read
	movwf   SEG2
	movlw   0xE
	movwf   SEG3
	movwf   SEG4
    bsf	    TMRFLAGS,2	    ;switch TIMER2 on
    bsf	    T1CON,TMR1ON 
    return
;============BTN2
btn2_
    btfsc   BTNFLAGS,0
    return
    bsf	    BTNFLAGS,0
    bsf	    TMRFLAGS,2
    bsf	    BTNFLAGS,4
    bsf	    PORTB,0
    bsf	    PORTB,5
	movlw   0x34
	call    Read
	movwf   SEG1
	movlw   0x35
	nop
	call    Read
	movwf   SEG2
	movlw   0xE
	movwf   SEG3
	movwf   SEG4
    bsf	    TMRFLAGS,2
    bsf	    T1CON,TMR1ON 
    return
;============TMR3
finish_timer3
    bcf	    T1CON,TMR1ON
    clrf    TIMER3
    clrf    TMRFLAGS
	btfss	BTNFLAGS,1
	goto	btn2
	btn1
	    stage = 3
	    eeprom_adr = 0x23
	    write_to_eeprom eeprom_adr, stage
	    goto    cont_finish_tmr3
	btn2
	    stage = 3
	    eeprom_adr = 0x33
	    write_to_eeprom eeprom_adr, stage
    cont_finish_tmr3
	stage = 1
	eeprom_adr = 0x11
	load_from_eeprom eeprom_adr, stage
    banksel BTNFLAGS
    clrf    BTNFLAGS
    return
;============TMR4
finish_timer4
	bcf	T1CON,TMR1ON
	bcf	STATUS,RP0
	clrf	TMRFLAGS
	bcf	PORTB,0
	bcf	PORTB,5
	    stage = 1
	    eeprom_adr = 0x11
	    load_from_eeprom eeprom_adr, stage
	    call    inc_counter
	    btfsc   BTNFLAGS,4
	    call    inc_counter
	    stage = 1
	    eeprom_adr = 0x11
	    write_to_eeprom eeprom_adr, stage
    clrf    BTNFLAGS
    return
    
inc_counter
    stage = 1
    plus stage
    return
;============TMR5   
finish_timer5
    bcf	    T1CON,TMR1ON
    bcf	    STATUS,RP0
    clrf    TMRFLAGS
    clrf    BTNFLAGS
    return
    
;********************************************************    
eeprom_pause
	movlw   0x9
	movwf   REG1
	movlw   0x9
	movwf   REG2
wr  
	decfsz  REG1, F
	goto    wr
	decfsz  REG2, F
	goto    wr
	return
   
displ_pause
	movlw	0x3
	movwf   REG9
	movlw	0x3
	movwf   REGA
wr3
	decfsz  REG9, F
	goto    wr3
	decfsz  REGA, F
	goto    wr3
	return	
	
pauseSTART
	movlw   0x4
	movwf   REG3
	movlw   0x4
	movwf   REG5
	movlw   0x4
	movwf   REG6
wr4  
	decfsz  REG3, F
	goto    wr4
	decfsz  REG5, F
	goto    wr4
	decfsz  REG6, F
	goto    wr4
	bsf	TIMER2,6
	return
	
delay_small
	movlw   0x3
	movwf   REG1
wr2
	decfsz  REG1, F
	goto    wr2
	return
END
