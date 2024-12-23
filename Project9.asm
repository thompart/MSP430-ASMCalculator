;-------------------------------------------------------------------------------
;******************************************************************************
;	Project 9
;
;	When the left most push button of the daughter board is pressed, input A is
;	defined by the scroll wheel potentiometer’s voltage  When the right most push button is pressed,
;	input B will be defined similarly. The display should be formatted as follows: “AA:BB”. For
;	example, an input of 25 times 16 should be “25:16”. Values must be in decimal format. When
;	the middle push button is pressed the display should read the multiplication of A and B. If any of
;	the three buttons are pressed, reset the display. All operations must be contained within interrupt
;	service routines (which do not have loops), and the multiplication must be done using the
;	Multiplication Unit of the MSP430. Additionally, ensure the values of A and B do not vary when
;	the scroll wheel is not being touched (reduce noise)
;
;   Tyler Lince
;   Texas Tech University
;******************************************************************************
;-------------------------------------------------------------------------------
            .cdecls C,LIST,"msp430.h"       ; Include device header file
;-------------------------------------------------------------------------------
            .def    RESET                   ; Export program entry-point to
                                            ; make it known to linker.
;-------------------------------------------------------------------------------
            .global _main
            .global __STACK_END
            .sect   .stack                  ; Make stack linker segment ?known?

            .text                           ; Assemble to Flash memory
            .retain                         ; Ensure current section gets linked
            .retainrefs
;-------------------------------------------------------------------------------
SEGA        .set    BIT0 ; P2.0
SEGB        .set    BIT1 ; P2.1
SEGC        .set    BIT2 ; P2.2
SEGD        .set    BIT3 ; P2.3
SEGE        .set    BIT4 ; P2.4
SEGF        .set    BIT5 ; P2.5
SEGG        .set    BIT6 ; P2.6
SEGDP       .set    BIT7 ; P2.7

DIG1        .set    BIT0 ; P3.0
DIG2        .set    BIT1 ; P3.1
DIG3        .set    BIT2 ; P3.2
DIG4        .set    BIT3 ; P3.3
DIGCOL      .set    BIT7 ; P3.7

BTN1		.set	BIT7 ; P4.7 **some boards appear to have BTN1 and BTN3 flipped?
BTN2		.set	BIT3 ; P1.3
BTN3		.set    BIT5 ; P1.5

state		.set	R15		; Stores state for calculator state machine

static		.set	0		; Default state
edita		.set	1		; Editting A state
editb		.set	2		; Editting B state
showmult	.set	3		; Showing multiplication state
showadd		.set	4		; Showing addition state
showsub		.set	5		; Showing subtraction state

digit       .set	R4   	; Digit of 7-seg currently being multiplexed
count		.set	R5	 	; Stores 4 values to be displayed on 7 seg displays
aBCD		.set	R6		; Stores binary coded decimal value of A
bBCD		.set	R7		; Stores binary coded decimal value of B
aHEX		.set	R8		; Stores hex value of A
bHEX		.set	R9		; Stores hex value of A
multHEX		.set	R10		; Stores hex value of multiplication
raw			.set	R11		; Stores raw value of ADC potentiometer
dabbleOut	.set	R12		; Out variable of double dabble algorithm
dabbleIn	.set	R13		; In variable of double dabble algorithm

;-------------------------------------------------------------------------------
_main
RESET       mov.w   #__STACK_END,SP         ; Initialize stackpointer
StopWDT     mov.w #WDTPW+WDTCNTCL+WDTTMSEL+7+WDTSSEL__ACLK+WDTIS_0,&WDTCTL ; Interval mode with ACLK
			bis.w #WDTIE, &SFRIE1                                       ; enable interrupts for the watchdog
;-------------------------------------------------------------------------------
; Configurations
;-------------------------------------------------------------------------------
SetupPB     bic.b   #BIT1+BIT2, &P1DIR      ; Set P1.1 to input direction (Push Button)
			bis.b   #BIT1+BIT2, &P1REN      ; **ENABLE RESISTORS ON BUTTONS
			bis.b   #BIT1+BIT2, &P1OUT      ; **SET TO BE PULLUP
			bis.b   #BIT1+BIT2, &P1IES
			bis.b   #BIT1+BIT2, &P1IE

			bic.b   #BTN1, &P4DIR
			bic.b   #BTN3+BTN2, &P1DIR
			bis.b   #BTN1, &P4REN
			bis.b   #BTN3+BTN2, &P1REN
			bis.b   #BTN1, &P4OUT
			bis.b   #BTN3+BTN2, &P1OUT
			bis.b   #BTN1, &P4IES
			bis.b   #BTN3+BTN2, &P1IES
			bis.b   #BTN1, &P4IE
			bis.b   #BTN3+BTN2, &P1IE

SetupSeg    bic.b   #SEGA+SEGB+SEGC+SEGD+SEGE+SEGF+SEGG+SEGDP,&P2OUT
            bic.b   #DIG1+DIG2+DIG3+DIG4+DIGCOL,&P3OUT
            bis.b   #SEGA+SEGB+SEGC+SEGD+SEGE+SEGF+SEGG+SEGDP,&P2DIR
            bis.b   #DIG1+DIG2+DIG3+DIG4+DIGCOL,&P3DIR
            bic.b   #SEGA+SEGB+SEGC+SEGD+SEGE+SEGF+SEGG+SEGDP,&P2OUT
            bic.b   #DIG1+DIG2+DIG3+DIG4,&P3OUT
            bis.b   #DIGCOL,&P3OUT

;-------------------------------------------------------------------------------
; Timers
;-------------------------------------------------------------------------------

EditClock   mov.b   #CSKEY_H,&CSCTL0_H      ; Unlock CS registers
            mov.w   #DCOFSEL_3,&CSCTL1      ; Set DCO setting for 4MHz
            mov.w   #DIVA__1+DIVS__1+DIVM__1,&CSCTL3 ; MCLK = SMCLK = DCO = 4MHz
            clr.b   &CSCTL0_H               ; Lock CS registers

SetupADC12  bis.w   #ADC12SHT0_10+ADC12MSC+ADC12ON, &ADC12CTL0 ; ADC12SHT = hold time. longer time= more time in isr
			bis.w   #ADC12SHP+ADC12SSEL_3+ADC12CONSEQ_2,&ADC12CTL1    ; Make ADC in consecutive mode
			bis.w   #ADC12RES_2,&ADC12CTL2  ; 12-bit conversion results
            bis.w   #ADC12INCH_10,&ADC12MCTL0; A10 ADC input select; Vref=AVCC
            bis.w   #ADC12IE0,&ADC12IER0    ; Enable ADC conv complete interrupt
			bis.w   #ADC12ENC+ADC12SC, &ADC12CTL0 ; Start conversions

; Refresh timer potentiometer ADC
SetupTA0	mov.w   #CCIE,&TA0CCTL0           ; TACCR0 interrupt enabled
            mov.w   #20000,&TA0CCR0           ;
            bis.w   #TASSEL__SMCLK+MC__CONTINUOUS,TA0CTL ; SMCLK continuous mode
; 50ms Debounce Timer
SetupTA1	mov.w   #CCIE,&TA1CCTL0           ; TACCR0 interrupt enabled
            mov.w   #50000,&TA1CCR0           ; count to 49999 for 50ms delay
            bis.w   #TASSEL__SMCLK+MC__STOP,TA1CTL ; SMCLK stop mode

SetupLED	bis.b   #BIT6, &P3DIR
			bic.b	#BIT6, &P3OUT

;-------------------------------------------------------------------------------

UnlockGPIO  bic.w   #LOCKLPM5,&PM5CTL0      ; Disable the GPIO power-on default
                                            ; high-impedance mode to activate
                                            ; previously configured port settings
;-------------------------------------------------------------------------------
; Reset all
;-------------------------------------------------------------------------------
			bic.b   #BTN3+BTN2,&P1IFG		; clear L and M button interrupt flags
			bic.b   #BTN1,&P4IFG			; clear L and M button interrupt flags
			mov.w	#static, state			; Default state on startup = static

			mov.w   #5, digit				; stores digit being multiplexed

			mov.w	#0,	count				; Clear variables
			mov.w	#0,	aBCD				;
			mov.w	#0,	aHEX				;
			mov.w	#0,	bBCD				;
			mov.w	#0,	bHEX				;
			mov.w	#0,	multHEX				;
			mov.w	#0,	dabbleOut			;
			mov.w	#0,	raw					;
			mov.w	#0, dabbleIn			;
;-------------------------------------------------------------------------------

			nop
			bis.b   #GIE, SR                ; enable all interrupts
			nop

Mainloop    jmp     Mainloop                ; Again

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; Interrupt Service Routines
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
WDT_ISR:	; Multiplexer
;-------------------------------------------------------------------------------
    		push        count		; store count in stack

    		dec         digit		; decrement digit each cycle to multiplex
    		jnz         SkipReset	; if not zero, skip resetting
    		mov         #4, digit	; reset digit back to 4 if digit=0

SkipReset:
    		clr.b       &P2OUT		; clear segments of previous cycle
    		bic.b       #0x0F, &P3OUT	; clear currently stored digit port

    		bis.b       sDIG(digit), &P3OUT		; assign P3OUT to current digit being multiplexed

CheckDig4: ; rightmost
    		cmp         #4, digit		; are we currently mpxing digit 4?
    		jne         CheckDig3		; if not, skip to test next digit
    		and         #0x000F, count	; mask rightmost digit from countdown variable
    		mov.b       BCD(count), &P2OUT	; move digits respective segments to P2OUT for display
    		jmp         WDTend

CheckDig3:
    		rra.w       count			; roll right x4 to make 3rd digit rightmost
    		rra.w       count			;
    		rra.w       count			;
    		rra.w       count			; 1234=count 0123 = 0003
    		cmp         #3, digit		; are we currently mpxing digit 3?
    		jne         CheckDig2		; if not, skip to next digit
    		and         #0x000F, count	; mask rightmost digit from countdown variable
    		mov.b       BCD(count), &P2OUT	; move digits respective segments to P2OUT for display
    		jmp         WDTend

CheckDig2:
    		rra.w       count			; roll right x4 to make 3rd digit (originally 2nd digit) rightmost
    		rra.w       count			;
    		rra.w       count			;
    		rra.w       count			;
    		cmp         #2, digit		; are we currently mpxing digit 2?
    		jne         CheckDig1		; if not, skip to next digit
    		and         #0x000F, count	; mask rightmost digit from countdown variable 0002
    		mov.b       BCD(count), &P2OUT	; move digits respective segments to P2OUT for display
    		jmp         WDTend

CheckDig1:
    		rra.w       count			; roll right x4 to make 3rd digit (originally 1st digit) rightmost
    		rra.w       count			;
    		rra.w       count			;
    		rra.w       count			;
    		cmp         #1, digit		; are we currently mpxing digit 1?
    		jne         WDTend		; if not, leave (not sure how we'd get here)
    		and         #0x000F, count	; mask rightmost digit from countdown variable
    		mov.b       BCD(count), &P2OUT	; move digits respective segments to P2OUT for display

WDTend:
    		pop.w       count	; pop original countdown value back onto count variable
	    	reti

;-------------------------------------------------------------------------------
TIMER0_A0_ISR: ; Editting Handler
; if editting A or B, constantly update aHEX or bHEX with ADC potentiometer value (0-99)
;-------------------------------------------------------------------------------
			cmp		#showmult, state		; Leave here if showing multiplication, addition, subtraction
			jge		T0Reti					;
			cmp		#edita,	state			; If left button pressed, map A to ADC pot value
			jeq		EditA					;
			cmp		#editb, state			; If right button pressed, map B to ADC pot value
			jeq		EditB					;
			cmp		#static,	state		; If in static mode, continue updating display, but don't map
			jeq		T0Exit					; ADC pot value to anything
			reti

EditA:
			rram.w	#4, raw					; Divide raw ADC by 32 (right shift = 1/2^n, n=5)
			rram.w	#1, raw					; Changes ADC range from d4096 -> d128

			cmp		#99, raw				; Cut off upper range of potentiometer to only allow up to d100
			jlo		EditA_Dabble			;
			mov		#99, raw				;

EditA_Dabble:
			mov.w	raw, aHEX				; move 0-100 to aHEX
			mov.w	aHEX, dabbleIn			; prepare dabbleIn for DoubleDabble call
			call	#DoubleDabble			; call DoubleDabble to convert aHEX to aBCD
			mov.w	dabbleOut, aBCD			; store aBCD
			jmp		T0Exit

EditB:
			rram.w	#4, raw					; Divide raw ADC by 32 (right shift = 1/2^n, n=5)
			rram.w	#1, raw					; Changes ADC range from d4096 -> d128

			cmp		#99, raw				; Cut off upper range of potentiometer to only allow up to d100
			jlo		EditB_Dabble			;
			mov		#99, raw				;

EditB_Dabble:
			mov.w	raw, bHEX				; move 0-100 to aHEX
			mov.w	bHEX, dabbleIn			; prepare dabbleIn for DoubleDabble call
			call	#DoubleDabble			; call DoubleDabble to convert aHEX to aBCD
			mov.w	dabbleOut, bBCD			; store aBCD
			jmp		T0Exit					;





T0Exit:
			clr		count					; Continuously updates count according to updated
			bis.w	aBCD,	count			; values of aBCD and bBCD
			swpb	count
			bis.w	bBCD, count
T0Reti:		reti

;-------------------------------------------------------------------------------
TIMER1_A0_ISR:	; Button Debounce / State Management
;-------------------------------------------------------------------------------
			bit.b	#BIT1, &P1IN			; Debounce all inputs, go to respective section
			jz		BTN1_1Pressed
			bit.b	#BIT2, &P1IN
			jz		BTN1_2Pressed
			bit.b	#BTN1, &P4IN
			jz		BTN1Pressed
			bit.b	#BTN2, &P1IN
			jz		BTN2Pressed
			bit.b	#BTN3, &P1IN
			jz		BTN3Pressed

			jmp		TA1Exit

BTN1_1Pressed	; MSP Left Button Pressed, Addition
			cmp		#showmult, state	; If showing an operation, skip changing states, enter static mode @ BufferExit
			jge		BufferExit

			mov.w	#showadd, state		; enter addition state
			call	#Add				;

			jmp		TA1Exit

BTN1_2Pressed	; MSP Right Button Pressed, Subtraction
			cmp		#showmult, state	; If showing an operation, skip changing states, enter static mode @ BufferExit
			jge		BufferExit

			mov.w	#showsub, state		; enter subtract state
			call	#Subtract

			jmp		TA1Exit

BTN1Pressed		; DB Right Button Pressed, Edit B
			cmp		#showmult, state	; If showing an operation, skip changing states, enter static mode @ BufferExit
			jge		BufferExit

			mov.b   #editb, state		; enter edit b state

			jmp		TA1Exit
BTN2Pressed		; DB Middle Button Pressed, Multiplication
			cmp		#showmult, state	; If showing an operation, skip changing states, enter static mode @ BufferExit
			jge		BufferExit

			mov.w	#showmult, state	; enter multiplication state
			call	#Multiply

			jmp		TA1Exit
BTN3Pressed		; DB Middle Button Pressed, EditA
			cmp		#showmult, state	; If showing an operation, skip changing states, enter static mode @ BufferExit
			jge		BufferExit

			mov.b   #edita, state		; enter edit a state

			jmp		TA1Exit

BufferExit	bis.b   #DIGCOL,&P3OUT		; Turn back on colon segment when exiting operation modes
			mov.w	#static, state		; Enter static mode here if any operation mode was on when button was pressed
TA1Exit		bic.w	#MC__UP, &TA1CTL	; Clear debounce timer
			reti


;-------------------------------------------------------------------------------
ADC12_ISR:;  ADC12 interrupt service routine
;-------------------------------------------------------------------------------
            add.w   &ADC12IV,PC             ; add offset to PC
            reti                            ; Vector  0:  No interrupt
            reti                            ; Vector  2:  ADC12MEMx Overflow
            reti                            ; Vector  4:  Conversion time overflow
            reti                            ; Vector  6:  ADC12HI
            reti                            ; Vector  8:  ADC12LO
            reti                            ; Vector 10:  ADC12IN
            jmp     MEM0                    ; Vector 12:  ADC12MEM0 Interrupt
            reti                            ; Vector 14:  ADC12MEM1
            reti                            ; Vector 16:  ADC12MEM2
            reti                            ; Vector 18:  ADC12MEM3
            reti                            ; Vector 20:  ADC12MEM4
            reti                            ; Vector 22:  ADC12MEM5
            reti                            ; Vector 24:  ADC12MEM6
            reti                            ; Vector 26:  ADC12MEM7
            reti                            ; Vector 28:  ADC12MEM8
            reti                            ; Vector 30:  ADC12MEM9
            reti                            ; Vector 32:  ADC12MEM10
            reti                            ; Vector 34:  ADC12MEM11
            reti                            ; Vector 36:  ADC12MEM12
            reti                            ; Vector 38:  ADC12MEM13
            reti                            ; Vector 40:  ADC12MEM14
            reti                            ; Vector 42:  ADC12MEM15
            reti                            ; Vector 44:  ADC12MEM16
            reti                            ; Vector 46:  ADC12MEM17
            reti                            ; Vector 48:  ADC12MEM18
            reti                            ; Vector 50:  ADC12MEM19
            reti                            ; Vector 52:  ADC12MEM20
            reti                            ; Vector 54:  ADC12MEM21
            reti                            ; Vector 56:  ADC12MEM22
            reti                            ; Vector 58:  ADC12MEM23
            reti                            ; Vector 60:  ADC12MEM24
            reti                            ; Vector 62:  ADC12MEM25
            reti                            ; Vector 64:  ADC12MEM26
            reti                            ; Vector 66:  ADC12MEM27
            reti                            ; Vector 68:  ADC12MEM28
            reti                            ; Vector 70:  ADC12MEM29
            reti                            ; Vector 72:  ADC12MEM30
            reti                            ; Vector 74:  ADC12MEM31
            reti                            ; Vector 76:  ADC12RDY

MEM0:      	mov.w   &ADC12MEM0, raw			; Mov the MEM0 value, clears flags
			reti

;-------------------------------------------------------------------------------
PORT1_ISR;    Port 1 ISR (Left and Middle DB Buttons, MSP 1.1 & 1.2 Buttons)
;-------------------------------------------------------------------------------
			bis.w	#MC__UP+TACLR, &TA1CTL  ; go to debounce timer
P1EXIT		bic.b   #BTN3+BTN2+BIT1+BIT2,&P1IFG	; clear all button interrupt flags
			reti

;-------------------------------------------------------------------------------
PORT4_ISR;    Port 4 ISR (Right DB Button)
;-------------------------------------------------------------------------------
			bis.w	#MC__UP+TACLR, &TA1CTL  ; go to debounce timer
P4EXIT		bic.b   #BTN1,&P4IFG	; clear button interrupt flag
			reti

;------------------------------------------------------------------------------
; Other Service Routines
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
DoubleDabble: ; Double Dabble Algorithm
; two 16 bit registers, input and output. shift input and output left such that
; leftmost bit of input is shifted onto rightmost bit of output. then test each
; nibble, if greater than 5, add 3. repeat
; for some god forsaken reason, this converts hex to binary
;------------------------------------------------------------------------------
			clr		dabbleOut			; clear output
			mov.w 	#16, count

DabbleLoop:	clrc				; clear carry bit
			rlc.w	dabbleIn	; roll input left with carry bit
			rlc.w	dabbleOut	; carry bit gets rolled into output
			dec.w	count		;
			jz		DoubleDabbleExit	; if 16 rolls complete, exit algorithm

DabbleOnes:	push	dabbleOut	; save dabbleOut
			and.w	#0x000F, dabbleOut	; mask ones place
			cmp.w	#0x0005, dabbleOut	;
			jl		DabbleTens			; if ones place > 5
			pop.w	dabbleOut			; bring back unmasked value
			add.w	#0x0003, dabbleOut	; add 3 to ones place
			push.w	dabbleOut			;

DabbleTens:	pop		dabbleOut			; all this push and pop nonsense is to get around using a
			push	dabbleOut			; temporary variable for the mask comparison
			and.w	#0x00F0, dabbleOut	;
			cmp.w	#0x0050, dabbleOut	; repeat for tens place
			jl		DabbleHundreds		;
			pop.w	dabbleOut			;
			add.w	#0x0030, dabbleOut	;
			push	dabbleOut			;

DabbleHundreds:
			pop		dabbleOut			;
			push	dabbleOut			;
			and.w	#0x0F00, dabbleOut	;
			cmp.w	#0x0500, dabbleOut	;
			jhs		dz					; honestly i don't remember why i did this but it works
			pop		dabbleOut			;
			jmp		DabbleLoop			;

dz:			pop		dabbleOut			; i have tamed the algorithm, i dare not taunt it
			add.w	#0x0300, dabbleOut
			jmp		DabbleLoop

DoubleDabbleExit:
			ret

;------------------------------------------------------------------------------
Multiply: ; Multiply Function
;------------------------------------------------------------------------------
			mov.w		aHEX, MPY			; Move hex of A to multiplier
			mov.w		bHEX, OP2			; Move hex of B to multiplier
			nop								; Wait 3 clock cycles for multiplier to update
			nop
			nop
			mov.w		RES0, multHEX		; Multiplied value -> multHEX
			mov.w		multHEX, dabbleIn	; Move multHEX -> dabbleIn for DoubleDabble
			call		#DoubleDabble		; Call DoubleDabble to convert result from hex to decimal
			mov.w		dabbleOut, count			; After double dabble, display dabbleOut result
			bic.b   #DIGCOL,&P3OUT	; Clear colon segment before showing operation
			ret

;------------------------------------------------------------------------------
Add: ; Add Function
;------------------------------------------------------------------------------
			push		bBCD				; Save bBCD
			dadd.w		aBCD, bBCD			; Decimal add aBCD and bBCD, sum goes to bBCD
			mov.w		bBCD, count			; Move sum to count
			pop			bBCD				; Load bBCD
			bic.b   #DIGCOL,&P3OUT	; Clear colon segment before showing operation
			ret

;------------------------------------------------------------------------------
Subtract: ; Subtract Function
;------------------------------------------------------------------------------
			cmp.w		bBCD, aBCD	; Compare A and B
			jlo			SubError	; If A is lower than B, go to Negative Mode

			; Positive Mode
			push		aHEX		; Save aHEX
			sub.w		bHEX, aHEX	; Subtract b from a
			mov.w		aHEX, dabbleIn	; Move aHEX to dabble input
			call		#DoubleDabble	; Run Double Dabble
			mov.w		dabbleOut, count	; Move result to count
			pop			aHEX			; Load aHEX
			bic.b   #DIGCOL,&P3OUT		; Clear colon segment before showing operation
			ret

			; Negative Mode
SubError	push		bHEX		; Save bHEX
			sub.w		aHEX, bHEX	; Subtract a from b
			mov.w		bHEX, dabbleIn	; Move bHEX to dabble input
			call		#DoubleDabble	; Run Double Dabble
			swpb		dabbleOut		; Swap bytes of dabble output
			bis.w		#0xDE, dabbleOut	; Move negative sign to dabble out
			swpb		dabbleOut			; Move negative sign to correct slot
			mov.w		dabbleOut, count	; Move result to count
			pop			bHEX			; Load bHEX
			bic.b   #DIGCOL,&P3OUT		; Clear colon segment before showing operation
			ret

;-------------------------------------------------------------------------------
; Look Up Tables
;-------------------------------------------------------------------------------
; Hex -> Segment conversion
BCD         .byte   SEGA+SEGB+SEGC+SEGD+SEGE+SEGF      ; 0
            .byte        SEGB+SEGC                     ; 1
            .byte   SEGA+SEGB+     SEGD+SEGE+     SEGG ; 2
            .byte   SEGA+SEGB+SEGC+SEGD+          SEGG ; 3
            .byte        SEGB+SEGC+          SEGF+SEGG ; 4
            .byte   SEGA+     SEGC+SEGD+     SEGF+SEGG ; 5
            .byte   SEGA+     SEGC+SEGD+SEGE+SEGF+SEGG ; 6
            .byte   SEGA+SEGB+SEGC                     ; 7
            .byte   SEGA+SEGB+SEGC+SEGD+SEGE+SEGF+SEGG ; 8
            .byte   SEGA+SEGB+SEGC+SEGD+     SEGF+SEGG ; 9
            .byte   SEGA+SEGB+SEGC+     SEGE+SEGF+SEGG ; A
            .byte             SEGC+SEGD+SEGE+SEGF+SEGG ; b
            .byte   SEGA+          SEGD+SEGE+SEGF      ; C
            .byte   0x0								   ; D (null)
            .byte   							  SEGG ; E (-)
            .byte                  		SEGE	 +SEGG ; F (r)

sDIG        .byte   0
			.byte   DIG1
			.byte   DIG2
			.byte   DIG3
			.byte   DIG4

;------------------------------------------------------------------------------
;           Interrupt Vectors
;------------------------------------------------------------------------------
            .sect   ".reset"                ; MSP430 RESET Vector
            .short  RESET
            .sect   WDT_VECTOR              ; Watchdog Timer
            .short  WDT_ISR
            .sect   ADC12_VECTOR            ; ADC12 Vector
            .short  ADC12_ISR               ;
            .sect   PORT1_VECTOR        ; BTN2+BTN3 Interrupt Vector
            .short  PORT1_ISR
            .sect   PORT4_VECTOR        ; BTN1 Interrupt Vector
            .short  PORT4_ISR
            .sect   TIMER0_A0_VECTOR        ; Timer0_A0 CC0 Interrupt Vector
            .short  TIMER0_A0_ISR
            .sect   TIMER1_A0_VECTOR        ; Timer0_A0 CC0 Interrupt Vector
            .short  TIMER1_A0_ISR
            .end
