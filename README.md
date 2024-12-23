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
