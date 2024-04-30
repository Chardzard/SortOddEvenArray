;*****************************************************************************************;
; Program Name: Array Calculations														  ;
;																						  ;
; Program Description: We will take a length, 'n', from the user that is greater than 25. ;
;					   We will then generate an 'n' length array and fill it with random  ;
;					   values from 1 to 1000. We will add all odd values to its own array ;
;					   and also do the same for even values. Then we sort the odd values  ;
;					   array in ascending order while all the evens will be sorted in     ;
;					   descending order. Finally, we will output all our findings to the  ;
;					   console in a pretty format that the end user can easily understand ;
;																						  ;
; Program Author: Parke																	  ;
; Creation Date: 04/17/2024																  ;
; Revisions: N/A					 													  ;
; Date Last Modified: 04/20/2024														  ;
;*****************************************************************************************;

;*********************************************;
; 8386, flat memory model, stack size and	  ;
; ExitProcess prototype initalizations as	  ;
; well as Irvine32 library INCLUDE statements ;
;*********************************************;
.386
.model flat,stdcall
.stack 4096
ExitProcess PROTO, dwExitCode:DWORD
INCLUDELIB C:\masm32\lib\Irvine32.lib
INCLUDE C:\masm32\include\Irvine32.inc

LOW_VAL = 1											 ; Low side of random num seed used in FILL_ARRAY proc
HIGH_VAL = 1000										 ; High side of random num seed used in FILL_ARRAY proc

; Declared and initialized (or un-initialized) variables
.data
	raw_data DWORD ?								 ; List of raw_data vals to fill in & read from

	user_prompt BYTE "Enter a length greater than "  ; Prompt for user input
				BYTE "25: ", 0

	bad_input_message BYTE "Bad input, please re-"	 ; Error message to display to console if user enters str with a length less than 25
					  BYTE "run the program & follow "
					  BYTE "the instructions provided", 0

	CLEAR_MEM BYTE 100 DUP(0)						 ; As annoying as it is I had so many strings declared in memory around similar addresses
													 ; so I needed to clear some space for strings I need to display to the user

	
	print_even_message BYTE "Here are your even values: ", 0
	print_odd_message BYTE "Here are your odd values: ", 0

	bad_len DWORD 25								 ; Value to compare with str taken from user if user enters str with a length less than 25 
	N DWORD ?										 ; Storage var for amount of words to iterate through based on user input
	odd_counter BYTE 0								 ; Odd and even counters to give proper loop vals to ECX for sorting procedures
	even_counter BYTE 0
	odd_vals DWORD 1000 DUP(?)						 ; Arrays to hold odd & even vals
	even_vals DWORD 1000 DUP(?)
	comma BYTE ", ", 0								 ; Comma seperator for prettyfied output

	CLEAR_MEM2 BYTE 100 DUP(0)						 ; As annoying as it is I had so many strings declared in memory around similar addresses
													 ; so I needed to clear some space for strings I need to display to the user

; Main program driver
.code
main PROC											 ; Program entry
	MOV EDX, OFFSET user_prompt						 ; WriteString uses EDX register for location of str to write to console
	CALL WriteString								 ; Write user_prompt to console
	CALL ReadDec									 ; Read array int val from user 
	CMP bad_len, EAX								 ; Check if user input was at least 25 chars
	JAE BAD_INPUT_LEN								 ; Jump if less than 25 for alternate processing
	MOV N, EAX										 ; Use 'N' as amount of words user wants to iterate over
	MOV ECX, N										 ; Use 'N' as loop counter 
	CALL FILL_ARRAY									 ; Fill_Array proc call
	MOV ECX, N										 ; Use 'N' as loop counter
	CALL FIND_EVENS									 ; Find_Evens proc call
	MOV ECX, N										 ; Use 'N' as loop counter
	CALL FIND_ODDS									 ; Find_Odds proc call
	MOV CL, odd_counter								 ; Use odd_counter to loop through exact size of odd_vals array
	DEC ECX											 ; Point to last array element for sorting
	CALL SORT_ODDS									 ; Sort_Odds proc call
	MOV CL, even_counter							 ; Use even_counter to loop through exact size of even_vals array
	DEC ECX											 ; Point to last array element for sorting
	CALL SORT_EVENS									 ; Sort_Evens proc call
	MOV CL, odd_counter								 ; Use odd_counter to loop through exact size of odd_vals array
	CALL Crlf										 ; Insert newline for readability
	MOV ESI, OFFSET odd_vals						 ; Ptr to odd_vals first array index
	MOV EDX, OFFSET print_odd_message				 ; Print message to user
	CALL WriteString
	CLD												 ; Direction = forward

PRINT_ODDS:
	LODSD											 ; Load [ESI] into EAX
	CALL WriteDec									 ; Send to console
	CMP ECX, 1
	JE EXIT_PROCESS
	MOV EDX, OFFSET comma							 ; Insert comma
	CALL WriteString
	LOOP PRINT_ODDS

EXIT_PROCESS:
	MOV CL, even_counter							 ; Use even_counter to loop through exact size of even_vals array
	CALL Crlf										 ; Insert newline for readability
	CALL Crlf										 ; Insert newline for readability
	MOV ESI, OFFSET even_vals						 ; Ptr to even_vals first array index
	MOV EDX, OFFSET print_even_message				 ; Print message to user
	CALL WriteString
	CLD												 ; Direction = forward
	PRINT_EVENS:
		LODSD										 ; Load [ESI] into EAX
		CALL WriteDec								 ; Send to console
		CMP ECX, 1
		JE FINAL_EXIT_PROCESS
		MOV EDX, OFFSET comma						 ; Insert comma
		CALL WriteString
		LOOP PRINT_EVENS

FINAL_EXIT_PROCESS:
	CALL Crlf										 ; Insert newline for readability
	CALL Crlf										 ; Insert newline for readability
	INVOKE ExitProcess, 0							 ; Return 0, exit success

BAD_INPUT_LEN:										 ; Label to handle exit process in case user enters a val <= 25
	CALL Crlf										 ; Insert newline for readability
	MOV EDX, OFFSET bad_input_message				 ; WriteString uses EDX register to read str from
	CALL WriteString								 ; Proc call
	CALL Crlf										 ; Insert newline for readability
	INVOKE ExitProcess, 0							 ; Return 0, exit success

main ENDP

;*****************************************************************;
; Function: Sort_Odds											  ;
; Return: Nothing												  ;
; Procedure: Sorts array of 32-bit unsigned integers in ascending ;									  
;			 order using the common bubble sort algorithm		  ;
;*****************************************************************;
SORT_ODDS PROC

L1:
	PUSH ECX										 ; Save outer loop count
	MOV ESI, OFFSET odd_vals						 ; Ptr to odd_vals first array value

L2:
	MOV EAX, [ESI]									 ; Get array val
	CMP [ESI+4], EAX								 ; Compare vals
	JG L3											 ; if [ESI] <= [ESI+4], no exchange
	XCHG EAX, [ESI+4]								 ; Else, exchange the pair
	MOV [ESI], EAX									 

L3:
	ADD ESI, 4										 ; Move both ptrs forward
	LOOP L2											 ; Inner loop
	POP ECX											 ; Retrieve outer loop count
	LOOP L1											 ; Else, repeat outer loop

L4:
	RET												 ; Return from proc
SORT_ODDS ENDP

;******************************************************************;
; Function: Sort_Evens											   ;
; Return: Nothing												   ;
; Procedure: Sorts array of 32-bit unsigned integers in descending ;									  
;			 order using the common bubble sort algorithm		   ;
;******************************************************************;
SORT_EVENS PROC

L1:
	PUSH ECX										 ; Save outer loop count
	MOV ESI, OFFSET even_vals						 ; Ptr to even_vals first array value

L2:
	MOV EAX, [ESI]									 ; Get array val
	CMP [ESI+4], EAX								 ; Compare vals
	JL L3											 ; if [ESI] >= [ESI+4], no exchange
	XCHG EAX, [ESI+4]								 ; Else, exchange the pair
	MOV [ESI], EAX									 

L3:
	ADD ESI, 4										 ; Move both ptrs forward
	LOOP L2											 ; Inner loop
	POP ECX											 ; Retrieve outer loop count
	LOOP L1											 ; Else, repeat outer loop

L4:
	RET												 ; Return from proc
SORT_EVENS ENDP

;*****************************************************************;
; Function: Find_Evens											  ;
; Return: Nothing												  ;
; Procedure: Sets proper offsets to raw_data and even_vals then   ;
;			 loops through raw_data and adds all even vals to	  ;
;			 'even_vals' array									  ;
;*****************************************************************;
FIND_EVENS PROC
	MOV ESI, OFFSET raw_data						 ; Next two lines move proper offsets into source and destination registers for proc call
	MOV EDI, OFFSET even_vals

L1:
	MOV EAX, [ESI]									 ; The next 8 lined code block takes value at loop index & divides by 2,
	MOV EBX, 2										 ; EDX now holds remainder. If EDX == 0, then index val is even and we can
	XOR EDX, EDX									 ; add to 'even_vals' array with our ALTERNATE label code block
	DIV EBX
	CMP EDX, 0
	JE ALTERNATE
	ADD ESI, TYPE raw_data							 ; Increment index val
	LOOP L1
	RET												 ; Return from proc

ALTERNATE:
	MUL EBX
	MOV [EDI], EAX
	INC even_counter
	ADD EDI, TYPE even_vals							 ; Increment index vals
	ADD ESI, TYPE raw_data
	LOOP L1
	RET												 ; Return from proc in case last val is even and has to go through alternate processing

FIND_EVENS ENDP

;*****************************************************************;
; Function: Find_Odds											  ;
; Return: Nothing												  ;
; Procedure: Sets proper offsets to raw_data and odd_vals then    ;
;			 loops through raw_data and adds all odd vals to	  ;
;			 'odd_vals' array									  ;
;*****************************************************************;
FIND_ODDS PROC
	MOV ESI, OFFSET raw_data						 ; Next two lines move proper offsets into source and destination registers for proc call
	MOV EDI, OFFSET odd_vals

L1:
	MOV EAX, [ESI]									 ; The next 8 lined code block takes value at loop index & divides by 2,
	MOV EBX, 2										 ; EDX now holds remainder. If EDX != 0, then index val is odd and we can
	XOR EDX, EDX									 ; add to 'odd_vals' array with our ALTERNATE label code block
	DIV EBX
	CMP EDX, 0
	JNE ALTERNATE
	ADD ESI, TYPE raw_data							 ; Increment index val
	LOOP L1
	RET												 ; Return from proc

ALTERNATE:
	INC EAX
	MUL EBX
	DEC EAX
	MOV [EDI], EAX
	INC odd_counter
	ADD EDI, TYPE odd_vals							 ; Increment index vals
	ADD ESI, TYPE raw_data
	LOOP L1
	RET												 ; Return from proc in case last val is odd and has to go through alternate processing

FIND_ODDS ENDP

;*****************************************************************;
; Function:	Fill_Array											  ;
; Return: Nothing												  ;
; Procedure: Sets ECX to val taken from user input and loops from ;
;			 offset of raw_data array until it hits delimeter	  ;
;			 val taken from user, filling each array index along  ;
;			 the way with a pseudo-random integer value			  ;
;*****************************************************************;
FILL_ARRAY PROC
	PUSH N											 ; Preserve N val for later use
	CALL Randomize									 ; Initialize starting seed val
	MOV ESI, DWORD PTR raw_data						 ; Ptr to primary array
	MOV EDX, HIGH_VAL
	SUB EDX, LOW_VAL
	CLD

L1:													 ; Loop until we hit user inputted delimeter, adding rand int vals along the way
	MOV EAX, EDX
	CALL RandomRange
	ADD EAX, LOW_VAL
	MOV raw_data[ESI], EAX
	ADD ESI, TYPE raw_data
	LOOP L1
	POP N											 ; Restore N val
	RET												 ; Return from proc

FILL_ARRAY ENDP

END main											 ; Program exit
