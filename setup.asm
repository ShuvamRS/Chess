.include "moves.asm"
.text

# This function inserts colored squares of the game board into the MMIO region.
initBoard:
	# Prologue
	addi $sp, $sp, -8
	sw $s0, 0($sp)
	sw $s1, 4($sp)
	
	# As the upper bit of each cell contains 1 byte of information for both bg and fg colors,
	# the first hex digit represents bg color whereas the second hex digit represents fg color.
	# Add value of foreground color ($a0) to both registers $a1 and $a2 and save the values into $s0 and $s1.
	addi $t1, $0, 0x10
	mul $s0, $a1, $t1
	mul $s1, $a2, $t1
	add $s0, $s0, $a0
	add $s1, $s1, $a0
	
	addi $t8, $0, 0xffff0000 # The MMIO region in MARS begins at addresss 0xffff0000.
	addi $t9, $0, 8 # Number of rows/cols
	
	add $t0, $0, $0 # Counter for outer loop
	initBoard_outerLoop:
		beq $t0, $t9, initBoard_return
		add $t1, $0, $0 # Counter for inner loop
		
		# Check parity of row-index and assign colors to the two registers
		addi $t2, $0, 2
		div $t0, $t2
		mfhi $t2 # $t2 will contain value of (row-index % 2)
		
		move $t3, $s1 # Light background
		move $t4, $s0 # Dark background
		beqz $t2, initBoard_innerLoop # If row-index is even use the values in $t3 and $t4, update values otherwise
		move $t3, $s0 # Dark background
		move $t4, $s1 # Light background
			
		initBoard_innerLoop:
			beq $t1, $t9, initBoard_outerLoop_reloop
			
			# Get address of Array[Reg[$t0]][Reg[$t1]] from the memory
			# address = base_address + (i * num_cols + j) * element_size
			mul $t5, $t0, $t9 # i * num_cols
			add $t5, $t5, $t1 # i * num_cols + j
			sll $t5, $t5, 1 # (i * num_cols + j) * element_size
			add $t5, $t5, $t8 # base_address + (i * num_cols + j) * element_size
			
			# Store ASCII character into lower byte of each cell
			addi $t2, $0, 'E' # To represent empty character
			sb $t2, 0($t5)
			
			# Following logic is to store hex color code into upper byte of each cell
			# Check parity of col-index
			addi $t2, $0, 2
			div $t1, $t2
			mfhi $t2  # $t2 will contain value of (col-index % 2)
			
			beqz $t2, initBoard_assign_color1 # If col-index is even, assign color1
			
			# If col-index is odd, assign color2
			initBoard_assign_color2:
				sb $t4, 1($t5) # Store value of color(bg+fg) into upper byte of the cell
				j initBoard_innerLoop_reloop
				
			initBoard_assign_color1:
				sb $t3, 1($t5) # Store value of color(bg+fg) into upper byte of the cell
				
			initBoard_innerLoop_reloop:
				addi $t1, $t1, 1
				j initBoard_innerLoop
		
		initBoard_outerLoop_reloop:
			addi $t0, $t0, 1
			j initBoard_outerLoop
		
	initBoard_return:
		# Epilogue
		lw $s0, 0($sp)
		lw $s1, 4($sp)
		addi $sp, $sp, 8
		jr $ra

########################################################################################################################################

# This function sets the Chess board square at (row, col) position to piece for player.
setSquare:
	lb $t8, 0($sp) # Fifth argument (fg) is placed on top of the stack frame by caller function

	# Validation of args
	addi $v0, $0, -1 # Function returns -1 on input error
	bltz $a0, setSquare_return # if row < 0: return -1
	bltz $a1, setSquare_return # if col < 0: return -1
	addi $t0, $0, 7
	bgt $a0, $t0, setSquare_return # if row > 7: return -1
	bgt $a1, $t0, setSquare_return # if col > 7: return -1
	addi $t0, $0, 2
	blez $a3, setSquare_return # if player < 1: return -1
	bgt $a3, $t0, setSquare_return # if player > 2: return -1
	addi $t0, $0, 0xF
	bgt $t8, $t0, setSquare_return # if fg > 0xF: return -1
	add $v0, $0, $0 # Function returns 0 if the arguments are valid
	
	# Access address of Board[row][col]
	addi $t0, $0, 0xffff0000 # The MMIO region in MARS begins at addresss 0xffff0000
	addi $t1, $0, 8 # Num of rows/cols
	
	# address = base_address + (i * num_cols + j) * element_size
	mul $t2, $a0, $t1
	add $t2, $t2, $a1
	sll $t2, $t2, 1
	add $t2, $t2, $t0
	
	# Store the ASCII character into lower byte
	sb $a2, 0($t2)
	
	addi $t0, $0, 'E'
	beq $a2, $t0, setSquare_pieceRemoval # If value in $a2 is E (empty), use fg from argument
	
	# Choose fg-value based on player 
	addi $t0, $0, 0xF # hex value for white color
	addi $t1, $0, 0x0 # hex value for black color
	addi $t3, $0, 2
	beq $a3, $t3, setSquare_player2
	
	setSquare_player1:
		move $t3, $t0 # Player 1 places white pieces
		j setSquare_update_fg
	
	setSquare_player2:
		move $t3, $t1 # Player 2 places black pieces
		j setSquare_update_fg
		
	setSquare_pieceRemoval:
		move $t3, $t8 # $t8 contains the value of fg from argument
		
	setSquare_update_fg:
		addi $t5, $0, 0x10
		lb $t4, 1($t2) # Load the byte containing bg+fg value
	
		sra $t4, $t4, 4 # Drop the LSB
		mul $t4, $t4, $t5 # Multiply by 10 to make space for new fg digit
		add $t4, $t4, $t3
		sb $t4, 1($t2) # Store (fg+bg) color value into upper byte
		
	setSquare_return:
		jr $ra

########################################################################################################################################

# This function inserts the player pieces into the Chess board in the initial state.
initPieces:
	# Prologue
	addi $sp, $sp, -32
	sw $s0, 0($sp)
	sw $s1, 4($sp)
	sw $s2, 8($sp)
	sw $s3, 12($sp)
	sw $s4, 16($sp)
	sw $s5, 20($sp)
	sw $s6, 24($sp)
	sw $ra, 28($sp)

	# Set pieces by calling setSquare
	addi $s0, $0, 2 # upper bound for row counter
	addi $s1, $0, 5 # upper bound for col counter
	addi $s4, $0, 3 # Initial index for Queen
	addi $s5, $0, 4 # Initial index for King
	
	add $s2, $0, $0 # row-index
	initPieces_outerLoop:
		beq $s2, $s0, initPieces_return
		add $s3, $0, $0 # col-index
		addi $t0, $0, 0x9 # Hex value for bright-red
		addi $sp, $sp, -4 # Store 5th argument at the top of stack
		sw $t0, 0($sp)
		
		initPieces_innerLoop:
			beq $s3, $s1, initPieces_outerLoop_reLoop
			
			move $a0, $s2 
			move $a1, $s3
			addi $a3, $0, 2 # Player2
			
			# Switch-case
			addi $t0, $0, 1
			beq $s2, $t0, initPieces_is_pawn
			beq $s3, $t0, initPieces_is_knight
			addi $t0, $0, 0
			beq $s3, $t0, initPieces_is_rook
			addi $t0, $0, 2
			beq $s3, $t0, initPieces_is_bishop
			beq $s3, $s4, initPieces_is_Queen
			beq $s3, $s5, initPieces_is_King			
			
			initPieces_is_pawn:
				li $a2, 'p'
				j initPieces_set
			initPieces_is_knight:
				li $a2, 'H'
				j initPieces_set
			initPieces_is_rook:
				li $a2, 'R'
				j initPieces_set
			initPieces_is_bishop:
				li $a2, 'B'
				j initPieces_set
			initPieces_is_King:
				addi $a2, $0, 'K'
				j initPieces_set_KingQueen
			initPieces_is_Queen:
				addi $a2, $0, 'Q'
				j initPieces_set_KingQueen
			initPieces_set_KingQueen:
				jal setSquare
				addi $t0, $0, 7
				sub $a0, $t0, $a0
				addi $a3, $0, 1 # Player1
				jal setSquare
				j initPieces_innerLoop_reloop

			initPieces_set:
				jal setSquare # Set piece in upper-left cell
				addi $t0, $0, 7
				sub $a1, $t0, $a1
				jal setSquare # Set piece in upper-right cell
				addi $a3, $0, 1 # Player1
				addi $t0, $0, 7
				sub $a0, $t0, $a0
				jal setSquare # Set piece in lower-right cell
				move $a1, $s3
				jal setSquare # Set piece in lower-left cell
				
			initPieces_innerLoop_reloop:
				addi $s3, $s3, 1
				j initPieces_innerLoop
		
		initPieces_outerLoop_reLoop:
			addi $s2, $s2, 1
			addi $sp, $sp, 4
			j initPieces_outerLoop
	
	initPieces_return:
		# Epilogue
		lw $s0, 0($sp)
		lw $s1, 4($sp)
		lw $s2, 8($sp)
		lw $s3, 12($sp)
		lw $s4, 16($sp)
		lw $s5, 20($sp)
		lw $s6, 24($sp)
		lw $ra, 28($sp)
		addi $sp, $sp, 32
		jr $ra

########################################################################################################################################

# This function translates  a Chess move (letter:column, number:row) into corresponding (row,col) of the board.
mapChessMove:
	# Input validation => letter ∈ [A, H], number ∈ ['1','8']
	addi $v0, $0, 0xFFFF # Return value for invalid input
	addi $t0, $0, 'H'
	bgt $a0, $t0, mapChessMove_return
	addi $t0, $0, 'A'
	blt $a0, $t0, mapChessMove_return
	addi $t0, $0, '8'
	bgt $a1, $t0, mapChessMove_return
	addi $t0, $0, '1'
	blt $a1, $t0, mapChessMove_return

	# Translation into (row, col)
	addi $t0, $0, 'A'
	sub $t0, $a0, $t0 #  Get col ∈ [0,7] from (letter - ASCII_value_of_A)
	addi $t1, $0, '8'
	sub $t1, $t1, $a1 #  Get row ∈ [0,7] from ('8' - number), where number is a char

	# Return a short value with row-value in the upper byte and col-value in the lower byte of the halfword
	sll $t1, $t1, 8
	add $v0, $t0, $t1
	
	mapChessMove_return:
		jr $ra

########################################################################################################################################

# This function loads the saved state of Chess game from file into MMOIO.
loadGame:
	# Prologue
	addi $sp, $sp, -24
	sw $s0, 0($sp)
	sw $s1, 4($sp)
	sw $s2, 8($sp)
	sw $s3, 12($sp)
	sw $s4, 16($sp)
	sw $ra, 20($sp)
	
	addi $v1, $0, -1 # Return value for $v1 if an error occurs while opening the file.
	
	# Open file
	# $a0 contains the starting address of null terminated file name.
	add $a1, $0, $0 # Reg[$a1] = flag, flag = 0 for read only
	add $a2, $0, $0 # Reg[$a2] = mode, must be set to 0
	addi $v0, $0, 13 # Syscall code for opening file
	syscall # Open file
	
	bltz $v0, loadGame_return # Return (-1,-1) if error occured when opening the file.
	
	# Initialize counters to 0
	add $s3, $0, $0
	add $s4, $0, $0
	
	# Read from file
	addi $sp, $sp, -5 # Create space in the stack frame to store characters (5 characters -> 5 bytes).
	move $a0, $v0 # If opening file was succcessful, $v0 will contain the file descriptor
	move $a1, $sp # Use address of the top of stack frame as buffer for reading from file.
	move $s2, $v0 # Save the file descriptor into $s2
	
	loadGame_loop:
		addi $a2, $0, 5 # Max number of characters to read per move; 5th characer is newline
		addi $v0, $0, 14 # Syscall code for reading from file
		syscall # Read from open file
		beqz $v0, loadGame_loop_Exit # $v0 contains 0 if end of file is reached
		
		move $s0, $a0
		move $s1, $a1
		lb $a0, 2($sp) # letter: col
		lb $a1, 3($sp) # number: row
		jal mapChessMove
		
		move $t0, $v0 # mapChessMove returns row-val and col-val stored in a halfword
		srl $a0, $t0, 8 # Upper byte contains row-val
		sll $a1, $a0, 8
		xor $a1, $a1, $t0 # Lower byte contains col-val
		
		lb $a3, 0($sp) # Player ∈ {1,2}
		lb $a2, 1($sp) # Piece
		
		addi $sp, $sp, -1 # Create space for 5th argument
		addi $t0, $0, 0x7 # fg color value
		sb $t0, 0($sp)
		
		# Convert player char to integer and keep count of moves
		addi $t0, $0, '1'
		beq $a3, $t0, loadGame_isPlayer1
		addi $a3, $0, 2
		addi $s4, $s4, 1
		j loadGame_continue
		
		loadGame_isPlayer1:
			addi $a3, $0, 1
			addi $s3, $s3, 1
		
		loadGame_continue:
			jal setSquare
			addi $sp, $sp, 1 # Move stack pointer back to pop the argument passed
		
			move $a0, $s0
			move $a1, $s1
			j loadGame_loop
	
	loadGame_loop_Exit:
		addi $sp, $sp, 5 # Move the stack pointer back to perform a pop operation
		
		# Close the file
		move $a0, $s2 # File descriptor
		addi $v0, $0, 16
		syscall
	
		# Place the moves count values to the return registers
		move $v0, $s3
		move $v1, $s4
	
	loadGame_return:
		# Epilogue
		lw $s0, 0($sp)
		lw $s1, 4($sp)
		lw $s2, 8($sp)
		lw $s3, 12($sp)
		lw $s4, 16($sp)
		lw $ra, 20($sp)
		addi $sp, $sp, 24
		jr $ra

########################################################################################################################################

# This function returns information of a cell at the location passed in as argument.
# Returns the type of piece and associated player if the cell is not empty, returns ('E', -1) otherwise.
getChessPiece:
	# Reg[$a0] == (row-val + col-val) in lower halfword
	# Extract upper byte (row-val) and put into $t0
	srl $t0, $a0, 8
	# Extract lower byte (col-val) and put into $t1
	sll $t1, $t0, 8 
	xor $t1, $t1, $a0
	
	
	addi $t2, $0, 8 # Total number of rows/cols
	addi $t3, $0, 0xffff0000 # The MMIO region in MARS begins at addresss 0xffff0000.
	
	# Get address of Array[Reg[$t0]][Reg[$t1]] from the memory
	# address = base_address + (i * num_cols + j) * element_size
	mul $t4, $t0, $t2 # i * num_cols
	add $t4, $t4, $t1 # i * num_cols + j
	sll $t4, $t4, 1 # (i * num_cols + j) * element_size
	add $t4, $t4, $t3 # base_address + (i * num_cols + j) * element_size
	
	lb $t0, 0($t4) # lower byte contains ASCII piece character or 'E'
	move $v0, $t0
	addi $t0, $0, 'E'
	addi $v1, $0, -1 # Return value if char is 'E'
	beq $v0, $t0, getChessPiece_return
	
	lb $t0, 1($t4) # upper byte contains color codes
	
	# Extract the lower 4 bits to get fg color value
	sra $t1,$t0, 4 
	sll $t1, $t1, 4
	xor $t0, $t0, $t1
	
	# If fg == 0, player == 2 else player == 1
	beqz $t0, getChessPiece_isPlayer2
	addi $v1, $0, 1
	j getChessPiece_return
	
	getChessPiece_isPlayer2:
		addi $v1, $0, 2
		
	getChessPiece_return:
		jr $ra

########################################################################################################################################

# This function performs the player's move, if valid, and updates Chess board accordingly.
perform_move:
	# Prologue
	addi $sp, $sp, -36
	sw $s0, 0($sp)
	sw $s1, 4($sp)
	sw $s2, 8($sp)
	sw $s3, 12($sp)
	sw $s4, 16($sp)
	sw $s5, 20($sp)
	sw $s6, 24($sp)
	sw $s7, 28($sp)
	sw $ra, 32($sp)

	# Store args into s-registers
	move $s0, $a0
	move $s1, $a1
	move $s2, $a2
	move $s3, $a3
	lw $s4, -4($sp) # Fifth argument saved into stack frame

	# Get information about piece at initial cell
	move $a0, $s1
	jal getChessPiece
	
	# Return argument error if the piece is not for the specified player.
	bne $s0, $v1, perform_move_argument_error
	
	# Set arguments to call a validMove function
	move $a0, $s1
	move $a1, $s2
	move $a2, $s0
	addi $sp, $sp, -4 # Make space in stack frame for capture locaion
	move $a3, $sp
	move $s5, $v0 # Save piece for setting the destination square
	
	# Switch-case to determine which validMove function to call
	addi $t0, $0, 'P'
	beq $t0, $v0, perform_move_is_pawn
	addi $t0, $0, 'p'
	beq $t0, $v0, perform_move_is_pawn
	addi $t0, $0, 'R'
	beq $t0, $v0, perform_move_is_rook
	addi $t0, $0, 'H'
	beq $t0, $v0, perform_move_is_knight
	addi $t0, $0, 'B'
	beq $t0, $v0, perform_move_is_bishop
	addi $t0, $0, 'Q'
	beq $t0, $v0, perform_move_is_queen
	addi $t0, $0, 'K'
	beq $t0, $v0, perform_move_is_king
	j perform_move_invalid_move # Invalid move if the initial cell is empty

	perform_move_is_pawn:
		# Make space in stack frame to store the piece letter
		addi $sp, $sp, -4
		sw $v0, 0($sp)
		jal validPawnMove
		addi $sp, $sp, 4
		addi $s5, $0, 'P'
		j postCheck_validMove
	perform_move_is_rook:
		jal validRookMove
		j postCheck_validMove
	perform_move_is_knight:
		jal validKnightMove
		j postCheck_validMove
	perform_move_is_bishop:
		jal validBishopMove
		j postCheck_validMove
	perform_move_is_queen:
		jal validQueenMove
		j postCheck_validMove
	perform_move_is_king:
		jal validKingMove
	
	postCheck_validMove:
		lw $a3, 0($a3)
		addi $sp, $sp, 4
		addi $t0, $0, -1
		blt $v0, $t0, perform_move_argument_error
		beq $v0, $t0, perform_move_invalid_move
		add $s6, $0, $0
		addi $s7, $0, '\0'
		bnez $v0, perform_move_capture
		
		perform_move_set_final:
			# Set the to-position on the board
			# Reg[$s2] == (row-val + col-val) in lower halfword
			# Extract upper byte (row-val) and put into $a0
			srl $a0, $s2, 8
			# Extract lower byte (col-val) and put into $a1
			sll $a1, $a0, 8 
			xor $a1, $a1, $s2
			move $a2, $s5
			move $a3, $s0
		
			add $t0, $0, $s3
			addi $sp, $sp, -4
			sw $t0, 0($sp)
			jal setSquare
			addi $sp, $sp, 4
			
			# If King was moved, save location into stack frame
			addi $t0, $0, 'K'
			bne $s5, $t0, perform_move_clear_init
			sw $s2, -4($sp)
		
		perform_move_clear_init:
			# Clear the from-position on the board
			# Reg[$s1] == (row-val + col-val) in lower halfword
			# Extract upper byte (row-val) and put into $a0
			srl $a0, $s1, 8
			# Extract lower byte (col-val) and put into $a1
			sll $a1, $a0, 8 
			xor $a1, $a1, $s1
			addi $a2, $0, 'E'
			move $a3, $s0
		
			add $t0, $0, $s3
			addi $sp, $sp, -4
			sw $t0, 0($sp)
			jal setSquare
			addi $sp, $sp, 4

			move $v0, $s6
			move $v1, $s7 
			j perform_move_return
		
		perform_move_capture:
			addi $s6, $0, 1
			move $s7, $v1 # $v1 contains letter of piece captured
			j perform_move_set_final
		
	perform_move_invalid_move:
		addi $v0, $0, -1
		addi $v1, $0, '\0'
		j perform_move_return
	
	perform_move_argument_error:
		addi $v0, $0, -2
		addi $v1, $0, '\0'
		
	perform_move_return:
		# Epilogue
		lw $s0, 0($sp)
		lw $s1, 4($sp)
		lw $s2, 8($sp)
		lw $s3, 12($sp)
		lw $s4, 16($sp)
		lw $s5, 20($sp)
		lw $s6, 24($sp)
		lw $s7, 28($sp)
		lw $ra, 32($sp)	
		addi $sp, $sp, 36
		jr $ra

#######################################################################################################################################

# This function determines if opponent's king is under threat of capture on the player's next turn.
check:
	# Prologue
	addi $sp, $sp, -28
	sw $s0, 0($sp)
	sw $s1, 4($sp)
	sw $s2, 8($sp)
	sw $s3, 12($sp)
	sw $s4, 16($sp)
	sw $s5, 20($sp)
	sw $ra, 24($sp)
	
	# Save arguments into s-registers
	move $s0, $a0
	move $s1, $a1
	
	# Check if player is in {1,2}
	addi $t0, $0, 2
	bgt $s0, $t0, check_return_argument_error
	blez $s0, check_return_argument_error
	
	move $a0, $s1
	jal getChessPiece
	beq $s0, $v1, check_return_argument_error # If player's own piece is in the square, return argument error
	
	addi $t0, $0, 'K'
	bne $v0, $t0, check_return_argument_error # If opponent's king is not in the specified square, return argument error
	
	# Iterate through each square of the board to determine if the opponent's king is in check.
	addi $s4, $0, 8 # Total number of row/col
	add $s2, $0, $0 # Row counter
	check_row_loop:
		addi $v0, $0, -1
		beq $s2, $s4, check_return
		add $s3, $0, $0 # Column counter
		check_col_loop:
			beq $s3, $s4, check_row_loop_reloop
			
			# Save (row,col) as a short into $a0 for calling getChessPiece
			move $a0, $s2
			sll $a0, $a0, 8
			add $a0, $a0, $s3
			beq $a0, $s1, check_col_loop_reloop # Skip if current square has opponent's king
			jal getChessPiece
			bne $s0, $v1, check_col_loop_reloop # Jump if opponent's piece is returned
			bltz $v1, check_col_loop_reloop # Skip if the square is empty
			
			move $s5, $v0 # Save piece letter into $s5
			move $a1, $s1 # Reg[$s1] contains opponent king's position
			move $a2, $s0 # Reg[$s0] contains player ∈ {1,2}
			addi $sp, $sp, -4 # Make space in stack frame for capture locaion
			move $a3, $sp
			
			# Switch-case to determine which validMove function to call
			addi $t0, $0, 'P'
			beq $t0, $s5, check_is_pawn
			addi $t0, $0, 'p'
			beq $t0, $s5, check_is_pawn
			addi $t0, $0, 'R'
			beq $t0, $s5, check_is_rook
			addi $t0, $0, 'H'
			beq $t0, $s5, check_is_knight
			addi $t0, $0, 'B'
			beq $t0, $s5, check_is_bishop
			addi $t0, $0, 'Q'
			beq $t0, $s5, check_is_queen
			addi $t0, $0, 'K'
			beq $t0, $s5, check_is_king
		
			check_is_pawn:
				# Make space in stack frame to store the piece letter
				addi $sp, $sp, -4
				sw $s5, 0($sp)
				jal validPawnMove
				addi $sp, $sp, 4
				j check_continue
			check_is_rook:
				jal validRookMove
				j check_continue
			check_is_knight:
				jal validKnightMove
				j check_continue
			check_is_bishop:
				jal validBishopMove
				j check_continue
			check_is_queen:
				jal validQueenMove
				j check_continue
			check_is_king:
				jal validKingMove
				j check_continue
			
			check_isEmpty:
				addi $sp, $sp, 4
				j check_col_loop_reloop
			
			check_continue:
				addi $sp, $sp, 4
				addi $t0, $0, 1
				move $t1, $v0
				add $v0, $0, $0
				beq $t0, $t1, check_return # Is in check
			
			check_col_loop_reloop:
				addi $s3, $s3, 1
				j check_col_loop
			
		check_row_loop_reloop:
			addi $s2, $s2, 1
			j check_row_loop
	
	check_return_argument_error:
		addi $v0, $0, -2
		
	check_return:
		# Epilogue
		lw $s0, 0($sp)
		lw $s1, 4($sp)
		lw $s2, 8($sp)
		lw $s3, 12($sp)
		lw $s4, 16($sp)
		lw $s5, 20($sp)
		lw $ra, 24($sp)
		addi $sp, $sp, 28
		jr $ra
