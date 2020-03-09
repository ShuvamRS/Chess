.text

# Determines if a bishop can successfully move from initial position given in Reg[$a0] to final position given in Reg[$a1].
# Assumption: There is a bishop at initial position(Reg[$a0]) of player specified in Reg[$a2].
validBishopMove:
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
	
	# Extract (row,col) from the firt argument (from-position)
	sra $s0, $a0, 8 # Reg[$s0] = row value of from-position
	sll $s1, $s0, 8
	xor $s1, $a0, $s1 # Reg[$s1] = col value of from-position
	sra $s2, $a1, 8 # Reg[$s2] = row value of to-position
	sll $s3, $s2, 8
	xor $s3, $a1, $s3 # Reg[$s3] = col value of to-position
	
	#########  Validation of input arguments ########
	addi $v0, $0, -2 # First return value for invalid argument(s)
	addi $v1, $0, '\0' # Second return value if any error occurs
	addi $t0, $0, 7 # Max value for row/col
	
	#  if !(0 <= row/col <= 7): return(-2,'\0');
	bltz $s0, validBishopMove_return
	bltz $s1, validBishopMove_return
	bltz $s2, validBishopMove_return
	bltz $s3, validBishopMove_return
	bgt $s0, $t0, validBishopMove_return
	bgt $s1, $t0, validBishopMove_return
	bgt $s2, $t0, validBishopMove_return
	bgt $s3, $t0, validBishopMove_return
	
	# Check if player is in {1,2}
	addi $t0, $0, 2
	bgt $a2, $t0, validBishopMove_return
	blez $a2, validBishopMove_return
	
	
	############ Move validation #################
	move $s6, $s0 # Row index
	move $s7, $s1 # col index
	sub $t0, $s2, $s0 # Calculate row difference
	sub $t1, $s3, $s1 # Calculate col difference
	addi $s4, $0, 1 # Row increment unit
	bgtz $t0, validBishopMove_set_col_increment
	addi $s4, $0, -1
	
	validBishopMove_set_col_increment:
		addi $s5, $0, 1 # Col increment unit
		bgtz $t1, validBishopMove_checkDiagonal
		addi $s5, $0, -1
		
	validBishopMove_checkDiagonal:
		sub $t0, $s2, $s0 # Row difference
		sub $t1, $s3, $s1 # Col difference
		# Drop the signed bits
		div $t0, $s4
		mflo $t0
		div $t1, $s5
		mflo $t1
		bne $t0, $t1, validBishopMove_invalid_move # row difference != col difference -> non-diagonal move
		
	validBishopMove_checkObsruction:
		# Move one cell per loop
		add $s6, $s6, $s4
		add $s7, $s7, $s5
		
		# Get piece and player information about the cell
		sll $a0, $s6, 8
		add $a0, $a0, $s7
		jal getChessPiece	

		# Break out of loop when destination row is reached
		beq $s6, $s2, validBishopMove_checkDestination
		
		# If Reg[$v1] != -1, path is obstructed
		addi $t0, $0, -1
		bne $v1, $t0, validBishopMove_invalid_move
		
		j validBishopMove_checkObsruction
		
	validBishopMove_checkDestination:
		addi $t0, $0, -1
		beq $v1, $t0, validBishopMove_valid_move # If Reg[$v1] == -1, the cell is empty
		beq $v1, $a2, validBishopMove_invalid_move # Invalid move if the player's own piece is in the square
		
		# The square has the other player's piece.
		# Set value for return with capture: (1,letter_of_piece)
		move $v1, $v0
		addi $v0, $0, 1
		
		# Store the location-value of the to-cell into $a3
		sll $s2, $s2, 8
		add $s2, $s2, $s3
		sw $s2, 0($a3)

		j validBishopMove_return
		
	validBishopMove_invalid_move:
		addi $v0, $0, -1
		addi $v1, $0, '\0'
		j validBishopMove_return
		
	validBishopMove_valid_move:
		add $v0, $0, $0
		addi $v1, $0, '\0'
		
	validBishopMove_return:
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
		
########################################################################################################################################

# Determines if a rook can successfully move from initial position given in Reg[$a0] to final position given in Reg[$a1].
# Assumption: There is a rook at initial position(Reg[$a0]) of player specified in Reg[$a2].
validRookMove:
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
	
	# Extract (row,col) from the firt argument (from-position)
	sra $s0, $a0, 8 # Reg[$s0] = row value of from-position
	sll $s1, $s0, 8
	xor $s1, $a0, $s1 # Reg[$s1] = col value of from-position
	sra $s2, $a1, 8 # Reg[$s2] = row value of to-position
	sll $s3, $s2, 8
	xor $s3, $a1, $s3 # Reg[$s3] = col value of to-position
	
	################# Validation of input arguments ####################
	addi $v0, $0, -2 # First return value for invalid argument(s)
	addi $v1, $0, '\0' # Second return value if any error occurs
	addi $t0, $0, 7 # Max value for row/col
	
	#  if !(0 <= row/col <= 7): return(-2,'\0');
	bltz $s0, validRookMove_return
	bltz $s1, validRookMove_return
	bltz $s2, validRookMove_return
	bltz $s3, validRookMove_return
	bgt $s0, $t0, validRookMove_return
	bgt $s1, $t0, validRookMove_return
	bgt $s2, $t0, validRookMove_return
	bgt $s3, $t0, validRookMove_return
	
	# Check if player is in {1,2}
	addi $t0, $0, 2
	bgt $a2, $t0, validRookMove_return
	blez $a2, validRookMove_return
	
	######################### Move validation ############################
	# (initial_row == final_row && initial_col == final_col)  -> invalid move
	beq $a0, $a1, validRookMove_invalid_move
	
	add $s6, $0, $0 # row increment unit
	add $s7, $0, $0 # column increment unit
	sub $s4, $s2, $s0 # row difference
	sub $s5, $s3, $s1 # col difference
	beqz $s4, validRookMove_setColInc
	beqz $s5, validRookMove_setRowInc
	# (initital_row == final_row && initial_col == final_col) -> invalid move
	j validRookMove_invalid_move

	validRookMove_setRowInc:
		addi $s6, $0, 1
		bgtz $s4, validRookMove_check_move
		addi $s6, $0, -1
		j validRookMove_check_move
	
	validRookMove_setColInc:
		addi $s7, $0, 1
		bgtz $s5, validRookMove_check_move
		addi $s7, $0, -1
		j validRookMove_check_move
	
	validRookMove_check_move:
		# Move one cell per loop
		add $s0, $s0, $s6
		add $s1, $s1, $s7
	
		# Get piece and player information about the cell
		sll $a0, $s0, 8
		add $a0, $a0, $s1
		jal getChessPiece
	
		# Break from the loop if destination cell is reached
		beq $s0, $s2, validRookMov_reached_row_check_col
		beq $s1, $s3, validRookMov_reached_col_check_row
		
		validRookMov_reached_row_check_col:
			beq $s1, $s3, validRookMove_check_destination_square
			j validRookMove_continue_move_check
			
		validRookMov_reached_col_check_row:
			beq $s0, $s2, validRookMove_check_destination_square
		
		validRookMove_continue_move_check:
			addi $t0, $0, -1
			bne $v1, $t0, validRookMove_invalid_move # Reg[$v1] != -1 -> path obstruction
		
			j validRookMove_check_move
	
	validRookMove_check_destination_square:
		addi $t0, $0, -1
		beq $v1, $t0, validRookMove_valid_move # Valid move if destination cell is empty.
		beq $v1, $a2, validRookMove_invalid_move # Invalid move if player's own piece is in the square.
		
		# The square has the other player's piece.
		# Set value for return with capture: (1,letter_of_piece)
		move $v1, $v0
		addi $v0, $0, 1
		
		# Store the location-value of the to-cell into $a3
		sll $s2, $s2, 8
		add $s2, $s2, $s3
		sw $s2, 0($a3)

		j validRookMove_return
	
	validRookMove_invalid_move:
		addi $v0, $0, -1
		addi $v1, $0, '\0'
		j validBishopMove_return
		
	validRookMove_valid_move:
		add $v0, $0, $0
		addi $v1, $0, '\0'
	
	validRookMove_return:
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

########################################################################################################################################

# Assuming that there is a king at the initial position, this function determines
# if the king can successfully move to the destination square.
validKingMove:
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
	
	# Extract (row,col) from the firt argument (from-position)
	sra $s0, $a0, 8	# Reg[$s0] = row value of from-position
	sll $s1, $s0, 8
	xor $s1, $a0, $s1 # Reg[$s1] = col value of from-position
	sra $s2, $a1, 8 # Reg[$s2] = row value of to-position
	sll $s3, $s2, 8
	xor $s3, $a1, $s3 # Reg[$s3] = col value of to-position
	
	################# Validation of input arguments ####################
	addi $v0, $0, -2 # First return value for invalid argument(s)
	addi $v1, $0, '\0' # Second return value if any error occurs
	addi $t0, $0, 7 # Max value for row/col
	
	#  if !(0 <= row/col <= 7): return(-2,'\0');
	bltz $s0, validKingMove_return
	bltz $s1, validKingMove_return
	bltz $s2, validKingMove_return
	bltz $s3, validKingMove_return
	bgt $s0, $t0, validKingMove_return
	bgt $s1, $t0, validKingMove_return
	bgt $s2, $t0, validKingMove_return
	bgt $s3, $t0, validKingMove_return
	
	# Check if player is in {1,2}
	addi $t0, $0, 2
	bgt $a2, $t0, validKingMove_return
	blez $a2, validKingMove_return
	
	######################### Move validation ############################
	sub $t0, $s2, $s0
	sub $t1, $s3, $s1
	addi $t2, $0, 1
	addi $t3, $0, -1
	
	# Return invalid move if initial and destination squares are same.
	beq $a0, $a1, validKingMove_invalid_move
	
	# A king can move one square in any direction.
	bgt $t0, $t2, validKingMove_invalid_move
	blt $t0, $t3, validKingMove_invalid_move
	bgt $t1, $t2, validKingMove_invalid_move
	blt $t1, $t3, validKingMove_invalid_move
	
	# Store args into s-registers
	move $s4, $a0 # short from-location
	move $s5, $a1 # short to-location
	move $s6, $a2 # int player
	move $s7, $a3 # short &capture
	
	# Get information about the destination square
	move $a0, $s5
	jal getChessPiece
	# Return invalid move if player's own piece is in the destination square.
	beq $v1, $s6, validKingMove_invalid_move
	
	# Store destination square's piece into the stack frame
	addi $sp, $sp, -4
	sw $v0, 0($sp)
	
	# Extract fg-color value from memory at destination cell index
	addi $t0, $0, 0xffff0000 # The MMIO region in MARS begins at addresss 0xffff0000
	addi $t1, $0, 8 # Num of rows/cols
	
	# address = base_address + (i * num_cols + j) * element_size
	mul $t1, $s2, $t1
	add $t1, $t1, $s3
	sll $t1, $t1, 1
	add $t1, $t1, $t0
	lb $t0, 1($t1) # Load the byte containing bg+fg value
	
	# Extract fg value
	sra $t1, $t0, 4
	sll $t1, $t1, 4
	xor $t0, $t1, $t0
	
	# Make space in the stack frame to call setSquare for passing 5th argument == fg-color
	addi $sp, $sp, -4
	sw $t0, 0($sp)
	
	# Temporarily remove king from initial position to determine if the
	# king would be in "check" state if moved to the destination square.
	move $a0, $s0
	move $a1, $s1
	addi $a2, $0, 'E' # Set initial square to Empty
	move $a3, $s6 # The value of player does not matter when setting square to Empty
	jal setSquare
	
	# Temporarily place king into destination square
	move $a0, $s2
	move $a1, $s3
	addi $a2, $0, 'K'
	move $a3, $s6
	jal setSquare
	
	# Check if player's king would be under threat of capture by one of opponent's pieces
	addi $a0, $0, 3
	sub $a0, $a0, $s6
	move $a1, $s5
	jal check
	move $s4, $v0 # Reg[$s4] == Check state
	
	# Place the pieces into their original positions on the board.
	move $a0, $s0
	move $a1, $s1
	addi $a2, $0, 'K'
	move $a3, $s6
	jal setSquare
	
	move $a0, $s2
	move $a1, $s3
	lw $s0, 4($sp) # Get opponent's piece from stack frame
	
	move $a2, $s0
	addi $a3, $0, 3
	sub $a3, $a3, $s6
	jal setSquare
	
	# Check if move to destination square is valid
	addi $sp, $sp, 8 # Re-position stack pointer
	beqz $s4, validKingMove_invalid_move # Invalid move if Check state is true
	addi $t0, $0, 'E'
	beq $s0, $t0, validKingMove_valid_move
	
	# Store captured piece's location (short val) into address passed into third argument
	sw $s5, 0($s7)
	addi $v0, $0, 1
	move $v1, $s0
	j validKingMove_return
	
	validKingMove_invalid_move:
		addi $v0, $0, -1
		addi $v1, $0, '\0'
		j validKingMove_return
		
	validKingMove_valid_move:
		add $v0, $0, $0
		addi $v1, $0, '\0'
	
	validKingMove_return:
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

########################################################################################################################################

# Assuming that there is a knight at the initial position, this function determines
# if the knight can successfully move to the destination square.
validKnightMove:
	# Prologue
	addi $sp, $sp, -20
	sw $s0, 0($sp)
	sw $s1, 4($sp)
	sw $s2, 8($sp)
	sw $s3, 12($sp)
	sw $ra, 16($sp)	
	
	# Extract (row,col) from the firt argument (from-position)
	sra $s0, $a0, 8 # Reg[$s0] = row value of from-position
	sll $s1, $s0, 8
	xor $s1, $a0, $s1 # Reg[$s1] = col value of from-position
	sra $s2, $a1, 8 # Reg[$s2] = row value of to-position
	sll $s3, $s2, 8
	xor $s3, $a1, $s3 # Reg[$s3] = col value of to-position
	
	################# Validation of input arguments ####################
	addi $v0, $0, -2 # First return value for invalid argument(s)
	addi $v1, $0, '\0' # Second return value if any error occurs
	addi $t0, $0, 7 # Max value for row/col
	
	#  if !(0 <= row/col <= 7): return(-2,'\0');
	bltz $s0, validKnightMove_return
	bltz $s1, validKnightMove_return
	bltz $s2, validKnightMove_return
	bltz $s3, validKnightMove_return
	bgt $s0, $t0, validKnightMove_return
	bgt $s1, $t0, validKnightMove_return
	bgt $s2, $t0, validKnightMove_return
	bgt $s3, $t0, validKnightMove_return
	
	# Check if player is in {1,2}
	addi $t0, $0, 2
	bgt $a2, $t0, validKnightMove_return
	blez $a2, validKnightMove_return
	
	######################### Move validation ############################
	
	# Valid subtraction values
	addi $t8, $0, -2
	addi $t9, $0, 2
	
	# Calculate row/col difference
	sub $t0, $s2, $s0
	sub $t1, $s3, $s1
	
	# Retun invalid move if row_diff == col_diff
	beq $t0, $t1, validKnightMove_invalid_move
	
	# row/col difference may not be 0
	beqz $t0, validKnightMove_invalid_move
	beqz $t1, validKnightMove_invalid_move
	
	# Check that -2 <= difference <= 2
	blt $t0, $t8, validKnightMove_invalid_move
	bgt $t0, $t9, validKnightMove_invalid_move
	blt $t1, $t8, validKnightMove_invalid_move
	bgt $t1, $t9, validKnightMove_invalid_move
	
	# Check that (+/-)row-difference != (+/-)col-difference
	addi $t2, $0, 1
	addi $t3, $0, -1
	div $t1, $t0
	mflo $t0
	beq $t0, $t2, validKnightMove_invalid_move
	beq $t0, $t3, validKnightMove_invalid_move
	
	# Get the piece that is in destination square
	move $a0, $a1
	jal getChessPiece
	bltz $v1, validKnightMove_valid_move # The square is empty if Reg[$v1] == -1
	beq $v1, $a2, validKnightMove_invalid_move # Invalid move if player's own piece is in the square
	
	move $v1, $v0 # Move opponent's captured piece into $v1
	addi $v0, $0, 1 # Return value for valid move + piece captured
	sw $a1, 0($a3) # Store address of captured piece into $a3
	j validKnightMove_return
	
	validKnightMove_invalid_move:
		addi $v0, $0, -1
		addi $v1, $0, '\0'
		j validKnightMove_return
		
	validKnightMove_valid_move:
		add $v0, $0, $0
		addi $v1, $0, '\0'
	
	validKnightMove_return:
		# Epilogue
		lw $s0, 0($sp)
		lw $s1, 4($sp)
		lw $s2, 8($sp)
		lw $s3, 12($sp)
		lw $ra, 16($sp)	
		addi $sp, $sp, 20
		jr $ra

########################################################################################################################################

# Assuming that there is a queen at the initial position, this function determines if the queen can successfully
# move to the destination square. This function calls validBishopMove and vaiidRookMove to validate move.
validQueenMove:
	# Prologue
	addi $sp, $sp, -4
	sw $ra, 0($sp)
	
	jal validBishopMove
	bgez $v0, validQueenMove_return
	jal validRookMove
	
	validQueenMove_return:
		# Epilogue
		lw $ra, 0($sp)
		addi $sp, $sp, 4
		jr $ra

########################################################################################################################################

# Assuming a pawn is at the initial position, this function determines
# if the pawn can successfully move to the destination square.
validPawnMove:
	# Prologue
	addi $sp, $sp, -24
	sw $s0, 0($sp)
	sw $s1, 4($sp)
	sw $s2, 8($sp)
	sw $s3, 12($sp)
	sw $s4, 16($sp)
	sw $ra, 20($sp)	
	
	# Extract (row,col) from the firt argument (from-position)
	sra $s0, $a0, 8 	# Reg[$s0] = row value of from-position
	sll $s1, $s0, 8
	xor $s1, $a0, $s1	# Reg[$s1] = col value of from-position
	sra $s2, $a1, 8 	# Reg[$s2] = row value of to-position
	sll $s3, $s2, 8
	xor $s3, $a1, $s3 # Reg[$s3] = col value of to-position
	
	################# Validation of input arguments ####################
	addi $v0, $0, -2 # First return value for invalid argument(s)
	addi $v1, $0, '\0' # Second return value if any error occurs
	addi $t0, $0, 7 # Max value for row/col
	
	#  if !(0 <= row/col <= 7): return(-2,'\0');
	bltz $s0, validPawnMove_return
	bltz $s1, validPawnMove_return
	bltz $s2, validPawnMove_return
	bltz $s3, validPawnMove_return
	bgt $s0, $t0, validPawnMove_return
	bgt $s1, $t0, validPawnMove_return
	bgt $s2, $t0, validPawnMove_return
	bgt $s3, $t0, validPawnMove_return
	
	# Check if player is in {1,2}
	addi $t0, $0, 2
	bgt $a2, $t0, validPawnMove_return
	blez $a2, validPawnMove_return
	
	######################### Move validation ############################
	# Invalid move if the from and to positions are the same
	beq $a0, $a1, validPawnMove_invalid_move
	
	# Calculate row difference for move validation
	sub $t0, $s2, $s0
	addi $s4, $0, -1
	bltz $t0, validPawnMove_getChessPiece_init # Reg[$s4]==row-difference < 0 <-> Player==1
	addi $s4, $0, 1
	
	validPawnMove_getChessPiece_init:
	# Check whether the initial square contains a pawn piece
	# 'p' -> not moved previously, 'P' -> moved atleast once
	jal getChessPiece
	
	# Invalid argument if initial square does not contain player's piece.
	bne $v1, $a2, validPawnMove_invalid_args
	
	# Check pawn character -> 'p'(first move), 'P'(not first move)
	addi $t0, $0, 'P'
	beq $v0, $t0, validPawnMove_notFirstMove
	addi $t0, $0, 'p'
	bne $v0, $t0, validPawnMove_invalid_args
	
	bne $s1, $s3, validPawnMove_notFirstMove # branch if from-cell and to-cell aren't in the same column
	add $t0, $s4, $s4
	add $t0, $s0, $t0
	bne $t0, $s2, validPawnMove_notFirstMove # branch if pawn doesn't reach destination-cell in 2 forward steps
	j validPawnMove_checkDestForward
	
	validPawnMove_notFirstMove:
	add $t0, $s0, $s4
	bne $s2, $t0, validPawnMove_invalid_move # invalid move if pawn's row != destination-cell's row in 1 forward step
	sub $t0, $s3, $s1 # Col difference
	beqz $t0, validPawnMove_checkDestForward
	addi $t1, $0, -1
	beq $t0, $t1, validPawnMove_checkDestDiagonal
	addi $t1, $0, 1
	beq $t0, $t1, validPawnMove_checkDestDiagonal
	j validPawnMove_invalid_move
	
	validPawnMove_checkDestDiagonal:
		move $a0, $a1
		jal getChessPiece
		addi $t0, $0, 3
		sub $t0, $t0, $a2
		bne $v1, $t0, validPawnMove_invalid_move # If one of opponent's pieces is in the cell
		move $v1, $v0
		addi $v0, $0, 1
		sw $a1, 0($a3) # Store address of captured piece into $a3
		j validPawnMove_return
		
	validPawnMove_checkDestForward:
		move $a0, $a1
		jal getChessPiece
		bltz $v1, validPawnMove_valid_move
		j validPawnMove_invalid_move
	
	validPawnMove_invalid_args:
		addi $v0, $0, -2
		addi $v1, $0, '\0'
		j validPawnMove_return
	
	validPawnMove_invalid_move:
		addi $v0, $0, -1
		addi $v1, $0, '\0'
		j validPawnMove_return
		
	validPawnMove_valid_move:
		add $v0, $0, $0
		addi $v1, $0, '\0'
	
	validPawnMove_return:
		# Epilogue
		lw $s0, 0($sp)
		lw $s1, 4($sp)
		lw $s2, 8($sp)
		lw $s3, 12($sp)
		lw $s4, 16($sp)
		lw $ra, 20($sp)	
		addi $sp, $sp, 24
		jr $ra