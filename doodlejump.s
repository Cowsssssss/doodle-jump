######################################################################
# CSC258H5S Fall 2020 Assembly Final Project
# University of Toronto, St. George
#
# Student: Zexi Lv, 1005982386
#
# Bitmap Display Configuration:
# - Unit width in pixels: 8
# - Unit height in pixels: 8
# - Display width in pixels: 256
# - Display height in pixels: 256
# - Base Address for Display: 0x10008000 ($gp)
#
# Which milestone is reached in this submission?
# (See the assignment handout for descriptions of the milestones)
# - Milestone 5
#
# Which approved additional features have been implemented?
# (See the assignment handout for the list of additional features)
# - Milestone 4: Scoreboard/score count, game over/retry 
# - Milestone 5: Realistic physics, power-ups, opponents/lethal creatures
#
# Any additional information that the TA needs to know:
# - j/k to move doodler left/right
# - r to (re)start game on the start or scoreboard screens
# - spring is a single grey square, and the rocket is a single orange square
# - "opponent" is a grey Y-shaped drawing, touching it will cause the game to end 
######################################################################

.data
	displayAddress: .word	0x10008000
	doodlerLocation: .word	0x10008938 # middle pixel of the 3x3 doodler
	screenEnd: .word 0x10009000
	doodlerColour: .word 0xff0000 # colours
	platformColour: .word 0x00ff00
	skyColour: .word 0x87ceeb
	textColour: .word 0x8a2be2
	enemyColour: .word 0x808080
	springColour: .word 0x808080
	rocketColour: .word 0xffa500
	platformLocations: .space 44 # locations of the 11 platforms
	scores: .space 12
	
.text
	# initialize variables w/ colours and starting positions
	lw $t0, displayAddress	# $t0 stores the base address for display
	lw $t2, platformColour	# $t2 stores the platform colour)
	li $t3, 0		# $t3 stores the rocket fuel
	lw $t4, doodlerLocation	# t4 stores the position of the doodler
	li $t5, 0		# t5 is the score
	la $t6, platformLocations # t6 points to the address of the first platform
	li $t8, 0 		# $t8 is the velocity of the doodler (positive up, neg down)

	jal drawSky		# draw the sky
	jal drawStart		# draw the starting screen
	
waitForStart:
	jal detectStartInput
	# sleep for a bit
	li $v0, 32
	li $a0, 1000
	syscall
	j waitForStart
startGame:
	# initialize the screen with platforms, the doodler, and all other objects
	jal initPlatforms
	jal makeEnemy		# make an enemy and store its location in $t1
	jal makeSpring
	jal makeRocket
	li $t3, 0		# $t3 stores the rocket fuel
	lw $t4, doodlerLocation	# t4 stores the initial position of the doodler
	li $t5, 0		# t5 is the score
	li $t8, 0 		# $t8 is the velocity of the doodler (positive up, neg down)
	
	jal drawSky		# draw the sky
	lw $s0, doodlerColour
	addi $sp, $sp, -4
	sw $s0, 0($sp)
	jal drawDoodler		# draw the doodler
	addi $sp, $sp, -4
	sw $t2, 0($sp)
	jal drawPlatforms	# draw the platforms
	
mainLoop:
	jal clearScreen		# clear the screen
	
	# next detect keyboard input to move left/right
	jal detectMovementInput

	beq $t3, 0, handleJumpFall # if there's no rocket fuel, regular jump/fall
	# there is rocket fuel, so set velocity and decrease rocket fuel before movement
	li $t8, 6		# rocket speed
	addi $t3, $t3, -1	# decrease rocket fuel
handleJumpFall:
	addi $t8, $t8, -1	# decrement velocity due to gravity
	blt $t8, 0, ifFall # if the doodler has negative velocity, fall
	# else, jump
	li $s0, 0		# counter for jump time
ifJump:
	beq $s0, $t8, endMovement	# stop jumping if we've jumped as specified by velocity
	
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# save counter on stack
	
	jal jump		# jump
	jal detectDeath		# check if doodler touched enemy
	jal detectRocket	# check if doodler touched rocket
	jal moveScreenDown	# move screen down if doodler is "too high"
	
	lw $s0, 0($sp)		# load counter from stack
	addi $sp, $sp, 4
	
	addi $s0, $s0, 1	# increment jump counter
	j ifJump
	
ifFall:
	li $s0, 0		# counter for fall time
fallLoop:
	beq $s0, $t8, endMovement	# stop falling if we've falled as specified by velocity
	
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# save counter on stack
	
	jal fall		# fall
	jal detectDeath		# check if doodler touched enemy
	jal detectEnd		# jumps to end in detectEnd if the doodler hits the bottom
	jal detectJump		# check if we need to start a jump
	jal detectSpring	# check if we need to start a super jump
	jal detectRocket	# check if doodler touched rocket
	
	lw $s0, 0($sp)		# load counter from stack
	addi $sp, $sp, 4
	
	addi $s0, $s0, -1	# increment fall counter
	beq $v0, 1, endMovement	# if we need to start a jump, stop falling
	j fallLoop
	
endMovement:
	
	# draw the score
	addi $s1, $t0, 3460	# using s1 cuz all t registers in use
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# push position on stack
	
	lw $s0, textColour	# using s0 cuz all t registers are used already
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# push text colour on stack
	jal drawScore
	
	# draw doodler
	lw $s0, doodlerColour
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# push doodler colour on stack
	jal drawDoodler		# draw the doodler
	
	# draw platforms
	addi $sp, $sp, -4
	sw $t2, 0($sp)		# push platform colour on stack
	jal drawPlatforms	# draw the platforms
	
	# draw enemy
	lw $s0, enemyColour
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# push enemy colour on stack
	jal drawEnemy		# draw the enemy
	
	# draw spring
	lw $s0, springColour
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# push the spring colour on stack
	jal drawSpring		# draw the spring
	
	# draw spring
	lw $s0, rocketColour
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# push the rocket colour on stack
	jal drawRocket		# draw the rocket
	
	# sleep for a bit
	li $v0, 32
	li $a0, 500
	syscall
	# maybe make some kind of exit condition
	j mainLoop 		# jump back to loop start
	
# detects if the doodler has hit the enemy
detectDeath:
	# want to check the 5x5 square centred at $t1
	# start $s0 at the top left
	addi $s0, $t1, -264	# position to check if $t1 is in
	li $s1, 0		# outer loop counter
	li $s2, 0		# inner loop counter
detectDeathOuterLoop:
	beq $s2, 5, endDetectDeathLoop # if checked all 5 rows, move out of loop
detectDeathInnerLoop:
	beq $s1, 5, detectDeathOuterLoopBody # if checked all squares in row, branch out
	beq $t4, $s0, loseGame	# doodler touching the enemy, die
	addi $s0, $s0, 4	# move to next square in row to check
	addi $s1, $s1, 1	# increment inner loop counter
	j detectDeathInnerLoop # jump back to loop start
detectDeathOuterLoopBody:
	addi $s2, $s2, 1	# increment outer loop counter
	li $s1, 0		# reset inner loop counter
	addi $s0, $s0, 108	# move to start of next row
	j detectDeathOuterLoop	# jump back to loop start
endDetectDeathLoop:
	jr $ra			# jump back to return location
	
# detects if the doodler has fallen to the ground
detectEnd:
	# load in first pixel of bottom row 
	li $s0, 0x10008F80
	# jump to end if doodler on bottom row
	bge $t4, $s0, loseGame
	# jump back if not
	jr $ra
	
	
############ keyboard input functions ############

# detect input and (re)start game accordingly
detectStartInput:
	lw $a0, 0xffff0000
	beq $a0, 1, input_detected
	# no input to just jump back
	jr $ra
input_detected:
	# branch to input handling code
	lw $a1, 0xffff0004
	beq $a1, 0x72, respond_to_R
	# some other key was pressed, ignore it
	jr $ra
respond_to_R:
	j startGame		# start the game

# detect keyboard input and change doodler position accordingly
detectMovementInput:
	lw $a0, 0xffff0000
	beq $a0, 1, movementInputDetected
	# no input to just jump back
	jr $ra
movementInputDetected:
	# branch to input handling code
	lw $a1, 0xffff0004
	beq $a1, 0x6A, respond_to_J
	beq $a1, 0x6B, respond_to_K
	# some other key was pressed, ignore it
	jr $ra
respond_to_J:
	addi $t4, $t4, -4
	jr $ra
respond_to_K:
	addi $t4, $t4, 4
	jr $ra

	
############ doodler jump/fall functions ############

moveScreenDown:
	# if the doodler is "high enough", move the platforms and doodler down 1 row
	lw $a0, displayAddress	# position of top left pixel
	addi $a0, $a0, 2058	# middle of screen
	blt $t4, $a0, moveDoodlerDown	# check if doodler position above middle of screen
	# doodler was not high enough, do nothing
	jr $ra

moveDoodlerDown:
	# moving the screen down, so increment platform generator counter and increase score
	addi $t5, $t5, 1	# score increment
	
	addi $t4, $t4, 128	# move doodler down
	addi $t1, $t1, 128	# move enemy down
	addi $t7, $t7, 128	# move spring down
	addi $t9, $t9, 128	# move rocket down
	
	# move platforms down - initialization
	add $a0, $zero, $zero	# loop counter
	add $a1, $t6, $zero	# address of the first platform
platformDownLoop:
	# when the end of platform array is reached, exit loop
	beq $a0, 11, endPlatformDownLoop
	# move the platform at the selected index down
	lw $a2, 0($a1)		# store position of platform at index a0
	addi $a2, $a2, 128	# move it down a row
	sw $a2, 0($a1)		# put it back in the array
	addi $a1, $a1, 4	# move the pointer to the next address in the array
	addi $a0, $a0, 1	# increment the loop counter
	j platformDownLoop	# jump back to loop start

endPlatformDownLoop:
	# save return location on stack
	addi $sp, $sp, -4
	sw $ra, 0($sp)
	jal checkMakePlatform	# check if we need to make a platform
	jal checkMakeEnemy	# check if we need to make an enemy
	jal checkMakeSpring	# check if we need to make a spring
	jal checkMakeRocket	# check if we need to make a spring
	
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	jr $ra

checkMakeRocket:
	li $s0, 100
	div $t5, $s0		# divide score by 100
	mfhi $s0		# move remainder in $s0
	beq $s0, 0, makeRocket	# every 100 score, make a new rocket
	jr $ra
makeRocket:
	# make a new rocket
	# generate rocket position
	li $v0, 42		# rng with range
	li $a0, 0		
	li $a1, 31		# number from 0-31
	syscall
	
	li $s0, 4
	mul $s0, $s0, $a0	# multiply the random num by 4
	add $t9, $t0, $s0	# move it to the first row and store it in $t9
	jr $ra			# jump back
	
checkMakeSpring:
	li $s0, 60
	div $t5, $s0		# divide score by 60
	mfhi $s0		# move remainder in $s0
	beq $s0, 0, makeSpring	# every 60 score, make a new spring
	jr $ra
makeSpring:
	# make a spring on top of the last platform
	lw $t7, 40($t6)		# position of platform to put spring on
	addi $t7, $t7, -120	# to the middle of row above platform
	jr $ra
	
checkMakeEnemy:
	li $s0, 40
	div $t5, $s0		# divide score by 40
	mfhi $s0		# move remainder in $s0
	beq $s0, 0, makeEnemy	# every 40 score, make a new enemy
	jr $ra
makeEnemy:
	# make the enemy, size 3x3
	# generate enemy position (centre pixel)
	li $v0, 42		# rng with range
	li $a0, 0		
	li $a1, 30		# number from 0-29
	syscall
	
	li $s0, 4
	addi $a0, $a0, 1	# shift right 1 (1-30)
	mul $s0, $s0, $a0	# multiply the random num 1-30 by 4
	add $s0, $t0, $s0	# move it to the first row
	addi $t1, $s0, 128	# move it down a row and store it in $t1
	jr $ra			# jump back

checkMakePlatform:
	# every 3 score, make a new platform
	li $s0, 3
	div $t5, $s0
	mfhi $s0
	beq $s0, 0, startMakePlatform	# make a new platform, if we need to
	jr $ra
startMakePlatform:
	# shift array, deleting the oldest platform in the proccess
	li $s0, 4		# init platform address relative to array
	add $s0, $s0, $t6	# now points to platformArray[1]
	li $s2, 1		# init loop counter
shiftArrayLoop:
	beq $s2, 11, makePlatform
	lw $s1, 0($s0)		# load in the platform location
	sw $s1, -4($s0)		# move it back 1 index
	addi $s0, $s0, 4	# move the pointer to the next platform in the array
	addi $s2, $s2, 1	# increment loop counter
	j shiftArrayLoop

# randomly generates a new platform
makePlatform:
	# make the platform
	# generate platform position
	li $v0, 42		# rng with range
	li $a0, 0		
	li $a1, 27		# number of pixels from the left of the screen
	syscall
	
	li $s0, 4
	mul $s0, $s0, $a0	# multiply the random num 0-27 by 4
	add $s0, $t0, $s0	# move it to the first row
	#addi $s0, $s0, -128	# move it up a row (off the screen)
	sw $s0, 40($t6)		# store it in the array
	jr $ra			# jump back

# refill rocket fuel and stores 1 in $v0 if touched rocket
detectRocket:
	bgtz $t3, rocketInUse	# if a rocket is already being used, don't check
	addi $s4, $t9, -132	# set first pixel to check if rocket collision
	beq $s4, $t4, detectRocketTrue # if collide, start rocket
	addi $s4, $s4, 4	# next place to check
	beq $s4, $t4, detectRocketTrue # if collide, start rocket
	addi $s4, $s4, 4	# next place to check
	beq $s4, $t4, detectRocketTrue # if collide, start rocket
	addi $s4, $s4, 120	# next row
	beq $s4, $t4, detectRocketTrue # if collide, start rocket
	addi $s4, $s4, 4	# next place to check
	beq $s4, $t4, detectRocketTrue # if collide, start rocket
	addi $s4, $s4, 4	# next place to check
	beq $s4, $t4, detectRocketTrue # if collide, start rocket
	addi $s4, $s4, 120	# next row
	beq $s4, $t4, detectRocketTrue # if collide, start rocket
	addi $s4, $s4, 4	# next place to check
	beq $s4, $t4, detectRocketTrue # if collide, start rocket
	addi $s4, $s4, 4	# next place to check
	beq $s4, $t4, detectRocketTrue # if collide, start rocket
	
	jr $ra

detectRocketTrue:
	li $t3, 6		# refill rocket fuel
	li $v0, 1		# send signal that doodler stops falling
	addi $t9, $t0, 4224	# sets rocket position off screen
	jr $ra

rocketInUse:
	jr $ra

# store 1 in $v0 and set velocity if touched spring, and store 0 if not
detectSpring:
	addi $s4, $t7, -132	# set first pixel to check if spring collision
	beq $s4, $t4, detectSpringTrue # if collide, start jump
	addi $s4, $s4, 4	# next place to check
	beq $s4, $t4, detectSpringTrue # if collide, start jump
	addi $s4, $s4, 4	# next place to check
	beq $s4, $t4, detectSpringTrue # if collide, start jump
	jr $ra
	
detectSpringTrue:
	li $t8, 8		# start a super jump
	li $v0, 1		# send signal that doodler is jumping
	jr $ra

# store 1 in $v0 and set velocity if need to start jumping, and 0 otherwise
detectJump:
	addi $sp, $sp, -4
	sw $ra, 0($sp)	# store return address on the stack
	
	# loop initialization
	add $s0, $zero, $zero	# loop counter
	add $s1, $t6, $zero	# address of the first platform
detectPlatformLoop:
	# when the end of platform array is reached, exit loop
	beq $s0, 11, endDetectPlatformLoop
	
	lw $a0, 0($s1)		# load in platform location to register used for parameters
	addi $sp, $sp, -4	# save registers on the stack
	sw $s0, 0($sp)
	addi $sp, $sp, -4	
	sw $s1, 0($sp)
	addi $sp, $sp, -4	
	sw $s2, 0($sp)
	
	jal detectSinglePlatform # check that platform
	
	lw $s2, 0($sp)		# load registers from the stack
	addi $sp, $sp, 4
	lw $s1, 0($sp)	
	addi $sp, $sp, 4
	lw $s0, 0($sp)
	addi $sp, $sp, 4
	
	beq $v0, 1, detectJumpTrue # if found that doodler is at a platform, no need to check the other platforms
	
	addi $s1, $s1, 4	# move the pointer to the next address in the array
	addi $s0, $s0, 1	# increment the loop counter
	j detectPlatformLoop	# jump back to loop start

endDetectPlatformLoop:
	# none of the platforms detected the doodler, return 0
	li $v0, 0
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	jr $ra
	
detectJumpTrue:
	li $t8, 5		# set velocity to jump
	li $v0, 1		# send signal that we are going to start jumping
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	jr $ra


# detects if the doodler is above the platform with leftmost pixel at $a0
# or touching the platform
# will load 1/0 in $v0 if it is true/false
detectSinglePlatform:
	# load in centre of doodler location
	addi $s0, $t4, 0
	# load in leftmost pixel of platform
	addi $s1, $a0, 0
	
	# first check if it's 1 row above the platform
	# if platform - 260 <= doodler location <= platform - 236, start the jump
	# i.e if doodler > platform -236 || doodler < platform - 260, no jump
	addi $s2, $s1, -260
	addi $s3, $s1, -236
	bgt $s0, $s3, detectAbovePlatformFalse
	blt $s0, $s2, detectAbovePlatformFalse
	# both branches failed, so start the jump
	li $v0, 1
	jr $ra
detectAbovePlatformFalse:
	# next check the row below
	addi $s2, $s2, 128 # shift the checkers down a row
	addi $s3, $s3, 128
	# check again
	bgt $s0, $s3, detectPlatformFalse
	blt $s0, $s2, detectPlatformFalse
	# both branches failed, for start the jump
	li $v0, 1
	jr $ra
detectPlatformFalse:
	# it's outside the designated start jump trigger range, so don't start a jump 
	li $v0, 0
	jr $ra

# make the doodler fall 1 row
fall:
	# $t4 is doodler position
	addi $t4, $t4, 128
	jr $ra

# move the doodler up 1 row
jump:
	# $t4 is doodler position
	addi $t4, $t4, -128
	jr $ra

# Initializes the platforms array
initPlatforms:
	# fill the platform array
	add $s0, $zero, $t6	# platform index points to platformArray[0]
	li $s1, 0		# init loop counter
	li $s2, 31		# $s2 is the row the platform will be generated in
	li $s3, 128
	mul $s2, $s2, $s3	# now $s2 contains the position of the first pixel of the row relative to the screen
	add $s2, $s2, $t0	# move it to the screen
initPlatformsLoop:
	beq $s1, 11, endInitPlatforms
	# generate platform position
	li $v0, 42		# rng with range
	li $a0, 0		
	li $a1, 27		# number of pixels from the left of the screen
	syscall
	
	li $s3, 4
	mul $s3, $s3, $a0	# multiply the random num 0-27 by 4
	add $s3, $s3, $s2	# move it to the desired row
	
	sw $s3, 0($s0)		# store it in the array
	
	addi $s0, $s0, 4	# move the pointer to the next platform in the array
	addi $s1, $s1, 1	# increment loop counter
	addi $s2, $s2, -384	# the next platform will be 3 rows higher
	j initPlatformsLoop
endInitPlatforms:
	jr $ra


############  drawing functions  ############

# draw the score in $t5 specified by the colour and position at the top of the stack
drawScore:
	# get colour from stack
	lw $s0, 0($sp)
	addi $sp, $sp, 4
	
	# get position of first digit from the stack
	lw $s1, 0($sp)
	addi $sp, $sp, 4
	
	# push return address on stack
	addi $sp, $sp, -4
	sw $ra, 0($sp)
	
	# get the hundreds digit and save the remainder on the stack
	li $s2, 100
	div $t5, $s2
	mflo $s2		# quotient
	mfhi $s3		# remainder
	
	# save registers on the stack
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# position
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# colour
	addi $sp, $sp, -4
	sw $s3, 0($sp)		# remainder
	
	# push position and colour on the stack
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# position
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# colour
	
	# push quotient on stack to draw hundreds digit
	addi $sp, $sp, -4
	sw $s2, 0($sp)
	
	jal checkDigit
	
	# restore registers from stack
	lw $s3, 0($sp)
	addi $sp, $sp, 4	# remainder
	lw $s0, 0($sp)
	addi $sp, $sp, 4	# colour
	lw $s1, 0($sp)
	addi $sp, $sp, 4	# position
	
	# move to the position of the second score digit
	addi $s1, $s1, 16
	
	# calculate tens digit and save the remainder on the stack
	li $s2, 10
	div $s3, $s2
	mflo $s2		# tens digit
	mfhi $s3		# remainder (ones digit)
	
	# save registers on the stack
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# position
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# colour
	addi $sp, $sp, -4
	sw $s3, 0($sp)		# remainder
	
	# push position and colour and digit on to stack again
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# position
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# colour
	addi $sp, $sp, -4
	sw $s2, 0($sp)		# tens digit
	
	jal checkDigit		# draw the tens digit
	
	# restore registers from stack
	lw $s2, 0($sp)
	addi $sp, $sp, 4	# remainder
	lw $s0, 0($sp)
	addi $sp, $sp, 4	# colour
	lw $s1, 0($sp)
	addi $sp, $sp, 4	# position
	
	addi $s1, $s1, 16	# move to next digit position
	
	# push position and colour and digit on to stack again
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# position
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# colour
	addi $sp, $sp, -4
	sw $s2, 0($sp)		# ones digit
	
	jal checkDigit		# draw the ones digit
	
	# restore jump location from the stack
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	jr $ra

# takes in a number 0-9 and calls the appropriate draw method
checkDigit:
	# load digit from stack, now the stack points to arguments for the draw functions
	lw $s6, 0($sp)
	addi $sp, $sp, 4
	
	# load colour and position from the stack
	lw $s1, 0($sp)		# colour
	addi $sp, $sp, 4
	lw $s2, 0($sp)		# position
	addi $sp, $sp, 4
	
	# push position, then colour on stack
	addi $sp, $sp, -4
	sw $s2, 0($sp)		# position
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# colour
	
	# detect digit of score and draw appropriately
	# if s0 (digit) == 0, draw 0. == 1, draw 1. ... == 8, draw 8. else, draw 9
	# drawing functions don't overwrite the digit, $s6
	beq $s6, 0, drawZero	# when they call jr $ra the $ra will refer to where this function was called
	beq $s6, 1, drawOne
	beq $s6, 2, drawTwo
	beq $s6, 3, drawThree
	beq $s6, 4, drawFour
	beq $s6, 5, drawFive
	beq $s6, 6, drawSix
	beq $s6, 7, drawSeven
	beq $s6, 8, drawEight
	bge $s6, 9, drawNine
	
	jr $ra 			# jump back to function call
	
# draw a 0 at the position specified at the top of the stack with the colour
# stack: colour, position, rest of stack
drawZero:
	lw $s0, 0($sp)		# colour
	addi $sp, $sp, 4
	lw $s1, 0($sp)		# position
	addi $sp, $sp, 4
	
	# draw
	sw $s0, 0($s1)
	sw $s0, 4($s1)
	sw $s0, 8($s1)
	sw $s0, 128($s1)
	sw $s0, 136($s1)
	sw $s0, 256($s1)
	sw $s0, 384($s1)
	sw $s0, 512($s1)
	sw $s0, 264($s1)
	sw $s0, 392($s1)
	sw $s0, 520($s1)
	sw $s0, 516($s1)
	
	jr $ra
	
# draw a 1 at the position specified at the top of the stack with the colour
# stack: colour, position, rest of stack
drawOne:
	lw $s0, 0($sp)		# colour
	addi $sp, $sp, 4
	lw $s1, 0($sp)		# position
	addi $sp, $sp, 4
	
	# draw
	sw $s0, 8($s1)
	sw $s0, 136($s1)
	sw $s0, 264($s1)
	sw $s0, 392($s1)
	sw $s0, 520($s1)
	
	jr $ra
	
# draw a 0 at the position specified at the top of the stack with the colour
# stack: colour, position, rest of stack
drawTwo:
	lw $s0, 0($sp)		# colour
	addi $sp, $sp, 4
	lw $s1, 0($sp)		# position
	addi $sp, $sp, 4
	
	# draw
	sw $s0, 0($s1)
	sw $s0, 4($s1)
	sw $s0, 8($s1)
	sw $s0, 136($s1)
	sw $s0, 256($s1)
	sw $s0, 260($s1)
	sw $s0, 264($s1)
	sw $s0, 384($s1)
	sw $s0, 512($s1)
	sw $s0, 516($s1)
	sw $s0, 520($s1)
	
	jr $ra
	
# draw a 3 at the position specified at the top of the stack with the colour
# stack: colour, position, rest of stack
drawThree:
	lw $s0, 0($sp)		# colour
	addi $sp, $sp, 4
	lw $s1, 0($sp)		# position
	addi $sp, $sp, 4
	
	# draw
	sw $s0, 0($s1)
	sw $s0, 4($s1)
	sw $s0, 8($s1)
	sw $s0, 136($s1)
	sw $s0, 256($s1)
	sw $s0, 260($s1)
	sw $s0, 264($s1)
	sw $s0, 392($s1)
	sw $s0, 512($s1)
	sw $s0, 516($s1)
	sw $s0, 520($s1)
	
	jr $ra
	
# draw a 4 at the position specified at the top of the stack with the colour
# stack: colour, position, rest of stack
drawFour:
	lw $s0, 0($sp)		# colour
	addi $sp, $sp, 4
	lw $s1, 0($sp)		# position
	addi $sp, $sp, 4
	
	# draw
	sw $s0, 0($s1)
	sw $s0, 8($s1)
	sw $s0, 128($s1)
	sw $s0, 136($s1)
	sw $s0, 256($s1)
	sw $s0, 260($s1)
	sw $s0, 264($s1)
	sw $s0, 392($s1)
	sw $s0, 520($s1)
	
	jr $ra
	
# draw a 5 at the position specified at the top of the stack with the colour
# stack: colour, position, rest of stack
drawFive:
	lw $s0, 0($sp)		# colour
	addi $sp, $sp, 4
	lw $s1, 0($sp)		# position
	addi $sp, $sp, 4
	
	# draw
	sw $s0, 0($s1)
	sw $s0, 4($s1)
	sw $s0, 8($s1)
	sw $s0, 128($s1)
	sw $s0, 256($s1)
	sw $s0, 260($s1)
	sw $s0, 264($s1)
	sw $s0, 392($s1)
	sw $s0, 512($s1)
	sw $s0, 516($s1)
	sw $s0, 520($s1)
	
	jr $ra
	
# draw a 6 at the position specified at the top of the stack with the colour
# stack: colour, position, rest of stack
drawSix:
	lw $s0, 0($sp)		# colour
	addi $sp, $sp, 4
	lw $s1, 0($sp)		# position
	addi $sp, $sp, 4
	
	# draw
	sw $s0, 0($s1)
	sw $s0, 128($s1)
	sw $s0, 256($s1)
	sw $s0, 260($s1)
	sw $s0, 264($s1)
	sw $s0, 384($s1)
	sw $s0, 392($s1)
	sw $s0, 512($s1)
	sw $s0, 516($s1)
	sw $s0, 520($s1)
	
	jr $ra
	
# draw a 7 at the position specified at the top of the stack with the colour
# stack: colour, position, rest of stack
drawSeven:
	lw $s0, 0($sp)		# colour
	addi $sp, $sp, 4
	lw $s1, 0($sp)		# position
	addi $sp, $sp, 4
	
	# draw
	sw $s0, 0($s1)
	sw $s0, 4($s1)
	sw $s0, 8($s1)
	sw $s0, 136($s1)
	sw $s0, 264($s1)
	sw $s0, 392($s1)
	sw $s0, 520($s1)
	
	jr $ra
	
# draw a 8 at the position specified at the top of the stack with the colour
# stack: colour, position, rest of stack
drawEight:
	lw $s0, 0($sp)		# colour
	addi $sp, $sp, 4
	lw $s1, 0($sp)		# position
	addi $sp, $sp, 4
	
	# draw
	sw $s0, 0($s1)
	sw $s0, 4($s1)
	sw $s0, 8($s1)
	sw $s0, 128($s1)
	sw $s0, 136($s1)
	sw $s0, 256($s1)
	sw $s0, 260($s1)
	sw $s0, 264($s1)
	sw $s0, 384($s1)
	sw $s0, 392($s1)
	sw $s0, 512($s1)
	sw $s0, 516($s1)
	sw $s0, 520($s1)
	
	jr $ra
	
# draw a 9 at the position specified at the top of the stack with the colour
# stack: colour, position, rest of stack
drawNine:
	lw $s0, 0($sp)		# colour
	addi $sp, $sp, 4
	lw $s1, 0($sp)		# position
	addi $sp, $sp, 4
	
	# draw
	sw $s0, 0($s1)
	sw $s0, 4($s1)
	sw $s0, 8($s1)
	sw $s0, 128($s1)
	sw $s0, 136($s1)
	sw $s0, 256($s1)
	sw $s0, 260($s1)
	sw $s0, 264($s1)
	sw $s0, 392($s1)
	sw $s0, 520($s1)
	
	jr $ra
	
# draw a S at the position specified at the top of the stack with the colour
# stack: colour, position, rest of stack
drawS:
	lw $s0, 0($sp)		# colour
	addi $sp, $sp, 4
	lw $s1, 0($sp)		# position
	addi $sp, $sp, 4
	
	# draw
	sw $s0, 0($s1)
	sw $s0, 4($s1)
	sw $s0, 8($s1)
	sw $s0, 128($s1)
	sw $s0, 256($s1)
	sw $s0, 260($s1)
	sw $s0, 264($s1)
	sw $s0, 392($s1)
	sw $s0, 512($s1)
	sw $s0, 516($s1)
	sw $s0, 520($s1)
	
	jr $ra

# draw a A at the position specified at the top of the stack with the colour
# stack: colour, position, rest of stack
drawA:
	lw $s0, 0($sp)		# colour
	addi $sp, $sp, 4
	lw $s1, 0($sp)		# position
	addi $sp, $sp, 4
	
	# draw
	sw $s0, 0($s1)
	sw $s0, 4($s1)
	sw $s0, 8($s1)
	sw $s0, 128($s1)
	sw $s0, 136($s1)
	sw $s0, 256($s1)
	sw $s0, 260($s1)
	sw $s0, 264($s1)
	sw $s0, 384($s1)
	sw $s0, 392($s1)
	sw $s0, 512($s1)
	sw $s0, 520($s1)
	
	jr $ra
	
# draw a R at the position specified at the top of the stack with the colour
# stack: colour, position, rest of stack
drawR:
	lw $s0, 0($sp)		# colour
	addi $sp, $sp, 4
	lw $s1, 0($sp)		# position
	addi $sp, $sp, 4
	
	# draw
	sw $s0, 0($s1)
	sw $s0, 4($s1)
	sw $s0, 128($s1)
	sw $s0, 136($s1)
	sw $s0, 256($s1)
	sw $s0, 260($s1)
	sw $s0, 384($s1)
	sw $s0, 392($s1)
	sw $s0, 512($s1)
	sw $s0, 520($s1)
	
	jr $ra
	
# draw a T at the position specified at the top of the stack with the colour
# stack: colour, position, rest of stack
drawT:
	lw $s0, 0($sp)		# colour
	addi $sp, $sp, 4
	lw $s1, 0($sp)		# position
	addi $sp, $sp, 4
	
	# draw
	sw $s0, 0($s1)
	sw $s0, 4($s1)
	sw $s0, 8($s1)
	sw $s0, 132($s1)
	sw $s0, 260($s1)
	sw $s0, 388($s1)
	sw $s0, 516($s1)
	
	jr $ra
	

clearScore:
	# colour all the pixels in the bottom left sky colour
	lw $s0, 0($sp) 		# sky colour from stack
	addi $sp, $sp, 4
	
	addi $s1, $t0, 3460	# position of top left of score
	
	# nested loop to colour score sky colour (5x7 rows x coloumns)
	# inner loop colours a row, outer loop changes rows
	# initialization
	li $s2, 0
	li $s3, 0
clearScoreOuterLoop:
	beq $s3, 5, clearScoreLoopEnd
clearScoreInnerLoop:
	beq $s2, 11, clearScoreOuterLoopBody
	# inner loop body
	sw $s0, 0($s1)
	addi $s1, $s1, 4	# move to next column
	addi $s2, $s2, 1	# increment inner loop counter
	j clearScoreInnerLoop
	
clearScoreOuterLoopBody:
	# outer loop body
	addi $s1, $s1, 84	# move to next row and reset column
	li $s2, 0		# reset inner loop counter
	addi $s3, $s3, 1	# increment outer loop counter
	j clearScoreOuterLoop
	
clearScoreLoopEnd:
	# loop end, jump back to function call location
	jr $ra
	
# Fill the screen with the colour of the sky
drawSky:
	add $s0, $t0, $zero 	# store the address of the screen into s0
	lw $s1, skyColour 	# store color of the sky in s1
drawSkyLoop:
	beq $s0, 0x10009000, endDrawSky # exit loop/function if the current position == ending position
	sw $s1, 0($s0)		# draw at the current location
	addi $s0, $s0, 4	# increment the location to the next pixel
	j drawSkyLoop		# jump back to loop start
endDrawSky:
	jr $ra 			# jump back to where this function was called
	
# Clears the screen by colouring in the doodler and platforms with the sky colour
clearScreen:
	addi $sp, $sp, -4
	sw $ra, 0($sp)		# put return location on the stack
	
	# clear doodler
	lw $s0, skyColour
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# push sky colour on the stack
	jal drawDoodler		# colour doodler with the colour of the sky
	
	# clear platforms
	lw $s0, skyColour
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# push sky colour on the stack
	jal drawPlatforms	# colour platforms with colour of the sky
	
	# clear score
	lw $s0, skyColour
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# push sky colour on the stack
	jal clearScore		# colour the score the colour of the sky
	
	# clear enemy
	lw $s0, skyColour
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# push sky colour on the stack
	jal drawEnemy		# colour the enemy the colour of the sky
	
	# clear spring
	lw $s0, skyColour
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# push sky colour on the stack
	jal drawSpring		# colour the spring the colour of the sky
	
	# clear rocket
	lw $s0, skyColour
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# push sky colour on the stack
	jal drawRocket		# colour the rocket the colour of the sky
	
	lw $ra, 0($sp)		# load return location from the stack
	addi $sp, $sp, 4
	jr $ra			# return to function call location

# draws the rocket using the colour on the stack
drawRocket:
	# $t7 is the location of the rocket
	lw $s0, 0($sp)		# store the colour passed in s0
	addi $sp, $sp, 4
	
	# draw the rocket
	sw $s0, 0($t9)
	jr $ra
	
# draws the spring using the colour on the stack
drawSpring:
	# $t7 is the location of the spring
	lw $s0, 0($sp)		# store the colour passed in s0
	addi $sp, $sp, 4
	
	# draw the spring
	sw $s0, 0($t7)
	jr $ra
	
# draws the enemy using the colour on the stack
drawEnemy:
	# $t1 is the location of the enemy
	lw $s0, 0($sp)		# store the colour passed in s0
	addi $sp, $sp, 4
	
	# draw the enemy
	sw $s0, -132($t1)
	sw $s0, -124($t1)
	sw $s0, 0($t1)
	sw $s0, 128($t1)
	#jump back
	jr $ra

# draws the doodler using the colour on the stack
drawDoodler:
	add $s0, $t4, $zero	# store the doodler location in s0
	
	lw $s1, 0($sp)		# store the colour passed in s1
	addi $sp, $sp, 4
	
	# draw the doodler
	sw $s1, -128($s0)
	sw $s1, 0($s0)
	sw $s1, 4($s0)
	sw $s1, -4($s0)
	sw $s1, 124($s0)
	sw $s1, 132($s0)
	#jump back
	jr $ra

# calls draw platform on each platform position
drawPlatforms:
	lw $s3, 0($sp)		# store the colour passed in s3
	addi $sp, $sp, 4
	
	addi $sp, $sp, -4	# move the stack pointer
	sw $ra 0($sp)		# record the place to jump back to
	
	# loop initialization
	add $s0, $zero, $zero	# loop counter
	add $s1, $t6, $zero	# address of the first platform
drawPlatformLoop:
	# when the end of platform array is reached, exit loop
	beq $s0, 11, endDrawPlatformLoop
	
	# save s0 and s1 on the stack
	addi $sp, $sp, -4
	sw $s0, 0($sp)
	addi $sp, $sp, -4
	sw $s1, 0($sp)
	
	# push the platform position and colour on stack
	lw $s2, 0($s1)		# platform position
	
	addi $sp, $sp, -4
	sw $s2, 0($sp)
	addi $sp, $sp, -4
	sw $s3, 0($sp)		# platform colour
	
	jal drawPlatform
	
	# retrieve s0 and s1 on the stack
	lw $s1, 0($sp)
	addi $sp, $sp, 4
	lw $s0, 0($sp)
	addi $sp, $sp, 4
	
	addi $s1, $s1, 4	# move the pointer to the next address in the array
	addi $s0, $s0, 1	# increment the loop counter
	j drawPlatformLoop	# jump back to loop start

endDrawPlatformLoop:
	# jump back to where the function was called
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	jr $ra

drawPlatform:
	# get position, then colour from stack in $s0, s1
	lw $s1, 0($sp)		# colour
	addi $sp, $sp, 4
	lw $s0, 0($sp)		# position
	addi $sp, $sp, 4
	
	sw $s1, 0($s0)
	sw $s1, 4($s0)
	sw $s1, 8($s0)
	sw $s1, 12($s0)
	sw $s1, 16($s0)
	# jump back
	jr $ra

# draw the word "start" at the top of the screen
drawStart:
	# store return location on stack
	addi $sp, $sp, -4
	sw $ra, 0($sp)
	
	addi $s0, $t0, 408	# position of first letter
	lw $s1, textColour	# colour of the text
	
	# draw "start"
	# save registers to the stack
	
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# save $s0 on the stack
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# save $s1 on the stack
	
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# push position to the stack
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# push colour to the stack
	
	jal drawS		# function call
	
	lw $s1, 0($sp)		# restore $s1 from stack
	addi $sp, $sp, 4
	lw $s0, 0($sp)		# restore $s0 from stack
	addi $sp, $sp, 4
	
	addi $s0, $s0, 16	# shift to next letter position
	
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# save $s0 on the stack
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# save $s1 on the stack
	
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# push position to the stack
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# push colour to the stack
	
	jal drawT		# function call
	
	lw $s1, 0($sp)		# restore $s1 from stack
	addi $sp, $sp, 4
	lw $s0, 0($sp)		# restore $s0 from stack
	addi $sp, $sp, 4
	
	addi $s0, $s0, 16	# shift to next letter position
	
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# save $s0 on the stack
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# save $s1 on the stack
	
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# push position to the stack
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# push colour to the stack
	
	jal drawA		# function call
	
	lw $s1, 0($sp)		# restore $s1 from stack
	addi $sp, $sp, 4
	lw $s0, 0($sp)		# restore $s0 from stack
	addi $sp, $sp, 4
	
	addi $s0, $s0, 16	# shift to next letter position
	
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# save $s0 on the stack
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# save $s1 on the stack
	
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# push position to the stack
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# push colour to the stack
	
	jal drawR		# function call
	
	lw $s1, 0($sp)		# restore $s1 from stack
	addi $sp, $sp, 4
	lw $s0, 0($sp)		# restore $s0 from stack
	addi $sp, $sp, 4
	
	addi $s0, $s0, 16	# shift to next letter position
	
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# save $s0 on the stack
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# save $s1 on the stack
	
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# push position to the stack
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# push colour to the stack
	
	jal drawT		# function call
	
	lw $s1, 0($sp)		# restore $s1 from stack
	addi $sp, $sp, 4
	lw $s0, 0($sp)		# restore $s0 from stack
	addi $sp, $sp, 4
	
	lw $ra, 0($sp)		# restore $ra from stack
	addi $sp, $sp, 4
	
	jr $ra

# insert the score in $t5 in the array, preserving order
insertScore:
	la $s0, scores		# location of first score
	# if $t5 > scores[0], shift everything right and insert at 0
	lw $s1, 0($s0)		# value of first score
	bgt $t5, $s1, insertScoreAtZero
	# if $t5 > scores[1], shift the last two right and insert at 1
	lw $s1, 4($s0)		# value of second score
	bgt $t5, $s1, insertScoreAtOne
	# if $t5 > scores[2], insert at 2
	lw $s1, 8($s0)		# value of third score
	bgt $t5, $s1, insertScoreAtTwo
	# else, do nothing
	jr $ra
	
insertScoreAtZero:
	lw $s2, 4($s0)		# second score
	sw $s2, 8($s0)		# put second score in third score location
	lw $s2, 0($s0)		# first score
	sw $s2, 4($s0)		# put first score in second score location
	sw $t5, 0($s0)		# put $t5 in index 0
	jr $ra			# jump back
	
insertScoreAtOne:
	lw $s2, 4($s0)		# second score
	sw $s2, 8($s0)		# put second score in third score location
	sw $t5, 4($s0)		# put $t5 in index 1
	jr $ra			# jump back
	
insertScoreAtTwo:
	sw $t5, 8($s0)		# put $t5 at index 2
	jr $ra			# jump back

drawHighScores:
	addi $sp, $sp, -4
	sw $ra, 0($sp)		# save jump location on stack
	
	# first insert the new score at the right place
	jal insertScore

	# then draw each score below each other
	lw $s0, textColour	# $s0 holds score colour
	addi $s1, $t0, 1320	# s1 holds score position
	la $s2, scores		# s2 holds scores
	li $s5, 0		# s5 holds loop counter
drawHighScoresLoop:
	beq $s5, 3, endDrawHighScoresLoop # branch when all scores have been drawn
	
	# save registers on the stack
	addi $sp, $sp, -4
	sw $s5, 0($sp)		# loop counter
	addi $sp, $sp, -4
	sw $s2, 0($sp)		# scores
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# colour
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# position
	
	# push position and colour on stack
	addi $sp, $sp, -4
	sw $s1, 0($sp)		# position
	addi $sp, $sp, -4
	sw $s0, 0($sp)		# colour
	
	lw $t5, 0($s2)		# load the score to draw into $t5
	jal drawScore 		# draw the score

	# restore registers from stack
	lw $s1, 0($sp)		# position
	addi $sp, $sp, 4
	lw $s0, 0($sp)		# colour
	addi $sp, $sp, 4
	lw $s2, 0($sp)		# scores
	addi $sp, $sp, 4
	lw $s5, 0($sp)		# loop counter
	addi $sp, $sp, 4
	
	addi $s1, $s1, 896	# next score location
	addi $s2, $s2, 4	# next score
	addi $s5, $s5, 1	# increment loop counter
	j drawHighScoresLoop	# back to loop start

endDrawHighScoresLoop:
	lw $ra, 0($sp)		# restore jump location from stack
	addi $sp, $sp, 4
	jr $ra			# jump back
	
loseGame:
	# print scoreboard on screen
	jal drawSky
	jal drawStart
	jal drawHighScores # TODO: make scoreboard
	# array of scores
	# change draw score function to draw at specified position
	# move score into t5 each time to draw
	
checkRestart:
	# check for input if the user wants to restart
	jal detectStartInput
	# sleep for a bit
	li $v0, 32
	li $a0, 1000
	syscall
	j checkRestart
	

# end of program
end:
.Exit:
	li $v0, 10 # terminate the program gracefully
	syscall
