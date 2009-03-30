/*******************************************************************************
Copyright(c) 2000 - 2002 Analog Devices. All Rights Reserved.
Developed by Joint Development Software Application Team, IPDC, Bangalore, India
for Blackfin DSPs  ( Micro Signal Architecture 1.0 specification).

By using this module you agree to the terms of the Analog Devices License
Agreement for DSP Software. 
********************************************************************************
Module Name     : r8x8dct.asm
Label Name      : __r8x8dct
Version         :   2.0
Change History  :

                Version     Date          Author        Comments
		2.0       01/09/2007      		Tested with VDSP++ 4.5
                                                        compiler 7.2.3.2                 
		1.3         11/18/2002    Swarnalatha   Tested with VDSP++ 3.0
                                                        compiler 6.2.2 on 
                                                        ADSP-21535 Rev.0.2
                1.2         11/13/2002    Swarnalatha   Tested with VDSP++3.0
                                                        on ADSP-21535 Rev.0.2
                1.1         02/19/2002    Vijay         Modified to match
                                                        silicon cycle count
                1.0         03/13/2001    Vijay         Original 

Description     : This is the implementation of Chen's algorithm of DCT.
                  It is based on the separable nature of DCT for multi-
                  dimension. The input matrix is 8x8 real data. First, one dime-
                  nsional 8-point DCT is calculated for each of the 8 rows. The
                  output is stored in a separate matrix after transpose. Then 
                  again 8-point DCT is calculated on each row of matrix. The
                  output is again stored in a transpose matrix. This is final
                  output.
                 
                  Chen's algorithm has 4 stages of implementation. In the first
                  stage there are additions and subtractions only. In the second
                  stage addition and subtraction is done with one multiplication
                  In the third and last (fourth) stages  more MAC operations are
                  involved.

                  This implementation works only for 8x8 input. The input data 
                  should be real. The range of input should be -256 to 255. 
                 
                  The algorithm is in-placed. 

                  Note : The algorithm reads the input data from the "in" 
                        matrix.  
                        First 8-point DCT will be calculated for all the 8 rows.
                        This output is stored in "temp" buffer in the
                        transposed form at bit reversed locations. 
                        Again the 8-point DCT is applied on all the 8 rows of 
                        "temp" buffer. Final output computed is stored in "in" 
                        buffer in transposed form at bit reversed locations.
                        The operation of transposing the matrix and 
                        calculation of bit reversed are carried out while
                        writing the data without any explicit code.
                         
                        Output of function is provided "in" buffer in normal 
                        order.

Prototype       : void _r8x8dct(fract16 *in, fract16 *coeff, fract16 *temp);

                 *in -> Pointer to Input vector.
                 *coeff -> Pointer to coefficients.
                 *temp -> Pointer to temporary data. 

Registers Used  : A0, A1, R0-R7, I0-I3, B0-B3, M0, M1, L0-L3, P0-P5, LC0, LC1.

Performance     :

              Code Size   : 236 Bytes.
              Cycle Count : 293 Cycles.
*******************************************************************************/
.text
.global     __r8x8dct;
.align      8;
    
__r8x8dct:

/******************************* Function Prologue ***************************/
    [--SP] = (R7:4, P5:3);  //Pushing the Registers on stack.
    B0 = R0;                //Pointer to Input matrix.
    B3 = R1;                //Pointer to Coefficients.
    B2 = R2;                //Pointer to Temporary matrix.
    L0 = 0;                 //L registers are initialized to 0
    L1 = 0;                 //-------- do --------
    L2 = 0;                 //-------- do --------
    L3 = 16;                //L3 is set to 16 to make the coefficients
                            //array Circular.
/*
 I0, I1, and I2 registers are used to read the input data. I3 register is used
 to read the coefficients. P0 and P1 registers are used for writing the output
 data.  
*/  
    
    M0 = 12 (X);            // All these initialization are used in the
                            // modification of address offsets.
    M1 = 16 (X);
    P2 = 16;        
    P3 = 32 (X);
    P4 = -110 (X);
    P5 = -62 (X);  
    P0 = 2;
/*
   According to Chen's algorithm, first 8-point DCT will be calculated for all
   the 8 rows. The output of this calculation is stored in another transpose 
   matrix. Now again the 8-point DCT is applied on all the 8 rows. The output
   is stored in matrix in transposed form. This is the final output. 
   Therefore,
   a loop of 2 iteration (DCT_START, DCT_end) is set.

   B0 points to the "in" buffer and B2 points to "temp" buffer in the first 
   iteration. The input is read from "in" buffer and output is written to
   "temp" buffer. In the second iteration of DCT_START B0 points to "temp" and
   B2 points to "in" buffer. The input is read from "temp" buffer and output
   is written to "in" buffer. "in" buffer holds the final output.
*/
    
    LSETUP (DCT_START, DCT_END) LC0 = P0;
DCT_START:
        I0 = B0;            //I0 points to Input Element (0, 0)
        I1 = B0;            //Element 1 and 0 is read in R0.
        I1 += M0  || R0 = [I0++];
                            //I1 points to Input Element (0, 6) 
        I2 = I1;            //Element 6 is read in R3.H
        I2 -= 4   || R3.H = W[I1++];
                            //I2 points to Input Element (0, 4) 
    
        I3 = B3;            //I3 points to Coefficients
        P0 = B2;            //P0 points to temporary array Element (0, 0)
        P1 = B2;            //P1 points to temporary array
        R7 = [P1++P2] || R2 = [I2++];
                            //P1 points to temporary array Element (1, 0) 
                            //R7 is a dummy read. Element 4 and 5 are read in R2
        R3.L = W[I1--];     //Element 7 is read in R3.L
        R1.H = W[I0++];     //Element 2 is read in R1.H
    
//******************************* Implementation of Part 1 ********************
/*
  All the additions from Stage 1 and Stage 2 are implemented in Part1 1. for 
the optimization
  It is taken out of the loop for optimization point of view. (Part 1)
  The following instruction does the following job.
  Element 0 = (Element 0 + Element 7) / 2.
  Element 1 = (Element 1 + Element 6) / 2.
  Element 6 = (Element 1 - Element 6) / 2.
  Element 7 = (Element 0 - Element 7) / 2.
  It reads the data 3 in R1.L. 
*/
    
        R0 = R0 +|+ R3, R3 = R0 -|- R3 (ASR) || R1.L = W[I0++] || NOP;
    
/*
 This single instruction does the following job.
 Element 2 = (Element 2 + Element 5) / 2.
 Element 3 = (Element 3 + Element 4) / 2.
 Element 4 = (Element 3 - Element 4) / 2.
 Element 5 = (Element 2 - Element 5) / 2.
 It reads the Coefficients C4 = cos(4*pi/16) in register R7.
*/
    
        R1 = R1 +|+ R2, R2 = R1 -|- R2 (ASR, CO) || NOP ||  R7 = [I3++];
    
/*
   At the end of stage 1 R0 has (1,0), R1 has (2,3), R2 has (4, 5) and
   R3 has (6,7). Where notation (x, y) means the element from column x is in
   upper half of register and element from column y is in lower half of the
   register.
*/
    
//******************************* Implementation of Part 2 *********************
/*
   The following addition/subtraction  instruction does - 
   Element 0 = Element 0 + Element 3.
   Element 1 = Element 1 + Element 2.
   Element 2 = Element 1 - Element 2.
   Element 3 = Element 0 - Element 3.
*/
        R0 = R0 +|+ R1, R1 = R0 -|- R1;
    
        LSETUP (ROW_START, ROW_END) LC1 = P2 >> 1;
                            //The loop is set for 8 rows. 
ROW_START:
/*
   This is part 2 computation continued.....
   The following two instructions do -
   A1 = Element 6 * cos(pi/4) 
   A0 =  Element 6 * cos(pi/4)
   A1 = A1 - Element 5 * cos(pi/4)
   A0 = A0 + Element 5 * cos(pi/4).
   The instruction W[I0] = R3.L is used for packing it to R2.L. 
*/
            A1 = R3.H * R7.L, A0 = R3.H * R7.L  ||  I1 += M1 || W[I0] = R3.L;
            R4.H = ( A1 -= R2.L * R7.L), R4.L = (A0 += R2.L * R7.L) || I2 += M0;
/*
   At the end of stage 2 R0 has (1,0), R1 has (2,3), R4 has (5, 6).
*/
//************************ Implementation of Part 3 ***************************
/*
   The following two instruction does the job of stage 3 -
   A1 = Element 0 * cos(pi/4) 
   A0 =  Element 0 * cos(pi/4)
   A1 = A1 - Element 1 * cos(pi/4)
   A0 = A0 + Element 1 * cos(pi/4)
   The value of coefficients C2 and C6 are read in register R7. 
*/      
            A1 = R0.L * R7.L, A0 = R0.L * R7.L ||  NOP  || R3.H = W[I1++];
            R5.H = (A1 -= R0.H * R7.L), R5.L = (A0 += R0.H * R7.L)
            || R7 = [I3++];
    
/*
   The following three instructions do -
   A1 = Element 2 * cos(3pi/8) 
   A0 =  Element 3 * cos(3pi/8)
   A1 = A1 + Element 3 * cos(pi/8)
   A0 = A0 - Element 2 * cos(pi/8)
   R3 reads the value of cos pi/4.
   The value of coefficients C7 and C1 is read in register R7.
   Element 4 = Element 4 + Element 5.
   Element 5 = Element 4 - Element 5.
   Element 6 = Element 7 - Element 6.
   Element 7 = Element 7 + Element 6.
*/
            A1 = R1.H * R7.L, A0 = R1.L * R7.L ||  W[P0++P3] = R5.L
            || R2.L = W[I0];
            R2 = R2 +|+ R4, R4 = R2 -|- R4  ||  I0 += 4 || R3.L = W[I1--];
            R6.H = (A1 += R1.L * R7.H), R6.L = (A0 -= R1.H * R7.H) ||  I0 += 4
            || R7 = [I3++];
    
/*
   At the end of part 3 R2 has (4, 7), R4 has (5,6), R5 has (1, 0) and
   R6 has (2,3).
*/
//****************************** Implementation of Part 4 **********************
/*
   The following two instructions do -
   A1 = Element 4 * cos(7pi/16) 
   A0 =  Element 7 * cos(7pi/16)
   A1 = A1 + Element 7 * cos(pi/16)
   A0 = A0 - Element 4 * cos(pi/16)
   The value of next coefficients are read, and the registers are written to 
   their locations.
*/
    
            A1 = R2.H * R7.L, A0 = R2.L * R7.L ||  W[P0++P3] = R6.H
            || R0 = [I0++];
            R2.H = ( A1 += R2.L * R7.H), R2.L = ( A0 -= R2.H * R7.H)
            || W[P0++P3] = R5.H || R7 = [I3++]; 
/*
   The following two instructions do -
   A1 = Element 5 * cos(3pi/16) 
   A0 =  Element 6 * cos(3pi/16)
   A1 = A1 + Element 6 * cos(5pi/16)
   A0 = A0 - Element 5 * cos(5pi/16)
   The output values are written.
*/
    
            A1 = R4.H * R7.H, A0 = R4.L *R7.H ||  W[P0++P2] = R6.L 
            || R1.H = W[I0++]; 
            R4.H = (A1 += R4.L * R7.L), R4.L = ( A0 -= R4.H * R7.L)
            ||  W[P0++P4] = R2.L || R1.L = W[I0++];      
    
//******************* Implementation of Part 1 *****************************
/*
  This is the same part as part 1 specified earlier. First time the part 1 
  calculation is done outside the loop, after wards it is done here. It serves
  two purpose. Firts it computes part 1 for the next data, and it writes the
  data 5, and 6 to its bit reversed order in transpose way. 
*/
            R0 = R0 +|+ R3, R3 = R0 -|- R3 (ASR) || W[P1++P3] = R2.H
            || R2 = [I2++];
            R1 = R1 +|+ R2, R2 = R1 -|- R2 (ASR, CO) ||  W[P1++P3] = R4.L
            || R7 = [I3++]; 
ROW_END:    R0 = R0 +|+ R1, R1 = R0 -|- R1 ||  W[P1++P5] = R4.H || NOP;
        B1 = B0;            //Swapping of Input and output address pointers
        B0 = B2;            //B0 points to input buffer.
DCT_END:B2 = B1;            //B2 points to output buffer.
    
TERMINATE:
    (R7:4,P5:3)=[SP++];     //Pop the registers before returning.
    RTS;                    //Return.
    NOP;                    //to avoid one stall if LINK or UNLINK happens to be
                            //the next instruction after RTS in the memory.
__r8x8dct.end:                            
