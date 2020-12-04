/*
* 
*      Authors: Gerard Mijares, Christopher Butler, Madankrishna Acharya, Malik Allahham
*         File: tangled.v
*      Project: Assignment 4 - "Pipelined Tangled + Qat"
*      Created: 02 December 2020
* 
*  Description: Implements a pipelined Tangled Processor (PTP) design. 
*      
*  Notes: Based on Assignment 3 design by Chris Butler, Matthew Castle, and Blair Hall     
*/

// ADD IN `DEFINES THAT WE NEED FROM MULTI-CYCLE TANGLED PROCESSOR
// ADD IN INTERLOCK FUNCTIONS TO HELP STAGES COMMUNICATE AS NECESSARY

/* ************************************************************************** */
/* ****************** FLOATY modules provided by Dr. Dietz ****************** */
/* ************************************************************************** */

// MUST ADD NaN HANDLING TO EACH MODULE 

// Floating point Verilog modules for CPE480 (http://aggregate.org/EE480/floaty.html)
// Created February 19, 2019 by Henry Dietz (http://aggregate.org/hankd)
// Distributed under CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/)

// Field definitions
`define WORD_SIZE   [15:0]  // generic machine word size
`define QAT_WORD_SIZE [255:0]
`define INVERTED_QAT_WORD_SIZE [0:255]
`define INT         signed [15:0]   // integer size
`define FLOAT       [15:0]  // half-precision float size
`define FSIGN       [15]    // sign bit
`define FEXP        [14:7]  // exponent
`define FFRAC       [6:0]   // fractional part (leading 1 implied)

// Constants
`define FZERO   16'b0     // float 0
`define F32767  16'h46ff  // closest approx to 32767, actually 32640
`define F32768  16'hc700  // -32768
`define NaN     16'hffc0  // value for float NaN
`define intNaN  16'h8000  // value for int NaN

// Count leading zeros, 16-bit (5-bit result) d=lead0s(s)
module lead0s(d, s);
	output wire [4:0] d;
	input wire `WORD_SIZE s;
	wire [4:0] t;
	wire [7:0] s8;
	wire [3:0] s4;
	wire [1:0] s2;
	assign t[4] = 0;
	assign {t[3],s8} = ((|s[15:8]) ? {1'b0,s[15:8]} : {1'b1,s[7:0]});
	assign {t[2],s4} = ((|s8[7:4]) ? {1'b0,s8[7:4]} : {1'b1,s8[3:0]});
	assign {t[1],s2} = ((|s4[3:2]) ? {1'b0,s4[3:2]} : {1'b1,s4[1:0]});
	assign t[0] = !s2[1];
	assign d = (s ? t : 16);
endmodule

// Count trailing zeros, 256-bit
module traling0s(d, s);
	output wire [8:0] d;
	input wire `QAT_WORD_SIZE s;
	wire [8:0] t;
	wire [127:0] s128;
	wire [63:0] s64;
	wire [31:0] s32;
	wire [15:0] s16;
	wire [7:0] s8;
	wire [3:0] s4;
	wire [1:0] s2;
	assign t[8] = 0;
	assign {t[7],s128} = ((|s[127:0]) ? {1'b0,s[127:0]} : {1'b1,s[255:128]});
	assign {t[6],s64} = ((|s128[63:0]) ? {1'b0,s128[63:0]} : {1'b1,s128[127:64]});
	assign {t[5],s32} = ((|s64[31:0]) ? {1'b0,s64[31:0]} : {1'b1,s64[63:32]});
	assign {t[4],s16} = ((|s32[15:0]) ? {1'b0,s32[15:0]} : {1'b1,s32[31:16]});
	assign {t[3],s8} = ((|s16[7:0]) ? {1'b0,s16[7:0]} : {1'b1,s16[15:8]});
	assign {t[2],s4} = ((|s8[3:0]) ? {1'b0,s8[3:0]} : {1'b1,s8[7:4]});
	assign {t[1],s2} = ((|s4[1:0]) ? {1'b0,s4[1:0]} : {1'b1,s4[3:2]});
	assign t[0] = !s2[1];
	assign d = (|s ? t+1 : 0);
endmodule

// Float set-less-than, 16-bit (1-bit result) torf=a<b
module fslt(torf, a, b);
	output wire torf;
	input wire `FLOAT a, b;
	assign torf = (a==`NaN ? 0 : (b==`NaN ? 0 : ((a `FSIGN && !(b `FSIGN)) ||
		(a `FSIGN && b `FSIGN && (a[14:0] > b[14:0])) ||
		(!(a `FSIGN) && !(b `FSIGN) && (a[14:0] < b[14:0])))));
endmodule

// Floating-point addition, 16-bit r=a+b
module fadd(r, a, b);
	output wire `FLOAT r;
	input wire `FLOAT a, b;
	wire `FLOAT s;
	wire [8:0] sexp, sman, sfrac;
	wire [7:0] texp, taman, tbman;
	wire [4:0] slead;
	wire ssign, aegt, amgt, eqsgn;
	assign r = (a==`NaN ? `NaN : (b==`NaN ? `NaN : ((a == 0) ? b : ((b == 0) ? a : s))));
	assign aegt = (a `FEXP > b `FEXP);
	assign texp = (aegt ? (a `FEXP) : (b `FEXP));
	assign taman = (aegt ? {1'b1, (a `FFRAC)} : ({1'b1, (a `FFRAC)} >> (texp - a `FEXP)));
	assign tbman = (aegt ? ({1'b1, (b `FFRAC)} >> (texp - b `FEXP)) : {1'b1, (b `FFRAC)});
	assign eqsgn = (a `FSIGN == b `FSIGN);
	assign amgt = (taman > tbman);
	assign sman = (eqsgn ? (taman + tbman) : (amgt ? (taman - tbman) : (tbman - taman)));
	lead0s m0(slead, {sman, 7'b0});
	assign ssign = (amgt ? (a `FSIGN) : (b `FSIGN));
	assign sfrac = sman << slead;
	assign sexp = (texp + 1) - slead;
	assign s = (sman ? (sexp ? {ssign, sexp[7:0], sfrac[7:1]} : 0) : 0);
endmodule

// Floating-point multiply, 16-bit r=a*b
module fmul(r, a, b);
	output wire `FLOAT r;
	input wire `FLOAT a, b;
	wire [15:0] m; // double the bits in a fraction, we need high bits
	wire [7:0] e;
	wire s;
	assign s = (a `FSIGN ^ b `FSIGN);
	assign m = ({1'b1, (a `FFRAC)} * {1'b1, (b `FFRAC)});
	assign e = (((a `FEXP) + (b `FEXP)) -127 + m[15]);
	assign r = ((a==`NaN) || (b==`NaN) ? `NaN : (((a == 0) || (b == 0)) ? 0 : (m[15] ? {s, e, m[14:8]} : {s, e, m[13:7]})));
endmodule

// Floating-point reciprocal, 16-bit r=1.0/a
// Note: requires initialized inverse fraction lookup table
module frecip(r, a);
	output wire `FLOAT r;
	input wire `FLOAT a;
    wire `FLOAT NaNWire;
	reg [6:0] look[127:0];
	// initial $readmemh0(look);   // aggregate.org Icarus Verilog CGI simulator compatible
	initial $readmemh("frecip_lookup.vmem", look);
	assign NaNWire = `NaN;
    assign r `FSIGN = ((a==`NaN) || (a==0) ? NaNWire `FSIGN : a `FSIGN);
	assign r `FEXP = ((a==`NaN) || (a==0) ? NaNWire `FEXP : 253 + (!(a `FFRAC)) - a `FEXP);
	assign r `FFRAC = ((a==`NaN) || (a==0) ? NaNWire `FFRAC : look[a `FFRAC]);
endmodule

// Integer to float conversion, 16 bit
module i2f(f, i);
	output wire `FLOAT f;
	input wire `INT i;
	wire [4:0] lead;
	wire `WORD_SIZE pos;
	assign pos = (i[15] ? (-i) : i);
	lead0s m0(lead, pos);
	assign f `FFRAC = (i ? ({pos, 8'b0} >> (16 - lead)) : 0);
	assign f `FSIGN = i[15];
	assign f `FEXP = (i ? (128 + (14 - lead)) : 0);
endmodule

// Float to integer conversion, 16 bit
// Note: out-of-range values go to -32768 or 32767
module f2i(i, f);
	output wire `INT i;
	input wire `FLOAT f;
	wire `FLOAT ui;
	wire tiny, big;
	fslt m0(tiny, f, `F32768);
	fslt m1(big, `F32767, f);
	assign ui = {1'b1, f `FFRAC, 16'b0} >> ((128+22) - f `FEXP);
	assign i = (f==`NaN ? `intNaN : (tiny ? 0 : (big ? 32767 : (f `FSIGN ? (-ui) : ui))));
endmodule

/* ************************************************************************** */
/* ******************************* ALU Module ******************************* */
/* ************************************************************************** */

//ALU OPs
`define ALUOP_SIZE  [3:0] // size of ALU op field
`define ALUNOT      4'b0000
`define ALUFLOAT    4'b0001
`define ALUINT      4'b0010
`define ALUNEG      4'b0011
`define ALUNEGF     4'b0100
`define ALURECIP    4'b0101
`define ALUADD      4'b0110
`define ALUMUL      4'b0111
`define ALUSLT      4'b1000
`define ALUAND      4'b1001
`define ALUOR       4'b1010
`define ALUSHIFT    4'b1011
`define ALUXOR      4'b1100
`define ALUADDF     4'b1101
`define ALUMULF     4'b1110
`define ALUSLTF     4'b1111

// Integer and floating-point ALU
module ALU (out, op, x, y);
	output `WORD_SIZE out;
	input `ALUOP_SIZE op;
	input `WORD_SIZE x, y;
	reg`WORD_SIZE temp;
	wire fsltout;
	wire `WORD_SIZE faddout, fmulout, frecipout, i2fout, f2iout;
	reg `WORD_SIZE sltCheck;

	// instantiate floating point modules
	fslt myfslt(fsltout, x, y);
	fadd myfadd(faddout, x, y);
	fmul myfmul(fmulout, x, y);
	frecip myfrecip(frecipout, x);
	i2f myi2f(i2fout, x);
	f2i myf2i(f2iout, x);

	// assign temp to output
	assign out = temp;

	// assign output based on op
	always @* begin
		case (op)
			`ALUNOT: temp = ~x;
			`ALUFLOAT: temp = i2fout;
			`ALUINT: temp = f2iout;
			`ALUNEG: temp = -x;
			`ALUNEGF: temp = ((x==`NaN) ? `NaN : {~x`FSIGN,x[14:0]});
			`ALURECIP: temp = frecipout;
			`ALUADD: temp = x+y;
			`ALUMUL: temp = x*y;
			`ALUSLT: 
				begin 
					sltCheck = x - y;
					temp = ((sltCheck[15]) ? 16'b1 : 16'b0);
				end
			`ALUAND: temp = x&y;
			`ALUOR: temp = x|y;
			`ALUSHIFT: temp = ((y < 32768) ? (x << y) : (x >> -y));
			`ALUXOR: temp = x^y;
			`ALUADDF: temp = faddout;
			`ALUMULF: temp = fmulout;
			`ALUSLTF: temp = fsltout;
		endcase
	end
endmodule

/* ************************************************************************** */
/* ****************** Pipelined Tangled Processor (PTP) Module  ************* */
/* ************************************************************************** */
// Define memory aray sizes
`define IMEM_SIZE       	[2**16-1:0] // Instruction memory size 
`define DMEM_SIZE       	[2**16-1:0] // Data memory size
`define REGFILE_SIZE    	[2**4-1:0]  // The size of the regfile (i.e. 16 regs)
`define QAT_REGFILE_SIZE	[255:0]

// Format A field & values
`define FA_FIELD        	[15]
`define FA_FIELD_F0     	1
`define FA_FIELD_F1to4  	0

// Format B field & values
`define FB_FIELD        	[14:13]
`define FB_FIELD_F1     	1
`define FB_FIELD_F2     	2
`define FB_FIELD_F3     	3
`define FB_FIELD_F4     	0

// Format 0 Op codes
`define F0_OP_FIELD_HIGH    [14:13]
`define F0_OP_FIELD_LOW     [8]
`define F0_OP_LEX           0
`define F0_OP_LHI           1
`define F0_OP_BRF           2
`define F0_OP_BRT           3
`define F0_OP_MEAS          4
`define F0_OP_NEXT          5
`define F0_OP_HAD           6

// Format 1 Op codes
`define F1_OPA_FIELD        [8]
`define F1_OPA_FIELD_ALU    0
`define F1_OPA_FIELD_OPB    1
`define F1_OPB_FIELD        [7:4]
`define F1_OPB_JUMPR        0
`define F1_OPB_LOAD         8
`define F1_OPB_STORE        9
`define F1_OPB_COPY         10

// Format 2 Op Codes
`define F2_OP_FIELD         [12:8]
`define F2_OP_ONE           0
`define F2_OP_ZERO          1
`define F2_OP_NOT           2

// Format 3 Op Codes
`define F3_OP_FIELD         [12:8]
`define F3_OP_CCNOT         0
`define F3_OP_CSWAP         1
`define F3_OP_AND           2
`define F3_OP_OR            3
`define F3_OP_XOR           4
`define F3_OP_SWAP          16
`define F3_OP_CNOT          17

// Define instruction operands
`define IR_RD_FIELD         [12:9]
`define IR_RS_FIELD         [3:0]
`define IR_RA_FIELD 		[7:0]
`define IR_RB_FIELD 		[7:0]
`define IR_RC_FIELD 		[15:8]
`define IR_ALU_OP_FIELD     [7:4]
`define IR_IMM4_FIELD       [12:9]
`define IR_IMM8_FIELD       [7:0]
`define IR_QAT_RA_FIELD     [7:0]
`define IR2_QAT_RB_FIELD    [7:0]
`define IR2_QAT_RC_FIELD    [15:8]

`define noop 16'hffff
`define sys 16'h0000

module PTP(halt, reset, clk);

	output reg halt;
	input reset;
	input clk;

	reg `WORD_SIZE text `IMEM_SIZE;         // Instruction memory
	reg `WORD_SIZE data `DMEM_SIZE;         // Data memory

	reg `WORD_SIZE regfile `REGFILE_SIZE;   // Register File
	reg `QAT_WORD_SIZE Qatregfile `QAT_REGFILE_SIZE;   // Qat Register File

	reg `WORD_SIZE pc;                      // Program counter register

    // Buffers between stages
    wire `WORD_SIZE stage1to2iwire;
    wire `WORD_SIZE stage1to2iwire2;
    reg `WORD_SIZE stage1to2ir;
    reg `WORD_SIZE stage2ir1temp;            // Instruction register 2 (top 16 bits for 32-bit instructions)
    reg [1:0] branchJumpHaltFlag;
    reg `WORD_SIZE offset;
    reg `WORD_SIZE jumpDest;
    reg `WORD_SIZE stage2to3ir;
    reg `WORD_SIZE stage2to3ir2;
    reg sys2to3Flag;
    reg sys3to4Flag;
    reg `WORD_SIZE rd2to3Value;
    reg `WORD_SIZE rs2to3Value;
	reg `QAT_WORD_SIZE ra2to3Value;
	reg `QAT_WORD_SIZE rb2to3Value;
	reg `QAT_WORD_SIZE rc2to3Value;
    reg `WORD_SIZE stage3to4Result;
    reg stage3to4Write;
    reg [3:0] rd3to4Index;
	wire [3:0] rd2to3Index;
	assign rd2to3Index = stage2to3ir `IR_RD_FIELD;
	reg movePCBackTwo;
	reg QatFlag32;

	// Define some instruction operands
	// (Not necessay, but easier to read/use than having macros everywhere)
	wire [3:0] rdIndex, rsIndex;
	//wire [7:0] raIndex, rbIndex, rcIndex;
	reg [7:0] ra2to3Index;
	wire [3:0] aluOp;
	wire [7:0] imm8;
	assign imm8 = stage1to2ir`IR_IMM8_FIELD;
	wire [7:0] stage3imm8;
	assign stage3imm8 = stage2to3ir`IR_IMM8_FIELD;
	assign aluOp = stage2to3ir `IR_ALU_OP_FIELD;
	assign rdIndex = stage1to2ir `IR_RD_FIELD;
    assign rsIndex = stage1to2ir `IR_RS_FIELD;
	//assign raIndex = stage1to2ir `IR_RA_FIELD;
	//assign rbIndex = stage1to2ir `IR_RB_FIELD;
	//assign rcIndex = stage1to2ir `IR_RC_FIELD;

	wire `QAT_WORD_SIZE swapPlace;
	wire `QAT_WORD_SIZE swapPlace2;
	wire `QAT_WORD_SIZE cSwapPlaceA;
	wire `QAT_WORD_SIZE cSwapPlaceB;
	wire `QAT_WORD_SIZE cSwapPlaceC;
	wire `QAT_WORD_SIZE shiftedA;
	wire `WORD_SIZE ending0s;

	traling0s lead(ending0s, shiftedA);

	// Instantiate the ALU
	wire `WORD_SIZE aluOut;
	ALU alu(.out(aluOut), .op(aluOp), .x(rd2to3Value), .y(rs2to3Value));

    assign stage1to2iwire = text[pc];

    always @(posedge reset) begin
		halt = 0;
        stage1to2ir = `noop;
        stage2ir1temp = `noop;
        stage2to3ir = `noop;
        stage2to3ir2 = `noop;
        pc = 0;
        branchJumpHaltFlag = 0;
        sys2to3Flag = 0;
        sys3to4Flag = 0;
        stage3to4Write = 0;
        stage3to4Result = 0;
    end

    // Stage 1: PC increment and instruction fetch
    always @(posedge clk) begin
        if (branchJumpHaltFlag == 0)
        begin
            // Set a value for instruction register that can be read by Stage 2  
            pc <= pc + 1;
            stage1to2ir <= stage1to2iwire;
        end
        if (branchJumpHaltFlag == 1) // If branch
        begin
			begin
            	pc <= pc + offset - 1; // -1 since pc incremented last clock cycle
			end
			stage1to2ir <= `noop;
        end
        if (branchJumpHaltFlag == 2) //If jump
        begin
            pc <= jumpDest;
            stage1to2ir <= `noop;
        end
        if (branchJumpHaltFlag == 3) //If halt
        begin
			//Check if need to repeat instruction because of dependency
			if (movePCBackTwo)
			begin
				pc <= pc - 2;
			end
            //Do nothing
            stage1to2ir <= `noop;
        end
    end

    // Stage 2: Register Read
    always @(posedge clk) begin
        // Make sure IR is not on a NOOP
        if (stage1to2ir == `noop) //What is starting value of PC? worried about first cycle when PC has not been given an increment 
        begin
            // Do nothing
        end
		else if (QatFlag32) // Waiting for second half of 32 bit Qat instruction
		begin
			QatFlag32 <= 0;
			sys2to3Flag <= 0;
			branchJumpHaltFlag <= 0;
			stage2ir1temp <= `noop;
			stage2to3ir <= stage2ir1temp;
			stage2to3ir2 <= stage1to2ir;
			//ra2to3Value <= Qatregfile[raIndex];
			//rb2to3Value <= Qatregfile[rbIndex];
			//rc2to3Value <= Qatregfile[rcIndex];
		end
        else
        begin
			if (branchJumpHaltFlag) // Ignore instruction from stage 1 if branching, jumping, or halting
			begin
				sys2to3Flag <= 0;
				branchJumpHaltFlag <= 0; 
				stage2to3ir <= `noop;
                stage2to3ir2 <=`noop;
				movePCBackTwo <=0;
			end
			else
			begin
				if(stage1to2ir == `sys)
				begin
					sys2to3Flag <= 1;
					branchJumpHaltFlag <= 3; 
					stage2to3ir <= `noop;
					stage2to3ir2 <=`noop;
				end
				else
				begin
					// Check for RAW dependencies and insert noops
					if (((stage2to3ir != `noop) && ((rdIndex == stage2to3ir`IR_RD_FIELD) || (rsIndex == stage2to3ir`IR_RD_FIELD)) || (stage3to4Write && ((rdIndex == rd3to4Index) || (rsIndex == rd3to4Index)))) && !((stage2to3ir `FA_FIELD == `FA_FIELD_F1to4) && (stage2to3ir `FB_FIELD == `FB_FIELD_F3)) && !((stage2to3ir `FA_FIELD == `FA_FIELD_F1to4) && (stage2to3ir `FB_FIELD == `FB_FIELD_F2)))
					begin
						branchJumpHaltFlag <= 3; // Stop pc from incrementing
						rd2to3Value <= 0; // Won't have valid data
						rs2to3Value <= 0; // Won't have valid data
						stage2to3ir <= `noop;
						stage2to3ir2 <= `noop;
						movePCBackTwo <= 1;
					end
					else
					begin
						if((stage1to2ir `FA_FIELD == `FA_FIELD_F1to4) && (stage1to2ir `FB_FIELD == `FB_FIELD_F1) && (stage1to2ir `F1_OPA_FIELD == `F1_OPA_FIELD_OPB) && (stage1to2ir `F1_OPB_FIELD == `F1_OPB_JUMPR)) //Checking if it is jump instruction
						begin
							//Jump
							branchJumpHaltFlag <= 2; 
							jumpDest <= regfile[rdIndex]; 
							stage2to3ir <= `noop;
							stage2to3ir2 <= `noop;
						end
						else if ((stage1to2ir `FA_FIELD == `FA_FIELD_F0) && ({stage1to2ir `F0_OP_FIELD_HIGH, stage1to2ir `F0_OP_FIELD_LOW} == `F0_OP_BRF)) //Checking if it is a branch false
						begin
							//Branch
							branchJumpHaltFlag <= !regfile[rdIndex]; 
							offset <= imm8;
							stage2to3ir <= `noop;
							stage2to3ir2 <= `noop;
						end
						else if ((stage1to2ir `FA_FIELD == `FA_FIELD_F0) && ({stage1to2ir `F0_OP_FIELD_HIGH, stage1to2ir `F0_OP_FIELD_LOW} == `F0_OP_BRT)) //Checking if it is a branch true
						begin
							//Branch
							branchJumpHaltFlag <= (regfile[rdIndex] != 0); 
							offset <= imm8;
							stage2to3ir <= `noop;
							stage2to3ir2 <= `noop;
						end
						//Check if its 32 bit instruction.
						else if ((stage1to2ir `FA_FIELD == `FA_FIELD_F1to4) && (stage1to2ir `FB_FIELD == `FB_FIELD_F3)) //32 bit Qat instructions
						begin
							QatFlag32 <= 1; //Mark that next instruction is bottom half of this 32 bit instruction
							sys2to3Flag <= 0;
							branchJumpHaltFlag <= 0;
							stage2ir1temp <= stage1to2ir; //Store top half of instruction to use in next clock cycle
							stage2to3ir <= `noop; //Pass on a noop
							stage2to3ir2 <= `noop;
						end
						else
						begin
							branchJumpHaltFlag <= 0;
							rd2to3Value <= regfile[rdIndex];
							rs2to3Value <= regfile[rsIndex];
							stage2to3ir <= stage1to2ir;
							stage2to3ir2 <= `noop;
						end
					end
				end
			end
        end
    end

	assign swapPlace = Qatregfile[stage2to3ir`IR_QAT_RA_FIELD];
	assign swapPlace2 = Qatregfile[stage2to3ir2`IR2_QAT_RB_FIELD];
	assign cSwapPlaceA = Qatregfile[stage2to3ir`IR_QAT_RA_FIELD];
	assign cSwapPlaceB = Qatregfile[stage2to3ir2`IR2_QAT_RB_FIELD];
	assign cSwapPlaceC = Qatregfile[stage2to3ir2`IR2_QAT_RC_FIELD];
	assign shiftedA = Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] >> rd2to3Value;

    // Stage 3: ReadMem and ALU
    always @(posedge clk) begin
        // Decode and execute instruction
        // Set a flag when it is done
        if(sys2to3Flag == 1)
        begin
            sys3to4Flag <= 1;
        end
		else if (stage2to3ir == `noop)
		begin
			stage3to4Write <= 0;
			stage3to4Result <= 0;
			rd3to4Index <= 0;
		end

		else if ((stage2to3ir `FA_FIELD == `FA_FIELD_F1to4) && (stage2to3ir `FB_FIELD == `FB_FIELD_F3)) //32 bit Qat instructions
		begin
			if (stage2to3ir[12:8] == `F3_OP_AND) begin // Qat AND
				Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= (Qatregfile[stage2to3ir2`IR2_QAT_RB_FIELD] & Qatregfile[stage2to3ir2`IR2_QAT_RC_FIELD]);
			end
			else if (stage2to3ir[12:8] == `F3_OP_OR) begin // Qat OR
				Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= (Qatregfile[stage2to3ir2`IR2_QAT_RB_FIELD] | Qatregfile[stage2to3ir2`IR2_QAT_RC_FIELD]);
			end
			else if (stage2to3ir[12:8] == `F3_OP_XOR) begin
				Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= (Qatregfile[stage2to3ir2`IR2_QAT_RB_FIELD] ^ Qatregfile[stage2to3ir2`IR2_QAT_RC_FIELD]);
			end
			else if (stage2to3ir[12:8] == `F3_OP_SWAP) begin // Qat SWAP
				Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= swapPlace2;
				Qatregfile[stage2to3ir2`IR2_QAT_RB_FIELD] <= swapPlace;
			end
			else if (stage2to3ir[12:8] == `F3_OP_CSWAP) begin // Qat CSWAP
				//     (c&b) | ((~c) & a) 
				Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= ((cSwapPlaceC & cSwapPlaceB) | ((~cSwapPlaceC) & cSwapPlaceA));
				Qatregfile[stage2to3ir2`IR2_QAT_RB_FIELD] <= ((cSwapPlaceC & cSwapPlaceA) | ((~cSwapPlaceC) & cSwapPlaceB));
			end
			else if (stage2to3ir[12:8] == `F3_OP_CNOT) begin // Qat CNOT
				Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= (Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] ^ Qatregfile[stage2to3ir2`IR2_QAT_RB_FIELD]);
			end
			else if (stage2to3ir[12:8] == `F3_OP_CCNOT) begin // Qat CCNOT
				Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= (Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] ^ (Qatregfile[stage2to3ir2`IR2_QAT_RB_FIELD] & Qatregfile[stage2to3ir2`IR2_QAT_RC_FIELD]));
			end
		end
		else if ((stage2to3ir `FA_FIELD == `FA_FIELD_F1to4) && (stage2to3ir `FB_FIELD == `FB_FIELD_F2)) //16 bit Qat instructions
		begin
			if (stage2to3ir[12:8] == `F2_OP_ONE) begin
				Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= 256'hffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff;
			end
			else if (stage2to3ir[12:8] == `F2_OP_ZERO) begin
				Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= 256'h0;
			end
			else if (stage2to3ir[12:8] == `F2_OP_NOT) begin
				Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= ~Qatregfile[stage2to3ir`IR_QAT_RA_FIELD];
			end
		end
		else if ((stage2to3ir `FA_FIELD == `FA_FIELD_F0) && {stage2to3ir`F0_OP_FIELD_HIGH, stage2to3ir`F0_OP_FIELD_LOW} == `F0_OP_HAD)
		begin
			case (stage2to3ir `IR_IMM4_FIELD)
			0: Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= {128{{1{1'b1}}, {1{1'b0}}}}; 
			1: Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= {64{{2{1'b1}}, {2{1'b0}}}}; 
			2: Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= {32{{4{1'b1}}, {4{1'b0}}}}; 
			3: Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= {16{{8{1'b1}}, {8{1'b0}}}}; 
			4: Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= {8{{16{1'b1}}, {16{1'b0}}}}; 
			5: Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= {4{{32{1'b1}}, {32{1'b0}}}}; 
			6: Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= {2{{64{1'b1}}, {64{1'b0}}}}; 
			7: Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= {1{{128{1'b1}}, {128{1'b0}}}}; 
			default: Qatregfile[stage2to3ir`IR_QAT_RA_FIELD] <= {128{{1{1'b0}}, {1{1'b0}}}};
			endcase
		end
		else if ((stage2to3ir `FA_FIELD == `FA_FIELD_F0) && {stage2to3ir`F0_OP_FIELD_HIGH, stage2to3ir`F0_OP_FIELD_LOW} == `F0_OP_MEAS)
		begin
			regfile[rd2to3Index] <= Qatregfile[stage2to3ir`IR_QAT_RA_FIELD][rd2to3Value];
		end
		else if ((stage2to3ir `FA_FIELD == `FA_FIELD_F0) && {stage2to3ir`F0_OP_FIELD_HIGH, stage2to3ir`F0_OP_FIELD_LOW} == `F0_OP_NEXT)
		begin
			if (|ending0s) begin
				regfile[rd2to3Index] <= ending0s + rd2to3Value;
			end
			else begin
				regfile[rd2to3Index] <= 0;
			end
		end
        else
        begin
            sys3to4Flag <= 0;
            if(stage2to3ir `FA_FIELD == `FA_FIELD_F1to4)
            begin
                if(stage2to3ir `FB_FIELD == `FB_FIELD_F1)
                begin
                    if(stage2to3ir `F1_OPA_FIELD == `F1_OPA_FIELD_ALU) //ALU Instructions
                    begin
                        stage3to4Result <= aluOut;
                        stage3to4Write <= 1;
						rd3to4Index <= rd2to3Index;
                    end
                    else
                    begin
                        if(stage2to3ir `F1_OPB_FIELD == `F1_OPB_LOAD)
                        begin
                            //load
                            stage3to4Result <= data[rs2to3Value];
                            stage3to4Write <= 1;
							rd3to4Index <= rd2to3Index;
                        end
                        if(stage2to3ir `F1_OPB_FIELD == `F1_OPB_STORE)
                        begin
                            //store
                            data[rs2to3Value] <= rd2to3Value;
                            stage3to4Write <= 0;
                        end
                        if(stage2to3ir `F1_OPB_FIELD == `F1_OPB_COPY)
                        begin
                            //copy
                            stage3to4Result <= rs2to3Value;
                            stage3to4Write <= 1;
							rd3to4Index <= rd2to3Index;
                        end
                    end
                end
            end
			else if(stage2to3ir `FA_FIELD == `FA_FIELD_F0)
            begin
				if ({stage2to3ir`F0_OP_FIELD_HIGH, stage2to3ir`F0_OP_FIELD_LOW} == `F0_OP_LEX)
				begin
					stage3to4Write <= 1;
					stage3to4Result <= {stage3imm8[7] ? 8'hFF : 8'h00, stage3imm8};
					rd3to4Index <= rd2to3Index;
				end
				else if ({stage2to3ir`F0_OP_FIELD_HIGH, stage2to3ir`F0_OP_FIELD_LOW} == `F0_OP_LHI)
				begin
					stage3to4Write <= 1;
					stage3to4Result <= {stage3imm8, regfile[rd2to3Index][7:0]};
					rd3to4Index <= rd2to3Index;
				end
			end
			else //No ops matched
			begin
				stage3to4Write <= 0;
				stage3to4Result <= 0;
				rd3to4Index <= 0;
			end
        end
    end

    // Stage 4: Register Write
    always @(posedge clk) begin
        // Wait for ALU done flag to be set
        // Put the result in its destination if necessary
        if(sys3to4Flag == 1)
        begin
            halt <= 1;
        end
        else
        if(stage3to4Write == 1)
        begin
            regfile[rd3to4Index] <= stage3to4Result; 
        end
    end
endmodule

module PTP_TB();

	// Observing signals
	wire halt;

	// Driving signals
	reg reset = 0;
	reg clk = 0;

	// Instantiate the Unit Under Test
	PTP uut (.halt(halt), .reset(reset), .clk(clk));

	// Run the tests
	initial begin 
		// $readmemh1(uut.text);	// aggregate.org Icarus Verilog CGI simulator compatible
		// $dumpfile;				// aggregate.org Icarus Verilog CGI simulator compatible
		$readmemh("test_cases.text", uut.text);
		$readmemh("test_cases.data", uut.data);
		$dumpfile("dump.txt");
		$dumpvars(0, uut, uut.regfile[0], uut.regfile[1], uut.regfile[2], uut.Qatregfile[0], uut.Qatregfile[1], uut.Qatregfile[2]); // would normally trace 0, PE

		// Put the machine into a known state
		#5 reset = 1;
		#5 reset = 0;

		// Step through instructions until the machine is halted
		while (!halt) begin
			// Load
			$display("PC: %4d", uut.pc);
			#5 clk = 1;
			#5 clk = 0;
			$display("IR: %4h", uut.stage1to2ir);
		end
	end

endmodule