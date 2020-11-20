/*
* 
*       Author: Chris Butler
*         File: ALU_TB.v
*      Project: Assignment 3 - "Pipelined Tangled"
*      Created: 18 October 2020
*      Updated: 6 November 2020
* 
*  Description: A testbench for verifying the ALU design.
*      
*/

module testbench();
	reg `WORD_SIZE result, a, b;
	reg `ALUOP_SIZE op;
	wire `WORD_SIZE temp;
	reg `WORD_SIZE correct = 0, total = 26;
	ALU myalu(temp, op, a, b);

	initial begin
		$dumpfile;              // aggregate.org Icarus Verilog CGI simulator compatible
		$dumpvars(2, testbench);

		a = 16'hffff;
		op = `ALUNOT;
		#1 result = temp;
		if (result == 16'h0000) begin
			correct = correct + 1;
		end else begin
			$display("NOT %x = %x", a, result);
		end

		a = 16'h0001;
		op = `ALUFLOAT;
		#1 result = temp;
		if (result == 16'h3f80) begin
			correct = correct + 1;
		end else begin
			$display("FLOAT %x = %x", a, result);
		end

		a = 16'h3f80;
		op = `ALUINT;
		#1 result = temp;
		if (result == 16'h0001) begin
			correct = correct + 1;
		end else begin
			$display("INT %x = %x", a, result);
		end

		a = 5;
		op = `ALUNEG;
		#1 result = temp;
		if (result == 16'hfffb) begin
			correct = correct + 1;
		end else begin
			$display("NEG %d = %x", a, result);
		end

		a = 16'h3f80;
		op = `ALUNEGF;
		#1 result = temp;
		if (result == 16'hbf80) begin
			correct = correct + 1;
		end else begin
			$display("NEGF %x = %x", a, result);
		end

		a = 16'h4000;
		op = `ALURECIP;
		#1 result = temp;
		if (result == 16'h3f00) begin
			correct = correct + 1;
		end else begin
			$display("RECIP %x = %x", a, result);
		end

		a = 4;
		b = 7;
		op = `ALUADD;
		#1 result = temp;
		if (result == 11) begin
			correct = correct + 1;
		end else begin
			$display("ADD %d = %d", a, result);
		end

		a = 5;
		b = 3;
		op = `ALUMUL;
		#1 result = temp;
		if (result == 15) begin
			correct = correct + 1;
		end else begin
			$display("MUL %d = %d", a, result);
		end

		a = 8;
		b = 9;
		op = `ALUSLT;
		#1 result = temp;
		if (result == 1) begin
			correct = correct + 1;
		end else begin
			$display("SLT %x = %x", a, result);
		end

		a = 16'hcfcf;
		b = 16'hfcfc;
		op = `ALUAND;
		#1 result = temp;
		if (result == 16'hcccc) begin
			correct = correct + 1;
		end else begin
			$display("AND %x = %x", a, result);
		end

		a = 16'hfcfc;
		b = 16'hcfcf;
		op = `ALUOR;
		#1 result = temp;
		if (result == 16'hffff) begin
			correct = correct + 1;
		end else begin
			$display("OR %x = %x", a, result);
		end

		a = 16'hf0f0;
		b = -4;
		op = `ALUSHIFT;
		#1 result = temp;
		if (result == 16'h0f0f) begin
			correct = correct + 1;
		end else begin
			$display("SHIFT %x = %x", a, result);
		end

		a = 16'hfcfc;
		b = 16'hcfcf;
		op = `ALUXOR;
		#1 result = temp;
		if (result == 16'h3333) begin
			correct = correct + 1;
		end else begin
			$display("XOR %x = %x", a, result);
		end

		a = 16'h3f80;
		b = 16'h4000;
		op = `ALUADDF;
		#1 result = temp;
		if (result == 16'h4040) begin
			correct = correct + 1;
		end else begin
			$display("ADDF %x = %x", a, result);
		end

		a = 16'h4000;
		b = 16'h4000;
		op = `ALUMULF;
		#1 result = temp;
		if (result == 16'h4080) begin
			correct = correct + 1;
		end else begin
			$display("MULF %x = %x", a, result);
		end

		a = 16'h3f80;
		b = 16'h4000;
		op = `ALUSLTF;
		#1 result = temp;
		if (result == 16'h0001) begin
			correct = correct + 1;
		end else begin
			$display("SLTF %x = %x", a, result);
		end

        //NaN test cases

	    a = `NaN;
		op = `ALUINT;
		#1 result = temp;
		if (result == `intNaN) begin
			correct = correct + 1;
		end else begin
			$display("INT %x = %x", a, result);
		end

        a = `NaN;
		op = `ALUNEGF;
		#1 result = temp;
		if (result == `NaN) begin
			correct = correct + 1;
		end else begin
			$display("NEGF %x = %x", a, result);
		end

		a = 0;
		op = `ALURECIP;
		#1 result = temp;
		if (result == `NaN) begin
			correct = correct + 1;
		end else begin
			$display("RECIP %x = %x", a, result);
		end

        a = `NaN;
		op = `ALURECIP;
		#1 result = temp;
		if (result == `NaN) begin
			correct = correct + 1;
		end else begin
			$display("RECIP %x = %x", a, result);
		end

        a = `NaN;
		b = 16'h4000;
		op = `ALUADDF;
		#1 result = temp;
		if (result == `NaN) begin
			correct = correct + 1;
		end else begin
			$display("ADDF %x = %x", a, result);
		end

        a = 16'h4000;
		b = `NaN;
		op = `ALUADDF;
		#1 result = temp;
		if (result == `NaN) begin
			correct = correct + 1;
		end else begin
			$display("ADDF %x = %x", a, result);
		end

		a = `NaN;
		b = 16'h4000;
		op = `ALUMULF;
		#1 result = temp;
		if (result == `NaN) begin
			correct = correct + 1;
		end else begin
			$display("MULF %x = %x", a, result);
		end

        a = 16'h4000;
		b = `NaN;
		op = `ALUMULF;
		#1 result = temp;
		if (result == `NaN) begin
			correct = correct + 1;
		end else begin
			$display("MULF %x = %x", a, result);
		end

		a = `NaN;
		b = 16'h4000;
		op = `ALUSLTF;
		#1 result = temp;
		if (result == 16'h0000) begin
			correct = correct + 1;
		end else begin
			$display("SLTF %x = %x", a, result);
		end

        a = 16'h4000;
		b = `NaN;
		op = `ALUSLTF;
		#1 result = temp;
		if (result == 16'h0000) begin
			correct = correct + 1;
		end else begin
			$display("SLTF %x = %x", a, result);
		end

		$display("Correct:%d, Failed:%d", correct, total-correct);

	end

endmodule


