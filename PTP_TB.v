/*
*
*      Authors:	Chris Butler, Matthew Castle, Blair Hall
*         File:	PTP_TB.v
*      Project:	Assignment 3 - "Pipelined Tangled"
*      Created:	6 November 2020
*
*  Description:	A testbench for verifying the Pipelined Tangled Processor 
*				(PTP) design.
*           	
*/



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
		readmemh("test_cases.data", uut.data);
		$dumpfile("dump.txt");
		$dumpvars(0, uut, PTP.r[0], uut.r[1], uut.r[2]); // would normally trace 0, PE

		// Put the machine into a known state
		#5 reset = 1;
		#5 reset = 0;

		// Step through instructions until the machine is halted
		while (!halt) begin
			// Load
			$display("PC: %4h", uut.pc);
			#5 clk = 1;
			#5 clk = 0;
			$display("IR: %4h", uut.stage1to2ir);
		end
	end

endmodule