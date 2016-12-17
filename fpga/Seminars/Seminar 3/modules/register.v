module register
	#(parameter		Width = 8)
	(output reg [Width-1:0] out,
	input 		[Width-1:0] in,
	input			clear, load, clock);
	
	always @(posedge clock)
		if (~clear)
			out <= 0;
		else if (~load)
			out <= in;
endmodule
