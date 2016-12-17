module adder
	#(parameter Width = 8)
	(input [Width-1:0]	a,b,
	output [Width-1:0]	sum);
	
	assign sum = a+b;
endmodule
