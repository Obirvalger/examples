module sort (clk, key1, key2, sw, led);

input clk, key1, key2;
input [3:0] sw;
output reg [7:0] led;

integer index, i, j;
reg [1:0] temp;
reg [1:0] array [0:3];


always @(posedge clk)
begin
	if (key1) begin
		index = sw[3:2];
 		array[index] = sw[1:0];
	end

	if (key2) begin
		for (i = 3; i >= 0; i = i - 1) begin
			for (j = 0 ; j < i; j = j + 1) begin
				if (array[j] > array[j + 1]) begin
					temp = array[j];
					array[j] = array[j + 1];
					array[j + 1] = temp;
				end 
		  	end
		end
	end

	for (i = 3; i >= 0; i = i - 1) begin
		index = 7 - 2*i;
		led[index -: 2] = array[i];
	end
end
endmodule