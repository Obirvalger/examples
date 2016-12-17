module counter(input load, reset, clk, output [3:0] out);
reg [3:0] out;
always @(posedge clk)
    if (reset) begin
        out <= 4'b0 ;
    end else if (load) begin
        out <= out + 1;
    end


endmodule

