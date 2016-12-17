`timescale 1 s/100 ms
module main;
    reg clk = 0;
    reg x = 0;
    reg y = 1;
    reg [7:0] a = 2;
    reg [7:0] b = 120;
    wire [7:0] sum;
    wire [3:0] out;
    
    adder myad(.a(a), .b(b), .sum(sum));
    //counter mycnt(.load(1'b1), .reset(1'b0), .clk(clk), .out(out)); 
    
    always @(posedge clk)
    begin
        x <= y;
        y <= x;
        #10000 $display("x = %d, y = %d\n", x, y);
    end


  initial 
    begin
        forever #5 clk = ~clk;

        $display("Hello, World %d", sum);
        $finish;
    end
    
endmodule
