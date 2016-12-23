`timescale 1 s / 100 ms

module sort_test();
reg clk;
reg [1:0] key;
reg [3:0] sw;
wire [7:0] led;
reg[7:0] tempdata;

sort ss( .clk(clk), .key1(key[0]), .key2(key[1]), .sw(sw), .led(led));

// iverilog sort.v test.v -o test
// vvp test

initial 
begin
  clk = 0;
  insert(4'b1111, tempdata);
  insert(4'b1001, tempdata);
  insert(4'b0110, tempdata);
  insert(4'b0011, tempdata);

  $display("Before sort %b", tempdata);
  run_sort(tempdata);
  $display("After sort %b", tempdata);
end

always
   #5 clk = ~clk;


task insert;
input[3:0] data;
output [7:0] out_data;
begin
    $display("Inserted %b", data);
    sw = data;
    key[0] = 1;
    @(posedge clk);
    begin
      #1 key[0] = 0;
    end
    out_data = led;
    $display("--Found %b", out_data);
end
endtask

task run_sort;
output [7:0] out_data;
begin
    key[1] = 1;
    @(posedge clk);
    begin
      #1 key[1] = 0;
    end
    out_data = led;
    $display("--Found %b", out_data);
end
endtask

endmodule