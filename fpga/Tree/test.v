`timescale 100 s / 10 s

module Tree_test();
reg clk; 
reg[3:0] sw;
reg[7:0] tempdata;
reg [1:0] k;
wire [7:0] led;
wire [2:0] tree_counter;

Tree ff( .clk(clk), .k0(k[0]), .k1(k[1]), .sw(sw), .led(led), 
     .buf_empty(buf_empty), .buf_full(buf_full), .tree_counter(tree_counter));

initial
begin
    clk = 0;

    tempdata = 0;


    find(5, tempdata);
    insert(5);
    find(11, tempdata);
    insert(11);
    find(9, tempdata);
    insert(9);
    find(3, tempdata);
    insert(3);
    find(1, tempdata);
    insert(1);
    find(13, tempdata);
    insert(13);
    find(7, tempdata);
    insert(7);
    find(15, tempdata);
    insert(15);

    find(5, tempdata);
    $display("Out %d", tempdata);

    find(11, tempdata);
    $display("Out %d", tempdata);

    find(9, tempdata);
    $display("Out %d", tempdata);

    find(3, tempdata);
    $display("Out %d", tempdata);

    find(1, tempdata);
    $display("Out %d", tempdata);

    rm();

    find(13, tempdata);
    $display("Out %d", tempdata);

    find(7, tempdata);
    $display("Out %d", tempdata);

    find(15, tempdata);
    $display("Out %d", tempdata);
end

always
   #100000000 clk = ~clk;

task rm;
        begin
            $display("All removed");
            sw = 0;
            k[1] = 1;
            @(posedge clk);
            begin
                #1 k[1] = 0;
            end
        end

endtask

task insert;
input[3:0] data;
   if( buf_full )
            $display("---Cannot insert: Buffer Full---");
        else
        begin
            $display("Inserted ",data );
            sw = data;
            k[1] = 1;
            @(posedge clk);
            begin
                #1 k[1] = 0;
            end
        end

endtask

task find;
    input [3:0] in_data;
    output [3:0] out_data;

    if( buf_empty )
        $display("---Cannot Find: Buffer Empty---");

    else
        begin
            sw = in_data;
            k[0] = 1;
             
            @(posedge clk);
            #1 k[0] = 0;


            out_data = led[3:0];
            $display("-------------------------------Found %d, %d", out_data,
                led[7]);

        end
endtask
endmodule
