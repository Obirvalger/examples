`timescale 1 s / 100 ms

module Tree_test();
reg clk, rst, wr_en, rd_en ;
reg[3:0] sw;
reg[7:0] tempdata;
reg [1:0] k;
wire [7:0] led;
wire [2:0] tree_counter;

Tree ff( .clk(clk), .k0(k[0]), .k1(k[1]), .rst(rst), .sw(sw), .led(led), 
         .wr_en(wr_en), .rd_en(rd_en), .buf_empty(buf_empty), 
         .buf_full(buf_full), .tree_counter(tree_counter) );

initial
begin
    clk = 0;
    rst = 1;
    rd_en = 0;
    wr_en = 0;

    tempdata = 0;

    insert(1);
    insert(10);
    insert(8);

    find(3, tempdata);
    $display("Out %d", tempdata);
    //sw = 0;


    //#15 rst = 0;

    //push(1);
    //fork
    //push(2);
    //pop(tempdata);
    //join              //push and pop together   
    //push(10);
    //push(20);
    //push(30);
    //push(40);
    //push(50);
    //push(60);
    //push(70);
    //push(80);
    //push(90);
    //push(100);
    //push(110);
    //push(120);
    //push(130);

    //pop(tempdata);
    //push(tempdata);
    //pop(tempdata);
    //pop(tempdata);
    //pop(tempdata);
    //pop(tempdata);
    //push(140);
    //pop(tempdata);
    //push(tempdata);//
    //pop(tempdata);
    //pop(tempdata);
    //pop(tempdata);
    //pop(tempdata);
    //pop(tempdata);
    //pop(tempdata);
    //pop(tempdata);
    //pop(tempdata);
    //pop(tempdata);
    //pop(tempdata);
    //pop(tempdata);
    //push(5);
    //pop(tempdata);
end

always
   #5 clk = ~clk;

task insert;
input[3:0] data;
   if( buf_full )
            $display("---Cannot insert: Buffer Full---");
        else
        begin
           $display("Inserted ",data );
           sw = data;
           k[1] = 1;
           //k[1] = 1;
                @(posedge clk);
                begin
                    #1 k[1] = 0;
                    //#1 k[1] = 1;
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

            out_data = led;
            $display("-------------------------------Found ", out_data);

        end
endtask

task remove;
output [3:0] data;

   if( buf_empty )
            $display("---Cannot Pop: Buffer Empty---");
   else
        begin

     rd_en = 1;
          @(posedge clk);

          #1 rd_en = 0;
          data = led;
           $display("-------------------------------Poped ", data);

        end
endtask

//task push;
//input[7:0] data;


   //if( buf_full )
            //$display("---Cannot push: Buffer Full---");
        //else
        //begin
           //$display("Pushed ",data );
           //sw = data;
           //wr_en = 1;
                //@(posedge clk);
                //#1 wr_en = 0;
        //end

//endtask

//task pop;
//output [7:0] data;

   //if( buf_empty )
            //$display("---Cannot Pop: Buffer Empty---");
   //else
        //begin

     //rd_en = 1;
          //@(posedge clk);

          //#1 rd_en = 0;
          //data = led;
           //$display("-------------------------------Poped ", data);

        //end
//endtask

endmodule
