//`define BUF_WIDTH 3    // BUF_SIZE = 16 -> BUF_WIDTH = 4, no. of bits to be used in pointer
//`define BUF_SIZE ( 1<<`BUF_WIDTH )
//`define ELEM_SIZE 4;

module Tree( clk, k0, k1, rst, sw, led, wr_en, rd_en, buf_empty, buf_full, tree_counter );

input                 rst, k0, k1, clk, wr_en, rd_en;   
// reset, system clock, write enable and read enable.
input [3:0]           sw;                   
// data input to be pushed to buffer
output[7:0]           led;                  
// port to output the data using pop.
output                buf_empty, buf_full;      
// buffer empty and full indication 
output[2:0] tree_counter;             
// number of data pushed in to buffer   

reg[7:0]              led;
reg                   exit_flg, found;
reg                   buf_empty, buf_full;
reg[2:0] tree_counter;
reg[2:0] rd_ptr, wr_ptr;           // pointer to read and write addresses  
reg[3:0] buf_mem[6:0]; //  
reg[2:0] left_ind[6:0]; //  
reg[2:0] right_ind[6:0]; //  

reg[2:0] i;

always @(tree_counter)
begin
   buf_empty = (tree_counter == 0);
   buf_full = (tree_counter == 7);

end

always @(posedge clk or posedge rst)
begin
   if( rst )
       tree_counter <= 0;

   else if( (!buf_full && wr_en) && ( !buf_empty && rd_en ) )
       tree_counter <= tree_counter;

   else if( !buf_full && wr_en )
       tree_counter <= tree_counter + 1;

   else if( !buf_empty && rd_en )
       tree_counter <= tree_counter - 1;

   else
      tree_counter <= tree_counter;
end

always @( posedge clk or posedge rst)
begin
   if( rst )
      led <= 0;
   else
   begin
      if( rd_en && !buf_empty )
          begin
              i = 0;
              exit_flg = 0;
              found = 0;
              while (!found || !exit_flg)
              begin
                  if (sw < buf_mem[i]) 
                      if (left_ind[i] < 7)
                          i = left_ind[i];
                      else
                          exit_flg = 1;

                  else if (sw > buf_mem[i])
                          i = right_ind[i];

                  else if (sw == buf_mem[i])
                      found = 1;
              end
              led[7] = found; 
              if (found)
                  led[3:0] = sw;
          end  
      else
         led <= led;

   end
end

//always @(posedge clk or rst)
//begin
    //if (k0)
    //begin
        //if (exit_flg) 
    //end
//end

always @(posedge clk)
begin

   if( wr_en && !buf_full )
      buf_mem[ wr_ptr ] <= sw;

   else
      buf_mem[ wr_ptr ] <= buf_mem[ wr_ptr ];
end

always@(posedge clk or posedge rst)
begin
   if( rst )
   begin
      wr_ptr <= 0;
      rd_ptr <= 0;
   end
   else
   begin
      if( !buf_full && wr_en )    wr_ptr <= wr_ptr + 1;
          else  wr_ptr <= wr_ptr;

      if( !buf_empty && rd_en )   rd_ptr <= rd_ptr + 1;
      else rd_ptr <= rd_ptr;
   end

end

always@(posedge clk)
begin
    led[6:4] = tree_counter;
    if (~buf_empty)
        led[3:0] = buf_mem[0];
    else 
        led[3:0] = 0;
end 

always@(posedge clk or posedge k1)
begin
    if (sw[0])
    begin
        tree_counter = 0;
        for (i = 0; i < 7; ++i)
        begin
            buf_mem[i] = 0;
            left_ind[i] = 7;
            right_ind[i] = 7;
        end
    end
end
endmodule
