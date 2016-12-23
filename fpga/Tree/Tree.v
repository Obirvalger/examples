module Tree( clk, k0, k1, sw, led, buf_empty, buf_full, tree_counter );

input                 k0, k1, clk;
input [3:0]           sw;                   
output[7:0]           led;                  
output                buf_empty, buf_full;      
output[2:0]           tree_counter;             

reg[7:0]              led;
reg                   exit_flg, found;
reg                   buf_empty, buf_full;
reg[2:0] tree_counter;
reg[3:0] buf_mem[6:0]; 
reg[2:0] left_ind[6:0];
reg[2:0] right_ind[6:0];

reg[2:0] i;
reg[2:0] curr;

initial
begin
    tree_counter = 0;
    for(i = 0; i < 7; i = i + 1)
    begin
        buf_mem[i] = 0;
        left_ind[i] = 7;
        right_ind[i] = 7;
    end
end

always @(tree_counter)
begin
   buf_empty = (tree_counter == 0);
   buf_full = (tree_counter == 7);

end

// Find
always @( posedge clk )
begin
      if( k0 && ~buf_empty )
          begin
              i = 0;
              exit_flg = 0;
              found = 0;
              while (~found && ~exit_flg)
              begin
                  if (sw < buf_mem[i]) 
                      if (left_ind[i] < 7)
                          i = left_ind[i];
                      else
                          exit_flg = 1;

                  else if (sw > buf_mem[i])
                      if (right_ind[i] < 7)
                          i = right_ind[i];
                      else
                          exit_flg = 1;

                  else if (sw == buf_mem[i])
                      found = 1;
              end
          end  
      else
      begin
          led <= led;
          exit_flg = 1;
          found = 0;
      end

    led[3:0] <= sw;
    led[7] <= found; 
end

// Insert
always @(posedge clk)
begin
    if (k1 && ~buf_full)
    begin
        if (~found)
            if (~buf_empty)
            begin
                if (sw < buf_mem[i]) 
                    left_ind[i] = tree_counter;
                if (sw > buf_mem[i]) 
                    right_ind[i] = tree_counter;
            end
            else
                curr = 0;

            buf_mem[tree_counter] <= sw;
            tree_counter = tree_counter + 1;
    end
end

// Travel
always @(posedge clk)
begin

end

// Output tree_counter
always@(posedge clk)
begin
    led[6:4] = tree_counter;
    if (~buf_empty)
        led[3:0] = buf_mem[0];
    else 
        led[3:0] = 0;
end 

// Remove all
always@(posedge clk or posedge k1)
begin
    if (~sw[0] && k1)
    begin
        tree_counter = 0;
        for(i = 0; i < 7; i = i + 1)
        begin
            buf_mem[i] = 0;
            left_ind[i] = 7;
            right_ind[i] = 7;
        end
    end
end
endmodule
