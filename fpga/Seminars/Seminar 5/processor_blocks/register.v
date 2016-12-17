// регистр со сбросом
// на выход out непрерывно подаётся сохранённое значение
// если load = 1, то по переднему фронту clock'а в сохранённое значение записывается in
// по переднему фронту сигнала reset в регистр сохраняется значение 0
module register
  #(parameter Width = 32)
  ( input [Width-1:0] in,
    output reg [Width-1:0] out,
    input load, clock, reset
  );
  
  always @(posedge clock, posedge reset)
    if(reset) out <= 0;
    else if(load) out <= in;
endmodule
