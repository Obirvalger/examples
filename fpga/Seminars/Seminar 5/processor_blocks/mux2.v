// обычный мультиплексор: если s = 0, то на выход o выдаётся значение i0, иначе - i1
// Width - ширина мультиплексируемых данных, по умолчанию - 32
module mux2
  #(parameter Width = 32)
  ( input [Width-1:0] i0, i1,
    input s,
    output [Width-1:0] o
  );
  assign o = (s ? i1 : i0);
endmodule
