// обычный мультиплексор: на выход o выдаётся значение ij, где j = s
// Width - ширина мультиплексируемых данных, по умолчанию - 32
module mux8
  #(parameter Width = 32)
  ( input [Width-1:0] i0, i1, i2, i3, i4, i5, i6, i7,
    input [2:0] s,
    output reg [Width-1:0] o
  );
  
  always @(s, i0, i1, i2, i3, i4, i5, i6, i7)
    case(s)
    3'd0: o = i0;
    3'd1: o = i1;
    3'd2: o = i2;
    3'd3: o = i3;
    3'd4: o = i4;
    3'd5: o = i5;
    3'd6: o = i6;
    3'd7: o = i7;
    default: o = 0;
    endcase
endmodule
