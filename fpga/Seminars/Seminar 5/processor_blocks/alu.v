// in1, in2 - два числа, над которыми надо произвести операцию
// out - результат операции, выводится немедленно непрерывно
// aluop - код операции
// Width - ширина входных чисел и выходного числа, по умолчанию 32
// OPR_ADD - сложить числа
// OPR_SUB - вычесть числа
module alu
  #(parameter Width = 32)
  ( input [Width-1:0] in1, in2,
    input [5:0] aluop,
    output reg [Width-1:0] out
  );
  
  parameter OPR_ADD = 6'b100000;
  parameter OPR_SUB = 6'b100010;
  
  always @(in1, in2, aluop)
    case(aluop)
    OPR_ADD : out = in1 + in2;
    OPR_SUB : out = in1 - in2;
    default : out = 0;
    endcase
endmodule
