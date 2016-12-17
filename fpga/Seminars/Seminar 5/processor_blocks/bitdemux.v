// у этого блока один однобитовый вход i, Num выходов o, организованные в шину, и один управляющий вход s
// на вход s подаётся (в двоичной записи) номер бита в шине o, на который нужно перенаправить вход i
// на остальные биты выхода подаётся ноль
//
// то есть в каждый момент времени имеем o[s] = i и o[k] = 0 при k != j
//
// параметр LogNum - ширина управляющей шины s; с помощью LogNum бит кодируются числа от 0 до 2**LogNum - 1 - это ширина выходной шины o
module bitdemux
  #(parameter LogNum = 3)
  ( input i,
    input [LogNum - 1:0] s,
    output reg [2**LogNum-1:0] o
  );
  parameter Num = 2**LogNum;
  
  always @(i, s)
  begin : switch_block
    integer k;
    for(k = 0; k < Num; k = k + 1)
      o[k] = (k == s) ? i : 0;
  end
endmodule
