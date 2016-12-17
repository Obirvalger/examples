// память данных состоит из восьми регистров и дополнительных структур, выдающих и записывающих в регистры нужные значения
// непрерывно в odata выдаётся значение регистра ri, i = addr[4:2], т.е. addr - это смещение в памяти, и память игнорирует два последних бита (выравнивание по словам) и принимает во внимание только три последних бита оставшегося массива
// если выставлен write, то по положительному фронту clock в регистр ri, i = addr[4:2], записывается значение idata
module data_memory
  ( input [31:0] addr,
    output [31:0] odata,
    input [31:0] idata,
    input write, clock
  );
  
  wire [7:0] l;
  wire [31:0] rout0, rout1, rout2, rout3, rout4, rout5, rout6, rout7;
  
  bitdemux bdm(.i(write), .s(addr[4:2]), .o(l));
  register r0(.in(idata), .out(rout0), .load(l[0]), .clock(clock));
  register r1(.in(idata), .out(rout1), .load(l[1]), .clock(clock));
  register r2(.in(idata), .out(rout2), .load(l[2]), .clock(clock));
  register r3(.in(idata), .out(rout3), .load(l[3]), .clock(clock));
  register r4(.in(idata), .out(rout4), .load(l[4]), .clock(clock));
  register r5(.in(idata), .out(rout5), .load(l[5]), .clock(clock));
  register r6(.in(idata), .out(rout6), .load(l[6]), .clock(clock));
  register r7(.in(idata), .out(rout7), .load(l[7]), .clock(clock));
  mux8 m(
    .i0(rout0),
    .i1(rout1),
    .i2(rout2),
    .i3(rout3),
    .i4(rout4),
    .i5(rout5),
    .i6(rout6),
    .i7(rout7),
    .s(addr[4:2]),
    .o(odata)
  );
endmodule
