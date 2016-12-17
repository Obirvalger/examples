// совокупность регистров процессора
// сейчас здесь 8 регистров общего назначения: r0 ... r7
// на выход rdata1 непрерывно выдаётся значение ri, i = raddr1[2:0], то есть игнорируются три старших бита номера регистра
// на выход rdata2 непрерывно выдаётся значение rj, j = raddr2[2:0]
// по сигналу reset немедленно все регистры сбрасываются в 0
// если write = 1, то по переднему фронту clock'а в регистр rk, k = waddr[2:0] записывается значение wdata
module register_file
  ( input [4:0] raddr1, raddr2, waddr,
    input [31:0] wdata,
    input write, clock, reset,
    output [31:0] rdata1, rdata2
  );
  
  wire [7:0] l;
  wire [31:0] rout0, rout1, rout2, rout3, rout4, rout5, rout6, rout7;
  
  bitdemux bdm(.i(write), .s(waddr[2:0]), .o(l));
  register r0(.in(wdata), .out(rout0), .load(l[0]), .clock(clock), .reset(reset));
  register r1(.in(wdata), .out(rout1), .load(l[1]), .clock(clock), .reset(reset));
  register r2(.in(wdata), .out(rout2), .load(l[2]), .clock(clock), .reset(reset));
  register r3(.in(wdata), .out(rout3), .load(l[3]), .clock(clock), .reset(reset));
  register r4(.in(wdata), .out(rout4), .load(l[4]), .clock(clock), .reset(reset));
  register r5(.in(wdata), .out(rout5), .load(l[5]), .clock(clock), .reset(reset));
  register r6(.in(wdata), .out(rout6), .load(l[6]), .clock(clock), .reset(reset));
  register r7(.in(wdata), .out(rout7), .load(l[7]), .clock(clock), .reset(reset));
  mux8 m1(
    .i0(rout0),
    .i1(rout1),
    .i2(rout2),
    .i3(rout3),
    .i4(rout4),
    .i5(rout5),
    .i6(rout6),
    .i7(rout7),
    .s(raddr1[2:0]),
    .o(rdata1)
  );
  mux8 m2(
    .i0(rout0),
    .i1(rout1),
    .i2(rout2),
    .i3(rout3),
    .i4(rout4),
    .i5(rout5),
    .i6(rout6),
    .i7(rout7),
    .s(raddr2[2:0]),
    .o(rdata2)
  );
endmodule
