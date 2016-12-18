module zhegalkin
	( input reset, enable,
		input [0:7] a,
		output reg[0:7] b);
		reg[7:0] w1, w2;
		reg [7:0] i;
		reg [7:0] l;
		always @ (*)
		begin
			if (enable == 1)
			begin
				for(i=0; i < 8; i=i+2) 
				begin
					w1[i] = a[i];
					w1[i+1] = a[i] ^ a[i+1];
				end
				for(l=0; l<8; l=l+4) 
				begin
					w2[l]=w1[l];
					w2[l+1]=w1[l+1];
					w2[l+2]=w1[l]^w1[l+2];
					w2[l+3]=w1[l+1]^w1[l+3];
				end
				b[0]=w2[0];
				b[1]=w2[1];
				b[2]=w2[2];
				b[3]=w2[3];
				b[4]=w2[0]^w2[4];
				b[5]=w2[1]^w2[5];
				b[6]=w2[2]^w2[6];
				b[7]=w2[3]^w2[7];
				
				if (reset == 1)
				begin
				b=8'b00000000;
				end
			end
		end
endmodule 