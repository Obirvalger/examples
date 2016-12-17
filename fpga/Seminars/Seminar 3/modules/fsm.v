module fsm (reset, clock);
	 
	 reg c_state, n_state;
		
always @ (c_state)
begin
	case (c_state)
		1'b0:
			begin //State A
				...
			end
		1'b1: //State B
			begin
				...
			end
		 endcase
end

always @ (posedge clock, negedge reset)
begin
	if (~reset)
		c_state <= 0;
	else
		c_state <= n_state;
end
endmodule
		