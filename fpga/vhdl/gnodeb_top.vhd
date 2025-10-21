library ieee ;
use ieee.std_logic_1164.all ;
use ieee.numeric_std.all ;


library nuand;
use nuand.fifo_readwrite_p.all;

entity gnodeb_top is
  port (
    tx_clock                :   in      std_logic ;
    tx_reset                :   in      std_logic ;
    tx_packet_control       :   in      packet_control_t;
    tx_packet_empty         :   in      std_logic;
    tx_packet_ready         :   out     std_logic;
    leds                    :   out     std_logic_vector( 2 downto 0)
    ) ;
end gnodeb_top ;

architecture simple of gnodeb_top is
  type fsm_tx_t is (IDLE, WAIT_FOR_SOP, READ_PACKET, DONE, ERR, TEST);

  type state_tx_t is record
    fsm : fsm_tx_t;
    ready_for_packet : std_logic;
    read_next_word   : std_logic;
    data             : std_logic_vector(31 downto 0);
    read_dwords      : integer;
  end record;

  signal current_tx_state, future_tx_state : state_tx_t;

  function NULL_TX_STATE return state_tx_t is
    variable rv : state_tx_t;
  begin
    rv.fsm := IDLE;
    rv.ready_for_packet := '0';
    rv.read_next_word   := '0';
    rv.data             := (others => '0');
    rv.read_dwords      := 0;
    return rv;
  end function;

begin

tx_packet_ready <= '1' when (current_tx_state.ready_for_packet = '1' or current_tx_state.read_next_word = '1') else '0';

  -- Drive LEDs based on FSM state (your existing code is fine)
   leds <= not "111" when current_tx_state.fsm = IDLE else
           not "001" when current_tx_state.fsm = WAIT_FOR_SOP else
           not "010" when current_tx_state.fsm = READ_PACKET else
           not "011" when current_tx_state.fsm = DONE else
           not "100" when current_tx_state.fsm = ERR else
           not "000" when current_tx_state.fsm = TEST else
           not "101";

  tx_state_comb : process(all)
  begin
    future_tx_state <= current_tx_state;
    future_tx_state.read_next_word <= '0';

    case current_tx_state.fsm is
      when IDLE =>
        if (tx_packet_empty = '0') then
          future_tx_state.fsm <= WAIT_FOR_SOP;
        end if;

      when WAIT_FOR_SOP =>
        future_tx_state.ready_for_packet <= '1';

        if (tx_packet_control.pkt_sop = '1' and tx_packet_control.data_valid = '1') then
          future_tx_state.ready_for_packet <= '0';
          future_tx_state.fsm <= READ_PACKET;
          future_tx_state.read_dwords <= 1;
          future_tx_state.data <= tx_packet_control.data;
        end if;

      when READ_PACKET =>
        future_tx_state.read_next_word <= '1';
        if (tx_packet_control.data_valid = '1') then
          future_tx_state.read_dwords <= current_tx_state.read_dwords + 1;
          future_tx_state.data <= tx_packet_control.data;
          if (tx_packet_control.pkt_eop = '1') then
             future_tx_state.fsm <= TEST;
             future_tx_state.read_next_word <= '0';
          end if;
        end if;

      when TEST =>
        if (current_tx_state.data = x"0000000D") then
            future_tx_state.fsm <= TEST;
        else
            future_tx_state.fsm <= ERR;
        end if;

      when DONE =>
        future_tx_state.fsm <= IDLE;

      when ERR =>
        future_tx_state.fsm <= IDLE;

    end case;
  end process tx_state_comb;


  tx_sync_proc : process(tx_clock, tx_reset)
  begin
    if (tx_reset = '1') then
      current_tx_state <= NULL_TX_STATE;
    elsif (rising_edge(tx_clock)) then
      current_tx_state <= future_tx_state;
    end if;
  end process tx_sync_proc;

end simple;
