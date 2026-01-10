library ieee ;
    use ieee.std_logic_1164.all ;
    use ieee.numeric_std.all ;

library gnodeb;
    use gnodeb.all;

library nuand;
    use nuand.fifo_readwrite_p.all;

entity gnodeb_top is
  port (
    rx_clock           :   in      std_logic ;
    rx_reset           :   in      std_logic ;
    rx_enable          :   in      std_logic ;

    tx_clock           :   in      std_logic ;
    tx_reset           :   in      std_logic ;
    tx_enable          :   in      std_logic ;

    -- TX packet interface (record)
    tx_packet_control  :   in      packet_control_t ;
    tx_packet_empty    :   in      std_logic ;
    tx_packet_ready    :   out     std_logic ;

    -- RX packet interface (record)
    rx_packet_control  :   out     packet_control_t ;
    rx_packet_ready    :   in      std_logic ;

    -- Legacy control (kept for top-level compatibility; unused internally)
    rx_packet_enable   :   in      std_logic ;

    -- LEDs
    leds            :   out     std_logic_vector( 2 downto 0)
  ) ;
end gnodeb_top ;

architecture rtl of gnodeb_top is
begin


  u_clash : entity work.gnodeb_fapi_top
    port map (
      -- clocks / resets / enables
      rx_clock        => rx_clock,
      rx_reset        => rx_reset,
      rx_enable       => rx_enable,

      tx_clock        => tx_clock,
      tx_reset        => tx_reset,
      tx_enable       => tx_enable,

      -- TX packet (rename *_data_valid -> *_valid; drop core_id/flags)
      tx_pkt_sop      => tx_packet_control.pkt_sop,
      tx_pkt_eop      => tx_packet_control.pkt_eop,
      tx_pkt_data     => tx_packet_control.data,
      tx_pkt_data_valid    => tx_packet_control.data_valid,

      -- RX packet (rename *_data_valid -> *_valid; drop core_id/flags)
      rx_pkt_sop      => rx_packet_control.pkt_sop,
      rx_pkt_eop      => rx_packet_control.pkt_eop,
      rx_pkt_data     => rx_packet_control.data,
      rx_pkt_data_valid    => rx_packet_control.data_valid,

      -- Handshakes
      rx_packet_enable => rx_packet_enable,
      rx_packet_ready => rx_packet_ready,
      tx_packet_empty => tx_packet_empty,
      tx_packet_ready => tx_packet_ready,

      -- LEDs (renamed in core)
      leds            => leds
    );

end architecture;
