library ieee;
use ieee.std_logic_1164.all;

package components is

  component usb_cdc is
    generic (
      VENDORID               : std_logic_vector(15 downto 0) := X"0000";
      PRODUCTID              : std_logic_vector(15 downto 0) := X"0000";
      CHANNELS               : integer                       := 1;
      IN_BULK_MAXPACKETSIZE  : integer                       := 8;
      OUT_BULK_MAXPACKETSIZE : integer                       := 8;
      BIT_SAMPLES            : integer                       := 4;
      USE_APP_CLK            : integer                       := 0;
      APP_CLK_FREQ           : integer                       := 12);
    port (
      app_clk_i    : in  std_logic;
      clk_i        : in  std_logic;
      rstn_i       : in  std_logic;
      out_ready_i  : in  std_logic;
      in_data_i    : in  std_logic_vector(7 downto 0);
      in_valid_i   : in  std_logic;
      dp_rx_i      : in  std_logic;
      dn_rx_i      : in  std_logic;
      frame_o      : out std_logic_vector(10 downto 0);
      configured_o : out std_logic;
      out_data_o   : out std_logic_vector(7 downto 0);
      out_valid_o  : out std_logic;
      in_ready_o   : out std_logic;
      dp_pu_o      : out std_logic;
      tx_en_o      : out std_logic;
      dp_tx_o      : out std_logic;
      dn_tx_o      : out std_logic);
  end component usb_cdc;

end components;
