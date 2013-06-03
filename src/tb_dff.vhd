
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_dff is
end tb_dff;
  
architecture test_bench of tb_dff is
  component dff
  port(
    d       : in std_logic;     -- input data
    clk     : in std_logic;     -- input clk
    ce      : in std_logic;     -- chip enable signal
    rst_n   : in std_logic;      -- Active low reset signal
    
    q       : out std_logic;
    q_n     : out std_logic
  );
  end component;

  signal d : std_logic := '0';
  signal clk : std_logic := '0';
  signal ce : std_logic := '0';
  signal rst_n : std_logic := '1';
  signal q : std_logic;
  signal q_n : std_logic;
  constant clk_period : time := 10 ns;

begin

  UUT: dff port map(
    d => d, clk => clk, ce => ce, rst_n => rst_n, q => q, q_n => q_n
    );

  clk_process: process
    begin
      wait for (clk_period/2);
      clk <= not clk;
  end process;

  stimulus: process
    begin
      wait for 10 * clk_period;
      report "Starting giving stimulus";
      wait;
  end process;
  
end architecture test_bench;
