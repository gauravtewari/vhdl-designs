--- Flip flop with synchronous active low reset and chip enable signal

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dff is
  port(
    d       : in std_logic;     -- input data
    clk     : in std_logic;     -- input clk
    ce      : in std_logic;     -- chip enable signal
    rst_n   : in std_logic;      -- Active low reset signal
    
    q       : out std_logic;
    q_n     : out std_logic
  );
end entity dff;

architecture behavior of dff is begin
  process (clk)
  begin
    if rising_edge(clk) then
      if rst_n = '0' then
        q <= '0';
        q_n <= '1';
      elsif ce = '1' then
        q <= d;
        q_n <= not d;
      end if;
    end if;
  end process;
end architecture behavior;

            
