library IEEE;
use IEEE.std_logic_1164.all;

library gtlib;
use gtlib.gt.all;

entity test is
  port(
    clk : in std_logic;
    in1 : in t1;
    in2 : in t1;
    o : out t1
    );
end test;

architecture behavior of test is
  begin
    process(clk)
      begin
        if rising_edge(clk) then
          o <= xor_it(in1, in2);
        end if;
    end process;
end behavior;


