--- This file defines a sample package

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic.arith.all;


---declaration of package elements
package gt is

	-- subtype to specify delays
	subtype delay is time;
	subtype byte_t is std_logic_vector (7 downto 0);
	type bytearray_t is array (natural range <>) of byte_t;
	
	
	--- constant
	constant width : integer := 8;
	constant flag : std_logic_vector (31 downto 0) := X"10FFFFFF";
	
  --- a record
  type t1 is record
  a : std_logic_vector (11 downto 0);
  b : std_logic_vector (15 downto 0);
  c : std_logic_vector (3 downto 0);
  end record;

--- a function
function xor_it( in1 : t1; in2 : t2 ) return t1 ;

end package gt;


---definition of package elements
package body gt is
  function xor_it(in1 : t1; in2 : t2) return t1 is
    variable sum : t1;
    begin
      sum.a := in1.a xor in2.a ;
      sum.b := in1.b xor in2.b ;
      sum.c := in1.c xor in2.c ;
      return sum;
  end xor_it;

end gt;

