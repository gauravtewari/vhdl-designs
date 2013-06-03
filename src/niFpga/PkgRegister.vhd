-- © 2012 National Instruments Corporation.
--Use of this file is subject to the Software License Agreement provided with 
--LabVIEW FPGA Module and, without limiting any of the provisions in that license, 
--modifying or distributing this file is prohibited.

--Adding to, deleting from, or otherwise modifying the contents of this file 
--may cause the software to malfunction.
-------------------------------------------------------------------------------
--
-- File: PkgRegister.vhd
--
-------------------------------------------------------------------------------
--
-- Purpose:
--  This package contains all the information for registers that are part
--  of the register framework.
--
-- Top-level VI Controls and Indicators
-- Each control and indicator has 5 constants associated with it:
-- (1)           *: address offset
-- (2)      *Width: bit-width of the register
-- (3)    *Default: initial value of the register
-- (4)      *Index: Internal use only. The Index is used to select an address
--                  decode in the address decode vector that is handshaked to
--                  the clock domain containing this particular register
-- (5) *ShiftCount: Internal use only.  Number of bus accesses required to
--                  transfer the data in this register.
-------------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library work;
  use work.PkgNiUtilities.all;

package PkgRegister is

  -----------------------------------------------------------------------------
  -- Top-level VI Controls and Indicators
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Registers for Internal Use Only                                           
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Register Block Addresses
  -- Note:
  -- 1). The Addresses are represented in byte format.
  -- 2). The First and Last addresses are inclusive in the Register Block 
  --     address range.
  -----------------------------------------------------------------------------

  constant kniFpgaFifo0FirstAddress : unsigned(31 downto 0) := X"0001FF80";
  constant kniFpgaFifo0LastAddress  : unsigned(31 downto 0) := X"0001FFAB";

  -----------------------------------------------------------------------------

  constant kViSignature : natural := 16#1FFF4#;
  constant kViSignatureWidth : natural := 128;
  constant kViSignatureDefault : std_logic_vector(127 downto 0) := "00010000110010111101010111000000001101111011000001010110010011101110110000011100101101001001101111100001110111000101011001011101";
  constant kViSignatureIndex : natural := 32765;
  constant kViSignatureShiftCount : natural := 4;

  constant kDiagramReset : natural := 16#1FFFC#;
  constant kDiagramResetWidth : natural := 32;
  constant kDiagramResetDefault : std_logic_vector(31 downto 0) := "00000000000000000000000000000000";
  constant kDiagramResetIndex : natural := 32767;
  constant kDiagramResetShiftCount : natural := 1;

  constant kViControl : natural := 16#1FFF8#;
  constant kViControlWidth : natural := 32;
  constant kViControlDefault : std_logic_vector(31 downto 0) := "00000000000000000000000000000000";
  constant kViControlIndex : natural := 32766;
  constant kViControlShiftCount : natural := 1;

  -----------------------------------------------------------------------------
end PkgRegister;

package body PkgRegister is
end PkgRegister;
