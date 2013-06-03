-- © 2012 National Instruments Corporation.
--Use of this file is subject to the Software License Agreement provided with 
--LabVIEW FPGA Module and, without limiting any of the provisions in that license, 
--modifying or distributing this file is prohibited.

--Adding to, deleting from, or otherwise modifying the contents of this file 
--may cause the software to malfunction.
-------------------------------------------------------------------------------
--
-- Purpose:
-- Common package for simulation procedures
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package PkgNiFpgaSimCommonProcedures is

  constant kNiFpga_OpenAttribute_NoRun : natural := 0;
  constant kNiFpga_RunAttribute_WaitUntilDone : natural := 0;
  constant kNiFpga_CloseAttribute_NoResetIfLastSession : natural := 0;


end package PkgNiFpgaSimCommonProcedures ;