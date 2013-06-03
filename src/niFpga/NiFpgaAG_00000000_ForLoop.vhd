-- © 2012 National Instruments Corporation.
--Use of this file is subject to the Software License Agreement provided with 
--LabVIEW FPGA Module and, without limiting any of the provisions in that license, 
--modifying or distributing this file is prohibited.

--Adding to, deleting from, or otherwise modifying the contents of this file 
--may cause the software to malfunction.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library work;
use work.PkgNiUtilities.all;
use work.PkgNiFpgaSimFxp.all;
use work.PkgNiFpgaSimInterfaceLvDataTypes.all;

entity NiFpgaAG_00000000_ForLoop is
		port (
			reset : in std_logic;
			enable_in : in std_logic;
			enable_out : out std_logic;
			enable_clr : in std_logic;
			iteration : in std_logic_vector(31 downto 0);
			max : in std_logic_vector(31 downto 0);
			res_80000001 : in std_logic_vector(1 downto 0);
			res_80000002 : out std_logic_vector(49 downto 0);
			RioClk40 : in std_logic
		);
end NiFpgaAG_00000000_ForLoop;

architecture vhdl_labview of NiFpgaAG_00000000_ForLoop is

	constant c_0 : std_logic_vector(0 downto 0) := "0";
	signal output : std_logic_vector(0 downto 0) := "0"; 
	signal s_Timeout_189 : std_logic_vector(31 downto 0) := "00000000000000000000000000000000"; 
	signal ei00000001 : std_logic;
	signal eo00000001 : std_logic;
	signal ei00000003 : std_logic;
	signal eo00000003 : std_logic;
	signal output_2 : std_logic_vector(15 downto 0); 

begin
	n_prim_CVT: entity work.NiLvCoerce (rtl)
		generic map (
			kInType => work.PkgNiLvPrims.Int,
			kInWidth => 32,
			kInSigned => true,
			kInEwl => 0,
			kInIwl => 32,
			kInFwl => 0,
			kInIncludesOverflow => false,
			kOutType => work.PkgNiLvPrims.Int,
			kOutWidth => 16,
			kOutSigned => true,
			kOutEwl => 0,
			kOutIwl => 16,
			kOutFwl => 0,
			kOutIncludesOverflow => false,
			kRoundMode => work.PkgFxp.ConvertLvToFpgaRoundMode(0),
			kOverflowMode => work.PkgFxp.ConvertLvToFpgaOverflowMode(1),
			kSatUpperLimit => work.PkgFxp.PackVectorForEntityBoundaryCrossing("0111111111111111"),
			kSatLowerLimit => work.PkgFxp.PackVectorForEntityBoundaryCrossing("1000000000000000"),
			kRegisterMode => work.PkgFxp.ConvertLvToFpgaRegisterMode(1)
		)
		port map(
			Clk => RioClk40,
			aReset => reset,
			cEnableIn => ei00000001,
			cEnableOut => eo00000001,
			cEnableOutClear => enable_clr,
			cIn => iteration,
			cOut => output_2
		);

	n_FIFO_Write_FIFO_Method_Node_49_Diagram: block
  signal cErrorStatus : std_logic := '0';
begin
  res_80000002 <= s_Timeout_189 & output_2 & enable_clr & (ei00000003 and not cErrorStatus);
  eo00000003 <= res_80000001(0) or cErrorStatus when (ei00000003 = '1' and enable_clr = '0') else '0';


end block;

	ei00000001 <= enable_in;
	ei00000003 <= eo00000001 AND enable_in;
	enable_out <= eo00000003;

end vhdl_labview;


