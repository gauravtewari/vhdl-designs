-- © 2012 National Instruments Corporation.
--You may only modify and distribute this file as expressly permitted in the Software License Agreement
--provided with LabVIEW FPGA Module.  Without limiting any of the provisions in that license agreement, 
--you may only distribute modified versions of this file to end-users who (a) you have verified have a 
--valid license to LabVIEW FPGA Module, and (b) you restrict from using the file for any purpose other 
--than the customization of the FPGA functionality of Hardware Supported by NI for Use with LabVIEW 
--FPGA Module (as that term is defined in Section 4 of the license agreement).
--
--*****************************************************************************************
--*                                                                                       *
--* Definitions: "niFpga" and "user" Subdirectories                                       *
--* -----------------------------------------------                                       *
--* The following comments refer to subdirectories,"niFpga" and "user," of the            *
--* "Destination Directory" specified in the "Information" category of the simulation     *
--* build specification.  These subdirectories have special functions.                    *
--*                                                                                       *
--* WARNING                                                                               *
--* -------                                                                               *
--* Only make modifications to the version of this file that exists in the                *
--* "user" subdirectory. Any edits to the version of this file in the "niFpga"            *
--* subdirectory will be overwritten during the next build.                               *
--*                                                                                       *
--* Test Bench Instructions                                                               *
--* -----------------------                                                               *
--*                                                                                       *
--* LabVIEW generates a test bench template every time you build a simulation export.     *
--* LabVIEW places the generated file in the "niFpga" subdirectory. If this file          *
--* does NOT exist already in the "user" subdirectory, LabVIEW places a copy there;       *
--* LabVIEW does not overwrite modifications to any file in the "user" subdirectory.      *
--*                                                                                       *
--* Integrate changes from the test bench file in the "niFpga" subdirectory to the test   *
--* bench file in the "user" subdirectory when (1) the target has changed, (2) project    *
--* items, such as DMA FIFOs, have changed or (3) when controls and indicators have       *
--* changed.                                                                              *
--*                                                                                       *
--* Signal Descriptions                                                                   *
--* -------------------                                                                   *
--* DiagramClocks:                                                                        *
--* DiagramClocks is a signal of tDiagramClocks type which is declared in                 *
--* PkgNiFpgaSimulationModel.vhd. tDiagramClocks includes an element for every clock      *
--* used to drive logic on the block diagram, including any external clocks.              *
--* Because the elements in DiagramClocks are driven by these block diagram and           *
--* external clocks, consider DiagramClocks a copy of these clocks.                       *
--* Elements in DiagramClocks will be delayed one (or more) delta(s) from the             *
--* source clocks. Use DiagramClocks to synchronize the testbench to block diagram        *
--* or external clocks.                                                                   *
--*                                                                                       *
--*****************************************************************************************

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library niFpga;
use niFpga.PkgRegister.all;
use nifpga.PkgNiFpgaSimCommonProcedures.all;
use niFpga.PkgNiFpgaSimControlAndIndicatorProcedures.all;
use niFpga.PkgNiFpgaSimFifoProcedures.all;
use niFpga.PkgNiFpgaSimIrqProcedures.all;
use niFpga.PkgNiFpgaSimMiscProcedures.all;
use niFpga.PkgNiFpgaSimInterfaceTypes.all;
use niFpga.PkgNiFpgaSimInterfaceLvDataTypes.all;
use niFpga.PkgNiFpgaSimulationModel.all;
use niFpga.PkgNiFpgaSimUserTypes.all;
use niFpga.PkgNiUtilities.all;

entity tb_NiFpgaSimulationModel_cosim_ModelSim is 
end entity tb_NiFpgaSimulationModel_cosim_ModelSim;

architecture testbench of tb_NiFpgaSimulationModel_cosim_ModelSim is

  signal DiagramClocks: tDiagramClocks ;
  signal FiClock: std_logic := '0';
  signal fiStopSim : boolean := false;
  
  signal DStarAClk : std_logic;
  signal aLvTrig0 : std_logic;
  signal aLvTrig1 : std_logic;
  signal aLvTrig2 : std_logic;
  signal aLvTrig3 : std_logic;
  signal aLvTrig4 : std_logic;
  signal aLvTrig5 : std_logic;
  signal aLvTrig6 : std_logic;
  signal aLvTrig7 : std_logic;
  signal StarClkpin : std_logic;
  signal aPxieDStarB : std_logic;
  signal aPxieDStarC : std_logic;
  signal aPxieSync100 : std_logic;
  signal rLvFpgaIoModPresent : std_logic;
  signal rLvFpgaIoModPowerGd : std_logic;
  signal rLvFpgaInsertedIoModId :  std_logic_vector(31 downto 0);
  signal rLvFpgaPcbTemp :  std_logic_vector(15 downto 0);
  signal aUserGpio : std_logic_vector(65 downto 0);
  signal aUserGpio_n : std_logic_vector(65 downto 0);
  signal UserGClkLvds : std_logic;
  signal UserGClkLvds_n : std_logic;
  signal UserGClkLvttl : std_logic;

begin

  --===========================================================================
  -- Clock Generation
  --===========================================================================

  FiClock <= not FiClock after kFiClockPulseWidth when not fiStopSim;




  

  --===========================================================================
  -- Simulation Model
  --===========================================================================

  NiFpgaSimulationModel_cosim_ModelSim_Instance: entity niFpga.NiFpgaSimulationModel_cosim_ModelSim
    port map (
      FiClock => FiClock,      
      fiStopSim => fiStopSim,
      DStarAClk => DStarAClk,
      aLvTrig0 => aLvTrig0,
      aLvTrig1 => aLvTrig1,
      aLvTrig2 => aLvTrig2,
      aLvTrig3 => aLvTrig3,
      aLvTrig4 => aLvTrig4,
      aLvTrig5 => aLvTrig5,
      aLvTrig6 => aLvTrig6,
      aLvTrig7 => aLvTrig7,
      StarClkpin => StarClkpin,
      aPxieDStarB => aPxieDStarB,
      aPxieDStarC => aPxieDStarC,
      aPxieSync100 => aPxieSync100,
      rLvFpgaIoModPresent => '1',
      rLvFpgaIoModPowerGd => '1',
      rLvFpgaInsertedIoModId => niFpga.pkgLvFpgaConst.kExpectedTbId,
      rLvFpgaPcbTemp => b"0000000001100100",
      aUserGpio => aUserGpio,
      aUserGpio_n => aUserGpio_n,
      UserGClkLvds => UserGClkLvds,
      UserGClkLvds_n => UserGClkLvds_n,
      UserGClkLvttl => UserGClkLvttl,
      DiagramClocks => DiagramClocks);

end testbench;
