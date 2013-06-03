-- © 2012 National Instruments Corporation.
--Use of this file is subject to the Software License Agreement provided with 
--LabVIEW FPGA Module and, without limiting any of the provisions in that license, 
--modifying or distributing this file is prohibited.

--Adding to, deleting from, or otherwise modifying the contents of this file 
--may cause the software to malfunction.






library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.PkgNiFpgaSimInterfaceTypes.all;
use work.PkgNiFpgaSimInterfaceLvDataTypes.all;
use work.PkgNiFpgaSimulationModel.all;
use work.PkgNiUtilities.all;
use work.PkgSwitchedChinch.all;

entity NiFpgaSimulationModel is
port (
    FiClock : in std_logic;    
    fiHostToTargetInterface : in tFiInterface;
    fiTargetToHostInterface : out tFiInterface;
    fiHostToTargetReady : out boolean;
    fiTargetToHostReady : in boolean;
    aInterrupt : out boolean;
    fiStopSim : in boolean;
    fiFifoInterrupt : out std_logic_vector(kDmaChannelConfigArray'range);
    DStarAClk : in std_logic;
    aLvTrig0 : inout std_logic;
    aLvTrig1 : inout std_logic;
    aLvTrig2 : inout std_logic;
    aLvTrig3 : inout std_logic;
    aLvTrig4 : inout std_logic;
    aLvTrig5 : inout std_logic;
    aLvTrig6 : inout std_logic;
    aLvTrig7 : inout std_logic;
    StarClkpin : in std_logic;
    aPxieDStarB : in std_logic;
    aPxieDStarC : out std_logic;
    aPxieSync100 : in std_logic;
    rLvFpgaIoModPresent : in std_logic;
    rLvFpgaIoModPowerGd : in std_logic;
    rLvFpgaInsertedIoModId : in  std_logic_vector(31 downto 0);
    rLvFpgaPcbTemp : in  std_logic_vector(15 downto 0);
    aUserGpio : inout std_logic_vector(65 downto 0);
    aUserGpio_n : inout std_logic_vector(65 downto 0);
    UserGClkLvds : in std_logic;
    UserGClkLvds_n : in std_logic;
    UserGClkLvttl : in std_logic;
    DiagramClocks   : out tDiagramClocks
    );
end entity NiFpgaSimulationModel;

architecture simulation of NiFpgaSimulationModel is

  constant kFpgaEndpoint : natural :=  3;
  signal ReliableClkIn_Local : std_logic := '0';
  signal RioClk40_Local : std_logic := '0';
  signal PxiClk10Fpga_Local : std_logic := '0';
  signal DramClkDiv100_Local : std_logic := '0';
  signal DramClk200_Local : std_logic := '0';
  signal Dram0ClkUser_Local : std_logic := '0';
  signal Dram1ClkUser_Local : std_logic := '0';

  signal fiStopSimLocal : boolean := false;

  signal InputClocks : tInputClocks;
    signal aReset, bReset : boolean;
  signal BusClk : std_logic;
  signal bIoPort2InputRx: SwitchedLinkRx_t;
  signal bIoPort2InputTx: SwitchedLinkTx_t;
  signal bIoPort2OutputRx: SwitchedLinkRx_t;
  signal bIoPort2OutputTx: SwitchedLinkTx_t;
  

begin

  fiStopSimLocal <= fiStopSim;


  assert kFiClockPulseWidth*2*kBusClkPwDivFactor*2 = kBusClkPeriod
    report "[NiFpgaSimulationModel]: Decrease simulator resolution so that simulation behaves properly.  Try 1 ps resolution."
    severity error;

  
  ReliableClkIn_ClockGenerator: 
  process
  begin
    if not fiStopSimLocal then
      ReliableClkIn_Local <= not ReliableClkIn_Local;
      InputClocks.ReliableClkIn <= ReliableClkIn_Local;
      wait for 4.000 ns;
    else
      wait until not fiStopSimLocal;
    end if;
  end process;

  RioClk40_ClockGenerator: 
  process
  begin
    if not fiStopSimLocal then
      RioClk40_Local <= not RioClk40_Local;
      InputClocks.RioClk40 <= RioClk40_Local;
      wait for 12.500 ns;
    else
      wait until not fiStopSimLocal;
    end if;
  end process;

  PxiClk10Fpga_ClockGenerator: 
  process
  begin
    if not fiStopSimLocal then
      PxiClk10Fpga_Local <= not PxiClk10Fpga_Local;
      InputClocks.PxiClk10Fpga <= PxiClk10Fpga_Local;
      wait for 50.000 ns;
    else
      wait until not fiStopSimLocal;
    end if;
  end process;

  DramClkDiv100_ClockGenerator: 
  process
  begin
    if not fiStopSimLocal then
      DramClkDiv100_Local <= not DramClkDiv100_Local;
      InputClocks.DramClkDiv100 <= DramClkDiv100_Local;
      wait for 5.000 ns;
    else
      wait until not fiStopSimLocal;
    end if;
  end process;

  DramClk200_ClockGenerator: 
  process
  begin
    if not fiStopSimLocal then
      DramClk200_Local <= not DramClk200_Local;
      InputClocks.DramClk200 <= DramClk200_Local;
      wait for 2.500 ns;
    else
      wait until not fiStopSimLocal;
    end if;
  end process;

  Dram0ClkUser_ClockGenerator: 
  process
  begin
    if not fiStopSimLocal then
      Dram0ClkUser_Local <= not Dram0ClkUser_Local;
      InputClocks.Dram0ClkUser <= Dram0ClkUser_Local;
      wait for 2.500 ns;
    else
      wait until not fiStopSimLocal;
    end if;
  end process;

  Dram1ClkUser_ClockGenerator: 
  process
  begin
    if not fiStopSimLocal then
      Dram1ClkUser_Local <= not Dram1ClkUser_Local;
      InputClocks.Dram1ClkUser <= Dram1ClkUser_Local;
      wait for 2.500 ns;
    else
      wait until not fiStopSimLocal;
    end if;
  end process;


  DiagramClocks.RioClk40 <= InputClocks.RioClk40;
  DiagramClocks.PxiClk10Fpga <= InputClocks.PxiClk10Fpga;
  DiagramClocks.DramClkDiv100 <= InputClocks.DramClkDiv100;
  DiagramClocks.DramClk200 <= InputClocks.DramClk200;

  NiFpgaSimInterfaceTopx: entity work.NiFpgaSimChinchInterfaceTop (struct)
    generic map (
      kFpgaEndpoint => kFpgaEndpoint,
      kNumDmaOperations => 8,      kDmaChannelConfigArray => kDmaChannelConfigArray,
      kBusClkPwDivFactor    => kBusClkPwDivFactor)
    port map (
      fiClock                 => fiClock,
      fiHostToTargetInterface => fiHostToTargetInterface,
      fiTargetToHostInterface => fiTargetToHostInterface,
      fiHostToTargetReady     => fiHostToTargetReady,
      fiTargetToHostReady     => fiTargetToHostReady,
      fiFifoInterrupt					=> fiFifoInterrupt,
      aResetOut => aReset,
      bResetOut => bReset,
      BusClkOut => BusClk,
      bIoPort2InputTx => bIoPort2InputTx,
      bIoPort2InputRx => bIoPort2InputRx,
      bIoPort2OutputTx => bIoPort2OutputTx,
      bIoPort2OutputRx => bIoPort2OutputRx
       );

  LvFpgaSim796xTop_Instance: entity work.LvFpgaSim796xTop
    port map (
      aResetOut => aReset,
      bResetOut => bReset,
      BusClkOut => BusClk,
      bIoPort2InputTx => bIoPort2InputTx,
      bIoPort2InputRx => bIoPort2InputRx,
      bIoPort2OutputTx => bIoPort2OutputTx,
      bIoPort2OutputRx => bIoPort2OutputRx,       
      aInterrupt  => aInterrupt,
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
      rLvFpgaIoModPresent => rLvFpgaIoModPresent,
      rLvFpgaIoModPowerGd => rLvFpgaIoModPowerGd,
      rLvFpgaInsertedIoModId => rLvFpgaInsertedIoModId,
      rLvFpgaPcbTemp => rLvFpgaPcbTemp,
      aUserGpio => aUserGpio,
      aUserGpio_n => aUserGpio_n,
      UserGClkLvds => UserGClkLvds,
      UserGClkLvds_n => UserGClkLvds_n,
      UserGClkLvttl => UserGClkLvttl,
      InputClocks       => InputClocks,
      DiagramClocks     => DiagramClocks);

   

end simulation;

