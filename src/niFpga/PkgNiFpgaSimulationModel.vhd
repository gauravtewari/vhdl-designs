-- © 2012 National Instruments Corporation.
--Use of this file is subject to the Software License Agreement provided with 
--LabVIEW FPGA Module and, without limiting any of the provisions in that license, 
--modifying or distributing this file is prohibited.

--Adding to, deleting from, or otherwise modifying the contents of this file 
--may cause the software to malfunction.
library ieee;
use ieee.std_logic_1164.all;
use work.PkgNiFpgaSimDma.all;
use work.PkgCommIntConfiguration.all;

package PkgNiFpgaSimulationModel is

  ---------------------------------------------------------------------------------------
  -- DMA FIFO CONSTANTS
  -- FIFO memories declared in the project for Host to Target DMA or Target to Host DMA
  -- must be accessed by a constant listed below.
  ---------------------------------------------------------------------------------------
  -- FIFO name: DMA FIFO
  constant kDMA_FIFO_ChannelNumber: natural :=  0;


  ---------------------------------------------------------------------------------------
  -- tInputClocks
  -- Includes any input clocks to the simulation model. Use the test bench to drive
  -- these clocks. Because the FPGA VI design determines the frequency of these clocks,
  -- do not modify the frequency manually.
  ---------------------------------------------------------------------------------------
  type tInputClocks is record
    ReliableClkIn : std_logic;
    RioClk40 : std_logic;
    PxiClk10Fpga : std_logic;
    DramClkDiv100 : std_logic;
    DramClk200 : std_logic;
    Dram0ClkUser : std_logic;
    Dram1ClkUser : std_logic;
  end record tInputClocks;

  ---------------------------------------------------------------------------------------
  -- tDiagramClocks
  -- Includes all clocks that the simulation model uses. tDiagramClocks includes:
  -- * clocks in the LabVIEW FPGA VI block diagram
  -- * clocks in CLIP and the IP Integration Node
  ---------------------------------------------------------------------------------------

  type tDiagramClocks is record
    RioClk40 : std_logic;
    PxiClk10Fpga : std_logic;
    DramClkDiv100 : std_logic;
    DramClk200 : std_logic;
    
  end record tDiagramClocks;

  ---------------------------------------------------------------------------------------
  -- Target Specific Constants
  ---------------------------------------------------------------------------------------
  constant kFpgaEndpoint : natural :=  3;
    constant kFpgaTargetSupportsIrqs : boolean := TRUE;


  ---------------------------------------------------------------------------------------
  -- BusClk and fiClock Period
  ---------------------------------------------------------------------------------------
  constant kBusClkPeriod : time := 8 ns;
  constant kFiClockPulseWidth : time := 0.200 ns;
  constant kBusClkPwDivFactor : integer := 10;   
  
  ---------------------------------------------------------------------------------------
  -- Utility Functions
  ---------------------------------------------------------------------------------------
    function GetFifoWidth(ChannelNumber : natural) return natural;
  constant kDmaChannelConfigArray : tDmaChannelConfigArray;


end package PkgNiFpgaSimulationModel;

package body PkgNiFpgaSimulationModel is

    ---------------------------------------------------------------------------------------
  -- GetFifoWidth
  --
  -- Determine the actual width of the data as it is transferred on the bus.  Note that
  -- this could differ from the size it is represented as on the FPGA.
  ---------------------------------------------------------------------------------------
  function GetFifoWidth(ChannelNumber : natural) return natural is
  begin
    
    -- For CHInCh, FXP types are always transferred as 64 bits.
    if kDmaFifoConfArray(ChannelNumber).FxpType then
      return 64;
    end if;
    
    -- If it is not a FXP type, it is rounded up to 8, 16, 32, or 64 bits.
    if kDmaFifoConfArray(ChannelNumber).FifoWidth <= 8 then
      return 8;
    elsif kDmaFifoConfArray(ChannelNumber).FifoWidth <= 16 then
      return 16;
    elsif kDmaFifoConfArray(ChannelNumber).FifoWidth <= 32 then
      return 32;
    else
      return 64;
    end if;
    
  end function GetFifoWidth;

  function ReturnDmaChannelConfigArray return tDmaChannelConfigArray is
    variable ret : tDmaChannelConfigArray(kDmaFifoConfArray'range) := 
      (others => (Mode      => None,
                  FifoWidth => 0));
  begin

    for i in kDmaFifoConfArray'range loop
    
      case kDmaFifoConfArray(i).Mode is
        when Disabled =>  ret(i).Mode := None;
        when NiFpgaTargetToHost | NiCoreTargetToHost =>
          ret(i).Mode := TargetToHost;
        when NiFpgaHostToTarget | NiCoreHostToTarget =>
          ret(i).Mode := HostToTarget;
        when others =>
          ret(i).Mode := None;
      end case;
      
      ret(i).FifoWidth := GetFifoWidth(i);
      
    end loop;
    return ret;
  end function ReturnDmaChannelConfigArray;

  constant kDmaChannelConfigArray : tDmaChannelConfigArray := ReturnDmaChannelConfigArray;


end package body PkgNiFpgaSimulationModel;
