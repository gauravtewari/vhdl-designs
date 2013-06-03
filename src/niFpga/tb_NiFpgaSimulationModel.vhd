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
--* FPGA Host Interface Procedures:                                                       *
--*                                                                                       *
--* Refer to the following packages to implement FPGA Host Interface functions to         *
--* interact with the simulated FPGA VI:                                                  *
--*   * Controls and indicators:  nifpga/PkgNiFpgaSimControlAndIndicatorProcedures.vhd    *
--*   * DMA FIFOs: nifpga/PkgNiFpgaSimFifoProcedures.vhd                                  *
--*   * Interrupts:  nifpga/PkgNiFpgaSimIrqProcedures.vhd                                 *
--*   * Remaining calls (such as Open, Run, and Close):                                   *
--*         nifpga/PkgNiFpgaSimMiscProcedures.vhd                                         *
--*   * LabVIEW data type declarations to use for declaring variables used for            *
--*         FIFOs, controls and indicators:  nifpga/PkgNiFpgaSimInterfaceLvDataTypes.vhd  *
--*                                                                                       *
--* Note:  Not all FPGA targets support DMA communication and interrupts.  To find out    *
--* whether a target supports DMA and interrupts access the FPGA Target Properties        *
--* dialog box and locate the Target Information section.                                 *
--*                                                                                       *
--* Register Offsets (Address input to procedure calls):                                  *
--* Refer to nifpga/PkgRegister.vhd for register offsets for controls and indicators.     *
--*                                                                                       *
--* DMA FIFO Numbers:                                                                     *
--* Refer to nifpga/PkgNiFgpaSimulationModel.vhd for FIFO numbers for DMA FIFOs.          *
--*                                                                                       *
--* Special Types:                                                                        *
--* Refer to nifpga/PkgNiFpgaSimUserTypes.vhd for type definitions for complex data       *
--* types.  Complex data types are clusters, arrays, enumerated types, and fixed-point    *
--* types.  The package contains an overloaded NiFpga_Read and NiFpga_Write for types     *
--* used in controls and indicators. The package also includes overloaded NiFpga_ReadFifo *
--* and NiFpga_WriteFifo procedures for types used for DMA.                               *
--*                                                                                       *
--* Fixed-Point Functionality:                                                            *
--* Refer to nifpga/PkgNiFpgaSimFxpArithmetic.vhd for some basic arithmetic operations    *
--* for any fixed-point types defined in PkgNiFpgaSimUserTypes.  Also refer to            *
--* nifpga/PkgNiFpgaSimFxp.vhd for some other miscellaneous fixed-point definitions.      *
--*                                                                                       *
--* Signal Descriptions                                                                   *
--* -------------------         														  *
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

entity tb_NiFpgaSimulationModel is 
end entity tb_NiFpgaSimulationModel;

architecture testbench of tb_NiFpgaSimulationModel is

  signal DiagramClocks: tDiagramClocks ;
  signal FiClock: std_logic := '0';
  signal fiStopSim : boolean := false;
  
  signal fiHostToTargetInterface: tFiInterface;
  signal fiHostToTargetReady: boolean;
  signal fiTargetToHostInterface: tFiInterface;
  signal fiTargetToHostReady: boolean;
  signal aInterrupt : boolean;
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
  -- Stimulus Block
  -- The MainStimulusProcess represents the Host VI for the FPGA target
  -- application VI.  Translate the Host VI into the VHDL procedures described
  -- in the header at the beginning of the file and using the example code below.
  --===========================================================================
  MainStimulusBlock: block
    
    --HostInterfaceStatus string ----------------------------------------------
    --This string can be used to report the host interface stimulus process
    --progress in the waveform and in the simulation console
    constant kTStringLength : integer := 50;
    -- This function resizes input string Arg into a string of length kTStringLength
    function Rsz (Arg : in string) return string is
     alias LclArg : string(1 to Arg'length) is Arg;
     variable ReturnVal : string(1 to kTStringLength) := (others => ' ');
    begin
     for i in 1 to Smaller(Arg'length, ReturnVal'length) loop
       ReturnVal(i) := LclArg(i);
     end loop;
     return ReturnVal;
    end Rsz;
    signal HostInterfaceStatus : string(1 to kTStringLength) := Rsz("Idle");

    procedure PrintHostInterfaceStatus(Arg: in string; signal HostInterfaceStatus : out string) is
    begin
        HostInterfaceStatus <= Rsz(Arg);
        report(Arg);      
    end PrintHostInterfaceStatus;

  begin

    MainStimulusProcess:process
      variable fiErrorStatus : tErrorStatus := (Status => false, Code => 0); 

        --=====================================================================
        --DMA FIFO Data Variables 
        -----------------------------------------------------------------------
        -- These variables should be used along with the NiFpga_FifoWrite
        -- and NiFpga_FifoRead commands to guarantee the correct data type
        -- read or write operation is performed. Please refer to the 
        -- auto-generated file PkgNiFpgaSimulationModel.vhd in the nifpga
        -- directory for DMA FIFO definitions.  A default size of 500 is
        -- given for arrays and should be changed as needed for your testbench.
        -----------------------------------------------------------------------
        -- FIFO Name: DMA FIFO
        variable DMA_FIFO_Data: tI16_Array(255 downto 0);
		variable remaining : natural;
        --=====================================================================
        begin

        PrintHostInterfaceStatus("Opening",HostInterfaceStatus);
        NiFpga_Open (attr => (kNiFpga_OpenAttribute_NoRun => '1', others => '0'),
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady =>  fiTargetToHostReady,
          fiErrorStatusIn => fiErrorStatus,
          fiErrorStatusOut => fiErrorStatus);

		-- Configure DMA
	    NiFpga_ConfigureFifo (fifo => kDMA_FIFO_ChannelNumber,
		  depth => 1024,
		  FiClock => FiClock,
	      fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady =>  fiTargetToHostReady,
          fiErrorStatusIn => fiErrorStatus,
          fiErrorStatusOut => fiErrorStatus);
		
		-- Start DMA
	    NiFpga_StartFifo (fifo => kDMA_FIFO_ChannelNumber,
		  FiClock => FiClock,
	      fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady =>  fiTargetToHostReady,
          fiErrorStatusIn => fiErrorStatus,
          fiErrorStatusOut => fiErrorStatus);	
		  
        PrintHostInterfaceStatus("Running",HostInterfaceStatus); 
        NiFpga_Run (attr => (kNiFpga_RunAttribute_WaitUntilDone => '1', others => '0'),
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady =>  fiTargetToHostReady,
          fiErrorStatusIn => fiErrorStatus,
          fiErrorStatusOut => fiErrorStatus);

		-- DMA Read
		NiFpga_ReadFifo (
            fifo => kDMA_FIFO_ChannelNumber,
            Data => DMA_FIFO_Data,
            timeout => 5000, --change the timeout appropriately
            remaining => remaining, --declare "variable remaining : natural;"
            FiClock => FiClock,
            fiHostToTargetInterface => fiHostToTargetInterface,
            fiTargetToHostInterface => fiTargetToHostInterface,
            fiHostToTargetReady => fiHostToTargetReady,
            fiTargetToHostReady =>  fiTargetToHostReady,
            fiErrorStatusIn => fiErrorStatus,
            fiErrorStatusOut => fiErrorStatus);
		  
        --*********************************************************************
        --
        -- INSERT HOST INTERFACE PROCEDURES HERE
        --
        --  For more info on Host Interface procedures, see the heading above.
        --
        --  This is an example of how to read an indicator:
        --  NiFpga_Read (
        --    Address => <constant in PkgRegister k[IndicatorName]_ind_X>,
        --    Data => <variable [IndicatorName]_ind_X_Data>,
        --    FiClock => FiClock,
        --    fiHostToTargetInterface => fiHostToTargetInterface,
        --    fiTargetToHostInterface => fiTargetToHostInterface,
        --    fiHostToTargetReady => fiHostToTargetReady,
        --    fiTargetToHostReady =>  fiTargetToHostReady,
        --    fiErrorStatusIn => fiErrorStatus,
        --    fiErrorStatusOut => fiErrorStatus);
        --
        --  This is an example of how to write a control:
        --  NiFpga_Write (
        --    Address => <constant in PkgRegister k[ControlName]_ctl_X>,
        --    Data => <variable [ControlName]_ctl_X_Data>,
        --    FiClock => FiClock,
        --    fiHostToTargetInterface => fiHostToTargetInterface,
        --    fiHostToTargetReady => fiHostToTargetReady,
        --    fiErrorStatusIn => fiErrorStatus,
        --    fiErrorStatusOut => fiErrorStatus);
        --
        --  This is an example of how to read a Target To Host DMA FIFO:
        --  NiFpga_ReadFifo (
        --    fifo => <constant in PkgNiFpgaSimulationModel k[FifoName]_ChannelNumber>,
        --    Data => <variable [FifoName]_Data>,
        --    timeout => -1, --change the timeout appropriately
        --    remaining => remaining, --declare "variable remaining : natural;"
        --    FiClock => FiClock,
        --    fiHostToTargetInterface => fiHostToTargetInterface,
        --    fiTargetToHostInterface => fiTargetToHostInterface,
        --    fiHostToTargetReady => fiHostToTargetReady,
        --    fiTargetToHostReady =>  fiTargetToHostReady,
        --    fiErrorStatusIn => fiErrorStatus,
        --    fiErrorStatusOut => fiErrorStatus);
        --
        --  This is an example of how to write a Host to Target DMA FIFO
        --  NiFpga_WriteFifo (
        --    fifo => <constant in PkgNiFpgaSimulationModel k[FifoName]_ChannelNumber>,
        --    Data => <variable [FifoName]_Data>,
        --    timeout => -1, --change the timeout appropriately
        --    remaining => remaining, --declare "variable remaining : natural;"
        --    FiClock => FiClock,
        --    fiHostToTargetInterface => fiHostToTargetInterface,
        --    fiTargetToHostInterface => fiTargetToHostInterface,
        --    fiHostToTargetReady => fiHostToTargetReady,
        --    fiTargetToHostReady =>  fiTargetToHostReady,
        --    fiErrorStatusIn => fiErrorStatus,
        --    fiErrorStatusOut => fiErrorStatus);
        --*********************************************************************

        PrintHostInterfaceStatus("Closing",HostInterfaceStatus); 
        NiFpga_Close (
          FiClock => FiClock,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady =>  fiTargetToHostReady,
          fiErrorStatusIn => fiErrorStatus,
          fiErrorStatusOut => fiErrorStatus);

        PrintHostInterfaceStatus("Idle",HostInterfaceStatus); 
        fiStopSim <= true;
        wait;

      end process MainStimulusProcess;

  end block MainStimulusBlock;


  --===========================================================================
  -- Simulation Model
  --===========================================================================

  NiFpgaSimulationModel_Instance: entity niFpga.NiFpgaSimulationModel
    port map (
      FiClock => FiClock,      
      fiHostToTargetInterface => fiHostToTargetInterface,
      fiTargetToHostInterface => fiTargetToHostInterface,
      fiHostToTargetReady => fiHostToTargetReady,
      fiTargetToHostReady => fiTargetToHostReady,
      aInterrupt => aInterrupt,
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
