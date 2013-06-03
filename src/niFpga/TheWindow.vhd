-- © 2012 National Instruments Corporation.
--Use of this file is subject to the Software License Agreement provided with 
--LabVIEW FPGA Module and, without limiting any of the provisions in that license, 
--modifying or distributing this file is prohibited.

--Adding to, deleting from, or otherwise modifying the contents of this file 
--may cause the software to malfunction.
Library ieee;
use ieee.Std_Logic_1164.all;
use ieee.numeric_std.all;

Library work;
use work.PkgNiUtilities.all;
use work.PkgCommIntConfiguration.all;
use work.PkgCommunicationInterface.all;
use work.PkgChinchCommunicationInterface.all;
use work.PkgDmaFifos.all;


Library UNISIM;
use UNISIM.vcomponents.all;


entity TheWindow is
   port(
      ChinchClk :  in std_logic;
ReliableClkIn :  in std_logic;
RioClk40 :  in std_logic;
PxiClk10Fpga :  in std_logic;
DramClkDiv100 :  in std_logic;
DramClk200 :  in std_logic;
DStarAClk :  in std_logic;
LvFpgaIoModClipClock0 :  in std_logic;
LvFpgaIoModClipClock1 :  in std_logic;
Dram0ClkUser :  in std_logic;
Dram1ClkUser :  in std_logic;
DiagramClocks : out work.PkgNiFpgaSimulationModel.tDiagramClocks;


      

      aBusReset : in std_logic;
      bBusReset : in std_logic;

          aLvTrig0 : inout std_logic;
    aLvTrig1 : inout std_logic;
    aLvTrig2 : inout std_logic;
    aLvTrig3 : inout std_logic;
    aLvTrig4 : inout std_logic;
    aLvTrig5 : inout std_logic;
    aLvTrig6 : inout std_logic;
    aLvTrig7 : inout std_logic;
    aPxi10Dio : in std_logic;
    StarClkpin : in std_logic;
    aPxieDStarB : in std_logic;
    aPxieDStarC : out std_logic;
    aPxieSync100 : in std_logic;
    rLvFpgaIoModPresent : in std_logic;
    rLvFpgaIoModPowerGd : in std_logic;
    rLvFpgaIoModPowerEn : in std_logic;
    rLvFpgaIoModVeepromEn : in std_logic;
    rLvFpgaIoModIoEn : in std_logic;
    rExpectedIoModId : in  std_logic_vector(31 downto 0);
    rLvFpgaInsertedIoModId : in  std_logic_vector(31 downto 0);
    rLvFpgaPcbTemp : in  std_logic_vector(15 downto 0);
    bClk100PllUnlockedSticky : in std_logic;


      

      

          aUserGpio : inout std_logic_vector(65 downto 0);
    aUserGpio_n : inout std_logic_vector(65 downto 0);
    IoModClipClock0 : out std_logic;
    IoModClipClock1 : out std_logic;
    rClkToSocket : in std_logic;
    rIoModGpioEn : in std_logic;
    rLvFpgaAckI2cBus : in std_logic;
    rLvFpgaI2cAck : in std_logic;
    rLvFpgaI2cDone : in std_logic;
    rLvFpgaI2cGo : out std_logic;
    rLvFpgaI2cRd : out std_logic;
    rLvFpgaI2cRdData : in std_logic_vector(7 downto 0);
    rLvFpgaI2cStart : out std_logic;
    rLvFpgaI2cStop : out std_logic;
    rLvFpgaI2cWtData : out std_logic_vector(7 downto 0);
    rLvFpgaReqI2cBus : out std_logic;
    UserGClkLvds : in std_logic;
    UserGClkLvds_n : in std_logic;
    UserGClkLvttl : in std_logic;
    dDram0AddrFifoAddr : out std_logic_vector(30 downto 0);
    dDram0AddrFifoCmd : out std_logic_vector(2 downto 0);
    dDram0AddrFifoFull : in std_logic;
    dDram0AddrFifoWrEn : out std_logic;
    dDram0PhyInitDone : in std_logic;
    dDram0RdDataValid : in std_logic;
    dDram0RdFifoDataOut : in std_logic_vector(63 downto 0);
    dDram0WrFifoDataIn : out std_logic_vector(63 downto 0);
    dDram0WrFifoFull : in std_logic;
    dDram0WrFifoMaskData : out std_logic_vector(7 downto 0);
    dDram0WrFifoWrEn : out std_logic;
    dDram1AddrFifoAddr : out std_logic_vector(30 downto 0);
    dDram1AddrFifoCmd : out std_logic_vector(2 downto 0);
    dDram1AddrFifoFull : in std_logic;
    dDram1AddrFifoWrEn : out std_logic;
    dDram1PhyInitDone : in std_logic;
    dDram1RdDataValid : in std_logic;
    dDram1RdFifoDataOut : in std_logic_vector(63 downto 0);
    dDram1WrFifoDataIn : out std_logic_vector(63 downto 0);
    dDram1WrFifoFull : in std_logic;
    dDram1WrFifoMaskData : out std_logic_vector(7 downto 0);
    dDram1WrFifoWrEn : out std_logic;


      bCommunicationTimeout : in boolean;

      bRegPortOut : out RegPortOut_t;
      bRegPortIn : in RegPortIn_t;

      bInputStreamInterfaceToFifo : in InputStreamInterfaceToFifoArray_t
        (Larger(kNumberOfDmaChannels,1)-1 downto 0);
      bInputStreamInterfaceFromFifo : out InputStreamInterfaceFromFifoArray_t
        (Larger(kNumberOfDmaChannels,1)-1 downto 0);
      bOutputStreamInterfaceToFifo : in OutputStreamInterfaceToFifoArray_t
        (Larger(kNumberOfDmaChannels,1)-1 downto 0);
      bOutputStreamInterfaceFromFifo : out OutputStreamInterfaceFromFifoArray_t
        (Larger(kNumberOfDmaChannels,1)-1 downto 0);
      bIrqToInterface : out IrqToInterfaceArray_t(Larger(kNumberOfIrqs,1)-1 downto 0);

      TopLevelClkOut : out std_logic;
      tDiagramActive : out std_logic;

      ReliableClkOut : out std_logic;
      rDiagramReset  : out std_logic;
      aDiagramReset  : out std_logic;
      rDerivedClockLostLockError : out std_logic;
      rBaseClksValid : in std_logic := '1';
      rGatedBaseClksValid : in std_logic := '1';
      aSafeToEnableGatedClks : out std_logic
   );
   attribute syn_maxfan : integer;
   
end TheWindow;

architecture behavioral of TheWindow is

   
   
   
   signal aLvTrig0_dout : std_logic_vector(0 downto 0);
signal aLvTrig0_EnableOutput : std_logic_vector(0 downto 0);
signal local_aLvTrig0_driver : std_logic_vector(0 downto 0);
signal aLvTrig1_dout : std_logic_vector(0 downto 0);
signal aLvTrig1_EnableOutput : std_logic_vector(0 downto 0);
signal local_aLvTrig1_driver : std_logic_vector(0 downto 0);
signal aLvTrig2_dout : std_logic_vector(0 downto 0);
signal aLvTrig2_EnableOutput : std_logic_vector(0 downto 0);
signal local_aLvTrig2_driver : std_logic_vector(0 downto 0);
signal aLvTrig3_dout : std_logic_vector(0 downto 0);
signal aLvTrig3_EnableOutput : std_logic_vector(0 downto 0);
signal local_aLvTrig3_driver : std_logic_vector(0 downto 0);
signal aLvTrig4_dout : std_logic_vector(0 downto 0);
signal aLvTrig4_EnableOutput : std_logic_vector(0 downto 0);
signal local_aLvTrig4_driver : std_logic_vector(0 downto 0);
signal aLvTrig5_dout : std_logic_vector(0 downto 0);
signal aLvTrig5_EnableOutput : std_logic_vector(0 downto 0);
signal local_aLvTrig5_driver : std_logic_vector(0 downto 0);
signal aLvTrig6_dout : std_logic_vector(0 downto 0);
signal aLvTrig6_EnableOutput : std_logic_vector(0 downto 0);
signal local_aLvTrig6_driver : std_logic_vector(0 downto 0);
signal aLvTrig7_dout : std_logic_vector(0 downto 0);
signal aLvTrig7_EnableOutput : std_logic_vector(0 downto 0);
signal local_aLvTrig7_driver : std_logic_vector(0 downto 0);
  signal internal_aPxi10Dio_din: std_logic_vector(0 downto 0);
  signal internal_StarClkpin_din: std_logic_vector(0 downto 0);
  signal internal_aPxieDStarB_din: std_logic_vector(0 downto 0);
  signal internal_aPxieDStarC_dout : std_logic_vector(0 downto 0);
  signal internal_aPxieSync100_din: std_logic_vector(0 downto 0);
  signal internal_rLvFpgaIoModPresent_din: std_logic_vector(0 downto 0);
  signal internal_rLvFpgaIoModPowerGd_din: std_logic_vector(0 downto 0);
  signal internal_rLvFpgaIoModPowerEn_din: std_logic_vector(0 downto 0);
  signal internal_rLvFpgaIoModVeepromEn_din: std_logic_vector(0 downto 0);
  signal internal_rLvFpgaIoModIoEn_din: std_logic_vector(0 downto 0);
  signal internal_rExpectedIoModId_din:  std_logic_vector(31 downto 0);
  signal internal_rLvFpgaInsertedIoModId_din:  std_logic_vector(31 downto 0);
  signal internal_rLvFpgaPcbTemp_din:  std_logic_vector(15 downto 0);
  signal internal_bClk100PllUnlockedSticky_din: std_logic_vector(0 downto 0);

   
   
   

           signal ReliableClkInThruBuf : std_logic;
        signal ChinchClkThruBuf : std_logic;
        signal RioClk40ThruBuf : std_logic;
        signal ReliableClk : std_logic;
        signal TopLvClk : std_logic;


   
   
   
   signal tDiagramEnableOut, tDiagramEnableIn, tDiagramEnableClear :  std_logic;
   signal rDiagramResetStatus : std_logic;
   signal rDiagramResetLoc, aDiagramResetLoc : std_logic;
   signal aSafeToEnableGatedClksLoc : std_logic;
   signal rGatedClkIsRunning : std_logic;
   signal rInternalClksValid : std_logic;
   signal rEnableClksForViRun : std_logic;
   signal rGatedBaseClkStartupErr : std_logic;
   signal rDiagramResetAssertionErr : std_logic;
   signal rGatedBaseClksValidLoc : std_logic;

   
   
   
   signal rDerivedClkStartupErr : std_logic;
   signal rDcmPllSourceClksValid : std_logic;
   signal rDerivedClksValid : std_logic;

   signal bIoWtToEnSafeBusCrossing : std_logic;

  
   signal bRegPortOutFlat : std_logic_vector(33 downto 0);
   signal bRegPortInFlat : std_logic_vector(kAlignedAddressWidth + 33 downto 0);



   signal bInputStreamInterfaceToFifoFlat : std_logic_vector(InputStreamInterfaceToFifoFlat_t'length*
     Larger(kNumberOfDmaChannels,1)-1 downto 0);
   signal bInputStreamInterfaceFromFifoFlat : std_logic_vector(InputStreamInterfaceFromFifoFlat_t'length*
     Larger(kNumberOfDmaChannels,1)-1 downto 0);
   signal bOutputStreamInterfaceToFifoFlat : std_logic_vector(OutputStreamInterfaceToFifoFlat_t'length*
     Larger(kNumberOfDmaChannels,1)-1 downto 0);
   signal bOutputStreamInterfaceFromFifoFlat : std_logic_vector(OutputStreamInterfaceFromFifoFlat_t'length*
     Larger(kNumberOfDmaChannels,1)-1 downto 0);
  signal bIrqToInterfaceFlat : std_logic_vector(66*Larger(kNumberOfIrqs,1)-1
    downto 0);

begin


   
   
   
   generateaLvTrig0 : 
   for i in 0 downto 0 generate
      local_aLvTrig0_driver(i) <= aLvTrig0_dout(i) 
         when aLvTrig0_EnableOutput(i) = '1'
         else 'Z';
end generate;
aLvTrig0 <= local_aLvTrig0_driver(0);
aLvTrig0_dout <= (others => '0');
aLvTrig0_EnableOutput <= (others => '0');
generateaLvTrig1 : 
   for i in 0 downto 0 generate
      local_aLvTrig1_driver(i) <= aLvTrig1_dout(i) 
         when aLvTrig1_EnableOutput(i) = '1'
         else 'Z';
end generate;
aLvTrig1 <= local_aLvTrig1_driver(0);
aLvTrig1_dout <= (others => '0');
aLvTrig1_EnableOutput <= (others => '0');
generateaLvTrig2 : 
   for i in 0 downto 0 generate
      local_aLvTrig2_driver(i) <= aLvTrig2_dout(i) 
         when aLvTrig2_EnableOutput(i) = '1'
         else 'Z';
end generate;
aLvTrig2 <= local_aLvTrig2_driver(0);
aLvTrig2_dout <= (others => '0');
aLvTrig2_EnableOutput <= (others => '0');
generateaLvTrig3 : 
   for i in 0 downto 0 generate
      local_aLvTrig3_driver(i) <= aLvTrig3_dout(i) 
         when aLvTrig3_EnableOutput(i) = '1'
         else 'Z';
end generate;
aLvTrig3 <= local_aLvTrig3_driver(0);
aLvTrig3_dout <= (others => '0');
aLvTrig3_EnableOutput <= (others => '0');
generateaLvTrig4 : 
   for i in 0 downto 0 generate
      local_aLvTrig4_driver(i) <= aLvTrig4_dout(i) 
         when aLvTrig4_EnableOutput(i) = '1'
         else 'Z';
end generate;
aLvTrig4 <= local_aLvTrig4_driver(0);
aLvTrig4_dout <= (others => '0');
aLvTrig4_EnableOutput <= (others => '0');
generateaLvTrig5 : 
   for i in 0 downto 0 generate
      local_aLvTrig5_driver(i) <= aLvTrig5_dout(i) 
         when aLvTrig5_EnableOutput(i) = '1'
         else 'Z';
end generate;
aLvTrig5 <= local_aLvTrig5_driver(0);
aLvTrig5_dout <= (others => '0');
aLvTrig5_EnableOutput <= (others => '0');
generateaLvTrig6 : 
   for i in 0 downto 0 generate
      local_aLvTrig6_driver(i) <= aLvTrig6_dout(i) 
         when aLvTrig6_EnableOutput(i) = '1'
         else 'Z';
end generate;
aLvTrig6 <= local_aLvTrig6_driver(0);
aLvTrig6_dout <= (others => '0');
aLvTrig6_EnableOutput <= (others => '0');
generateaLvTrig7 : 
   for i in 0 downto 0 generate
      local_aLvTrig7_driver(i) <= aLvTrig7_dout(i) 
         when aLvTrig7_EnableOutput(i) = '1'
         else 'Z';
end generate;
aLvTrig7 <= local_aLvTrig7_driver(0);
aLvTrig7_dout <= (others => '0');
aLvTrig7_EnableOutput <= (others => '0');
  internal_aPxi10Dio_din(0) <= to_X01(aPxi10Dio);
  internal_StarClkpin_din(0) <= to_X01(StarClkpin);
  internal_aPxieDStarB_din(0) <= to_X01(aPxieDStarB);
      aPxieDStarC <= internal_aPxieDStarC_dout(0);
      internal_aPxieDStarC_dout (0) <= '0';
  internal_aPxieSync100_din(0) <= to_X01(aPxieSync100);
  internal_rLvFpgaIoModPresent_din(0) <= to_X01(rLvFpgaIoModPresent);
  internal_rLvFpgaIoModPowerGd_din(0) <= to_X01(rLvFpgaIoModPowerGd);
  internal_rLvFpgaIoModPowerEn_din(0) <= to_X01(rLvFpgaIoModPowerEn);
  internal_rLvFpgaIoModVeepromEn_din(0) <= to_X01(rLvFpgaIoModVeepromEn);
  internal_rLvFpgaIoModIoEn_din(0) <= to_X01(rLvFpgaIoModIoEn);
  internal_rExpectedIoModId_din <= to_X01(rExpectedIoModId);
  internal_rLvFpgaInsertedIoModId_din <= to_X01(rLvFpgaInsertedIoModId);
  internal_rLvFpgaPcbTemp_din <= to_X01(rLvFpgaPcbTemp);
  internal_bClk100PllUnlockedSticky_din(0) <= to_X01(bClk100PllUnlockedSticky);

   

     TopLvClk <= RioClk40;
  ReliableClk <= ReliableClkIn;
        rDerivedClksValid <=  '1';
   rDerivedClkStartupErr <= '0';

ReliableClkInThruBuf <= ReliableClkIn;
ChinchClkThruBuf <= ChinchClk;
RioClk40ThruBuf <= RioClk40;
   rGatedBaseClksValidLoc <= rGatedBaseClksValid;
   rGatedBaseClkStartupErr <= '0';


   
   DiagramClocks.RioClk40 <= 'Z';
  DiagramClocks.PxiClk10Fpga <= 'Z';
  DiagramClocks.DramClkDiv100 <= 'Z';
  DiagramClocks.DramClk200 <= 'Z';

   
   
   
   
   

   
   
   
   

      bRegPortOut <= BuildRegPortOut(bRegPortOutFlat);
   bRegPortInFlat <= to_StdLogicVector(bRegPortIn);

   bInputStreamInterfaceToFifoFlat <= StreamInterfaceArrayToVector(
     FlattenStreamInterface(bInputStreamInterfaceToFifo));
   bOutputStreamInterfaceToFifoFlat <= StreamInterfaceArrayToVector(
     FlattenStreamInterface(bOutputStreamInterfaceToFifo));

   bInputStreamInterfaceFromFifo <= UnflattenStreamInterface(StreamInterfaceVectorToArray(
    ArraySize => bInputStreamInterfaceFromFifo'length,
    Vector => bInputStreamInterfaceFromFifoFlat));
   bOutputStreamInterfaceFromFifo <= UnflattenStreamInterface(StreamInterfaceVectorToArray(
    ArraySize => bOutputStreamInterfaceFromFifo'length,
    Vector => bOutputStreamInterfaceFromFifoFlat));
   bIrqToInterface <= BuildIrqToInterfaceArray(bIrqToInterfaceFlat);

   
   
   
   theVI : entity work.NiFpgaAG_DMA_Example_dash_dash_Target_VI (vhdl_labview)
      port map(
         
         
         

         		   ReliableClkIn => ReliableClkIn,
		   ChinchClk => ChinchClk,
		   RioClk40 => RioClk40,


         

         TopLvClk       => TopLvClk, 

         enable_in                  =>  tDiagramEnableIn ,
         enable_out                 =>  tDiagramEnableOut,
         enable_clr                 =>  tDiagramEnableClear,
         tDiagramEnableIn           =>  tDiagramEnableIn ,
         tDiagramEnableOut          =>  tDiagramEnableOut,
         tDiagramEnableClear        =>  tDiagramEnableClear,
         bCommunicationTimeoutIn    =>  to_StdLogic(bCommunicationTimeout),

         rDiagramResetOut           =>  rDiagramResetLoc,
         rDiagramResetIn            =>  rDiagramResetLoc,
     
         aDiagramResetOut           =>  aDiagramResetLoc,
         reset                      =>  aDiagramResetLoc,

         aBusReset                  =>  aBusReset,
         bBusReset                  =>  bBusReset,
         bIoWtToEnSafeBusCrossing   =>  '1',

         bRegPortOut => bRegPortOutFlat,
         bRegPortIn => bRegPortInFlat,

         bInputStreamInterfaceToFifo => bInputStreamInterfaceToFifoFlat,
         bInputStreamInterfaceFromFifo => bInputStreamInterfaceFromFifoFlat,
         bOutputStreamInterfaceToFifo => bOutputStreamInterfaceToFifoFlat,
         bOutputStreamInterfaceFromFifo => bOutputStreamInterfaceFromFifoFlat,
         bIrqToInterface => bIrqToInterfaceFlat,

         rBaseClksValid             =>  rBaseClksValid,
         rGatedBaseClksValid          => rGatedBaseClksValidLoc,
         rSafeToEnableGatedClks       => aSafeToEnableGatedClksLoc,
         rGatedBaseClkStartupErr      => rGatedBaseClkStartupErr,
         rDcmPllSourceClksValidOut    => rDcmPllSourceClksValid,
         rDerivedClksValid            => rDerivedClksValid,
         rDerivedClockLostLockError   => rDerivedClockLostLockError,
         rDiagramResetStatusIn        => rDiagramResetStatus,
         rDiagramResetStatusOut       => rDiagramResetStatus,
         rInternalClksValidIn         => rInternalClksValid,
         rInternalClksValidOut        => rInternalClksValid,
         rEnableClksForViRunIn        => rEnableClksForViRun,
         rEnableClksForViRunOut       => rEnableClksForViRun,
         rDerivedClkStartupErr        => rDerivedClkStartupErr,
         rDiagramResetAssertionErrIn  => rDiagramResetAssertionErr,
         rDiagramResetAssertionErrOut => rDiagramResetAssertionErr
      );


   
   
   
   

   rDiagramReset <= rDiagramResetLoc;
   aDiagramReset <= aDiagramResetLoc;
   ReliableClkOut <= ReliableClk;
   TopLevelClkOut <= TopLvClk;
   aSafeToEnableGatedClks <= aSafeToEnableGatedClksLoc;


   
   
   tDiagramActive <= tDiagramEnableIn and not tDiagramEnableOut;

end architecture behavioral;


