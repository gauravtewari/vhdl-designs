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
  use work.PkgNiFpgaSimulationModel.all;
  use work.PkgCommunicationInterface.all;
  use work.PkgSwitchedChinch.all;   
  use work.PkgChinchCommunicationInterface.all;
  use work.PkgCommIntConfiguration.all; 
  use work.PkgDmaFifos.all; 
  use work.PkgLvFpgaConst.all;
  use work.PkgTbMgr.all;
  
library UNISIM; 
  use UNISIM.vcomponents.all; 
  use UNISIM.all; 
 
entity LvFpgaSim796xTop is
  port (
    
    aResetOut: in boolean;
    bResetOut: in boolean;
    BusClkOut: in std_logic; 
    aInterrupt: out boolean;
  
    
    bIoPort2InputTx  : out SwitchedLinkTx_t;
    bIoPort2InputRx  : in  SwitchedLinkRx_t;
    bIoPort2OutputTx : in  SwitchedLinkTx_t;
    bIoPort2OutputRx : out SwitchedLinkRx_t;
  
    
    InputClocks: in tInputClocks;
    DiagramClocks: out tDiagramClocks;
    aPxieSync100  : in std_logic;  
    StarClkPin    : in std_logic;  
    DStarAClk     : in std_logic;
    aPxieDStarB   : in std_logic;
    aPxieDStarC   : out std_logic;
    aLvTrig0 : inout std_logic;
    aLvTrig1 : inout std_logic;
    aLvTrig2 : inout std_logic;
    aLvTrig3 : inout std_logic;
    aLvTrig4 : inout std_logic;
    aLvTrig5 : inout std_logic;
    aLvTrig6 : inout std_logic;
    aLvTrig7 : inout std_logic;

    
    aUserGpio        : inout std_logic_vector(65 downto 0);
    aUserGpio_n      : inout std_logic_vector(65 downto 0);
    UserGClkLvds     : in    std_logic;
    UserGClkLvds_n   : in    std_logic;
    UserGClkLvttl    : in    std_logic;
    
    
    
    rLvFpgaIoModPresent : in std_logic;
    rLvFpgaIoModPowerGd : in std_logic; 
    rLvFpgaInsertedIoModId : in std_logic_vector(31 downto 0); 
    
    
    rLvFpgaPcbTemp  : in std_logic_vector(15 downto 0)      
  ); 
end LvFpgaSim796xTop ; 
 
architecture rtl of LvFpgaSim796xTop is 
  
  component BUFG 
    port ( 
      I : in std_logic; 
      O : out std_logic); 
  end component;
  
  
  signal bDiagramReset: std_logic;
  signal aReset: boolean;
  signal bBusReset: boolean := false;
  signal bBusReset_ms: boolean := false; 
  signal bBaseClksValid: std_logic; 
  signal SlowBusClk: std_logic;
  signal slowBusClkLcl:std_logic:='0';
  signal SlowBusClkTmp: std_logic:='0'; 
  signal bSlowClkCnt: integer range 0 to 1 := 1;
  
  
  constant kNumClientDmaPorts : natural := 0; 
  constant kClientDmaChannelConfiguration : DmaChannelConfArray_t(
      kNumClientDmaPorts-1 downto 0) := (others => kDmaChannelConfigurationZero); 
  signal bInputStreamInterfaceFromFifo: InputStreamInterfaceFromFifoArray_t 
    (Larger(kNumberOfDmaChannels,1)-1 downto 0); 
  signal bInputStreamInterfaceToFifo: InputStreamInterfaceToFifoArray_t 
    (Larger(kNumberOfDmaChannels,1)-1 downto 0); 
  signal bOutputStreamInterfaceFromFifo: OutputStreamInterfaceFromFifoArray_t 
    (Larger(kNumberOfDmaChannels,1)-1 downto 0); 
  signal bOutputStreamInterfaceToFifo: OutputStreamInterfaceToFifoArray_t 
    (Larger(kNumberOfDmaChannels,1)-1 downto 0); 
  signal bClientInputStreamInterfaceFromFifo: InputStreamInterfaceFromFifoArray_t 
    (Larger(kNumClientDmaPorts,1)-1 downto 0);
  signal bClientOutputStreamInterfaceFromFifo: OutputStreamInterfaceFromFifoArray_t 
    (Larger(kNumClientDmaPorts,1)-1 downto 0); 
  signal bIrq: std_logic_vector(kNumberOfIrqs-1 downto 0); 
  signal bLvFpgaIrq: IrqToInterfaceArray_t(Larger(kNumberOfIrqs,1)-1 downto 0);
  signal bRegPortIn: RegPortIn_t; 
  signal bRegPortOut: RegPortOut_t; 
  signal bTimeout: boolean; 
  
  
  signal bLvFpgaRegPortOut: RegPortOut_t;
  
  
  signal aPll200Locked: std_logic;
  signal bPll200Locked: std_logic;
  signal bPll200UnlockedSticky: std_logic;
  
  
  signal dDram0WrFifoFull     : std_logic;
  signal dDram0AddrFifoFull   : std_logic;
  signal dDram0RdDataValid    : std_logic;
  signal dDram0WrFifoWrEn     : std_logic;
  signal dDram0AddrFifoWrEn   : std_logic;
  signal dDram0AddrFifoAddr   : std_logic_vector(30 downto 0);
  signal dDram0AddrFifoCmd    : std_logic_vector(2 downto 0);
  signal dDram0RdFifoDataOut  : std_logic_vector(63 downto 0);
  signal dDram0WrFifoDataIn   : std_logic_vector(63 downto 0);
  signal dDram0WrFifoMaskData : std_logic_vector(7 downto 0);
  signal dDram0PhyInitDone    : std_logic;
  signal Dram0ClkUser         : std_logic;
  
  
  signal dDram1WrFifoFull     : std_logic;
  signal dDram1AddrFifoFull   : std_logic;
  signal dDram1RdDataValid    : std_logic;
  signal dDram1WrFifoWrEn     : std_logic;
  signal dDram1AddrFifoWrEn   : std_logic;
  signal dDram1AddrFifoAddr   : std_logic_vector(30 downto 0);
  signal dDram1AddrFifoCmd    : std_logic_vector(2 downto 0);
  signal dDram1RdFifoDataOut  : std_logic_vector(63 downto 0);
  signal dDram1WrFifoDataIn   : std_logic_vector(63 downto 0);
  signal dDram1WrFifoMaskData : std_logic_vector(7 downto 0);
  signal dDram1PhyInitDone    : std_logic;
  signal Dram1ClkUser         : std_logic;
  
  
  constant kFoundDumbTb: std_logic := '0';
  signal IoModClipClock0: std_logic;
  signal IoModClipClock1: std_logic;
  signal LvFpgaIoModClipClock0: std_logic;
  signal LvFpgaIoModClipClock1: std_logic;
  signal rLvFpgaInsertedTbId: std_logic_vector(31 downto 0);
  signal rLvFpgaPcbTempOut: std_logic_vector(15 downto 0);
  signal rLvFpgaTbIoEn: std_logic;
  signal rLvFpgaTbPowerEn: std_logic;
  signal rLvFpgaTbPowerGd: std_logic;
  signal rLvFpgaTbPresent: std_logic;
  signal rLvFpgaTbVeepromEn: std_logic;
  signal mPcbTempValueI16: std_logic_vector(15 downto 0);
  signal mTbStatus: TbStatus_t;
  signal mExpectedTbId: std_logic_vector(31 downto 0):=kExpectedTbId;
  signal mExpectedDumbTb: std_logic:=kExpectDumbTb;
  
  signal TopLevelClkOut: std_logic;
  signal rDerivedClockLostLockError: std_logic;

begin

  aReset <=aResetOut;
  
  
  SynchronizeBusReset: process(BusClkOut)
  begin 
    if rising_edge(BusClkOut) then
      bBusReset_ms <= aReset; 
      bBusReset <= bBusReset_ms; 
    end if; 
  end process;

  
  
  
  
  LvFpgaIoModClipClock0 <= IoModClipClock0;
  LvFpgaIoModClipClock1 <= IoModClipClock1;

  TheWindowX: entity work.TheWindow (behavioral) 
    port map ( 
      RioClk40                       => InputClocks.RioClk40,  
      ChinchClk                      => BusClkOut,
      PxiClk10Fpga                   => InputClocks.PxiClk10Fpga,
      DramClkDiv100                  => InputClocks.DramClkDiv100,         
      DramClk200                     => InputClocks.DramClk200,
      DStarAClk                      => DStarAClk,
      LvFpgaIoModClipClock0          => LvFpgaIoModClipClock0, 
      LvFpgaIoModClipClock1          => LvFpgaIoModClipClock1,
      rDiagramReset                  => bDiagramReset, 
      aBusReset                      => to_StdLogic(aReset), 
      bBusReset                      => to_StdLogic(bBusReset),  
      bIrqToInterface                => bLvFpgaIrq, 
      bRegPortOut                    => bLvFPGARegPortOut, 
      bRegPortIn                     => bRegPortIn,
      bInputStreamInterfaceToFifo    => bInputStreamInterfaceToFifo, 
      bInputStreamInterfaceFromFifo  => bInputStreamInterfaceFromFifo, 
      bOutputStreamInterfaceToFifo   => bOutputStreamInterfaceToFifo,
      bOutputStreamInterfaceFromFifo => bOutputStreamInterfaceFromFifo,
      bCommunicationTimeout          => bTimeout, 
      dDram0AddrFifoAddr             => dDram0AddrFifoAddr,    
      dDram0AddrFifoCmd              => dDram0AddrFifoCmd,      
      dDram0AddrFifoFull             => dDram0AddrFifoFull,    
      dDram0AddrFifoWrEn             => dDram0AddrFifoWrEn,     
      dDram0PhyInitDone              => dDram0PhyInitDone,      
      dDram0RdDataValid              => dDram0RdDataValid,     
      dDram0RdFifoDataOut            => dDram0RdFifoDataOut,    
      dDram0WrFifoDataIn             => dDram0WrFifoDataIn,     
      dDram0WrFifoFull               => dDram0WrFifoFull,      
      dDram0WrFifoMaskData           => dDram0WrFifoMaskData,   
      dDram0WrFifoWrEn               => dDram0WrFifoWrEn,       
      Dram0ClkUser                   => Dram0ClkUser,           
      dDram1AddrFifoAddr             => dDram1AddrFifoAddr,     
      dDram1AddrFifoCmd              => dDram1AddrFifoCmd,      
      dDram1AddrFifoFull             => dDram1AddrFifoFull,     
      dDram1AddrFifoWrEn             => dDram1AddrFifoWrEn,     
      dDram1PhyInitDone              => dDram1PhyInitDone,      
      dDram1RdDataValid              => dDram1RdDataValid,      
      dDram1RdFifoDataOut            => dDram1RdFifoDataOut,    
      dDram1WrFifoDataIn             => dDram1WrFifoDataIn,     
      dDram1WrFifoFull               => dDram1WrFifoFull,       
      dDram1WrFifoMaskData           => dDram1WrFifoMaskData,   
      dDram1WrFifoWrEn               => dDram1WrFifoWrEn,       
      Dram1ClkUser                   => Dram1ClkUser,
      IoModClipClock0                => IoModClipClock0,        
      IoModClipClock1                => IoModClipClock1,
      rClkToSocket                   => InputClocks.RioClk40,
      rIoModGpioEn                   => rLvFpgaTbIoEn, 
      rLvFpgaReqI2cBus               => open,      
      rLvFpgaAckI2cBus               => '0',      
      rLvFpgaI2cGo                   => open,     
      rLvFpgaI2cStart                => open,     
      rLvFpgaI2cStop                 => open,
      rLvFpgaI2cRd                   => open,    
      rLvFpgaI2cWtData               => open, 
      rLvFpgaI2cDone                 => '1',      
      rLvFpgaI2cAck                  => '1',          
      rLvFpgaI2cRdData               => (others => '0'),      
      aUserGpio                      => aUserGpio,             
      aUserGpio_n                    => aUserGpio_n,            
      UserGClkLvds                   => UserGClkLvds,           
      UserGClkLvds_n                 => UserGClkLvds_n,         
      UserGClkLvttl                  => UserGClkLvttl,
      aLvTrig0                       => aLvTrig0,           
      aLvTrig1                       => aLvTrig1,             
      aLvTrig2                       => aLvTrig2,             
      aLvTrig3                       => aLvTrig3,           
      aLvTrig4                       => aLvTrig4,          
      aLvTrig5                       => aLvTrig5,           
      aLvTrig6                       => aLvTrig6,          
      aLvTrig7                       => aLvTrig7,             
      aPxi10Dio                      => InputClocks.PxiClk10Fpga,          
      StarClkpin                     => StarClkpin,
      aPxieDStarB                    => aPxieDStarB,
      aPxieDStarC                    => aPxieDStarC,
      aPxieSync100                   => aPxieSync100,
      rLvFpgaIoModPresent            => rLvFpgaTbPresent,      
      rLvFpgaIoModPowerGd            => rLvFpgaTbPowerGd,       
      rLvFpgaIoModPowerEn            => rLvFpgaTbPowerEn,      
      rLvFpgaIoModVeepromEn          => rLvFpgaTbVeepromEn,     
      rLvFpgaIoModIoEn               => rLvFpgaTbIoEn,         
      rExpectedIoModId               => kExpectedTbId,         
      rLvFpgaInsertedIoModId         => rLvFpgaInsertedTbId,    
      rLvFpgaPcbTemp                 => rLvFpgaPcbTempOut,
      ReliableClkIn                  => BusClkOut,
      bClk100PllUnlockedSticky       => bPll200UnlockedSticky,
      rBaseClksValid                 => bBaseClksValid, 
      ReliableClkOut                 => open, 
      TopLevelClkOut                 => TopLevelClkOut,
      DiagramClocks                  => DiagramClocks,
      tDiagramActive                 => open,
      rDerivedClockLostLockError     => rDerivedClockLostLockError
   );
  bRegPortOut.Data <= bLvFpgaRegPortOut.Data;  
  bRegPortOut.DataValid <=  bLvFpgaRegPortOut.DataValid;
  bRegPortOut.Ready <=  bLvFpgaRegPortOut.Ready;
  aInterrupt<=to_boolean(bIrq(0)); 
  
  
  
  
  
  SlowBusClkGen: process (BusClkOut)
  begin
    if rising_edge(BusClkOut) then
      if bSlowClkCnt = 0 then
        SlowBusClkLcl <= not SlowBusClkLcl;
        bSlowClkCnt <= 1;
      else
        bSlowClkCnt <= bSlowClkCnt -1;
      end if;
    end if;
  end process;

  SlowBusClk_BUFG : BUFG
    port map (I=> SlowBusClkLcl,
              O=> SlowBusClkTmp);
  SlowBusClk <= SlowBusClkTmp;  
  
  
  
  Startup: Process(aReset, BusClkOut)
  begin  
    if aReset then
      bBaseClksValid<= '0';
      aPll200Locked<='0';
      bPll200UnlockedSticky<='0';
    elsif rising_edge(BusClkOut) then
      aPll200Locked<='1';
      bBaseClksValid<=aPll200Locked;
      bPll200Locked<=bBaseClksValid;
      bPll200UnlockedSticky<='0'; 
    end if;  
  end process;
   
  
  
  
  SimDetectTB: process(SlowBusClk,bDiagramReset)
  begin
    if bDiagramReset='1' then 
      mTbStatus <= kClearedTbStatus;
    elsif rising_edge(SlowBusClk)then
      
      mTbStatus.TbPresent <=rLvFpgaIoModPresent;
      mTbStatus.TbPowerGood <= rLvFpgaIoModPowerGd;
      mTbStatus.TbPwrGdTimeout <= '0'; 
      
      if rLvFpgaIoModPresent='1' then
        
        mTbStatus.TbVeepromEn <= '1';
        mTbStatus.TbInsertedDumb <= kFoundDumbTb;
        mTbStatus.InsertedTbId <=rLvFpgaInsertedIoModId;

        if not(mExpectedTbId=rLvFpgaInsertedIoModId 
                and mExpectedDumbTb=kFoundDumbTb) then
          
          mTbStatus.TbMgrState <=  TbMgrState_ToSlv(TbDead);
          mTbStatus.TbIdMismatch <= '1';
          mTbStatus.TbPowerEn <='0';
          mTbStatus.TbVeepromEn <= '0';
          mTbStatus.TbIoEn <= '0';   
        else
          
          mTbStatus.TbIdMismatch <= '0';
          mTbStatus.TbPowerEn <='1';
          mTbStatus.TbVeepromEn <= '1';
          
          if rLvFpgaIoModPowerGd='1' then
            mTbStatus.TbIoEn <= '1';
            mTbStatus.TbMgrState <=  TbMgrState_ToSlv(TbAlive);
                else
            
            if mTbStatus.TbMgrState = TbMgrState_ToSlv(TbAlive) then
              mTbStatus.TbMgrState <=  TbMgrState_ToSlv(TbDead);
              mTbStatus.TbPowerEn <='0';
              mTbStatus.TbVeepromEn <= '0';
            end if;  
            mTbStatus.TbIoEn <= '0';
          end if;  
        end if;
      else
        mTbStatus.TbMgrState <=  TbMgrState_ToSlv(NoTb);
        mTbStatus.TbIdMismatch <= '0';
        mTbStatus.TbPowerEn <='0';
        mTbStatus.TbVeepromEn <= '0';
        mTbStatus.TbIoEn <= '0';
      end if;
    end if;
  end process SimDetectTB;
 
  SimReadTemperature: process(bDiagramReset, SlowBusClk)
  begin
    if bDiagramReset ='1' then
       mPcbTempValueI16 <= X"0000";
    elsif (rising_edge(SlowBusClk)) then
      
      mPcbTempValueI16  <=rLvFpgaPcbTemp; 
    end if;
  end process SimReadTemperature;
  
  
  rLvFpgaTbPresent  <=  mTbStatus.TbPresent;
  rLvFpgaTbPowerGd  <=  mTbStatus.TbPowerGood;
  rLvFpgaTbPowerEn  <=  mTbStatus.TbPowerEn;
  rLvFpgaTbVeepromEn<=  mTbStatus.TbVeepromEn;
  rLvFpgaTbIoEn     <=  mTbStatus.TbIoEn;       
  rLvFpgaInsertedTbId <= mTbStatus.InsertedTbId;
  rLvFpgaPcbTempOut <= mPcbTempValueI16;
 
  
  
  
  ChinchLvFpgaInterfacex: entity work.ChinchLvFpgaInterface (struct)
    generic map (
      kEndpointNumber                => 3,                               
      kRegisterTimeout               => 1024,                            
      kNumClientDmaPorts             => 0,                               
      kNumClientIrqPorts             => 0,                               
      kClientDmaChannelConfiguration => kClientDmaChannelConfiguration)  
    port map (
      aReset                               => False,                                 
      bReset                               => bBusReset,                             
      BusClk                               => BusClkOut,                             
      bIoPort2InputTx                      => bIoPort2InputTx,                       
      bIoPort2InputRx                      => bIoPort2InputRx,                       
      bIoPort2OutputTx                     => bIoPort2OutputTx,                      
      bIoPort2OutputRx                     => bIoPort2OutputRx,                      
      bInputStreamInterfaceFromFifo        => bInputStreamInterfaceFromFifo,         
      bInputStreamInterfaceToFifo          => bInputStreamInterfaceToFifo,           
      bOutputStreamInterfaceFromFifo       => bOutputStreamInterfaceFromFifo,        
      bOutputStreamInterfaceToFifo         => bOutputStreamInterfaceToFifo,          
      bClientInputStreamInterfaceFromFifo  => bClientInputStreamInterfaceFromFifo,   
      bClientInputStreamInterfaceToFifo    => open,                                  
      bClientOutputStreamInterfaceFromFifo => bClientOutputStreamInterfaceFromFifo,  
      bClientOutputStreamInterfaceToFifo   => open,                                  
      bIrq                                 => bIrq,                                  
      bLvFpgaIrq                           => bLvFpgaIrq,                            
      bClientIrq                           => (others=>kIrqStatusToInterfaceZero),   
      bRegPortIn                           => bRegPortIn,                            
      bRegPortOut                          => bRegPortOut,                           
      bTimeout                             => bTimeout);                             

  bClientOutputStreamInterfaceFromFifo <= (others=>kOutputStreamInterfaceFromFifoZero); 
  bClientInputStreamInterfaceFromFifo <= (others=>kInputStreamInterfaceFromFifoZero); 

  
  
  
  LvFpgaSim796xDramMainx: entity work.LvFpgaSim796xDramMain (Struct)
    generic map (
      InsertBank0Mig => InsertBank0Mig,  
      InsertBank1Mig => InsertBank1Mig)  
    port map (
      DramClk200           => InputClocks.DramClk200,
      DramClkDiv100        => InputClocks.DramClk200,
      aDiagramReset        => bDiagramReset,         
      aPll200Locked        => bPll200Locked,         
      dDram0WrFifoFull     => dDram0WrFifoFull,      
      dDram0AddrFifoFull   => dDram0AddrFifoFull,    
      dDram0RdDataValid    => dDram0RdDataValid,     
      dDram0WrFifoWrEn     => dDram0WrFifoWrEn,      
      dDram0AddrFifoWrEn   => dDram0AddrFifoWrEn,    
      dDram0AddrFifoAddr   => dDram0AddrFifoAddr,    
      dDram0AddrFifoCmd    => dDram0AddrFifoCmd,     
      dDram0RdFifoDataOut  => dDram0RdFifoDataOut,   
      dDram0WrFifoDataIn   => dDram0WrFifoDataIn,    
      dDram0WrFifoMaskData => dDram0WrFifoMaskData,  
      dDram0PhyInitDone    => dDram0PhyInitDone,     
      Dram0ClkUser         => Dram0ClkUser,          
      dDram1WrFifoFull     => dDram1WrFifoFull,      
      dDram1AddrFifoFull   => dDram1AddrFifoFull,    
      dDram1RdDataValid    => dDram1RdDataValid,     
      dDram1WrFifoWrEn     => dDram1WrFifoWrEn,      
      dDram1AddrFifoWrEn   => dDram1AddrFifoWrEn,    
      dDram1AddrFifoAddr   => dDram1AddrFifoAddr,    
      dDram1AddrFifoCmd    => dDram1AddrFifoCmd,     
      dDram1RdFifoDataOut  => dDram1RdFifoDataOut,   
      dDram1WrFifoDataIn   => dDram1WrFifoDataIn,    
      dDram1WrFifoMaskData => dDram1WrFifoMaskData,  
      dDram1PhyInitDone    => dDram1PhyInitDone,     
      Dram1ClkUser         => Dram1ClkUser);         
end rtl; 

