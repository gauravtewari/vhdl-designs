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
use work.PkgNiFpgaSimInterfaceTypes.all;
use work.PkgNiFpgaSimInterfaceLvDataTypes.all;
use work.PkgNiFpgaSimCommonProcedures.all;
use work.PkgNiFpgaSimCommonProceduresPrivate.all;
use work.PkgNiFpgaSimFifoProceduresPrivate.all;
use work.PkgNiFpgaSimFifoProceduresInterfaceSpecificPrivate.all;
use work.PkgNiFpgaSimFifoProceduresCommonPrivate.all;
use work.PkgNiFpgaSimIrqProceduresPrivate.all;
use work.PkgNiFpgaSimMiscProceduresPrivate.all;
use work.PkgNiFpgaSimLvInterface.all;
use work.PkgNiUtilities.all;
use work.PkgRegister.all;
use work.PkgNiFpgaIrqRegisters.all;
use work.PkgNiFpgaSimDma.all;

entity NiFpgaSimLvInterface is
  port (
    
    
    
    FiClock : in std_logic;
    fiStopSim : out boolean;
    fiHostToTargetInterface : out tFiInterface;
    
    
    fiTargetToHostInterface : in tFiInterface;
    
    
    fiHostToTargetReady : in boolean;
    fiTargetToHostReady : out boolean;
    
    
    aInterrupt : in boolean;
    
    lvStopSim : in std_logic := '0';
    lvCommand : in natural;
    lvAddress : in natural;
    lvWrData  : in std_logic_vector(tData'range);
    lvRdData  : out std_logic_vector(tData'range);
    lvTransactionIdReq : in  natural;
    lvTransactionIdResp : out natural;
    lvNumOfDataPhases : in natural;
    lvTimeout : in integer;
    lvTimedOut : out std_logic;
    lvFifoRemaining : out natural;
    
    
    lvAttribute : in integer;
    lvRequestPush : in std_logic;
    lvRequestReady : out std_logic;
    lvRdDataValid : out std_logic;
    lvResponsePop : in std_logic
    );
end entity NiFpgaSimLvInterface;

architecture behave of NiFpgaSimLvInterface is
  type tState is (InReset, Idle, PerformRequest, Error);
  signal lvState : tState := InReset;
  
  signal fiHostToTargetInterfaceLoc : tFiInterface;
  
  signal fiTargetToHostReadyLoc : boolean;
begin

  fiStopSim <= lvStopSim='1';
  fiHostToTargetInterface <= fiHostToTargetInterfaceLoc;
  fiTargetToHostReady <= fiTargetToHostReadyLoc ;

  LvInterfaceStateProcess:
  process
    
    variable Address : natural;
    
    
    
    
    variable WrData, RdData : tData;
    variable RdDataU32 : tU32;
    variable FifoWrData, FifoRdData : tDataArray(0 to 2**tNumOfDataPhases'length-1);
    variable Command : natural;
    
    variable AttributeVal : unsigned(31 downto 0);
    
    variable TransactionIdReq, TransactionIdResp : tTransactionID;
    
    variable NumOfDataPhases : natural;
    
    variable fiErrorStatusLoc : tErrorStatus;
    
    variable Timeout : integer;
    variable TimedOut : boolean;
    
    variable FifoRemaining : natural;
    variable enabled : boolean;
    variable FifoCount : unsigned(31 downto 0);
    
    variable BufferDepth : natural;

    
    
    procedure ReturnRdData(ret : tData) is
    begin
          
          lvRdData <= ret;
          lvRdDataValid <= '1';
          
          
          if lvResponsePop /= '1' then
            wait until lvResponsePop = '1';
          end if;
          
          lvRdDataValid <= '0';
          
          wait until lvResponsePop = '0';
    end procedure ReturnRdData;


  begin
    case lvState is
      when InReset =>
        
        Address := 0;
        Command := kNoCommand;
        WrData  := (others => '0');
        RdData  := (others => '0');
        AttributeVal := (others => '0');
        TransactionIdReq := (others => '0');
        TransactionIdResp := (others => '0');
        NumOfDataPhases := 0;
        enabled := false;
        FifoCount := (others => '0');

        lvFifoRemaining <= 0;
        lvRdData <= (others => '0');
        lvTimedOut <= '0';
        
        lvRequestReady <= '0';
        
        lvRdDataValid <= '0';
        if not fiHostToTargetReady then
          wait until fiHostToTargetReady;
        end if;
        lvState <= Idle;

      when Idle =>

        lvFifoRemaining <= 0;
        lvRdData <= (others => '0');
        lvTransactionIdResp <= 0;
        lvTimedOut <= '0';

        lvRequestReady <= '1';
        if lvRequestPush /= '1' then
          wait until lvRequestPush='1';
        end if;

        
        Address := lvAddress;
        Command := lvCommand;
        WrData  := tData(lvWrData);
        AttributeVal := to_unsigned(lvAttribute,32);
        assert lvTransactionIdReq < 2**(tTransactionId'length)
          report "[NiFpgaSimLvInterface]:  Value of TransactionId exceeds the field.  Must be less than "
            & integer'image(2**(tTransactionId'length))
          severity error;
        TransactionIdReq := std_logic_vector(to_unsigned(lvTransactionIdReq,tTransactionId'length));
        TransactionIdResp := (others => '0');
        assert lvNumOfDataPhases < 2**(tNumOfDataPhases'length)
          report "[NiFpgaSimLvInterface]:  Value of NumOfDataPhases exceeds the field.  Must be less than "
            & integer'image(2**(tNumOfDataPhases'length))
          severity error;
        NumOfDataPhases := lvNumOfDataPhases;
        Timeout := lvTimeout;
        enabled := false;
        FifoCount := (others => '0');

        
        lvRequestReady <= '0';
        
        if lvRequestPush /= '0' then
          wait until lvRequestPush='0';
        end if;
        
        lvState <= PerformRequest;


      when PerformRequest =>
        case Command is

          when kWriteCommand =>
            NiFpga_Write(
              Address => Address,
              TransactionId => TransactionIdReq,
              Data => WrData,
              Command => kWrite,
              FiClock => FiClock,
              fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
              fiHostToTargetReady => fiHostToTargetReady,
              fiErrorStatusIn => fiErrorStatusLoc,
              fiErrorStatusOut => fiErrorStatusLoc);

          when kReadCommand =>
            NiFpga_Read(
              kViControl => kViControl,
              Address => Address,
              Data => RdData,
              Command => kRead,
              FiClock => FiClock,
              fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
              fiTargetToHostInterface => fiTargetToHostInterface,
              fiHostToTargetReady => fiHostToTargetReady,
              fiTargetToHostReady => fiTargetToHostReadyLoc,
              TransactionIdReq => TransactionIdReq,
              TransactionIdResp => TransactionIdResp,
              fiErrorStatusIn => fiErrorStatusLoc,
              fiErrorStatusOut => fiErrorStatusLoc);

            lvTransactionIdResp <= to_integer(unsigned(TransactionIdResp));
            ReturnRdData(RdData);

          when kOpenViCommand =>
            NiFpga_Open
              (attr => AttributeVal,
              kNiFpga_OpenAttribute_NoRun => kNiFpga_OpenAttribute_NoRun,
              kNiFpga_RunAttribute_WaitUntilDone => kNiFpga_RunAttribute_WaitUntilDone,
              kViControl => kViControl,
              FiClock => FiClock,
              fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
              fiTargetToHostInterface => fiTargetToHostInterface,
              fiHostToTargetReady => fiHostToTargetReady,
              fiTargetToHostReady =>  fiTargetToHostReadyLoc,
              TransactionIdReq => TransactionIdReq,
              TransactionIdResp => TransactionIdResp,
              fiErrorStatusIn => fiErrorStatusLoc,
              fiErrorStatusOut => fiErrorStatusLoc);

              
              lvTransactionIdResp <= to_integer(unsigned(TransactionIdResp));

          when kRunCommand =>
            NiFpga_Run
              (attr => AttributeVal,
              kViControl => kViControl,
              kNiFpga_RunAttribute_WaitUntilDone => kNiFpga_RunAttribute_WaitUntilDone,
              FiClock => FiClock,
              fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
              fiTargetToHostInterface => fiTargetToHostInterface,
              fiHostToTargetReady => fiHostToTargetReady,
              fiTargetToHostReady =>  fiTargetToHostReadyLoc,
              TransactionIdReq => TransactionIdReq,
              TransactionIdResp => TransactionIdResp,
              fiErrorStatusIn => fiErrorStatusLoc,
              fiErrorStatusOut => fiErrorStatusLoc);

              
              lvTransactionIdResp <= to_integer(unsigned(TransactionIdResp));

          when kCloseViCommand =>
            NiFpga_Close
             (attr => AttributeVal,
              kNiFpga_CloseAttribute_NoResetIfLastSession => kNiFpga_CloseAttribute_NoResetIfLastSession,
              kDiagramReset => kDiagramReset,
              kViControl => kViControl,
              FiClock => FiClock,
              fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
              fiTargetToHostInterface => fiTargetToHostInterface,
              fiHostToTargetReady => fiHostToTargetReady,
              fiTargetToHostReady =>  fiTargetToHostReadyLoc,
              TransactionIdReq => TransactionIdReq,
              TransactionIdResp => TransactionIdResp,
              fiErrorStatusIn => fiErrorStatusLoc,
              fiErrorStatusOut => fiErrorStatusLoc);

              
              lvTransactionIdResp <= to_integer(unsigned(TransactionIdResp));


          when kFifoWriteCommand | kFifoWriteMoveDataCommand =>
            FifoWrData(0) := WrData;
            for i in 1 to NumOfDataPhases-1 loop 
              lvRequestReady <= '1';
              if lvRequestPush /= '1' then
                wait until lvRequestPush='1';
              end if;
              FifoWrData(i) := tData(lvWrData);
              
              lvRequestReady <= '0';
              wait until lvRequestPush /= '1';
            end loop;

            case Command is
              when kFifoWriteCommand =>
                NiFpga_WriteFifo
                  (kViControl => kViControl,
                  fifo => Address,
                  Data => FifoWrData(0 to NumOfDataPhases-1),
                  timeout => Timeout,
                  TimedOut => TimedOut,
                  remaining => FifoRemaining,
                  FiClock => FiClock,
                  fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
                  fiTargetToHostInterface => fiTargetToHostInterface,
                  fiHostToTargetReady => fiHostToTargetReady,
                  fiTargetToHostReady => fiTargetToHostReadyLoc,
                  TransactionIdReq => TransactionIdReq,
                  TransactionIdResp => TransactionIdResp,
                  fiErrorStatusIn => fiErrorStatusLoc,
                  fiErrorStatusOut => fiErrorStatusLoc);

                  
                  RdData := (others => '0');
                  ReturnRdData(RdData);

              when kFifoWriteMoveDataCommand =>
                NiFpga_WriteFifoData(
                  Fifo => Address,
                  Data => FifoWrData(0 to NumOfDataPhases-1),
                  FiClock => FiClock,
                  fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
                  fiHostToTargetReady => fiHostToTargetReady,
                  fiErrorStatusIn => fiErrorStatusLoc,
                  fiErrorStatusOut => fiErrorStatusLoc);
                TimedOut := false;
              when others => null;

            end case;

            lvTimedOut <= to_StdLogic(TimedOut);
            lvFifoRemaining <= FifoRemaining;
            lvTransactionIdResp <= to_integer(unsigned(TransactionIdResp));


          when kFifoWriteCompletion =>

            NiFpga_WriteFifoPostInterfaceSpecific
              (kViControl => kViControl,
              fifo => Address,
              DataWords => NumOfDataPhases,
              FiClock => FiClock,
              fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
              fiTargetToHostInterface => fiTargetToHostInterface,
              fiHostToTargetReady => fiHostToTargetReady,
              fiTargetToHostReady => fiTargetToHostReadyLoc,
              TransactionIdReq => TransactionIdReq,
              TransactionIdResp => TransactionIdResp,
              fiErrorStatusIn => fiErrorStatusLoc,
              fiErrorStatusOut => fiErrorStatusLoc);

      NiFpga_FifoStatus(
              kViControl => kViControl,
              FifoNumber => Address,
              enabled => enabled,
              FifoCount => FifoCount,
              BufferDepth => BufferDepth,
              FiClock => FiClock,
              fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
              fiTargetToHostInterface => fiTargetToHostInterface,
              fiHostToTargetReady => fiHostToTargetReady,
              fiTargetToHostReady => fiTargetToHostReadyLoc,
              TransactionidReq => TransactionIdReq,
              TransactionIdResp => TransactionIdResp,
              fiErrorStatusIn => fiErrorStatusLoc,
              fiErrorStatusOut => fiErrorStatusLoc);

            lvFifoRemaining <= to_integer(unsigned(FifoCount));
            lvTransactionIdResp <= to_integer(unsigned(TransactionIdResp));
            
            
      ReturnRdData(std_logic_vector(FifoCount));

          when kFifoReadCommand | kFifoReadMoveDataCommand =>

            TimedOut := false;

            case Command is
              when kFifoReadCommand =>
                NiFpga_ReadFifo
                  (kViControl => kViControl,
                  fifo => Address,
                  Data => FifoRdData(0 to NumOfDataPhases-1),
                  timeout => Timeout,
                  TimedOut => TimedOut,
                  remaining => FifoRemaining,
                  FiClock => FiClock,
                  fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
                  fiTargetToHostInterface => fiTargetToHostInterface,
                  fiHostToTargetReady => fiHostToTargetReady,
                  fiTargetToHostReady => fiTargetToHostReadyLoc,
                  TransactionIdReq => TransactionIdReq,
                  TransactionIdResp => TransactionIdResp,
                  fiErrorStatusIn => fiErrorStatusLoc,
                  fiErrorStatusOut => fiErrorStatusLoc);

              when kFifoReadMoveDataCommand =>

                NiFpga_ReadFifoData(
                  Fifo => Address,
                  Data => FifoRdData(0 to NumOfDataPhases-1),
                  TransactionIdReq => TransactionIdReq,
                  TransactionIdResp => TransactionIdResp,
                  FiClock => FiClock,
                  fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
                  fiTargetToHostInterface => fiTargetToHostInterface,
                  fiHostToTargetReady => fiHostToTargetReady,
                  fiTargetToHostReady => fiTargetToHostReadyLoc,
                  fiErrorStatusIn => fiErrorStatusLoc,
                  fiErrorStatusOut => fiErrorStatusLoc);

                when others => null;

              end case;

            lvTimedOut <= to_StdLogic(TimedOut);
            lvFifoRemaining <= FifoRemaining;
            lvTransactionIdResp <= to_integer(unsigned(TransactionIdResp));
            for i in 0 to NumOfDataPhases-1 loop
              ReturnRdData(FifoRdData(i));
            end loop;

          when kFifoReadCompletion =>

            NiFpga_ReadFifoDataResponseCompletion
              (kViControl => kViControl,
              FifoNumber => Address,
              CountValue => GetTransferCountValue(Address,NumOfDataPhases),
              kCountValueRegOffset => kDmaTransferCountRegOffset,
              remaining => FifoRemaining,
              FiClock => FiClock,
              fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
              fiTargetToHostInterface => fiTargetToHostInterface,
              fiHostToTargetReady => fiHostToTargetReady,
              fiTargetToHostReady => fiTargetToHostReadyLoc,
              TransactionIdReq => TransactionIdReq,
              TransactionIdResp => TransactionIdResp,
              fiErrorStatusIn => fiErrorStatusLoc,
              fiErrorStatusOut => fiErrorStatusLoc);

            lvFifoRemaining <= FifoRemaining;
            lvTransactionIdResp <= to_integer(unsigned(TransactionIdResp));
            
            
      ReturnRdData(std_logic_vector(to_unsigned(FifoRemaining,tData'length)));

          when kFifoRequest =>

            NiFpga_FifoRequest (
              kViControl => kViControl,
              FifoNumber => Address,
              DirectionRead => to_Boolean(WrData(0)), 
              DataLength => NumOfDataPhases,
              FiClock => FiClock,
              fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
              fiTargetToHostInterface => fiTargetToHostInterface,
              fiHostToTargetReady => fiHostToTargetReady,
              fiTargetToHostReady => fiTargetToHostReadyLoc,
              TransactionidReq => TransactionIdReq,
              TransactionIdResp => TransactionIdResp,
              fiErrorStatusIn => fiErrorStatusLoc,
              fiErrorStatusOut => fiErrorStatusLoc
            );
            lvTransactionIdResp <= to_integer(unsigned(TransactionIdResp));

          when kGetFifoStatus =>
            NiFpga_FifoStatus(
              kViControl => kViControl,
              FifoNumber => Address,
              enabled => enabled,
              FifoCount => FifoCount,
              BufferDepth => BufferDepth,
              FiClock => FiClock,
              fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
              fiTargetToHostInterface => fiTargetToHostInterface,
              fiHostToTargetReady => fiHostToTargetReady,
              fiTargetToHostReady => fiTargetToHostReadyLoc,
              TransactionidReq => TransactionIdReq,
              TransactionIdResp => TransactionIdResp,
              fiErrorStatusIn => fiErrorStatusLoc,
              fiErrorStatusOut => fiErrorStatusLoc
            );

            
            lvTransactionIdResp <= to_integer(unsigned(TransactionIdResp));
      ReturnRdData(std_logic_vector(FifoCount));

          when kFifoStart =>
            StartFifo
              (kViControl => kViControl,
              fifo => Address,
              FiClock => FiClock,
              fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
              fiHostToTargetReady => fiHostToTargetReady,
              fiTargetToHostInterface => fiTargetToHostInterface,
              fiTargetToHostReady => fiTargetToHostReadyLoc,
              TransactionidReq => TransactionIdReq,
              TransactionIdResp => TransactionIdResp,
              fiErrorStatusIn => fiErrorStatusLoc,
              fiErrorStatusOut => fiErrorStatusLoc);

              
              lvTransactionIdResp <= to_integer(unsigned(TransactionIdResp));

          when kFifoConfigure =>
            NiFpga_ConfigureFifo
              (kViControl => kViControl,
              FifoNumber => Address,
              Depth => to_integer(unsigned(WrData)),
              FiClock => FiClock,
              fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
              fiHostToTargetReady => fiHostToTargetReady,
              fiTargetToHostInterface => fiTargetToHostInterface,
              fiTargetToHostReady => fiTargetToHostReadyLoc,
              fiErrorStatusIn => fiErrorStatusLoc,
              fiErrorStatusOut => fiErrorStatusLoc);

          when kFifoStop =>

            StopFifo
              (kViControl => kViControl,
              fifo => Address,
              FiClock => FiClock,
              fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
              fiHostToTargetReady => fiHostToTargetReady,
              fiTargetToHostInterface => fiTargetToHostInterface,
              fiTargetToHostReady => fiTargetToHostReadyLoc,
              fiErrorStatusIn => fiErrorStatusLoc,
              fiErrorStatusOut => fiErrorStatusLoc);

          when kWaitOnIrq =>

            NiFpga_WaitOnIrq
                (kViControl => kViControl,
                Irqs => unsigned(WrData),
                Timeout => Timeout,
                TimedOut => TimedOut,
                IrqsAsserted => RdDataU32,
                FiClock => FiClock,
                fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
                fiTargetToHostInterface => fiTargetToHostInterface,
                fiHostToTargetReady => fiHostToTargetReady,
                fiTargetToHostReady => fiTargetToHostReadyLoc,
                aInterrupt => aInterrupt,
                TransactionIdReq => TransactionIdReq,
                TransactionIdResp => TransactionIdResp,
                fiErrorStatusIn => fiErrorStatusLoc,
                fiErrorStatusOut => fiErrorStatusLoc);

            lvTransactionIdResp <= to_integer(unsigned(TransactionIdResp));
            lvTimedOut <= to_StdLogic(TimedOut);
            RdData := ReturnDataType(RdDataU32);
            ReturnRdData(RdData);

          when kAcknowledgeIrq =>

            NiFpga_AcknowledgeIrq
              (IrqsToAcknowledge => unsigned(WrData),
              FiClock => FiClock,
              fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
              fiHostToTargetReady => fiHostToTargetReady,
              fiErrorStatusIn => fiErrorStatusLoc,
              fiErrorStatusOut => fiErrorStatusLoc);

          when kEnableIrqs =>

            
            
      
      
      
      
      
            NiFpga_Write(
              Address => (kMaskRegOffset*4),
              Data => WrData,
              Command => kSetIrqMask,
              FiClock => FiClock,
              fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
              fiHostToTargetReady => fiHostToTargetReady,
              fiErrorStatusIn => fiErrorStatusLoc,
              fiErrorStatusOut => fiErrorStatusLoc
              );

          when kReadIrqStatus =>
           
            NiFpga_Read(
              kViControl => kViControl,
              Address => (kStatusRegOffset*4),
              Data => RdData,
              TransactionIdReq => TransactionIdReq,
              TransactionIdResp => TransactionIdResp,
              FiClock => FiClock,
              fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
              fiTargetToHostInterface => fiTargetToHostInterface,
              fiHostToTargetReady => fiHostToTargetReady,
              fiTargetToHostReady => fiTargetToHostReadyLoc,
              fiErrorStatusIn => fiErrorStatusLoc,
              fiErrorStatusOut => fiErrorStatusLoc);

            lvTransactionIdResp <= to_integer(unsigned(TransactionIdResp));
            ReturnRdData(RdData);

          when kFifoConfigureInterrupt =>
            NiFpga_Write(
              Address => Address,
              Command => kFifoConfigureInterruptWrite,
              TransactionId => TransactionIdReq,
              Data => WrData,
              FiClock => FiClock,
              fiHostToTargetInterface => fiHostToTargetInterfaceLoc,
              fiHostToTargetReady => fiHostToTargetReady,
              fiErrorStatusIn => fiErrorStatusLoc,
              fiErrorStatusOut => fiErrorStatusLoc);

          when others =>
            assert false
              report "[NiFpgaSimLvInterface]: Command not supported."
              severity error;

        end case;

        lvState <= Idle;
      when others => NULL;
    end case;

    wait for 0 ns; 

  end process LvInterfaceStateProcess;


end architecture behave;
