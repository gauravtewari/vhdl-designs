-- © 2012 National Instruments Corporation.
--Use of this file is subject to the Software License Agreement provided with 
--LabVIEW FPGA Module and, without limiting any of the provisions in that license, 
--modifying or distributing this file is prohibited.

--Adding to, deleting from, or otherwise modifying the contents of this file 
--may cause the software to malfunction.
-------------------------------------------------------------------------------
--
-- Purpose:
-- This package contains procedures that the Host Interface test bench can use to
-- directly access DMA FIFOs.
-- Refer to the following packages for the remaining Host Interface procedures:
-- > reading and writing controls (see PkgNiFpgaSimControlAndIndicatorsProcedures.vhd)
-- > related to interrupts (see PkgNiFpgaSimIrqProceduresPrivate.vhd)
-- > related to open, close, etc.(see NiFpgaSimMiscProcedures.vhd)
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.PkgNiUtilities.all;
use work.PkgNiFpgaSimInterfaceTypes.all;
use work.PkgNiFpgaSimInterfaceLvDataTypes.all;
use work.PkgNiFpgaSimFifoProceduresPrivate.all;
use work.PkgNiFpgaSimCommonProceduresPrivate.all;
use work.PkgNiFpgaSimFifoProceduresInterfaceSpecificPrivate.all;
use work.PkgNiFpgaUtilities.all;

--Generated packages
use work.PkgRegister.all;
use work.PkgCommIntConfiguration.all;
use work.PkgNiFpgaSimulationModel.all;

package PkgNiFpgaSimFifoProcedures is

  -----------------------------------------------------------------------------
  -- FIFO PROCEDURES - Declarations
  -----------------------------------------------------------------------------
  procedure NiFpga_StartFifo (fifo : natural;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiHostToTargetReady : in boolean;
     signal fiTargetToHostInterface : in tFiInterface;
     signal fiTargetToHostReady : out boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_StopFifo (fifo : natural;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiHostToTargetReady : in boolean;
     signal fiTargetToHostInterface : in tFiInterface;
     signal fiTargetToHostReady : out boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_ConfigureFifo (fifo : natural;
     depth : natural;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiHostToTargetReady : in boolean;
     signal fiTargetToHostInterface : in tFiInterface;
     signal fiTargetToHostReady : out boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus);

  ---------------------------------------------------------------------------------------
  -- tI64 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ReadFifo (
    fifo : in natural;
    Data : out tI64_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_WriteFifo (
    fifo : in natural;
    Data : in tI64_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);


  ---------------------------------------------------------------------------------------
  -- tI32 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ReadFifo (
    fifo : in natural;
    Data : out tI32_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_WriteFifo (
    fifo : in natural;
    Data : in tI32_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  ---------------------------------------------------------------------------------------
  -- tI16 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ReadFifo (
    fifo : in natural;
    Data : out tI16_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_WriteFifo (
    fifo : in natural;
    Data : in tI16_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);
  ---------------------------------------------------------------------------------------
  -- tI8 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ReadFifo (
    fifo : in natural;
    Data : out tI8_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_WriteFifo (
    fifo : in natural;
    Data : in tI8_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  ---------------------------------------------------------------------------------------
  -- tU64 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ReadFifo (
    fifo : in natural;
    Data : out tU64_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_WriteFifo (
    fifo : in natural;
    Data : in tU64_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);


  ---------------------------------------------------------------------------------------
  -- tU32 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ReadFifo (
    fifo : in natural;
    Data : out tU32_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_WriteFifo (
    fifo : in natural;
    Data : in tU32_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  ---------------------------------------------------------------------------------------
  -- tU16 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ReadFifo (
    fifo : in natural;
    Data : out tU16_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_WriteFifo (
    fifo : in natural;
    Data : in tU16_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);
  ---------------------------------------------------------------------------------------
  -- tU8 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ReadFifo (
    fifo : in natural;
    Data : out tU8_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_WriteFifo (
    fifo : in natural;
    Data : in tU8_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  ---------------------------------------------------------------------------------------
  -- Boolean Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ReadFifo (
    fifo : in natural;
    Data : out tBoolean_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_WriteFifo (
    fifo : in natural;
    Data : in tBoolean_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

end package;
--=============================================================================
-- PACKAGE BODY
--=============================================================================
package body PkgNiFpgaSimFifoProcedures is

  -----------------------------------------------------------------------------
  -- UTILITY FUNCTIONS
  -----------------------------------------------------------------------------
  procedure ReportIncorrectDataError(
    fifo : natural;
    dataType : string
  ) is
  begin
    ReportError
     (ErrorType => ExternalError,
      ShortDescription => "Fifo command called with " & dataType & " data for DMA channel " &
                          integer'image(fifo) & ".  This is the incorrect data type for this FIFO." &
                          "  Please refer to PkgNiFpgaSimulationModel in the nifpga directory to " &
                          "map the channel number to the DMA FIFO in the project.");
  end procedure ReportIncorrectDataError;

  procedure ReportTransactionIdError is
  begin
    ReportError
     (ErrorType => InternalError,
      ShortDescription => "Transaction IDs do not match.");
  end procedure ReportTransactionIdError;

  -----------------------------------------------------------------------------
  --FIFO PROCEDURES - DEFINITIONS
  -----------------------------------------------------------------------------
  procedure NiFpga_StartFifo (fifo : natural;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiHostToTargetReady : in boolean;
     signal fiTargetToHostInterface : in tFiInterface;
     signal fiTargetToHostReady : out boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus
  ) is
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    if not fiErrorStatusLoc.Status then

      StartFifo
        (kViControl => kViControl,
        fifo => fifo,
        FiClock => FiClock,
        fiHostToTargetInterface => fiHostToTargetInterface,
        fiHostToTargetReady => fiHostToTargetReady,
        fiTargetToHostInterface => fiTargetToHostInterface,
        fiTargetToHostReady => fiTargetToHostReady,
        TransactionIdReq => TransactionIdReq,
        TransactionIdResp => TransactionIdResp,
        fiErrorStatusIn => fiErrorStatusLoc,
        fiErrorStatusOut => fiErrorStatusLoc);

      if TransactionIdReq /= TransactionIdResp then
        ReportTransactionIdError;
        fiErrorStatusLoc.Status := true;
      end if;

    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_StartFifo ;
  ---------------------------------------------------------------------------------------
  -- NiFpga_StopFifo
  ---------------------------------------------------------------------------------------
  procedure NiFpga_StopFifo (fifo : natural;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiHostToTargetReady : in boolean;
     signal fiTargetToHostInterface : in tFiInterface;
     signal fiTargetToHostReady : out boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus)      is
  begin

    StopFifo
      (kViControl => kViControl,
      fifo => fifo,
      FiClock => FiClock,
      fiHostToTargetInterface => fiHostToTargetInterface,
      fiHostToTargetReady => fiHostToTargetReady,
      fiTargetToHostInterface => fiTargetToHostInterface,
      fiTargetToHostReady => fiTargetToHostReady,
      fiErrorStatusIn => fiErrorStatusIn,
      fiErrorStatusOut => fiErrorStatusOut);

  end procedure NiFpga_StopFifo;

  ---------------------------------------------------------------------------------------
  -- NiFpga_ConfigureFifo
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ConfigureFifo (fifo : natural;
     depth : natural;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiHostToTargetReady : in boolean;
     signal fiTargetToHostInterface : in tFiInterface;
     signal fiTargetToHostReady : out boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus)
  is
  begin

    NiFpga_ConfigureFifo
      (kViControl => kViControl,
      FifoNumber => fifo,
      Depth => depth,
      FiClock => FiClock,
      fiHostToTargetInterface => fiHostToTargetInterface,
      fiHostToTargetReady => fiHostToTargetReady,
      fiTargetToHostInterface => fiTargetToHostInterface,
      fiTargetToHostReady => fiTargetToHostReady,
      fiErrorStatusIn => fiErrorStatusIn,
      fiErrorStatusOut => fiErrorStatusOut);

  end procedure NiFpga_ConfigureFifo;

  ---------------------------------------------------------------------------------------
  -- tI64 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ReadFifo (
    fifo : in natural;
    Data : out tI64_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus)
    is
    variable ReadData : tDataArray(Data'length*2-1 downto 0);
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable TimedOut : boolean;
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    fiErrorStatusLoc.Status := NiFpga_VerifyFifoNumber(Fifo, fiErrorStatusLoc);

    if not fiErrorStatusLoc.Status then

      if kDmaFifoConfArray(fifo).FifoWidth /= 64 or not kDmaFifoConfArray(fifo).SignedData then
        ReportIncorrectDataError(fifo, "I64");
        fiErrorStatusLoc.Status := true;
      else
        NiFpga_ReadFifo
          (kViControl => kViControl,
          fifo => fifo,
          Data => ReadData,
          timeout => timeout,
          TimedOut => TimedOut,
          remaining => remaining,
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady => fiTargetToHostReady,
          TransactionIdReq => TransactionIdReq,
          TransactionIdResp => TransactionIdResp,
          fiErrorStatusIn => fiErrorStatusLoc,
          fiErrorStatusOut => fiErrorStatusLoc);
        if TransactionIdReq /= TransactionIdResp then
          ReportTransactionIdError;
          fiErrorStatusLoc.Status := true;
        end if;
        Data := ReturnI64_ArrayType(ReadData);
      end if;
    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_ReadFifo;

  procedure NiFpga_WriteFifo (
    fifo : in natural;
    Data : in tI64_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus) is
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable TimedOut : boolean; --ignored for now
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    fiErrorStatusLoc.Status := NiFpga_VerifyFifoNumber(Fifo, fiErrorStatusLoc);

    if not fiErrorStatusLoc.Status then

      if kDmaFifoConfArray(fifo).FifoWidth /= 64 or not kDmaFifoConfArray(fifo).SignedData then
        ReportIncorrectDataError(fifo, "I64");
        fiErrorStatusLoc.Status := true;
      else
        NiFpga_WriteFifo
          (kViControl => kViControl,
          TransactionIdReq => TransactionIdReq,
          TransactionIdResp => TransactionIdResp,
          fifo => fifo,
          Data => ReturnDataArrayType(Data),
          timeout => timeout,
          TimedOut => TimedOut,
          remaining => remaining,
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady => fiTargetToHostReady,
          fiErrorStatusIn => fiErrorStatusLoc,
          fiErrorStatusOut => fiErrorStatusLoc);
        if TransactionIdReq /= TransactionIdResp then
          ReportTransactionIdError;
          fiErrorStatusLoc.Status := true;
        end if;
      end if;

    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_WriteFifo;

  ---------------------------------------------------------------------------------------
  -- tI32 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ReadFifo (
    fifo : in natural;
    Data : out tI32_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus) is
    variable ReadData : tDataArray(Data'range);
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable TimedOut : boolean; --ignored for now
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    fiErrorStatusLoc.Status := NiFpga_VerifyFifoNumber(Fifo, fiErrorStatusLoc);

    if not fiErrorStatusLoc.Status then

      if kDmaFifoConfArray(fifo).FifoWidth /= 32 or not kDmaFifoConfArray(fifo).SignedData then
          ReportIncorrectDataError(fifo, "I32");
        fiErrorStatusLoc.Status := true;
      else
        NiFpga_ReadFifo
          (kViControl => kViControl,
          fifo => fifo,
          Data => ReadData,
          timeout => timeout,
          TimedOut => TimedOut,
          remaining => remaining,
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady => fiTargetToHostReady,
          TransactionIdReq => TransactionIdReq,
          TransactionIdResp => TransactionIdResp,
          fiErrorStatusIn => fiErrorStatusLoc,
          fiErrorStatusOut => fiErrorStatusLoc);
        if TransactionIdReq /= TransactionIdResp then
          ReportTransactionIdError;
          fiErrorStatusLoc.Status := true;
        end if;
        Data := ReturnI32_ArrayType(ReadData);
      end if;
    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_ReadFifo;

  procedure NiFpga_WriteFifo (
    fifo : in natural;
    Data : in tI32_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus) is
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable TimedOut : boolean;
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    fiErrorStatusLoc.Status := NiFpga_VerifyFifoNumber(Fifo, fiErrorStatusLoc);

    if not fiErrorStatusLoc.Status then

      if kDmaFifoConfArray(fifo).FifoWidth /= 32 or not kDmaFifoConfArray(fifo).SignedData then
          ReportIncorrectDataError(fifo, "I32");
        fiErrorStatusLoc.Status := true;
      else
        NiFpga_WriteFifo
          (kViControl => kViControl,
          TransactionIdReq => TransactionIdReq,
          TransactionIdResp => TransactionIdResp,
          fifo => fifo,
          Data => ReturnDataArrayType(Data),
          timeout => timeout,
          TimedOut => TimedOut,
          remaining => remaining,
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady => fiTargetToHostReady,
          fiErrorStatusIn => fiErrorStatusLoc,
          fiErrorStatusOut => fiErrorStatusLoc);
        if TransactionIdReq /= TransactionIdResp then
          ReportTransactionIdError;
          fiErrorStatusLoc.Status := true;
        end if;
      end if;
    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_WriteFifo;

  ---------------------------------------------------------------------------------------
  -- tI16 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ReadFifo (
    fifo : in natural;
    Data : out tI16_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus) is
    variable ReadData : tDataArray(Data'range);
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable TimedOut : boolean;
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    fiErrorStatusLoc.Status := NiFpga_VerifyFifoNumber(Fifo, fiErrorStatusLoc);

    if not fiErrorStatusLoc.Status then

      if kDmaFifoConfArray(fifo).FifoWidth /= 16 or not kDmaFifoConfArray(fifo).SignedData then
        ReportIncorrectDataError(fifo, "I16");
        fiErrorStatusLoc.Status := true;
      else
        NiFpga_ReadFifo
          (kViControl => kViControl,
          fifo => fifo,
          Data => ReadData,
          timeout => timeout,
          TimedOut => TimedOut,
          remaining => remaining,
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady => fiTargetToHostReady,
          TransactionIdReq => TransactionIdReq,
          TransactionIdResp => TransactionIdResp,
          fiErrorStatusIn => fiErrorStatusLoc,
          fiErrorStatusOut => fiErrorStatusLoc);
        if TransactionIdReq /= TransactionIdResp then
          ReportTransactionIdError;
          fiErrorStatusLoc.Status := true;
        end if;
        Data := ReturnI16_ArrayType(ReadData);
      end if;
    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_ReadFifo;

  procedure NiFpga_WriteFifo (
    fifo : in natural;
    Data : in tI16_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus) is
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable TimedOut : boolean;
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    fiErrorStatusLoc.Status := NiFpga_VerifyFifoNumber(Fifo, fiErrorStatusLoc);

    if not fiErrorStatusLoc.Status then
      if kDmaFifoConfArray(fifo).FifoWidth /= 16 or not kDmaFifoConfArray(fifo).SignedData then
          ReportIncorrectDataError(fifo, "I16");
        fiErrorStatusOut.Status := true;
      else
        NiFpga_WriteFifo
          (kViControl => kViControl,
          TransactionIdReq => TransactionIdReq,
          TransactionIdResp => TransactionIdResp,
          fifo => fifo,
          Data => ReturnDataArrayType(Data),
          timeout => timeout,
          TimedOut => TimedOut,
          remaining => remaining,
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady => fiTargetToHostReady,
          fiErrorStatusIn => fiErrorStatusLoc,
          fiErrorStatusOut => fiErrorStatusLoc);
        if TransactionIdReq /= TransactionIdResp then
          ReportTransactionIdError;
          fiErrorStatusLoc.Status := true;
        end if;
      end if;
    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_WriteFifo;

  ---------------------------------------------------------------------------------------
  -- tI8 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ReadFifo (
    fifo : in natural;
    Data : out tI8_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus) is
    variable ReadData : tDataArray(Data'range);
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable TimedOut : boolean;
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    fiErrorStatusLoc.Status := NiFpga_VerifyFifoNumber(Fifo, fiErrorStatusLoc);

    if not fiErrorStatusLoc.Status then

      if kDmaFifoConfArray(fifo).FifoWidth /= 8 or not kDmaFifoConfArray(fifo).SignedData then
          ReportIncorrectDataError(fifo, "I8");
        fiErrorStatusLoc.Status := true;
      else
        NiFpga_ReadFifo
          (kViControl => kViControl,
          fifo => fifo,
          Data => ReadData,
          timeout => timeout,
          TimedOut => TimedOut,
          remaining => remaining,
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady => fiTargetToHostReady,
          TransactionIdReq => TransactionIdReq,
          TransactionIdResp => TransactionIdResp,
          fiErrorStatusIn => fiErrorStatusLoc,
          fiErrorStatusOut => fiErrorStatusLoc);
        if TransactionIdReq /= TransactionIdResp then
          ReportTransactionIdError;
          fiErrorStatusLoc.Status := true;
        end if;
        Data := ReturnI8_ArrayType(ReadData);
      end if;
    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_ReadFifo;

  procedure NiFpga_WriteFifo (
    fifo : in natural;
    Data : in tI8_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus) is
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable TimedOut : boolean;
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    fiErrorStatusLoc.Status := NiFpga_VerifyFifoNumber(Fifo, fiErrorStatusLoc);

    if not fiErrorStatusLoc.Status then
      if kDmaFifoConfArray(fifo).FifoWidth /= 8 or not kDmaFifoConfArray(fifo).SignedData then
          ReportIncorrectDataError(fifo, "I8");
        fiErrorStatusLoc.Status := true;
      else
        NiFpga_WriteFifo
          (kViControl => kViControl,
          TransactionIdReq => TransactionIdReq,
          TransactionIdResp => TransactionIdResp,
          fifo => fifo,
          Data => ReturnDataArrayType(Data),
          timeout => timeout,
          TimedOut => TimedOut,
          remaining => remaining,
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady => fiTargetToHostReady,
          fiErrorStatusIn => fiErrorStatusLoc,
          fiErrorStatusOut => fiErrorStatusLoc);
        if TransactionIdReq /= TransactionIdResp then
          ReportTransactionIdError;
          fiErrorStatusLoc.Status := true;
        end if;
      end if;
    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_WriteFifo;

  ---------------------------------------------------------------------------------------
  -- tU64 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ReadFifo (
    fifo : in natural;
    Data : out tU64_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus) is
    variable ReadData : tDataArray(Data'length*2-1 downto 0);
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable TimedOut : boolean;
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    fiErrorStatusLoc.Status := NiFpga_VerifyFifoNumber(Fifo, fiErrorStatusLoc);

    if not fiErrorStatusLoc.Status then
      if kDmaFifoConfArray(fifo).FifoWidth /= 64 or kDmaFifoConfArray(fifo).SignedData then
          ReportIncorrectDataError(fifo, "U16");
        fiErrorStatusLoc.Status := true;
      else
        NiFpga_ReadFifo
          (kViControl => kViControl,
          fifo => fifo,
          Data => ReadData,
          timeout => timeout,
          TimedOut => TimedOut,
          remaining => remaining,
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady => fiTargetToHostReady,
          TransactionIdReq => TransactionIdReq,
          TransactionIdResp => TransactionIdResp,
          fiErrorStatusIn => fiErrorStatusLoc,
          fiErrorStatusOut => fiErrorStatusLoc);
        if TransactionIdReq /= TransactionIdResp then
          ReportTransactionIdError;
          fiErrorStatusLoc.Status := true;
        end if;
        Data := ReturnU64_ArrayType(ReadData);
      end if;
    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_ReadFifo;

  procedure NiFpga_WriteFifo (
    fifo : in natural;
    Data : in tU64_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus) is
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable TimedOut : boolean;
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    fiErrorStatusLoc.Status := NiFpga_VerifyFifoNumber(Fifo, fiErrorStatusLoc);

    if not fiErrorStatusLoc.Status then
      if kDmaFifoConfArray(fifo).FifoWidth /= 64 or kDmaFifoConfArray(fifo).SignedData then
          ReportIncorrectDataError(fifo, "U64");
        fiErrorStatusLoc.Status := true;
      else
        NiFpga_WriteFifo
          (kViControl => kViControl,
          TransactionIdReq => TransactionIdReq,
          TransactionIdResp => TransactionIdResp,
          fifo => fifo,
          Data => ReturnDataArrayType(Data),
          timeout => timeout,
          TimedOut => TimedOut,
          remaining => remaining,
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady => fiTargetToHostReady,
          fiErrorStatusIn => fiErrorStatusLoc,
          fiErrorStatusOut => fiErrorStatusLoc);
        if TransactionIdReq /= TransactionIdResp then
          ReportTransactionIdError;
          fiErrorStatusLoc.Status := true;
        end if;
      end if;
    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_WriteFifo;

  ---------------------------------------------------------------------------------------
  -- tU32 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ReadFifo (
    fifo : in natural;
    Data : out tU32_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus) is
    variable ReadData : tDataArray(Data'range);
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable TimedOut : boolean;
    variable fiErrorStatusLoc : tErrorStatus;
  begin
    fiErrorStatusLoc := fiErrorStatusIn;

    fiErrorStatusLoc.Status := NiFpga_VerifyFifoNumber(Fifo, fiErrorStatusLoc);

    if not fiErrorStatusLoc.Status then
      if kDmaFifoConfArray(fifo).FifoWidth /= 32 or kDmaFifoConfArray(fifo).SignedData then
          ReportIncorrectDataError(fifo, "U32");
        fiErrorStatusLoc.Status := true;
      else
        NiFpga_ReadFifo
          (kViControl => kViControl,
          fifo => fifo,
          Data => ReadData,
          timeout => timeout,
          TimedOut => TimedOut,
          remaining => remaining,
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady => fiTargetToHostReady,
          TransactionIdReq => TransactionIdReq,
          TransactionIdResp => TransactionIdResp,
          fiErrorStatusIn => fiErrorStatusLoc,
          fiErrorStatusOut => fiErrorStatusLoc);
        if TransactionIdReq /= TransactionIdResp then
          ReportTransactionIdError;
          fiErrorStatusLoc.Status := true;
        end if;
        Data := ReturnU32_ArrayType(ReadData);
      end if;
    end if;
    fiErrorStatusOut := fiErrorStatusLoc;
  end procedure NiFpga_ReadFifo;

  procedure NiFpga_WriteFifo (
    fifo : in natural;
    Data : in tU32_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus) is
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable TimedOut : boolean;
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    fiErrorStatusLoc.Status := NiFpga_VerifyFifoNumber(Fifo, fiErrorStatusLoc);

    if not fiErrorStatusLoc.Status then
      if kDmaFifoConfArray(fifo).FifoWidth /= 32 or kDmaFifoConfArray(fifo).SignedData then
          ReportIncorrectDataError(fifo, "U32");
        fiErrorStatusLoc.Status := true;
      else
        NiFpga_WriteFifo
          (kViControl => kViControl,
          TransactionIdReq => TransactionIdReq,
          TransactionIdResp => TransactionIdResp,
          fifo => fifo,
          Data => ReturnDataArrayType(Data),
          timeout => timeout,
          TimedOut => TimedOut,
          remaining => remaining,
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady => fiTargetToHostReady,
          fiErrorStatusIn => fiErrorStatusLoc,
          fiErrorStatusOut => fiErrorStatusLoc);
        if TransactionIdReq /= TransactionIdResp then
          ReportTransactionIdError;
          fiErrorStatusOut.Status := true;
        end if;
      end if;
    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_WriteFifo;

  ---------------------------------------------------------------------------------------
  -- tU16 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ReadFifo (
    fifo : in natural;
    Data : out tU16_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus) is
    variable ReadData : tDataArray(Data'range);
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable TimedOut : boolean;
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    fiErrorStatusLoc.Status := NiFpga_VerifyFifoNumber(Fifo, fiErrorStatusLoc);

    if not fiErrorStatusLoc.Status then
      if kDmaFifoConfArray(fifo).FifoWidth /= 16 or kDmaFifoConfArray(fifo).SignedData then
          ReportIncorrectDataError(fifo, "U16");
        fiErrorStatusLoc.Status := true;
      else
        NiFpga_ReadFifo
          (kViControl => kViControl,
          fifo => fifo,
          Data => ReadData,
          timeout => timeout,
          TimedOut => TimedOut,
          remaining => remaining,
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady => fiTargetToHostReady,
          TransactionIdReq => TransactionIdReq,
          TransactionIdResp => TransactionIdResp,
          fiErrorStatusIn => fiErrorStatusLoc,
          fiErrorStatusOut => fiErrorStatusLoc);
        if TransactionIdReq /= TransactionIdResp then
          ReportTransactionIdError;
          fiErrorStatusLoc.Status := true;
        end if;
        Data := ReturnU16_ArrayType(ReadData);
      end if;
    end if;
    fiErrorStatusOut := fiErrorStatusLoc;
  end procedure NiFpga_ReadFifo;

  procedure NiFpga_WriteFifo (
    fifo : in natural;
    Data : in tU16_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus) is
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable TimedOut : boolean;
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    fiErrorStatusLoc.Status := NiFpga_VerifyFifoNumber(Fifo, fiErrorStatusLoc);

    if not fiErrorStatusLoc.Status then
      if kDmaFifoConfArray(fifo).FifoWidth /= 16 or kDmaFifoConfArray(fifo).SignedData then
          ReportIncorrectDataError(fifo, "U16");
        fiErrorStatusLoc.Status := true;
      else
        NiFpga_WriteFifo
          (kViControl => kViControl,
          TransactionIdReq => TransactionIdReq,
          TransactionIdResp => TransactionIdResp,
          fifo => fifo,
          Data => ReturnDataArrayType(Data),
          timeout => timeout,
          TimedOut => TimedOut,
          remaining => remaining,
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady => fiTargetToHostReady,
          fiErrorStatusIn => fiErrorStatusLoc,
          fiErrorStatusOut => fiErrorStatusLoc);
        if TransactionIdReq /= TransactionIdResp then
          ReportTransactionIdError;
          fiErrorStatusLoc.Status := true;
        end if;
      end if;
    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_WriteFifo;

  ---------------------------------------------------------------------------------------
  -- tU8 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ReadFifo (
    fifo : in natural;
    Data : out tU8_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus) is
    variable ReadData : tDataArray(Data'range);
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable TimedOut : boolean;
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    fiErrorStatusLoc.Status := NiFpga_VerifyFifoNumber(Fifo, fiErrorStatusLoc);

    if not fiErrorStatusLoc.Status then
      if kDmaFifoConfArray(fifo).FifoWidth /= 8 or kDmaFifoConfArray(fifo).SignedData then
          ReportIncorrectDataError(fifo, "U8");
        fiErrorStatusLoc.Status := true;
      else
        NiFpga_ReadFifo
          (kViControl => kViControl,
          fifo => fifo,
          Data => ReadData,
          timeout => timeout,
          TimedOut => TimedOut,
          remaining => remaining,
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady => fiTargetToHostReady,
          TransactionIdReq => TransactionIdReq,
          TransactionIdResp => TransactionIdResp,
          fiErrorStatusIn => fiErrorStatusLoc,
          fiErrorStatusOut => fiErrorStatusLoc);
        if TransactionIdReq /= TransactionIdResp then
          ReportTransactionIdError;
          fiErrorStatusLoc.Status := true;
        end if;
        Data := ReturnU8_ArrayType(ReadData);
      end if;
    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_ReadFifo;

  procedure NiFpga_WriteFifo (
    fifo : in natural;
    Data : in tU8_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus) is
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable TimedOut : boolean;
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    fiErrorStatusLoc.Status := NiFpga_VerifyFifoNumber(Fifo, fiErrorStatusLoc);

    if not fiErrorStatusLoc.Status then
      if kDmaFifoConfArray(fifo).FifoWidth /= 8 or kDmaFifoConfArray(fifo).SignedData then
          ReportIncorrectDataError(fifo, "U8");
        fiErrorStatusLoc.Status := true;
      else
        NiFpga_WriteFifo
          (kViControl => kViControl,
          TransactionIdReq => TransactionIdReq,
          TransactionIdResp => TransactionIdResp,
          fifo => fifo,
          Data => ReturnDataArrayType(Data),
          timeout => timeout,
          TimedOut => TimedOut,
          remaining => remaining,
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady => fiTargetToHostReady,
          fiErrorStatusIn => fiErrorStatusLoc,
          fiErrorStatusOut => fiErrorStatusLoc);
        if TransactionIdReq /= TransactionIdResp then
          ReportTransactionIdError;
          fiErrorStatusLoc.Status := true;
        end if;
      end if;
    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_WriteFifo;

  ---------------------------------------------------------------------------------------
  -- Boolean Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_ReadFifo (
    fifo : in natural;
    Data : out tBoolean_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus) is
    variable ReadData : tDataArray(Data'range);
    variable TransactionIdLoc : tTransactionId; --ignored for now
    variable TimedOut : boolean;
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    fiErrorStatusLoc.Status := NiFpga_VerifyFifoNumber(Fifo, fiErrorStatusLoc);

    if not fiErrorStatusLoc.Status then
      if kDmaFifoConfArray(fifo).FifoWidth /= 1 then
          ReportIncorrectDataError(fifo, "boolean");
        fiErrorStatusLoc.Status := true;
      else
        NiFpga_ReadFifo
          (kViControl => kViControl,
          fifo => fifo,
          Data => ReadData,
          timeout => timeout,
          TimedOut => TimedOut,
          remaining => remaining,
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady => fiTargetToHostReady,
          TransactionIdResp => TransactionIdLoc,
          fiErrorStatusIn => fiErrorStatusIn,
          fiErrorStatusOut => fiErrorStatusOut);
        Data := ReturnBoolean_ArrayType(ReadData);
      end if;
    end if;

  end procedure NiFpga_ReadFifo;

  procedure NiFpga_WriteFifo (
    fifo : in natural;
    Data : in tBoolean_Array;
    timeout : in integer;
    remaining : out natural;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus) is
    variable TransactionIdLoc : tTransactionId;
    variable TimedOut : boolean;
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    fiErrorStatusLoc.Status := NiFpga_VerifyFifoNumber(Fifo, fiErrorStatusLoc);

    if not fiErrorStatusLoc.Status then
      if kDmaFifoConfArray(fifo).FifoWidth /= 1 then
          ReportIncorrectDataError(fifo, "boolean");
        fiErrorStatusLoc.Status := true;
      else
        NiFpga_WriteFifo
          (kViControl => kViControl,
          TransactionIdResp => TransactionIdLoc,
          fifo => fifo,
          Data => ReturnDataArrayType(Data),
          timeout => timeout,
          TimedOut => TimedOut,
          remaining => remaining,
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiTargetToHostInterface => fiTargetToHostInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiTargetToHostReady => fiTargetToHostReady,
          fiErrorStatusIn => fiErrorStatusIn,
          fiErrorStatusOut => fiErrorStatusOut);
      end if;
    end if;

  end procedure NiFpga_WriteFifo;


end package body;
