-- © 2012 National Instruments Corporation.
--Use of this file is subject to the Software License Agreement provided with 
--LabVIEW FPGA Module and, without limiting any of the provisions in that license, 
--modifying or distributing this file is prohibited.

--Adding to, deleting from, or otherwise modifying the contents of this file 
--may cause the software to malfunction.
-------------------------------------------------------------------------------
--
-- Purpose:
-- This package defines procedures that the Host Interface test bench can use
-- to access controls and indicators on the FPGA VI directly.
-- Refer to the following packages for the remaining Host Interface procedures:
-- > accessing DMA FIFOs (see PkgNiFpgaSimFifoProcedures.vhd)
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
use work.PkgNiFpgaSimCommonProceduresPrivate.all;
use work.PkgNiFpgaSimFxp.all;
use work.PkgNiFpgaUtilities.all;
--Generated packages
use work.PkgRegister.all;
use work.PkgCommIntConfiguration.all;
use work.PkgNiFpgaSimulationModel.all;

package PkgNiFpgaSimControlAndIndicatorProcedures is

  --===========================================================================
  --FPGA INTERFACE PROCEDURE CALLS
  --===========================================================================
  -----------------------------------------------------------------------------
  --REGISTER ACCESS PROCEDURES - Declarations
  -----------------------------------------------------------------------------
  ---------------------------------------------------------------------------------------
  -- tI64, tI32, tI16, and tI8 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_Write
    (Address : natural;
     Data : signed;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiHostToTargetReady : in boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_Read
    (Address : natural;
     Data : out signed;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiTargetToHostInterface : in tFiInterface;
     signal fiHostToTargetReady : in boolean;
     signal fiTargetToHostReady : out boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus);

  ---------------------------------------------------------------------------------------
  -- tU64, tU32, tU16, and tU8 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_Write
    (Address : natural;
     Data : unsigned;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiHostToTargetReady : in boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_Read
    (Address : natural;
     Data : out unsigned;
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
  procedure NiFpga_Write
    (Address : natural;
     Data : boolean;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiHostToTargetReady : in boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_Read (Address : natural;
     Data : out boolean;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiTargetToHostInterface : in tFiInterface;
     signal fiHostToTargetReady : in boolean;
     signal fiTargetToHostReady : out boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus);

  ---------------------------------------------------------------------------------------
  -- FXP Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_Read
   (Address : natural;
    Data : out tFxpGen;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_Write
   (Address : natural;
    Data : tFxpGen;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  ---------------------------------------------------------------------------------------
  --Single-precision Floating Point (SGL) Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_Read
   (Address : natural;
    Data : out tSGL;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_Write
   (Address : natural;
    Data : tSGL;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);
    
end package;

--=============================================================================
-- PACKAGE BODY
--=============================================================================
package body PkgNiFpgaSimControlAndIndicatorProcedures is
  -----------------------------------------------------------------------------
  --REGISTER ACCESS PROCEDURES - DEFINITIONS
  -----------------------------------------------------------------------------
  ---------------------------------------------------------------------------------------
  -- tI64, tI32, tI16, and tI8 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_Write
    (Address : natural;
    Data : signed;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus)
    is
    variable WrData : tDataArray(1 downto 0) := (others => (others => '0'));
    variable WrDataTemp : tData ;
    variable fiErrorStatusLoc : tErrorStatus;
    variable LoopIndex : natural;
  begin
    fiErrorStatusLoc := fiErrorStatusIn;

    if not fiErrorStatusLoc.Status then

      case Data'length is
        when 64 | 32 | 16 | 8 =>
          if Data'length = 64 then
            --The driver performs 2 32bit writes on a 64bit register
            LoopIndex := 1;
            WrData := ReturnDataArrayType(Data);
          else LoopIndex := 0;
          end if;

          for i in 0 to LoopIndex loop

            if Data'length = 64 then
              WrDataTemp := WrData(i);
            else WrDataTemp := ReturnDataType(Data);
            end if;

            NiFpga_Write(Address => Address,
                Data => WrDataTemp,
                FiClock => FiClock,
                fiHostToTargetInterface => fiHostToTargetInterface,
                fiHostToTargetReady => fiHostToTargetReady,
                fiErrorStatusIn => fiErrorStatusLoc,
                fiErrorStatusOut => fiErrorStatusLoc);

          end loop;

        when others =>
          fiErrorStatusLoc.Status := true;
          ReportError
            (ErrorType => ExternalError,
            ShortDescription => "NiFpga_Write Data'length not supported. Supported types are tI64, tI32, tI16, and tI8.");

      end case;

    end if;

    fiErrorStatusOut := fiErrorStatusLoc;
  end procedure NiFpga_Write;

  procedure NiFpga_Read
    (Address : natural;
    Data : out signed;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus
     ) is
    variable RdData : tDataArray(1 downto 0);
    variable RdDataTemp : tData;
    variable fiErrorStatusLoc : tErrorStatus;
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable LoopIndex : natural;
  begin
    fiErrorStatusLoc := fiErrorStatusIn;

    if not fiErrorStatusLoc.Status then

      case Data'length is
        when 64 | 32 | 16 | 8 =>

          if Data'length = 64 then
            --The driver performs 2 32bit writes on a 64bit register
            LoopIndex := 1;
          else LoopIndex := 0;
          end if;

          for i in 0 to LoopIndex loop
            NiFpga_Read
              (kViControl => kViControl,
              Address => Address,
              Data => RdDataTemp,
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
              ReportError
               (ErrorType => InternalError,
                ShortDescription => "Transaction IDs do not match for NiFpga_Read");
              fiErrorStatusLoc.Status := true;
            end if;
            if Data'length = 64 then
              RdData(i) := RdDataTemp;
            end if;
          end loop;

          if Data'length = 64 then
            Data := ReturnI64(RdData);
          elsif Data'length = 32 then
            Data := ReturnI32(RdDataTemp);
          elsif Data'length = 16 then
            Data := ReturnI16(RdDataTemp);
          elsif Data'length = 8 then
            Data := ReturnI8(RdDataTemp);
          end if;

        when others =>
          fiErrorStatusLoc.Status := true;
          ReportError
            (ErrorType => ExternalError,
            ShortDescription => "NiFpga_Read Data'length not supported. Supported types are tI64, tI32, tI16, and tI8.");
        end case;
      end if;
    fiErrorStatusOut := fiErrorStatusLoc;
  end procedure NiFpga_Read;

  ---------------------------------------------------------------------------------------
  -- tU64, tU32, tU16, and tU8 Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_Write
    (Address : natural;
     Data : unsigned;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiHostToTargetReady : in boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus)
     is
    variable WrData : tDataArray(1 downto 0) := (others => (others => '0'));
    variable WrDataTemp : tData ;
    variable fiErrorStatusLoc : tErrorStatus;
    variable LoopIndex : natural;
  begin
    fiErrorStatusLoc := fiErrorStatusIn;

    if not fiErrorStatusLoc.Status then

      case Data'length is
        when 64 | 32 | 16 | 8 =>

          if Data'length = 64 then
            --The driver performs 2 32bit writes on a 64bit register
            LoopIndex := 1;
            WrData := ReturnDataArrayType(Data);
          else LoopIndex := 0;
          end if;

          for i in 0 to LoopIndex loop

            if Data'length = 64 then
              WrDataTemp := WrData(i);
            else WrDataTemp := ReturnDataType(Data);
            end if;

            NiFpga_Write(Address => Address,
                Data => WrDataTemp,
                FiClock => FiClock,
                fiHostToTargetInterface => fiHostToTargetInterface,
                fiHostToTargetReady => fiHostToTargetReady,
                fiErrorStatusIn => fiErrorStatusLoc,
                fiErrorStatusOut => fiErrorStatusLoc);

          end loop;
        when others =>
          fiErrorStatusLoc.Status := true;
          ReportError
            (ErrorType => ExternalError,
            ShortDescription => "NiFpga_Write Data'length not supported. Supported types are tU64, tU32, tU16, and tU8.");
      end case;
    end if;
    fiErrorStatusOut := fiErrorStatusLoc;
  end procedure NiFpga_Write;

  procedure NiFpga_Read
    (Address : natural;
     Data : out unsigned;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiTargetToHostInterface : in tFiInterface;
     signal fiHostToTargetReady : in boolean;
     signal fiTargetToHostReady : out boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus
  ) is
    variable RdData : tDataArray(1 downto 0);
    variable RdDataTemp : tData;
    variable fiErrorStatusLoc : tErrorStatus;
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
    variable LoopIndex : natural;
  begin
    fiErrorStatusLoc := fiErrorStatusIn;

    if not fiErrorStatusLoc.Status then

      case Data'length is
        when 64 | 32 | 16 | 8 =>

          if Data'length = 64 then
            --The driver performs 2 32bit writes on a 64bit register
            LoopIndex := 1;
          else LoopIndex := 0;
          end if;

          for i in 0 to LoopIndex loop
            NiFpga_Read
              (kViControl => kViControl,
              Address => Address,
              Data => RdDataTemp,
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
              ReportError
               (ErrorType => InternalError,
                ShortDescription => "Transaction IDs do not match for NiFpga_Read");
              fiErrorStatusLoc.Status := true;
            end if;
            if Data'length = 64 then
              RdData(i) := RdDataTemp;
            end if;
          end loop;

          if Data'length = 64 then
            Data := ReturnU64(RdData);
          elsif Data'length = 32 then
            Data := ReturnU32(RdDataTemp);
          elsif Data'length = 16 then
            Data := ReturnU16(RdDataTemp);
          elsif Data'length = 8 then
            Data := ReturnU8(RdDataTemp);
          end if;

        when others =>
          fiErrorStatusLoc.Status := true;
          ReportError
            (ErrorType => ExternalError,
            ShortDescription => "NiFpga_Read Data'length not supported. Supported types are tU64, tU32, tU16, and tU8.");
      end case;
    end if;
    fiErrorStatusOut := fiErrorStatusLoc;
  end procedure NiFpga_Read;

  ---------------------------------------------------------------------------------------
  -- Boolean Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_Write
    (Address : natural;
     Data : boolean;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiHostToTargetReady : in boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus)
     is
     variable WrData : tData := (others => '0');
     variable fiErrorStatusLoc : tErrorStatus;
  begin
    fiErrorStatusLoc := fiErrorStatusIn;
    if not fiErrorStatusLoc.Status then
      if Data then
        WrData(0) := '1';
      end if;
      NiFpga_Write(Address => Address,
           Data => WrData,
           FiClock => FiClock,
           fiHostToTargetInterface => fiHostToTargetInterface,
           fiHostToTargetReady => fiHostToTargetReady,
           fiErrorStatusIn => fiErrorStatusLoc,
           fiErrorStatusOut => fiErrorStatusLoc);
    end if;
    fiErrorStatusOut := fiErrorStatusLoc;
  end procedure NiFpga_Write;

  procedure NiFpga_Read (Address : natural;
    Data : out boolean;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus
     ) is
    variable RdData : tData;
    variable TempInt : integer;
    variable fiErrorStatusLoc : tErrorStatus;
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;
  begin
    fiErrorStatusLoc := fiErrorStatusIn;

    if not fiErrorStatusLoc.Status then

      NiFpga_Read
        (kViControl => kViControl,
        Address => Address,
        Data => RdData,
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
        ReportError
         (ErrorType => InternalError,
          ShortDescription => "Transaction IDs do not match for NiFpga_Read");
        fiErrorStatusLoc.Status := true;
      end if;
      if RdData(0) = '1' then
          Data := true;
      else
          Data := false;
      end if;
    end if;
    fiErrorStatusOut := fiErrorStatusLoc;
  end procedure NiFpga_Read;

  ---------------------------------------------------------------------------------------
  -- FXP Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_Write
   (Address : natural;
    Data : tFxpGen;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus)
  is
    -- The number of writes to perform is the ceiling of the data width (in bits)
    -- divided by the FiDataWidth.
    variable WrCount : natural := (Data'length + kFiDataWidth-1)/kFiDataWidth;
    variable WrData : std_logic_vector(WrCount*kFiDataWidth-1 downto 0) := (others => '0');
    variable WrDataTemp : tData;
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    if not fiErrorStatusLoc.Status then

      -- Regular register are right justified while wide registers are left jusftified.
      if WrCount > 1 then
        WrData(WrData'length-1 downto WrData'length-Data'length) := to_slv(Data);
      else
        WrData(Data'length-1 downto 0) := to_slv(Data);
      end if;

      for i in WrCount-1 downto 0 loop

        WrDataTemp := WrData((i+1)*kFiDataWidth-1 downto i*kFiDataWidth);

        NiFpga_Write(Address => Address,
            Data => WrDataTemp,
            FiClock => FiClock,
            fiHostToTargetInterface => fiHostToTargetInterface,
            fiHostToTargetReady => fiHostToTargetReady,
            fiErrorStatusIn => fiErrorStatusLoc,
            fiErrorStatusOut => fiErrorStatusLoc);

      end loop;

    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_Write;

  procedure NiFpga_Read
   (Address : natural;
    Data : out tFxpGen;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus
  ) is

    -- The number of reads to perform is the ceiling of the data width (in bits)
    -- divided by the FiDataWidth.
    variable RdCount : natural := (Data'length + kFiDataWidth-1)/kFiDataWidth;
    variable RdData : std_logic_vector(RdCount*kFiDataWidth-1 downto 0);
    variable RdDataTemp : tData;
    variable FxpDataConfig : tFxpGen(Data'range);
    variable fiErrorStatusLoc : tErrorStatus;
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;

  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    if not fiErrorStatusLoc.Status then

      for i in RdCount-1 downto 0 loop
        NiFpga_Read
         (kViControl => kViControl,
          Address => Address,
          Data => RdDataTemp,
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
          ReportError
           (ErrorType => InternalError,
            ShortDescription => "Transaction IDs do not match for NiFpga_Read");
          fiErrorStatusLoc.Status := true;
        end if;
        RdData((i+1)*kFiDataWidth-1 downto i*kFiDataWidth) := RdDataTemp;
      end loop;

      -- Regular registers are right justified, while wide registers are left justified.
      if RdCount > 1 then
        Data := to_Fxp(RdData(RdData'length-1 downto RdData'length-Data'length),
          FxpDataConfig);
      else
        Data := to_Fxp(RdData(Data'length-1 downto 0), FxpDataConfig);
      end if;

    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_Read;

  ---------------------------------------------------------------------------------------
  -- Single-precision Floating Point (SGL) Data Type Procedures
  ---------------------------------------------------------------------------------------
  procedure NiFpga_Write
   (Address : natural;
    Data : tSGL;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus)
  is
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    if not fiErrorStatusLoc.Status then

      NiFpga_Write(Address => Address,
          Data => std_logic_vector(Data),
          FiClock => FiClock,
          fiHostToTargetInterface => fiHostToTargetInterface,
          fiHostToTargetReady => fiHostToTargetReady,
          fiErrorStatusIn => fiErrorStatusLoc,
          fiErrorStatusOut => fiErrorStatusLoc);

    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_Write;

  procedure NiFpga_Read
   (Address : natural;
    Data : out tSGL;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus
  ) is

    -- The number of reads to perform is the ceiling of the data width (in bits)
    -- divided by the FiDataWidth.
    variable RdDataTemp : tData := (others => 'X');
    variable fiErrorStatusLoc : tErrorStatus;
    variable TransactionIdReq : tTransactionId := (others=>'0');
    variable TransactionIdResp : tTransactionId;

  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    if not fiErrorStatusLoc.Status then

      NiFpga_Read
       (kViControl => kViControl,
        Address => Address,
        Data => RdDataTemp,
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
        ReportError
         (ErrorType => InternalError,
          ShortDescription => "Transaction IDs do not match for NiFpga_Read");
        fiErrorStatusLoc.Status := true;
      end if;
 
    end if;

    Data := tSGL(RdDataTemp);
    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_Read;

  
end package body;
