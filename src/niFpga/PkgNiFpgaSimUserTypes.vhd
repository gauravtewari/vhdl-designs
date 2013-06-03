-- © 2012 National Instruments Corporation.
--Use of this file is subject to the Software License Agreement provided with 
--LabVIEW FPGA Module and, without limiting any of the provisions in that license, 
--modifying or distributing this file is prohibited.

--Adding to, deleting from, or otherwise modifying the contents of this file 
--may cause the software to malfunction.
-------------------------------------------------------------------------------
--
-- Purpose:
--
--   This package contains definitions for user types used for controls
-- and indicators.  This includes clusters, arrays, and enumerated types.
-- Conversion functions are also included.
--
-------------------------------------------------------------------------------

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.PkgNiUtilities.all;
use work.PkgNiFpgaSimInterfaceTypes.all;
use work.PkgNiFpgaSimCommonProceduresPrivate.all;
use work.PkgNiFpgaSimControlAndIndicatorProcedures.all;
use work.PkgNiFpgaSimInterfaceLvDataTypes.all;
use work.PkgNiFpgaSimulationModel.all;
use work.PkgNiFpgaSimFifoProceduresPrivate.all;
use work.PkgNiFpgaSimDma.all;
use work.PkgNiFpgaSimFxp.all;
use work.PkgRegister.all;

package PkgNiFpgaSimUserTypes is

  -----------------------------------------------------------------------------
  -- TYPE DEFINITIONS
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- FIFO PROCEDURES
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- CONTROL/INDICATOR READ/WRITE PROCEDURES
  -----------------------------------------------------------------------------


  ---------------------------------------------------------------------------------------
  -- FXP DATA TYPE READ/WRITE PROCEDURES
  ---------------------------------------------------------------------------------------
  procedure NiFpga_Write
    (Address : natural;
     Data : tFxpUns;
     Overflow : boolean;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiHostToTargetReady : in boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_Write
    (Address : natural;
     Data : tFxpUns;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiHostToTargetReady : in boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_Read
    (Address : natural;
     Data : out tFxpUns;
     Overflow : out boolean;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiTargetToHostInterface : in tFiInterface;
     signal fiHostToTargetReady : in boolean;
     signal fiTargetToHostReady : out boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_Read
    (Address : natural;
     Data : out tFxpUns;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiTargetToHostInterface : in tFiInterface;
     signal fiHostToTargetReady : in boolean;
     signal fiTargetToHostReady : out boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_Write
    (Address : natural;
     Data : tFxpSgn;
     Overflow : boolean;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiHostToTargetReady : in boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_Write
    (Address : natural;
     Data : tFxpSgn;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiHostToTargetReady : in boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_Read
    (Address : natural;
     Data : out tFxpSgn;
     Overflow : out boolean;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiTargetToHostInterface : in tFiInterface;
     signal fiHostToTargetReady : in boolean;
     signal fiTargetToHostReady : out boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_Read
    (Address : natural;
     Data : out tFxpSgn;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiTargetToHostInterface : in tFiInterface;
     signal fiHostToTargetReady : in boolean;
     signal fiTargetToHostReady : out boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus);

end PkgNiFpgaSimUserTypes;

package body PkgNiFpgaSimUserTypes is

  -----------------------------------------------------------------------------
  -- BASE READ/WRITE PROCEDURES
  -----------------------------------------------------------------------------

  procedure NiFpga_ReadUserType
   (Address : natural;
    Data : out std_logic_vector;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus)
  is
    constant kDataWidth : natural := Data'length;
    constant kRdCount : natural := (kDataWidth + kFiDataWidth-1)/kFiDataWidth;
    constant kVectorWidth : natural := kRdCount*kFiDataWidth;
    variable RdData : std_logic_vector(kVectorWidth-1 downto 0) := (others=>'0');
    variable RdDataTemp : tData;
    variable fiErrorStatusLoc : tErrorStatus;
    variable TransactionIdLoc : tTransactionId; -- ignored for now
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    for i in kRdCount-1 downto 0 loop
      NiFpga_Read
       (kViControl => kViControl,
        Address => Address,
        Data => RdDataTemp,
        FiClock => FiClock,
        fiHostToTargetInterface => fiHostToTargetInterface,
        fiTargetToHostInterface => fiTargetToHostInterface,
        fiHostToTargetReady => fiHostToTargetReady,
        fiTargetToHostReady => fiTargetToHostReady,
        TransactionIdResp => TransactionIdLoc,
        fiErrorStatusIn => fiErrorStatusLoc,
        fiErrorStatusOut => fiErrorStatusLoc);
      RdData((i+1)*kFiDataWidth-1 downto i*kFiDataWidth) := RdDataTemp;
    end loop;

    if kRdCount > 1 then
      -- Wide registers left align the data.
      Data := RdData(kVectorWidth-1 downto kVectorWidth-kDataWidth);
    else
      -- Regular register right align the data.
      Data := RdData(kDataWidth-1 downto 0);
    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_ReadUserType;


  procedure NiFpga_WriteUserType
   (Address : natural;
    Data : in std_logic_vector;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus)
  is
    constant kDataWidth : natural := Data'length;
    constant kWrCount : natural := (kDataWidth + kFiDataWidth - 1)/kFiDataWidth;
    constant kVectorWidth : natural := kWrCount*kFiDataWidth;
    variable WrData : std_logic_vector(kVectorWidth-1 downto 0) := (others=>'0');
    variable WrDataTemp : tData;
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    if kWrCount > 1 then
      -- Wide registers left align the data.
      WrData(kVectorWidth-1 downto kVectorWidth-kDataWidth) := Data;
    else
      -- Regular registers right align the data.
      WrData(kDataWidth-1 downto 0) := Data;
    end if;

    for i in kWrCount-1 downto 0 loop
      WrDataTemp := WrData((i+1)*kFiDataWidth-1 downto i*kFiDataWidth);
      NiFpga_Write(Address => Address,
        Data => WrDataTemp,
        FiClock => FiClock,
        fiHostToTargetInterface => fiHostToTargetInterface,
        fiHostToTargetReady => fiHostToTargetReady,
        fiErrorStatusIn => fiErrorStatusLoc,
        fiErrorStatusOut => fiErrorStatusLoc);
    end loop;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_WriteUserType;

  -----------------------------------------------------------------------------
  -- CONVERSION FUNCTIONS
  -----------------------------------------------------------------------------



  -----------------------------------------------------------------------------
  -- FIFO PROCEDURES
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- FXP UTILITY FUNCTIONS
  -----------------------------------------------------------------------------

  function IncludeOverflow(Address : natural) return boolean is
  begin
    return false;
  end function IncludeOverflow;


  -----------------------------------------------------------------------------
  -- CONTROL/INDICATOR PROCEDURES
  -----------------------------------------------------------------------------



  -- FXP Write Procedures
  procedure NiFpga_Write
   (Address : natural;
    Data : tFxpSgn;
    Overflow : boolean;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus)
  is
    variable WrData : tFxpGen(Data'left+1 downto Data'right);
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    assert IncludeOverflow(Address)
      report "NiFpga_Write with overflow called for non-overflow register.  Check register address."
      severity error;

    WrData := to_Fxp(PackFxpValueAndOverflow(to_slv(Data), to_StdLogic(Overflow)), WrData);

    NiFpga_Write
     (Address => Address,
      Data => WrData,
      FiClock => FiClock,
      fiHostToTargetInterface => fiHostToTargetInterface,
      fiHostToTargetReady => fiHostToTargetReady,
      fiErrorStatusIn => fiErrorStatusLoc,
      fiErrorStatusOut => fiErrorStatusLoc);

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_Write;

  procedure NiFpga_Write
   (Address : natural;
    Data : tFxpSgn;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus)
  is
    variable WrData : tFxpGen(Data'range);
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    if IncludeOverflow(Address) then
      NiFpga_Write
       (Address => Address,
        Data => Data,
        Overflow => false,
        FiClock => FiClock,
        fiHostToTargetInterface => fiHostToTargetInterface,
        fiHostToTargetReady => fiHostToTargetReady,
        fiErrorStatusIn => fiErrorStatusLoc,
        fiErrorStatusOut => fiErrorStatusLoc);
    else
      WrData := tFxpGen(Data);
      NiFpga_Write
       (Address => Address,
        Data => WrData,
        FiClock => FiClock,
        fiHostToTargetInterface => fiHostToTargetInterface,
        fiHostToTargetReady => fiHostToTargetReady,
        fiErrorStatusIn => fiErrorStatusLoc,
        fiErrorStatusOut => fiErrorStatusLoc);
    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_Write;

  procedure NiFpga_Write
   (Address : natural;
    Data : tFxpUns;
    Overflow : boolean;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus)
  is
    variable WrData : tFxpGen(Data'left+1 downto Data'right);
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    assert IncludeOverflow(Address)
      report "NiFpga_Write with overflow called for non-overflow register.  Check register address."
      severity error;

    WrData := to_Fxp(PackFxpValueAndOverflow(to_slv(Data), to_StdLogic(Overflow)), WrData);

    NiFpga_Write
     (Address => Address,
      Data => WrData,
      FiClock => FiClock,
      fiHostToTargetInterface => fiHostToTargetInterface,
      fiHostToTargetReady => fiHostToTargetReady,
      fiErrorStatusIn => fiErrorStatusLoc,
      fiErrorStatusOut => fiErrorStatusLoc);

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_Write;

  procedure NiFpga_Write
   (Address : natural;
    Data : tFxpUns;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus)
  is
    variable WrData : tFxpGen(Data'range);
    variable fiErrorStatusLoc : tErrorStatus;
  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    if IncludeOverflow(Address) then
      NiFpga_Write
       (Address => Address,
        Data => Data,
        Overflow => false,
        FiClock => FiClock,
        fiHostToTargetInterface => fiHostToTargetInterface,
        fiHostToTargetReady => fiHostToTargetReady,
        fiErrorStatusIn => fiErrorStatusLoc,
        fiErrorStatusOut => fiErrorStatusLoc);
    else
      WrData := tFxpGen(Data);
      NiFpga_Write
       (Address => Address,
        Data => WrData,
        FiClock => FiClock,
        fiHostToTargetInterface => fiHostToTargetInterface,
        fiHostToTargetReady => fiHostToTargetReady,
        fiErrorStatusIn => fiErrorStatusLoc,
        fiErrorStatusOut => fiErrorStatusLoc);
    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_Write;


  -- FXP Read Procedures
  procedure NiFpga_Read
   (Address : natural;
    Data : out tFxpSgn;
    Overflow : out boolean;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus
  ) is

    variable RdData : tFxpGen(Data'left+1 downto Data'right);
    variable FxpDataConfig : tFxpSgn(Data'range);
    variable fiErrorStatusLoc : tErrorStatus;

  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    assert IncludeOverflow(Address)
      report "NiFpga_Read with overflow called for register without overflow.  Check the address."
      severity error;

    NiFpga_Read
     (Address => Address,
      Data => RdData,
      FiClock => FiClock,
      fiHostToTargetInterface => fiHostToTargetInterface,
      fiTargetToHostInterface => fiTargetToHostInterface,
      fiHostToTargetReady => fiHostToTargetReady,
      fiTargetToHostReady => fiTargetToHostReady,
      fiErrorStatusIn => fiErrorStatusLoc,
      fiErrorStatusOut => fiErrorStatusLoc);

    Data := to_Fxp(ExtractFxpValue(true, to_slv(RdData)), FxpDataConfig);
    Overflow := to_Boolean(ExtractFxpOverflow(true, to_slv(RdData)));

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_Read;

  procedure NiFpga_Read
   (Address : natural;
    Data : out tFxpSgn;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus
  ) is

    variable RdData : tFxpGen(Data'range);
    variable Overflow : boolean;
    variable fiErrorStatusLoc : tErrorStatus;

  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    if IncludeOverflow(Address) then
      NiFpga_Read
       (Address => Address,
        Data => Data,
        Overflow => Overflow,
        FiClock => FiClock,
        fiHostToTargetInterface => fiHostToTargetInterface,
        fiTargetToHostInterface => fiTargetToHostInterface,
        fiHostToTargetReady => fiHostToTargetReady,
        fiTargetToHostReady => fiTargetToHostReady,
        fiErrorStatusIn => fiErrorStatusLoc,
        fiErrorStatusOut => fiErrorStatusLoc);
    else
      NiFpga_Read
       (Address => Address,
        Data => RdData,
        FiClock => FiClock,
        fiHostToTargetInterface => fiHostToTargetInterface,
        fiTargetToHostInterface => fiTargetToHostInterface,
        fiHostToTargetReady => fiHostToTargetReady,
        fiTargetToHostReady => fiTargetToHostReady,
        fiErrorStatusIn => fiErrorStatusLoc,
        fiErrorStatusOut => fiErrorStatusLoc);
      Data := tFxpSgn(RdData);
    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_Read;

  procedure NiFpga_Read
   (Address : natural;
    Data : out tFxpUns;
    Overflow : out boolean;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus
  ) is

    variable RdData : tFxpGen(Data'left+1 downto Data'right);
    variable FxpDataConfig : tFxpUns(Data'range);
    variable fiErrorStatusLoc : tErrorStatus;

  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    assert IncludeOverflow(Address)
      report "NiFpga_Read with overflow called for register without overflow.  Check the address."
      severity error;

    NiFpga_Read
     (Address => Address,
      Data => RdData,
      FiClock => FiClock,
      fiHostToTargetInterface => fiHostToTargetInterface,
      fiTargetToHostInterface => fiTargetToHostInterface,
      fiHostToTargetReady => fiHostToTargetReady,
      fiTargetToHostReady => fiTargetToHostReady,
      fiErrorStatusIn => fiErrorStatusLoc,
      fiErrorStatusOut => fiErrorStatusLoc);

    Data := to_Fxp(ExtractFxpValue(true, to_slv(RdData)), FxpDataConfig);
    Overflow := to_Boolean(ExtractFxpOverflow(true, to_slv(RdData)));

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_Read;

  procedure NiFpga_Read
   (Address : natural;
    Data : out tFxpUns;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus
  ) is

    variable RdData : tFxpGen(Data'range);
    variable Overflow : boolean;
    variable fiErrorStatusLoc : tErrorStatus;

  begin

    fiErrorStatusLoc := fiErrorStatusIn;

    if IncludeOverflow(Address) then
      NiFpga_Read
       (Address => Address,
        Data => Data,
        Overflow => Overflow,
        FiClock => FiClock,
        fiHostToTargetInterface => fiHostToTargetInterface,
        fiTargetToHostInterface => fiTargetToHostInterface,
        fiHostToTargetReady => fiHostToTargetReady,
        fiTargetToHostReady => fiTargetToHostReady,
        fiErrorStatusIn => fiErrorStatusLoc,
        fiErrorStatusOut => fiErrorStatusLoc);
    else
      NiFpga_Read
       (Address => Address,
        Data => RdData,
        FiClock => FiClock,
        fiHostToTargetInterface => fiHostToTargetInterface,
        fiTargetToHostInterface => fiTargetToHostInterface,
        fiHostToTargetReady => fiHostToTargetReady,
        fiTargetToHostReady => fiTargetToHostReady,
        fiErrorStatusIn => fiErrorStatusLoc,
        fiErrorStatusOut => fiErrorStatusLoc);
      Data := tFxpUns(RdData);
    end if;

    fiErrorStatusOut := fiErrorStatusLoc;

  end procedure NiFpga_Read;

end PkgNiFpgaSimUserTypes;
