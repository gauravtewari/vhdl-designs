-- © 2012 National Instruments Corporation.
--Use of this file is subject to the Software License Agreement provided with 
--LabVIEW FPGA Module and, without limiting any of the provisions in that license, 
--modifying or distributing this file is prohibited.

--Adding to, deleting from, or otherwise modifying the contents of this file 
--may cause the software to malfunction.
-------------------------------------------------------------------------------
--
-- Purpose:
-- This package contains procedures that the Host Interface test bench can use
-- to open, run, close, and reset.
-- Refer to the following packages for the remaining Host Interface procedures:
-- > reading and writing controls (see PkgNiFpgaSimControlAndIndicatorsProcedures.vhd)
-- > accessing DMA FIFOs (see PkgNiFpgaSimFifoProcedures.vhd)
-- > related to interrupts (see PkgNiFpgaSimIrqProceduresPrivate.vhd)
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.PkgNiUtilities.all;
use work.PkgRegister.all;
use work.PkgNiFpgaSimCommonProcedures.all;
use work.PkgNiFpgaSimMiscProceduresPrivate.all;
use work.PkgNiFpgaSimInterfaceTypes.all;
use work.PkgNiFpgaUtilities.all;

package PkgNiFpgaSimMiscProcedures is

  --===========================================================================
  --FPGA INTERFACE PROCEDURE CALLS
  --===========================================================================
  -----------------------------------------------------------------------------
  --Miscellaneous FPGA INTERFACE PROCEDURES - Declarations
  -----------------------------------------------------------------------------
  --===========================================================================
  --
  -- [Procedure] NiFpga_Open
  -- This procedure simulates opening a session to the FPGA. The FPGA runs
  -- unless you set the attribute, attr, kNiFpga_OpenAttribute_NoRun bit to
  -- '1'.
  --
  --===========================================================================
  procedure NiFpga_Open
    (attr : unsigned(31 downto 0) := (kNiFpga_OpenAttribute_NoRun => '0', others => '0');
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus );

  --===========================================================================
  --
  -- [Procedure] NiFpga_Run
  -- Runs the FPGA VI on the target. If you set the attribute, attr,
  -- kNiFpga_RunAttribute_WaitUntilDone to '1', NiFpga_Run blocks
  -- until the FPGA finishes running.
  --
  --===========================================================================
  procedure NiFpga_Run
    (attr : unsigned(31 downto 0) := (kNiFpga_RunAttribute_WaitUntilDone => '0', others => '0');
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  --===========================================================================
  --
  -- [Procedure] NiFpga_Close
  -- This procedure simulates closing a session to the FPGA. The FPGA resets
  -- unless you set the attribute, attr,
  -- kNiFpga_CloseAttribute_NoResetIfLastSession bit to '1'.
  --
  --===========================================================================
  procedure NiFpga_Close
    (attr : unsigned(31 downto 0) := (kNiFpga_CloseAttribute_NoResetIfLastSession => '0', others => '0');
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  --===========================================================================
  --
  -- [Procedure] NiFpga_Reset
  -- Resets the VI.
  --
  --===========================================================================
  procedure NiFpga_Reset (
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  --===========================================================================
  --
  -- [Procedure] NiFpga_Download
  -- NOT SUPPORTED, DO NOT USE!
  --
  --===========================================================================
  procedure NiFpga_Download
    (signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusOut : out tErrorStatus);

end package;
--=============================================================================
-- PACKAGE BODY
--=============================================================================
package body PkgNiFpgaSimMiscProcedures is

  -----------------------------------------------------------------------------
  --FPGA INTERFACE PROCEDURES - DEFINITIONS
  -----------------------------------------------------------------------------

  --===========================================================================
  --
  -- [Procedure] NiFpga_Open
  -- This procedure simulates opening a session to the FPGA. The FPGA runs
  -- unless you set the attribute, attr, kNiFpga_OpenAttribute_NoRun bit to
  -- '1'.
  --
  --===========================================================================
  procedure NiFpga_Open
    (attr : unsigned(31 downto 0) := (kNiFpga_OpenAttribute_NoRun => '0', others => '0');
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus ) is

    variable TransactionIdLoc : tTransactionId;
  begin
    NiFpga_Open
    ( attr => attr,
      kNiFpga_OpenAttribute_NoRun => kNiFpga_OpenAttribute_NoRun,
      kNiFpga_RunAttribute_WaitUntilDone => kNiFpga_RunAttribute_WaitUntilDone,
      kViControl => kViControl,
      FiClock => FiClock,
      fiHostToTargetInterface => fiHostToTargetInterface,
      fiTargetToHostInterface => fiTargetToHostInterface,
      fiHostToTargetReady => fiHostToTargetReady,
      fiTargetToHostReady => fiTargetToHostReady,
      TransactionIdResp => TransactionIdLoc,
      fiErrorStatusIn => fiErrorStatusIn,
      fiErrorStatusOut => fiErrorStatusOut
    );
  end procedure NiFpga_Open;

  --===========================================================================
  --
  -- [Procedure] NiFpga_Run
  -- Runs the FPGA VI on the target. If you set the attribute, attr,
  -- kNiFpga_RunAttribute_WaitUntilDone to '1', NiFpga_Run blocks
  -- until the FPGA finishes running.
  --
  --===========================================================================
  procedure NiFpga_Run
    (attr : unsigned(31 downto 0) := (kNiFpga_RunAttribute_WaitUntilDone => '0', others => '0');
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus ) is

    variable TransactionIdLoc : tTransactionId;
  begin
    NiFpga_Run
    ( attr => attr,
      kViControl => kViControl,
      kNiFpga_RunAttribute_WaitUntilDone => kNiFpga_RunAttribute_WaitUntilDone,
      FiClock => FiClock,
      fiHostToTargetInterface => fiHostToTargetInterface,
      fiTargetToHostInterface => fiTargetToHostInterface,
      fiHostToTargetReady => fiHostToTargetReady,
      fiTargetToHostReady => fiTargetToHostReady,
      TransactionIdResp => TransactionIdLoc,
      fiErrorStatusIn => fiErrorStatusIn,
      fiErrorStatusOut => fiErrorStatusOut
    );
  end procedure NiFpga_Run;

  --===========================================================================
  --
  -- [Procedure] NiFpga_Close
  -- This procedure simulates closing a session to the FPGA. The FPGA resets
  -- unless you set the attribute, attr,
  -- kNiFpga_CloseAttribute_NoResetIfLastSession bit to '1'.
  --
  --===========================================================================
  procedure NiFpga_Close
    (attr : unsigned(31 downto 0) := (kNiFpga_CloseAttribute_NoResetIfLastSession => '0', others => '0');
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus ) is

    variable TransactionIdLoc : tTransactionId;
  begin
    NiFpga_Close
    ( attr => attr,
      kNiFpga_CloseAttribute_NoResetIfLastSession => kNiFpga_CloseAttribute_NoResetIfLastSession,
      kDiagramReset => kDiagramReset,
      kViControl => kViControl,
      FiClock => FiClock,
      fiHostToTargetInterface => fiHostToTargetInterface,
      fiTargetToHostInterface => fiTargetToHostInterface,
      fiHostToTargetReady => fiHostToTargetReady,
      fiTargetToHostReady => fiTargetToHostReady,
      TransactionIdResp => TransactionIdLoc,
      fiErrorStatusIn => fiErrorStatusIn,
      fiErrorStatusOut => fiErrorStatusOut
    );
  end procedure NiFpga_Close;

  --===========================================================================
  --
  -- [Procedure] NiFpga_Reset
  -- Resets the VI.
  --
  --===========================================================================
  procedure NiFpga_Reset (
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus ) is

    variable TransactionIdLoc : tTransactionId;
  begin

    NiFpga_Reset
    ( kViControl => kViControl,
      kDiagramReset => kDiagramReset,
      FiClock => FiClock,
      fiHostToTargetInterface => fiHostToTargetInterface,
      fiTargetToHostInterface => fiTargetToHostInterface,
      fiHostToTargetReady => fiHostToTargetReady,
      fiTargetToHostReady => fiTargetToHostReady,
      TransactionIdResp => TransactionIdLoc,
      fiErrorStatusIn => fiErrorStatusIn,
      fiErrorStatusOut => fiErrorStatusOut
    );

  end procedure NiFpga_Reset;

  --===========================================================================
  --
  -- [Procedure] NiFpga_Download
  -- NOT SUPPORTED, DO NOT USE!
  --
  --===========================================================================
  procedure NiFpga_Download
    (signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiTargetToHostReady : out boolean;
    fiErrorStatusOut : out tErrorStatus ) is
    variable TransactionIdLoc : tTransactionId;
  begin

    ReportError
       (ErrorType => ExternalError,
        ShortDescription => "NiFpga_Download procedure is not supported in cycle-accurate simulation."
                            & "  Remove all references to this procedure.");
      fiErrorStatusOut.Status := true;
      fiErrorStatusOut.Code := -1;

  end procedure NiFpga_Download;

end package body;
