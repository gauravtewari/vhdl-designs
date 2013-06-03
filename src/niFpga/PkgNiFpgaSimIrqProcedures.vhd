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
-- to wait on and acknowledge interrupts on the FPGA VI. 
-- Refer to the following packages for the remaining Host Interface procedures:
-- > reading and writing controls (see PkgNiFpgaSimControlAndIndicatorsProcedures.vhd)
-- > accessing DMA FIFOs (see PkgNiFpgaSimFifoProcedures.vhd)
-- > related to open, close, etc.(see NiFpgaSimMiscProcedures.vhd)
--
------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.PkgNiUtilities.all;
use work.PkgNiFpgaSimIrqProceduresPrivate.all;
use work.PkgNiFpgaSimInterfaceLvDataTypes.all;
use work.PkgNiFpgaSimInterfaceTypes.all;
use work.PkgRegister.all;

package PkgNiFpgaSimIrqProcedures is

  ---------------------------------------------------------------------------------------
  -- FPGA INTERFACE INTERRUPT PROCEDURES - Declarations
  ---------------------------------------------------------------------------------------
  procedure NiFpga_WaitOnIrq(
    Irqs : in tU32;
    Timeout : in integer;
    TimedOut : out boolean;
    IrqsAsserted : out tU32;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    signal aInterrupt : in boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);

  procedure NiFpga_AcknowledgeIrq(
    Irqs : in tU32;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiHostToTargetReady : in boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus);


 end package PkgNiFpgaSimIrqProcedures;

 package body PkgNiFpgaSimIrqProcedures is


  ---------------------------------------------------------------------------------------
   -- FPGA INTERFACE INTERRUPT PROCEDURES
   ---------------------------------------------------------------------------------------
   ---------------------------------------------------------------------------------------
   -- NiFpga_WaitOnIrq
   ---------------------------------------------------------------------------------------
   procedure NiFpga_WaitOnIrq
    (Irqs : in tU32;
    timeout : in integer;
    timedOut : out boolean;
    IrqsAsserted : out tU32;
    signal FiClock : in std_logic;
    signal fiHostToTargetInterface : out tFiInterface;
    signal fiTargetToHostInterface : in tFiInterface;
    signal fiHostToTargetReady : in boolean;
    signal fiTargetToHostReady : out boolean;
    signal aInterrupt : in boolean;
    fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
    fiErrorStatusOut : out tErrorStatus)  is

    variable TransactionIdLoc : tTransactionId;
   begin

      NiFpga_WaitOnIrq
      ( kViControl => kViControl,
        Irqs => Irqs,
        timeout => timeout,
        timedout => timedout,
        IrqsAsserted => IrqsAsserted,
        aInterrupt => aInterrupt,
        FiClock => FiClock,
        fiHostToTargetInterface => fiHostToTargetInterface,
        fiTargetToHostInterface => fiTargetToHostInterface,
        fiHostToTargetReady => fiHostToTargetReady,
        fiTargetToHostReady => fiTargetToHostReady,
        TransactionIdResp => TransactionIdLoc,
        fiErrorStatusIn => fiErrorStatusIn,
        fiErrorStatusOut => fiErrorStatusOut);

   end procedure NiFpga_WaitOnIrq;
   ---------------------------------------------------------------------------------------
   -- NiFpga_AcknowledgeIrq
   ---------------------------------------------------------------------------------------
   procedure NiFpga_AcknowledgeIrq(
     irqs : in tU32;
     signal FiClock : in std_logic;
     signal fiHostToTargetInterface : out tFiInterface;
     signal fiHostToTargetReady : in boolean;
     fiErrorStatusIn : in tErrorStatus := (Status => false, Code => 0);
     fiErrorStatusOut : out tErrorStatus)
   is
   begin
      NiFpga_AcknowledgeIrq
      ( irqsToAcknowledge => Irqs,
        FiClock => FiClock,
        fiHostToTargetInterface => fiHostToTargetInterface,
        fiHostToTargetReady => fiHostToTargetReady,
        fiErrorStatusIn => fiErrorStatusIn,
        fiErrorStatusOut => fiErrorStatusOut);
  end procedure NiFpga_AcknowledgeIrq;


 end package body PkgNiFpgaSimIrqProcedures;