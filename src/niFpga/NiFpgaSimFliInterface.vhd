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

entity NiFpgaSimFliInterface is
  generic(kFifoInterruptLength : natural);
  port(
    aInterrupt : in std_logic;
    fiFifoInterrupt  : in std_logic_vector(kFifoInterruptLength-1 downto 0);
    
    
    lvStopSim : out std_logic := '0';
    lvCommand : out natural;
    lvAddress : out natural;
    lvWrData  : out std_logic_vector(tData'range);
    lvTransactionIdReq : out  natural;
    lvTransactionIdResp : in natural;
    lvNumOfDataPhases : out natural;
    lvTimeout : out integer;
    lvTimedOut : in std_logic;
    lvFifoRemaining : in natural;
    lvAttribute : out integer;
    lvRequestPush : out std_logic;
    lvRequestReady : in std_logic;
    lvRdData  : in std_logic_vector(tData'range);
    lvRdDataValid : in std_logic;
    lvResponsePop : out std_logic
  );
end entity NiFpgaSimFliInterface;

architecture only of NiFpgaSimFliInterface is
    attribute foreign : string;
    attribute foreign of only : architecture is "simulation_init {C:/Program Files (x86)/National Instruments/LabVIEW 2012/vi.lib/rvi/Simulation/modelsimLscClient.dll}";
begin
end architecture only;
