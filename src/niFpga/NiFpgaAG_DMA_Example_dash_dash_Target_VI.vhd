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
use work.PkgRegister.all;
use work.PkgNiFpgaSimFxp.all;
use work.PkgNiFpgaSimInterfaceLvDataTypes.all;

entity NiFpgaAG_DMA_Example_dash_dash_Target_VI is
		port (
			reset : in std_logic;
			enable_in : in std_logic;
			enable_out : out std_logic;
			enable_clr : in std_logic;
			RioClk40 : in std_logic;
			bBusReset : in std_logic;
			bRegPortOut : out std_logic_vector(33 downto 0);
			bRegPortIn : in std_logic_vector(50 downto 0);
			bInputStreamInterfaceToFifo : in std_logic_vector(127 downto 0);
			bInputStreamInterfaceFromFifo : out std_logic_vector(1711 downto 0);
			bOutputStreamInterfaceToFifo : in std_logic_vector(1695 downto 0);
			bOutputStreamInterfaceFromFifo : out std_logic_vector(1135 downto 0);
			bIrqToInterface : out std_logic_vector(65 downto 0);
			ChinchClk : in std_logic;
			ReliableClkIn : in std_logic;
			aBusReset : in std_logic;
			bIoWtToEnSafeBusCrossing : in std_logic;
			rDiagramResetAssertionErrIn : in std_logic;
			rGatedBaseClkStartupErr : in std_logic;
			rDerivedClkStartupErr : in std_logic;
			rInternalClksValidIn : in std_logic;
			rEnableClksForViRunOut : out std_logic;
			bCommunicationTimeoutIn : in std_logic;
			rDiagramResetStatusIn : in std_logic;
			rDerivedClksValid : in std_logic;
			rDiagramResetIn : in std_logic;
			rDerivedClockLostLockError : out std_logic;
			TopLvClk : in std_logic;
			tDiagramEnableIn : out std_logic;
			tDiagramEnableClear : out std_logic;
			tDiagramEnableOut : in std_logic;
			rDiagramResetAssertionErrOut : out std_logic;
			rInternalClksValidOut : out std_logic;
			rEnableClksForViRunIn : in std_logic;
			rSafeToEnableGatedClks : out std_logic;
			rGatedBaseClksValid : in std_logic;
			rDiagramResetStatusOut : out std_logic;
			rDiagramResetOut : out std_logic;
			rDcmPllSourceClksValidOut : out std_logic;
			rBaseClksValid : in std_logic;
			aDiagramResetOut : out std_logic
		);
end NiFpgaAG_DMA_Example_dash_dash_Target_VI;

architecture vhdl_labview of NiFpgaAG_DMA_Example_dash_dash_Target_VI is

	constant c_0 : std_logic_vector(0 downto 0) := "0";
	signal output : std_logic_vector(31 downto 0) := "00000000000000000000000100000000"; 
	signal ei00000001 : std_logic;
	signal eo00000001 : std_logic;
	signal ei00000003 : std_logic;
	signal eo00000003 : std_logic;
	signal subdiag_enable_00000004 : std_logic;
	signal subdiag_done_00000004 : std_logic;
	signal subdiag_clear_00000004 : std_logic;
	signal lc00000002 : std_logic_vector(31 downto 0);
	signal li00000002 : std_logic;
	signal zeroIterationLoop00000002 : std_logic;
	signal res00000007_wi : std_logic_vector(49 downto 0);
	signal res00000007_wo : std_logic_vector(1 downto 0);
	signal res00000009_wi : std_logic_vector(1 downto 0);
	signal res00000009_wo : std_logic_vector(0 downto 0);
	signal fromArbToRes0000000d : std_logic_vector(33 downto 0);
	signal fromResToArb0000000d : std_logic_vector(50 downto 0);
	signal resholder00000000ToArb0000000d : std_logic_vector(33 downto 0);
	signal arb0000000dToResholder00000000 : std_logic_vector(50 downto 0);
	signal res00000010_wi : std_logic_vector(1 downto 0);
	signal res00000010_wo : std_logic_vector(32 downto 0);
	signal res00000012_wi : std_logic_vector(33 downto 0);
	signal res00000012_wo : std_logic_vector(0 downto 0);
	signal res00000014_wi : std_logic_vector(1 downto 0);
	signal res00000014_wo : std_logic_vector(32 downto 0);
	signal res00000016_wi : std_logic_vector(33 downto 0);
	signal res00000016_wo : std_logic_vector(0 downto 0);
	signal res00000018_wi : std_logic_vector(1 downto 0);
	signal res00000018_wo : std_logic_vector(128 downto 0);

begin
	n_NiFpgaAG_00000000_ForLoop_Diagram : entity work.NiFpgaAG_00000000_ForLoop
		port map(
			reset => reset,
			enable_in => ei00000001,
			enable_out => eo00000001,
			enable_clr => subdiag_clear_00000004,
			iteration => lc00000002,
			max => output,
			res_80000001 => res00000007_wo,
			res_80000002 => res00000007_wi,
			RioClk40 => RioClk40
		);

	n_forloop: entity work.forloop (rtl)
		generic map (
			kBitsNeededForCounter => 32,
			kDiagramWithInit => 0,
			kJustDiagram => 0,
			kCounterUnconnected => 0,
			kZeroIterations => 0,
			INIT_NEEDED => 0,
			WIDTH => 32,
			ANYINDEXARRAYS => 0,
			MINFIXEDARRAY => 0,
			kRegisterMode => 1
		)
		port map(
			Clk => RioClk40,
			reset => reset,
			enable_in => ei00000003,
			enable_out => eo00000003,
			enable_clr => enable_clr,
			subdiag_en => subdiag_enable_00000004,
			subdiag_done => subdiag_done_00000004,
			subdiag_clr => subdiag_clear_00000004,
			iteration => lc00000002,
			loopinit => li00000002,
			zeroLoopIterations => zeroIterationLoop00000002,
			max => output
		);

	n_InvisibleResholder : entity work.InvisibleResholder
		generic map(
			kbusholddummyWidthOut => 2,
			kbusholddummyWidthIn => 1
		)
		port map(
			Clk => RioClk40,
			reset => reset,
			ToResbusholddummy => res00000009_wi,
			FromResbusholddummy => res00000009_wo
		);

	n_Interface : entity work.Interface
		generic map(
			kDMAwFIFOWritePortWidthIn => 50,
			kDMAwFIFOWritePortWidthOut => 2,
			kMiteIoLikeWidthIn => 34,
			kMiteIoLikeWidthOut => 51
		)
		port map(
			ChinchClk => ChinchClk,
			RioClk40 => RioClk40,
			reset => reset,
			rioClk40DMAwFIFOWritePortFromReshold => res00000007_wi,
			rioClk40DMAwFIFOWritePortToReshold => res00000007_wo,
			chinchClkMiteIoLikeFromReshold => fromArbToRes0000000d,
			chinchClkMiteIoLikeToReshold => fromResToArb0000000d,
			bBusReset => bBusReset,
			bRegPortOut => bRegPortOut,
			bRegPortIn => bRegPortIn,
			bInputStreamInterfaceToFifo => bInputStreamInterfaceToFifo,
			bInputStreamInterfaceFromFifo => bInputStreamInterfaceFromFifo,
			bOutputStreamInterfaceToFifo => bOutputStreamInterfaceToFifo,
			bOutputStreamInterfaceFromFifo => bOutputStreamInterfaceFromFifo,
			bIrqToInterface => bIrqToInterface
		);

	n_bushold : entity work.bushold
		generic map(
			kdummyWidthIn => 2,
			kdummyWidthOut => 1,
			kInterfaceMiteIoLikeWidthOut => 34,
			kInterfaceMiteIoLikeWidthIn => 51,
			kViControlHostReadWidthOut => 2,
			kViControlHostReadWidthIn => 33,
			kViControlHostWriteWidthOut => 34,
			kViControlHostWriteWidthIn => 1,
			kDiagramResetHostReadWidthOut => 2,
			kDiagramResetHostReadWidthIn => 33,
			kDiagramResetHostWriteWidthOut => 34,
			kDiagramResetHostWriteWidthIn => 1,
			kViSignatureHostReadWidthOut => 2,
			kViSignatureHostReadWidthIn => 129
		)
		port map(
			ReliableClkIn => ReliableClkIn,
			ChinchClk => ChinchClk,
			reset => reset,
			dummyFromReshold => res00000009_wi,
			dummyToReshold => res00000009_wo,
			chinchClkToResInterfaceMiteIoLike => resholder00000000ToArb0000000d,
			chinchClkFromResInterfaceMiteIoLike => arb0000000dToResholder00000000,
			chinchClkToResViControlHostRead => res00000010_wi,
			chinchClkFromResViControlHostRead => res00000010_wo,
			chinchClkToResViControlHostWrite => res00000012_wi,
			chinchClkFromResViControlHostWrite => res00000012_wo,
			chinchClkToResDiagramResetHostRead => res00000014_wi,
			chinchClkFromResDiagramResetHostRead => res00000014_wo,
			chinchClkToResDiagramResetHostWrite => res00000016_wi,
			chinchClkFromResDiagramResetHostWrite => res00000016_wo,
			chinchClkToResViSignatureHostRead => res00000018_wi,
			chinchClkFromResViSignatureHostRead => res00000018_wo,
			aBusReset => aBusReset
		);

	ViControlx : entity work.ViControl (rtl) 
   generic map (
      kHostReadWidthIn => 2,
      kHostReadWidthOut => 33,
      kHostWriteWidthIn => 34,
      kHostWriteWidthOut => 1,
      kAutoRun => work.PkgCommIntConfiguration.kAutoRun,
      kInitDuration => 0,
      kAllowEnableRemoval => false
  )

  port map (
      ReliableClk => ReliableClkIn,
      BusClk => ChinchClk,
      aDiagramReset => to_Boolean(reset),
      bHostReadIn => res00000010_wi,
      bHostReadOut => res00000010_wo,
      bHostWriteIn => res00000012_wi,
      bHostWriteOut => res00000012_wo,
      bBusReset => to_Boolean(bBusReset),
      aBusReset => to_Boolean('0'),
      bIoWtToEnSafeBusCrossing => to_Boolean(bIoWtToEnSafeBusCrossing),
      rDiagramResetAssertionErr => to_Boolean(rDiagramResetAssertionErrIn),
      rGatedBaseClkStartupErr => to_Boolean(rGatedBaseClkStartupErr),
      rDerivedClkStartupErr => to_Boolean(rDerivedClkStartupErr),
      rInternalClksValid => to_Boolean(rInternalClksValidIn),
      rEnableClksForViRun => rEnableClksForViRunOut,
      bCommunicationTimeout => to_Boolean(bCommunicationTimeoutIn),
      rDiagramResetStatus => to_Boolean(rDiagramResetStatusIn),
      rDerivedClksValid => to_Boolean(rDerivedClksValid),
      rDiagramReset => to_Boolean(rDiagramResetIn),
      rDerivedClockLostLockError => rDerivedClockLostLockError,
      TopLvClk => TopLvClk,
      tDiagramEnableIn => tDiagramEnableIn,
      tDiagramEnableClear => tDiagramEnableClear,
      tDiagramEnableOut => tDiagramEnableOut
  );

	DiagramResetx : entity work.DiagramReset (rtl) 
   generic map (
      kHostReadWidthIn => 2,
      kHostReadWidthOut => 33,
      kHostWriteWidthIn => 34,
      kHostWriteWidthOut => 1,
      kDiagRstDeAsrtPropDlyWait => 130,
      kDiagRstAssertionDuration => 0,
      kAllowEnableRemoval => false
  )

  port map (
      ReliableClk => ReliableClkIn,
      BusClk => ChinchClk,
      bHostReadIn => res00000014_wi,
      bHostReadOut => res00000014_wo,
      bHostWriteIn => res00000016_wi,
      bHostWriteOut => res00000016_wo,
      aBusReset => to_Boolean('0'),
      bIoWtToEnSafeBusCrossing => to_Boolean(bIoWtToEnSafeBusCrossing),
      rDerivedClksValid => to_Boolean(rDerivedClksValid),
      rDiagramResetAssertionErr => rDiagramResetAssertionErrOut,
      rInternalClksValid => rInternalClksValidOut,
      rEnableClksForViRun => to_Boolean(rEnableClksForViRunIn),
      rSafeToEnableGatedClks => rSafeToEnableGatedClks,
      rGatedBaseClksValid => to_Boolean(rGatedBaseClksValid),
      rDiagramResetStatus => rDiagramResetStatusOut,
      rDiagramReset => rDiagramResetOut,
      rDcmPllSourceClksValid => rDcmPllSourceClksValidOut,
      rBaseClksValid => to_Boolean(rBaseClksValid),
      aDiagramReset => aDiagramResetOut
  );

	n_ViSignature : entity work.ViSignature
		generic map(
			kHostReadWidthIn => 2,
			kHostReadWidthOut => 129
		)
		port map(
			Clk => ChinchClk,
			reset => reset,
			clkHostReadFromReshold => res00000018_wi,
			clkHostReadToReshold => res00000018_wo
		);

	n_CustomArbForMiteIoLikePortOnResInterface : entity work.CustomArbForMiteIoLikePortOnResInterface
		generic map(
			kNumResholders => 1,
			kResWidthIn => 34,
			kResWidthOut => 51,
			kResName => "Interface",
			kResPortName => "MiteIoLike"
		)
		port map(
			Clk => ChinchClk,
			reset => reset,
			interfaceClockToRes => fromArbToRes0000000d,
			interfaceClockFromRes => fromResToArb0000000d,
			interfaceClockFromResholder00000000 => resholder00000000ToArb0000000d,
			interfaceClockToResholder00000000 => arb0000000dToResholder00000000
		);

	subdiag_done_00000004 <= eo00000001;
	ei00000001 <= subdiag_enable_00000004;
	ei00000003 <= enable_in;
	enable_out <= eo00000003;

end vhdl_labview;


