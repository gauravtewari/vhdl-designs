-- © 2012 National Instruments Corporation.
--Use of this file is subject to the Software License Agreement provided with 
--LabVIEW FPGA Module and, without limiting any of the provisions in that license, 
--modifying or distributing this file is prohibited.

--Adding to, deleting from, or otherwise modifying the contents of this file 
--may cause the software to malfunction.
-------------------------------------------------------------------------------
--
-- Purpose:
-- This package contains operators for arithmetic operations
-- including addition, subtraction, multiplication, etc. See the function
-- definitions for more details.
--
-- Several functions, including 'Add', 'Subtract', and 'Multiply',
-- use inputs and outputs to perform operations in a highly
-- optimized way. These functions return values correctly rounded
-- with overflow status.
--
-- Some functions have a variation that includes a 'Trp' extension. These
-- functions include additional parameters for tight-range propogation
-- constants that you use with saturating arithmetic.
--
-------------------------------------------------------------------------------

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.PkgNiFpgaSimFxp.all;
use work.PkgNiUtilities.all;
use work.PkgNiFpgaUtilities.all;

package PkgNiFpgaSimFxpArithmetic is

  -----------------------------------------------------------------------------
  -- Resize
  -----------------------------------------------------------------------------

  -- The following resize functions are the hammer when it comes to resizing.
  -- They handle overflow and quantization with all the supported modes and
  -- configurations. These functions are used often and are highly optimized.

  -- There are two flavors of the resize functions to allow them to be called
  -- from within a function or procedure and from an entity using signals. The
  -- 'ResizeVar' versions take variable inputs for use within other funcitons
  -- and procedures. The 'Resize' versions are for use from entities outside
  -- the fixed-point package and take signal inputs.

  -----------------------------------------------------------------------------
  -- Variable Based Procedures

  procedure ResizeTrpVar (
    constant kInSigned      : in  boolean;
    variable vIn            : in  tFxpGen;
    constant kOutSigned     : in  boolean;
    variable vOut           : out tFxpGen;
    variable vOverflow      : out boolean;
    constant kOverflowMode  : in  FxpOverflowMode_t;
    constant kRoundMode     : in  FxpRoundMode_t;
    constant kSatUpperLimit : in  tFxpGen;
    constant kSatLowerLimit : in  tFxpGen);

  procedure ResizeVar (
    constant kInSigned     : in  boolean;
    variable vIn           : in  tFxpGen;
    constant kOutSigned    : in  boolean;
    variable vOut          : out tFxpGen;
    variable vOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t);

  procedure ResizeVar (
    variable vIn           : in  tFxpUns;
    variable vOut          : out tFxpUns;
    variable vOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t);

  procedure ResizeVar (
    variable vIn           : in  tFxpSgn;
    variable vOut          : out tFxpUns;
    variable vOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t);

  procedure ResizeVar (
    variable vIn           : in  tFxpUns;
    variable vOut          : out tFxpSgn;
    variable vOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t);

  procedure ResizeVar (
    variable vIn           : in  tFxpSgn;
    variable vOut          : out tFxpSgn;
    variable vOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t);

  -----------------------------------------------------------------------------
  -- Signal Based Wrappers

  -- Because this procedure has two outputs, we had the choice to either do
  -- some magic with the return type and make it more like a function to handle
  -- the signal/variable conflicts or create different functions based on
  -- variables and then create wrappers to handle the signal case. We decided
  -- to go with the second as it seemed to keep the interfaces and conversion
  -- functions to a minimum.

  procedure ResizeTrp (
    constant kInSigned      : in  boolean;
    signal   sIn            : in  tFxpGen;
    constant kOutSigned     : in  boolean;
    signal   sOut           : out tFxpGen;
    signal   sOverflow      : out boolean;
    constant kOverflowMode  : in  FxpOverflowMode_t;
    constant kRoundMode     : in  FxpRoundMode_t;
    constant kSatUpperLimit : in  tFxpGen;
    constant kSatLowerLimit : in  tFxpGen);

  procedure Resize (
    constant kInSigned     : in  boolean;
    signal   sIn           : in  tFxpGen;
    constant kOutSigned    : in  boolean;
    signal   sOut          : out tFxpGen;
    signal   sOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t);

  procedure Resize (
    signal   sIn           : in  tFxpUns;
    signal   sOut          : out tFxpUns;
    signal   sOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t);

  procedure Resize (
    signal   sIn           : in  tFxpSgn;
    signal   sOut          : out tFxpUns;
    signal   sOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t);

  procedure Resize (
    signal   sIn           : in  tFxpUns;
    signal   sOut          : out tFxpSgn;
    signal   sOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t);

  procedure Resize (
    signal   sIn           : in  tFxpSgn;
    signal   sOut          : out tFxpSgn;
    signal   sOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t);

  -----------------------------------------------------------------------------
  -- Arithmetic Operators
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Sizing Functions

  -- The following function can be used to determine the output size of a given
  -- operation. Basically, call the function with the two input operands and
  -- the operations symbol and it will return the iwl, fwl, and sign of the
  -- resulting number.

  type OutputDesc_t is record
    kSigned : boolean;
    kIwl    : integer;
    kFwl    : integer;
  end record;

  function DescribeOutput (
    kInOneSigned : boolean;
    vInOne       : tFxpGen;
    kOperator    : character;
    kInTwoSigned : boolean;
    vInTwo       : tFxpGen)
    return OutputDesc_t;

  function DescribeOutput (
    vInOne    : tFxpUns;
    kOperator : character;
    vInTwo    : tFxpUns)
    return OutputDesc_t;

  function DescribeOutput (
    vInOne    : tFxpSgn;
    kOperator : character;
    vInTwo    : tFxpUns)
    return OutputDesc_t;

  function DescribeOutput (
    vInOne    : tFxpUns;
    kOperator : character;
    vInTwo    : tFxpSgn)
    return OutputDesc_t;

  function DescribeOutput (
    vInOne    : tFxpSgn;
    kOperator : character;
    vInTwo    : tFxpSgn)
    return OutputDesc_t;

  -----------------------------------------------------------------------------
  -- Addition

  -- The tFxpGen addition operator is provided as a shortcut for
  -- basic unsigned addition. No sign-extension or overflow handling
  -- is performed.  The more specific unsigned and signed versions
  -- will increase the size of the output to avoid overflow.

  function "+" (x : tFxpGen; y : tFxpGen) return tFxpGen;

  function "+" (x : tFxpUns; y : tFxpUns) return tFxpUns;
  function "+" (x : tFxpSgn; y : tFxpUns) return tFxpSgn;
  function "+" (x : tFxpUns; y : tFxpSgn) return tFxpSgn;
  function "+" (x : tFxpSgn; y : tFxpSgn) return tFxpSgn;

  procedure AddTrpVar (
    constant kInOneSigned   : in  boolean;
    variable vInOne         : in  tFxpGen;
    constant kInTwoSigned   : in  boolean;
    variable vInTwo         : in  tFxpGen;
    constant kOutSigned     : in  boolean;
    variable vOut           : out tFxpGen;
    variable vOverflow      : out boolean;
    constant kRoundMode     : in  FxpRoundMode_t;
    constant kOverflowMode  : in  FxpOverflowMode_t;
    constant kSatUpperLimit : in  tFxpGen;
    constant kSatLowerLimit : in  tFxpGen);

  procedure AddVar (
    constant kInOneSigned  : in  boolean;
    variable vInOne        : in  tFxpGen;
    constant kInTwoSigned  : in  boolean;
    variable vInTwo        : in  tFxpGen;
    constant kOutSigned    : in  boolean;
    variable vOut          : out tFxpGen;
    variable vOverflow     : out boolean;
    constant kRoundMode    : in  FxpRoundMode_t;
    constant kOverflowMode : in  FxpOverflowMode_t);

  procedure AddTrp (
    constant kInOneSigned   : in  boolean;
    signal   sInOne         : in  tFxpGen;
    constant kInTwoSigned   : in  boolean;
    signal   sInTwo         : in  tFxpGen;
    constant kOutSigned     : in  boolean;
    signal   sOut           : out tFxpGen;
    signal   sOverflow      : out boolean;
    constant kRoundMode     : in  FxpRoundMode_t;
    constant kOverflowMode  : in  FxpOverflowMode_t;
    constant kSatUpperLimit : in  tFxpGen;
    constant kSatLowerLimit : in  tFxpGen);

  procedure Add (
    constant kInOneSigned  : in  boolean;
    signal   sInOne        : in  tFxpGen;
    constant kInTwoSigned  : in  boolean;
    signal   sInTwo        : in  tFxpGen;
    constant kOutSigned    : in  boolean;
    signal   sOut          : out tFxpGen;
    signal   sOverflow     : out boolean;
    constant kRoundMode    : in  FxpRoundMode_t;
    constant kOverflowMode : in  FxpOverflowMode_t);

  -----------------------------------------------------------------------------
  -- Subtraction

  function "-" (x : tFxpGen; y : tFxpGen) return tFxpGen;

  function "-" (x : tFxpUns; y : tFxpUns) return tFxpUns;
  function "-" (x : tFxpSgn; y : tFxpUns) return tFxpSgn;
  function "-" (x : tFxpUns; y : tFxpSgn) return tFxpSgn;
  function "-" (x : tFxpSgn; y : tFxpSgn) return tFxpSgn;

  procedure SubtractTrpVar (
    constant kInOneSigned   : in  boolean;
    variable vInOne         : in  tFxpGen;
    constant kInTwoSigned   : in  boolean;
    variable vInTwo         : in  tFxpGen;
    constant kOutSigned     : in  boolean;
    variable vOut           : out tFxpGen;
    variable vOverflow      : out boolean;
    constant kRoundMode     : in  FxpRoundMode_t;
    constant kOverflowMode  : in  FxpOverflowMode_t;
    constant kSatUpperLimit : in  tFxpGen;
    constant kSatLowerLimit : in  tFxpGen);

  procedure SubtractVar (
    constant kInOneSigned  : in  boolean;
    variable vInOne        : in  tFxpGen;
    constant kInTwoSigned  : in  boolean;
    variable vInTwo        : in  tFxpGen;
    constant kOutSigned    : in  boolean;
    variable vOut          : out tFxpGen;
    variable vOverflow     : out boolean;
    constant kRoundMode    : in  FxpRoundMode_t;
    constant kOverflowMode : in  FxpOverflowMode_t);

  procedure SubtractTrp (
    constant kInOneSigned   : in  boolean;
    signal   sInOne         : in  tFxpGen;
    constant kInTwoSigned   : in  boolean;
    signal   sInTwo         : in  tFxpGen;
    constant kOutSigned     : in  boolean;
    signal   sOut           : out tFxpGen;
    signal   sOverflow      : out boolean;
    constant kRoundMode     : in  FxpRoundMode_t;
    constant kOverflowMode  : in  FxpOverflowMode_t;
    constant kSatUpperLimit : in  tFxpGen;
    constant kSatLowerLimit : in  tFxpGen);

  procedure Subtract (
    constant kInOneSigned  : in  boolean;
    signal   sInOne        : in  tFxpGen;
    constant kInTwoSigned  : in  boolean;
    signal   sInTwo        : in  tFxpGen;
    constant kOutSigned    : in  boolean;
    signal   sOut          : out tFxpGen;
    signal   sOverflow     : out boolean;
    constant kRoundMode    : in  FxpRoundMode_t;
    constant kOverflowMode : in  FxpOverflowMode_t);

  -----------------------------------------------------------------------------
  -- Multiplication

  -- Standard multiplication operations. The output type for the
  -- unsigned/unsigned and signed/signed cases will be
  -- < x'iwl+y'iwl-2 downto -(x'fwl+y'fwl) >

  function "*" (x : tFxpUns; y : tFxpUns) return tFxpUns;
  function "*" (x : tFxpUns; y : tFxpSgn) return tFxpSgn;
  function "*" (x : tFxpSgn; y : tFxpUns) return tFxpSgn;
  function "*" (x : tFxpSgn; y : tFxpSgn) return tFxpSgn;

  procedure MultiplyTrpVar (
    constant kInOneSigned   : in  boolean;
    variable vInOne         : in  tFxpGen;
    constant kInTwoSigned   : in  boolean;
    variable vInTwo         : in  tFxpGen;
    constant kOutSigned     : in  boolean;
    variable vOut           : out tFxpGen;
    variable vOverflow      : out boolean;
    constant kRoundMode     : in  FxpRoundMode_t;
    constant kOverflowMode  : in  FxpOverflowMode_t;
    constant kSatUpperLimit : in  tFxpGen;
    constant kSatLowerLimit : in  tFxpGen);

  procedure MultiplyVar (
    constant kInOneSigned  : in  boolean;
    variable vInOne        : in  tFxpGen;
    constant kInTwoSigned  : in  boolean;
    variable vInTwo        : in  tFxpGen;
    constant kOutSigned    : in  boolean;
    variable vOut          : out tFxpGen;
    variable vOverflow     : out boolean;
    constant kRoundMode    : in  FxpRoundMode_t;
    constant kOverflowMode : in  FxpOverflowMode_t);

  procedure MultiplyTrp (
    constant kInOneSigned   : in  boolean;
    signal   sInOne         : in  tFxpGen;
    constant kInTwoSigned   : in  boolean;
    signal   sInTwo         : in  tFxpGen;
    constant kOutSigned     : in  boolean;
    signal   sOut           : out tFxpGen;
    signal   sOverflow      : out boolean;
    constant kRoundMode     : in  FxpRoundMode_t;
    constant kOverflowMode  : in  FxpOverflowMode_t;
    constant kSatUpperLimit : in  tFxpGen;
    constant kSatLowerLimit : in  tFxpGen);

  procedure Multiply (
    constant kInOneSigned  : in  boolean;
    signal   sInOne        : in  tFxpGen;
    constant kInTwoSigned  : in  boolean;
    signal   sInTwo        : in  tFxpGen;
    constant kOutSigned    : in  boolean;
    signal   sOut          : out tFxpGen;
    signal   sOverflow     : out boolean;
    constant kRoundMode    : in  FxpRoundMode_t;
    constant kOverflowMode : in  FxpOverflowMode_t);

  -----------------------------------------------------------------------------
  -- Basic Operations
  -----------------------------------------------------------------------------

  function Invert (arg : tFxpGen) return tFxpGen;
  function Invert (arg : tFxpUns) return tFxpUns;
  function Invert (arg : tFxpSgn) return tFxpSgn;

  -----------------------------------------------------------------------------

  -- Negate - Negates the given operand. Unsigned numbers are converted to
  -- signed numbers and then inverted causing the word-length to grow by 2
  -- instead of 1.

  function Negate (
    kArgSigned : boolean;
    arg        : tFxpGen;
    kIncLength : boolean := true)
    return tFxpGen;

  function Negate (arg : tFxpUns) return tFxpSgn;
  function Negate (arg : tFxpSgn) return tFxpSgn;

  -----------------------------------------------------------------------------
  -- Shift Operations
  -----------------------------------------------------------------------------

  -- The following shift operators can take either a constant shift or a
  -- variable shift. The variable shift results in a large block of hardware.
  -- Logical shifts are only defined for unsigned numbers and arithmetic shifts
  -- are only defined for signed numbers. Cast from one to the other if you
  -- need the undefined behaviors. The generic versions will treat the number
  -- as unsigned for logical shifts and signed for arithmetic shifts.

  function "sll" (x : tFxpGen; n : natural) return tFxpGen;
  function "sll" (x : tFxpUns; n : natural) return tFxpUns;

  function "srl" (x : tFxpGen; n : natural) return tFxpGen;
  function "srl" (x : tFxpUns; n : natural) return tFxpUns;

  function "sla" (x : tFxpGen; n : natural) return tFxpGen;
  function "sla" (x : tFxpSgn; n : natural) return tFxpSgn;

  function "sra" (x : tFxpGen; n : natural) return tFxpGen;
  function "sra" (x : tFxpSgn; n : natural) return tFxpSgn;

  function "rol" (x : tFxpGen; n : natural) return tFxpGen;
  function "rol" (x : tFxpUns; n : natural) return tFxpUns;
  function "rol" (x : tFxpSgn; n : natural) return tFxpSgn;

  function "ror" (x : tFxpGen; n : natural) return tFxpGen;
  function "ror" (x : tFxpUns; n : natural) return tFxpUns;
  function "ror" (x : tFxpSgn; n : natural) return tFxpSgn;

end PkgNiFpgaSimFxpArithmetic;

package body PkgNiFpgaSimFxpArithmetic is

  -----------------------------------------------------------------------------
  -- Resize
  -----------------------------------------------------------------------------

  function RoundTruncate (
    kInSigned : boolean;
    vIn       : tFxpGen;
    vSizeType : tFxpGen)
    return tFxpGen is
  begin

    -- For truncation, we can simply lop off bits and just extend the
    -- sign bit based on the input.

    return SimpleResize(kInSigned, vIn, vSizeType'left+1, -vSizeType'right);

  end function RoundTruncate;

  function RoundToNearest (
    kInSigned : boolean;
    vIn       : tFxpGen;
    vSizeType : tFxpGen)
    return tFxpGen
  is
    -- Add in a single bit which is zero-extended to the length of the output type.
    variable vBitForAdd : tFxpGen(vSizeType'left+1 downto vSizeType'right) := (others => '0');
  begin

    if vIn'right >= vSizeType'right then

      -- No need to do any rounding since the type we want has more
      -- fractional bits than the input type. Just do a truncation
      -- (which does a resize).

      return RoundTruncate(kInSigned, vIn, vSizeType);

    else

      -- For this case, we simply need to add one in the one-half LSB
      -- location to correctly round. We may overflow. The exact half
      -- case for round to nearest is simply rounded up to the next
      -- value.

      vBitForAdd(vSizeType'right) := vIn(vSizeType'right-1);

      return SimpleResize(
        kInSigned,
        SimpleResize(kInSigned, vIn, vSizeType'left+1, -vSizeType'right) + vBitForAdd,
        vSizeType'left+1,
        -vSizeType'right
        );

    end if;

  end function RoundToNearest;

  function RoundToPosInf (
    kInSigned : boolean;
    vIn       : tFxpGen;
    vSizeType : tFxpGen)
    return tFxpGen
  is

    -- Add in a single bit which is zero-extended to the length of the output type.
    variable vBitForAdd : tFxpGen(vSizeType'left+1 downto vSizeType'right) := (others => '0');

    constant kLeftIdx : integer := Minimum(vSizeType'right-1, vIn'left);

  begin

    if vIn'right >= vSizeType'right then

      -- No need to do any rounding since the type we want has more
      -- fractional bits than the input type.

      return RoundTruncate(kInSigned, vIn, vSizeType);

    else

      -- For this case, we simply need to add one in the one-half LSB
      -- location to correctly round. We may overflow.

      vBitForAdd(vSizeType'right) := to_stdlogic(not IsZero(vIn(kLeftIdx downto vIn'right)));

      return SimpleResize(
        kInSigned,
        SimpleResize(kInSigned, vIn, vSizeType'left+1, -vSizeType'right) + vBitForAdd,
        vSizeType'left+1,
        -vSizeType'right
        );

    end if;

  end function RoundToPosInf;

  -----------------------------------------------------------------------------
  -- Quantization Helpers

  type QuantDesc_t is record
    kRoundingNeeded : boolean;
    kSigned        : boolean;
    kIwl, kFwl     : integer;
  end record;

  function GetQuantInfo (
    kInSigned        : boolean;
    kInIwl, kInFwl   : integer;
    kOutSigned       : boolean;
    kOutIwl, kOutFwl : integer;
    kRoundMode       : FxpRoundMode_t)
    return QuantDesc_t
  is
    variable vResult            : QuantDesc_t;
    variable vOverflowBitNeeded : boolean := true;
  begin

    -- We only need to round if the fractional length of input is
    -- greater than the fractional length of the output. In addition,
    -- for some rounding modes, we only have to round if the integer
    -- word length of the input is at least bumping the lsb of the
    -- output vector. If there is a gap, we can always truncate.

    if kRoundMode = kRoundToPosInf then
      vResult.kRoundingNeeded := kInFwl > kOutFwl;
    else
      vResult.kRoundingNeeded := kInFwl > kOutFwl and (kInSigned or kInIwl >= -kOutFwl);
    end if;

    -- Since we can overflow with quantization, we may have to add one
    -- extra upper-level bit.

    if vResult.kRoundingNeeded then
      case kRoundMode is
        when kNotNeeded | kTruncate => vOverflowBitNeeded := false;
        when others                 => vOverflowBitNeeded := true;
      end case;
    end if;

    -- Finalize our quantization type. We must ensure we have at least
    -- one bit in the quantization value and have the extra overflow
    -- control bit if needed.

    vResult.kSigned := kInSigned;
    vResult.kIwl    := Maximum(kInIwl, 1-kOutFwl) + choose(vOverflowBitNeeded, 1, 0);
    vResult.kFwl    := kOutFwl;

    -- Make sure quantized value has at least one bit.

    if not (vResult.kIwl > -vResult.kFwl) then
      ReportError
       (ErrorType => ExternalError,
        ShortDescription => "Quantization vector must be at least one bit in length.");
    end if;

    return vResult;

  end function GetQuantInfo;

  ---------------------------------------------------------------------------
  -- Saturation limits.

  -- Its possible the user doesn't want to saturate at the absolute
  -- maximal and minimal values that can be contained by the bits
  -- we're allocated. This may be because the user wants to specify
  -- a tighter range on input values to a block of code so the
  -- ranges can be tightly contained. We only support this tight
  -- range checking in saturation mode and we only want to
  -- instantiate the comparators if the values are actually
  -- different from the maximal and minimal values. The following
  -- constants help us figure this out.
  --
  -- For simplicities sake, we'll create signed versions of the
  -- saturation limits to make the overflow handling code a bit
  -- simpler.

  function IsLimitSpecified (
    kOutSigned       : boolean;
    kOutIwl, kOutFwl : integer;
    kLimit           : tFxpGen;
    kLimitType       : SaturateLimit_t;
    kOverflowMode    : FxpOverflowMode_t)
    return boolean
  is
    constant kEdgeLimit : tFxpGen := Saturate(kOutSigned, kOutIwl, kOutFwl, kLimitType);
    variable vRet       : boolean  := false;
  begin

    if kOverflowMode = kSaturate then
      vRet := kLimit /= kEdgeLimit;
    end if;

    return vRet;

  end IsLimitSpecified;

  function GetLimit (
    kOutSigned : boolean;
    kLimit     : tFxpGen;
    kLimitType : SaturateLimit_t)
    return tFxpSgn
  is
    constant kIwlOffset : integer := choose(kOutSigned, 0, 1);
    variable vRet       : tFxpSgn(kLimit'left+kIwlOffset downto kLimit'right);
  begin
    if kOutSigned then
      vRet := tFxpSgn(kLimit);
    else
      vRet := to_FxpSgn(tFxpUns(kLimit));
    end if;
    return vRet;
  end GetLimit;

  -----------------------------------------------------------------------------
  -- Variable Based Procedures

  procedure ResizeTrpVar (
    constant kInSigned      : in  boolean;
    variable vIn            : in  tFxpGen;
    constant kOutSigned     : in  boolean;
    variable vOut           : out tFxpGen;
    variable vOverflow      : out boolean;
    constant kOverflowMode  : in  FxpOverflowMode_t;
    constant kRoundMode     : in  FxpRoundMode_t;
    constant kSatUpperLimit : in  tFxpGen;
    constant kSatLowerLimit : in  tFxpGen)
  is

    constant kInIwl : integer := vIn'left+1;
    constant kInFwl : integer := -vIn'right;

    constant kOutIwl : integer := vOut'left+1;
    constant kOutFwl : integer := -vOut'right;

    -- Saturation Limit Info --------------------------------------------------

    constant kUpperLimitSpecified : boolean := IsLimitSpecified(kOutSigned, kOutIwl, kOutFwl, kSatUpperLimit, kUpper, kOverflowMode);
    constant kLowerLimitSpecified : boolean := IsLimitSpecified(kOutSigned, kOutIwl, kOutFwl, kSatLowerLimit, kLower, kOverflowMode);

    subtype SgnLimit_t is tFxpSgn(vOut'left+choose(kOutSigned,0,1) downto vOut'right);

    constant kUpperLimit : SgnLimit_t := GetLimit(kOutSigned, kSatUpperLimit, kUpper);
    constant kLowerLimit : SgnLimit_t := GetLimit(kOutSigned, kSatLowerLimit, kLower);

    -- Intermediate quantized values ------------------------------------------

    constant kQuantDesc : QuantDesc_t :=
      GetQuantInfo(
        kInSigned, kInIwl, kInFwl,
        kOutSigned, kOutIwl, kOutFwl,
        kRoundMode
        );

    constant kQuantSigned : boolean := kQuantDesc.kSigned;
    constant kQuantIwl    : integer := kQuantDesc.kIwl;
    constant kQuantFwl    : integer := kQuantDesc.kFwl;

    variable vQuant : tFxpGen(kQuantDesc.kIwl-1 downto -kQuantDesc.kFwl);

    variable vQuantSgn : tFxpSgn(vQuant'range);  -- Temporary signed version
    variable vQuantUns : tFxpUns(vQuant'range);  -- Temporary unsigned version

    -- If we need to quantize the input, we need to make sure that at
    -- least one bit of the input overlaps with the quantize vector or
    -- else we can get null vectors in the intermediate results.

    variable vInLoc : tFxpGen(Maximum(kInIwl-1, -kQuantFwl) downto -kInFwl);

    -- Overflow variables -----------------------------------------------------

    -- vOverflowHi is set when the value being represented by the
    -- output is smaller than quantized result. vOverflowLo is set
    -- when the output value ends up being larger than the quantized
    -- result.

    variable vOverflowHi : boolean := false;
    variable vOverflowLo : boolean := false;

    variable vOutLoc : tFxpGen(vOut'range);
  begin

    CheckInvariant(vIn);

    ---------------------------------------------------------------------------
    -- Handle Quantization
    -----------------------------------------------------------------------------

    if not kQuantDesc.kRoundingNeeded then

      -- Input has less fractional bit than the output signal. In this
      -- case, we have to optionally sign extend the input and pad
      -- zeroes to the right.
      --
      --    ?----IN----|>>>|   ?----IN---|
      --    ?------OUT-----|               |---OUT---|
      --

      vQuant := SimpleResize(kInSigned, vIn, kQuantIwl, kQuantFwl);

    elsif -kOutFwl > kInIwl then

      -- The input is completely disjoint from below the output.
      --
      --               |----IN----|
      --   ?---OUT---|
      --

      case kRoundMode is

        when kTruncate | kNotNeeded =>

          vQuant := RoundTruncate(kInSigned, vIn, vQuant);

        when kRoundToNearest | kConvergent =>

          vQuant := (others => '0');

        when kRoundToPosInf =>

          vQuant := RoundToPosInf(kInSigned, vIn, vQuant);

        when others =>

          ReportError
           (ErrorType => ExternalError,
            ShortDescription => "Unknown rounding mode " &
                                FxpRoundMode_t'image(kRoundMode) & " found in Resize.");

      end case;

    else

      -- We've calculated that rounding is required, so let's do
      -- it. To get here, for all but the round to + infinity mode,
      -- the input has more fractional bits than the output and is
      -- either signed or at least butting up against the output
      -- type. For round to + infinity, we have to round even if the
      -- types are completely disjoint because if any bits are set we
      -- need to round up.
      --
      --      ?------IN------|              |----IN----|
      --    ?------OUT-----|      ?---OUT---|
      --

      case kRoundMode is

        when kTruncate | kNotNeeded =>

          vQuant := RoundTruncate(kInSigned, vIn, vQuant);

        when kRoundToNearest =>

          vQuant := RoundToNearest(kInSigned, vIn, vQuant);

        when kConvergent =>

          -- This one is a bit hairy. Here, we need to check and see
          -- if an addition is actually needed. If any bits below
          -- one-half LSB are set, we want to round regularly (round
          -- to nearest). However, if all the bits after one-half LSB
          -- are zeros (exactly half) and the LSB is zero, we want to
          -- truncate.

          -- <-K-> <-R-> -> Bits to keep and those to round
          -- XXXX0 0XXXX -> Truncate (Round to Nearest, Truncate)
          -- XXXX0 10000 -> Truncate (Truncate) *** Special case ***
          -- XXXX0 1XXXX -> Round Up (Round to Nearest)
          -- XXXX1 0XXXX -> Truncate (Round to Nearest, Truncate)
          -- XXXX1 1XXXX -> Round UP (Round to Nearest)

          -- To handle the case where the input is butting up against
          -- the output vector, we need to sign extend it by one bit
          -- to get it to overlap with the quantized vector by at
          -- least one bit so we can do the rounding properly which is
          -- exactly what vInLoc is for.

          vInLoc := SignExtend(kInSigned, vIn, vInLoc'left+1);

          -- The if and elsif cases below could be condensed into just
          -- "if vInLoc(-kQuantFwl) = '0' and IsExactlyHalf(vInLoc(-kQuantFwl-1 downto -kInFwl)) then"
          -- to catch the '*** Special case ***' described above.
          if not kInSigned and -kQuantFwl >= kInIwl and IsExactlyHalf(vIn(-kQuantFwl-1 downto -kInFwl)) then

            vQuant := RoundTruncate(kInSigned, vInLoc, vQuant);

          elsif (kInSigned or -kQuantFwl < kInIwl) and vInLoc(-kQuantFwl) = '0' and IsExactlyHalf(vInLoc(-kQuantFwl-1 downto -kInFwl)) then

            vQuant := RoundTruncate(kInSigned, vInLoc, vQuant);

          else

            vQuant := RoundToNearest(kInSigned, vInLoc, vQuant);

          end if;

        when kRoundToPosInf =>

          vQuant := RoundToPosInf(kInSigned, vIn, vQuant);

        when others =>

          ReportError
           (ErrorType => ExternalError,
            ShortDescription => "Unknown rounding mode " &
                                FxpRoundMode_t'image(kRoundMode) & " found in Resize.");

      end case;
    end if;

    CheckInvariant(vQuant);

    ---------------------------------------------------------------------------
    -- Handle Overflow
    -----------------------------------------------------------------------------

    -- This section handles overflow from the input to the output
    -- according to the overflow mode given. The low-order bits of
    -- vQuant and vOutLoc are aligned and need not be worried about.
    --
    -- NOTE: The overflow indicator bits default to false and need
    -- only be overwritten when its possible for them to be set.

    if kOverflowMode = kNotNeeded then

      -------------------------------------------------------------------------
      -- The overflow bit is not needed, just set it to false and do a
      -- simple resize. No need to mess with the overflow bits.
      -------------------------------------------------------------------------

    elsif kOutIwl > kQuantIwl and not (kUpperLimitSpecified or kLowerLimitSpecified) then

      -------------------------------------------------------------------------
      -- In this case, the output is large enough to hold the
      -- quantized result.  The only way to overflow is if we go from
      -- a negative signed number to an unsigned number.
      --
      --    |<<<----Quant----|
      --    |------Out-------|
      --
      -------------------------------------------------------------------------

      if kQuantSigned and not kOutSigned then
        vOverflowLo := to_boolean(vQuant(vQuant'left));
      end if;

    elsif kOverflowMode = kWrap or not (kUpperLimitSpecified or kLowerLimitSpecified) then

      -------------------------------------------------------------------------
      -- For the rest of the case, the quantized result may require
      -- more room than is available in the output type. Because of
      -- this case, overflow is possible in any of the signed/unsigned
      -- combinations.
      --
      --    |>>>---Quant------|   |------Quant------|
      --        |-----Out-----|   |-------Out-------|
      --
      -- Within this block, we are in either wrap mode or we have
      -- determined that even if we're in saturation mode we don't
      -- actually need to look at the saturation limits given by the
      -- user (because their at the maximal and minimal limits).
      --
      -------------------------------------------------------------------------

      if kQuantIwl = kOutIwl then

        -- If the vectors are the same length, we can only overflow if
        -- the signs are of opposite direction and the top bit is
        -- set. If the quantized value is signed and the output is
        -- unsigned, we get negative overflow if the msb of quantized
        -- value is set. The opposite happens if the signs are
        -- reversed.

        vOverflowHi := not kQuantSigned and kOutSigned and vQuant(vQuant'left) = '1';
        vOverflowLo := kQuantSigned and not kOutSigned and vQuant(vQuant'left) = '1';

      elsif not kQuantSigned then

        -- The quantized result has at least one more MSB than the
        -- output vector.
        --
        -- The quantized result is unsigned, the only way we can
        -- overflow is in the positive direction. This can be checked
        -- by making sure all the bits above and including the sign
        -- bit of the output are zeros.

        vOverflowHi := not IsZero(vQuant(kQuantIwl-1 downto kOutIwl-choose(kOutSigned, 1, 0)));

      else

        -- The quantized result is signed, then we will overflow if
        -- the bits above and including the output sign bit are not
        -- all sign bits (sign-extended). In this case, its possible
        -- that we can overflow in either direction. To overflow in
        -- the positive direction, the sign bit of the quantized
        -- result must be zero and at least one other bit above the
        -- sign bit of the output must be set. Negative overflow is
        -- exactly opposite.
        --
        -- Notice we look at the MSB of the quantized value in both
        -- evaluations for each of the overflow bits. This ensures
        -- the second evaluation will never create a null vector.

        vOverflowHi :=
          vQuant(kQuantIwl-1) = '0' and
          not IsZero(vQuant(kQuantIwl-1 downto kOutIwl-choose(kOutSigned, 1, 0)));

        vOverflowLo :=
          vQuant(kQuantIwl-1) = '1' and
          (not kOutSigned or not IsAllOnes(vQuant(kQuantIwl-1 downto kOutIwl-1)));

      end if;

    else  -- saturating arithmetic with tight ranges ---------------------------

      -- We're doing saturating arithmetic and at least one of the
      -- saturation limits needs to be checked. Here, we need to take
      -- into account the saturation limits and make sure we only set
      -- the overflow bit if the output is actually less than the
      -- quantized result.

      if not kQuantSigned then

        -- The quantized result is unsigned. Unless we have user
        -- specified saturation limits (non maximal and minimal), we
        -- can only saturate in the upper limit direction.

        vQuantUns := tFxpUns(vQuant);

        if kUpperLimitSpecified then

          -- The saturation limits specified require us to do a full
          -- compare to see if we overflowed.

          if kUpperLimit < to_Fxp(0, kUpperLimit) then
            -- The quantized result is unsigned, but the saturation
            -- upper limit we're given is negative, therefore we have
            -- to overflow no matter what.
            vOverflowHi := true;
          else

            vOverflowHi := to_signed(vQuantUns) > to_signed(kUpperLimit);

          end if;

        elsif kQuantIwl > kOutIwl-choose(kOutSigned, 1, 0) then

          -- The quantized result has more bits than the output result
          -- that we need to check. We can only overflow in the
          -- positive direction.

          vOverflowHi := not IsZero(vQuant(kQuantIwl-1 downto kOutIwl-choose(kOutSigned, 1, 0)));

        end if;

        if kLowerLimitSpecified and kLowerLimit > to_Fxp(0, kLowerLimit) then

          -- If we're in saturation mode and the lower limit is
          -- greater than zero, there's a chance the quantized value
          -- could be less than the limit. Therefore, we need to check
          -- for this and saturate if need be.

          vOverflowLo := to_signed(vQuantUns) < to_signed(kLowerLimit);

        end if;

      else  -- quantized result is signed -------------------------------------

        -- Since the quantized result is signed, the output can
        -- overflow in both the upper and lower directions. If the
        -- output is signed, the input bits above iwl can be either
        -- all ones or all zeros without overflowing (an AND and OR
        -- tree).

        vQuantSgn := tFxpSgn(vQuant);

        if kUpperLimitSpecified then

          -- The user has specified an upper saturation limit that is
          -- not at the bounds of the output type. The quantized value
          -- is signed, therefore we have to compare it against the
          -- saturation upper limit because it may be anywhere.

          vOverflowHi := to_signed(vQuantSgn) > to_signed(kUpperLimit);

        elsif kQuantIwl > kOutIwl-choose(kOutSigned, 0, 1) then

          -- The quantized result has more bits than we need, check
          -- for overflow by making sure all the quantized bits above
          -- the outputs sign bit are zeros except for the sign bit of
          -- the quantized number.

          vOverflowHi :=
            vQuant(kQuantIwl-1) = '0' and
            not IsZero(vQuant(kQuantIwl-1 downto kOutIwl-choose(kOutSigned, 1, 0)));

        end if;

        if kLowerLimitSpecified then

          --report "A lower limit was specified.";

          -- The output configuration specifies a saturation limit
          -- other than the minimum saturation value (tight range
          -- propogation). We must do a compare to see if we
          -- underflow.

          vOverflowLo := to_signed(vQuantSgn) < to_signed(kLowerLimit);

        elsif kQuantIwl > kOutIwl-choose(kOutSigned, 0, 1) then

          -- The quantized result contains more bits than can fit in
          -- the output. We need to check and make sure we didn't
          -- underflow. This can happen simply by having a negative
          -- quantized result when the output is unsigned or by having
          -- anything but all sign bits when the output is signed.

          if not kOutSigned then

            vOverflowLo := to_boolean(vQuant(vQuant'left));

          else

            vOverflowLo :=
              vQuant(vQuant'left) = '1' and
              not IsAllOnes(vQuant(kQuantIwl-1 downto kOutIwl-1));

          end if;

        end if;

      end if;

    end if;

    -------------------------------------------------------------------------
    -- Depending on the overflow mode chosen and the overflow bits
    -- calculated above, correctly wrap or saturate the output.

    if kOverflowMode /= kSaturate or not (vOverflowHi or vOverflowLo) then

      -- Either we're wrapping and don't care about the overflow
      -- bit or we didn't overflow. Either way, just truncate the
      -- quantized value into the output variable.

      vOutLoc := SimpleResize(kQuantSigned, vQuant, kOutIwl, kOutFwl);

    elsif vOverflowHi then

      -- Saturated in upper limit. Pin the output value to the
      -- upper limit described by either the constant give or a
      -- calculated saturation value.

      vOutLoc := kSatUpperLimit;

    else

      -- Saturated in lower limit. Same as above, just pin the
      -- output value to the lower limit.

      vOutLoc := kSatLowerLimit;

    end if;

    ---------------------------------------------------------------------------
    -- Assign the outputs
    ---------------------------------------------------------------------------

    vOut      := vOutLoc;
    vOverflow := vOverflowHi or vOverflowLo;

  end procedure ResizeTrpVar;

  procedure ResizeVar (
    constant kInSigned     : in  boolean;
    variable vIn           : in  tFxpGen;
    constant kOutSigned    : in  boolean;
    variable vOut          : out tFxpGen;
    variable vOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t)
  is
    constant kUpperLimit : tFxpGen(vOut'range) := Saturate(kOutSigned, vOut'left+1, -vOut'right, kUpper);
    constant kLowerLimit : tFxpGen(vOut'range) := Saturate(kOutSigned, vOut'left+1, -vOut'right, kLower);
  begin
    ResizeTrpVar(
      kInSigned, vIn,
      kOutSigned, vOut, vOverflow,
      kOverflowMode, kRoundMode,
      kUpperLimit, kLowerLimit
      );
  end procedure ResizeVar;

  procedure ResizeVar (
    variable vIn           : in  tFxpUns;
    variable vOut          : out tFxpUns;
    variable vOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t)
  is
    variable vInGen  : tFxpGen(vIn'range);
    variable vOutGen : tFxpGen(vOut'range);
  begin
    vInGen := tFxpGen(vIn);

    ResizeVar(
      false, vInGen,
      false, vOutGen, vOverflow,
      kOverflowMode, kRoundMode
      );

    vOut := tFxpUns(vOutGen);
  end procedure ResizeVar;

  procedure ResizeVar (
    variable vIn           : in  tFxpUns;
    variable vOut          : out tFxpSgn;
    variable vOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t)
  is
    variable vInGen  : tFxpGen(vIn'range);
    variable vOutGen : tFxpGen(vOut'range);
  begin
    vInGen := tFxpGen(vIn);

    ResizeVar(
      false, vInGen,
      true, vOutGen, vOverflow,
      kOverflowMode, kRoundMode
      );

    vOut := tFxpSgn(vOutGen);
  end procedure ResizeVar;

  procedure ResizeVar (
    variable vIn           : in  tFxpSgn;
    variable vOut          : out tFxpUns;
    variable vOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t)
  is
    variable vInGen  : tFxpGen(vIn'range);
    variable vOutGen : tFxpGen(vOut'range);
  begin
    vInGen := tFxpGen(vIn);

    ResizeVar(
      true, vInGen,
      false, vOutGen, vOverflow,
      kOverflowMode, kRoundMode
      );

    vOut := tFxpUns(vOutGen);
  end procedure ResizeVar;

  procedure ResizeVar (
    variable vIn           : in  tFxpSgn;
    variable vOut          : out tFxpSgn;
    variable vOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t)
  is
    variable vInGen  : tFxpGen(vIn'range);
    variable vOutGen : tFxpGen(vOut'range);
  begin
    vInGen := tFxpGen(vIn);

    ResizeVar(
      true, vInGen,
      true, vOutGen, vOverflow,
      kOverflowMode, kRoundMode
      );

    vOut := tFxpSgn(vOutGen);
  end procedure ResizeVar;

  -----------------------------------------------------------------------------
  -- Signal Based Wrappers

  -- Because this procedure has two outputs, we had the choice to
  -- either do some magic with the return type and make it more like a
  -- function to handle the signal/variable conflicts or create
  -- different functions based on variables and then create wrappers
  -- to handle the signal case. We decided to go with the second as it
  -- seemed to keep the interfaces and conversion functions to a
  -- minimum.

  procedure ResizeTrp (
    constant kInSigned      : in  boolean;
    signal   sIn            : in  tFxpGen;
    constant kOutSigned     : in  boolean;
    signal   sOut           : out tFxpGen;
    signal   sOverflow      : out boolean;
    constant kOverflowMode  : in  FxpOverflowMode_t;
    constant kRoundMode     : in  FxpRoundMode_t;
    constant kSatUpperLimit : in  tFxpGen;
    constant kSatLowerLimit : in  tFxpGen)
  is
    variable vIn       : tFxpGen(sIn'range);
    variable vOut      : tFxpGen(sOut'range);
    variable vOverflow : boolean;
  begin
    vIn := sIn;
    ResizeTrpVar(
      kInSigned, vIn,
      kOutSigned, vOut, vOverflow,
      kOverflowMode, kRoundMode,
      kSatUpperLimit, kSatLowerLimit
      );
    sOut      <= vOut;
    sOverflow <= vOverflow;
  end procedure ResizeTrp;

  procedure Resize (
    constant kInSigned     : in  boolean;
    signal   sIn           : in  tFxpGen;
    constant kOutSigned    : in  boolean;
    signal   sOut          : out tFxpGen;
    signal   sOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t)
  is
    variable vIn       : tFxpGen(sIn'range);
    variable vOut      : tFxpGen(sOut'range);
    variable vOverflow : boolean;
  begin
    vIn := sIn;
    ResizeVar(
      kInSigned, vIn,
      kOutSigned, vOut, vOverflow,
      kOverflowMode, kRoundMode
      );
    sOut      <= vOut;
    sOverflow <= vOverflow;
  end procedure Resize;

  -----------------------------------------------------------------------------

  procedure Resize (
    signal   sIn           : in  tFxpUns;
    signal   sOut          : out tFxpUns;
    signal   sOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t)
  is
    variable vIn       : tFxpUns(sIn'range);
    variable vOut      : tFxpUns(sOut'range);
    variable vOverflow : boolean;
  begin
    vIn       := sIn;
    ResizeVar(vIn, vOut, vOverflow, kOverflowMode, kRoundMode);
    sOut      <= vOut;
    sOverflow <= vOverflow;
  end procedure Resize;

  procedure Resize (
    signal   sIn           : in  tFxpSgn;
    signal   sOut          : out tFxpSgn;
    signal   sOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t)
  is
    variable vIn       : tFxpSgn(sIn'range);
    variable vOut      : tFxpSgn(sOut'range);
    variable vOverflow : boolean;
  begin
    vIn       := sIn;
    ResizeVar(vIn, vOut, vOverflow, kOverflowMode, kRoundMode);
    sOut      <= vOut;
    sOverflow <= vOverflow;
  end procedure Resize;

  procedure Resize (
    signal   sIn           : in  tFxpUns;
    signal   sOut          : out tFxpSgn;
    signal   sOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t)
  is
    variable vIn       : tFxpUns(sIn'range);
    variable vOut      : tFxpSgn(sOut'range);
    variable vOverflow : boolean;
  begin
    vIn       := sIn;
    ResizeVar(vIn, vOut, vOverflow, kOverflowMode, kRoundMode);
    sOut      <= vOut;
    sOverflow <= vOverflow;
  end procedure Resize;

  procedure Resize (
    signal   sIn           : in  tFxpSgn;
    signal   sOut          : out tFxpUns;
    signal   sOverflow     : out boolean;
    constant kOverflowMode : in  FxpOverflowMode_t;
    constant kRoundMode    : in  FxpRoundMode_t)
  is
    variable vIn       : tFxpSgn(sIn'range);
    variable vOut      : tFxpUns(sOut'range);
    variable vOverflow : boolean;
  begin
    vIn       := sIn;
    ResizeVar(vIn, vOut, vOverflow, kOverflowMode, kRoundMode);
    sOut      <= vOut;
    sOverflow <= vOverflow;
  end procedure Resize;

  -----------------------------------------------------------------------------
  -- Sizing Functions
  -----------------------------------------------------------------------------

  function DescribeOutput (
    kInOneSigned : boolean;
    vInOne       : tFxpGen;
    kOperator    : character;
    kInTwoSigned : boolean;
    vInTwo       : tFxpGen)
    return OutputDesc_t
  is
    variable vResult     : OutputDesc_t;
    constant kMixedSigns : boolean := kInOneSigned xor kInTwoSigned;
  begin

    -- Most operations produce a signed output when either input is
    -- signed, subtract overrides below.

    vResult.kSigned := kInOneSigned or kInTwoSigned;

    case kOperator is
      when '+' =>
        vResult.kIwl := Maximum(vInOne'left+1, vInTwo'left+1)+1;
        vResult.kFwl := -Minimum(vInOne'right, vInTwo'right);
      when '-' =>
        vResult.kSigned := true;        -- always true with subtract
        vResult.kIwl    := Maximum(vInOne'left+1, vInTwo'left+1)+1;
        vResult.kFwl    := -Minimum(vInOne'right, vInTwo'right);
      when '*' =>
        vResult.kIwl := vInOne'left+vInTwo'left+2;
        vResult.kFwl := -(vInOne'right+vInTwo'right);
      when others =>
        ReportError
         (ErrorType => ExternalError,
          ShortDescription => "No support for operator " & kOperator &
                              " in DescribeOutput.");
    end case;

    -- Currently, if the signs on the input are mixed, the output will
    -- have one additional bit.

    vResult.kIwl := vResult.kIwl + choose(kMixedSigns, 1, 0);

    return vResult;

  end function DescribeOutput;

  function DescribeOutput (
    vInOne    : tFxpUns;
    kOperator : character;
    vInTwo    : tFxpUns)
    return OutputDesc_t is
  begin
    return DescribeOutput(
      false, tFxpGen(vInOne),
      kOperator,
      false, tFxpGen(vInTwo)
      );
  end function DescribeOutput;

  function DescribeOutput (
    vInOne    : tFxpSgn;
    kOperator : character;
    vInTwo    : tFxpUns)
    return OutputDesc_t is
  begin
    return DescribeOutput(
      true, tFxpGen(vInOne),
      kOperator,
      false, tFxpGen(vInTwo)
      );
  end function DescribeOutput;

  function DescribeOutput (
    vInOne    : tFxpUns;
    kOperator : character;
    vInTwo    : tFxpSgn)
    return OutputDesc_t is
  begin
    return DescribeOutput(
      false, tFxpGen(vInOne),
      kOperator,
      true, tFxpGen(vInTwo)
      );
  end function DescribeOutput;

  function DescribeOutput (
    vInOne    : tFxpSgn;
    kOperator : character;
    vInTwo    : tFxpSgn)
    return OutputDesc_t is
  begin
    return DescribeOutput(
      true, tFxpGen(vInOne),
      kOperator,
      true, tFxpGen(vInTwo)
      );
  end function DescribeOutput;

  -----------------------------------------------------------------------------
  -- Addition
  -----------------------------------------------------------------------------

  function "+" (x : tFxpGen; y : tFxpGen) return tFxpGen is
    constant kOutIwl : integer := Maximum(x'left+1, y'left+1);
    constant kOutFwl : integer := -Minimum(x'right, y'right);
    variable vResult : tFxpGen(kOutIwl-1 downto -kOutFwl);
  begin
    vResult := tFxpGen(
      to_unsigned(SimpleResize(false, x, kOutIwl, kOutFwl)) +
      to_unsigned(SimpleResize(false, y, kOutIwl, kOutFwl))
      );
    CheckInvariant(vResult);
    return vResult;
  end function "+";

  function "+" (x : tFxpUns; y : tFxpUns) return tFxpUns is
    constant kOutIwl : integer := Maximum(x'left+1, y'left+1)+1;
    constant kOutFwl : integer := -Minimum(x'right, y'right);
    variable vResult : tFxpUns(kOutIwl-1 downto -kOutFwl);
  begin
    vResult := tFxpUns(
      to_unsigned(SimpleResize(x, kOutIwl, kOutFwl)) +
      to_unsigned(SimpleResize(y, kOutIwl, kOutFwl))
      );
    CheckInvariant(vResult);
    return vResult;
  end function "+";

  function "+" (x : tFxpSgn; y : tFxpUns) return tFxpSgn is
  begin
    return x + to_FxpSgn(y);
  end function "+";

  function "+" (x : tFxpUns; y : tFxpSgn) return tFxpSgn is
  begin
    return to_FxpSgn(x) + y;
  end function "+";

  function "+" (x : tFxpSgn; y : tFxpSgn) return tFxpSgn is
    constant kOutIwl : integer := Maximum(x'left+1, y'left+1)+1;
    constant kOutFwl : integer := -Minimum(x'right, y'right);
    variable vResult : tFxpSgn(kOutIwl-1 downto -kOutFwl);
  begin
    vResult := tFxpSgn(
      to_signed(SimpleResize(x, kOutIwl, kOutFwl)) +
      to_signed(SimpleResize(y, kOutIwl, kOutFwl))
      );
    CheckInvariant(vResult);
    return vResult;
  end function "+";

  -----------------------------------------------------------------------------
  -- Variable based implementations

  procedure AddTrpVar (
    constant kInOneSigned   : in  boolean;
    variable vInOne         : in  tFxpGen;
    constant kInTwoSigned   : in  boolean;
    variable vInTwo         : in  tFxpGen;
    constant kOutSigned     : in  boolean;
    variable vOut           : out tFxpGen;
    variable vOverflow      : out boolean;
    constant kRoundMode     : in  FxpRoundMode_t;
    constant kOverflowMode  : in  FxpOverflowMode_t;
    constant kSatUpperLimit : in  tFxpGen;
    constant kSatLowerLimit : in  tFxpGen)
  is

    -- helper functions -------------------------------------------------------

    type QuantDesc_t is record
      Overlap : boolean;
      Meta    : FxpMeta_t;
    end record;

    function GetQuantDesc (
      kLgMeta : FxpMeta_t;
      kSmMeta : FxpMeta_t)
      return QuantDesc_t
    is
      variable vRet : QuantDesc_t;
    begin

      vRet.Overlap := kSmMeta.kIwl > -kLgMeta.kFwl;

      vRet.Meta.kSigned := kLgMeta.kSigned or kSmMeta.kSigned;
      vRet.Meta.kIwl    := kLgMeta.kIwl;
      vRet.Meta.kFwl    := Maximum(kLgMeta.kFwl, kSmMeta.kFwl);

      if vRet.Overlap or kSmMeta.kSigned then
        vRet.Meta.kIwl := vRet.Meta.kIwl + 1;
      end if;

      if kSmMeta.kSigned xor kLgMeta.kSigned then
        vRet.Meta.kIwl := vRet.Meta.kIwl + 1;
      end if;

      return vRet;

    end GetQuantDesc;

    -- local variables and constants ------------------------------------------

    -- We'll allow the operands to be passed to us in any order. We'll swap them
    -- around to get them in an order easier for us to handle.

    constant kOpDesc : OpDescriptionRetVal_t := DescribeOperands(kInOneSigned, vInOne, kInTwoSigned, vInTwo);

    constant kLgMeta : FxpMeta_t := (kOpDesc.kLargeSigned, kOpDesc.kLargeIwl, kOpDesc.kLargeFwl);
    constant kSmMeta : FxpMeta_t := (kOpDesc.kSmallSigned, kOpDesc.kSmallIwl, kOpDesc.kSmallFwl);

    variable vLarge : tFxpGen(kLgMeta.kIwl-1 downto -kLgMeta.kFwl);
    variable vSmall : tFxpGen(kSmMeta.kIwl-1 downto -kSmMeta.kFwl);

    -- Generate information about our intermediate signal. First, we
    -- need to know whether the signals overlap. If not and the input
    -- isn't signed, we don't need to append an extra bit to the
    -- intermediate result because no overflow can occur.

    constant kQuantDesc : QuantDesc_t := GetQuantDesc(kLgMeta, kSmMeta);

    variable vQuant : tFxpGen(kQuantDesc.Meta.kIwl-1 downto -kQuantDesc.Meta.kFwl);

  begin  -----------------------------------------------------------------------

    -- Get the correct signals routed to the large and small vectors.

    vLarge := choose(kOpDesc.kFirstOpLarger, vInOne, vInTwo);
    vSmall := choose(kOpDesc.kFirstOpLarger, vInTwo, vInOne);

    CheckInvariant(vLarge);
    CheckInvariant(vSmall);

    -- The following code is fairly optimized to only mess with bits
    -- that actually need to be modified (add, sign-extended,
    -- etc.). However, we are currently relying on the synthesizer to
    -- get rid of any bits that need not be carried along in the
    -- intermediate result if the output vector is not actually going
    -- to need them. One such case would be if the round mode is set
    -- to truncate in which we only need the intermediate vector to
    -- contain bits down to vOut'right. Currently, the intermediate
    -- vector contains all possible bits and the resize function will
    -- eliminate the unneeded ones (see the diagram in the
    -- non-overlapping case).

    if not kQuantDesc.Overlap then

      --
      -- No Overlap
      --
      -- Here, the two signals do not overlap. We can simplify the
      -- addition by looking at whether the inputs are signed and
      -- unsigned. Since the output may include portions of the
      -- large and small numbers, we have to keep enough of the bits
      -- around to correctly round the result.
      --
      --    |---Large---|
      --                  |----Small----|
      --   |?-----------Int-------------|
      --      ?------Out------?
      --

      if kSmMeta.kSigned then  -- Need to add in the sign bits from smaller operand
        vQuant(vQuant'left downto vLarge'right) :=
          SignExtend(kLgMeta.kSigned, vLarge, kQuantDesc.Meta.kIwl) +
          SimpleResize(kSmMeta.kSigned, vSmall(vSmall'left downto vSmall'left), kQuantDesc.Meta.kIwl, kLgMeta.kFwl);
      else                     -- No need to add, just bring down bits.
        vQuant(vQuant'left downto vLarge'right) := SignExtend(kLgMeta.kSigned, vLarge, kQuantDesc.Meta.kIwl);
      end if;

      -- The rest of the vector can be filled in with the small value.
      vQuant(vLarge'right-1 downto vSmall'right) := SignExtend(kSmMeta.kSigned, vSmall, vLarge'right);

    elsif vSmall'left >= vLarge'right and vLarge'right > vSmall'right then

      --
      -- Straddling
      --
      -- The small vector overlaps with the larger vector but has a
      -- larger fractional word length. We have to do a full add in
      -- this case.
      --
      --    |------Large------|
      --              |----Small----|
      --   |?---------Int-----------|
      --       ?-------Out-------?
      --

      vQuant(vQuant'left downto vLarge'right) :=
        SignExtend(kLgMeta.kSigned, vLarge, kQuantDesc.Meta.kIwl) +
        SimpleResize(kSmMeta.kSigned, vSmall, kQuantDesc.Meta.kIwl, kLgMeta.kFwl);

      vQuant(vLarge'right-1 downto vSmall'right) := vSmall(vLarge'right-1 downto vSmall'right);

    else

      --
      -- Containment
      --
      -- In this case, the signal with the largest integer word length
      -- also has more fractional bits as well. To do the addition, we
      -- can simply add the overlapping parts and then bring along the
      -- rest.
      --
      --    |------Large------|            |-----Large -----|
      --       |---Small---|                  |---Small-----|
      --   |?-------Int-------|           |?------Int-------|
      --     ?---------Out----------?    ?-----Out----?
      --

      vQuant(vQuant'left downto vSmall'right) :=
        SimpleResize(kLgMeta.kSigned, vLarge, kQuantDesc.Meta.kIwl, kSmMeta.kFwl) +
        SignExtend(kSmMeta.kSigned, vSmall, kQuantDesc.Meta.kIwl);

      if kLgMeta.kFwl /= kSmMeta.kFwl then
        vQuant(vSmall'right-1 downto vLarge'right) := vLarge(vSmall'right-1 downto vLarge'right);
      end if;

    end if;

    CheckInvariant(vQuant);

    -- Finally, we have the intermediate result and can correctly
    -- round and handle any overflow.

    ResizeTrpVar(
      kQuantDesc.Meta.kSigned, vQuant,
      kOutSigned, vOut, vOverflow,
      kOverflowMode, kRoundMode,
      kSatUpperLimit, kSatLowerLimit
      );

  end procedure AddTrpVar;

  procedure AddVar (
    constant kInOneSigned  : in  boolean;
    variable vInOne        : in  tFxpGen;
    constant kInTwoSigned  : in  boolean;
    variable vInTwo        : in  tFxpGen;
    constant kOutSigned    : in  boolean;
    variable vOut          : out tFxpGen;
    variable vOverflow     : out boolean;
    constant kRoundMode    : in  FxpRoundMode_t;
    constant kOverflowMode : in  FxpOverflowMode_t)
  is
    constant kUpperLimit : tFxpGen(vOut'range) := Saturate(kOutSigned, vOut'left+1, -vOut'right, kUpper);
    constant kLowerLimit : tFxpGen(vOut'range) := Saturate(kOutSigned, vOut'left+1, -vOut'right, kLower);
  begin
    AddTrpVar (
      kInOneSigned, vInOne,
      kInTwoSigned, vInTwo,
      kOutSigned, vOut, vOverflow,
      kRoundMode, kOverflowMode,
      kUpperLimit, kLowerLimit
      );
  end procedure AddVar;

  -----------------------------------------------------------------------------
  -- Signal Based Implementations

  procedure AddTrp (
    constant kInOneSigned   : in  boolean;
    signal   sInOne         : in  tFxpGen;
    constant kInTwoSigned   : in  boolean;
    signal   sInTwo         : in  tFxpGen;
    constant kOutSigned     : in  boolean;
    signal   sOut           : out tFxpGen;
    signal   sOverflow      : out boolean;
    constant kRoundMode     : in  FxpRoundMode_t;
    constant kOverflowMode  : in  FxpOverflowMode_t;
    constant kSatUpperLimit : in  tFxpGen;
    constant kSatLowerLimit : in  tFxpGen)
  is
    variable vInOne    : tFxpGen(sInOne'range);
    variable vInTwo    : tFxpGen(sInTwo'range);
    variable vOut      : tFxpGen(sOut'range);
    variable vOverflow : boolean;
  begin
    vInOne := sInOne;
    vInTwo := sInTwo;
    AddTrpVar(
      kInOneSigned, vInOne,
      kInTwoSigned, vInTwo,
      kOutSigned, vOut, vOverflow,
      kRoundMode, kOverflowMode,
      kSatUpperLimit, kSatLowerLimit
      );
    sOut      <= vOut;
    sOverflow <= vOverflow;
  end procedure AddTrp;

  procedure Add (
    constant kInOneSigned  : in  boolean;
    signal   sInOne        : in  tFxpGen;
    constant kInTwoSigned  : in  boolean;
    signal   sInTwo        : in  tFxpGen;
    constant kOutSigned    : in  boolean;
    signal   sOut          : out tFxpGen;
    signal   sOverflow     : out boolean;
    constant kRoundMode    : in  FxpRoundMode_t;
    constant kOverflowMode : in  FxpOverflowMode_t)
  is
    variable vInOne    : tFxpGen(sInOne'range);
    variable vInTwo    : tFxpGen(sInTwo'range);
    variable vOut      : tFxpGen(sOut'range);
    variable vOverflow : boolean;
  begin
    vInOne := sInOne;
    vInTwo := sInTwo;
    AddVar(
      kInOneSigned, vInOne,
      kInTwoSigned, vInTwo,
      kOutSigned, vOut, vOverflow,
      kRoundMode, kOverflowMode
      );
    sOut      <= vOut;
    sOverflow <= vOverflow;
  end procedure Add;

  -----------------------------------------------------------------------------
  -- Subtraction
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Operators

  function "-" (x : tFxpGen; y : tFxpGen) return tFxpGen is
    constant kOutIwl : integer := Maximum(x'left+1, y'left+1);
    constant kOutFwl : integer := -Minimum(x'right, y'right);
    variable vResult : tFxpGen(kOutIwl-1 downto -kOutFwl);
  begin
    vResult := tFxpGen(
      to_unsigned(SimpleResize(false, x, kOutIwl, kOutFwl)) -
      to_unsigned(SimpleResize(false, y, kOutIwl, kOutFwl))
      );
    return vResult;
  end function "-";

  function "-" (x : tFxpUns; y : tFxpUns) return tFxpUns is
    constant kOutIwl : integer := Maximum(x'left+1, y'left+1)+1;
    constant kOutFwl : integer := -Minimum(x'right, y'right);
    variable vResult : tFxpUns(kOutIwl-1 downto -kOutFwl);
  begin
    vResult := tFxpUns(
      to_unsigned(SimpleResize(x, kOutIwl, kOutFwl)) -
      to_unsigned(SimpleResize(y, kOutIwl, kOutFwl))
      );
    return vResult;
  end function "-";

  function "-" (x : tFxpSgn; y : tFxpUns) return tFxpSgn is
  begin
    return x - to_FxpSgn(y);
  end function "-";

  function "-" (x : tFxpUns; y : tFxpSgn) return tFxpSgn is
  begin
    return to_FxpSgn(x) - y;
  end function "-";

  function "-" (x : tFxpSgn; y : tFxpSgn) return tFxpSgn is
    constant kOutIwl : integer := Maximum(x'left+1, y'left+1)+1;
    constant kOutFwl : integer := -Minimum(x'right, y'right);
    variable vResult : tFxpSgn(kOutIwl-1 downto -kOutFwl);
  begin
    vResult := tFxpSgn(
      to_signed(SimpleResize(x, kOutIwl, kOutFwl)) -
      to_signed(SimpleResize(y, kOutIwl, kOutFwl))
      );
    return vResult;
  end function "-";

  -----------------------------------------------------------------------------
  -- Variable based procedures

  procedure SubtractDisjointTrpVar (
    constant kInOneSigned   : in  boolean;
    variable vInOne         : in  tFxpGen;
    constant kInTwoSigned   : in  boolean;
    variable vInTwo         : in  tFxpGen;
    constant kOutSigned     : in  boolean;
    variable vOut           : out tFxpGen;
    variable vOverflow      : out boolean;
    constant kRoundMode     : in  FxpRoundMode_t;
    constant kOverflowMode  : in  FxpOverflowMode_t;
    constant kSatUpperLimit : in  tFxpGen;
    constant kSatLowerLimit : in  tFxpGen)
  is
    variable vInTwoNeg : tFxpGen(vInTwo'left+1 downto vInTwo'right);
  begin
    vInTwoNeg := Negate(kInTwoSigned, vInTwo);
    AddTrpVar(
      kInOneSigned, vInOne,
      true, vInTwoNeg,                  -- Negate makes us signed.
      kOutSigned, vOut, vOverflow,
      kRoundMode, kOverflowMode,
      kSatUpperLimit, kSatLowerLimit
      );
  end procedure SubtractDisjointTrpVar;

  procedure SubtractOverlapTrpVar (
    constant kInOneSigned   : in  boolean;
    variable vInOne         : in  tFxpGen;
    constant kInTwoSigned   : in  boolean;
    variable vInTwo         : in  tFxpGen;
    constant kOutSigned     : in  boolean;
    variable vOut           : out tFxpGen;
    variable vOverflow      : out boolean;
    constant kRoundMode     : in  FxpRoundMode_t;
    constant kOverflowMode  : in  FxpOverflowMode_t;
    constant kSatUpperLimit : in  tFxpGen;
    constant kSatLowerLimit : in  tFxpGen)
  is
    constant kOutDesc : OutputDesc_t := DescribeOutput(kInOneSigned, vInOne, '-', kInTwoSigned, vInTwo);
    variable vInt     : tFxpGen(kOutDesc.kIwl-1 downto -kOutDesc.kFwl);
  begin

    vInt := SimpleResize(kInOneSigned, vInOne, kOutDesc.kIwl, kOutDesc.kFwl) -
            SimpleResize(kInTwoSigned, vInTwo, kOutDesc.kIwl, kOutDesc.kFwl);

    CheckInvariant(vInt);

    ResizeTrpVar(
      kOutDesc.kSigned, vInt,
      kOutSigned, vOut, vOverflow,
      kOverflowMode, kRoundMode,
      kSatUpperLimit, kSatLowerLimit
      );

  end procedure SubtractOverlapTrpVar;

  procedure SubtractTrpVar (
    constant kInOneSigned   : in  boolean;
    variable vInOne         : in  tFxpGen;
    constant kInTwoSigned   : in  boolean;
    variable vInTwo         : in  tFxpGen;
    constant kOutSigned     : in  boolean;
    variable vOut           : out tFxpGen;
    variable vOverflow      : out boolean;
    constant kRoundMode     : in  FxpRoundMode_t;
    constant kOverflowMode  : in  FxpOverflowMode_t;
    constant kSatUpperLimit : in  tFxpGen;
    constant kSatLowerLimit : in  tFxpGen)
  is
    constant kDisjoint : boolean := vInOne'right > vInTwo'left or vInTwo'right > vInOne'left;
  begin
    if kDisjoint then
      SubtractDisjointTrpVar(
        kInOneSigned, vInOne,
        kInTwoSigned, vInTwo,
        kOutSigned, vOut, vOverflow,
        kRoundMode, kOverflowMode,
        kSatUpperLimit, kSatLowerLimit
        );
    else
      SubtractOverlapTrpVar(
        kInOneSigned, vInOne,
        kInTwoSigned, vInTwo,
        kOutSigned, vOut, vOverflow,
        kRoundMode, kOverflowMode,
        kSatUpperLimit, kSatLowerLimit
        );
    end if;
  end procedure SubtractTrpVar;

  procedure SubtractVar (
    constant kInOneSigned  : in  boolean;
    variable vInOne        : in  tFxpGen;
    constant kInTwoSigned  : in  boolean;
    variable vInTwo        : in  tFxpGen;
    constant kOutSigned    : in  boolean;
    variable vOut          : out tFxpGen;
    variable vOverflow     : out boolean;
    constant kRoundMode    : in  FxpRoundMode_t;
    constant kOverflowMode : in  FxpOverflowMode_t)
  is
    constant kUpperLimit : tFxpGen(vOut'range) := Saturate(kOutSigned, vOut'left+1, -vOut'right, kUpper);
    constant kLowerLimit : tFxpGen(vOut'range) := Saturate(kOutSigned, vOut'left+1, -vOut'right, kLower);
  begin
    SubtractTrpVar (
      kInOneSigned, vInOne,
      kInTwoSigned, vInTwo,
      kOutSigned, vOut, vOverflow,
      kRoundMode, kOverflowMode,
      kUpperLimit, kLowerLimit
      );
  end procedure SubtractVar;

  -----------------------------------------------------------------------------
  -- Signal based procedures

  procedure SubtractTrp (
    constant kInOneSigned   : in  boolean;
    signal   sInOne         : in  tFxpGen;
    constant kInTwoSigned   : in  boolean;
    signal   sInTwo         : in  tFxpGen;
    constant kOutSigned     : in  boolean;
    signal   sOut           : out tFxpGen;
    signal   sOverflow      : out boolean;
    constant kRoundMode     : in  FxpRoundMode_t;
    constant kOverflowMode  : in  FxpOverflowMode_t;
    constant kSatUpperLimit : in  tFxpGen;
    constant kSatLowerLimit : in  tFxpGen)
  is
    variable vInOne    : tFxpGen(sInOne'range);
    variable vInTwo    : tFxpGen(sInTwo'range);
    variable vOut      : tFxpGen(sOut'range);
    variable vOverflow : boolean;
  begin
    vInOne := sInOne;
    vInTwo := sInTwo;
    SubtractTrpVar(
      kInOneSigned, vInOne,
      kInTwoSigned, vInTwo,
      kOutSigned, vOut, vOverflow,
      kRoundMode, kOverflowMode,
      kSatUpperLimit, kSatLowerLimit
      );
    sOut      <= vOut;
    sOverflow <= vOverflow;
  end procedure SubtractTrp;

  procedure Subtract (
    constant kInOneSigned  : in  boolean;
    signal   sInOne        : in  tFxpGen;
    constant kInTwoSigned  : in  boolean;
    signal   sInTwo        : in  tFxpGen;
    constant kOutSigned    : in  boolean;
    signal   sOut          : out tFxpGen;
    signal   sOverflow     : out boolean;
    constant kRoundMode    : in  FxpRoundMode_t;
    constant kOverflowMode : in  FxpOverflowMode_t)
  is
    variable vInOne    : tFxpGen(sInOne'range);
    variable vInTwo    : tFxpGen(sInTwo'range);
    variable vOut      : tFxpGen(sOut'range);
    variable vOverflow : boolean;
  begin
    vInOne := sInOne;
    vInTwo := sInTwo;
    SubtractVar(
      kInOneSigned, vInOne,
      kInTwoSigned, vInTwo,
      kOutSigned, vOut, vOverflow,
      kRoundMode, kOverflowMode
      );
    sOut      <= vOut;
    sOverflow <= vOverflow;
  end procedure Subtract;

  -----------------------------------------------------------------------------
  -- Multiplication
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Operators

  function "*" (x : tFxpUns; y : tFxpUns) return tFxpUns is
    variable vResult : tFxpUns(x'left+y'left+1 downto x'right+y'right);
  begin
    vResult := tFxpUns(to_unsigned(x) * to_unsigned(y));
    return vResult;
  end function "*";

  function "*" (x : tFxpUns; y : tFxpSgn) return tFxpSgn is
  begin
    return to_FxpSgn(x) * y;
  end function "*";

  function "*" (x : tFxpSgn; y : tFxpUns) return tFxpSgn is
  begin
    return x * to_FxpSgn(y);
  end function "*";

  function "*" (x : tFxpSgn; y : tFxpSgn) return tFxpSgn is
    variable vResult : tFxpSgn(x'left+y'left+1 downto x'right+y'right);
  begin
    vResult := tFxpSgn(to_signed(x) * to_signed(y));
    return vResult;
  end function "*";

  -----------------------------------------------------------------------------
  -- Variable based procedures

  procedure MultiplyTrpVar (
    constant kInOneSigned   : in  boolean;
    variable vInOne         : in  tFxpGen;
    constant kInTwoSigned   : in  boolean;
    variable vInTwo         : in  tFxpGen;
    constant kOutSigned     : in  boolean;
    variable vOut           : out tFxpGen;
    variable vOverflow      : out boolean;
    constant kRoundMode     : in  FxpRoundMode_t;
    constant kOverflowMode  : in  FxpOverflowMode_t;
    constant kSatUpperLimit : in  tFxpGen;
    constant kSatLowerLimit : in  tFxpGen)
  is
    constant kInOneIwl : integer := vInOne'left + 1;
    constant kInOneFwl : integer := -vInOne'right;

    constant kInTwoIwl : integer := vInTwo'left + 1;
    constant kInTwoFwl : integer := -vInTwo'right;

    -- If the inputs are of opposite sign, one will get sign extended before
    -- the multiplication takes place.

    constant kPadBitNeeded : boolean := kInOneSigned xor kInTwoSigned;

    -- Using the sizes of the inputs, we can determine what storage is needed
    -- for the intermediate value from the mutliplication.

    constant kIntResSigned : boolean := kInOneSigned or kInTwoSigned;
    constant kIntResIwl    : integer := kInOneIwl+kInTwoIwl+choose(kPadBitNeeded, 1, 0);
    constant kIntResFwl    : integer := kInOneFwl+kInTwoFwl;

    variable vIntRes : tFxpGen(kIntResIwl-1 downto -kIntResFwl);
  begin

    if kInOneSigned and kInTwoSigned then
      vIntRes := tFxpGen(tFxpSgn(vInOne) * tFxpSgn(vInTwo));
    elsif kInOneSigned then
      vIntRes := tFxpGen(tFxpSgn(vInOne) * tFxpUns(vInTwo));
    elsif kInTwoSigned then
      vIntRes := tFxpGen(tFxpUns(vInOne) * tFxpSgn(vInTwo));
    else
      vIntRes := tFxpGen(tFxpUns(vInOne) * tFxpUns(vInTwo));
    end if;

    CheckInvariant(vIntRes);

    ResizeVar(kIntResSigned, vIntRes, kOutSigned, vOut, vOverflow, kOverflowMode, kRoundMode);

  end procedure MultiplyTrpVar;

  procedure MultiplyVar (
    constant kInOneSigned  : in  boolean;
    variable vInOne        : in  tFxpGen;
    constant kInTwoSigned  : in  boolean;
    variable vInTwo        : in  tFxpGen;
    constant kOutSigned    : in  boolean;
    variable vOut          : out tFxpGen;
    variable vOverflow     : out boolean;
    constant kRoundMode    : in  FxpRoundMode_t;
    constant kOverflowMode : in  FxpOverflowMode_t)
  is
    constant kUpperLimit : tFxpGen(vOut'range) := Saturate(kOutSigned, vOut'left+1, -vOut'right, kUpper);
    constant kLowerLimit : tFxpGen(vOut'range) := Saturate(kOutSigned, vOut'left+1, -vOut'right, kLower);
  begin
    MultiplyTrpVar (
      kInOneSigned, vInOne,
      kInTwoSigned, vInTwo,
      kOutSigned, vOut, vOverflow,
      kRoundMode, kOverflowMode,
      kUpperLimit, kLowerLimit
      );
  end procedure MultiplyVar;

  -----------------------------------------------------------------------------
  -- Signal based procedures

  procedure MultiplyTrp (
    constant kInOneSigned   : in  boolean;
    signal   sInOne         : in  tFxpGen;
    constant kInTwoSigned   : in  boolean;
    signal   sInTwo         : in  tFxpGen;
    constant kOutSigned     : in  boolean;
    signal   sOut           : out tFxpGen;
    signal   sOverflow      : out boolean;
    constant kRoundMode     : in  FxpRoundMode_t;
    constant kOverflowMode  : in  FxpOverflowMode_t;
    constant kSatUpperLimit : in  tFxpGen;
    constant kSatLowerLimit : in  tFxpGen)
  is
    variable vInOne    : tFxpGen(sInOne'range);
    variable vInTwo    : tFxpGen(sInTwo'range);
    variable vOut      : tFxpGen(sOut'range);
    variable vOverflow : boolean;
  begin
    vInOne := sInOne;
    vInTwo := sInTwo;
    MultiplyTrpVar(
      kInOneSigned, vInOne,
      kInTwoSigned, vInTwo,
      kOutSigned, vOut, vOverflow,
      kRoundMode, kOverflowMode,
      kSatUpperLimit, kSatLowerLimit
      );
    sOut      <= vOut;
    sOverflow <= vOverflow;
  end procedure MultiplyTrp;

  procedure Multiply (
    constant kInOneSigned  : in  boolean;
    signal   sInOne        : in  tFxpGen;
    constant kInTwoSigned  : in  boolean;
    signal   sInTwo        : in  tFxpGen;
    constant kOutSigned    : in  boolean;
    signal   sOut          : out tFxpGen;
    signal   sOverflow     : out boolean;
    constant kRoundMode    : in  FxpRoundMode_t;
    constant kOverflowMode : in  FxpOverflowMode_t)
  is
    variable vInOne    : tFxpGen(sInOne'range);
    variable vInTwo    : tFxpGen(sInTwo'range);
    variable vOut      : tFxpGen(sOut'range);
    variable vOverflow : boolean;
  begin
    vInOne := sInOne;
    vInTwo := sInTwo;
    MultiplyVar(
      kInOneSigned, vInOne,
      kInTwoSigned, vInTwo,
      kOutSigned, vOut, vOverflow,
      kRoundMode, kOverflowMode
      );
    sOut      <= vOut;
    sOverflow <= vOverflow;
  end procedure Multiply;

  -----------------------------------------------------------------------------
  -- Inversion and Negation
  -----------------------------------------------------------------------------

  function Invert (arg : tFxpGen) return tFxpGen is
    variable vResult : tFxpGen(arg'range);
  begin
    vResult := tFxpGen(not to_slv(arg));
    CheckInvariant(vResult);
    return vResult;
  end function Invert;

  function Invert (arg : tFxpUns) return tFxpUns is
  begin
    return tFxpUns(Invert(tFxpGen(arg)));
  end function Invert;

  function Invert (arg : tFxpSgn) return tFxpSgn is
  begin
    return tFxpSgn(Invert(tFxpGen(arg)));
  end function Invert;

  -----------------------------------------------------------------------------

  function Negate (
    kArgSigned : boolean;
    arg        : tFxpGen;
    kIncLength : boolean := true)
    return tFxpGen
  is
    constant kLSB : tFxpGen(arg'right downto arg'right) := "1";

    -- The output iwl may incremented by one to handle the possibility
    -- of overflow.

    constant kResIwl : integer := arg'left + 1 + choose(kIncLength, 1, 0);
    constant kResFwl : integer := -arg'right;
    variable vResult : tFxpGen(kResIwl-1 downto -kResFwl);
  begin
    vResult := Invert(SignExtend(kArgSigned, arg, kResIwl)) + kLSB;
    CheckInvariant(vResult);
    return vResult;
  end function Negate;

  function Negate (arg : tFxpUns) return tFxpSgn is
  begin
    -- We don't need the generic negate to add a buffer bit in this
    -- case because we know the original number was unsigned and when
    -- we convert to a signed we get our buffer bit.
    return tFxpSgn(Negate(true, tFxpGen(to_FxpSgn(arg)), false));
  end function Negate;

  function Negate (arg : tFxpSgn) return tFxpSgn is
  begin
    return tFxpSgn(Negate(true, tFxpGen(arg)));
  end function Negate;

  -----------------------------------------------------------------------------
  -- Shift Operators
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Shift Left Logical

  function "sll" (x : tFxpGen; n : natural) return tFxpGen is
    variable vResult : tFxpGen(x'range);
  begin
    vResult := tFxpGen(to_unsigned(x) sll n);
    return vResult;
  end function "sll";

  function "sll" (x : tFxpUns; n : natural) return tFxpUns is
  begin
    return tFxpUns(tFxpGen(x) sll n);
  end function "sll";

  -----------------------------------------------------------------------------
  -- Shift Right Logical

  function "srl" (x : tFxpGen; n : natural) return tFxpGen is
    variable vResult : tFxpGen(x'range);
  begin
    vResult := tFxpGen(to_unsigned(x) srl n);
    return vResult;
  end function "srl";

  function "srl" (x : tFxpUns; n : natural) return tFxpUns is
  begin
    return tFxpUns(tFxpGen(x) srl n);
  end function "srl";

  -----------------------------------------------------------------------------
  -- Shift Left Arithmetic

  function "sla" (x : tFxpGen; n : natural) return tFxpGen is
  begin
    return x sll n;
  end function "sla";

  function "sla" (x : tFxpSgn; n : natural) return tFxpSgn is
  begin
    return tFxpSgn(tFxpGen(x) sla n);
  end function "sla";

  -----------------------------------------------------------------------------
  -- Shift Right Arithmetic

  function "sra" (x : tFxpGen; n : natural) return tFxpGen is
    variable vResult : tFxpGen(x'range);
  begin
    if x'length = 1 then
      vResult := x;
    else
      vResult := tFxpGen(SHIFT_RIGHT(to_signed(x), n));
    end if;
    return vResult;
  end function "sra";

  function "sra" (x : tFxpSgn; n : natural) return tFxpSgn is
  begin
    return tFxpSgn(tFxpGen(x) sra n);
  end function "sra";

  -----------------------------------------------------------------------------
  -- Rotate Left

  function "rol" (x : tFxpGen; n : natural) return tFxpGen is
    variable vResult : tFxpGen(x'range);
  begin
    vResult := tFxpGen(to_unsigned(x) rol n);
    return vResult;
  end function "rol";

  function "rol" (x : tFxpUns; n : natural) return tFxpUns is
  begin
    return tFxpUns(tFxpGen(x) rol n);
  end function "rol";

  function "rol" (x : tFxpSgn; n : natural) return tFxpSgn is
  begin
    return tFxpSgn(tFxpGen(x) rol n);
  end function "rol";

  -----------------------------------------------------------------------------
  -- Rotate Right

  function "ror" (x : tFxpGen; n : natural) return tFxpGen is
    variable vResult : tFxpGen(x'range);
  begin
    vResult := tFxpGen(to_unsigned(x) ror n);
    return vResult;
  end function "ror";

  function "ror" (x : tFxpUns; n : natural) return tFxpUns is
  begin
    return tFxpUns(tFxpGen(x) ror n);
  end function "ror";

  function "ror" (x : tFxpSgn; n : natural) return tFxpSgn is
  begin
    return tFxpSgn(tFxpGen(x) ror n);
  end function "ror";

end PkgNiFpgaSimFxpArithmetic;
