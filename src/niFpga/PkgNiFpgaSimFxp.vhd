-- © 2012 National Instruments Corporation.
--Use of this file is subject to the Software License Agreement provided with 
--LabVIEW FPGA Module and, without limiting any of the provisions in that license, 
--modifying or distributing this file is prohibited.

--Adding to, deleting from, or otherwise modifying the contents of this file 
--may cause the software to malfunction.
-------------------------------------------------------------------------------
--
-- Purpose: Top-level fixed point utility package. This package includes the
-- actual fixed-point types and many utility functions for use with those
-- types. This package extends the fixed_pkg provided by the vhdl standards
-- committee.
--
-- The two key types are tFxpSgn and tFxpUns. Conversion and cast
-- operators have been provided for going to and from other types.
--
-- Several abbreviations found in this package:
--   IWL - Integer word length, the portion of the number to the left of the
--     binary point.
--   FWL - Fractional word length, the portion of the number to the right of
--     the binary point.
--   WL - Word length - The length of the entire number. Can be substituted
--     with "arg'length".
--
-- There are three fixed-point types included in this package. A fixed-point
-- generic type that has no sign, unsigned, and signed. The fixed-point number
-- is described with a WL, and IWL, and a FWL. The WL is simply length of the
-- vector. The IWL and FWL are described by the range of the vector. Therefore,
-- a vector of range 5 downto -6 would have an an IWL of 6 and a FWL of 6.
-- Notice that FWL is correctly negated to make it easier to declare a
-- fixed-point signal or variable. The binary point is always assumed to be
-- located between the 0 and -1 indices.
--
--           |<----------------.---------------->|
-- IWL --> 6 |+5 +4 +3 +2 +1 +0 -1 -2 -3 -4 -5 -6| <-- -FWL
--
-------------------------------------------------------------------------------

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.PkgNiUtilities.all;
use work.PkgNiFpgaUtilities.all;

package PkgNiFpgaSimFxp is

  -----------------------------------------------------------------------------
  -- Basic Fixed-Point Types
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Signed and unsigned fixed-point types. These are designed to be
  -- compatible with the fixed-point up-and-coming standard from
  -- Accellera.

  type tFxpUns is array (integer range <>) of std_logic;
  type tFxpSgn is array (integer range <>) of std_logic;

  -- Generic fixed-point type that has no sign. Functions using this
  -- type must supply whether it is signed. Rarely should this type be
  -- required outside the fixed-point package and its associated
  -- components.

  type tFxpGen is array (integer range <>) of std_logic;

  -- Meta data type that can be used with any of the types above to
  -- pass around data about the number in the fixed-point vector. This
  -- is especially useful for the generic fixed-point type.

  type FxpMeta_t is record
    kSigned    : boolean;
    kIwl, kFwl : integer;
  end record;

  -- Maximum lengths we currently allow for signals going to and
  -- from LabVIEW. Intermediate signals need not worry about the
  -- limit.

  constant kFxpMaxWl  : positive := 64;
  constant kFxpMaxIwl : positive := 1024;
  constant kFxpMinIwl : integer  := -1024;

  -----------------------------------------------------------------------------
  -- Round (Quantization) and Overflow Modes

  type FxpRoundMode_t is (
    kNotNeeded,                         -- Rounding not needed (truncate)
    kTruncate,                          -- Chop off extra bits (Round to -Inf)
    kRoundToNearest,                    -- Round up when 1/2 LSB is set
    kConvergent,                        -- Round to even when exactly 1/2 LSB
    kRoundToPosInf                      -- Round to positive infinity
    );

  constant kFxpDefaultRoundMode : FxpRoundMode_t := kTruncate;

  type FxpOverflowMode_t is (
    kNotNeeded,                         -- Don't need, optimize.
    kWrap,                              -- Ignore extra MSBs
    kSaturate                           -- Disallow overflow into MSBs
    );

  constant kFxpDefaultOverflowMode : FxpOverflowMode_t := kNotNeeded;

  -----------------------------------------------------------------------------
  -- Comparison Modes

  -- The following modes are named after the name of the LabVIEW primitive
  -- doing the same operation (minus the '?').

  type FxpComparisonOp_t is (
    kEqual,
    kNotEqual,
    kGreater,
    kGreaterOrEqual,
    kLess,
    kLessOrEqual
    );

  -----------------------------------------------------------------------------
  -- Misc Helper Functions
  -----------------------------------------------------------------------------

  function ExtractFxpOverflow(
    kIncludesOverflow : boolean;
    fxpArg            : std_logic_vector)
    return std_logic;

  function ExtractFxpValue(
    kIncludesOverflow : boolean;
    fxpArg            : std_logic_vector)
    return std_logic_vector;

  function PackFxpValueAndOverflow(
    fxpVal          : std_logic_vector;
    fxpOvfl         : std_logic;
    includeOverflow : boolean := true)
    return std_logic_vector;

  function NumOfOvflBits(constant kIncludesOverflow : boolean) return natural;

  constant kPackedVectorMaxWl : positive := 65;
  subtype PackedVector_t is std_logic_vector(kPackedVectorMaxWl-1 downto 0);

  function PackVectorForEntityBoundaryCrossing (
    arg : std_logic_vector)
    return PackedVector_t;

  function UnpackVectorFromEntityBoundaryCrossing (
    arg        : PackedVector_t;
    kIwl, kFwl : integer)
    return tFxpGen;

  -----------------------------------------------------------------------------

  -- Put this anywhere you want to ensure the types and values of the
  -- fixed-point number are within the fixed-point specifications.

  procedure CheckInvariant (arg : in tFxpGen);
  procedure CheckInvariant (arg : in tFxpUns);
  procedure CheckInvariant (arg : in tFxpSgn);

  -----------------------------------------------------------------------------

  -- Max and Min routines.

  function Maximum (x, y    : integer) return integer;
  function Maximum (x, y, z : integer) return integer;

  function Minimum (x, y    : integer) return integer;
  function Minimum (x, y, z : integer) return integer;

  -----------------------------------------------------------------------------

  -- Choose - used to select between two different inputs bases on the choice
  -- boolean. This is helpful because it can be called when declaring signals
  -- to select between different parameters, etc. If choice is true, the first
  -- input will be returned, otherwise the second input is returned.

  function Choose (selectFirst : boolean; a, b : boolean) return boolean;
  function Choose (selectFirst : boolean; a, b : integer) return integer;
  function Choose (selectFirst : boolean; a, b : tFxpGen) return tFxpGen;
  function Choose (selectFirst : boolean; a, b : tFxpUns) return tFxpUns;
  function Choose (selectFirst : boolean; a, b : tFxpSgn) return tFxpSgn;
  function Choose (selectFirst : boolean; a, b : FxpRoundMode_t) return FxpRoundMode_t;
  function Choose (selectFirst : boolean; a, b : FxpOverflowMode_t)
  return FxpOverflowMode_t;

  -----------------------------------------------------------------------------

  -- Sign extends the argument to the given integer word length. If the
  -- requested integer word length is less than the arguments integer word
  -- length, an error is thrown.

  function SignExtend (
    constant kArgSigned : boolean;
    arg                 : tFxpGen;
    constant kReqIwl    : integer)
    return tFxpGen;

  function SignExtend (
    arg              : tFxpUns;
    constant kReqIwl : integer)
    return tFxpUns;
  function SignExtend (arg : tFxpSgn; constant kReqIwl : integer) return tFxpSgn;

  -----------------------------------------------------------------------------

  -- Extends or truncates the argument to the requested integer and fractional
  -- word lengths. If the input is signed and the integer word length
  -- requested is larger than the input argument, the argument will be sign
  -- extended. If the argument extends further in either direction than the
  -- requested lengths, the extra bits will be truncated.

  function SimpleResize (
    constant kArgSigned       : boolean;
    arg                       : tFxpGen;
    constant kReqIwl, kReqFwl : integer)
    return tFxpGen;

  function SimpleResize (
    arg                       : tFxpUns;
    constant kReqIwl, kReqFwl : integer)
    return tFxpUns;
  function SimpleResize (
    arg                       : tFxpSgn;
    constant kReqIwl, kReqFwl : integer)
    return tFxpSgn;

  -----------------------------------------------------------------------------

  -- IsZero - If the input is a null-array, the function returns true.

  function IsZero (arg : tFxpGen) return boolean;
  function IsZero (arg : tFxpUns) return boolean;
  function IsZero (arg : tFxpSgn) return boolean;

  -- IsAllOnes - If the input is a null-array, false is returned.

  function IsAllOnes (arg : tFxpGen) return boolean;
  function IsAllOnes (arg : tFxpUns) return boolean;
  function IsAllOnes (arg : tFxpSgn) return boolean;

  -- IsExactlyHalf - Assumes the msb of the input is the 1/2 bit position. This
  -- function returns true if the msb is a one and the rest of the bits are
  -- zero.

  function IsExactlyHalf (arg : tFxpGen) return boolean;
  function IsExactlyHalf (arg : tFxpUns) return boolean;
  function IsExactlyHalf (arg : tFxpSgn) return boolean;

  -----------------------------------------------------------------------------

  -- Saturate - Returns a constant saturated at the limit given.

  type SaturateLimit_t is (kUpper, kLower);

  function Saturate (
    constant kSigned    : boolean;
    constant kIwl, kFwl : integer;
    constant kLimitType : SaturateLimit_t)
    return tFxpGen;

  function Saturate (
    constant kIwl, kFwl : integer;
    constant kLimitType : SaturateLimit_t)
    return tFxpUns;

  function Saturate (
    constant kIwl, kFwl : integer;
    constant kLimitType : SaturateLimit_t)
    return tFxpSgn;

  -----------------------------------------------------------------------------

  -- Helper function for use when trying to swap operands among other things.
  -- The input signals are only used to read attributes such as IWL, etc.

  type OpDescriptionRetVal_t is record
    kFirstOpLarger : boolean;
    kSmallSigned   : boolean;
    kSmallIwl      : integer;
    kSmallFwl      : integer;
    kLargeSigned   : boolean;
    kLargeIwl      : integer;
    kLargeFwl      : integer;
  end record;

  function DescribeOperands (
    constant kLeftSigned  :    boolean; left : tFxpGen;
    constant kRightSigned : in boolean; right : tFxpGen)
    return OpDescriptionRetVal_t;

  function DescribeOperands (
    left  : tFxpUns;
    right : tFxpUns)
    return OpDescriptionRetVal_t;
  function DescribeOperands (
    left  : tFxpSgn;
    right : tFxpSgn)
    return OpDescriptionRetVal_t;
  function DescribeOperands (
    left  : tFxpSgn;
    right : tFxpUns)
    return OpDescriptionRetVal_t;
  function DescribeOperands (
    left  : tFxpUns;
    right : tFxpSgn)
    return OpDescriptionRetVal_t;

  -----------------------------------------------------------------------------
  -- Conversion Operations
  -----------------------------------------------------------------------------

  function to_Fxp (
    arg      : std_logic_vector;
    sizeType : tFxpGen;
    kSigned  : boolean := false)
    return tFxpGen;

  function to_Fxp (arg : integer; sizeType : tFxpGen) return tFxpGen;

  function to_slv (arg      : tFxpGen) return std_logic_vector;
  function to_unsigned (arg : tFxpGen) return unsigned;
  function to_signed (arg   : tFxpGen) return signed;

  -----------------------------------------------------------------------------
  -- unsigned cast operators

  function to_Fxp (arg : std_logic_vector; sizeType : tFxpUns) return tFxpUns;
  function to_Fxp (arg : integer; sizeType : tFxpUns) return tFxpUns;

  function to_slv (arg      : tFxpUns) return std_logic_vector;
  function to_unsigned (arg : tFxpUns) return unsigned;
  function to_signed (arg   : tFxpUns) return signed;

  -----------------------------------------------------------------------------
  -- signed cast operators

  function to_FxpSgn (arg : tFxpUns) return tFxpSgn;

  function to_Fxp (arg : std_logic_vector; sizeType : tFxpSgn) return tFxpSgn;
  function to_Fxp (arg : integer; sizeType : tFxpSgn) return tFxpSgn;

  function to_slv (arg    : tFxpSgn) return std_logic_vector;
  function to_signed (arg : tFxpSgn) return signed;

  -----------------------------------------------------------------------------
  -- Comparison Operations

  function "=" (x : tFxpUns; y : tFxpUns) return boolean;
  function "=" (x : tFxpSgn; y : tFxpUns) return boolean;
  function "=" (x : tFxpUns; y : tFxpSgn) return boolean;
  function "=" (x : tFxpSgn; y : tFxpSgn) return boolean;

  -----------------------------------------------------------------------------

  function "/=" (x : tFxpUns; y : tFxpUns) return boolean;
  function "/=" (x : tFxpSgn; y : tFxpUns) return boolean;
  function "/=" (x : tFxpUns; y : tFxpSgn) return boolean;
  function "/=" (x : tFxpSgn; y : tFxpSgn) return boolean;

  -----------------------------------------------------------------------------

  function ">" (x : tFxpUns; y : tFxpUns) return boolean;
  function ">" (x : tFxpSgn; y : tFxpUns) return boolean;
  function ">" (x : tFxpUns; y : tFxpSgn) return boolean;
  function ">" (x : tFxpSgn; y : tFxpSgn) return boolean;

  -----------------------------------------------------------------------------

  function ">=" (x : tFxpUns; y : tFxpUns) return boolean;
  function ">=" (x : tFxpSgn; y : tFxpUns) return boolean;
  function ">=" (x : tFxpUns; y : tFxpSgn) return boolean;
  function ">=" (x : tFxpSgn; y : tFxpSgn) return boolean;

  -----------------------------------------------------------------------------

  function "<" (x : tFxpUns; y : tFxpUns) return boolean;
  function "<" (x : tFxpSgn; y : tFxpUns) return boolean;
  function "<" (x : tFxpUns; y : tFxpSgn) return boolean;
  function "<" (x : tFxpSgn; y : tFxpSgn) return boolean;

  -----------------------------------------------------------------------------

  function "<=" (x : tFxpUns; y : tFxpUns) return boolean;
  function "<=" (x : tFxpSgn; y : tFxpUns) return boolean;
  function "<=" (x : tFxpUns; y : tFxpSgn) return boolean;
  function "<=" (x : tFxpSgn; y : tFxpSgn) return boolean;

end PkgNiFpgaSimFxp;

-------------------------------------------------------------------------------
-- PkgNiFpgaSimFxp Body ----------------------------------------------------------------
-------------------------------------------------------------------------------

package body PkgNiFpgaSimFxp is

  -----------------------------------------------------------------------------
  -- Functions for overflow flag packing and extraction
  -----------------------------------------------------------------------------

  function ExtractFxpOverflow(
    kIncludesOverflow : boolean;
    fxpArg            : std_logic_vector)
    return std_logic
  is
    variable ovfl : std_logic := '0';
  begin
    if kIncludesOverflow then
      ovfl := fxpArg(fxpArg'left);  --assuming the left most element is overflow
    end if;
    return ovfl;
  end function ExtractFxpOverflow;

  -----------------------------------------------------------------------------

  function ExtractFxpValue(
    kIncludesOverflow : boolean;
    fxpArg            : std_logic_vector)
    return std_logic_vector
  is
    alias arg          : std_logic_vector(fxpArg'length-1 downto 0) is fxpArg;
    variable leftBound : natural;
  begin
    if kIncludesOverflow then
      leftBound := arg'left-1;
    else
      leftBound := arg'left;
    end if;
    return arg(leftBound downto 0);
  end function ExtractFxpValue;

  -----------------------------------------------------------------------------

  function PackFxpValueAndOverflow(
    fxpVal          : std_logic_vector;
    fxpOvfl         : std_logic;
    includeOverflow : boolean := true)
    return std_logic_vector is
  begin
    if includeOverflow then
      return fxpOvfl & fxpVal;
    else
      return fxpVal;
    end if;
  end function PackFxpValueAndOverflow;

  -----------------------------------------------------------------------------

  function NumOfOvflBits(constant kIncludesOverflow : boolean) return natural
  is
    variable num : natural;
  begin
    if kIncludesOverflow then
      num := 1;
    else
      num := 0;
    end if;
    return num;
  end function NumOfOvflBits;

  -----------------------------------------------------------------------------

  function PackVectorForEntityBoundaryCrossing (
    arg : std_logic_vector)
    return PackedVector_t
  is
    variable result : PackedVector_t := (others => '0');
  begin

    if not (kPackedVectorMaxWl >= arg'length) then
      ReportError
       (ErrorType => ExternalError,
        ShortDescription => "PackVectorForEntityBoundaryCrossing: We were given an " &
                            "argument of length " & integer'image(arg'length) &
                            " which is larger than maximum allowed length of " &
                            integer'image(kPackedVectorMaxWl));
    end if;

    if arg'high /= arg'left then
      -- Handles literals that are inferred as "to" format
      for i in arg'range loop
        result(arg'length-(i-arg'low)-1) := arg(i);
      end loop;
    else
      result(arg'length-1 downto 0) := arg;
    end if;

    return result;
  end function PackVectorForEntityBoundaryCrossing;

  function UnpackVectorFromEntityBoundaryCrossing (
    arg        : PackedVector_t;
    kIwl, kFwl : integer)
    return tFxpGen
  is
    constant kPadBits : integer := arg'length - (kIwl+kFwl);
    variable vResult  : tFxpGen(kIwl-1 downto -kFwl);
  begin
    vResult := tFxpGen(arg(arg'left-kPadBits downto arg'right));
    return vResult;
  end function UnpackVectorFromEntityBoundaryCrossing;

  -----------------------------------------------------------------------------

  function OpNeedsSpecialCoercion (
    constant kIntegerOperation : boolean;
    constant kInOneSigned      : boolean;
    constant kInOneIwl         : integer;
    constant kInTwoSigned      : boolean;
    constant kInTwoIwl         : integer)
    return boolean is
  begin

    if not kIntegerOperation then
      return false;
    elsif kInOneSigned = kInTwoSigned then
      return false;
    elsif kInOneIwl > kInTwoIwl then
      return not kInOneSigned;
    elsif kInTwoIwl > kInOneIwl then
      return not kInTwoSigned;
    end if;

    return true;

  end function OpNeedsSpecialCoercion;

  -----------------------------------------------------------------------------

  procedure CheckInvariant (arg : in tFxpGen) is
  begin

    if arg'length <= 0 then
      ReportError
       (ErrorType => ExternalError,
        ShortDescription => "Argument length (" & integer'image(arg'length) &
                            ") must be greater than zero.");
    end if;

  end procedure CheckInvariant;

  procedure CheckInvariant (arg : in tFxpUns) is
  begin
    CheckInvariant(tFxpGen(arg));
  end procedure CheckInvariant;

  procedure CheckInvariant (arg : in tFxpSgn)is
  begin
    CheckInvariant(tFxpGen(arg));
  end procedure CheckInvariant;

  -----------------------------------------------------------------------------
  -- Max/Min

  function Maximum (x, y : integer) return integer is
  begin
    return Larger(x, y);
  end function Maximum;

  function Maximum (x, y, z : integer) return integer is
  begin
    return Maximum(Maximum(x, y), z);
  end function Maximum;

  function Minimum (x, y : integer) return integer is
  begin
    return Smaller(x, y);
  end function Minimum;

  function Minimum (x, y, z : integer) return integer is
  begin
    return Minimum(Minimum(x, y), z);
  end function Minimum;

  -----------------------------------------------------------------------------
  -- Choose

  function Choose (selectFirst : boolean; a, b : boolean) return boolean is
  begin
    if selectFirst then
      return a;
    else
      return b;
    end if;
  end function Choose;

  function Choose (selectFirst : boolean; a, b : integer) return integer is
  begin
    if selectFirst then
      return a;
    else
      return b;
    end if;
  end function Choose;

  function Choose (selectFirst : boolean; a, b : tFxpGen) return tFxpGen is
  begin
    if selectFirst then
      return a;
    else
      return b;
    end if;
  end function Choose;

  function Choose (selectFirst : boolean; a, b : tFxpUns) return tFxpUns is
  begin
    return tFxpUns(Choose(selectFirst, tFxpGen(a), tFxpGen(b)));
  end function Choose;

  function Choose (selectFirst : boolean; a, b : tFxpSgn) return tFxpSgn is
  begin
    return tFxpSgn(Choose(selectFirst, tFxpGen(a), tFxpGen(b)));
  end function Choose;

  function Choose (selectFirst : boolean; a, b : FxpRoundMode_t) return FxpRoundMode_t is
  begin
    if selectFirst then
      return a;
    else
      return b;
    end if;
  end function Choose;

  function Choose (selectFirst : boolean; a, b : FxpOverflowMode_t)
  return FxpOverflowMode_t is
  begin
    if selectFirst then
      return a;
    else
      return b;
    end if;
  end function Choose;

  -----------------------------------------------------------------------------

  function SimpleResize (
    constant kArgSigned       : boolean;
    arg                       : tFxpGen;
    constant kReqIwl, kReqFwl : integer)
    return tFxpGen
  is

    constant kArgIwl : integer := arg'high+1;
    constant kArgFwl : integer := -arg'low;

    constant kArgHiIdx : integer := Minimum(kArgIwl, kReqIwl)-1;
    constant kArgLoIdx : integer := -Minimum(kArgFwl, kReqFwl);

    constant kOverlap : boolean := kArgHiIdx >= kArgLoIdx;

    -- Determine whether a sign (or zero) extension is required and
    -- create the sign extension vector we'll append onto the
    -- result. The HiIdx is created solely to ensure we don't get null
    -- vector. It won't be used in that case anyway.

    constant kSignExtReq   : boolean := kReqIwl > kArgIwl;
    constant kSignExtLoIdx : integer := Maximum(-kReqFwl, kArgHiIdx + 1);
    constant kSignExtHiIdx : integer := Maximum(kReqIwl, kSignExtLoIdx + 1);

    variable vSignExt : tFxpGen(kSignExtHiIdx-1 downto kSignExtLoIdx)
                        := (others => '0');

    variable vResult : tFxpGen(kReqIwl-1 downto -kReqFwl) := (others => '0');

  begin

    CheckInvariant(vResult);

    if kOverlap then
      -- Get the bits from the input that overlap with the output into
      -- the output vector.
      vResult(kArgHiIdx downto kArgLoIdx) := arg(kArgHiIdx downto kArgLoIdx);
    end if;

    if kSignExtReq then
      if kArgSigned then
        -- If the input argument is signed, we have to sign extend
        -- out. If its unsigned, we don't have to do anyting since the
        -- default value for the sign extension is set to zeros in the
        -- declaration.
        vSignExt := (others => arg(arg'left));
      end if;
      vResult(kReqIwl-1 downto kSignExtLoIdx) := vSignExt;
    end if;

    -- Notice we don't have to worry about the bits in the output to
    -- the right of the input argument since they are assigned zeros
    -- in the declaration of our result variable and can only be
    -- zeros.

    return vResult;

  end function SimpleResize;

  function SimpleResize (
    arg                       : tFxpUns;
    constant kReqIwl, kReqFwl : integer)
    return tFxpUns
  is
  begin
    return tFxpUns(SimpleResize(false, tFxpGen(arg), kReqIwl, kReqFwl));
  end function SimpleResize;

  function SimpleResize (
    arg                       : tFxpSgn;
    constant kReqIwl, kReqFwl : integer)
    return tFxpSgn
  is
  begin
    return tFxpSgn(SimpleResize(true, tFxpGen(arg), kReqIwl, kReqFwl));
  end function SimpleResize;

  -----------------------------------------------------------------------------

  function SignExtend (
    constant kArgSigned : boolean;
    arg                 : tFxpGen;
    constant kReqIwl    : integer)
    return tFxpGen
  is
    constant kArgIwl : integer := arg'left+1;
    constant kArgFwl : integer := -arg'right;
  begin

    if not (kReqIwl >= kArgIwl) then
      ReportError
       (ErrorType => ExternalError,
        ShortDescription => "Requested Iwl of " & integer'image(kReqIwl) &
                            " is less than original IWL of " &
                            integer'image(kArgIwl) & ".");
    end if;

    return SimpleResize(kArgSigned, arg, kReqIwl, kArgFwl);
  end function SignExtend;

  function SignExtend (arg : tFxpUns; constant kReqIwl : integer) return tFxpUns is
  begin
    return tFxpUns(SignExtend(false, tFxpGen(arg), kReqIwl));
  end function SignExtend;

  function SignExtend (arg : tFxpSgn; constant kReqIwl : integer) return tFxpSgn is
  begin
    return tFxpSgn(SignExtend(true, tFxpGen(arg), kReqIwl));
  end function SignExtend;

  -----------------------------------------------------------------------------

  function IsZero (arg : tFxpGen) return boolean is
    variable vZeros : tFxpGen(arg'range) := (others => '0');
  begin

    if arg'length <= 0 then
      return true;
    end if;

    CheckInvariant(vZeros);
    return arg = vZeros;
  end function IsZero;

  function IsZero (arg : tFxpUns) return boolean is
  begin
    return IsZero(tFxpGen(arg));
  end function IsZero;

  function IsZero (arg : tFxpSgn) return boolean is
  begin
    return IsZero(tFxpGen(arg));
  end function IsZero;

  function IsAllOnes (arg : tFxpGen) return boolean is
    variable vOnes : tFxpGen(arg'range) := (others => '1');
  begin
    return arg = vOnes;
  end function IsAllOnes;

  function IsAllOnes (arg : tFxpUns) return boolean is
  begin
    return IsAllOnes(tFxpGen(arg));
  end function IsAllOnes;

  function IsAllOnes (arg : tFxpSgn) return boolean is
  begin
    return IsAllOnes(tFxpGen(arg));
  end function IsAllOnes;

  -----------------------------------------------------------------------------

  function IsExactlyHalf (arg : tFxpGen) return boolean is
    variable vCompare : tFxpGen(arg'range) := (others => '0');
  begin
    CheckInvariant(vCompare);
    vCompare(vCompare'left) := '1';
    return arg = vCompare;
  end function IsExactlyHalf;

  function IsExactlyHalf (arg : tFxpUns) return boolean is
  begin
    return IsExactlyHalf(tFxpGen(arg));
  end function IsExactlyHalf;

  function IsExactlyHalf (arg : tFxpSgn) return boolean is
  begin
    return IsExactlyHalf(tFxpGen(arg));
  end function IsExactlyHalf;

  -----------------------------------------------------------------------------

  function Saturate (
    constant kSigned    : boolean;
    constant kIwl, kFwl : integer;
    constant kLimitType : SaturateLimit_t)
    return tFxpGen
  is
    variable vResult : tFxpGen(kIwl-1 downto -kFwl);
  begin

    if not (kIwl > -kFwl) then
      ReportError
       (ErrorType => ExternalError,
        ShortDescription => "Saturate called with parameters for a zero sized vector.");
    end if;

    case kLimitType is
      when kUpper =>
        if kSigned then
          vResult(vResult'left) := '0';
          if vResult'length > 1 then
            vResult(vResult'left-1 downto vResult'right) := (others => '1');
          end if;
        else
          vResult := (others => '1');
        end if;
      when kLower =>
        if kSigned then
          vResult(vResult'left) := '1';
          if vResult'length > 1 then
            vResult(vResult'left-1 downto vResult'right) := (others => '0');
          end if;
        else
          vResult := (others => '0');
        end if;
      when others =>
        ReportError
         (ErrorType => ExternalError,
          ShortDescription => "Invalid limit " & SaturateLimit_t'image(kLimitType) &
                              " passed to Saturate.");
    end case;

    CheckInvariant(vResult);

    return vResult;
  end function Saturate;

  function Saturate (
    constant kIwl, kFwl : integer;
    constant kLimitType : SaturateLimit_t)
    return tFxpUns
  is
  begin
    return tFxpUns(Saturate(false, kIwl, kFwl, kLimitType));
  end function Saturate;

  function Saturate (
    constant kIwl, kFwl : integer;
    constant kLimitType : SaturateLimit_t)
    return tFxpSgn
  is
  begin
    return tFxpSgn(Saturate(true, kIwl, kFwl, kLimitType));
  end function Saturate;

  -----------------------------------------------------------------------------
  function DescribeOperands (
    constant kLeftSigned  : boolean; left : tFxpGen;
    constant kRightSigned : boolean; right : tFxpGen)
    return OpDescriptionRetVal_t
  is
    constant kLeftLarger : boolean := Maximum(left'high, right'high) = left'high;

    constant kLargeSigned : boolean := Choose(kLeftLarger, kLeftSigned, kRightSigned);
    constant kLargeIwl    : integer := Choose(kLeftLarger, left'high+1, right'high+1);
    constant kLargeFwl    : integer := Choose(kLeftLarger, -left'low, -right'low);
    constant kLargeWl     : integer := kLargeIwl+kLargeFwl;

    constant kSmallSigned : boolean := Choose(kLeftLarger, kRightSigned, kLeftSigned);
    constant kSmallIwl    : integer := Choose(kLeftLarger, right'high+1, left'high+1);
    constant kSmallFwl    : integer := Choose(kLeftLarger, -right'low, -left'low);
    constant kSmallWl     : integer := kSmallIwl+kSmallFwl;

    variable vResult : OpDescriptionRetVal_t;
  begin

    CheckInvariant(left);
    CheckInvariant(right);

    vResult.kFirstOpLarger := kLeftLarger;
    vResult.kSmallSigned   := kSmallSigned;
    vResult.kSmallIwl      := kSmallIwl;
    vResult.kSmallFwl      := kSmallFwl;
    vResult.kLargeSigned   := kLargeSigned;
    vResult.kLargeIwl      := kLargeIwl;
    vResult.kLargeFwl      := kLargeFwl;
    return vResult;
  end function DescribeOperands;

  function DescribeOperands (
    left  : tFxpUns;
    right : tFxpUns)
    return OpDescriptionRetVal_t
  is
  begin
    return DescribeOperands(false, tFxpGen(left), false, tFxpGen(right));
  end function DescribeOperands;

  function DescribeOperands (
    left  : tFxpUns;
    right : tFxpSgn)
    return OpDescriptionRetVal_t
  is
  begin
    return DescribeOperands(false, tFxpGen(left), true, tFxpGen(right));
  end function DescribeOperands;

  function DescribeOperands (
    left  : tFxpSgn;
    right : tFxpUns)
    return OpDescriptionRetVal_t
  is
  begin
    return DescribeOperands(true, tFxpGen(left), false, tFxpGen(right));
  end function DescribeOperands;

  function DescribeOperands (
    left  : tFxpSgn;
    right : tFxpSgn)
    return OpDescriptionRetVal_t
  is
  begin
    return DescribeOperands(true, tFxpGen(left), true, tFxpGen(right));
  end function DescribeOperands;

  -----------------------------------------------------------------------------
  -- generic cast operators

  function to_Fxp (
    arg      : std_logic_vector;
    sizeType : tFxpGen;
    kSigned  : boolean := false)
    return tFxpGen
  is
    variable result : tFxpGen(arg'length + sizeType'low - 1 downto sizeType'low);
  begin
    result := tFxpGen(arg);
    CheckInvariant(result);
    return SimpleResize(kSigned, result, sizeType'left+1, -sizeType'right);
  end function to_Fxp;

  function to_Fxp (arg : integer; sizeType : tFxpGen) return tFxpGen is
    variable result : tFxpGen(sizeType'range);
  begin
    return to_Fxp(std_logic_vector(to_signed(arg, result'length)), result);
  end function to_Fxp;

  function to_slv (arg : tFxpGen) return std_logic_vector is
    variable vResult : std_logic_vector(arg'length-1 downto 0);
    variable vResIndex : integer := 0;
  begin
    CheckInvariant(arg);
    for i in arg'low to arg'high loop
      vResult(vResIndex) := arg(i);
      vResIndex := vResIndex + 1;
    end loop;
    return vResult;
  end function to_slv;

  function to_unsigned (arg : tFxpGen) return unsigned is
  begin
    return unsigned(to_slv(arg));
  end function to_unsigned;

  function to_signed (arg : tFxpGen) return signed is
  begin
    return signed(to_slv(arg));
  end function to_signed;

  -----------------------------------------------------------------------------
  -- unsigned cast operators

  function to_Fxp (arg : std_logic_vector; sizeType : tFxpUns) return tFxpUns is
  begin
    return tFxpUns(to_Fxp(arg, tFxpGen(sizeType), false));
  end function to_Fxp;

  function to_Fxp (arg : integer; sizeType : tFxpUns) return tFxpUns is
    variable result : tFxpUns(sizeType'range);
  begin
    return to_Fxp(std_logic_vector(to_unsigned(arg, result'length)), result);
  end function to_Fxp;

  function to_slv (arg : tFxpUns) return std_logic_vector is
  begin
    return to_slv(tFxpGen(arg));
  end to_slv;

  function to_unsigned (arg : tFxpUns) return unsigned is
  begin
    return to_unsigned(tFxpGen(arg));
  end function to_unsigned;

  function to_signed (arg : tFxpUns) return signed is
    variable vConcat : tFxpGen(arg'length downto 0);
  begin
    vConcat := '0' & tFxpGen(arg);
    return to_signed(vConcat);
  end function to_signed;

  -----------------------------------------------------------------------------
  -- signed cast operators

  function to_FxpSgn (arg : tFxpUns) return tFxpSgn is
    variable result : tFxpSgn(arg'high+1 downto arg'low);
  begin
    result := '0' & tFxpSgn(arg);
    return result;
  end function to_FxpSgn;

  function to_Fxp (arg : std_logic_vector; sizeType : tFxpSgn) return tFxpSgn is
  begin
    return tFxpSgn(to_Fxp(arg, tFxpGen(sizeType), true));
  end function to_Fxp;

  function to_Fxp (arg : integer; sizeType : tFxpSgn) return tFxpSgn is
    variable result : tFxpSgn(sizeType'range);
  begin
    return to_Fxp(std_logic_vector(to_signed(arg, result'length)), result);
  end function to_Fxp;

  function to_slv (arg : tFxpSgn) return std_logic_vector is
  begin
    return to_slv(tFxpGen(arg));
  end to_slv;

  function to_signed (arg : tFxpSgn) return signed is
  begin
    return to_signed(tFxpGen(arg));
  end function to_signed;

  -----------------------------------------------------------------------------
  -- Comparison Operations

  function "=" (x : tFxpUns; y : tFxpUns) return boolean is
    constant kResFwl : integer := -Minimum(x'right, y'right);
    constant kResIwl : integer := Maximum(x'left+1, y'left+1)+1;
    variable vRes    : tFxpUns(kResIwl-1 downto -kResFwl);
  begin

    return to_unsigned(SimpleResize(x, x'left+1, kResFwl)) =
      to_unsigned(SimpleResize(y, y'left+1, kResFwl));

  end function "=";

  function "=" (x : tFxpSgn; y : tFxpUns) return boolean is
  begin
    return x = to_FxpSgn(y);
  end function "=";

  function "=" (x : tFxpUns; y : tFxpSgn) return boolean is
  begin
    return to_FxpSgn(x) = y;
  end function "=";

  function "=" (x : tFxpSgn; y : tFxpSgn) return boolean is
    constant kResFwl : integer := -Minimum(x'right, y'right);
    constant kResIwl : integer := Maximum(x'left+1, y'left+1)+1;
    variable vRes    : tFxpSgn(kResIwl-1 downto -kResFwl);
  begin

    return to_signed(SimpleResize(x, x'left+1, kResFwl)) =
      to_signed(SimpleResize(y, y'left+1, kResFwl));

  end function "=";

  -----------------------------------------------------------------------------

  function "/=" (x : tFxpUns; y : tFxpUns) return boolean is
  begin
    return not (x = y);
  end function "/=";

  function "/=" (x : tFxpSgn; y : tFxpUns) return boolean is
  begin
    return not (x = y);
  end function "/=";

  function "/=" (x : tFxpUns; y : tFxpSgn) return boolean is
  begin
    return not (x = y);
  end function "/=";

  function "/=" (x : tFxpSgn; y : tFxpSgn) return boolean is
  begin
    return not (x = y);
  end function "/=";

  -----------------------------------------------------------------------------

  function ">" (x : tFxpUns; y : tFxpUns) return boolean is
  begin
    return not (x <= y);
  end function ">";

  function ">" (x : tFxpSgn; y : tFxpUns) return boolean is
  begin
    return not (x <= y);
  end function ">";

  function ">" (x : tFxpUns; y : tFxpSgn) return boolean is
  begin
    return not (x <= y);
  end function ">";

  function ">" (x : tFxpSgn; y : tFxpSgn) return boolean is
  begin
    return not (x <= y);
  end function ">";

  function ">=" (x : tFxpUns; y : tFxpUns) return boolean is
    constant kResFwl : integer := -Minimum(x'right, y'right);
    constant kResIwl : integer := Maximum(x'left+1, y'left+1)+1;
    variable vRes    : tFxpUns(kResIwl-1 downto -kResFwl);
  begin

    vRes := tFxpUns(
      to_unsigned(SimpleResize(x, kResIwl, kResFwl)) -
      to_unsigned(SimpleResize(y, kResIwl, kResFwl))
      );

    return vRes(vRes'left) = '0';

  end function ">=";

  function ">=" (x : tFxpSgn; y : tFxpUns) return boolean is
  begin
    return x >= to_FxpSgn(y);
  end function ">=";

  function ">=" (x : tFxpUns; y : tFxpSgn) return boolean is
  begin
    return to_FxpSgn(x) >= y;
  end function ">=";

  function ">=" (x : tFxpSgn; y : tFxpSgn) return boolean is
    constant kResFwl : integer := -Minimum(x'right, y'right);
    constant kResIwl : integer := Maximum(x'left+1, y'left+1)+1;
    variable vRes    : tFxpSgn(kResIwl-1 downto -kResFwl);
  begin

    vRes := tFxpSgn(
      to_signed(SimpleResize(x, kResIwl, kResFwl)) -
      to_signed(SimpleResize(y, kResIwl, kResFwl))
      );

    return vRes(vRes'left) = '0';

  end function ">=";

  -----------------------------------------------------------------------------

  function "<" (x : tFxpUns; y : tFxpUns) return boolean is
  begin
    return not (x >= y);
  end function "<";

  function "<" (x : tFxpSgn; y : tFxpUns) return boolean is
  begin
    return not (x >= y);
  end function "<";

  function "<" (x : tFxpUns; y : tFxpSgn) return boolean is
  begin
    return not (x >= y);
  end function "<";

  function "<" (x : tFxpSgn; y : tFxpSgn) return boolean is
  begin
    return not (x >= y);
  end function "<";

  -----------------------------------------------------------------------------

  function "<=" (x : tFxpUns; y : tFxpUns) return boolean is
  begin
    return y >= x;
  end function "<=";

  function "<=" (x : tFxpSgn; y : tFxpUns) return boolean is
  begin
    return x <= to_FxpSgn(y);
  end function "<=";

  function "<=" (x : tFxpUns; y : tFxpSgn) return boolean is
  begin
    return to_FxpSgn(x) <= y;
  end function "<=";

  function "<=" (x : tFxpSgn; y : tFxpSgn) return boolean is
  begin
    return y >= x;
  end function "<=";

end PkgNiFpgaSimFxp;
