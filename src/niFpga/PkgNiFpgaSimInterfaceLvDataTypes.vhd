-- © 2012 National Instruments Corporation.
--Use of this file is subject to the Software License Agreement provided with 
--LabVIEW FPGA Module and, without limiting any of the provisions in that license, 
--modifying or distributing this file is prohibited.

--Adding to, deleting from, or otherwise modifying the contents of this file 
--may cause the software to malfunction.
-------------------------------------------------------------------------------
--
-- Purpose:
-- This package contains types specific to LabVIEW that you use most commonly
-- in the test bench environment.
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.PkgNiUtilities.all;
use work.PkgNiFpgaSimInterfaceTypes.all;
use work.PkgNiFpgaUtilities.all;
use work.PkgFloat.all;

package PkgNiFpgaSimInterfaceLvDataTypes is

  --===========================================================================
  --LabVIEW Data Types
  --===========================================================================

  subtype tI8 is signed(7 downto 0);
  subtype tI16 is signed(15 downto 0);
  subtype tI32 is signed(31 downto 0);
  subtype tI64 is signed(63 downto 0);
  subtype tU8 is unsigned(7 downto 0);
  subtype tU16 is unsigned(15 downto 0);
  subtype tU32 is unsigned(31 downto 0);
  subtype tU64 is unsigned(63 downto 0);
  subtype tSGL is FloatGen_t(31 downto 0);

  type tI8_Array is array (natural range <>) of tI8;
  type tI16_Array is array (natural range <>) of tI16;
  type tI32_Array is array (natural range <>) of tI32;
  type tI64_Array is array (natural range <>) of tI64;
  type tU8_Array is array (natural range <>) of tU8;
  type tU16_Array is array (natural range <>) of tU16;
  type tU32_Array is array (natural range <>) of tU32;
  type tU64_Array is array (natural range <>) of tU64;
  type tBoolean_Array is array (natural range <>) of boolean;

  ---------------------------------------------------------------------------------------
  -- Data Type to/from tDataArray Conversion Functions
  ---------------------------------------------------------------------------------------
  function ReturnDataArrayType (Data : tI64) return tDataArray;
  function ReturnI64 (Data : tDataArray) return tI64;
  function ReturnDataType (Data : signed) return tData;
  function ReturnI32 (Data : tData) return tI32;
  function ReturnI16 (Data : tData) return tI16;
  function ReturnI8 (Data : tData) return tI8;
  function ReturnDataArrayType (Data : tU64) return tDataArray;
  function ReturnU64 (Data : tDataArray) return tU64;
  function ReturnDataType (Data : unsigned) return tData;
  function ReturnU32 (Data : tData) return tU32;
  function ReturnU16 (Data : tData) return tU16;
  function ReturnU8 (Data : tData) return tU8;
  function ReturnDataType (Data : boolean) return tData;

  function ReturnI64_ArrayType (Data : tDataArray) return tI64_Array;
  function ReturnDataArrayType (Data : tI64_Array) return tDataArray;
  function ReturnI32_ArrayType (Data : tDataArray) return tI32_Array;
  function ReturnDataArrayType (Data : tI32_Array) return tDataArray;
  function ReturnI16_ArrayType (Data : tDataArray) return tI16_Array;
  function ReturnDataArrayType (Data : tI16_Array) return tDataArray;
  function ReturnI8_ArrayType (Data : tDataArray) return tI8_Array;
  function ReturnDataArrayType (Data : tI8_Array) return tDataArray;
  function ReturnU64_ArrayType (Data : tDataArray) return tU64_Array;
  function ReturnDataArrayType (Data : tU64_Array) return tDataArray;
  function ReturnU32_ArrayType (Data : tDataArray) return tU32_Array;
  function ReturnDataArrayType (Data : tU32_Array) return tDataArray;
  function ReturnU16_ArrayType (Data : tDataArray) return tU16_Array;
  function ReturnDataArrayType (Data : tU16_Array) return tDataArray;
  function ReturnU8_ArrayType (Data : tDataArray) return tU8_Array;
  function ReturnDataArrayType (Data : tU8_Array) return tDataArray;
  function ReturnBoolean_ArrayType (Data : tDataArray) return tBoolean_Array;
  function ReturnDataArrayType (Data : tBoolean_Array) return tDataArray;

end package PkgNiFpgaSimInterfaceLvDataTypes;


package body PkgNiFpgaSimInterfaceLvDataTypes is
  ---------------------------------------------------------------------------------------
  -- Data Type to/from tDataArray Conversion Functions
  ---------------------------------------------------------------------------------------
  function ReturnDataArrayType (Data : tI64) return tDataArray is
    variable Var : tDataArray(1 downto 0) := (others => (others => '0'));
  begin

    Var(0) := std_logic_vector(Data(63 downto 32));
    Var(1) := std_logic_vector(Data(31 downto 0));
    return Var;
  end ReturnDataArrayType;

  function ReturnDataType (Data : signed) return tData is
    variable Var : tData := (others => '0');
  begin

    if not (Data'length = 32 or Data'length = 16 or Data'length = 8) then
      ReportError
        (ErrorType => ExternalError,
        ShortDescription => "ReturnDataType signed range doesn't match a support LabVIEW I32, I16 or I8 data type.");
    end if;
    Var(Data'range) := std_logic_vector(Data);
    return Var;
  end ReturnDataType;

  function ReturnDataArrayType (Data : tU64) return tDataArray is
    variable Var : tDataArray(1 downto 0) := (others => (others => '0'));
  begin
    Var(0) := std_logic_vector(Data(63 downto 32));
    Var(1) := std_logic_vector(Data(31 downto 0));
    return Var;
  end ReturnDataArrayType;

  function ReturnDataType (Data : unsigned) return tData is
    variable Var : tData := (others => '0');
  begin
    if not (Data'length = 32 or Data'length = 16 or Data'length = 8) then
      ReportError
        (ErrorType => ExternalError,
        ShortDescription => "ReturnDataType unsigned range doesn't match a support LabVIEW U32, U16 or U8 data type.");
    end if;
    Var(Data'range) := std_logic_vector(Data);
    return Var;
  end ReturnDataType;

  function ReturnI64 (Data : tDataArray) return tI64 is
    variable Var : tI64 := (others => '0');
    variable VarTemp : std_logic_vector(kFiDataWidth*2-1 downto 0);

  begin
    VarTemp := Data(0) & Data(1);
    Var := tI64(signed(VarTemp));
    return Var;
  end ReturnI64;

  function ReturnI32 (Data : tData) return tI32 is
    variable Var : tI32 := (others => '0');
  begin
    Var := signed(Data(Var'range));
    return Var;
  end ReturnI32;

  function ReturnI16 (Data : tData) return tI16 is
    variable Var : tI16 := (others => '0');
  begin
    Var := signed(Data(Var'range));
    return Var;
  end ReturnI16;

  function ReturnI8 (Data : tData) return tI8 is
    variable Var : tI8:= (others => '0');
  begin
    Var := signed(Data(Var'range));    return Var;
  end ReturnI8;

  function ReturnU64 (Data : tDataArray) return tU64 is
    variable Var : tU64 := (others => '0');
    variable VarTemp : std_logic_vector(kFiDataWidth*2-1 downto 0);
  begin
    VarTemp := Data(0)&Data(1);
    Var := unsigned(VarTemp);
    return Var;
  end ReturnU64;

  function ReturnU32 (Data : tData) return tU32 is
    variable Var : tU32 := (others => '0');
  begin
    Var := tU32(unsigned(Data));
    return Var;
  end ReturnU32;

  function ReturnU16 (Data : tData) return tU16 is
    variable Var : tU16 := (others => '0');
  begin
    Var := tU16(unsigned(Data(Var'range)));
    return Var;
  end ReturnU16;

  function ReturnU8 (Data : tData) return tU8 is
    variable Var : tU8 := (others => '0');
  begin
    Var := tU8(unsigned(Data(Var'range)));
    return Var;
  end ReturnU8;

  function ReturnI64_ArrayType (Data : tDataArray) return tI64_Array is
    variable Var : tI64_Array(Data'length/2-1 downto 0) := (others => (others => '0'));
    variable DataElement : tDataArray(1 downto 0);
  begin
    for i in Var'range loop
      DataElement(0) := Data(i*2);
      DataElement(1) := Data(i*2+1);
      Var(i) := ReturnI64(DataElement);
    end loop;
    return Var;
  end ReturnI64_ArrayType;

  function ReturnDataArrayType (Data : tI64_Array) return tDataArray is
    variable Var : tDataArray(Data'length*2-1 downto 0) := (others => (others => '0'));
    variable DataElement : tDataArray(1 downto 0);
  begin
    for i in Data'range loop
      DataElement := ReturnDataArrayType(Data(i));
      Var(i*2) := DataElement(0);
      Var(i*2+1) := DataElement(1);
    end loop;
    return Var;
  end ReturnDataArrayType;

  function ReturnI32_ArrayType (Data : tDataArray) return tI32_Array is
    variable Var : tI32_Array(Data'range) := (others => (others => '0'));
  begin
    for i in Var'range loop
      Var(i) := ReturnI32(Data(i));
    end loop;
    return Var;
  end ReturnI32_ArrayType;

  function ReturnDataArrayType (Data : tI32_Array) return tDataArray is
    variable Var : tDataArray(Data'range) := (others => (others => '0'));
  begin
    for i in Data'range loop
      Var(i) := ReturnDataType(Data(i));
    end loop;
    return Var;
  end ReturnDataArrayType;

  function ReturnI16_ArrayType (Data : tDataArray) return tI16_Array is
    variable Var : tI16_Array(Data'range) := (others => (others => '0'));
  begin
    for i in Var'range loop
      Var(i) := ReturnI16(Data(i));
    end loop;
    return Var;
  end ReturnI16_ArrayType;

  function ReturnDataArrayType (Data : tI16_Array) return tDataArray is
    variable Var : tDataArray(Data'range) := (others => (others => '0'));
  begin
    for i in Data'range loop
      Var(i) := ReturnDataType(Data(i));
    end loop;
    return Var;
  end ReturnDataArrayType;

  function ReturnI8_ArrayType (Data : tDataArray) return tI8_Array is
    variable Var : tI8_Array(Data'range) := (others => (others => '0'));
  begin
    for i in Var'range loop
      Var(i) := ReturnI8(Data(i));
    end loop;
    return Var;
  end ReturnI8_ArrayType;

  function ReturnDataArrayType (Data : tI8_Array) return tDataArray is
    variable Var : tDataArray(Data'range) := (others => (others => '0'));
  begin
    for i in Data'range loop
      Var(i) := ReturnDataType(Data(i));
    end loop;
    return Var;
  end ReturnDataArrayType;

  function ReturnU64_ArrayType (Data : tDataArray) return tU64_Array is
    variable Var : tU64_Array(Data'length/2-1 downto 0) := (others => (others => '0'));
    variable DataElement : tDataArray(1 downto 0);
  begin
    for i in Var'range loop
      DataElement(0) := Data(i*2);
      DataElement(1) := Data(i*2+1);
      Var(i) := ReturnU64(DataElement);
    end loop;
    return Var;
  end ReturnU64_ArrayType;

  function ReturnDataArrayType (Data : tU64_Array) return tDataArray is
    variable Var : tDataArray(Data'length*2-1 downto 0) := (others => (others => '0'));
    variable DataElement : tDataArray(1 downto 0);
  begin
    for i in Data'range loop
      DataElement := ReturnDataArrayType(Data(i));
      Var(i*2) := DataElement(0);
      Var(i*2+1) := DataElement(1);
    end loop;
    return Var;
  end ReturnDataArrayType;

  function ReturnU32_ArrayType (Data : tDataArray) return tU32_Array is
    variable Var : tU32_Array(Data'range) := (others => (others => '0'));
  begin
    for i in Var'range loop
      Var(i) := ReturnU32(Data(i));
    end loop;
    return Var;
  end ReturnU32_ArrayType;

  function ReturnDataArrayType (Data : tU32_Array) return tDataArray is
    variable Var : tDataArray(Data'range) := (others => (others => '0'));
  begin
    for i in Data'range loop
      Var(i) := ReturnDataType(Data(i));
    end loop;
    return Var;
  end ReturnDataArrayType;

  function ReturnU16_ArrayType (Data : tDataArray) return tU16_Array is
    variable Var : tU16_Array(Data'range) := (others => (others => '0'));
  begin
    for i in Var'range loop
      Var(i) := ReturnU16(Data(i));
    end loop;
    return Var;
  end ReturnU16_ArrayType;

  function ReturnDataArrayType (Data : tU16_Array) return tDataArray is
    variable Var : tDataArray(Data'range) := (others => (others => '0'));
  begin
    for i in Data'range loop
      Var(i) := ReturnDataType(Data(i));
    end loop;
    return Var;
  end ReturnDataArrayType;

  function ReturnU8_ArrayType (Data : tDataArray) return tU8_Array is
    variable Var : tU8_Array(Data'range) := (others =>(others => '0'));
  begin
    for i in Var'range loop
      Var(i) := ReturnU8(Data(i));
    end loop;
    return Var;
  end ReturnU8_ArrayType;

  function ReturnDataArrayType (Data : tU8_Array) return tDataArray is
    variable Var : tDataArray(Data'range) := (others => (others => '0'));
  begin
    for i in Data'range loop
      Var(i) := ReturnDataType(Data(i));
    end loop;
    return Var;
  end ReturnDataArrayType;

  function ReturnDataType (Data : boolean) return tData is
    variable Var : tData := (others => '0');
  begin
    Var(0) := to_StdLogic(Data);
    return Var;
  end ReturnDataType;

  function ReturnBoolean_ArrayType (Data : tDataArray) return tBoolean_Array is
    variable Var : tBoolean_Array(Data'range) := (others =>false);
  begin
    for i in Var'range loop
      Var(i) := to_Boolean(Data(i)(0));
    end loop;
    return Var;
  end ReturnBoolean_ArrayType;

  function ReturnDataArrayType (Data : tBoolean_Array) return tDataArray is
    variable Var : tDataArray(Data'range) := (others => (others => '0'));
  begin
    for i in Data'range loop
      Var(i) := ReturnDataType(Data(i));
    end loop;
    return Var;
  end ReturnDataArrayType;

end package body PkgNiFpgaSimInterfaceLvDataTypes;