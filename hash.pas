program hash;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Forms, Interfaces, Windows, Dialogs, HlpHashFactory, Clipbrd;

type

  { TMyHash }

  TMyHash = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteVersion; virtual;
    procedure WriteLicense; virtual;
    procedure WriteHelp; virtual;
    procedure WriteAlgorithmNotFound; virtual;
    procedure WriteFileNotFound; virtual;
  end;

{ TMyHash }

function adler32(FileName: String): String;
begin
  adler32 := LowerCase(THashFactory.TChecksum.CreateAdler32.ComputeFile(FileName).ToString());
end;

function crc16(FileName: String): String;
begin
  crc16 := LowerCase(THashFactory.TChecksum.TCRC.CreateCRC16_BUYPASS.ComputeFile(FileName).ToString());
end;

function crc32(FileName: String): String;
begin
  crc32 := LowerCase(THashFactory.TChecksum.TCRC.CreateCRC32_PKZIP.ComputeFile(FileName).ToString());
end;

function crc64(FileName: String): String;
begin
  crc64 := LowerCase(THashFactory.TChecksum.TCRC.CreateCRC64_ECMA_182.ComputeFile(FileName).ToString());
end;

function gost(FileName: String): String;
begin
  gost := LowerCase(THashFactory.TCrypto.CreateGost().ComputeFile(FileName).ToString());
end;

function md2(FileName: String): String;
begin
  md2 := LowerCase(THashFactory.TCrypto.CreateMD2().ComputeFile(FileName).ToString());
end;

function md4(FileName: String): String;
begin
  md4 := LowerCase(THashFactory.TCrypto.CreateMD4().ComputeFile(FileName).ToString());
end;

function md5(FileName: String): String;
begin
  md5 := LowerCase(THashFactory.TCrypto.CreateMD5().ComputeFile(FileName).ToString());
end;

function panama(FileName: String): String;
begin
  panama := LowerCase(THashFactory.TCrypto.CreatePanama().ComputeFile(FileName).ToString());
end;

function ripemd(FileName: String): String;
begin
  ripemd := LowerCase(THashFactory.TCrypto.CreateRIPEMD().ComputeFile(FileName).ToString());
end;

function ripemd128(FileName: String): String;
begin
  ripemd128 := LowerCase(THashFactory.TCrypto.CreateRIPEMD128().ComputeFile(FileName).ToString());
end;

function ripemd160(FileName: String): String;
begin
  ripemd160 := LowerCase(THashFactory.TCrypto.CreateRIPEMD160().ComputeFile(FileName).ToString());
end;

function ripemd256(FileName: String): String;
begin
  ripemd256 := LowerCase(THashFactory.TCrypto.CreateRIPEMD256().ComputeFile(FileName).ToString());
end;

function ripemd320(FileName: String): String;
begin
  ripemd320 := LowerCase(THashFactory.TCrypto.CreateRIPEMD320().ComputeFile(FileName).ToString());
end;

function sha0(FileName: String): String;
begin
  sha0 := LowerCase(THashFactory.TCrypto.CreateSHA0().ComputeFile(FileName).ToString());
end;

function sha1(FileName: String): String;
begin
  sha1 := LowerCase(THashFactory.TCrypto.CreateSHA1().ComputeFile(FileName).ToString());
end;

function sha224(FileName: String): String;
begin
  sha224 := LowerCase(THashFactory.TCrypto.CreateSHA2_224().ComputeFile(FileName).ToString());
end;

function sha256(FileName: String): String;
begin
  sha256 := LowerCase(THashFactory.TCrypto.CreateSHA2_256().ComputeFile(FileName).ToString());
end;

function sha384(FileName: String): String;
begin
  sha384 := LowerCase(THashFactory.TCrypto.CreateSHA2_384().ComputeFile(FileName).ToString());
end;

function sha512(FileName: String): String;
begin
  sha512 := LowerCase(THashFactory.TCrypto.CreateSHA2_512().ComputeFile(FileName).ToString());
end;

function sha512224(FileName: String): String;
begin
  sha512224 := LowerCase(THashFactory.TCrypto.CreateSHA2_512_224().ComputeFile(FileName).ToString());
end;

function sha512256(FileName: String): String;
begin
  sha512256 := LowerCase(THashFactory.TCrypto.CreateSHA2_512_256().ComputeFile(FileName).ToString());
end;

function sha3224(FileName: String): String;
begin
  sha3224 := LowerCase(THashFactory.TCrypto.CreateSHA3_224().ComputeFile(FileName).ToString());
end;

function sha3256(FileName: String): String;
begin
  sha3256 := LowerCase(THashFactory.TCrypto.CreateSHA3_256().ComputeFile(FileName).ToString());
end;

function sha3384(FileName: String): String;
begin
  sha3384 := LowerCase(THashFactory.TCrypto.CreateSHA3_384().ComputeFile(FileName).ToString());
end;

function sha3512(FileName: String): String;
begin
  sha3512 := LowerCase(THashFactory.TCrypto.CreateSHA3_512().ComputeFile(FileName).ToString());
end;

function tiger(FileName: String): String;
begin
  tiger := LowerCase(THashFactory.TCrypto.CreateTiger_3_192().ComputeFile(FileName).ToString());
end;

function tiger2(FileName: String): String;
begin
  tiger2 := LowerCase(THashFactory.TCrypto.CreateTiger2_3_192().ComputeFile(FileName).ToString());
end;

function whirlpool(FileName: String): String;
begin
  whirlpool := LowerCase(THashFactory.TCrypto.CreateWhirlPool().ComputeFile(FileName).ToString());
end;

procedure TMyHash.DoRun;
var
  ErrorMsg, AlgorithmName, FileName, Hash, HashMsg: String;
begin
  // check parameters
  ErrorMsg := CheckOptions('a:cf:hmv', 'algorithm: clipboard file: help message version');
  if ErrorMsg <> '' then
  begin
    WriteVersion;
    WriteHelp;
    Terminate;
    Exit;
  end;

  if (HasOption('h', 'help')) then
  begin
    WriteVersion;
    WriteHelp;
    Terminate;
    Exit;
  end;

  if (HasOption('v', 'version')) then
  begin
    WriteVersion;
    WriteLicense;
    Terminate;
    Exit;
  end;

  if ((HasOption('a', 'algorithm')) = false) or ((HasOption('f', 'file')) = false) then
  begin
    WriteVersion;
    WriteHelp;
    Terminate;
    Exit;
  end;

  // get values in parameters
  AlgorithmName := GetOptionValue('a', 'algorithm');
  FileName := GetOptionValue('f', 'file');

  // verify file exist
  if FileExists(FileName) = false then
  begin
    WriteFileNotFound;
    Terminate;
    Exit;
  end;

  // hash
  case AlgorithmName of
    'adler32' :
      begin
        Hash := adler32(FileName);
        HashMsg := 'The Adler-32 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'crc16' :
      begin
        Hash := crc16(FileName);
        HashMsg := 'The CRC-16 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'crc32' :
      begin
        Hash := crc32(FileName);
        HashMsg := 'The CRC-32 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'crc64' :
      begin
        Hash := crc64(FileName);
        HashMsg := 'The CRC-64 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'gost' :
      begin
        Hash := gost(FileName);
        HashMsg := 'The Gost hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'md2' :
      begin
        Hash := md2(FileName);
        HashMsg := 'The MD2 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'md4' :
      begin
        Hash := md4(FileName);
        HashMsg := 'The MD4 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'md5' :
      begin
        Hash := md5(FileName);
        HashMsg := 'The MD5 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'panama' :
      begin
        Hash := panama(FileName);
        HashMsg := 'The Panama hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'ripemd' :
      begin
        Hash := ripemd(FileName);
        HashMsg := 'The RIPEMD hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'ripemd128' :
      begin
        Hash := ripemd128(FileName);
        HashMsg := 'The RIPEMD-128 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'ripemd160' :
      begin
        Hash := ripemd160(FileName);
        HashMsg := 'The RIPEMD-160 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'ripemd256' :
      begin
        Hash := ripemd256(FileName);
        HashMsg := 'The RIPEMD-256 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'ripemd320' :
      begin
        Hash := ripemd320(FileName);
        HashMsg := 'The RIPEMD-320 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'sha0' :
      begin
        Hash := sha0(FileName);
        HashMsg := 'The SHA-0 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'sha1' :
      begin
        Hash := sha1(FileName);
        HashMsg := 'The SHA-1 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'sha224' :
      begin
        Hash := sha224(FileName);
        HashMsg := 'The SHA-224 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'sha256' :
      begin
        Hash := sha256(FileName);
        HashMsg := 'The SHA-256 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'sha384' :
      begin
        Hash := sha384(FileName);
        HashMsg := 'The SHA-384 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'sha512' :
      begin
        Hash := sha512(FileName);
        HashMsg := 'The SHA-512 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'sha512/224' :
      begin
        Hash := sha512224(FileName);
        HashMsg := 'The SHA-512/224 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'sha512/256' :
      begin
        Hash := sha512256(FileName);
        HashMsg := 'The SHA-512/256 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'sha3/224' :
      begin
        Hash := sha3224(FileName);
        HashMsg := 'The SHA3-224 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'sha3/256' :
      begin
        Hash := sha3256(FileName);
        HashMsg := 'The SHA3-256 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'sha3/384' :
      begin
        Hash := sha3384(FileName);
        HashMsg := 'The SHA3-384 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'sha3/512' :
      begin
        Hash := sha3512(FileName);
        HashMsg := 'The SHA3-512 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'tiger' :
      begin
        Hash := tiger(FileName);
        HashMsg := 'The Tiger hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'tiger2' :
      begin
        Hash := tiger2(FileName);
        HashMsg := 'The Tiger2 hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
    'whirlpool' :
      begin
        Hash := whirlpool(FileName);
        HashMsg := 'The WhirlPool hash of the file '''+ FileName + ''' is : ' + #13#10 + Hash;
      end;
  else
    WriteAlgorithmNotFound;
    Terminate;
    Exit;
  end;

  // clipboard
  if HasOption('c', 'clipboard') then begin
    Clipboard.AsText := Hash;
  end;

  // message
  if HasOption('m', 'message') then begin
    ShowWindow(GetConsoleWindow, SW_HIDE);
    Application.Initialize;
    Application.MessageBox(PChar(HashMsg), 'hash', $00000040);
  end
  else
    WriteLn(Hash);

  Terminate;
end;

constructor TMyHash.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TMyHash.Destroy;
begin
  inherited Destroy;
end;

procedure TMyHash.WriteVersion;
begin
  writeln('hash 1.1 : Copyright (c) 2021 Yoann LAMY');
  writeln();
end;

procedure TMyHash.WriteLicense;
begin
  writeln('You may redistribute copies of the program under the terms of the GNU General Public License v3 : https://github.com/ynlamy/hash.');
  writeln('This program come with ABSOLUTELY NO WARRANTY.');
  writeln('This program use the HashLib4Pascal library : https://github.com/Xor-el/HashLib4Pascal.');
end;

procedure TMyHash.WriteHelp;
begin
  writeln('Usage : ', ExeName, ' -a <algorithm> -f <filename> [-c] [-m]');
  writeln();
  writeln('-a <algorithm>, --algorithm=<algorithm> : Hash algorithm (adler32, crc16, crc32, crc64, gost, md2, md4, md5, panama, ripemd, ripemd128, ripemd160, ripemd256, ripemd320, sha0, sha1, sha224, sha256, sha384, sha512, sha512/224, sha512/256, sha3/224, sha3/256, sha3/384, sha3/512, tiger, tiger2, whirlpool)');
  writeln('-c, --clipboard : Copy the hash result to the clipboard');
  writeln('-f <filename>, --file=<filename> : Filename to generate the hash');
  writeln('-m, --message : Display the hash result in a dialog box');
  writeln('-h, --help : Print this help screen');
  writeln('-v, --version : Print the version of the program and exit');
end;

procedure TMyHash.WriteAlgorithmNotFound;
begin
  writeln('algorithm not found');
end;

procedure TMyHash.WriteFileNotFound;
begin
  writeln('file not found');
end;

var
  Application: TMyHash;

{$R *.res}

begin
  Application := TMyHash.Create(nil);
  Application.Run;
  Application.Free;
end.
