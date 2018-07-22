program hash;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Forms, Interfaces, Dialogs, md5, Clipbrd;

type

  { TMyHash }

  TMyHash = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure WriteLicense; virtual;
    procedure WriteVersion; virtual;
    procedure WriteAlgorithmNotFound; virtual;
    procedure WriteFileNotFound; virtual;
  end;

{ TMyHash }

function md5(FileName: String): String;
begin
  // md5 hash
  md5 := MD5Print(MD5File(FileName));
end;

procedure TMyHash.DoRun;
var
  ErrorMsg, AlgorithmName, FileName, Hash, HashMsg: String;
begin
  // check parameters
  ErrorMsg := CheckOptions('a:cf:hmv', 'algorithm: clipboard file: help message version');
  if ErrorMsg <> '' then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if (HasOption('h', 'help')) then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if (HasOption('v', 'version')) then
  begin
    WriteVersion;
    Terminate;
    Exit;
  end;

  if ((HasOption('a', 'algorithm')) = false) or ((HasOption('f', 'file')) = false) then
  begin
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
    'md5' :
      begin
        Hash := md5(FileName);
        HashMsg := 'The md5 hash of the file '''+ FileName + ''' is : ' + Hash;
      end;
    'sha1' :
      begin

      end;
    'sha256' :
      begin

      end;
    'sha512' :
      begin

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
    Application.Initialize;
    //ShowMessage(HashMsg);
    Application.MessageBox('test', 'test', $00000040);
  end
  else
    WriteLn(HashMsg);

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

procedure TMyHash.WriteHelp;
begin
  WriteVersion;
  writeln();
  writeln('Usage: ', ExeName, ' -a <algorithm> -f <file_name> [-c] [-m]');
  writeln();
  writeln('-a <algorithm>, --algorithm=<algorithm> : Hash algorithm (md5, sha1, sha256, sha512)');
  writeln('-c, --clipboard : Place the hash result on the clipboard');
  writeln('-f <file_name>, --file=<file_name> : Filename to generate the hash');
  writeln('-m, --message : Show the hash result in a dialog box');
  writeln('-h, --help : Print this help screen');
  writeln('-v, --version : Print the version of the program and exit');
  writeln();
  WriteLicense;
end;

procedure TMyHash.WriteLicense;
begin
  writeln('This program come with ABSOLUTELY NO WARRANTY.');
  writeln('You may redistribute copies of the program under the terms of the GNU General Public License v2.');
end;

procedure TMyHash.WriteVersion;
begin
  writeln('hash 1.0 : Copyright (c) 2018 Yoann LAMY');
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
  Application.Title := 'hash';
  Application.Run;
  Application.Free;
end.
