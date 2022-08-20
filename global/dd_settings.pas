{****************************************************
* 01.02.2021 /bc simple app settings persistence    *
****************************************************}
unit dd_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Inifiles, daily_diary_const;
type
  { TDDSettings }
  TDDSettings = class
  private
    fBatchCount: integer;
    fBatchUpdates: boolean;
    fIni: TIniFile;
    fBackupname: string;
    fDbname: string;
    function ReadInifile: boolean;
    function WriteInifile: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Update;
    property Databasename: string read fDbname write fDbname;
    property BackupPath: string read fBackupname write fBackupname;
    property BatchUpdates: boolean read fBatchUpdates write fBatchUpdates;
    property BatchCount: integer read fBatchCount write fBatchCount;
  end;

function DDSettings: TDDSettings;

implementation
var
  Singleton: TDDSettings;

function DDSettings: TDDSettings;
begin
  if not assigned(Singleton) then Singleton:= TDDSettings.Create;
  Result:= Singleton;
end;

{ TDDSettings }

function TDDSettings.ReadInifile: boolean;
begin
  if not FileExists(fIni.FileName) then WriteInifile; { create the darn thing }
  fDbname:= fIni.ReadString('Files','Databasename',DD_Databasename); { defaults to ./db/DD.db3 }
  fBackupname:= fIni.ReadString('Files','BackupName','Not defined'); { could be empty }
  fBatchUpdates:= fIni.ReadBool('Engine','BatchUpdates',false); { cache updates in db-engine }
  fBatchCount:= fIni.ReadInteger('Engine','BatchCount',3); { no of cache updates in db-engine }
  Result:= true;
end;

function TDDSettings.WriteInifile: boolean;
begin
  if fDbname <> '' then fIni.WriteString('Files','DatabaseName',fDbname)
  else fIni.WriteString('Files','DatabaseName',DD_Databasename); { failsafe }
  if fBackupname <> '' then fIni.WriteString('Files','BackupName',fBackupname)
  else fIni.WriteString('Files','BackupName','Not defined');
  fIni.WriteBool('Engine','BatchUpdates',fBatchUpdates); { initialized on 1.st run to false }
  fIni.WriteInteger('Engine','BatchCount',3); { no of cache updates in db-engine }
  fIni.UpdateFile;
  Result:= true;;
end;

constructor TDDSettings.Create;
begin
  inherited Create;
  fIni:= TIniFile.Create(DD_Inifilename); { reads the ini if it exists... }
  fIni.CacheUpdates:= false; { update file immediately }
  ReadInifile;
end;

destructor TDDSettings.Destroy;
begin
  FreeAndNil(fIni);
  inherited Destroy;
end;

procedure TDDSettings.Update;
begin
  WriteInifile;
end;
initialization
  Singleton:= nil;
finalization
  if assigned(Singleton) then FreeAndNil(Singleton);
end.

