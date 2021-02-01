unit bom_dd;
{$mode objfpc}{$H+}
{$define debug}
interface
uses
  Classes, SysUtils, db,
  bc_utilities,
  dd_settings,
  daily_diary_const,
  bc_datetime,
  bc_mtlinklist,
  bc_litedb;
type
  { TNamedMemorystream }
  TNamedMemorystream = class(TMemoryStream)
  private
    fName: string;
  public
    constructor Create(const aName: string);
    property Name: string read fName write fName;
  end;

  TDDCollection = class;
  { *** TDDCollectionItem *** }
  TDDCollectionItem = class(TCollectionItem)
  private
    fId_DD: ptruint;       // id from database
    fDate: TIsoDate;       // well duh!
    fWeekNumber: ptruint;  // week number
    fText: TStream;        // binary large object - can be anything
//    fText: TNamedMemorystream;
    fReserved: string;     // text field reserved for future use
    fModified: byte;       // modification status
  protected
    procedure AssignData(aSource: TDDCollectionItem); // nifty little feature
  public
    constructor Create(aCollection: TDDCollection);
    destructor Destroy;
    property Id_DD: ptruint read fId_DD write fId_DD;
    property Date: TIsoDate read fDate write fDate;
    property WeekNumber: ptruint read fWeekNumber write fWeekNumber;
    property Text: TStream read fText write fText;
//    property Text: TNamedMemorystream read fText write fText;
    property Reserved: string read fReserved write fReserved;
    property Modified: byte read fModified write fModified;
  end; { TDDCollectionItem }

  { *** TDDQueue *** }
  TDDQueue = class(TbcQueue)
  public
    function CreateNew: TDDCollectionItem;
    procedure Enqueue(anItem: TDDCollectionItem);
    function Dequeue: TDDCollectionItem;
    function Peek: TDDCollectionItem; // only peeking does not remove from Q
  end;

  { *** TDDCollection *** }
  TDDCollection = class(TCollection)
  private
    fBatch: boolean;
    fSortOrder: integer;
    function get_DbName: string;
    function get_EngineVersion: string;
    procedure set_DbName(aValue: string);
    procedure DeleteItem(anItem: TDDCollectionItem); { remove item from collection }
  protected
    fDb: TLiteDb;
    fDeltaQueue: TDDQueue;
    fUpdateCount: ptrint;
    procedure DoUpdate; { refactored 29.07.2015 bc }
    function AddRecord(anItem: TDDCollectionItem): ptruint; // result is the new ID for the current record
    procedure UpdateRecord(anItem: TDDCollectionItem);
    procedure DeleteRecord(anItem: TDDCollectionItem); { remove from database backend }
  public
    constructor Create(anItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function CheckTable: boolean; { creates a new table in db-file }
    function AddNew: TDDCollectionItem;
    procedure BackupDb; { 19.04.2015 bc }
    procedure AppendToDelta(anItem: TDDCollectionItem); { api }
    function IndexOf(anItem: TDDCollectionItem): ptrint; { 11.05.2015 bc, returns -1 on not found else collection ID }
    function UpdateDb(const UpdateNow: boolean): boolean; { refactored 29.07.2015 bc }
    function ReadDb: boolean;
    property UpdateCount: ptrint read fUpdateCount write fUpdateCount;
    property DbName: string read get_DbName write set_DbName;
    property BatchUpdate: boolean read fBatch write fBatch;
    property SortOrder: integer read fSortOrder write fSortOrder;
    property EngineVersion: string read get_EngineVersion;
  end; { *** TDDCollection *** }

{ Bom - factory }
function CreateBom: TDDCollection;

implementation
uses bc_memdataset;
var Singleton: TDDCollection;

function CreateBom: TDDCollection;
begin
  if not assigned(Singleton) then Singleton:= TDDCollection.Create(TDDCollectionItem);
  Result:= Singleton;
end;

{ TDDCollection }

function TDDCollection.get_DbName: string;
begin

end;

function TDDCollection.get_EngineVersion: string;
begin

end;

procedure TDDCollection.set_DbName(aValue: string);
begin

end;

procedure TDDCollection.DeleteItem(anItem: TDDCollectionItem);
begin

end;

procedure TDDCollection.DoUpdate;
begin

end;

function TDDCollection.AddRecord(anItem: TDDCollectionItem): ptruint;
begin

end;

procedure TDDCollection.UpdateRecord(anItem: TDDCollectionItem);
begin

end;

procedure TDDCollection.DeleteRecord(anItem: TDDCollectionItem);
begin

end;

constructor TDDCollection.Create(anItemClass: TCollectionItemClass);
begin
  inherited Create(anItemClass);
  fDb:= TLiteDb.Create;
  fDb.DbName:= DDSettings.Databasename; { 19.04.2015 /bc }
  fDb.Connect;
  CheckTable; // create table if non existing
  if fDb.Connected then fDb.DisConnect; // no idle connections
  fDeltaQueue:= TDDQueue.Create; // delta for use with insertion
  fBatch:= false;
  fBatch:= DDSettings.BatchUpdates; { 19.04.2015 /bc }
  fUpdateCount:= DDSettings.BatchCount; { 19.04.2015 /bc }
end;

destructor TDDCollection.Destroy;
begin
  inherited Destroy;
end;

function TDDCollection.CheckTable: boolean;
begin

end;

function TDDCollection.AddNew: TDDCollectionItem;
begin

end;

procedure TDDCollection.BackupDb;
begin

end;

procedure TDDCollection.AppendToDelta(anItem: TDDCollectionItem);
begin

end;

function TDDCollection.IndexOf(anItem: TDDCollectionItem): ptrint;
begin

end;

function TDDCollection.UpdateDb(const UpdateNow: boolean): boolean;
begin

end;

function TDDCollection.ReadDb: boolean;
begin

end;

{ TDDCollectionItem }
{ nifty feature if you need to clone an item }
procedure TDDCollectionItem.AssignData(aSource: TDDCollectionItem);
begin
  fId_DD:= aSource.Id_DD;           // id from database
  fDate:= aSource.Date;             // well duh!
  fWeekNumber:= aSource.WeekNumber; // week number
  fText:= aSource.Text;             // binary large object - can be anything
  fReserved:= aSource.Reserved;     // text field reserved for future use
  fModified:= aSource.Modified;     // modification states ~ mNone, mAdded, mAltered & mDelete
end;

constructor TDDCollectionItem.Create(aCollection: TDDCollection);
begin

end;

destructor TDDCollectionItem.Destroy;
begin

end;

{ TNamedMemorystream }

constructor TNamedMemorystream.Create(const aName: string);
begin
  inherited Create;
  fName:= aName;
end;

{ ================== TBlobQueue ================== }
function TBlobQueue.CreateNew: TBlobCollectionItem;
begin
  Result:= TBlobCollectionItem.Create(nil); // no collection, ie. not appended yet
end;

procedure TBlobQueue.Enqueue(anItem: TBlobCollectionItem);
begin
  En_Queue(pointer(anItem));
end;

function TBlobQueue.Dequeue: TBlobCollectionItem;
begin
  Result:= TBlobCollectionItem(De_Queue);
end;

function TBlobQueue.Peek: TBlobCollectionItem;
begin
  Result:= TBlobCollectionItem(Examine);
end;

{ ================== TBlobQueue ================== }

{ compares first names ie,:
    Result = 1 -> Item1 is greater than Item2
    Result = -1 -> Item1 is smaller than Item2
    else they are equal -> 0 }
function BlobCompareDocName(Item1, Item2: TCollectionItem): Integer;
begin
  if TBlobCollectionItem(Item1).DocName > TBlobCollectionItem(Item2).DocName then Result:= 1
  else if TBlobCollectionItem(Item1).DocName < TBlobCollectionItem(Item2).DocName then Result:= -1
  else Result:= 0;
end;

function BlobCompareDate(Item1, Item2: TCollectionItem): Integer;
begin
  if TBlobCollectionItem(Item1).Date.AsInteger > TBlobCollectionItem(Item2).Date.AsInteger then Result:= 1
  else if TBlobCollectionItem(Item1).Date.AsInteger < TBlobCollectionItem(Item2).Date.AsInteger then Result:= -1
  else Result:= 0;
end;

function BlobCompareDocType(Item1, Item2: TCollectionItem): Integer;
begin
  if TBlobCollectionItem(Item1).DocType > TBlobCollectionItem(Item2).DocType then Result:= 1
  else if TBlobCollectionItem(Item1).DocType < TBlobCollectionItem(Item2).DocType then Result:= -1
  else Result:= 0;
end;

function TDDCollection.get_DbName: string;
begin
  Result:= fDb.DbName;
end;

function TDDCollection.get_EngineVersion: string;
begin
  Result:= daily_diary_const.UnitVersion;
end;

procedure TDDCollection.set_DbName(aValue: string);
begin
  fDb.DbName:= aValue;
end;

procedure TDDCollection.DeleteItem(anItem: TDDCollectionItem);
var
  Res,ItemId: ptrint;
begin
  Res:= -1;
  ItemId:= anItem.ID;
  Res:= bcSearch(ItemId,Self);
  if Res > -1 then Delete(Res);
end;

function TDDCollection.CheckTable: boolean; { creates a new db if not existing }
begin
  Result:= true;
  { creates a new db if not existing }
  try fDb.RunSQL(daily_diary_const.CreateDb); except  end;
end;

procedure TDDCollection.DoUpdate; { refactored 29.07.2015 bc }
var
  Tmp,New: TDDCollectionItem;
begin
  while not fDeltaQueue.IsEmpty do begin
    Tmp:= fDeltaQueue.Dequeue;
    case Tmp.Modified of
      mAdded: begin
                New:= TDDCollectionItem(Add); { gets an ownership from collection }
                AddRecord(Tmp);
                New.AssignData(Tmp);
                //ææ
              end;
      mAltered: fDb.RunSQL('');
      mDelete: begin
                 DeleteRecord(Tmp);  { takes care of the database back-end }
                 DeleteItem(Tmp);   { removes the item from our collection }
               end;
    end;
    FreeAndNil(Tmp);
  end;
end;

{ addrecord persists anitem to database and returns the new row_id as a result }
function TDDCollection.AddRecord(anItem: TDDCollectionItem): ptruint;
begin
  if fDb.Connect then begin
    if not fDb.Transaction.Active then begin
      fDb.Transaction.StartTransaction;
      fDb.Query.Close;
      fDb.Query.SQL.Text:= InsBlobSql;
      fDb.Query.Prepare;
      fDb.Query.ParamByName('pdocname').AsString:= anItem.DocName;
      fDb.Query.ParamByName('pdoctype').AsString:= anItem.DocType;
      fDb.Query.ParamByName('pdate').AsInteger:= anItem.Date.AsInteger;
      anItem.Blob.Position:= 0; { always remember to reset position }
      fDb.Query.ParamByName('pblob').LoadFromStream(anItem.Blob,ftBlob);
      fDb.Query.ParamByName('pflag').AsInteger:= anItem.Flag.ToInteger;
      fDb.Query.ParamByName('preserved').AsString:= anItem.Reserved;
      fDb.Query.ExecSQL;
      { now get a hold of our last entry ID }
      fDb.Query.Close;
      fDb.Query.SQL.Text:= LastIdSql; { 'SELECT LAST_INSERT_ROWID() AS Id_Last;' }
      fDb.Query.Open;
      Result:= fDb.Query.FieldByName('Id_Last').AsInteger;
      anItem.Id_DD:= Result;
      fDb.Query.Close;
      fDb.Transaction.Commit;
      anItem.Modified:= mNone;
    end;
    fDb.DisConnect; { no dangling connections }
  end;
end;

procedure TDDCollection.UpdateRecord(anItem: TBlobCollectionItem);
begin
  // TODO
end;

procedure TDDCollection.DeleteRecord(anItem: TDDCollectionItem); { ok }
begin
  if fDb.Connect then begin
    if not fDb.Transaction.Active then begin
      fDb.Transaction.StartTransaction;
      fDb.Query.Close;
      fDb.Query.SQL.Text:= DelBlobSql;
      fDb.Query.Prepare;
      fDb.Query.ParamByName('pid').AsInteger:= anItem.Id_DD;
      fDb.Query.ExecSQL;
      fDb.Transaction.Commit;
    end;
    fDb.DisConnect;
  end;
end;

constructor TDDCollection.Create(anItemClass: TCollectionItemClass); { ok }
begin
  inherited Create(anItemClass);
  fDb:= TLiteDb.Create;
  fDb.DbName:= DDSettings.Databasename; { 19.04.2015 bc }
  fDb.Connect;
  CheckTable; // create table if non existing
  if fDb.Connected then fDb.DisConnect; // no idle connections
  fDeltaQueue:= TDDQueue.Create;
  fBatch:= false;
  fBatch:= DDSettings.BatchUpdates; { 19.04.2015 bc }
  fUpdateCount:= DDSettings.BatchCount; { 19.04.2015 bc }
end;

destructor TDDCollection.Destroy;
begin
  if not fDeltaQueue.IsEmpty then UpdateDb(true);
  fDeltaQueue.Free;
  if fDb.Connected then fDb.DisConnect;
  fDb.Free;
  inherited Destroy;
end;

function TDDCollection.AddNew: TDDCollectionItem; { ok }
begin
  Result:= fDeltaQueue.CreateNew;
  Result.Id_DD:= 0;
  Result.Date.AsDate:= now;
end;

procedure TDDCollection.BackupDb; { ok }
var
  BackupFilename: string;
  Buffer: array[0..4095] of byte; { 4 Kb buffer }
  InStream,OutStream: TFileStream;
  Cnt,I,Res: Int64;
begin
  BackupFilename:= DDSettings.BackupPath;
  FillChar(Buffer,4096,0); { 11.05.2015 bc }
  if BackupFilename <> 'Not defined' then begin
    { now construct the actual backupname with a date and .bak extension }
    BackupFilename:= ExtractFilePath(BlobSettings.BackupPath)+ExtractFileName(BlobSettings.Databasename); ;
    system.insert('_',BackupFilename,length(BackupFilename)-3); // +_
    system.insert(bcDateToStr(now),BackupFilename,length(BackupFilename)-3); // +19.04.2015
    BackupFilename:= ChangeFileExt(BackupFilename,'.bak');  // *.bak
    if FileExists(BackupFilename) then DeleteFile(BackupFilename); { 09.05.2015 bc }
    if fDb.Connected then fDb.DisConnect; { sanity check }
    InStream:= TFileStream.Create(BlobSettings.Databasename,fmOpenRead);
    try
      InStream.Seek(0,fsFromBeginning);
      OutStream:= TFileStream.Create(BackupFilename,fmCreate);
      try
        OutStream.Seek(0,fsFromBeginning);
        Cnt:= InStream.Size; Res:= 0;
        { implemented by hand }
        while Cnt > 0 do begin
          if Cnt > 4096 then I:= 4096 else I:= Cnt;
          InStream.ReadBuffer(Buffer,I);
          OutStream.WriteBuffer(Buffer,I);
          dec(Cnt,I);
          inc(Res,I);
        end;
        if Res <> InStream.Size then raise Exception.Create('Backup failed! Db-file and backup-file differs in size!');
        { implemented in TStream, uses much bigger buffer }
//        OutStream.CopyFrom(InStream,InStream.Size);
      finally
        FreeAndNil(OutStream);
      end;
    finally
      FreeAndNil(InStream);
    end;
  end;
end;

procedure TDDCollection.AppendToDelta(anItem: TDDCollectionItem); { ok }
begin
  if assigned(anItem) then begin
    fDeltaQueue.Enqueue(anItem); // add to delta for db persistence
    { in case of delete, force updatenow 29.07.2015 bc }
    if anItem.Modified = mDelete then UpdateDb(true)
    else UpdateDb(not fBatch); // updates now or every n-th record
  end;
end;

function TDDCollection.IndexOf(anItem: TDDCollectionItem): ptrint; { ok }
var
  Idx: longint;
  Tmp: TDDCollectionItem;
begin
  Result:= -1; { not found }
  for Idx:= 0 to Count-1 do begin { linear search (O(n)) }
    Tmp:= TDDCollectionItem(Items[Idx]);
    if ((Tmp.Date.AsInteger = anItem.Date.AsInteger) and (Tmp.WeekNumber = anItem.WeekNumber) and
       (Tmp.Text = anItem.Text) and (Tmp.Reserved = anItem.Reserved)) then begin
      Result:= Idx;
      Break;
    end;
  end;
end;

function TDDCollection.UpdateDb(const UpdateNow: boolean): boolean; { ok }// db writes
begin { original code moved to "cutaway.txt" }
  Result:= false;
  if not fDb.Connected then fDb.Connect;
  if not UpdateNow then begin { cater for batch updates }
    if fDeltaQueue.Count >= fUpdateCount then DoUpdate;
  end else DoUpdate;
  if fDb.Connected then fDb.DisConnect;
  Result:= true;
end; { refactored 29.07.2015 bc -> actual writes moved to "DoUpdate" }

function TDDCollection.ReadDb: boolean; // db reads
var
  Ds: TMemDataset;
  Bci: TDDCollectionItem;
begin
  Result:= false;
  Clear;
  if not fDb.Connected then fDb.Connect;
  BeginUpdate;
  Ds:= TMemDataset.Create(nil);
  try
    fDb.QuerySQL(daily_diary_const.SelSql,Ds); // fills the dataset with fielddefs and data
    Ds.First;
    while not Ds.EOF do begin
      Bci:= TDDCollectionItem(Add);
      Bci.Id_DD:= Ds.FieldByName('id_dd').AsInteger;
      Bci.Date.AsInteger:= Ds.FieldByName('date_dd').AsInteger;
      Bci.WeekNumber:= Ds.FieldByName('weeknumber_dd').AsInteger;
      Bci.Text:= Ds.FieldByName('text_dd'); // should not work!
      Bci.Reserved:= Ds.FieldByName('reserved_dd').AsString;
      Ds.Next;
    end;
    Result:= true;
  finally Ds.Free; end;
  { now sort the collection, according to user preference }
  case fSortOrder of
    0: Sort(@BlobCompareDocName); // sort by docname
    1: Sort(@BlobCompareDate); // sort by date
    6: Sort(@BlobCompareDocType); // sort by doctype
  end;
  EndUpdate;
  if fDb.Connected then fDb.DisConnect;
  FPONotifyObservers(Self,ooCustom,pointer(Self.Count)); { fpc built-in observer pattern }
end;

constructor TBlobCollectionItem.Create(aCollection: TBlobCollection);
begin
  inherited Create(aCollection);
  fDate:= TIsoDate.Create(now);
  fBlob:= TMemoryStream.Create;
end;

destructor TBlobCollectionItem.Destroy;
begin
  FreeAndNil(fDate);
  FreeAndNil(fBlob);
  inherited Destroy;
end;

initialization
  Singleton:= nil;
finalization
  FreeAndNil(Singleton);
end.


