unit bom_dd;
{$mode objfpc}{$H+}
{$define debug}
interface
uses
  Classes, SysUtils, db,
  bc_utilities,
//  blob_settings,
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
  { *** TBlobCollectionItem *** }

  { TDDCollectionItem }

  TDDCollectionItem = class(TCollectionItem)
  private
    fId_DD: ptruint;       // id from database
    fDate: TIsoDate;       // well duh!
    fWeekNumber: ptruint;  // week number
    fText: TStream;        // binary large object - can be anything
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
    property Reserved: string read fReserved write fReserved;
    property Modified: byte read fModified write fModified;
  end;

  { *** TBlobQueue *** }
  TBlobQueue = class(TbcQueue)
  public
    function CreateNew: TBlobCollectionItem;
    procedure Enqueue(anItem: TBlobCollectionItem);
    function Dequeue: TBlobCollectionItem;
    function Peek: TBlobCollectionItem; // only peeking does not remove from Q
  end;

  { TBlobCollection }

  TBlobCollection = class(TCollection)
  private
    fBatch: boolean;
    fSortOrder: integer;
    function get_DbName: string;
    function get_EngineVersion: string;
    procedure set_DbName(aValue: string);
    procedure DeleteItem(anItem: TBlobCollectionItem); { remove item from collection }
  protected
    fDb: TLiteDb;
    fDeltaQueue: TBlobQueue;
    fUpdateCount: ptrint;
    procedure DoUpdate; { refactored 29.07.2015 bc }
    function AddRecord(anItem: TBlobCollectionItem): ptruint; // result is the new ID for the current record
    procedure UpdateRecord(anItem: TBlobCollectionItem);
    procedure DeleteRecord(anItem: TBlobCollectionItem); { remove from database backend }
  public
    constructor Create(anItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function CheckTable: boolean; { creates a new table in db-file }
    function AddNew: TBlobCollectionItem;
    procedure BackupDb; { 19.04.2015 bc }
    procedure AppendToDelta(anItem: TBlobCollectionItem);
    function IndexOf(anItem: TBlobCollectionItem): ptrint; { 11.05.2015 bc, returns -1 on not found else collection ID }
    function UpdateDb(const UpdateNow: boolean): boolean; { refactored 29.07.2015 bc }
    function ReadDb: boolean;
    property UpdateCount: ptrint read fUpdateCount write fUpdateCount;
    property DbName: string read get_DbName write set_DbName;
    property BatchUpdate: boolean read fBatch write fBatch;
    property SortOrder: integer read fSortOrder write fSortOrder;
    property EngineVersion: string read get_EngineVersion;
  end;

{ Bom - factory }
function CreateBom: TBlobCollection;

implementation
uses bc_memdataset;

function CreateBom: TBlobCollection;
begin
  Result:= TBlobCollection.Create(TBlobCollectionItem);
end;

{ TDDCollectionItem }

procedure TDDCollectionItem.AssignData(aSource: TDDCollectionItem);
begin
  fId_DD:= aSource.Id_DD;           // id from database
  fDate:= aSource.Date;             // well duh!
  fWeekNumber:= aSource.WeekNumber; // week number
  fText:= TStream;                  // binary large object - can be anything
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

function TBlobCollection.get_DbName: string;
begin
  Result:= fDb.DbName;
end;

function TBlobCollection.get_EngineVersion: string;
begin
  Result:= blob_const.UnitVersion;
end;

procedure TBlobCollection.set_DbName(aValue: string);
begin
  fDb.DbName:= aValue;
end;

procedure TBlobCollection.DeleteItem(anItem: TBlobCollectionItem);
var
  Res,ItemId: ptrint;
begin
  Res:= -1;
  ItemId:= anItem.ID;
  Res:= bcSearch(ItemId,Self);
  if Res > -1 then Delete(Res);
end;

function TBlobCollection.CheckTable: boolean; { creates a new db if not existing }
begin
  Result:= true;
  { creates a new db if not existing }
  try fDb.RunSQL(CreateBlobDb); except  end;
end;

procedure TBlobCollection.DoUpdate; { refactored 29.07.2015 bc }
var
  Tmp,New: TBlobCollectionItem;
begin
  while not fDeltaQueue.IsEmpty do begin
    Tmp:= fDeltaQueue.Dequeue;
    case Tmp.Modified of
      mAdded: begin
                New:= TBlobCollectionItem(Add); { gets an ownership from collection }
                AddRecord(Tmp);
                New.AssignData(Tmp);
                //ææ
              end;
      mAltered: fDb.RunSQL('');
      mDelete: begin
                 DeleteRecord(Tmp); { takes care of the database back-end }
                 DeleteItem(Tmp); { removes the item from our collection }
               end;
    end;
    FreeAndNil(Tmp);
  end;
end;

{ addrecord persists anitem to database and returns the new row_id as a result }
function TBlobCollection.AddRecord(anItem: TBlobCollectionItem): ptruint;
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
      anItem.Blob_Id:= Result;
      fDb.Query.Close;
      fDb.Transaction.Commit;
      anItem.Modified:= mNone;
    end;
    fDb.DisConnect; { no dangling connections }
  end;
end;

procedure TBlobCollection.UpdateRecord(anItem: TBlobCollectionItem);
begin

end;

procedure TBlobCollection.DeleteRecord(anItem: TBlobCollectionItem); { ok }
begin
  if fDb.Connect then begin
    if not fDb.Transaction.Active then begin
      fDb.Transaction.StartTransaction;
      fDb.Query.Close;
      fDb.Query.SQL.Text:= DelBlobSql;
      fDb.Query.Prepare;
      fDb.Query.ParamByName('pid').AsInteger:= anItem.Blob_Id;
      fDb.Query.ExecSQL;
      fDb.Transaction.Commit;
    end;
    fDb.DisConnect;
  end;
end;

constructor TBlobCollection.Create(anItemClass: TCollectionItemClass); { ok }
begin
  inherited Create(anItemClass);
  fDb:= TLiteDb.Create;
  fDb.DbName:= BlobSettings.Databasename; { 19.04.2015 bc }
  fDb.Connect;
  CheckTable; // create table if non existing
  if fDb.Connected then fDb.DisConnect; // no idle connections
  fDeltaQueue:= TBlobQueue.Create;
  fBatch:= false;
  fBatch:= BlobSettings.BatchUpdates; { 19.04.2015 bc }
  fUpdateCount:= BlobSettings.BatchCount; { 19.04.2015 bc }
end;

destructor TBlobCollection.Destroy;
begin
  if not fDeltaQueue.IsEmpty then UpdateDb(true);
  fDeltaQueue.Free;
  if fDb.Connected then fDb.DisConnect;
  fDb.Free;
  inherited Destroy;
end;

function TBlobCollection.AddNew: TBlobCollectionItem; { ok }
begin
  Result:= fDeltaQueue.CreateNew;
  Result.Blob_Id:= -1;        //ææ
  Result.Date.AsDate:= now;
end;

procedure TBlobCollection.BackupDb; { ok }
var
  BackupFilename: string;
  Buffer: array[0..4095] of byte; { 4 Kb buffer }
  InStream,OutStream: TFileStream;
  Cnt,I,Res: Int64;
begin
  BackupFilename:= BlobSettings.BackupPath;
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

procedure TBlobCollection.AppendToDelta(anItem: TBlobCollectionItem); { ok }
begin
  if assigned(anItem) then begin
    fDeltaQueue.Enqueue(anItem); // add to delta for db persistence
    { in case of delete, force updatenow 29.07.2015 bc }
    if anItem.Modified = mDelete then UpdateDb(true)
    else UpdateDb(not fBatch); // updates now or every n-th record
  end;
end;

function TBlobCollection.IndexOf(anItem: TBlobCollectionItem): ptrint; { ok }
var
  Idx: longint;
  Tmp: TBlobCollectionItem;
begin
  Result:= -1; { not found }
  for Idx:= 0 to Count-1 do begin { linear search (O(n)) }
    Tmp:= TBlobCollectionItem(Items[Idx]);
    if ((Tmp.DocName = anItem.DocName) and (Tmp.DocType = anItem.DocType) and
       (Tmp.Date.AsInteger = anItem.Date.AsInteger) and (Tmp.Flag = anItem.Flag) and
       (Tmp.Reserved = anItem.Reserved)) then begin
      Result:= Idx;
      Break;
    end;
  end;
end;

function TBlobCollection.UpdateDb(const UpdateNow: boolean): boolean; { ok }// db writes
begin { original code moved to "cutaway.txt" }
  Result:= false;
  if not fDb.Connected then fDb.Connect;
  if not UpdateNow then begin { cater for batch updates }
    if fDeltaQueue.Count >= fUpdateCount then DoUpdate;
  end else DoUpdate;
  if fDb.Connected then fDb.DisConnect;
  Result:= true;
end; { refactored 29.07.2015 bc -> actual writes moved to "DoUpdate" }

function TBlobCollection.ReadDb: boolean; // db reads
var
  Ds: TMemDataset;
  Bci: TBlobCollectionItem;
begin
  Result:= false;
  Clear;
  if not fDb.Connected then fDb.Connect;
  BeginUpdate;
  Ds:= TMemDataset.Create(nil);
  try
    fDb.QuerySQL(SelBlobSql,Ds); // fills the dataset with fielddefs and data
    Ds.First;
    while not Ds.EOF do begin
      Bci:= TBlobCollectionItem(Add);
      Bci.Blob_Id:= Ds.FieldByName('id_blob').AsInteger;
      Bci.DocName:= Ds.FieldByName('docname_blob').AsString;
      Bci.DocType:= Ds.FieldByName('doctype_blob').AsString;
      Bci.Date.AsInteger:= Ds.FieldByName('date_blob').AsInteger;
      // ??? ææ
      case Ds.FieldByName('flag_blob').AsInteger of
        0: Bci.Flag:= false;
        1: Bci.Flag:= true;
      end;
      Bci.Reserved:= Ds.FieldByName('reserved_blob').AsString;
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

{ TBlobCollectionItem }

procedure TBlobCollectionItem.AssignData(aSource: TBlobCollectionItem);
begin
  fBlob_Id:= aSource.Blob_Id;
  fDocName:= aSource.DocName;
  fDocType:= aSource.DocType;
  fDate.AsInteger:= aSource.Date.AsInteger;
  fBlobFilename:= aSource.BlobFilename;
  fBlob.Position:= 0;
  aSource.Blob.Position:= 0;
  fBlob.CopyFrom(aSource.Blob,aSource.Blob.Size);
  fBlob.Position:= 0;
  aSource.Blob.Position:= 0;
  fFlag:= aSource.Flag;
  fReserved:= aSource.Reserved;
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

end.


