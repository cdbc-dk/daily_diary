
uses
  Db, bc_guardian, bc_msgqueue, bc_strings, bc_datetime,
  bc_litedb, blob_const;

procedure TForm1.btnDeleteClick(Sender: TObject);
begin
  Application.QueueAsyncCall(@DeleteRecord,fDb.Query.FieldByName('id_photos').AsInteger);
end;

procedure TForm1.btnDownloadClick(Sender: TObject);
var
  BlobStream: TStream;
  M: TFileStream;
begin
  SaveDialog1.FileName:= fDb.Query.FieldByName('filename_photos').AsString;
  if SaveDialog1.Execute then begin
    BlobStream:= fDb.Query.CreateBlobStream(fDb.Query.FieldByName('content_photos'),bmRead);
    M:= TFileStream.Create(SaveDialog1.FileName,fmCreate);
    BlobStream.Position:= 0;
    M.CopyFrom(BlobStream,BlobStream.Size);
    M.Free;
    BlobStream.Free;
  end;
end;

procedure TForm1.btnShowPhotoClick(Sender: TObject);
var
  BlobStream: TStream;
  Filename: string;
begin
  Filename:= fDb.Query.FieldByName('filename_photos').AsString;
  BlobStream:= fDb.Query.CreateBlobStream(fDb.Query.FieldByName('content_photos'),bmRead);
  BlobStream.Seek(0,soFromBeginning); { always remember to reset cursor }
  lfm_showphoto.ShowPhoto(Filename,BlobStream);
  BlobStream.Free;
end;

procedure TForm1.btnTestSingletonClick(Sender: TObject);
var
  BlobStream: TStream;
  Filename: string;
begin
  LiteDb.Connect(DatabaseName);
//  LiteDb.DbName:= DatabaseName;
  LiteDb.Query.SQL.Text:= SelPhotosSql;
  LiteDb.Query.Open;
  LiteDb.Query.Last;
  Filename:= LiteDb.Query.FieldByName('filename_photos').AsString;
  BlobStream:= LiteDb.Query.CreateBlobStream(LiteDb.Query.FieldByName('content_photos'),bmRead);
  BlobStream.Position:= 0;
  ShowPhoto(Filename,BlobStream);
  BlobStream.Free;
  LiteDb.DisConnect;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if assigned(fDb) then begin
    fDb.DisConnect;
    FreeAndNil(fDb);
  end;
end;

{ check for database existence, if none, create one }
function TForm1.CreateDbIfNotExisting: boolean;
var Db: TLiteDb;
begin
  Result:= true;
  Db:= TLiteDb.Create;
  try
    Db.DbName:= DatabaseName;
    { creates a new db if not existing }
    try Db.RunSQL(CreateBlobDb); except Result:= false; end;
  finally FreeAndNil(Db); end;

{ setup our database, temporarily disabling connected components }
procedure TForm1.PrepareTransaction;
begin
  DataSource1.DataSet:= nil;                          { remove link to dataset }
  fDb.DisConnect;                 { takes care of transaction and query aswell }
  fDb.Connect;                                                 { connect again }
  fDb.Transaction.StartTransaction;                  { get a transaction going }
end;

{ commit our transaction and reconnect components }
procedure TForm1.CommitTransaction;
begin
  fDb.Transaction.Commit;                                     { persist change }
  fDb.Transaction.StartTransaction;                  { setup a new transaction }
  fDb.Query.SQL.Text:= blob_const.SelPhotosSql;              { select all data }
  fDb.Query.Open;                                  { now we can see data again }
  DataSource1.DataSet:= fDb.Query;                              { reconnect ui }
end;

{ gets called via "Application.QueueAsyncCall" }
procedure TForm1.InsertRecord(Data: ptrint);
var LastId: ptruint;
begin
  fLastCursorPos:= fDb.Query.RecNo; { 1-based ?!? }
  if OpenDialog1.Execute then begin
    PrepareTransaction;         { setup our connection to receive a new record }
    fDb.Query.SQL.Text:= blob_const.InsPhotoSql;           { assign insert sql }
    fDb.Query.Prepare;   { needed for us to use parameters in the insert query }
    fDb.Query.ParamByName('pfilename').AsString:= ExtractFileName(OpenDialog1.FileName);
    fDb.Query.ParamByName('pcontent').LoadFromFile(OpenDialog1.FileName, ftBlob);
    fDb.Query.ExecSQL;                { now go ahead and insert our new record }
    {$ifdef debug}
      fDb.Query.Close;
      fDb.Query.SQL.Text:= LastIdSql;
      fDb.Query.Open;
      LastId:= fDb.Query.FieldByName('Id_Last').AsInteger;
      fDb.Query.Close;
      ShowMessageFmt('Insert successful via "SELECT LAST_INSERT_ROWID() AS Id_Last;", Last Id: %d',[LastId]);
    {$endif}
    CommitTransaction;{ turn back to reading mode, after committing the record }
  end; { OpenDialog1.Execute }
  fDb.Query.RecNo:= fLastCursorPos + 1;
//  showmessage(inttostr(fDb.Query.RecordCount));
end;

{ gets called via "Application.QueueAsyncCall" }
procedure TForm1.DeleteRecord(Data: ptrint);
begin
  fLastCursorPos:= fDb.Query.RecNo; { remember our position until we come back }
  DataSource1.DataSet:= nil;                          { remove link to dataset }
  fDb.DisConnect;                 { takes care of transaction and query aswell }
  fDb.Connect;                                                 { connect again }
  fDb.Transaction.StartTransaction;                  { get a transaction going }
  fDb.Query.SQL.Text:= blob_const.DelPhotoSql;           { assign deletion sql }
  fDb.Query.Prepare;          { prepare the statement, to use parameter ":pid" }
  fDb.Query.ParamByName('pid').AsInteger:= Data;                    { nice one }
  fDb.Query.ExecSQL;                                      { delete the bastard }
  fDb.Transaction.Commit;                                     { persist change }
  fDb.Transaction.StartTransaction;                  { setup a new transaction }
  fDb.Query.SQL.Text:= blob_const.SelPhotosSql;              { select all data }
  fDb.Query.Open;                                  { now we can see data again }
  DataSource1.DataSet:= fDb.Query;                              { reconnect ui }
  if fLastCursorPos > 1 then fDb.Query.RecNo:= fLastCursorPos - 1;    { return }
end;







