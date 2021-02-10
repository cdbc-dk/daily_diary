unit lfm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, ExtDlgs, Buttons,
  daily_diary_const,
  bom_dd,
  bc_datetime,
  bc_observer;

(*


*)



type
  TObjectClass = class of TObject;
  {*** TDD_Observer ***}

  { TDD_Observer }

  TDD_Observer = class(TObserver)
  protected
    fOwner: TObject;
  public
    constructor Create(const anOwner: TObject);
    destructor Destroy; override;
    Procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer); override;
  end;

  { TfrmMain }
  TfrmMain = class(TForm)
    Button1: TButton;
    btnCalender: TButton;
    dlgCalender: TCalendarDialog;
    gbxControls: TGroupBox;
    gbxDates: TGroupBox;
    gbxText: TGroupBox;
    edtDate: TLabeledEdit;
    imglAll: TImageList;
    lblActions: TLabel;
    memText: TMemo;
    btnAdd: TSpeedButton;
    btnDelete: TSpeedButton;
    btnSave: TSpeedButton;
    btnEdit: TSpeedButton;
    Splitter1: TSplitter;
    stbInfo: TStatusBar;
    trvDates: TTreeView;
    procedure btnAddClick(Sender: TObject);
    procedure btnCalenderClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    fBom: TDDCollection;
    fRootNode: TTreeNode;
    fObserver: TDD_Observer;
    fDate: TIsoDate;
    procedure AddRootNode;
    procedure AddWeekNodes(aCollection: TDDCollection);
    function AddEntryToTreeView(const anEntry: TDDCollectionItem): integer;
    function AddChildNodes(const aDate: string): integer;  { flexible result, better than boolean }
    procedure DeleteDateNode;
  public
    function DbRead: boolean;
    function test_tv: integer;
    property Observer: TDD_Observer read fObserver;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TDD_Observer }

constructor TDD_Observer.Create(const anOwner: TObject);
begin
  inherited Create(anOwner);
  fOwner:= anOwner;
end;

destructor TDD_Observer.Destroy;
begin
  fOwner:= nil;
  inherited Destroy;
end;

procedure TDD_Observer.FPOObservedChanged(ASender: TObject;
                                          Operation: TFPObservedOperation;
                                          Data: Pointer);
var
  ddEntry: TDDCollectionItem;
  ddCollection: TDDCollection;
  mText: TMemo;
begin
  mText:= TMemo(Self.fOwner);
//  inherited FPOObservedChanged(ASender, Operation, Data);
  case Operation of
    ooAddItem:    begin
                    ddEntry:= TDDCollectionItem(Data);
  //                 AddEntryToTreeView();
                    mText.Lines.Add('< AddItem received... >');
                    mText.Lines.Add(ddEntry.Date.AsString+' | '+
                                    ddEntry.DateStr+' | '+
                                    ddEntry.WeekNumber.ToString+' | '+
                                    'Text'+' | '+
                                    ddEntry.Reserved);
                  end;
    ooChange:     begin
                    // TODO
                  end;
    ooDeleteItem: begin
                    // TODO
                  end;
    ooCustom:     begin                             { dataset read from db }
                    ddCollection:= TDDCollection(Sender);
                    if ddCollection.Count > 0 then
                      frmMain.AddWeekNodes(ddCollection);
                  end;
  end;
end;

{ TfrmMain }
{ implements the observed, so we have to roll our own observer }
procedure TfrmMain.FormShow(Sender: TObject); { ok }
begin
//  trvDates.Items.Add(nil,'Root');
  { if there are no nodes, create a root node with a parent of nil }
  Caption:= MainTitle;                            { from daily_diary_const }
  AddRootNode;                                    { get the treeview going }
end;

procedure TfrmMain.AddRootNode; { ok }
begin
  if trvDates.Items.Count = 0 then begin       { create a parent root node }
    fRootNode:= trvDates.Items.AddFirst(nil,'Dates:');
    fRootNode.Data:= nil;
  end;
end;

procedure TfrmMain.AddWeekNodes(aCollection: TDDCollection);
var
  Idx: ptruint;
  ddItem: TDDCollectionItem;
begin
  try
    if trvDates.Selected = nil then begin
      trvDates.select(fRootNode);     { make sure the rootnode is selected }
    end;
    for Idx:= 0 to aCollection.Count-1 do begin
      { add childnodes under the parent node (frootnode) }
      ddItem:= TDDCollectionItem(fBom.Items[Idx]);
      trvDates.Items.AddChild(fRootNode, ddItem.WeekNumber.ToString); // try with fixed root
//    trvDates.Items.AddChild(trvDates.Selected ,aDate);
    end;
  except on E:Exception do
    ShowMessage('ERROR: '+E.Message);
  end;
  if trvDates.Items.Count > 0 then fRootNode.Expand(false);
end;

function TfrmMain.AddEntryToTreeView(const anEntry: TDDCollectionItem): integer;
begin
  Result:= 0;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  if DbRead then AddWeekNodes(fBom);
//  test_tv;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  fBom:= CreateBom;                     { create our business object model }
  fDate:= TIsoDate.Create(now);
  fObserver:= TDD_Observer.Create(memText);
  fBom.Observed.FPOAttachObserver(fObserver);
end;

procedure TfrmMain.btnCalenderClick(Sender: TObject);
begin
  dlgCalender.Date:= now;
  if dlgCalender.Execute then begin
    fDate.AsDate:= dlgCalender.Date;
    edtDate.Text:= fDate.AsString;
    memText.Lines.Add(fDate.AsString);
    memText.Lines.Add(DateToStr(fDate.AsDate));
    memText.Lines.Add(fDate.AsInteger.ToString);
    memText.Lines.Add('Weeknumber: '+fDate.ISOWeekNumber.ToString);
  end;
end;

procedure TfrmMain.btnDeleteClick(Sender: TObject);
var
  anItem: TDDCollectionItem;
begin
  anItem:= fBom.GetItemFromID(5);
  if anItem = nil then exit;               { nothing to delete ~ count = 0 }
  if MessageDlg('Do you really want to delete:',
                'Item: '+anItem.Date.AsString,
                mtConfirmation,
                [mbYes, mbNo],
                0) = mrOK then begin                       { are you sure? }
    anItem.Modified:= mDelete;
    fbom.AppendToDelta(anItem);
  end;
end;

procedure TfrmMain.btnEditClick(Sender: TObject);
begin
  Self.ActiveControl:= memText;
end;

procedure TfrmMain.btnAddClick(Sender: TObject); { ok }
var
  ddItem: TDDCollectionItem;
begin
  if memText.Lines.Count = 0 then exit;       { nothing to add to database }
  ddItem:= fbom.AddNew;
  ddItem.Date.AsDate:= dlgCalender.Date;
  ddItem.DateStr:= ddItem.Date.AsString;
  ddItem.WeekNumber:= ddItem.Date.ISOWeekNumber;
  ddItem.Text.Position:= 0;
  memText.Lines.SaveToStream(ddItem.Text);
  ddItem.Reserved:= 'Test driving... 04';
  ddItem.Modified:= mAdded;                              { add to database }
  fBom.AppendToDelta(ddItem);         { ddItem gets freed in fBom.DoUpdate }
end;

procedure TfrmMain.FormDestroy(Sender: TObject); { ok }
begin
  fBom.Observed.FPODetachObserver(fObserver);       { stop monitoring fbom }
  FreeAndNil(fObserver);                         { get rid of our observer }
  fBom:= nil;                           { just unlink, will be freed later }
  FreeAndNil(fDate);                        { get rid of our fdate utility }
end;

function TfrmMain.AddChildNodes(const aDate: string): integer;
begin
  Result:= HR_ERROR;                           { initialize to error state }
  try
    if trvDates.Selected = nil then begin
      trvDates.select(fRootNode);     { make sure the rootnode is selected }
    end;
    { add a childnode under the parent node (frootnode) }
    trvDates.Items.AddChild(fRootNode ,aDate); // try with fixed root
//    trvDates.Items.AddChild(trvDates.Selected ,aDate);
  except on E:Exception do
    ShowMessage('ERROR: '+E.Message);
  end;
  if trvDates.Items.Count > 0 then fRootNode.Expand(false);
//  if trvDates.Items.Count > 0 then trvDates.Selected.Expand(false);
  Result:= HR_OK;                                       { signal no errors }
end;

procedure TfrmMain.DeleteDateNode;
  { a subprocedure to recursively delete nodes }
  procedure DeleteNode(aNode: TTreeNode);
  begin
    while aNode.HasChildren do DeleteNode(aNode.GetLastChild);
    trvDates.Items.Delete(aNode) ;
  end;
begin
  if trvDates.Selected = nil then exit;
  { if selected node has child nodes, first ask for confirmation }
  if trvDates.Selected.HasChildren then
    if messagedlg('Delete date and all children ?', mtConfirmation, [mbNo,mbYes], 0 ) <> mrYes then
      exit;
  DeleteNode(trvDates.Selected);
end;

function TfrmMain.DbRead: boolean;
begin
  Result:= false;
  if fBom.ReadBlobDb(false) then Result:= true;          { read descending }
end;

function TfrmMain.test_tv: integer;
var I: integer;
begin
  for I:= 14 downto 0 do
  AddChildNodes(DateToStr(Date)+' '+I.ToString);
end;

end.

