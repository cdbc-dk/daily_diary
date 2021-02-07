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
  TDD_Observer = class(TObserver)
  protected
    fOwner: TObject;
  public
    constructor Create(const anOwner: TObject;const aClass: TObjectClass);
    destructor Destroy; override;
    Procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer); override;
  end;
  { TDDObserver }
  {$interfaces corba}
  TDDObserver = class(TInterfacedObject,IFPObserver)
  protected
    fTreeView: TTreeView;
    fMemo: TMemo;
  public
    constructor Create(const aTreeview: TTreeView;
                       const aMemo: TMemo);
    destructor Destroy; override;
    Procedure FPOObservedChanged(aSender: TObject;Operation: TFPObservedOperation;Data: Pointer);
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
    function AddEntryToTreeView(const anEntry: TDDCollectionItem): integer;
    function AddChildNodes(const aDate: string): integer;  { flexible result, better than boolean }
    procedure DeleteDateNode;
  public
    function test_tv: integer;
    property Observer: TDD_Observer read fObserver;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TDD_Observer }

procedure TDD_Observer.FPOObservedChanged(ASender: TObject;
                                          Operation: TFPObservedOperation;
                                          Data: Pointer);
var
  ddEntry: TDDCollectionItem;
  mText: TMemo;
begin
  ddEntry:= TDDCollectionItem(Data);
  mText:= TMemo(Self.ActiveObject);
//  inherited FPOObservedChanged(ASender, Operation, Data);
  case Operation of
    ooAddItem: begin
                 AddEntryToTreeView();
                 mText.Lines.Add('< AddItem received... >');
                 mText.Lines.Add(ddEntry.Date.AsString+' '+
                                 ddEntry.WeekNumber.ToString+' '+
                                 'Text'+' '+
                                 ddEntry.Reserved);
               end;
  end;
end;

{ *** TDDObserver *** }
constructor TDDObserver.Create(const aTreeview: TTreeView; const aMemo: TMemo);
begin
  inherited Create;
  fTreeView:= aTreeview;
  fMemo:= aMemo;
end;

destructor TDDObserver.Destroy;
begin
  fTreeView:= nil;
  fMemo:= nil;
  inherited Destroy;
end;

procedure TDDObserver.FPOObservedChanged(aSender: TObject;
                                         Operation: TFPObservedOperation;
                                         Data: Pointer);
var
  ddCollection: bom_dd.TDDCollection;
  ddItem: bom_dd.TDDCollectionItem;
  lId_DD: integer;
begin
  ddCollection:= nil;
  ddItem:= nil;
  ddItem:= TDDCollectionItem(Data);
//  lId_DD:= integer(Data);
  ddCollection:= TDDCollection(aSender);

//  ddItem:= fbom.GetItemFromID();
//  ddItem:= bom_dd.TDDCollectionItem(Data);
  case Operation of
    ooAddItem   : begin
                    fMemo.Lines.Add(ddItem.Date.AsString+' '+
                                    ddItem.WeekNumber.ToString+' '+
                                    'Text'+' '+
                                    ddItem.Reserved);
                  end;
    ooChange    : ;
    ooDeleteItem: ;
    ooFree      : ;
    ooCustom    : ;
  end;
end;

{ TfrmMain }
{ implements the observed, so we have to roll our own observer }
procedure TfrmMain.FormShow(Sender: TObject);
begin
//  trvDates.Items.Add(nil,'Root');
  { if there are no nodes, create a root node with a parent of nil }
  Caption:= MainTitle;                            { from daily_diary_const }
  if trvDates.Items.Count = 0 then begin       { create a parent root node }
    fRootNode:= trvDates.Items.AddFirst(nil,'Dates:');
    fRootNode.Data:= nil;
  end;
//  fObserver:= TDDObserver.Create(trvDates,memText);
//  fBom.FPOAttachObserver(fObserver);
end;

function TfrmMain.AddEntryToTreeView(const anEntry: TDDCollectionItem): integer;
begin

end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  test_tv;
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

procedure TfrmMain.btnEditClick(Sender: TObject);
begin
  // TODO
end;

procedure TfrmMain.btnAddClick(Sender: TObject);
var
  ddItem: TDDCollectionItem;
begin
  ddItem:= fbom.AddNew;
  ddItem.Date.AsDate:= dlgCalender.Date;
  ddItem.WeekNumber:= ddItem.Date.ISOWeekNumber;
  ddItem.Text.Position:= 0;
  memText.Lines.SaveToStream(ddItem.Text);
  ddItem.Reserved:= 'Test driving... 03';
  ddItem.Modified:= mAdded; { add to database }
  fBom.AppendToDelta(ddItem);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
//  fBom.FPODetachObserver(fObserver);
  fBom.Observed.FPODetachObserver(fObserver);
  FreeAndNil(fObserver);
  fBom:= nil; { just unlink, will be freed later }
  FreeAndNil(fDate);
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

function TfrmMain.test_tv: integer;
var I: integer;
begin
  for I:= 14 downto 0 do
  AddChildNodes(DateToStr(Date)+' '+I.ToString);
end;

end.

