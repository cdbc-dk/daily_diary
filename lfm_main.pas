unit lfm_main;

{$mode objfpc}{$H+}
{$define debug}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, ExtDlgs, Buttons,
  daily_diary_const,
  treeview_helper,
  bom_dd,
  bc_datetime,
  bc_observer;

type
  TObjectClass = class of TObject;
  {*** TDD_Observer ***}
  TDD_Observer = class(TObserver)
  protected
    fOwner: TObject;
  public
    constructor Create(const anOwner: TObject);
    destructor Destroy; override;
    Procedure FPOObservedChanged(ASender: TObject;Operation: TFPObservedOperation;Data: Pointer); override;
    property Owner: TObject read fOwner write fOwner;
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
    procedure AddYearNodes(const aCollection: TDDCollection);
    procedure AddWeekNodes(aCollection: TDDCollection);
    function AddEntryToTreeView(const anEntry: TDDCollectionItem): integer;
    function AddChildNodes(const aDate: string): integer;  { flexible result, better than boolean }
    procedure DeleteDateNode(RunAutonomous: boolean = false);
    function ClearTreeview: integer;
    { returns true if found, else false }
    function SearchTreeViewBool(const aNode: TTreeNode;const aSearchString: string): boolean;
    { returns a node if found, else nil }
    function SearchTreeViewItem(const aNode: TTreeNode;const aSearchString: string): TTreeNode; overload;
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
  ddform: TfrmMain;
begin
  ddform:= TfrmMain(fOwner);
  ddCollection:= TDDCollection(aSender);
//  inherited FPOObservedChanged(ASender, Operation, Data);
  case Operation of
    ooAddItem:    begin

                    ddEntry:= TDDCollectionItem(Data);
                    ddform.AddYearNodes(ddCollection);
  //                 AddEntryToTreeView();
                    ddform.memText.Lines.Add('< AddItem received... >');
                    ddform.memText.Lines.Add(ddEntry.Date.AsString+' | '+
                                             ddEntry.DateStr+' | '+
                                             ddEntry.WeekNumber.ToString+' | '+
                                             'Text'+' | '+
                                             ddEntry.Reserved+' | '+
                                             bcTimeToStr(now));
                  end;
    ooChange:     begin
                    // TODO
                  end;
    ooDeleteItem: begin
                    // TODO
                  end;
    ooCustom:     begin                             { dataset read from db }

                    if ddCollection.Count > 0 then
                    //  ddform.AddWeekNodes(ddCollection);
                    ddform.AddYearNodes(ddCollection);

                  end;
  end;
end;

{ TfrmMain }
{ implements the observed, so we have to roll our own observer }
procedure TfrmMain.FormShow(Sender: TObject); { ok }
begin
  edtDate.Text:= bcDateToStr(Now);
          { if there are no nodes, create a root node with a parent of nil }
  Caption:= MainTitle;                            { from daily_diary_const }
  AddRootNode;       { get the treeview going, DO NOT MESS WITH ROOT-NODE! }
end;

procedure TfrmMain.AddRootNode; { ok }
begin
  if trvDates.Items.Count = 0 then begin       { create a parent root node }
    fRootNode:= trvDates.Items.AddFirst(nil,'Dates:');
    fRootNode.Selected:= true;  { make sure that our root-node is selected }
    fRootNode.Data:= nil;
  end;
  {$ifdef debug}
    memText.Lines.Add('AddRootNode -> OK');
  {$endif}
end;

procedure TfrmMain.AddYearNodes(const aCollection: TDDCollection); { OK }
var
  YearExists: boolean;
  Idx: ptruint;
  ddItem: TDDCollectionItem;
  ChildNode: TTreeNode;
begin
  if trvDates.Selected = nil then begin
    trvDates.select(fRootNode);       { make sure the rootnode is selected }
  end;
  ClearTreeview;                { clears the treeview except the root node }
  for Idx:= 0 to aCollection.Count-1 do begin
    { add childnodes under the parent node (frootnode) if not existing }
    ddItem:= TDDCollectionItem(aCollection.Items[Idx]);
    { this works! }
    YearExists:= treeview_helper.SearchTreeViewLevelBool(trvDates,
                                                         ddItem.Date.YearAsString);
    if not YearExists then begin { add year-node under the parent node (frootnode) }
      ChildNode:= trvDates.Items.AddChild(fRootNode,ddItem.Date.YearAsString); // try with fixed root
      ChildNode.Data:= Pointer(ddItem.Id_DD); { save the ID, may come in handy }
      ChildNode.ImageIndex:= 3;    { normal image }
      ChildNode.SelectedIndex:= 2; { selected image }
    end;
  end;
  fRootNode.Expanded:= true;
  {$ifdef debug}
    memText.Lines.Add('AddYearNodes -> OK');
  {$endif}
end; // TODO: differentiate search criteria...

procedure TfrmMain.AddWeekNodes(aCollection: TDDCollection);
var
  Idx: ptruint;
  ddItem: TDDCollectionItem;
  ChildNode: TTreeNode;
  ParentNode: TTreeNode;
begin
  try
    if trvDates.Selected = nil then begin
      trvDates.select(fRootNode);     { make sure the rootnode is selected }
    end;
//    ClearTreeview;              { clears the treeview except the root node }
    for Idx:= 0 to aCollection.Count-1 do begin
      { add childnodes under the parent node (yearnode) }

      ddItem:= TDDCollectionItem(aCollection.Items[Idx]);
      ChildNode:= trvDates.Items.AddChild(fRootNode, ddItem.WeekNumber.ToString); // try with fixed root
      ChildNode.Data:= Pointer(ddItem.Id_DD);
//    trvDates.Items.AddChild(trvDates.Selected ,aDate);
    end;
  except on E:Exception do
    ShowMessage('ERROR: '+E.Message);
  end;
  if trvDates.Items.Count > 0 then fRootNode.Expand(false);
  {$ifdef debug}
    memText.Lines.Add('AddWeekNodes -> OK');
  {$endif}
end;

function TfrmMain.AddEntryToTreeView(const anEntry: TDDCollectionItem): integer;
begin
  Result:= 0;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  DbRead;        { reads all data in database into our collection one time }
  { observer will take care of the rest }
  {$ifdef debug}
    memText.Lines.Add('DbRead -> OK');
  {$endif}
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  fBom:= CreateBom;                     { create our business object model }
  fDate:= TIsoDate.Create(now);
  fObserver:= TDD_Observer.Create(Self);
  fBom.Observed.FPOAttachObserver(fObserver);
end;

procedure TfrmMain.btnCalenderClick(Sender: TObject);
begin
  dlgCalender.Date:= Date;
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
var
  Res: integer;
  lNode: TTreeNode;
begin
  Res:= 0;
  Res:= integer(SearchTreeViewBool(fRootNode,'15'));
  ShowMessageFmt('Node 15 returns %d',[Res]);
  lNode:= SearchTreeViewItem(fRootNode,'17');
  if lNode <> nil then
    ShowMessage('Result from SearchTreeViewItem: [ '+lNode.Text+' ]');
//  ClearTreeview;
//  fDate.Year.ToString;
//  Self.ActiveControl:= memText;
end;

procedure TfrmMain.btnAddClick(Sender: TObject); { ok }
var
  ddItem: TDDCollectionItem;
begin
  btnCalenderClick(Sender);    { set the calendar to today for convenience }
  if memText.Lines.Count = 0 then begin
    Self.ActiveControl:= memText;
    exit;       { nothing to add to database }
  end;
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

procedure TfrmMain.DeleteDateNode(RunAutonomous: boolean = false);
  { a subprocedure to recursively delete nodes }
  procedure DeleteNode(aNode: TTreeNode);
  begin
    while aNode.HasChildren do DeleteNode(aNode.GetLastChild);
    if aNode <> fRootNode then
      trvDates.Items.Delete(aNode);
  end;
begin
  if trvDates.Selected = nil then exit;
  { if selected node has child nodes, first ask for confirmation }
  if trvDates.Selected.HasChildren then begin
    if not RunAutonomous then begin
      if messagedlg('Delete date and all children ?', mtConfirmation, [mbNo,mbYes], 0 ) <> mrYes then
        exit;
    end;
  end;
  { recurse deleting items }
  DeleteNode(trvDates.Selected);
end;

function TfrmMain.ClearTreeview: integer;
begin
  trvDates.Select(fRootNode);        { make sure the root-node is selected }
  DeleteDateNode;                { recursively remove nodes excluding root }
end;

function TfrmMain.SearchTreeViewBool(const aNode: TTreeNode;
                                     const aSearchString: string): boolean;
var
  lNode: TTreeNode;
(*
{ a subprocedure to recursively delete nodes }
procedure DeleteNode(aNode: TTreeNode);
begin
  while aNode.HasChildren do DeleteNode(aNode.GetLastChild);
  if aNode <> fRootNode then
    trvDates.Items.Delete(aNode);
end;
*)
begin
  Result:= false;
  if trvDates.Items.Count <= 1 then begin
    exit;
(*
    if messagedlg('ERROR: Cannot search an empty dataset!',
                  mtError,
                  [mbClose],
                  0 ) = mrClose then exit;
*)
  end;
  if aNode.HasChildren then lNode:= aNode.GetLastChild;
  while aNode.HasChildren do begin
    if lNode = nil then begin
      Result:= false;
      break; // ææ let's see
    end;
    if lNode.Text = aSearchString then begin
//ShowMessage('Found Item: '+lNode.Text);
      Result:= true;
      break;
    end;
    lNode:= lNode.GetPrevSibling;
  end;
end;

function TfrmMain.SearchTreeViewItem(const aNode: TTreeNode;
                                     const aSearchString: string): TTreeNode;
var
  lNode: TTreeNode;
begin
  Result:= nil;
  if trvDates.Items.Count <= 1 then begin
    if messagedlg('ERROR: Cannot search an empty dataset!',
                  mtError,
                  [mbClose],
                  0 ) = mrClose then exit;
  end;
  if aNode.HasChildren then lNode:= aNode.GetLastChild;
  while aNode.HasChildren do begin
    if lNode.Text = aSearchString then begin
ShowMessage('Found Item: '+lNode.Text);
      Result:= lNode;
      break;
    end;
    lNode:= lNode.GetPrevSibling;
  end;
end;

function TfrmMain.DbRead: boolean;
begin
  Result:= false;
  Result:= fBom.ReadBlobDb(false);           { read ascending / descending }
end;

function TfrmMain.test_tv: integer;
var I: integer;
begin
  for I:= 14 downto 0 do
  AddChildNodes(DateToStr(Date)+' '+I.ToString);
end;

end.

