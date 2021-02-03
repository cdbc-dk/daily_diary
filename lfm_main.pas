unit lfm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, ExtDlgs, Buttons,
  daily_diary_const,
  bom_dd,
  bc_datetime;

(*
{*** THKObserver ***}
THKObserver = class(TInterfacedObject,IFPObserver)
private
  fDs: THKCollection;
  fGrid: TStringGrid;
public
  constructor Create(aGrid: TStringGrid);
  procedure ClearGrid;
  procedure CreateGridHeaders;
  procedure PopulateGrid;
  Procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer);
end;
*)



type
  { *** tddobserver *** }

  { TDDObserver }

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
    memText: TMemo;
    btnAdd: TSpeedButton;
    btnDelete: TSpeedButton;
    btnSave: TSpeedButton;
    btnEdit: TSpeedButton;
    Splitter1: TSplitter;
    stbInfo: TStatusBar;
    trvDates: TTreeView;
    procedure btnCalenderClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    fBom: TDDCollection;
    fRootNode: TTreeNode;

    fDate: TIsoDate;
    function AddChildNodes(const aDate: string): integer;  { flexible result, better than boolean }
    procedure DeleteDateNode;
  public
    function test_tv: integer;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ *** TDDObserver *** }
constructor TDDObserver.Create(const aTreeview: TTreeView; const aMemo: TMemo);
begin
  inherited Create;
  fTreeView:= aTreeview
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
begin
  case Operation of
    ooAddItem   : ;
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
  fBom:= CreateBom;                     { create our business object model }
  fDate:= TIsoDate.Create(now);
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  test_tv;
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

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fBom:= nil; { just unlink, will be freed later }
  FreeAndNil(fDate);
end;

function TfrmMain.AddChildNodes(const aDate: string): integer;
var
  I: integer;
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

