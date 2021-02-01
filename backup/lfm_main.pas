unit lfm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls,
  daily_diary_const;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    Button1: TButton;
    gbxControls: TGroupBox;
    gbxDates: TGroupBox;
    gbxText: TGroupBox;
    memText: TMemo;
    Splitter1: TSplitter;
    stbInfo: TStatusBar;
    trvDates: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    fRootNode: TTreeNode;
    function AddChildNodes(const aDate: string): integer;  { flexible result, better than boolean }
    procedure DeleteDateNode;
  public
    function test_tv: integer;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormShow(Sender: TObject);
begin
//  trvDates.Items.Add(nil,'Root');
  { if there are no nodes, create a root node with a parent of nil }
  Caption:= MainTitle;                            { from daily_diary_const }
  if trvDates.Items.Count = 0 then begin       { create a parent root node }
    fRootNode:= trvDates.Items.AddFirst(nil,'Dates');
    fRootNode.Data:= nil;
  end;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  test_tv;
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
  for I:= 0 to 14 do
  AddChildNodes(DateToStr(Date)+' '+I.ToString);
end;

end.

