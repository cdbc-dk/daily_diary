unit lfm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    gbxControls: TGroupBox;
    gbxDates: TGroupBox;
    gbxText: TGroupBox;
    memText: TMemo;
    Splitter1: TSplitter;
    stbInfo: TStatusBar;
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

end.

