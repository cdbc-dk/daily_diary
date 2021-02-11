
{------------------------------------------------------------------------------|
| Project name: Debug Server                                                   |
| Unit name   : lfm_main.pas                                                   |
| Copyright   : (c) 2021 cdbc.dk                                               |
| Programmer  : Benny Christensen /bc                                          |
| Created     : 2021.01.13 /bc initial design and coding                       |
| Updated     : 2020.01.13 /bc Setting up environment, structure and vision    |
|                                                                              |
|                                                                              |
|                                                                              |
|                                                                              |
|------------------------------------------------------------------------------|
| Abstract:                                                                    |
|   A debug server to connect to while running a live application.             |
|                                                                              |
|                                                                              |
|                                                                              |
|                                                                              |
-------------------------------------------------------------------------------}

unit app_createform;
{$mode objfpc}{$H+}
{.$define debug}
interface

uses
  Classes, SysUtils;

//function Example: TObject; { global singleton }   

implementation

(*
{------------------------------------------------------------------------------
  TApplication CreateForm

  Note: The name is confusing and only kept for Delphi compatibility. It can
  create any kind of components.

  Create a Component instance and sets the pointer to the component variable
  and loads the component. If it is a form it will be added to the applications
  forms list
------------------------------------------------------------------------------}
procedure TApplication.CreateForm(InstanceClass: TComponentClass;
  out Reference);
var
  Instance: TComponent;
  ok: boolean;
  AForm: TForm;
begin
  // Allocate the instance, without calling the constructor
  Instance := TComponent(InstanceClass.NewInstance);
  // set the Reference before the constructor is called, so that
  // events and constructors can refer to it
  TComponent(Reference) := Instance;

  ok:=false;
  try
    if (FCreatingForm=nil) and (Instance is TForm) then
      FCreatingForm:=TForm(Instance);
    Instance.Create(Self);
    ok:=true;
  finally
    if not ok then begin
      TComponent(Reference) := nil;
      if FCreatingForm=Instance then
        FCreatingForm:=nil;
    end;
  end;

  if (Instance is TForm) then
  begin
    AForm := TForm(Instance);
    UpdateMainForm(AForm);
    if FMainForm = AForm then
      AForm.HandleNeeded;
    if AForm.FormStyle = fsSplash then
    begin
      // show the splash form and handle the paint message
      AForm.Show;
      AForm.Invalidate;
      ProcessMessages;
    end;
  end;
  {$IFDEF AfterConstructionDataModuleNotWorking}
  if (Instance is TDataModule) then
  begin
    TDataModule(instance).AfterConstruction;
  end;
  {$ENDIF}
end; 

*)

(*
var 
  __Example: TObject;

function Example: TObject; { singleton }
begin
  if not assigned(__Example) then __Example:= TObject.Create;
  Result:= __Example;
end; { gets released on progam end }
*)

initialization
//  __Example:= nil;

finalization 
//  FreeAndNil(__Example);
  
end.

