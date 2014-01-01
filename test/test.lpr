program test;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, AwsS3Test;

{$R *.res}

begin
  Application.Title := 'AWSTest';
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

