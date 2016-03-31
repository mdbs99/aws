unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, aws_client, aws_ses, math;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    edtAcessKeyId: TEdit;
    edtRegion: TEdit;
    edtFromMail: TEdit;
    edtToEmail: TEdit;
    edtToName: TEdit;
    edtSubject: TEdit;
    edtSecretKey: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    memMessage: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  oMessage: TSESMessage;
  FRegionSES: ISESRegion;
begin
  FRegionSES := TSESRegion.Create(
    TAWSClient.Create(
      TAWSCredentials.Create(edtAcessKeyId.Text, edtSecretKey.Text, True)
    ), edtRegion.Text
  );

  oMessage := TSESMessage.Create;
  oMessage.From := edtFromMail.Text;
  oMessage.TOAddress:= edtToEmail.Text;
  oMessage.TOName:= edtToName.Text;
  oMessage.Subject:= edtSubject.Text;
  oMessage.Message:= memMessage.Text;
  oMessage.Format:=teFormato(ifthen(CheckBox1.Checked, 0, 1));

  FRegionSES.SESObjects.SendEmail(oMessage);

end;

end.

