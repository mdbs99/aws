unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, aws_client, aws_ses, aws_credentials, math;

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
    Label8: TLabel;
    memMessage: TMemo;
    memRetorno: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  FRegionSES := TSESRegion.New(
    TAWSClient.New(
      TAWSSignatureVersion3.New(TAWSCredentials.New(edtAcessKeyId.Text, edtSecretKey.Text, True))
    ), edtRegion.Text
  );

  oMessage := TSESMessage.Create;
  oMessage.From := edtFromMail.Text;
  oMessage.TOAddress:= edtToEmail.Text;
  oMessage.TOName:= edtToName.Text;
  oMessage.Subject:= edtSubject.Text;
  oMessage.Message:= memMessage.Text;
  oMessage.Format:=teFormato(ifthen(CheckBox1.Checked, 0, 1));

  FRegionSES.SESObjects.SendEmail(oMessage).Stream.SaveToFile('retorno.txt');

  memRetorno.Lines.LoadFromFile('retorno.txt');

  PageControl1.ActivePage := TabSheet2;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage := TabSheet1;
end;

end.

