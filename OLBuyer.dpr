program OLBuyer;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  OLB_HTTP in 'OLB_HTTP.pas',
  Unit2 in 'Unit2.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
