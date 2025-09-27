unit screen2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls;

type

  { TTitle }

  TTitle = class(TForm)
    Hintergrund: TImage;
    Image2: TImage;
    Label3: TLabel;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  Title: TTitle;

implementation

{$R *.lfm}

{ TTitle }

procedure TTitle.Timer1Timer(Sender: TObject);
begin
  ProgressBar1.Enabled:=true;
  ProgressBar1.Position:=ProgressBar1.Position+1;
  if ProgressBar1.Position=100 then Title.Close;
end;

end.

