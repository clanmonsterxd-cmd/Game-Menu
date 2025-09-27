unit screen1;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls;

type

  { TPresents }

  TPresents = class(TForm)
    Hintergrund: TImage;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  Presents: TPresents;

implementation

{$R *.lfm}

{ TPresents }

procedure TPresents.Timer1Timer(Sender: TObject);
begin
  ProgressBar1.Enabled:=true;
  ProgressBar1.Position:=ProgressBar1.Position+1;
  if ProgressBar1.Position=100 then Presents.Close;
end;

end.

