unit menu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, LCLType, ComCtrls, Windows, ShellAPI, bass, DateUtils, unit_captcha,
  dglOpenGL, IniFiles;

type

  { TGameMenu }

  TGameMenu = class(TForm)
    HelpB: TBitBtn;
    Black: TPanel;
    Hel: TPanel;
    Roul: TPanel;
    Puzz: TPanel;
    Mem: TPanel;
    Rat: TPanel;
    Jumb: TPanel;
    Tet: TPanel;
    Klack: TPanel;
    Ping: TPanel;
    Invad: TPanel;
    Brak: TPanel;
    Snak: TPanel;
    Ratehexe: TBitBtn;
    VT: TLabel;
    Memory: TBitBtn;
    Puzzle: TBitBtn;
    Roulette: TBitBtn;
    Breakout: TBitBtn;
    Snake: TBitBtn;
    Pong: TBitBtn;
    Invaders: TBitBtn;
    Black_Jack: TBitBtn;
    Jump: TBitBtn;
    Tetris: TBitBtn;
    Clicker: TBitBtn;
    Hintergrund: TImage;
    ButtonStartKonsolenProgramm: TButton;
    Timer1: TTimer;
    VolumeTheme: TTrackBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure HelpBClick(Sender: TObject);
    procedure MemoryClick(Sender: TObject);
    procedure Black_JackClick(Sender: TObject);
    procedure BreakoutClick(Sender: TObject);
    procedure ClickerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormWindowStateChange(Sender: TObject);
    procedure InvadersClick(Sender: TObject);
    procedure JumpClick(Sender: TObject);
    procedure PongClick(Sender: TObject);
    procedure PuzzleClick(Sender: TObject);
    procedure RatehexeClick(Sender: TObject);
    procedure RouletteClick(Sender: TObject);
    procedure SnakeClick(Sender: TObject);
    procedure TetrisClick(Sender: TObject);
    procedure ExtractResourceToExe(const ResName, OutputFile: string);
    procedure RunEmbeddedExe(const ResName: string);
    procedure RunEmbeddedBatch(const ResName: string);
    procedure RunEmbeddedExeInCustomFolder(const ResName: string);
    procedure PlaySoundFromResourceBASS(const ResName: string);
    function GetEnvStr(const VarName: string): string;
    procedure Timer1Timer(Sender: TObject);
    procedure VolumeThemeChange(Sender: TObject);
  private
    LastFocusTime: TDateTime;
    FocusCheckTimer: TTimer;
    procedure ShowAntiCaptcha;
    procedure OnFocusTimer(Sender: TObject);

  public
    procedure ApplicationDeactivate(Sender: TObject);
    procedure ApplicationActivate(Sender: TObject);
  end;

var
  GameMenu: TGameMenu;
  LoopStream: HSTREAM;
  open: boolean = false;
  ResPtr: Pointer;
  ResSize: DWORD;
  Volume: integer;

implementation
 uses Help;

{$R *.lfm}
    {$R game.res}
    {$R sound.res}

{ TGameMenu }

function LoadResourceToMemory(ResName, ResType: PChar; out Size: DWORD): Pointer;
var
  ResHandle: HRSRC;
  ResDataHandle: HGLOBAL;
begin
  Result := nil;
  Size := 0;

  ResHandle := FindResource(HInstance, ResName, ResType);
  if ResHandle = 0 then Exit;

  Size := SizeofResource(HInstance, ResHandle);
  if Size = 0 then Exit;

  ResDataHandle := LoadResource(HInstance, ResHandle);
  if ResDataHandle = 0 then Exit;

  Result := LockResource(ResDataHandle);
end;

function GetEnvVar(const VarName: string): string;
var
  Buf: array[0..MAX_PATH] of Char;
  Len: DWORD;
begin
  Len := GetEnvironmentVariable(PChar(VarName), Buf, MAX_PATH);
  if Len > 0 then
    Result := Buf
  else
    Result := '';
end;

function GetSafeIniFolder: string;
var
  TempPath: array[0..MAX_PATH] of Char;
  IniFolder: string;
  Attr: DWORD;
begin
  if GetTempPath(MAX_PATH, TempPath) = 0 then
    raise Exception.Create('Temp-Pfad konnte nicht ermittelt werden.');

  IniFolder := IncludeTrailingPathDelimiter(StrPas(TempPath)) + 'MeineGameSaves\';

  if not DirectoryExists(IniFolder) then
  begin
    if not ForceDirectories(IniFolder) then
      raise Exception.Create('Ordner für INI-Dateien konnte nicht erstellt werden.');

    Attr := GetFileAttributes(PChar(IniFolder));
    if Attr <> INVALID_FILE_ATTRIBUTES then
      SetFileAttributes(PChar(IniFolder), Attr or FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM);
  end;

  Result := IncludeTrailingPathDelimiter(IniFolder);
end;

procedure SaveLastRun;
var
  Ini: TIniFile;
  SavePath: string;
begin
  SavePath := GetSafeIniFolder + 'GameSettings.ini';
  Ini := TIniFile.Create(SavePath);
  try
    Ini.WriteDateTime('General', 'LastRun', Now);
  finally
    Ini.Free;
  end;
end;

procedure CheckLastRun;
var
  Ini: TIniFile;
  SavePath: string;
  LastRun: TDateTime;
  Days: Integer;
  Res: HRSRC;
  ResHandle: HGLOBAL;
begin
  SavePath := GetSafeIniFolder + 'GameSettings.ini';
  Ini := TIniFile.Create(SavePath);
  try
    if Ini.ValueExists('General', 'LastRun') then
    begin
      LastRun := Ini.ReadDateTime('General', 'LastRun', Now);
      Days := DaysBetween(Now, LastRun);

      // andere Ressource je nach letzter Nutzung laden
      if Days >= 90 then
        Res := FindResource(HInstance, 'Eternity', Windows.RT_RCDATA)
      else
        Res := FindResource(HInstance, 'THEME', Windows.RT_RCDATA);

      if Res <> 0 then
      begin
        ResHandle := LoadResource(HInstance, Res);
        ResSize := SizeofResource(HInstance, Res);
        ResPtr := LockResource(ResHandle);
      end
      else
        ResPtr := nil;
    end
    else
    begin
      // falls Programm zum ersten Mal gestartet wird → Standardtheme
      Res := FindResource(HInstance, 'THEME', Windows.RT_RCDATA);
      if Res <> 0 then
      begin
        ResHandle := LoadResource(HInstance, Res);
        ResSize := SizeofResource(HInstance, Res);
        ResPtr := LockResource(ResHandle);
      end;
    end;

    // aktuellen Start abspeichern
    Ini.WriteDateTime('General', 'LastRun', Now);

  finally
    Ini.Free;
  end;
end;

procedure TGameMenu.PlaySoundFromResourceBASS(const ResName: string);
var
  Res: HRSRC;
  ResHandle: HGLOBAL;
  ResPtr: Pointer;
  ResSize: DWORD;
  Stream: HSTREAM;
begin
  Res := FindResource(HInstance, PChar(ResName), Windows.RT_RCDATA);
  if Res = 0 then Exit;
  ResHandle := LoadResource(HInstance, Res);
  ResSize := SizeofResource(HInstance, Res);
  ResPtr := LockResource(ResHandle);

  Stream := BASS_StreamCreateFile(True, ResPtr, 0, ResSize, BASS_STREAM_AUTOFREE);
  if Stream = 0 then
  begin
    ShowMessage('Fehler beim Abspielen: ' + IntToStr(BASS_ErrorGetCode));
    Exit;
  end;

  BASS_ChannelSetAttribute(Stream,
      BASS_ATTRIB_VOL, 0.05);

  BASS_ChannelPlay(Stream, False);
end;

procedure PlayThemeLoop;
begin
  if not BASS_Init(-1, 44100, 0, 0, nil) then
  begin
    MessageBox(0, 'BASS Init fehlgeschlagen!', 'Fehler', MB_ICONERROR);
    Exit;
  end;

  CheckLastRun;  // setzt ResPtr und ResSize global

  if ResPtr = nil then
  begin
    MessageBox(0, 'Keine Musik-Ressource gefunden!', 'Fehler', MB_ICONERROR);
    Exit;
  end;

  LoopStream := BASS_StreamCreateFile(True, ResPtr, 0, ResSize, BASS_SAMPLE_LOOP);
  if LoopStream = 0 then
  begin
    MessageBox(0, PChar('Stream-Fehler: ' + IntToStr(BASS_ErrorGetCode)), 'Fehler', MB_ICONERROR);
    Exit;
  end;

  BASS_ChannelSetAttribute(LoopStream, BASS_ATTRIB_VOL, Volume/100);
  BASS_ChannelPlay(LoopStream, False);
end;

procedure PauseThemeLoop;
begin
  if LoopStream <> 0 then
  begin
    BASS_ChannelPause(LoopStream);
  end;
end;

procedure ResumeThemeLoop;
begin
  if LoopStream <> 0 then
  begin
    if BASS_ChannelIsActive(LoopStream) <> BASS_ACTIVE_PLAYING then
    begin
      BASS_ChannelPlay(LoopStream, False); // False: Resume statt Neustart
    end;
  end;
end;

function TGameMenu.GetEnvStr(const VarName: string): string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if GetEnvironmentVariable(PChar(VarName), Buffer, MAX_PATH) > 0 then
    Result := string(Buffer)
  else
    Result := '';
end;

procedure TGameMenu.Timer1Timer(Sender: TObject);
begin
  Form1.Top:= GameMenu.Top ;
  Form1.Left:= GameMenu.Left + GameMenu.Width;
end;

procedure TGameMenu.VolumeThemeChange(Sender: TObject);
begin
  VT.Caption:= inttostr(VolumeTheme.Position);
  Volume:= VolumeTheme.Position;
  BASS_ChannelSetAttribute(LoopStream, BASS_ATTRIB_VOL, Volume/100);
end;

procedure TGameMenu.ExtractResourceToExe(const ResName, OutputFile: string);
var
  ResStream: TResourceStream;
begin
  ResStream := TResourceStream.Create(HInstance, PChar(ResName), RT_RCDATA);
  try
    ResStream.SaveToFile(OutputFile);
  finally
    ResStream.Free;
  end;
end;

procedure TGameMenu.RunEmbeddedBatch(const ResName: string);
var
  TempPath, CmdLine: string;
begin
  TempPath := IncludeTrailingPathDelimiter(GetEnvVar('TEMP')) + ResName + '.bat';

  ExtractResourceToExe(ResName, TempPath);

  // Batch über cmd.exe starten
  CmdLine := '/c "' + TempPath + '"';

  ShellExecute(0, 'open', 'cmd.exe', PChar(CmdLine), nil, SW_SHOW);
end;

procedure TGameMenu.RunEmbeddedExe(const ResName: string);
var
  TempPath: string;
begin
  TempPath := IncludeTrailingPathDelimiter(GetEnvStr('TEMP')) + ResName + '.exe';
  ExtractResourceToExe(ResName, TempPath);
  ShellExecute(0, 'open', PChar(TempPath), nil, nil, SW_SHOWNORMAL);
end;

procedure TGameMenu.RunEmbeddedExeInCustomFolder(const ResName: string);
var
  TempDir, TargetDir, ExePath: string;
begin
  TempDir := GetEnvStr('TEMP');
  TargetDir := IncludeTrailingPathDelimiter(TempDir) + 'MeineGameSaves\';

  if not DirectoryExists(TargetDir) then
    ForceDirectories(TargetDir);

  ExePath := TargetDir + ResName + '.exe';

  ExtractResourceToExe(ResName, ExePath);
  ShellExecute(0, 'open', PChar(ExePath), nil, PChar(TargetDir), SW_SHOWNORMAL);
end;

procedure TGameMenu.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ssCtrl in Shift then
  begin
    case Key of
    //  R
    $52: RunEmbeddedExe('RRoulette');
    //  W
    $57: RunEmbeddedBatch('Batch');
    //  C
    $43: RunEmbeddedExeInCustomFolder('Loesch');
    //  M
    $4D: RunEmbeddedExe('Maze');
    //  S
    $53: RunEmbeddedExe('Survivor');
   end;
  end;
end;

procedure TGameMenu.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then PauseThemeLoop
     else ResumeThemeLoop;
end;

procedure TGameMenu.InvadersClick(Sender: TObject);
begin
  Invad.BevelOuter:= bvLowered;
  PlaySoundFromResourceBASS('CLICK');
  RunEmbeddedExe('INVADERS');
  Invad.BevelOuter:= bvRaised;
end;

procedure TGameMenu.Black_JackClick(Sender: TObject);
begin
  Black.BevelOuter:= bvLowered;
  PlaySoundFromResourceBASS('CLICK');
  RunEmbeddedExe('BLACKJACK');
  Black.BevelOuter:= bvRaised;
end;

procedure TGameMenu.MemoryClick(Sender: TObject);
begin
  Mem.BevelOuter:= bvLowered;
  PlaySoundFromResourceBASS('CLICK');
  RunEmbeddedExe('MEMORY');
  Mem.BevelOuter:= bvRaised;
end;

procedure TGameMenu.HelpBClick(Sender: TObject);
begin
  Hel.BevelOuter:= bvLowered;
  if open = false then Form1.Show
    else Form1.Close;

  GameMenu.SetFocus;

  open:= not open;
  Hel.BevelOuter:= bvRaised;
end;

procedure TGameMenu.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveLastRun;
end;

procedure TGameMenu.BreakoutClick(Sender: TObject);
begin
  Brak.BevelOuter:= bvLowered;
  PlaySoundFromResourceBASS('CLICK');
  RunEmbeddedExe('BREAK');
  Brak.BevelOuter:= bvRaised;
end;

procedure TGameMenu.ClickerClick(Sender: TObject);
begin
  Klack.BevelOuter:= bvLowered;
  PlaySoundFromResourceBASS('CLICK');
  RunEmbeddedExe('DRAGONCLICKER');
  Klack.BevelOuter:= bvRaised;
end;

procedure TGameMenu.FormCreate(Sender: TObject);
begin
  GameMenu.Width:= Hel.Left+Hel.Width;
  GameMenu.Height:= Klack.Top + Klack.Height;

  VolumeTheme.Position:= 60;
  VT.Caption:= inttostr(VolumeTheme.Position);
  Volume:= VolumeTheme.Position;

  PlayThemeLoop;

  Application.OnDeactivate := @ApplicationDeactivate;
  Application.OnActivate := @ApplicationActivate;

  LastFocusTime := Now;
  FocusCheckTimer := TTimer.Create(Self);
  FocusCheckTimer.Interval := 1;
  FocusCheckTimer.OnTimer := @OnFocusTimer;
  FocusCheckTimer.Enabled := True;
end;

procedure TGameMenu.ApplicationDeactivate(Sender: TObject);
begin
  PauseThemeLoop;
  FocusCheckTimer.Enabled:= false;
  LastFocusTime := Now; // Fokus-Verlust → Timer zurücksetzen
end;

procedure TGameMenu.ApplicationActivate(Sender: TObject);
begin
  ResumeThemeLoop;
  FocusCheckTimer.Enabled:= true;
  LastFocusTime := Now; // Fokus-Rückkehr → Timer zurücksetzen
end;

procedure TGameMenu.RatehexeClick(Sender: TObject);
begin
  Rat.BevelOuter:= bvLowered;
  PlaySoundFromResourceBASS('CLICK');
  RunEmbeddedExe('RATEHEXE');
  Rat.BevelOuter:= bvRaised;
end;

procedure TGameMenu.RouletteClick(Sender: TObject);
begin
  Roul.BevelOuter:= bvLowered;
  PlaySoundFromResourceBASS('CLICK');
  RunEmbeddedExe('ROULETTE');
  Roul.BevelOuter:= bvRaised;
end;

procedure TGameMenu.SnakeClick(Sender: TObject);
begin
  Snak.BevelOuter:= bvLowered;
  PlaySoundFromResourceBASS('CLICK');
  RunEmbeddedExe('SNAKE');
  Snak.BevelOuter:= bvRaised;
end;

procedure TGameMenu.JumpClick(Sender: TObject);
begin
  Jumb.BevelOuter:= bvLowered;
  PlaySoundFromResourceBASS('CLICK');
  RunEmbeddedExe('JUMP');
  Jumb.BevelOuter:= bvRaised;
end;

procedure TGameMenu.PongClick(Sender: TObject);
begin
  Ping.BevelOuter:= bvLowered;
  PlaySoundFromResourceBASS('CLICK');
  RunEmbeddedExe('PONG');
  Ping.BevelOuter:= bvRaised;
end;

procedure TGameMenu.PuzzleClick(Sender: TObject);
begin
  Puzz.BevelOuter:= bvLowered;
  PlaySoundFromResourceBASS('CLICK');
  RunEmbeddedExe('PUZZLE');
  Puzz.BevelOuter:= bvRaised;
end;

procedure TGameMenu.TetrisClick(Sender: TObject);
begin
  Tet.BevelOuter:= bvLowered;
  PlaySoundFromResourceBASS('CLICK');
  RunEmbeddedExe('TETRIS');
  Tet.BevelOuter:= bvRaised;
end;

procedure TGameMenu.OnFocusTimer(Sender: TObject);
begin
  GameMenu.Width:= Hel.Left+Hel.Width;
  GameMenu.Height:= Klack.Top + Klack.Height + Black.Top;

  if SecondsBetween(Now, LastFocusTime) >= 300 then
  begin
    FocusCheckTimer.Enabled := False;
    ShowAntiCaptcha;
  end;

  if SecondsBetween(Now, LastFocusTime) = 2 then HelpB.Visible:= true;
end;

procedure TGameMenu.ShowAntiCaptcha;
var
  CaptchaForm: TFormCaptcha;
begin
  CaptchaForm := TFormCaptcha.Create(nil);
  try
    if CaptchaForm.ShowCaptcha then
    begin
      // Captcha bestanden: Timer zurücksetzen, Monitoring neu starten
      LastFocusTime := Now;
      FocusCheckTimer.Enabled := True;
    end
    else
    begin
      // Captcha nicht bestanden: z.B. Programm beenden oder andere Aktion
      ShowMessage('Captcha nicht bestanden. Programm wird beendet.');
      Application.Terminate;
    end;
  finally
    CaptchaForm.Free;
  end;
end;

end.

