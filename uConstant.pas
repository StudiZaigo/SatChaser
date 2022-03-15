unit uConstant;

interface

type
  TAzRotator = (az450, az360);
  TElRotator = (el180, el90, elNone);
  TRotateMode = (gsNormal, gsOverlap, gsFlip, gsCrossing);
  TControlMode = (ctNone, ctManual, ctTracking);

const
  cnCalsat: string = 'CALSAT32';
  cnCalsatText: string = 'CALSAT32(SGP)';
  cnNodeMain: string  ='/Main';
  cnNodeAlert: string ='/Alert';
  cnRegistry: string = '/Registry';
  cnNodeOptions: string = '/Options';
  cnNodeApp: string = '/Options/App';
  cnNodeCom: string = '/Options/Com';
  cnNodeGs232: string = '/Options/GS232';
  cnInterval: string = 'Interval';
  cnApp: string ='App';
  cnLeft: string ='Left';
  cnTop: string = 'Top';
  cnTabIndex: string = 'TabIndex';
  cnInifile: string = 'Inifile';
  cnElements: string = 'Elements';
  cnRegKey: string = 'RegKey';
  cnComPort: string = 'ComPort';
  cnBaudRate: string = 'BaudRate';
  cnDataBits: string = 'DataBits';
  cnParity: string = 'Parity';
  cnStopBits: string = 'StopBits';
  cnFlowControl: string = 'FlowControl';
  cnAzRotator: string = 'AzRotator';
  cnElRotator: string = 'ElRotator';
  cnParkingAz: string = 'ParkingAz';
  cnParkingEl: string = 'ParkingEl';
  cnGoParking: string = 'GoParking';
  cnRotateSpeed: string = 'RotateSpeed';
  cnAzOffset: string = 'AzOffset';
  cnRotateMode: string = 'RotateMode';
  cnRotateModeName: array[0..3] of string = ('Normal', 'Overlap', 'Flip', 'Crossing');
  cnPreAOSTime: string = 'PreAosTime';
  cnAlert: string = 'Alert';


implementation

end.
