let serverList =
      [ { id = 1, name = "横須賀鎮守府", address = "http://203.104.209.71/" }
      , { id = 2, name = "呉鎮守府", address = "http://203.104.209.87/" }
      , { id = 3, name = "佐世保鎮守府", address = "http://125.6.184.215/" }
      , { id = 4, name = "舞鶴鎮守府", address = "http://203.104.209.183/" }
      , { id = 5, name = "大湊警備府", address = "http://203.104.209.150/" }
      , { id = 6, name = "トラック泊地", address = "http://203.104.209.134/" }
      , { id = 7, name = "リンガ泊地", address = "http://203.104.209.167/" }
      , { id = 8, name = "ラバウル基地", address = "http://203.104.209.199/" }
      , { id = 9, name = "ショートランド泊地", address = "http://125.6.189.7/" }
      , { id = 10, name = "ブイン基地", address = "http://125.6.189.39/" }
      , { id = 11, name = "タウイタウイ泊地", address = "http://125.6.189.71/" }
      , { id = 12, name = "パラオ泊地", address = "http://125.6.189.103/" }
      , { id = 13, name = "ブルネイ泊地", address = "http://125.6.189.135/" }
      , { id = 14, name = "単冠湾泊地", address = "http://125.6.189.167/" }
      , { id = 15, name = "幌筵泊地", address = "http://125.6.189.215/" }
      , { id = 16, name = "宿毛湾泊地", address = "http://125.6.189.247/" }
      , { id = 17, name = "鹿屋基地", address = "http://203.104.209.23/" }
      , { id = 18, name = "岩川基地", address = "http://203.104.209.39/" }
      , { id = 19, name = "佐伯湾泊地", address = "http://203.104.209.55/" }
      , { id = 20, name = "柱島泊地", address = "http://203.104.209.102/" }
      ]

let baseOutputPath = "/user/kc-static-get/output"

in  { serverList = serverList, baseOutputPath = baseOutputPath }
