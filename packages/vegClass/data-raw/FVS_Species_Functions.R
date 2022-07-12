#############################################################################
#FVS_Species_Functions.R
#
#Remember this option for future vector updating!
#options(width = 40)
#FIA
#############################################################################

#############################################################################
#Vector FVS variants
#############################################################################

variants<-c("AK", "BM", "CA", "CI", "CR", "CS", "EC", "EM", "IE", "KT",
            "LS", "NC", "NE", "PN", "SN", "SO", "TT", "UT", "WC", "WS")

#############################################################################
#List of FVS character codes for each FVS variant
#############################################################################

fvsCharList<-list(
  "AK" = c('SF','AF','YC','TA','WS','LS','BE','SS','LP','RC',
           'WH','MH','OS','AD','RA','PB','AB','BA','AS','CW',
           'WI','SU','OH'),
  "BM" = c('WP','WL','DF','GF','MH','WJ','LP','ES','AF','PP',
           'WB','LM','PY','YC','AS','CW','OS','OH'),
  "CA" = c('PC','IC','RC','WF','RF','SH','DF','WH','MH','WB',
           'KP','LP','CP','LM','JP','SP','WP','PP','MP','GP',
           'WJ','BR','GS','PY','OS','LO','CY','BL','EO','WO',
           'BO','VO','IO','BM','BU','RA','MA','GC','DG','FL',
           'WN','TO','SY','AS','CW','WI','CN','CL','OH','RW'),
  "CI" = c('WP','WL','DF','GF','WH','RC','LP','ES','AF','PP',
           'WB','PY','AS','WJ','MC','LM','CW','OS','OH'),
  "CR" = c('AF','CB','DF','GF','WF','MH','RC','WL','BC','LM',
           'LP','PI','PP','WB','SW','UJ','BS','ES','WS','AS',
           'NC','PW','GO','AW','EM','BK','SO','PB','AJ','RM',
           'OJ','ER','PM','PD','AZ','CI','OS','OH'),
  "CS" = c('RC','JU','SP','VP','LP','OS','WP','WN','BN','TL',
           'TS','WT','BG','  ','SH','SL','MH','PH','HI','WH',
           'BH','PE','BI','AB','BA','PA','UA','EC','RM','BE',
           'SV','BC','AE','SG','HK','WE','EL','SI','RL','RE',
           'YP','BW','SM','AS','WA','GA','WO','RO','SK','BO',
           'SO','BJ','CK','SW','BR','SN','PO','DO','CO','PN',
           'CB','QI','OV','WK','NK','WL','QS','  ','SS','OB',
           'CA','PS','HL','BP','BT','QA','BK','  ','SY','BY',
           'RB','SU','WI','BL','OH','AH','RD','DW','HT','KC',
           'OO','CT','MV','MB','HH','SD'),
  "EC" = c('WP','WL','DF','SF','RC','GF','LP','ES','AF','PP',
           'WH','MH','PY','WB','NF','WF','LL','YC','WJ','BM',
           'VN','RA','PB','GC','DG','AS','CW','WO','PL','WI',
           'OS','OH'),
  "EM" = c('WB','WL','DF','LM','LL','RM','LP','ES','AF','PP',
           'GA','AS','CW','BA','PW','NC','PB','OS','OH'),
  "IE" = c('WP','WL','DF','GF','WH','RC','LP','ES','AF','PP',
           'MH','WB','LM','LL','PM','RM','PY','AS','CO','MM',
           'PB','OH','OS'),
  "KT" = c('WP','WL','DF','GF','WH','RC','LP','ES','AF','PP',
           'OT'),
  "LS" = c('JP','SC','RN','RP','WP','WS','NS','BF','BS','TA',
           'WC','EH','OS','RC','BA','GA','EC','SV','RM','BC',
           'AE','RL','RE','YB','BW','SM','BM','AB','WA','WO',
           'SW','BR','CK','RO','BO','NP','BH','PH','SH','BT',
           'QA','BP','PB','  ','BN','WN','HH','BK','OH','BE',
           'ST','MM','AH','AC','HK','DW','HT','AP','BG','SY',
           'PR','CC','PL','WI','BL','DM','SS','MA'),
  "NC" = c('OS','SP','DF','WF','MA','IC','BO','TO','RF','PP',
           'OH','RW'),
  "NE" = c('BF','TA','WS','RS','NS','BS','PI','RN','WP','LP',
           'VP','WC','AW','RC','OC','EH','HM','OP','JP','SP',
           'TM','PP','PD','SC','OS','RM','SM','BM','SV','YB',
           'SB','RB','PB','GB','HI','PH','SL','SH','MH','AB',
           'AS','WA','BA','GA','PA','YP','SU','CT','QA','BP',
           'EC','BT','PY','BC','WO','BR','CK','PO','OK','SO',
           'QI','WK','PN','CO','SW','SN','RO','SK','BO','CB',
           '  ','BU','YY','WR','HK','PS','HY','BN','WN','OO',
           'MG','MV','AP','WT','BG','SD','PW','SY','WL','BK',
           'BL','SS','BW','WB','EL','AE','RL','OH','BE','ST',
           'AI','SE','AH','DW','HT','HH','PL','PR'),
  "PN" = c('SF','WF','GF','AF','RF','SS','NF','YC','IC','ES',
           'LP','JP','SP','WP','PP','DF','RW','RC','WH','MH',
           'BM','RA','WA','PB','GC','AS','CW','WO','WJ','LL',
           'WB','KP','PY','DG','HT','CH','WI','  ','OT'),
  "SN" = c('FR','JU','PI','PU','SP','SA','SR','LL','TM','PP',
           'PD','WP','LP','VP','BY','PC','HM','FM','BE','RM',
           'SV','SM','BU','BB','SB','AH','HI','CA','HB','RD',
           'DW','PS','AB','AS','WA','BA','GA','HL','LB','HA',
           'HY','BN','WN','SU','YP','MG','CT','MS','MV','ML',
           'AP','MB','WT','BG','TS','HH','SD','RA','SY','CW',
           'BT','BC','WO','SO','SK','CB','TO','LK','OV','BJ',
           'SN','CK','WK','CO','RO','QS','PO','BO','LO','BK',
           'WI','SS','BD','EL','WE','AE','RL','OS','OH','OT'),
  "SO" = c('WP','SP','DF','WF','MH','IC','LP','ES','SH','PP',
           'WJ','GF','AF','SF','NF','WB','WL','RC','WH','PY',
           'WA','RA','BM','AS','CW','CH','WO','WI','GC','MC',
           'MB','OS','OH'),
  "TT" = c('WB','LM','DF','PM','BS','AS','LP','ES','AF','PP',
           'UJ','RM','BI','MM','NC','MC','OS','OH'),
  "UT" = c('WB','LM','DF','WF','BS','AS','LP','ES','AF','PP',
           'PI','WJ','GO','PM','RM','UJ','GB','NC','FC','MC',
           'BI','BE','OS','OH'),
  "WC" = c('SF','WF','GF','AF','RF','  ','NF','YC','IC','ES',
           'LP','JP','SP','WP','PP','DF','RW','RC','WH','MH',
           'BM','RA','WA','PB','GC','AS','CW','WO','WJ','LL',
           'WB','KP','PY','DG','HT','CH','WI','--','OT'),
  "WS" = c('SP','DF','WF','GS','IC', 'JP','RF','PP','LP','WB', 
           'WP','PM','SF','KP','FP', 'CP','LM','MP','GP','WE', 
           'GB','BD','RW','MH','WJ', 'UJ','CJ','LO','CY','BL', 
           'BO','VO','IO','TO','GC', 'AS','CL','MA','DG','BM', 
           'MC','OS','OH')
)

#############################################################################
#List of FIA codes for each FVS variant
#############################################################################

fvsFiaList<-list(
  "AK" = c('011','019','042','071','094','   ','095','098','108','242',
           '263','264','299','350','351','375','376','741','746','747',
           '920','928','998'),
  "BM" = c('119','073','202','017','264','064','108','093','019','122',
           '101','113','231','042','746','747','299','998'),
  "CA" = c('041','081','242','015','020','021','202','263','264','101',
           '103','108','109','113','116','117','119','122','124','127',
           '064','092','212','231','299','801','805','807','811','815',
           '818','821','839','312','333','351','361','431','492','542',
           '600','631','730','746','747','920','251','981','998','211'),
  "CI" = c('119','073','202','017','263','242','108','093','019','122',
           '101','231','746','064','475','113','747','299','998'),
  "CR" = c('019','018','202','017','015','264','242','073','102','113',
           '108','106','122','101','114','065','096','093','094','746',
           '749','745','814','803','810','823','843','375','063','066',
           '069','068','133','134','143','118','299','998'),
  "CS" = c('068','057','110','132','131','299','129','602','601','690',
           '694','691','693','   ','407','405','409','403','400','401',
           '402','404','408','531','543','545','546','742','316','313',
           '317','762','972','461','462','971','970','974','975','977',
           '621','951','318','540','541','544','802','833','812','837',
           '806','824','826','804','823','825','835','836','832','830',
           '813','817','822','827','828','831','834','   ','931','331',
           '450','521','552','741','743','746','901','   ','731','221',
           '373','611','920','922','998','391','471','491','500','571',
           '641','651','653','680','701','711'),
  "EC" = c('119','073','202','011','242','017','108','093','019','122',
           '263','264','231','101','022','015','072','042','064','312',
           '324','351','375','431','492','746','747','815','760','920',
           '299','998'),
  "EM" = c('101','073','202','113','072','066','108','093','019','122',
           '544','746','747','741','745','749','375','299','998'),
  "IE" = c('119','073','202','017','263','242','108','093','019','122',
           '264','101','113','072','133','066','231','746','740','321',
           '375','998','299'),
  "KT" = c('119','073','202','017','263','242','108','093','019','122',
           '999'),
  "LS" = c('105','130','125','125','129','094','091','012','095','071',
           '241','261','299','068','543','544','742','317','316','762',
           '972','975','977','371','951','318','314','531','541','802',
           '804','823','826','833','837','809','402','403','407','743',
           '746','741','375','   ','601','602','701','901','998','313',
           '315','319','391','421','462','491','500','660','693','731',
           '761','763','760','920','922','923','931','935'),
  "NC" = c('299','117','202','015','361','081','818','631','020','122',
           '998','211'),
  "NE" = c('012','071','094','097','091','095','090','125','129','131',
           '132','241','043','068','057','261','260','100','105','110',
           '123','126','128','130','299','316','318','314','317','371',
           '372','373','375','379','400','403','405','407','409','531',
           '540','541','543','544','545','621','611','651','746','741',
           '742','743','744','762','802','823','826','835','800','806',
           '817','827','830','832','804','825','833','812','837','813',
           '   ','330','332','374','462','521','591','601','602','641',
           '650','653','660','691','693','711','712','731','831','901',
           '922','931','951','952','970','972','975','998','313','315',
           '341','356','391','491','500','701','760','761'),
  "PN" = c('011','015','017','019','020','098','022','042','081','093',
           '108','116','117','119','122','202','211','242','263','264',
           '312','351','352','375','431','746','747','815','064','072',
           '101','103','231','492','500','768','920','   ','999'),
  "SN" = c('010','057','090','107','110','111','115','121','123','126',
           '128','129','131','132','221','222','260','311','313','316',
           '317','318','330','370','372','391','400','450','460','471',
           '491','521','531','540','541','543','544','552','555','580',
           '591','601','602','611','621','650','651','652','653','654',
           '660','680','691','693','694','701','711','721','731','740',
           '743','762','802','806','812','813','819','820','822','824',
           '825','826','827','832','833','834','835','837','838','901',
           '920','931','950','970','971','972','975','299','998','999'),
  "SO" = c('119','117','202','015','264','081','108','093','021','122',
           '064','017','019','011','022','101','073','242','263','231',
           '352','351','312','746','747','768','815','920','431','475',
           '478','298','998'),
  "TT" = c('101','113','202','133','096','746','108','093','019','122',
           '065','066','322','321','749','475','299','998'),
  "UT" = c('101','113','202','015','096','746','108','093','019','122',
           '106','064','814','133','066','065','142','749','748','475',
           '322','313','299','998'),
  "WC" = c('011','015','017','019','020','   ','022','042','081','093',
           '108','116','117','119','122','202','211','242','263','264',
           '312','351','352','375','431','746','747','815','064','072',
           '101','103','231','492','500','768','920','   ','999'),
  "WS" = c('117','202','015','212','081','116','020','122','108','101',
           '119','133','011','103','104','109','113','124','127','137',
           '142','201','211','264','064','065','062','801','805','807',
           '818','821','839','631','431','746','981','361','492','312',
           '475','299','998')
)

#############################################################################
#List of USDA plant symbols for each FVS variant
#############################################################################

fvsPlantList<-list("AK" = c('ABAM','ABLA','CANO9','LALA','PIGL',
                          'PILU','PIMA','PISI','PICO','THPL',
                          'TSHE','TSME','2TN','ALNUS','ALRU2',
                          'BEPA','BENE4','POBA2','POTR5','POBAT',
                          'SALIX','SASC','2TB'),
                   "BM" = c('PIMO3','LAOC','PSME','ABGR','TSME',
                          'JUOC','PICO','PIEN','ABLA','PIPO',
                          'PIAL','PIFL2','TABR2','CANO9','POTR5',
                          'POBAT','2TN','2TB'),
                   "CA" = c('CHLA','CADE27','THPL','ABCO','ABMA',
                          'ABSH','PSME', 'TSHE','TSME','PIAL',
                          'PIAT','PICO','PICO3','PIFL2','PIJE',
                          'PILA','PIMO3','PIPO','PIRA2','PISA2',
                          'JUOC','PIBR','SEGI2','TABR2','2TN',
                          'QUAG','QUCH2','QUDO','QUEN','QUGA4',
                          'QUKE','QULO','QUWI2','ACMA3','AECA',
                          'ALRU2','ARME','CHCHC4','CONU4','FRLA',
                          'JUGLA','LIDE3','PLRA','POTR5','POBAT',
                          'SALIX','TOCA','UMCA','2TB','SESE3'),
                   "CI" = c('PIMO3','LAOC','PSME','ABGR','TSHE',
                          'THPL','PICO','PIEN','ABLA','PIPO',
                          'PIAL','TABR2','POTR5','JUOC','CELE3',
                          'PIFL2','POBAT','2TN','2TB'),
                   "CR" = c('ABLA','ABLAA','PSME','ABGR','ABCO',
                          'TSME','THPL','LAOC','PIAR','PIFL2',
                          'PICO','PIED','PIPO','PIAL','PIST3',
                          'JUOS','PIPU','PIEN','PIGL','POTR5',
                          'POAN3','PODEM','QUGA','QUAR','QUEM',
                          'QUMA2','QUHY','BEPA','JUDE2','JUSC2',
                          'JUMO','JUVI','PIMO','PIDI3','PIMOF',
                          'PILE','2TN','2TB'),
                   "CS" = c('JUVI','JUNIP','PIEC2','PIVI2','PITA',
                          '2TN','PIST','JUNI','JUCI','NYSSA',
                          'NYBI','NYAQ2','NYSY','    ','CAOV2',
                          'CALA21','CAAL27','CAGL8','CARYA','CAAQ2',
                          'CACO15','CAIL2','CATE9','FAGR','FRNI',
                          'FRPR','FRQU','PODE3','ACRU','ACNE2',
                          'ACSA2','PRSE2','ULAM','CELA','CEOC',
                          'ULAL','ULMUS','ULPU','ULRU','ULTH',
                          'LITU','TIAM','ACSA3','FRAXI','FRAM2',
                          'FRPE','QUAL','QURU','QUFA','QUVE',
                          'QUCO2','QUMA3','QUMU','QUBI','QUMA2',
                          'QUMI','QUST','QUSI2','QUPR2','QUPA2',
                          'QUPA5','QUIM','QULY','QUNI','QUNU',
                          'QUPH','QUSH','    ','SAAL5','AEGL',
                          'CATAL','DIVI5','GLTR','POBA2','POGR4',
                          'POTR5','ROPS','    ','PLOC','TADI2',
                          'BENI','LIST2','SALIX','SANI','2TB',
                          'CACA18','CECA4','COFL2','CRATA','GYDI',
                          'MAPO','MAAC','MAVI2','MORUS','OSVI',
                          'OXAR'),
                   "EC" = c('PIMO3','LAOC','PSME','ABAM','THPL',
                          'ABGR','PICO','PIEN','ABLA','PIPO',
                          'TSHE','TSME','TABR2','PIAL','ABPR',
                          'ABCO','LALY','CANO9','JUOC','ACMA3',
                          'ACCI','ALRU2','BEPA','CHCHC4','CONU4',
                          'POTR5','POBAT','QUGA4','PRUNU','SALIX',
                          '2TN','2TB'),
                   "EM" = c('PIAL','LAOC','PSME','PIFL2','LALY',
                          'JUSC2','PICO','PIEN','ABLA','PIPO',
                          'FRPE','POTR5','POBAT','POBA2','PODEM',
                          'POAN3','BEPA','2TN','2TB'),
                   "IE" = c('PIMO3','LAOC','PSME','ABGR','TSHE',
                          'THPL','PICO','PIEN','ABLA','PIPO',
                          'TSME','PIAL','PIFL2','LALY','PIMO',
                          'JUSC2','TABR2','POTR5','POPUL','ACGL',
                          'BEPA','2TB','2TN'),
                   "KT" = c('PIMO3','LAOC','PSME','ABGR','TSHE',
                          'THPL','PICO','PIEN','ABLA','PIPO',
                          '2TREE'),
                   "LS" = c('PIBA2','PISY','PIRE','PIRE','PIST',
                          'PIGL','PIAB','ABBA','PIMA','LALA',
                          'THOC2','TSCA','2TN','JUVI','FRNI',
                          'FRPE','PODE3','ACSA2','ACRU','PRSE2',
                          'ULAM','ULRU','ULTH','BEAL2','TIAM',
                          'ACSA3','ACNI5','FAGR','FRAM2','QUAL',
                          'QUBI','QUMA2','QUMU','QURU','QUVE',
                          'QUEL','CACO15','CAGL8','CAOV2','POGR4',
                          'POTR5','POBA2','BEPA','    ','JUCI',
                          'JUNI','OSVI','ROPS','2TB','ACNE2',
                          'ACPE','ACSP2','CACA18','CADE12','CEOC',
                          'COFL2','CRATA','MALUS','NYSY','PLOC',
                          'PRPE2','PRVI','PRUNU','SALIX','SANI',
                          'SAERF','SAAL5','SOAM3'),
                   "NC" = c('2TN','PILA','PSME','ABCO','ARME',
                          'CADE27','QUKE','LIDE3','ABMA','PIPO',
                          '2TB','SESE3'),
                   "NE" = c('ABBA','LALA','PIGL','PIRU','PIAB',
                          'PIMA','PICEA','PIRE','PIST','PITA',
                          'PIVI2','THOC2','CHTH2','JUVI','JUNIP',
                          'TSCA','TSUGA','PINUS','PIBA2','PIEC2',
                          'PIPU5','PIRI','PISE','PISY','2TN',
                          'ACRU','ACSA3','ACNI5','ACSA2','BEAL2',
                          'BELE','BENI','BEPA','BEPO','CARYA',
                          'CAGL8','CALA21','CAOV2','CAAL27','FAGR',
                          'FRAXI','FRAM2','FRNI','FRPE','FRPR',
                          'LITU','LIST2','MAAC','POTR5','POBA2',
                          'PODE3','POGR4','POHE4','PRSE2','QUAL',
                          'QUMA2','QUMU','QUST','QUERC','QUCO2',
                          'QUIM','QUNI','QUPA2','QUPR2','QUBI',
                          'QUMI','QURU','QUFA','QUVE','QUPA5',
                          '     ','AESCU','AEFL','BEOC2','CEOC',
                          'DIVI5','ILOP','JUCI','JUNI','MAPO',
                          'MAGNO','MAVI2','MALUS','NYAQ2','NYSY',
                          'OXAR','PATO2','PLOC','QUPH','ROPS',
                          'SANI','SAAL5','TIAM','TIAMH','ULMUS',
                          'ULAM','ULRU','2TB','ACNE2','ACPE',
                          'AIAL','AMELA','CACA18','COFL2','CRATA',
                          'OSVI','PRUNU','PRPE2'),
                   "PN" = c('ABAM','ABCO','ABGR','ABLA','ABMA',
                          'PISI','ABPR','CANO9','CADE27','PIEN',
                          'PICO','PIJE','PILA','PIMO3','PIPO',
                          'PSME','SESE3','THPL','TSHE','TSME',
                          'ACMA3','ALRU2','ALRH2','BEPA','CHCHC4',
                          'POTR5','POBAT','QUGA4','JUOC','LALY',
                          'PIAL','PIAT','TABR2','CONU4','CRATA',
                          'PREM','SALIX','    ','2TREE'),
                   "SN" = c('ABIES','JUNIP','PICEA','PICL','PIEC2',
                          'PIEL','PIGL2','PIPA2','PIPU5','PIRI',
                          'PISE','PIST','PITA','PIVI2','TADI2',
                          'TAAS','TSUGA','ACBA3','ACNE2','ACRU',
                          'ACSA2','ACSA3','AESCU','BETUL','BELE',
                          'CACA18','CARYA','CATAL','CELTI','CECA4',
                          'COFL2','DIVI5','FAGR','FRAXI','FRAM2',
                          'FRNI','FRPE','GLTR','GOLA','HALES',
                          'ILOP','JUCI','JUNI','LIST2','LITU',
                          'MAGNO','MAAC','MAGR4','MAVI2','MAMA2',
                          'MALUS','MORUS','NYAQ2','NYSY','NYBI',
                          'OSVI','OXAR','PEBO','PLOC','POPUL',
                          'POGR4','PRSE2','QUAL','QUCO2','QUFA',
                          'QUPA5','QULA2','QULA3','QULY','QUMA3',
                          'QUMI','QUMU','QUNI','QUPR2','QURU',
                          'QUSH','QUST','QUVE','QUVI','ROPS',
                          'SALIX','SAAL5','TILIA','ULMUS','ULAL',
                          'ULAM','ULRU','2TN','2TB','2TREE'),
                   "SO" = c('PIMO3','PILA','PSME','ABCO','TSME',
                          'CADE27','PICO','PIEN','ABSH','PIPO',
                          'JUOC','ABGR','ABLA','ABAM','ABPR',
                          'PIAL','LAOC','THPL','TSHE','TABR2',
                          'ALRH2','ALRU2','ACMA3','POTR5','POBAT',
                          'PREM','QUGA4','SALIX','CHCHC4','CELE3',
                          'CEMOG','2TE','2TD'),
                   "TT" = c('PIAL','PIFL2','PSME','PIMO','PIPU',
                          'POTR5','PICO','PIEN','ABLA','PIPO',
                          'JUOS','JUSC2','ACGR3','ACGL','POAN3',
                          'CELE3','2TN','2TB'),
                   "UT" = c('PIAL','PIFL2','PSME','ABCO','PIPU',
                          'POTR5','PICO','PIEN','ABLA','PIPO',
                          'PIED','JUOC','QUGA','PIMO','JUSC2',
                          'JUOS','PILO','POAN3','POFR2','CELE3',
                          'ACGR3','ACNE2','2TN','2TB'),
                   "WC" = c('ABAM','ABCO','ABGR','ABLA','ABMA',
                          '    ','ABPR','CANO9','CADE27','PIEN',
                          'PICO','PIJE','PILA','PIMO3','PIPO',
                          'PSME','SESE3','THPL','TSHE','TSME',
                          'ACMA3','ALRU2','ALRH2','BEPA','CHCHC4',
                          'POTR5','POBAT','QUGA4','JUOC','LALY',
                          'PIAL','PIAT','TABR2','CONU4','CRATA',
                          'PREM','SALIX','    ','2TREE'),
                   "WS" = c('PILA','PSME','ABCO','SEGI2','CADE27',
                          'PIJE','ABMA','PIPO','PICO','PIAL',
                          'PIMO3','PIMO','ABAM','PIAT','PIBA',
                          'PICO3','PIFL2','PIRA2','PISA2','PIWA',
                          'PILO','PSMA','SESE3','TSME','JUOC',
                          'JUOS','JUCA7','QUAG','QUCH2','QUDO',
                          'QUKE','QULO','QUWI2','LIDE3','CHCHC4',
                          'POTR5','UMCA','ARME','CONU4','ACMA3',
                          'CELE3','2TN','2TB')
)

##############################################################
#List of FVS sequence numbers for each variant
##############################################################

MINSEQ = 1
MAXSEQ = c(23, 18, 50, 19, 38, 96, 32, 19, 23, 11,
           68, 12, 108, 39, 90, 33, 18, 24, 39, 43)

fvsSeqList<-list("AK" = seq(from = MINSEQ, to = MAXSEQ[1], by = 1),
                 "BM" = seq(from = MINSEQ, to = MAXSEQ[2], by = 1),
                 "CA" = seq(from = MINSEQ, to = MAXSEQ[3], by = 1),
                 "CI" = seq(from = MINSEQ, to = MAXSEQ[4], by = 1),
                 "CI" = seq(from = MINSEQ, to = MAXSEQ[5], by = 1),
                 "CS" = seq(from = MINSEQ, to = MAXSEQ[6], by = 1),
                 "EC" = seq(from = MINSEQ, to = MAXSEQ[7], by = 1),
                 "EM" = seq(from = MINSEQ, to = MAXSEQ[8], by = 1),
                 "IE" = seq(from = MINSEQ, to = MAXSEQ[9], by = 1),
                 "KT" = seq(from = MINSEQ, to = MAXSEQ[10], by = 1),
                 "LS" = seq(from = MINSEQ, to = MAXSEQ[11], by = 1),
                 "NC" = seq(from = MINSEQ, to = MAXSEQ[12], by = 1),
                 "NE" = seq(from = MINSEQ, to = MAXSEQ[13], by = 1),
                 "PN" = seq(from = MINSEQ, to = MAXSEQ[14], by = 1),
                 "SN" = seq(from = MINSEQ, to = MAXSEQ[15], by = 1),
                 "SO" = seq(from = MINSEQ, to = MAXSEQ[16], by = 1),
                 "TT" = seq(from = MINSEQ, to = MAXSEQ[17], by = 1),
                 "UT" = seq(from = MINSEQ, to = MAXSEQ[18], by = 1),
                 "WC" = seq(from = MINSEQ, to = MAXSEQ[19], by = 1),
                 "WS" = seq(from = MINSEQ, to = MAXSEQ[20], by = 1))

#############################################################################
#Start fvs-family functions
#
#The following functions comprise the sp-family of functions.
#- fvsSearchVar
#- fvsGetSp
#- fvsGetSpData
#- fvsConvert
#
#Function descriptions are provided in the comment headers below.
#############################################################################

#############################################################################
#Function: fvsSearchVar
#
#This function searches through variant vector and returns an index if the 
#input variant is found. If the input variant is not found in the variant
#vector then a value of NA is returned.
#
#Arguments:
#
#var: two digit character of FVS variant. If input variant is not a valid
#     variant, then a NA value will be returned.
#############################################################################

fvsSearchVar<-function(var)
{
  #Capitalize variant
  var<-toupper(var)
  
  #Check if variant is a valid variant. If it is not, assign NA to varIndex and
  #return
  if(!var %in% variants)
  {
    varIndex = NA
  }
  
  #Search for variant index
  else
  {
    varIndex = match(var, variants)
  }
  
  return(varIndex)
}

#############################################################################
#Function: fvsGetSpData
#
#This function returns a vector of FVS character species codes, FIA codes,
#USDA plant symbols, or FVS sequence numbers based on the input variant
#and TYPE arguments
#
#Arguments:
#
#var:  two digit character of FVS variant. If input variant is not a valid
#      variant, then a blank dataframe will be returned from function.
#TYPE: Integer value signifying what type of species to return.
#      1 = FVS character species codes
#      2 = FIA species codes
#      3 = PLANT symbols
#      4 = FVS sequence numbers
#      If the value of TYPE is not a value from 1 - 4 then a empty vector
#      will be returned.
#############################################################################

fvsGetSp<-function(var, type = 2)
{
  #Capitalize variant
  var<-toupper(var)
  
  #Check if type is a value from 1-4. Assign NA to sp and return.
  if(!type %in% c(1, 2, 3, 4))
  {
    sp<-NA
    return(sp)
  }
  
  #Search for variant index
  varIndex<-fvsSearchVar(var)
  
  #Return NA if varIndex is na
  if(is.na(varIndex))
  {
    sp = NA
    return(sp)
  }
  
  #FVS character species code
  if(type == 1)
  {
    sp<-fvsCharList[[varIndex]]
  }
  
  #FIA species codes
  else if(type == 2)
  {
    sp<-fvsFiaList[[varIndex]]
  }
  
  #Plant symbols
  else if(type == 3)
  {
    sp<-fvsPlantList[[varIndex]]
  }
  
  #FVS sequence numbers
  else
  {
    sp<-fvsSeqList[[varIndex]]
  }
  
  return(sp)
}

#############################################################################
#Function: fvsGetSpData
#
#This function returns a dataframe containing FVS character codes, FIA
#species codes, USDA plant symbols, and FVS sequence numbers for the input
#variant
#
#Arguments:
#
#var: two digit character of FVS variant. If input variant is not a valid
#     variant, then a blank dataframe will be returned from function.
#############################################################################

fvsGetSpData<-function(var)
{
  #Capitalize variant
  var<-toupper(var)
  
  #Check if variant is a valid variant. If it is not, assign NA to varIndex and
  #return
  if(!var %in% variants)
  {
    spData<-NA
    return(spData)
  }
  
  #Search for variant index
  varIndex<-fvsSearchVar(var)
  
  #Create dataset to return
  spData<-data.frame(FVS = fvsCharList[[varIndex]],
                     FIA = fvsFiaList[[varIndex]],
                     PLANT = fvsPlantList[[varIndex]],
                     SEQ = fvsSeqList[[varIndex]])
  
  return(spData)
}

#############################################################################
#Function: fvsConvert
#
#This function is used to convert between FVS character codes, FIA species
#codes, USDA plant symbols, and FVS sequence numbers. This function will
#return a single species code based on the input variant, from, and to
#arguments.
#
#Arguments:
#
#var:  two digit character of FVS variant. If input variant is not a valid
#      variant, then a NA value will be returned.
#sp:   incoming species code. This can be either an FVS species code,
#from: Integer value signifying the type of species you are converting from.
#      1 = FVS character code
#      2 = FIA species code
#      3 = Plant symbol
#      4 = FVS sequence number
#      If the value of from argument is anything other than 1 - 3, then a NA
#      value  will be returned from the function.
#to:   Integer value signifying the type of species you are converting to.
#      1 = FVS character code
#      2 = FIA species code
#      3 = Plant symbol
#      4 = FVS sequence number
#      If the value of to argument  is anything other than 1 - 3, then a NA
#      value will be returned from the function.
#############################################################################

fvsConvert<-function(var, sp, from, to)
{
  #Test if from and to are anything other than 1 - 4
  #If values are anything other than 1 - 4, then code of NA is returned
  if(!from %in% c(1, 2, 3, 4) | !to %in% c(1, 2, 3, 4))
  {
    spTo = NA
    return(spTo)
  }
  
  #Search for variant index
  varIndex<-fvsSearchVar(var)
  
  if(is.na(varIndex))
  {
    spTo = NA
    return(spTo)
  }
  
  #Determine which species codes to search through based on from code
  spIndex<-fvsGetSpIndex(var, sp, from)
  
  #If spIndex returns as NA from search, then spTo is set to NA
  if(is.na(spIndex))
  {
    spTo = NA
  }
  
  #Determine which species codes to pull value from based on to code
  else
  {
    if(to == 1)
    {
      spTo = fvsCharList[[varIndex]][spIndex]
    }
    
    if(to == 2)
    {
      spTo = fvsFiaList[[varIndex]][spIndex]
    }
    
    if(to == 3)
    {
      spTo = fvsPlantList[[varIndex]][spIndex]
    }
    
    if(to == 4)
    {
      spTo = fvsSeqList[[varIndex]][spIndex]
    }
  }
  
  return(spTo)
}

#############################################################################
#Function: fvsGetSpIndex
#
#This is a helper function that is used to search for a species index in
#FVS character codes, FIA species codes, plant symbols, and fvs sequence
#number vectors.
#
#Arguments:
#
#sp:   incoming species code. This can be either an FIA species code, USDA
#      plant symbol, or scientific name.
#from: Integer value signifying the type of species you are converting from.
#      1 = FVS character code
#      2 = FIA species code
#      3 = Plant symbol
#      4 = FVS sequence number
#      If the value of from argument is anything other than 1 - 4, then a NA
#      value  will be returned from the function.
############################################################################

fvsGetSpIndex<- function(var, sp, from)
{
  if(!toupper(var) %in% variants | !from %in% c(1, 2, 3, 4))
  {
    spIndex = NA
    return(spIndex)
  }
  
  #Search for variant index
  varIndex<-fvsSearchVar(var)
  
  if(is.na(varIndex))
  {
    spIndex = NA
    return(spIndex)
  }
  
  #Determine which species codes to search through based on from code
  #FVS character codes
  if(from == 1)
  {
    spIndex = match(sp, fvsCharList[[varIndex]])
  }
  
  #FIA species code
  else if(from == 2)
  {
    spIndex = match(as.character(sp), fvsFiaList[[varIndex]])
  }
  
  #Plant symbol
  else if(from == 3)
  {
    spIndex = match(toupper(sp), fvsPlantList[[varIndex]])
  }
  
  #FVS sequence number
  else
  {
    spIndex = match(sp, fvsSeqList[[varIndex]])
  }
  
  #If spIndex returns as NA from search, then spTo is set to NA
  if(is.na(spIndex))
  {
    spIndex = NA
  }
  
  return(spIndex)
}
