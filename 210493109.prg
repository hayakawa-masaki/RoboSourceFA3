1 ' ===================================
2 '
3 '  21049001 STEP5 Assy1プログラム
4 '
5 ' 作成者：自動化T
6 ' 作成日：2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1から流用
8 ' Ver 0.3 2021.12.22 画像検査関数ISInspection→ISInspectionSingle、画像検査追加 file:210542003
9 ' ===================================
10 '===== <Insight定数> =====
11 '===== <Insight変数定義> =====
12 Dim PInspPosition(30)               '画像処理Function引渡し用位置変数
13 Dim MInspGroup%(30)                 '画像処理Function引渡し用変数
14 Def Inte MIN_IS_Ready               '【入力IO】Insight準備OK
15 Def Inte MIN_IS_JobLoadOK           '【入力IO】Insightジョブロード正常終了
16 Def Inte MIN_IS_JobLoadNG           '【入力IO】Insightジョブロード異常終了
17 Def Inte MIN_IS_InspGSetOK          '【入力IO】Insight検査グループ番号設定正常終了
18 Def Inte MIN_IS_InspGSetNG          '【入力IO】Insight検査グループ番号設定異常終了
19 Def Inte MIN_IS_InspOK              '【入力IO】Insight検査OK
20 Def Inte MIN_IS_InspNG              '【入力IO】Insight検査NG
21 Def Inte MIN_IS_InspErr             '【入力IO】Insight検査異常終了
22 Def Inte MIN_IS_InspCapDone         '【入力IO】Insight検査画像取込完了
23 '
24 Def Inte MIN_IS_ErrNum              '【入力IO】Insight処理エラー番号取得開始アドレス(16bit)
25 'Output Signal
26 Def Inte MOUT_IS_JobLoadReq         '【出力IO】Insight JOBロード要求
27 Def Inte MOUT_IS_InspGSetReq        '【出力IO】Insight 検査グループ番号設定要求
28 Def Inte MOUT_IS_Insp               '【出力IO】Insight 検査実行要求
29 '
30 Def Inte MOUT_IS_JobNum             '【出力IO】Insight JOB番号設定開始アドレス(16bit)
31 Def Inte MOUT_IS_InspGNum           '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
32 '
33 Def Inte MOUT_InspErrNum            '【出力IO】検査実行エラー番号開始アドレス(16bit)
34 Def Inte MOUT_InspNGStepNum         '【出力IO】検査実行NGStep番号開始アドレス(16bit)
35 '作業用変数
36 Def Inte MInspErrNum                '検査実行エラー番号
37 Def Inte MInspNGStepNum             '検査実行NGStep番号
38 Def Inte MRtn                       'Function戻り値取得用
39 Def Inte MRtn2                      'Function戻り値取得用
40 Def Inte MRet3                      'Function戻り値取得用
41 Def Inte MGRtn                      'Function戻り値取得用 ネジ供給機
42 Def Inte MInspErrNumSub             '検査実行エラー番号sub　20190820追加
43 Def Inte MovrdA                     'ネジ締めOvrd 可変用   20191127追加
44 Def Float MSpdA                     'ネジ締めSpd　可変用   20191127追加
45 Def Pos PTemp                       'ネジ締め上空位置計算用    20200312追加'
46 MovrdA% = 20                        'ネジ締めOvrd 可変用   20191127追加
47 MSpdA = 800                        'ネジ締めSpd　可変用   20191127追加
48 '===== <Insight変数設定> =====
49 MIN_IS_Ready%        =   11380      '【入力IO】Insight準備OK
50 MIN_IS_JobLoadOK%    =   11381      '【入力IO】Insightジョブロード正常終了
51 MIN_IS_JobLoadNG%    =   11382      '【入力IO】Insightジョブロード異常終了
52 MIN_IS_InspGSetOK%   =   11383      '【入力IO】Insight検査グループ番号設定正常終了
53 MIN_IS_InspGSetNG%   =   11384      '【入力IO】Insight検査グループ番号設定異常終了
54 MIN_IS_InspOK%       =   11385      '【入力IO】Insight検査OK
55 MIN_IS_InspNG%       =   11386      '【入力IO】Insight検査NG
56 MIN_IS_InspErr%      =   11387      '【入力IO】Insight検査異常終了
57 MIN_IS_InspCapDone%  =   11388      '【入力IO】Insight検査画像取込完了
58 MIN_IS_ErrNum%       =   11408      '【入力IO】Insight処理エラー番号開始アドレス(16bit)
59 'Output Signal
60 MOUT_IS_JobLoadReq%  =   12370      '【出力IO】Insight JOBロード要求
61 MOUT_IS_InspGSetReq% =   12371      '【出力IO】Insight 検査グループ番号設定要求
62 MOUT_IS_Insp%        =   12372      '【出力IO】Insight 検査実行要求
63 MOUT_IS_JobNum%      =   12384      '【出力IO】Insight JOB番号設定開始アドレス(16bit)
64 MOUT_IS_InspGNum%    =   12400      '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
65 MOUT_InspErrNum%     =   12416      '【出力IO】検査実行エラー番号開始アドレス(16bit)
66 MOUT_InspNGStepNum%  =   12432      '【出力IO】検査実行NGStep番号開始アドレス(16bit)
67 '===== <電ドラ定数> =====
68 '===== <電ドラ変数定義> =====
69 X20_Driver=11248                    '電ドラステイタス1　Driver Status 1
70 X21_Driver=11249 '電ドラステイタス2  Driver Status 2
71 X22_Driver=11250 '電ドラステイタス3  Driver Status 3
72 X23_Driver=11251 '電ドラステイタス4  Driver Status 4
73 X24_Driver=11252 '電ドラエラーメッセージ1 Driver Error E1
74 X25_Driver=11253 '電ドラエラーメッセージ2 Driver Error E2
75 X26_Driver=11254 '電ドラエラーメッセージ3 Driver Error E3
76 X27_Driver=11255 '電ドラエラーメッセージ4 Driver Error E4
77 X28_Driver=11256 '電ドラトータルエラーシグナル Total Error
78 X29_Driver=11257 '電ドラ終了シグナル Comlete signal
79 X2A_Driver=11258 '電ドラエラーメッセージ5 Driver Error E5
80 '11584   'toRBトルクドライバ-COMP_ERR送信
81 Y60_Driver=12240 '電ドラ半時計回り CCW
82 Y61_Driver=12241 '電ドラ時計回り CW
83 Y62_Driver=12242 'バンクセッティング BANK C1
84 Y63_Driver=12243 'バンクセッティング BANK C2
85 Y64_Driver=12244 'バンクセッティング BANK C3
86 Y65_Driver=12245 'プログラムセッティング PRG SET F1
87 Y66_Driver=12246 'プログラムセッティング PRG SET F2
88 Y67_Driver=12247 'プログラムセッティング PRG SET F3
89 '組立2
90 X34_NG1=11268 'ねじっこ1　Read
91 X35_NG2=11269 'ねじっこ2　Read
92 '組立3
93 X3F_NG1=11279 'ねじっこ1　Read
94 '
95 Dim PScrewPosTemp(10)                                               'ネジ締め用Function引数変数
96 Dim PGetScrewPosTemp(10)                                            'ねじ供給機からねじを得るFunction引数変数
97 Dim PEscapePosi(10)
98 MLoopCnt% = 0'
99 '===== <ロボット定数> =====
100 '===== <ロボット変数定義> =====
101 MRBTOpeGroupNo = 0                    'ロボット動作番号初期化
102 MCommentD1001 = 0
103 MCommentD1002 = 0
104 MCommentD1003 = 0
105 MScreenNo = 0
106 '
107 MCommentTSU = 0
108 MCommentTSD = 0
109 'ウィンド画面番号設定
110 MWindReSet = 0
111 MWindInfoScr = 5
112 MWindErrScr = 10
113 MWindErrScr2 = 11
114 MWindErrScr3 = 13
115 MWindErrScr17 = 17
116 MWindErrScr18 = 18
117 MWindCmmnScr = 20
118 MWindJigRelase19049 = 60
119 MWindJigRelase19050 = 61
120 MWindJigRelase19051 = 62
121 '
122 MClear% = 0        'KEY_のクリア
123 MAbout% = 1        'KEY_停止
124 MNext% = 2         'KEY_次のステップへ移行
125 MContinue% = 3     'KEY_継続 再度同じ動作を行う
126 '
127 Def Inte MNgProcess
128 MNgProcess% = 5      'KEY_NG
129 '
130 MAssyOK% = 6       '組立完了
131 MPass% = 7         '工程パス
132 MPiasNG% = 8       'Pias確認時履歴NG
133 '
134 '初期化用KEY番号   '
135 MRobotInit1% = 11  '初期位置用
136 MRobotInit2% = 12  '初期位置用
137 MRobotInit3% = 13  '初期位置用
138 MRobotInit4% = 14  '初期位置用
139 '
140 MIN_INIT1REQUEST% = 11568 'toRBT_ロボット初期位置1要求
141 MIN_INIT2REQUEST% = 11569 'toRBT_ロボット初期位置2要求
142 MIN_INIT3REQUEST% = 11570 'toRBT_ロボット初期位置3要求
143 MIN_INIT4REQUEST% = 11571 'toRBT_ロボット初期位置4要求
144 '
145 MOUT_INIT1RECIVE% = 12560 'toPLC_ロボット初期位置1受信
146 MOUT_INIT2RECIVE% = 12561 'toPLC_ロボット初期位置2受信
147 MOUT_INIT3RECIVE% = 12562 'toPLC_ロボット初期位置3受信
148 MOUT_INIT4RECIVE% = 12563 'toPLC_ロボット初期位置4受信
149 '
150 MopeNo = 0
151 '
152 MOK% = 1               '各判定用
153 MNG% = 0               '各判定用
154 MTIMEOUT% = -1         '各判定用
155 MJudge% = 0            '判定情報格納用
156 '
157 '
158 MRECIVETIME& = 0
159 MSETTIMEOUT10& = 10000&                '10秒設定
160 MSETTIMEOUT03& = 3000&                 '3秒設定
161 MSETTIMEOUT01& = 1000&                 '1秒設定
162 MSETTIMEOUT05& = 5000&                 '5秒設定
163 MSETTIMEOUT009& = 900&                 '0.9秒設定
164 MSETTIMEOUT008& = 800&                 '0.8秒設定
165 MSETTIMEOUT007& = 700&                 '0.7秒設定
166 MSETTIMEOUT006& = 600&                 '0.6秒設定
167 MSETTIMEOUT005& = 500&                 '0.5秒設定
168 MSETTIMEOUT004& = 400&                 '0.4秒設定
169 MSETTIMEOUT003& = 300&                 '0.3秒設定
170 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
171 MIN_PIAS_ComOK% = 11552                'PC通信OK
172 MIN_PIAS_ComTimeOut% = 11576           'PC通信確認タイムアウト
173 MIN_PIAS_ComNG% = 11553                'PC通信NG
174 MOUT_PIAS_ComCheck% = 12544            'PC通信確認要求
175 MOUT_PIAS_Missing_Process% = 12546     '工程抜け確認要求
176 MIN_PIAS_ModelTypeNG% = 11554          'モデル仕向NG
177 MIN_PIAS_ProcessHistryNG% = 11555      '前工程履歴NG
178 MIN_PIAS_ProcessHistryOK% = 11556      '前工程履歴OK
179 MIN_PIAS_ProcessHistryErr% = 11557     '工程履歴処理エラー
180 MIN_PIAS_MyProcessComp% = 11573        '自工程履歴あり
181 MIN_PIAS_ProcessHistryTimeOut% = 11578 '工程履歴タイムアウト
182 MOUT_OKNG% = 12549                     'PLC OUT でOK=1, NG=0 出力
183 '
184 MOUT_PiasPCBNumberCheck = 12557        '基板番号照合
185 MIN_PiasPCBNumberOK% = 11566          '基板番号OK
186 MIN_PiasPCBNumberNG% = 11565          '基板番号NG
187 MIN_PiasPCBNumberErr% = 11567         '基板番号処理エラー
188 '
189 MOUT_PiasAssyResultOK% = 12549    '組立OK
190 MOUT_PiasAssyResultNG% = 12550    '組立NG
191 MOUT_PiasAssyResultWr% = 12548    '工程履歴書き込み
192 '
193 MIN_PiasProcessNG% = 11559        '工程履歴処理NG
194 MIN_PiasProcessOtherErr% = 11560  '工程履歴処理エラー(なんかのトラブル)
195 MIN_PiasProcessOK% = 11558        '工程履歴処理OK
196 '
197 MIN_Insight_Use% = 11369               '画像確認ON
198 MIN_TorqueCheck% = 11348               'トルクチェック
199 '
200 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT操作権
201 MOUT_RED_LIGHT% = 12356            'PATLIGHT 赤 点灯
202 MOUT_RED_FLASH% = 12357            'PATLIGHT 赤 点滅
203 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT 黄 点灯
204 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT 黄 点滅
205 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT 青 点灯
206 MOUT_GREEN_FLASH% = 12361          'PATLIGHT 青 点滅
207 '
208 MOUT_ST_DATETIME% = 12551          '組立開始日付時刻
209 MOUT_ED_DATETIME% = 12552          '組立終了日付時刻
210 '
211 MOUT_TORQUE_CHECK% = 12367         'PLCへトルクチェック中を送信
212 '
213 MIN_ASSY_CANCEL% = 11366           '組立を行うかのフラグ
214 '
215 MLoopFlg% = 0                      'KEY入力後のOK or NG内容
216 MRtn% = 0
217 MopeNo = 0
218 MRet = 0
219 'MRtn = 0
220 MRet3% = 0
221 '
222 Def Inte MInputQty          '投入数 演算変数
223 Def Inte MAssyOkQty         '組立ＯＫ数 演算変数
224 Def Inte MAssyNgQty         '組立ＮＧ数 演算変数(未使用)
225 Def Inte MSuctionErrQty     '吸着エラー数 演算変数 2022/04/27 渡辺
226 Def Inte nAssyOkQty         '未使用
227 Def Inte MScrewNo
228 Def Inte MReTry
229 '===== <IO変数定義> =====
230 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
231 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
232 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
233 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
234 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
235 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
236 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
237 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
238 '
239 Def Inte Y68_VV1            ' アーム先端　ネジ吸着バルブ
240 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
241 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
242 '
243 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
244 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
245 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
246 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
247 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
248 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
249 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
250 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
251 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
252 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
253 '
254 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
255 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
256 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
257 '
258 Def Inte MOUT_LED1          ' 画像処理用LED照明
259 '
260 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
261 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
262 '
263 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
264 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
265 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
266 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
267 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
268 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
269 '
270 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
271 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
272 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
273 '
274 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
275 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
276 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
277 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
278 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
279 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
280 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
281 Y68_VV1%    =  12248    ' アーム先端　ネジ吸着バルブ 'Y68_VV1% = 12250をY68_VV1% = 12248に変更(8/27中村)
282 Y6B_VB1%    =  12250    'アーム先端　吸着破壊バルブ'Y6B_VB1% = 12251をY6B_VB1% = 12250に変更(8/27中村)
283 MOUT_VB1%   =  12251    ' アーム先端　ネジ吸着破壊バルブ
284 '
285 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
286 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
287 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
288 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
289 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
290 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
291 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
292 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
293 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
294 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
295 '
296 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
297 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
298 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
299 '
300 MOUT_LED1%  =  12239    ' 画像処理用LED照明
301 '
302 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
303 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
304 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
305 '
306 '
307 '共通
308 Def Inte MTEST_KEY                      'デバックテスト用
309 Def Inte MOn                            '出力=1
310 Def Inte MOff                           '出力=0
311 '
312 'ねじ締め装置_出力アドレス
313 Def Inte MOUT_ScwT_ComChk               '通信確認
314 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
315 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
316 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
317 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
318 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
319 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
320 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
321 'ねじ締め装置_入力アドレス
322 Def Inte MIN_ScwT_comOK                 '通信確認返信
323 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
324 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
325 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
326 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
327 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
328 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
329 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
330 '
331 '
332 Def Inte MRetryLimit                    ' リトライ回数
333 Def Inte MRetryCount                    ' リトライカウント
334 '
335 Dim MScwT_Case1%(2)               '条件1停止変数
336 Dim MScwT_Case2%(2)               '条件2停止変数
337 Dim MScwT_Case3%(2)               '条件3停止変数
338 Dim MScwT_Case4%(2)               '条件4停止変数
339 Dim MScwT_Case5%(2)               '条件5停止変数
340 Def Pos PActive                     '直交座標系 位置変数 現在位置
341 Def Pos Pmove                       '直交座標系 位置変数 移動先
342 Def Inte MRecoveryPass              '復帰動作パスフラグ　1=復帰動作をパス　0=復帰動作を実行'
343 '共通
344 MTEST_KEY% = 11359                       'デバッグ用テストKEY
345 MOn% = 1                                 '出力 = 1
346 MOff% = 0                                '出力 = 0
347 '
348 'ねじ締め機_アドレス設定
349 MOUT_ScwT_ComChk% = 12816               '通信確認送信
350 MOUT_ScwT_ST% = 12849                   'ねじ締め開始を送信
351 MOUT_ScwT_ReSTOK% = 12850               '再開始受信を送信
352 MOUT_ScwT_FinOK% = 12852                'ねじ締め完了受信を送信
353 MOUT_ScwT_Case1OK% = 12858              '条件1停止受信を送信
354 MOUT_ScwT_Case2OK% = 12859              '条件2停止受信を送信
355 MOUT_ScwT_Case3OK% = 12860              '条件3停止受信を送信
356 MOUT_ScwT_Case4OK% = 12861              '条件4停止受信を送信
357 MOUT_ScwT_Case5OK% = 12862              '条件5停止受信を送信
358 '
359 MIN_ScwT_comOK% = 11824                 'ねじ締め装置から返信
360 MIN_ScwT_STRec% = 11857                 'ねじ締め開始を受信
361 MIN_ScwT_ReST% = 11858                  '再開始を受信
362 MIN_ScwT_Fin% = 11860                   'ねじ締め完了を受信
363 MIN_ScwT_Case1% = 11866                 '条件1停止待機を受信
364 MIN_ScwT_Case2% = 11867                 '条件2停止待機を受信
365 MIN_ScwT_Case3% = 11868                 '条件3停止待機を受信
366 MIN_ScwT_Case4% = 11869                 '条件4停止待機を受信
367 MIN_ScwT_Case5% = 11870                 '条件5停止待機を受信
368 '
369 MScwT_Case1%(1) = MIN_ScwT_Case1%
370 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
371 MScwT_Case2%(1) = MIN_ScwT_Case2%
372 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
373 MScwT_Case3%(1) = MIN_ScwT_Case3%
374 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
375 MScwT_Case4%(1) = MIN_ScwT_Case4%
376 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
377 MScwT_Case5%(1) = MIN_ScwT_Case5%
378 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
379 '
380 MRetryLimit% = 2
381 '
382 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
383 Function M% fnAssyStart
384     M_20# = MClear%                       '初期化
385     '組み立て開始
386     'プログラム原点
387     Ovrd 100
388     ' 初期位置をIDチケット上とするため削除 9/16 M.Hayakawa
389 '    Mov PInitialPosition        '原点回避
390     '初期位置を設定
391     PTemp = P_Curr
392     MRtn = 0
393     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
394         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
395             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
396                 MRtn = 1
397                 Break
398             EndIf
399             Break
400         EndIf
401         Break
402     EndIf
403     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
404     If MRtn = 1 Then
405         M_Out(12256) = 1 Dly 0.3    '位置決め出ON
406         Mov PTicketRead
407         Break
408     Else
409         Mov PInitialPosition
410         M_Out(12256) = 1 Dly 0.3    '位置決め出ON
411         Mov PTicketRead_1           'チケットID読み取り回避点
412         Mvs PTicketRead             'ID読み位置
413         Break
414     EndIf
415     *RE_PUSH
416 '    If M_20# = MContinue% Then M_Out(12257) = 0
417     If M_20# = MContinue% Then M_Out(12256) = 1 Dly 0.3
418     If M_20# = MContinue% Then M_20# = MClear%
419     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    '位置決め出端検出(8/26中村)
420     If MRtn = 1 Then GoTo *CompPush
421 '    M_Out(12256) = 0 Dly 0.3
422     M_Out(12257) = 1 Dly 0.3
423     fErrorProcess(11,231,282,0)
424     If M_20# = MNext% Then M_20# = MClear%
425     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
426     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
427     If M_20# = MContinue% Then GoTo *RE_PUSH
428     *CompPush
429     '
430     *RE_READ
431     If M_20# = MContinue% Then M_20# = MClear%
432 '
433     MRtn = 1                            'MRtn初期化
434     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
435         MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
436     EndIf
437         '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
438         '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
439 '
440     If MRtn = 1 Then GoTo *CompRead
441     Mvs PTicketRead_1                       ' 一旦上空に待避 追加 22/07/16 M,H
442     'fErrorProcess(11,97,25,0)
443     If M_20# = MPass% Then GoTo *AssyEnd    ' 次へを押された時のコメント解除＋ジャンプ先変更 2022/07/20 M.H
444     If M_20# = MNext% Then M_20# = MClear%
445     If M_20# = MAbout% Then GoTo *AssyEnd       ' コメント解除＋ジャンプ先変更 22/07/16 M,H
446     If M_20# = MNgProcess% Then GoTo *AssyEnd   ' コメント解除＋ジャンプ先変更 22/07/16 M,H
447     If M_20# = MContinue% Then GoTo *RE_READ
448 '    If M_20# = MNext% Then M_20# = MPass%
449     GoTo *ASSY_ERROR_END
450     *CompRead
451     '
452 '【MAIN基板ID読み込み】
453     *RE_MEIN_CHECK
454     PInspPosition(1) = PMainPcbRead 'MAIN基板読込位置
455     MInspGroup%(1) = 2              '検査G番号
456     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
457 '
458     If MRtn = 1 Then GoTo *CompMainCheck
459     fErrorProcess(11,38,25,0)
460     If M_20# = MNext% Then M_20# = MClear%
461     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
462     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
463     If M_20# = MContinue% Then GoTo *RE_MEIN_CHECK
464     *CompMainCheck
465 '【GYRO基板ID読み込み】
466     *RE_GYRO_CHECK
467     PInspPosition(1) = PGyroPcbRead 'GYRO基板読込位置
468     MInspGroup%(1) = 3              '検査G番号
469     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
470 '
471     If MRtn = 1 Then GoTo *CompGyroCheck
472     fErrorProcess(11,38,25,0)
473     If M_20# = MNext% Then M_20# = MClear%
474     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
475     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
476     If M_20# = MContinue% Then GoTo *RE_GYRO_CHECK
477     *CompGyroCheck
478 '【基板IDコピー】
479     *RE_PCB_RECORD
480     M_Out(12571) = 1    ' 領域1 基板番号コピー (D2600-) On
481     Dly 0.1
482     M_Out(12572) = 1    ' 領域2 基板番号コピー (D2612-) On
483     Dly 0.1
484     M_Out(12566) = 1    ' toPLC_基板番号コピー要求 On
485 '
486     MRtn = frInCheck(11581,1,MSETTIMEOUT05&)    ' toRBT_基板番号コピー完了 On
487     If MRtn = 1 Then
488         M_Out(12571) = 0  ' 領域1 基板番号コピー (D2600-) Off
489         Dly 0.1
490         M_Out(12572) = 0  ' 領域2 基板番号コピー (D2612-) Off
491         Dly 0.1
492         M_Out(12566) = 0  ' toPLC_基板番号コピー要求 Off
493 '        GoTo *RE_PCB_COMPAIRE   ' 基板番号照合にスキップ
494     Else
495         fErrorProcess(11,39,25,0)
496         If M_20# = MNext% Then M_20# = MClear%
497         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
498         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
499         If M_20# = MContinue% Then GoTo *RE_PCB_RECORD
500     EndIf
501 '【基板ID照合（紐付け）】
502     MRetryCount% = 0
503     While (MRetryCount% <= MRetryLimit%)
504         *RE_PCB_COMPAIRE
505         M_Out(12557)= 1 ' 基板番号照合ビットON
506         MRtn = frInCheck(11566,1,MSETTIMEOUT05&)    ' toRBT_基板番号照合OK(M420) On
507         If MRtn = 1 Then
508             M_Out(12557)= 0     ' 基板番号照合ビットOff
509             ' リトライ回数設定でループを抜ける
510             MRetryCount% = 99
511         Else
512             If MRetryCount% = MRetryLimit% Then
513                 If M_In(11565) = 1 Then
514                     fErrorProcess(11,37,25,0)
515                 Else
516                     fErrorProcess(11,38,25,0)
517                 EndIf
518                 If M_20# = MNext% Then
519                     M_20# = MClear%
520                     ' リトライ回数設定でループを抜ける
521                     MRetryCount% = 99
522                 EndIf
523                 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
524                 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
525                 If M_20# = MContinue% Then
526                     MRetryCount% = 0
527                 EndIf
528             Else
529                 ' リトライ回数インクリメント
530                 MRetryCount% = MRetryCount% + 1
531                 Dly 0.1  ' 他の工程とタイミングをずらす為のディレイ
532             EndIf
533         EndIf
534     WEnd
535 '
536     *RE_CHECK
537     PInspPosition(1) = PParts1Check '部品1画像チェック位置（本体1-1）
538     MInspGroup%(1) = 4              '検査G番号
539     PInspPosition(2) = PParts5Check '部品5画像チェック位置（本体1-2）
540     MInspGroup%(2) = 8              '検査G番号
541     PInspPosition(3) = PParts2Check '部品2画像チェック位置
542     MInspGroup%(3) = 5              '検査G番号
543     PInspPosition(4) = PParts3Check '部品3画像チェック位置
544     MInspGroup%(4) = 6              '検査G番号
545     PInspPosition(5) = PParts4Check '部品4画像チェック位置
546     MInspGroup%(5) = 7              '検査G番号
547     PInspPosition(6) = PParts6Check '部品6画像チェック位置(穴位置2画像保存用)
548     MInspGroup%(6) = 9              '検査G番号
549     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 6, -1, 1 )  '画像処理検査実行
550     If MRtn = 1 Then GoTo *CompCheck
551       ' 暫定削除
552     fErrorProcess(11,43,46,0)
553     If M_20# = MNext% Then M_20# = MClear%
554     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
555     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
556     If M_20# = MContinue% Then GoTo *RE_CHECK
557     *CompCheck
558     ' 部品検査追加 9/16 M.Hayakawa
559     ' 部品検査1
560 '    Mvs PParts1Check             '部品1検査
561     ' 部品検査2
562 '    Mvs PParts2Check             '部品2検査
563     ' 部品検査3
564 '    Mvs PParts3Check             '部品3検査
565     ' 部品検査4
566 '    Mvs PParts4Check             '部品4検査
567 '
568     'Main基板IDを読む
569 '    Mvs PMainPcbRead            'MAIN基板ID読取り
570     '
571     'GYRO基板IDを読む
572 '    Mvs PGyroPcbRead            'GYRO基板ID読み取り
573     '
574     '製品位置決め(ID読込後に変更 9/16 M.Hayakawa）
575     *RE_POS
576     If M_20# = MContinue% Then M_20# = MClear%
577     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
578     M_Out(12256)=1 Dly 0.3      '位置決めCY用SV出端パルス出力
579     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
580 '
581     'Wait M_In(11266)=1          '位置決め出端検出修正につきコメントアウト(8/26中村))
582     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    '位置決め出端検出(8/26中村)
583     If MRtn = 1 Then GoTo *Comp_Pos_1
584     fErrorProcess(11,231,282,0)
585     If M_20# = MNext% Then M_20# = MClear%
586     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
587     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
588     If M_20# = MContinue% Then GoTo *RE_POS
589     *Comp_Pos_1
590     '
591     M_Out(12258)=1 Dly 0.3      'プッシュCY用SV出端パルス出力(タクト短縮のため位置移動(12/13中村))
592     M_Out(12260)=1 Dly 0.3      'FANクランプ戻端パルス出力(タクト短縮のため位置移動(12/13中村))
593     Mov PScrewSupplyMain_1
594 '
595 '    M_Out(12258)=1 Dly 0.3      'プッシュCY用SV出端パルス出力
596     'Wait M_In(11268)=1          'プッシュ出端検出(修正につきコメントアウト(8/26中村))
597     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'プッシュ出端検出
598     If MRtn = 1 Then GoTo *Comp_Pos_2
599     fErrorProcess(11,231,282,0)
600     If M_20# = MNext% Then M_20# = MClear%
601     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
602     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
603     If M_20# = MContinue% Then GoTo *RE_POS
604     *Comp_Pos_2
605     '
606 '    M_Out(12260)=1 Dly 0.3      'FANクランプ戻端パルス出力(メインねじ締め後に変更 M.Hayakawa)(タクト短縮のため位置移動(12/13中村))
607     'Wait M_In(11270)=1          'FANクランプ戻端検出(修正につきコメントアウト(8/26中村))
608     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)    'FANクランプ戻端検出(8/26中村)
609     If MRtn = 1 Then GoTo *Comp_Pos_3
610     fErrorProcess(11,231,282,0)
611     If M_20# = MNext% Then M_20# = MClear%
612     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
613     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
614     If M_20# = MContinue% Then GoTo *RE_POS
615     *Comp_Pos_3
616     '
617     '
618     'Main基板のネジ締め
619     'Main基板用ネジ供給機へネジを取りに行く
620     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
621     '
622     '*ScrewSupplyMain           '一時コメントアウト(以下5行,8/5中村)
623 '    Mov PScrewSupplyMain_2      'ネジ供給機回避点
624 '    Mov PScrewSupplyMain_1      'ネジピックアップ上空
625 '    Mvs PScrewSupplyMain        'ネジピックアップ
626 '    Mvs PScrewSupplyMain_1      'ネジピックアップ上空
627 '    Mov PScrewSupplyMain_2      'ネジ供給機回避点
628     'Return                     '一時コメントアウト(8/4中村)
629     'ScrewPositionDebug_1()      'デバック用(別関数使用のためコメントアウト(8/26中村))
630     '
631     PGetScrewPosTemp(1) = PScrewSupplyMain_1   'ネジピックアップ上空を代入(8/26中村)
632     PGetScrewPosTemp(2) = PScrewSupplyMain_2   'ネジ供給回避点を代入(8/26中村)
633     PGetScrewPosTemp(9) = PScrewSupplyMain_9   'ネジ供給機上空ネジ捨て位置(10/6 M.H追加)
634     PGetScrewPosTemp(10) = PScrewSupplyMain    'ネジピックアップを代入(8/26中村)
635     *RE_SCREW_GET_1                                'リトライ用ラベル
636     If M_20# = MContinue% Then M_20# = MClear%
637     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
638     If M_20# = MClear% Then GoTo *Comp_Screw_1
639     If M_20# = MNext% Then M_20# = MClear%
640     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
641     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
642     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
643     *Comp_Screw_1
644     '
645     '①番ネジ締め
646 '    Mov PScrewMain1_1           '①上空(以下5行一時コメントアウト(8/26中村))
647 '    Ovrd 5
648 '    Mvs PScrewMain1             '①ネジ着座
649 '    Ovrd 10
650 '    Mvs PScrewMain1_1           '①上空
651     PScrewPosTemp(1) = PScrewMain1_1    'ネジ1締め開始位置上空を代入(8/26中村)
652     PScrewPosTemp(2) = PScrewMain1_0    'ネジ1締め開始位置を代入(8/26中村)
653     PScrewPosTemp(10) = PScrewMain1     'ネジ1締め終了位置を代入(8/26中村)
654     M_Out16(12672) = 1              'ネジ締め位置番号送信
655     MRtn = ScrewTight(PScrewPosTemp,1,10.0)    'ネジ1締めの実行(8/26中村) 4.9に変更20220205林
656     M_Out16(12672) = 0              'ネジ締め位置番号クリア
657     If MRtn = 1 Then GoTo *CompScrew1
658     Mov PInitialPosition
659     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
660     MScrewErrorCord% = MScrewErrorCord% + 1
661     fErrorProcess(11,MScrewErrorCord%,52,0)
662 '    fErrorProcess(11,53,52,0)
663     If M_20# = MNext% Then M_20# = MClear%
664     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
665     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
666     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
667     *CompScrew1
668     '
669     'Main基板用ネジ供給機へネジを取りに行く
670     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
671     'ScrewPositionDebug_1()      'デバック用(別関数使用のためコメントアウト(8/26中村))
672     *RE_SCREW_GET_2                                'リトライ用ラベル
673     If M_20# = MContinue% Then M_20# = MClear%
674     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
675     If M_20# = MClear% Then GoTo *Comp_Screw_2
676     If M_20# = MNext% Then M_20# = MClear%
677     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
678     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
679     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
680     *Comp_Screw_2
681     '②番ネジ締め
682 '    Mov PScrewMain2_1           '②上空(以下5行一時コメントアウト(8/26中村))
683 '    Ovrd 5
684 '    Mvs PScrewMain2             '②ネジ着座
685 '    Ovrd 10
686 '    Mvs PScrewMain2_1           '②上空
687     PScrewPosTemp(1) = PScrewMain2_1    'ネジ2締め開始位置上空を代入(8/26中村)
688     PScrewPosTemp(2) = PScrewMain2_0    'ネジ2締め開始位置を代入(8/26中村)
689     PScrewPosTemp(10) = PScrewMain2     'ネジ1締め終了位置を代入(8/26中村)
690     M_Out16(12672) = 2              'ネジ締め位置番号送信
691     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        'ネジ締め2の実行(8/26中村)
692     M_Out16(12672) = 0              'ネジ締め位置番号クリア
693     If MRtn = 1 Then GoTo *CompScrew2
694     Mov PInitialPosition
695     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
696     MScrewErrorCord% = MScrewErrorCord% + 2
697     fErrorProcess(11,MScrewErrorCord%,52,0)
698 '    fErrorProcess(11,54,52,0)
699     If M_20# = MNext% Then M_20# = MClear%
700     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
701     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
702     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
703     *CompScrew2
704     '
705     'Main基板用ネジ供給機へネジを取りに行く
706     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
707     *RE_SCREW_GET_3                                'リトライ用ラベル
708     If M_20# = MContinue% Then M_20# = MClear%
709     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
710     If M_20# = MClear% Then GoTo *Comp_Screw_3
711     If M_20# = MNext% Then M_20# = MClear%
712     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
713     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
714     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
715     *Comp_Screw_3
716     '③番ネジ締め
717 '    Mov PScrewMain3_1           '③上空(以下5行一時コメントアウト(8/26中村))
718 '    Ovrd 5
719 '    Mvs PScrewMain3             '③ネジ着座
720 '    Ovrd 10
721 '    Mvs PScrewMain3_1           '③上空
722     PScrewPosTemp(1) = PScrewMain3_1    'ネジ3締め開始位置上空を代入(8/26中村)
723     PScrewPosTemp(2) = PScrewMain3_0    'ネジ3締め開始位置を代入(8/26中村)
724     PScrewPosTemp(10) = PScrewMain3     'ネジ3締め終了位置を代入(8/26中村)
725     M_Out16(12672) = 3              'ネジ締め位置番号送信
726     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        'ネジ締め3の実行(8/26中村)
727     M_Out16(12672) = 0              'ネジ締め位置番号クリア
728     If MRtn = 1 Then GoTo *CompScrew3
729     Mov PInitialPosition
730     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
731     MScrewErrorCord% = MScrewErrorCord% + 3
732     fErrorProcess(11,MScrewErrorCord%,52,0)
733 '    fErrorProcess(11,55,52,0)
734     If M_20# = MNext% Then M_20# = MClear%
735     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
736     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
737     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
738     *CompScrew3
739 '
740     '
741     'Main基板用ネジ供給機へネジを取りに行く
742     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
743     *RE_SCREW_GET_4                                'リトライ用ラベル
744     If M_20# = MContinue% Then M_20# = MClear%
745     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
746     If M_20# = MClear% Then GoTo *Comp_Screw_4
747     If M_20# = MNext% Then M_20# = MClear%
748     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
749     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
750     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
751     *Comp_Screw_4
752     '④番ネジ締め
753 '    Mov PScrewMain4_1           '④上空(以下5行一時コメントアウト(8/26中村))
754 '    Ovrd 5
755 '    Mvs PScrewMain4             '④ネジ着座
756 '    Ovrd 10
757 '    Mvs PScrewMain4_1           '④上空
758     PScrewPosTemp(1) = PScrewMain4_1    'ネジ4締め開始位置上空を代入(8/26中村)
759     PScrewPosTemp(2) = PScrewMain4_0    'ネジ4締め開始位置を代入(8/26中村)
760     PScrewPosTemp(10) = PScrewMain4     'ネジ4締め終了位置を代入(8/26中村)
761     M_Out16(12672) = 4              'ネジ締め位置番号送信
762     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        'ネジ締め4の実行(8/26中村)
763     M_Out16(12672) = 0              'ネジ締め位置番号クリア
764     If MRtn = 1 Then GoTo *CompScrew4
765     Mov PInitialPosition
766     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
767     MScrewErrorCord% = MScrewErrorCord% + 4
768     fErrorProcess(11,MScrewErrorCord%,52,0)
769 '    fErrorProcess(11,56,52,0)
770     If M_20# = MNext% Then M_20# = MClear%
771     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
772     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
773     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
774     *CompScrew4
775     '
776     'Main基板用ネジ供給機へネジを取りに行く
777     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
778     *RE_SCREW_GET_5                                'リトライ用ラベル
779     If M_20# = MContinue% Then M_20# = MClear%
780     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
781     If M_20# = MClear% Then GoTo *Comp_Screw_5
782     If M_20# = MNext% Then M_20# = MClear%
783     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
784     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
785     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
786     *Comp_Screw_5
787     '⑤番ネジ締め
788 '    Mov PScrewMain5_1           '⑤上空(以下5行一時コメントアウト(8/26中村))
789 '    Ovrd 5
790 '    Mvs PScrewMain5             '⑤ネジ着座
791 '    Ovrd 10
792 '    Mvs PScrewMain5_1           '⑤上空
793     PScrewPosTemp(1) = PScrewMain5_1    'ネジ5締め開始位置上空を代入(8/26中村)
794     PScrewPosTemp(2) = PScrewMain5_0    'ネジ5締め開始位置を代入(8/26中村)
795     PScrewPosTemp(10) = PScrewMain5     'ネジ5締め終了位置を代入(8/26中村)
796     M_Out16(12672) = 5              'ネジ締め位置番号送信
797     MRtn = ScrewTight(PScrewPosTemp,6,10.0)        'ネジ締め5の実行(8/26中村)  バンク変更1→6　阿部　2/11
798     M_Out16(12672) = 0              'ネジ締め位置番号クリア
799     If MRtn = 1 Then GoTo *CompScrew5
800     Mov PInitialPosition
801     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
802     MScrewErrorCord% = MScrewErrorCord% + 5
803     fErrorProcess(11,MScrewErrorCord%,52,0)
804 '    fErrorProcess(11,57,52,0)
805     If M_20# = MNext% Then M_20# = MClear%
806     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
807     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
808     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
809     *CompScrew5
810     '
811     'Main基板用ネジ供給機へネジを取りに行く
812     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
813 '以下3行PP品にネジ穴がないため一時削除 9/16 M.Hayakawa
814     *RE_SCREW_GET_6                                'リトライ用ラベル
815     If M_20# = MContinue% Then M_20# = MClear%
816     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
817     If M_20# = MClear% Then GoTo *Comp_Screw_6
818     If M_20# = MNext% Then M_20# = MClear%
819     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
820     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
821     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
822     *Comp_Screw_6
823     '⑥番ネジ締め
824 '    Mov PScrewMain6_1           '⑥上空(以下5行一時コメントアウト(8/26中村))
825 '    Ovrd 5
826 '    Mvs PScrewMain6             '⑥ネジ着座
827 '    Ovrd 10
828 '    Mvs PScrewMain6_1           '⑥上空
829 '以下3行PP品にネジ穴がないため一時削除 9/16 M.Hayakawa
830     PScrewPosTemp(1) = PScrewMain6_1    'ネジ6締め開始位置上空を代入(8/26中村)
831     PScrewPosTemp(2) = PScrewMain6_0    'ネジ6締め開始位置を代入(8/26中村)
832     PScrewPosTemp(10) = PScrewMain6     'ネジ6締め終了位置を代入(8/26中村)
833     M_Out16(12672) = 6              'ネジ締め位置番号送信
834     MRtn = ScrewTight(PScrewPosTemp,6,10.0)        'ネジ締め6の実行(8/26中村)　　バンク変更1→6　阿部　2/11
835     M_Out16(12672) = 0              'ネジ締め位置番号クリア
836     If MRtn = 1 Then GoTo *CompScrew6
837     Mov PInitialPosition
838     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
839     MScrewErrorCord% = MScrewErrorCord% + 6
840     fErrorProcess(11,MScrewErrorCord%,52,0)
841 '    fErrorProcess(11,58,52,0)
842     If M_20# = MNext% Then M_20# = MClear%
843     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
844     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
845     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
846     *CompScrew6
847     '
848     'FAN用ネジ供給機へネジを取りに行く
849     'GoSub *ScrewSupplyFan      '一時コメントアウト(8/4中村)
850 ' ネジ位置指定前に取りにいっている？ 1行一時削除 9/16 M.Hayakawa
851 '    MRtn = ScrewGet(PGetScrewPosTemp)       'ネジを取りに行く(8/26中村)
852     '
853 '    *ScrewSupplyFan
854 '    Mov PScrewSupplyFan_2       'ネジ供給機回避点
855 '    Mov PScrewSupplyFan_1       'ネジピックアップ上空
856 '    Mvs PScrewSupplyFan         ''ネジピックアップ
857 '    Mvs PScrewSupplyFan_1       'ネジピックアップ上空
858 '    Mov PScrewSupplyFan_2       'ネジ供給機回避点
859    ' Return                     '一時コメントアウト(8/4中村)
860     'ScrewPositionDebug_2()       'デバック用(別関数使用のためコメントアウト(8/26中村))
861     '
862     PGetScrewPosTemp(1) = PScrewSupplyFan_1   'ネジピックアップ上空を代入(8/26中村)
863     PGetScrewPosTemp(2) = PScrewSupplyFan_2   'ネジ供給回避点を代入(8/26中村)
864     PGetScrewPosTemp(9) = PScrewSupplyFan_9   'ネジ供給機上空ネジ捨て位置(10/6 M.H追加)
865     PGetScrewPosTemp(10) = PScrewSupplyFan    'ネジピックアップを代入(8/26中村)
866     *RE_SCREW_GET_7                                'リトライ用ラベル
867     If M_20# = MContinue% Then M_20# = MClear%
868     ScrewGet(PGetScrewPosTemp , 11260 , 0)          'ネジ受け取り開始
869     If M_20# = MClear% Then GoTo *Comp_Screw_7
870     If M_20# = MNext% Then M_20# = MClear%
871     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
872     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
873     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_7
874     *Comp_Screw_7
875     '⑦番ネジ締め
876 '    Mov PScrewFan1_1            '⑦上空(以下5行一時コメントアウト(8/26中村))
877 '    Ovrd 5
878 '    Mvs PScrewFan1              '⑦ネジ着座
879 '    Ovrd 10
880 '    Mvs PScrewFan1_1            '⑦上空
881     PScrewPosTemp(1) = PScrewFan1_1    'Fan1ネジ締め開始位置上空を代入(8/26中村)
882     PScrewPosTemp(2) = PScrewFan1_0    'Fan1ネジ締め開始位置を代入(8/26中村)
883     PScrewPosTemp(10) = PScrewFan1     'Fan1ネジ締め終了位置を代入(8/26中村)
884     M_Out16(12672) = 7              'ネジ締め位置番号送信
885     MRtn = ScrewTight(PScrewPosTemp,2,6.7)       'Fanネジ締め1の実行(8/26中村)
886     M_Out16(12672) = 0              'ネジ締め位置番号クリア
887     If MRtn = 1 Then GoTo *CompScrew7
888     Mov PInitialPosition
889     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
890     MScrewErrorCord% = MScrewErrorCord% + 7
891     fErrorProcess(11,MScrewErrorCord%,52,0)
892 '    fErrorProcess(11,59,52,0)
893     If M_20# = MNext% Then M_20# = MClear%
894     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
895     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
896     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_7
897     *CompScrew7
898     '
899     'FAN用ネジ供給機へネジを取りに行く
900     'GoSub *ScrewSupplyFan      '一時コメントアウト(8/4中村)
901     'ScrewPositionDebug_2()      'デバック用(別関数使用のためコメントアウト(8/26中村))
902     *RE_SCREW_GET_8                                'リトライ用ラベル
903     If M_20# = MContinue% Then M_20# = MClear%
904     ScrewGet(PGetScrewPosTemp , 11260 , 0)          'ネジ受け取り開始
905     If M_20# = MClear% Then GoTo *Comp_Screw_8
906     If M_20# = MNext% Then M_20# = MClear%
907     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
908     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
909     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_8
910     *Comp_Screw_8
911     '⑧番ネジ締め
912 '    Mov PScrewFan2_1            '⑧上空(以下5行一時コメントアウト(8/26中村))
913 '    Ovrd 5
914 '    Mvs PScrewFan2              '⑧ネジ着座
915 '    Ovrd 10
916 '    Mvs PScrewFan2_1            '⑧上空
917     PScrewPosTemp(1) = PScrewFan2_1    'Fan2ネジ締め開始位置上空を代入(8/26中村)
918     PScrewPosTemp(2) = PScrewFan2_0    'Fan2ネジ締め開始位置を代入(8/26中村)
919     PScrewPosTemp(10) = PScrewFan2     'Fan2ネジ締め終了位置を代入(8/26中村)
920     M_Out16(12672) = 8              'ネジ締め位置番号送信
921     MRtn = ScrewTight(PScrewPosTemp,2,6.7)       'Fanネジ締め2の実行(8/26中村)
922     M_Out16(12672) = 0              'ネジ締め位置番号クリア
923     If MRtn = 1 Then GoTo *CompScrew8
924     Mov PInitialPosition
925     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
926     MScrewErrorCord% = MScrewErrorCord% + 8
927     fErrorProcess(11,MScrewErrorCord%,52,0)
928 '    fErrorProcess(11,60,52,0)
929     If M_20# = MNext% Then M_20# = MClear%
930     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
931     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
932     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_8
933     *CompScrew8
934 '
935     'プログラム原点
936     'Mov PInitialPosition        ' 原点回避
937     MRtn = FnCtlValue2(2)       '組立ＯＫ＋１  2022/04/28 渡辺
938     Mov PTicketRead_1           ' チケットリード位置
939     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
940     InitialState()              ' 初期状態にする
941     M_20# = MAssyOK%              ' 正常終了処理
942     GoTo *fnAssyStart_FEndPosi
943 '
944 *ASSY_ERROR_END
945     fnInitialZone()   ' 初期位置に移動
946 *AssyEnd
947     InitialState()  ' 初期状態にする
948 *fnAssyStart_FEndPosi
949     Exit Function
950 FEnd
951 '
952 '■fnPiasCheck
953 ''' <summary>
954 ''' PIASチケット読込み
955 ''' </summary>
956 ''' <returns>   0 : NG
957 '''             1 : OK(読込み完了)
958 ''' </returns>
959 ''' <remarks>
960 ''' Date   : 2021/07/07 : M.Hayakawa
961 ''' </remarks>'
962 Function M% fnPiasCheck
963     fnPiasCheck = 0
964     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
965     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功
966 '
967 *RETRY_PIAS
968     M_20# = MClear%
969     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
970     '
971     '【IDチケット読み込み】
972     PInspPosition(1) = PTicketRead  'IDチケット読取位置
973     MInspGroup%(1) = 1              '検査G番号
974     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
975 '
976     'エラーの場合
977     If MRtn <> 1 Then
978         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
979         If MRtn <> 1 Then
980             'D720 -> D1300 コピー要求
981             M_Out(12565) = 1
982             Dly 0.5
983             M_Out(12565) = 0
984             'エラー処理記述
985             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
986             'GOT KEY入力待ち
987             MKeyNumber = fnKEY_WAIT()
988             '
989             Select MKeyNumber
990                 Case MNext%         '次へを選択した場合
991                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
992                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
993                     GoTo *fnPiasCheck_End                   'PIASチェック終了
994                     Break
995                 Case MAbout%        '停止を選択した場合
996                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
997                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
998                     GoTo *fnPiasCheck_End                   'PIASチェック終了
999                     Break
1000                 Case MNgProcess%    'NGを選択した場合
1001                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1002                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1003                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1004                     Break
1005                 Case MContinue%     '継続を選択した場合
1006                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1007                     M_20# = MContinue%
1008                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1009                     Break
1010             End Select
1011         EndIf
1012     EndIf
1013     If M_20# = MPass% Then GoTo *fnPiasCheck_End            'PIASチェック終了
1014     If M_20# = MAbout% Then GoTo *fnPiasCheck_End           'PIASチェック終了
1015     If M_20# = MNgProcess% Then GoTo *fnPiasCheck_End       'PIASチェック終了
1016     If M_20# = MContinue% Then GoTo *RETRY_PIAS             'PIASチェックリトライ
1017 '
1018 '----------D720 -> D1300 コピー要求----------
1019     M_Out(12565) = 1
1020     Dly 0.5
1021     M_Out(12565) = 0
1022 '----------通信確認をする----------
1023     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1024     MRtn = 0                ' 初期化
1025     M_20# = MClear%         ' 初期化
1026     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1027     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1028 '    If MRtn <> 1 Then
1029 '        If M_20# = MContinue% Then
1030 '            GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1031 '        Else
1032 '            GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1033 '        EndIf
1034 '    EndIf
1035     If MRtn = 1 Then GoTo *PCComu_OK                '通信OK時ラベルへジャンプ
1036     If M_20# = MContinue% Then GoTo *RETRY_PIAS      'チケット読み直しからリトライ
1037     GoTo *fnPiasCheck_End                           'その他の場合PIASチェック終了
1038     *PCComu_OK
1039 '----------工程抜け確認----------
1040     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1041     MRtn = 0                ' 初期化
1042     M_20# = MClear%         ' 初期化
1043     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1044     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1045 '    If MRtn <> 1 Then
1046 '        If M_20# = MContinue% Then
1047 '            GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1048 '        Else
1049 '            GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1050 '        EndIf
1051 '    EndIf
1052     If MRtn = 1 Then GoTo *ProcessCheck_OK                '工程チェックOK時ラベルへジャンプ
1053     If M_20# = MContinue% Then GoTo *RETRY_PIAS      'チケット読み直しからリトライ
1054     GoTo *fnPiasCheck_End                           'その他の場合PIASチェック終了
1055     *ProcessCheck_OK
1056     '
1057     fnPiasCheck = 1
1058     *fnPiasCheck_End
1059     Exit Function
1060 FEnd
1061 '
1062 '■fnPCComuCheck
1063 ''' <summary>
1064 ''' PC-PLC通信チェック
1065 ''' </summary>
1066 ''' <returns>   0 : NG
1067 '''             1 : OK(読込み完了)
1068 ''' </returns>
1069 ''' <remarks>
1070 ''' Date   : 2021/07/07 : M.Hayakawa
1071 ''' </remarks>'
1072 Function M% fnPCComuCheck
1073     fnPCComuCheck = 0
1074     MJudge% = 0                                  '初期化
1075     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1076     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1077     '
1078     For MStaNo = 0 To 5
1079         '
1080         If M_In(MIN_PIAS_ComOK%) = 1 Then
1081             'PC通信OK(M400)
1082             MJudge% = MOK%
1083             MStaNo = 5
1084             Break
1085         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1086             'toRBT_通信確認time out
1087             MJudge% = MNG%
1088             MCommentD1001 = 15
1089             MCommentD1002 = 21
1090             MStaNo = 5
1091             Break
1092         Else
1093             'toRBT_通信確認time out
1094             MJudge% = MNG%
1095             MCommentD1001 = 14
1096             MCommentD1002 = 21
1097             Break
1098         EndIf
1099     Next MStaNo
1100     '
1101     '上記で返信フラグを受信してからPC通信確認OFF
1102     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1103     '
1104     'エラー画面
1105     If MJudge% <> MOK% Then
1106         M_20# = MClear%     '初期化
1107         'エラー処理記述
1108         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1109         'GOT KEY入力待ち
1110         MKeyNumber = fnKEY_WAIT()
1111         '
1112         If MKeyNumber = MAbout% Then            '停止を選択した場合
1113             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1114             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1115             Break
1116         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1117             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1118             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1119             Break
1120         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1121             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1122             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1123             Break
1124         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1125             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1126             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1127             Break
1128         EndIf
1129     Else
1130         'OKの場合
1131         fnPCComuCheck = 1
1132     EndIf
1133     Exit Function
1134 FEnd
1135 '
1136 '■fnProcessCheck
1137 ''' <summary>
1138 ''' 工程抜け確認
1139 ''' </summary>
1140 ''' <returns>    1：工程履歴OK     0：異常終了
1141 '''             -1：前工程履歴NG  -2：自工程履歴あり
1142 '''             -3：モデル仕向NG  -4：タイムアウト
1143 '''             -5：履歴処理エラー
1144 ''' </returns>
1145 ''' <remarks>
1146 ''' Date   : 2021/07/07 : M.Hayakawa
1147 ''' </remarks>'
1148 Function M% fnProcessCheck
1149     fnProcessCheck = 0
1150     MJudge% = MNG%      '一旦NGを初期化とする
1151 '----------工程抜け確認----------
1152     MCommentD1001 = 0   'コメント初期化
1153     For MStaNo = 0 To 5
1154         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1155         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1156         '
1157         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1158             MJudge% = MOK%
1159             fnAutoScreenComment(85)     ' AUTO画面
1160             MStaNo = 5
1161             Break
1162         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1163             MFlgLoop% = 0
1164             MJudge% = MNG%
1165             MCommentD1001 = 27
1166             MCommentD1002 = 22
1167             fnAutoScreenComment(94)     ' AUTO画面
1168             fnProcessCheck = -2         ' NGは-2を返す
1169             MStaNo = 5
1170             Break
1171         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1172            MJudge% = MNG%
1173             MCommentD1001 = 31
1174             MCommentD1002 = 22
1175             fnAutoScreenComment(83)     ' AUTO画面
1176             fnProcessCheck = -3         ' NGは-3を返す
1177             MStaNo = 5
1178             Break
1179         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1180             '履歴NGは直ぐに終了せず繰り返し確認を行う
1181             '前工程の書込みが終了していない可能性があるため
1182             MJudge% = MNG%
1183             MCommentD1001 = 32
1184             MCommentD1002 = 22
1185             fnAutoScreenComment(84)     ' AUTO画面
1186             fnProcessCheck = -1         ' NGは-1を返す
1187             Dly 1.0
1188             '工程抜け確認OFF
1189             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1190             Dly 1.0
1191            'MStaNo = 5
1192             Break
1193         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1194             MFlgLoop% = 0
1195             MJudge% = MNG%
1196             MCommentD1001 = 29
1197             MCommentD1002 = 22
1198             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1199             fnProcessCheck = -5         ' NGは-5を返す
1200             MStaNo = 5
1201             Break
1202         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1203             MJudge% = MNG%
1204             If MCommentD1001 = 32 Then
1205                 '何もしない
1206             Else
1207                 MCommentD1001 = 26
1208             EndIf
1209             MCommentD1002 = 22
1210             fnProcessCheck = -4         ' NGは-4を返す
1211             MStaNo = 5
1212             Break
1213         Else
1214             MJudge% = MNG%
1215             MCommentD1001 = 28
1216             MCommentD1002 = 22
1217         EndIf
1218     Next MStaNo
1219     '工程抜け確認OFF
1220     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1221     '通過履歴NG 工程抜けの場合
1222     If MJudge% = MPass% Then
1223         M_20# = MPass%
1224     EndIf
1225     '
1226     'エラー画面
1227     If MJudge% <> MOK% Then
1228         M_20# = MClear%     '初期化
1229         'エラー処理記述
1230         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1231         'GOT KEY入力待ち
1232         MKeyNumber = fnKEY_WAIT()
1233         '
1234         Select MKeyNumber
1235             Case MAbout%        '停止を選択した場合
1236                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1237                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1238                 Break
1239             Case MNext%         '次へを選択した場合
1240                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1241                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1242                 Break
1243             Case MContinue%     '継続を選択した場合
1244                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1245                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1246                 Break
1247             Case MNgProcess%    'NGを選択した場合
1248                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1249                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1250                 Break
1251         End Select
1252     Else
1253         fnProcessCheck = 1  ' OKは1を返す
1254     EndIf
1255     Exit Function
1256 FEnd
1257 '
1258 '■fnPiasWrite
1259 ''' <summary>
1260 ''' Pias 組立結果書込み要求
1261 ''' </summary>
1262 '''<param name="MFlg%">
1263 ''' MOK%(1) = 工程履歴にOKを書込む
1264 ''' MNG%(0) = 工程履歴にNGを書込む
1265 '''</param>
1266 '''<returns></returns>
1267 ''' <remarks>
1268 ''' Date   : 2021/07/07 : M.Hayakawa
1269 ''' </remarks>'
1270 Function M% fnPiasWrite(ByVal MFlg%)
1271       fnPiasWrite = 0
1272 *RETRY_PIASWRITE
1273     '
1274     '組立OK(MOK%)の場合　M306 ON
1275    '組立NG(MNG%)の場合　M307 ON
1276     If MFlg% = MOK% Then
1277         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1278     Else
1279         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1280     EndIf
1281     Dly 0.1                  '念のため
1282     '
1283     'Piasへ書込み開始 M305 -> ON
1284     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1285     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1286     '
1287     MJudge% = MNG%
1288     '
1289     For MStaNo = 0 To 5
1290         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1291             MJudge% = MOK%
1292             'MRet = fnAutoScreenComment(85)  'AUTO画面
1293             MStaNo = 5
1294             Break
1295         '
1296         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1297             MJudge% = MNG%
1298             'MRet = fnAutoScreenComment(85)  'AUTO画面
1299            MCommentD1001 = 34
1300            MCommentD1002 = 25
1301             MStaNo = 5
1302             Break
1303         '
1304         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1305             MJudge% = MNG%
1306             'MRet = fnAutoScreenComment(85)  'AUTO画面
1307            MCommentD1001 = 35
1308            MCommentD1002 = 25
1309             MStaNo = 5
1310             Break
1311         '
1312         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1313             MJudge% = MNG%
1314             'MRet = fnAutoScreenComment(85)  'AUTO画面
1315            MCommentD1001 = 36
1316            MCommentD1002 = 25
1317             MStaNo = 5
1318             Break
1319         '
1320         Else
1321             MJudge% = MNG%
1322            MCommentD1001 = 42
1323            MCommentD1002 = 25
1324         '
1325         EndIf
1326         '
1327     Next MStaNo
1328     '
1329     'Piasへ書込み開始 M305 -> OfF
1330     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1331     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1332     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1333     '
1334     '
1335     '通過履歴NG 工程抜けの場合
1336     If MJudge% = MPass% Then
1337         M_20# = MPass%
1338     EndIf
1339     '
1340    M_20# = MClear%     '初期化
1341     '
1342     'エラー画面
1343     If MJudge% < MOK% Then
1344     '
1345 '残しておくが現状では使用しないラベル
1346 *RETRY_ERR_WRITE
1347         M_20# = MClear%     '初期化
1348         'エラー処理記述
1349         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1350         'GOT KEY入力待ち
1351         MKeyNumber = fnKEY_WAIT()
1352         '
1353         If MKeyNumber = MAbout% Then   '停止を選択した場合
1354             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1355             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1356             Break
1357         '
1358         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1359             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1360             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1361             Break
1362         '
1363         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1364             M_20# = MPass%            'M_20# プログラム間共通外部変数
1365             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1366             Break
1367         '
1368         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1369             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1370             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1371             Break
1372         '
1373         EndIf
1374         '
1375 '        If M_20# = MClear% Then *RETRY_ERR_WRITE
1376         '
1377     EndIf
1378     '
1379     If M_20# = MContinue% Then *RETRY_PIASWRITE
1380     '
1381     fnPiasWrite = 1
1382     Exit Function
1383 FEnd
1384 '
1385 '■fnPCBNumberCheck
1386 ''' <summary>
1387 ''' Pias 基板番号照合要求
1388 ''' </summary>
1389 '''<returns>0（固定）</returns>
1390 ''' <remarks>
1391 ''' Date   : 2021/07/07 : M.Hayakawa
1392 ''' </remarks>'
1393 Function M% fnPCBNumberCheck
1394       fnPCBNumberCheck = 0
1395     '
1396 *RETRY_PCBCHECK
1397     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1398     'Piasへ基板照合開始 M310 -> ON
1399     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1400     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1401     '
1402     MJudge% = MNG%
1403     '
1404     For MStaNo = 0 To 5
1405         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1406             MJudge% = MOK%
1407             fnAutoScreenComment(96)  'AUTO画面
1408             MStaNo = 5
1409             Break
1410         '
1411         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1412             MJudge% = MNG%
1413             fnAutoScreenComment(97)  'AUTO画面
1414             MCommentD1001 = 37
1415             MCommentD1002 = 25
1416             MStaNo = 5
1417             Break
1418         '
1419         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1420             MJudge% = MNG%
1421             fnAutoScreenComment(98)  'AUTO画面
1422             MCommentD1001 = 38
1423             MCommentD1002 = 25
1424             MStaNo = 5
1425             Break
1426         '
1427         ElseIf M_In(11580) = 1 Then                         'time out
1428             MJudge% = MNG%
1429             fnAutoScreenComment(99)  'AUTO画面
1430             MCommentD1001 = 39
1431             MCommentD1002 = 25
1432             MStaNo = 5
1433             Break
1434         '
1435         Else
1436             MJudge% = MNG%
1437            MCommentD1001 = 41
1438            MCommentD1002 = 25
1439         '
1440         EndIf
1441         '
1442     Next MStaNo
1443     '
1444     'Piasへ基板照合開始 M310 -> OfF
1445     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1446     '
1447     '
1448     '通過履歴NG 工程抜けの場合
1449     If MJudge% = MPass% Then
1450         M_20# = MPass%
1451     EndIf
1452     '
1453    M_20# = MClear%     '初期化
1454     '
1455     'エラー画面
1456     If MJudge% < MOK% Then
1457     '
1458 '残しておくが現状では使用しないラベル
1459 *RETRY_ERR_PCBNUMBER
1460         M_20# = MClear%     '初期化
1461         'エラー処理記述
1462         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1463         'GOT KEY入力待ち
1464         MKeyNumber = fnKEY_WAIT()
1465         '
1466         If MKeyNumber = MAbout% Then   '停止を選択した場合
1467             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1468             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1469             Break
1470         '
1471         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1472             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1473             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1474         '
1475         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1476             M_20# = MPass%            'M_20# プログラム間共通外部変数
1477             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1478         '
1479         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1480             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1481             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1482             Break
1483         '
1484         EndIf
1485         '
1486 '        If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1487         '
1488     EndIf
1489     '
1490     If M_20# = MContinue% Then *RETRY_PCBCHECK
1491     Exit Function
1492 FEnd
1493 '
1494 '■ScrewTight
1495 ''' <summary>
1496 ''' ねじ締めを行う(Sタイト)
1497 ''' </summary>
1498 '''<param name="PScrewPos()">
1499 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
1500 '''             PScrewPos(2)    ：ねじ締め回避点
1501 '''             PScrewPos(10)   ：ねじ締め終了高さ
1502 '''<param name="MScrewType">ネジタイプ(mm/sec)
1503 '''             1:6mm Sタイト銀ネジ
1504 '''             2:8mm Pタイト
1505 '''             3:6mm Sタイト黒ネジ
1506 '''             4:13mm Sタイト
1507 '''             5:6mm Mネジ
1508 '''</param>
1509 '''<param name="MFeedSpd">送り速度(mm/sec)</param>
1510 '''<returns>整数
1511 '''         0=異常終了、1=正常終了
1512 '''</returns>
1513 ''' <remarks>
1514 ''' Date   : 2021/07/07 : M.Hayakawa
1515 ''' Update : 2021/09/28 : M.Hayakawa ネジタイプ、送り速度を引数に追加
1516 ''' </remarks>'
1517 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   'ネジ締め個別設定
1518     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
1519     ScrewTight = 0
1520     MOKNGFlg = 0
1521     Ovrd 100
1522     Mov PScrewPosition(1)       ' パレット上ねじ締めS①の安全回避位置
1523     Fine 0.05 , P
1524     Ovrd MOvrdA%
1525     ' 減速設定
1526     Accel 100, 10
1527     ' パレット上ねじ締め開始位置へ移動
1528     Mvs PScrewPosition(2)
1529     ' 加減速を元に戻す
1530     Accel
1531     ' 内部Ovrd設定
1532 '    Ovrd MOvrdA%
1533     Ovrd 100
1534     ' Spd設定
1535 '    Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
1536     Spd MFeedSpd
1537     ' 設定進み量5.0 × 操作パネルのオーバーライド係数 × プログラム内オーバーライド係数
1538     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
1539     Select MScrewType%
1540         Case 1
1541             ' Sタイト：プログラム1、バンク1に設定
1542             ProgramBankSet(1,1)
1543             Break
1544         Case 2
1545             ' Pタイト：プログラム1、バンク1に設定
1546             ProgramBankSet(3,1)
1547             Break
1548         Case 3
1549             ' Sタイト黒：プログラム1、バンク1に設定
1550             ProgramBankSet(1,1)
1551             Break
1552         Case 4
1553             ' Sタイト13mm：プログラム1、バンク1に設定
1554             ProgramBankSet(1,1)
1555             Break
1556         Case 5
1557             ' Mネジ：プログラム1、バンク1に設定
1558             ProgramBankSet(1,1)
1559             Break
1560         Case 6
1561             ' Sタイト：プログラム1、バンク4に設定
1562             ProgramBankSet(1,4)
1563             Break
1564         Default
1565             ' プログラム1、バンクなし設定
1566             ProgramBankSet(0,0)
1567             Break
1568     End Select
1569 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     'ドライバーON　CW
1570      'ドライバーON　CW
1571     M_Out(12241)=1
1572     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1573     Wait M_In(11584)=1          '完了/エラー検出 暫定コメント 10/6 M.H
1574     Dly 0.1
1575     Fine 0 , P
1576     Spd M_NSpd
1577     '
1578     If M_In(11256)=1 Then  'ねじトータルエラー検出時
1579         M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1580         Dly 0.1
1581        ' プログラム・バンク解除
1582         ProgramBankSet(0,0)
1583         'パレット上ねじ締め終了位置上空へ移動
1584         Mvs PScrewPosition(10),-80
1585         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1586         M_Out(12249)=1 Dly 0.3
1587         MOKNGFlg = -1
1588         ScrewTight = 0
1589     Else
1590          'ドライバーOFF　CW
1591         M_Out(12241)=0
1592 '        エラーがない場合はネジ締め終了位置で増し締め
1593 '        Select MScrewType%
1594 '            Case 1
1595 '                ' Sタイト：プログラム1、バンク3に設定
1596 '                ProgramBankSet(1,3)
1597 '                Break
1598 '            Case 2
1599 '                ' Pタイト：プログラム1、バンク3に設定
1600 '                ProgramBankSet(3,3)
1601 '                Break
1602 '            Case 3
1603 '                ' Sタイト黒：プログラム1、バンク3に設定
1604 '                ProgramBankSet(1,3)
1605 '                Break
1606 '            Case 4
1607 '                ' Sタイト13mm：プログラム1、バンク3に設定
1608 '                ProgramBankSet(1,3)
1609 '                Break
1610 '            Case 5
1611 '                ' Mネジ：プログラム1、バンク3に設定
1612 '                ProgramBankSet(1,3)
1613 '                Break
1614 '            Default
1615 '                ' プログラム1、バンクなし設定
1616 '                ProgramBankSet(0,0)
1617 '                Break
1618 '        End Select
1619 '         'ドライバーON　CW
1620 '        Mvs PScrewPosition(10)
1621 '        M_Out(12241)=1
1622 '        Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1623 '        Fine 0 , P
1624 '
1625          'ドライバーOFF　CW
1626         M_Out(12241)=0
1627        ' プログラム・バンク解除
1628         ProgramBankSet(0,0)
1629         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1630         M_Out(12249)=1 Dly 0.3
1631     '     ↓PScrewPos(2) → PScrewPosition(10)に変更 9/16 M.Hayakawa
1632         'パレット上ねじ締め終了位置上空へ移動
1633        Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1634         'Mvs PScrewPosition(10),-80
1635         ScrewTight = 1
1636     EndIf
1637 ' 暫定（暫定マスク　9/16 M.Hayakawa)
1638 '    Ovrd 10
1639 '    Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1640     Ovrd 100
1641     Exit Function
1642 FEnd
1643 '
1644 '■ScrewGet
1645 ''' <summary>
1646 ''' ねじ供給機からねじを得る
1647 ''' </summary>
1648 '''<param name="%">
1649 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
1650 '''         PScrewPos(2)    ：ねじ供給器回避点
1651 '''         PScrewPos(9)    ：ねじ供給器上空ネジ捨位置
1652 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1653 '''         PScrewPos(3)    ：Mねじポカヨケ位置
1654 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
1655 '''</param>
1656 '''<param name = FeederReadyNo%> 指定の供給機Ready </param>
1657 '''<param name = FeederScrewSensor%> 指定の誤供給防止センサー指定(0でセンサー無し)</param>
1658 '''<returns>整数
1659 '''         0=異常終了、1=正常終了、-1=ねじ供給NG、-2=ねじ誤供給NG、-3=吸着エラー
1660 '''</returns>
1661 ''' <remarks>
1662 ''' Date   : 2021/07/07 : M.Hayakawa
1663 ''' </remarks>
1664 '''<update>
1665 '''Date    : 2021/11/15 : 中村
1666 '''Date    : 2021/02/07 : 早川 念のため確認を削除
1667 '''</update>
1668 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
1669     fnAutoScreenComment(522)    '状態表示[ネジ供給待ち] 2022/05/09 渡辺
1670     ScrewGet = 0
1671     MScrewJudge% = 0
1672     'ねじ供給器初期動作エラーチェック
1673 ' ↓暫定削除
1674     'Mov PScrewPosition(2)   'ねじ供給機回避点へ移動
1675     For MCnt% = 0 To MFinCnt%
1676         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '指定ねじ供給機がReadyになっているか確認(5秒間)
1677         If MRtn = 0 Then
1678             M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
1679             ScrewGet = -1
1680             MScrewJudge% = 2
1681         EndIf
1682         Ovrd 100
1683         If FeederScrewSensor% <> 0 Then
1684             If M_In(FeederScrewSensor%) = 1 Then  '誤供給が検出されたら
1685                 'Ovrd 30
1686                 M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
1687                 'NGとしてここの関数から抜ける
1688                 ScrewGet = -2
1689                 MScrewJudge% = 3
1690             EndIf
1691         EndIf
1692         Ovrd 100
1693         Spd M_NSpd
1694         If MScrewJudge% = 0 Then
1695     '        ScrewGet = 0
1696             M_Out(Y63_Driver)=1         ' バンクセッティング　C2
1697             Dly 0.3
1698             MScrewCnt% = 0
1699             MFinCnt% = 2
1700             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
1701             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1702             '
1703             '
1704             'Ovrd 40 '2に変更 10/6 M.H '5に変更10/7中村
1705             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1706             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
1707             'Mvs PScrewPosition(10), 1.2
1708            Mvs PScrewPosition(10)       'Fan用ねじ吸着位置修正のため変更 2022-02-01AJI
1709             'ビット回転(シーケンス見直しにつき処理位置変更3/30中村)
1710             M_Out(Y60_Driver)=1
1711             'ビット回転安定状態監視開始
1712             M_Timer(4) = 0
1713             MloopFlg = 0
1714             MCrtTime& = 0
1715            'ビット回転安定まで待機
1716             While MloopFlg = 0
1717                 MCrtTime& = M_Timer(4)
1718                 If MCrtTime& >= 180 Then
1719                     MloopFlg = 1
1720                 EndIf
1721             WEnd
1722             '
1723            M_Out(Y68_VV1)=1 Dly 0.3     ' ねじ吸着　ON'Dly 0.3追加(8/27中村)
1724             'ビット回転(シーケンス見直しにつき処理位置変更3/30中村)
1725 '            M_Out(Y60_Driver)=1
1726 '            Dly 0.2
1727             '吸着位置にて吸着確認
1728             MRtn = 0
1729             MRtn = frInCheck(11264, 1, MSETTIMEOUT01&)      'チェックはするがエラー判定はしない
1730             '
1731             JOvrd M_NJovrd
1732             Spd M_NSpd
1733             'ネジ吸着確認位置移動
1734             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
1735             Mvs PScrewPosition(10), -30  ' ネジ吸着確認位置
1736            'Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1737             'ビット回転停止
1738             M_Out(Y60_Driver)=0
1739             '
1740 '            If MRtn = 1 Then       '最初の閾値チェックを行わない
1741                 '1秒間ネジ吸着確認 始めの閾値
1742                 MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1743 '            EndIf
1744             'MRtn = 0'強制エラー
1745             '吸着エラーの場合
1746             'ネジをねじ太郎に戻す
1747             If MRtn = 0 Then
1748                 Ovrd 30      '2から5に変更'5から30に変更(3/30中村)
1749                 'ビット回転停止
1750                 M_Out(Y60_Driver)=0
1751                 'ネジ供給機上空
1752                 Mvs PScrewPosition(1)
1753                 '更に上空
1754                 Mov PScrewPosition(1), -140
1755                 'ネジ捨て位置
1756                 If FeederReadyNo% = 11260 Then     '供給機別に吸着エラー数をカウント　2022/05/19 渡辺
1757                     MRtn = FnCtlValue2(3)          '供給機２吸着エラー数＋１
1758                 Else
1759                     MRtn = FnCtlValue2(4)          '供給機１吸着エラー数＋１  2022/04/28 渡辺
1760                 EndIf
1761                 Mov PScrewPosition(9)
1762                 MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1763                 '吸着OFF
1764                 M_Out(12249)=1 Dly 0.3 'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1765                 Dly 0.2
1766                 '破壊ON
1767                 M_Out(Y6B_VB1)=1 '真空破壊ON
1768                 'ビット回転
1769                 M_Out(Y61_Driver)=1
1770                 Dly 0.5
1771                 '                '
1772                 Ovrd 100
1773                 JOvrd M_NJovrd
1774                 Spd M_NSpd
1775                 'ドライバーを上下させねじを振り落とす
1776                 Mov PScrewPosition(9), 10
1777                 Mov PScrewPosition(9)
1778                 Dly 0.1
1779                 Mov PScrewPosition(9), 10
1780                 Mov PScrewPosition(9)
1781                 '
1782                 'ネジ落ち待ち
1783                 Wait M_In(11265) = 0
1784                 'ビット回転停止
1785                 M_Out(Y61_Driver)=0
1786                 Dly 0.1
1787                 '破壊OFF
1788                 M_Out(Y6B_VB1)=0 '真空破壊OFF
1789                 'ねじ落ちたとして、移動更に上空
1790                 Mov PScrewPosition(1), -140
1791                 Ovrd 100
1792                 Spd M_NSpd
1793                 'ネジ供給機上空
1794                 Mvs PScrewPosition(1)
1795 '                '
1796                 ScrewGet = -3
1797                 If MCnt% = MFinCnt% Then
1798                     MScrewJudge% = 4
1799                     Mov PScrewPosition(2)
1800                     Break
1801                 EndIf
1802                 Break
1803 '                '
1804             Else
1805                 MCnt% = MFinCnt%
1806                 ScrewGet = 1
1807             EndIf
1808         Else
1809             MCnt% =MFinCnt%
1810         EndIf
1811     Next  MCnt%
1812         '
1813 '    If MScrewJudge% = 0 Then
1814 '        Ovrd 100
1815 '        Spd M_NSpd
1816 '        PScrewPosition(1)
1817 '        Mvs PScrewPosition(10), -30  ' ねじピックアップ位置 -30mm
1818 '        'Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1819 '        M_Out(Y60_Driver)=0     ' ビット回転停止
1820 '        M_Out(Y63_Driver)=0     ' バンクセッティング　C2
1821 '        'Mvs PScrewPosition(10), -30  ' ねじピックアップ位置 -30mm
1822 '        Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1823 '        'Mov PScrewPosition(2)
1824 '        'もう一度吸着確認　上空の最終閾値
1825 '        MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1826 '        If MRtn = 0 Then      '吸着エラーの場合
1827 '            MScrewJudge% = 4
1828 '            ScrewGet = -3
1829 '        ElseIf MRtn = 1 Then      '吸着OKの場合
1830 '            MScrewJudge% = 1
1831 '            ScrewGet = 1
1832 '        EndIf
1833 '        Break
1834 '    EndIf
1835     '
1836 '    If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
1837     If MScrewJudge% = 0 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
1838     '
1839     Select MScrewJudge%
1840 '        Case 0
1841 ''            fErrorProcess(11,162,163,0) '異常終了
1842 '            MCommentD1001 = 162
1843 '            MCommentD1002 = 96
1844 '            Break
1845         Case 2
1846 '            fErrorProcess(11,63,161,0) '供給NG
1847             MCommentD1001 = 63
1848             MCommentD1002 = 96
1849             Break
1850         Case 3
1851 '            fErrorProcess(11,160,164,0) '誤供給
1852             MCommentD1001 = 237
1853             MCommentD1002 = 96
1854             Break
1855         Case 4
1856 '            fErrorProcess(11,94,95,0) '吸着NG
1857             MCommentD1001 = 94
1858             MCommentD1002 = 95
1859             Break
1860     End Select
1861     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
1862     '
1863     Select M_20#
1864         Case MAbout%          '停止が押された場合
1865             Mov PScrewPosition(2)                  '初期位置に戻って停止処理
1866             Mov PInitialPosition
1867             Break
1868         Case MContinue%       'リトライが押されていた場合(関数を抜けた先で処理)
1869             Break
1870         Case MNext%           '継続が押された場合
1871             M_20# = MClear%     '初期化
1872             Break
1873         Case MNgProcess%      'NGが押された場合
1874             Mov PScrewPosition(2)   'PIASにNG書き込みを行い,初期位置に戻って行程終了
1875             Mov PInitialPosition
1876             Break
1877         End Select
1878 *End_ScrewGet
1879     Exit Function
1880 FEnd
1881 '
1882 '■ProgramBankSet
1883 ''' <summary>
1884 ''' ねじ締めを行う(Pタイト)
1885 ''' </summary>
1886 '''<param name="MProgramNo">プログラム番号</param>
1887 '''<param name="MBankNo">バンク番号</param>
1888 '''</returns>
1889 ''' <remarks>
1890 ''' Date   : 2021/10/05 : M.Hayakawa
1891 ''' </remarks>'
1892 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
1893 '
1894     MLocalPrgNo% = (MProgramNo% - 1) * 32
1895     MLocalBankNo% = MBankNo% * 4
1896 '
1897     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
1898         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
1899     Else
1900         MLocalOutNo% = 0
1901     EndIf
1902 '
1903     M_Out8(12240) = MLocalOutNo%
1904     Dly 0.1
1905     Exit Function
1906 FEnd
1907 '
1908 '■fnKEY_WAIT()
1909 ''' <summary>
1910 ''' GOTからのキー入力待ち
1911 ''' </summary>
1912 '''<returns>1：停止    2：次へ
1913 '''         3：継続    4：トルクチェック開始
1914 '''         5：NG
1915 '''         11：ロボット初期位置1    12：ロボット初期位置2
1916 '''         13：ロボット初期位置3    14：ロボット初期位置4
1917 '''</returns>
1918 ''' <remarks>
1919 ''' Date   : 2021/07/07 : M.Hayakawa
1920 ''' </remarks>'
1921 Function M% fnKEY_WAIT()
1922     fnKEY_WAIT = 0
1923     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
1924     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
1925     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
1926     '下記キー待ちの継続に反応させないため
1927     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
1928     Dly 0.2
1929     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
1930     MLocalLoopFlg=1
1931     While MLocalLoopFlg=1
1932         If M_In(11345) = 1 Then         '停止   M5345
1933             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
1934             fnKEY_WAIT = 1
1935             MLocalLoopFlg=-1
1936             Break
1937         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
1938             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
1939             fnKEY_WAIT = 2
1940             MLocalLoopFlg=-1
1941             Break
1942         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
1943             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
1944             fnKEY_WAIT = 3
1945             MLocalLoopFlg=-1
1946             Break
1947         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
1948             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
1949             fnKEY_WAIT = 4
1950             MLocalLoopFlg=-1
1951             Break
1952         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
1953             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
1954             fnKEY_WAIT = 5
1955             MLocalLoopFlg=-1
1956             Break
1957         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
1958             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
1959             fnKEY_WAIT = MRobotInit1%
1960             MLocalLoopFlg=-1
1961             Break
1962         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
1963             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
1964             fnKEY_WAIT = MRobotInit2%
1965             MLocalLoopFlg=-1
1966             Break
1967         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
1968             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
1969             fnKEY_WAIT = MRobotInit3%
1970             MLocalLoopFlg=-1
1971             Break
1972         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
1973             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
1974             fnKEY_WAIT = MRobotInit4%
1975             MLocalLoopFlg=-1
1976             Break
1977         Else
1978         EndIf
1979     WEnd
1980     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
1981     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
1982     Exit Function
1983 FEnd
1984 '
1985 '■ fnAUTO_CTL
1986 ''' <summary>
1987 ''' AUTOモードOFF、PLCからの開始待ち
1988 ''' </summary>
1989 ''' <remarks>
1990 ''' Date   : 2021/07/07 : M.Hayakawa
1991 ''' </remarks>
1992 Function M% fnAUTO_CTL
1993     fnAUTO_CTL = 0
1994     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
1995     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
1996     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
1997     '
1998     If M_Svo=0 Then             'サーボON確認
1999         Servo On
2000     EndIf
2001     Wait M_Svo=1
2002     Exit Function
2003 FEnd
2004 '
2005 '■ fnWindScreenOpen
2006 ''' <summary>
2007 ''' ウィンド画面の表示、非表示設定
2008 ''' </summary>
2009 '''<param name="%"></param>
2010 '''<param name="%"></param>
2011 '''<param name="%"></param>
2012 '''<param name="%"></param>
2013 ''' <remarks>
2014 ''' コメントD1001, D1002, D1003の設定
2015 ''' MWindReSet = 0     画面非表示
2016 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2017 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2018 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2019 ''' Date   : 2021/07/07 : M.Hayakawa
2020 ''' </remarks>
2021 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2022     If MCommentD1001 <> 0 Then              'コメント 0 は設定がないので確認
2023         M_Out16(12480) = MCommentD1001      'D1001 コメント
2024     EndIf
2025     '
2026     If MCommentD1002 <> 0 Then              'コメント 0 は設定がないので確認
2027         M_Out16(12496) = MCommentD1002      'D1002 コメント
2028     EndIf
2029     '
2030     If MCommentD1003 <> 0 Then              'コメント 0 は設定がないので確認
2031        M_Out16(12512) = MCommentD1003       'D1003 コメント
2032     EndIf
2033     '
2034     M_Out16(12448) = MScreenNo              '画面番号  M6448   10=エラー画面
2035     M_Out(12363) = 1 Dly 0.5                'ウィンド画面設定  M6362
2036     Exit Function
2037 FEnd
2038 '
2039 '■FnCtlValue2
2040 ''' <summary>
2041 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2042 ''' </summary>
2043 ''' <param name="MCtlNo%"></param>
2044 ''' <remarks>
2045 ''' Date : 2022/04/28 渡辺
2046 ''' </remarks>
2047 '''
2048 '''  1：投入数       ＋１
2049 '''  2：組立ＯＫ数   ＋１
2050 '''  3：供給機２吸着エラー数 ＋１　　組立NGから変更 2022/05/19 渡辺
2051 '''  4：供給機１吸着エラー数 ＋１
2052 ''' 99：読書開始信号 OFF
2053 '''
2054 Function M% FnCtlValue2(ByVal MCtlNo%)
2055     FnCtlValue2 = 1
2056     Select MCtlNo%
2057         Case 1        '投入数＋１
2058             M_Out(12569) = 0             '書込み開始信号OFF
2059             M_Out(12568) = 1             '読込み開始信号ON
2060             MInputQty = M_In16(11600)    '投入数受信
2061             MInputQty = MInputQty + 1    '投入数＋１
2062             M_Out16(12592) = MInputQty   '投入数送信
2063             M_Out(12569) = 1             '書込み開始信号ON
2064             Break
2065             '
2066         Case 2        '組立ＯＫ数＋１
2067             M_Out(12569) = 0             '書込み開始信号OFF
2068             M_Out(12568) = 1             '読込み開始信号ON
2069             MAssyOkQty = M_In16(11616)   '組立OK数受信
2070             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2071             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2072             M_Out(12569) = 1             '書込み開始信号ON
2073             Break
2074             '
2075         Case 3        '供給機２吸着エラー数＋１
2076             M_Out(12569) = 0                       '書込み開始信号OFF
2077             M_Out(12568) = 1                       '読込み開始信号ON
2078             MSuctionErrQty = M_In16(11632)         '供給機２吸着エラー数受信
2079             MSuctionErrQty = MSuctionErrQty + 1    '供給機２吸着エラー数＋１
2080             M_Out16(12624) = MSuctionErrQty        '供給機２吸着エラー数送信
2081             M_Out(12569) = 1                       '書込み開始信号ON
2082             Break
2083             '
2084         Case 4        '供給機１吸着エラー数＋１
2085             M_Out(12569) = 0                       '書込み開始信号OFF
2086             M_Out(12568) = 1                       '読込み開始信号ON
2087             MSuctionErrQty = M_In16(11648)         '供給機１吸着エラー数受信
2088             MSuctionErrQty = MSuctionErrQty + 1    '供給機１吸着エラー数＋１
2089             M_Out16(12640) = MSuctionErrQty        '供給機１吸着エラー数送信
2090             M_Out(12569) = 1                       '書込み開始信号ON
2091             Break
2092             '
2093         Case 99        '読書開始信号OFF
2094             M_Out(12568) = 0        '読込み開始信号OFF
2095             M_Out(12569) = 0        '書込み開始信号OFF
2096             Break
2097             '
2098     End Select
2099     Exit Function
2100 FEnd
2101 '
2102 '
2103 '■FnScreEroorCord
2104 ''' 電動ドライバーのエラーコードを含めたコメントを出す為のコメント番号の作成
2105 ''' 新規作成：2022/05/23 : 渡辺
2106 '''
2107 Function M% FnScreEroorCord()
2108     MScrewErrorCord% = 0
2109     If M_In(11252) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 1    '11252:E_driver Error Massage1 E1
2110     If M_In(11253) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 2    '11253:E_driver Error Massage1 E2
2111     If M_In(11254) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 4    '11254:E_driver Error Massage1 E3
2112     If M_In(11255) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 8    '11255:E_driver Error Massage1 E4
2113     If M_In(11258) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 16   '11258:E_driver Error Massage1 E5
2114     MScrewErrorCord% = MScrewErrorCord% * 10
2115     MScrewErrorCord% = MScrewErrorCord% + 500
2116     FnScreEroorCord = MScrewErrorCord%
2117     Exit Function
2118 FEnd
2119 '
2120 '
2121 'Insightによる画像処理検査実行（並列処理なし）
2122 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2123 '-------------------------------------------------------------------------------
2124 'Insightによる画像処理検査実行（並列処理なし）
2125 '   引数
2126 '       PInspPos()      ：検査位置
2127 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2128 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2129 '       MInspCnt%       ：検査位置数
2130 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2131 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2132 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2133 '   戻り値：整数
2134 '       0=異常終了、1=正常終了
2135 '
2136 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2137 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2138 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2139 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2140 '   20200410    :   検査グループ設定Retry追加
2141 '-------------------------------------------------------------------------------
2142     '----- 初期設定 -----
2143     Cnt 0                                                           '移動効率化解除(初期値=0)
2144     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2145 '    Cnt 1,0.1,0.1
2146     '変数宣言・初期化
2147     Def Inte MNum                                                   '検査番号(検査順1～)
2148     MNum% = 1                                                       '検査番号初期値設定
2149     Def Inte MEndFlg                                                '検査終了フラグ
2150     MEndFlg% = 0
2151     '
2152     '検査G番号設定要求・検査実行要求off
2153     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2154     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2155     'エラー番号クリア
2156     MInspErrNum = 0                                                 '検査実行エラー番号
2157     M_Out16(MOUT_InspErrNum) = MInspErrNum
2158     MInspNGStepNum = 0                                              '検査実行NGStep番号
2159     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2160     '
2161     'Insight Ready check?
2162     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2163         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2164         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2165         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2166         ISInspectionSingle = 0                                      '異常終了戻り値設定
2167         'Exit Function
2168     EndIf
2169     If MInspErrNum = 20 Then GoTo *ISInspectionSingle_End
2170     '
2171     '検査位置数確認
2172     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2173         MInspErrNum = 21                                            '検査データなし 21　引数<1
2174         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2175         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2176         ISInspectionSingle = 0                                      '異常終了戻り値設定
2177         'Exit Function
2178     EndIf
2179    If MInspErrNum = 21 Then GoTo *ISInspectionSingle_End
2180     '
2181     '
2182     '
2183     '----- メイン処理 -----
2184     '設定された検査位置数分の検査実行
2185     While( MEndFlg% = 0 )
2186         '----- 検査グループ番号設定Retry追加 20200410
2187         MSetGrNumRetryExitFlg = 0
2188         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2189         While( MSetGrNumRetryExitFlg = 0 )
2190         '----- 検査グループ番号設定Retry追加ここまで 20200410
2191             '
2192             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2193             '
2194             '----- 検査グループ番号設定 -----
2195             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2196             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2197             '
2198             '検査位置へ移動・移動完了待ち
2199             fnAutoScreenComment(521)                                    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2200             Mov PInspPos( MNum% )                                       '移動
2201             fnAutoScreenComment(523)                                    '状態表示[画像処理検査中] 2022/05/09 渡辺
2202             Dly 0.2                                                     '移動完了後Delay 0.05>>0.2
2203             '
2204             '検査グループ番号設定終了確認
2205             M_Timer(1) = 0
2206             MExitFlg = 0
2207             While( MExitFlg = 0 )
2208                 '検査G設定正常終了?
2209                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2210                     MExitFlg = 1
2211                 '
2212                 '検査G設定異常終了?
2213                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2214                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2215                     If MInspErrNum = 0 Then                             '1回目のエラー?
2216                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2217                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2218                     EndIf
2219                     MExitFlg = 1
2220                 '
2221                 'timeoutチェック
2222                 ElseIf 1000 < M_Timer(1) Then
2223                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2224                     If MInspErrNum = 0 Then                             '1回目のエラー?
2225                         MInspErrNum = 12                                'timeout エラー番号=12
2226                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2227                     EndIf
2228                     MExitFlg = 1
2229                 EndIf
2230             WEnd
2231             '
2232             '検査G番号設定要求off
2233             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2234             '
2235             '----- 検査グループ設定Retry追加 20200410
2236             'NGなければ抜ける
2237             If MCurrentStepErr = 0 Then
2238                 MSetGrNumRetryExitFlg = 1
2239             Else
2240                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2241                 If MSetGrNumRetryCnt = 0 Then
2242                     MSetGrNumRetryExitFlg = 1
2243                 Else
2244                     'Retryへ　その前にDelay
2245                     Dly 0.5
2246                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2247                 EndIf
2248             EndIf
2249             '----- 検査グループ設定Retry追加ここまで 20200410
2250             '
2251         WEnd
2252         '
2253         '
2254         '
2255         '----- 検査実行 -----
2256         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2257             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2258                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2259                 MInspRetryExitFlg = 0
2260                 MRetryCnt = 2                                        'Retry回数設定
2261                 While( MInspRetryExitFlg = 0 )
2262                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2263                     '
2264                     '検査完了確認
2265                     MRetryCnt = MRetryCnt - 1
2266                     M_Timer(1) = 0
2267                     MExitFlg = 0
2268                     While( MExitFlg = 0 )
2269                     '検査完了待ち
2270                         '検査OK終了?
2271                         If M_In( MIN_IS_InspOK% ) = 1  Then
2272                             MJudgeOKFlg = 1                         '検査OKフラグON
2273                             MExitFlg = 1
2274                         '
2275                         '検査NG終了?
2276                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2277                             If MInspErrNum = 0 Then                 '1回目のエラー?
2278                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2279                                     MInspErrNum = 32                    '検査NG エラー番号=32
2280                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2281                                 EndIf
2282                             EndIf
2283                             MExitFlg = 1
2284                         '
2285                         '検査異常終了(IS timeout)?
2286                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2287                             If MInspErrNum = 0 Then                 '1回目のエラー?
2288                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2289                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2290                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2291                                 EndIf
2292                             EndIf
2293                             MExitFlg = 1
2294                         '
2295                         'timeoutチェック
2296                         ElseIf 3000 < M_Timer(1) Then
2297                             If MInspErrNum = 0 Then                 '1回目のエラー?
2298                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2299                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2300                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2301                                 EndIf
2302                             EndIf
2303                             MExitFlg = 1
2304                         EndIf
2305                     WEnd
2306                     '
2307                     '検査開始要求off
2308                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2309                     '
2310                     'OKなら抜ける
2311                     If MJudgeOKFlg = 1 Then
2312                         MInspRetryExitFlg = 1
2313                     Else
2314                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2315                         If MRetryCnt = 0 Then
2316                             MInspRetryExitFlg = 1
2317                         Else
2318                             'Retryへ　その前にDelay
2319                             Dly 0.3
2320                         EndIf
2321                     EndIf
2322                     '
2323                 WEnd
2324             EndIf
2325         EndIf
2326         '
2327         '
2328         '
2329         MNum% = MNum% + 1                                           '検査Step+1
2330         '検査終了確認　検査終了フラグセット
2331         If (MInspCnt% < MNum% ) Then
2332             MEndFlg% = 1                                            '検査終了フラグセット
2333         EndIf
2334         'NG発生時続行時処理
2335         If MInspErrNum <> 0 Then                                    'NGあり?
2336             If MNgContinue% <> 1 Then                               'NG続行?
2337                 MEndFlg% = 1                                        '検査終了フラグセット
2338             EndIf
2339         EndIf
2340     WEnd
2341     '
2342     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2343     If 0 < MZAxis% Then
2344         PCurrentPos = P_Curr                                        '現在位置取得
2345         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2346         fnAutoScreenComment(521)                                    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2347         Mvs PCurrentPos                                             '現在位置上空へ移動
2348     EndIf
2349     '
2350     '戻り値設定
2351     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      'カメラ検査強制OK(M_In(11372)=1)追加(12/21中村)
2352         ISInspectionSingle = 1                                      '正常終了戻り値設定
2353     Else
2354         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2355         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2356         ISInspectionSingle = 0                                      '異常終了戻り値設定
2357     EndIf
2358 '
2359 *ISInspectionSingle_End
2360     Exit Function
2361 FEnd
2362 '
2363 '■fnAutoScreenComment
2364 ''' <summary>
2365 ''' メイン画面の動作状況表示
2366 ''' コメントD1005の設定
2367 ''' </summary>
2368 '''<param name="McommentD1005%">コメントID</param>
2369 ''' <remarks>
2370 ''' Date   : 2021/07/07 : M.Hayakawa
2371 ''' </remarks>
2372 Function fnAutoScreenComment(ByVal McommentD1005%)
2373     M_Out16(12576) = McommentD1005%
2374     Exit Function
2375 FEnd
2376 '
2377 '■fnRoboPosChk
2378 ''' <summary>
2379 ''' 最後に終了したロボットポジションの確認
2380 ''' </summary>
2381 '''<param name="MINNumber%">入力番号</param>
2382 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2383 '''<param name="MTimeCnt&">タイムアウト時間</param>
2384 ''' PLCに保続した番号を読込み、確認
2385 ''' MRBTOpeGroupNo = 5 が初期位置に設定
2386 '''<returns>整数 0:タイムアウト 1:OK</returns>
2387 ''' <remarks>
2388 ''' Date   : 2021/07/07 : M.Hayakawa
2389 ''' </remarks>
2390 Function M% fnRoboPosChk
2391     fnRoboPosChk = 0
2392     MRet = fnStepRead()
2393     '初期位置でないと判断した場合
2394     'ウィンド画面切換え
2395     If MRBTOpeGroupNo > 5 Then
2396         '下記キー待ちの継続に反応させないため
2397         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2398         Dly 0.2
2399         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2400         Dly 1.5
2401         '
2402         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
2403         '
2404         MLoopFlg% = 1
2405         While MLoopFlg% = 1
2406             '
2407             '
2408             MKeyNumber% = fnKEY_WAIT()
2409             Select MKeyNumber%
2410                 Case Is = MAbout%       '停止
2411                     M_20# = MAbout%
2412                     MLoopFlg% = -1
2413                     Break
2414                 Case Is = MNext%        '次へ
2415                     'MLoopFlg% = -1
2416                     Break
2417                 Case Is = MContinue%    '継続
2418                     M_20# = MContinue%
2419                     MLoopFlg% = -1
2420                     Break
2421                 Default
2422                     Break
2423             End Select
2424         WEnd
2425     EndIf
2426     '
2427     If M_20# = MContinue% Then                              '継続ボタンが押された場合
2428         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
2429         Ovrd 5                                   '低速オーバーライド値設定
2430         Select MRBTOpeGroupNo
2431             Case Is = 5                          '何もしない
2432                 Break
2433             Case Is = 10                         '初期位置へ戻す
2434                 'Mov PTEST001
2435                 Break
2436             Case Is = 15                         '初期位置へ戻す
2437                 'Mov PTEST002
2438                 Dly 0.5
2439                 'Mov PTEST001
2440                 Dly 0.5
2441                 Break
2442             Default
2443                 Break
2444         End Select
2445         '
2446         Ovrd M_NOvrd                            'システムの初期値を設定
2447         M_Out(12364) = 1                        'toPLC_データ保存ON
2448         MRBTOpeGroupNo = 5
2449         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
2450         Dly 1.0
2451         M_Out(12364) = 0                        'toPLC_データ保存OFF
2452         fnRoboPosChk = 1                        '初期位置動作実行
2453         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
2454     EndIf
2455     Exit Function
2456 FEnd
2457 '
2458 '■frInCheck
2459 ''' <summary>
2460 ''' センサーINチェック
2461 ''' </summary>
2462 '''<param name="MINNumber%">入力番号</param>
2463 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2464 '''<param name="MTimeCnt&">タイムアウト時間</param>
2465 '''<returns>整数 0:タイムアウト 1:OK</returns>
2466 ''' <remarks>
2467 ''' Date   : 2021/07/07 : M.Hayakawa
2468 ''' </remarks>
2469 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2470     M_Timer(4) = 0
2471     MloopFlg = 0
2472     While MloopFlg = 0
2473         MCrtTime& = M_Timer(4)
2474         If M_In(MINNumber%) = MCMPFLG% Then
2475             MloopFlg = 1
2476             frInCheck = 1
2477         ElseIf MCrtTime& > MTimeCnt& Then
2478             MloopFlg = 1
2479             frInCheck = 0
2480         EndIf
2481     WEnd
2482     Exit Function
2483 FEnd
2484 '-----------------------------------------------
2485 '
2486 'ねじ締め機通信確認
2487 '
2488 '-----------------------------------------------
2489 Function M% fScewTcomChk
2490     fScewTcomChk = 0
2491     '通信確認送信
2492     M_Out(MOUT_ScwT_ComChk%) = MOn%
2493     '通信確認受信待機
2494     Wait M_In(MIN_ScwT_comOK%) = MOn%
2495     '通信確認送信終了
2496     M_Out(MOUT_ScwT_ComChk%) = MOff%
2497     Exit Function
2498 FEnd
2499 '
2500 '
2501 '-----------------------------------------------
2502 '
2503 'ねじ締め開始送信
2504 '
2505 '-----------------------------------------------
2506 Function M% fScewTStart
2507     fScewTStart = 0
2508     'ねじ締め開始待機を受信
2509     Wait M_In(MIN_ScwT_STRec%) = MOn%
2510     Dly 0.1
2511     'ねじ締め開始受信を送信
2512     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msecパルス
2513     Exit Function
2514 FEnd
2515 '
2516 '
2517 '-----------------------------------------------
2518 '
2519 'ねじ締め完了受信
2520 '
2521 '-----------------------------------------------
2522 Function M% fScewTFinish
2523     fScewTFinish = 0
2524     'ねじ締め完了待機を受信
2525     Wait M_In(MIN_ScwT_Fin%) = MOn%
2526     Dly 0.1
2527     'ねじ締め完了受信を送信
2528     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msecパルス
2529     Exit Function
2530 FEnd
2531 '
2532 '
2533 '-----------------------------------------------
2534 '
2535 '条件xx停止受信
2536 '
2537 '-----------------------------------------------
2538 Function M% fScewTCaseStop(ByVal MCase%())
2539     fScewTCaseStop = 0
2540     '条件xx停止を受信
2541     Wait M_In(MCase%(1)) = MOn%
2542     Dly 0.1
2543     '条件xx停止受信を送信
2544     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msecパルス
2545     Exit Function
2546 FEnd
2547 '
2548 '-----------------------------------------------
2549 '
2550 '再開始受信
2551 '
2552 '-----------------------------------------------
2553 Function M% fScewTReStart()
2554     fScewTReStart = 0
2555     '再開始を受信
2556     Wait M_In(MIN_ScwT_ReST%) = MOn%
2557     Dly 0.1
2558     '再開始受信を送信
2559     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msecパルス
2560     Exit Function
2561 FEnd
2562 '
2563 '■fErrorProcess
2564 '<summary>
2565 'エラー処理
2566 '</summary>
2567 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
2568 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
2569 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
2570 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
2571 '<make>
2572 '2021/11/5 中村天哉
2573 '</make>
2574 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2575     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
2576     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
2577     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
2578     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
2579 *RETRY_ERR_PROCESS
2580      M_20# = MClear%     '初期化
2581 '        'エラー処理記述
2582         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2583 '        'GOT KEY入力待ち
2584         MKeyNumber = fnKEY_WAIT()
2585 '        '
2586         If MKeyNumber = MAbout% Then   '停止を選択した場合
2587             M_20# = MAbout%            'M_20# プログラム間共通外部変数
2588  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2589             Break
2590          '
2591         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
2592             M_20# = MContinue%            'M_20# プログラム間共通外部変数
2593  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2594         '
2595         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
2596             M_20# = MNext%            'M_20# プログラム間共通外部変数
2597  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2598          '
2599         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
2600             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
2601  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2602             Break
2603         '
2604         EndIf
2605         '
2606         '
2607         '
2608         If M_20# = MClear% Then *RETRY_ERR_PROCESS
2609         fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2610         Exit Function
2611 FEnd
2612 '
2613 '■fnInitialZone
2614 ''' <summary>
2615 ''' 現在位置から上空に待避し、初期位置に戻る
2616 ''' </summary>
2617 ''' <remarks>
2618 ''' Date : 2021/12/2 : M.Hayakawa
2619 ''' Update:2022/06/2 : M.Hayakawa 他工程の非常停止復帰に合わせて変更
2620 ''' </remarks>
2621 Function fnInitialZone()
2622     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中]
2623 '
2624     Ovrd 5
2625 ' 上空退避
2626     PActive = P_Curr
2627     Pmove = PActive
2628 '
2629     If PActive.X > 580 Then
2630         Pmove.Z =380        'パレット上に腕を伸ばしているときは500まで上げられない為、例外処置
2631     Else
2632         Pmove.Z =500        '上記以外はZ:500まで持ち上げ
2633     EndIf
2634 '
2635     Mvs Pmove
2636     Mov PInitialPosition
2637 ' ロックを開放
2638     InitialState()
2639 ' 一旦停止
2640     fErrorProcess(20,70,256,0)
2641     Exit Function
2642 FEnd
2643 '
2644 '■InitialState
2645 ''' <summary>
2646 ''' ハンド、治具を初期位置にする
2647 ''' </summary>
2648 ''' <returns>   0 : OK
2649 '''             1 : NG
2650 ''' </returns>
2651 ''' <remarks>
2652 ''' Date : 2021/12/2 : M.Hayakawa
2653 ''' </remarks>
2654 Function M% InitialState()
2655     InitialState = 0
2656     '製品位置決め解除
2657     M_Out(12261)=1 Dly 0.3      'FANクランプ出端パルス出力
2658     'Wait M_In(11271)=1          'FANクランプ出端検出(修正につきコメントアウト(8/26中村))
2659     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'FANクランプ出端検出(8/26中村)
2660     If MRtn = 0 Then
2661         fErrorProcess(11,234,284,0)
2662         Select M_20#
2663             Case MAbout%            '停止か
2664                 InitialState = 1
2665                 Break
2666             Case MNgProcess%        'NGが押された場合
2667                 InitialState = 0
2668                 Break
2669             Case MContinue%
2670                 M_20# = MClear%
2671                 InitialState = 0
2672                 Break
2673             Case MNext%
2674                 M_20# = MClear%
2675                 InitialState = 0
2676                 Break
2677         End Select
2678     EndIf
2679     '
2680     M_Out(12259)=1 Dly 0.3      'プッシュCY用SV戻端パルス出力
2681     'Wait M_In(11269)=1          'プッシュ戻端検出(修正につきコメントアウト(8/26中村))
2682     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)    'プッシュ戻端検出
2683     If MRtn = 0 Then
2684         fErrorProcess(11,234,284,0)
2685         Select M_20#
2686             Case MAbout%            '停止か
2687                 InitialState = 1
2688                 Break
2689             Case MNgProcess%        'NGが押された場合
2690                 InitialState = 1
2691                 Break
2692             Case MContinue%
2693                 M_20# = MClear%
2694                 InitialState = 0
2695                 Break
2696             Case MNext%
2697                 M_20# = MClear%
2698                 InitialState = 0
2699                 Break
2700         End Select
2701     EndIf
2702     '
2703     M_Out(12257)=1 Dly 0.3      '位置決めCY用SV戻端パルス出力
2704     'Wait M_In(11267)=1          '位置決め戻端検出(修正につきコメントアウト(8/26中村))
2705     MRtn = frInCheck(11267,1,MSETTIMEOUT05&)   '位置決め戻端検出(8/26中村)
2706     If MRtn = 0 Then
2707         fErrorProcess(11,234,284,0)
2708         Select M_20#
2709             Case MAbout%            '停止か
2710                 InitialState = 1
2711                 Break
2712             Case MNgProcess%        'NGが押された場合
2713                 InitialState = 1
2714                 Break
2715             Case MContinue%
2716                 M_20# = MClear%
2717                 InitialState = 0
2718                 Break
2719             Case MNext%
2720                 M_20# = MClear%
2721                 InitialState = 0
2722                 Break
2723         End Select
2724     EndIf
2725     Exit Function
2726 FEnd
2727 '
2728 '■fnTorqueCheck
2729 ''' <summary>
2730 ''' トルクチェック動作用のメイン
2731 ''' </summary>
2732 ''' <remarks>
2733 ''' Date   : 2021/12/21 : H.AJI
2734 ''' </remarks>'
2735 Function M% fnTorqueCheck
2736     'トルクチェック中送信  搬送系停止
2737     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
2738     '
2739     fnTorqueCheck = 0
2740     Ovrd 20
2741     Mov PInitialPosition              '初期位置移動
2742     Ovrd 100
2743     '下記キー待ちの継続に反応させないため
2744     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2745     Dly 0.2
2746     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2747     '
2748     'M6340  トルクチェック受信
2749     M_Out(12340) = 1 Dly 1.0          'トルクチェック受信 M6340
2750     '
2751     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
2752     '
2753     MLoopFlg = 1
2754     While MLoopFlg = 1
2755         '
2756         Mov PInitialPosition              '初期位置移動
2757         '
2758         MKeyNumber = fnKEY_WAIT()
2759         Select MKeyNumber
2760             Case Is = 1           '停止
2761                 M_Out(12343) = 1 Dly 1.0         '停止要求開始要求受信 M6343
2762                 Ovrd 20
2763                 Mov PTicketRead_1
2764                 Ovrd 100
2765                 M_20# = 1
2766                 MLoopFlg = -1
2767                 Break
2768             Case Is = 2           '次へ
2769                 Break
2770             Case Is = 3           '継続
2771                 Break
2772             Case Is = 4           'トルクチェック開始
2773                 M_Out(12545) = 1    ' toPLC_PCトルクチェック1要求受信(M315)
2774                 M_Out(12342) = 1 Dly 1.0    'トルクチェック開始要求受信 M6342                Dly 1.0
2775                 M_Out(12342) = 0
2776                 fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
2777                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
2778                 MRet = fnMoveTorquePosi()
2779                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
2780                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2781                 Break
2782             Default
2783                 Break
2784         End Select
2785     WEnd
2786     '
2787     'トルクチェック中停止送信
2788     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
2789     '
2790     'ロボットの位置を元に戻す
2791     '
2792     Exit Function
2793  FEnd
2794  '
2795 '
2796 '
2797 '---------------------------
2798 '
2799 '    メイン画面の表示、非表示設定
2800 '         コメントD1001, D1002, D1003の設定
2801 '           MWindReSet = 0     画面非表示
2802 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
2803 '           MWindErrScr = 10    エラー画面 D1001, D1002
2804 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2805 '
2806 '---------------------------
2807 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2808     fnMainScreenOpen = 0
2809     '
2810    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2811         M_Out16(12480) = MCommentD1001            'D1001 コメント
2812     EndIf
2813     '
2814     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2815         M_Out16(12496) = MCommentD1002            'D1002 コメント
2816     EndIf
2817     '
2818     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2819         M_Out16(12512) = MCommentD1003            'D1003 コメント
2820     EndIf
2821     '
2822     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2823     M_Out(12362) = 1                         'ウィンド画面設定  M6362
2824     Dly 0.5
2825     M_Out(12362) = 0                         'ウィンド画面設定
2826     Exit Function
2827 FEnd
2828 '
2829 '■Main
2830 ''' <summary>
2831 ''' トルクチェック実動作
2832 ''' </summary>
2833 ''' <remarks>
2834 ''' Date   : 2021/12/21 : H.AJI
2835 ''' </remarks>'
2836 Function M% fnMoveTorquePosi
2837      fnMoveTorquePosi = 0
2838      Ovrd 50
2839      Mov PTorqueCheck_1 'トルクチェックメーター上空へ移動
2840     '
2841     Spd M_NSpd
2842 '-------------      ドライバーRST
2843     M_Out(12240)=0     'ドライバーOFF CCW
2844     M_Out(12241)=0     'ドライバーOFF CW
2845     M_Out(12242)=1     'ドライバー解除 C1
2846     M_Out(12243)=1     'ドライバー解除 C2
2847     M_Out(12245)=0     'プログラム解除 F1/プログラム2
2848 '---------------------------------------
2849 '[P-11]
2850 '--------------------------------------------------------------   【トルクチェック 0.4N - P11】
2851     Mov PTorqueCheck, -50                     ' トルク-1　置き位置上空 50mm へ移動
2852     Dly 0.1
2853 '-----------------------
2854    'Cnt 0                           'Cnt動作-2　終了
2855 '-----------------------
2856     Mov PTorqueCheck , -5                      'トルク-1　置き位置上空 5mm へ移動
2857     Dly 0.2
2858 '-----------------------
2859     ProgramBankSet(1,3)
2860     M_Out(12241)=0                   'ドライバーOFF  CW
2861     'Dly 0.1
2862 '--------------------------------
2863     Ovrd 40
2864    'Dly 0.1
2865 '--------------------------------  ネジ締め速度設定
2866     Spd 14                            'ライド 100-40 100% :Spd 12
2867     Dly 0.1
2868 '--------------------------------
2869 '--------------------------------
2870 '---------------------------------【ねじ締め動作】
2871 '
2872     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '移動中エラー検出
2873    Mvs PTorqueCheck               'トルクチェック位置へ移動
2874     Dly 0.3                          '動作安定待ち
2875    M_Out(12241)=1                   'ドライバーON  CW
2876 '
2877     Wait M_In(11584)=1                '完了/エラー検出
2878     Dly 0.1
2879     Spd M_NSpd
2880    'Ovrd 20
2881     If M_In(11256)=1 Then *LBL1       'ネジトータルエラー検出
2882     Wait M_In(11257)=1                'ネジ完了SC
2883 '---------------------------------
2884     Dly 0.1
2885     M_Out(12241)=0                    'ドライバーOFF CW
2886     Dly 0.1
2887     M_Out(12242)=0                    'ドライバー解除 C1
2888     Dly 0.1
2889     M_Out(12243)=0                    'ドライバー解除 C2 (バンク3)
2890     Dly 0.1
2891     M_Out(12245)=0                    'プログラム2解除 F1
2892 '--------------------------------------------------------------   【トルクチェック 0.4N - P11ここまで】
2893 '
2894     Mvs PTorqueCheck,-60                       'あえてmov から変更
2895     Dly 0.1
2896 '--------------------------------------------------------------
2897    'Ovrd 80
2898 '--------------------------------------------------------------
2899 '---------------------------------------
2900 '---------------------------------------
2901 '---------------------------------------エラー離脱処理
2902    *LBL1
2903    Fsc Off            '力覚センサ　Off   *STEP1は不要
2904    Mvs ,-100
2905    M_Out(12241)=0     'ドライバーOFF CW
2906    Dly 0.1
2907    M_Out(12242)=0     'ドライバー解除 C1
2908    Dly 0.1
2909    M_Out(12243)=0     'ドライバー解除 C2 (バンク3)
2910    Dly 0.1
2911    M_Out(12245)=0     'プログラム解除 F1
2912 '---------------------------------------
2913 '---------------------------------------
2914 '-------------
2915    'Mov PInitPos19049
2916    Dly 0.1
2917 '
2918 '
2919     Exit Function
2920 FEnd
2921 '
2922 '■Main
2923 ''' <summary>
2924 ''' 組立動作用のメイン
2925 ''' </summary>
2926 ''' <remarks>
2927 ''' Date   : 2021/07/07 : M.Hayakawa
2928 ''' </remarks>'
2929 Function Main
2930     MopeNo = M_21#         '外部変数にて動作番号代入
2931     '
2932     If M_Svo=0 Then
2933         Servo On
2934     EndIf
2935     Wait M_Svo=1
2936 '組立スタート日付時刻要求パルスON
2937     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
2938 'パトライト操作
2939     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
2940     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
2941     '
2942     M_20# = 0                                   'KEY入力初期化
2943     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
2944     MRet% = 0
2945 '復帰動作　実行・未実行判別      2022/03/22 渡辺 作成
2946     PActive = P_Curr                    '現在位置を取得
2947     MRecoveryPass% = 0
2948     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
2949         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
2950             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
2951             MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
2952         EndIf
2953     EndIf
2954     EndIf
2955     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
2956         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
2957             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
2958                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
2959             EndIf
2960         EndIf
2961     EndIf
2962     If MRecoveryPass% = 0 Then
2963         fnInitialZone()        '復帰動作パスフラグが立っていない時は復帰動作を実行
2964     EndIf
2965     '
2966     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
2967         M_Out(12364) = 1            'toPLC_データ保存ON
2968 'トルクチェック
2969         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
2970             MRet% = fnTorqueCheck()
2971             Break
2972         Else
2973 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
2974 '                MRtn = InspInit()               '画像処理初期化処理
2975 '            EndIf
2976             '
2977            M_20# = MClear%                    '初期化
2978 '組立開始
2979             If M_In(MIN_ASSY_CANCEL%) = 0 Then
2980                 MRet% = fnAssyStart()
2981             Else
2982                 M_20# = MPass%
2983             EndIf
2984 '組立終了日付時刻
2985             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
2986             Wait M_In(11572) = 1            '日付取得完了
2987             Dly 0.1
2988             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
2989 'リフターユニットへのOUT
2990             '  KEY入力が何もない場合 OKと判断
2991             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
2992             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
2993 'OK/NGフラグ出力
2994 '            If M_20# <= 0 Then
2995 '                M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
2996 '            ElseIf M_20# = MPass% Then
2997 '                M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
2998 '            EndIf
2999 'PIASに組立完了書込み
3000             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3001                 If M_20# = MPass% Then
3002                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3003                 Else
3004                     'KEY入力がNGの場合
3005                     If M_20# = MNgProcess% Then
3006                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3007                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3008                         MRet% = fnPiasWrite(MNG%)
3009                        nAssyNgQty = nAssyNgQty + 1
3010                     EndIf
3011                     '
3012                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/17中村)
3013                     If M_20# = MAssyOK% Then
3014                             '-----------------------
3015                             'D732 -> D2600 コピー要求
3016                             M_Out(12566) = 1
3017 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3018                             M_Out(12566) = 0
3019                             '
3020                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3021                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3022                             '基板番号照合(PPは未使用）
3023 '                            MRet% = fnPCBNumberCheck()
3024                         Else
3025                             MRet% = 1
3026                         EndIf
3027                         '
3028                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3029                             If M_20# <> MAbout% Then
3030                                 '工程履歴OK書き込み
3031                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3032                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3033                                 MRet% = fnPiasWrite(MOK%)
3034                                 nAssyOkQty = 0
3035                                 nAssyOkQty = nAssyOkQty + 1
3036                             Else
3037                                 nAssyOkQty = nAssyOkQty + 1
3038                             EndIf
3039                         EndIf
3040                     EndIf
3041 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3042 '                    MRet% = fnPiasWrite(MOK%)
3043                 EndIf
3044             Else
3045                 nAssyOkQty = nAssyOkQty + 1
3046             EndIf
3047             '
3048             '組立終了日付時刻解除
3049             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3050             '投入数、組立OK数、組立NG数書込み
3051 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3052             '
3053 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3054 '                '画像処理終了処理
3055 '                MRtn = InspQuit()
3056 '            EndIf
3057         EndIf
3058         M_Out(12364) = 0                          'toPLC_データ保存OFF
3059     EndIf
3060 'パトライト操作
3061     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3062     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3063 'GOT表示
3064     fnAutoScreenComment(93)  'AUTO画面 工程完了
3065 FEnd
3066 End
3067 '
3068 '
3069 'おまじないコメント
3070 '絶対削除するな
3071 '
3072 '
3073 '
3074 '
PInspPosition(1)=(+271.04,-78.15,+420.92,+160.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PInspPosition(2)=(+271.04,+18.95,+420.92,+160.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PInspPosition(3)=(+334.93,+81.60,+410.00,+180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
PInspPosition(4)=(+538.88,+54.12,+420.00,+180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PInspPosition(5)=(+486.73,-160.17,+397.00,+180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PInspPosition(6)=(+324.58,-173.46,+417.00,-180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
PInspPosition(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(11)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(12)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(13)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(14)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(15)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(16)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(17)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(18)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(19)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(20)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(21)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(22)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(23)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(24)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(25)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(26)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(27)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(28)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(29)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(30)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PTemp=(+602.00,-150.75,+450.00,-180.00,-0.02,+90.00,+0.00,+0.00)(7,0)
PScrewPosTemp(1)=(+349.16,+91.52,+370.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPosTemp(2)=(+349.16,+91.52,+313.35,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPosTemp(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(10)=(+349.16,+91.52,+303.35,+180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PGetScrewPosTemp(1)=(+233.44,+389.39,+380.00,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PGetScrewPosTemp(2)=(+166.05,+146.93,+400.00,-180.00,+0.00,+128.05,+0.00,+0.00)(7,0)
PGetScrewPosTemp(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(9)=(+127.52,+411.68,+432.42,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PGetScrewPosTemp(10)=(+233.44,+389.39,+338.70,-180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
PEscapePosi(1)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PActive=(+602.00,-150.75,+450.00,-180.00,-0.02,+90.00)(7,0)
Pmove=(+602.00,-150.75,+380.00,+180.00,-0.02,+90.00)(7,0)
PGyroPcbRead=(+315.06,-65.41,+419.00,+160.00,+0.00,+90.00)(7,0)
PGyroPcbRead_1=(+329.62,-57.71,+450.00,-180.00,+0.00,+90.00)(7,0)
PInitialPosition=(+300.00,+0.00,+450.00,-180.00,+0.00,-180.00)(7,0)
PMainPcbRead=(+310.35,-173.29,+417.00,-180.00,+0.00,-180.00)(7,0)
PMainPcbRead_1=(+310.35,-173.29,+450.00,-180.00,+0.00,-180.00)(7,0)
PParts1Check=(+271.04,-78.15,+420.92,+160.00,+0.00,+90.00)(7,0)
PParts1Check_1=(+271.04,-78.15,+450.00,+160.00,+0.00,+90.00)(7,0)
PParts2Check=(+334.93,+81.60,+410.00,+180.00,+0.00,+180.00)(7,0)
PParts2Check_1=(+334.93,+81.60,+450.00,+180.00,+0.00,+180.00)(7,0)
PParts3Check=(+538.88,+54.12,+420.00,+180.00,+0.00,-180.00)(7,0)
PParts3Check_1=(+538.88,+54.12,+460.00,-180.00,+0.00,+180.00)(7,0)
PParts4Check=(+486.73,-160.17,+397.00,+180.00,+0.00,-180.00)(7,0)
PParts4Check_1=(+486.73,-160.17,+450.00,+180.00,+0.00,-180.00)(7,0)
PParts5Check=(+271.04,+18.95,+420.92,+160.00,+0.00,+90.00)(7,0)
PParts5Check_1=(+271.04,+18.95,+450.00,+160.00,+0.00,+90.00)(7,0)
PParts6Check=(+324.58,-173.46,+417.00,-180.00,+0.00,+180.00)(7,0)
PParts6Check_1=(+324.58,-173.46,+450.00,-180.00,+0.00,+180.00)(7,0)
PScrewFan1=(+317.38,+123.20,+303.40,-180.00,+0.00,+90.00)(7,0)
PScrewFan1_0=(+317.38,+123.20,+313.40,-180.00,+0.00,+90.00)(7,0)
PScrewFan1_1=(+317.38,+123.20,+370.00,-180.00,+0.00,+90.00)(7,0)
PScrewFan2=(+349.16,+91.52,+303.35,+180.00,+0.00,+90.00)(7,0)
PScrewFan2_0=(+349.16,+91.52,+313.35,-180.00,+0.00,+90.00)(7,0)
PScrewFan2_1=(+349.16,+91.52,+370.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain1=(+305.16,-26.35,+312.86,-180.00,+0.00,+90.00)(7,0)
PScrewMain1_0=(+305.16,-26.35,+318.86,-180.00,+0.00,+90.00)(7,0)
PScrewMain1_1=(+305.16,-26.35,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain2=(+304.96,-174.86,+312.74,-180.00,+0.00,+90.00)(7,0)
PScrewMain2_0=(+304.96,-174.86,+318.74,-180.00,+0.00,+90.00)(7,0)
PScrewMain2_1=(+304.96,-174.86,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain3=(+366.92,-180.63,+312.90,-180.00,+0.00,+90.00)(7,0)
PScrewMain3_0=(+366.92,-180.63,+318.90,-180.00,+0.00,+90.00)(7,0)
PScrewMain3_1=(+366.92,-180.63,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain4=(+379.36,+24.74,+313.94,-180.00,+0.00,+60.00)(7,0)
PScrewMain4_0=(+379.36,+24.74,+319.94,-180.00,+0.00,+60.00)(7,0)
PScrewMain4_1=(+379.36,+24.74,+380.00,-180.00,+0.00,+60.00)(7,0)
PScrewMain5=(+384.95,-104.71,+330.57,-180.00,+0.00,+90.00)(7,0)
PScrewMain5_0=(+384.95,-104.71,+341.97,-180.00,+0.00,+90.00)(7,0)
PScrewMain5_1=(+384.95,-104.71,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain6=(+325.22,-78.83,+332.30,-180.00,+0.00,+90.00)(7,0)
PScrewMain6_0=(+325.22,-78.83,+342.70,-180.00,+0.00,+90.00)(7,0)
PScrewMain6_1=(+325.22,-78.83,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewSupplyFan=(+233.52,+389.28,+338.64,-180.00,+0.00,+180.00)(7,0)
PScrewSupplyFan_1=(+233.52,+389.28,+380.00,-180.00,+0.00,-180.00)(7,0)
PScrewSupplyFan_2=(+166.05,+146.93,+400.00,-180.00,+0.00,+128.05)(7,0)
PScrewSupplyFan_9=(+127.52,+411.68,+432.42,-180.00,+0.00,-180.00)(7,0)
PScrewSupplyMain=(+103.30,+195.32,+338.34,-180.00,+0.00,+180.00)(7,0)
PScrewSupplyMain_1=(+103.30,+195.32,+380.00,-180.00,+0.00,+180.00)(7,0)
PScrewSupplyMain_2=(+166.05,+146.93,+447.34,-180.00,+0.00,+128.05)(7,0)
PScrewSupplyMain_9=(-3.19,+216.64,+432.44,+180.00,+0.00,-180.00)(7,0)
PTicketRead=(+602.00,-150.75,+373.00,+180.00,-0.02,+90.00)(7,0)
PTicketRead_1=(+602.00,-150.75,+450.00,+180.00,-0.02,+90.00)(7,0)
PTorqueCheck=(+143.66,-242.00,+340.00,-180.00,-0.01,+90.00)(7,0)
PTorqueCheck_1=(+143.66,-242.00,+360.00,-180.00,-0.01,+90.00)(7,0)
