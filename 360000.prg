1 ' ===================================
2 '
3 '  21054001 STEP5 Assy6プログラム
4 '
5 ' 作成者：M.Hayakawa
6 ' 作成日：2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1から流用
8 '
9 ' Ver 0.3 2021.12.17 画像検査関数ISInspection→ISInspectionSingle、画像検査追加 file:210542003
10 ' ===================================
11 '===== <Insight定数> =====
12 '===== <Insight変数定義> =====
13 Dim PInspPosition(30)               '画像処理Function引渡し用位置変数
14 Dim MInspGroup%(30)                 '画像処理Function引渡し用変数
15 Def Inte MIN_IS_Ready               '【入力IO】Insight準備OK
16 Def Inte MIN_IS_JobLoadOK           '【入力IO】Insightジョブロード正常終了
17 Def Inte MIN_IS_JobLoadNG           '【入力IO】Insightジョブロード異常終了
18 Def Inte MIN_IS_InspGSetOK          '【入力IO】Insight検査グループ番号設定正常終了
19 Def Inte MIN_IS_InspGSetNG          '【入力IO】Insight検査グループ番号設定異常終了
20 Def Inte MIN_IS_InspOK              '【入力IO】Insight検査OK
21 Def Inte MIN_IS_InspNG              '【入力IO】Insight検査NG
22 Def Inte MIN_IS_InspErr             '【入力IO】Insight検査異常終了
23 Def Inte MIN_IS_InspCapDone         '【入力IO】Insight検査画像取込完了
24 '
25 Def Inte MIN_IS_ErrNum              '【入力IO】Insight処理エラー番号取得開始アドレス(16bit)
26 'Output Signal
27 Def Inte MOUT_IS_JobLoadReq         '【出力IO】Insight JOBロード要求
28 Def Inte MOUT_IS_InspGSetReq        '【出力IO】Insight 検査グループ番号設定要求
29 Def Inte MOUT_IS_Insp               '【出力IO】Insight 検査実行要求
30 '
31 Def Inte MOUT_IS_JobNum             '【出力IO】Insight JOB番号設定開始アドレス(16bit)
32 Def Inte MOUT_IS_InspGNum           '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
33 '
34 Def Inte MOUT_InspErrNum            '【出力IO】検査実行エラー番号開始アドレス(16bit)
35 Def Inte MOUT_InspNGStepNum         '【出力IO】検査実行NGStep番号開始アドレス(16bit)
36 Def Inte MOUT_OKNG                 '
37 '作業用変数
38 Def Inte MInspErrNum                '検査実行エラー番号
39 Def Inte MInspNGStepNum             '検査実行NGStep番号
40 Def Inte MRtn                       'Function戻り値取得用
41 Def Inte MRtn2                      'Function戻り値取得用
42 Def Inte MRet3                      'Function戻り値取得用
43 Def Inte MGRtn                      'Function戻り値取得用 ネジ供給機
44 Def Inte MInspErrNumSub             '検査実行エラー番号sub　20190820追加
45 Def Inte MovrdA                     'ネジ締めOvrd 可変用
46 Def Float MSpdA                     'ネジ締めSpd　可変用
47 Def Pos PTemp                       'ネジ締め上空位置計算用
48 Def Inte MKeyNum                    'KEY入力受け取り用(追加12/20中村)
49 '===== <Insight変数設定> =====
50 MIN_IS_Ready%        =   11380      '【入力IO】Insight準備OK
51 MIN_IS_JobLoadOK%    =   11381      '【入力IO】Insightジョブロード正常終了
52 MIN_IS_JobLoadNG%    =   11382      '【入力IO】Insightジョブロード異常終了
53 MIN_IS_InspGSetOK%   =   11383      '【入力IO】Insight検査グループ番号設定正常終了
54 MIN_IS_InspGSetNG%   =   11384      '【入力IO】Insight検査グループ番号設定異常終了
55 MIN_IS_InspOK%       =   11385      '【入力IO】Insight検査OK
56 MIN_IS_InspNG%       =   11386      '【入力IO】Insight検査NG
57 MIN_IS_InspErr%      =   11387      '【入力IO】Insight検査異常終了
58 MIN_IS_InspCapDone%  =   11388      '【入力IO】Insight検査画像取込完了
59 MIN_IS_ErrNum%       =   11408      '【入力IO】Insight処理エラー番号開始アドレス(16bit)
60 'Output Signal
61 MOUT_IS_JobLoadReq%  =   12370      '【出力IO】Insight JOBロード要求
62 MOUT_IS_InspGSetReq% =   12371      '【出力IO】Insight 検査グループ番号設定要求
63 MOUT_IS_Insp%        =   12372      '【出力IO】Insight 検査実行要求
64 MOUT_IS_JobNum%      =   12384      '【出力IO】Insight JOB番号設定開始アドレス(16bit)
65 MOUT_IS_InspGNum%    =   12400      '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
66 MOUT_InspErrNum%     =   12416      '【出力IO】検査実行エラー番号開始アドレス(16bit)
67 MOUT_InspNGStepNum%  =   12432      '【出力IO】検査実行NGStep番号開始アドレス(16bit)
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
89 X34_ScrewReady1=11259 'ねじっこ1　Read
90 '===== <電ドラ定数> =====
91 Dim PScrewPos(10)       'ネジ締め用Function引数変数
92 Dim PGetScrewPos(10)    'ねじ供給機からねじを得るFunction引数変数
93 Dim PEscapePosi(10)
94 MLoopCnt% = 0'
95 '===== <ロボット定数> =====
96 '===== <ロボット変数定義> =====
97 MRBTOpeGroupNo = 0      'ロボット動作番号初期化
98 MCommentD1001 = 0
99 MCommentD1002 = 0
100 MCommentD1003 = 0
101 MScreenNo = 0
102 '
103 MCommentTSU = 0
104 MCommentTSD = 0
105 'ウィンド画面番号設定
106 MWindReSet = 0
107 MWindInfoScr = 5
108 MWindErrScr = 10
109 MWindErrScr2 = 11
110 MWindErrScr3 = 13
111 MWindErrScr17 = 17
112 MWindErrScr18 = 18
113 MWindCmmnScr = 20
114 MWindJigRelase19049 = 60
115 MWindJigRelase19050 = 61
116 MWindJigRelase19051 = 62
117 '
118 MClear% = 0        'KEY_のクリア
119 MAbout% = 1        'KEY_停止
120 MNext% = 2         'KEY_次のステップへ移行
121 MContinue% = 3     'KEY_継続 再度同じ動作を行う
122 '
123 MKeyNum% = 0       'KEY入力を受け取る
124 '
125 Def Inte MNgProcess
126 MNgProcess% = 5      'KEY_NG
127 '
128 MAssyOK% = 6       '組立完了
129 MPass% = 7         '工程パス
130 MPiasNG% = 8       'Pias確認時履歴NG
131 MIrregular% = 10   '例外処理実行用
132 MPiasOK% = 11      'PIAS先読みOK時
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
150 MOK% = 1               '各判定用
151 MNG% = 0               '各判定用
152 MTIMEOUT% = -1         '各判定用
153 MJudge% = 0            '判定情報格納用
154 '
155 MRECIVETIME& = 0
156 MSETTIMEOUT10& = 10000&                '10秒設定
157 MSETTIMEOUT08& = 8000&                 '10秒設定
158 MSETTIMEOUT03& = 3000&                 '3秒設定
159 MSETTIMEOUT01& = 1000&                 '1秒設定
160 MSETTIMEOUT05& = 5000&                 '5秒設定
161 MSETTIMEOUT009& = 900&                 '0.9秒設定
162 MSETTIMEOUT008& = 800&                 '0.8秒設定
163 MSETTIMEOUT007& = 700&                 '0.7秒設定
164 MSETTIMEOUT006& = 600&                 '0.6秒設定
165 MSETTIMEOUT005& = 500&                 '0.5秒設定
166 MSETTIMEOUT004& = 400&                 '0.4秒設定
167 MSETTIMEOUT003& = 300&                 '0.3秒設定
168 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
169 MIN_PIAS_ComOK% = 11552                'PC通信OK
170 MIN_PIAS_ComTimeOut% = 11576           'PC通信確認タイムアウト
171 MIN_PIAS_ComNG% = 11553                'PC通信NG
172 MOUT_PIAS_ComCheck% = 12544            'PC通信確認要求
173 MOUT_PIAS_Missing_Process% = 12546     '工程抜け確認要求
174 MIN_PIAS_ModelTypeNG% = 11554          'モデル仕向NG
175 MIN_PIAS_ProcessHistryNG% = 11555      '前工程履歴NG
176 MIN_PIAS_ProcessHistryOK% = 11556      '前工程履歴OK
177 MIN_PIAS_ProcessHistryErr% = 11557     '工程履歴処理エラー
178 MIN_PIAS_MyProcessComp% = 11573        '自工程履歴あり
179 MIN_PIAS_ProcessHistryTimeOut% = 11578 '工程履歴タイムアウト
180 MOUT_OKNG% = 12226                     'PLC OUT でOK=1, NG=0 出力
181 '
182 MOUT_PiasPCBNumberCheck = 12557        '基板番号照合
183 MIN_PiasPCBNumberOK% = 11566          '基板番号OK
184 MIN_PiasPCBNumberNG% = 11565          '基板番号NG
185 MIN_PiasPCBNumberErr% = 11567         '基板番号処理エラー
186 '
187 MOUT_PiasAssyResultOK% = 12549    '組立OK
188 MOUT_PiasAssyResultNG% = 12550    '組立NG
189 MOUT_PiasAssyResultWr% = 12548    '工程履歴書き込み
190 '
191 MIN_PiasProcessNG% = 11559        '工程履歴処理NG
192 MIN_PiasProcessOtherErr% = 11560  '工程履歴処理エラー(なんかのトラブル)
193 MIN_PiasProcessOK% = 11558        '工程履歴処理OK
194 '
195 MIN_Insight_Use% = 11374               '画像確認ON
196 MIN_TorqueCheck% = 11348               'トルクチェック
197 '
198 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT操作権
199 MOUT_RED_LIGHT% = 12356            'PATLIGHT 赤 点灯
200 MOUT_RED_FLASH% = 12357            'PATLIGHT 赤 点滅
201 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT 黄 点灯
202 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT 黄 点滅
203 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT 青 点灯
204 MOUT_GREEN_FLASH% = 12361          'PATLIGHT 青 点滅
205 '
206 MOUT_ST_DATETIME% = 12551          '組立開始日付時刻
207 MOUT_ED_DATETIME% = 12552          '組立終了日付時刻
208 '
209 MOUT_TORQUE_CHECK% = 12367         'PLCへトルクチェック中を送信
210 '
211 MIN_ASSY_CANCEL% = 11366           '組立を行うかのフラグ
212 '
213 MLoopFlg% = 0                      'KEY入力後のOK or NG内容
214 MopeNo% = 0
215 MRtn% = 0
216 MRet = 0
217 MRet3% = 0
218 '
219 Def Inte MInputQty          '投入数 演算変数
220 Def Inte MAssyOkQty         '組立ＯＫ数 演算変数
221 Def Inte MAssyNgQty         '組立ＮＧ数 演算変数(未使用)
222 Def Inte MSuctionErrQty     '吸着エラー数 演算変数 2022/04/27 渡辺
223 Def Inte nAssyOkQty         '未使用
224 Def Inte MScrewNo
225 Def Inte MReTry
226 '===== <IO変数定義> =====
227 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
228 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
229 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
230 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
231 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
232 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
233 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
234 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
235 '
236 Def Inte Y6A_VV1            ' アーム先端　ネジ吸着バルブ
237 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
238 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
239 '
240 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
241 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
242 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
243 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
244 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
245 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
246 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
247 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
248 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
249 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
250 '
251 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
252 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
253 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
254 '
255 Def Inte MOUT_LED1          ' 画像処理用LED照明
256 '
257 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
258 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
259 '
260 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
261 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
262 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
263 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
264 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
265 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
266 '
267 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
268 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
269 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
270 '
271 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
272 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
273 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
274 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
275 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
276 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
277 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
278 Y6A_VV1%    =  12250    ' アーム先端　ネジ吸着バルブ
279 Y6B_VB1%    =  12251    'アーム先端　吸着破壊バルブ
280 MOUT_VB1%   =  12251    ' アーム先端　ネジ吸着破壊バルブ
281 '
282 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
283 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
284 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
285 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
286 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
287 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
288 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
289 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
290 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
291 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
292 '
293 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
294 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
295 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
296 '
297 MOUT_LED1%  =  12239    ' 画像処理用LED照明
298 '
299 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
300 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
301 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
302 '
303 '共通
304 Def Inte MTEST_KEY                      'デバックテスト用
305 Def Inte MOn                            '出力=1
306 Def Inte MOff                           '出力=0
307 '
308 'ねじ締め装置_出力アドレス
309 Def Inte MOUT_ScwT_ComChk               '通信確認
310 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
311 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
312 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
313 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
314 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
315 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
316 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
317 'ねじ締め装置_入力アドレス
318 Def Inte MIN_ScwT_comOK                 '通信確認返信
319 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
320 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
321 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
322 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
323 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
324 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
325 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
326 '
327 Dim MScwT_Case1%(2)               '条件1停止変数
328 Dim MScwT_Case2%(2)               '条件2停止変数
329 Dim MScwT_Case3%(2)               '条件3停止変数
330 Dim MScwT_Case4%(2)               '条件4停止変数
331 Dim MScwT_Case5%(2)               '条件5停止変数
332 '
333 '共通
334 MTEST_KEY% = 11359                       'デバッグ用テストKEY
335 MOn% = 1                                 '出力 = 1
336 MOff% = 0                                '出力 = 0
337 '
338 'ねじ締め機_アドレス設定
339 MOUT_ScwT_ComChk% = 12832               '通信確認送信
340 MOUT_ScwT_ST% = 12865                   'ねじ締め開始を送信
341 MOUT_ScwT_ReSTOK% = 12866               '再開始受信を送信
342 MOUT_ScwT_FinOK% = 12868                'ねじ締め完了受信を送信
343 MOUT_ScwT_Case1OK% = 12858              '条件1停止受信を送信
344 MOUT_ScwT_Case2OK% = 12859              '条件2停止受信を送信
345 MOUT_ScwT_Case3OK% = 12860              '条件3停止受信を送信
346 MOUT_ScwT_Case4OK% = 12861              '条件4停止受信を送信
347 MOUT_ScwT_Case5OK% = 12862              '条件5停止受信を送信
348 '
349 MIN_ScwT_comOK% = 11840                 'ねじ締め装置から返信
350 MIN_ScwT_STRec% = 11873                 'ねじ締め開始を受信
351 MIN_ScwT_ReST% = 11874                  '再開始を受信
352 MIN_ScwT_Fin% = 11876                   'ねじ締め完了を受信
353 MIN_ScwT_Case1% = 11866                 '条件1停止待機を受信
354 MIN_ScwT_Case2% = 11867                 '条件2停止待機を受信
355 MIN_ScwT_Case3% = 11868                 '条件3停止待機を受信
356 MIN_ScwT_Case4% = 11869                 '条件4停止待機を受信
357 MIN_ScwT_Case5% = 11870                 '条件5停止待機を受信
358 '
359 MScwT_Case1%(1) = MIN_ScwT_Case1%
360 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
361 MScwT_Case2%(1) = MIN_ScwT_Case2%
362 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
363 MScwT_Case3%(1) = MIN_ScwT_Case3%
364 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
365 MScwT_Case4%(1) = MIN_ScwT_Case4%
366 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
367 MScwT_Case5%(1) = MIN_ScwT_Case5%
368 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
369 '
370 '設定 InitialZoneBで使用する変数
371 Def Pos PActive       '直交座標系 位置変数 現在位置
372 Def Pos Pmove         '直交座標系 位置変数 移動先
373 Def Jnt JActive       '関節座標系 位置変数 現在位置
374 Def Jnt Jmove         '関節座標系 位置変数 移動先
375 Def Jnt JTaihi        '関節座標系 位置変数 退避ポジション ティーチングで設定
376 Def Inte MRecoveryPass         '復帰動作パスフラグ　1=復帰動作をパス　0=復帰動作を実行
377 Def Inte MStandby              '待機位置確認フラグ
378 Def Inte MRecoveryChuckOpen    'チャック解放フラグ（復帰動作前）両掴み対策
379 '★注意★初期位置を変更した時には、変更が必要！
380 '
381 '
382 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
383 Function M% fnAssyStart
384     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
385 ' PIASチケット読込み工程抜け確認
386     M_20# = MClear%                       '初期化
387     If M_22# = MIrregular% Then GoTo *IRREGULAR     'Assy完了後DVDメカを把持している場合
388     '開始位置がイニシャルポジションだった場合(ハンド交換治具が無いかチェック)
389     *RE_START
390     PTemp = P_Curr
391     MRtn = 0
392     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
393         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
394             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
395                 MRtn = 1
396             EndIf
397         EndIf
398     EndIf
399     'イニシャルポジションにいた場合センサーチェック
400     If MRtn = 1 Then
401         Ovrd 20
402         Accel 100 , 20
403         Mvs PHandChange                             'ハンド交換位置に移動
404         Ovrd 100
405         Accel 100 , 100
406         MRtn = frInCheck(11264,0,MSETTIMEOUT03&)    'センサーチェック(治具が無いか)
407         Mvs PInitialPosition                        'イニシャルポジションに移動
408     Else
409         MRtn = 1
410     EndIf
411     If MRtn = 1 Then GoTo *AssyStart
412     fErrorProcess(11,286,287,0)                        'ハンド交換用治具がある
413     If M_20# = MNext% Then M_20# = MClear%
414     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
415     If M_20# = MNgProcess% Then GoTo *RE_START
416     If M_20# = MContinue% Then GoTo *RE_START
417 '
418     *AssyStart
419 '
420 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
421 '        MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
422 '        '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
423 '        '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
424 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' メニューへ戻る
425 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NGを工程履歴に書込み次の工程へ
426 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NGを工程履歴に書込み次の工程へ
427 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' 履歴NG, 工程抜け
428 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK以外は組立終了
429 '    EndIf
430 ' ネジ締め機テスト用 ----------
431 '    'Mret% = fScewTcomChk()
432 '    'ねじ締め開始
433     MRtn2 = fScewTStart()
434 '    '
435 '    '座標移動
436 '    '
437 '    '条件xx停止
438 '    fScewTCaseStop(MScwT_Case5%)
439 '    '
440 '    'ベースユニットKEY
441 '    Wait M_In(MTEST_KEY%) = MOn%
442 '    '
443 '    '再開始
444 '    fScewTReStart()
445 '    '
446 '    '座標移動
447 '    '
448 '    'ねじ締め完了
449 '    Mret% = fScewTFinish()
450 ' ネジ締めテスト終了
451 ' PIASテスト -----------
452 '    MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
453 '    MRet% = fnPiasWrite(MNG%)
454  '   MRet% = fnPCBNumberCheck()     'デバック用にコメントアウト(9/17中村)
455 ' PIASテスト終了 -------
456 '組み立て開始(仮組み9/10中村)
457 'プログラム原点
458 Ovrd 100
459 If MRtn2 = 0 Then GoTo *INITIAL_CHECK
460     fErrorProcess(11,329,201,0)
461     If M_20# = MNext% Then GoTo *AssyStart
462     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
463     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
464     If M_20# = MContinue% Then GoTo *AssyStart
465 'ハンドにDVDメカ,ブラケットが無いか
466 '
467 *INITIAL_CHECK
468 '
469 If M_In(11264) = 0 And M_In(11267) = 0 And M_In(11270) = 0 Then GoTo *CompInitial1  'DVDメカ,ブラケットが無いか
470 fErrorProcess(11,253,287,0)                 '284→287に変更6/2中村
471 If M_20# = MNext% Then M_20# = MClear%
472 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
473 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
474 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
475 *CompInitial1
476 '
477 'ハンドをイニシャルに戻す
478 If M_In(11266) = 1 Then     'DVDチャック閉検出
479     M_Out(12256) = 0        'DVDチャック閉OFF
480     M_Out(12257) = 1        'DVDチャック開ON
481     Break
482 EndIf
483 If M_In(11269) = 1 Then     'Fシリンダー出検出
484     M_Out(12258) = 0        'Fシリンダー出OFF
485     M_Out(12259) = 1        'Fシリンダー戻ON
486     Break
487 EndIf
488 If M_In(11272) = 1 Then     'Rシリンダー出検出
489     M_Out(12260) = 0        'Rシリンダー出OFF
490     M_Out(12261) = 1        'Rシリンダー戻ON
491     Break
492 EndIf
493 '
494 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    'DVDチャック開検出
495 If MRtn = 1 Then GoTo *CompInitial2
496 fErrorProcess(11,270,284,0)
497 If M_20# = MNext% Then M_20# = MClear%
498 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
499 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
500 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
501 *CompInitial2
502 '
503 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)    'Fシリンダー戻検出
504 If MRtn = 1 Then GoTo *CompInitial3
505 fErrorProcess(11,278,284,0)
506 If M_20# = MNext% Then M_20# = MClear%
507 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
508 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
509 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
510 *CompInitial3
511 '
512 MRtn = frInCheck(11271,1,MSETTIMEOUT05&)    'Rシリンダー戻検出
513 If MRtn = 1 Then GoTo *CompInitial4
514 fErrorProcess(11,276,284,0)
515 If M_20# = MNext% Then M_20# = MClear%
516 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
517 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
518 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
519 *CompInitial4
520 '
521 '
522 ' 2022/04/11 安全方向へ条件追加 渡辺
523 ' PInitialPosition 在席 MStandby=1
524 ' PMechaOnJigGet_3 在席 MStandby=2
525 '
526 MStandby = 0    '待機位置フラグを初期化
527 PTemp = P_Curr
528 If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
529     If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
530         If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
531             MRtn = 1
532         EndIf
533     EndIf
534 EndIf
535 If (PTemp.X <= PMechaOnJigGet_3.X + 1.0) And (PTemp.X >= PMechaOnJigGet_3.X - 1.0) Then
536     If ((PTemp.Y <= PMechaOnJigGet_3.Y + 1.0) And (PTemp.Y >= PMechaOnJigGet_3.Y - 1.0)) Then
537         If ((PTemp.Z <= PMechaOnJigGet_3.Z + 1.0) And (PTemp.Z >= PMechaOnJigGet_3.Z - 1.0)) Then
538             MRtn = 2
539         EndIf
540     EndIf
541 EndIf
542 If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
543     If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
544         If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
545             MStandby = 3
546         EndIf
547     EndIf
548 EndIf
549 If MRtn = 0 Then                   '初期位置にいない時はエラーにする
550     fErrorProcess(11,230,281,0)    'エラー停止
551     If M_20# = MNext% Then GoTo *ASSY_ERROR_END
552     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
553     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
554     If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
555 EndIf
556 '
557 '
558 'DVD無しPASSプログラムから通常プログラムに切替えた時の対策 2022.05.13 渡辺
559 PTemp = P_Curr
560 If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
561     If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
562         If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
563             Ovrd 50
564             Mov PInitialPosition
565             Ovrd 100
566         EndIf
567     EndIf
568 EndIf
569 '
570 '
571 *ReCheck1
572     M_Out(12912) = 1                  'Ver 0.4 追加　仮置き台フラグ立て(工程6優先のため工程5のフラグ監視の前に出力)
573     Mov PMechaOnJigGet_3    'タクト短縮のため初期位置変更
574     'Wait M_In(11920) = 0              'BaseUnit5仮置き台フラグ確認(処理位置移動2/9中村)
575     MRet = fTimeOutJudge(11920,0)      'BaseUnit5仮置き台フラグ確認(タイムアウト処理を追加22/09/29中村)
576     If MRet = 0 Then GoTo *ASSY_ERROR_END
577     If MRet = 2 Then GoTo *ReCheck1
578     M_Out(12912) = 1                  '仮置き台フラグ立て(念のため追加2/9中村)
579 '
580 'Ver 0.4 追加--------------------
581 If M_In(11920) = 1 Then GoTo *CompInitial4         '工程6が動作中の場合11920 = 1 ループ
582 'Ver 0.4 ここまで----------------           '工程6優先のため12912=0にしない
583 '
584 'Mov PInitialPosition   '1/20コメントアウト(中村)
585 '
586 '仮置き台回避点へ移動
587 Mov PMechaOnJigGet_2         '仮置き台回避点
588 '
589 '仮置き台からDVDメカを受け取る
590 'Wait M_In(12914) = 1              '供給許可を受信
591 'Wait M_In(11920) = 0              'BaseUnit5仮置き台フラグ確認(追加ここから10/1中村)
592 '
593     MRtn2 = 0                   'エラー時次へが押された時用
594 *RE_JIG_GET
595     M_20# = MClear%
596 '
597     M_Out(12912) = 1                  '仮置き台フラグ立て
598     If M_In(11278) = 1 Then          '方向の確認(CW端センサー)(追加ここまで10/1中村)
599         Mov PMechaOnJigGet1_1    '仮置き台上空
600         Ovrd 25
601         Mvs PMechaOnJigGet1      'DVDメカ受け取り位置
602         M_Out(12257) = 0         'DVDメカチャック開OFF
603         M_Out(12256) = 1         'DVDメカチャック閉ON
604 '        Wait M_In(11266) = 1     'DVDメカチャック閉検出
605         MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
606         If MRtn = 0 And MRtn2 = 0 Then             'DVDメカを把持できなかった場合チャックを開く(6/2中村)
607             M_Out(12256) = 0         'DVDメカチャック閉OFF
608             M_Out(12257) = 1         'DVDメカチャック開ON
609         EndIf
610         Mvs PMechaOnJigGet1_1    '仮置き台上空
611         Ovrd 100
612         Break
613     ElseIf M_In(11277) = 1 Then       '方向の確認(CCW端センサー)(追加ここから10/1中村)
614         Mov PMechaOnJigGet2_1    '仮置き台上空
615         Ovrd 25
616         Mvs PMechaOnJigGet2      'DVDメカ受け取り位置
617         M_Out(12257) = 0         'DVDメカチャック開OFF
618         M_Out(12256) = 1         'DVDメカチャック閉ON
619 '        Wait M_In(11266) = 1     'DVDメカチャック閉検出
620         MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
621         If MRtn = 0 And MRtn2 = 0 Then             'DVDメカを把持できなかった場合チャックを開く(6/2中村)
622             M_Out(12256) = 0         'DVDメカチャック閉OFF
623             M_Out(12257) = 1         'DVDメカチャック開ON
624         EndIf
625         Mvs PMechaOnJigGet2_1    '仮置き台上空
626         Ovrd 100
627         Break
628     EndIf
629 '
630     If MRtn = 1 Or MRtn2 = 1 Then GoTo *CompMechaGet1    'DVDメカチャックエラー処理(次へが押された時もエラー表示なしで先に進むように変更(6/2中村))
631 '    PTemp = P_Curr              '処理位置の変更1/12中村
632 '    Mvs PTemp , -150
633     Mov PMechaOnJigGet_2
634     fErrorProcess(11,269,294,0)'11,279,284,0から11,269,294,0に変更(6/2中村)
635 '    PTemp = P_Curr             '処理位置の変更1/12中村
636 '    Mvs PTemp , -150
637 '    Mov PMechaOnJigGet_2
638     If M_20# = MNext% Then
639         M_20# = MClear%
640         MRtn2 = 1
641     EndIf
642     If M_20# = MContinue% Then MRtn2 = 0
643     If M_20# = MAbout% Or M_20# = MNgProcess% Then
644         Mov PInitialPosition
645         Break
646     EndIf
647     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
648     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
649     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_JIG_GET
650 *CompMechaGet1
651 '
652     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
653 Mov PMechaOnJigGet_2            '仮置き台回避点
654     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
655 '
656 'DVDメカセンサー検出
657 '
658 'M_Out(12912) = 0                  '仮置き台フラグ回収(処理位置変更2/9中村)
659 '
660 '
661 '    ' 部品供給要求送信(処理位置変更3/31中村)
662     M_Out(12787) = 1
663 'DVDメカをねじロボ3に置く
664 Mov PMechaOnRoboSet_2        'ねじロボ回避点
665 '
666 'Wait M_In(11888) = 1         'ねじロボ3停止1まで待機
667 MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
668 If MRtn = 0 Then Mov PInitialPosition     '"イニシャルに戻る動き"
669 If MRtn = 0 Then GoTo *ASSY_ERROR_END
670 '
671 Mov PMechaOnRoboSet_1        'ねじロボ上空
672 Ovrd 25
673 Mvs PMechaOnRoboSet          'DVDメカ置き場
674 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止1～停止2まで)
675 '
676 'Wait M_In(11889) = 1         'ねじロボ3停止2まで待機
677 MRtn = fScrewTighenRoboCheck(11889)    '停止状態を受信する
678 'If MRtn = 0 Then
679 '    M_Out(12256) = 0             'DVDメカチャック閉OFF
680 '    M_Out(12257) = 1             'DVDメカチャック開ON
681 '    Mvs PMechaOnRoboSet_1
682 '    Mov PMechaOnRoboSet_2
683 '    Mov PInitialPosition
684 'EndIf
685 If MRtn = 0 Then GoTo *ASSY_ERROR_END   'その場で停止処理(故障の可能性)
686 '
687 *RE_ROBO_SET_1
688 '
689 M_Out(12256) = 0             'DVDメカチャック閉OFF
690 M_Out(12257) = 1             'DVDメカチャック開ON
691 '
692 'Wait M_In(11265) = 1         'DVDメカチャック開検出
693 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
694 If MRtn = 1 Then GoTo *CompRoboSet1
695 fErrorProcess(11,270,284,0)
696 If M_20# = MNext% Then M_20# = MClear%
697 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
698 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
699 If M_20# = MContinue% Then GoTo *RE_ROBO_SET_1
700 *CompRoboSet1
701 '
702 ''    ' 部品供給要求送信(処理位置変更3/31中村)
703 '    M_Out(12787) = 1
704 '
705 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止2～停止3まで)
706 '
707 'Wait M_In(11890) = 1         'ねじロボ3停止3まで待機
708 MRtn = fScrewTighenRoboCheck(11890)    '停止状態を受信する
709 MScrewRoboNgFlg% = 0
710 If MRtn = 0 Then MScrewRoboNgFlg% = 1
711 '
712 Mvs PMechaOnRoboSet_1        'ねじロボ上空
713 Ovrd 100
714 Mov PMechaOnRoboSet_2        'ねじロボ回避点
715 M_Out(12912) = 0                  '仮置き台フラグ回収(処理位置変更2/9中村)
716 '
717 '
718 If MScrewRoboNgFlg% = 1 Then Mov PInitialPosition   'ねじロボで停止かNGが押されていた場合
719 If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END   '
720 '
721 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止3～停止4まで)
722 '
723 ''    ' 部品供給要求送信(処理位置位置変更1/20中村)
724 '    M_Out(12787) = 1
725 '    '    ' 部品供給完了待ち
726 '    Wait M_In(11810) = 1
727 '
728 'DVD金具(R)を取る
729 *RE_R_GET_3
730 Mov PBracketRGet_3           '経路
731 Mov PBracketRGet_2           '金具(R)受け取り回避点
732 M_Out(12261) = 0             '金具(R)シリンダー戻OFF
733 M_Out(12260) = 1             '金具(R)シリンダー出ON
734 '
735     '    ' 部品供給完了待ち(処理変更2/27中村)
736 *RE_FEEDER_READY
737 '    Wait M_In(11810) = 1
738     fnAutoScreenComment(513)    '状態表示[部品供給待ち] 2022/04/26 渡辺
739 'MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '供給待ち
740 MRtn = frInCheck(11810,1,MSETTIMEOUT08&)   '供給待ち
741 If MRtn = 1 Then GoTo *CompFeederReady
742 '    ' 部品供給要求終了
743     M_Out(12787) = 0
744 fErrorProcess(11,289,284,0)
745 If M_20# = MNext% Then M_20# = MClear%
746 If M_20# = MAbout% Or M_20# = MNgProcess% Then
747     Mov PBracketRGet_2
748     Mov PBracketRGet_3
749     Mov PBracketRSet_3
750     Mov PInitialPosition1
751 EndIf
752 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
753 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
754 '    ' 部品供給要求
755     M_Out(12787) = 1
756 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
757 *CompFeederReady
758 '    ' 部品供給要求終了
759     M_Out(12787) = 0
760 '
761     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
762 Mov PBracketRGet_1           '金具(R)受け取り上空
763 '
764 *RE_R_GET_1
765 '
766 If M_20# = MContinue% Then
767     M_Out(12261) = 0             '金具(R)シリンダー戻OFF
768     M_Out(12260) = 1             '金具(R)シリンダー出ON
769     M_20# = MClear%
770 EndIf
771 '
772 'Wait M_In(11272) = 1         '金具(R)シリンダー出端検出
773     fnAutoScreenComment(513)    '状態表示[部品供給待ち] 2022/04/26 渡辺
774 MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '金具(R)シリンダー出端検出
775 If MRtn = 1 Then GoTo *CompRGet1
776 fErrorProcess(11,275,284,0)
777 If M_20# = MNext% Then M_20# = MClear%
778 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
779 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
780 If M_20# = MContinue% Then GoTo *RE_R_GET_1
781 *CompRGet1
782 '
783 Ovrd 25
784 '
785 *RE_R_GET_2
786 '
787     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
788 Mvs PBracketRGet             '金具(R)受け取り位置
789 '
790 '
791 M_Out(12252) = 0             '真空OFFバルブOFF
792 M_Out(12253) = 0             '真空破壊バルブOFF(念のため)
793 M_Out(12251) = 1             '真空ONバルブON
794 '
795 'Wait M_In(11270) = 1         '金具(R)吸着センサーON検出
796 MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   '金具(R)吸着センサーON検出
797 If MRtn = 1 Then GoTo *CompRGet2
798 Mvs PBracketRGet_1           '金具(R)受け取り上空
799 fErrorProcess(11,279,295,0)  '284→295へ変更6/2中村
800 If M_20# = MNext% Then M_20# = MClear%
801 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
802 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
803 If M_20# = MContinue% Then GoTo *RE_R_GET_2
804 *CompRGet2
805 '
806 Mvs PBracketRGet_1           '金具(R)受け取り上空
807 Ovrd 100
808 Mov PBracketRGet_2           '金具(R)受け取り回避点
809 Mov PBracketRGet_3           '経路
810 '
811 'DVD金具(R)を置く
812 Mov PBracketRSet_3           '回避点
813 MRtn = frInCheck(11270,1,MSETTIMEOUT05&)              'もう一度金具(R)吸着センサーON検出
814 If MRtn = 1 Then GoTo *CompRGet3
815 fErrorProcess(11,279,295,0)  '284→295に変更6/2中村
816 If M_20# = MNext% Then M_20# = MClear%
817 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
818 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
819 If M_20# = MContinue% Then GoTo *RE_R_GET_3
820 *CompRGet3
821 '
822 'Wait M_In(11891) = 1         'ねじロボ3停止4まで待機
823 MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
824 If MRtn = 0 Then Mov PInitialPosition
825 If MRtn = 0 Then GoTo *ASSY_ERROR_END
826 '
827 Mov PBracketRSet_2           '金具(R)置き位置回避点
828 Mvs PBracketRSet_1           '金具(R)置き位置上空
829 *RE_R_SET_1                  '戻し位置(6/2変更,中村)
830 Ovrd 10
831 Mvs PBracketRSet             '金具(R)置き位置
832 Dly 0.1
833 '
834 '*RE_R_SET_1                  '戻し位置(6/2変更,中村)
835 '
836 M_Out(12251) = 0             '真空ONバルブOFF
837 M_Out(12252) = 1             '真空OFFバルブON
838 M_Out(12253) = 1             '真空破壊バルブON
839 '
840 MRtn = frInCheck(11270,0,MSETTIMEOUT05&)
841 '
842 Dly 0.2
843 'M_Out(12253) = 0             '真空破壊バルブOFF
844 '
845 If MRtn = 1 Then GoTo *CompRSet1
846 Mvs PBracketRSet_1           '金具(R)置き位置上空
847 M_Out(12253) = 0             '真空破壊バルブOFF
848 fErrorProcess(11,296,297,0)  '236,284→296,297に変更6/2中村
849 If M_20# = MNext% Then M_20# = MClear%
850 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
851 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
852 If M_20# = MContinue% Then GoTo *RE_R_SET_1
853 *CompRSet1
854 '
855 'M_Out(12260) = 0             '金具(R)シリンダー出OFF(戻し位置変更1/20中村)
856 'M_Out(12261) = 1             '金具(R)シリンダー戻ON
857 ''
858 '*RE_R_SET_2
859 ''
860 ''Wait M_In(11271) = 1         '金具(R)シリンダー戻端検出
861 'MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   '金具(R)シリンダー戻端検出
862 'If MRtn = 1 Then GoTo *CompRSet2
863 'fErrorProcess(11,276,284,0)
864 'If M_20# = MNext% Then M_20# = MClear%
865 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
866 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
867 'If M_20# = MContinue% Then GoTo *RE_R_SET_2
868 '*CompRSet2
869 '
870 Ovrd 1                       'ブラケット置き位置ずれ対策(5/17中村)
871 Mvs PBracketRSet , -5        'ブラケット置き位置ずれ対策(5/17中村)
872 Ovrd 100
873 Mvs PBracketRSet_1           '金具(R)置き位置上空
874 M_Out(12253) = 0             '真空破壊バルブOFF
875 Ovrd 100
876 Mov PBracketRSet_2           '金具(R)置き位置回避点
877 Mov PBracketRSet_3           '回避点
878 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止4～停止5まで)
879 '
880 *RE_R_SET_2
881 '
882 M_Out(12260) = 0             '金具(R)シリンダー出OFF(戻し位置変更1/20中村)
883 M_Out(12261) = 1             '金具(R)シリンダー戻ON
884 '
885 'Wait M_In(11271) = 1         '金具(R)シリンダー戻端検出
886 MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   '金具(R)シリンダー戻端検出
887 If MRtn = 1 Then GoTo *CompRSet2
888 fErrorProcess(11,276,284,0)
889 If M_20# = MNext% Then M_20# = MClear%
890 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
891 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
892 If M_20# = MContinue% Then GoTo *RE_R_SET_2
893 *CompRSet2
894 '
895 '
896 '置き位置画像検査(処理位置移動)
897 'Wait M_In(11892) = 1         'ねじロボ3停止5まで待機(画像検査からF側ブラケット置き)
898 'MRtn = fScrewTighenRoboCheck(11892)    '停止状態を受信する
899 'If MRtn = 0 Then Mov PInitialPosition
900 'If MRtn = 0 Then GoTo *ASSY_ERROR_END
901 '
902 'Wait M_In(11920) = 0              'BaseUnit5仮置き台フラグ確認(追加ここから10/1中村)
903 ''
904 'M_Out(12912) = 1                  '仮置き台フラグ立て(衝突防止)
905 ''
906 ''Mov PBracketRCheck_2         '経路
907 ''Mov PBracketRCheck           '検査位置
908 ''
909 '*RE_R_CHECK
910 ''If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck1
911 'PInspPosition(1) = PBracketRCheck1
912 'MInspGroup%(1) = 2
913 'PInspPosition(2) = PBracketRCheck2
914 'MInspGroup%(2) = 3
915 'MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
916 'If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck1
917 'If MRtn = 0 Then
918 '    MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
919 'EndIf
920 'If MRtn = 1 Then GoTo *CompRCheck
921 'fErrorProcess(11,43,46,0)
922 'If M_20# = MNext% Then M_20# = MClear%
923 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
924 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
925 'If M_20# = MContinue% Then GoTo *RE_R_CHECK
926 '*CompRCheck
927 '*SkipCheck1
928 '
929 'M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止5～停止6まで)
930 '
931 'Mov PBracketRCheck
932 '
933 'DVD金具(F)を取る
934 *RE_F_GET_3
935 'M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止5～停止6まで)(処理位置変更1/20中村)
936 Mov PBracketFGet_3           '回避点
937 '
938 M_Out(12912) = 0                  '仮置き台フラグ回収(衝突防止)
939 '
940 Mov PBracketFGet_2           '金具(F)受け取り回避点
941 'Mov PBracketFGet_1           '金具(F)受け取り上空(移動タイミング変更1/20中村)
942 '
943 *RE_F_GET_1
944 '
945 M_Out(12259) = 0             '金具(F)シリンダー戻OFF
946 M_Out(12258) = 1             '金具(F)シリンダー出ON
947 '
948 Mov PBracketFGet_1           '金具(F)受け取り上空(移動タイミング変更1/20中村)
949 '
950 'Wait M_In(11269) = 1         '金具(F)シリンダー出端検出
951 MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   '金具(F)シリンダー出端検出
952 If MRtn = 1 Then GoTo *CompFGet1
953 fErrorProcess(11,277,284,0)
954 If M_20# = MNext% Then M_20# = MClear%
955 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
956 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
957 If M_20# = MContinue% Then GoTo *RE_F_GET_1
958 *CompFGet1
959 '
960 Ovrd 25
961 *RE_F_GET_2
962 Mvs PBracketFGet             '金具(F)受け取り位置
963 '
964 '
965 '
966 M_Out(12249) = 0             '真空OFFバルブOFF
967 M_Out(12250) = 0             '真空破壊バルブOFF
968 M_Out(12248) = 1             '真空ONバルブON
969 '
970 'Wait M_In(11267) = 1         '金具(F)吸着センサーON検出
971 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)   '金具(F)吸着センサーON検出
972 If MRtn = 1 Then GoTo *CompFGet2
973 Mvs PBracketFGet_1           '金具(F)受け取り上空
974 fErrorProcess(11,280,295,0)  '284→295に変更6/2中村
975 If M_20# = MNext% Then M_20# = MClear%
976 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
977 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
978 If M_20# = MContinue% Then GoTo *RE_F_GET_2
979 *CompFGet2
980 '
981 Mvs PBracketFGet_1           '金具(F)受け取り上空
982 'Ovrd 100
983 Mov PBracketFGet_2           '金具(F)受け取り回避点
984 Mov PBracketFGet_3           '回避点
985 '
986 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)          'もう一度金具(F)吸着センサーON検出
987 If MRtn = 1 Then GoTo *CompFGet3
988 fErrorProcess(11,280,295,0)  '284→295に変更6/2中村
989 If M_20# = MNext% Then M_20# = MClear%
990 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
991 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
992 If M_20# = MContinue% Then GoTo *RE_F_GET_3
993 *CompFGet3
994 '
995 'DVD金具(F)を置く
996 Mov PBracketFSet_3           '回避点
997 '    ' 部品供給要求終了
998     M_Out(12787) = 0
999 '    ' 部品取得完了送信(パルス)
1000     M_Out(12800) = 1 Dly 0.5
1001     '
1002 Ovrd 70
1003 Mov PBracketFSet_2           '金具(F)置き位置回避点
1004 'Wait M_In(11893) = 1         'ねじロボ3停止5まで待機
1005 MRtn = fScrewTighenRoboCheck(11892)    '停止状態を受信する
1006 If MRtn = 0 Then Mov PInitialPosition1  '停止位置に移動
1007 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1008 '
1009     fnAutoScreenComment(533)    '状態表示[工程５のロボ動作終了待ち] 2022/04/26 渡辺
1010 Wait M_In(11920) = 0              'BaseUnit5仮置き台フラグ確認(追加ここから10/1中村)
1011 '
1012 M_Out(12912) = 1                  '仮置き台フラグ立て(衝突防止)
1013 '
1014     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1015 'Ovrd 70
1016 'Mov PBracketFSet_2           '金具(F)置き位置回避点
1017 Mvs PBracketFSet_1           '金具(F)置き位置上空
1018 *RE_F_SET_1                  '戻し位置(変更6/2中村)
1019 Ovrd 10
1020 Mvs PBracketFSet             '金具(F)置き位置
1021 Dly 0.2
1022 '
1023 '*RE_F_SET_1                  '戻し位置(変更6/2中村)
1024 '
1025 M_Out(12248) = 0             '真空ONバルブOFF
1026 M_Out(12249) = 1             '真空OFFバルブON
1027 M_Out(12250) = 1             '真空破壊バルブON
1028 '
1029 MRtn = frInCheck(11267,0,MSETTIMEOUT05&)
1030 Dly 0.2
1031 'M_Out(12250) = 0             '真空破壊バルブOFF
1032 '
1033 If MRtn = 1 Then GoTo *CompFSet1
1034 M_Out(12250) = 0             '真空破壊バルブOFF
1035 fErrorProcess(11,296,297,0)  '236,284→296,297に変更6/2中村
1036 If M_20# = MNext% Then M_20# = MClear%
1037 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1038 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1039 If M_20# = MContinue% Then GoTo *RE_F_SET_1
1040 *CompFSet1
1041 '
1042 '
1043 '
1044 '*RE_F_SET_2'(戻し位置変更)
1045 ''
1046 'M_Out(12258) = 0             '金具(F)シリンダー出OFF
1047 'M_Out(12259) = 1             '金具(F)シリンダー戻ON
1048 ''
1049 ''Wait M_In(11268) = 1         '金具(F)シリンダー戻端検出
1050 'MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   '金具(F)シリンダー戻端検出
1051 'If MRtn = 1 Then GoTo *CompFSet2
1052 'fErrorProcess(11,277,284,0)
1053 'If M_20# = MNext% Then M_20# = MClear%
1054 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1055 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1056 'If M_20# = MContinue% Then GoTo *RE_F_SET_2
1057 '*CompFSet2
1058 '
1059 Ovrd 1                       'ブラケット置き位置ずれ対策(5/17中村)
1060 Mvs PBracketFSet , -5        'ブラケット置き位置ずれ対策(5/17中村)
1061 Ovrd 100
1062 Mvs PBracketFSet_1           '金具(F)置き位置上空
1063 M_Out(12250) = 0             '真空破壊バルブOFF
1064 Ovrd 100
1065 Mvs PBracketFSet_2           '金具(F)置き位置回避点
1066 M_Out(12258) = 0             '金具(F)シリンダー出OFF(追加3/31中村)
1067 M_Out(12259) = 1             '金具(F)シリンダー戻ON(追加3/31中村)
1068 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止5～停止6まで)
1069 Mov PBracketFSet_3           '回避点
1070 M_Out(12912) = 0             '仮置き台フラグ回収
1071 'M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止5～停止6まで)
1072 *RE_F_SET_2'(戻し位置変更)
1073 '
1074 M_Out(12258) = 0             '金具(F)シリンダー出OFF
1075 M_Out(12259) = 1             '金具(F)シリンダー戻ON
1076 '
1077 'Wait M_In(11268) = 1         '金具(F)シリンダー戻端検出
1078 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   '金具(F)シリンダー戻端検出
1079 If MRtn = 1 Then GoTo *CompFSet2
1080 fErrorProcess(11,277,284,0)
1081 If M_20# = MNext% Then M_20# = MClear%
1082 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1083 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1084 If M_20# = MContinue% Then GoTo *RE_F_SET_2
1085 *CompFSet2
1086 '
1087 '----------PIAS先読み----------
1088 '概要：読みに行くかどうかを決めるときにタイムアウトで先に進めるようにするタイマー
1089     Mov PTicketRead_2
1090     Mov PTicketRead_1
1091     M_22# = MClear%
1092     M_20# = MClear%
1093     M_Timer(4) = 0
1094     MloopFlg = 0
1095     While MloopFlg = 0
1096         MCrtTime& = M_Timer(4)
1097         If M_In(11354) = 1 Then
1098             MloopFlg = 1
1099         ElseIf MCrtTime& > 5000 Then    '暫定5秒(分割しているのは秒数調整しやすくしている為)
1100             MloopFlg = 1
1101         EndIf
1102     WEnd
1103     MRtn = 0
1104     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
1105         If M_In(11354) = 1 Then         '組立開始がONなら
1106             M_Out(12346) = 1 Dly 0.5        ' 組立開始を受信
1107             Mvs PTicketRead
1108             MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
1109 '        '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1110 '        '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1111             If MRtn = 1 Then M_22# = MAssyOK%
1112             If M_20# = MContinue% Then M_22# = MContinue%
1113             If M_20# = MPass% Then M_22# = MPass%
1114             If M_20# = MNext% Then M_22# = MPass%
1115         EndIf
1116     EndIf
1117     If M_20# = MNgProcess% Or M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1118     M_20# = MClear%
1119     Mov PTicketRead_1
1120     Mov PTicketRead_2
1121     Mov PBracketFSet_3
1122 '
1123 '置き位置画像検査
1124 'Wait M_In(11893) = 1         'ねじロボ3停止6まで待機
1125 MRtn = fScrewTighenRoboCheck(11893)    '停止状態を受信する
1126 If MRtn = 0 Then Mov PInitialPosition1  '停止位置に移動
1127 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1128 '
1129 If M_In(11369) = 0 Then M_Out(12912) = 1    '画像判定未使用時ここでフラグ立て6/23中村
1130 If M_In(11369) = 0 Then GoTo *SkipCheck1    '画像判定未使用時ジャンプ
1131 *RE_FLG_SET_1
1132 'Wait M_In(11920) = 0                'BaseUnit5仮置き台フラグ確認(追加2/27中村)
1133 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5仮置き台フラグ確認(タイムアウト処理を追加22/09/29中村)
1134 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1135 If MRtn = 2 Then GoTo *RE_FLG_SET_1
1136 M_Out(12912) = 1                    '仮置き台フラグ立て
1137 'Wait M_In(11920) = 0              'Ver 0.4 追加
1138 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5仮置き台フラグ確認(タイムアウト処理を追加22/09/29中村)
1139 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1140 If MRtn = 2 Then GoTo *RE_FLG_SET_1
1141 Dly 0.3
1142 'If M_In(11920) = 1 Then M_Out(12912) = 0     'Ver 0.4 コメントアウト　工程6優先
1143 If M_In(11920) = 1 Then GoTo *RE_FLG_SET_1   '
1144 '
1145 'Wait M_In(11920) = 0              'BaseUnit5仮置き台フラグ確認(追加ここから10/1中村)
1146 ''
1147 'M_Out(12912) = 1                  '仮置き台フラグ立て(衝突防止)
1148 '
1149 'Mov PBracketFCheck_2         '経路
1150 'Mov PBracketFCheck           '検査位置
1151 *RE_F_CHECK
1152 'If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck2
1153 PInspPosition(1) = PBracketFCheck1
1154 MInspGroup%(1) = 4
1155 PInspPosition(2) = PBracketFCheck2
1156 MInspGroup%(2) = 5
1157 MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1158 If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck1
1159 If MRtn = 0 Then
1160     MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1161 EndIf
1162 If MRtn = 1 Then GoTo *CompFCheck
1163 fErrorProcess(11,43,46,0)
1164 If M_20# = MNext% Then M_20# = MClear%
1165 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1166     Mov PBracketFCheck
1167     Mov PInitialPosition
1168     M_Out(12912) = 0
1169 EndIf
1170 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1171 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1172 If M_20# = MContinue% Then GoTo *RE_F_CHECK
1173 *CompFCheck
1174 *SkipCheck1
1175 '
1176 '
1177 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止6～停止7まで)
1178 If M_In(11369) = 1 Then Mov PBracketRCheck1
1179 'Wait M_In(11894) = 1         'ねじロボ3停止7まで待機
1180 MRtn = fScrewTighenRoboCheck(11894)    '停止状態を受信する
1181 If MRtn = 0 Then
1182     Mov PBracketFCheck
1183     Mov PInitialPosition
1184     M_Out(12912) = 0
1185 EndIf
1186 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1187 '
1188 If M_In(11369) = 0 Then GoTo *SkipCheck2    '画像判定未使用時ジャンプ
1189 *RE_R_CHECK
1190 PInspPosition(1) = PBracketRCheck1
1191 MInspGroup%(1) = 2
1192 PInspPosition(2) = PBracketRCheck2
1193 MInspGroup%(2) = 3
1194 MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1195 If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck2
1196 If MRtn = 0 Then
1197     MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1198 EndIf
1199 If MRtn = 1 Then GoTo *CompRCheck
1200 fErrorProcess(11,43,46,0)
1201 If M_20# = MNext% Then M_20# = MClear%
1202 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1203     Mov PBracketFCheck
1204     Mov PInitialPosition
1205     M_Out(12912) = 0
1206 EndIf
1207 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1208 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1209 If M_20# = MContinue% Then GoTo *RE_R_CHECK
1210 *CompRCheck
1211 *SkipCheck2
1212 '
1213 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止7～停止8まで)処理位置変更1/20中村
1214 '
1215 If M_In(11369) = 1 Then Mov PBracketFCheck
1216 '
1217 '
1218 'ねじロボ3のDVDassyを取る
1219 'M_Out(12866) = 1 Dly 0.5                   'ねじロボ3に再開要求(停止7～停止8まで)処理位置変更1/20中村
1220 Mov PMechaOnRoboGet_3                       '向き合わせ
1221 'M_Out(12912) = 0                            '仮置き台フラグ回収(6/23暫定コメントアウト(中村))
1222 '
1223 Mov PMechaOnRoboGet_2                       '回避点(処理位置変更1/20中村)
1224 '
1225 'Wait M_In(11895) = 1                       'ねじロボ3停止8まで待機
1226 MRtn = fScrewTighenRoboCheck(11895)         '停止状態を受信する
1227 If MRtn = 0 Then Mov PInitialPosition1      '停止位置に移動
1228 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1229 '
1230 *RE_FLG_SET_2
1231     fnAutoScreenComment(533)    '状態表示[工程５のロボ動作終了待ち] 2022/04/26 渡辺
1232 'Wait M_In(11920) = 0                        'BaseUnit5仮置き台フラグ確認(処理追加2/9中村)
1233 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5仮置き台フラグ確認(タイムアウト処理を追加22/09/29中村)
1234 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1235 If MRtn = 2 Then GoTo *RE_FLG_SET_2
1236 '
1237 M_Out(12912) = 1                            '仮置き台フラグ立て(衝突防止)
1238 Dly 0.3
1239 'Wait M_In(11920) = 0                        'Ver 0.4 追加　工程6優先
1240 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5仮置き台フラグ確認(タイムアウト処理を追加22/09/29中村)
1241 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1242 If MRtn = 2 Then GoTo *RE_FLG_SET_2
1243 'If M_In(11920) = 1 Then M_Out(12912) = 0   'Ver 0.4 コメントアウト
1244 If M_In(11920) = 1 Then GoTo *RE_FLG_SET_2
1245 '
1246 'Mov PMechaOnRoboGet_2                      '処理位置変更(1/20中村)
1247 Mov PMechaOnRoboGet_1                       'ねじロボ上空
1248 Ovrd 25
1249 '
1250 *RE_ROBO_GET_1
1251 '
1252     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1253 Mvs PMechaOnRoboGet          'DVDメカ受け取り位置
1254 Dly 0.1
1255 M_Out(12257) = 0             'DVDチャック開OFF
1256 M_Out(12256) = 1             'DVDチャック閉ON
1257 '
1258 'Wait M_In(11266) = 1         'DVDメカチャック閉検出
1259 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
1260 If MRtn = 1 Then GoTo *CompRoboGet1
1261 fErrorProcess(11,269,284,0)
1262 If M_20# = MNext% Then M_20# = MClear%
1263 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1264 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1265 If M_20# = MContinue% Then GoTo *RE_ROBO_GET_1
1266 *CompRoboGet1
1267 '
1268 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止8～停止9まで)
1269 '
1270 'Wait M_In(11876) = 1         'ねじロボ3ねじ締め完了を受信
1271 '
1272 'Wait M_In(11896) = 1         'ねじロボ3停止9まで待機
1273 MRtn = fScrewTighenRoboCheck(11876)    '停止状態を受信する
1274 'If MRtn = 0 Then
1275 '    M_Out(12256) = 0             'DVDメカチャック閉OFF
1276 '    M_Out(12257) = 1             'DVDメカチャック開ON
1277 '    Mvs PMechaOnRoboGet_1
1278 '    Mov PMechaOnRoboGet_2
1279 '    Mov PInitialPosition
1280 'EndIf
1281 If MRtn = 0 Then GoTo *ASSY_ERROR_END   'その場で停止
1282 '
1283 'Wait M_In(11264) = 1         'DVDメカ検出
1284 *RE_ROBO_GET_2
1285 MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   'DVDメカ検出
1286 If MRtn = 1 Then GoTo *CompRoboGet2
1287 fErrorProcess(11,273,284,0)
1288 If M_20# = MNext% Then M_20# = MClear%
1289 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1290 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1291 If M_20# = MContinue% Then GoTo *RE_ROBO_GET_2
1292 *CompRoboGet2
1293 '
1294 Mvs PMechaOnRoboGet_1        'ねじロボ上空
1295 Ovrd 100
1296 Mov PMechaOnRoboGet_2        'ねじロボ回避点
1297 'M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止9～ねじ締め完了まで)
1298 M_Out(12912) = 0                  '仮置き台フラグ回収(追加10/1中村)
1299 '
1300 Mov PTicketRead_2
1301 M_Out(12868) = 1 Dly 0.5     'ねじロボ3ねじ締め完了を送信(処理位置変更3/26中村)
1302 '
1303     If M_22# = MAssyOK% Then GoTo *CompRead
1304 *IRREGULAR      '開始時Assy完了後DVDメカ把持ジャンプ先
1305     Mov PTicketRead_1
1306 '
1307 'DVDassyをパレットへ置く
1308 '    Wait M_In(11218) = 1         'パレットが上昇していることを確認
1309     If M_22# <> MClear% And M_22# <> MIrregular% Then GoTo *RE_PIAS_CHECK
1310     fnAutoScreenComment(95)    '状態表示[パレット搬送待機中] 2022/04/26 渡辺
1311     Wait M_In(11354) = 1         ' 組立開始信号が出ていることを確認
1312     M_Out(12346) = 1 Dly 0.5         ' 組立開始を受信
1313     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1314 *RE_PIAS_CHECK
1315     M_20# = MClear%                 '初期化
1316     MRtn = 1
1317     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
1318         If M_22# = MClear% Or M_22# = MContinue% Or M_22# = MIrregular% Then'PIASチェックにて未検査かリトライ時に入る
1319                 Mvs PTicketRead
1320                 MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
1321 '        '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1322 '        '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1323         EndIf
1324     EndIf
1325 '
1326     If M_22# = MPass% Then M_20# = MPass%
1327     M_22# = MClear%
1328     If MRtn = 1 And M_20# = MClear% Then GoTo *CompRead
1329 '    fErrorProcess(11,17,0,0)
1330 '    Dly 10                                      'デバッグ用
1331 '    If M_In(11359) = 1 Then M_22# = MIrregular%'デバッグ用
1332 '    If M_22# = MIrregular% Then *ASSY_ERROR_END  'デバッグ用
1333 '    If M_20# = MPass% Then M_20# = MClear%      'デバッグ用
1334     If M_20# = MPass% Then
1335         M_22# = MIrregular%
1336         M_20# = MAssyOK%
1337         Dly 0.1
1338         Mvs PTicketRead_1                         '22/04/11 追加 渡辺
1339     EndIf
1340     If M_20# = MAssyOK% Then GoTo *AssyEnd
1341     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1342     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1343     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1344     If M_20# = MContinue% Then GoTo *RE_PIAS_CHECK
1345 *CompRead
1346 '
1347 'Mov PTicketRead_1
1348 Accel 25 , 25                'デバッグ中
1349 Mov PMechaOnPltSet_2         'パレット回避点
1350 Accel 100 , 100
1351 Mov PMechaOnPltSet_1         'パレット上空
1352 '
1353 '置く前にDVDを持っているか？確認    2022/04/11 渡辺
1354 'DVDを持っていおらず、チャック閉の場合、閉端の信号はONしない
1355 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
1356 If MRtn = 0 Then                           'チャックの閉端がOFFの場合
1357     fErrorProcess(11,269,284,0)
1358     If M_20# = MNext% Then
1359         M_20# = MClear%
1360         GoTo *CompPltSet
1361     EndIf
1362     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1363     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1364     If M_20# = MContinue% Then GoTo *CompRead
1365 EndIf
1366 '
1367 Dly 0.1
1368 Ovrd 10
1369 Mvs PMechaOnPltSet           'DVDメカ置き場所
1370 Dly 0.1
1371 '
1372 *RE_PLT_SET
1373 '
1374 M_Out(12256) = 0             'DVDメカチャック閉OFF
1375 M_Out(12257) = 1             'DVDメカチャック開ON
1376 '
1377 'Wait M_In(11265) = 1         'DVDメカチャック開検出
1378 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1379 If MRtn = 1 Then GoTo *CompPltSet
1380 fErrorProcess(11,270,284,0)
1381 If M_20# = MNext% Then M_20# = MClear%
1382 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1383 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1384 If M_20# = MContinue% Then GoTo *RE_PLT_SET
1385 *CompPltSet
1386 M_22# = MClear%
1387 '
1388 Ovrd 100
1389 Mvs PMechaOnPltSet_1         'パレット上空
1390 'Ovrd 100
1391     MRtn = FnCtlValue2(2)          '組立ＯＫ＋１  2022/04/28 渡辺
1392 Mov PMechaOnJigGet_3
1393     MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1394 'Mov PInitialPosition   'コメントアウト(1/20中村)
1395 '
1396 'Wait M_In(11876) = 1         'ねじロボ3ねじ締め完了を受信
1397 'MRtn = frInCheck(11876,1,MSETTIMEOUT05&)   'ねじロボ3ねじ締め完了を受信・
1398 'If MRtn = 0 Then
1399 '    fErrorProcess()         'エラー処理
1400 'EndIf
1401 'M_Out(12868) = 1 Dly 0.5     'ねじロボ3ねじ締め完了を送信(処理位置変更3/26中村)
1402 'チケットID書き込み
1403 'If M_20# <> MPass% Then M_20# = MAssyOK%
1404 M_20# = MAssyOK%
1405 '
1406 *ASSY_ERROR_END
1407 *AssyEnd
1408 *fnAssyStart_FEndPosi
1409 FEnd
1410 '
1411 '■fnPiasCheck
1412 ''' <summary>
1413 ''' PIASチケット読込み
1414 ''' </summary>
1415 ''' <returns>   0 : NG
1416 '''             1 : OK(読込み完了)
1417 ''' </returns>
1418 ''' <remarks>
1419 ''' Date   : 2021/07/07 : M.Hayakawa
1420 ''' </remarks>'
1421 Function M% fnPiasCheck
1422     fnPiasCheck = 0
1423     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1424     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1425 '
1426 *RETRY_PIAS
1427     M_20# = MClear%
1428     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1429     '
1430     '【IDチケット読み込み】
1431     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1432     MInspGroup%(1) = 1              '検査G番号
1433     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1434 '
1435     'エラーの場合
1436     If MRtn <> 1 Then
1437         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1438         If MRtn <> 1 Then
1439             'D720 -> D1300 コピー要求
1440             M_Out(12565) = 1
1441             Dly 0.5
1442             M_Out(12565) = 0
1443             'エラー処理記述
1444             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1445             'GOT KEY入力待ち
1446             MKeyNumber = fnKEY_WAIT()
1447         '
1448             Select MKeyNumber
1449                 Case MNext%         '次へを選択した場合
1450                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1451                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1452 '                    GoTo *fnPiasCheck_End                   'PIASチェック終了(関数外部で分岐するよう変更3/26中村)
1453                     Break
1454                 Case MAbout%        '停止を選択した場合
1455                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1456                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1457 '                    GoTo *fnPiasCheck_End                   'PIASチェック終了(関数外部で分岐するよう変更3/26中村)
1458                     Break
1459                 Case MNgProcess%    'NGを選択した場合
1460                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1461                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1462 '                    GoTo *fnPiasCheck_End                   'PIASチェック終了(関数外部で分岐するよう変更3/26中村)
1463                     Break
1464                 Case MContinue%     '継続を選択した場合
1465                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1466                     M_20# = MContinue%
1467 '                    GoTo *RETRY_PIAS                        'PIASチェックリトライ(関数外部で分岐するよう変更3/26中村)
1468                     Break
1469             End Select
1470         EndIf
1471     EndIf
1472     If M_20# <> MClear% Then GoTo *fnPiasCheck_End
1473 '
1474 '----------D720 -> D1300 コピー要求----------
1475     M_Out(12565) = 1
1476     Dly 0.5
1477     M_Out(12565) = 0
1478 '----------通信確認をする----------
1479     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1480     MRtn = 0                ' 初期化
1481     M_20# = MClear%         ' 初期化
1482     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1483     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）(関数外部で分岐するように変更3/26中村)
1484     If MRtn <> 1 Then GoTo *fnPiasCheck_End
1485 '        If M_20# = MContinue% Then
1486 '            GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1487 '        Else
1488 '            GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1489 '        EndIf
1490 '    EndIf
1491 '----------工程抜け確認----------
1492     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1493     MRtn = 0                ' 初期化
1494     M_20# = MClear%         ' 初期化
1495     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1496     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）(関数外部で分岐するように変更3/26中村)
1497     If MRtn <> 1 Then GoTo *fnPiasCheck_End
1498 '        If M_20# = MContinue% Then
1499 '            GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1500 '        Else
1501 '            GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1502 '        EndIf
1503 '    EndIf
1504     '
1505     fnPiasCheck = 1
1506     *fnPiasCheck_End
1507 FEnd
1508 '
1509 '■fnPCComuCheck
1510 ''' <summary>
1511 ''' PC-PLC通信チェック
1512 ''' </summary>
1513 ''' <returns>   0 : NG
1514 '''             1 : OK(読込み完了)
1515 ''' </returns>
1516 ''' <remarks>
1517 ''' Date   : 2021/07/07 : M.Hayakawa
1518 ''' </remarks>'
1519 Function M% fnPCComuCheck
1520     fnPCComuCheck = 0
1521     MJudge% = 0                                  '初期化
1522     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1523     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1524     '
1525     For MStaNo = 0 To 5
1526         '
1527         If M_In(MIN_PIAS_ComOK%) = 1 Then
1528             'PC通信OK(M400)
1529             MJudge% = MOK%
1530             MStaNo = 5
1531             Break
1532         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1533             'toRBT_通信確認time out
1534             MJudge% = MNG%
1535             MCommentD1001 = 15
1536             MCommentD1002 = 21
1537             MStaNo = 5
1538             Break
1539         Else
1540             'toRBT_通信確認time out
1541             MJudge% = MNG%
1542             MCommentD1001 = 14
1543             MCommentD1002 = 21
1544             Break
1545         EndIf
1546     Next MStaNo
1547     '
1548     '上記で返信フラグを受信してからPC通信確認OFF
1549     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1550     '
1551     'エラー画面
1552     If MJudge% <> MOK% Then
1553         M_20# = MClear%     '初期化
1554         'エラー処理記述
1555         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1556         'GOT KEY入力待ち
1557         MKeyNumber = fnKEY_WAIT()
1558         '
1559         If MKeyNumber = MAbout% Then            '停止を選択した場合
1560             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1561             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1562             Break
1563         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1564             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1565             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1566             Break
1567         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1568             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1569             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1570             Break
1571         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1572             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1573             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1574             Break
1575         EndIf
1576     Else
1577         'OKの場合
1578         fnPCComuCheck = 1
1579     EndIf
1580 FEnd
1581 '
1582 '■fnProcessCheck
1583 ''' <summary>
1584 ''' 工程抜け確認
1585 ''' </summary>
1586 ''' <returns>    1：工程履歴OK     0：異常終了
1587 '''             -1：前工程履歴NG  -2：自工程履歴あり
1588 '''             -3：モデル仕向NG  -4：タイムアウト
1589 '''             -5：履歴処理エラー
1590 ''' </returns>
1591 ''' <remarks>
1592 ''' Date   : 2021/07/07 : M.Hayakawa
1593 ''' </remarks>'
1594 Function M% fnProcessCheck
1595     fnProcessCheck = 0
1596     MJudge% = MNG%      '一旦NGを初期化とする
1597 '----------工程抜け確認----------
1598     MCommentD1001 = 0   'コメント初期化
1599     For MStaNo = 0 To 5
1600         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1601         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1602         '
1603         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1604             MJudge% = MOK%
1605             fnAutoScreenComment(85)     ' AUTO画面
1606             MStaNo = 5
1607             Break
1608         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1609             MFlgLoop% = 0
1610             MJudge% = MNG%
1611             MCommentD1001 = 27
1612             MCommentD1002 = 22
1613             fnAutoScreenComment(94)     ' AUTO画面
1614             fnProcessCheck = -2         ' NGは-2を返す
1615             MStaNo = 5
1616             Break
1617         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1618            MJudge% = MNG%
1619             MCommentD1001 = 31
1620             MCommentD1002 = 22
1621             fnAutoScreenComment(83)     ' AUTO画面
1622             fnProcessCheck = -3         ' NGは-3を返す
1623             MStaNo = 5
1624             Break
1625         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1626             '履歴NGは直ぐに終了せず繰り返し確認を行う
1627             '前工程の書込みが終了していない可能性があるため
1628             MJudge% = MNG%
1629             MCommentD1001 = 32
1630             MCommentD1002 = 22
1631             fnAutoScreenComment(84)     ' AUTO画面
1632             fnProcessCheck = -1         ' NGは-1を返す
1633             Dly 1.0
1634             '工程抜け確認OFF
1635             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1636             Dly 1.0
1637            'MStaNo = 5
1638             Break
1639         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1640             MFlgLoop% = 0
1641             MJudge% = MNG%
1642             MCommentD1001 = 29
1643             MCommentD1002 = 22
1644             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1645             fnProcessCheck = -5         ' NGは-5を返す
1646             MStaNo = 5
1647             Break
1648         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1649             MJudge% = MNG%
1650             If MCommentD1001 = 32 Then
1651                 '何もしない
1652             Else
1653                 MCommentD1001 = 26
1654             EndIf
1655             MCommentD1002 = 22
1656             fnProcessCheck = -4         ' NGは-4を返す
1657             MStaNo = 5
1658             Break
1659         Else
1660             MJudge% = MNG%
1661             MCommentD1001 = 28
1662             MCommentD1002 = 22
1663         EndIf
1664     Next MStaNo
1665     '工程抜け確認OFF
1666     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1667     '通過履歴NG 工程抜けの場合
1668     If MJudge% = MPass% Then
1669         M_20# = MPass%
1670     EndIf
1671     '
1672     'エラー画面
1673     If MJudge% <> MOK% Then
1674         M_20# = MClear%     '初期化
1675         'エラー処理記述
1676         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1677         'GOT KEY入力待ち
1678         MKeyNumber = fnKEY_WAIT()
1679         '
1680         Select MKeyNumber
1681             Case MAbout%        '停止を選択した場合
1682                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1683                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1684                 Break
1685             Case MNext%         '次へを選択した場合
1686                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1687                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1688                 Break
1689             Case MContinue%     '継続を選択した場合
1690                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1691                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1692                 Break
1693             Case MNgProcess%    'NGを選択した場合
1694                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1695                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1696                 Break
1697         End Select
1698     Else
1699         fnProcessCheck = 1  ' OKは1を返す
1700     EndIf
1701 FEnd
1702 '
1703 '■fnPiasWrite
1704 ''' <summary>
1705 ''' Pias 組立結果書込み要求
1706 ''' </summary>
1707 '''<param name="MFlg%">
1708 '''                 MOK%(1) = 工程履歴にOKを書込む
1709 '''                 MNG%(0) = 工程履歴にNGを書込む
1710 '''</param>
1711 '''<returns></returns>
1712 ''' <remarks>
1713 ''' Date   : 2021/07/07 : M.Hayakawa
1714 ''' </remarks>'
1715 Function M% fnPiasWrite(ByVal MFlg%)
1716       fnPiasWrite = 0
1717 *RETRY_PIASWRITE
1718     '
1719     '組立OK(MOK%)の場合　M306 ON
1720    '組立NG(MNG%)の場合　M307 ON
1721     If MFlg% = MOK% Then
1722         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1723     Else
1724         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1725     EndIf
1726     Dly 0.1                  '念のため
1727     '
1728     'Piasへ書込み開始 M305 -> ON
1729     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1730     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1731     '
1732     MJudge% = MNG%
1733     '
1734     For MStaNo = 0 To 5
1735         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1736             MJudge% = MOK%
1737             'MRet = fnAutoScreenComment(85)  'AUTO画面
1738             MStaNo = 5
1739             Break
1740         '
1741         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1742             MJudge% = MNG%
1743             'MRet = fnAutoScreenComment(85)  'AUTO画面
1744            MCommentD1001 = 34
1745            MCommentD1002 = 25
1746             MStaNo = 5
1747             Break
1748         '
1749         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1750             MJudge% = MNG%
1751             'MRet = fnAutoScreenComment(85)  'AUTO画面
1752            MCommentD1001 = 35
1753            MCommentD1002 = 25
1754             MStaNo = 5
1755             Break
1756         '
1757         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1758             MJudge% = MNG%
1759             'MRet = fnAutoScreenComment(85)  'AUTO画面
1760            MCommentD1001 = 36
1761            MCommentD1002 = 25
1762             MStaNo = 5
1763             Break
1764         '
1765         Else
1766             MJudge% = MNG%
1767            MCommentD1001 = 42
1768            MCommentD1002 = 25
1769         '
1770         EndIf
1771         '
1772     Next MStaNo
1773     '
1774     'Piasへ書込み開始 M305 -> OfF
1775     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1776     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1777     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1778     '
1779     '
1780     '通過履歴NG 工程抜けの場合
1781     If MJudge% = MPass% Then
1782         M_20# = MPass%
1783     EndIf
1784     '
1785    M_20# = MClear%     '初期化
1786     '
1787     'エラー画面
1788     If MJudge% < MOK% Then
1789     '
1790 '残しておくが現状では使用しないラベル
1791 *RETRY_ERR_WRITE
1792         M_20# = MClear%     '初期化
1793         'エラー処理記述
1794         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1795         'GOT KEY入力待ち
1796         MKeyNumber = fnKEY_WAIT()
1797         '
1798         If MKeyNumber = MAbout% Then   '停止を選択した場合
1799             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1800            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1801             Break
1802         '
1803         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1804             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1805             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1806         '
1807         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1808             M_20# = MPass%            'M_20# プログラム間共通外部変数
1809             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1810         '
1811         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1812             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1813            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1814             Break
1815         '
1816         EndIf
1817         '
1818         If M_20# = MClear% Then *RETRY_ERR_WRITE
1819         '
1820     EndIf
1821     '
1822     If M_20# = MContinue% Then *RETRY_PIASWRITE
1823     '
1824     fnPiasWrite = 1
1825     '
1826 FEnd
1827 '
1828 '■fnPCBNumberCheck
1829 ''' <summary>
1830 ''' Pias 基板番号照合要求
1831 ''' </summary>
1832 '''<param name="%"></param>
1833 '''<param name="%"></param>
1834 '''<returns></returns>
1835 ''' <remarks>
1836 ''' Date   : 2021/07/07 : M.Hayakawa
1837 ''' </remarks>'
1838 Function M% fnPCBNumberCheck
1839       fnPCBNumberCheck = 0
1840     '
1841 *RETRY_PCBCHECK
1842     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1843     'Piasへ基板照合開始 M310 -> ON
1844     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1845     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1846     '
1847     MJudge% = MNG%
1848     '
1849     For MStaNo = 0 To 5
1850         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1851             MJudge% = MOK%
1852             fnAutoScreenComment(96)  'AUTO画面
1853             MStaNo = 5
1854             Break
1855         '
1856         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1857             MJudge% = MNG%
1858             fnAutoScreenComment(97)  'AUTO画面
1859             MCommentD1001 = 37
1860             MCommentD1002 = 25
1861             MStaNo = 5
1862             Break
1863         '
1864         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1865             MJudge% = MNG%
1866             fnAutoScreenComment(98)  'AUTO画面
1867             MCommentD1001 = 38
1868             MCommentD1002 = 25
1869             MStaNo = 5
1870             Break
1871         '
1872         ElseIf M_In(11580) = 1 Then                         'time out
1873             MJudge% = MNG%
1874             fnAutoScreenComment(99)  'AUTO画面
1875             MCommentD1001 = 39
1876             MCommentD1002 = 25
1877             MStaNo = 5
1878             Break
1879         '
1880         Else
1881             MJudge% = MNG%
1882            MCommentD1001 = 41
1883            MCommentD1002 = 25
1884         '
1885         EndIf
1886         '
1887     Next MStaNo
1888     '
1889     'Piasへ基板照合開始 M310 -> OfF
1890     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1891     '
1892     '
1893     '通過履歴NG 工程抜けの場合
1894     If MJudge% = MPass% Then
1895         M_20# = MPass%
1896     EndIf
1897     '
1898    M_20# = MClear%     '初期化
1899     '
1900     'エラー画面
1901     If MJudge% < MOK% Then
1902     '
1903 '残しておくが現状では使用しないラベル
1904 *RETRY_ERR_PCBNUMBER
1905         M_20# = MClear%     '初期化
1906         'エラー処理記述
1907         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1908         'GOT KEY入力待ち
1909         MKeyNumber = fnKEY_WAIT()
1910         '
1911         If MKeyNumber = MAbout% Then   '停止を選択した場合
1912             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1913             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1914             Break
1915         '
1916         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1917             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1918             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1919         '
1920         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1921             M_20# = MPass%            'M_20# プログラム間共通外部変数
1922             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1923         '
1924         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1925             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1926             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1927             Break
1928         '
1929         EndIf
1930         '
1931         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1932         '
1933     EndIf
1934     '
1935     If M_20# = MContinue% Then *RETRY_PCBCHECK
1936 FEnd
1937 '
1938 '■ScrewTight_S2
1939 ''' <summary>
1940 ''' ねじ締めを行う
1941 ''' </summary>
1942 '''<param name="PScrewPos()">
1943 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
1944 '''             PScrewPos(2)    ：ねじ締め回避点
1945 '''             PScrewPos(10)   ：ねじ締め終了高さ
1946 '''</param>
1947 '''<returns>整数
1948 '''         0=異常終了、1=正常終了
1949 '''</returns>
1950 ''' <remarks>
1951 ''' Date   : 2021/07/07 : M.Hayakawa
1952 ''' </remarks>'
1953 Function M% ScrewTight_S2(ByVal PScrewPosition())   'ネジ締め個別設定
1954     ScrewTight_S2 = 0
1955     MOKNGFlg = 0
1956     Ovrd 100
1957     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1958     ' 暫定
1959     Ovrd 5
1960     Mvs PScrewPosition(10),-10    ' パレット上ねじ締めS①の上空へ移動
1961 '    Ovrd MOvrdA
1962     '暫定マスク
1963 '    M_Out(Y62_Driver)=1     ' バンクセッティング　C1
1964 '    Dly 0.1
1965 '    M_Out(Y61_Driver)=1     'ドライバーON　CW
1966 '    'Spd 8.3 '外部ライド100  内部ライド60   'ライド100-40　100%：Spd　15　'ねじ締め速度設定
1967 '    Spd MSpdA               'ネジ締め時Spd個別設定
1968     ' 暫定移動のみ
1969     Mvs PScrewPosition(10)
1970 '    '
1971 '    Dly 0.1
1972 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1973 '    Wait M_In(11584)=1          '完了/エラー検出
1974 '    Dly 0.1
1975 '    Spd M_NSpd
1976 '    '
1977 '    If M_In(X28_Driver)=1 Then  'ねじトータルエラー検出時
1978 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1979 '        Dly 0.1
1980 '        M_Out(Y62_Driver)=0     'バンクセッティング解除　C1
1981 '        Dly 0.1
1982 '        M_Out(Y63_Driver)=0     'バンクセッティング解除　C1
1983 '        Dly 0.1
1984 '        M_Out(Y65_Driver)=0     'プログラム解除　F1
1985 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
1986 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
1987 '        MOKNGFlg = -1
1988 '        ScrewTight_S2 = 0
1989 '    Else
1990 '        Wait M_In(X29_Driver)=1 ' 正常完了時
1991 '        Dly 0.1
1992 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1993 '        Dly 0.1
1994 '        M_Out(Y62_Driver)=0     'バンクセッティング解除
1995 '        Dly 0.1
1996 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
1997 '        Dly 0.1
1998 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
1999 '        ScrewTight_S2 = 1
2000 '    EndIf
2001 ' 暫定
2002     Ovrd 10
2003     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
2004     Ovrd 100
2005 FEnd
2006 '
2007 '■ScrewGet_S3
2008 ''' <summary>
2009 ''' ねじ供給機からねじを得る
2010 ''' </summary>
2011 '''<param name="%"></param>
2012 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
2013 '''         PScrewPos(2)    ：ねじ供給器回避点
2014 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2015 '''         PScrewPos(3)    ：Mねじポカヨケ位置
2016 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
2017 '''<returns>整数
2018 '''         0=異常終了、1=正常終了、-1=MネジセンサーNG、-2=MネジセンサーON、-3=吸着エラー
2019 '''</returns>
2020 ''' <remarks>
2021 ''' Date   : 2021/07/07 : M.Hayakawa
2022 ''' </remarks>'
2023 Function M% ScrewGet_S3(ByVal PScrewPosition())
2024     ScrewGet_S3 = 0
2025     MMScrewJudge% = 0
2026     'ねじ供給器初期動作エラーチェック
2027 ' ↓暫定削除
2028 '    Wait M_In(X34_ScrewReady1)=1 'ねじ供給器SがReadyになるまで待つ　←　何秒か待ってReadyにならなければ抜けるプログラムが必要？
2029 '    Ovrd 100
2030 '    If M_In(X33_SS2)=0 Then  'Mねじ検出センサがOFF（故障）していた場合
2031 '        Ovrd 30
2032 '        Mvs,-80             'その場所から80mm上空へ移動
2033 '        Mov PInitPos19049   '19049初期位置へ移動
2034 '        M_Out(Y6A_VV1)=0    'ねじ吸着 Off
2035 '        'NGとしてここの関数から抜ける
2036 '        ScrewGet_S3 = -1
2037 '        MMScrewJudge% = 1
2038 '        MCommentD1001 = 61
2039 '    EndIf
2040 '    If ScrewGet_S3 = 0 Then
2041 '        'Sタイト用ねじ供給機にMねじが混入していないか監視
2042 '        MMScrewJudge% = 0 'MMScrewJudgeを初期化する
2043 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT05&)
2044 '        If MRtn = 0 Then
2045 '            Ovrd 30
2046 '            Mvs,-80            'その場所から50mm上空へ移動
2047 '            Mov PInitPos19049  '19049初期位置へ移動
2048 '            MMScrewJudge% = 2
2049 '            MRtn = All_CLamp_Release()'全てのクランプ解除へ分岐
2050 '            MCnt% = 2   '2を設定
2051 '            MCommentD1001 = 62
2052 '        EndIf
2053 '        If MMScrewJudge% = 2 Then
2054 '            ScrewGet_S3 = -2
2055 '        EndIf
2056 '    EndIf
2057 '    'Mネジ判定がONの場合 NGとして関数を抜ける
2058 '    If MMScrewJudge% = 2 Then
2059 '        ScrewGet_S3 = -2
2060 '    EndIf
2061     'Sネジ用ねじ太郎のMネジ混入確認用ここまで
2062     Ovrd 100
2063     Spd M_NSpd
2064     If MMScrewJudge% = 0 Then
2065         ScrewGet_S3 = 0
2066         M_Out(Y63_Driver)=1         ' バンクセッティング　C2
2067         MScrewCnt% = 0
2068         MFinCnt% = 2
2069 '        For MCnt% = 0 To MFinCnt%
2070             Mov PScrewPosition(2)        ' ねじ供給機回避点
2071             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
2072             Ovrd 80
2073             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2074             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
2075             Mvs PScrewPosition(10), 1.2
2076             M_Out(Y6A_VV1)=1        ' ねじ吸着　ON
2077             'ビット回転
2078             M_Out(Y60_Driver)=1
2079             Dly 0.2
2080             '
2081             Ovrd 100
2082             JOvrd M_NJovrd
2083             Spd M_NSpd
2084             'ネジ吸着確認位置移動
2085             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
2086             Mvs PScrewPosition(10), -15  ' ネジ吸着確認位置
2087             'ビット回転停止
2088             'M_Out(Y60_Driver)=0
2089             '
2090             '1秒間ネジ吸着確認
2091 ' 以下暫定削除
2092 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2093 '            'MRtn = 0'強制エラー
2094 '            '吸着エラーの場合
2095 '            'ネジをねじ太郎に戻す
2096 '            If MRtn = 0 Then
2097 '                Ovrd 30
2098 '                'ビット回転停止
2099 '                M_Out(Y60_Driver)=0
2100 '                'ネジ供給機上空
2101 '                Mvs PScrewPos(1)
2102 '                '更に上空
2103 '                Mov PScrewPos(1), -75
2104 '                'ネジ捨て位置
2105 '                Mov PScrewFeedS021
2106 '                '吸着OFF
2107 '                M_Out(Y6A_VV1)=0 'ねじ吸着　OFF
2108 '                Dly 0.2
2109 '                '破壊ON
2110 '                M_Out(Y6B_VB1)=1 '真空破壊ON
2111 '                'ビット回転
2112 '                M_Out(Y61_Driver)=1
2113 '                Dly 0.5
2114 '                '
2115 '                Ovrd 100
2116 '                JOvrd M_NJovrd
2117 '                Spd M_NSpd
2118 '                'ドライバーを上下させねじを振り落とす
2119 '                Mov PScrewFeedS021, 10
2120 '                Mov PScrewFeedS021
2121 '                Dly 0.1
2122 '                Mov PScrewFeedS021, 10
2123 '                Mov PScrewFeedS021
2124 '                '
2125 '                'ネジ落ち待ち
2126 '                'ビット回転停止
2127 '                M_Out(Y61_Driver)=0
2128 '                Dly 0.1
2129 '                '破壊OFF
2130 '                M_Out(Y6B_VB1)=0 '真空破壊OFF
2131 '                '
2132 '                '
2133 '                'ねじ落ちたとして、移動更に上空
2134 '                Mov PScrewPos(1), -75
2135 '                Ovrd 100
2136 '                Spd M_NSpd
2137 '                'ネジ供給機上空
2138 '                Mvs PScrewPos(1)
2139 '                '
2140 '                ScrewGet_S3 = -3
2141 '                Break
2142 '                '
2143 '            Else
2144 '                MCnt% = MFinCnt%
2145 '                ScrewGet_S3 = 0
2146 '            EndIf
2147 '        Next  MCnt%
2148         '
2149         Ovrd 100
2150         Spd M_NSpd
2151         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2152         M_Out(Y60_Driver)=0     ' ビット回転停止
2153         M_Out(Y63_Driver)=0     ' バンクセッティング　C2
2154         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2155         'もう一度吸着確認
2156 ' 以下暫定削除
2157 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2158 '        If MRtn = 0 Then      '吸着エラーの場合
2159 '            MCommentD1001 = 94
2160 '            MCommentD1002 = 95
2161 '            ScrewGet_S3 = -3
2162 '        EndIf
2163 '        If MRtn = 1 Then      '吸着OKの場合
2164 '            ScrewGet_S3 = 1
2165 '        EndIf
2166 '        Break
2167     Else
2168         'Mネジ
2169         If MMScrewJudge% = 2 Then
2170             ScrewGet_S3 = -2
2171         EndIf
2172     EndIf
2173 FEnd
2174 '
2175 '■fnKEY_WAIT()
2176 ''' <summary>
2177 ''' GOTからのキー入力待ち
2178 ''' </summary>
2179 '''<returns>1：停止    2：次へ
2180 '''         3：継続    4：トルクチェック開始
2181 '''         5：NG
2182 '''         11：ロボット初期位置1    12：ロボット初期位置2
2183 '''         13：ロボット初期位置3    14：ロボット初期位置4
2184 '''</returns>
2185 ''' <remarks>
2186 ''' Date   : 2021/07/07 : M.Hayakawa
2187 ''' </remarks>'
2188 Function M% fnKEY_WAIT()
2189     fnKEY_WAIT = 0
2190     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
2191     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
2192     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
2193     '下記キー待ちの継続に反応させないため
2194     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
2195     Dly 0.2
2196     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
2197     MLocalLoopFlg=1
2198     While MLocalLoopFlg=1
2199         If M_In(11345) = 1 Then         '停止   M5345
2200             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
2201             fnKEY_WAIT = 1
2202             MLocalLoopFlg=-1
2203             Break
2204         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
2205             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
2206             fnKEY_WAIT = 2
2207             MLocalLoopFlg=-1
2208             Break
2209         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
2210             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
2211             fnKEY_WAIT = 3
2212             MLocalLoopFlg=-1
2213             Break
2214         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
2215             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
2216             fnKEY_WAIT = 4
2217             MLocalLoopFlg=-1
2218             Break
2219         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
2220             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
2221             fnKEY_WAIT = 5
2222             MLocalLoopFlg=-1
2223             Break
2224             '
2225         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
2226             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
2227             fnKEY_WAIT = MRobotInit1%
2228             MLocalLoopFlg=-1
2229             Break
2230             '
2231         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
2232             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
2233             fnKEY_WAIT = MRobotInit2%
2234             MLocalLoopFlg=-1
2235             Break
2236             '
2237         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
2238             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
2239             fnKEY_WAIT = MRobotInit3%
2240             MLocalLoopFlg=-1
2241             Break
2242             '
2243         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
2244             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
2245             fnKEY_WAIT = MRobotInit4%
2246             MLocalLoopFlg=-1
2247             Break
2248             '
2249         Else
2250         EndIf
2251     WEnd
2252     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
2253     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
2254 FEnd
2255 '
2256 '■ fnAUTO_CTL
2257 ''' <summary>
2258 ''' AUTOモードOFF、PLCからの開始待ち
2259 ''' </summary>
2260 ''' <remarks>
2261 ''' Date   : 2021/07/07 : M.Hayakawa
2262 ''' </remarks>
2263 Function M% fnAUTO_CTL
2264     fnAUTO_CTL = 0
2265     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2266     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
2267     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2268     '
2269     If M_Svo=0 Then             'サーボON確認
2270         Servo On
2271     EndIf
2272     Wait M_Svo=1
2273 FEnd
2274 '
2275 '■ fnWindScreenOpen
2276 ''' <summary>
2277 ''' ウィンド画面の表示、非表示設定
2278 ''' </summary>
2279 '''<param name="%"></param>
2280 '''<param name="%"></param>
2281 '''<param name="%"></param>
2282 '''<param name="%"></param>
2283 ''' <remarks>
2284 ''' コメントD1001, D1002, D1003の設定
2285 ''' MWindReSet = 0     画面非表示
2286 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2287 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2288 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2289 ''' Date   : 2021/07/07 : M.Hayakawa
2290 ''' </remarks>
2291 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2292     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2293         M_Out16(12480) = MCommentD1001            'D1001 コメント
2294     EndIf
2295     '
2296     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2297         M_Out16(12496) = MCommentD1002            'D1002 コメント
2298     EndIf
2299     '
2300     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2301        M_Out16(12512) = MCommentD1003            'D1003 コメント
2302     EndIf
2303     '
2304     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2305     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2306     Dly 0.5
2307     M_Out(12363) = 0                         'ウィンド画面設定
2308 FEnd
2309 '
2310 '■FnCtlValue2
2311 ''' <summary>
2312 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2313 ''' </summary>
2314 ''' <param name="MCtlNo%"></param>
2315 ''' <remarks>
2316 ''' Date : 2022/04/28 渡辺
2317 ''' </remarks>
2318 '''
2319 '''  1：投入数       ＋１
2320 '''  2：組立ＯＫ数   ＋１
2321 '''  3：組立ＮＧ数   ＋１ (未使用)
2322 '''  4：吸着エラー数 ＋１
2323 ''' 99：読書開始信号 OFF
2324 '''
2325 Function M% FnCtlValue2(ByVal MCtlNo%)
2326     FnCtlValue2 = 1
2327     Select MCtlNo%
2328         Case 1        '投入数＋１
2329             M_Out(12569) = 0             '書込み開始信号OFF
2330             M_Out(12568) = 1             '読込み開始信号ON
2331             MInputQty = M_In16(11600)    '投入数受信
2332             MInputQty = MInputQty + 1    '投入数＋１
2333             M_Out16(12592) = MInputQty   '投入数送信
2334             M_Out(12569) = 1             '書込み開始信号ON
2335             Break
2336             '
2337         Case 2        '組立ＯＫ数＋１
2338             M_Out(12569) = 0             '書込み開始信号OFF
2339             M_Out(12568) = 1             '読込み開始信号ON
2340             MAssyOkQty = M_In16(11616)   '組立OK数受信
2341             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2342             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2343             M_Out(12569) = 1             '書込み開始信号ON
2344             Break
2345             '
2346         Case 4        '吸着エラー数＋１
2347             M_Out(12569) = 0                       '書込み開始信号OFF
2348             M_Out(12568) = 1                       '読込み開始信号ON
2349             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
2350             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
2351             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
2352             M_Out(12569) = 1                       '書込み開始信号ON
2353             Break
2354             '
2355         Case 99        '読書開始信号OFF
2356             M_Out(12568) = 0        '読込み開始信号OFF
2357             M_Out(12569) = 0        '書込み開始信号OFF
2358             Break
2359             '
2360     End Select
2361     Exit Function
2362 FEnd
2363 '
2364 'Insightによる画像処理検査実行（並列処理なし）
2365 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2366 '-------------------------------------------------------------------------------
2367 'Insightによる画像処理検査実行（並列処理なし）
2368 '   引数
2369 '       PInspPos()      ：検査位置
2370 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2371 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2372 '       MInspCnt%       ：検査位置数
2373 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2374 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2375 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2376 '   戻り値：整数
2377 '       0=異常終了、1=正常終了
2378 '
2379 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2380 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2381 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2382 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2383 '   20200410    :   検査グループ設定Retry追加
2384 '-------------------------------------------------------------------------------
2385     '----- 初期設定 -----
2386     Cnt 0                                                           '移動効率化解除(初期値=0)
2387     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2388 '    Cnt 1,0.1,0.1
2389     '変数宣言・初期化
2390     Def Inte MNum                                                   '検査番号(検査順1～)
2391     MNum% = 1                                                       '検査番号初期値設定
2392     Def Inte MEndFlg                                                '検査終了フラグ
2393     MEndFlg% = 0
2394     '
2395     '検査G番号設定要求・検査実行要求off
2396     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2397     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2398     'エラー番号クリア
2399     MInspErrNum = 0                                                 '検査実行エラー番号
2400     M_Out16(MOUT_InspErrNum) = MInspErrNum
2401     MInspNGStepNum = 0                                              '検査実行NGStep番号
2402     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2403     '
2404     'Insight Ready check?
2405     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2406         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2407         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2408         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2409         ISInspectionSingle = 0                                      '異常終了戻り値設定
2410         Exit Function
2411     EndIf
2412     '
2413     '検査位置数確認
2414     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2415         MInspErrNum = 21                                            '検査データなし 21　引数<1
2416         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2417         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2418         ISInspectionSingle = 0                                      '異常終了戻り値設定
2419         Exit Function
2420     EndIf
2421     '
2422     '
2423     '
2424     '----- メイン処理 -----
2425     '設定された検査位置数分の検査実行
2426     While( MEndFlg% = 0 )
2427         '----- 検査グループ番号設定Retry追加 20200410
2428         MSetGrNumRetryExitFlg = 0
2429         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2430         While( MSetGrNumRetryExitFlg = 0 )
2431         '----- 検査グループ番号設定Retry追加ここまで 20200410
2432             '
2433             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2434             '
2435             '----- 検査グループ番号設定 -----
2436             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2437             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2438             '
2439             '検査位置へ移動・移動完了待ち
2440             Mvs PInspPos( MNum% )                                       '移動
2441             Dly 0.05                                                    '移動完了後Delay
2442             '
2443             '検査グループ番号設定終了確認
2444             M_Timer(1) = 0
2445             MExitFlg = 0
2446             While( MExitFlg = 0 )
2447                 '検査G設定正常終了?
2448                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2449                     MExitFlg = 1
2450                 '
2451                 '検査G設定異常終了?
2452                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2453                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2454                     If MInspErrNum = 0 Then                             '1回目のエラー?
2455                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2456                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2457                     EndIf
2458                     MExitFlg = 1
2459                 '
2460                 'timeoutチェック
2461                 ElseIf 1000 < M_Timer(1) Then
2462                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2463                     If MInspErrNum = 0 Then                             '1回目のエラー?
2464                         MInspErrNum = 12                                'timeout エラー番号=12
2465                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2466                     EndIf
2467                     MExitFlg = 1
2468                 EndIf
2469             WEnd
2470             '
2471             '検査G番号設定要求off
2472             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2473             '
2474             '----- 検査グループ設定Retry追加 20200410
2475             'NGなければ抜ける
2476             If MCurrentStepErr = 0 Then
2477                 MSetGrNumRetryExitFlg = 1
2478             Else
2479                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2480                 If MSetGrNumRetryCnt = 0 Then
2481                     MSetGrNumRetryExitFlg = 1
2482                 Else
2483                     'Retryへ　その前にDelay
2484                     Dly 0.5
2485                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2486                 EndIf
2487             EndIf
2488             '----- 検査グループ設定Retry追加ここまで 20200410
2489             '
2490         WEnd
2491         '
2492         '
2493         '
2494         '----- 検査実行 -----
2495         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2496             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2497                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2498                 MInspRetryExitFlg = 0
2499                 MRetryCnt = 2                                        'Retry回数設定
2500                 While( MInspRetryExitFlg = 0 )
2501                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2502                     '
2503                     '検査完了確認
2504                     MRetryCnt = MRetryCnt - 1
2505                     M_Timer(1) = 0
2506                     MExitFlg = 0
2507                     While( MExitFlg = 0 )
2508                     '検査完了待ち
2509                         '検査OK終了?
2510                         If M_In( MIN_IS_InspOK% ) = 1  Then
2511                             MJudgeOKFlg = 1                         '検査OKフラグON
2512                             MExitFlg = 1
2513                         '
2514                         '検査NG終了?
2515                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2516                             If MInspErrNum = 0 Then                 '1回目のエラー?
2517                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2518                                     MInspErrNum = 32                    '検査NG エラー番号=32
2519                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2520                                 EndIf
2521                             EndIf
2522                             MExitFlg = 1
2523                         '
2524                         '検査異常終了(IS timeout)?
2525                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2526                             If MInspErrNum = 0 Then                 '1回目のエラー?
2527                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2528                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2529                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2530                                 EndIf
2531                             EndIf
2532                             MExitFlg = 1
2533                         '
2534                         'timeoutチェック
2535                         ElseIf 3000 < M_Timer(1) Then
2536                             If MInspErrNum = 0 Then                 '1回目のエラー?
2537                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2538                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2539                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2540                                 EndIf
2541                             EndIf
2542                             MExitFlg = 1
2543                         EndIf
2544                     WEnd
2545                     '
2546                     '検査開始要求off
2547                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2548                     '
2549                     'OKなら抜ける
2550                     If MJudgeOKFlg = 1 Then
2551                         MInspRetryExitFlg = 1
2552                     Else
2553                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2554                         If MRetryCnt = 0 Then
2555                             MInspRetryExitFlg = 1
2556                         Else
2557                             'Retryへ　その前にDelay
2558                             Dly 0.3
2559                         EndIf
2560                     EndIf
2561                     '
2562                 WEnd
2563             EndIf
2564         EndIf
2565         '
2566         '
2567         '
2568         MNum% = MNum% + 1                                           '検査Step+1
2569         '検査終了確認　検査終了フラグセット
2570         If (MInspCnt% < MNum% ) Then
2571             MEndFlg% = 1                                            '検査終了フラグセット
2572         EndIf
2573         'NG発生時続行時処理
2574         If MInspErrNum <> 0 Then                                    'NGあり?
2575             If MNgContinue% <> 1 Then                               'NG続行?
2576                 MEndFlg% = 1                                        '検査終了フラグセット
2577             EndIf
2578         EndIf
2579     WEnd
2580     '
2581     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2582     If 0 < MZAxis% Then
2583         PCurrentPos = P_Curr                                        '現在位置取得
2584         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2585         Mvs PCurrentPos                                             '現在位置上空へ移動
2586     EndIf
2587     '
2588     '戻り値設定
2589     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      'カメラ検査強制OK(M_In(11372)=1)追加(12/21中村)
2590         ISInspectionSingle = 1                                      '正常終了戻り値設定
2591     Else
2592         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2593         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2594         ISInspectionSingle = 0                                      '異常終了戻り値設定
2595     EndIf
2596     Fine 0 , P
2597     '
2598 FEnd
2599 '
2600 ' ■ISInspection
2601 ''' <summary>
2602 ''' Insightによる画像処理検査実行
2603 ''' </summary>
2604 '''<param name="PInspPos()">検査位置</param>
2605 '''<param name="MInspGrNum%()">検査位置での検査グループ番号（=0：画像検査未実施）</param>
2606 '''             PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2607 '''<param name="MInspCnt%">検査位置数</param>
2608 '''<param name="MZAxis%">終了時のZ軸退避座標（-1:無効）</param>
2609 '''             終了時にZ軸をMZAxisで設定された位置まで上昇させる
2610 '''<param name="MNgContinue%">=1で検査エラー・NG発生時に全Stepの検査を行う</param>
2611 '''<returns>    整数 0=異常終了、1=正常終了</returns>
2612 '''         MInspErrNum     ：異常終了時にエラー番号が設定される
2613 '''         MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される"
2614 ''' <remarks>
2615 ''' Date   : 2021/07/07 : M.Hayakawa
2616 ''' </remarks>
2617 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2618 '    '画像使用確認 0<- 画像確認無しの場合
2619 '    If M_In(11369) = 0 Then            'toRBT_使用確認
2620 '        ISInspection = 1                                        '正常終了戻り値設定
2621 '    EndIf
2622 ''
2623 '    Cnt 0                                                       '移動効率化解除(初期値=0)
2624 '    Fine 0.05,P                                                 '位置決め完了条件設置　0.05mm
2625 '    MNum% = 1                                                   '検査番号初期値設定
2626 '    Def Inte MEndFlg                                            '検査終了フラグ
2627 '    MEndFlg% = 0
2628 '    '
2629 '    'エラー番号クリア
2630 '    MInspErrNumSub = 0                                          '検査実行エラー番号sub
2631 '    MInspErrNum = 0                                             '検査実行エラー番号
2632 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2633 '    MInspNGStepNum = 0                                          '検査実行NGStep番号
2634 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2635 '    '
2636 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready offなら終了
2637 '        MInspErrNum = 20                                        '検査実行エラー番号 20 Insight offline
2638 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2639 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2640 '        ISInspection = 0                                        '異常終了戻り値設定
2641 ''
2642 '    EndIf
2643 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2644 '    '
2645 '    '検査位置数確認
2646 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2647 '        MInspErrNum = 21                                        '検査データなし 21　引数<1
2648 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2649 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2650 '        ISInspection = 0                                        '異常終了戻り値設定
2651 ''
2652 '    EndIf
2653 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2654 '    '
2655 '    '設定された検査位置数分の検査実行
2656 '    While( MEndFlg% = 0 )
2657 '        '検査終了確認　検査終了フラグセット
2658 '        If (MInspCnt% < MNum% ) Then
2659 '            MEndFlg% = 1                                        '検査終了フラグセット
2660 '        EndIf
2661 '        '
2662 '        'タスク　検査G番号設定・検査完了確認処理開始　INSPTAST1
2663 '        If MEndFlg% = 0 Then
2664 '            M_01# = MInspGrNum%(MNum%)                          '検査G番号引渡し
2665 '        EndIf
2666 '        M_02# = MEndFlg%                                        '検査終了フラグ引渡し
2667 '        M_05# = MNum%                                           '検査番号(検査順1～)
2668 '        'タスク　検査G設定フラグ引渡し
2669 '        If MEndFlg% = 0 Then
2670 '            If 0 < MInspGrNum%(MNum%) Then
2671 '                M_03# = 1
2672 '            Else
2673 '                M_03# = 0
2674 '            EndIf
2675 '        Else
2676 '            M_03# = 0
2677 '        EndIf
2678 '        'タスク　検査結果確認フラグ引渡し
2679 '        If 1 < MNum% Then
2680 '            If 0 < MInspGrNum%(MNum%-1) Then
2681 '                M_04# = 1
2682 '            Else
2683 '                M_04# = 0
2684 '            EndIf
2685 '        Else
2686 '            M_04# = 0
2687 '        EndIf
2688 '        '
2689 '        'タスク処理開始
2690 '        M_00# = 1                                               'TASK処理開始
2691 '        'タスク処理開始確認
2692 '        M_Timer(1) = 0
2693 '        MExitFlg = 0
2694 '        While( MExitFlg = 0 )
2695 '            '処理開始完了確認
2696 '            If M_00# = 0 And M_10# = 8 Then
2697 '                MExitFlg = 1
2698 '            EndIf
2699 '            'timeoutチェック
2700 '            If 2000 < M_Timer(1) Then
2701 '                If MNgContinue% = 1 Then                        'NG続行?
2702 '                    MInspErrNumSub = 36                         'エラー番号設定36
2703 '                Else
2704 '                    MInspErrNum = 36                            'エラー番号設定36
2705 '                EndIf
2706 '                MExitFlg = 1
2707 '            EndIf
2708 '        WEnd
2709 '        '
2710 '        '検査位置へ移動・移動完了待ち
2711 '        If 0 = MInspErrNum Then
2712 '            If MEndFlg% = 0 Then
2713 '                Mvs PInspPos( MNum% )                           '移動
2714 '            EndIf
2715 '        EndIf
2716 '        '
2717 '        'タスク　検査G番号設定・検査完了確認処理終了待ち　INSPTAST1
2718 '        If 0 = MInspErrNum Then
2719 '            M_Timer(1) = 0
2720 '            MExitFlg = 0
2721 '            While( MExitFlg = 0 )
2722 '                '処理完了待ち（正常終了）
2723 '                If M_10# = 1 Then
2724 '                    MExitFlg = 1
2725 '                EndIf
2726 '                '処理完了待ち（異常終了）
2727 '                If M_10# = 0 Then
2728 '                    If MNgContinue% = 1 Then                    'NG続行?
2729 '                        MInspErrNumSub = M_12#                  'エラー番号設定　M12
2730 '                    Else
2731 '                        MInspErrNum = M_12#                     'エラー番号設定　M12
2732 '                    EndIf
2733 '                    MExitFlg = 1
2734 '                EndIf
2735 '                'timeoutチェック
2736 '                If 5000 < M_Timer(1) Then
2737 '                    If MNgContinue% = 1 Then                    'NG続行?
2738 '                        MInspErrNumSub = 31                     'エラー番号設定31
2739 '                    Else
2740 '                        MInspErrNum = 31                        'エラー番号設定31
2741 '                    EndIf
2742 '                    MExitFlg = 1
2743 '                EndIf
2744 '            WEnd
2745 '        EndIf
2746 '        '
2747 '        '検査結果確認
2748 '        If 0 = MInspErrNum Then
2749 '            If 1 < MNum% Then
2750 '                If 0 < MInspGrNum%(MNum%-1) Then                '検査あり?
2751 '                    If M_11# = 2 Then                           '検査NG?
2752 '                        If MNgContinue% = 1 Then                'NG続行?
2753 '                            If MInspNGStepNum = 0 Then          'NG未発生?
2754 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2755 '                            EndIf
2756 '                            MInspErrNumSub = 32                 'エラー番号設定 32:検査NG
2757 '                        Else
2758 ''                            MInspNGStepNum = MNum% - 1          '検査実行NGStep番号設定
2759 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2760 '                            MInspErrNum = 32                    'エラー番号設定 32:検査NG
2761 '                        EndIf
2762 '                   EndIf
2763 '                EndIf
2764 '            EndIf
2765 '        EndIf
2766 '        '
2767 '        'エラーなら検査中断終了するのでLoopから抜けるため終了フラグセット
2768 '        If 0 <> MInspErrNum Then
2769 '            MEndFlg% = 1
2770 '        EndIf
2771 '        '
2772 '        '検査実行、取込完了待ち
2773 '        If 0 = MInspErrNum Then
2774 '            If MEndFlg% = 0 Then
2775 '                If 0 < MInspGrNum%(MNum%) Then                  '検査あり?
2776 '                    M_Out(MOUT_IS_Insp%) = 1                    '検査実行要求on
2777 '                    '取込完了確認
2778 '                    M_Timer(1) = 0
2779 '                    MExitFlg = 0
2780 '                    While( MExitFlg = 0 )
2781 '                        '処理完了待ち
2782 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2783 '                            MExitFlg = 1
2784 '                        EndIf
2785 '                        'timeoutチェック
2786 '                        If 2000 < M_Timer(1) Then
2787 '                            If MNgContinue% = 1 Then            'NG続行?
2788 '                                MInspErrNumSub = 33             'エラー番号設定33
2789 '                            Else
2790 '                                MInspErrNum = 33                'エラー番号設定33
2791 '                            EndIf
2792 '                            MExitFlg = 1
2793 '                        EndIf
2794 '                    WEnd
2795 '                EndIf
2796 '                '
2797 '            EndIf
2798 '        EndIf
2799 '        MNum% = MNum% + 1
2800 '    WEnd
2801 '    '
2802 '    '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2803 '    If 0 < MZAxis% Then
2804 '        PCurrentPos = P_Curr                                    '現在位置取得
2805 '        PCurrentPos.Z = MZAxis%                                 'Z軸を設定
2806 '        Mvs PCurrentPos                                         '現在位置上空へ移動
2807 '    EndIf
2808 '    '
2809 '    'NG続行時処理
2810 '    If MNgContinue% = 1 Then                                    'NG続行?
2811 '        MInspErrNum = MInspErrNumSub                            'エラー番号設定
2812 '    EndIf
2813 '    '
2814 '    '戻り値設定
2815 '    If MInspErrNum = 0 Then
2816 '        ISInspection = 1                                        '正常終了戻り値設定
2817 '    Else
2818 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2819 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2820 '        ISInspection = 0                                        '異常終了戻り値設定
2821 '    EndIf
2822 '    '
2823 '*ISInspection_End
2824 'FEnd
2825 '
2826 '■InitialZoneB
2827 ''' <summary>
2828 ''' 非常停止後の復帰動作
2829 ''' 1)上空退避　Z方向上に移動
2830 ''' 2)J1軸以外を退避ポジションへ移動
2831 ''' 3)J1軸のみを退避ポジションへ移動
2832 ''' 4)イニシャルポジションへ移動
2833 ''' </summary>
2834 ''' <remarks>
2835 ''' Date : 2022/04/11 : N.Watanabe
2836 ''' </remarks>
2837 Function V fnInitialZoneB()
2838     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中] 2022/04/26 渡辺
2839 '
2840 'パラメータ
2841     Ovrd 5
2842 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2843 '    Cmp Pos, &B100011
2844 '
2845 '復帰動作開始
2846 '
2847 '置き台と両掴みの場所は、チャックを解放する
2848 *RecoveryChuckOpen
2849     PActive = P_Curr          '現在位置を取得
2850     MRecoveryChuckOpen = 0    'チャック解放フラグ 初期化
2851 'PMechaOnRoboSet(DVDメカ置き場)は、チャック解放
2852     If (PActive.X <= PMechaOnRoboSet.X + 1.0) And (PActive.X >= PMechaOnRoboSet.X -1.0) Then
2853         If (PActive.Y <= PMechaOnRoboSet.Y + 1.0) And (PActive.Y >= PMechaOnRoboSet.Y -1.0) Then
2854             If (PActive.Z <= PMechaOnRoboSet.Z + 1.0) And (PActive.Z >= PMechaOnRoboSet.Z -1.0) Then
2855                 MRecoveryChuckOpen = 1
2856             EndIf
2857         EndIf
2858     EndIf
2859 'PMechaOnRoboGet(DVDメカ受け取り位置)は、チャック解放
2860     If (PActive.X <= PMechaOnRoboGet.X + 1.0) And (PActive.X >= PMechaOnRoboGet.X -1.0) Then
2861         If (PActive.Y <= PMechaOnRoboGet.Y + 1.0) And (PActive.Y >= PMechaOnRoboGet.Y -1.0) Then
2862             If (PActive.Z <= PMechaOnRoboGet.Z + 1.0) And (PActive.Z >= PMechaOnRoboGet.Z -1.0) Then
2863                 MRecoveryChuckOpen = 1
2864             EndIf
2865         EndIf
2866     EndIf
2867     If MRecoveryChuckOpen = 1 Then
2868         M_Out(12256) = 0        'DVDチャック閉OFF
2869         M_Out(12257) = 1        'DVDチャック開ON
2870         M_20# = 0               'KEY入力初期化
2871         MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
2872         If MRtn = 0 Then
2873             fErrorProcess(11,270,284,0)
2874             If M_20# = MNext% Then M_20# = MClear%
2875             If M_20# = MAbout% Then GoTo *RecoveryEnd
2876             If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2877             If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2878         Else
2879             M_Out(12257) = 0        'DVDチャック開OFF
2880         EndIf
2881     EndIf
2882 '
2883 '特殊回避　直接、上空退避が出来ない所の対処
2884 '
2885 'PMechaOnRoboSet(Get)～PMechaOnRoboSet(Get)_1のエリアにいるときは、PMechaOnRoboSet_1へ
2886 '・PMechaOnRoboSet
2887 '・PMechaOnRoboSet_1
2888 '・PMechaOnRoboGet
2889 '・PMechaOnRoboGet_1
2890 '上記４点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
2891     PActive = P_Curr                    '現在位置を取得
2892     If (PActive.X >= -150) And (PActive.X <= -60) Then
2893         If (PActive.Y >= 540) And (PActive.Y <= 570) Then
2894             If (PActive.Z >= 290) And (PActive.Z <= 320) Then
2895                 Mvs PMechaOnRoboSet_1
2896                 Dly 1.0
2897             EndIf
2898         EndIf
2899     EndIf
2900 '
2901 'PMechaOnRoboSet(Get)_1～PMechaOnRoboSet(Get)_2のエリアにいるときは、PMechaOnRoboSet_2へ
2902 '・PMechaOnRoboSet_1
2903 '・PMechaOnRoboSet_2
2904 '・PMechaOnRoboGet_1
2905 '・PMechaOnRoboGet_2
2906 '上記４点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
2907 '    PActive = P_Curr                    '現在位置を取得
2908 '    If (PActive.X >= -90) And (PActive.X <= 50) Then
2909 '        If (PActive.Y >= 300) And (PActive.Y <= 570) Then
2910 '            If (PActive.Z >= 290) And (PActive.Z <= 430) Then
2911 '                Mvs PMechaOnRoboSet_2
2912 '                Dly 1.0
2913 '            EndIf
2914 '        EndIf
2915 '    EndIf
2916 '
2917 '上空退避
2918     PActive = P_Curr
2919     Pmove = PActive
2920     Pmove.Z = 500           '上空退避する一律の高さ
2921      If PActive.X < -400 Then
2922         Pmove.Z =290        '金具(F)受け取り位置上に腕を伸ばしているときは500まで上げられない為、例外処置
2923     EndIf
2924     If PActive.X > 400 Then
2925         Pmove.Z =400        'パレット上に腕を伸ばしているときは500まで上げられない為、例外処置
2926     EndIf
2927     If PActive.Z < Pmove.Z Then
2928         Mvs Pmove
2929     EndIf
2930     Dly 1.0
2931 'J1軸以外を退避ポジションへ移動
2932     JActive = J_Curr
2933     Jmove = JTaihi
2934     Jmove.J1 = JActive.J1        'J1軸は現在値を使用し、JTaihiのポーズを取る
2935     Jmove.J6 = JActive.J6        'J6軸は現在値を使用し、JTaihiのポーズを取る
2936     Mov Jmove
2937     Dly 1.0
2938 'J1軸のみを退避ポジションへ移動
2939     Mov JTaihi
2940     Dly 1.0
2941 'イニシャルポジションへ移動
2942     Mov PInitialPosition
2943     Cmp Off
2944     Ovrd 100
2945 ' ねじロボを初期位置に戻すために強制的に自動運転開始         '2022/04/20 ファンクションの外へ移動 渡辺
2946 '    If M_In(11856) = 0 Then                 ' 停止中のみ
2947 '        M_Out(12834) = 1                    ' 自動運転開始ON 12834   M6834
2948 '        MRet = frInCheck(11842, 1, 30000&)  ' 自動運転開始受信待ち    11842   M5842
2949 '        If MRet = 0 Then
2950 '        Else
2951 '            M_Out(12834) = 0    ' 自動運転開始OFF 12834   M6834
2952 '        EndIf
2953 '    EndIf
2954     fErrorProcess(11,253,281,0)
2955     Exit Function
2956 *RecoveryEnd
2957 FEnd
2958 '
2959 '
2960 '■fnAutoScreenComment
2961 ''' <summary>
2962 ''' メイン画面の動作状況表示
2963 ''' コメントD1005の設定
2964 ''' </summary>
2965 '''<param name="McommentD1005%">コメントID</param>
2966 ''' <remarks>
2967 ''' Date   : 2021/07/07 : M.Hayakawa
2968 ''' </remarks>
2969 Function fnAutoScreenComment(ByVal McommentD1005%)
2970     M_Out16(12576) = McommentD1005%
2971 FEnd
2972 '
2973 '■fnRoboPosChk
2974 ''' <summary>
2975 ''' 最後に終了したロボットポジションの確認
2976 ''' </summary>
2977 '''<param name="MINNumber%">入力番号</param>
2978 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2979 '''<param name="MTimeCnt&">タイムアウト時間</param>
2980 ''' PLCに保続した番号を読込み、確認
2981 ''' MRBTOpeGroupNo = 5 が初期位置に設定
2982 '''<returns>整数 0:タイムアウト 1:OK</returns>
2983 ''' <remarks>
2984 ''' Date   : 2021/07/07 : M.Hayakawa
2985 ''' </remarks>
2986 Function M% fnRoboPosChk
2987     fnRoboPosChk = 0
2988     MRet = fnStepRead()
2989     '初期位置でないと判断した場合
2990     'ウィンド画面切換え
2991     If MRBTOpeGroupNo > 5 Then
2992         '下記キー待ちの継続に反応させないため
2993         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2994         Dly 0.2
2995         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2996         Dly 1.5
2997         '
2998         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
2999         '
3000         MLoopFlg% = 1
3001         While MLoopFlg% = 1
3002             '
3003             '
3004             MKeyNumber% = fnKEY_WAIT()
3005             Select MKeyNumber%
3006                 Case Is = MAbout%       '停止
3007                     M_20# = MAbout%
3008                     MLoopFlg% = -1
3009                     Break
3010                 Case Is = MNext%        '次へ
3011                     'MLoopFlg% = -1
3012                     Break
3013                 Case Is = MContinue%    '継続
3014                     M_20# = MContinue%
3015                     MLoopFlg% = -1
3016                     Break
3017                 Default
3018                     Break
3019             End Select
3020         WEnd
3021     EndIf
3022     '
3023     If M_20# = MContinue% Then                              '継続ボタンが押された場合
3024         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
3025         Ovrd 5                                   '低速オーバーライド値設定
3026         Select MRBTOpeGroupNo
3027             Case Is = 5                          '何もしない
3028                 Break
3029             Case Is = 10                         '初期位置へ戻す
3030                 'Mov PTEST001
3031                 Break
3032             Case Is = 15                         '初期位置へ戻す
3033                 'Mov PTEST002
3034                 Dly 0.5
3035                 'Mov PTEST001
3036                 Dly 0.5
3037                 Break
3038             Default
3039                 Break
3040         End Select
3041         '
3042         Ovrd M_NOvrd                            'システムの初期値を設定
3043         M_Out(12364) = 1                        'toPLC_データ保存ON
3044         MRBTOpeGroupNo = 5
3045         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
3046         Dly 1.0
3047         M_Out(12364) = 0                        'toPLC_データ保存OFF
3048         fnRoboPosChk = 1                        '初期位置動作実行
3049         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
3050     EndIf
3051     Exit Function
3052 FEnd
3053 '
3054 '■frInCheck
3055 ''' <summary>
3056 ''' センサーINチェック
3057 ''' </summary>
3058 '''<param name="MINNumber%">入力番号</param>
3059 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
3060 '''<param name="MTimeCnt&">タイムアウト時間</param>
3061 '''<returns>整数 0:タイムアウト 1:OK</returns>
3062 ''' <remarks>
3063 ''' Date   : 2021/07/07 : M.Hayakawa
3064 ''' </remarks>
3065 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
3066     M_Timer(4) = 0
3067     MloopFlg = 0
3068     While MloopFlg = 0
3069         MCrtTime& = M_Timer(4)
3070         If M_In(MINNumber%) = MCMPFLG% Then
3071             MloopFlg = 1
3072             frInCheck = 1
3073         ElseIf MCrtTime& > MTimeCnt& Then
3074             MloopFlg = 1
3075             frInCheck = 0
3076         EndIf
3077     WEnd
3078 FEnd
3079 '-----------------------------------------------
3080 '
3081 'ねじ締め機通信確認
3082 '
3083 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3084 'fScewTcomChk = 0　：正常終了
3085 '          　　 -1 ：異常終了
3086 '-----------------------------------------------
3087 Function M% fScewTcomChk
3088 *ReCheckScewTcomChk
3089     fScewTcomChk = 0
3090     '通信確認送信
3091     M_Out(MOUT_ScwT_ComChk%) = MOn%
3092     '通信確認受信待機
3093 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
3094     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
3095     '通信確認送信終了
3096     M_Out(MOUT_ScwT_ComChk%) = MOff%
3097     If MRtn = 0 Then
3098         fScewTcomChk = -1
3099     EndIf
3100     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
3101  '
3102 FEnd
3103 '
3104 '
3105 '-----------------------------------------------
3106 '
3107 'ねじ締め開始送信
3108 '
3109 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3110 'fScewTStart = 0　：正常終了
3111 '          　　-1 ：異常終了
3112 '-----------------------------------------------
3113 Function M% fScewTStart
3114     fScewTStart = 0
3115     nRet% = 0
3116     'ねじ締め開始待機を受信
3117 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
3118     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
3119     If MRtn = 0 Then nRet% = -1
3120     If MRtn = 0 Then GoTo *ScrewStartERROR      '開始できなかった場合ジャンプ
3121     Dly 0.1
3122     'ねじ締め開始受信を送信
3123     M_Out(MOUT_ScwT_ST%) = MOn%
3124     Dly 0.5
3125     'Wait M_In(MTEST_KEY%) = MOn%
3126     'ねじ締め開始送信終了
3127     M_Out(MOUT_ScwT_ST%) = MOff%
3128     '
3129 *ScrewStartERROR
3130     fScewTStart = nRet%
3131 FEnd
3132 '
3133 '
3134 '
3135 '-----------------------------------------------
3136 '
3137 'ねじ締め完了受信
3138 '
3139 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3140 'fScewTcomChk = 0　：正常終了
3141 '          　 　-1 ：異常終了
3142 '-----------------------------------------------
3143 Function M% fScewTFinish
3144 *ReCheckScewTFinish
3145     fScewTFinish = 0
3146     'ねじ締め完了待機を受信
3147 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3148     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3149     If MRtn = 0 Then
3150         fScewTFinish = -1
3151     EndIf
3152     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3153     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3154     Dly 0.1
3155     'ねじ締め完了受信を送信
3156     M_Out(MOUT_ScwT_FinOK%) = MOn%
3157     Dly 0.5                          'とりあえず保持時間0.5msec
3158     'ねじ締め開始送信終了
3159     M_Out(MOUT_ScwT_FinOK%) = MOff%
3160     'Wait M_In(MTEST_KEY%) = MOn%
3161     '
3162 *ScewTFinish_ErrEnd
3163 FEnd
3164 '
3165 '
3166 '-----------------------------------------------
3167 '
3168 '条件xx停止受信
3169 '
3170 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3171 'fScewTCaseStop = 0　：正常終了
3172 '          　   　-1 ：異常終了
3173 '-----------------------------------------------
3174 Function M% fScewTCaseStop(ByVal MCase%())
3175 *ReCheckScewTCaseStop
3176     fScewTCaseStop = 0
3177     '条件xx停止を受信
3178     Wait M_In(MCase%(1)) = MOn%
3179     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3180     If MRtn = 0 Then
3181         fScewTCaseStop = -1
3182     EndIf
3183     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3184     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3185     Dly 0.1
3186     '条件xx停止受信を送信
3187     M_Out(MCase%(2)) = MOn%
3188     Dly 0.5                          'とりあえず保持時間0.5msec
3189     'ねじ締め開始送信終了
3190     M_Out(MCase%(2)) = MOff%
3191 *ScewTCaseStop_ErrEnd
3192     '
3193 FEnd
3194 '
3195 '
3196 '■fScrewTighenRoboCheck
3197 '<summary>
3198 'ねじロボ監視
3199 '</summary>
3200 '<param name = "MStopNum%"> 停止番号</param>
3201 '<returns>整数 0:ねじロボ異常終了 1:OK </returns>
3202 '<make>
3203 '2021/12/2 中村天哉
3204 '</make>
3205 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3206     fnAutoScreenComment(503)    '状態表示[ねじロボ動作終了待ち] 2022/04/26 渡辺
3207     fScrewTighenRoboCheck = 1
3208     MScrewTighenRoboFlg% = 1    'フラグの初期化
3209     MCheck% = 0
3210     While MScrewTighenRoboFlg% = 1
3211         MCheck% = M_In16(11904)
3212         If M_In(MStopNum%) = 1 Then '停止位置まで来たら
3213             MScrewTighenRoboFlg% = 0 '関数を抜ける
3214             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
3215         EndIf
3216         If MCheck% <> 0 Then
3217             fScrewTighenRoboError(MCheck%)
3218             Select M_20#
3219                 Case MAbout%            '停止が押された場合
3220                     M_Out(12869) = 1 Dly 1.0
3221                     MScrewTighenRoboFlg% = 0
3222                     fScrewTighenRoboCheck = 0   '異常終了
3223                     Break
3224                 Case MNgProcess%        'NGが押された場合
3225                     M_Out(12873) = 1 Dly 1.0
3226                     MScrewTighenRoboFlg% = 0
3227                     fScrewTighenRoboCheck = 0   '異常終了
3228                     Break
3229                 Case MContinue%             'リトライが押された場合
3230                     M_20# = MClear%         'M_20#初期化
3231                     M_Out(12871) = 1 Dly 1.0
3232                     Break
3233                 Case MNext%                 '次へが押された場合
3234                     M_20# = MClear%         'M_20#初期化
3235                     M_Out(12874) = 1 Dly 1.0
3236                     Break
3237             End Select
3238             Dly 0.5
3239         EndIf
3240     WEnd
3241 FEnd
3242 '■fScrewTighenRoboError
3243 '<summary>
3244 'ねじロボエラー処理
3245 '</summary>
3246 '<param name = "ErrorCode%"> エラー番号</param>
3247 '<make>
3248 '2021/12/2 中村天哉
3249 '</make>
3250 Function fScrewTighenRoboError(ErrorCode%)
3251     MCommentD1001 = ErrorCode% + 300
3252     fErrorProcess(11,MCommentD1001,0,0)
3253 FEnd
3254 '
3255 '■fErrorProcess
3256 '<summary>
3257 'エラー処理
3258 '</summary>
3259 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
3260 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
3261 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
3262 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
3263 '<make>
3264 '2021/11/5 中村天哉
3265 '</make>
3266 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3267     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
3268     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
3269     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
3270     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
3271     MKeyNum% = 0
3272 *RETRY_ERR_PROCESS
3273      M_20# = MClear%     '初期化
3274 '        'エラー処理記述
3275         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3276 '        'GOT KEY入力待ち
3277         MKeyNum% = fnKEY_WAIT()
3278 '        '
3279         If MKeyNum% = MAbout% Then   '停止を選択した場合
3280             M_20# = MAbout%            'M_20# プログラム間共通外部変数
3281             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3282             Break
3283          '
3284         ElseIf MKeyNum% = MContinue% Then   '継続を選択した場合
3285             M_20# = MContinue%            'M_20# プログラム間共通外部変数
3286             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3287         '
3288         ElseIf MKeyNum% = MNext% Then   '次へを選択した場合
3289             M_20# = MNext%            'M_20# プログラム間共通外部変数
3290             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3291          '
3292         ElseIf MKeyNum% = MNgProcess% Then   '停止を選択した場合
3293             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
3294             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3295             Break
3296         '
3297         EndIf
3298         '
3299         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3300 FEnd
3301 '
3302 '■fnTorqueCheck
3303 ''' <summary>
3304 ''' トルクチェック動作用のメイン
3305 ''' </summary>
3306 ''' <remarks>
3307 ''' Date   : 2021/12/21 : H.AJI
3308 ''' </remarks>'
3309 Function M% fnTorqueCheck
3310     'トルクチェック中送信  搬送系停止
3311     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
3312     '
3313     fnTorqueCheck = 0
3314     Ovrd 20
3315     Mov PInitialPosition              '初期位置移動
3316     Accel 100 , 20
3317     Mvs PHandChange                   'ハンド交換位置
3318     Accel 100 , 100
3319     Ovrd 100
3320     '下記キー待ちの継続に反応させないため
3321     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3322     Dly 0.2
3323     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3324     '
3325     'M6340  トルクチェック受信
3326     'Dly 5.0
3327     M_Out(12340) = 1          'トルクチェック受信 M6340
3328     Dly 1.0
3329     M_Out(12340) = 0
3330     '
3331     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
3332     M_Out(12835) = 1                         'ねじロボトルクチェック画面切替
3333    Wait M_In(11843) = 1                         'ねじロボトルクチェック画面切替
3334     M_Out(12835) = 0                         'ねじロボトルクチェック画面切替完了
3335     '
3336     '
3337     MLoopFlg = 1
3338     While MLoopFlg = 1
3339         '
3340 '        Mov PInitialPosition              '初期位置移動
3341         '
3342         MKeyNumber = fnKEY_WAIT()
3343         Select MKeyNumber
3344             Case Is = 1           '停止
3345                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
3346                 Dly 1.0
3347                 M_Out(12343) = 0
3348                 Ovrd 20
3349                 'Mov PTicketRead_1
3350                 M_Out(12840) = 1          'トルクチェック終了
3351                 Wait M_In(11859) = 1      'ねじロボからの終了
3352                 M_Out(12840) = 0          'トルクチェック終了
3353                 Ovrd 100
3354                 M_20# = 1
3355                 MLoopFlg = -1
3356                 Break
3357             Case Is = 2           '次へ
3358                 Break
3359             Case Is = 3           '継続
3360                 Break
3361             Case Is = 4           'トルクチェック開始
3362                 M_Out(12342) = 1          'トルクチェック開始要求受信 M6342
3363                 Dly 1.0
3364                 M_Out(12342) = 0
3365                 If M_In(11862) = 1 Then             'トルクチェッカー確認
3366                     fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
3367                     MRet = fnScrewMTorque()           'ねじロボ用トルクチェック
3368                 EndIf
3369                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
3370                 'MRet = fnMoveTorquePosi()
3371                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
3372                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3373                 Break
3374             Default
3375                 Break
3376         End Select
3377     WEnd
3378     '
3379     'トルクチェック中停止送信
3380     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
3381     '
3382     'ロボットの位置を元に戻す
3383     Mvs PInitialPosition            'イニシャルポジション
3384     '
3385     '
3386  FEnd
3387  '
3388 '
3389 '
3390 '---------------------------
3391 '
3392 '    メイン画面の表示、非表示設定
3393 '         コメントD1001, D1002, D1003の設定
3394 '           MWindReSet = 0     画面非表示
3395 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
3396 '           MWindErrScr = 10    エラー画面 D1001, D1002
3397 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
3398 '
3399 '---------------------------
3400 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3401     fnMainScreenOpen = 0
3402     '
3403    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
3404         M_Out16(12480) = MCommentD1001            'D1001 コメント
3405     EndIf
3406     '
3407     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
3408         M_Out16(12496) = MCommentD1002            'D1002 コメント
3409     EndIf
3410     '
3411     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
3412         M_Out16(12512) = MCommentD1003            'D1003 コメント
3413     EndIf
3414     '
3415     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
3416     M_Out(12362) = 1                         'ウィンド画面設定  M6362
3417     Dly 0.5
3418     M_Out(12362) = 0                         'ウィンド画面設定
3419 FEnd
3420 '
3421 '■Main
3422 ''' <summary>
3423 ''' トルクチェック実動作
3424 ''' </summary>
3425 ''' <remarks>
3426 ''' Date   : 2021/12/21 : H.AJI
3427 ''' </remarks>'
3428 Function M% fnScrewMTorque
3429     fnScrewMTorque = 0
3430     M_Out(12838) = 1                         'トルクチェック開始1
3431     Wait M_In(11857) = 1                     '受信完了
3432     M_Out(12838) = 0                         'トルクチェック開始1
3433     Dly 2.0
3434 FEnd
3435 '
3436 '
3437 '----------------------------------------------------------------
3438 'fTimeOutJudge
3439 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3440 '引数
3441 'Address% = 監視アドレス番号
3442 'JudgeFlg% = 対象アドレスの正常終了時の値
3443 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3444 '戻り値 = 0 エラー
3445 '         1 正常終了
3446 '         2 リトライ
3447 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3448 '作成日
3449 '2022/9/20 中村
3450 '----------------------------------------------------------------
3451 '
3452 Function M% fTimeOutJudge(ByVal MAddress , ByVal MJudgeFlg)
3453     fTimeOutJudge = 0
3454     MJudge% = 1
3455     MRtn = 0
3456     M_20# = MClear%
3457     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3458 *TimeOutLoop
3459     If MRtn = 1 Then GoTo *TimeOut
3460         fErrorProcess(11,202,203,0)
3461         If M_20# = MNext% Then GoTo *TimeOutLoop
3462         If M_20# = MContinue% Then MJudge% = 2
3463         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3464 *TimeOut
3465     fTimeOutJudge = MJudge%
3466 '
3467 *JUDGE_ERROR_END
3468 Exit Function
3469 FEnd
3470 '
3471 '■Main
3472 ''' <summary>
3473 ''' トルクチェック実動作
3474 ''' </summary>
3475 ''' <remarks>
3476 ''' Date   : 2021/12/21 : H.AJI
3477 ''' </remarks>'
3478 Function M% fnMoveTorquePosi
3479      fnMoveTorquePosi = 0
3480      Ovrd 50
3481     'Mov PTorquePosi000 'トルクチェック回避位置へ移動
3482      Mov PTorqueCheck_1 'トルクチェックメーター上空へ移動
3483     'Mov PTorquePosi020 'トルクチェックビットジョイント上空
3484     '
3485     '
3486      '以下は阿部さんが作成したトルクチェックプラグラム
3487     '
3488     Spd M_NSpd
3489 '-------------      ドライバーRST
3490     M_Out(12240)=0     'ドライバーOFF CCW
3491     M_Out(12241)=0     'ドライバーOFF CW
3492     M_Out(12242)=0     'ドライバー解除 C1
3493     M_Out(12243)=0     'ドライバー解除 C2
3494     M_Out(12245)=0     'プログラム解除 F1/プログラム2
3495 '---------------------------------------
3496 '---------------------------------------
3497     Fsc Off            '力覚センサ　Off  STEP1は不要
3498 '--------------------------------------------------------------
3499 '--------------------------------------------------------------
3500 '[P-11]
3501 '--------------------------------------------------------------   【トルクチェック 0.4N - P11】
3502     Mov PTorqueCheck, -50                     ' トルク-1　置き位置上空 50mm へ移動
3503    'Mov PTorquePosi020, -10                    ' トルク-1　置き位置上空 10mm へ移動
3504     Dly 0.1
3505 '-----------------------
3506    'Cnt 0                           'Cnt動作-2　終了
3507 '-----------------------
3508     Mov PTorqueCheck , -5                      'トルク-1　置き位置上空 5mm へ移動
3509     Dly 0.2
3510 '-----------------------
3511     M_Out(12242)=1                   'ドライバーセット C1
3512     Dly 0.1
3513     M_Out(12243)=1                   'ドライバーセット C2 (バンク3)
3514     Dly 0.1
3515     M_Out(12245)=1                   'プログラム2セット F1  Mネジ
3516     Dly 0.1
3517     'M_Out(12241)=1                   'ドライバーON  CW
3518    M_Out(12241)=0                   'ドライバーOFF  CW
3519     'Dly 0.1
3520 '--------------------------------
3521     Ovrd 40
3522    'Dly 0.1
3523 '--------------------------------  ネジ締め速度設定
3524     Spd 14                            'ライド 100-40 100% :Spd 12
3525     Dly 0.1
3526 '--------------------------------
3527 '--------------------------------
3528 '---------------------------------【ねじ締め動作】
3529 '
3530     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '移動中エラー検出
3531    Mvs PTorqueCheck               'トルクチェック位置へ移動
3532     Dly 0.3                          '動作安定待ち
3533    M_Out(12241)=1                   'ドライバーON  CW
3534 '
3535     Wait M_In(11584)=1                '完了/エラー検出
3536     Dly 0.1
3537     Spd M_NSpd
3538    'Ovrd 20
3539     If M_In(11256)=1 Then *LBL1       'ネジトータルエラー検出
3540     Wait M_In(11257)=1                'ネジ完了SC
3541 '---------------------------------
3542     Dly 0.1
3543     M_Out(12241)=0                    'ドライバーOFF CW
3544     Dly 0.1
3545     M_Out(12242)=0                    'ドライバー解除 C1
3546     Dly 0.1
3547     M_Out(12243)=0                    'ドライバー解除 C2 (バンク3)
3548     Dly 0.1
3549     M_Out(12245)=0                    'プログラム2解除 F1
3550 '--------------------------------------------------------------   【トルクチェック 0.4N - P11ここまで】
3551 '
3552     Mvs PTorqueCheck,-60                       'あえてmov から変更
3553     Dly 0.1
3554 '--------------------------------------------------------------
3555    'Ovrd 80
3556 '--------------------------------------------------------------
3557 '---------------------------------------
3558 '---------------------------------------
3559 '---------------------------------------エラー離脱処理
3560    *LBL1
3561    Fsc Off            '力覚センサ　Off   *STEP1は不要
3562    Mvs ,-100
3563    M_Out(12241)=0     'ドライバーOFF CW
3564    Dly 0.1
3565    M_Out(12242)=0     'ドライバー解除 C1
3566    Dly 0.1
3567    M_Out(12243)=0     'ドライバー解除 C2 (バンク3)
3568    Dly 0.1
3569    M_Out(12245)=0     'プログラム解除 F1
3570 '---------------------------------------
3571 '---------------------------------------
3572 '-------------
3573    'Mov PInitPos19049
3574    Dly 0.1
3575 '
3576 '
3577 '
3578 FEnd
3579 '
3580 ''■Main
3581 ''' <summary>
3582 ''' 組立動作用のメイン
3583 ''' </summary>
3584 ''' <remarks>
3585 ''' Date   : 2021/07/07 : M.Hayakawa
3586 ''' </remarks>'
3587 Function Main
3588     MopeNo = M_21#         '外部変数にて動作番号代入
3589     '
3590     If M_Svo=0 Then
3591         Servo On
3592     EndIf
3593     Wait M_Svo=1
3594 '組立スタート日付時刻要求パルスON (別スロットの8から要求に変更）
3595 '    M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3596 'パトライト操作
3597     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
3598     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
3599     '
3600     M_20# = 0                                   'KEY入力初期化
3601 '    M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化(位置移動1/18中村)
3602     MRet% = 0
3603 '初期位置の確認と移動
3604 '
3605 '復帰動作　実行・未実行判別      2022/03/22 渡辺 作成
3606     PActive = P_Curr                    '現在位置を取得
3607     MRecoveryPass% = 0
3608     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3609         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3610             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3611                 MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
3612             EndIf
3613         EndIf
3614     EndIf
3615     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3616         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3617             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3618                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
3619             EndIf
3620         EndIf
3621     EndIf
3622     If (PActive.X <= PMechaOnJigGet_3.X + 1.0) And (PActive.X >= PMechaOnJigGet_3.X -1.0) Then
3623         If (PActive.Y <= PMechaOnJigGet_3.Y + 1.0) And (PActive.Y >= PMechaOnJigGet_3.Y -1.0) Then
3624             If (PActive.Z <= PMechaOnJigGet_3.Z + 1.0) And (PActive.Z >= PMechaOnJigGet_3.Z -1.0) Then
3625                 MRecoveryPass% = 1       'DVD取り位置上空位置は復帰動作パス
3626             EndIf
3627         EndIf
3628     EndIf
3629     If MRecoveryPass% = 0 Then
3630        fnInitialZoneB()        '復帰動作パスフラグが立っていない時は復帰動作を実行
3631     EndIf
3632 '
3633 ' ねじロボを初期位置に戻すために強制的に自動運転開始        'fnInitialZoneBの外へ移動 2022/04/20 渡辺
3634     If MopeNo <> 2 And M_In(MIN_TorqueCheck%) <> 1 Then       'トルクチェックの時は以下を実行しない 2022/04/21 渡辺
3635         If M_In(11856) = 0 Then                 ' 停止中のみ
3636             fnAutoScreenComment(501)            ' 状態表示[ネジ締め機自動運転開始中] 2022/04/25 渡辺
3637             M_Out(12834) = 1                    ' 自動運転開始ON 12834   M6834
3638             MRet% = frInCheck(11842, 1, 30000&)  ' 自動運転開始受信待ち    11842   M5842
3639             If MRet% = 0 Then
3640             Else
3641                 M_Out(12834) = 0    ' 自動運転開始OFF 12834   M6834
3642             EndIf
3643         EndIf
3644     EndIf
3645 '
3646 '
3647 '    MRet% = fnRoboPosChk()
3648 '    If MRet% = 1 Then                           '初期位置の動作を行った場合     '2022/04/20 コメントアウト 渡辺
3649 '        fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  'ウィンド画面
3650 '        MKeyNumber% = fnKEY_WAIT()
3651 '        Select MKeyNumber%
3652 '            Case Is = MAbout%       '停止
3653 '                M_20# = MAbout%
3654 '                MLoopFlg% = -1
3655 '                Break
3656 '            Case Is = MNext%        '次へ
3657 '                'MLoopFlg = -1
3658 '                Break
3659 '            Case Is = MContinue%    '継続
3660 '                M_20# = MContinue%
3661 '                MLoopFlg% = -1
3662 '                Break
3663 '            Default
3664 '                Break
3665 '        End Select
3666 '    EndIf
3667     '
3668     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
3669         M_Out(12364) = 1            'toPLC_データ保存ON
3670 'トルクチェック
3671         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3672             MRet% = fnTorqueCheck()
3673             Break
3674         Else
3675 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認'12/21コメントアウト(中村)
3676 '                MRtn = InspInit()               '画像処理初期化処理
3677 '            EndIf
3678             '
3679            M_20# = MClear%                    '初期化
3680 '組立開始
3681             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3682                 'MRet% = fnAssyStart()  '暫定コメントアウト
3683                 fnAssyStart()
3684             Else
3685                 M_20# = MPass%
3686             EndIf
3687             M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化(位置移動1/18中村)
3688 '組立終了日付時刻
3689             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
3690             Wait M_In(11572) = 1            '日付取得完了
3691             Dly 0.1
3692             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
3693 'リフターユニットへのOUT
3694             '  KEY入力が何もない場合 OKと判断
3695             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
3696             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
3697 'OK/NGフラグ出力
3698             If M_20# = MAssyOK% Or M_22# = MIrregular% Then
3699                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
3700             ElseIf M_20# = MPass% Then
3701                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
3702             EndIf
3703 'PIASに組立完了書込み
3704             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3705                 If M_20# = MPass% Then
3706                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3707                 Else
3708                     'KEY入力がNGの場合
3709                     If M_20# = MNgProcess% Then
3710                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3711                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3712                         MRet% = fnPiasWrite(MNG%)
3713                        nAssyNgQty = nAssyNgQty + 1
3714                     EndIf
3715                     '
3716                     'KEY入力が何もない場合 OKと判断(0からMAssyOK%へ変更1/12中村)
3717                     If M_20# = MAssyOK% Or M_22# = MIrregular% Then
3718                             '-----------------------
3719                             'D732 -> D2600 コピー要求
3720                             M_Out(12566) = 1
3721 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3722                             M_Out(12566) = 0
3723                             '
3724                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3725                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3726                             '基板番号照合(PPは未使用）
3727 '                            MRet% = fnPCBNumberCheck()
3728                         Else
3729                             MRet% = 1
3730                         EndIf
3731                         '
3732                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3733                             If M_20# <> MAbout% Then
3734                                 '工程履歴OK書き込み
3735                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3736                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3737                                 MRet% = fnPiasWrite(MOK%)
3738                                 nAssyOkQty = 0
3739                                 nAssyOkQty = nAssyOkQty + 1
3740                             Else
3741                                 nAssyOkQty = nAssyOkQty + 1
3742                             EndIf
3743                         EndIf
3744                     EndIf
3745 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3746 '                    MRet% = fnPiasWrite(MOK%)
3747                 EndIf
3748             Else
3749                 nAssyOkQty = nAssyOkQty + 1
3750             EndIf
3751             '
3752             '組立終了日付時刻解除
3753             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3754             '投入数、組立OK数、組立NG数書込み
3755 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3756             '
3757 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認'コメントアウト12/21(中村)
3758 '                '画像処理終了処理
3759 '                MRtn = InspQuit()
3760 '            EndIf
3761         EndIf
3762         M_Out(12364) = 0                          'toPLC_データ保存OFF
3763     EndIf
3764 'パトライト操作
3765     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3766     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3767 'GOT表示
3768     fnAutoScreenComment(93)  'AUTO画面 工程完了
3769 FEnd
3770 End
3771 '
3772 '
3773 'おまじないコメント
3774 '絶対削除するな
3775 '
3776 '
3777 '
3778 '
3779 '
PInspPosition(1)=(+600.00,-152.00,+403.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PInspPosition(2)=(+6.08,+550.00,+537.59,+145.00,+0.00,-90.00,+0.00,+0.00)(7,0)
PInspPosition(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
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
PTemp=(+160.71,+268.79,+438.34,-105.40,+88.56,-15.27,+0.00,+0.00)(6,0)
PScrewPos(1)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(1)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
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
PActive=(+160.71,+268.79,+438.34,-105.40,+88.56,-15.27)(6,0)
Pmove=(+302.91,+7.18,+500.00,+180.00,+0.00,-180.00)(7,0)
PBracketFCheck=(-21.45,+489.13,+480.00,-180.00,+0.00,+0.00)(7,0)
PBracketFCheck1=(+2.27,+435.02,+540.18,+145.00,-0.01,-90.00)(7,0)
PBracketFCheck2=(+2.27,+535.02,+540.18,+145.00,-0.01,-90.00)(7,0)
PBracketFCheck_2=(-171.46,+546.61,+480.00,-180.00,+0.00,-90.00)(7,0)
PBracketFCheck_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFCheck_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFCheck_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFCheck_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFGet=(-605.12,-91.91,+244.44,+179.99,+0.37,+91.79)(7,1)
PBracketFGet_1=(-605.12,-91.91,+269.93,+179.98,+0.37,+91.79)(7,1)
PBracketFGet_2=(-287.41,+0.11,+490.00,-180.00,+0.00,+0.00)(7,0)
PBracketFGet_3=(-177.20,+193.83,+460.54,-179.99,-0.01,-85.26)(7,0)
PBracketFGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFSet=(-197.54,+547.27,+454.92,-179.60,-0.07,-89.30)(7,0)
PBracketFSet_1=(-197.54,+547.27,+470.00,-179.60,-0.07,-89.30)(7,0)
PBracketFSet_2=(-197.54,+547.27,+540.00,-179.60,-0.07,-89.30)(7,0)
PBracketFSet_3=(-26.65,+244.35,+539.98,-179.99,-0.01,-85.26)(7,0)
PBracketFSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck=(-21.45,+489.13,+480.00,+180.00,+0.00,+0.00)(7,0)
PBracketRCheck1=(+6.08,+420.00,+537.59,+145.00,+0.00,-90.00)(7,0)
PBracketRCheck2=(+6.08,+550.00,+537.59,+145.00,+0.00,-90.00)(7,0)
PBracketRCheck_2=(-177.57,+546.61,+479.97,-180.00,+0.00,-90.00)(7,0)
PBracketRCheck_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRGet=(-539.54,-90.81,+244.62,-179.85,-0.06,+92.52)(7,1)
PBracketRGet_1=(-539.54,-90.81,+290.00,-179.85,-0.06,+92.52)(7,1)
PBracketRGet_2=(-287.41,+0.11,+490.00,-180.00,+0.00,+0.00)(7,0)
PBracketRGet_3=(-177.20,+193.83,+460.54,-179.99,-0.01,-85.26)(7,0)
PBracketRGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRSet=(-152.04,+544.93,+454.54,+179.36,+0.16,-88.84)(7,0)
PBracketRSet_1=(-152.04,+544.93,+460.00,+179.36,+0.16,-88.84)(7,0)
PBracketRSet_2=(-152.04,+544.93,+520.00,+179.36,+0.16,-88.84)(7,0)
PBracketRSet_3=(-26.65,+244.35,+539.98,-179.99,-0.01,-85.26)(7,0)
PBracketRSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PHandChange=(+347.78,-1.40,+382.90,-90.00,+89.23,-89.99)(6,0)
PInitialPosition=(+303.63,-1.43,+467.78,+165.41,+90.00,+165.42)(6,0)
PInitialPosition1=(+302.91,+7.16,+469.96,-180.00,+0.00,+180.00)(7,0)
PMechaOnJigGet1=(+163.68,+364.78,+175.70,-92.91,+88.79,-2.80)(6,0)
PMechaOnJigGet1_1=(+163.68,+364.78,+200.00,-92.91,+88.79,-2.80)(6,0)
PMechaOnJigGet2=(+163.36,+364.43,+174.78,-73.21,+89.04,+16.52)(6,0)
PMechaOnJigGet2_1=(+163.36,+364.43,+200.00,-73.21,+89.04,+16.52)(6,0)
PMechaOnJigGet_2=(+160.73,+362.85,+350.00,-105.22,+88.56,-15.10)(6,0)
PMechaOnJigGet_3=(+160.71,+268.79,+438.34,-105.40,+88.56,-15.27)(6,0)
PMechaOnJigGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnJigGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnJigGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet=(+317.54,+114.36,+240.37,-41.97,+89.17,-42.02)(6,0)
PMechaOnPltSet_1=(+317.54,+114.36,+280.00,-41.97,+89.17,-42.02)(6,0)
PMechaOnPltSet_2=(+317.54,+114.36,+450.00,-41.97,+89.17,-42.02)(6,0)
PMechaOnPltSet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboGet=(-131.85,+556.66,+307.59,-179.98,-0.29,-179.49)(7,0)
PMechaOnRoboGet_1=(-80.00,+556.66,+307.20,-179.97,-0.29,-179.49)(7,0)
PMechaOnRoboGet_2=(-80.00,+316.96,+419.50,+180.00,+0.00,+180.00)(7,0)
PMechaOnRoboGet_3=(+0.00,+350.00,+480.00,-180.00,+0.00,-180.00)(7,0)
PMechaOnRoboGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet=(-131.65,+556.36,+307.20,-179.98,-0.29,-179.49)(7,0)
PMechaOnRoboSet_1=(-80.00,+556.36,+307.20,-179.98,-0.29,-179.49)(7,0)
PMechaOnRoboSet_2=(+40.77,+316.96,+419.50,+180.00,+0.00,+180.00)(7,0)
PMechaOnRoboSet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead=(+600.00,-152.00,+403.00,-180.00,+0.00,+90.00)(7,0)
PTicketRead_1=(+600.00,-152.00,+450.00,-180.00,+0.00,+90.00)(7,0)
PTicketRead_2=(+198.38,+259.83,+450.00,+180.00,+0.00,+128.47)(7,0)
PTicketRead_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
JActive=(+1.36,-5.10,+116.17,+0.00,+68.93,+1.35)
Jmove=(+1.36,-15.80,+124.16,+0.00,+71.59,+1.35,+0.00,+0.00)
JTaihi=(+0.00,-15.80,+124.16,+0.00,+71.59,+0.00,+0.00,+0.00)
