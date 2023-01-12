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
67 '===== <電ドラ変数定義> =====
68 X20_Driver=11248                    '電ドラステイタス1　Driver Status 1
69 X21_Driver=11249 '電ドラステイタス2  Driver Status 2
70 X22_Driver=11250 '電ドラステイタス3  Driver Status 3
71 X23_Driver=11251 '電ドラステイタス4  Driver Status 4
72 X24_Driver=11252 '電ドラエラーメッセージ1 Driver Error E1
73 X25_Driver=11253 '電ドラエラーメッセージ2 Driver Error E2
74 X26_Driver=11254 '電ドラエラーメッセージ3 Driver Error E3
75 X27_Driver=11255 '電ドラエラーメッセージ4 Driver Error E4
76 X28_Driver=11256 '電ドラトータルエラーシグナル Total Error
77 X29_Driver=11257 '電ドラ終了シグナル Comlete signal
78 X2A_Driver=11258 '電ドラエラーメッセージ5 Driver Error E5
79 '11584   'toRBトルクドライバ-COMP_ERR送信
80 Y60_Driver=12240 '電ドラ半時計回り CCW
81 Y61_Driver=12241 '電ドラ時計回り CW
82 Y62_Driver=12242 'バンクセッティング BANK C1
83 Y63_Driver=12243 'バンクセッティング BANK C2
84 Y64_Driver=12244 'バンクセッティング BANK C3
85 Y65_Driver=12245 'プログラムセッティング PRG SET F1
86 Y66_Driver=12246 'プログラムセッティング PRG SET F2
87 Y67_Driver=12247 'プログラムセッティング PRG SET F3
88 X34_ScrewReady1=11259 'ねじっこ1　Read
89 '===== <電ドラ定数> =====
90 Dim PScrewPos(10)       'ネジ締め用Function引数変数
91 Dim PGetScrewPos(10)    'ねじ供給機からねじを得るFunction引数変数
92 Dim PEscapePosi(10)
93 MLoopCnt% = 0'
94 '===== <ロボット定数> =====
95 '===== <ロボット変数定義> =====
96 MRBTOpeGroupNo = 0      'ロボット動作番号初期化
97 MCommentD1001 = 0
98 MCommentD1002 = 0
99 MCommentD1003 = 0
100 MScreenNo = 0
101 '
102 MCommentTSU = 0
103 MCommentTSD = 0
104 'ウィンド画面番号設定
105 MWindReSet = 0
106 MWindInfoScr = 5
107 MWindErrScr = 10
108 MWindErrScr2 = 11
109 MWindErrScr3 = 13
110 MWindErrScr17 = 17
111 MWindErrScr18 = 18
112 MWindCmmnScr = 20
113 MWindJigRelase19049 = 60
114 MWindJigRelase19050 = 61
115 MWindJigRelase19051 = 62
116 '
117 MClear% = 0        'KEY_のクリア
118 MAbout% = 1        'KEY_停止
119 MNext% = 2         'KEY_次のステップへ移行
120 MContinue% = 3     'KEY_継続 再度同じ動作を行う
121 '
122 Def Inte MNgProcess
123 MNgProcess% = 5      'KEY_NG
124 '
125 MAssyOK% = 6       '組立完了
126 MPass% = 7         '工程パス
127 MPiasNG% = 8       'Pias確認時履歴NG
128 MIrregular% = 10   '例外処理実行用
129 '
130 '初期化用KEY番号   '
131 MRobotInit1% = 11  '初期位置用
132 MRobotInit2% = 12  '初期位置用
133 MRobotInit3% = 13  '初期位置用
134 MRobotInit4% = 14  '初期位置用
135 '
136 MIN_INIT1REQUEST% = 11568 'toRBT_ロボット初期位置1要求
137 MIN_INIT2REQUEST% = 11569 'toRBT_ロボット初期位置2要求
138 MIN_INIT3REQUEST% = 11570 'toRBT_ロボット初期位置3要求
139 MIN_INIT4REQUEST% = 11571 'toRBT_ロボット初期位置4要求
140 '
141 MOUT_INIT1RECIVE% = 12560 'toPLC_ロボット初期位置1受信
142 MOUT_INIT2RECIVE% = 12561 'toPLC_ロボット初期位置2受信
143 MOUT_INIT3RECIVE% = 12562 'toPLC_ロボット初期位置3受信
144 MOUT_INIT4RECIVE% = 12563 'toPLC_ロボット初期位置4受信
145 '
146 MOK% = 1               '各判定用
147 MNG% = 0               '各判定用
148 MTIMEOUT% = -1         '各判定用
149 MJudge% = 0            '判定情報格納用
150 '
151 MRECIVETIME& = 0
152 MSETTIMEOUT10& = 10000&                '10秒設定
153 MSETTIMEOUT03& = 3000&                 '3秒設定
154 MSETTIMEOUT01& = 1000&                 '1秒設定
155 MSETTIMEOUT05& = 5000&                 '5秒設定
156 MSETTIMEOUT009& = 900&                 '0.9秒設定
157 MSETTIMEOUT008& = 800&                 '0.8秒設定
158 MSETTIMEOUT007& = 700&                 '0.7秒設定
159 MSETTIMEOUT006& = 600&                 '0.6秒設定
160 MSETTIMEOUT005& = 500&                 '0.5秒設定
161 MSETTIMEOUT004& = 400&                 '0.4秒設定
162 MSETTIMEOUT003& = 300&                 '0.3秒設定
163 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
164 MIN_PIAS_ComOK% = 11552                'PC通信OK
165 MIN_PIAS_ComTimeOut% = 11576           'PC通信確認タイムアウト
166 MIN_PIAS_ComNG% = 11553                'PC通信NG
167 MOUT_PIAS_ComCheck% = 12544            'PC通信確認要求
168 MOUT_PIAS_Missing_Process% = 12546     '工程抜け確認要求
169 MIN_PIAS_ModelTypeNG% = 11554          'モデル仕向NG
170 MIN_PIAS_ProcessHistryNG% = 11555      '前工程履歴NG
171 MIN_PIAS_ProcessHistryOK% = 11556      '前工程履歴OK
172 MIN_PIAS_ProcessHistryErr% = 11557     '工程履歴処理エラー
173 MIN_PIAS_MyProcessComp% = 11573        '自工程履歴あり
174 MIN_PIAS_ProcessHistryTimeOut% = 11578 '工程履歴タイムアウト
175 MOUT_OKNG% = 12226                     'PLC OUT でOK=1, NG=0 出力
176 '
177 MOUT_PiasPCBNumberCheck = 12557        '基板番号照合
178 MIN_PiasPCBNumberOK% = 11566          '基板番号OK
179 MIN_PiasPCBNumberNG% = 11565          '基板番号NG
180 MIN_PiasPCBNumberErr% = 11567         '基板番号処理エラー
181 '
182 MOUT_PiasAssyResultOK% = 12549    '組立OK
183 MOUT_PiasAssyResultNG% = 12550    '組立NG
184 MOUT_PiasAssyResultWr% = 12548    '工程履歴書き込み
185 '
186 MIN_PiasProcessNG% = 11559        '工程履歴処理NG
187 MIN_PiasProcessOtherErr% = 11560  '工程履歴処理エラー(なんかのトラブル)
188 MIN_PiasProcessOK% = 11558        '工程履歴処理OK
189 '
190 MIN_Insight_Use% = 11374               '画像確認ON
191 MIN_TorqueCheck% = 11348               'トルクチェック
192 '
193 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT操作権
194 MOUT_RED_LIGHT% = 12356            'PATLIGHT 赤 点灯
195 MOUT_RED_FLASH% = 12357            'PATLIGHT 赤 点滅
196 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT 黄 点灯
197 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT 黄 点滅
198 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT 青 点灯
199 MOUT_GREEN_FLASH% = 12361          'PATLIGHT 青 点滅
200 '
201 MOUT_ST_DATETIME% = 12551          '組立開始日付時刻
202 MOUT_ED_DATETIME% = 12552          '組立終了日付時刻
203 '
204 MOUT_TORQUE_CHECK% = 12367         'PLCへトルクチェック中を送信
205 '
206 MIN_ASSY_CANCEL% = 11366           '組立を行うかのフラグ
207 '
208 MLoopFlg% = 0                      'KEY入力後のOK or NG内容
209 MopeNo% = 0
210 MRtn% = 0
211 MRet = 0
212 MRet3% = 0
213 '
214 Def Inte MInputQty          '投入数 演算変数
215 Def Inte MAssyOkQty         '組立ＯＫ数 演算変数
216 Def Inte MAssyNgQty         '組立ＮＧ数 演算変数(未使用)
217 Def Inte MSuctionErrQty     '吸着エラー数 演算変数 2022/04/27 渡辺
218 Def Inte nAssyOkQty         '未使用
219 Def Inte MScrewNo
220 Def Inte MReTry
221 '===== <IO変数定義> =====
222 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
223 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
224 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
225 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
226 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
227 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
228 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
229 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
230 '
231 Def Inte Y6A_VV1            ' アーム先端　ネジ吸着バルブ
232 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
233 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
234 '
235 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
236 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
237 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
238 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
239 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
240 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
241 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
242 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
243 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
244 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
245 '
246 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
247 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
248 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
249 '
250 Def Inte MOUT_LED1          ' 画像処理用LED照明
251 '
252 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
253 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
254 '
255 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
256 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
257 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
258 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
259 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
260 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
261 '
262 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
263 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
264 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
265 '
266 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
267 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
268 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
269 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
270 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
271 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
272 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
273 Y6A_VV1%    =  12250    ' アーム先端　ネジ吸着バルブ
274 Y6B_VB1%    =  12251    'アーム先端　吸着破壊バルブ
275 MOUT_VB1%   =  12251    ' アーム先端　ネジ吸着破壊バルブ
276 '
277 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
278 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
279 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
280 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
281 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
282 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
283 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
284 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
285 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
286 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
287 '
288 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
289 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
290 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
291 '
292 MOUT_LED1%  =  12239    ' 画像処理用LED照明
293 '
294 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
295 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
296 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
297 '
298 '共通
299 Def Inte MTEST_KEY                      'デバックテスト用
300 Def Inte MOn                            '出力=1
301 Def Inte MOff                           '出力=0
302 '
303 'ねじ締め装置_出力アドレス
304 Def Inte MOUT_ScwT_ComChk               '通信確認
305 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
306 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
307 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
308 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
309 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
310 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
311 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
312 'ねじ締め装置_入力アドレス
313 Def Inte MIN_ScwT_comOK                 '通信確認返信
314 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
315 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
316 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
317 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
318 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
319 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
320 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
321 '
322 Dim MScwT_Case1%(2)               '条件1停止変数
323 Dim MScwT_Case2%(2)               '条件2停止変数
324 Dim MScwT_Case3%(2)               '条件3停止変数
325 Dim MScwT_Case4%(2)               '条件4停止変数
326 Dim MScwT_Case5%(2)               '条件5停止変数
327 '
328 '共通
329 MTEST_KEY% = 11359                       'デバッグ用テストKEY
330 MOn% = 1                                 '出力 = 1
331 MOff% = 0                                '出力 = 0
332 '
333 'ねじ締め機_アドレス設定
334 MOUT_ScwT_ComChk% = 12832               '通信確認送信
335 MOUT_ScwT_ST% = 12865                   'ねじ締め開始を送信
336 MOUT_ScwT_ReSTOK% = 12866               '再開始受信を送信
337 MOUT_ScwT_FinOK% = 12868                'ねじ締め完了受信を送信
338 MOUT_ScwT_Case1OK% = 12858              '条件1停止受信を送信
339 MOUT_ScwT_Case2OK% = 12859              '条件2停止受信を送信
340 MOUT_ScwT_Case3OK% = 12860              '条件3停止受信を送信
341 MOUT_ScwT_Case4OK% = 12861              '条件4停止受信を送信
342 MOUT_ScwT_Case5OK% = 12862              '条件5停止受信を送信
343 '
344 MIN_ScwT_comOK% = 11840                 'ねじ締め装置から返信
345 MIN_ScwT_STRec% = 11873                 'ねじ締め開始を受信
346 MIN_ScwT_ReST% = 11874                  '再開始を受信
347 MIN_ScwT_Fin% = 11876                   'ねじ締め完了を受信
348 MIN_ScwT_Case1% = 11866                 '条件1停止待機を受信
349 MIN_ScwT_Case2% = 11867                 '条件2停止待機を受信
350 MIN_ScwT_Case3% = 11868                 '条件3停止待機を受信
351 MIN_ScwT_Case4% = 11869                 '条件4停止待機を受信
352 MIN_ScwT_Case5% = 11870                 '条件5停止待機を受信
353 '
354 MScwT_Case1%(1) = MIN_ScwT_Case1%
355 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
356 MScwT_Case2%(1) = MIN_ScwT_Case2%
357 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
358 MScwT_Case3%(1) = MIN_ScwT_Case3%
359 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
360 MScwT_Case4%(1) = MIN_ScwT_Case4%
361 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
362 MScwT_Case5%(1) = MIN_ScwT_Case5%
363 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
364 '
365 '設定 InitialZoneBで使用する変数
366 Def Pos PActive       '直交座標系 位置変数 現在位置
367 Def Pos Pmove         '直交座標系 位置変数 移動先
368 Def Jnt JActive       '関節座標系 位置変数 現在位置
369 Def Jnt Jmove         '関節座標系 位置変数 移動先
370 Def Jnt JTaihi        '関節座標系 位置変数 退避ポジション ティーチングで設定
371 Def Inte MRecoveryPass         '復帰動作パスフラグ　1=復帰動作をパス　0=復帰動作を実行
372 Def Inte MStandby              '待機位置確認フラグ
373 Def Inte MRecoveryChuckOpen    'チャック解放フラグ（復帰動作前）両掴み対策
374 '★注意★初期位置を変更した時には、変更が必要！
375 '
376 '
377 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
378 Function M% fnAssyStart
379     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
380 ' PIASチケット読込み工程抜け確認
381     M_20# = MClear%                       '初期化
382     If M_22# = MIrregular% Then GoTo *IRREGULAR     'Assy完了後DVDメカを把持している場合
383     '開始位置がイニシャルポジションだった場合(ハンド交換治具が無いかチェック)
384     *RE_START
385     PTemp = P_Curr
386     MRtn = 0
387     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
388         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
389             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
390                 MRtn = 1
391             EndIf
392         EndIf
393     EndIf
394     'イニシャルポジションにいた場合センサーチェック
395     If MRtn = 1 Then
396         Ovrd 20
397         Accel 100 , 20
398         Mvs PHandChange                             'ハンド交換位置に移動
399         Ovrd 100
400         Accel 100 , 100
401         MRtn = frInCheck(11264,0,MSETTIMEOUT03&)    'センサーチェック(治具が無いか)
402         Mvs PInitialPosition                        'イニシャルポジションに移動
403     Else
404         MRtn = 1
405     EndIf
406     If MRtn = 1 Then GoTo *AssyStart
407     fErrorProcess(11,286,287,0)                        'ハンド交換用治具がある
408     If M_20# = MNext% Then M_20# = MClear%
409     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
410     If M_20# = MNgProcess% Then GoTo *RE_START
411     If M_20# = MContinue% Then GoTo *RE_START
412 '
413     *AssyStart
414 '
415 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
416 '        MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
417 '        '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
418 '        '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
419 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' メニューへ戻る
420 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NGを工程履歴に書込み次の工程へ
421 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NGを工程履歴に書込み次の工程へ
422 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' 履歴NG, 工程抜け
423 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK以外は組立終了
424 '    EndIf
425 ' ネジ締め機テスト用 ----------
426 '    'Mret% = fScewTcomChk()
427 '    'ねじ締め開始
428     MRtn2 = fScewTStart()
429 '    '
430 '    '座標移動
431 '    '
432 '    '条件xx停止
433 '    fScewTCaseStop(MScwT_Case5%)
434 '    '
435 '    'ベースユニットKEY
436 '    Wait M_In(MTEST_KEY%) = MOn%
437 '    '
438 '    '再開始
439 '    fScewTReStart()
440 '    '
441 '    '座標移動
442 '    '
443 '    'ねじ締め完了
444 '    Mret% = fScewTFinish()
445 ' ネジ締めテスト終了
446 ' PIASテスト -----------
447 '    MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
448 '    MRet% = fnPiasWrite(MNG%)
449  '   MRet% = fnPCBNumberCheck()     'デバック用にコメントアウト(9/17中村)
450 ' PIASテスト終了 -------
451 '組み立て開始(仮組み9/10中村)
452 'プログラム原点
453 Ovrd 100
454 If MRtn2 = 0 Then GoTo *INITIAL_CHECK
455     fErrorProcess(11,329,201,0)
456     If M_20# = MNext% Then GoTo *AssyStart
457     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
458     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
459     If M_20# = MContinue% Then GoTo *AssyStart
460 'ハンドにDVDメカ,ブラケットが無いか
461 '
462 *INITIAL_CHECK
463 '
464 If M_In(11264) = 0 And M_In(11267) = 0 And M_In(11270) = 0 Then GoTo *CompInitial1  'DVDメカ,ブラケットが無いか
465 fErrorProcess(11,253,287,0)                         '284→287に変更6/2中村
466 If M_20# = MNext% Then M_20# = MClear%
467 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
468 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
469 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
470 *CompInitial1
471 '
472 'ハンドをイニシャルに戻す
473 If M_In(11266) = 1 Then     'DVDチャック閉検出
474     M_Out(12256) = 0        'DVDチャック閉OFF
475     M_Out(12257) = 1        'DVDチャック開ON
476     Break
477 EndIf
478 If M_In(11269) = 1 Then     'Fシリンダー出検出
479     M_Out(12258) = 0        'Fシリンダー出OFF
480     M_Out(12259) = 1        'Fシリンダー戻ON
481     Break
482 EndIf
483 If M_In(11272) = 1 Then     'Rシリンダー出検出
484     M_Out(12260) = 0        'Rシリンダー出OFF
485     M_Out(12261) = 1        'Rシリンダー戻ON
486     Break
487 EndIf
488 '
489 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    'DVDチャック開検出
490 If MRtn = 1 Then GoTo *CompInitial2
491 fErrorProcess(11,270,284,0)
492 If M_20# = MNext% Then M_20# = MClear%
493 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
494 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
495 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
496 *CompInitial2
497 '
498 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)    'Fシリンダー戻検出
499 If MRtn = 1 Then GoTo *CompInitial3
500 fErrorProcess(11,278,284,0)
501 If M_20# = MNext% Then M_20# = MClear%
502 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
503 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
504 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
505 *CompInitial3
506 '
507 MRtn = frInCheck(11271,1,MSETTIMEOUT05&)    'Rシリンダー戻検出
508 If MRtn = 1 Then GoTo *CompInitial4
509 fErrorProcess(11,276,284,0)
510 If M_20# = MNext% Then M_20# = MClear%
511 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
512 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
513 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
514 *CompInitial4
515 '
516 '
517 ' 2022/04/11 安全方向へ条件追加 渡辺
518 ' PInitialPosition 在席 MStandby=1
519 ' PMechaOnJigGet_3 在席 MStandby=2
520 '
521 MStandby = 0    '待機位置フラグを初期化
522 PTemp = P_Curr
523 If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
524     If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
525         If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
526             MStandby = 1
527         EndIf
528     EndIf
529 EndIf
530 If (PTemp.X <= PMechaOnJigGet_3.X + 1.0) And (PTemp.X >= PMechaOnJigGet_3.X - 1.0) Then
531     If ((PTemp.Y <= PMechaOnJigGet_3.Y + 1.0) And (PTemp.Y >= PMechaOnJigGet_3.Y - 1.0)) Then
532         If ((PTemp.Z <= PMechaOnJigGet_3.Z + 1.0) And (PTemp.Z >= PMechaOnJigGet_3.Z - 1.0)) Then
533             MStandby = 2
534         EndIf
535     EndIf
536 EndIf
537 If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
538     If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
539         If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
540             MStandby = 3
541         EndIf
542     EndIf
543 EndIf
544 If MStandby <> 0 Then GoTo *PositionOK
545 fErrorProcess(11,230,281,0)          '初期位置にいない時はエラーにする
546 If M_20# = MNext% Then GoTo *ASSY_ERROR_END
547 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
548 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
549 If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
550 *PositionOK
551 '
552 '
553 'DVD無しPASSプログラムから通常プログラムに切替えた時の対策 2022.05.13 渡辺
554 PTemp = P_Curr
555 If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
556     If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
557         If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
558             Ovrd 50
559             Mov PInitialPosition
560             Ovrd 100
561         EndIf
562     EndIf
563 EndIf
564 '
565 '
566 *ReCheck1
567     M_Out(12912) = 1                  'Ver 0.4 追加　仮置き台フラグ立て(工程6優先のため工程5のフラグ監視の前に出力)
568     Mov PMechaOnJigGet_3    'タクト短縮のため初期位置変更
569     'Wait M_In(11920) = 0              'BaseUnit5仮置き台フラグ確認(処理位置移動2/9中村)
570     MRtn = fTimeOutJudge(11920,0)    'BaseUnit5仮置き台フラグ確認(タイムアウト処理を追加22/09/29中村)
571     If MRtn = 0 Then GoTo *ASSY_ERROR_END
572     If MRtn = 2 Then GoTo *ReCheck1
573     M_Out(12912) = 1                  '仮置き台フラグ立て(念のため追加2/9中村)
574 '
575 'Ver 0.4 追加--------------------
576 If M_In(11920) = 1 Then GoTo *CompInitial4         '工程6が動作中の場合11920 = 1 ループ
577 'Ver 0.4 ここまで----------------           '工程6優先のため12912=0にしない
578 '
579 'Mov PInitialPosition   '1/20コメントアウト(中村)
580 '
581 '仮置き台回避点へ移動
582 Mov PMechaOnJigGet_2         '仮置き台回避点
583 '
584 '仮置き台からDVDメカを受け取る
585 'Wait M_In(12914) = 1              '供給許可を受信
586 'Wait M_In(11920) = 0              'BaseUnit5仮置き台フラグ確認(追加ここから10/1中村)
587 '
588     MRtn2 = 0
589 *RE_JIG_GET
590     M_20# = MClear%
591 '
592     If M_In(11278) = 1 Then          '方向の確認(CW端センサー)(追加ここまで10/1中村)
593         Mov PMechaOnJigGet1_1    '仮置き台上空
594         Ovrd 25
595         Mvs PMechaOnJigGet1      'DVDメカ受け取り位置
596         M_Out(12257) = 0         'DVDメカチャック開OFF
597         M_Out(12256) = 1         'DVDメカチャック閉ON
598 '        Wait M_In(11266) = 1     'DVDメカチャック閉検出
599         MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
600         If MRtn = 0 And MRtn2 = 0 Then      'DVDメカを把持できなかった場合チャックを開く(6/2中村)
601             M_Out(12256) = 0         'DVDメカチャック閉OFF
602             M_Out(12257) = 1         'DVDメカチャック開ON
603         EndIf
604         Mvs PMechaOnJigGet1_1    '仮置き台上空
605         Ovrd 100
606         Break
607     ElseIf M_In(11277) = 1 Then       '方向の確認(CCW端センサー)(追加ここから10/1中村)
608         Mov PMechaOnJigGet2_1    '仮置き台上空
609         Ovrd 25
610         Mvs PMechaOnJigGet2      'DVDメカ受け取り位置
611         M_Out(12257) = 0         'DVDメカチャック開OFF
612         M_Out(12256) = 1         'DVDメカチャック閉ON
613 '        Wait M_In(11266) = 1     'DVDメカチャック閉検出
614         MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
615         If MRtn = 0 And MRtn2 = 0 Then  'DVDメカを把持できなかった場合チャックを開く(6/2中村)
616             M_Out(12256) = 0         'DVDメカチャック閉OFF
617             M_Out(12257) = 1         'DVDメカチャック開ON
618         EndIf
619         Mvs PMechaOnJigGet2_1    '仮置き台上空
620         Ovrd 100
621         Break
622     EndIf
623 '
624     If MRtn = 1 Or MRtn2 = 1 Then GoTo *CompMechaGet1    'DVDメカチャックエラー処理(次へが押された時もエラー表示なしで先に進むように変更(6/2中村))
625 '    PTemp = P_Curr              '処理位置の変更1/12中村
626 '    Mvs PTemp , -150
627     Mov PMechaOnJigGet_2
628     fErrorProcess(11,269,294,0)  '279,284→269,294に変更6/2中村
629 '    PTemp = P_Curr             '処理位置の変更1/12中村
630 '    Mvs PTemp , -150
631 '    Mov PMechaOnJigGet_2
632     If M_20# = MNext% Then
633         M_20# = MClear%
634         MRtn2 = 1
635     EndIf
636     If M_20# = MContinue% Then MRtn2 = 0
637     If M_20# = MAbout% Or M_20# = MNgProcess% Then
638         Mov PInitialPosition
639         Break
640     EndIf
641     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
642     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
643     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_JIG_GET
644 *CompMechaGet1
645 '
646     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
647 Mov PMechaOnJigGet_2            '仮置き台回避点
648     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
649 '
650 'DVDメカセンサー検出
651 '
652 'M_Out(12912) = 0                  '仮置き台フラグ回収(処理位置変更2/9中村)
653 '
654 '
655 'DVDメカをねじロボ3に置く
656 Mov PMechaOnRoboSet_2        'ねじロボ回避点
657 '
658 'Wait M_In(11888) = 1         'ねじロボ3停止1まで待機
659 MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
660 If MRtn = 0 Then Mov PInitialPosition     '"イニシャルに戻る動き"
661 If MRtn = 0 Then GoTo *ASSY_ERROR_END
662 '
663 Mov PMechaOnRoboSet_1        'ねじロボ上空
664 Ovrd 25
665 Mvs PMechaOnRoboSet          'DVDメカ置き場
666 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止1～停止2まで)
667 '
668 'Wait M_In(11889) = 1         'ねじロボ3停止2まで待機
669 MRtn = fScrewTighenRoboCheck(11889)    '停止状態を受信する
670 'If MRtn = 0 Then
671 '    M_Out(12256) = 0             'DVDメカチャック閉OFF
672 '    M_Out(12257) = 1             'DVDメカチャック開ON
673 '    Mvs PMechaOnRoboSet_1
674 '    Mov PMechaOnRoboSet_2
675 '    Mov PInitialPosition
676 'EndIf
677 If MRtn = 0 Then GoTo *ASSY_ERROR_END   'その場で停止処理(故障の可能性)
678 '
679 *RE_ROBO_SET_1
680 '
681 M_Out(12256) = 0             'DVDメカチャック閉OFF
682 M_Out(12257) = 1             'DVDメカチャック開ON
683 '
684 'Wait M_In(11265) = 1         'DVDメカチャック開検出
685 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
686 If MRtn = 1 Then GoTo *CompRoboSet1
687 fErrorProcess(11,270,284,0)
688 If M_20# = MNext% Then M_20# = MClear%
689 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
690 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
691 If M_20# = MContinue% Then GoTo *RE_ROBO_SET_1
692 *CompRoboSet1
693 '
694 '    ' 部品供給要求送信(処理位置変更1/20中村)
695     M_Out(12787) = 1
696 '
697 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止2～停止3まで)
698 '
699 'Wait M_In(11890) = 1         'ねじロボ3停止3まで待機
700 MRtn = fScrewTighenRoboCheck(11890)    '停止状態を受信する
701 MScrewRoboNgFlg% = 0
702 If MRtn = 0 Then MScrewRoboNgFlg% = 1
703 '
704 Mvs PMechaOnRoboSet_1        'ねじロボ上空
705 Ovrd 100
706 Mov PMechaOnRoboSet_2        'ねじロボ回避点
707 M_Out(12912) = 0                  '仮置き台フラグ回収(処理位置変更2/9中村)
708 '
709 '
710 If MScrewRoboNgFlg% = 1 Then Mov PInitialPosition   'ねじロボで停止かNGが押されていた場合
711 If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END   '
712 '
713 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止3～停止4まで)
714 '
715 ''    ' 部品供給要求送信(処理位置位置変更1/20中村)
716 '    M_Out(12787) = 1
717 '    '    ' 部品供給完了待ち
718 '    Wait M_In(11810) = 1
719 '
720 'DVD金具(R)を取る
721 *RE_R_GET_3
722 Mov PBracketRGet_3           '経路
723 Mov PBracketRGet_2           '金具(R)受け取り回避点
724 M_Out(12261) = 0             '金具(R)シリンダー戻OFF
725 M_Out(12260) = 1             '金具(R)シリンダー出ON
726 '
727     '    ' 部品供給完了待ち(処理変更2/27中村)
728 *RE_FEEDER_READY
729 '    Wait M_In(11810) = 1
730     fnAutoScreenComment(513)    '状態表示[部品供給待ち] 2022/04/26 渡辺
731     MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '供給待ち
732     If MRtn = 1 Then GoTo *CompFeederReady
733 '   ' 部品供給要求終了
734     M_Out(12787) = 0
735     fErrorProcess(11,289,290,0) '284→290に変更6/2中村
736     If M_20# = MNext% Then M_20# = MClear%
737     If M_20# = MAbout% Or M_20# = MNgProcess% Then
738         Mov PBracketRGet_2
739         Mov PBracketRGet_3
740         Mov PBracketRSet_3
741         Mov PInitialPosition1
742     EndIf
743     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
744     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
745     ' 部品供給要求
746     M_Out(12787) = 1
747     If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
748 *CompFeederReady
749 '    ' 部品供給要求終了
750     M_Out(12787) = 0
751 '
752     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
753     Mov PBracketRGet_1           '金具(R)受け取り上空
754 '
755 *RE_R_GET_1
756 '
757 If M_20# = MContinue% Then
758     M_Out(12261) = 0             '金具(R)シリンダー戻OFF
759     M_Out(12260) = 1             '金具(R)シリンダー出ON
760     M_20# = MClear%
761 EndIf
762 '
763 'Wait M_In(11272) = 1         '金具(R)シリンダー出端検出
764 MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '金具(R)シリンダー出端検出
765 If MRtn = 1 Then GoTo *CompRGet1
766 fErrorProcess(11,275,284,0)
767 If M_20# = MNext% Then M_20# = MClear%
768 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
769 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
770 If M_20# = MContinue% Then GoTo *RE_R_GET_1
771 *CompRGet1
772 '
773 Ovrd 25
774 '
775 *RE_R_GET_2
776 '
777 Mvs PBracketRGet             '金具(R)受け取り位置
778 '
779 '
780 M_Out(12252) = 0             '真空OFFバルブOFF
781 M_Out(12253) = 0             '真空破壊バルブOFF(念のため)
782 M_Out(12251) = 1             '真空ONバルブON
783 '
784 'Wait M_In(11270) = 1         '金具(R)吸着センサーON検出
785 MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   '金具(R)吸着センサーON検出
786 If MRtn = 1 Then GoTo *CompRGet2
787 Mvs PBracketRGet_1           '金具(R)受け取り上空
788 fErrorProcess(11,279,295,0)  '284→295に変更6/2中村
789 If M_20# = MNext% Then M_20# = MClear%
790 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
791 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
792 If M_20# = MContinue% Then GoTo *RE_R_GET_2
793 *CompRGet2
794 '
795 Mvs PBracketRGet_1           '金具(R)受け取り上空
796 Ovrd 100
797 Mov PBracketRGet_2           '金具(R)受け取り回避点
798 Mov PBracketRGet_3           '経路
799 '
800 'DVD金具(R)を置く
801 Mov PBracketRSet_3           '回避点
802 MRtn = frInCheck(11270,1,MSETTIMEOUT05&)              'もう一度金具(R)吸着センサーON検出
803 If MRtn = 1 Then GoTo *CompRGet3
804 fErrorProcess(11,279,295,0)  '284→295に変更6/2中村
805 If M_20# = MNext% Then M_20# = MClear%
806 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
807 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
808 If M_20# = MContinue% Then GoTo *RE_R_GET_3
809 *CompRGet3
810 '
811 'Wait M_In(11891) = 1         'ねじロボ3停止4まで待機
812 MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
813 If MRtn = 0 Then Mov PInitialPosition
814 If MRtn = 0 Then GoTo *ASSY_ERROR_END
815 '
816 Mov PBracketRSet_2           '金具(R)置き位置回避点
817 Mvs PBracketRSet_1           '金具(R)置き位置上空
818 Ovrd 10
819 Mvs PBracketRSet             '金具(R)置き位置
820 Dly 0.1
821 '
822 *RE_R_SET_1
823 '
824 M_Out(12251) = 0             '真空ONバルブOFF
825 M_Out(12252) = 1             '真空OFFバルブON
826 M_Out(12253) = 1             '真空破壊バルブON
827 '
828 MRtn = frInCheck(11270,0,MSETTIMEOUT05&)
829 '
830 Dly 0.2
831 'M_Out(12253) = 0             '真空破壊バルブOFF
832 '
833 If MRtn = 1 Then GoTo *CompRSet1
834 Mvs PBracketRSet_1           '金具(R)置き位置上空
835 M_Out(12253) = 0             '真空破壊バルブOFF
836 fErrorProcess(11,296,297,0)  '236,284→296,297に変更6/2中村
837 If M_20# = MNext% Then M_20# = MClear%
838 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
839 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
840 If M_20# = MContinue% Then GoTo *RE_R_SET_1
841 *CompRSet1
842 '
843 'M_Out(12260) = 0             '金具(R)シリンダー出OFF(戻し位置変更1/20中村)
844 'M_Out(12261) = 1             '金具(R)シリンダー戻ON
845 ''
846 '*RE_R_SET_2
847 ''
848 ''Wait M_In(11271) = 1         '金具(R)シリンダー戻端検出
849 'MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   '金具(R)シリンダー戻端検出
850 'If MRtn = 1 Then GoTo *CompRSet2
851 'fErrorProcess(11,276,284,0)
852 'If M_20# = MNext% Then M_20# = MClear%
853 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
854 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
855 'If M_20# = MContinue% Then GoTo *RE_R_SET_2
856 '*CompRSet2
857 '
858 Ovrd 1                       '引っかかり防止(5/13中村)
859 Mvs PBracketRSet , -5        '引っかかり防止(5/13中村)
860 Ovrd 100                     '引っかかり防止(5/13中村)
861 Mvs PBracketRSet_1           '金具(R)置き位置上空
862 M_Out(12253) = 0             '真空破壊バルブOFF
863 Ovrd 100
864 Mov PBracketRSet_2           '金具(R)置き位置回避点
865 Mov PBracketRSet_3           '回避点
866 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止4～停止5まで)
867 '
868 *RE_R_SET_2
869 '
870 M_Out(12260) = 0             '金具(R)シリンダー出OFF(戻し位置変更1/20中村)
871 M_Out(12261) = 1             '金具(R)シリンダー戻ON
872 '
873 'Wait M_In(11271) = 1         '金具(R)シリンダー戻端検出
874 MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   '金具(R)シリンダー戻端検出
875 If MRtn = 1 Then GoTo *CompRSet2
876 fErrorProcess(11,276,284,0)
877 If M_20# = MNext% Then M_20# = MClear%
878 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
879 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
880 If M_20# = MContinue% Then GoTo *RE_R_SET_2
881 *CompRSet2
882 '
883 '
884 '置き位置画像検査(処理位置移動)
885 'Wait M_In(11892) = 1         'ねじロボ3停止5まで待機(画像検査からF側ブラケット置き)
886 'MRtn = fScrewTighenRoboCheck(11892)    '停止状態を受信する
887 'If MRtn = 0 Then Mov PInitialPosition
888 'If MRtn = 0 Then GoTo *ASSY_ERROR_END
889 '
890 'Wait M_In(11920) = 0              'BaseUnit5仮置き台フラグ確認(追加ここから10/1中村)
891 ''
892 'M_Out(12912) = 1                  '仮置き台フラグ立て(衝突防止)
893 ''
894 ''Mov PBracketRCheck_2         '経路
895 ''Mov PBracketRCheck           '検査位置
896 ''
897 '*RE_R_CHECK
898 ''If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck1
899 'PInspPosition(1) = PBracketRCheck1
900 'MInspGroup%(1) = 2
901 'PInspPosition(2) = PBracketRCheck2
902 'MInspGroup%(2) = 3
903 'MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
904 'If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck1
905 'If MRtn = 0 Then
906 '    MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
907 'EndIf
908 'If MRtn = 1 Then GoTo *CompRCheck
909 'fErrorProcess(11,43,46,0)
910 'If M_20# = MNext% Then M_20# = MClear%
911 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
912 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
913 'If M_20# = MContinue% Then GoTo *RE_R_CHECK
914 '*CompRCheck
915 '*SkipCheck1
916 '
917 'M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止5～停止6まで)
918 '
919 'Mov PBracketRCheck
920 '
921 'DVD金具(F)を取る
922 *RE_F_GET_3
923 'M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止5～停止6まで)(処理位置変更1/20中村)
924 Mov PBracketFGet_3           '回避点
925 '
926 M_Out(12912) = 0                  '仮置き台フラグ回収(衝突防止)
927 '
928 Mov PBracketFGet_2           '金具(F)受け取り回避点
929 'Mov PBracketFGet_1           '金具(F)受け取り上空(移動タイミング変更1/20中村)
930 '
931 *RE_F_GET_1
932 '
933 M_Out(12259) = 0             '金具(F)シリンダー戻OFF
934 M_Out(12258) = 1             '金具(F)シリンダー出ON
935 '
936 Mvs PBracketFGet_1           '金具(F)受け取り上空(移動タイミング変更1/20中村)
937 '
938 'Wait M_In(11269) = 1         '金具(F)シリンダー出端検出
939     fnAutoScreenComment(513)    '状態表示[部品供給待ち] 2022/04/26 渡辺
940 MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   '金具(F)シリンダー出端検出
941 If MRtn = 1 Then GoTo *CompFGet1
942 fErrorProcess(11,277,284,0)
943 If M_20# = MNext% Then M_20# = MClear%
944 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
945 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
946 If M_20# = MContinue% Then GoTo *RE_F_GET_1
947 *CompFGet1
948 '
949 Ovrd 25
950 *RE_F_GET_2
951     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
952 Mvs PBracketFGet             '金具(F)受け取り位置
953 '
954 '
955 '
956 M_Out(12249) = 0             '真空OFFバルブOFF
957 M_Out(12250) = 0             '真空破壊バルブOFF
958 M_Out(12248) = 1             '真空ONバルブON
959 '
960 'Wait M_In(11267) = 1         '金具(F)吸着センサーON検出
961 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)   '金具(F)吸着センサーON検出
962 If MRtn = 1 Then GoTo *CompFGet2
963 Mvs PBracketFGet_1           '金具(F)受け取り上空
964 fErrorProcess(11,280,295,0)  '284→295に変更6/2中村
965 If M_20# = MNext% Then M_20# = MClear%
966 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
967 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
968 If M_20# = MContinue% Then GoTo *RE_F_GET_2
969 *CompFGet2
970 '
971 Mvs PBracketFGet_1           '金具(F)受け取り上空
972 'Ovrd 100
973 Mov PBracketFGet_2           '金具(F)受け取り回避点
974 Mov PBracketFGet_3           '回避点
975 '
976 'DVD金具(F)を置く
977 Mov PBracketFSet_3           '回避点
978 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)   '金具(F)吸着センサーONもう一度確認
979 Ovrd 100
980 If MRtn = 1 Then GoTo *CompFGet3
981 fErrorProcess(11,280,295,0)  '284→295に変更6/2中村
982 If M_20# = MNext% Then M_20# = MClear%
983 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
984 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
985 If M_20# = MContinue% Then GoTo *RE_F_GET_3
986 *CompFGet3
987 '    ' 部品供給要求終了
988     M_Out(12787) = 0
989 '    ' 部品取得完了送信(パルス)
990     M_Out(12800) = 1 Dly 0.5
991     '
992 'Wait M_In(11892) = 1         'ねじロボ3停止5まで待機
993 MRtn = fScrewTighenRoboCheck(11892)    '停止状態を受信する
994 If MRtn = 0 Then Mov PInitialPosition1  '停止位置に移動
995 If MRtn = 0 Then GoTo *ASSY_ERROR_END
996 '
997 '    fnAutoScreenComment(533)    '状態表示[工程５のロボ動作終了待ち] 2022/04/26 渡辺(コメントアウト5/13中村)
998 'Wait M_In(11920) = 0              'BaseUnit5仮置き台フラグ確認(追加ここから10/1中村)(コメントアウト5/13中村)
999 ''
1000 'M_Out(12912) = 1                  '仮置き台フラグ立て(衝突防止)(コメントアウト5/13中村)
1001 '
1002     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1003 Mov PBracketFSet_2           '金具(F)置き位置回避点
1004 Mvs PBracketFSet_1           '金具(F)置き位置上空
1005 Ovrd 10
1006 Mvs PBracketFSet             '金具(F)置き位置
1007 Dly 0.2
1008 '
1009 *RE_F_SET_1
1010 '
1011 M_Out(12248) = 0             '真空ONバルブOFF
1012 M_Out(12249) = 1             '真空OFFバルブON
1013 M_Out(12250) = 1             '真空破壊バルブON
1014 '
1015 MRtn = frInCheck(11267,0,MSETTIMEOUT05&)
1016 Dly 0.5
1017 'M_Out(12250) = 0             '真空破壊バルブOFF
1018 '
1019 If MRtn = 1 Then GoTo *CompFSet1
1020 M_Out(12250) = 0             '真空破壊バルブOFF
1021 fErrorProcess(11,296,297,0)  '236,284→296,297に変更6/2中村
1022 If M_20# = MNext% Then M_20# = MClear%
1023 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1024 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1025 If M_20# = MContinue% Then GoTo *RE_F_SET_1
1026 *CompFSet1
1027 '
1028 '*RE_F_SET_2
1029 'M_Out(12258) = 0             '金具(F)シリンダー出OFF
1030 'M_Out(12259) = 1             '金具(F)シリンダー戻ON
1031 ''
1032 ''Wait M_In(11268) = 1         '金具(F)シリンダー戻端検出
1033 'MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   '金具(F)シリンダー戻端検出
1034 'If MRtn = 1 Then GoTo *CompFSet2
1035 'fErrorProcess(11,278,284,0)
1036 'If M_20# = MNext% Then M_20# = MClear%
1037 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1038 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1039 'If M_20# = MContinue% Then GoTo *RE_F_SET_2
1040 '*CompFSet2
1041 '
1042 Ovrd 1                       '引っかかり防止(5/13中村)
1043 Mvs PBracketFSet , -5        '引っかかり防止(5/13中村)
1044 Ovrd 100                     '引っかかり防止(5/13中村)
1045 Mvs PBracketFSet_1           '金具(F)置き位置上空
1046 M_Out(12250) = 0             '真空破壊バルブOFF
1047 Ovrd 100
1048 Mvs PBracketFSet_2           '金具(F)置き位置回避点
1049 Mov PBracketFSet_3           '回避点
1050 M_Out(12912) = 0             '仮置き台フラグ回収
1051 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止5～停止6まで)
1052 '
1053 *RE_F_SET_2
1054 M_Out(12258) = 0             '金具(F)シリンダー出OFF
1055 M_Out(12259) = 1             '金具(F)シリンダー戻ON
1056 '
1057 'Wait M_In(11268) = 1         '金具(F)シリンダー戻端検出
1058 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   '金具(F)シリンダー戻端検出
1059 If MRtn = 1 Then GoTo *CompFSet2
1060 fErrorProcess(11,278,284,0)
1061 If M_20# = MNext% Then M_20# = MClear%
1062 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1063 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1064 If M_20# = MContinue% Then GoTo *RE_F_SET_2
1065 *CompFSet2
1066 '
1067 '----------PIAS先読み----------
1068 '概要：読みに行くかどうかを決めるときにタイムアウトで先に進めるようにするタイマー
1069     Mov PTicketRead_2
1070     Mov PTicketRead_1
1071     M_22# = MClear%
1072     M_20# = MClear%
1073     M_Timer(4) = 0
1074     MloopFlg = 0
1075     While MloopFlg = 0
1076         MCrtTime& = M_Timer(4)
1077         If M_In(11354) = 1 Then
1078             MloopFlg = 1
1079         ElseIf MCrtTime& > 5000 Then    '暫定5秒(分割しているのは秒数調整しやすくしている為)
1080             MloopFlg = 1
1081         EndIf
1082     WEnd
1083     MRtn = 0
1084     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
1085         If M_In(11354) = 1 Then         '組立開始がONなら
1086             M_Out(12346) = 1 Dly 0.5        ' 組立開始を受信
1087             Mvs PTicketRead
1088             MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
1089 '        '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1090 '        '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1091             If MRtn = 1 Then M_22# = MAssyOK%
1092             If M_20# = MContinue% Then M_22# = MContinue%
1093             If M_20# = MPass% Then M_22# = MPass%
1094             If M_20# = MNext% Then M_22# = MPass%
1095         EndIf
1096     EndIf
1097     If M_20# = MNgProcess% Or M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1098     M_20# = MClear%
1099     Mov PTicketRead_1
1100     Mov PTicketRead_2
1101     Mov PBracketFSet_3
1102 '
1103 '置き位置画像検査
1104 'Wait M_In(11893) = 1         'ねじロボ3停止6まで待機
1105 MRtn = fScrewTighenRoboCheck(11893)    '停止状態を受信する
1106 If MRtn = 0 Then Mov PInitialPosition1  '停止位置に移動
1107 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1108 '
1109 If M_In(11369) = 0 Then M_Out(12912) = 1    '画像判定未使用時ここでフラグ立て6/23中村
1110 If M_In(11369) = 0 Then GoTo *SkipCheck1    '画像判定未使用時ジャンプ
1111 *RE_FLG_SET_1
1112 'Wait M_In(11920) = 0                'BaseUnit5仮置き台フラグ確認(追加2/27中村)
1113 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5仮置き台フラグ確認(タイムアウト処理を追加22/09/29中村)
1114 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1115 If MRtn = 2 Then GoTo *RE_FLG_SET_1
1116 M_Out(12912) = 1                    '仮置き台フラグ立て
1117 'Wait M_In(11920) = 0              'Ver 0.4 追加
1118 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5仮置き台フラグ確認(タイムアウト処理を追加22/09/29中村)
1119 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1120 If MRtn = 2 Then GoTo *RE_FLG_SET_1
1121 Dly 0.3
1122 'If M_In(11920) = 1 Then M_Out(12912) = 0
1123 If M_In(11920) = 1 Then GoTo *RE_FLG_SET_1
1124 '
1125 'Wait M_In(11920) = 0              'BaseUnit5仮置き台フラグ確認(追加ここから10/1中村)
1126 ''
1127 'M_Out(12912) = 1                  '仮置き台フラグ立て(衝突防止)
1128 '
1129 'Mov PBracketFCheck_2         '経路
1130 'Mov PBracketFCheck           '検査位置
1131 *RE_F_CHECK
1132 'If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck2
1133 PInspPosition(1) = PBracketFCheck1
1134 MInspGroup%(1) = 4
1135 PInspPosition(2) = PBracketFCheck2
1136 MInspGroup%(2) = 5
1137 MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1138 If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck1
1139 If MRtn = 0 Then
1140     MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1141 EndIf
1142 If MRtn = 1 Then GoTo *CompFCheck
1143 fErrorProcess(11,43,46,0)
1144 If M_20# = MNext% Then M_20# = MClear%
1145 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1146     Mov PBracketFCheck
1147     Mov PInitialPosition
1148     M_Out(12912) = 0
1149 EndIf
1150 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1151 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1152 If M_20# = MContinue% Then GoTo *RE_F_CHECK
1153 *CompFCheck
1154 *SkipCheck1
1155 '
1156 '
1157 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止6～停止7まで)
1158 If M_In(11369) = 1 Then Mov PBracketRCheck1
1159 'Wait M_In(11894) = 1         'ねじロボ3停止7まで待機
1160 MRtn = fScrewTighenRoboCheck(11894)    '停止状態を受信する
1161 If MRtn = 0 Then
1162     Mov PBracketFCheck
1163     Mov PInitialPosition
1164     M_Out(12912) = 0
1165 EndIf
1166 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1167 '
1168 If M_In(11369) = 0 Then GoTo *SkipCheck2    '画像判定未使用時ジャンプ
1169 *RE_R_CHECK
1170 PInspPosition(1) = PBracketRCheck1
1171 MInspGroup%(1) = 2
1172 PInspPosition(2) = PBracketRCheck2
1173 MInspGroup%(2) = 3
1174 MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1175 If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck2
1176 If MRtn = 0 Then
1177     MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1178 EndIf
1179 If MRtn = 1 Then GoTo *CompRCheck
1180 fErrorProcess(11,43,46,0)
1181 If M_20# = MNext% Then M_20# = MClear%
1182 If M_20# = MAbout% Or M_20# =  MNgProcess% Then
1183     Mov PBracketFCheck
1184     Mov PInitialPosition
1185     M_Out(12912) = 0
1186 EndIf
1187 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1188 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1189 If M_20# = MContinue% Then GoTo *RE_R_CHECK
1190 *CompRCheck
1191 *SkipCheck2
1192 '
1193 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止7～停止8まで)処理位置変更1/20中村
1194 '
1195 If M_In(11369) = 1 Then Mov PBracketFCheck
1196 '
1197 '
1198 'ねじロボ3のDVDassyを取る
1199 'M_Out(12866) = 1 Dly 0.5                   'ねじロボ3に再開要求(停止7～停止8まで)処理位置変更1/20中村
1200 Mov PMechaOnRoboGet_3                       '向き合わせ
1201 'M_Out(12912) = 0                            '仮置き台フラグ回収(6/23暫定コメントアウト(中村))
1202 '
1203 Mov PMechaOnRoboGet_2                       '回避点(処理位置変更1/20中村)
1204 '
1205 'Wait M_In(11895) = 1                       'ねじロボ3停止8まで待機
1206 MRtn = fScrewTighenRoboCheck(11895)         '停止状態を受信する
1207 If MRtn = 0 Then Mov PInitialPosition1      '停止位置に移動
1208 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1209 *RE_FLG_SET_2
1210     fnAutoScreenComment(533)    '状態表示[工程５のロボ動作終了待ち] 2022/04/26 渡辺
1211 'Wait M_In(11920) = 0                        'BaseUnit5仮置き台フラグ確認(処理追加2/9中村)
1212 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5仮置き台フラグ確認(タイムアウト処理を追加22/09/29中村)
1213 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1214 If MRtn = 2 Then GoTo *RE_FLG_SET_2
1215 '
1216 '
1217 M_Out(12912) = 1                            '仮置き台フラグ立て(衝突防止)
1218 Dly 0.3
1219 'Wait M_In(11920) = 0                        'Ver 0.4 追加　工程6優先
1220 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5仮置き台フラグ確認(タイムアウト処理を追加22/09/29中村)
1221 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1222 If MRtn = 2 Then GoTo *RE_FLG_SET_2
1223 '
1224 'If M_In(11920) = 1 Then M_Out(12912) = 0   'Ver 0.4 コメントアウト
1225 If M_In(11920) = 1 Then GoTo *RE_FLG_SET_2
1226 '
1227 'Mov PMechaOnRoboGet_2                      '処理位置変更(1/20中村)
1228 Mov PMechaOnRoboGet_1                       'ねじロボ上空
1229 Ovrd 25
1230 '
1231 *RE_ROBO_GET_1
1232 '
1233     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1234 Mvs PMechaOnRoboGet          'DVDメカ受け取り位置
1235 M_Out(12257) = 0             'DVDチャック開OFF
1236 M_Out(12256) = 1             'DVDチャック閉ON
1237 '
1238 'Wait M_In(11266) = 1         'DVDメカチャック閉検出
1239 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
1240 If MRtn = 1 Then GoTo *CompRoboGet1
1241 fErrorProcess(11,269,284,0)
1242 If M_20# = MNext% Then M_20# = MClear%
1243 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1244 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1245 If M_20# = MContinue% Then GoTo *RE_ROBO_GET_1
1246 *CompRoboGet1
1247 '
1248 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止8～停止9まで)
1249 '
1250 'Wait M_In(11876) = 1         'ねじロボ3ねじ締め完了を受信
1251 '
1252 'Wait M_In(11896) = 1         'ねじロボ3停止9まで待機
1253 MRtn = fScrewTighenRoboCheck(11876)    '停止状態を受信する
1254 'If MRtn = 0 Then
1255 '    M_Out(12256) = 0             'DVDメカチャック閉OFF
1256 '    M_Out(12257) = 1             'DVDメカチャック開ON
1257 '    Mvs PMechaOnRoboGet_1
1258 '    Mov PMechaOnRoboGet_2
1259 '    Mov PInitialPosition
1260 'EndIf
1261 If MRtn = 0 Then GoTo *ASSY_ERROR_END   'その場で停止
1262 '
1263 'Wait M_In(11264) = 1         'DVDメカ検出
1264 *RE_ROBO_GET_2
1265 MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   'DVDメカ検出
1266 If MRtn = 1 Then GoTo *CompRoboGet2
1267 fErrorProcess(11,273,284,0)
1268 If M_20# = MNext% Then M_20# = MClear%
1269 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1270 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1271 If M_20# = MContinue% Then GoTo *RE_ROBO_GET_2
1272 *CompRoboGet2
1273 '
1274 Mvs PMechaOnRoboGet_1        'ねじロボ上空
1275 Ovrd 100
1276 Mov PMechaOnRoboGet_2        'ねじロボ回避点
1277 'M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止9～ねじ締め完了まで)
1278 M_Out(12912) = 0                  '仮置き台フラグ回収(追加10/1中村)
1279 '
1280 Mov PTicketRead_2
1281 M_Out(12868) = 1 Dly 0.5     'ねじロボ3ねじ締め完了を送信(処理位置変更3/26中村)
1282 '
1283 '
1284     If M_22# = MAssyOK% Then GoTo *CompRead
1285 *IRREGULAR      '開始時Assy完了後DVDメカ把持ジャンプ先
1286     Mov PTicketRead_1
1287 '
1288 'DVDassyをパレットへ置く
1289 '    Wait M_In(11218) = 1         'パレットが上昇していることを確認
1290     If M_22# <> MClear% And M_22# <> MIrregular% Then GoTo *RE_PIAS_CHECK
1291     fnAutoScreenComment(95)    '状態表示[パレット搬送待機中] 2022/04/26 渡辺
1292     Wait M_In(11354) = 1         ' 組立開始信号が出ていることを確認
1293     M_Out(12346) = 1 Dly 0.5         ' 組立開始を受信
1294     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1295 *RE_PIAS_CHECK
1296     M_20# = MClear%                 '初期化
1297     MRtn = 1
1298     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
1299         If M_22# = MClear% Or M_22# = MContinue% Or M_22# = MIrregular% Then'PIASチェックにて未検査かリトライ時に入る
1300                 Mvs PTicketRead
1301                 MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
1302 '        '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1303 '        '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1304         EndIf
1305     EndIf
1306 '
1307     If M_22# = MPass% Then M_20# = MPass%
1308     M_22# = MClear%
1309     If MRtn = 1 And M_20# = MClear% Then GoTo *CompRead
1310 '    fErrorProcess(11,17,0,0)
1311 '    Dly 10                                      'デバッグ用
1312 '    If M_In(11359) = 1 Then M_22# = MIrregular%'デバッグ用
1313 '    If M_22# = MIrregular% Then *ASSY_ERROR_END  'デバッグ用
1314 '    If M_20# = MPass% Then M_20# = MClear%      'デバッグ用
1315     If M_20# = MPass% Then
1316         M_22# = MIrregular%
1317         M_20# = MAssyOK%
1318         Dly 0.1
1319         Mvs PTicketRead_1                         '22/04/26 追加 渡辺
1320     EndIf
1321     If M_20# = MAssyOK% Then GoTo *AssyEnd
1322     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1323     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1324     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1325     If M_20# = MContinue% Then GoTo *RE_PIAS_CHECK
1326 *CompRead
1327 '
1328 'Mov PTicketRead_1
1329 Accel 25 , 25                'デバッグ中
1330 Mov PMechaOnPltSet_2         'パレット回避点
1331 Accel 100 , 100
1332 Mov PMechaOnPltSet_1         'パレット上空
1333 '
1334 '置く前にDVDを持っているか？確認    2022/04/12 渡辺
1335 'DVDを持っていおらず、チャック閉の場合、閉端の信号はONしない
1336 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
1337 If MRtn = 1 Then GoTo *DVDCheckEnd         'チャックの閉端がONの場合
1338 fErrorProcess(11,269,284,0)
1339 If M_20# = MNext% Then M_20# = MClear%
1340 If M_20# = MClear% Then GoTo *CompPltSet
1341 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1342 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1343 If M_20# = MContinue% Then GoTo *CompRead
1344 *DVDCheckEnd
1345 '
1346 Dly 0.2
1347 Ovrd 10
1348 Mvs PMechaOnPltSet           'DVDメカ置き場所
1349 Dly 0.2
1350 '
1351 *RE_PLT_SET
1352 '
1353 M_Out(12256) = 0             'DVDメカチャック閉OFF
1354 M_Out(12257) = 1             'DVDメカチャック開ON
1355 '
1356 'Wait M_In(11265) = 1         'DVDメカチャック開検出
1357 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1358 If MRtn = 1 Then GoTo *CompPltSet
1359 fErrorProcess(11,270,284,0)
1360 If M_20# = MNext% Then M_20# = MClear%
1361 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1362 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1363 If M_20# = MContinue% Then GoTo *RE_PLT_SET
1364 *CompPltSet
1365 M_22# = MClear%
1366 '
1367 Mvs PMechaOnPltSet_1         'パレット上空
1368 Ovrd 100
1369     MRtn = FnCtlValue2(2)          '組立ＯＫ＋１  2022/04/28 渡辺
1370 Mov PMechaOnJigGet_3
1371     MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1372 'Mov PInitialPosition   'コメントアウト(1/20中村)
1373 '
1374 'Wait M_In(11876) = 1         'ねじロボ3ねじ締め完了を受信
1375 'MRtn = frInCheck(11876,1,MSETTIMEOUT05&)   'ねじロボ3ねじ締め完了を受信・
1376 'If MRtn = 0 Then
1377 '    fErrorProcess()         'エラー処理
1378 'EndIf
1379 'M_Out(12868) = 1 Dly 0.5     'ねじロボ3ねじ締め完了を送信(処理位置変更3/26中村)
1380 'チケットID書き込み
1381 'If M_20# <> MPass% Then M_20# = MAssyOK%
1382 M_20# = MAssyOK%
1383 '
1384 *ASSY_ERROR_END
1385 *AssyEnd
1386 *fnAssyStart_FEndPosi
1387 FEnd
1388 '
1389 '■fnPiasCheck
1390 ''' <summary>
1391 ''' PIASチケット読込み
1392 ''' </summary>
1393 ''' <returns>   0 : NG
1394 '''             1 : OK(読込み完了)
1395 ''' </returns>
1396 ''' <remarks>
1397 ''' Date   : 2021/07/07 : M.Hayakawa
1398 ''' </remarks>'
1399 Function M% fnPiasCheck
1400     fnPiasCheck = 0
1401     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1402     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1403 '
1404 *RETRY_PIAS
1405     M_20# = MClear%
1406     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1407     '
1408     '【IDチケット読み込み】
1409     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1410     MInspGroup%(1) = 1              '検査G番号
1411     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1412 '
1413     'エラーの場合
1414     If MRtn <> 1 Then
1415         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1416         If MRtn <> 1 Then
1417             'D720 -> D1300 コピー要求
1418             M_Out(12565) = 1
1419             Dly 0.5
1420             M_Out(12565) = 0
1421             'エラー処理記述
1422             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1423             'GOT KEY入力待ち
1424             MKeyNumber = fnKEY_WAIT()
1425         '
1426             Select MKeyNumber
1427                 Case MNext%         '次へを選択した場合
1428                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1429                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1430 '                    GoTo *fnPiasCheck_End                   'PIASチェック終了(関数外部で分岐するよう変更3/26中村)
1431                     Break
1432                 Case MAbout%        '停止を選択した場合
1433                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1434                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1435 '                    GoTo *fnPiasCheck_End                   'PIASチェック終了(関数外部で分岐するよう変更3/26中村)
1436                     Break
1437                 Case MNgProcess%    'NGを選択した場合
1438                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1439                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1440 '                    GoTo *fnPiasCheck_End                   'PIASチェック終了(関数外部で分岐するよう変更3/26中村)
1441                     Break
1442                 Case MContinue%     '継続を選択した場合
1443                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1444                     M_20# = MContinue%
1445 '                    GoTo *RETRY_PIAS                        'PIASチェックリトライ(関数外部で分岐するよう変更3/26中村)
1446                     Break
1447             End Select
1448         EndIf
1449     EndIf
1450     If M_20# <> MClear% Then GoTo *fnPiasCheck_End
1451 '
1452 '----------D720 -> D1300 コピー要求----------
1453     M_Out(12565) = 1
1454     Dly 0.5
1455     M_Out(12565) = 0
1456 '----------通信確認をする----------
1457     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1458     MRtn = 0                ' 初期化
1459     M_20# = MClear%         ' 初期化
1460     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1461     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）(関数外部で分岐するように変更3/26中村)
1462     If MRtn <> 1 Then GoTo *fnPiasCheck_End
1463 '        If M_20# = MContinue% Then
1464 '            GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1465 '        Else
1466 '            GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1467 '        EndIf
1468 '    EndIf
1469 '----------工程抜け確認----------
1470     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1471     MRtn = 0                ' 初期化
1472     M_20# = MClear%         ' 初期化
1473     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1474     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）(関数外部で分岐するように変更3/26中村)
1475     If MRtn <> 1 Then GoTo *fnPiasCheck_End
1476 '        If M_20# = MContinue% Then
1477 '            GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1478 '        Else
1479 '            GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1480 '        EndIf
1481 '    EndIf
1482     '
1483     fnPiasCheck = 1
1484     *fnPiasCheck_End
1485 FEnd
1486 '
1487 '■fnPCComuCheck
1488 ''' <summary>
1489 ''' PC-PLC通信チェック
1490 ''' </summary>
1491 ''' <returns>   0 : NG
1492 '''             1 : OK(読込み完了)
1493 ''' </returns>
1494 ''' <remarks>
1495 ''' Date   : 2021/07/07 : M.Hayakawa
1496 ''' </remarks>'
1497 Function M% fnPCComuCheck
1498     fnPCComuCheck = 0
1499     MJudge% = 0                                  '初期化
1500     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1501     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1502     '
1503     For MStaNo = 0 To 5
1504         '
1505         If M_In(MIN_PIAS_ComOK%) = 1 Then
1506             'PC通信OK(M400)
1507             MJudge% = MOK%
1508             MStaNo = 5
1509             Break
1510         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1511             'toRBT_通信確認time out
1512             MJudge% = MNG%
1513             MCommentD1001 = 15
1514             MCommentD1002 = 21
1515             MStaNo = 5
1516             Break
1517         Else
1518             'toRBT_通信確認time out
1519             MJudge% = MNG%
1520             MCommentD1001 = 14
1521             MCommentD1002 = 21
1522             Break
1523         EndIf
1524     Next MStaNo
1525     '
1526     '上記で返信フラグを受信してからPC通信確認OFF
1527     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1528     '
1529     'エラー画面
1530     If MJudge% <> MOK% Then
1531         M_20# = MClear%     '初期化
1532         'エラー処理記述
1533         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1534         'GOT KEY入力待ち
1535         MKeyNumber = fnKEY_WAIT()
1536         '
1537         If MKeyNumber = MAbout% Then            '停止を選択した場合
1538             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1539             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1540             Break
1541         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1542             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1543             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1544             Break
1545         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1546             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1547             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1548             Break
1549         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1550             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1551             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1552             Break
1553         EndIf
1554     Else
1555         'OKの場合
1556         fnPCComuCheck = 1
1557     EndIf
1558 FEnd
1559 '
1560 '■fnProcessCheck
1561 ''' <summary>
1562 ''' 工程抜け確認
1563 ''' </summary>
1564 ''' <returns>    1：工程履歴OK     0：異常終了
1565 '''             -1：前工程履歴NG  -2：自工程履歴あり
1566 '''             -3：モデル仕向NG  -4：タイムアウト
1567 '''             -5：履歴処理エラー
1568 ''' </returns>
1569 ''' <remarks>
1570 ''' Date   : 2021/07/07 : M.Hayakawa
1571 ''' </remarks>'
1572 Function M% fnProcessCheck
1573     fnProcessCheck = 0
1574     MJudge% = MNG%      '一旦NGを初期化とする
1575 '----------工程抜け確認----------
1576     MCommentD1001 = 0   'コメント初期化
1577     For MStaNo = 0 To 5
1578         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1579         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1580         '
1581         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1582             MJudge% = MOK%
1583             fnAutoScreenComment(85)     ' AUTO画面
1584             MStaNo = 5
1585             Break
1586         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1587             MFlgLoop% = 0
1588             MJudge% = MNG%
1589             MCommentD1001 = 27
1590             MCommentD1002 = 22
1591             fnAutoScreenComment(94)     ' AUTO画面
1592             fnProcessCheck = -2         ' NGは-2を返す
1593             MStaNo = 5
1594             Break
1595         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1596            MJudge% = MNG%
1597             MCommentD1001 = 31
1598             MCommentD1002 = 22
1599             fnAutoScreenComment(83)     ' AUTO画面
1600             fnProcessCheck = -3         ' NGは-3を返す
1601             MStaNo = 5
1602             Break
1603         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1604             '履歴NGは直ぐに終了せず繰り返し確認を行う
1605             '前工程の書込みが終了していない可能性があるため
1606             MJudge% = MNG%
1607             MCommentD1001 = 32
1608             MCommentD1002 = 22
1609             fnAutoScreenComment(84)     ' AUTO画面
1610             fnProcessCheck = -1         ' NGは-1を返す
1611             Dly 1.0
1612             '工程抜け確認OFF
1613             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1614             Dly 1.0
1615            'MStaNo = 5
1616             Break
1617         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1618             MFlgLoop% = 0
1619             MJudge% = MNG%
1620             MCommentD1001 = 29
1621             MCommentD1002 = 22
1622             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1623             fnProcessCheck = -5         ' NGは-5を返す
1624             MStaNo = 5
1625             Break
1626         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1627             MJudge% = MNG%
1628             If MCommentD1001 = 32 Then
1629                 '何もしない
1630             Else
1631                 MCommentD1001 = 26
1632             EndIf
1633             MCommentD1002 = 22
1634             fnProcessCheck = -4         ' NGは-4を返す
1635             MStaNo = 5
1636             Break
1637         Else
1638             MJudge% = MNG%
1639             MCommentD1001 = 28
1640             MCommentD1002 = 22
1641         EndIf
1642     Next MStaNo
1643     '工程抜け確認OFF
1644     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1645     '通過履歴NG 工程抜けの場合
1646     If MJudge% = MPass% Then
1647         M_20# = MPass%
1648     EndIf
1649     '
1650     'エラー画面
1651     If MJudge% <> MOK% Then
1652         M_20# = MClear%     '初期化
1653         'エラー処理記述
1654         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1655         'GOT KEY入力待ち
1656         MKeyNumber = fnKEY_WAIT()
1657         '
1658         Select MKeyNumber
1659             Case MAbout%        '停止を選択した場合
1660                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1661                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1662                 Break
1663             Case MNext%         '次へを選択した場合
1664                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1665                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1666                 Break
1667             Case MContinue%     '継続を選択した場合
1668                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1669                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1670                 Break
1671             Case MNgProcess%    'NGを選択した場合
1672                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1673                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1674                 Break
1675         End Select
1676     Else
1677         fnProcessCheck = 1  ' OKは1を返す
1678     EndIf
1679 FEnd
1680 '
1681 '■fnPiasWrite
1682 ''' <summary>
1683 ''' Pias 組立結果書込み要求
1684 ''' </summary>
1685 '''<param name="MFlg%">
1686 '''                 MOK%(1) = 工程履歴にOKを書込む
1687 '''                 MNG%(0) = 工程履歴にNGを書込む
1688 '''</param>
1689 '''<returns></returns>
1690 ''' <remarks>
1691 ''' Date   : 2021/07/07 : M.Hayakawa
1692 ''' </remarks>'
1693 Function M% fnPiasWrite(ByVal MFlg%)
1694       fnPiasWrite = 0
1695 *RETRY_PIASWRITE
1696     '
1697     '組立OK(MOK%)の場合　M306 ON
1698    '組立NG(MNG%)の場合　M307 ON
1699     If MFlg% = MOK% Then
1700         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1701     Else
1702         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1703     EndIf
1704     Dly 0.1                  '念のため
1705     '
1706     'Piasへ書込み開始 M305 -> ON
1707     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1708     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1709     '
1710     MJudge% = MNG%
1711     '
1712     For MStaNo = 0 To 5
1713         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1714             MJudge% = MOK%
1715             'MRet = fnAutoScreenComment(85)  'AUTO画面
1716             MStaNo = 5
1717             Break
1718         '
1719         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1720             MJudge% = MNG%
1721             'MRet = fnAutoScreenComment(85)  'AUTO画面
1722            MCommentD1001 = 34
1723            MCommentD1002 = 25
1724             MStaNo = 5
1725             Break
1726         '
1727         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1728             MJudge% = MNG%
1729             'MRet = fnAutoScreenComment(85)  'AUTO画面
1730            MCommentD1001 = 35
1731            MCommentD1002 = 25
1732             MStaNo = 5
1733             Break
1734         '
1735         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1736             MJudge% = MNG%
1737             'MRet = fnAutoScreenComment(85)  'AUTO画面
1738            MCommentD1001 = 36
1739            MCommentD1002 = 25
1740             MStaNo = 5
1741             Break
1742         '
1743         Else
1744             MJudge% = MNG%
1745            MCommentD1001 = 42
1746            MCommentD1002 = 25
1747         '
1748         EndIf
1749         '
1750     Next MStaNo
1751     '
1752     'Piasへ書込み開始 M305 -> OfF
1753     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1754     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1755     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1756     '
1757     '
1758     '通過履歴NG 工程抜けの場合
1759     If MJudge% = MPass% Then
1760         M_20# = MPass%
1761     EndIf
1762     '
1763    M_20# = MClear%     '初期化
1764     '
1765     'エラー画面
1766     If MJudge% < MOK% Then
1767     '
1768 '残しておくが現状では使用しないラベル
1769 *RETRY_ERR_WRITE
1770         M_20# = MClear%     '初期化
1771         'エラー処理記述
1772         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1773         'GOT KEY入力待ち
1774         MKeyNumber = fnKEY_WAIT()
1775         '
1776         If MKeyNumber = MAbout% Then   '停止を選択した場合
1777             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1778            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1779             Break
1780         '
1781         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1782             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1783             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1784         '
1785         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1786             M_20# = MPass%            'M_20# プログラム間共通外部変数
1787             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1788         '
1789         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1790             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1791            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1792             Break
1793         '
1794         EndIf
1795         '
1796         If M_20# = MClear% Then *RETRY_ERR_WRITE
1797         '
1798     EndIf
1799     '
1800     If M_20# = MContinue% Then *RETRY_PIASWRITE
1801     '
1802     fnPiasWrite = 1
1803     '
1804 FEnd
1805 '
1806 '■fnPCBNumberCheck
1807 ''' <summary>
1808 ''' Pias 基板番号照合要求
1809 ''' </summary>
1810 '''<param name="%"></param>
1811 '''<param name="%"></param>
1812 '''<returns></returns>
1813 ''' <remarks>
1814 ''' Date   : 2021/07/07 : M.Hayakawa
1815 ''' </remarks>'
1816 Function M% fnPCBNumberCheck
1817       fnPCBNumberCheck = 0
1818     '
1819 *RETRY_PCBCHECK
1820     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1821     'Piasへ基板照合開始 M310 -> ON
1822     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1823     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1824     '
1825     MJudge% = MNG%
1826     '
1827     For MStaNo = 0 To 5
1828         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1829             MJudge% = MOK%
1830             fnAutoScreenComment(96)  'AUTO画面
1831             MStaNo = 5
1832             Break
1833         '
1834         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1835             MJudge% = MNG%
1836             fnAutoScreenComment(97)  'AUTO画面
1837             MCommentD1001 = 37
1838             MCommentD1002 = 25
1839             MStaNo = 5
1840             Break
1841         '
1842         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1843             MJudge% = MNG%
1844             fnAutoScreenComment(98)  'AUTO画面
1845             MCommentD1001 = 38
1846             MCommentD1002 = 25
1847             MStaNo = 5
1848             Break
1849         '
1850         ElseIf M_In(11580) = 1 Then                         'time out
1851             MJudge% = MNG%
1852             fnAutoScreenComment(99)  'AUTO画面
1853             MCommentD1001 = 39
1854             MCommentD1002 = 25
1855             MStaNo = 5
1856             Break
1857         '
1858         Else
1859             MJudge% = MNG%
1860            MCommentD1001 = 41
1861            MCommentD1002 = 25
1862         '
1863         EndIf
1864         '
1865     Next MStaNo
1866     '
1867     'Piasへ基板照合開始 M310 -> OfF
1868     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1869     '
1870     '
1871     '通過履歴NG 工程抜けの場合
1872     If MJudge% = MPass% Then
1873         M_20# = MPass%
1874     EndIf
1875     '
1876    M_20# = MClear%     '初期化
1877     '
1878     'エラー画面
1879     If MJudge% < MOK% Then
1880     '
1881 '残しておくが現状では使用しないラベル
1882 *RETRY_ERR_PCBNUMBER
1883         M_20# = MClear%     '初期化
1884         'エラー処理記述
1885         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1886         'GOT KEY入力待ち
1887         MKeyNumber = fnKEY_WAIT()
1888         '
1889         If MKeyNumber = MAbout% Then   '停止を選択した場合
1890             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1891             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1892             Break
1893         '
1894         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1895             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1896             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1897         '
1898         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1899             M_20# = MPass%            'M_20# プログラム間共通外部変数
1900             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1901         '
1902         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1903             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1904             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1905             Break
1906         '
1907         EndIf
1908         '
1909         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1910         '
1911     EndIf
1912     '
1913     If M_20# = MContinue% Then *RETRY_PCBCHECK
1914 FEnd
1915 '
1916 '■ScrewTight_S2
1917 ''' <summary>
1918 ''' ねじ締めを行う
1919 ''' </summary>
1920 '''<param name="PScrewPos()">
1921 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
1922 '''             PScrewPos(2)    ：ねじ締め回避点
1923 '''             PScrewPos(10)   ：ねじ締め終了高さ
1924 '''</param>
1925 '''<returns>整数
1926 '''         0=異常終了、1=正常終了
1927 '''</returns>
1928 ''' <remarks>
1929 ''' Date   : 2021/07/07 : M.Hayakawa
1930 ''' </remarks>'
1931 Function M% ScrewTight_S2(ByVal PScrewPosition())   'ネジ締め個別設定
1932     ScrewTight_S2 = 0
1933     MOKNGFlg = 0
1934     Ovrd 100
1935     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1936     ' 暫定
1937     Ovrd 5
1938     Mvs PScrewPosition(10),-10    ' パレット上ねじ締めS①の上空へ移動
1939 '    Ovrd MOvrdA
1940     '暫定マスク
1941 '    M_Out(Y62_Driver)=1     ' バンクセッティング　C1
1942 '    Dly 0.1
1943 '    M_Out(Y61_Driver)=1     'ドライバーON　CW
1944 '    'Spd 8.3 '外部ライド100  内部ライド60   'ライド100-40　100%：Spd　15　'ねじ締め速度設定
1945 '    Spd MSpdA               'ネジ締め時Spd個別設定
1946     ' 暫定移動のみ
1947     Mvs PScrewPosition(10)
1948 '    '
1949 '    Dly 0.1
1950 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1951 '    Wait M_In(11584)=1          '完了/エラー検出
1952 '    Dly 0.1
1953 '    Spd M_NSpd
1954 '    '
1955 '    If M_In(X28_Driver)=1 Then  'ねじトータルエラー検出時
1956 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1957 '        Dly 0.1
1958 '        M_Out(Y62_Driver)=0     'バンクセッティング解除　C1
1959 '        Dly 0.1
1960 '        M_Out(Y63_Driver)=0     'バンクセッティング解除　C1
1961 '        Dly 0.1
1962 '        M_Out(Y65_Driver)=0     'プログラム解除　F1
1963 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
1964 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
1965 '        MOKNGFlg = -1
1966 '        ScrewTight_S2 = 0
1967 '    Else
1968 '        Wait M_In(X29_Driver)=1 ' 正常完了時
1969 '        Dly 0.1
1970 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1971 '        Dly 0.1
1972 '        M_Out(Y62_Driver)=0     'バンクセッティング解除
1973 '        Dly 0.1
1974 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
1975 '        Dly 0.1
1976 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
1977 '        ScrewTight_S2 = 1
1978 '    EndIf
1979 ' 暫定
1980     Ovrd 10
1981     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1982     Ovrd 100
1983 FEnd
1984 '
1985 '■ScrewGet_S3
1986 ''' <summary>
1987 ''' ねじ供給機からねじを得る
1988 ''' </summary>
1989 '''<param name="%"></param>
1990 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
1991 '''         PScrewPos(2)    ：ねじ供給器回避点
1992 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1993 '''         PScrewPos(3)    ：Mねじポカヨケ位置
1994 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
1995 '''<returns>整数
1996 '''         0=異常終了、1=正常終了、-1=MネジセンサーNG、-2=MネジセンサーON、-3=吸着エラー
1997 '''</returns>
1998 ''' <remarks>
1999 ''' Date   : 2021/07/07 : M.Hayakawa
2000 ''' </remarks>'
2001 Function M% ScrewGet_S3(ByVal PScrewPosition())
2002     ScrewGet_S3 = 0
2003     MMScrewJudge% = 0
2004     'ねじ供給器初期動作エラーチェック
2005 ' ↓暫定削除
2006 '    Wait M_In(X34_ScrewReady1)=1 'ねじ供給器SがReadyになるまで待つ　←　何秒か待ってReadyにならなければ抜けるプログラムが必要？
2007 '    Ovrd 100
2008 '    If M_In(X33_SS2)=0 Then  'Mねじ検出センサがOFF（故障）していた場合
2009 '        Ovrd 30
2010 '        Mvs,-80             'その場所から80mm上空へ移動
2011 '        Mov PInitPos19049   '19049初期位置へ移動
2012 '        M_Out(Y6A_VV1)=0    'ねじ吸着 Off
2013 '        'NGとしてここの関数から抜ける
2014 '        ScrewGet_S3 = -1
2015 '        MMScrewJudge% = 1
2016 '        MCommentD1001 = 61
2017 '    EndIf
2018 '    If ScrewGet_S3 = 0 Then
2019 '        'Sタイト用ねじ供給機にMねじが混入していないか監視
2020 '        MMScrewJudge% = 0 'MMScrewJudgeを初期化する
2021 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT05&)
2022 '        If MRtn = 0 Then
2023 '            Ovrd 30
2024 '            Mvs,-80            'その場所から50mm上空へ移動
2025 '            Mov PInitPos19049  '19049初期位置へ移動
2026 '            MMScrewJudge% = 2
2027 '            MRtn = All_CLamp_Release()'全てのクランプ解除へ分岐
2028 '            MCnt% = 2   '2を設定
2029 '            MCommentD1001 = 62
2030 '        EndIf
2031 '        If MMScrewJudge% = 2 Then
2032 '            ScrewGet_S3 = -2
2033 '        EndIf
2034 '    EndIf
2035 '    'Mネジ判定がONの場合 NGとして関数を抜ける
2036 '    If MMScrewJudge% = 2 Then
2037 '        ScrewGet_S3 = -2
2038 '    EndIf
2039     'Sネジ用ねじ太郎のMネジ混入確認用ここまで
2040     Ovrd 100
2041     Spd M_NSpd
2042     If MMScrewJudge% = 0 Then
2043         ScrewGet_S3 = 0
2044         M_Out(Y63_Driver)=1         ' バンクセッティング　C2
2045         MScrewCnt% = 0
2046         MFinCnt% = 2
2047 '        For MCnt% = 0 To MFinCnt%
2048             Mov PScrewPosition(2)        ' ねじ供給機回避点
2049             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
2050             Ovrd 80
2051             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2052             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
2053             Mvs PScrewPosition(10), 1.2
2054             M_Out(Y6A_VV1)=1        ' ねじ吸着　ON
2055             'ビット回転
2056             M_Out(Y60_Driver)=1
2057             Dly 0.2
2058             '
2059             Ovrd 100
2060             JOvrd M_NJovrd
2061             Spd M_NSpd
2062             'ネジ吸着確認位置移動
2063             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
2064             Mvs PScrewPosition(10), -15  ' ネジ吸着確認位置
2065             'ビット回転停止
2066             'M_Out(Y60_Driver)=0
2067             '
2068             '1秒間ネジ吸着確認
2069 ' 以下暫定削除
2070 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2071 '            'MRtn = 0'強制エラー
2072 '            '吸着エラーの場合
2073 '            'ネジをねじ太郎に戻す
2074 '            If MRtn = 0 Then
2075 '                Ovrd 30
2076 '                'ビット回転停止
2077 '                M_Out(Y60_Driver)=0
2078 '                'ネジ供給機上空
2079 '                Mvs PScrewPos(1)
2080 '                '更に上空
2081 '                Mov PScrewPos(1), -75
2082 '                'ネジ捨て位置
2083 '                Mov PScrewFeedS021
2084 '                '吸着OFF
2085 '                M_Out(Y6A_VV1)=0 'ねじ吸着　OFF
2086 '                Dly 0.2
2087 '                '破壊ON
2088 '                M_Out(Y6B_VB1)=1 '真空破壊ON
2089 '                'ビット回転
2090 '                M_Out(Y61_Driver)=1
2091 '                Dly 0.5
2092 '                '
2093 '                Ovrd 100
2094 '                JOvrd M_NJovrd
2095 '                Spd M_NSpd
2096 '                'ドライバーを上下させねじを振り落とす
2097 '                Mov PScrewFeedS021, 10
2098 '                Mov PScrewFeedS021
2099 '                Dly 0.1
2100 '                Mov PScrewFeedS021, 10
2101 '                Mov PScrewFeedS021
2102 '                '
2103 '                'ネジ落ち待ち
2104 '                'ビット回転停止
2105 '                M_Out(Y61_Driver)=0
2106 '                Dly 0.1
2107 '                '破壊OFF
2108 '                M_Out(Y6B_VB1)=0 '真空破壊OFF
2109 '                '
2110 '                '
2111 '                'ねじ落ちたとして、移動更に上空
2112 '                Mov PScrewPos(1), -75
2113 '                Ovrd 100
2114 '                Spd M_NSpd
2115 '                'ネジ供給機上空
2116 '                Mvs PScrewPos(1)
2117 '                '
2118 '                ScrewGet_S3 = -3
2119 '                Break
2120 '                '
2121 '            Else
2122 '                MCnt% = MFinCnt%
2123 '                ScrewGet_S3 = 0
2124 '            EndIf
2125 '        Next  MCnt%
2126         '
2127         Ovrd 100
2128         Spd M_NSpd
2129         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2130         M_Out(Y60_Driver)=0     ' ビット回転停止
2131         M_Out(Y63_Driver)=0     ' バンクセッティング　C2
2132         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2133         'もう一度吸着確認
2134 ' 以下暫定削除
2135 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2136 '        If MRtn = 0 Then      '吸着エラーの場合
2137 '            MCommentD1001 = 94
2138 '            MCommentD1002 = 95
2139 '            ScrewGet_S3 = -3
2140 '        EndIf
2141 '        If MRtn = 1 Then      '吸着OKの場合
2142 '            ScrewGet_S3 = 1
2143 '        EndIf
2144 '        Break
2145     Else
2146         'Mネジ
2147         If MMScrewJudge% = 2 Then
2148             ScrewGet_S3 = -2
2149         EndIf
2150     EndIf
2151 FEnd
2152 '
2153 '■fnKEY_WAIT()
2154 ''' <summary>
2155 ''' GOTからのキー入力待ち
2156 ''' </summary>
2157 '''<returns>1：停止    2：次へ
2158 '''         3：継続    4：トルクチェック開始
2159 '''         5：NG
2160 '''         11：ロボット初期位置1    12：ロボット初期位置2
2161 '''         13：ロボット初期位置3    14：ロボット初期位置4
2162 '''</returns>
2163 ''' <remarks>
2164 ''' Date   : 2021/07/07 : M.Hayakawa
2165 ''' </remarks>'
2166 Function M% fnKEY_WAIT()
2167     fnKEY_WAIT = 0
2168     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
2169     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
2170     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
2171     '下記キー待ちの継続に反応させないため
2172     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
2173     Dly 0.2
2174     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
2175     MLocalLoopFlg=1
2176     While MLocalLoopFlg=1
2177         If M_In(11345) = 1 Then         '停止   M5345
2178             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
2179             fnKEY_WAIT = 1
2180             MLocalLoopFlg=-1
2181             Break
2182         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
2183             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
2184             fnKEY_WAIT = 2
2185             MLocalLoopFlg=-1
2186             Break
2187         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
2188             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
2189             fnKEY_WAIT = 3
2190             MLocalLoopFlg=-1
2191             Break
2192         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
2193             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
2194             fnKEY_WAIT = 4
2195             MLocalLoopFlg=-1
2196             Break
2197         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
2198             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
2199             fnKEY_WAIT = 5
2200             MLocalLoopFlg=-1
2201             Break
2202             '
2203         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
2204             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
2205             fnKEY_WAIT = MRobotInit1%
2206             MLocalLoopFlg=-1
2207             Break
2208             '
2209         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
2210             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
2211             fnKEY_WAIT = MRobotInit2%
2212             MLocalLoopFlg=-1
2213             Break
2214             '
2215         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
2216             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
2217             fnKEY_WAIT = MRobotInit3%
2218             MLocalLoopFlg=-1
2219             Break
2220             '
2221         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
2222             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
2223             fnKEY_WAIT = MRobotInit4%
2224             MLocalLoopFlg=-1
2225             Break
2226             '
2227         Else
2228         EndIf
2229     WEnd
2230     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
2231     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
2232 FEnd
2233 '
2234 '■ fnAUTO_CTL
2235 ''' <summary>
2236 ''' AUTOモードOFF、PLCからの開始待ち
2237 ''' </summary>
2238 ''' <remarks>
2239 ''' Date   : 2021/07/07 : M.Hayakawa
2240 ''' </remarks>
2241 Function M% fnAUTO_CTL
2242     fnAUTO_CTL = 0
2243     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2244     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
2245     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2246     '
2247     If M_Svo=0 Then             'サーボON確認
2248         Servo On
2249     EndIf
2250     Wait M_Svo=1
2251 FEnd
2252 '
2253 '■ fnWindScreenOpen
2254 ''' <summary>
2255 ''' ウィンド画面の表示、非表示設定
2256 ''' </summary>
2257 '''<param name="%"></param>
2258 '''<param name="%"></param>
2259 '''<param name="%"></param>
2260 '''<param name="%"></param>
2261 ''' <remarks>
2262 ''' コメントD1001, D1002, D1003の設定
2263 ''' MWindReSet = 0     画面非表示
2264 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2265 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2266 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2267 ''' Date   : 2021/07/07 : M.Hayakawa
2268 ''' </remarks>
2269 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2270     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2271         M_Out16(12480) = MCommentD1001            'D1001 コメント
2272     EndIf
2273     '
2274     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2275         M_Out16(12496) = MCommentD1002            'D1002 コメント
2276     EndIf
2277     '
2278     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2279        M_Out16(12512) = MCommentD1003            'D1003 コメント
2280     EndIf
2281     '
2282     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2283     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2284     Dly 0.5
2285     M_Out(12363) = 0                         'ウィンド画面設定
2286 FEnd
2287 '
2288 '■FnCtlValue2
2289 ''' <summary>
2290 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2291 ''' </summary>
2292 ''' <param name="MCtlNo%"></param>
2293 ''' <remarks>
2294 ''' Date : 2022/04/28 渡辺
2295 ''' </remarks>
2296 '''
2297 '''  1：投入数       ＋１
2298 '''  2：組立ＯＫ数   ＋１
2299 '''  3：組立ＮＧ数   ＋１ (未使用)
2300 '''  4：吸着エラー数 ＋１
2301 ''' 99：読書開始信号 OFF
2302 '''
2303 Function M% FnCtlValue2(ByVal MCtlNo%)
2304     FnCtlValue2 = 1
2305     Select MCtlNo%
2306         Case 1        '投入数＋１
2307             M_Out(12569) = 0             '書込み開始信号OFF
2308             M_Out(12568) = 1             '読込み開始信号ON
2309             MInputQty = M_In16(11600)    '投入数受信
2310             MInputQty = MInputQty + 1    '投入数＋１
2311             M_Out16(12592) = MInputQty   '投入数送信
2312             M_Out(12569) = 1             '書込み開始信号ON
2313             Break
2314             '
2315         Case 2        '組立ＯＫ数＋１
2316             M_Out(12569) = 0             '書込み開始信号OFF
2317             M_Out(12568) = 1             '読込み開始信号ON
2318             MAssyOkQty = M_In16(11616)   '組立OK数受信
2319             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2320             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2321             M_Out(12569) = 1             '書込み開始信号ON
2322             Break
2323             '
2324         Case 4        '吸着エラー数＋１
2325             M_Out(12569) = 0                       '書込み開始信号OFF
2326             M_Out(12568) = 1                       '読込み開始信号ON
2327             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
2328             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
2329             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
2330             M_Out(12569) = 1                       '書込み開始信号ON
2331             Break
2332             '
2333         Case 99        '読書開始信号OFF
2334             M_Out(12568) = 0        '読込み開始信号OFF
2335             M_Out(12569) = 0        '書込み開始信号OFF
2336             Break
2337             '
2338     End Select
2339     Exit Function
2340 FEnd
2341 '
2342 'Insightによる画像処理検査実行（並列処理なし）
2343 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2344 '-------------------------------------------------------------------------------
2345 'Insightによる画像処理検査実行（並列処理なし）
2346 '   引数
2347 '       PInspPos()      ：検査位置
2348 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2349 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2350 '       MInspCnt%       ：検査位置数
2351 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2352 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2353 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2354 '   戻り値：整数
2355 '       0=異常終了、1=正常終了
2356 '
2357 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2358 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2359 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2360 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2361 '   20200410    :   検査グループ設定Retry追加
2362 '-------------------------------------------------------------------------------
2363     '----- 初期設定 -----
2364     Cnt 0                                                           '移動効率化解除(初期値=0)
2365     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2366 '    Cnt 1,0.1,0.1
2367     '変数宣言・初期化
2368     Def Inte MNum                                                   '検査番号(検査順1～)
2369     MNum% = 1                                                       '検査番号初期値設定
2370     Def Inte MEndFlg                                                '検査終了フラグ
2371     MEndFlg% = 0
2372     '
2373     '検査G番号設定要求・検査実行要求off
2374     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2375     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2376     'エラー番号クリア
2377     MInspErrNum = 0                                                 '検査実行エラー番号
2378     M_Out16(MOUT_InspErrNum) = MInspErrNum
2379     MInspNGStepNum = 0                                              '検査実行NGStep番号
2380     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2381     '
2382     'Insight Ready check?
2383     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2384         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2385         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2386         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2387         ISInspectionSingle = 0                                      '異常終了戻り値設定
2388         Exit Function
2389     EndIf
2390     '
2391     '検査位置数確認
2392     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2393         MInspErrNum = 21                                            '検査データなし 21　引数<1
2394         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2395         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2396         ISInspectionSingle = 0                                      '異常終了戻り値設定
2397         Exit Function
2398     EndIf
2399     '
2400     '
2401     '
2402     '----- メイン処理 -----
2403     '設定された検査位置数分の検査実行
2404     While( MEndFlg% = 0 )
2405         '----- 検査グループ番号設定Retry追加 20200410
2406         MSetGrNumRetryExitFlg = 0
2407         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2408         While( MSetGrNumRetryExitFlg = 0 )
2409         '----- 検査グループ番号設定Retry追加ここまで 20200410
2410             '
2411             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2412             '
2413             '----- 検査グループ番号設定 -----
2414             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2415             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2416             '
2417             '検査位置へ移動・移動完了待ち
2418             Mvs PInspPos( MNum% )                                       '移動
2419             Dly 0.05                                                    '移動完了後Delay
2420             '
2421             '検査グループ番号設定終了確認
2422             M_Timer(1) = 0
2423             MExitFlg = 0
2424             While( MExitFlg = 0 )
2425                 '検査G設定正常終了?
2426                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2427                     MExitFlg = 1
2428                 '
2429                 '検査G設定異常終了?
2430                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2431                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2432                     If MInspErrNum = 0 Then                             '1回目のエラー?
2433                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2434                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2435                     EndIf
2436                     MExitFlg = 1
2437                 '
2438                 'timeoutチェック
2439                 ElseIf 1000 < M_Timer(1) Then
2440                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2441                     If MInspErrNum = 0 Then                             '1回目のエラー?
2442                         MInspErrNum = 12                                'timeout エラー番号=12
2443                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2444                     EndIf
2445                     MExitFlg = 1
2446                 EndIf
2447             WEnd
2448             '
2449             '検査G番号設定要求off
2450             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2451             '
2452             '----- 検査グループ設定Retry追加 20200410
2453             'NGなければ抜ける
2454             If MCurrentStepErr = 0 Then
2455                 MSetGrNumRetryExitFlg = 1
2456             Else
2457                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2458                 If MSetGrNumRetryCnt = 0 Then
2459                     MSetGrNumRetryExitFlg = 1
2460                 Else
2461                     'Retryへ　その前にDelay
2462                     Dly 0.5
2463                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2464                 EndIf
2465             EndIf
2466             '----- 検査グループ設定Retry追加ここまで 20200410
2467             '
2468         WEnd
2469         '
2470         '
2471         '
2472         '----- 検査実行 -----
2473         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2474             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2475                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2476                 MInspRetryExitFlg = 0
2477                 MRetryCnt = 2                                        'Retry回数設定
2478                 While( MInspRetryExitFlg = 0 )
2479                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2480                     '
2481                     '検査完了確認
2482                     MRetryCnt = MRetryCnt - 1
2483                     M_Timer(1) = 0
2484                     MExitFlg = 0
2485                     While( MExitFlg = 0 )
2486                     '検査完了待ち
2487                         '検査OK終了?
2488                         If M_In( MIN_IS_InspOK% ) = 1  Then
2489                             MJudgeOKFlg = 1                         '検査OKフラグON
2490                             MExitFlg = 1
2491                         '
2492                         '検査NG終了?
2493                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2494                             If MInspErrNum = 0 Then                 '1回目のエラー?
2495                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2496                                     MInspErrNum = 32                    '検査NG エラー番号=32
2497                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2498                                 EndIf
2499                             EndIf
2500                             MExitFlg = 1
2501                         '
2502                         '検査異常終了(IS timeout)?
2503                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2504                             If MInspErrNum = 0 Then                 '1回目のエラー?
2505                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2506                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2507                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2508                                 EndIf
2509                             EndIf
2510                             MExitFlg = 1
2511                         '
2512                         'timeoutチェック
2513                         ElseIf 3000 < M_Timer(1) Then
2514                             If MInspErrNum = 0 Then                 '1回目のエラー?
2515                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2516                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2517                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2518                                 EndIf
2519                             EndIf
2520                             MExitFlg = 1
2521                         EndIf
2522                     WEnd
2523                     '
2524                     '検査開始要求off
2525                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2526                     '
2527                     'OKなら抜ける
2528                     If MJudgeOKFlg = 1 Then
2529                         MInspRetryExitFlg = 1
2530                     Else
2531                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2532                         If MRetryCnt = 0 Then
2533                             MInspRetryExitFlg = 1
2534                         Else
2535                             'Retryへ　その前にDelay
2536                             Dly 0.3
2537                         EndIf
2538                     EndIf
2539                     '
2540                 WEnd
2541             EndIf
2542         EndIf
2543         '
2544         '
2545         '
2546         MNum% = MNum% + 1                                           '検査Step+1
2547         '検査終了確認　検査終了フラグセット
2548         If (MInspCnt% < MNum% ) Then
2549             MEndFlg% = 1                                            '検査終了フラグセット
2550         EndIf
2551         'NG発生時続行時処理
2552         If MInspErrNum <> 0 Then                                    'NGあり?
2553             If MNgContinue% <> 1 Then                               'NG続行?
2554                 MEndFlg% = 1                                        '検査終了フラグセット
2555             EndIf
2556         EndIf
2557     WEnd
2558     '
2559     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2560     If 0 < MZAxis% Then
2561         PCurrentPos = P_Curr                                        '現在位置取得
2562         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2563         Mvs PCurrentPos                                             '現在位置上空へ移動
2564     EndIf
2565     '
2566     '戻り値設定
2567     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      'カメラ検査強制OK(M_In(11372)=1)追加(12/21中村)
2568         ISInspectionSingle = 1                                      '正常終了戻り値設定
2569     Else
2570         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2571         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2572         ISInspectionSingle = 0                                      '異常終了戻り値設定
2573     EndIf
2574     '
2575 FEnd
2576 '
2577 ' ■ISInspection
2578 ''' <summary>
2579 ''' Insightによる画像処理検査実行
2580 ''' </summary>
2581 '''<param name="PInspPos()">検査位置</param>
2582 '''<param name="MInspGrNum%()">検査位置での検査グループ番号（=0：画像検査未実施）</param>
2583 '''             PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2584 '''<param name="MInspCnt%">検査位置数</param>
2585 '''<param name="MZAxis%">終了時のZ軸退避座標（-1:無効）</param>
2586 '''             終了時にZ軸をMZAxisで設定された位置まで上昇させる
2587 '''<param name="MNgContinue%">=1で検査エラー・NG発生時に全Stepの検査を行う</param>
2588 '''<returns>    整数 0=異常終了、1=正常終了</returns>
2589 '''         MInspErrNum     ：異常終了時にエラー番号が設定される
2590 '''         MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される"
2591 ''' <remarks>
2592 ''' Date   : 2021/07/07 : M.Hayakawa
2593 ''' </remarks>
2594 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2595 '    '画像使用確認 0<- 画像確認無しの場合
2596 '    If M_In(11369) = 0 Then            'toRBT_使用確認
2597 '        ISInspection = 1                                        '正常終了戻り値設定
2598 '    EndIf
2599 ''
2600 '    Cnt 0                                                       '移動効率化解除(初期値=0)
2601 '    Fine 0.05,P                                                 '位置決め完了条件設置　0.05mm
2602 '    MNum% = 1                                                   '検査番号初期値設定
2603 '    Def Inte MEndFlg                                            '検査終了フラグ
2604 '    MEndFlg% = 0
2605 '    '
2606 '    'エラー番号クリア
2607 '    MInspErrNumSub = 0                                          '検査実行エラー番号sub
2608 '    MInspErrNum = 0                                             '検査実行エラー番号
2609 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2610 '    MInspNGStepNum = 0                                          '検査実行NGStep番号
2611 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2612 '    '
2613 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready offなら終了
2614 '        MInspErrNum = 20                                        '検査実行エラー番号 20 Insight offline
2615 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2616 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2617 '        ISInspection = 0                                        '異常終了戻り値設定
2618 ''
2619 '    EndIf
2620 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2621 '    '
2622 '    '検査位置数確認
2623 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2624 '        MInspErrNum = 21                                        '検査データなし 21　引数<1
2625 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2626 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2627 '        ISInspection = 0                                        '異常終了戻り値設定
2628 ''
2629 '    EndIf
2630 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2631 '    '
2632 '    '設定された検査位置数分の検査実行
2633 '    While( MEndFlg% = 0 )
2634 '        '検査終了確認　検査終了フラグセット
2635 '        If (MInspCnt% < MNum% ) Then
2636 '            MEndFlg% = 1                                        '検査終了フラグセット
2637 '        EndIf
2638 '        '
2639 '        'タスク　検査G番号設定・検査完了確認処理開始　INSPTAST1
2640 '        If MEndFlg% = 0 Then
2641 '            M_01# = MInspGrNum%(MNum%)                          '検査G番号引渡し
2642 '        EndIf
2643 '        M_02# = MEndFlg%                                        '検査終了フラグ引渡し
2644 '        M_05# = MNum%                                           '検査番号(検査順1～)
2645 '        'タスク　検査G設定フラグ引渡し
2646 '        If MEndFlg% = 0 Then
2647 '            If 0 < MInspGrNum%(MNum%) Then
2648 '                M_03# = 1
2649 '            Else
2650 '                M_03# = 0
2651 '            EndIf
2652 '        Else
2653 '            M_03# = 0
2654 '        EndIf
2655 '        'タスク　検査結果確認フラグ引渡し
2656 '        If 1 < MNum% Then
2657 '            If 0 < MInspGrNum%(MNum%-1) Then
2658 '                M_04# = 1
2659 '            Else
2660 '                M_04# = 0
2661 '            EndIf
2662 '        Else
2663 '            M_04# = 0
2664 '        EndIf
2665 '        '
2666 '        'タスク処理開始
2667 '        M_00# = 1                                               'TASK処理開始
2668 '        'タスク処理開始確認
2669 '        M_Timer(1) = 0
2670 '        MExitFlg = 0
2671 '        While( MExitFlg = 0 )
2672 '            '処理開始完了確認
2673 '            If M_00# = 0 And M_10# = 8 Then
2674 '                MExitFlg = 1
2675 '            EndIf
2676 '            'timeoutチェック
2677 '            If 2000 < M_Timer(1) Then
2678 '                If MNgContinue% = 1 Then                        'NG続行?
2679 '                    MInspErrNumSub = 36                         'エラー番号設定36
2680 '                Else
2681 '                    MInspErrNum = 36                            'エラー番号設定36
2682 '                EndIf
2683 '                MExitFlg = 1
2684 '            EndIf
2685 '        WEnd
2686 '        '
2687 '        '検査位置へ移動・移動完了待ち
2688 '        If 0 = MInspErrNum Then
2689 '            If MEndFlg% = 0 Then
2690 '                Mvs PInspPos( MNum% )                           '移動
2691 '            EndIf
2692 '        EndIf
2693 '        '
2694 '        'タスク　検査G番号設定・検査完了確認処理終了待ち　INSPTAST1
2695 '        If 0 = MInspErrNum Then
2696 '            M_Timer(1) = 0
2697 '            MExitFlg = 0
2698 '            While( MExitFlg = 0 )
2699 '                '処理完了待ち（正常終了）
2700 '                If M_10# = 1 Then
2701 '                    MExitFlg = 1
2702 '                EndIf
2703 '                '処理完了待ち（異常終了）
2704 '                If M_10# = 0 Then
2705 '                    If MNgContinue% = 1 Then                    'NG続行?
2706 '                        MInspErrNumSub = M_12#                  'エラー番号設定　M12
2707 '                    Else
2708 '                        MInspErrNum = M_12#                     'エラー番号設定　M12
2709 '                    EndIf
2710 '                    MExitFlg = 1
2711 '                EndIf
2712 '                'timeoutチェック
2713 '                If 5000 < M_Timer(1) Then
2714 '                    If MNgContinue% = 1 Then                    'NG続行?
2715 '                        MInspErrNumSub = 31                     'エラー番号設定31
2716 '                    Else
2717 '                        MInspErrNum = 31                        'エラー番号設定31
2718 '                    EndIf
2719 '                    MExitFlg = 1
2720 '                EndIf
2721 '            WEnd
2722 '        EndIf
2723 '        '
2724 '        '検査結果確認
2725 '        If 0 = MInspErrNum Then
2726 '            If 1 < MNum% Then
2727 '                If 0 < MInspGrNum%(MNum%-1) Then                '検査あり?
2728 '                    If M_11# = 2 Then                           '検査NG?
2729 '                        If MNgContinue% = 1 Then                'NG続行?
2730 '                            If MInspNGStepNum = 0 Then          'NG未発生?
2731 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2732 '                            EndIf
2733 '                            MInspErrNumSub = 32                 'エラー番号設定 32:検査NG
2734 '                        Else
2735 ''                            MInspNGStepNum = MNum% - 1          '検査実行NGStep番号設定
2736 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2737 '                            MInspErrNum = 32                    'エラー番号設定 32:検査NG
2738 '                        EndIf
2739 '                   EndIf
2740 '                EndIf
2741 '            EndIf
2742 '        EndIf
2743 '        '
2744 '        'エラーなら検査中断終了するのでLoopから抜けるため終了フラグセット
2745 '        If 0 <> MInspErrNum Then
2746 '            MEndFlg% = 1
2747 '        EndIf
2748 '        '
2749 '        '検査実行、取込完了待ち
2750 '        If 0 = MInspErrNum Then
2751 '            If MEndFlg% = 0 Then
2752 '                If 0 < MInspGrNum%(MNum%) Then                  '検査あり?
2753 '                    M_Out(MOUT_IS_Insp%) = 1                    '検査実行要求on
2754 '                    '取込完了確認
2755 '                    M_Timer(1) = 0
2756 '                    MExitFlg = 0
2757 '                    While( MExitFlg = 0 )
2758 '                        '処理完了待ち
2759 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2760 '                            MExitFlg = 1
2761 '                        EndIf
2762 '                        'timeoutチェック
2763 '                        If 2000 < M_Timer(1) Then
2764 '                            If MNgContinue% = 1 Then            'NG続行?
2765 '                                MInspErrNumSub = 33             'エラー番号設定33
2766 '                            Else
2767 '                                MInspErrNum = 33                'エラー番号設定33
2768 '                            EndIf
2769 '                            MExitFlg = 1
2770 '                        EndIf
2771 '                    WEnd
2772 '                EndIf
2773 '                '
2774 '            EndIf
2775 '        EndIf
2776 '        MNum% = MNum% + 1
2777 '    WEnd
2778 '    '
2779 '    '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2780 '    If 0 < MZAxis% Then
2781 '        PCurrentPos = P_Curr                                    '現在位置取得
2782 '        PCurrentPos.Z = MZAxis%                                 'Z軸を設定
2783 '        Mvs PCurrentPos                                         '現在位置上空へ移動
2784 '    EndIf
2785 '    '
2786 '    'NG続行時処理
2787 '    If MNgContinue% = 1 Then                                    'NG続行?
2788 '        MInspErrNum = MInspErrNumSub                            'エラー番号設定
2789 '    EndIf
2790 '    '
2791 '    '戻り値設定
2792 '    If MInspErrNum = 0 Then
2793 '        ISInspection = 1                                        '正常終了戻り値設定
2794 '    Else
2795 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2796 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2797 '        ISInspection = 0                                        '異常終了戻り値設定
2798 '    EndIf
2799 '    '
2800 '*ISInspection_End
2801 'FEnd
2802 '
2803 '■InitialZoneB
2804 ''' <summary>
2805 ''' 非常停止後の復帰動作
2806 ''' 1)上空退避　Z方向上に移動
2807 ''' 2)J1軸以外を退避ポジションへ移動
2808 ''' 3)J1軸のみを退避ポジションへ移動
2809 ''' 4)イニシャルポジションへ移動
2810 ''' </summary>
2811 ''' <remarks>
2812 ''' Date : 2022/04/11 : N.Watanabe
2813 ''' </remarks>
2814 Function V fnInitialZoneB()
2815     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中] 2022/04/26 渡辺
2816 '
2817 'パラメータ
2818     Ovrd 5
2819 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2820 '    Cmp Pos, &B100011
2821 '
2822 '復帰動作開始
2823 '
2824 '置き台と両掴みの場所は、チャックを解放する
2825 *RecoveryChuckOpen
2826     PActive = P_Curr          '現在位置を取得
2827     MRecoveryChuckOpen = 0    'チャック解放フラグ 初期化
2828 'PMechaOnRoboSet(DVDメカ置き場)は、チャック解放
2829     If (PActive.X <= PMechaOnRoboSet.X + 1.0) And (PActive.X >= PMechaOnRoboSet.X -1.0) Then
2830         If (PActive.Y <= PMechaOnRoboSet.Y + 1.0) And (PActive.Y >= PMechaOnRoboSet.Y -1.0) Then
2831             If (PActive.Z <= PMechaOnRoboSet.Z + 1.0) And (PActive.Z >= PMechaOnRoboSet.Z -1.0) Then
2832                 MRecoveryChuckOpen = 1
2833             EndIf
2834         EndIf
2835     EndIf
2836 'PMechaOnRoboGet(DVDメカ受け取り位置)は、チャック解放
2837     If (PActive.X <= PMechaOnRoboGet.X + 1.0) And (PActive.X >= PMechaOnRoboGet.X -1.0) Then
2838         If (PActive.Y <= PMechaOnRoboGet.Y + 1.0) And (PActive.Y >= PMechaOnRoboGet.Y -1.0) Then
2839             If (PActive.Z <= PMechaOnRoboGet.Z + 1.0) And (PActive.Z >= PMechaOnRoboGet.Z -1.0) Then
2840                 MRecoveryChuckOpen = 1
2841             EndIf
2842         EndIf
2843     EndIf
2844 '
2845 '    If MRecoveryChuckOpen = 1 Then
2846 '        M_Out(12256) = 0        'DVDチャック閉OFF
2847 '        M_Out(12257) = 1        'DVDチャック開ON
2848 '        M_20# = 0               'KEY入力初期化
2849 '        MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
2850 '        If MRtn = 0 Then
2851 '            fErrorProcess(11,270,284,0)
2852 '            If M_20# = MNext% Then M_20# = MClear%
2853 '            If M_20# = MAbout% Then GoTo *RecoveryEnd
2854 '            If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2855 '            If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2856 '        Else
2857 '            M_Out(12257) = 0        'DVDチャック開OFF
2858 '        EndIf
2859 '    EndIf
2860 '
2861     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2862     M_Out(12256) = 0                           'DVDチャック閉OFF
2863     M_Out(12257) = 1                           'DVDチャック開ON
2864 '
2865     M_20# = 0                                  'KEY入力初期化
2866     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
2867     If MRtn = 1 Then M_Out(12257) = 0          'DVDチャック開OFF
2868     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2869 '
2870     fErrorProcess(11,270,284,0)
2871     If M_20# = MNext% Then M_20# = MClear%
2872     If M_20# = MAbout% Then GoTo *RecoveryEnd
2873     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2874     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2875 '
2876     *RecoveryChuckOpenEnd
2877 '
2878 '特殊回避　直接、上空退避が出来ない所の対処
2879 '
2880 'PMechaOnRoboSet(Get)～PMechaOnRoboSet(Get)_1のエリアにいるときは、PMechaOnRoboSet_1へ
2881 '・PMechaOnRoboSet
2882 '・PMechaOnRoboSet_1
2883 '・PMechaOnRoboGet
2884 '・PMechaOnRoboGet_1
2885 '上記４点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
2886     PActive = P_Curr                    '現在位置を取得
2887     If (PActive.X >= -150) And (PActive.X <= -60) Then
2888         If (PActive.Y >= 540) And (PActive.Y <= 580) Then
2889             If (PActive.Z >= 290) And (PActive.Z <= 330) Then
2890                 Mvs PMechaOnRoboSet_1
2891                 Dly 1.0
2892             EndIf
2893         EndIf
2894     EndIf
2895 '
2896 'PMechaOnRoboSet(Get)_1～PMechaOnRoboSet(Get)_2のエリアにいるときは、PMechaOnRoboSet_2へ
2897 '・PMechaOnRoboSet_1
2898 '・PMechaOnRoboSet_2
2899 '・PMechaOnRoboGet_1
2900 '・PMechaOnRoboGet_2
2901 '上記４点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
2902 '    PActive = P_Curr                    '現在位置を取得
2903 '    If (PActive.X >= -90) And (PActive.X <= 50) Then
2904 '        If (PActive.Y >= 300) And (PActive.Y <= 570) Then
2905 '            If (PActive.Z >= 290) And (PActive.Z <= 430) Then
2906 '                Mvs PMechaOnRoboSet_2
2907 '                Dly 1.0
2908 '            EndIf
2909 '        EndIf
2910 '    EndIf
2911 '
2912 '上空退避
2913     PActive = P_Curr
2914     Pmove = PActive
2915     Pmove.Z = 500           '上空退避する一律の高さ
2916      If PActive.X < -400 Then
2917         Pmove.Z =290        '金具(F)受け取り位置上に腕を伸ばしているときは500まで上げられない為、例外処置
2918     EndIf
2919     If PActive.X > 400 Then
2920         Pmove.Z =400        'パレット上に腕を伸ばしているときは500まで上げられない為、例外処置
2921     EndIf
2922     If PActive.Z < Pmove.Z Then
2923         Mvs Pmove
2924     EndIf
2925     Dly 1.0
2926 'J1軸以外を退避ポジションへ移動
2927     JActive = J_Curr
2928     Jmove = JTaihi
2929     Jmove.J1 = JActive.J1        'J1軸は現在値を使用し、JTaihiのポーズを取る
2930     Jmove.J6 = JActive.J6        'J6軸は現在値を使用し、JTaihiのポーズを取る
2931     Mov Jmove
2932     Dly 1.0
2933 'J1軸のみを退避ポジションへ移動
2934     Mov JTaihi
2935     Dly 1.0
2936 'イニシャルポジションへ移動
2937     Mov PInitialPosition
2938     Cmp Off
2939     Ovrd 100
2940 ' ねじロボを初期位置に戻すために強制的に自動運転開始         '2022/04/20 ファンクションの外へ移動 渡辺
2941 '    If M_In(11856) = 0 Then                 ' 停止中のみ
2942 '        M_Out(12834) = 1                    ' 自動運転開始ON 12834   M6834
2943 '        MRet = frInCheck(11842, 1, 30000&)  ' 自動運転開始受信待ち    11842   M5842
2944 '        If MRet = 0 Then
2945 '        Else
2946 '            M_Out(12834) = 0    ' 自動運転開始OFF 12834   M6834
2947 '        EndIf
2948 '    EndIf
2949     fErrorProcess(11,253,281,0)
2950 *RecoveryEnd
2951     Exit Function
2952 FEnd
2953 '
2954 '
2955 '■fnAutoScreenComment
2956 ''' <summary>
2957 ''' メイン画面の動作状況表示
2958 ''' コメントD1005の設定
2959 ''' </summary>
2960 '''<param name="McommentD1005%">コメントID</param>
2961 ''' <remarks>
2962 ''' Date   : 2021/07/07 : M.Hayakawa
2963 ''' </remarks>
2964 Function fnAutoScreenComment(ByVal McommentD1005%)
2965     M_Out16(12576) = McommentD1005%
2966 FEnd
2967 '
2968 '■fnRoboPosChk
2969 ''' <summary>
2970 ''' 最後に終了したロボットポジションの確認
2971 ''' </summary>
2972 '''<param name="MINNumber%">入力番号</param>
2973 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2974 '''<param name="MTimeCnt&">タイムアウト時間</param>
2975 ''' PLCに保続した番号を読込み、確認
2976 ''' MRBTOpeGroupNo = 5 が初期位置に設定
2977 '''<returns>整数 0:タイムアウト 1:OK</returns>
2978 ''' <remarks>
2979 ''' Date   : 2021/07/07 : M.Hayakawa
2980 ''' </remarks>
2981 Function M% fnRoboPosChk
2982     fnRoboPosChk = 0
2983     MRet = fnStepRead()
2984     '初期位置でないと判断した場合
2985     'ウィンド画面切換え
2986     If MRBTOpeGroupNo > 5 Then
2987         '下記キー待ちの継続に反応させないため
2988         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2989         Dly 0.2
2990         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2991         Dly 1.5
2992         '
2993         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
2994         '
2995         MLoopFlg% = 1
2996         While MLoopFlg% = 1
2997             '
2998             '
2999             MKeyNumber% = fnKEY_WAIT()
3000             Select MKeyNumber%
3001                 Case Is = MAbout%       '停止
3002                     M_20# = MAbout%
3003                     MLoopFlg% = -1
3004                     Break
3005                 Case Is = MNext%        '次へ
3006                     'MLoopFlg% = -1
3007                     Break
3008                 Case Is = MContinue%    '継続
3009                     M_20# = MContinue%
3010                     MLoopFlg% = -1
3011                     Break
3012                 Default
3013                     Break
3014             End Select
3015         WEnd
3016     EndIf
3017     '
3018     If M_20# = MContinue% Then                              '継続ボタンが押された場合
3019         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
3020         Ovrd 5                                   '低速オーバーライド値設定
3021         Select MRBTOpeGroupNo
3022             Case Is = 5                          '何もしない
3023                 Break
3024             Case Is = 10                         '初期位置へ戻す
3025                 'Mov PTEST001
3026                 Break
3027             Case Is = 15                         '初期位置へ戻す
3028                 'Mov PTEST002
3029                 Dly 0.5
3030                 'Mov PTEST001
3031                 Dly 0.5
3032                 Break
3033             Default
3034                 Break
3035         End Select
3036         '
3037         Ovrd M_NOvrd                            'システムの初期値を設定
3038         M_Out(12364) = 1                        'toPLC_データ保存ON
3039         MRBTOpeGroupNo = 5
3040         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
3041         Dly 1.0
3042         M_Out(12364) = 0                        'toPLC_データ保存OFF
3043         fnRoboPosChk = 1                        '初期位置動作実行
3044         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
3045     EndIf
3046     Exit Function
3047 FEnd
3048 '
3049 '■frInCheck
3050 ''' <summary>
3051 ''' センサーINチェック
3052 ''' </summary>
3053 '''<param name="MINNumber%">入力番号</param>
3054 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
3055 '''<param name="MTimeCnt&">タイムアウト時間</param>
3056 '''<returns>整数 0:タイムアウト 1:OK</returns>
3057 ''' <remarks>
3058 ''' Date   : 2021/07/07 : M.Hayakawa
3059 ''' </remarks>
3060 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
3061     M_Timer(4) = 0
3062     MloopFlg = 0
3063     While MloopFlg = 0
3064         MCrtTime& = M_Timer(4)
3065         If M_In(MINNumber%) = MCMPFLG% Then
3066             MloopFlg = 1
3067             frInCheck = 1
3068         ElseIf MCrtTime& > MTimeCnt& Then
3069             MloopFlg = 1
3070             frInCheck = 0
3071         EndIf
3072     WEnd
3073 FEnd
3074 '-----------------------------------------------
3075 '
3076 'ねじ締め機通信確認
3077 '
3078 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3079 'fScewTcomChk = 0　：正常終了
3080 '          　　 -1 ：異常終了
3081 '-----------------------------------------------
3082 Function M% fScewTcomChk
3083 *ReCheckScewTcomChk
3084     fScewTcomChk = 0
3085     '通信確認送信
3086     M_Out(MOUT_ScwT_ComChk%) = MOn%
3087     '通信確認受信待機
3088 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
3089     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
3090     '通信確認送信終了
3091     M_Out(MOUT_ScwT_ComChk%) = MOff%
3092     If MRtn = 0 Then
3093         fScewTcomChk = -1
3094     EndIf
3095     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
3096  '
3097 FEnd
3098 '
3099 '
3100 '-----------------------------------------------
3101 '
3102 'ねじ締め開始送信
3103 '
3104 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3105 'fScewTStart = 0　：正常終了
3106 '          　　-1 ：異常終了
3107 '-----------------------------------------------
3108 Function M% fScewTStart
3109     fScewTStart = 0
3110     nRet% = 0
3111     'ねじ締め開始待機を受信
3112 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
3113     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
3114     If MRtn = 0 Then nRet% = -1
3115     If MRtn = 0 Then GoTo *ScrewStartERROR      '開始できなかった場合ジャンプ
3116     Dly 0.1
3117     'ねじ締め開始受信を送信
3118     M_Out(MOUT_ScwT_ST%) = MOn%
3119     Dly 0.5
3120     'Wait M_In(MTEST_KEY%) = MOn%
3121     'ねじ締め開始送信終了
3122     M_Out(MOUT_ScwT_ST%) = MOff%
3123     '
3124 *ScrewStartERROR
3125     fScewTStart = nRet%
3126 FEnd
3127 '
3128 '
3129 '
3130 '-----------------------------------------------
3131 '
3132 'ねじ締め完了受信
3133 '
3134 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3135 'fScewTcomChk = 0　：正常終了
3136 '          　 　-1 ：異常終了
3137 '-----------------------------------------------
3138 Function M% fScewTFinish
3139 *ReCheckScewTFinish
3140     fScewTFinish = 0
3141     'ねじ締め完了待機を受信
3142 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3143     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3144     If MRtn = 0 Then
3145         fScewTFinish = -1
3146     EndIf
3147     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3148     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3149     Dly 0.1
3150     'ねじ締め完了受信を送信
3151     M_Out(MOUT_ScwT_FinOK%) = MOn%
3152     Dly 0.5                          'とりあえず保持時間0.5msec
3153     'ねじ締め開始送信終了
3154     M_Out(MOUT_ScwT_FinOK%) = MOff%
3155     'Wait M_In(MTEST_KEY%) = MOn%
3156     '
3157 *ScewTFinish_ErrEnd
3158 FEnd
3159 '
3160 '
3161 '-----------------------------------------------
3162 '
3163 '条件xx停止受信
3164 '
3165 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3166 'fScewTCaseStop = 0　：正常終了
3167 '          　   　-1 ：異常終了
3168 '-----------------------------------------------
3169 Function M% fScewTCaseStop(ByVal MCase%())
3170 *ReCheckScewTCaseStop
3171     fScewTCaseStop = 0
3172     '条件xx停止を受信
3173     Wait M_In(MCase%(1)) = MOn%
3174     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3175     If MRtn = 0 Then
3176         fScewTCaseStop = -1
3177     EndIf
3178     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3179     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3180     Dly 0.1
3181     '条件xx停止受信を送信
3182     M_Out(MCase%(2)) = MOn%
3183     Dly 0.5                          'とりあえず保持時間0.5msec
3184     'ねじ締め開始送信終了
3185     M_Out(MCase%(2)) = MOff%
3186 *ScewTCaseStop_ErrEnd
3187     '
3188 FEnd
3189 ''
3190 '■fScrewTighenRoboCheck
3191 '<summary>
3192 'ねじロボ監視
3193 '</summary>
3194 '<param name = "MStopNum%"> 停止番号</param>
3195 '<returns>整数 0:ねじロボ異常終了 1:OK </returns>
3196 '<make>
3197 '2021/12/2 中村天哉
3198 '</make>
3199 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3200     fnAutoScreenComment(503)    '状態表示[ねじロボ動作終了待ち] 2022/04/26 渡辺
3201     fScrewTighenRoboCheck = 1
3202     MScrewTighenRoboFlg% = 1    'フラグの初期化
3203     MCheck% = 0
3204     While MScrewTighenRoboFlg% = 1
3205         MCheck% = M_In16(11904)
3206         If M_In(MStopNum%) = 1 Then     '停止位置まで来たら
3207             MScrewTighenRoboFlg% = 0    '関数を抜ける
3208             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
3209         EndIf
3210         If MCheck% <> 0 Then
3211             fScrewTighenRoboError(MCheck%)
3212             Select M_20#
3213                 Case MAbout%            '停止が押された場合
3214                     M_Out(12869) = 1 Dly 1.0
3215                     MScrewTighenRoboFlg% = 0
3216                     fScrewTighenRoboCheck = 0   '異常終了
3217                     Break
3218                 Case MNgProcess%        'NGが押された場合
3219                     M_Out(12873) = 1 Dly 1.0
3220                     MScrewTighenRoboFlg% = 0
3221                     fScrewTighenRoboCheck = 0   '異常終了
3222                     Break
3223                 Case MContinue%             'リトライが押された場合
3224                     M_20# = MClear%         'M_20#初期化
3225                     M_Out(12871) = 1 Dly 1.0
3226                     Break
3227                 Case MNext%                 '次へが押された場合
3228                     M_20# = MClear%         'M_20#初期化
3229                     M_Out(12874) = 1 Dly 1.0
3230                     Break
3231             End Select
3232             Dly 0.5
3233         EndIf
3234     WEnd
3235 FEnd
3236 '■fScrewTighenRoboError
3237 '<summary>
3238 'ねじロボエラー処理
3239 '</summary>
3240 '<param name = "ErrorCode%"> エラー番号</param>
3241 '<make>
3242 '2021/12/2 中村天哉
3243 '</make>
3244 Function fScrewTighenRoboError(ErrorCode%)
3245     MCommentD1001 = ErrorCode% + 300
3246     fErrorProcess(11,MCommentD1001,0,0)
3247 FEnd
3248 '
3249 '■fErrorProcess
3250 '<summary>
3251 'エラー処理
3252 '</summary>
3253 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
3254 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
3255 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
3256 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
3257 '<make>
3258 '2021/11/5 中村天哉
3259 '</make>
3260 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3261     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
3262     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
3263     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
3264     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
3265     MKeyNum% = 0
3266 *RETRY_ERR_PROCESS
3267      M_20# = MClear%     '初期化
3268 '        'エラー処理記述
3269         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3270 '        'GOT KEY入力待ち
3271         MKeyNum% = fnKEY_WAIT()
3272 '        '
3273         If MKeyNum% = MAbout% Then   '停止を選択した場合
3274             M_20# = MAbout%            'M_20# プログラム間共通外部変数
3275             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3276             Break
3277          '
3278         ElseIf MKeyNum% = MContinue% Then   '継続を選択した場合
3279             M_20# = MContinue%            'M_20# プログラム間共通外部変数
3280             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3281         '
3282         ElseIf MKeyNum% = MNext% Then   '次へを選択した場合
3283             M_20# = MNext%            'M_20# プログラム間共通外部変数
3284             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3285          '
3286         ElseIf MKeyNum% = MNgProcess% Then   '停止を選択した場合
3287             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
3288             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3289             Break
3290         '
3291         EndIf
3292         '
3293         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3294 FEnd
3295 '
3296 '■fnTorqueCheck
3297 ''' <summary>
3298 ''' トルクチェック動作用のメイン
3299 ''' </summary>
3300 ''' <remarks>
3301 ''' Date   : 2021/12/21 : H.AJI
3302 ''' </remarks>'
3303 Function M% fnTorqueCheck
3304     'トルクチェック中送信  搬送系停止
3305     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
3306     '
3307     fnTorqueCheck = 0
3308     Ovrd 20
3309     Mov PInitialPosition              '初期位置移動
3310     Accel 100 , 20
3311     Mvs PHandChange                   'ハンド交換位置
3312     Accel 100 , 100
3313     Ovrd 100
3314     Ovrd 100
3315     '下記キー待ちの継続に反応させないため
3316     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3317     Dly 0.2
3318     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3319     '
3320     'M6340  トルクチェック受信
3321     'Dly 5.0
3322     M_Out(12340) = 1          'トルクチェック受信 M6340
3323     Dly 1.0
3324     M_Out(12340) = 0
3325     '
3326     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
3327     M_Out(12835) = 1                         'ねじロボトルクチェック画面切替
3328    Wait M_In(11843) = 1                         'ねじロボトルクチェック画面切替
3329     M_Out(12835) = 0                         'ねじロボトルクチェック画面切替完了
3330     '
3331     '
3332     MLoopFlg = 1
3333     While MLoopFlg = 1
3334         '
3335 '        Mov PInitialPosition              '初期位置移動
3336         '
3337         MKeyNumber = fnKEY_WAIT()
3338         Select MKeyNumber
3339             Case Is = 1           '停止
3340                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
3341                 Dly 1.0
3342                 M_Out(12343) = 0
3343                 Ovrd 20
3344                 'Mov PTicketRead_1
3345                 M_Out(12840) = 1          'トルクチェック終了
3346                 Wait M_In(11859) = 1      'ねじロボからの終了
3347                 M_Out(12840) = 0          'トルクチェック終了
3348                 Ovrd 100
3349                 M_20# = 1
3350                 MLoopFlg = -1
3351                 Break
3352             Case Is = 2           '次へ
3353                 Break
3354             Case Is = 3           '継続
3355                 Break
3356             Case Is = 4           'トルクチェック開始
3357                 M_Out(12342) = 1          'トルクチェック開始要求受信 M6342
3358                 Dly 1.0
3359                 M_Out(12342) = 0
3360                 If M_In(11862) = 1 Then             'トルクチェッカー確認
3361                     fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
3362                     MRet = fnScrewMTorque()           'ねじロボ用トルクチェック
3363                 EndIf
3364                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
3365                 'MRet = fnMoveTorquePosi()
3366                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
3367                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3368                 Break
3369             Default
3370                 Break
3371         End Select
3372     WEnd
3373     '
3374     'トルクチェック中停止送信
3375     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
3376     '
3377     'ロボットの位置を元に戻す
3378     Mvs PInitialPosition              'イニシャルポジション
3379     '
3380     '
3381  FEnd
3382  '
3383 '
3384 '
3385 '---------------------------
3386 '
3387 '    メイン画面の表示、非表示設定
3388 '         コメントD1001, D1002, D1003の設定
3389 '           MWindReSet = 0     画面非表示
3390 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
3391 '           MWindErrScr = 10    エラー画面 D1001, D1002
3392 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
3393 '
3394 '---------------------------
3395 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3396     fnMainScreenOpen = 0
3397     '
3398    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
3399         M_Out16(12480) = MCommentD1001            'D1001 コメント
3400     EndIf
3401     '
3402     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
3403         M_Out16(12496) = MCommentD1002            'D1002 コメント
3404     EndIf
3405     '
3406     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
3407         M_Out16(12512) = MCommentD1003            'D1003 コメント
3408     EndIf
3409     '
3410     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
3411     M_Out(12362) = 1                         'ウィンド画面設定  M6362
3412     Dly 0.5
3413     M_Out(12362) = 0                         'ウィンド画面設定
3414 FEnd
3415 '
3416 '■Main
3417 ''' <summary>
3418 ''' トルクチェック実動作
3419 ''' </summary>
3420 ''' <remarks>
3421 ''' Date   : 2021/12/21 : H.AJI
3422 ''' </remarks>'
3423 Function M% fnScrewMTorque
3424     fnScrewMTorque = 0
3425     M_Out(12838) = 1                         'トルクチェック開始1
3426     Wait M_In(11857) = 1                     '受信完了
3427     M_Out(12838) = 0                         'トルクチェック開始1
3428     Dly 2.0
3429 FEnd
3430 '
3431 '
3432 '----------------------------------------------------------------
3433 'fTimeOutJudge
3434 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3435 '引数
3436 'Address% = 監視アドレス番号
3437 'JudgeFlg% = 対象アドレスの正常終了時の値
3438 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3439 '戻り値 = 0 エラー
3440 '         1 正常終了
3441 '         2 リトライ
3442 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3443 '作成日
3444 '2022/9/20 中村
3445 '----------------------------------------------------------------
3446 '
3447 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3448     fTimeOutJudge = 0
3449     MJudge% = 1
3450     MRtn = 0
3451     M_20# = MClear%
3452     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3453 *TimeOutLoop
3454     If MRtn = 1 Then GoTo *TimeOut
3455         fErrorProcess(11,202,203,0)
3456         If M_20# = MNext% Then GoTo *TimeOutLoop
3457         If M_20# = MContinue% Then MJudge% = 2
3458         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3459 *TimeOut
3460     fTimeOutJudge = MJudge%
3461 '
3462 *JUDGE_ERROR_END
3463 Exit Function
3464 FEnd
3465 '
3466 ''■Main
3467 ''' <summary>
3468 ''' 組立動作用のメイン
3469 ''' </summary>
3470 ''' <remarks>
3471 ''' Date   : 2021/07/07 : M.Hayakawa
3472 ''' </remarks>'
3473 Function Main
3474     MopeNo = M_21#         '外部変数にて動作番号代入
3475     '
3476     If M_Svo=0 Then
3477         Servo On
3478     EndIf
3479     Wait M_Svo=1
3480 '組立スタート日付時刻要求パルスON (別スロットの8から要求に変更）
3481 '    M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3482 'パトライト操作
3483     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
3484     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
3485     '
3486     M_20# = 0                                   'KEY入力初期化
3487     'M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化(位置移動1/18中村)
3488     MRet% = 0
3489 '初期位置の確認と移動
3490 '
3491 '復帰動作　実行・未実行判別      2022/04/11 渡辺 作成
3492     PActive = P_Curr                    '現在位置を取得
3493     MRecoveryPass% = 0
3494     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3495         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3496             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3497                 MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
3498             EndIf
3499         EndIf
3500     EndIf
3501     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3502         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3503             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3504                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
3505             EndIf
3506         EndIf
3507     EndIf
3508     If (PActive.X <= PMechaOnJigGet_3.X + 1.0) And (PActive.X >= PMechaOnJigGet_3.X -1.0) Then
3509         If (PActive.Y <= PMechaOnJigGet_3.Y + 1.0) And (PActive.Y >= PMechaOnJigGet_3.Y -1.0) Then
3510             If (PActive.Z <= PMechaOnJigGet_3.Z + 1.0) And (PActive.Z >= PMechaOnJigGet_3.Z -1.0) Then
3511                 MRecoveryPass% = 1       'DVD取り位置上空位置は復帰動作パス
3512             EndIf
3513         EndIf
3514     EndIf
3515     If MRecoveryPass% = 0 Then
3516        fnInitialZoneB()        '復帰動作パスフラグが立っていない時は復帰動作を実行
3517     EndIf
3518 '
3519 ' ねじロボを初期位置に戻すために強制的に自動運転開始        'fnInitialZoneBの外へ移動 2022/04/20 渡辺
3520     If MopeNo <> 2 And M_In(MIN_TorqueCheck%) <> 1 Then       'トルクチェックの時は以下を実行しない 2022/04/21 渡辺
3521         If M_In(11856) = 0 Then                 ' 停止中のみ
3522             fnAutoScreenComment(501)            ' 状態表示[ネジ締め機自動運転開始中] 2022/04/25 渡辺
3523             M_Out(12834) = 1                    ' 自動運転開始ON 12834   M6834
3524             MRet% = frInCheck(11842, 1, 30000&)  ' 自動運転開始受信待ち    11842   M5842
3525             If MRet% = 0 Then
3526             Else
3527                 M_Out(12834) = 0    ' 自動運転開始OFF 12834   M6834
3528             EndIf
3529         EndIf
3530     EndIf
3531 '
3532 '
3533 '    MRet% = fnRoboPosChk()
3534 '    If MRet% = 1 Then                           '初期位置の動作を行った場合
3535 '        fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  'ウィンド画面
3536 '        MKeyNumber% = fnKEY_WAIT()
3537 '        Select MKeyNumber%
3538 '            Case Is = MAbout%       '停止
3539 '                M_20# = MAbout%
3540 '                MLoopFlg% = -1
3541 '                Break
3542 '            Case Is = MNext%        '次へ
3543 '                'MLoopFlg = -1
3544 '                Break
3545 '            Case Is = MContinue%    '継続
3546 '                M_20# = MContinue%
3547 '                MLoopFlg% = -1
3548 '                Break
3549 '            Default
3550 '                Break
3551 '        End Select
3552 '    EndIf
3553     '
3554     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
3555         M_Out(12364) = 1            'toPLC_データ保存ON
3556 'トルクチェック
3557         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3558             MRet% = fnTorqueCheck()
3559             Break
3560         Else
3561 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
3562 '                MRtn = InspInit()               '画像処理初期化処理
3563 '            EndIf
3564             '
3565            M_20# = MClear%                    '初期化
3566 '組立開始
3567             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3568                 'MRet% = fnAssyStart()  '暫定コメントアウト
3569                 fnAssyStart()
3570             Else
3571                 M_20# = MPass%
3572             EndIf
3573             M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化(位置移動1/18中村)
3574 '組立終了日付時刻
3575             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
3576             Wait M_In(11572) = 1            '日付取得完了
3577             Dly 0.1
3578             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
3579 'リフターユニットへのOUT
3580             '  KEY入力が何もない場合 OKと判断
3581             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
3582             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
3583 'OK/NGフラグ出力
3584             If M_20# = MAssyOK% Or M_22# = MIrregular% Then
3585                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
3586             ElseIf M_20# = MPass% Then
3587                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
3588             EndIf
3589 'PIASに組立完了書込み
3590             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3591                 If M_20# = MPass% And M_22# <> MIrregular% Then
3592                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3593                 Else
3594                     'KEY入力がNGの場合
3595                     If M_20# = MNgProcess% Then
3596                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3597                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3598                         MRet% = fnPiasWrite(MNG%)
3599                        nAssyNgQty = nAssyNgQty + 1
3600                     EndIf
3601                     '
3602                     'KEY入力が何もない場合 OKと判断(0からMAssyOK%に変更1/12中村)
3603                     If M_20# = MAssyOK% Then
3604                             '-----------------------
3605                             'D732 -> D2600 コピー要求
3606                             M_Out(12566) = 1
3607 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3608                             M_Out(12566) = 0
3609                             '
3610                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3611                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3612                             '基板番号照合(PPは未使用）
3613 '                            MRet% = fnPCBNumberCheck()
3614                         Else
3615                             MRet% = 1
3616                         EndIf
3617                         '
3618                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3619                             If M_20# <> MAbout% Then
3620                                 '工程履歴OK書き込み
3621                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3622                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3623                                 MRet% = fnPiasWrite(MOK%)
3624                                 nAssyOkQty = 0
3625                                 nAssyOkQty = nAssyOkQty + 1
3626                             Else
3627                                 nAssyOkQty = nAssyOkQty + 1
3628                             EndIf
3629                         EndIf
3630                     EndIf
3631 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3632 '                    MRet% = fnPiasWrite(MOK%)
3633                 EndIf
3634             Else
3635                 nAssyOkQty = nAssyOkQty + 1
3636             EndIf
3637             '
3638             '組立終了日付時刻解除
3639             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3640             '投入数、組立OK数、組立NG数書込み
3641 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3642             '
3643 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3644 '                '画像処理終了処理
3645 '                MRtn = InspQuit()
3646 '            EndIf
3647         EndIf
3648         M_Out(12364) = 0                          'toPLC_データ保存OFF
3649     EndIf
3650 'パトライト操作
3651     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3652     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3653 'GOT表示
3654     fnAutoScreenComment(93)  'AUTO画面 工程完了
3655 FEnd
3656 End
3657 '
3658 '
3659 'おまじないコメント
3660 '絶対削除するな
3661 '
3662 '
3663 '
3664 '
3665 '
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
PTemp=(+160.73,+268.78,+438.32,-105.54,+88.56,-15.42,+0.00,+0.00)(6,0)
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
PActive=(+160.73,+268.78,+438.32,-105.54,+88.56,-15.42)(6,0)
Pmove=(+309.64,-1.37,+500.00,-179.87,+67.95,+179.97)(6,0)
PBracketFCheck=(-21.45,+489.13,+480.00,-180.00,+0.00,+0.00)(7,0)
PBracketFCheck1=(+2.27,+435.02,+540.18,+145.00,-0.01,-90.00)(7,0)
PBracketFCheck2=(+2.27,+535.02,+540.18,+145.00,-0.01,-90.00)(7,0)
PBracketFCheck_2=(-171.46,+546.61,+480.00,-180.00,+0.00,-90.00)(7,0)
PBracketFCheck_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFCheck_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFCheck_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFCheck_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFGet=(-595.13,-91.69,+243.56,-179.81,+0.51,+91.79)(7,1)
PBracketFGet_1=(-595.13,-91.96,+280.00,-179.81,+0.51,+91.79)(7,1)
PBracketFGet_2=(-287.41,+0.11,+490.00,-180.00,+0.00,+0.00)(7,0)
PBracketFGet_3=(-177.20,+193.83,+460.54,-179.99,-0.01,-85.26)(7,0)
PBracketFGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFSet=(-198.12,+546.19,+453.88,-179.44,+0.16,-88.98)(7,0)
PBracketFSet_1=(-198.12,+546.19,+470.00,-179.44,-0.16,-88.98)(7,0)
PBracketFSet_2=(-197.04,+546.94,+540.00,-179.88,-0.07,-88.98)(7,0)
PBracketFSet_3=(-26.65,+244.35,+540.00,-179.99,-0.01,-85.26)(7,0)
PBracketFSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck=(-177.56,+416.30,+479.98,-180.00,+0.00,-89.99)(7,0)
PBracketRCheck1=(+6.08,+420.00,+537.59,+145.00,+0.00,-90.00)(7,0)
PBracketRCheck2=(+6.08,+550.00,+537.59,+145.00,+0.00,-90.00)(7,0)
PBracketRCheck_2=(-177.57,+546.61,+479.97,-180.00,+0.00,-90.00)(7,0)
PBracketRCheck_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRGet=(-538.13,-90.29,+244.19,-179.51,+0.39,+92.03)(7,1)
PBracketRGet_1=(-538.13,-90.29,+290.00,-179.51,+0.39,+92.03)(7,1)
PBracketRGet_2=(-287.41,+0.11,+490.00,-180.00,+0.00,+0.00)(7,0)
PBracketRGet_3=(-177.20,+193.83,+460.54,-179.99,-0.01,-85.26)(7,0)
PBracketRGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRSet=(-155.13,+545.39,+453.83,+179.76,+0.30,-89.39)(7,0)
PBracketRSet_1=(-155.13,+545.39,+460.00,+179.76,+0.30,-89.39)(7,0)
PBracketRSet_2=(-155.13,+545.39,+520.00,+179.76,+0.30,-89.39)(7,0)
PBracketRSet_3=(-26.65,+244.35,+539.98,-179.99,-0.01,-85.26)(7,0)
PBracketRSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PHandChange=(+347.78,-1.40,+382.90,-90.00,+89.23,-89.99)(6,0)
PInitialPosition=(+303.63,-1.43,+467.78,+165.41,+90.00,+165.42)(6,0)
PInitialPosition1=(+302.91,+7.16,+469.96,-180.00,+0.00,+180.00)(7,0)
PMechaOnJigGet1=(+163.54,+364.18,+175.90,-99.82,+88.63,-9.71)(6,0)
PMechaOnJigGet1_1=(+163.54,+363.18,+200.00,-99.82,+88.63,-9.71)(6,0)
PMechaOnJigGet2=(+162.55,+364.22,+175.52,-89.65,+88.72,+0.00)(6,0)
PMechaOnJigGet2_1=(+162.55,+364.22,+200.00,-89.65,+89.72,+0.00)(6,0)
PMechaOnJigGet_2=(+160.73,+362.85,+350.00,-105.22,+88.56,-15.10)(6,0)
PMechaOnJigGet_3=(+160.73,+268.78,+438.32,-105.54,+88.56,-15.42)(6,0)
PMechaOnJigGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnJigGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnJigGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet=(+315.15,+116.04,+239.94,-29.59,+88.55,-30.45)(6,0)
PMechaOnPltSet_1=(+315.15,+116.04,+280.00,-29.59,+88.55,-30.45)(6,0)
PMechaOnPltSet_2=(+315.15,+116.04,+450.00,-29.59,+88.55,-30.45)(6,0)
PMechaOnPltSet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboGet=(-132.57,+557.02,+307.25,-179.78,-0.31,-179.26)(7,0)
PMechaOnRoboGet_1=(-90.06,+557.02,+306.36,-179.78,-0.31,-179.25)(7,0)
PMechaOnRoboGet_2=(-70.00,+316.96,+419.50,+180.00,+0.00,+180.00)(7,0)
PMechaOnRoboGet_3=(+0.00,+350.00,+480.00,-180.00,+0.00,-180.00)(7,0)
PMechaOnRoboGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet=(-131.67,+557.03,+307.29,-179.78,-0.31,-179.26)(7,0)
PMechaOnRoboSet_1=(-90.06,+557.03,+307.29,-179.78,-0.31,-179.26)(7,0)
PMechaOnRoboSet_2=(+40.77,+316.96,+419.50,+180.00,+0.00,+180.00)(7,0)
PMechaOnRoboSet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead=(+600.00,-152.00,+403.00,-180.00,+0.00,+90.00)(7,0)
PTicketRead_1=(+600.00,-152.00,+450.00,-180.00,+0.00,+90.00)(7,0)
PTicketRead_2=(+198.38,+259.83,+450.00,-180.00,+0.00,+128.47)(7,0)
PTicketRead_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
JActive=(-0.28,-16.41,+134.83,-0.95,-6.37,+0.85)
Jmove=(-0.28,-15.80,+124.16,+0.00,+71.59,+0.85)
JTaihi=(+0.00,-15.80,+124.16,+0.00,+71.59,+0.00)
