1 ' ===================================
2 '
3 '  21050001 STEP5 Assy2プログラム
4 '
5 ' 作成者：M.Hayakawa
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
43 Def Inte MOvrdA                     'ネジ締めOvrd 可変用
44 Def Float MSpdA                     'ネジ締めSpd　可変用
45 Def Pos PTemp                       'ネジ締め上空位置計算用
46 '===== <Insight変数設定> =====
47 MIN_IS_Ready%        =   11380      '【入力IO】Insight準備OK
48 MIN_IS_JobLoadOK%    =   11381      '【入力IO】Insightジョブロード正常終了
49 MIN_IS_JobLoadNG%    =   11382      '【入力IO】Insightジョブロード異常終了
50 MIN_IS_InspGSetOK%   =   11383      '【入力IO】Insight検査グループ番号設定正常終了
51 MIN_IS_InspGSetNG%   =   11384      '【入力IO】Insight検査グループ番号設定異常終了
52 MIN_IS_InspOK%       =   11385      '【入力IO】Insight検査OK
53 MIN_IS_InspNG%       =   11386      '【入力IO】Insight検査NG
54 MIN_IS_InspErr%      =   11387      '【入力IO】Insight検査異常終了
55 MIN_IS_InspCapDone%  =   11388      '【入力IO】Insight検査画像取込完了
56 MIN_IS_ErrNum%       =   11408      '【入力IO】Insight処理エラー番号開始アドレス(16bit)
57 'Output Signal
58 MOUT_IS_JobLoadReq%  =   12370      '【出力IO】Insight JOBロード要求
59 MOUT_IS_InspGSetReq% =   12371      '【出力IO】Insight 検査グループ番号設定要求
60 MOUT_IS_Insp%        =   12372      '【出力IO】Insight 検査実行要求
61 MOUT_IS_JobNum%      =   12384      '【出力IO】Insight JOB番号設定開始アドレス(16bit)
62 MOUT_IS_InspGNum%    =   12400      '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
63 MOUT_InspErrNum%     =   12416      '【出力IO】検査実行エラー番号開始アドレス(16bit)
64 MOUT_InspNGStepNum%  =   12432      '【出力IO】検査実行NGStep番号開始アドレス(16bit)
65 '===== <電ドラ変数定義> =====
66 X20_Driver=11248                    '電ドラステイタス1　Driver Status 1
67 X21_Driver=11249 '電ドラステイタス2  Driver Status 2
68 X22_Driver=11250 '電ドラステイタス3  Driver Status 3
69 X23_Driver=11251 '電ドラステイタス4  Driver Status 4
70 X24_Driver=11252 '電ドラエラーメッセージ1 Driver Error E1
71 X25_Driver=11253 '電ドラエラーメッセージ2 Driver Error E2
72 X26_Driver=11254 '電ドラエラーメッセージ3 Driver Error E3
73 X27_Driver=11255 '電ドラエラーメッセージ4 Driver Error E4
74 X28_Driver=11256 '電ドラトータルエラーシグナル Total Error
75 X29_Driver=11257 '電ドラ終了シグナル Comlete signal
76 X2A_Driver=11258 '電ドラエラーメッセージ5 Driver Error E5
77 '11584   'toRBトルクドライバ-COMP_ERR送信
78 Y60_Driver=12240 '電ドラ半時計回り CCW
79 Y61_Driver=12241 '電ドラ時計回り CW
80 Y62_Driver=12242 'バンクセッティング BANK C1
81 Y63_Driver=12243 'バンクセッティング BANK C2
82 Y64_Driver=12244 'バンクセッティング BANK C3
83 Y65_Driver=12245 'プログラムセッティング PRG SET F1
84 Y66_Driver=12246 'プログラムセッティング PRG SET F2
85 Y67_Driver=12247 'プログラムセッティング PRG SET F3
86 X34_ScrewReady1=11259 'ねじっこ1　Read
87 '===== <電ドラ定数> =====
88 Dim PScrewPos(10)       'ネジ締め用Function引数変数
89 Dim PGetScrewPos(10)    'ねじ供給機からねじを得るFunction引数変数
90 Dim PEscapePosi(10)
91 MLoopCnt% = 0'
92 '===== <ロボット定数> =====
93 '===== <ロボット変数定義> =====
94 MRBTOpeGroupNo = 0      'ロボット動作番号初期化
95 MCommentD1001 = 0
96 MCommentD1002 = 0
97 MCommentD1003 = 0
98 MScreenNo = 0
99 '
100 MCommentTSU = 0
101 MCommentTSD = 0
102 'ウィンド画面番号設定
103 MWindReSet = 0
104 MWindInfoScr = 5
105 MWindErrScr = 10
106 MWindErrScr2 = 11
107 MWindErrScr3 = 13
108 MWindErrScr17 = 17
109 MWindErrScr18 = 18
110 MWindCmmnScr = 20
111 MWindJigRelase19049 = 60
112 MWindJigRelase19050 = 61
113 MWindJigRelase19051 = 62
114 '
115 MClear% = 0        'KEY_のクリア
116 MAbout% = 1        'KEY_停止
117 MNext% = 2         'KEY_次のステップへ移行
118 MContinue% = 3     'KEY_継続 再度同じ動作を行う
119 '
120 Def Inte MNgProcess
121 MNgProcess% = 5      'KEY_NG
122 '
123 MAssyOK% = 6       '組立完了
124 MPass% = 7         '工程パス
125 MPiasNG% = 8       'Pias確認時履歴NG
126 '
127 '初期化用KEY番号   '
128 MRobotInit1% = 11  '初期位置用
129 MRobotInit2% = 12  '初期位置用
130 MRobotInit3% = 13  '初期位置用
131 MRobotInit4% = 14  '初期位置用
132 '
133 MIN_INIT1REQUEST% = 11568 'toRBT_ロボット初期位置1要求
134 MIN_INIT2REQUEST% = 11569 'toRBT_ロボット初期位置2要求
135 MIN_INIT3REQUEST% = 11570 'toRBT_ロボット初期位置3要求
136 MIN_INIT4REQUEST% = 11571 'toRBT_ロボット初期位置4要求
137 '
138 MOUT_INIT1RECIVE% = 12560 'toPLC_ロボット初期位置1受信
139 MOUT_INIT2RECIVE% = 12561 'toPLC_ロボット初期位置2受信
140 MOUT_INIT3RECIVE% = 12562 'toPLC_ロボット初期位置3受信
141 MOUT_INIT4RECIVE% = 12563 'toPLC_ロボット初期位置4受信
142 '
143 MOK% = 1               '各判定用
144 MNG% = 0               '各判定用
145 MTIMEOUT% = -1         '各判定用
146 MJudge% = 0            '判定情報格納用
147 '
148 MRECIVETIME& = 0
149 MSETTIMEOUT10& = 10000&                '10秒設定
150 MSETTIMEOUT03& = 3000&                 '3秒設定
151 MSETTIMEOUT01& = 1000&                 '1秒設定
152 MSETTIMEOUT05& = 5000&                 '5秒設定
153 MSETTIMEOUT009& = 900&                 '0.9秒設定
154 MSETTIMEOUT008& = 800&                 '0.8秒設定
155 MSETTIMEOUT007& = 700&                 '0.7秒設定
156 MSETTIMEOUT006& = 600&                 '0.6秒設定
157 MSETTIMEOUT005& = 500&                 '0.5秒設定
158 MSETTIMEOUT004& = 400&                 '0.4秒設定
159 MSETTIMEOUT003& = 300&                 '0.3秒設定
160 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
161 MIN_PIAS_ComOK% = 11552                'PC通信OK
162 MIN_PIAS_ComTimeOut% = 11576           'PC通信確認タイムアウト
163 MIN_PIAS_ComNG% = 11553                'PC通信NG
164 MOUT_PIAS_ComCheck% = 12544            'PC通信確認要求
165 MOUT_PIAS_Missing_Process% = 12546     '工程抜け確認要求
166 MIN_PIAS_ModelTypeNG% = 11554          'モデル仕向NG
167 MIN_PIAS_ProcessHistryNG% = 11555      '前工程履歴NG
168 MIN_PIAS_ProcessHistryOK% = 11556      '前工程履歴OK
169 MIN_PIAS_ProcessHistryErr% = 11557     '工程履歴処理エラー
170 MIN_PIAS_MyProcessComp% = 11573        '自工程履歴あり
171 MIN_PIAS_ProcessHistryTimeOut% = 11578 '工程履歴タイムアウト
172 MOUT_OKNG% = 12549                     'PLC OUT でOK=1, NG=0 出力
173 '
174 MOUT_PiasPCBNumberCheck = 12557        '基板番号照合
175 MIN_PiasPCBNumberOK% = 11566          '基板番号OK
176 MIN_PiasPCBNumberNG% = 11565          '基板番号NG
177 MIN_PiasPCBNumberErr% = 11567         '基板番号処理エラー
178 '
179 MOUT_PiasAssyResultOK% = 12549    '組立OK
180 MOUT_PiasAssyResultNG% = 12550    '組立NG
181 MOUT_PiasAssyResultWr% = 12548    '工程履歴書き込み
182 '
183 MIN_PiasProcessNG% = 11559        '工程履歴処理NG
184 MIN_PiasProcessOtherErr% = 11560  '工程履歴処理エラー(なんかのトラブル)
185 MIN_PiasProcessOK% = 11558        '工程履歴処理OK
186 '
187 MIN_Insight_Use% = 11369               '画像確認ON
188 MIN_TorqueCheck% = 11348               'トルクチェック
189 '
190 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT操作権
191 MOUT_RED_LIGHT% = 12356            'PATLIGHT 赤 点灯
192 MOUT_RED_FLASH% = 12357            'PATLIGHT 赤 点滅
193 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT 黄 点灯
194 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT 黄 点滅
195 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT 青 点灯
196 MOUT_GREEN_FLASH% = 12361          'PATLIGHT 青 点滅
197 '
198 MOUT_ST_DATETIME% = 12551          '組立開始日付時刻
199 MOUT_ED_DATETIME% = 12552          '組立終了日付時刻
200 '
201 MOUT_TORQUE_CHECK% = 12367         'PLCへトルクチェック中を送信
202 '
203 MIN_ASSY_CANCEL% = 11366           '組立を行うかのフラグ
204 '
205 MLoopFlg% = 0                      'KEY入力後のOK or NG内容
206 MopeNo% = 0
207 MOvrdA% = 10
208 MRtn% = 0
209 MRet = 0
210 MRet3% = 0
211 '
212 Def Inte MInputQty          '投入数 演算変数
213 Def Inte MAssyOkQty         '組立ＯＫ数 演算変数
214 Def Inte MAssyNgQty         '組立ＮＧ数 演算変数(未使用)
215 Def Inte MSuctionErrQty     '吸着エラー数 演算変数 2022/04/27 渡辺
216 Def Inte nAssyOkQty         '未使用
217 Def Inte MScrewNo
218 Def Inte MReTry
219 '===== <IO変数定義> =====
220 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
221 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
222 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
223 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
224 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
225 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
226 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
227 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
228 '
229 Def Inte Y68_VV1            ' アーム先端　ネジ吸着バルブ
230 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
231 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
232 '
233 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
234 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
235 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
236 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
237 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
238 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
239 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
240 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
241 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
242 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
243 '
244 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
245 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
246 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
247 '
248 Def Inte MOUT_LED1          ' 画像処理用LED照明
249 '
250 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
251 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
252 '
253 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
254 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
255 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
256 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
257 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
258 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
259 '
260 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
261 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
262 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
263 '
264 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
265 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
266 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
267 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
268 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
269 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
270 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
271 Y68_VV1%    =  12248    ' アーム先端　ネジ吸着バルブ '数値12250から12248へ変更(8/5中村)
272 Y6B_VB1%    =  12250    'アーム先端　吸着破壊バルブ  '数値12251から12250へ変更(8/5中村)
273 MOUT_VB1%   =  12250    ' アーム先端　ネジ吸着破壊バルブ  '数値12251から12250へ変更(8/5中村)
274 '
275 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
276 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
277 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
278 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
279 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
280 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
281 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
282 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
283 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
284 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
285 '
286 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
287 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
288 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
289 '
290 MOUT_LED1%  =  12239    ' 画像処理用LED照明
291 '
292 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
293 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
294 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
295 '
296 '共通
297 Def Inte MTEST_KEY                      'デバックテスト用
298 Def Inte MOn                            '出力=1
299 Def Inte MOff                           '出力=0
300 '
301 'ねじ締め装置_出力アドレス
302 Def Inte MOUT_ScwT_ComChk               '通信確認
303 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
304 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
305 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
306 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
307 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
308 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
309 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
310 'ねじ締め装置_入力アドレス
311 Def Inte MIN_ScwT_comOK                 '通信確認返信
312 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
313 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
314 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
315 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
316 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
317 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
318 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
319 '
320 Def Inte MRetryLimit                    ' リトライ回数
321 Def Inte MRetryCount                    ' リトライカウント
322 '
323 Dim MScwT_Case1%(2)               '条件1停止変数
324 Dim MScwT_Case2%(2)               '条件2停止変数
325 Dim MScwT_Case3%(2)               '条件3停止変数
326 Dim MScwT_Case4%(2)               '条件4停止変数
327 Dim MScwT_Case5%(2)               '条件5停止変数
328 '
329 Def Pos PActive                     '直交座標系 位置変数 現在位置
330 Def Pos Pmove                       '直交座標系 位置変数 移動先
331 Def Inte MRecoveryPass              '復帰動作パスフラグ　1=復帰動作をパス　0=復帰動作を実行'
332 '共通
333 MTEST_KEY% = 11359                       'デバッグ用テストKEY
334 MOn% = 1                                 '出力 = 1
335 MOff% = 0                                '出力 = 0
336 '
337 'ねじ締め機_アドレス設定
338 MOUT_ScwT_ComChk% = 12816               '通信確認送信
339 MOUT_ScwT_ST% = 12849                   'ねじ締め開始を送信
340 MOUT_ScwT_ReSTOK% = 12850               '再開始受信を送信
341 MOUT_ScwT_FinOK% = 12852                'ねじ締め完了受信を送信
342 MOUT_ScwT_Case1OK% = 12858              '条件1停止受信を送信
343 MOUT_ScwT_Case2OK% = 12859              '条件2停止受信を送信
344 MOUT_ScwT_Case3OK% = 12860              '条件3停止受信を送信
345 MOUT_ScwT_Case4OK% = 12861              '条件4停止受信を送信
346 MOUT_ScwT_Case5OK% = 12862              '条件5停止受信を送信
347 '
348 MIN_ScwT_comOK% = 11824                 'ねじ締め装置から返信
349 MIN_ScwT_STRec% = 11857                 'ねじ締め開始を受信
350 MIN_ScwT_ReST% = 11858                  '再開始を受信
351 MIN_ScwT_Fin% = 11860                   'ねじ締め完了を受信
352 MIN_ScwT_Case1% = 11866                 '条件1停止待機を受信
353 MIN_ScwT_Case2% = 11867                 '条件2停止待機を受信
354 MIN_ScwT_Case3% = 11868                 '条件3停止待機を受信
355 MIN_ScwT_Case4% = 11869                 '条件4停止待機を受信
356 MIN_ScwT_Case5% = 11870                 '条件5停止待機を受信
357 '
358 MScwT_Case1%(1) = MIN_ScwT_Case1%
359 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
360 MScwT_Case2%(1) = MIN_ScwT_Case2%
361 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
362 MScwT_Case3%(1) = MIN_ScwT_Case3%
363 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
364 MScwT_Case4%(1) = MIN_ScwT_Case4%
365 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
366 MScwT_Case5%(1) = MIN_ScwT_Case5%
367 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
368 '
369 MRetryLimit% = 2
370 '
371 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
372 Function M% fnAssyStart
373     M_20# = MClear%                       '初期化
374 '
375 '組み立て開始
376     Ovrd 100
377 '    Mov PInitialPosition   '暫定コメントアウト
378     'チケットIDを読む
379 '    Mvs PTicketRead             'ID読み位置
380     '初期位置を設定
381     PTemp = P_Curr
382     MRtn = 0
383     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
384         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
385             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
386                 MRtn = 1
387                 Break
388             EndIf
389             Break
390         EndIf
391         Break
392     EndIf
393     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
394     If MRtn = 1 Then
395         M_Out(12262) = 1            '位置決め出ON
396         Mov PTicketRead
397         Break
398     Else
399         Mov PInitialPosition
400         M_Out(12262) = 1            '位置決め出ON
401         Mov PTicketRead_1           'チケットID読み取り回避点
402         Mvs PTicketRead             'ID読み位置
403         Break
404     EndIf
405 '
406     MRtn = 1                        'MRtn初期化
407     *RE_TICKET_READ
408     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
409         MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
410         '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
411         '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
412     EndIf
413     If MRtn = 1 Then GoTo *CompRead
414     Mvs PTicketRead_1                       ' 一旦上空に待避 追加 22/07/16 M,H
415 '    fErrorProcess(11,111,254,0)
416     If M_20# = MPass% Then GoTo *AssyEnd    ' 次へを押された時のコメント解除＋ジャンプ先変更 2022/07/20 M.H
417     If M_20# = MNext% Then M_20# = MClear%
418     If M_20# = MAbout% Then GoTo *AssyEnd       ' コメント解除＋ジャンプ先変更 22/07/16 M,H
419     If M_20# = MNgProcess% Then GoTo *AssyEnd   ' コメント解除＋ジャンプ先変更 22/07/16 M,H
420     If M_20# = MContinue% Then GoTo *RE_TICKET_READ
421     GoTo *ASSY_ERROR_END
422     *CompRead
423     '
424     *INITIAL_CHECK
425     'ハンドの状態をイニシャルに戻す
426     MRtn =frInCheck(11264,0,MSETTIMEOUT05&) 'PCB検出(あるとエラー)
427     If MRtn = 1 Then GoTo *CompCheck_1
428     fErrorProcess(11,0,281,0)
429     If M_20# = MNext% Then M_20# = MClear%
430     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
431     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
432     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
433     *CompCheck_1
434     '
435     If M_In(11266) = 1 Then
436         M_Out(12256) = 0        'PCBチャック開OFF
437         M_Out(12257) = 1        'PCBチャック閉ON
438         Break
439     EndIf
440     If M_In(11268) = 1 Then
441         M_Out(12258) = 0        'PCBシリンダー出OFF
442         M_Out(12259) = 1        'PCBシリンダー戻ON
443         Break
444     EndIf
445     If M_In(11270) = 1 Then
446         M_Out(12260) = 0        'BtoBシリンダー出OFF
447         M_Out(12261) = 1        'BtoBシリンダー戻ON
448         Break
449     EndIf
450     '
451     MRtn =frInCheck(11265,1,MSETTIMEOUT05&) 'PCBチャック閉検出
452     If MRtn = 1 Then GoTo *CompCheck_2
453     fErrorProcess(11,240,281,0)
454     If M_20# = MNext% Then M_20# = MClear%
455     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
456     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
457     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
458     *CompCheck_2
459     '
460     MRtn =frInCheck(11267,1,MSETTIMEOUT05&) 'PCBシリンダー戻検出
461      If MRtn = 1 Then GoTo *CompCheck_3
462     fErrorProcess(11,239,281,0)
463     If M_20# = MNext% Then M_20# = MClear%
464     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
465     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
466     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
467     *CompCheck_3
468     '
469     MRtn =frInCheck(11269,1,MSETTIMEOUT05&) 'BtoBシリンダー戻検出
470     If MRtn = 1 Then GoTo *CompCheck_4
471     fErrorProcess(11,243,281,0)
472     If M_20# = MNext% Then M_20# = MClear%
473     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
474     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
475     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
476     *CompCheck_4
477     '
478 '
479 '---------------------------------------------------------------
480     '圧力設定(低圧)07/30中村
481     M_Out(12266) = 1
482     M_Out(12267) = 0
483 '---------------------------------------------------------------
484 '
485 '
486     '製品位置決め
487     *RE_POSITIONING        '位置決めリトライ用
488     M_Out(12262)=1 Dly 0.3      '位置決めパルス信号
489     'Wait M_In(11273)=1          '位置決め出端検出(修正につきコメントアウト(8/26中村))
490     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)    '位置決め出端検出(8/26中村)
491     If MRtn = 1 Then GoTo *CompPosition_1
492     fErrorProcess(11,231,282,0)
493     If M_20# = MNext% Then M_20# = MClear%
494     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
495     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
496     If M_20# = MContinue% Then GoTo *RE_POSITIONING
497     *CompPosition_1
498     '
499     Dly 0.5
500     M_Out(12264)=1 Dly 0.3      'プッシュパルス信号
501     'Wait M_In(11275)=1          'プッシュ出端検出(修正につきコメントアウト(8/26中村))
502     MRtn = frInCheck(11275,1,MSETTIMEOUT05&)    'プッシュ位置出端検出(8/26中村)
503     If MRtn = 1 Then GoTo *CompPosition_2
504     fErrorProcess(11,232,282,0)
505     If M_20# = MNext% Then M_20# = MClear%
506     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
507     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
508     If M_20# = MContinue% Then GoTo *RE_POSITIONING
509     *CompPosition_2
510     '
511 '    '【SOC基板ID読み込み】
512 '    *RE_SOC_CHECK1
513 '    PInspPosition(1) = PSocPcbRead  'SOC基板ID読取位置
514 '    MInspGroup%(1) = 2              '検査G番号
515 '    MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
516 '    If MRtn = 1 Then
517 '        M_Out(12571) = 1        '基板番号コピー1ON
518 '        Dly 0.3
519 '        M_Out(12566) = 1        '基板番号コピー要求ON
520 '        Wait M_In(11581) = 1    '基板番号コピー完了
521 '        M_Out(12571) = 0        '基板番号コピー1OFF
522 '        M_Out(12566) = 0        '基板番号コピー要求OFF
523 '        Dly 0.3
524 '        M_Out(12557)= 1 Dly 0.3         ' 基板番号照合ビットON
525 '    EndIf
526 '    If MRtn = 1 Then GoTo *CompSocCheck1
527 '    fErrorProcess(11,97,25,0)
528 '    If M_20# = MNext% Then M_20# = MClear%
529 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
530 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
531 '    If M_20# = MContinue% Then GoTo *RE_SOC_CHECK1
532 '    *CompSocCheck1
533 '    'SOC基板IDを読む
534 ''    Mov PSocPcbRead             'ID読み位置
535 '    '
536     'SOC基板を取る
537     *RE_GET_SOC
538     '
539     Mov PSocGet_2               '基板ピックアップ回避点  Y:変更 107.160→106.160
540     M_Out(12256)=0              '基板チャック開OFF
541     M_Out(12257)=1              '基板チャック閉ON
542     '
543     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    'チャック閉センサーON
544     If MRtn = 1 Then GoTo *CompGetSOC_1
545     fErrorProcess(11,240,284,0)
546     If M_20# = MNext% Then M_20# = MClear%
547     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
548     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
549     If M_20# = MContinue% Then GoTo *RE_GET_SOC
550     *CompGetSOC_1
551     '
552     M_Out(12259)=0              'PCBシリンダー戻OFF
553     M_Out(12258)=1              'PCBシリンダー出ON
554     Dly 0.2
555     '
556 '    Wait M_In(11268)=1          'PCBシリンダー出端センサーON
557     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)
558     If MRtn = 1 Then GoTo *CompGetSOC_2
559     fErrorProcess(11,238,284,0)
560     If M_20# = MNext% Then M_20# = MClear%
561     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
562     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
563     If M_20# = MContinue% Then GoTo *RE_GET_SOC
564     *CompGetSOC_2
565     '
566     Mov PSocGet_1               '基板上空 Y:変更 107.160→106.160
567     Ovrd 40
568     Mvs PSocGet                 '基板ピックアップ位置  Y:変更 107.170→106.170
569     Dly 0.3
570     Ovrd 5                      '2021-12-19追加 AJ
571     '
572     '
573 '    Wait M_In(11264)=1          'PCB検出センサーON
574     MRtn = frInCheck(11264 , 1 , MSETTIMEOUT05&)
575     If MRtn = 1 Then GoTo *CompGetSOC_3
576     fErrorProcess(11,0,284,0)
577     If M_20# = MNext% Then M_20# = MClear%
578     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
579     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
580     If M_20# = MContinue% Then GoTo *RE_GET_SOC
581     *CompGetSOC_3
582     '
583     M_Out(12257)=0              '基板チャック閉OFF
584     M_Out(12256)=1              '基板チャック開ON
585     Dly 0.2                     '把持力確保用ディレイ(22/09/30中村)
586     '
587 '    Wait M_In(11266)=1          'チャック開センサーON
588     MRtn = frInCheck(11266 , 1 , MSETTIMEOUT05&)
589     Mvs PSocGet_1               '基板上空  Y:変更 107.160→106.160
590     If MRtn = 1 Then GoTo *CompGetSOC_4
591     Mov PSocGet_2               ' Y:変更 107.160→106.160
592     fErrorProcess(11,241,284,0)
593     If M_20# = MNext% Then M_20# = MClear%
594     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
595     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
596     If M_20# = MContinue% Then GoTo *RE_GET_SOC
597     *CompGetSOC_4
598     '
599     'Wait M_In(11264)=1          'PCB検出センサーON
600     '
601     '下記、PCB判定とエラー処理追加 2021-12-19 AJ
602     MRtn = frInCheck(11264 , 1 , MSETTIMEOUT05&)
603     If MRtn = 1 Then GoTo *CompGetSOC_41
604     fErrorProcess(11,0,284,0)
605     If M_20# = MNext% Then M_20# = MClear%
606     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
607     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
608     If M_20# = MContinue% Then GoTo *RE_GET_SOC
609     *CompGetSOC_41
610     '
611     '
612     'Ovrd 100                   '位置変更2021-12-19追加AJ
613     Ovrd 15                     '追加1/17中村
614     Mov PSocGet_2               '基板ピックアップ回避点
615     Ovrd 100                    '2021-12-19追加AJ
616     '
617     'SOC基板を製品上に置く
618     Mov PSocSet_2               '基板置き回避点
619     Mov PSocSet_1               '製品上空
620     Dly 0.1
621     Ovrd 40
622     Mvs PSocSet                 '基板置き位置（空中で離す）
623     M_Out(12256)=0              '基板チャック開OFF
624     M_Out(12257)=1              '基板チャック閉ON
625 '
626     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
627     Mvs PSocSet_1               '製品上空
628     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
629 '
630     'Wait M_In(11265)=1          'チャック閉センサーON
631     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)
632     If MRtn = 1 Then GoTo *CompGetSOC_5
633     fErrorProcess(11,240,284,0)
634     If M_20# = MNext% Then M_20# = MClear%
635     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
636     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
637     If M_20# = MContinue% Then GoTo *RE_GET_SOC
638     *CompGetSOC_5
639     '
640 '    Wait M_In(11268)=1          'PCBシリンダー出端センサーON・・・もしOFFだったら基板挿入を失敗している
641     MRtn = frInCheck(11268 , 1 , MSETTIMEOUT05&)
642     If MRtn = 1 Then GoTo *CompGetSOC_6
643     fErrorProcess(11,0,284,0)
644     If M_20# = MNext% Then M_20# = MClear%
645     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
646     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
647     If M_20# = MContinue% Then GoTo *RE_GET_SOC
648     *CompGetSOC_6
649     '
650     'Wait M_In(11264)=0          'PCB検出センサーOFF
651     M_Out(12258)=0              'PCBシリンダー出OFF
652     M_Out(12259)=1              'PCBシリンダー戻ON
653     '
654 '    Wait M_In(11267)=1          'PCBシリンダー戻端センサーON
655     MRtn = frInCheck(11267 , 1 , MSETTIMEOUT05&)
656     If MRtn = 1 Then GoTo *CompGetSOC_7
657     fErrorProcess(11,239,284,0)
658     If M_20# = MNext% Then M_20# = MClear%
659     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
660     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
661     If M_20# = MContinue% Then GoTo *RE_GET_SOC
662     *CompGetSOC_7
663     Ovrd 100
664     Mov PSocSet_2               '基板置き回避点
665 '【SOC基板ID読み込み】
666     *RE_SOC_CHECK1
667     PInspPosition(1) = PSocPcbRead  'SOC基板ID読取位置
668     MInspGroup%(1) = 2              '検査G番号
669     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
670 '
671     If MRtn = 1 Then GoTo *CompSocCheck1
672     fErrorProcess(11,97,25,0)
673     If M_20# = MNext% Then M_20# = MClear%
674     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
675     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
676     If M_20# = MContinue% Then GoTo *RE_SOC_CHECK1
677     *CompSocCheck1
678 '【基板IDコピー】
679     *RE_PCB_RECORD
680     M_Out(12571) = 1    ' 領域1 基板番号コピー (D2600-) On
681     Dly 0.1
682     M_Out(12566) = 1    ' toPLC_基板番号コピー要求 On
683 '
684     MRtn = frInCheck(11581,1,MSETTIMEOUT05&)    ' toRBT_基板番号コピー完了 On
685     If MRtn = 1 Then
686         M_Out(12571) = 0  ' 領域1 基板番号コピー (D2600-) Off
687         Dly 0.1
688         M_Out(12566) = 0  ' toPLC_基板番号コピー要求 Off
689 '        GoTo *RE_PCB_COMPAIRE   ' 基板番号照合にスキップ
690     Else
691         fErrorProcess(11,39,25,0)
692         If M_20# = MNext% Then M_20# = MClear%
693         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
694         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
695         If M_20# = MContinue% Then GoTo *RE_PCB_RECORD
696     EndIf
697 '【基板ID照合（紐付け）】
698     MRetryCount% = 0
699     While (MRetryCount% <= MRetryLimit%)
700         *RE_PCB_COMPAIRE
701         M_Out(12557)= 1 ' 基板番号照合ビットON
702         MRtn = frInCheck(11566,1,MSETTIMEOUT05&)    ' toRBT_基板番号照合OK(M420) On
703         If MRtn = 1 Then
704             M_Out(12557)= 0     ' 基板番号照合ビットOff
705             ' リトライ回数設定でループを抜ける
706             MRetryCount% = 99
707         Else
708             If MRetryCount% = MRetryLimit% Then
709                 If M_In(11565) = 1 Then
710                     fErrorProcess(11,37,25,0)
711                 Else
712                     fErrorProcess(11,38,25,0)
713                 EndIf
714                 If M_20# = MNext% Then
715                     M_20# = MClear%
716                     ' リトライ回数設定でループを抜ける
717                     MRetryCount% = 99
718                 EndIf
719                 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
720                 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
721                 If M_20# = MContinue% Then
722                     MRetryCount% = 0
723                 EndIf
724             Else
725                 ' リトライ回数インクリメント
726                 MRetryCount% = MRetryCount% + 1
727                 Dly 0.5 ' 他の工程とタイミングをずらす為のディレイ
728             EndIf
729         EndIf
730     WEnd
731 '
732 '【Soc基板画像チェック】
733 '    *RE_SOC_CHECK2
734 '    PInspPosition(1) = PSocCheck    'Soc基板画像チェック位置
735 '    MInspGroup%(1) = 3              '検査G番号
736 '    MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
737 '    If MRtn = 1 Then GoTo *CompSocCheck2
738 '    fErrorProcess(11,43,46,0)
739 '    If M_20# = MNext% Then M_20# = MClear%
740 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
741 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
742 '    If M_20# = MContinue% Then GoTo *RE_SOC_CHECK2
743 '    *CompSocCheck2
744     '基板置き位置画像検査（不要？）
745     '
746     'SOC基板BtoBプレス
747 '
748 '
749 '
750 '
751     'Mov PSocPress_2             'BtoBプレス回避点
752     *RE_BtoBPRESS   'BtoBシリンダーリトライ
753     Mov PSocPress_1             'プレス上空
754     M_Out(12261)=0              'プレスシリンダー戻OFF
755     M_Out(12260)=1              'プレスシリンダー出ON
756     Dly 0.2
757     '
758     'Wait M_In(11270)=1          'プレスシリンダー出端センサーON(修正につきコメントアウト(8/27中村))
759     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)    'プレスシリンダー出端センサーON(8/27中村)
760     If MRtn = 1 Then GoTo *CompPress_1
761     fErrorProcess(11,242,284,0)
762     If M_20# = MNext% Then M_20# = MClear%
763     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
764     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
765     If M_20# = MContinue% Then GoTo *RE_BtoBPRESS
766     *CompPress_1
767 '--------------------------------------------
768     '圧力検出(低圧力)
769     '22/07/29追加 中村
770 '--------------------------------------------
771 *RE_Pa_OUT
772     If M_20# = MContinue% Then
773     M_Out(12266) = 1
774     M_Out(12267) = 0
775     Dly 0.5
776     M_20# = MClear%
777     EndIf
778     MRtn = frInCheck(11277,1,MSETTIMEOUT05&)     'MDV用圧力検出(22/07/29中村)
779     MRtn2 = frInCheck(11278,0,MSETTIMEOUT05&)    'KA用圧力検出(ONで上がりすぎ)(22/07/29中村)
780     If MRtn = 1 And MRtn2 = 1 Then GoTo *CompPaOut
781     If MRtn = 0 Then
782         fErrorProcess(11,200,201,0)
783     ElseIf MRtn2 = 0 Then
784         fErrorProcess(11,200,201,0)
785     EndIf
786     If M_20# = MNext% Then M_20# = MClear%
787     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
788     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
789     If M_20# = MContinue% Then GoTo *RE_Pa_OUT
790 *CompPaOut
791     '
792     Ovrd 40
793     Mvs PSocPress               'プレスエンド端まで移動
794     Dly 0.5
795     'Wait M_In(11270)=1          'プレスシリンダー出端センサーON…もしOFFだったらコネクタカバー有
796     MRtn = frInCheck(11270,0,MSETTIMEOUT05&)    'プレスシリンダー出端センサーON(8/27中村)
797     If MRtn = 1 Then GoTo *CompPress_2
798     Mvs PSocPress_1              'プレス上空
799     fErrorProcess(11,70,71,0)
800     If M_20# = MNext% Then M_20# = MClear%
801     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
802     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
803     If M_20# = MContinue% Then GoTo *RE_BtoBPRESS
804     *CompPress_2
805     '
806     'Dly 0.2
807     '
808     *RE_BtoB_REST
809     M_Out(12260)=0              'プレスシリンダー出OFF
810     M_Out(12261)=1              'プレスシリンダー戻ON
811 '    Wait M_In(11269)=1          'プレスシリンダー戻端センサーON
812     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)    'プレスシリンダー戻端センサーON(8/27中村)
813     If MRtn = 1 Then GoTo *CompBtoBRest
814     fErrorProcess(11,243,284,0)
815     If M_20# = MNext% Then M_20# = MClear%
816     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
817     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
818     If M_20# = MContinue% Then GoTo *RE_BtoB_REST
819     *CompBtoBRest
820     '
821     Ovrd 100
822     Mov PSocPress_1             'プレス上空
823     M_Out(12266) = 0
824     M_Out(12267) = 0
825     'Mov PSocPress_2            'BtoBプレス回避点
826     '
827 'Soc基板ネジ締め
828     PGetScrewPos(1) = PScrewSupply_1        ' ねじピックアップ上空を代入
829     PGetScrewPos(2) = PScrewSupply_2        ' ねじ供給機回避点を代入
830     PGetScrewPos(9) = PScrewSupply_9        ' ネジ供給機上空ネジ捨て位置(10/6 M.H追加)
831     PGetScrewPos(10) = PScrewSupply         ' ねじピックアップ上空を代入
832     '
833     'Soc基板用ネジ供給機へネジを取りに行く
834     *RE_SCREW_GET_1
835     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
836     If MRtn = 1 Then GoTo *CompScrewGet_1
837     If M_20# = MNext% Then M_20# = MClear%
838     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
839     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
840     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
841     *CompScrewGet_1
842     '
843     PScrewPos(1) = PScrewSoc1_1             ' ねじピックアップ上空を代入
844     PScrewPos(2) = PScrewSoc1_0             ' ネジ1締め開始位置を代入(10/8 M.H)
845     PScrewPos(10) = PScrewSoc1              ' ねじ供給機回避点を代入
846     '①番ネジ締め
847     M_Out16(12672) = 1              'ネジ締め位置番号送信
848     MRtn = ScrewTight(PScrewPos,1,10.0)
849     M_Out16(12672) = 0              'ネジ締め位置番号クリア
850     If MRtn = 1 Then GoTo *CompScrew1
851     Mov PInitialPosition
852     fErrorProcess(11,53,52,0)
853     If M_20# = MNext% Then M_20# = MClear%
854     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
855     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
856     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
857     *CompScrew1
858     '
859     'Soc基板用ネジ供給機へネジを取りに行く
860     *RE_SCREW_GET_2
861     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
862     If MRtn = 1 Then GoTo *CompScrewGet_2
863     If M_20# = MNext% Then M_20# = MClear%
864     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
865     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
866     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
867     *CompScrewGet_2
868     '
869     PScrewPos(1) = PScrewSoc2_1             ' ねじピックアップ上空を代入
870     PScrewPos(2) = PScrewSoc2_0             ' ネジ2締め開始位置を代入(10/8 M.H)
871     PScrewPos(10) = PScrewSoc2              ' ねじ供給機回避点を代入
872     '②番ネジ締め
873     M_Out16(12672) = 2              'ネジ締め位置番号送信
874     MRtn = ScrewTight(PScrewPos,1,10.0)
875     M_Out16(12672) = 0              'ネジ締め位置番号クリア
876     If MRtn = 1 Then GoTo *CompScrew2
877     Mov PInitialPosition
878     fErrorProcess(11,54,52,0)
879     If M_20# = MNext% Then M_20# = MClear%
880     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
881     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
882     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
883     *CompScrew2
884     '
885     'Soc基板用ネジ供給機へネジを取りに行く
886     *RE_SCREW_GET_3
887     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
888     If MRtn = 1 Then GoTo *CompScrewGet_3
889     If M_20# = MNext% Then M_20# = MClear%
890     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
891     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
892     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
893     *CompScrewGet_3
894     '
895     PScrewPos(1) = PScrewSoc3_1             ' ねじピックアップ上空を代入
896     PScrewPos(2) = PScrewSoc3_0             ' ネジ3締め開始位置を代入(10/8 M.H)
897     PScrewPos(10) = PScrewSoc3              ' ねじ供給機回避点を代入
898     '③番ネジ締め
899     M_Out16(12672) = 3              'ネジ締め位置番号送信
900     MRtn = ScrewTight(PScrewPos,1,10.0)
901     M_Out16(12672) = 0              'ネジ締め位置番号クリア
902     If MRtn = 1 Then GoTo *CompScrew3
903     Mov PInitialPosition
904     fErrorProcess(11,55,52,0)
905     If M_20# = MNext% Then M_20# = MClear%
906     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
907     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
908     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
909     *CompScrew3
910     '
911     'Soc基板用ネジ供給機へネジを取りに行く
912     *RE_SCREW_GET_4
913     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
914     If MRtn = 1 Then GoTo *CompScrewGet_4
915     If M_20# = MNext% Then M_20# = MClear%
916     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
917     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
918     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
919     *CompScrewGet_4
920     '
921     PScrewPos(1) = PScrewSoc4_1             ' ねじピックアップ上空を代入
922     PScrewPos(2) = PScrewSoc4_0             ' ネジ4締め開始位置を代入(10/8 M.H)
923     PScrewPos(10) = PScrewSoc4              ' ねじ供給機回避点を代入
924     '④番ネジ締め
925     M_Out16(12672) = 4              'ネジ締め位置番号送信
926     MRtn = ScrewTight(PScrewPos,1,10.0)
927     M_Out16(12672) = 0              'ネジ締め位置番号クリア
928     If MRtn = 1 Then GoTo *CompScrew4
929     Mov PInitialPosition
930     fErrorProcess(11,56,52,0)
931     If M_20# = MNext% Then M_20# = MClear%
932     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
933     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
934     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
935     *CompScrew4
936     '
937     'Soc基板用ネジ供給機へネジを取りに行く
938     *RE_SCREW_GET_5
939     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
940     If MRtn = 1 Then GoTo *CompScrewGet_5
941     If M_20# = MNext% Then M_20# = MClear%
942     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
943     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
944     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
945     *CompScrewGet_5
946     '
947     PScrewPos(1) = PScrewSoc5_1             ' ねじピックアップ上空を代入
948     PScrewPos(2) = PScrewSoc5_0             ' ネジ5締め開始位置を代入(10/8 M.H)
949     PScrewPos(10) = PScrewSoc5              ' ねじ供給機回避点を代入
950     '⑤番ネジ締め
951     M_Out16(12672) = 5              'ネジ締め位置番号送信
952     MRtn = ScrewTight(PScrewPos,1,10.0)
953     M_Out16(12672) = 0              'ネジ締め位置番号クリア
954     If MRtn = 1 Then GoTo *CompScrew5
955     Mov PInitialPosition
956     fErrorProcess(11,57,52,0)
957     If M_20# = MNext% Then M_20# = MClear%
958     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
959     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
960     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
961     *CompScrew5
962     '
963     'Soc基板用ネジ供給機へネジを取りに行く
964     *RE_SCREW_GET_6
965     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
966     If MRtn = 1 Then GoTo *CompScrewGet_6
967     If M_20# = MNext% Then M_20# = MClear%
968     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
969     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
970     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
971     *CompScrewGet_6
972     '
973     PScrewPos(1) = PScrewSoc6_1             ' ねじピックアップ上空を代入
974     PScrewPos(2) = PScrewSoc6_0             ' ネジ6締め開始位置を代入(10/8 M.H)
975     PScrewPos(10) = PScrewSoc6              ' ねじ供給機回避点を代入
976     '⑥番ネジ締め
977     M_Out16(12672) = 6              'ネジ締め位置番号送信
978     MRtn = ScrewTight(PScrewPos,1,10.0)
979     M_Out16(12672) = 0              'ネジ締め位置番号クリア
980     If MRtn = 1 Then GoTo *CompScrew6
981     Mov PInitialPosition
982     fErrorProcess(11,58,52,0)
983     If M_20# = MNext% Then M_20# = MClear%
984     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
985     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
986     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
987     *CompScrew6
988 '
989     MRtn = FnCtlValue2(2)       '組立ＯＫ＋１  2022/04/28 渡辺
990     Mov PTicketRead_1   'チケット読み取り位置上空
991     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
992     InitialState()  ' 初期状態にする*AssyEnd
993     M_20# = MAssyOK%              ' 正常終了処理
994     GoTo *fnAssyStart_FEndPosi
995 '
996 *ASSY_ERROR_END
997     fnInitialZone()   ' 初期位置に移動
998 *AssyEnd
999     InitialState()  ' 初期状態にする
1000 *fnAssyStart_FEndPosi
1001     Exit Function
1002 FEnd
1003 '
1004 '■fnPiasCheck
1005 ''' <summary>
1006 ''' PIASチケット読込み
1007 ''' </summary>
1008 ''' <returns>   0 : NG
1009 '''             1 : OK(読込み完了)
1010 ''' </returns>
1011 ''' <remarks>
1012 ''' Date   : 2021/07/07 : M.Hayakawa
1013 ''' </remarks>'
1014 Function M% fnPiasCheck
1015     fnPiasCheck = 0
1016     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1017     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1018 '
1019 *RETRY_PIAS
1020     M_20# = MClear%
1021     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1022     '
1023     '【IDチケット読み込み】
1024     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1025     MInspGroup%(1) = 1              '検査G番号
1026     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1027 '
1028     'エラーの場合
1029     If MRtn <> 1 Then
1030         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1031         If MRtn <> 1 Then
1032             'D720 -> D1300 コピー要求
1033             M_Out(12565) = 1
1034             Dly 0.5
1035             M_Out(12565) = 0
1036             'エラー処理記述
1037             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1038             'GOT KEY入力待ち
1039             MKeyNumber = fnKEY_WAIT()
1040             '
1041             Select MKeyNumber
1042                 Case MNext%         '次へを選択した場合
1043                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1044                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1045                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1046                     Break
1047                 Case MAbout%        '停止を選択した場合
1048                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1049                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1050                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1051                     Break
1052                 Case MNgProcess%    'NGを選択した場合
1053                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1054                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1055                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1056                     Break
1057                 Case MContinue%     '継続を選択した場合
1058                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1059                     M_20# = MContinue%
1060                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1061                     Break
1062             End Select
1063         EndIf
1064     EndIf
1065 '----------D720 -> D1300 コピー要求----------
1066     M_Out(12565) = 1
1067     Dly 0.5
1068     M_Out(12565) = 0
1069 '----------通信確認をする----------
1070     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1071     MRtn = 0                ' 初期化
1072     M_20# = MClear%         ' 初期化
1073     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1074     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1075     If MRtn <> 1 Then
1076         If M_20# = MContinue% Then
1077             GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1078         Else
1079             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1080         EndIf
1081     EndIf
1082 '----------工程抜け確認----------
1083     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1084     MRtn = 0                ' 初期化
1085     M_20# = MClear%         ' 初期化
1086     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1087     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1088     If MRtn <> 1 Then
1089         If M_20# = MContinue% Then
1090             GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1091         Else
1092             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1093         EndIf
1094     EndIf
1095     '
1096     fnPiasCheck = 1
1097     *fnPiasCheck_End
1098     Exit Function
1099 FEnd
1100 '
1101 '■fnPCComuCheck
1102 ''' <summary>
1103 ''' PC-PLC通信チェック
1104 ''' </summary>
1105 ''' <returns>   0 : NG
1106 '''             1 : OK(読込み完了)
1107 ''' </returns>
1108 ''' <remarks>
1109 ''' Date   : 2021/07/07 : M.Hayakawa
1110 ''' </remarks>'
1111 Function M% fnPCComuCheck
1112     fnPCComuCheck = 0
1113     MJudge% = 0                                  '初期化
1114     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1115     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1116     '
1117     For MStaNo = 0 To 5
1118         '
1119         If M_In(MIN_PIAS_ComOK%) = 1 Then
1120             'PC通信OK(M400)
1121             MJudge% = MOK%
1122             MStaNo = 5
1123             Break
1124         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1125             'toRBT_通信確認time out
1126             MJudge% = MNG%
1127             MCommentD1001 = 15
1128             MCommentD1002 = 21
1129             MStaNo = 5
1130             Break
1131         Else
1132             'toRBT_通信確認time out
1133             MJudge% = MNG%
1134             MCommentD1001 = 14
1135             MCommentD1002 = 21
1136             Break
1137         EndIf
1138     Next MStaNo
1139     '
1140     '上記で返信フラグを受信してからPC通信確認OFF
1141     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1142     '
1143     'エラー画面
1144     If MJudge% <> MOK% Then
1145         M_20# = MClear%     '初期化
1146         'エラー処理記述
1147         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1148         'GOT KEY入力待ち
1149         MKeyNumber = fnKEY_WAIT()
1150         '
1151         If MKeyNumber = MAbout% Then            '停止を選択した場合
1152             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1153             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1154             Break
1155         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1156             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1157             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1158             Break
1159         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1160             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1161             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1162             Break
1163         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1164             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1165             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1166             Break
1167         EndIf
1168     Else
1169         'OKの場合
1170         fnPCComuCheck = 1
1171     EndIf
1172     Exit Function
1173 FEnd
1174 '
1175 '■fnProcessCheck
1176 ''' <summary>
1177 ''' 工程抜け確認
1178 ''' </summary>
1179 ''' <returns>    1：工程履歴OK     0：異常終了
1180 '''             -1：前工程履歴NG  -2：自工程履歴あり
1181 '''             -3：モデル仕向NG  -4：タイムアウト
1182 '''             -5：履歴処理エラー
1183 ''' </returns>
1184 ''' <remarks>
1185 ''' Date   : 2021/07/07 : M.Hayakawa
1186 ''' </remarks>'
1187 Function M% fnProcessCheck
1188     fnProcessCheck = 0
1189     MJudge% = MNG%      '一旦NGを初期化とする
1190 '----------工程抜け確認----------
1191     MCommentD1001 = 0   'コメント初期化
1192     For MStaNo = 0 To 5
1193         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1194         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1195         '
1196         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1197             MJudge% = MOK%
1198             fnAutoScreenComment(85)     ' AUTO画面
1199             MStaNo = 5
1200             Break
1201         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1202             MFlgLoop% = 0
1203             MJudge% = MNG%
1204             MCommentD1001 = 27
1205             MCommentD1002 = 22
1206             fnAutoScreenComment(94)     ' AUTO画面
1207             fnProcessCheck = -2         ' NGは-2を返す
1208             MStaNo = 5
1209             Break
1210         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1211            MJudge% = MNG%
1212             MCommentD1001 = 31
1213             MCommentD1002 = 22
1214             fnAutoScreenComment(83)     ' AUTO画面
1215             fnProcessCheck = -3         ' NGは-3を返す
1216             MStaNo = 5
1217             Break
1218         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1219             '履歴NGは直ぐに終了せず繰り返し確認を行う
1220             '前工程の書込みが終了していない可能性があるため
1221             MJudge% = MNG%
1222             MCommentD1001 = 32
1223             MCommentD1002 = 22
1224             fnAutoScreenComment(84)     ' AUTO画面
1225             fnProcessCheck = -1         ' NGは-1を返す
1226             Dly 1.0
1227             '工程抜け確認OFF
1228             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1229             Dly 1.0
1230            'MStaNo = 5
1231             Break
1232         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1233             MFlgLoop% = 0
1234             MJudge% = MNG%
1235             MCommentD1001 = 29
1236             MCommentD1002 = 22
1237             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1238             fnProcessCheck = -5         ' NGは-5を返す
1239             MStaNo = 5
1240             Break
1241         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1242             MJudge% = MNG%
1243             If MCommentD1001 = 32 Then
1244                 '何もしない
1245             Else
1246                 MCommentD1001 = 26
1247             EndIf
1248             MCommentD1002 = 22
1249             fnProcessCheck = -4         ' NGは-4を返す
1250             MStaNo = 5
1251             Break
1252         Else
1253             MJudge% = MNG%
1254             MCommentD1001 = 28
1255             MCommentD1002 = 22
1256         EndIf
1257     Next MStaNo
1258     '工程抜け確認OFF
1259     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1260     '通過履歴NG 工程抜けの場合
1261     If MJudge% = MPass% Then
1262         M_20# = MPass%
1263     EndIf
1264     '
1265     'エラー画面
1266     If MJudge% <> MOK% Then
1267         M_20# = MClear%     '初期化
1268         'エラー処理記述
1269         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1270         'GOT KEY入力待ち
1271         MKeyNumber = fnKEY_WAIT()
1272         '
1273         Select MKeyNumber
1274             Case MAbout%        '停止を選択した場合
1275                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1276                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1277                 Break
1278             Case MNext%         '次へを選択した場合
1279                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1280                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1281                 Break
1282             Case MContinue%     '継続を選択した場合
1283                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1284                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1285                 Break
1286             Case MNgProcess%    'NGを選択した場合
1287                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1288                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1289                 Break
1290         End Select
1291     Else
1292         fnProcessCheck = 1  ' OKは1を返す
1293     EndIf
1294     Exit Function
1295 FEnd
1296 '
1297 '■fnPiasWrite
1298 ''' <summary>
1299 ''' Pias 組立結果書込み要求
1300 ''' </summary>
1301 '''<param name="MFlg%">
1302 '''                 MOK%(1) = 工程履歴にOKを書込む
1303 '''                 MNG%(0) = 工程履歴にNGを書込む
1304 '''</param>
1305 '''<returns></returns>
1306 ''' <remarks>
1307 ''' Date   : 2021/07/07 : M.Hayakawa
1308 ''' </remarks>'
1309 Function M% fnPiasWrite(ByVal MFlg%)
1310       fnPiasWrite = 0
1311 *RETRY_PIASWRITE
1312     '
1313     '組立OK(MOK%)の場合　M306 ON
1314    '組立NG(MNG%)の場合　M307 ON
1315     If MFlg% = MOK% Then
1316         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1317     Else
1318         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1319     EndIf
1320     Dly 0.1                  '念のため
1321     '
1322     'Piasへ書込み開始 M305 -> ON
1323     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1324     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1325     '
1326     MJudge% = MNG%
1327     '
1328     For MStaNo = 0 To 5
1329         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1330             MJudge% = MOK%
1331             'MRet = fnAutoScreenComment(85)  'AUTO画面
1332             MStaNo = 5
1333             Break
1334         '
1335         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1336             MJudge% = MNG%
1337             'MRet = fnAutoScreenComment(85)  'AUTO画面
1338            MCommentD1001 = 34
1339            MCommentD1002 = 25
1340             MStaNo = 5
1341             Break
1342         '
1343         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1344             MJudge% = MNG%
1345             'MRet = fnAutoScreenComment(85)  'AUTO画面
1346            MCommentD1001 = 35
1347            MCommentD1002 = 25
1348             MStaNo = 5
1349             Break
1350         '
1351         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1352             MJudge% = MNG%
1353             'MRet = fnAutoScreenComment(85)  'AUTO画面
1354            MCommentD1001 = 36
1355            MCommentD1002 = 25
1356             MStaNo = 5
1357             Break
1358         '
1359         Else
1360             MJudge% = MNG%
1361            MCommentD1001 = 42
1362            MCommentD1002 = 25
1363         '
1364         EndIf
1365         '
1366     Next MStaNo
1367     '
1368     'Piasへ書込み開始 M305 -> OfF
1369     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1370     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1371     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1372     '
1373     '
1374     '通過履歴NG 工程抜けの場合
1375     If MJudge% = MPass% Then
1376         M_20# = MPass%
1377     EndIf
1378     '
1379    M_20# = MClear%     '初期化
1380     '
1381     'エラー画面
1382     If MJudge% < MOK% Then
1383     '
1384 '残しておくが現状では使用しないラベル
1385 *RETRY_ERR_WRITE
1386         M_20# = MClear%     '初期化
1387         'エラー処理記述
1388         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1389         'GOT KEY入力待ち
1390         MKeyNumber = fnKEY_WAIT()
1391         '
1392         If MKeyNumber = MAbout% Then   '停止を選択した場合
1393             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1394            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1395             Break
1396         '
1397         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1398             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1399             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1400         '
1401         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1402             M_20# = MPass%            'M_20# プログラム間共通外部変数
1403             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1404         '
1405         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1406             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1407            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1408             Break
1409         '
1410         EndIf
1411         '
1412         If M_20# = MClear% Then *RETRY_ERR_WRITE
1413         '
1414     EndIf
1415     '
1416     If M_20# = MContinue% Then *RETRY_PIASWRITE
1417     '
1418     fnPiasWrite = 1
1419     Exit Function
1420 FEnd
1421 '
1422 '■fnPCBNumberCheck
1423 ''' <summary>
1424 ''' Pias 基板番号照合要求
1425 ''' </summary>
1426 '''<param name="%"></param>
1427 '''<param name="%"></param>
1428 '''<returns></returns>
1429 ''' <remarks>
1430 ''' Date   : 2021/07/07 : M.Hayakawa
1431 ''' </remarks>'
1432 Function M% fnPCBNumberCheck
1433       fnPCBNumberCheck = 0
1434     '
1435 *RETRY_PCBCHECK
1436     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1437     'Piasへ基板照合開始 M310 -> ON
1438     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1439     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1440     '
1441     MJudge% = MNG%
1442     '
1443     For MStaNo = 0 To 5
1444         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1445             MJudge% = MOK%
1446             fnAutoScreenComment(96)  'AUTO画面
1447             MStaNo = 5
1448             Break
1449         '
1450         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1451             MJudge% = MNG%
1452             fnAutoScreenComment(97)  'AUTO画面
1453             MCommentD1001 = 37
1454             MCommentD1002 = 25
1455             MStaNo = 5
1456             Break
1457         '
1458         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1459             MJudge% = MNG%
1460             fnAutoScreenComment(98)  'AUTO画面
1461             MCommentD1001 = 38
1462             MCommentD1002 = 25
1463             MStaNo = 5
1464             Break
1465         '
1466         ElseIf M_In(11580) = 1 Then                         'time out
1467             MJudge% = MNG%
1468             fnAutoScreenComment(99)  'AUTO画面
1469             MCommentD1001 = 39
1470             MCommentD1002 = 25
1471             MStaNo = 5
1472             Break
1473         '
1474         Else
1475             MJudge% = MNG%
1476            MCommentD1001 = 41
1477            MCommentD1002 = 25
1478         '
1479         EndIf
1480         '
1481     Next MStaNo
1482     '
1483     'Piasへ基板照合開始 M310 -> OfF
1484     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1485     '
1486     '
1487     '通過履歴NG 工程抜けの場合
1488     If MJudge% = MPass% Then
1489         M_20# = MPass%
1490     EndIf
1491     '
1492    M_20# = MClear%     '初期化
1493     '
1494     'エラー画面
1495     If MJudge% < MOK% Then
1496     '
1497 '残しておくが現状では使用しないラベル
1498 *RETRY_ERR_PCBNUMBER
1499         M_20# = MClear%     '初期化
1500         'エラー処理記述
1501         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1502         'GOT KEY入力待ち
1503         MKeyNumber = fnKEY_WAIT()
1504         '
1505         If MKeyNumber = MAbout% Then   '停止を選択した場合
1506             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1507             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1508             Break
1509         '
1510         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1511             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1512             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1513         '
1514         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1515             M_20# = MPass%            'M_20# プログラム間共通外部変数
1516             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1517         '
1518         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1519             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1520             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1521             Break
1522         '
1523         EndIf
1524         '
1525         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1526         '
1527     EndIf
1528     '
1529     If M_20# = MContinue% Then *RETRY_PCBCHECK
1530     Exit Function
1531 FEnd
1532 '
1533 '■ScrewTight
1534 ''' <summary>
1535 ''' ねじ締めを行う(Sタイト)
1536 ''' </summary>
1537 '''<param name="PScrewPos()">
1538 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
1539 '''             PScrewPos(2)    ：ねじ締め回避点
1540 '''             PScrewPos(10)   ：ねじ締め終了高さ
1541 '''<param name="MScrewType">ネジタイプ(mm/sec)
1542 '''             1:6mm Sタイト銀ネジ
1543 '''             2:8mm Pタイト
1544 '''             3:6mm Sタイト黒ネジ
1545 '''             4:13mm Sタイト
1546 '''             5:6mm Mネジ
1547 '''</param>
1548 '''<param name="MFeedSpd">送り速度(mm/sec)</param>
1549 '''<returns>整数
1550 '''         0=異常終了、1=正常終了
1551 '''</returns>
1552 ''' <remarks>
1553 ''' Date   : 2021/07/07 : M.Hayakawa
1554 ''' Update : 2021/09/28 : M.Hayakawa ネジタイプ、送り速度を引数に追加
1555 ''' </remarks>'
1556 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   'ネジ締め個別設定
1557     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
1558     ScrewTight = 0
1559     MOKNGFlg = 0
1560     Ovrd 100
1561     Mov PScrewPosition(1)       ' パレット上ねじ締めS①の安全回避位置
1562     Fine 0.05 , P
1563     Ovrd MOvrdA%
1564     ' 減速設定
1565     Accel 100, 10
1566     ' パレット上ねじ締め開始位置へ移動
1567     Mvs PScrewPosition(2)
1568     ' 加減速を元に戻す
1569     Accel
1570     ' 内部Ovrd設定
1571 '    Ovrd MOvrdA%
1572     Ovrd 100
1573     ' Spd設定
1574 '    Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
1575     Spd MFeedSpd
1576     ' 設定進み量5.0 × 操作パネルのオーバーライド係数 × プログラム内オーバーライド係数
1577     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
1578     Select MScrewType%
1579         Case 1
1580             ' Sタイト：プログラム1、バンク1に設定
1581             ProgramBankSet(1,1)
1582             Break
1583         Case 2
1584             ' Pタイト：プログラム1、バンク1に設定
1585             ProgramBankSet(3,1)
1586             Break
1587         Case 3
1588             ' Sタイト黒：プログラム1、バンク1に設定
1589             ProgramBankSet(1,1)
1590             Break
1591         Case 4
1592             ' Sタイト13mm：プログラム1、バンク1に設定
1593             ProgramBankSet(1,1)
1594             Break
1595         Case 5
1596             ' Mネジ：プログラム1、バンク1に設定
1597             ProgramBankSet(1,1)
1598             Break
1599         Case 6
1600             ' Sタイト：プログラム1、バンク4に設定
1601             ProgramBankSet(1,4)
1602             Break
1603         Default
1604             ' プログラム1、バンクなし設定
1605             ProgramBankSet(0,0)
1606             Break
1607     End Select
1608 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     'ドライバーON　CW
1609      'ドライバーON　CW
1610     M_Out(12241)=1
1611     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1612     Wait M_In(11584)=1          '完了/エラー検出 暫定コメント 10/6 M.H
1613     Dly 0.1
1614     Fine 0 , P
1615     Spd M_NSpd
1616     '
1617     If M_In(11256)=1 Then  'ねじトータルエラー検出時
1618         M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1619         Dly 0.1
1620        ' プログラム・バンク解除
1621         ProgramBankSet(0,0)
1622         'パレット上ねじ締め終了位置上空へ移動
1623         Mvs PScrewPosition(10),-80
1624         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1625         M_Out(12249)=1 Dly 0.3
1626         MOKNGFlg = -1
1627         ScrewTight = 0
1628     Else
1629          'ドライバーOFF　CW
1630         M_Out(12241)=0
1631 '        エラーがない場合はネジ締め終了位置で増し締め
1632 '        Select MScrewType%
1633 '            Case 1
1634 '                ' Sタイト：プログラム1、バンク3に設定
1635 '                ProgramBankSet(1,3)
1636 '                Break
1637 '            Case 2
1638 '                ' Pタイト：プログラム1、バンク3に設定
1639 '                ProgramBankSet(3,3)
1640 '                Break
1641 '            Case 3
1642 '                ' Sタイト黒：プログラム1、バンク3に設定
1643 '                ProgramBankSet(1,3)
1644 '                Break
1645 '            Case 4
1646 '                ' Sタイト13mm：プログラム1、バンク3に設定
1647 '                ProgramBankSet(1,3)
1648 '                Break
1649 '            Case 5
1650 '                ' Mネジ：プログラム1、バンク3に設定
1651 '                ProgramBankSet(1,3)
1652 '                Break
1653 '            Default
1654 '                ' プログラム1、バンクなし設定
1655 '                ProgramBankSet(0,0)
1656 '                Break
1657 '        End Select
1658 '         'ドライバーON　CW
1659 '        Mvs PScrewPosition(10)
1660 '        M_Out(12241)=1
1661 '        Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1662 '        Fine 0 , P
1663 '
1664          'ドライバーOFF　CW
1665         M_Out(12241)=0
1666        ' プログラム・バンク解除
1667         ProgramBankSet(0,0)
1668         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1669         M_Out(12249)=1 Dly 0.3
1670     '     ↓PScrewPos(2) → PScrewPosition(10)に変更 9/16 M.Hayakawa
1671         'パレット上ねじ締め終了位置上空へ移動
1672        Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1673         'Mvs PScrewPosition(10),-80
1674         ScrewTight = 1
1675     EndIf
1676 ' 暫定（暫定マスク　9/16 M.Hayakawa)
1677 '    Ovrd 10
1678 '    Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1679     Ovrd 100
1680     Exit Function
1681 FEnd
1682 '
1683 '■ScrewGet
1684 ''' <summary>
1685 ''' ねじ供給機からねじを得る
1686 ''' </summary>
1687 '''<param name="%">
1688 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
1689 '''         PScrewPos(2)    ：ねじ供給器回避点
1690 '''         PScrewPos(9)    ：ねじ供給器上空ネジ捨位置
1691 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1692 '''         PScrewPos(3)    ：Mねじポカヨケ位置
1693 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
1694 '''</param>
1695 '''<param name = FeederReadyNo%> 指定の供給機Ready </param>
1696 '''<param name = FeederScrewSensor%> 指定の誤供給防止センサー指定(0でセンサー無し)</param>
1697 '''<returns>整数
1698 '''         0=異常終了、1=正常終了、-1=ねじ供給NG、-2=ねじ誤供給NG、-3=吸着エラー
1699 '''</returns>
1700 ''' <remarks>
1701 ''' Date   : 2021/07/07 : M.Hayakawa
1702 ''' </remarks>
1703 '''<update>
1704 '''Date    : 2021/11/15 : 中村
1705 '''</update>
1706 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
1707     fnAutoScreenComment(522)    '状態表示[ネジ供給待ち] 2022/05/09 渡辺
1708     ScrewGet = 0
1709     MScrewJudge% = 0
1710     'ねじ供給器初期動作エラーチェック
1711 ' ↓暫定削除
1712     'Mov PScrewPosition(2)   'ねじ供給機回避点へ移動
1713     For MCnt% = 0 To MFinCnt%
1714         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '指定ねじ供給機がReadyになっているか確認(5秒間)
1715         If MRtn = 0 Then
1716             M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
1717             ScrewGet = -1
1718             MScrewJudge% = 2
1719         EndIf
1720         Ovrd 100
1721         If FeederScrewSensor% <> 0 Then
1722             If M_In(FeederScrewSensor%) = 1 Then  '誤供給が検出されたら
1723                 'Ovrd 30
1724                 M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
1725                 'NGとしてここの関数から抜ける
1726                 ScrewGet = -2
1727                 MScrewJudge% = 3
1728             EndIf
1729         EndIf
1730         Ovrd 100
1731         Spd M_NSpd
1732         If MScrewJudge% = 0 Then
1733     '        ScrewGet = 0
1734             M_Out(Y63_Driver)=1         ' バンクセッティング　C2
1735             Dly 0.3
1736             MScrewCnt% = 0
1737             MFinCnt% = 2
1738             fnAutoScreenComment(521)     '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
1739             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1740             'Ovrd 40 '2に変更 10/6 M.H '5に変更10/7中村
1741             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1742             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
1743             'Mvs PScrewPosition(10), 1.2
1744            Mvs PScrewPosition(10)       'Fan用ねじ吸着位置修正のため変更 2022-02-01AJI
1745             'ビット回転(処理位置変更)
1746             M_Out(Y60_Driver)=1
1747             M_Timer(4) = 0
1748             MloopFlg = 0
1749             MCntTime& = 0
1750             While MloopFlg = 0
1751                 MCrtTime& = M_Timer(4)
1752                 If MCrtTime& >= 180 Then
1753                     MloopFlg = 1
1754                 EndIf
1755             WEnd
1756             M_Out(Y68_VV1)=1 Dly 0.3     ' ねじ吸着　ON'Dly 0.3追加(8/27中村)
1757             '吸着確認
1758             MRtn = 0
1759             MRtn = frInCheck(11271, 1, MSETTIMEOUT01&)
1760 '            'ビット回転(処理位置変更)
1761 '            M_Out(Y60_Driver)=1
1762 '            Dly 0.2
1763             '
1764             JOvrd M_NJovrd
1765             Spd M_NSpd
1766             'ネジ吸着確認位置移動
1767             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
1768             Mvs PScrewPosition(10), -30  ' ネジ吸着確認位置
1769            'Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1770             'ビット回転停止
1771             M_Out(Y60_Driver)=0
1772             '
1773 '            If MRtn = 1 Then           '最初の閾値を確認しない
1774                 '1秒間ネジ吸着確認
1775                 MRtn = frInCheck(11272, 1, MSETTIMEOUT01&)
1776 '            EndIf
1777             'MRtn = 0'強制エラー
1778             '吸着エラーの場合
1779             'ネジをねじ太郎に戻す
1780             If MRtn = 0 Then
1781                 Ovrd 30      '2から5に変更
1782                 'ビット回転停止
1783                 M_Out(Y60_Driver)=0
1784                 'ネジ供給機上空
1785                 Mvs PScrewPosition(1)
1786                 '更に上空
1787                 Mov PScrewPosition(1), -140
1788                 'ネジ捨て位置
1789                 MRtn = FnCtlValue2(4)          '吸着エラー数＋１  2022/04/28 渡辺
1790                 Mov PScrewPosition(9)
1791                 MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1792                 '吸着OFF
1793                 M_Out(12249)=1 Dly 0.3 'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1794                 Dly 0.2
1795                 '破壊ON
1796                 M_Out(Y6B_VB1)=1 '真空破壊ON
1797                 'ビット回転
1798                 M_Out(Y61_Driver)=1
1799                 Dly 0.5
1800                 '                '
1801                 Ovrd 100
1802                 JOvrd M_NJovrd
1803                 Spd M_NSpd
1804                 'ドライバーを上下させねじを振り落とす
1805                 Mov PScrewPosition(9), 10
1806                 Mov PScrewPosition(9)
1807                 Dly 0.1
1808                 Mov PScrewPosition(9), 10
1809                 Mov PScrewPosition(9)
1810                 '
1811                 'ネジ落ち待ち
1812                 Wait M_In(11272) = 0
1813                 'ビット回転停止
1814                 M_Out(Y61_Driver)=0
1815                 Dly 0.1
1816                 '破壊OFF
1817                 M_Out(Y6B_VB1)=0 '真空破壊OFF
1818                 'ねじ落ちたとして、移動更に上空
1819                 Mov PScrewPosition(1), -140
1820                 Ovrd 100
1821                 Spd M_NSpd
1822                 'ネジ供給機上空
1823                 Mvs PScrewPosition(1)
1824 '                '
1825                 ScrewGet = -3
1826                 If MCnt% = MFinCnt% Then
1827                     MScrewJudge% = 4
1828                     Mov PScrewPosition(2)
1829                     Break
1830                 EndIf
1831                 Break
1832 '                '
1833             Else
1834                 MCnt% = MFinCnt%
1835                 ScrewGet = 1
1836             EndIf
1837         Else
1838             MCnt% =MFinCnt%
1839         EndIf
1840     Next  MCnt%
1841         '
1842 '    If MScrewJudge% = 0 Then
1843 '        Ovrd 100
1844 '        Spd M_NSpd
1845 '        PScrewPosition(1)
1846 '        Mvs PScrewPosition(10), -30  ' ねじピックアップ位置 -30mm
1847 '        'Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1848 '        M_Out(Y60_Driver)=0     ' ビット回転停止
1849 '        M_Out(Y63_Driver)=0     ' バンクセッティング　C2
1850 '        'Mvs PScrewPosition(10), -30  ' ねじピックアップ位置 -30mm
1851 '        Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1852 '        'Mov PScrewPosition(2)
1853 '        'もう一度吸着確認　上空の最終閾値
1854 '        MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1855 '        If MRtn = 0 Then      '吸着エラーの場合
1856 '            MScrewJudge% = 4
1857 '            ScrewGet = -3
1858 '        ElseIf MRtn = 1 Then      '吸着OKの場合
1859 '            MScrewJudge% = 1
1860 '            ScrewGet = 1
1861 '        EndIf
1862 '        Break
1863 '    EndIf
1864     '
1865 '    If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
1866     If MScrewJudge% = 0 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
1867     '
1868     Select MScrewJudge%
1869 '        Case 0
1870 ''            fErrorProcess(11,162,163,0) '異常終了
1871 '            MCommentD1001 = 162
1872 '            MCommentD1002 = 96
1873 '            Break
1874         Case 2
1875 '            fErrorProcess(11,63,161,0) '供給NG
1876             MCommentD1001 = 63
1877             MCommentD1002 = 96
1878             Break
1879         Case 3
1880 '            fErrorProcess(11,160,164,0) '誤供給
1881             MCommentD1001 = 237
1882             MCommentD1002 = 96
1883             Break
1884         Case 4
1885 '            fErrorProcess(11,94,95,0) '吸着NG
1886             MCommentD1001 = 94
1887             MCommentD1002 = 95
1888             Break
1889     End Select
1890     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
1891     '
1892     Select M_20#
1893         Case MAbout%          '停止が押された場合
1894             Mov PScrewPosition(2)                  '初期位置に戻って停止処理
1895             Mov PInitialPosition
1896             Break
1897         Case MContinue%       'リトライが押されていた場合(関数を抜けた先で処理)
1898             Break
1899         Case MNext%           '継続が押された場合
1900             M_20# = MClear%     '初期化
1901             Break
1902         Case MNgProcess%      'NGが押された場合
1903             Mov PScrewPosition(2)   'PIASにNG書き込みを行い,初期位置に戻って行程終了
1904             Mov PInitialPosition
1905             Break
1906         End Select
1907 *End_ScrewGet
1908     Exit Function
1909 FEnd
1910 '
1911 '■ProgramBankSet
1912 ''' <summary>
1913 ''' ねじ締めを行う(Pタイト)
1914 ''' </summary>
1915 '''<param name="MProgramNo">プログラム番号</param>
1916 '''<param name="MBankNo">バンク番号</param>
1917 '''</returns>
1918 ''' <remarks>
1919 ''' Date   : 2021/10/05 : M.Hayakawa
1920 ''' </remarks>'
1921 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
1922 '
1923     MLocalPrgNo% = (MProgramNo% - 1) * 32
1924     MLocalBankNo% = MBankNo% * 4
1925 '
1926     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
1927         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
1928     Else
1929         MLocalOutNo% = 0
1930     EndIf
1931 '
1932     M_Out8(12240) = MLocalOutNo%
1933     Dly 0.1
1934     Exit Function
1935 FEnd
1936 '
1937 '■fnKEY_WAIT()
1938 ''' <summary>
1939 ''' GOTからのキー入力待ち
1940 ''' </summary>
1941 '''<returns>1：停止    2：次へ
1942 '''         3：継続    4：トルクチェック開始
1943 '''         5：NG
1944 '''         11：ロボット初期位置1    12：ロボット初期位置2
1945 '''         13：ロボット初期位置3    14：ロボット初期位置4
1946 '''</returns>
1947 ''' <remarks>
1948 ''' Date   : 2021/07/07 : M.Hayakawa
1949 ''' </remarks>'
1950 Function M% fnKEY_WAIT()
1951     fnKEY_WAIT = 0
1952     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
1953     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
1954     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
1955     '下記キー待ちの継続に反応させないため
1956     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
1957     Dly 0.2
1958     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
1959     MLocalLoopFlg=1
1960     While MLocalLoopFlg=1
1961         If M_In(11345) = 1 Then         '停止   M5345
1962             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
1963             fnKEY_WAIT = 1
1964             MLocalLoopFlg=-1
1965             Break
1966         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
1967             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
1968             fnKEY_WAIT = 2
1969             MLocalLoopFlg=-1
1970             Break
1971         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
1972             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
1973             fnKEY_WAIT = 3
1974             MLocalLoopFlg=-1
1975             Break
1976         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
1977             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
1978             fnKEY_WAIT = 4
1979             MLocalLoopFlg=-1
1980             Break
1981         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
1982             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
1983             fnKEY_WAIT = 5
1984             MLocalLoopFlg=-1
1985             Break
1986             '
1987         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
1988             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
1989             fnKEY_WAIT = MRobotInit1%
1990             MLocalLoopFlg=-1
1991             Break
1992             '
1993         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
1994             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
1995             fnKEY_WAIT = MRobotInit2%
1996             MLocalLoopFlg=-1
1997             Break
1998             '
1999         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
2000             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
2001             fnKEY_WAIT = MRobotInit3%
2002             MLocalLoopFlg=-1
2003             Break
2004             '
2005         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
2006             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
2007             fnKEY_WAIT = MRobotInit4%
2008             MLocalLoopFlg=-1
2009             Break
2010             '
2011         Else
2012         EndIf
2013     WEnd
2014     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
2015     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
2016     Exit Function
2017 FEnd
2018 '
2019 '■ fnAUTO_CTL
2020 ''' <summary>
2021 ''' AUTOモードOFF、PLCからの開始待ち
2022 ''' </summary>
2023 ''' <remarks>
2024 ''' Date   : 2021/07/07 : M.Hayakawa
2025 ''' </remarks>
2026 Function M% fnAUTO_CTL
2027     fnAUTO_CTL = 0
2028     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2029     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
2030     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2031     '
2032     If M_Svo=0 Then             'サーボON確認
2033         Servo On
2034     EndIf
2035     Wait M_Svo=1
2036     Exit Function
2037 FEnd
2038 '
2039 '■ fnWindScreenOpen
2040 ''' <summary>
2041 ''' ウィンド画面の表示、非表示設定
2042 ''' </summary>
2043 '''<param name="%"></param>
2044 '''<param name="%"></param>
2045 '''<param name="%"></param>
2046 '''<param name="%"></param>
2047 ''' <remarks>
2048 ''' コメントD1001, D1002, D1003の設定
2049 ''' MWindReSet = 0     画面非表示
2050 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2051 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2052 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2053 ''' Date   : 2021/07/07 : M.Hayakawa
2054 ''' </remarks>
2055 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2056     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2057         M_Out16(12480) = MCommentD1001            'D1001 コメント
2058     EndIf
2059     '
2060     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2061         M_Out16(12496) = MCommentD1002            'D1002 コメント
2062     EndIf
2063     '
2064     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2065        M_Out16(12512) = MCommentD1003            'D1003 コメント
2066     EndIf
2067     '
2068     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2069     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2070     Dly 0.5
2071     M_Out(12363) = 0                         'ウィンド画面設定
2072     Exit Function
2073 FEnd
2074 '
2075 '■FnCtlValue2
2076 ''' <summary>
2077 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2078 ''' </summary>
2079 ''' <param name="MCtlNo%"></param>
2080 ''' <remarks>
2081 ''' Date : 2022/04/28 渡辺
2082 ''' </remarks>
2083 '''
2084 '''  1：投入数       ＋１
2085 '''  2：組立ＯＫ数   ＋１
2086 '''  3：組立ＮＧ数   ＋１ (未使用)
2087 '''  4：吸着エラー数 ＋１
2088 ''' 99：読書開始信号 OFF
2089 '''
2090 Function M% FnCtlValue2(ByVal MCtlNo%)
2091     FnCtlValue2 = 1
2092     Select MCtlNo%
2093         Case 1        '投入数＋１
2094             M_Out(12569) = 0             '書込み開始信号OFF
2095             M_Out(12568) = 1             '読込み開始信号ON
2096             MInputQty = M_In16(11600)    '投入数受信
2097             MInputQty = MInputQty + 1    '投入数＋１
2098             M_Out16(12592) = MInputQty   '投入数送信
2099             M_Out(12569) = 1             '書込み開始信号ON
2100             Break
2101             '
2102         Case 2        '組立ＯＫ数＋１
2103             M_Out(12569) = 0             '書込み開始信号OFF
2104             M_Out(12568) = 1             '読込み開始信号ON
2105             MAssyOkQty = M_In16(11616)   '組立OK数受信
2106             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2107             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2108             M_Out(12569) = 1             '書込み開始信号ON
2109             Break
2110             '
2111         Case 4        '吸着エラー数＋１
2112             M_Out(12569) = 0                       '書込み開始信号OFF
2113             M_Out(12568) = 1                       '読込み開始信号ON
2114             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
2115             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
2116             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
2117             M_Out(12569) = 1                       '書込み開始信号ON
2118             Break
2119             '
2120         Case 99        '読書開始信号OFF
2121             M_Out(12568) = 0        '読込み開始信号OFF
2122             M_Out(12569) = 0        '書込み開始信号OFF
2123             Break
2124             '
2125     End Select
2126     Exit Function
2127 FEnd
2128 '
2129 'Insightによる画像処理検査実行（並列処理なし）
2130 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2131 '-------------------------------------------------------------------------------
2132 'Insightによる画像処理検査実行（並列処理なし）
2133 '   引数
2134 '       PInspPos()      ：検査位置
2135 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2136 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2137 '       MInspCnt%       ：検査位置数
2138 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2139 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2140 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2141 '   戻り値：整数
2142 '       0=異常終了、1=正常終了
2143 '
2144 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2145 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2146 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2147 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2148 '   20200410    :   検査グループ設定Retry追加
2149 '-------------------------------------------------------------------------------
2150     '----- 初期設定 -----
2151     Cnt 0                                                           '移動効率化解除(初期値=0)
2152     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2153 '    Cnt 1,0.1,0.1
2154     '変数宣言・初期化
2155     Def Inte MNum                                                   '検査番号(検査順1～)
2156     MNum% = 1                                                       '検査番号初期値設定
2157     Def Inte MEndFlg                                                '検査終了フラグ
2158     MEndFlg% = 0
2159     '
2160     '検査G番号設定要求・検査実行要求off
2161     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2162     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2163     'エラー番号クリア
2164     MInspErrNum = 0                                                 '検査実行エラー番号
2165     M_Out16(MOUT_InspErrNum) = MInspErrNum
2166     MInspNGStepNum = 0                                              '検査実行NGStep番号
2167     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2168     '
2169     'Insight Ready check?
2170     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2171         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2172         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2173         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2174         ISInspectionSingle = 0                                      '異常終了戻り値設定
2175         Exit Function
2176     EndIf
2177     '
2178     '検査位置数確認
2179     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2180         MInspErrNum = 21                                            '検査データなし 21　引数<1
2181         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2182         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2183         ISInspectionSingle = 0                                      '異常終了戻り値設定
2184         Exit Function
2185     EndIf
2186     '
2187     '
2188     '
2189     '----- メイン処理 -----
2190     '設定された検査位置数分の検査実行
2191     While( MEndFlg% = 0 )
2192         '----- 検査グループ番号設定Retry追加 20200410
2193         MSetGrNumRetryExitFlg = 0
2194         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2195         While( MSetGrNumRetryExitFlg = 0 )
2196         '----- 検査グループ番号設定Retry追加ここまで 20200410
2197             '
2198             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2199             '
2200             '----- 検査グループ番号設定 -----
2201             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2202             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2203             '
2204             '検査位置へ移動・移動完了待ち
2205             fnAutoScreenComment(521)                                    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2206             Mvs PInspPos( MNum% )                                       '移動
2207             fnAutoScreenComment(523)                                    '状態表示[画像処理検査中] 2022/05/09 渡辺
2208             Dly 0.05                                                    '移動完了後Delay
2209             '
2210             '検査グループ番号設定終了確認
2211             M_Timer(1) = 0
2212             MExitFlg = 0
2213             While( MExitFlg = 0 )
2214                 '検査G設定正常終了?
2215                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2216                     MExitFlg = 1
2217                 '
2218                 '検査G設定異常終了?
2219                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2220                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2221                     If MInspErrNum = 0 Then                             '1回目のエラー?
2222                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2223                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2224                     EndIf
2225                     MExitFlg = 1
2226                 '
2227                 'timeoutチェック
2228                 ElseIf 1000 < M_Timer(1) Then
2229                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2230                     If MInspErrNum = 0 Then                             '1回目のエラー?
2231                         MInspErrNum = 12                                'timeout エラー番号=12
2232                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2233                     EndIf
2234                     MExitFlg = 1
2235                 EndIf
2236             WEnd
2237             '
2238             '検査G番号設定要求off
2239             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2240             '
2241             '----- 検査グループ設定Retry追加 20200410
2242             'NGなければ抜ける
2243             If MCurrentStepErr = 0 Then
2244                 MSetGrNumRetryExitFlg = 1
2245             Else
2246                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2247                 If MSetGrNumRetryCnt = 0 Then
2248                     MSetGrNumRetryExitFlg = 1
2249                 Else
2250                     'Retryへ　その前にDelay
2251                     Dly 0.5
2252                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2253                 EndIf
2254             EndIf
2255             '----- 検査グループ設定Retry追加ここまで 20200410
2256             '
2257         WEnd
2258         '
2259         '
2260         '
2261         '----- 検査実行 -----
2262         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2263             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2264                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2265                 MInspRetryExitFlg = 0
2266                 MRetryCnt = 2                                        'Retry回数設定
2267                 While( MInspRetryExitFlg = 0 )
2268                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2269                     '
2270                     '検査完了確認
2271                     MRetryCnt = MRetryCnt - 1
2272                     M_Timer(1) = 0
2273                     MExitFlg = 0
2274                     While( MExitFlg = 0 )
2275                     '検査完了待ち
2276                         '検査OK終了?
2277                         If M_In( MIN_IS_InspOK% ) = 1  Then
2278                             MJudgeOKFlg = 1                         '検査OKフラグON
2279                             MExitFlg = 1
2280                         '
2281                         '検査NG終了?
2282                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2283                             If MInspErrNum = 0 Then                 '1回目のエラー?
2284                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2285                                     MInspErrNum = 32                    '検査NG エラー番号=32
2286                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2287                                 EndIf
2288                             EndIf
2289                             MExitFlg = 1
2290                         '
2291                         '検査異常終了(IS timeout)?
2292                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2293                             If MInspErrNum = 0 Then                 '1回目のエラー?
2294                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2295                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2296                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2297                                 EndIf
2298                             EndIf
2299                             MExitFlg = 1
2300                         '
2301                         'timeoutチェック
2302                         ElseIf 3000 < M_Timer(1) Then
2303                             If MInspErrNum = 0 Then                 '1回目のエラー?
2304                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2305                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2306                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2307                                 EndIf
2308                             EndIf
2309                             MExitFlg = 1
2310                         EndIf
2311                     WEnd
2312                     '
2313                     '検査開始要求off
2314                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2315                     '
2316                     'OKなら抜ける
2317                     If MJudgeOKFlg = 1 Then
2318                         MInspRetryExitFlg = 1
2319                     Else
2320                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2321                         If MRetryCnt = 0 Then
2322                             MInspRetryExitFlg = 1
2323                         Else
2324                             'Retryへ　その前にDelay
2325                             Dly 0.3
2326                         EndIf
2327                     EndIf
2328                     '
2329                 WEnd
2330             EndIf
2331         EndIf
2332         '
2333         '
2334         '
2335         MNum% = MNum% + 1                                           '検査Step+1
2336         '検査終了確認　検査終了フラグセット
2337         If (MInspCnt% < MNum% ) Then
2338             MEndFlg% = 1                                            '検査終了フラグセット
2339         EndIf
2340         'NG発生時続行時処理
2341         If MInspErrNum <> 0 Then                                    'NGあり?
2342             If MNgContinue% <> 1 Then                               'NG続行?
2343                 MEndFlg% = 1                                        '検査終了フラグセット
2344             EndIf
2345         EndIf
2346     WEnd
2347     '
2348     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2349     If 0 < MZAxis% Then
2350         PCurrentPos = P_Curr                                        '現在位置取得
2351         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2352         fnAutoScreenComment(521)                                    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2353         Mvs PCurrentPos                                             '現在位置上空へ移動
2354     EndIf
2355     '
2356     '戻り値設定
2357     If MInspErrNum = 0 Then
2358         ISInspectionSingle = 1                                      '正常終了戻り値設定
2359     Else
2360         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2361         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2362         ISInspectionSingle = 0                                      '異常終了戻り値設定
2363     EndIf
2364     Fine 0 , P
2365     Exit Function
2366 FEnd
2367 '
2368 '■fnAutoScreenComment
2369 ''' <summary>
2370 ''' メイン画面の動作状況表示
2371 ''' コメントD1005の設定
2372 ''' </summary>
2373 '''<param name="McommentD1005%">コメントID</param>
2374 ''' <remarks>
2375 ''' Date   : 2021/07/07 : M.Hayakawa
2376 ''' </remarks>
2377 Function fnAutoScreenComment(ByVal McommentD1005%)
2378     M_Out16(12576) = McommentD1005%
2379     Exit Function
2380 FEnd
2381 '
2382 '■fnRoboPosChk
2383 ''' <summary>
2384 ''' 最後に終了したロボットポジションの確認
2385 ''' </summary>
2386 '''<param name="MINNumber%">入力番号</param>
2387 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2388 '''<param name="MTimeCnt&">タイムアウト時間</param>
2389 ''' PLCに保続した番号を読込み、確認
2390 ''' MRBTOpeGroupNo = 5 が初期位置に設定
2391 '''<returns>整数 0:タイムアウト 1:OK</returns>
2392 ''' <remarks>
2393 ''' Date   : 2021/07/07 : M.Hayakawa
2394 ''' </remarks>
2395 Function M% fnRoboPosChk
2396     fnRoboPosChk = 0
2397     MRet = fnStepRead()
2398     '初期位置でないと判断した場合
2399     'ウィンド画面切換え
2400     If MRBTOpeGroupNo > 5 Then
2401         '下記キー待ちの継続に反応させないため
2402         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2403         Dly 0.2
2404         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2405         Dly 1.5
2406         '
2407         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
2408         '
2409         MLoopFlg% = 1
2410         While MLoopFlg% = 1
2411             '
2412             '
2413             MKeyNumber% = fnKEY_WAIT()
2414             Select MKeyNumber%
2415                 Case Is = MAbout%       '停止
2416                     M_20# = MAbout%
2417                     MLoopFlg% = -1
2418                     Break
2419                 Case Is = MNext%        '次へ
2420                     'MLoopFlg% = -1
2421                     Break
2422                 Case Is = MContinue%    '継続
2423                     M_20# = MContinue%
2424                     MLoopFlg% = -1
2425                     Break
2426                 Default
2427                     Break
2428             End Select
2429         WEnd
2430     EndIf
2431     '
2432     If M_20# = MContinue% Then                              '継続ボタンが押された場合
2433         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
2434         Ovrd 5                                   '低速オーバーライド値設定
2435         Select MRBTOpeGroupNo
2436             Case Is = 5                          '何もしない
2437                 Break
2438             Case Is = 10                         '初期位置へ戻す
2439                 'Mov PTEST001
2440                 Break
2441             Case Is = 15                         '初期位置へ戻す
2442                 'Mov PTEST002
2443                 Dly 0.5
2444                 'Mov PTEST001
2445                 Dly 0.5
2446                 Break
2447             Default
2448                 Break
2449         End Select
2450         '
2451         Ovrd M_NOvrd                            'システムの初期値を設定
2452         M_Out(12364) = 1                        'toPLC_データ保存ON
2453         MRBTOpeGroupNo = 5
2454         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
2455         Dly 1.0
2456         M_Out(12364) = 0                        'toPLC_データ保存OFF
2457         fnRoboPosChk = 1                        '初期位置動作実行
2458         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
2459     EndIf
2460     Exit Function
2461 FEnd
2462 '
2463 '■frInCheck
2464 ''' <summary>
2465 ''' センサーINチェック
2466 ''' </summary>
2467 '''<param name="MINNumber%">入力番号</param>
2468 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2469 '''<param name="MTimeCnt&">タイムアウト時間</param>
2470 '''<returns>整数 0:タイムアウト 1:OK</returns>
2471 ''' <remarks>
2472 ''' Date   : 2021/07/07 : M.Hayakawa
2473 ''' </remarks>
2474 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2475     M_Timer(4) = 0
2476     MloopFlg = 0
2477     While MloopFlg = 0
2478         MCrtTime& = M_Timer(4)
2479         If M_In(MINNumber%) = MCMPFLG% Then
2480             MloopFlg = 1
2481             frInCheck = 1
2482         ElseIf MCrtTime& > MTimeCnt& Then
2483             MloopFlg = 1
2484             frInCheck = 0
2485         EndIf
2486     WEnd
2487     Exit Function
2488 FEnd
2489 '-----------------------------------------------
2490 '
2491 'ねじ締め機通信確認
2492 '
2493 '-----------------------------------------------
2494 Function M% fScewTcomChk
2495     fScewTcomChk = 0
2496     '通信確認送信
2497     M_Out(MOUT_ScwT_ComChk%) = MOn%
2498     '通信確認受信待機
2499     Wait M_In(MIN_ScwT_comOK%) = MOn%
2500     '通信確認送信終了
2501     M_Out(MOUT_ScwT_ComChk%) = MOff%
2502     Exit Function
2503 FEnd
2504 '
2505 '
2506 '-----------------------------------------------
2507 '
2508 'ねじ締め開始送信
2509 '
2510 '-----------------------------------------------
2511 Function M% fScewTStart
2512     fScewTStart = 0
2513     'ねじ締め開始待機を受信
2514     Wait M_In(MIN_ScwT_STRec%) = MOn%
2515     Dly 0.1
2516     'ねじ締め開始受信を送信
2517     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msecパルス
2518     Exit Function
2519 FEnd
2520 '
2521 '
2522 '-----------------------------------------------
2523 '
2524 'ねじ締め完了受信
2525 '
2526 '-----------------------------------------------
2527 Function M% fScewTFinish
2528     fScewTFinish = 0
2529     'ねじ締め完了待機を受信
2530     Wait M_In(MIN_ScwT_Fin%) = MOn%
2531     Dly 0.1
2532     'ねじ締め完了受信を送信
2533     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msecパルス
2534     Exit Function
2535 FEnd
2536 '
2537 '
2538 '-----------------------------------------------
2539 '
2540 '条件xx停止受信
2541 '
2542 '-----------------------------------------------
2543 Function M% fScewTCaseStop(ByVal MCase%())
2544     fScewTCaseStop = 0
2545     '条件xx停止を受信
2546     Wait M_In(MCase%(1)) = MOn%
2547     Dly 0.1
2548     '条件xx停止受信を送信
2549     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msecパルス
2550     Exit Function
2551 FEnd
2552 '
2553 '-----------------------------------------------
2554 '
2555 '再開始受信
2556 '
2557 '-----------------------------------------------
2558 Function M% fScewTReStart()
2559     fScewTReStart = 0
2560     '再開始を受信
2561     Wait M_In(MIN_ScwT_ReST%) = MOn%
2562     Dly 0.1
2563     '再開始受信を送信
2564     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msecパルス
2565     Exit Function
2566 FEnd
2567 '
2568 '■fErrorProcess
2569 '<summary>
2570 'エラー処理
2571 '</summary>
2572 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
2573 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
2574 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
2575 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
2576 '<make>
2577 '2021/11/5 中村天哉
2578 '</make>
2579 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2580     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
2581     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
2582     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
2583     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
2584 *RETRY_ERR_PROCESS
2585      M_20# = MClear%     '初期化
2586 '        'エラー処理記述
2587         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2588 '        'GOT KEY入力待ち
2589         MKeyNumber = fnKEY_WAIT()
2590 '        '
2591         If MKeyNumber = MAbout% Then   '停止を選択した場合
2592             M_20# = MAbout%            'M_20# プログラム間共通外部変数
2593 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2594             Break
2595          '
2596         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
2597             M_20# = MContinue%            'M_20# プログラム間共通外部変数
2598 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2599         '
2600         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
2601             M_20# = MNext%            'M_20# プログラム間共通外部変数
2602 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2603          '
2604         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
2605             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
2606 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2607             Break
2608         '
2609         EndIf
2610         '
2611         If M_20# = MClear% Then *RETRY_ERR_PROCESS
2612         fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2613     Exit Function
2614 FEnd
2615 '
2616 '■fnInitialZone
2617 ''' <summary>
2618 ''' 現在位置から上空に待避し、初期位置に戻る
2619 ''' </summary>
2620 ''' <param name="posNum%">移動先のポジション番号</param>
2621 ''' <remarks>
2622 ''' Date : 2021/12/2 : M.Hayakawa
2623 ''' Update:2022/06/2 : M.Hayakawa 他工程の非常停止復帰に合わせて変更
2624 ''' </remarks>
2625 Function fnInitialZone()
2626     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中]
2627 '
2628     Ovrd 5
2629 ' 上空退避
2630     PActive = P_Curr
2631     Pmove = PActive
2632 '
2633     If PActive.X > 580 Then
2634         Pmove.Z =380        'パレット上に腕を伸ばしているときは500まで上げられない為、例外処置
2635     Else
2636         Pmove.Z =500        '上記以外はZ:500まで持ち上げ
2637     EndIf
2638 '
2639     Mvs Pmove
2640     Mov PInitialPosition
2641 ' ロックを開放
2642     InitialState()
2643 ' 一旦停止
2644     fErrorProcess(20,70,256,0)
2645     Exit Function
2646  FEnd
2647 '
2648 '■InitialState
2649 ''' <summary>
2650 ''' ハンド、治具を初期位置にする
2651 ''' </summary>
2652 ''' <returns>   0 : OK
2653 '''             1 : NG
2654 ''' </returns>
2655 ''' <remarks>
2656 ''' Date : 2021/12/2 : M.Hayakawa
2657 ''' </remarks>
2658 Function M% InitialState()
2659     InitialState = 0
2660     '
2661     '位置決め解除
2662     M_Out(12264) = 0
2663     M_Out(12265)=1 Dly 0.3                  'プッシュ解除
2664     'Wait M_In(11276)=1                      'プッシュ戻端検出(修正につきコメントアウト(8/26中村))
2665     MRtn = frInCheck(11276,1,MSETTIMEOUT05&)    'プッシュ位置戻端検出(8/26中村)
2666     If MRtn = 0 Then
2667         fErrorProcess(11,234,284,0)
2668         Select M_20#
2669             Case MAbout%                    '停止が押された場合
2670                 InitialState = 1
2671                 Break
2672             Case MNgProcess%
2673                 InitialState = 1
2674                 Break
2675             Case MContinue%                 'リトライが押された場合
2676                 M_20# = MClear%
2677                 InitialState = 0
2678                 Break
2679             Case MNext%                     '次へが押された場合
2680                 M_20# = MClear%
2681                 InitialState = 0
2682                 Break
2683         End Select
2684     EndIf
2685     *RETRY_POSITIONING_RESTORE
2686     '
2687     M_Out(12262) = 0
2688     M_Out(12263)=1 Dly 0.3                  '位置決め解除
2689     'Wait M_In(11274)=1                      '位置決め戻端検出(修正につきコメントアウト(8/26中村))
2690     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)   '位置決め戻端検出(8/26中村)
2691     If MRtn = 0 Then
2692         fErrorProcess(11,234,284,0)
2693         Select M_20#
2694             Case MAbout%                    '停止が押された場合
2695                 InitialState = 1
2696                 Break
2697             Case MNgProcess%
2698                 InitialState = 1
2699                 Break
2700             Case MContinue%                 'リトライが押された場合
2701                 M_20# = MClear%
2702                 InitialState = 0
2703                 Break
2704             Case MNext%                     '次へが押された場合
2705                 M_20# = MClear%
2706                 InitialState = 0
2707                 Break
2708         End Select
2709     EndIf
2710     Exit Function
2711 FEnd
2712 '
2713 '■fnTorqueCheck
2714 ''' <summary>
2715 ''' トルクチェック動作用のメイン
2716 ''' </summary>
2717 ''' <remarks>
2718 ''' Date   : 2021/12/21 : H.AJI
2719 ''' </remarks>'
2720 Function M% fnTorqueCheck
2721     'トルクチェック中送信  搬送系停止
2722     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
2723     '
2724     fnTorqueCheck = 0
2725     Ovrd 20
2726     Mov PInitialPosition              '初期位置移動
2727     Ovrd 100
2728     '下記キー待ちの継続に反応させないため
2729     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2730     Dly 0.2
2731     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2732     '
2733     'M6340  トルクチェック受信
2734     'Dly 5.0
2735     M_Out(12340) = 1          'トルクチェック受信 M6340
2736     Dly 1.0
2737     M_Out(12340) = 0
2738     '
2739     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
2740     '
2741     MLoopFlg = 1
2742     While MLoopFlg = 1
2743         '
2744         Mov PInitialPosition              '初期位置移動
2745         '
2746         MKeyNumber = fnKEY_WAIT()
2747         Select MKeyNumber
2748             Case Is = 1           '停止
2749                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
2750                 Dly 1.0
2751                 M_Out(12343) = 0
2752                 Ovrd 20
2753                 Mov PTicketRead_1
2754                 Ovrd 100
2755                 M_20# = 1
2756                 MLoopFlg = -1
2757                 Break
2758             Case Is = 2           '次へ
2759                 Break
2760             Case Is = 3           '継続
2761                 Break
2762             Case Is = 4           'トルクチェック開始
2763                 M_Out(12545) = 1    ' toPLC_PCトルクチェック1要求受信(M315)
2764                 M_Out(12342) = 1 Dly 1.0    'トルクチェック開始要求受信 M6342
2765                 fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
2766                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
2767                 MRet = fnMoveTorquePosi()
2768                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
2769                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2770                 Break
2771             Default
2772                 Break
2773         End Select
2774     WEnd
2775     '
2776     'トルクチェック中停止送信
2777     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
2778     '
2779     'ロボットの位置を元に戻す
2780     '
2781     Exit Function
2782  FEnd
2783  '
2784 '
2785 '
2786 '---------------------------
2787 '
2788 '    メイン画面の表示、非表示設定
2789 '         コメントD1001, D1002, D1003の設定
2790 '           MWindReSet = 0     画面非表示
2791 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
2792 '           MWindErrScr = 10    エラー画面 D1001, D1002
2793 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2794 '
2795 '---------------------------
2796 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2797     fnMainScreenOpen = 0
2798     '
2799    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2800         M_Out16(12480) = MCommentD1001            'D1001 コメント
2801     EndIf
2802     '
2803     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2804         M_Out16(12496) = MCommentD1002            'D1002 コメント
2805     EndIf
2806     '
2807     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2808         M_Out16(12512) = MCommentD1003            'D1003 コメント
2809     EndIf
2810     '
2811     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2812     M_Out(12362) = 1                         'ウィンド画面設定  M6362
2813     Dly 0.5
2814     M_Out(12362) = 0                         'ウィンド画面設定
2815     Exit Function
2816 FEnd
2817 '
2818 '■Main
2819 ''' <summary>
2820 ''' トルクチェック実動作
2821 ''' </summary>
2822 ''' <remarks>
2823 ''' Date   : 2021/12/21 : H.AJI
2824 ''' </remarks>'
2825 Function M% fnMoveTorquePosi
2826      fnMoveTorquePosi = 0
2827      Ovrd 50
2828      Mov PTorqueCheck_1 'トルクチェックメーター上空へ移動
2829     '
2830     Spd M_NSpd
2831 '-------------      ドライバーRST
2832     M_Out(12240)=0     'ドライバーOFF CCW
2833     M_Out(12241)=0     'ドライバーOFF CW
2834     M_Out(12242)=1     'ドライバー解除 C1
2835     M_Out(12243)=1     'ドライバー解除 C2
2836     M_Out(12245)=0     'プログラム解除 F1/プログラム2
2837 '---------------------------------------
2838 '[P-11]
2839 '--------------------------------------------------------------   【トルクチェック 0.4N - P11】
2840     Mov PTorqueCheck, -50                     ' トルク-1　置き位置上空 50mm へ移動
2841     Dly 0.1
2842 '-----------------------
2843    'Cnt 0                           'Cnt動作-2　終了
2844 '-----------------------
2845     Mov PTorqueCheck , -5                      'トルク-1　置き位置上空 5mm へ移動
2846     Dly 0.2
2847 '-----------------------
2848     ProgramBankSet(1,3)
2849     M_Out(12241)=0                   'ドライバーOFF  CW
2850     'Dly 0.1
2851 '--------------------------------
2852     Ovrd 40
2853    'Dly 0.1
2854 '--------------------------------  ネジ締め速度設定
2855     Spd 14                            'ライド 100-40 100% :Spd 12
2856     Dly 0.1
2857 '--------------------------------
2858 '--------------------------------
2859 '---------------------------------【ねじ締め動作】
2860 '
2861     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '移動中エラー検出
2862    Mvs PTorqueCheck               'トルクチェック位置へ移動
2863     Dly 0.3                          '動作安定待ち
2864    M_Out(12241)=1                   'ドライバーON  CW
2865 '
2866     Wait M_In(11584)=1                '完了/エラー検出
2867     Dly 0.1
2868     Spd M_NSpd
2869    'Ovrd 20
2870     If M_In(11256)=1 Then *LBL1       'ネジトータルエラー検出
2871     Wait M_In(11257)=1                'ネジ完了SC
2872 '---------------------------------
2873     Dly 0.1
2874     M_Out(12241)=0                    'ドライバーOFF CW
2875     Dly 0.1
2876     M_Out(12242)=0                    'ドライバー解除 C1
2877     Dly 0.1
2878     M_Out(12243)=0                    'ドライバー解除 C2 (バンク3)
2879     Dly 0.1
2880     M_Out(12245)=0                    'プログラム2解除 F1
2881 '--------------------------------------------------------------   【トルクチェック 0.4N - P11ここまで】
2882 '
2883     Mvs PTorqueCheck,-60                       'あえてmov から変更
2884     Dly 0.1
2885 '--------------------------------------------------------------
2886    'Ovrd 80
2887 '--------------------------------------------------------------
2888 '---------------------------------------
2889 '---------------------------------------
2890 '---------------------------------------エラー離脱処理
2891    *LBL1
2892    Fsc Off            '力覚センサ　Off   *STEP1は不要
2893    Mvs ,-100
2894    M_Out(12241)=0     'ドライバーOFF CW
2895    Dly 0.1
2896    M_Out(12242)=0     'ドライバー解除 C1
2897    Dly 0.1
2898    M_Out(12243)=0     'ドライバー解除 C2 (バンク3)
2899    Dly 0.1
2900    M_Out(12245)=0     'プログラム解除 F1
2901 '---------------------------------------
2902 '---------------------------------------
2903 '-------------
2904    'Mov PInitPos19049
2905    Dly 0.1
2906 '
2907 '
2908     Exit Function
2909 FEnd
2910 '
2911 '■Main
2912 ''' <summary>
2913 ''' 組立動作用のメイン
2914 ''' </summary>
2915 ''' <remarks>
2916 ''' Date   : 2021/07/07 : M.Hayakawa
2917 ''' </remarks>'
2918 Function Main
2919     MopeNo = M_21#         '外部変数にて動作番号代入
2920     '
2921     If M_Svo=0 Then
2922         Servo On
2923     EndIf
2924     Wait M_Svo=1
2925 '組立スタート日付時刻要求パルスON
2926     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
2927 'パトライト操作
2928     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
2929     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
2930     '
2931     M_20# = 0                                   'KEY入力初期化
2932     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
2933     MRet% = 0
2934 '復帰動作　実行・未実行判別      2022/03/22 渡辺 作成
2935     PActive = P_Curr                    '現在位置を取得
2936     MRecoveryPass% = 0
2937     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
2938         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
2939             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
2940             MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
2941         EndIf
2942     EndIf
2943     EndIf
2944     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
2945         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
2946             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
2947                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
2948             EndIf
2949         EndIf
2950     EndIf
2951     If MRecoveryPass% = 0 Then
2952         fnInitialZone()        '復帰動作パスフラグが立っていない時は復帰動作を実行
2953     EndIf
2954     '
2955     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
2956         M_Out(12364) = 1            'toPLC_データ保存ON
2957 'トルクチェック
2958         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
2959             MRet% = fnTorqueCheck()
2960             Break
2961         Else
2962 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
2963 '                MRtn = InspInit()               '画像処理初期化処理
2964 '            EndIf
2965             '
2966            M_20# = MClear%                    '初期化
2967 '組立開始
2968             If M_In(MIN_ASSY_CANCEL%) = 0 Then
2969                 MRet% = fnAssyStart()
2970             Else
2971                 M_20# = MPass%
2972             EndIf
2973 '組立終了日付時刻
2974             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
2975             Wait M_In(11572) = 1            '日付取得完了
2976             Dly 0.1
2977             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
2978 'リフターユニットへのOUT
2979             '  KEY入力が何もない場合 OKと判断
2980             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
2981             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
2982 'OK/NGフラグ出力
2983             If M_20# <= 0 Then
2984                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
2985             ElseIf M_20# = MPass% Then
2986                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
2987             EndIf
2988 'PIASに組立完了書込み
2989             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
2990                 If M_20# = MPass% Then
2991                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
2992                 Else
2993                     'KEY入力がNGの場合
2994                     If M_20# = MNgProcess% Then
2995                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
2996                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
2997                         MRet% = fnPiasWrite(MNG%)
2998                        nAssyNgQty = nAssyNgQty + 1
2999                     EndIf
3000                     '
3001                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/07中村)
3002                     If M_20# = MAssyOK% Then
3003                             '-----------------------
3004                             'D732 -> D2600 コピー要求
3005                             M_Out(12566) = 1
3006 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3007                             M_Out(12566) = 0
3008                             '
3009                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3010                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3011                             '基板番号照合(PPは未使用）
3012 '                            MRet% = fnPCBNumberCheck()
3013                         Else
3014                             MRet% = 1
3015                         EndIf
3016                         '
3017                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3018                             If M_20# <> MAbout% Then
3019                                 '工程履歴OK書き込み
3020                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3021                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3022                                 MRet% = fnPiasWrite(MOK%)
3023                                 nAssyOkQty = 0
3024                                 nAssyOkQty = nAssyOkQty + 1
3025                             Else
3026                                 nAssyOkQty = nAssyOkQty + 1
3027                             EndIf
3028                         EndIf
3029                     EndIf
3030 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3031 '                    MRet% = fnPiasWrite(MOK%)
3032                 EndIf
3033             Else
3034                 nAssyOkQty = nAssyOkQty + 1
3035             EndIf
3036             '
3037             '組立終了日付時刻解除
3038             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3039             '投入数、組立OK数、組立NG数書込み
3040 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3041             '
3042 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3043 '                '画像処理終了処理
3044 '                MRtn = InspQuit()
3045 '            EndIf
3046         EndIf
3047         M_Out(12364) = 0                          'toPLC_データ保存OFF
3048     EndIf
3049 'パトライト操作
3050     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3051     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3052 'GOT表示
3053     fnAutoScreenComment(93)  'AUTO画面 工程完了
3054 FEnd
3055 End
3056 '
3057 'おまじないコメント
3058 '絶対削除するな
3059 '
3060 '
3061 '
3062 '
PInspPosition(1)=(+343.72,-16.25,+435.00,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PInspPosition(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
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
PTemp=(+603.00,-149.18,+450.00,-179.99,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPos(1)=(+320.73,-172.58,+395.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPos(2)=(+320.73,-172.58,+335.34,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(10)=(+320.73,-172.58,+329.34,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PGetScrewPos(1)=(+180.56,+239.41,+380.00,+180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
PGetScrewPos(2)=(+182.97,+239.67,+400.00,+180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
PGetScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(9)=(+78.32,+270.81,+429.99,-180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
PGetScrewPos(10)=(+180.56,+239.41,+338.63,+180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
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
PActive=(+603.00,-149.18,+450.00,-179.99,+0.00,+90.00,+0.00,+0.00)(7,0)
Pmove=(+370.97,-153.62,+500.00,-179.97,-0.01,+89.99,+0.00,+0.00)(7,0)
PInitialPosition=(+250.00,+0.00,+450.00,+180.00,+0.00,+180.00)(7,0)
PScrewSoc1=(+299.62,-64.99,+329.08,-180.00,+0.00,+90.00)(7,0)
PScrewSoc1_0=(+299.62,-64.99,+335.08,-180.00,+0.00,+90.00)(7,0)
PScrewSoc1_1=(+299.62,-64.99,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc2=(+320.95,-24.67,+329.23,-180.00,+0.00,+90.00)(7,0)
PScrewSoc2_0=(+320.95,-24.67,+335.23,-180.00,+0.00,+90.00)(7,0)
PScrewSoc2_1=(+320.95,-24.67,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc3=(+381.00,-24.88,+330.40,-180.00,+0.00,+90.00)(7,0)
PScrewSoc3_0=(+381.00,-24.88,+336.40,-180.00,+0.00,+90.00)(7,0)
PScrewSoc3_1=(+381.00,-24.88,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc4=(+391.22,-85.80,+329.84,-180.00,+0.00,+90.00)(7,0)
PScrewSoc4_0=(+391.22,-85.80,+335.84,-180.00,+0.00,+90.00)(7,0)
PScrewSoc4_1=(+391.22,-85.80,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc5=(+371.00,-153.62,+329.34,-180.00,+0.00,+90.00)(7,0)
PScrewSoc5_0=(+371.00,-153.62,+335.34,-180.00,+0.00,+90.00)(7,0)
PScrewSoc5_1=(+371.00,-153.62,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc6=(+320.73,-172.58,+329.34,-180.00,+0.00,+90.00)(7,0)
PScrewSoc6_0=(+320.73,-172.58,+335.34,-180.00,+0.00,+90.00)(7,0)
PScrewSoc6_1=(+320.73,-172.58,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSupply=(+180.56,+239.41,+338.63,+180.00,+0.00,-120.00)(7,0)
PScrewSupply_1=(+180.56,+239.41,+380.00,+180.00,+0.00,-120.00)(7,0)
PScrewSupply_2=(+182.97,+239.67,+400.00,+180.00,+0.00,+180.00)(7,0)
PScrewSupply_9=(+78.32,+270.81,+429.99,-180.00,+0.00,-120.00)(7,0)
PSocCheck=(+325.25,-60.87,+444.00,+180.00,-0.01,-180.00)(7,0)
PSocCheck_1=(+325.25,-60.87,+470.00,+180.00,-0.01,-180.00)(7,0)
PSocGet=(+627.82,+106.92,+311.65,-179.93,+0.04,-179.12)(7,0)
PSocGet_1=(+627.82,+106.92,+330.00,-179.93,+0.04,-179.12)(7,0)
PSocGet_2=(+627.82,+106.92,+380.00,-179.93,+0.04,-179.12)(7,0)
PSocPcbRead=(+343.72,-16.25,+435.00,-180.00,+0.00,-180.00)(7,0)
PSocPcbRead_1=(+343.72,-16.25,+480.00,-180.00,+0.00,-180.00)(7,0)
PSocPress=(+391.84,+11.73,+354.93,-180.00,-0.01,+180.00)(7,0)
PSocPress_1=(+391.84,+11.73,+369.00,-180.00,+0.00,+180.00)(7,0)
PSocPress_2=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PSocSet=(+486.15,-97.80,+349.50,+180.00,-0.04,-179.20)(7,0)
PSocSet_1=(+486.15,-97.80,+361.91,+180.00,-0.04,-179.20)(7,0)
PSocSet_2=(+486.15,-97.80,+380.00,+180.00,-0.04,-179.20)(7,0)
PTicketRead=(+603.00,-149.18,+373.00,-179.99,+0.00,+90.00)(7,0)
PTicketRead_1=(+603.00,-149.18,+450.00,-179.99,+0.00,+90.00)(7,0)
PTorqueCheck=(+144.46,-240.78,+340.00,-179.99,-0.01,+90.02)(7,0)
PTorqueCheck_1=(+144.45,-240.80,+360.00,-179.99,+0.00,+90.01)(7,0)
