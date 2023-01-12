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
406 '---------------------------------------------------------------
407     '圧力設定(低圧)22/08/09中村
408     M_Out(12266) = 1
409     M_Out(12267) = 0
410 '---------------------------------------------------------------
411 '
412 '
413     MRtn = 1                        'MRtn初期化
414     *RE_TICKET_READ
415 '    MRtn = fnPiasCheck()               'ID読み取り
416 '    PInspPosition(1) = PTicketRead  'IDチケット読取位置
417 '    MInspGroup%(1) = 1              '検査G番号
418 '    MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
419     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
420         MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
421         '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
422         '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
423     EndIf
424     If MRtn = 1 Then GoTo *CompRead
425     Mvs PTicketRead_1                       ' 一旦上空に待避 追加 22/07/16 M,H
426 '    fErrorProcess(11,111,254,0)
427     If M_20# = MPass% Then GoTo *AssyEnd    ' 次へを押された時のコメント解除＋ジャンプ先変更 2022/07/20 M.H
428     If M_20# = MNext% Then M_20# = MClear%
429     If M_20# = MAbout% Then GoTo *AssyEnd       ' コメント解除＋ジャンプ先変更 22/07/16 M,H
430     If M_20# = MNgProcess% Then GoTo *AssyEnd   ' コメント解除＋ジャンプ先変更 22/07/16 M,H
431     If M_20# = MContinue% Then GoTo *RE_TICKET_READ
432     GoTo *ASSY_ERROR_END
433     *CompRead
434     '
435     *INITIAL_CHECK
436     'ハンドの状態をイニシャルに戻す
437     MRtn =frInCheck(11264,0,MSETTIMEOUT05&) 'PCB検出(あるとエラー)
438     If MRtn = 1 Then GoTo *CompCheck_1
439     fErrorProcess(11,0,281,0)
440     If M_20# = MNext% Then M_20# = MClear%
441     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
442     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
443     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
444     *CompCheck_1
445     '
446     If M_In(11266) = 1 Then
447         M_Out(12256) = 0        'PCBチャック開OFF
448         M_Out(12257) = 1        'PCBチャック閉ON
449         Break
450     EndIf
451     If M_In(11268) = 1 Then
452         M_Out(12258) = 0        'PCBシリンダー出OFF
453         M_Out(12259) = 1        'PCBシリンダー戻ON
454         Break
455     EndIf
456     If M_In(11270) = 1 Then
457         M_Out(12260) = 0        'BtoBシリンダー出OFF
458         M_Out(12261) = 1        'BtoBシリンダー戻ON
459         Break
460     EndIf
461     '
462     MRtn =frInCheck(11265,1,MSETTIMEOUT05&) 'PCBチャック閉検出
463     If MRtn = 1 Then GoTo *CompCheck_2
464     fErrorProcess(11,240,281,0)
465     If M_20# = MNext% Then M_20# = MClear%
466     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
467     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
468     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
469     *CompCheck_2
470     '
471     MRtn =frInCheck(11267,1,MSETTIMEOUT05&) 'PCBシリンダー戻検出
472      If MRtn = 1 Then GoTo *CompCheck_3
473     fErrorProcess(11,239,281,0)
474     If M_20# = MNext% Then M_20# = MClear%
475     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
476     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
477     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
478     *CompCheck_3
479     '
480     MRtn =frInCheck(11269,1,MSETTIMEOUT05&) 'BtoBシリンダー戻検出
481     If MRtn = 1 Then GoTo *CompCheck_4
482     fErrorProcess(11,243,281,0)
483     If M_20# = MNext% Then M_20# = MClear%
484     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
485     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
486     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
487     *CompCheck_4
488     '
489     '製品位置決め
490     *RE_POSITIONING        '位置決めリトライ用
491     M_Out(12262)=1 Dly 0.3      '位置決めパルス信号
492     'Wait M_In(11273)=1          '位置決め出端検出(修正につきコメントアウト(8/26中村))
493     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)    '位置決め出端検出(8/26中村)
494     If MRtn = 1 Then GoTo *CompPosition_1
495     fErrorProcess(11,231,282,0)
496     If M_20# = MNext% Then M_20# = MClear%
497     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
498     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
499     If M_20# = MContinue% Then GoTo *RE_POSITIONING
500     *CompPosition_1
501     '
502     Dly 0.5
503     M_Out(12264)=1 Dly 0.3      'プッシュパルス信号
504     'Wait M_In(11275)=1          'プッシュ出端検出(修正につきコメントアウト(8/26中村))
505     MRtn = frInCheck(11275,1,MSETTIMEOUT05&)    'プッシュ位置出端検出(8/26中村)
506     If MRtn = 1 Then GoTo *CompPosition_2
507     fErrorProcess(11,232,282,0)
508     If M_20# = MNext% Then M_20# = MClear%
509     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
510     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
511     If M_20# = MContinue% Then GoTo *RE_POSITIONING
512     *CompPosition_2
513     '
514 '    '【SOC基板ID読み込み】
515 '    *RE_SOC_CHECK1
516 '    PInspPosition(1) = PSocPcbRead  'SOC基板ID読取位置
517 '    MInspGroup%(1) = 2              '検査G番号
518 '    MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
519 '    If MRtn = 1 Then
520 '        M_Out(12571) = 1        '基板番号コピー1ON
521 '        Dly 0.3
522 '        M_Out(12566) = 1        '基板番号コピー要求ON
523 '        Wait M_In(11581) = 1    '基板番号コピー完了
524 '        M_Out(12571) = 0        '基板番号コピー1OFF
525 '        M_Out(12566) = 0        '基板番号コピー要求OFF
526 '        Dly 0.3
527 '        M_Out(12557)= 1 Dly 0.3         ' 基板番号照合ビットON
528 '    EndIf
529 '    If MRtn = 1 Then GoTo *CompSocCheck1
530 '    fErrorProcess(11,97,25,0)
531 '    If M_20# = MNext% Then M_20# = MClear%
532 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
533 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
534 '    If M_20# = MContinue% Then GoTo *RE_SOC_CHECK1
535 '    *CompSocCheck1
536 '    'SOC基板IDを読む
537 ''    Mov PSocPcbRead             'ID読み位置
538 '    '
539     'SOC基板を取る
540     *RE_GET_SOC
541     '
542     Mov PSocGet_2               '基板ピックアップ回避点  Y:変更 107.160→106.160
543     M_Out(12256)=0              '基板チャック開OFF
544     M_Out(12257)=1              '基板チャック閉ON
545     '
546     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    'チャック閉センサーON
547     If MRtn = 1 Then GoTo *CompGetSOC_1
548     fErrorProcess(11,240,284,0)
549     If M_20# = MNext% Then M_20# = MClear%
550     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
551     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
552     If M_20# = MContinue% Then GoTo *RE_GET_SOC
553     *CompGetSOC_1
554     '
555     M_Out(12259)=0              'PCBシリンダー戻OFF
556     M_Out(12258)=1              'PCBシリンダー出ON
557     Dly 0.2
558     '
559 '    Wait M_In(11268)=1          'PCBシリンダー出端センサーON
560     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)
561     If MRtn = 1 Then GoTo *CompGetSOC_2
562     fErrorProcess(11,238,284,0)
563     If M_20# = MNext% Then M_20# = MClear%
564     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
565     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
566     If M_20# = MContinue% Then GoTo *RE_GET_SOC
567     *CompGetSOC_2
568     '
569     Mov PSocGet_1               '基板上空 Y:変更 107.160→106.160
570     Ovrd 40
571     Mvs PSocGet                 '基板ピックアップ位置  Y:変更 107.170→106.170
572     Dly 0.3
573     Ovrd 5                      '2021-12-19追加 AJ
574     '
575     '
576 '    Wait M_In(11264)=1          'PCB検出センサーON
577     MRtn = frInCheck(11264 , 1 , MSETTIMEOUT05&)
578     If MRtn = 1 Then GoTo *CompGetSOC_3
579     fErrorProcess(11,0,284,0)
580     If M_20# = MNext% Then M_20# = MClear%
581     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
582     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
583     If M_20# = MContinue% Then GoTo *RE_GET_SOC
584     *CompGetSOC_3
585     '
586     M_Out(12257)=0              '基板チャック閉OFF
587     M_Out(12256)=1              '基板チャック開ON
588     Dly 0.2                     '把持力確保用ディレイ(22/09/30中村)
589     '
590 '    Wait M_In(11266)=1          'チャック開センサーON
591     MRtn = frInCheck(11266 , 1 , MSETTIMEOUT05&)
592     Mvs PSocGet_1               '基板上空  Y:変更 107.160→106.160
593     If MRtn = 1 Then GoTo *CompGetSOC_4
594     Mov PSocGet_2               ' Y:変更 107.160→106.160
595     fErrorProcess(11,241,284,0)
596     If M_20# = MNext% Then M_20# = MClear%
597     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
598     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
599     If M_20# = MContinue% Then GoTo *RE_GET_SOC
600     *CompGetSOC_4
601     '
602     'Wait M_In(11264)=1          'PCB検出センサーON
603     '
604     '下記、PCB判定とエラー処理追加 2021-12-19 AJ
605     MRtn = frInCheck(11264 , 1 , MSETTIMEOUT05&)
606     If MRtn = 1 Then GoTo *CompGetSOC_41
607     fErrorProcess(11,0,284,0)
608     If M_20# = MNext% Then M_20# = MClear%
609     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
610     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
611     If M_20# = MContinue% Then GoTo *RE_GET_SOC
612     *CompGetSOC_41
613     '
614     '
615     'Ovrd 100                   '位置変更2021-12-19追加AJ
616     Ovrd 15                     '追加1/17中村
617     Mov PSocGet_2               '基板ピックアップ回避点
618     Ovrd 100                    '2021-12-19追加AJ
619     '
620     'SOC基板を製品上に置く
621     Mov PSocSet_2               '基板置き回避点
622     Mov PSocSet_1               '製品上空
623     Dly 0.1
624     Ovrd 40
625     Mvs PSocSet                 '基板置き位置（空中で離す）
626     M_Out(12256)=0              '基板チャック開OFF
627     M_Out(12257)=1              '基板チャック閉ON
628 '
629     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
630     Mvs PSocSet_1               '製品上空
631     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
632 '
633     'Wait M_In(11265)=1          'チャック閉センサーON
634     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)
635     If MRtn = 1 Then GoTo *CompGetSOC_5
636     fErrorProcess(11,240,284,0)
637     If M_20# = MNext% Then M_20# = MClear%
638     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
639     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
640     If M_20# = MContinue% Then GoTo *RE_GET_SOC
641     *CompGetSOC_5
642     '
643 '    Wait M_In(11268)=1          'PCBシリンダー出端センサーON・・・もしOFFだったら基板挿入を失敗している
644     MRtn = frInCheck(11268 , 1 , MSETTIMEOUT05&)
645     If MRtn = 1 Then GoTo *CompGetSOC_6
646     fErrorProcess(11,0,284,0)
647     If M_20# = MNext% Then M_20# = MClear%
648     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
649     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
650     If M_20# = MContinue% Then GoTo *RE_GET_SOC
651     *CompGetSOC_6
652     '
653     'Wait M_In(11264)=0          'PCB検出センサーOFF
654     M_Out(12258)=0              'PCBシリンダー出OFF
655     M_Out(12259)=1              'PCBシリンダー戻ON
656     '
657 '    Wait M_In(11267)=1          'PCBシリンダー戻端センサーON
658     MRtn = frInCheck(11267 , 1 , MSETTIMEOUT05&)
659     If MRtn = 1 Then GoTo *CompGetSOC_7
660     fErrorProcess(11,239,284,0)
661     If M_20# = MNext% Then M_20# = MClear%
662     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
663     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
664     If M_20# = MContinue% Then GoTo *RE_GET_SOC
665     *CompGetSOC_7
666     Ovrd 100
667     Mov PSocSet_2               '基板置き回避点
668 '【SOC基板ID読み込み】
669     *RE_SOC_CHECK1
670     PInspPosition(1) = PSocPcbRead  'SOC基板ID読取位置
671     MInspGroup%(1) = 2              '検査G番号
672     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
673 '
674     If MRtn = 1 Then GoTo *CompSocCheck1
675     fErrorProcess(11,97,25,0)
676     If M_20# = MNext% Then M_20# = MClear%
677     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
678     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
679     If M_20# = MContinue% Then GoTo *RE_SOC_CHECK1
680     *CompSocCheck1
681 '【基板IDコピー】
682     *RE_PCB_RECORD
683     M_Out(12571) = 1    ' 領域1 基板番号コピー (D2600-) On
684     Dly 0.1
685     M_Out(12566) = 1    ' toPLC_基板番号コピー要求 On
686 '
687     MRtn = frInCheck(11581,1,MSETTIMEOUT05&)    ' toRBT_基板番号コピー完了 On
688     If MRtn = 1 Then
689         M_Out(12571) = 0  ' 領域1 基板番号コピー (D2600-) Off
690         Dly 0.1
691         M_Out(12566) = 0  ' toPLC_基板番号コピー要求 Off
692 '        GoTo *RE_PCB_COMPAIRE   ' 基板番号照合にスキップ
693     Else
694         fErrorProcess(11,39,25,0)
695         If M_20# = MNext% Then M_20# = MClear%
696         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
697         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
698         If M_20# = MContinue% Then GoTo *RE_PCB_RECORD
699     EndIf
700 '【基板ID照合（紐付け）】
701     MRetryCount% = 0
702     While (MRetryCount% <= MRetryLimit%)
703         *RE_PCB_COMPAIRE
704         M_Out(12557)= 1 ' 基板番号照合ビットON
705         MRtn = frInCheck(11566,1,MSETTIMEOUT05&)    ' toRBT_基板番号照合OK(M420) On
706         If MRtn = 1 Then
707             M_Out(12557)= 0     ' 基板番号照合ビットOff
708             ' リトライ回数設定でループを抜ける
709             MRetryCount% = 99
710         Else
711             If MRetryCount% = MRetryLimit% Then
712                 If M_In(11565) = 1 Then
713                     fErrorProcess(11,37,25,0)
714                 Else
715                     fErrorProcess(11,38,25,0)
716                 EndIf
717                 If M_20# = MNext% Then
718                     M_20# = MClear%
719                     ' リトライ回数設定でループを抜ける
720                     MRetryCount% = 99
721                 EndIf
722                 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
723                 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
724                 If M_20# = MContinue% Then
725                     MRetryCount% = 0
726                 EndIf
727             Else
728                 ' リトライ回数インクリメント
729                 MRetryCount% = MRetryCount% + 1
730                 Dly 0.5 ' 他の工程とタイミングをずらす為のディレイ
731             EndIf
732         EndIf
733     WEnd
734 '【Soc基板画像チェック】
735 '    *RE_SOC_CHECK2
736 '    PInspPosition(1) = PSocCheck    'Soc基板画像チェック位置
737 '    MInspGroup%(1) = 3              '検査G番号
738 '    MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
739 '    If MRtn = 1 Then GoTo *CompSocCheck2
740 '    fErrorProcess(11,43,46,0)
741 '    If M_20# = MNext% Then M_20# = MClear%
742 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
743 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
744 '    If M_20# = MContinue% Then GoTo *RE_SOC_CHECK2
745 '    *CompSocCheck2
746     '基板置き位置画像検査（不要？）
747     '
748     'SOC基板BtoBプレス
749     'Mov PSocPress_2             'BtoBプレス回避点
750     *RE_BtoBPRESS   'BtoBシリンダーリトライ
751     Mov PSocPress_1             'プレス上空
752     M_Out(12261)=0              'プレスシリンダー戻OFF
753     M_Out(12260)=1              'プレスシリンダー出ON
754     Dly 0.2
755     '
756     'Wait M_In(11270)=1          'プレスシリンダー出端センサーON(修正につきコメントアウト(8/27中村))
757     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)    'プレスシリンダー出端センサーON(8/27中村)
758     If MRtn = 1 Then GoTo *CompPress_1
759     fErrorProcess(11,242,284,0)
760     If M_20# = MNext% Then M_20# = MClear%
761     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
762     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
763     If M_20# = MContinue% Then GoTo *RE_BtoBPRESS
764     *CompPress_1
765 '
766 '--------------------------------------------
767     '圧力検出(低圧力)
768     '22/07/29追加 中村
769 '--------------------------------------------
770 *RE_Pa_OUT
771     If M_20# = MContinue% Then
772     M_Out(12266) = 1
773     M_Out(12267) = 0
774     Dly 0.5
775     M_20# = MClear%
776     EndIf
777     MRtn = frInCheck(11277,1,MSETTIMEOUT05&)     'MDV用圧力検出(22/07/29中村)
778     MRtn2 = frInCheck(11278,0,MSETTIMEOUT05&)    'KA用圧力検出(ONで上がりすぎ)(22/07/29中村)
779     If MRtn = 1 And MRtn2 = 1 Then GoTo *CompPaOut
780     If MRtn = 0 Then
781         fErrorProcess(11,200,201,0)
782     ElseIf MRtn2 = 0 Then
783         fErrorProcess(11,200,201,0)
784     EndIf
785     If M_20# = MNext% Then M_20# = MClear%
786     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
787     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
788     If M_20# = MContinue% Then GoTo *RE_Pa_OUT
789 *CompPaOut
790     '
791     '
792     Ovrd 40
793     Mvs PSocPress               'プレスエンド端まで移動
794     Dly 0.5
795     'Wait M_In(11270)=1          'プレスシリンダー出端センサーON…もしOFFだったらコネクタカバー有
796     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)    'プレスシリンダー出端センサーON(8/27中村)
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
1561     Mvs PScrewPosition(1)       ' パレット上ねじ締めS①の安全回避位置
1562     Select MScrewType%      '読み込み位置変更(1/19中村)
1563         Case 1
1564             ' Sタイト：プログラム1、バンク1に設定
1565             ProgramBankSet(1,1)
1566             Break
1567         Case 2
1568             ' Pタイト：プログラム1、バンク1に設定
1569             ProgramBankSet(3,1)
1570             Break
1571         Case 3
1572             ' Sタイト黒：プログラム1、バンク1に設定
1573             ProgramBankSet(1,1)
1574             Break
1575         Case 4
1576             ' Sタイト13mm：プログラム1、バンク1に設定
1577             ProgramBankSet(1,1)
1578             Break
1579         Case 5
1580             ' Mネジ：プログラム1、バンク1に設定
1581             ProgramBankSet(1,1)
1582             Break
1583         Default
1584             ' プログラム1、バンクなし設定
1585             ProgramBankSet(0,0)
1586             Break
1587     End Select
1588     Fine 0.05 , P
1589     Ovrd MOvrdA%
1590     ' 減速を10に設定
1591     Accel 100,10
1592     ' パレット上ねじ締め開始位置へ移動
1593     Mvs PScrewPosition(2)
1594     ' 加減速を元に戻す
1595     Accel
1596     ' 内部Ovrd設定
1597 '    Ovrd MOvrdA%
1598     Ovrd 100
1599     ' Spd設定
1600 '    Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
1601     Spd MFeedSpd
1602     ' 設定進み量5.0 × 操作パネルのオーバーライド係数 × プログラム内オーバーライド係数
1603     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
1604 '    Select MScrewType%      '読み込み位置変更(1/19中村)
1605 '        Case 1
1606 '            ' Sタイト：プログラム1、バンク1に設定
1607 '            ProgramBankSet(1,1)
1608 '            Break
1609 '        Case 2
1610 '            ' Pタイト：プログラム1、バンク1に設定
1611 '            ProgramBankSet(3,1)
1612 '            Break
1613 '        Case 3
1614 '            ' Sタイト黒：プログラム1、バンク1に設定
1615 '            ProgramBankSet(1,1)
1616 '            Break
1617 '        Case 4
1618 '            ' Sタイト13mm：プログラム1、バンク1に設定
1619 '            ProgramBankSet(1,1)
1620 '            Break
1621 '        Case 5
1622 '            ' Mネジ：プログラム1、バンク1に設定
1623 '            ProgramBankSet(1,1)
1624 '            Break
1625 '        Default
1626 '            ' プログラム1、バンクなし設定
1627 '            ProgramBankSet(0,0)
1628 '            Break
1629 '    End Select
1630 '
1631 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     'ドライバーON　CW
1632      'ドライバーON　CW
1633     M_Out(12241)=1
1634     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1635     Wait M_In(11584)=1          '完了/エラー検出 暫定コメント 10/6 M.H
1636     Dly 0.1
1637     Fine 0 , P
1638     Spd M_NSpd
1639     '
1640     If M_In(11256)=1 Then  'ねじトータルエラー検出時
1641         M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1642         Dly 0.1
1643        ' プログラム・バンク解除
1644         ProgramBankSet(0,0)
1645         'パレット上ねじ締め終了位置上空へ移動
1646         Mvs PScrewPosition(10),-80
1647         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1648         M_Out(12249)=1 Dly 0.3
1649         MOKNGFlg = -1
1650         ScrewTight = 0
1651     Else
1652          'ドライバーOFF　CW
1653         M_Out(12241)=0
1654 ''        エラーがない場合はネジ締め終了位置で増し締め
1655 '        Select MScrewType%
1656 '            Case 1
1657 '                ' Sタイト：プログラム1、バンク1に設定
1658 '                ProgramBankSet(1,3)
1659 '                Break
1660 '            Case 2
1661 '                ' Pタイト：プログラム1、バンク1に設定
1662 '                ProgramBankSet(3,3)
1663 '                Break
1664 '            Case 3
1665 '                ' Sタイト黒：プログラム1、バンク1に設定
1666 '                ProgramBankSet(1,3)
1667 '                Break
1668 '            Case 4
1669 '                ' Sタイト13mm：プログラム1、バンク1に設定
1670 '                ProgramBankSet(1,3)
1671 '                Break
1672 '            Case 5
1673 '                ' Mネジ：プログラム1、バンク1に設定
1674 '                ProgramBankSet(1,3)
1675 '                Break
1676 '            Default
1677 '                ' プログラム1、バンクなし設定
1678 '                ProgramBankSet(0,0)
1679 '                Break
1680 '        End Select
1681 '         'ドライバーON　CW
1682 '        Mvs PScrewPosition(10)
1683 '        M_Out(12241)=1
1684 '        Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1685 '        Fine 0 , P
1686 ''
1687          'ドライバーOFF　CW
1688         M_Out(12241)=0
1689        ' プログラム・バンク解除
1690         ProgramBankSet(0,0)
1691         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1692         M_Out(12249)=1 Dly 0.3
1693     '     ↓PScrewPos(2) → PScrewPosition(10)に変更 9/16 M.Hayakawa
1694         'パレット上ねじ締め終了位置上空へ移動
1695         Mvs PScrewPosition(10),-80
1696         ScrewTight = 1
1697     EndIf
1698 ' 暫定（暫定マスク　9/16 M.Hayakawa)
1699 '    Ovrd 10
1700 '    Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1701     Ovrd 100
1702     Exit Function
1703 FEnd
1704 '
1705 '■ScrewGet
1706 ''' <summary>
1707 ''' ねじ供給機からねじを得る
1708 ''' </summary>
1709 '''<param name="%">
1710 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
1711 '''         PScrewPos(2)    ：ねじ供給器回避点
1712 '''         PScrewPos(9)    ：ねじ供給器上空ネジ捨位置
1713 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1714 '''         PScrewPos(3)    ：Mねじポカヨケ位置
1715 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
1716 '''</param>
1717 '''<param name = FeederReadyNo%> 指定の供給機Ready </param>
1718 '''<param name = FeederScrewSensor%> 指定の誤供給防止センサー指定(0でセンサー無し)</param>
1719 '''<returns>整数
1720 '''         0=異常終了、1=正常終了、-1=ねじ供給NG、-2=ねじ誤供給NG、-3=吸着エラー
1721 '''</returns>
1722 ''' <remarks>
1723 ''' Date   : 2021/07/07 : M.Hayakawa
1724 ''' </remarks>
1725 '''<update>
1726 '''Date    : 2021/11/15 : 中村
1727 '''</update>
1728 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
1729     fnAutoScreenComment(522)    '状態表示[ネジ供給待ち] 2022/05/09 渡辺
1730     ScrewGet = 0
1731     MScrewJudge% = 0
1732     'ねじ供給器初期動作エラーチェック
1733 ' ↓暫定削除
1734     'Mov PScrewPosition(2)   'ねじ供給機回避点へ移動
1735     For MCnt% = 0 To MFinCnt%
1736         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '指定ねじ供給機がReadyになっているか確認(5秒間)
1737         If MRtn = 0 Then
1738             M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
1739             ScrewGet = -1
1740             MScrewJudge% = 2
1741         EndIf
1742         Ovrd 100
1743         If FeederScrewSensor% <> 0 Then
1744             If M_In(FeederScrewSensor%) = 1 Then  '誤供給が検出されたら
1745                 'Ovrd 30
1746                 M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
1747                 'NGとしてここの関数から抜ける
1748                 ScrewGet = -2
1749                 MScrewJudge% = 3
1750             EndIf
1751         EndIf
1752         Ovrd 100
1753         Spd M_NSpd
1754         If MScrewJudge% = 0 Then
1755     '        ScrewGet = 0
1756             M_Out(Y63_Driver)=1         ' バンクセッティング　C2
1757             Dly 0.3
1758             MScrewCnt% = 0
1759             MFinCnt% = 2
1760             fnAutoScreenComment(521)     '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
1761             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1762             'Ovrd 40 '2に変更 10/6 M.H '5に変更10/7中村
1763             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1764             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
1765             'Mvs PScrewPosition(10), 1.2
1766            Mvs PScrewPosition(10)       'Fan用ねじ吸着位置修正のため変更 2022-02-01AJI
1767             'ビット回転(処理位置変更)
1768             M_Out(Y60_Driver)=1
1769             M_Timer(4) = 0
1770             MloopFlg = 0
1771             MCntTime& = 0
1772             While MloopFlg = 0
1773                 MCrtTime& = M_Timer(4)
1774                 If MCrtTime& >= 180 Then
1775                     MloopFlg = 1
1776                 EndIf
1777             WEnd
1778             M_Out(Y68_VV1)=1 Dly 0.3     ' ねじ吸着　ON'Dly 0.3追加(8/27中村)
1779             '吸着確認
1780             MRtn = 0
1781             MRtn = frInCheck(11271, 1, MSETTIMEOUT01&)
1782 '            'ビット回転(処理位置変更)
1783 '            M_Out(Y60_Driver)=1
1784 '            Dly 0.2
1785             '
1786             JOvrd M_NJovrd
1787             Spd M_NSpd
1788             'ネジ吸着確認位置移動
1789             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
1790             Mvs PScrewPosition(10), -30  ' ネジ吸着確認位置
1791            'Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1792             'ビット回転停止
1793             M_Out(Y60_Driver)=0
1794             '
1795 '            If MRtn = 1 Then           '最初の閾値を確認しない
1796                 '1秒間ネジ吸着確認
1797                 MRtn = frInCheck(11272, 1, MSETTIMEOUT01&)
1798 '            EndIf
1799             'MRtn = 0'強制エラー
1800             '吸着エラーの場合
1801             'ネジをねじ太郎に戻す
1802             If MRtn = 0 Then
1803                 Ovrd 30      '2から5に変更
1804                 'ビット回転停止
1805                 M_Out(Y60_Driver)=0
1806                 'ネジ供給機上空
1807                 Mvs PScrewPosition(1)
1808                 '更に上空
1809                 Mov PScrewPosition(1), -140
1810                 'ネジ捨て位置
1811                 MRtn = FnCtlValue2(4)          '吸着エラー数＋１  2022/04/28 渡辺
1812                 Mov PScrewPosition(9)
1813                 MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1814                 '吸着OFF
1815                 M_Out(12249)=1 Dly 0.3 'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1816                 Dly 0.2
1817                 '破壊ON
1818                 M_Out(Y6B_VB1)=1 '真空破壊ON
1819                 'ビット回転
1820                 M_Out(Y61_Driver)=1
1821                 Dly 0.5
1822                 '                '
1823                 Ovrd 100
1824                 JOvrd M_NJovrd
1825                 Spd M_NSpd
1826                 'ドライバーを上下させねじを振り落とす
1827                 Mov PScrewPosition(9), 10
1828                 Mov PScrewPosition(9)
1829                 Dly 0.1
1830                 Mov PScrewPosition(9), 10
1831                 Mov PScrewPosition(9)
1832                 '
1833                 'ネジ落ち待ち
1834                 Wait M_In(11272) = 0
1835                 'ビット回転停止
1836                 M_Out(Y61_Driver)=0
1837                 Dly 0.1
1838                 '破壊OFF
1839                 M_Out(Y6B_VB1)=0 '真空破壊OFF
1840                 'ねじ落ちたとして、移動更に上空
1841                 Mov PScrewPosition(1), -140
1842                 Ovrd 100
1843                 Spd M_NSpd
1844                 'ネジ供給機上空
1845                 Mvs PScrewPosition(1)
1846 '                '
1847                 ScrewGet = -3
1848                 If MCnt% = MFinCnt% Then
1849                     MScrewJudge% = 4
1850                     Mov PScrewPosition(2)
1851                     Break
1852                 EndIf
1853                 Break
1854 '                '
1855             Else
1856                 MCnt% = MFinCnt%
1857                 ScrewGet = 1
1858             EndIf
1859         Else
1860             MCnt% =MFinCnt%
1861         EndIf
1862     Next  MCnt%
1863         '
1864 '    If MScrewJudge% = 0 Then
1865 '        Ovrd 100
1866 '        Spd M_NSpd
1867 '        PScrewPosition(1)
1868 '        Mvs PScrewPosition(10), -30  ' ねじピックアップ位置 -30mm
1869 '        'Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1870 '        M_Out(Y60_Driver)=0     ' ビット回転停止
1871 '        M_Out(Y63_Driver)=0     ' バンクセッティング　C2
1872 '        'Mvs PScrewPosition(10), -30  ' ねじピックアップ位置 -30mm
1873 '        Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1874 '        'Mov PScrewPosition(2)
1875 '        'もう一度吸着確認　上空の最終閾値
1876 '        MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1877 '        If MRtn = 0 Then      '吸着エラーの場合
1878 '            MScrewJudge% = 4
1879 '            ScrewGet = -3
1880 '        ElseIf MRtn = 1 Then      '吸着OKの場合
1881 '            MScrewJudge% = 1
1882 '            ScrewGet = 1
1883 '        EndIf
1884 '        Break
1885 '    EndIf
1886     '
1887 '    If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
1888     If MScrewJudge% = 0 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
1889     '
1890     Select MScrewJudge%
1891 '        Case 0
1892 ''            fErrorProcess(11,162,163,0) '異常終了
1893 '            MCommentD1001 = 162
1894 '            MCommentD1002 = 96
1895 '            Break
1896         Case 2
1897 '            fErrorProcess(11,63,161,0) '供給NG
1898             MCommentD1001 = 63
1899             MCommentD1002 = 96
1900             Break
1901         Case 3
1902 '            fErrorProcess(11,160,164,0) '誤供給
1903             MCommentD1001 = 237
1904             MCommentD1002 = 96
1905             Break
1906         Case 4
1907 '            fErrorProcess(11,94,95,0) '吸着NG
1908             MCommentD1001 = 94
1909             MCommentD1002 = 95
1910             Break
1911     End Select
1912     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
1913     '
1914     Select M_20#
1915         Case MAbout%          '停止が押された場合
1916             Mov PScrewPosition(2)                  '初期位置に戻って停止処理
1917             Mov PInitialPosition
1918             Break
1919         Case MContinue%       'リトライが押されていた場合(関数を抜けた先で処理)
1920             Break
1921         Case MNext%           '継続が押された場合
1922             M_20# = MClear%     '初期化
1923             Break
1924         Case MNgProcess%      'NGが押された場合
1925             Mov PScrewPosition(2)   'PIASにNG書き込みを行い,初期位置に戻って行程終了
1926             Mov PInitialPosition
1927             Break
1928         End Select
1929 *End_ScrewGet
1930     Exit Function
1931 FEnd
1932 '
1933 '■ProgramBankSet
1934 ''' <summary>
1935 ''' ねじ締めを行う(Pタイト)
1936 ''' </summary>
1937 '''<param name="MProgramNo">プログラム番号</param>
1938 '''<param name="MBankNo">バンク番号</param>
1939 '''</returns>
1940 ''' <remarks>
1941 ''' Date   : 2021/10/05 : M.Hayakawa
1942 ''' </remarks>'
1943 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
1944 '
1945     MLocalPrgNo% = (MProgramNo% - 1) * 32
1946     MLocalBankNo% = MBankNo% * 4
1947 '
1948     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
1949         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
1950     Else
1951         MLocalOutNo% = 0
1952     EndIf
1953 '
1954     M_Out8(12240) = MLocalOutNo%
1955     Dly 0.1
1956     Exit Function
1957 FEnd
1958 '
1959 '■fnKEY_WAIT()
1960 ''' <summary>
1961 ''' GOTからのキー入力待ち
1962 ''' </summary>
1963 '''<returns>1：停止    2：次へ
1964 '''         3：継続    4：トルクチェック開始
1965 '''         5：NG
1966 '''         11：ロボット初期位置1    12：ロボット初期位置2
1967 '''         13：ロボット初期位置3    14：ロボット初期位置4
1968 '''</returns>
1969 ''' <remarks>
1970 ''' Date   : 2021/07/07 : M.Hayakawa
1971 ''' </remarks>'
1972 Function M% fnKEY_WAIT()
1973     fnKEY_WAIT = 0
1974     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
1975     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
1976     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
1977     '下記キー待ちの継続に反応させないため
1978     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
1979     Dly 0.2
1980     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
1981     MLocalLoopFlg=1
1982     While MLocalLoopFlg=1
1983         If M_In(11345) = 1 Then         '停止   M5345
1984             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
1985             fnKEY_WAIT = 1
1986             MLocalLoopFlg=-1
1987             Break
1988         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
1989             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
1990             fnKEY_WAIT = 2
1991             MLocalLoopFlg=-1
1992             Break
1993         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
1994             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
1995             fnKEY_WAIT = 3
1996             MLocalLoopFlg=-1
1997             Break
1998         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
1999             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
2000             fnKEY_WAIT = 4
2001             MLocalLoopFlg=-1
2002             Break
2003         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
2004             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
2005             fnKEY_WAIT = 5
2006             MLocalLoopFlg=-1
2007             Break
2008             '
2009         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
2010             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
2011             fnKEY_WAIT = MRobotInit1%
2012             MLocalLoopFlg=-1
2013             Break
2014             '
2015         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
2016             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
2017             fnKEY_WAIT = MRobotInit2%
2018             MLocalLoopFlg=-1
2019             Break
2020             '
2021         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
2022             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
2023             fnKEY_WAIT = MRobotInit3%
2024             MLocalLoopFlg=-1
2025             Break
2026             '
2027         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
2028             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
2029             fnKEY_WAIT = MRobotInit4%
2030             MLocalLoopFlg=-1
2031             Break
2032             '
2033         Else
2034         EndIf
2035     WEnd
2036     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
2037     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
2038     Exit Function
2039 FEnd
2040 '
2041 '■ fnAUTO_CTL
2042 ''' <summary>
2043 ''' AUTOモードOFF、PLCからの開始待ち
2044 ''' </summary>
2045 ''' <remarks>
2046 ''' Date   : 2021/07/07 : M.Hayakawa
2047 ''' </remarks>
2048 Function M% fnAUTO_CTL
2049     fnAUTO_CTL = 0
2050     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2051     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
2052     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2053     '
2054     If M_Svo=0 Then             'サーボON確認
2055         Servo On
2056     EndIf
2057     Wait M_Svo=1
2058     Exit Function
2059 FEnd
2060 '
2061 '■ fnWindScreenOpen
2062 ''' <summary>
2063 ''' ウィンド画面の表示、非表示設定
2064 ''' </summary>
2065 '''<param name="%"></param>
2066 '''<param name="%"></param>
2067 '''<param name="%"></param>
2068 '''<param name="%"></param>
2069 ''' <remarks>
2070 ''' コメントD1001, D1002, D1003の設定
2071 ''' MWindReSet = 0     画面非表示
2072 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2073 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2074 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2075 ''' Date   : 2021/07/07 : M.Hayakawa
2076 ''' </remarks>
2077 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2078     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2079         M_Out16(12480) = MCommentD1001            'D1001 コメント
2080     EndIf
2081     '
2082     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2083         M_Out16(12496) = MCommentD1002            'D1002 コメント
2084     EndIf
2085     '
2086     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2087        M_Out16(12512) = MCommentD1003            'D1003 コメント
2088     EndIf
2089     '
2090     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2091     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2092     Dly 0.5
2093     M_Out(12363) = 0                         'ウィンド画面設定
2094     Exit Function
2095 FEnd
2096 '
2097 '■FnCtlValue2
2098 ''' <summary>
2099 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2100 ''' </summary>
2101 ''' <param name="MCtlNo%"></param>
2102 ''' <remarks>
2103 ''' Date : 2022/04/28 渡辺
2104 ''' </remarks>
2105 '''
2106 '''  1：投入数       ＋１
2107 '''  2：組立ＯＫ数   ＋１
2108 '''  3：組立ＮＧ数   ＋１ (未使用)
2109 '''  4：吸着エラー数 ＋１
2110 ''' 99：読書開始信号 OFF
2111 '''
2112 Function M% FnCtlValue2(ByVal MCtlNo%)
2113     FnCtlValue2 = 1
2114     Select MCtlNo%
2115         Case 1        '投入数＋１
2116             M_Out(12569) = 0             '書込み開始信号OFF
2117             M_Out(12568) = 1             '読込み開始信号ON
2118             MInputQty = M_In16(11600)    '投入数受信
2119             MInputQty = MInputQty + 1    '投入数＋１
2120             M_Out16(12592) = MInputQty   '投入数送信
2121             M_Out(12569) = 1             '書込み開始信号ON
2122             Break
2123             '
2124         Case 2        '組立ＯＫ数＋１
2125             M_Out(12569) = 0             '書込み開始信号OFF
2126             M_Out(12568) = 1             '読込み開始信号ON
2127             MAssyOkQty = M_In16(11616)   '組立OK数受信
2128             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2129             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2130             M_Out(12569) = 1             '書込み開始信号ON
2131             Break
2132             '
2133         Case 4        '吸着エラー数＋１
2134             M_Out(12569) = 0                       '書込み開始信号OFF
2135             M_Out(12568) = 1                       '読込み開始信号ON
2136             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
2137             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
2138             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
2139             M_Out(12569) = 1                       '書込み開始信号ON
2140             Break
2141             '
2142         Case 99        '読書開始信号OFF
2143             M_Out(12568) = 0        '読込み開始信号OFF
2144             M_Out(12569) = 0        '書込み開始信号OFF
2145             Break
2146             '
2147     End Select
2148     Exit Function
2149 FEnd
2150 '
2151 'Insightによる画像処理検査実行（並列処理なし）
2152 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2153 '-------------------------------------------------------------------------------
2154 'Insightによる画像処理検査実行（並列処理なし）
2155 '   引数
2156 '       PInspPos()      ：検査位置
2157 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2158 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2159 '       MInspCnt%       ：検査位置数
2160 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2161 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2162 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2163 '   戻り値：整数
2164 '       0=異常終了、1=正常終了
2165 '
2166 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2167 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2168 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2169 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2170 '   20200410    :   検査グループ設定Retry追加
2171 '-------------------------------------------------------------------------------
2172     '----- 初期設定 -----
2173     Cnt 0                                                           '移動効率化解除(初期値=0)
2174     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2175 '    Cnt 1,0.1,0.1
2176     '変数宣言・初期化
2177     Def Inte MNum                                                   '検査番号(検査順1～)
2178     MNum% = 1                                                       '検査番号初期値設定
2179     Def Inte MEndFlg                                                '検査終了フラグ
2180     MEndFlg% = 0
2181     '
2182     '検査G番号設定要求・検査実行要求off
2183     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2184     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2185     'エラー番号クリア
2186     MInspErrNum = 0                                                 '検査実行エラー番号
2187     M_Out16(MOUT_InspErrNum) = MInspErrNum
2188     MInspNGStepNum = 0                                              '検査実行NGStep番号
2189     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2190     '
2191     'Insight Ready check?
2192     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2193         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2194         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2195         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2196         ISInspectionSingle = 0                                      '異常終了戻り値設定
2197         Exit Function
2198     EndIf
2199     '
2200     '検査位置数確認
2201     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2202         MInspErrNum = 21                                            '検査データなし 21　引数<1
2203         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2204         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2205         ISInspectionSingle = 0                                      '異常終了戻り値設定
2206         Exit Function
2207     EndIf
2208     '
2209     '
2210     '
2211     '----- メイン処理 -----
2212     '設定された検査位置数分の検査実行
2213     While( MEndFlg% = 0 )
2214         '----- 検査グループ番号設定Retry追加 20200410
2215         MSetGrNumRetryExitFlg = 0
2216         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2217         While( MSetGrNumRetryExitFlg = 0 )
2218         '----- 検査グループ番号設定Retry追加ここまで 20200410
2219             '
2220             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2221             '
2222             '----- 検査グループ番号設定 -----
2223             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2224             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2225             '
2226             '検査位置へ移動・移動完了待ち
2227             fnAutoScreenComment(521)                                    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2228             Mvs PInspPos( MNum% )                                       '移動
2229             fnAutoScreenComment(523)                                    '状態表示[画像処理検査中] 2022/05/09 渡辺
2230             Dly 0.05                                                    '移動完了後Delay
2231             '
2232             '検査グループ番号設定終了確認
2233             M_Timer(1) = 0
2234             MExitFlg = 0
2235             While( MExitFlg = 0 )
2236                 '検査G設定正常終了?
2237                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2238                     MExitFlg = 1
2239                 '
2240                 '検査G設定異常終了?
2241                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2242                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2243                     If MInspErrNum = 0 Then                             '1回目のエラー?
2244                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2245                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2246                     EndIf
2247                     MExitFlg = 1
2248                 '
2249                 'timeoutチェック
2250                 ElseIf 1000 < M_Timer(1) Then
2251                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2252                     If MInspErrNum = 0 Then                             '1回目のエラー?
2253                         MInspErrNum = 12                                'timeout エラー番号=12
2254                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2255                     EndIf
2256                     MExitFlg = 1
2257                 EndIf
2258             WEnd
2259             '
2260             '検査G番号設定要求off
2261             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2262             '
2263             '----- 検査グループ設定Retry追加 20200410
2264             'NGなければ抜ける
2265             If MCurrentStepErr = 0 Then
2266                 MSetGrNumRetryExitFlg = 1
2267             Else
2268                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2269                 If MSetGrNumRetryCnt = 0 Then
2270                     MSetGrNumRetryExitFlg = 1
2271                 Else
2272                     'Retryへ　その前にDelay
2273                     Dly 0.5
2274                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2275                 EndIf
2276             EndIf
2277             '----- 検査グループ設定Retry追加ここまで 20200410
2278             '
2279         WEnd
2280         '
2281         '
2282         '
2283         '----- 検査実行 -----
2284         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2285             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2286                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2287                 MInspRetryExitFlg = 0
2288                 MRetryCnt = 2                                        'Retry回数設定
2289                 While( MInspRetryExitFlg = 0 )
2290                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2291                     '
2292                     '検査完了確認
2293                     MRetryCnt = MRetryCnt - 1
2294                     M_Timer(1) = 0
2295                     MExitFlg = 0
2296                     While( MExitFlg = 0 )
2297                     '検査完了待ち
2298                         '検査OK終了?
2299                         If M_In( MIN_IS_InspOK% ) = 1  Then
2300                             MJudgeOKFlg = 1                         '検査OKフラグON
2301                             MExitFlg = 1
2302                         '
2303                         '検査NG終了?
2304                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2305                             If MInspErrNum = 0 Then                 '1回目のエラー?
2306                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2307                                     MInspErrNum = 32                    '検査NG エラー番号=32
2308                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2309                                 EndIf
2310                             EndIf
2311                             MExitFlg = 1
2312                         '
2313                         '検査異常終了(IS timeout)?
2314                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2315                             If MInspErrNum = 0 Then                 '1回目のエラー?
2316                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2317                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2318                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2319                                 EndIf
2320                             EndIf
2321                             MExitFlg = 1
2322                         '
2323                         'timeoutチェック
2324                         ElseIf 3000 < M_Timer(1) Then
2325                             If MInspErrNum = 0 Then                 '1回目のエラー?
2326                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2327                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2328                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2329                                 EndIf
2330                             EndIf
2331                             MExitFlg = 1
2332                         EndIf
2333                     WEnd
2334                     '
2335                     '検査開始要求off
2336                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2337                     '
2338                     'OKなら抜ける
2339                     If MJudgeOKFlg = 1 Then
2340                         MInspRetryExitFlg = 1
2341                     Else
2342                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2343                         If MRetryCnt = 0 Then
2344                             MInspRetryExitFlg = 1
2345                         Else
2346                             'Retryへ　その前にDelay
2347                             Dly 0.3
2348                         EndIf
2349                     EndIf
2350                     '
2351                 WEnd
2352             EndIf
2353         EndIf
2354         '
2355         '
2356         '
2357         MNum% = MNum% + 1                                           '検査Step+1
2358         '検査終了確認　検査終了フラグセット
2359         If (MInspCnt% < MNum% ) Then
2360             MEndFlg% = 1                                            '検査終了フラグセット
2361         EndIf
2362         'NG発生時続行時処理
2363         If MInspErrNum <> 0 Then                                    'NGあり?
2364             If MNgContinue% <> 1 Then                               'NG続行?
2365                 MEndFlg% = 1                                        '検査終了フラグセット
2366             EndIf
2367         EndIf
2368     WEnd
2369     '
2370     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2371     If 0 < MZAxis% Then
2372         PCurrentPos = P_Curr                                        '現在位置取得
2373         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2374         fnAutoScreenComment(521)                                    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2375         Mvs PCurrentPos                                             '現在位置上空へ移動
2376     EndIf
2377     '
2378     '戻り値設定
2379     If MInspErrNum = 0 Then
2380         ISInspectionSingle = 1                                      '正常終了戻り値設定
2381     Else
2382         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2383         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2384         ISInspectionSingle = 0                                      '異常終了戻り値設定
2385     EndIf
2386     Exit Function
2387 FEnd
2388 '
2389 '■fnAutoScreenComment
2390 ''' <summary>
2391 ''' メイン画面の動作状況表示
2392 ''' コメントD1005の設定
2393 ''' </summary>
2394 '''<param name="McommentD1005%">コメントID</param>
2395 ''' <remarks>
2396 ''' Date   : 2021/07/07 : M.Hayakawa
2397 ''' </remarks>
2398 Function fnAutoScreenComment(ByVal McommentD1005%)
2399     M_Out16(12576) = McommentD1005%
2400     Exit Function
2401 FEnd
2402 '
2403 '■fnRoboPosChk
2404 ''' <summary>
2405 ''' 最後に終了したロボットポジションの確認
2406 ''' </summary>
2407 '''<param name="MINNumber%">入力番号</param>
2408 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2409 '''<param name="MTimeCnt&">タイムアウト時間</param>
2410 ''' PLCに保続した番号を読込み、確認
2411 ''' MRBTOpeGroupNo = 5 が初期位置に設定
2412 '''<returns>整数 0:タイムアウト 1:OK</returns>
2413 ''' <remarks>
2414 ''' Date   : 2021/07/07 : M.Hayakawa
2415 ''' </remarks>
2416 Function M% fnRoboPosChk
2417     fnRoboPosChk = 0
2418     MRet = fnStepRead()
2419     '初期位置でないと判断した場合
2420     'ウィンド画面切換え
2421     If MRBTOpeGroupNo > 5 Then
2422         '下記キー待ちの継続に反応させないため
2423         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2424         Dly 0.2
2425         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2426         Dly 1.5
2427         '
2428         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
2429         '
2430         MLoopFlg% = 1
2431         While MLoopFlg% = 1
2432             '
2433             '
2434             MKeyNumber% = fnKEY_WAIT()
2435             Select MKeyNumber%
2436                 Case Is = MAbout%       '停止
2437                     M_20# = MAbout%
2438                     MLoopFlg% = -1
2439                     Break
2440                 Case Is = MNext%        '次へ
2441                     'MLoopFlg% = -1
2442                     Break
2443                 Case Is = MContinue%    '継続
2444                     M_20# = MContinue%
2445                     MLoopFlg% = -1
2446                     Break
2447                 Default
2448                     Break
2449             End Select
2450         WEnd
2451     EndIf
2452     '
2453     If M_20# = MContinue% Then                              '継続ボタンが押された場合
2454         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
2455         Ovrd 5                                   '低速オーバーライド値設定
2456         Select MRBTOpeGroupNo
2457             Case Is = 5                          '何もしない
2458                 Break
2459             Case Is = 10                         '初期位置へ戻す
2460                 'Mov PTEST001
2461                 Break
2462             Case Is = 15                         '初期位置へ戻す
2463                 'Mov PTEST002
2464                 Dly 0.5
2465                 'Mov PTEST001
2466                 Dly 0.5
2467                 Break
2468             Default
2469                 Break
2470         End Select
2471         '
2472         Ovrd M_NOvrd                            'システムの初期値を設定
2473         M_Out(12364) = 1                        'toPLC_データ保存ON
2474         MRBTOpeGroupNo = 5
2475         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
2476         Dly 1.0
2477         M_Out(12364) = 0                        'toPLC_データ保存OFF
2478         fnRoboPosChk = 1                        '初期位置動作実行
2479         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
2480     EndIf
2481     Exit Function
2482 FEnd
2483 '
2484 '■frInCheck
2485 ''' <summary>
2486 ''' センサーINチェック
2487 ''' </summary>
2488 '''<param name="MINNumber%">入力番号</param>
2489 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2490 '''<param name="MTimeCnt&">タイムアウト時間</param>
2491 '''<returns>整数 0:タイムアウト 1:OK</returns>
2492 ''' <remarks>
2493 ''' Date   : 2021/07/07 : M.Hayakawa
2494 ''' </remarks>
2495 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2496     M_Timer(4) = 0
2497     MloopFlg = 0
2498     While MloopFlg = 0
2499         MCrtTime& = M_Timer(4)
2500         If M_In(MINNumber%) = MCMPFLG% Then
2501             MloopFlg = 1
2502             frInCheck = 1
2503         ElseIf MCrtTime& > MTimeCnt& Then
2504             MloopFlg = 1
2505             frInCheck = 0
2506         EndIf
2507     WEnd
2508     Exit Function
2509 FEnd
2510 '-----------------------------------------------
2511 '
2512 'ねじ締め機通信確認
2513 '
2514 '-----------------------------------------------
2515 Function M% fScewTcomChk
2516     fScewTcomChk = 0
2517     '通信確認送信
2518     M_Out(MOUT_ScwT_ComChk%) = MOn%
2519     '通信確認受信待機
2520     Wait M_In(MIN_ScwT_comOK%) = MOn%
2521     '通信確認送信終了
2522     M_Out(MOUT_ScwT_ComChk%) = MOff%
2523     Exit Function
2524 FEnd
2525 '
2526 '
2527 '-----------------------------------------------
2528 '
2529 'ねじ締め開始送信
2530 '
2531 '-----------------------------------------------
2532 Function M% fScewTStart
2533     fScewTStart = 0
2534     'ねじ締め開始待機を受信
2535     Wait M_In(MIN_ScwT_STRec%) = MOn%
2536     Dly 0.1
2537     'ねじ締め開始受信を送信
2538     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msecパルス
2539     Exit Function
2540 FEnd
2541 '
2542 '
2543 '-----------------------------------------------
2544 '
2545 'ねじ締め完了受信
2546 '
2547 '-----------------------------------------------
2548 Function M% fScewTFinish
2549     fScewTFinish = 0
2550     'ねじ締め完了待機を受信
2551     Wait M_In(MIN_ScwT_Fin%) = MOn%
2552     Dly 0.1
2553     'ねじ締め完了受信を送信
2554     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msecパルス
2555     Exit Function
2556 FEnd
2557 '
2558 '
2559 '-----------------------------------------------
2560 '
2561 '条件xx停止受信
2562 '
2563 '-----------------------------------------------
2564 Function M% fScewTCaseStop(ByVal MCase%())
2565     fScewTCaseStop = 0
2566     '条件xx停止を受信
2567     Wait M_In(MCase%(1)) = MOn%
2568     Dly 0.1
2569     '条件xx停止受信を送信
2570     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msecパルス
2571     Exit Function
2572 FEnd
2573 '
2574 '-----------------------------------------------
2575 '
2576 '再開始受信
2577 '
2578 '-----------------------------------------------
2579 Function M% fScewTReStart()
2580     fScewTReStart = 0
2581     '再開始を受信
2582     Wait M_In(MIN_ScwT_ReST%) = MOn%
2583     Dly 0.1
2584     '再開始受信を送信
2585     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msecパルス
2586     Exit Function
2587 FEnd
2588 '
2589 '■fErrorProcess
2590 '<summary>
2591 'エラー処理
2592 '</summary>
2593 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
2594 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
2595 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
2596 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
2597 '<make>
2598 '2021/11/5 中村天哉
2599 '</make>
2600 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2601     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
2602     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
2603     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
2604     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
2605 *RETRY_ERR_PROCESS
2606      M_20# = MClear%     '初期化
2607 '        'エラー処理記述
2608         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2609 '        'GOT KEY入力待ち
2610         MKeyNumber = fnKEY_WAIT()
2611 '        '
2612         If MKeyNumber = MAbout% Then   '停止を選択した場合
2613             M_20# = MAbout%            'M_20# プログラム間共通外部変数
2614 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2615             Break
2616          '
2617         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
2618             M_20# = MContinue%            'M_20# プログラム間共通外部変数
2619 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2620         '
2621         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
2622             M_20# = MNext%            'M_20# プログラム間共通外部変数
2623 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2624          '
2625         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
2626             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
2627 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2628             Break
2629         '
2630         EndIf
2631         '
2632         If M_20# = MClear% Then *RETRY_ERR_PROCESS
2633         fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2634     Exit Function
2635 FEnd
2636 '
2637 '■fnInitialZone
2638 ''' <summary>
2639 ''' 現在位置から上空に待避し、初期位置に戻る
2640 ''' </summary>
2641 ''' <param name="posNum%">移動先のポジション番号</param>
2642 ''' <remarks>
2643 ''' Date : 2021/12/2 : M.Hayakawa
2644 ''' Update:2022/06/2 : M.Hayakawa 他工程の非常停止復帰に合わせて変更
2645 ''' </remarks>
2646 Function fnInitialZone()
2647     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中]
2648 '
2649     Ovrd 5
2650 ' 上空退避
2651     PActive = P_Curr
2652     Pmove = PActive
2653 '
2654     If PActive.X > 580 Then
2655         Pmove.Z =380        'パレット上に腕を伸ばしているときは500まで上げられない為、例外処置
2656     Else
2657         Pmove.Z =500        '上記以外はZ:500まで持ち上げ
2658     EndIf
2659 '
2660     Mvs Pmove
2661     Mov PInitialPosition
2662 ' ロックを開放
2663     InitialState()
2664 ' 一旦停止
2665     fErrorProcess(20,70,256,0)
2666     Exit Function
2667  FEnd
2668 '
2669 '■InitialState
2670 ''' <summary>
2671 ''' ハンド、治具を初期位置にする
2672 ''' </summary>
2673 ''' <returns>   0 : OK
2674 '''             1 : NG
2675 ''' </returns>
2676 ''' <remarks>
2677 ''' Date : 2021/12/2 : M.Hayakawa
2678 ''' </remarks>
2679 Function M% InitialState()
2680     InitialState = 0
2681     '
2682     '位置決め解除
2683     M_Out(12264)=0
2684     M_Out(12265)=1 Dly 0.3                  'プッシュ解除
2685     'Wait M_In(11276)=1                      'プッシュ戻端検出(修正につきコメントアウト(8/26中村))
2686     MRtn = frInCheck(11276,1,MSETTIMEOUT05&)    'プッシュ位置戻端検出(8/26中村)
2687     If MRtn = 0 Then
2688         fErrorProcess(11,234,284,0)
2689         Select M_20#
2690             Case MAbout%                    '停止が押された場合
2691                 InitialState = 1
2692                 Break
2693             Case MNgProcess%
2694                 InitialState = 1
2695                 Break
2696             Case MContinue%                 'リトライが押された場合
2697                 M_20# = MClear%
2698                 InitialState = 0
2699                 Break
2700             Case MNext%                     '次へが押された場合
2701                 M_20# = MClear%
2702                 InitialState = 0
2703                 Break
2704         End Select
2705     EndIf
2706     *RETRY_POSITIONING_RESTORE
2707     '
2708     M_Out(12262)=0
2709     M_Out(12263)=1 Dly 0.3                  '位置決め解除
2710     'Wait M_In(11274)=1                      '位置決め戻端検出(修正につきコメントアウト(8/26中村))
2711     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)   '位置決め戻端検出(8/26中村)
2712     If MRtn = 0 Then
2713         fErrorProcess(11,234,284,0)
2714         Select M_20#
2715             Case MAbout%                    '停止が押された場合
2716                 InitialState = 1
2717                 Break
2718             Case MNgProcess%
2719                 InitialState = 1
2720                 Break
2721             Case MContinue%                 'リトライが押された場合
2722                 M_20# = MClear%
2723                 InitialState = 0
2724                 Break
2725             Case MNext%                     '次へが押された場合
2726                 M_20# = MClear%
2727                 InitialState = 0
2728                 Break
2729         End Select
2730     EndIf
2731     Exit Function
2732 FEnd
2733 '
2734 '■fnTorqueCheck
2735 ''' <summary>
2736 ''' トルクチェック動作用のメイン
2737 ''' </summary>
2738 ''' <remarks>
2739 ''' Date   : 2021/12/21 : H.AJI
2740 ''' </remarks>'
2741 Function M% fnTorqueCheck
2742     'トルクチェック中送信  搬送系停止
2743     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
2744     '
2745     fnTorqueCheck = 0
2746     Ovrd 20
2747     Mov PInitialPosition              '初期位置移動
2748     Ovrd 100
2749     '下記キー待ちの継続に反応させないため
2750     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2751     Dly 0.2
2752     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2753     '
2754     'M6340  トルクチェック受信
2755     'Dly 5.0
2756     M_Out(12340) = 1          'トルクチェック受信 M6340
2757     Dly 1.0
2758     M_Out(12340) = 0
2759     '
2760     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
2761     '
2762     MLoopFlg = 1
2763     While MLoopFlg = 1
2764         '
2765         Mov PInitialPosition              '初期位置移動
2766         '
2767         MKeyNumber = fnKEY_WAIT()
2768         Select MKeyNumber
2769             Case Is = 1           '停止
2770                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
2771                 Dly 1.0
2772                 M_Out(12343) = 0
2773                 Ovrd 20
2774                 Mov PTicketRead_1
2775                 Ovrd 100
2776                 M_20# = 1
2777                 MLoopFlg = -1
2778                 Break
2779             Case Is = 2           '次へ
2780                 Break
2781             Case Is = 3           '継続
2782                 Break
2783             Case Is = 4           'トルクチェック開始
2784                 M_Out(12545) = 1    ' toPLC_PCトルクチェック1要求受信(M315)
2785                 M_Out(12342) = 1 Dly 1.0    'トルクチェック開始要求受信 M6342
2786                 fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
2787                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
2788                 MRet = fnMoveTorquePosi()
2789                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
2790                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2791                 Break
2792             Default
2793                 Break
2794         End Select
2795     WEnd
2796     '
2797     'トルクチェック中停止送信
2798     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
2799     '
2800     'ロボットの位置を元に戻す
2801     '
2802     Exit Function
2803  FEnd
2804  '
2805 '
2806 '
2807 '---------------------------
2808 '
2809 '    メイン画面の表示、非表示設定
2810 '         コメントD1001, D1002, D1003の設定
2811 '           MWindReSet = 0     画面非表示
2812 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
2813 '           MWindErrScr = 10    エラー画面 D1001, D1002
2814 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2815 '
2816 '---------------------------
2817 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2818     fnMainScreenOpen = 0
2819     '
2820    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2821         M_Out16(12480) = MCommentD1001            'D1001 コメント
2822     EndIf
2823     '
2824     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2825         M_Out16(12496) = MCommentD1002            'D1002 コメント
2826     EndIf
2827     '
2828     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2829         M_Out16(12512) = MCommentD1003            'D1003 コメント
2830     EndIf
2831     '
2832     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2833     M_Out(12362) = 1                         'ウィンド画面設定  M6362
2834     Dly 0.5
2835     M_Out(12362) = 0                         'ウィンド画面設定
2836     Exit Function
2837 FEnd
2838 '
2839 '■Main
2840 ''' <summary>
2841 ''' トルクチェック実動作
2842 ''' </summary>
2843 ''' <remarks>
2844 ''' Date   : 2021/12/21 : H.AJI
2845 ''' </remarks>'
2846 Function M% fnMoveTorquePosi
2847      fnMoveTorquePosi = 0
2848      Ovrd 50
2849      Mov PTorqueCheck_1 'トルクチェックメーター上空へ移動
2850     '
2851     Spd M_NSpd
2852 '-------------      ドライバーRST
2853     M_Out(12240)=0     'ドライバーOFF CCW
2854     M_Out(12241)=0     'ドライバーOFF CW
2855     M_Out(12242)=1     'ドライバー解除 C1
2856     M_Out(12243)=1     'ドライバー解除 C2
2857     M_Out(12245)=0     'プログラム解除 F1/プログラム2
2858 '---------------------------------------
2859 '[P-11]
2860 '--------------------------------------------------------------   【トルクチェック 0.4N - P11】
2861     Mov PTorqueCheck, -50                     ' トルク-1　置き位置上空 50mm へ移動
2862     Dly 0.1
2863 '-----------------------
2864    'Cnt 0                           'Cnt動作-2　終了
2865 '-----------------------
2866     Mov PTorqueCheck , -5                      'トルク-1　置き位置上空 5mm へ移動
2867     Dly 0.2
2868 '-----------------------
2869     ProgramBankSet(1,3)
2870     M_Out(12241)=0                   'ドライバーOFF  CW
2871     'Dly 0.1
2872 '--------------------------------
2873     Ovrd 40
2874    'Dly 0.1
2875 '--------------------------------  ネジ締め速度設定
2876     Spd 14                            'ライド 100-40 100% :Spd 12
2877     Dly 0.1
2878 '--------------------------------
2879 '--------------------------------
2880 '---------------------------------【ねじ締め動作】
2881 '
2882     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '移動中エラー検出
2883    Mvs PTorqueCheck               'トルクチェック位置へ移動
2884     Dly 0.3                          '動作安定待ち
2885    M_Out(12241)=1                   'ドライバーON  CW
2886 '
2887     Wait M_In(11584)=1                '完了/エラー検出
2888     Dly 0.1
2889     Spd M_NSpd
2890    'Ovrd 20
2891     If M_In(11256)=1 Then *LBL1       'ネジトータルエラー検出
2892     Wait M_In(11257)=1                'ネジ完了SC
2893 '---------------------------------
2894     Dly 0.1
2895     M_Out(12241)=0                    'ドライバーOFF CW
2896     Dly 0.1
2897     M_Out(12242)=0                    'ドライバー解除 C1
2898     Dly 0.1
2899     M_Out(12243)=0                    'ドライバー解除 C2 (バンク3)
2900     Dly 0.1
2901     M_Out(12245)=0                    'プログラム2解除 F1
2902 '--------------------------------------------------------------   【トルクチェック 0.4N - P11ここまで】
2903 '
2904     Mvs PTorqueCheck,-60                       'あえてmov から変更
2905     Dly 0.1
2906 '--------------------------------------------------------------
2907    'Ovrd 80
2908 '--------------------------------------------------------------
2909 '---------------------------------------
2910 '---------------------------------------
2911 '---------------------------------------エラー離脱処理
2912    *LBL1
2913    Fsc Off            '力覚センサ　Off   *STEP1は不要
2914    Mvs ,-100
2915    M_Out(12241)=0     'ドライバーOFF CW
2916    Dly 0.1
2917    M_Out(12242)=0     'ドライバー解除 C1
2918    Dly 0.1
2919    M_Out(12243)=0     'ドライバー解除 C2 (バンク3)
2920    Dly 0.1
2921    M_Out(12245)=0     'プログラム解除 F1
2922 '---------------------------------------
2923 '---------------------------------------
2924 '-------------
2925    'Mov PInitPos19049
2926    Dly 0.1
2927 '
2928 '
2929     Exit Function
2930 FEnd
2931 '
2932 '■Main
2933 ''' <summary>
2934 ''' 組立動作用のメイン
2935 ''' </summary>
2936 ''' <remarks>
2937 ''' Date   : 2021/07/07 : M.Hayakawa
2938 ''' </remarks>'
2939 Function Main
2940     MopeNo = M_21#         '外部変数にて動作番号代入
2941     '
2942     If M_Svo=0 Then
2943         Servo On
2944     EndIf
2945     Wait M_Svo=1
2946 '組立スタート日付時刻要求パルスON
2947     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
2948 'パトライト操作
2949     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
2950     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
2951     '
2952     M_20# = 0                                   'KEY入力初期化
2953     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
2954     MRet% = 0
2955 '復帰動作　実行・未実行判別      2022/03/22 渡辺 作成
2956     PActive = P_Curr                    '現在位置を取得
2957     MRecoveryPass% = 0
2958     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
2959         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
2960             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
2961             MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
2962         EndIf
2963     EndIf
2964     EndIf
2965     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
2966         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
2967             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
2968                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
2969             EndIf
2970         EndIf
2971     EndIf
2972     If MRecoveryPass% = 0 Then
2973         fnInitialZone()        '復帰動作パスフラグが立っていない時は復帰動作を実行
2974     EndIf
2975     '
2976     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
2977         M_Out(12364) = 1            'toPLC_データ保存ON
2978 'トルクチェック
2979         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
2980             MRet% = fnTorqueCheck()
2981             Break
2982         Else
2983 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
2984 '                MRtn = InspInit()               '画像処理初期化処理
2985 '            EndIf
2986             '
2987            M_20# = MClear%                    '初期化
2988 '組立開始
2989             If M_In(MIN_ASSY_CANCEL%) = 0 Then
2990                 MRet% = fnAssyStart()
2991             Else
2992                 M_20# = MPass%
2993             EndIf
2994 '組立終了日付時刻
2995             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
2996             Wait M_In(11572) = 1            '日付取得完了
2997             Dly 0.1
2998             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
2999 'リフターユニットへのOUT
3000             '  KEY入力が何もない場合 OKと判断
3001             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
3002             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
3003 'OK/NGフラグ出力
3004             If M_20# <= 0 Then
3005                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
3006             ElseIf M_20# = MPass% Then
3007                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
3008             EndIf
3009 'PIASに組立完了書込み
3010             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3011                 If M_20# = MPass% Then
3012                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3013                 Else
3014                     'KEY入力がNGの場合
3015                     If M_20# = MNgProcess% Then
3016                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3017                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3018                         MRet% = fnPiasWrite(MNG%)
3019                        nAssyNgQty = nAssyNgQty + 1
3020                     EndIf
3021                     '
3022                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/07中村)
3023                     If M_20# = MAssyOK% Then
3024                             '-----------------------
3025                             'D732 -> D2600 コピー要求
3026                             M_Out(12566) = 1
3027 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3028                             M_Out(12566) = 0
3029                             '
3030                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3031                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3032                             '基板番号照合(PPは未使用）
3033 '                            MRet% = fnPCBNumberCheck()
3034                         Else
3035                             MRet% = 1
3036                         EndIf
3037                         '
3038                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3039                             If M_20# <> MAbout% Then
3040                                 '工程履歴OK書き込み
3041                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3042                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3043                                 MRet% = fnPiasWrite(MOK%)
3044                                 nAssyOkQty = 0
3045                                 nAssyOkQty = nAssyOkQty + 1
3046                             Else
3047                                 nAssyOkQty = nAssyOkQty + 1
3048                             EndIf
3049                         EndIf
3050                     EndIf
3051 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3052 '                    MRet% = fnPiasWrite(MOK%)
3053                 EndIf
3054             Else
3055                 nAssyOkQty = nAssyOkQty + 1
3056             EndIf
3057             '
3058             '組立終了日付時刻解除
3059             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3060             '投入数、組立OK数、組立NG数書込み
3061 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3062             '
3063 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3064 '                '画像処理終了処理
3065 '                MRtn = InspQuit()
3066 '            EndIf
3067         EndIf
3068         M_Out(12364) = 0                          'toPLC_データ保存OFF
3069     EndIf
3070 'パトライト操作
3071     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3072     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3073 'GOT表示
3074     fnAutoScreenComment(93)  'AUTO画面 工程完了
3075 FEnd
3076 End
3077 '
3078 'おまじないコメント
3079 '絶対削除するな
3080 '
3081 '
3082 '
3083 '
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
PScrewPos(1)=(+320.61,-172.83,+395.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPos(2)=(+320.61,-172.83,+345.08,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(10)=(+320.61,-172.83,+339.08,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PGetScrewPos(1)=(+180.56,+239.25,+380.00,+180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
PGetScrewPos(2)=(+182.97,+239.67,+400.00,+180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
PGetScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(9)=(+78.32,+270.81,+429.99,-180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
PGetScrewPos(10)=(+180.56,+239.25,+339.00,+180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
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
Pmove=(+603.00,-149.18,+380.00,-179.99,+0.00,+90.00,+0.00,+0.00)(7,0)
PInitialPosition=(+250.00,+0.00,+450.00,+180.00,+0.00,+180.00)(7,0)
PScrewSoc1=(+299.38,-64.81,+337.50,-180.00,+0.00,+90.00)(7,0)
PScrewSoc1_0=(+299.38,-64.81,+343.50,-180.00,+0.00,+90.00)(7,0)
PScrewSoc1_1=(+299.38,-64.81,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc2=(+320.71,-24.83,+337.59,-180.00,+0.00,+90.00)(7,0)
PScrewSoc2_0=(+320.71,-24.83,+343.59,-180.00,+0.00,+90.00)(7,0)
PScrewSoc2_1=(+320.71,-24.83,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc3=(+380.77,-25.04,+338.79,-180.00,+0.00,+90.00)(7,0)
PScrewSoc3_0=(+380.77,-25.04,+344.79,-180.00,+0.00,+90.00)(7,0)
PScrewSoc3_1=(+380.77,-25.04,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc4=(+391.11,-85.99,+338.72,-180.00,+0.00,+90.00)(7,0)
PScrewSoc4_0=(+391.11,-85.99,+344.72,-180.00,+0.00,+90.00)(7,0)
PScrewSoc4_1=(+391.11,-85.99,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc5=(+370.61,-153.98,+338.92,-180.00,+0.00,+90.00)(7,0)
PScrewSoc5_0=(+370.61,-153.98,+344.92,-180.00,+0.00,+90.00)(7,0)
PScrewSoc5_1=(+370.61,-153.98,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc6=(+320.61,-172.83,+339.08,-180.00,+0.00,+90.00)(7,0)
PScrewSoc6_0=(+320.61,-172.83,+345.08,-180.00,+0.00,+90.00)(7,0)
PScrewSoc6_1=(+320.61,-172.83,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSupply=(+180.56,+239.25,+339.00,+180.00,+0.00,-120.00)(7,0)
PScrewSupply_1=(+180.56,+239.25,+380.00,+180.00,+0.00,-120.00)(7,0)
PScrewSupply_2=(+182.97,+239.67,+400.00,+180.00,+0.00,+180.00)(7,0)
PScrewSupply_9=(+78.32,+270.81,+429.99,-180.00,+0.00,-120.00)(7,0)
PSocCheck=(+325.25,-60.87,+444.00,+180.00,-0.01,-180.00)(7,0)
PSocCheck_1=(+325.25,-60.87,+470.00,+180.00,-0.01,-180.00)(7,0)
PSocGet=(+627.82,+106.92,+311.65,-179.93,+0.04,-179.12)(7,0)
PSocGet_1=(+627.82,+106.92,+330.00,-179.93,+0.04,-179.12)(7,0)
PSocGet_2=(+627.82,+106.92,+380.00,-179.93,+0.04,-179.12)(7,0)
PSocPcbRead=(+343.72,-16.25,+435.00,-180.00,+0.00,-180.00)(7,0)
PSocPcbRead_1=(+343.72,-16.25,+480.00,-180.00,+0.00,-180.00)(7,0)
PSocPress=(+391.85,+11.73,+365.50,-180.00,+0.00,+180.00)(7,0)
PSocPress_1=(+391.85,+11.73,+380.00,-180.00,+0.00,+180.00)(7,0)
PSocPress_2=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PSocSet=(+486.15,-97.80,+352.52,+180.00,-0.04,-179.20)(7,0)
PSocSet_1=(+486.15,-97.80,+361.91,+180.00,-0.04,-179.20)(7,0)
PSocSet_2=(+486.15,-97.80,+380.00,+180.00,-0.04,-179.20)(7,0)
PTicketRead=(+603.00,-149.18,+373.00,-179.99,+0.00,+90.00)(7,0)
PTicketRead_1=(+603.00,-149.18,+450.00,-179.99,+0.00,+90.00)(7,0)
PTorqueCheck=(+144.46,-240.78,+340.00,-179.99,-0.01,+90.02)(7,0)
PTorqueCheck_1=(+144.45,-240.80,+360.00,-179.99,+0.00,+90.01)(7,0)
