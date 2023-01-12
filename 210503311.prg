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
480     '圧力設定(高圧)07/30中村
481     M_Out(12266) = 0
482     M_Out(12267) = 1
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
767 '
768 '--------------------------------------------
769     '圧力検出(高圧力)
770     '22/07/29追加 中村
771 '--------------------------------------------
772 *RE_Pa_OUT
773     If M_20# = MContinue% Then M_20# = MClear%
774     M_Out(12266) = 0
775     M_Out(12267) = 1
776     MRtn = frInCheck(11278,1,MSETTIMEOUT05&)    'KA用圧力検出(22/07/29中村)
777     If MRtn = 1 Then GoTo *CompPaOut
778     fErrorProcess(11,200,201,0)
779     If M_20# = MNext% Then M_20# = MClear%
780     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
781     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
782     If M_20# = MContinue% Then GoTo *RE_Pa_OUT
783 *CompPaOut
784     '
785     Ovrd 40
786     Mvs PSocPress               'プレスエンド端まで移動
787     Dly 0.5
788     'Wait M_In(11270)=1          'プレスシリンダー出端センサーON…もしOFFだったらコネクタカバー有
789     MRtn = frInCheck(11270,0,MSETTIMEOUT05&)    'プレスシリンダー出端センサーON(8/27中村)
790     If MRtn = 1 Then GoTo *CompPress_2
791     Mvs PSocPress_1              'プレス上空
792     fErrorProcess(11,70,71,0)
793     If M_20# = MNext% Then M_20# = MClear%
794     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
795     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
796     If M_20# = MContinue% Then GoTo *RE_BtoBPRESS
797     *CompPress_2
798     '
799     'Dly 0.2
800     '
801     *RE_BtoB_REST
802     M_Out(12260)=0              'プレスシリンダー出OFF
803     M_Out(12261)=1              'プレスシリンダー戻ON
804 '    Wait M_In(11269)=1          'プレスシリンダー戻端センサーON
805     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)    'プレスシリンダー戻端センサーON(8/27中村)
806     If MRtn = 1 Then GoTo *CompBtoBRest
807     fErrorProcess(11,243,284,0)
808     If M_20# = MNext% Then M_20# = MClear%
809     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
810     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
811     If M_20# = MContinue% Then GoTo *RE_BtoB_REST
812     *CompBtoBRest
813     '
814     Ovrd 100
815     Mov PSocPress_1             'プレス上空
816     M_Out(12266) = 0
817     M_Out(12267) = 0
818     'Mov PSocPress_2            'BtoBプレス回避点
819     '
820 'Soc基板ネジ締め
821     PGetScrewPos(1) = PScrewSupply_1        ' ねじピックアップ上空を代入
822     PGetScrewPos(2) = PScrewSupply_2        ' ねじ供給機回避点を代入
823     PGetScrewPos(9) = PScrewSupply_9        ' ネジ供給機上空ネジ捨て位置(10/6 M.H追加)
824     PGetScrewPos(10) = PScrewSupply         ' ねじピックアップ上空を代入
825     '
826     'Soc基板用ネジ供給機へネジを取りに行く
827     *RE_SCREW_GET_1
828     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
829     If MRtn = 1 Then GoTo *CompScrewGet_1
830     If M_20# = MNext% Then M_20# = MClear%
831     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
832     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
833     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
834     *CompScrewGet_1
835     '
836     PScrewPos(1) = PScrewSoc1_1             ' ねじピックアップ上空を代入
837     PScrewPos(2) = PScrewSoc1_0             ' ネジ1締め開始位置を代入(10/8 M.H)
838     PScrewPos(10) = PScrewSoc1              ' ねじ供給機回避点を代入
839     '①番ネジ締め
840     M_Out16(12672) = 1              'ネジ締め位置番号送信
841     MRtn = ScrewTight(PScrewPos,1,10.0)
842     M_Out16(12672) = 0              'ネジ締め位置番号クリア
843     If MRtn = 1 Then GoTo *CompScrew1
844     Mov PInitialPosition
845     fErrorProcess(11,53,52,0)
846     If M_20# = MNext% Then M_20# = MClear%
847     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
848     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
849     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
850     *CompScrew1
851     '
852     'Soc基板用ネジ供給機へネジを取りに行く
853     *RE_SCREW_GET_2
854     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
855     If MRtn = 1 Then GoTo *CompScrewGet_2
856     If M_20# = MNext% Then M_20# = MClear%
857     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
858     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
859     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
860     *CompScrewGet_2
861     '
862     PScrewPos(1) = PScrewSoc2_1             ' ねじピックアップ上空を代入
863     PScrewPos(2) = PScrewSoc2_0             ' ネジ2締め開始位置を代入(10/8 M.H)
864     PScrewPos(10) = PScrewSoc2              ' ねじ供給機回避点を代入
865     '②番ネジ締め
866     M_Out16(12672) = 2              'ネジ締め位置番号送信
867     MRtn = ScrewTight(PScrewPos,1,10.0)
868     M_Out16(12672) = 0              'ネジ締め位置番号クリア
869     If MRtn = 1 Then GoTo *CompScrew2
870     Mov PInitialPosition
871     fErrorProcess(11,54,52,0)
872     If M_20# = MNext% Then M_20# = MClear%
873     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
874     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
875     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
876     *CompScrew2
877     '
878     'Soc基板用ネジ供給機へネジを取りに行く
879     *RE_SCREW_GET_3
880     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
881     If MRtn = 1 Then GoTo *CompScrewGet_3
882     If M_20# = MNext% Then M_20# = MClear%
883     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
884     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
885     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
886     *CompScrewGet_3
887     '
888     PScrewPos(1) = PScrewSoc3_1             ' ねじピックアップ上空を代入
889     PScrewPos(2) = PScrewSoc3_0             ' ネジ3締め開始位置を代入(10/8 M.H)
890     PScrewPos(10) = PScrewSoc3              ' ねじ供給機回避点を代入
891     '③番ネジ締め
892     M_Out16(12672) = 3              'ネジ締め位置番号送信
893     MRtn = ScrewTight(PScrewPos,1,10.0)
894     M_Out16(12672) = 0              'ネジ締め位置番号クリア
895     If MRtn = 1 Then GoTo *CompScrew3
896     Mov PInitialPosition
897     fErrorProcess(11,55,52,0)
898     If M_20# = MNext% Then M_20# = MClear%
899     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
900     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
901     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
902     *CompScrew3
903     '
904     'Soc基板用ネジ供給機へネジを取りに行く
905     *RE_SCREW_GET_4
906     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
907     If MRtn = 1 Then GoTo *CompScrewGet_4
908     If M_20# = MNext% Then M_20# = MClear%
909     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
910     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
911     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
912     *CompScrewGet_4
913     '
914     PScrewPos(1) = PScrewSoc4_1             ' ねじピックアップ上空を代入
915     PScrewPos(2) = PScrewSoc4_0             ' ネジ4締め開始位置を代入(10/8 M.H)
916     PScrewPos(10) = PScrewSoc4              ' ねじ供給機回避点を代入
917     '④番ネジ締め
918     M_Out16(12672) = 4              'ネジ締め位置番号送信
919     MRtn = ScrewTight(PScrewPos,1,10.0)
920     M_Out16(12672) = 0              'ネジ締め位置番号クリア
921     If MRtn = 1 Then GoTo *CompScrew4
922     Mov PInitialPosition
923     fErrorProcess(11,56,52,0)
924     If M_20# = MNext% Then M_20# = MClear%
925     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
926     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
927     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
928     *CompScrew4
929     '
930     'Soc基板用ネジ供給機へネジを取りに行く
931     *RE_SCREW_GET_5
932     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
933     If MRtn = 1 Then GoTo *CompScrewGet_5
934     If M_20# = MNext% Then M_20# = MClear%
935     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
936     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
937     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
938     *CompScrewGet_5
939     '
940     PScrewPos(1) = PScrewSoc5_1             ' ねじピックアップ上空を代入
941     PScrewPos(2) = PScrewSoc5_0             ' ネジ5締め開始位置を代入(10/8 M.H)
942     PScrewPos(10) = PScrewSoc5              ' ねじ供給機回避点を代入
943     '⑤番ネジ締め
944     M_Out16(12672) = 5              'ネジ締め位置番号送信
945     MRtn = ScrewTight(PScrewPos,1,10.0)
946     M_Out16(12672) = 0              'ネジ締め位置番号クリア
947     If MRtn = 1 Then GoTo *CompScrew5
948     Mov PInitialPosition
949     fErrorProcess(11,57,52,0)
950     If M_20# = MNext% Then M_20# = MClear%
951     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
952     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
953     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
954     *CompScrew5
955     '
956     'Soc基板用ネジ供給機へネジを取りに行く
957     *RE_SCREW_GET_6
958     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
959     If MRtn = 1 Then GoTo *CompScrewGet_6
960     If M_20# = MNext% Then M_20# = MClear%
961     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
962     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
963     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
964     *CompScrewGet_6
965     '
966     PScrewPos(1) = PScrewSoc6_1             ' ねじピックアップ上空を代入
967     PScrewPos(2) = PScrewSoc6_0             ' ネジ6締め開始位置を代入(10/8 M.H)
968     PScrewPos(10) = PScrewSoc6              ' ねじ供給機回避点を代入
969     '⑥番ネジ締め
970     M_Out16(12672) = 6              'ネジ締め位置番号送信
971     MRtn = ScrewTight(PScrewPos,1,10.0)
972     M_Out16(12672) = 0              'ネジ締め位置番号クリア
973     If MRtn = 1 Then GoTo *CompScrew6
974     Mov PInitialPosition
975     fErrorProcess(11,58,52,0)
976     If M_20# = MNext% Then M_20# = MClear%
977     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
978     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
979     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
980     *CompScrew6
981 '
982     MRtn = FnCtlValue2(2)       '組立ＯＫ＋１  2022/04/28 渡辺
983     Mov PTicketRead_1   'チケット読み取り位置上空
984     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
985     InitialState()  ' 初期状態にする*AssyEnd
986     M_20# = MAssyOK%              ' 正常終了処理
987     GoTo *fnAssyStart_FEndPosi
988 '
989 *ASSY_ERROR_END
990     fnInitialZone()   ' 初期位置に移動
991 *AssyEnd
992     InitialState()  ' 初期状態にする
993 *fnAssyStart_FEndPosi
994     Exit Function
995 FEnd
996 '
997 '■fnPiasCheck
998 ''' <summary>
999 ''' PIASチケット読込み
1000 ''' </summary>
1001 ''' <returns>   0 : NG
1002 '''             1 : OK(読込み完了)
1003 ''' </returns>
1004 ''' <remarks>
1005 ''' Date   : 2021/07/07 : M.Hayakawa
1006 ''' </remarks>'
1007 Function M% fnPiasCheck
1008     fnPiasCheck = 0
1009     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1010     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1011 '
1012 *RETRY_PIAS
1013     M_20# = MClear%
1014     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1015     '
1016     '【IDチケット読み込み】
1017     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1018     MInspGroup%(1) = 1              '検査G番号
1019     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1020 '
1021     'エラーの場合
1022     If MRtn <> 1 Then
1023         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1024         If MRtn <> 1 Then
1025             'D720 -> D1300 コピー要求
1026             M_Out(12565) = 1
1027             Dly 0.5
1028             M_Out(12565) = 0
1029             'エラー処理記述
1030             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1031             'GOT KEY入力待ち
1032             MKeyNumber = fnKEY_WAIT()
1033             '
1034             Select MKeyNumber
1035                 Case MNext%         '次へを選択した場合
1036                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1037                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1038                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1039                     Break
1040                 Case MAbout%        '停止を選択した場合
1041                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1042                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1043                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1044                     Break
1045                 Case MNgProcess%    'NGを選択した場合
1046                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1047                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1048                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1049                     Break
1050                 Case MContinue%     '継続を選択した場合
1051                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1052                     M_20# = MContinue%
1053                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1054                     Break
1055             End Select
1056         EndIf
1057     EndIf
1058 '----------D720 -> D1300 コピー要求----------
1059     M_Out(12565) = 1
1060     Dly 0.5
1061     M_Out(12565) = 0
1062 '----------通信確認をする----------
1063     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1064     MRtn = 0                ' 初期化
1065     M_20# = MClear%         ' 初期化
1066     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1067     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1068     If MRtn <> 1 Then
1069         If M_20# = MContinue% Then
1070             GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1071         Else
1072             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1073         EndIf
1074     EndIf
1075 '----------工程抜け確認----------
1076     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1077     MRtn = 0                ' 初期化
1078     M_20# = MClear%         ' 初期化
1079     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1080     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1081     If MRtn <> 1 Then
1082         If M_20# = MContinue% Then
1083             GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1084         Else
1085             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1086         EndIf
1087     EndIf
1088     '
1089     fnPiasCheck = 1
1090     *fnPiasCheck_End
1091     Exit Function
1092 FEnd
1093 '
1094 '■fnPCComuCheck
1095 ''' <summary>
1096 ''' PC-PLC通信チェック
1097 ''' </summary>
1098 ''' <returns>   0 : NG
1099 '''             1 : OK(読込み完了)
1100 ''' </returns>
1101 ''' <remarks>
1102 ''' Date   : 2021/07/07 : M.Hayakawa
1103 ''' </remarks>'
1104 Function M% fnPCComuCheck
1105     fnPCComuCheck = 0
1106     MJudge% = 0                                  '初期化
1107     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1108     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1109     '
1110     For MStaNo = 0 To 5
1111         '
1112         If M_In(MIN_PIAS_ComOK%) = 1 Then
1113             'PC通信OK(M400)
1114             MJudge% = MOK%
1115             MStaNo = 5
1116             Break
1117         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1118             'toRBT_通信確認time out
1119             MJudge% = MNG%
1120             MCommentD1001 = 15
1121             MCommentD1002 = 21
1122             MStaNo = 5
1123             Break
1124         Else
1125             'toRBT_通信確認time out
1126             MJudge% = MNG%
1127             MCommentD1001 = 14
1128             MCommentD1002 = 21
1129             Break
1130         EndIf
1131     Next MStaNo
1132     '
1133     '上記で返信フラグを受信してからPC通信確認OFF
1134     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1135     '
1136     'エラー画面
1137     If MJudge% <> MOK% Then
1138         M_20# = MClear%     '初期化
1139         'エラー処理記述
1140         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1141         'GOT KEY入力待ち
1142         MKeyNumber = fnKEY_WAIT()
1143         '
1144         If MKeyNumber = MAbout% Then            '停止を選択した場合
1145             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1146             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1147             Break
1148         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1149             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1150             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1151             Break
1152         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1153             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1154             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1155             Break
1156         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1157             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1158             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1159             Break
1160         EndIf
1161     Else
1162         'OKの場合
1163         fnPCComuCheck = 1
1164     EndIf
1165     Exit Function
1166 FEnd
1167 '
1168 '■fnProcessCheck
1169 ''' <summary>
1170 ''' 工程抜け確認
1171 ''' </summary>
1172 ''' <returns>    1：工程履歴OK     0：異常終了
1173 '''             -1：前工程履歴NG  -2：自工程履歴あり
1174 '''             -3：モデル仕向NG  -4：タイムアウト
1175 '''             -5：履歴処理エラー
1176 ''' </returns>
1177 ''' <remarks>
1178 ''' Date   : 2021/07/07 : M.Hayakawa
1179 ''' </remarks>'
1180 Function M% fnProcessCheck
1181     fnProcessCheck = 0
1182     MJudge% = MNG%      '一旦NGを初期化とする
1183 '----------工程抜け確認----------
1184     MCommentD1001 = 0   'コメント初期化
1185     For MStaNo = 0 To 5
1186         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1187         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1188         '
1189         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1190             MJudge% = MOK%
1191             fnAutoScreenComment(85)     ' AUTO画面
1192             MStaNo = 5
1193             Break
1194         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1195             MFlgLoop% = 0
1196             MJudge% = MNG%
1197             MCommentD1001 = 27
1198             MCommentD1002 = 22
1199             fnAutoScreenComment(94)     ' AUTO画面
1200             fnProcessCheck = -2         ' NGは-2を返す
1201             MStaNo = 5
1202             Break
1203         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1204            MJudge% = MNG%
1205             MCommentD1001 = 31
1206             MCommentD1002 = 22
1207             fnAutoScreenComment(83)     ' AUTO画面
1208             fnProcessCheck = -3         ' NGは-3を返す
1209             MStaNo = 5
1210             Break
1211         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1212             '履歴NGは直ぐに終了せず繰り返し確認を行う
1213             '前工程の書込みが終了していない可能性があるため
1214             MJudge% = MNG%
1215             MCommentD1001 = 32
1216             MCommentD1002 = 22
1217             fnAutoScreenComment(84)     ' AUTO画面
1218             fnProcessCheck = -1         ' NGは-1を返す
1219             Dly 1.0
1220             '工程抜け確認OFF
1221             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1222             Dly 1.0
1223            'MStaNo = 5
1224             Break
1225         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1226             MFlgLoop% = 0
1227             MJudge% = MNG%
1228             MCommentD1001 = 29
1229             MCommentD1002 = 22
1230             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1231             fnProcessCheck = -5         ' NGは-5を返す
1232             MStaNo = 5
1233             Break
1234         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1235             MJudge% = MNG%
1236             If MCommentD1001 = 32 Then
1237                 '何もしない
1238             Else
1239                 MCommentD1001 = 26
1240             EndIf
1241             MCommentD1002 = 22
1242             fnProcessCheck = -4         ' NGは-4を返す
1243             MStaNo = 5
1244             Break
1245         Else
1246             MJudge% = MNG%
1247             MCommentD1001 = 28
1248             MCommentD1002 = 22
1249         EndIf
1250     Next MStaNo
1251     '工程抜け確認OFF
1252     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1253     '通過履歴NG 工程抜けの場合
1254     If MJudge% = MPass% Then
1255         M_20# = MPass%
1256     EndIf
1257     '
1258     'エラー画面
1259     If MJudge% <> MOK% Then
1260         M_20# = MClear%     '初期化
1261         'エラー処理記述
1262         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1263         'GOT KEY入力待ち
1264         MKeyNumber = fnKEY_WAIT()
1265         '
1266         Select MKeyNumber
1267             Case MAbout%        '停止を選択した場合
1268                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1269                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1270                 Break
1271             Case MNext%         '次へを選択した場合
1272                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1273                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1274                 Break
1275             Case MContinue%     '継続を選択した場合
1276                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1277                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1278                 Break
1279             Case MNgProcess%    'NGを選択した場合
1280                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1281                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1282                 Break
1283         End Select
1284     Else
1285         fnProcessCheck = 1  ' OKは1を返す
1286     EndIf
1287     Exit Function
1288 FEnd
1289 '
1290 '■fnPiasWrite
1291 ''' <summary>
1292 ''' Pias 組立結果書込み要求
1293 ''' </summary>
1294 '''<param name="MFlg%">
1295 '''                 MOK%(1) = 工程履歴にOKを書込む
1296 '''                 MNG%(0) = 工程履歴にNGを書込む
1297 '''</param>
1298 '''<returns></returns>
1299 ''' <remarks>
1300 ''' Date   : 2021/07/07 : M.Hayakawa
1301 ''' </remarks>'
1302 Function M% fnPiasWrite(ByVal MFlg%)
1303       fnPiasWrite = 0
1304 *RETRY_PIASWRITE
1305     '
1306     '組立OK(MOK%)の場合　M306 ON
1307    '組立NG(MNG%)の場合　M307 ON
1308     If MFlg% = MOK% Then
1309         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1310     Else
1311         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1312     EndIf
1313     Dly 0.1                  '念のため
1314     '
1315     'Piasへ書込み開始 M305 -> ON
1316     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1317     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1318     '
1319     MJudge% = MNG%
1320     '
1321     For MStaNo = 0 To 5
1322         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1323             MJudge% = MOK%
1324             'MRet = fnAutoScreenComment(85)  'AUTO画面
1325             MStaNo = 5
1326             Break
1327         '
1328         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1329             MJudge% = MNG%
1330             'MRet = fnAutoScreenComment(85)  'AUTO画面
1331            MCommentD1001 = 34
1332            MCommentD1002 = 25
1333             MStaNo = 5
1334             Break
1335         '
1336         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1337             MJudge% = MNG%
1338             'MRet = fnAutoScreenComment(85)  'AUTO画面
1339            MCommentD1001 = 35
1340            MCommentD1002 = 25
1341             MStaNo = 5
1342             Break
1343         '
1344         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1345             MJudge% = MNG%
1346             'MRet = fnAutoScreenComment(85)  'AUTO画面
1347            MCommentD1001 = 36
1348            MCommentD1002 = 25
1349             MStaNo = 5
1350             Break
1351         '
1352         Else
1353             MJudge% = MNG%
1354            MCommentD1001 = 42
1355            MCommentD1002 = 25
1356         '
1357         EndIf
1358         '
1359     Next MStaNo
1360     '
1361     'Piasへ書込み開始 M305 -> OfF
1362     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1363     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1364     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1365     '
1366     '
1367     '通過履歴NG 工程抜けの場合
1368     If MJudge% = MPass% Then
1369         M_20# = MPass%
1370     EndIf
1371     '
1372    M_20# = MClear%     '初期化
1373     '
1374     'エラー画面
1375     If MJudge% < MOK% Then
1376     '
1377 '残しておくが現状では使用しないラベル
1378 *RETRY_ERR_WRITE
1379         M_20# = MClear%     '初期化
1380         'エラー処理記述
1381         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1382         'GOT KEY入力待ち
1383         MKeyNumber = fnKEY_WAIT()
1384         '
1385         If MKeyNumber = MAbout% Then   '停止を選択した場合
1386             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1387            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1388             Break
1389         '
1390         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1391             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1392             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1393         '
1394         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1395             M_20# = MPass%            'M_20# プログラム間共通外部変数
1396             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1397         '
1398         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1399             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1400            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1401             Break
1402         '
1403         EndIf
1404         '
1405         If M_20# = MClear% Then *RETRY_ERR_WRITE
1406         '
1407     EndIf
1408     '
1409     If M_20# = MContinue% Then *RETRY_PIASWRITE
1410     '
1411     fnPiasWrite = 1
1412     Exit Function
1413 FEnd
1414 '
1415 '■fnPCBNumberCheck
1416 ''' <summary>
1417 ''' Pias 基板番号照合要求
1418 ''' </summary>
1419 '''<param name="%"></param>
1420 '''<param name="%"></param>
1421 '''<returns></returns>
1422 ''' <remarks>
1423 ''' Date   : 2021/07/07 : M.Hayakawa
1424 ''' </remarks>'
1425 Function M% fnPCBNumberCheck
1426       fnPCBNumberCheck = 0
1427     '
1428 *RETRY_PCBCHECK
1429     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1430     'Piasへ基板照合開始 M310 -> ON
1431     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1432     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1433     '
1434     MJudge% = MNG%
1435     '
1436     For MStaNo = 0 To 5
1437         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1438             MJudge% = MOK%
1439             fnAutoScreenComment(96)  'AUTO画面
1440             MStaNo = 5
1441             Break
1442         '
1443         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1444             MJudge% = MNG%
1445             fnAutoScreenComment(97)  'AUTO画面
1446             MCommentD1001 = 37
1447             MCommentD1002 = 25
1448             MStaNo = 5
1449             Break
1450         '
1451         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1452             MJudge% = MNG%
1453             fnAutoScreenComment(98)  'AUTO画面
1454             MCommentD1001 = 38
1455             MCommentD1002 = 25
1456             MStaNo = 5
1457             Break
1458         '
1459         ElseIf M_In(11580) = 1 Then                         'time out
1460             MJudge% = MNG%
1461             fnAutoScreenComment(99)  'AUTO画面
1462             MCommentD1001 = 39
1463             MCommentD1002 = 25
1464             MStaNo = 5
1465             Break
1466         '
1467         Else
1468             MJudge% = MNG%
1469            MCommentD1001 = 41
1470            MCommentD1002 = 25
1471         '
1472         EndIf
1473         '
1474     Next MStaNo
1475     '
1476     'Piasへ基板照合開始 M310 -> OfF
1477     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1478     '
1479     '
1480     '通過履歴NG 工程抜けの場合
1481     If MJudge% = MPass% Then
1482         M_20# = MPass%
1483     EndIf
1484     '
1485    M_20# = MClear%     '初期化
1486     '
1487     'エラー画面
1488     If MJudge% < MOK% Then
1489     '
1490 '残しておくが現状では使用しないラベル
1491 *RETRY_ERR_PCBNUMBER
1492         M_20# = MClear%     '初期化
1493         'エラー処理記述
1494         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1495         'GOT KEY入力待ち
1496         MKeyNumber = fnKEY_WAIT()
1497         '
1498         If MKeyNumber = MAbout% Then   '停止を選択した場合
1499             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1500             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1501             Break
1502         '
1503         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1504             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1505             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1506         '
1507         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1508             M_20# = MPass%            'M_20# プログラム間共通外部変数
1509             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1510         '
1511         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1512             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1513             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1514             Break
1515         '
1516         EndIf
1517         '
1518         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1519         '
1520     EndIf
1521     '
1522     If M_20# = MContinue% Then *RETRY_PCBCHECK
1523     Exit Function
1524 FEnd
1525 '
1526 '■ScrewTight
1527 ''' <summary>
1528 ''' ねじ締めを行う(Sタイト)
1529 ''' </summary>
1530 '''<param name="PScrewPos()">
1531 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
1532 '''             PScrewPos(2)    ：ねじ締め回避点
1533 '''             PScrewPos(10)   ：ねじ締め終了高さ
1534 '''<param name="MScrewType">ネジタイプ(mm/sec)
1535 '''             1:6mm Sタイト銀ネジ
1536 '''             2:8mm Pタイト
1537 '''             3:6mm Sタイト黒ネジ
1538 '''             4:13mm Sタイト
1539 '''             5:6mm Mネジ
1540 '''</param>
1541 '''<param name="MFeedSpd">送り速度(mm/sec)</param>
1542 '''<returns>整数
1543 '''         0=異常終了、1=正常終了
1544 '''</returns>
1545 ''' <remarks>
1546 ''' Date   : 2021/07/07 : M.Hayakawa
1547 ''' Update : 2021/09/28 : M.Hayakawa ネジタイプ、送り速度を引数に追加
1548 ''' </remarks>'
1549 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   'ネジ締め個別設定
1550     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
1551     ScrewTight = 0
1552     MOKNGFlg = 0
1553     Ovrd 100
1554     Mov PScrewPosition(1)       ' パレット上ねじ締めS①の安全回避位置
1555     Fine 0.05 , P
1556     Ovrd MOvrdA%
1557     ' 減速設定
1558     Accel 100, 10
1559     ' パレット上ねじ締め開始位置へ移動
1560     Mvs PScrewPosition(2)
1561     ' 加減速を元に戻す
1562     Accel
1563     ' 内部Ovrd設定
1564 '    Ovrd MOvrdA%
1565     Ovrd 100
1566     ' Spd設定
1567 '    Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
1568     Spd MFeedSpd
1569     ' 設定進み量5.0 × 操作パネルのオーバーライド係数 × プログラム内オーバーライド係数
1570     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
1571     Select MScrewType%
1572         Case 1
1573             ' Sタイト：プログラム1、バンク1に設定
1574             ProgramBankSet(1,1)
1575             Break
1576         Case 2
1577             ' Pタイト：プログラム1、バンク1に設定
1578             ProgramBankSet(3,1)
1579             Break
1580         Case 3
1581             ' Sタイト黒：プログラム1、バンク1に設定
1582             ProgramBankSet(1,1)
1583             Break
1584         Case 4
1585             ' Sタイト13mm：プログラム1、バンク1に設定
1586             ProgramBankSet(1,1)
1587             Break
1588         Case 5
1589             ' Mネジ：プログラム1、バンク1に設定
1590             ProgramBankSet(1,1)
1591             Break
1592         Case 6
1593             ' Sタイト：プログラム1、バンク4に設定
1594             ProgramBankSet(1,4)
1595             Break
1596         Default
1597             ' プログラム1、バンクなし設定
1598             ProgramBankSet(0,0)
1599             Break
1600     End Select
1601 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     'ドライバーON　CW
1602      'ドライバーON　CW
1603     M_Out(12241)=1
1604     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1605     Wait M_In(11584)=1          '完了/エラー検出 暫定コメント 10/6 M.H
1606     Dly 0.1
1607     Fine 0 , P
1608     Spd M_NSpd
1609     '
1610     If M_In(11256)=1 Then  'ねじトータルエラー検出時
1611         M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1612         Dly 0.1
1613        ' プログラム・バンク解除
1614         ProgramBankSet(0,0)
1615         'パレット上ねじ締め終了位置上空へ移動
1616         Mvs PScrewPosition(10),-80
1617         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1618         M_Out(12249)=1 Dly 0.3
1619         MOKNGFlg = -1
1620         ScrewTight = 0
1621     Else
1622          'ドライバーOFF　CW
1623         M_Out(12241)=0
1624 '        エラーがない場合はネジ締め終了位置で増し締め
1625 '        Select MScrewType%
1626 '            Case 1
1627 '                ' Sタイト：プログラム1、バンク3に設定
1628 '                ProgramBankSet(1,3)
1629 '                Break
1630 '            Case 2
1631 '                ' Pタイト：プログラム1、バンク3に設定
1632 '                ProgramBankSet(3,3)
1633 '                Break
1634 '            Case 3
1635 '                ' Sタイト黒：プログラム1、バンク3に設定
1636 '                ProgramBankSet(1,3)
1637 '                Break
1638 '            Case 4
1639 '                ' Sタイト13mm：プログラム1、バンク3に設定
1640 '                ProgramBankSet(1,3)
1641 '                Break
1642 '            Case 5
1643 '                ' Mネジ：プログラム1、バンク3に設定
1644 '                ProgramBankSet(1,3)
1645 '                Break
1646 '            Default
1647 '                ' プログラム1、バンクなし設定
1648 '                ProgramBankSet(0,0)
1649 '                Break
1650 '        End Select
1651 '         'ドライバーON　CW
1652 '        Mvs PScrewPosition(10)
1653 '        M_Out(12241)=1
1654 '        Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1655 '        Fine 0 , P
1656 '
1657          'ドライバーOFF　CW
1658         M_Out(12241)=0
1659        ' プログラム・バンク解除
1660         ProgramBankSet(0,0)
1661         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1662         M_Out(12249)=1 Dly 0.3
1663     '     ↓PScrewPos(2) → PScrewPosition(10)に変更 9/16 M.Hayakawa
1664         'パレット上ねじ締め終了位置上空へ移動
1665        Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1666         'Mvs PScrewPosition(10),-80
1667         ScrewTight = 1
1668     EndIf
1669 ' 暫定（暫定マスク　9/16 M.Hayakawa)
1670 '    Ovrd 10
1671 '    Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1672     Ovrd 100
1673     Exit Function
1674 FEnd
1675 '
1676 '■ScrewGet
1677 ''' <summary>
1678 ''' ねじ供給機からねじを得る
1679 ''' </summary>
1680 '''<param name="%">
1681 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
1682 '''         PScrewPos(2)    ：ねじ供給器回避点
1683 '''         PScrewPos(9)    ：ねじ供給器上空ネジ捨位置
1684 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1685 '''         PScrewPos(3)    ：Mねじポカヨケ位置
1686 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
1687 '''</param>
1688 '''<param name = FeederReadyNo%> 指定の供給機Ready </param>
1689 '''<param name = FeederScrewSensor%> 指定の誤供給防止センサー指定(0でセンサー無し)</param>
1690 '''<returns>整数
1691 '''         0=異常終了、1=正常終了、-1=ねじ供給NG、-2=ねじ誤供給NG、-3=吸着エラー
1692 '''</returns>
1693 ''' <remarks>
1694 ''' Date   : 2021/07/07 : M.Hayakawa
1695 ''' </remarks>
1696 '''<update>
1697 '''Date    : 2021/11/15 : 中村
1698 '''</update>
1699 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
1700     fnAutoScreenComment(522)    '状態表示[ネジ供給待ち] 2022/05/09 渡辺
1701     ScrewGet = 0
1702     MScrewJudge% = 0
1703     'ねじ供給器初期動作エラーチェック
1704 ' ↓暫定削除
1705     'Mov PScrewPosition(2)   'ねじ供給機回避点へ移動
1706     For MCnt% = 0 To MFinCnt%
1707         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '指定ねじ供給機がReadyになっているか確認(5秒間)
1708         If MRtn = 0 Then
1709             M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
1710             ScrewGet = -1
1711             MScrewJudge% = 2
1712         EndIf
1713         Ovrd 100
1714         If FeederScrewSensor% <> 0 Then
1715             If M_In(FeederScrewSensor%) = 1 Then  '誤供給が検出されたら
1716                 'Ovrd 30
1717                 M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
1718                 'NGとしてここの関数から抜ける
1719                 ScrewGet = -2
1720                 MScrewJudge% = 3
1721             EndIf
1722         EndIf
1723         Ovrd 100
1724         Spd M_NSpd
1725         If MScrewJudge% = 0 Then
1726     '        ScrewGet = 0
1727             M_Out(Y63_Driver)=1         ' バンクセッティング　C2
1728             Dly 0.3
1729             MScrewCnt% = 0
1730             MFinCnt% = 2
1731             fnAutoScreenComment(521)     '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
1732             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1733             'Ovrd 40 '2に変更 10/6 M.H '5に変更10/7中村
1734             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1735             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
1736             'Mvs PScrewPosition(10), 1.2
1737            Mvs PScrewPosition(10)       'Fan用ねじ吸着位置修正のため変更 2022-02-01AJI
1738             'ビット回転(処理位置変更)
1739             M_Out(Y60_Driver)=1
1740             M_Timer(4) = 0
1741             MloopFlg = 0
1742             MCntTime& = 0
1743             While MloopFlg = 0
1744                 MCrtTime& = M_Timer(4)
1745                 If MCrtTime& >= 180 Then
1746                     MloopFlg = 1
1747                 EndIf
1748             WEnd
1749             M_Out(Y68_VV1)=1 Dly 0.3     ' ねじ吸着　ON'Dly 0.3追加(8/27中村)
1750             '吸着確認
1751             MRtn = 0
1752             MRtn = frInCheck(11271, 1, MSETTIMEOUT01&)
1753 '            'ビット回転(処理位置変更)
1754 '            M_Out(Y60_Driver)=1
1755 '            Dly 0.2
1756             '
1757             JOvrd M_NJovrd
1758             Spd M_NSpd
1759             'ネジ吸着確認位置移動
1760             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
1761             Mvs PScrewPosition(10), -30  ' ネジ吸着確認位置
1762            'Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1763             'ビット回転停止
1764             M_Out(Y60_Driver)=0
1765             '
1766 '            If MRtn = 1 Then           '最初の閾値を確認しない
1767                 '1秒間ネジ吸着確認
1768                 MRtn = frInCheck(11272, 1, MSETTIMEOUT01&)
1769 '            EndIf
1770             'MRtn = 0'強制エラー
1771             '吸着エラーの場合
1772             'ネジをねじ太郎に戻す
1773             If MRtn = 0 Then
1774                 Ovrd 30      '2から5に変更
1775                 'ビット回転停止
1776                 M_Out(Y60_Driver)=0
1777                 'ネジ供給機上空
1778                 Mvs PScrewPosition(1)
1779                 '更に上空
1780                 Mov PScrewPosition(1), -140
1781                 'ネジ捨て位置
1782                 MRtn = FnCtlValue2(4)          '吸着エラー数＋１  2022/04/28 渡辺
1783                 Mov PScrewPosition(9)
1784                 MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1785                 '吸着OFF
1786                 M_Out(12249)=1 Dly 0.3 'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1787                 Dly 0.2
1788                 '破壊ON
1789                 M_Out(Y6B_VB1)=1 '真空破壊ON
1790                 'ビット回転
1791                 M_Out(Y61_Driver)=1
1792                 Dly 0.5
1793                 '                '
1794                 Ovrd 100
1795                 JOvrd M_NJovrd
1796                 Spd M_NSpd
1797                 'ドライバーを上下させねじを振り落とす
1798                 Mov PScrewPosition(9), 10
1799                 Mov PScrewPosition(9)
1800                 Dly 0.1
1801                 Mov PScrewPosition(9), 10
1802                 Mov PScrewPosition(9)
1803                 '
1804                 'ネジ落ち待ち
1805                 Wait M_In(11272) = 0
1806                 'ビット回転停止
1807                 M_Out(Y61_Driver)=0
1808                 Dly 0.1
1809                 '破壊OFF
1810                 M_Out(Y6B_VB1)=0 '真空破壊OFF
1811                 'ねじ落ちたとして、移動更に上空
1812                 Mov PScrewPosition(1), -140
1813                 Ovrd 100
1814                 Spd M_NSpd
1815                 'ネジ供給機上空
1816                 Mvs PScrewPosition(1)
1817 '                '
1818                 ScrewGet = -3
1819                 If MCnt% = MFinCnt% Then
1820                     MScrewJudge% = 4
1821                     Mov PScrewPosition(2)
1822                     Break
1823                 EndIf
1824                 Break
1825 '                '
1826             Else
1827                 MCnt% = MFinCnt%
1828                 ScrewGet = 1
1829             EndIf
1830         Else
1831             MCnt% =MFinCnt%
1832         EndIf
1833     Next  MCnt%
1834         '
1835 '    If MScrewJudge% = 0 Then
1836 '        Ovrd 100
1837 '        Spd M_NSpd
1838 '        PScrewPosition(1)
1839 '        Mvs PScrewPosition(10), -30  ' ねじピックアップ位置 -30mm
1840 '        'Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1841 '        M_Out(Y60_Driver)=0     ' ビット回転停止
1842 '        M_Out(Y63_Driver)=0     ' バンクセッティング　C2
1843 '        'Mvs PScrewPosition(10), -30  ' ねじピックアップ位置 -30mm
1844 '        Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1845 '        'Mov PScrewPosition(2)
1846 '        'もう一度吸着確認　上空の最終閾値
1847 '        MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1848 '        If MRtn = 0 Then      '吸着エラーの場合
1849 '            MScrewJudge% = 4
1850 '            ScrewGet = -3
1851 '        ElseIf MRtn = 1 Then      '吸着OKの場合
1852 '            MScrewJudge% = 1
1853 '            ScrewGet = 1
1854 '        EndIf
1855 '        Break
1856 '    EndIf
1857     '
1858 '    If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
1859     If MScrewJudge% = 0 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
1860     '
1861     Select MScrewJudge%
1862 '        Case 0
1863 ''            fErrorProcess(11,162,163,0) '異常終了
1864 '            MCommentD1001 = 162
1865 '            MCommentD1002 = 96
1866 '            Break
1867         Case 2
1868 '            fErrorProcess(11,63,161,0) '供給NG
1869             MCommentD1001 = 63
1870             MCommentD1002 = 96
1871             Break
1872         Case 3
1873 '            fErrorProcess(11,160,164,0) '誤供給
1874             MCommentD1001 = 237
1875             MCommentD1002 = 96
1876             Break
1877         Case 4
1878 '            fErrorProcess(11,94,95,0) '吸着NG
1879             MCommentD1001 = 94
1880             MCommentD1002 = 95
1881             Break
1882     End Select
1883     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
1884     '
1885     Select M_20#
1886         Case MAbout%          '停止が押された場合
1887             Mov PScrewPosition(2)                  '初期位置に戻って停止処理
1888             Mov PInitialPosition
1889             Break
1890         Case MContinue%       'リトライが押されていた場合(関数を抜けた先で処理)
1891             Break
1892         Case MNext%           '継続が押された場合
1893             M_20# = MClear%     '初期化
1894             Break
1895         Case MNgProcess%      'NGが押された場合
1896             Mov PScrewPosition(2)   'PIASにNG書き込みを行い,初期位置に戻って行程終了
1897             Mov PInitialPosition
1898             Break
1899         End Select
1900 *End_ScrewGet
1901     Exit Function
1902 FEnd
1903 '
1904 '■ProgramBankSet
1905 ''' <summary>
1906 ''' ねじ締めを行う(Pタイト)
1907 ''' </summary>
1908 '''<param name="MProgramNo">プログラム番号</param>
1909 '''<param name="MBankNo">バンク番号</param>
1910 '''</returns>
1911 ''' <remarks>
1912 ''' Date   : 2021/10/05 : M.Hayakawa
1913 ''' </remarks>'
1914 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
1915 '
1916     MLocalPrgNo% = (MProgramNo% - 1) * 32
1917     MLocalBankNo% = MBankNo% * 4
1918 '
1919     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
1920         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
1921     Else
1922         MLocalOutNo% = 0
1923     EndIf
1924 '
1925     M_Out8(12240) = MLocalOutNo%
1926     Dly 0.1
1927     Exit Function
1928 FEnd
1929 '
1930 '■fnKEY_WAIT()
1931 ''' <summary>
1932 ''' GOTからのキー入力待ち
1933 ''' </summary>
1934 '''<returns>1：停止    2：次へ
1935 '''         3：継続    4：トルクチェック開始
1936 '''         5：NG
1937 '''         11：ロボット初期位置1    12：ロボット初期位置2
1938 '''         13：ロボット初期位置3    14：ロボット初期位置4
1939 '''</returns>
1940 ''' <remarks>
1941 ''' Date   : 2021/07/07 : M.Hayakawa
1942 ''' </remarks>'
1943 Function M% fnKEY_WAIT()
1944     fnKEY_WAIT = 0
1945     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
1946     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
1947     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
1948     '下記キー待ちの継続に反応させないため
1949     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
1950     Dly 0.2
1951     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
1952     MLocalLoopFlg=1
1953     While MLocalLoopFlg=1
1954         If M_In(11345) = 1 Then         '停止   M5345
1955             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
1956             fnKEY_WAIT = 1
1957             MLocalLoopFlg=-1
1958             Break
1959         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
1960             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
1961             fnKEY_WAIT = 2
1962             MLocalLoopFlg=-1
1963             Break
1964         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
1965             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
1966             fnKEY_WAIT = 3
1967             MLocalLoopFlg=-1
1968             Break
1969         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
1970             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
1971             fnKEY_WAIT = 4
1972             MLocalLoopFlg=-1
1973             Break
1974         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
1975             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
1976             fnKEY_WAIT = 5
1977             MLocalLoopFlg=-1
1978             Break
1979             '
1980         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
1981             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
1982             fnKEY_WAIT = MRobotInit1%
1983             MLocalLoopFlg=-1
1984             Break
1985             '
1986         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
1987             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
1988             fnKEY_WAIT = MRobotInit2%
1989             MLocalLoopFlg=-1
1990             Break
1991             '
1992         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
1993             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
1994             fnKEY_WAIT = MRobotInit3%
1995             MLocalLoopFlg=-1
1996             Break
1997             '
1998         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
1999             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
2000             fnKEY_WAIT = MRobotInit4%
2001             MLocalLoopFlg=-1
2002             Break
2003             '
2004         Else
2005         EndIf
2006     WEnd
2007     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
2008     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
2009     Exit Function
2010 FEnd
2011 '
2012 '■ fnAUTO_CTL
2013 ''' <summary>
2014 ''' AUTOモードOFF、PLCからの開始待ち
2015 ''' </summary>
2016 ''' <remarks>
2017 ''' Date   : 2021/07/07 : M.Hayakawa
2018 ''' </remarks>
2019 Function M% fnAUTO_CTL
2020     fnAUTO_CTL = 0
2021     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2022     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
2023     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2024     '
2025     If M_Svo=0 Then             'サーボON確認
2026         Servo On
2027     EndIf
2028     Wait M_Svo=1
2029     Exit Function
2030 FEnd
2031 '
2032 '■ fnWindScreenOpen
2033 ''' <summary>
2034 ''' ウィンド画面の表示、非表示設定
2035 ''' </summary>
2036 '''<param name="%"></param>
2037 '''<param name="%"></param>
2038 '''<param name="%"></param>
2039 '''<param name="%"></param>
2040 ''' <remarks>
2041 ''' コメントD1001, D1002, D1003の設定
2042 ''' MWindReSet = 0     画面非表示
2043 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2044 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2045 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2046 ''' Date   : 2021/07/07 : M.Hayakawa
2047 ''' </remarks>
2048 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2049     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2050         M_Out16(12480) = MCommentD1001            'D1001 コメント
2051     EndIf
2052     '
2053     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2054         M_Out16(12496) = MCommentD1002            'D1002 コメント
2055     EndIf
2056     '
2057     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2058        M_Out16(12512) = MCommentD1003            'D1003 コメント
2059     EndIf
2060     '
2061     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2062     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2063     Dly 0.5
2064     M_Out(12363) = 0                         'ウィンド画面設定
2065     Exit Function
2066 FEnd
2067 '
2068 '■FnCtlValue2
2069 ''' <summary>
2070 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2071 ''' </summary>
2072 ''' <param name="MCtlNo%"></param>
2073 ''' <remarks>
2074 ''' Date : 2022/04/28 渡辺
2075 ''' </remarks>
2076 '''
2077 '''  1：投入数       ＋１
2078 '''  2：組立ＯＫ数   ＋１
2079 '''  3：組立ＮＧ数   ＋１ (未使用)
2080 '''  4：吸着エラー数 ＋１
2081 ''' 99：読書開始信号 OFF
2082 '''
2083 Function M% FnCtlValue2(ByVal MCtlNo%)
2084     FnCtlValue2 = 1
2085     Select MCtlNo%
2086         Case 1        '投入数＋１
2087             M_Out(12569) = 0             '書込み開始信号OFF
2088             M_Out(12568) = 1             '読込み開始信号ON
2089             MInputQty = M_In16(11600)    '投入数受信
2090             MInputQty = MInputQty + 1    '投入数＋１
2091             M_Out16(12592) = MInputQty   '投入数送信
2092             M_Out(12569) = 1             '書込み開始信号ON
2093             Break
2094             '
2095         Case 2        '組立ＯＫ数＋１
2096             M_Out(12569) = 0             '書込み開始信号OFF
2097             M_Out(12568) = 1             '読込み開始信号ON
2098             MAssyOkQty = M_In16(11616)   '組立OK数受信
2099             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2100             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2101             M_Out(12569) = 1             '書込み開始信号ON
2102             Break
2103             '
2104         Case 4        '吸着エラー数＋１
2105             M_Out(12569) = 0                       '書込み開始信号OFF
2106             M_Out(12568) = 1                       '読込み開始信号ON
2107             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
2108             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
2109             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
2110             M_Out(12569) = 1                       '書込み開始信号ON
2111             Break
2112             '
2113         Case 99        '読書開始信号OFF
2114             M_Out(12568) = 0        '読込み開始信号OFF
2115             M_Out(12569) = 0        '書込み開始信号OFF
2116             Break
2117             '
2118     End Select
2119     Exit Function
2120 FEnd
2121 '
2122 'Insightによる画像処理検査実行（並列処理なし）
2123 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2124 '-------------------------------------------------------------------------------
2125 'Insightによる画像処理検査実行（並列処理なし）
2126 '   引数
2127 '       PInspPos()      ：検査位置
2128 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2129 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2130 '       MInspCnt%       ：検査位置数
2131 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2132 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2133 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2134 '   戻り値：整数
2135 '       0=異常終了、1=正常終了
2136 '
2137 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2138 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2139 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2140 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2141 '   20200410    :   検査グループ設定Retry追加
2142 '-------------------------------------------------------------------------------
2143     '----- 初期設定 -----
2144     Cnt 0                                                           '移動効率化解除(初期値=0)
2145     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2146 '    Cnt 1,0.1,0.1
2147     '変数宣言・初期化
2148     Def Inte MNum                                                   '検査番号(検査順1～)
2149     MNum% = 1                                                       '検査番号初期値設定
2150     Def Inte MEndFlg                                                '検査終了フラグ
2151     MEndFlg% = 0
2152     '
2153     '検査G番号設定要求・検査実行要求off
2154     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2155     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2156     'エラー番号クリア
2157     MInspErrNum = 0                                                 '検査実行エラー番号
2158     M_Out16(MOUT_InspErrNum) = MInspErrNum
2159     MInspNGStepNum = 0                                              '検査実行NGStep番号
2160     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2161     '
2162     'Insight Ready check?
2163     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2164         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2165         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2166         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2167         ISInspectionSingle = 0                                      '異常終了戻り値設定
2168         Exit Function
2169     EndIf
2170     '
2171     '検査位置数確認
2172     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2173         MInspErrNum = 21                                            '検査データなし 21　引数<1
2174         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2175         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2176         ISInspectionSingle = 0                                      '異常終了戻り値設定
2177         Exit Function
2178     EndIf
2179     '
2180     '
2181     '
2182     '----- メイン処理 -----
2183     '設定された検査位置数分の検査実行
2184     While( MEndFlg% = 0 )
2185         '----- 検査グループ番号設定Retry追加 20200410
2186         MSetGrNumRetryExitFlg = 0
2187         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2188         While( MSetGrNumRetryExitFlg = 0 )
2189         '----- 検査グループ番号設定Retry追加ここまで 20200410
2190             '
2191             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2192             '
2193             '----- 検査グループ番号設定 -----
2194             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2195             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2196             '
2197             '検査位置へ移動・移動完了待ち
2198             fnAutoScreenComment(521)                                    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2199             Mvs PInspPos( MNum% )                                       '移動
2200             fnAutoScreenComment(523)                                    '状態表示[画像処理検査中] 2022/05/09 渡辺
2201             Dly 0.05                                                    '移動完了後Delay
2202             '
2203             '検査グループ番号設定終了確認
2204             M_Timer(1) = 0
2205             MExitFlg = 0
2206             While( MExitFlg = 0 )
2207                 '検査G設定正常終了?
2208                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2209                     MExitFlg = 1
2210                 '
2211                 '検査G設定異常終了?
2212                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2213                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2214                     If MInspErrNum = 0 Then                             '1回目のエラー?
2215                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2216                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2217                     EndIf
2218                     MExitFlg = 1
2219                 '
2220                 'timeoutチェック
2221                 ElseIf 1000 < M_Timer(1) Then
2222                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2223                     If MInspErrNum = 0 Then                             '1回目のエラー?
2224                         MInspErrNum = 12                                'timeout エラー番号=12
2225                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2226                     EndIf
2227                     MExitFlg = 1
2228                 EndIf
2229             WEnd
2230             '
2231             '検査G番号設定要求off
2232             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2233             '
2234             '----- 検査グループ設定Retry追加 20200410
2235             'NGなければ抜ける
2236             If MCurrentStepErr = 0 Then
2237                 MSetGrNumRetryExitFlg = 1
2238             Else
2239                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2240                 If MSetGrNumRetryCnt = 0 Then
2241                     MSetGrNumRetryExitFlg = 1
2242                 Else
2243                     'Retryへ　その前にDelay
2244                     Dly 0.5
2245                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2246                 EndIf
2247             EndIf
2248             '----- 検査グループ設定Retry追加ここまで 20200410
2249             '
2250         WEnd
2251         '
2252         '
2253         '
2254         '----- 検査実行 -----
2255         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2256             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2257                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2258                 MInspRetryExitFlg = 0
2259                 MRetryCnt = 2                                        'Retry回数設定
2260                 While( MInspRetryExitFlg = 0 )
2261                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2262                     '
2263                     '検査完了確認
2264                     MRetryCnt = MRetryCnt - 1
2265                     M_Timer(1) = 0
2266                     MExitFlg = 0
2267                     While( MExitFlg = 0 )
2268                     '検査完了待ち
2269                         '検査OK終了?
2270                         If M_In( MIN_IS_InspOK% ) = 1  Then
2271                             MJudgeOKFlg = 1                         '検査OKフラグON
2272                             MExitFlg = 1
2273                         '
2274                         '検査NG終了?
2275                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2276                             If MInspErrNum = 0 Then                 '1回目のエラー?
2277                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2278                                     MInspErrNum = 32                    '検査NG エラー番号=32
2279                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2280                                 EndIf
2281                             EndIf
2282                             MExitFlg = 1
2283                         '
2284                         '検査異常終了(IS timeout)?
2285                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2286                             If MInspErrNum = 0 Then                 '1回目のエラー?
2287                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2288                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2289                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2290                                 EndIf
2291                             EndIf
2292                             MExitFlg = 1
2293                         '
2294                         'timeoutチェック
2295                         ElseIf 3000 < M_Timer(1) Then
2296                             If MInspErrNum = 0 Then                 '1回目のエラー?
2297                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2298                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2299                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2300                                 EndIf
2301                             EndIf
2302                             MExitFlg = 1
2303                         EndIf
2304                     WEnd
2305                     '
2306                     '検査開始要求off
2307                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2308                     '
2309                     'OKなら抜ける
2310                     If MJudgeOKFlg = 1 Then
2311                         MInspRetryExitFlg = 1
2312                     Else
2313                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2314                         If MRetryCnt = 0 Then
2315                             MInspRetryExitFlg = 1
2316                         Else
2317                             'Retryへ　その前にDelay
2318                             Dly 0.3
2319                         EndIf
2320                     EndIf
2321                     '
2322                 WEnd
2323             EndIf
2324         EndIf
2325         '
2326         '
2327         '
2328         MNum% = MNum% + 1                                           '検査Step+1
2329         '検査終了確認　検査終了フラグセット
2330         If (MInspCnt% < MNum% ) Then
2331             MEndFlg% = 1                                            '検査終了フラグセット
2332         EndIf
2333         'NG発生時続行時処理
2334         If MInspErrNum <> 0 Then                                    'NGあり?
2335             If MNgContinue% <> 1 Then                               'NG続行?
2336                 MEndFlg% = 1                                        '検査終了フラグセット
2337             EndIf
2338         EndIf
2339     WEnd
2340     '
2341     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2342     If 0 < MZAxis% Then
2343         PCurrentPos = P_Curr                                        '現在位置取得
2344         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2345         fnAutoScreenComment(521)                                    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2346         Mvs PCurrentPos                                             '現在位置上空へ移動
2347     EndIf
2348     '
2349     '戻り値設定
2350     If MInspErrNum = 0 Then
2351         ISInspectionSingle = 1                                      '正常終了戻り値設定
2352     Else
2353         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2354         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2355         ISInspectionSingle = 0                                      '異常終了戻り値設定
2356     EndIf
2357     Fine 0 , P
2358     Exit Function
2359 FEnd
2360 '
2361 '■fnAutoScreenComment
2362 ''' <summary>
2363 ''' メイン画面の動作状況表示
2364 ''' コメントD1005の設定
2365 ''' </summary>
2366 '''<param name="McommentD1005%">コメントID</param>
2367 ''' <remarks>
2368 ''' Date   : 2021/07/07 : M.Hayakawa
2369 ''' </remarks>
2370 Function fnAutoScreenComment(ByVal McommentD1005%)
2371     M_Out16(12576) = McommentD1005%
2372     Exit Function
2373 FEnd
2374 '
2375 '■fnRoboPosChk
2376 ''' <summary>
2377 ''' 最後に終了したロボットポジションの確認
2378 ''' </summary>
2379 '''<param name="MINNumber%">入力番号</param>
2380 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2381 '''<param name="MTimeCnt&">タイムアウト時間</param>
2382 ''' PLCに保続した番号を読込み、確認
2383 ''' MRBTOpeGroupNo = 5 が初期位置に設定
2384 '''<returns>整数 0:タイムアウト 1:OK</returns>
2385 ''' <remarks>
2386 ''' Date   : 2021/07/07 : M.Hayakawa
2387 ''' </remarks>
2388 Function M% fnRoboPosChk
2389     fnRoboPosChk = 0
2390     MRet = fnStepRead()
2391     '初期位置でないと判断した場合
2392     'ウィンド画面切換え
2393     If MRBTOpeGroupNo > 5 Then
2394         '下記キー待ちの継続に反応させないため
2395         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2396         Dly 0.2
2397         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2398         Dly 1.5
2399         '
2400         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
2401         '
2402         MLoopFlg% = 1
2403         While MLoopFlg% = 1
2404             '
2405             '
2406             MKeyNumber% = fnKEY_WAIT()
2407             Select MKeyNumber%
2408                 Case Is = MAbout%       '停止
2409                     M_20# = MAbout%
2410                     MLoopFlg% = -1
2411                     Break
2412                 Case Is = MNext%        '次へ
2413                     'MLoopFlg% = -1
2414                     Break
2415                 Case Is = MContinue%    '継続
2416                     M_20# = MContinue%
2417                     MLoopFlg% = -1
2418                     Break
2419                 Default
2420                     Break
2421             End Select
2422         WEnd
2423     EndIf
2424     '
2425     If M_20# = MContinue% Then                              '継続ボタンが押された場合
2426         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
2427         Ovrd 5                                   '低速オーバーライド値設定
2428         Select MRBTOpeGroupNo
2429             Case Is = 5                          '何もしない
2430                 Break
2431             Case Is = 10                         '初期位置へ戻す
2432                 'Mov PTEST001
2433                 Break
2434             Case Is = 15                         '初期位置へ戻す
2435                 'Mov PTEST002
2436                 Dly 0.5
2437                 'Mov PTEST001
2438                 Dly 0.5
2439                 Break
2440             Default
2441                 Break
2442         End Select
2443         '
2444         Ovrd M_NOvrd                            'システムの初期値を設定
2445         M_Out(12364) = 1                        'toPLC_データ保存ON
2446         MRBTOpeGroupNo = 5
2447         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
2448         Dly 1.0
2449         M_Out(12364) = 0                        'toPLC_データ保存OFF
2450         fnRoboPosChk = 1                        '初期位置動作実行
2451         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
2452     EndIf
2453     Exit Function
2454 FEnd
2455 '
2456 '■frInCheck
2457 ''' <summary>
2458 ''' センサーINチェック
2459 ''' </summary>
2460 '''<param name="MINNumber%">入力番号</param>
2461 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2462 '''<param name="MTimeCnt&">タイムアウト時間</param>
2463 '''<returns>整数 0:タイムアウト 1:OK</returns>
2464 ''' <remarks>
2465 ''' Date   : 2021/07/07 : M.Hayakawa
2466 ''' </remarks>
2467 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2468     M_Timer(4) = 0
2469     MloopFlg = 0
2470     While MloopFlg = 0
2471         MCrtTime& = M_Timer(4)
2472         If M_In(MINNumber%) = MCMPFLG% Then
2473             MloopFlg = 1
2474             frInCheck = 1
2475         ElseIf MCrtTime& > MTimeCnt& Then
2476             MloopFlg = 1
2477             frInCheck = 0
2478         EndIf
2479     WEnd
2480     Exit Function
2481 FEnd
2482 '-----------------------------------------------
2483 '
2484 'ねじ締め機通信確認
2485 '
2486 '-----------------------------------------------
2487 Function M% fScewTcomChk
2488     fScewTcomChk = 0
2489     '通信確認送信
2490     M_Out(MOUT_ScwT_ComChk%) = MOn%
2491     '通信確認受信待機
2492     Wait M_In(MIN_ScwT_comOK%) = MOn%
2493     '通信確認送信終了
2494     M_Out(MOUT_ScwT_ComChk%) = MOff%
2495     Exit Function
2496 FEnd
2497 '
2498 '
2499 '-----------------------------------------------
2500 '
2501 'ねじ締め開始送信
2502 '
2503 '-----------------------------------------------
2504 Function M% fScewTStart
2505     fScewTStart = 0
2506     'ねじ締め開始待機を受信
2507     Wait M_In(MIN_ScwT_STRec%) = MOn%
2508     Dly 0.1
2509     'ねじ締め開始受信を送信
2510     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msecパルス
2511     Exit Function
2512 FEnd
2513 '
2514 '
2515 '-----------------------------------------------
2516 '
2517 'ねじ締め完了受信
2518 '
2519 '-----------------------------------------------
2520 Function M% fScewTFinish
2521     fScewTFinish = 0
2522     'ねじ締め完了待機を受信
2523     Wait M_In(MIN_ScwT_Fin%) = MOn%
2524     Dly 0.1
2525     'ねじ締め完了受信を送信
2526     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msecパルス
2527     Exit Function
2528 FEnd
2529 '
2530 '
2531 '-----------------------------------------------
2532 '
2533 '条件xx停止受信
2534 '
2535 '-----------------------------------------------
2536 Function M% fScewTCaseStop(ByVal MCase%())
2537     fScewTCaseStop = 0
2538     '条件xx停止を受信
2539     Wait M_In(MCase%(1)) = MOn%
2540     Dly 0.1
2541     '条件xx停止受信を送信
2542     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msecパルス
2543     Exit Function
2544 FEnd
2545 '
2546 '-----------------------------------------------
2547 '
2548 '再開始受信
2549 '
2550 '-----------------------------------------------
2551 Function M% fScewTReStart()
2552     fScewTReStart = 0
2553     '再開始を受信
2554     Wait M_In(MIN_ScwT_ReST%) = MOn%
2555     Dly 0.1
2556     '再開始受信を送信
2557     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msecパルス
2558     Exit Function
2559 FEnd
2560 '
2561 '■fErrorProcess
2562 '<summary>
2563 'エラー処理
2564 '</summary>
2565 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
2566 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
2567 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
2568 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
2569 '<make>
2570 '2021/11/5 中村天哉
2571 '</make>
2572 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2573     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
2574     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
2575     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
2576     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
2577 *RETRY_ERR_PROCESS
2578      M_20# = MClear%     '初期化
2579 '        'エラー処理記述
2580         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2581 '        'GOT KEY入力待ち
2582         MKeyNumber = fnKEY_WAIT()
2583 '        '
2584         If MKeyNumber = MAbout% Then   '停止を選択した場合
2585             M_20# = MAbout%            'M_20# プログラム間共通外部変数
2586 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2587             Break
2588          '
2589         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
2590             M_20# = MContinue%            'M_20# プログラム間共通外部変数
2591 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2592         '
2593         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
2594             M_20# = MNext%            'M_20# プログラム間共通外部変数
2595 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2596          '
2597         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
2598             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
2599 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2600             Break
2601         '
2602         EndIf
2603         '
2604         If M_20# = MClear% Then *RETRY_ERR_PROCESS
2605         fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2606     Exit Function
2607 FEnd
2608 '
2609 '■fnInitialZone
2610 ''' <summary>
2611 ''' 現在位置から上空に待避し、初期位置に戻る
2612 ''' </summary>
2613 ''' <param name="posNum%">移動先のポジション番号</param>
2614 ''' <remarks>
2615 ''' Date : 2021/12/2 : M.Hayakawa
2616 ''' Update:2022/06/2 : M.Hayakawa 他工程の非常停止復帰に合わせて変更
2617 ''' </remarks>
2618 Function fnInitialZone()
2619     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中]
2620 '
2621     Ovrd 5
2622 ' 上空退避
2623     PActive = P_Curr
2624     Pmove = PActive
2625 '
2626     If PActive.X > 580 Then
2627         Pmove.Z =380        'パレット上に腕を伸ばしているときは500まで上げられない為、例外処置
2628     Else
2629         Pmove.Z =500        '上記以外はZ:500まで持ち上げ
2630     EndIf
2631 '
2632     Mvs Pmove
2633     Mov PInitialPosition
2634 ' ロックを開放
2635     InitialState()
2636 ' 一旦停止
2637     fErrorProcess(20,70,256,0)
2638     Exit Function
2639  FEnd
2640 '
2641 '■InitialState
2642 ''' <summary>
2643 ''' ハンド、治具を初期位置にする
2644 ''' </summary>
2645 ''' <returns>   0 : OK
2646 '''             1 : NG
2647 ''' </returns>
2648 ''' <remarks>
2649 ''' Date : 2021/12/2 : M.Hayakawa
2650 ''' </remarks>
2651 Function M% InitialState()
2652     InitialState = 0
2653     '
2654     '位置決め解除
2655     M_Out(12264) = 0
2656     M_Out(12265)=1 Dly 0.3                  'プッシュ解除
2657     'Wait M_In(11276)=1                      'プッシュ戻端検出(修正につきコメントアウト(8/26中村))
2658     MRtn = frInCheck(11276,1,MSETTIMEOUT05&)    'プッシュ位置戻端検出(8/26中村)
2659     If MRtn = 0 Then
2660         fErrorProcess(11,234,284,0)
2661         Select M_20#
2662             Case MAbout%                    '停止が押された場合
2663                 InitialState = 1
2664                 Break
2665             Case MNgProcess%
2666                 InitialState = 1
2667                 Break
2668             Case MContinue%                 'リトライが押された場合
2669                 M_20# = MClear%
2670                 InitialState = 0
2671                 Break
2672             Case MNext%                     '次へが押された場合
2673                 M_20# = MClear%
2674                 InitialState = 0
2675                 Break
2676         End Select
2677     EndIf
2678     *RETRY_POSITIONING_RESTORE
2679     '
2680     M_Out(12262) = 0
2681     M_Out(12263)=1 Dly 0.3                  '位置決め解除
2682     'Wait M_In(11274)=1                      '位置決め戻端検出(修正につきコメントアウト(8/26中村))
2683     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)   '位置決め戻端検出(8/26中村)
2684     If MRtn = 0 Then
2685         fErrorProcess(11,234,284,0)
2686         Select M_20#
2687             Case MAbout%                    '停止が押された場合
2688                 InitialState = 1
2689                 Break
2690             Case MNgProcess%
2691                 InitialState = 1
2692                 Break
2693             Case MContinue%                 'リトライが押された場合
2694                 M_20# = MClear%
2695                 InitialState = 0
2696                 Break
2697             Case MNext%                     '次へが押された場合
2698                 M_20# = MClear%
2699                 InitialState = 0
2700                 Break
2701         End Select
2702     EndIf
2703     Exit Function
2704 FEnd
2705 '
2706 '■fnTorqueCheck
2707 ''' <summary>
2708 ''' トルクチェック動作用のメイン
2709 ''' </summary>
2710 ''' <remarks>
2711 ''' Date   : 2021/12/21 : H.AJI
2712 ''' </remarks>'
2713 Function M% fnTorqueCheck
2714     'トルクチェック中送信  搬送系停止
2715     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
2716     '
2717     fnTorqueCheck = 0
2718     Ovrd 20
2719     Mov PInitialPosition              '初期位置移動
2720     Ovrd 100
2721     '下記キー待ちの継続に反応させないため
2722     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2723     Dly 0.2
2724     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2725     '
2726     'M6340  トルクチェック受信
2727     'Dly 5.0
2728     M_Out(12340) = 1          'トルクチェック受信 M6340
2729     Dly 1.0
2730     M_Out(12340) = 0
2731     '
2732     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
2733     '
2734     MLoopFlg = 1
2735     While MLoopFlg = 1
2736         '
2737         Mov PInitialPosition              '初期位置移動
2738         '
2739         MKeyNumber = fnKEY_WAIT()
2740         Select MKeyNumber
2741             Case Is = 1           '停止
2742                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
2743                 Dly 1.0
2744                 M_Out(12343) = 0
2745                 Ovrd 20
2746                 Mov PTicketRead_1
2747                 Ovrd 100
2748                 M_20# = 1
2749                 MLoopFlg = -1
2750                 Break
2751             Case Is = 2           '次へ
2752                 Break
2753             Case Is = 3           '継続
2754                 Break
2755             Case Is = 4           'トルクチェック開始
2756                 M_Out(12545) = 1    ' toPLC_PCトルクチェック1要求受信(M315)
2757                 M_Out(12342) = 1 Dly 1.0    'トルクチェック開始要求受信 M6342
2758                 fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
2759                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
2760                 MRet = fnMoveTorquePosi()
2761                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
2762                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2763                 Break
2764             Default
2765                 Break
2766         End Select
2767     WEnd
2768     '
2769     'トルクチェック中停止送信
2770     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
2771     '
2772     'ロボットの位置を元に戻す
2773     '
2774     Exit Function
2775  FEnd
2776  '
2777 '
2778 '
2779 '---------------------------
2780 '
2781 '    メイン画面の表示、非表示設定
2782 '         コメントD1001, D1002, D1003の設定
2783 '           MWindReSet = 0     画面非表示
2784 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
2785 '           MWindErrScr = 10    エラー画面 D1001, D1002
2786 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2787 '
2788 '---------------------------
2789 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2790     fnMainScreenOpen = 0
2791     '
2792    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2793         M_Out16(12480) = MCommentD1001            'D1001 コメント
2794     EndIf
2795     '
2796     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2797         M_Out16(12496) = MCommentD1002            'D1002 コメント
2798     EndIf
2799     '
2800     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2801         M_Out16(12512) = MCommentD1003            'D1003 コメント
2802     EndIf
2803     '
2804     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2805     M_Out(12362) = 1                         'ウィンド画面設定  M6362
2806     Dly 0.5
2807     M_Out(12362) = 0                         'ウィンド画面設定
2808     Exit Function
2809 FEnd
2810 '
2811 '■Main
2812 ''' <summary>
2813 ''' トルクチェック実動作
2814 ''' </summary>
2815 ''' <remarks>
2816 ''' Date   : 2021/12/21 : H.AJI
2817 ''' </remarks>'
2818 Function M% fnMoveTorquePosi
2819      fnMoveTorquePosi = 0
2820      Ovrd 50
2821      Mov PTorqueCheck_1 'トルクチェックメーター上空へ移動
2822     '
2823     Spd M_NSpd
2824 '-------------      ドライバーRST
2825     M_Out(12240)=0     'ドライバーOFF CCW
2826     M_Out(12241)=0     'ドライバーOFF CW
2827     M_Out(12242)=1     'ドライバー解除 C1
2828     M_Out(12243)=1     'ドライバー解除 C2
2829     M_Out(12245)=0     'プログラム解除 F1/プログラム2
2830 '---------------------------------------
2831 '[P-11]
2832 '--------------------------------------------------------------   【トルクチェック 0.4N - P11】
2833     Mov PTorqueCheck, -50                     ' トルク-1　置き位置上空 50mm へ移動
2834     Dly 0.1
2835 '-----------------------
2836    'Cnt 0                           'Cnt動作-2　終了
2837 '-----------------------
2838     Mov PTorqueCheck , -5                      'トルク-1　置き位置上空 5mm へ移動
2839     Dly 0.2
2840 '-----------------------
2841     ProgramBankSet(1,3)
2842     M_Out(12241)=0                   'ドライバーOFF  CW
2843     'Dly 0.1
2844 '--------------------------------
2845     Ovrd 40
2846    'Dly 0.1
2847 '--------------------------------  ネジ締め速度設定
2848     Spd 14                            'ライド 100-40 100% :Spd 12
2849     Dly 0.1
2850 '--------------------------------
2851 '--------------------------------
2852 '---------------------------------【ねじ締め動作】
2853 '
2854     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '移動中エラー検出
2855    Mvs PTorqueCheck               'トルクチェック位置へ移動
2856     Dly 0.3                          '動作安定待ち
2857    M_Out(12241)=1                   'ドライバーON  CW
2858 '
2859     Wait M_In(11584)=1                '完了/エラー検出
2860     Dly 0.1
2861     Spd M_NSpd
2862    'Ovrd 20
2863     If M_In(11256)=1 Then *LBL1       'ネジトータルエラー検出
2864     Wait M_In(11257)=1                'ネジ完了SC
2865 '---------------------------------
2866     Dly 0.1
2867     M_Out(12241)=0                    'ドライバーOFF CW
2868     Dly 0.1
2869     M_Out(12242)=0                    'ドライバー解除 C1
2870     Dly 0.1
2871     M_Out(12243)=0                    'ドライバー解除 C2 (バンク3)
2872     Dly 0.1
2873     M_Out(12245)=0                    'プログラム2解除 F1
2874 '--------------------------------------------------------------   【トルクチェック 0.4N - P11ここまで】
2875 '
2876     Mvs PTorqueCheck,-60                       'あえてmov から変更
2877     Dly 0.1
2878 '--------------------------------------------------------------
2879    'Ovrd 80
2880 '--------------------------------------------------------------
2881 '---------------------------------------
2882 '---------------------------------------
2883 '---------------------------------------エラー離脱処理
2884    *LBL1
2885    Fsc Off            '力覚センサ　Off   *STEP1は不要
2886    Mvs ,-100
2887    M_Out(12241)=0     'ドライバーOFF CW
2888    Dly 0.1
2889    M_Out(12242)=0     'ドライバー解除 C1
2890    Dly 0.1
2891    M_Out(12243)=0     'ドライバー解除 C2 (バンク3)
2892    Dly 0.1
2893    M_Out(12245)=0     'プログラム解除 F1
2894 '---------------------------------------
2895 '---------------------------------------
2896 '-------------
2897    'Mov PInitPos19049
2898    Dly 0.1
2899 '
2900 '
2901     Exit Function
2902 FEnd
2903 '
2904 '■Main
2905 ''' <summary>
2906 ''' 組立動作用のメイン
2907 ''' </summary>
2908 ''' <remarks>
2909 ''' Date   : 2021/07/07 : M.Hayakawa
2910 ''' </remarks>'
2911 Function Main
2912     MopeNo = M_21#         '外部変数にて動作番号代入
2913     '
2914     If M_Svo=0 Then
2915         Servo On
2916     EndIf
2917     Wait M_Svo=1
2918 '組立スタート日付時刻要求パルスON
2919     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
2920 'パトライト操作
2921     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
2922     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
2923     '
2924     M_20# = 0                                   'KEY入力初期化
2925     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
2926     MRet% = 0
2927 '復帰動作　実行・未実行判別      2022/03/22 渡辺 作成
2928     PActive = P_Curr                    '現在位置を取得
2929     MRecoveryPass% = 0
2930     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
2931         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
2932             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
2933             MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
2934         EndIf
2935     EndIf
2936     EndIf
2937     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
2938         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
2939             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
2940                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
2941             EndIf
2942         EndIf
2943     EndIf
2944     If MRecoveryPass% = 0 Then
2945         fnInitialZone()        '復帰動作パスフラグが立っていない時は復帰動作を実行
2946     EndIf
2947     '
2948     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
2949         M_Out(12364) = 1            'toPLC_データ保存ON
2950 'トルクチェック
2951         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
2952             MRet% = fnTorqueCheck()
2953             Break
2954         Else
2955 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
2956 '                MRtn = InspInit()               '画像処理初期化処理
2957 '            EndIf
2958             '
2959            M_20# = MClear%                    '初期化
2960 '組立開始
2961             If M_In(MIN_ASSY_CANCEL%) = 0 Then
2962                 MRet% = fnAssyStart()
2963             Else
2964                 M_20# = MPass%
2965             EndIf
2966 '組立終了日付時刻
2967             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
2968             Wait M_In(11572) = 1            '日付取得完了
2969             Dly 0.1
2970             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
2971 'リフターユニットへのOUT
2972             '  KEY入力が何もない場合 OKと判断
2973             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
2974             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
2975 'OK/NGフラグ出力
2976             If M_20# <= 0 Then
2977                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
2978             ElseIf M_20# = MPass% Then
2979                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
2980             EndIf
2981 'PIASに組立完了書込み
2982             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
2983                 If M_20# = MPass% Then
2984                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
2985                 Else
2986                     'KEY入力がNGの場合
2987                     If M_20# = MNgProcess% Then
2988                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
2989                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
2990                         MRet% = fnPiasWrite(MNG%)
2991                        nAssyNgQty = nAssyNgQty + 1
2992                     EndIf
2993                     '
2994                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/07中村)
2995                     If M_20# = MAssyOK% Then
2996                             '-----------------------
2997                             'D732 -> D2600 コピー要求
2998                             M_Out(12566) = 1
2999 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3000                             M_Out(12566) = 0
3001                             '
3002                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3003                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3004                             '基板番号照合(PPは未使用）
3005 '                            MRet% = fnPCBNumberCheck()
3006                         Else
3007                             MRet% = 1
3008                         EndIf
3009                         '
3010                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3011                             If M_20# <> MAbout% Then
3012                                 '工程履歴OK書き込み
3013                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3014                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3015                                 MRet% = fnPiasWrite(MOK%)
3016                                 nAssyOkQty = 0
3017                                 nAssyOkQty = nAssyOkQty + 1
3018                             Else
3019                                 nAssyOkQty = nAssyOkQty + 1
3020                             EndIf
3021                         EndIf
3022                     EndIf
3023 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3024 '                    MRet% = fnPiasWrite(MOK%)
3025                 EndIf
3026             Else
3027                 nAssyOkQty = nAssyOkQty + 1
3028             EndIf
3029             '
3030             '組立終了日付時刻解除
3031             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3032             '投入数、組立OK数、組立NG数書込み
3033 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3034             '
3035 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3036 '                '画像処理終了処理
3037 '                MRtn = InspQuit()
3038 '            EndIf
3039         EndIf
3040         M_Out(12364) = 0                          'toPLC_データ保存OFF
3041     EndIf
3042 'パトライト操作
3043     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3044     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3045 'GOT表示
3046     fnAutoScreenComment(93)  'AUTO画面 工程完了
3047 FEnd
3048 End
3049 '
3050 'おまじないコメント
3051 '絶対削除するな
3052 '
3053 '
3054 '
3055 '
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
PGetScrewPos(1)=(+180.56,+239.25,+380.00,+180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
PGetScrewPos(2)=(+182.97,+239.67,+400.00,+180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
PGetScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(9)=(+78.32,+270.81,+429.99,-180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
PGetScrewPos(10)=(+180.56,+239.25,+338.35,+180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
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
Pmove=(+343.72,-16.25,+500.00,-180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
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
PSocPress=(+391.84,+11.73,+353.15,+180.00,-0.01,-180.00)(7,0)
PSocPress_1=(+391.84,+11.73,+369.00,-180.00,+0.00,+180.00)(7,0)
PSocPress_2=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PSocSet=(+486.15,-97.80,+349.50,+180.00,-0.04,-179.20)(7,0)
PSocSet_1=(+486.15,-97.80,+361.91,+180.00,-0.04,-179.20)(7,0)
PSocSet_2=(+486.15,-97.80,+380.00,+180.00,-0.04,-179.20)(7,0)
PTicketRead=(+603.00,-149.18,+373.00,-179.99,+0.00,+90.00)(7,0)
PTicketRead_1=(+603.00,-149.18,+450.00,-179.99,+0.00,+90.00)(7,0)
PTorqueCheck=(+144.46,-240.78,+340.00,-179.99,-0.01,+90.02)(7,0)
PTorqueCheck_1=(+144.45,-240.80,+360.00,-179.99,+0.00,+90.01)(7,0)
