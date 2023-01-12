1 ' ===================================
2 '
3 '  21049001 STEP5 Assy1プログラム
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
331 Def Inte MRetryLimit                    ' リトライ回数
332 Def Inte MRetryCount                    ' リトライカウント
333 '
334 Dim MScwT_Case1%(2)               '条件1停止変数
335 Dim MScwT_Case2%(2)               '条件2停止変数
336 Dim MScwT_Case3%(2)               '条件3停止変数
337 Dim MScwT_Case4%(2)               '条件4停止変数
338 Dim MScwT_Case5%(2)               '条件5停止変数
339 '
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
380 '
381 PCalcGetMainScrew = (+0.00,+0.00,-1.20,+0.00,+0.00,+0.00,+0.00)  'Mainねじ供給機の補正値
382 PCalcGetFanScrew = (+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)   'Fanねじ供給機の補正値
383 '
384 MRetryLimit% = 2
385 '
386 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
387 Function M% fnAssyStart
388     M_20# = MClear%                       '初期化
389     '組み立て開始
390     'プログラム原点
391     Ovrd 100
392     ' 初期位置をIDチケット上とするため削除 9/16 M.Hayakawa
393 '    Mov PInitialPosition        '原点回避
394     '初期位置を設定
395     PTemp = P_Curr
396     MRtn = 0
397     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
398         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
399             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
400                 MRtn = 1
401                 Break
402             EndIf
403             Break
404         EndIf
405         Break
406     EndIf
407     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
408     If MRtn = 1 Then
409         M_Out(12256) = 1 Dly 0.3            '位置決め出ON
410         Mov PTicketRead
411         Break
412     Else
413         Mov PInitialPosition
414         M_Out(12256) = 1 Dly 0.3           '位置決め出ON
415         Mov PTicketRead_1           'チケットID読み取り回避点
416         Mvs PTicketRead             'ID読み位置
417         Break
418     EndIf
419     *RE_PUSH
420 '    If M_20# = MContinue% Then M_Out(12257) = 0
421     If M_20# = MContinue% Then M_Out(12256) = 1 Dly 0.3
422     If M_20# = MContinue% Then M_20# = MClear%
423     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    '位置決め出端検出(8/26中村)
424     If MRtn = 1 Then GoTo *CompPush
425         M_Out(12257) = 1 Dly 0.3    ' Y71 1:位置決めCY 解除
426         fErrorProcess(11,231,282,0)
427     If M_20# = MNext% Then M_Out(12256) = 1 Dly 0.3 'Y70 1:位置決めCY 固定
428     If M_20# = MNext% Then M_20# = MClear%
429     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
430     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
431     If M_20# = MContinue% Then GoTo *RE_PUSH
432     *CompPush
433 '
434     *RE_READ
435     If M_20# = MContinue% Then M_20# = MClear%
436 '
437     MRtn = 1                            'MRtn初期化
438     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
439         MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
440     EndIf
441         '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
442         '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
443 '
444     If MRtn = 1 Then GoTo *CompRead
445     Mvs PTicketRead_1                       ' 一旦上空に待避 追加 22/07/16 M,H
446     'fErrorProcess(11,97,25,0)
447     If M_20# = MPass% Then GoTo *AssyEnd    ' 次へを押された時のコメント解除＋ジャンプ先変更 2022/07/20 M.H
448     If M_20# = MNext% Then M_20# = MClear%
449     If M_20# = MAbout% Then GoTo *AssyEnd       ' コメント解除＋ジャンプ先変更 22/07/16 M,H
450     If M_20# = MNgProcess% Then GoTo *AssyEnd   ' コメント解除＋ジャンプ先変更 22/07/16 M,H
451     If M_20# = MContinue% Then GoTo *RE_READ
452 '    If M_20# = MNext% Then M_20# = MPass%
453     GoTo *ASSY_ERROR_END
454     *CompRead
455     '
456 '【MAIN基板ID読み込み】
457     *RE_MEIN_CHECK
458     PInspPosition(1) = PMainPcbRead 'MAIN基板読込位置
459     MInspGroup%(1) = 2              '検査G番号
460     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
461 '
462     If MRtn = 1 Then GoTo *CompMainCheck
463     fErrorProcess(11,38,25,0)
464     If M_20# = MNext% Then M_20# = MClear%
465     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
466     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
467     If M_20# = MContinue% Then GoTo *RE_MEIN_CHECK
468     *CompMainCheck
469 '【GYRO基板ID読み込み】
470     *RE_GYRO_CHECK
471     PInspPosition(1) = PGyroPcbRead 'GYRO基板読込位置
472     MInspGroup%(1) = 3              '検査G番号
473     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
474 '
475     If MRtn = 1 Then GoTo *CompGyroCheck
476     fErrorProcess(11,38,25,0)
477     If M_20# = MNext% Then M_20# = MClear%
478     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
479     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
480     If M_20# = MContinue% Then GoTo *RE_GYRO_CHECK
481     *CompGyroCheck
482 '【基板IDコピー】
483     *RE_PCB_RECORD
484     M_Out(12571) = 1    ' 領域1 基板番号コピー (D2600-) On
485     Dly 0.1
486     M_Out(12572) = 1    ' 領域2 基板番号コピー (D2612-) On
487     Dly 0.1
488     M_Out(12566) = 1    ' toPLC_基板番号コピー要求 On
489 '
490     MRtn = frInCheck(11581,1,MSETTIMEOUT05&)    ' toRBT_基板番号コピー完了 On
491     If MRtn = 1 Then
492         M_Out(12571) = 0  ' 領域1 基板番号コピー (D2600-) Off
493         Dly 0.1
494         M_Out(12572) = 0  ' 領域2 基板番号コピー (D2612-) Off
495         Dly 0.1
496         M_Out(12566) = 0  ' toPLC_基板番号コピー要求 Off
497 '        GoTo *RE_PCB_COMPAIRE   ' 基板番号照合にスキップ
498     Else
499         fErrorProcess(11,39,25,0)
500         If M_20# = MNext% Then M_20# = MClear%
501         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
502         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
503         If M_20# = MContinue% Then GoTo *RE_PCB_RECORD
504     EndIf
505 '【基板ID照合（紐付け）】
506     MRetryCount% = 0
507     While (MRetryCount% <= MRetryLimit%)
508         *RE_PCB_COMPAIRE
509         M_Out(12557)= 1 ' 基板番号照合ビットON
510         MRtn = frInCheck(11566,1,MSETTIMEOUT05&)    ' toRBT_基板番号照合OK(M420) On
511         If MRtn = 1 Then
512             M_Out(12557)= 0     ' 基板番号照合ビットOff
513             ' リトライ回数設定でループを抜ける
514             MRetryCount% = 99
515         Else
516             If MRetryCount% = MRetryLimit% Then
517                 If M_In(11565) = 1 Then
518                     fErrorProcess(11,37,25,0)
519                 Else
520                     fErrorProcess(11,38,25,0)
521                 EndIf
522                 If M_20# = MNext% Then
523                     M_20# = MClear%
524                     ' リトライ回数設定でループを抜ける
525                     MRetryCount% = 99
526                 EndIf
527                 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
528                 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
529                 If M_20# = MContinue% Then
530                     MRetryCount% = 0
531                 EndIf
532             Else
533                 ' リトライ回数インクリメント
534                 MRetryCount% = MRetryCount% + 1
535                 Dly 0.1  ' 他の工程とタイミングをずらす為のディレイ
536             EndIf
537         EndIf
538     WEnd
539 '
540     *RE_CHECK
541     PInspPosition(1) = PParts1Check '部品1画像チェック位置(MAIN基板周辺）
542     MInspGroup%(1) = 4              '検査G番号
543     PInspPosition(2) = PParts2Check '部品2画像チェック位置（背面板周辺）
544     MInspGroup%(2) = 5              '検査G番号
545 '    PInspPosition(3) = PParts3Check '部品3画像チェック位置（SOC基板周辺）
546 '    MInspGroup%(3) = 6              '検査G番号
547 '    PInspPosition(4) = PParts4Check '部品4画像チェック位置（側板周辺）
548 '    MInspGroup%(4) = 7              '検査G番号
549     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 2, -1, 1 )  '画像処理検査実行
550     If MRtn = 1 Then GoTo *CompCheck
551     fErrorProcess(11,43,23,0)
552     If M_20# = MNext% Then M_20# = MClear%
553     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
554     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
555     If M_20# = MContinue% Then GoTo *RE_CHECK
556     *CompCheck
557     '
558     MRtn = FnCtlValue2(1)          '投入数＋１  2022/04/28 渡辺
559     '製品位置決め(ID読込後に変更 9/16 M.Hayakawa）
560     *RE_POS
561     If M_20# = MContinue% Then M_20# = MClear%
562     M_Out(12256)=1 Dly 0.3      '位置決めCY用SV出端パルス出力
563     MRtn = FnCtlValue2(99)       '読書開始信号OFF  2022/04/28 渡辺
564 '
565     'Wait M_In(11266)=1          '位置決め出端検出修正につきコメントアウト(8/26中村))
566     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    '位置決め出端検出(8/26中村)
567     If MRtn = 1 Then GoTo *Comp_Pos_1
568     fErrorProcess(11,231,282,0)
569     If M_20# = MNext% Then M_20# = MClear%
570     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
571     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
572     If M_20# = MContinue% Then GoTo *RE_POS
573     *Comp_Pos_1
574     '
575     M_Out(12258)=1 Dly 0.3      'プッシュCY用SV出端パルス出力(タクト短縮のため位置移動(12/13中村))
576     M_Out(12260)=1 Dly 0.3      'FANクランプ戻端パルス出力(タクト短縮のため位置移動(12/13中村))
577     Mov PScrewSupplyMain_1
578 '
579 '    M_Out(12258)=1 Dly 0.3      'プッシュCY用SV出端パルス出力
580     'Wait M_In(11268)=1          'プッシュ出端検出(修正につきコメントアウト(8/26中村))
581     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'プッシュ出端検出
582     If MRtn = 1 Then GoTo *Comp_Pos_2
583     fErrorProcess(11,231,282,0)
584     If M_20# = MNext% Then M_20# = MClear%
585     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
586     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
587     If M_20# = MContinue% Then GoTo *RE_POS
588     *Comp_Pos_2
589     '
590 '    M_Out(12260)=1 Dly 0.3      'FANクランプ戻端パルス出力(メインねじ締め後に変更 M.Hayakawa)(タクト短縮のため位置移動(12/13中村))
591     'Wait M_In(11270)=1          'FANクランプ戻端検出(修正につきコメントアウト(8/26中村))
592     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)    'FANクランプ戻端検出(8/26中村)
593     If MRtn = 1 Then GoTo *Comp_Pos_3
594     fErrorProcess(11,231,282,0)
595     If M_20# = MNext% Then M_20# = MClear%
596     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
597     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
598     If M_20# = MContinue% Then GoTo *RE_POS
599     *Comp_Pos_3
600     '
601     '
602     'Main基板のネジ締め
603     'Main基板用ネジ供給機へネジを取りに行く
604     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
605     '
606     '*ScrewSupplyMain           '一時コメントアウト(以下5行,8/5中村)
607 '    Mov PScrewSupplyMain_2      'ネジ供給機回避点
608 '    Mov PScrewSupplyMain_1      'ネジピックアップ上空
609 '    Mvs PScrewSupplyMain        'ネジピックアップ
610 '    Mvs PScrewSupplyMain_1      'ネジピックアップ上空
611 '    Mov PScrewSupplyMain_2      'ネジ供給機回避点
612     'Return                     '一時コメントアウト(8/4中村)
613     'ScrewPositionDebug_1()      'デバック用(別関数使用のためコメントアウト(8/26中村))
614     '
615     PGetScrewPosTemp(1) = PScrewSupplyMain_1   'ネジピックアップ上空を代入(8/26中村)
616     PGetScrewPosTemp(2) = PScrewSupplyMain_2   'ネジ供給回避点を代入(8/26中村)
617     PGetScrewPosTemp(9) = PScrewSupplyMain_9   'ネジ供給機上空ネジ捨て位置(10/6 M.H追加)
618     PGetScrewPosTemp(10) = PScrewSupplyMain    'ネジピックアップを代入(8/26中村)
619     '
620     *RE_SCREW_GET_1                                'リトライ用ラベル
621     If M_20# = MContinue% Then M_20# = MClear%
622     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
623     If M_20# = MClear% Then GoTo *Comp_Screw_1
624     If M_20# = MNext% Then M_20# = MClear%
625     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
626     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
627     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
628     *Comp_Screw_1
629     '
630     '①番ネジ締め
631 '    Mov PScrewMain1_1           '①上空(以下5行一時コメントアウト(8/26中村))
632 '    Ovrd 5
633 '    Mvs PScrewMain1             '①ネジ着座
634 '    Ovrd 10
635 '    Mvs PScrewMain1_1           '①上空
636     PScrewPosTemp(1) = PScrewMain1_1    'ネジ1締め開始位置上空を代入(8/26中村)
637     PScrewPosTemp(2) = PScrewMain1_0    'ネジ1締め開始位置を代入(8/26中村)
638     PScrewPosTemp(10) = PScrewMain1     'ネジ1締め終了位置を代入(8/26中村)
639     M_Out16(12672) = 1                  'ネジ締め位置番号送信
640     MRtn = ScrewTight(PScrewPosTemp,1,10.0)    'ネジ1締めの実行(8/26中村)
641     M_Out16(12672) = 0                  'ネジ締め位置番号クリア
642     If MRtn = 1 Then GoTo *CompScrew1
643     Mov PInitialPosition
644     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
645     MScrewErrorCord% = MScrewErrorCord% + 1
646     If MRtn = 0 Then
647         fErrorProcess(11,MScrewErrorCord%,52,0)
648     ElseIf MRtn = -1 Then
649         fErrorProcess(11,94,95,0)
650     EndIf
651 '    fErrorProcess(11,53,52,0)
652     If M_20# = MNext% Then M_20# = MClear%
653     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
654     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
655     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
656     *CompScrew1
657     '
658     'Main基板用ネジ供給機へネジを取りに行く
659     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
660     'ScrewPositionDebug_1()      'デバック用(別関数使用のためコメントアウト(8/26中村))
661     *RE_SCREW_GET_2                                'リトライ用ラベル
662     If M_20# = MContinue% Then M_20# = MClear%
663     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
664     If M_20# = MClear% Then GoTo *Comp_Screw_2
665     If M_20# = MNext% Then M_20# = MClear%
666     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
667     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
668     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
669     *Comp_Screw_2
670     '②番ネジ締め
671 '    Mov PScrewMain2_1           '②上空(以下5行一時コメントアウト(8/26中村))
672 '    Ovrd 5
673 '    Mvs PScrewMain2             '②ネジ着座
674 '    Ovrd 10
675 '    Mvs PScrewMain2_1           '②上空
676     PScrewPosTemp(1) = PScrewMain2_1    'ネジ2締め開始位置上空を代入(8/26中村)
677     PScrewPosTemp(2) = PScrewMain2_0    'ネジ2締め開始位置を代入(8/26中村)
678     PScrewPosTemp(10) = PScrewMain2     'ネジ1締め終了位置を代入(8/26中村)
679     M_Out16(12672) = 2                  'ネジ締め位置番号送信
680     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        'ネジ締め2の実行(8/26中村)
681     M_Out16(12672) = 0                  'ネジ締め位置番号クリア
682     If MRtn = 1 Then GoTo *CompScrew2
683     Mov PInitialPosition
684     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
685     MScrewErrorCord% = MScrewErrorCord% + 2
686     If MRtn = 0 Then
687         fErrorProcess(11,MScrewErrorCord%,52,0)
688     ElseIf MRtn = -1 Then
689         fErrorProcess(11,94,95,0)
690     EndIf
691 '    fErrorProcess(11,54,52,0)
692     If M_20# = MNext% Then M_20# = MClear%
693     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
694     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
695     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
696     *CompScrew2
697     '
698     'Main基板用ネジ供給機へネジを取りに行く
699     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
700     *RE_SCREW_GET_3                                'リトライ用ラベル
701     If M_20# = MContinue% Then M_20# = MClear%
702     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
703     If M_20# = MClear% Then GoTo *Comp_Screw_3
704     If M_20# = MNext% Then M_20# = MClear%
705     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
706     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
707     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
708     *Comp_Screw_3
709     '③番ネジ締め
710 '    Mov PScrewMain3_1           '③上空(以下5行一時コメントアウト(8/26中村))
711 '    Ovrd 5
712 '    Mvs PScrewMain3             '③ネジ着座
713 '    Ovrd 10
714 '    Mvs PScrewMain3_1           '③上空
715     PScrewPosTemp(1) = PScrewMain3_1    'ネジ3締め開始位置上空を代入(8/26中村)
716     PScrewPosTemp(2) = PScrewMain3_0    'ネジ3締め開始位置を代入(8/26中村)
717     PScrewPosTemp(10) = PScrewMain3     'ネジ3締め終了位置を代入(8/26中村)
718     M_Out16(12672) = 3                  'ネジ締め位置番号送信
719     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        'ネジ締め3の実行(8/26中村)
720     M_Out16(12672) = 0                  'ネジ締め位置番号クリア
721     If MRtn = 1 Then GoTo *CompScrew3
722     Mov PInitialPosition
723     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
724     MScrewErrorCord% = MScrewErrorCord% + 3
725     If MRtn = 0 Then
726         fErrorProcess(11,MScrewErrorCord%,52,0)
727     ElseIf MRtn = -1 Then
728         fErrorProcess(11,94,95,0)
729     EndIf
730 '    fErrorProcess(11,55,52,0)
731     If M_20# = MNext% Then M_20# = MClear%
732     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
733     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
734     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
735     *CompScrew3
736     '
737     'Main基板用ネジ供給機へネジを取りに行く
738     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
739     *RE_SCREW_GET_4                                'リトライ用ラベル
740     If M_20# = MContinue% Then M_20# = MClear%
741     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
742     If M_20# = MClear% Then GoTo *Comp_Screw_4
743     If M_20# = MNext% Then M_20# = MClear%
744     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
745     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
746     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
747     *Comp_Screw_4
748     '④番ネジ締め
749 '    Mov PScrewMain4_1           '④上空(以下5行一時コメントアウト(8/26中村))
750 '    Ovrd 5
751 '    Mvs PScrewMain4             '④ネジ着座
752 '    Ovrd 10
753 '    Mvs PScrewMain4_1           '④上空
754     PScrewPosTemp(1) = PScrewMain4_1    'ネジ4締め開始位置上空を代入(8/26中村)
755     PScrewPosTemp(2) = PScrewMain4_0    'ネジ4締め開始位置を代入(8/26中村)
756     PScrewPosTemp(10) = PScrewMain4     'ネジ4締め終了位置を代入(8/26中村)
757     M_Out16(12672) = 4                  'ネジ締め位置番号送信
758     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        'ネジ締め4の実行(8/26中村)
759     M_Out16(12672) = 0                  'ネジ締め位置番号クリア
760     If MRtn = 1 Then GoTo *CompScrew4
761     Mov PInitialPosition
762     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
763     MScrewErrorCord% = MScrewErrorCord% + 4
764     If MRtn = 0 Then
765         fErrorProcess(11,MScrewErrorCord%,52,0)
766     ElseIf MRtn = -1 Then
767         fErrorProcess(11,94,95,0)
768     EndIf
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
796     M_Out16(12672) = 5                  'ネジ締め位置番号送信
797     MRtn = ScrewTight(PScrewPosTemp,6,10.0)        'ネジ締め5の実行(8/26中村)
798     M_Out16(12672) = 0                  'ネジ締め位置番号クリア
799     If MRtn = 1 Then GoTo *CompScrew5
800     Mov PInitialPosition
801     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
802     MScrewErrorCord% = MScrewErrorCord% + 5
803     If MRtn = 0 Then
804         fErrorProcess(11,MScrewErrorCord%,52,0)
805     ElseIf MRtn = -1 Then
806         fErrorProcess(11,94,95,0)
807     EndIf
808 '    fErrorProcess(11,57,52,0)
809     If M_20# = MNext% Then M_20# = MClear%
810     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
811     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
812     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
813     *CompScrew5
814     '
815     'Main基板用ネジ供給機へネジを取りに行く
816     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
817 '以下3行PP品にネジ穴がないため一時削除 9/16 M.Hayakawa
818     *RE_SCREW_GET_6                                'リトライ用ラベル
819     If M_20# = MContinue% Then M_20# = MClear%
820     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
821     If M_20# = MClear% Then GoTo *Comp_Screw_6
822     If M_20# = MNext% Then M_20# = MClear%
823     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
824     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
825     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
826     *Comp_Screw_6
827     '⑥番ネジ締め
828 '    Mov PScrewMain6_1           '⑥上空(以下5行一時コメントアウト(8/26中村))
829 '    Ovrd 5
830 '    Mvs PScrewMain6             '⑥ネジ着座
831 '    Ovrd 10
832 '    Mvs PScrewMain6_1           '⑥上空
833 '以下3行PP品にネジ穴がないため一時削除 9/16 M.Hayakawa
834     PScrewPosTemp(1) = PScrewMain6_1    'ネジ6締め開始位置上空を代入(8/26中村)
835     PScrewPosTemp(2) = PScrewMain6_0    'ネジ6締め開始位置を代入(8/26中村)
836     PScrewPosTemp(10) = PScrewMain6     'ネジ6締め終了位置を代入(8/26中村)
837     M_Out16(12672) = 6                  'ネジ締め位置番号送信
838     MRtn = ScrewTight(PScrewPosTemp,6,10.0)        'ネジ締め6の実行(8/26中村)
839     M_Out16(12672) = 0                  'ネジ締め位置番号クリア
840     If MRtn = 1 Then GoTo *CompScrew6
841     Mov PInitialPosition
842     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
843     MScrewErrorCord% = MScrewErrorCord% + 6
844     If MRtn = 0 Then
845         fErrorProcess(11,MScrewErrorCord%,52,0)
846     ElseIf MRtn = -1 Then
847         fErrorProcess(11,94,95,0)
848     EndIf
849 '    fErrorProcess(11,58,52,0)
850     If M_20# = MNext% Then M_20# = MClear%
851     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
852     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
853     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
854     *CompScrew6
855     '
856     'FAN用ネジ供給機へネジを取りに行く
857     'GoSub *ScrewSupplyFan      '一時コメントアウト(8/4中村)
858 ' ネジ位置指定前に取りにいっている？ 1行一時削除 9/16 M.Hayakawa
859 '    MRtn = ScrewGet(PGetScrewPosTemp)       'ネジを取りに行く(8/26中村)
860     '
861 '    *ScrewSupplyFan
862 '    Mov PScrewSupplyFan_2       'ネジ供給機回避点
863 '    Mov PScrewSupplyFan_1       'ネジピックアップ上空
864 '    Mvs PScrewSupplyFan         ''ネジピックアップ
865 '    Mvs PScrewSupplyFan_1       'ネジピックアップ上空
866 '    Mov PScrewSupplyFan_2       'ネジ供給機回避点
867    ' Return                     '一時コメントアウト(8/4中村)
868     'ScrewPositionDebug_2()       'デバック用(別関数使用のためコメントアウト(8/26中村))
869     '
870     PGetScrewPosTemp(1) = PScrewSupplyFan_1   'ネジピックアップ上空を代入(8/26中村)
871     PGetScrewPosTemp(2) = PScrewSupplyFan_2   'ネジ供給回避点を代入(8/26中村)
872     PGetScrewPosTemp(9) = PScrewSupplyFan_9   'ネジ供給機上空ネジ捨て位置(10/6 M.H追加)
873     PGetScrewPosTemp(10) = PScrewSupplyFan    'ネジピックアップを代入(8/26中村)
874 '
875     *RE_SCREW_GET_7                                'リトライ用ラベル
876 '
877     If M_20# = MContinue% Then M_20# = MClear%
878     ScrewGet(PGetScrewPosTemp , 11260 , 0)          'ネジ受け取り開始
879     If M_20# = MClear% Then GoTo *Comp_Screw_7
880     If M_20# = MNext% Then M_20# = MClear%
881     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
882     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
883     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_7
884     *Comp_Screw_7
885     '⑦番ネジ締め
886 '    Mov PScrewFan1_1            '⑦上空(以下5行一時コメントアウト(8/26中村))
887 '    Ovrd 5
888 '    Mvs PScrewFan1              '⑦ネジ着座
889 '    Ovrd 10
890 '    Mvs PScrewFan1_1            '⑦上空
891     PScrewPosTemp(1) = PScrewFan1_1    'Fan1ネジ締め開始位置上空を代入(8/26中村)
892     PScrewPosTemp(2) = PScrewFan1_0    'Fan1ネジ締め開始位置を代入(8/26中村)
893     PScrewPosTemp(10) = PScrewFan1     'Fan1ネジ締め終了位置を代入(8/26中村)
894     M_Out16(12672) = 7                  'ネジ締め位置番号送信
895     MRtn = ScrewTight(PScrewPosTemp,2,10.0)       'Fanネジ締め1の実行(8/26中村)送り速度6.7から10.0へ7/27中村
896     M_Out16(12672) = 0                  'ネジ締め位置番号クリア
897     If MRtn = 1 Then GoTo *CompScrew7
898     Mov PInitialPosition
899     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
900     MScrewErrorCord% = MScrewErrorCord% + 7
901     If MRtn = 0 Then
902         fErrorProcess(11,MScrewErrorCord%,52,0)
903     ElseIf MRtn = -1 Then
904         fErrorProcess(11,94,95,0)
905     EndIf
906 '    fErrorProcess(11,59,52,0)
907     If M_20# = MNext% Then M_20# = MClear%
908     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
909     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
910     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_7
911     *CompScrew7
912 '
913     '
914     'FAN用ネジ供給機へネジを取りに行く
915     'GoSub *ScrewSupplyFan      '一時コメントアウト(8/4中村)
916     'ScrewPositionDebug_2()      'デバック用(別関数使用のためコメントアウト(8/26中村))
917     *RE_SCREW_GET_8                                'リトライ用ラベル
918     If M_20# = MContinue% Then M_20# = MClear%
919     ScrewGet(PGetScrewPosTemp , 11260 , 0)          'ネジ受け取り開始
920     If M_20# = MClear% Then GoTo *Comp_Screw_8
921     If M_20# = MNext% Then M_20# = MClear%
922     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
923     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
924     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_8
925     *Comp_Screw_8
926     '⑧番ネジ締め
927 '    Mov PScrewFan2_1            '⑧上空(以下5行一時コメントアウト(8/26中村))
928 '    Ovrd 5
929 '    Mvs PScrewFan2              '⑧ネジ着座
930 '    Ovrd 10
931 '    Mvs PScrewFan2_1            '⑧上空
932     PScrewPosTemp(1) = PScrewFan2_1    'Fan2ネジ締め開始位置上空を代入(8/26中村)
933     PScrewPosTemp(2) = PScrewFan2_0    'Fan2ネジ締め開始位置を代入(8/26中村)
934     PScrewPosTemp(10) = PScrewFan2     'Fan2ネジ締め終了位置を代入(8/26中村)
935     M_Out16(12672) = 8                  'ネジ締め位置番号送信
936     MRtn = ScrewTight(PScrewPosTemp,2,10.0)       'Fanネジ締め2の実行(8/26中村)送り速度6.7から10.0へ7/27中村
937     M_Out16(12672) = 0                  'ネジ締め位置番号クリア
938     If MRtn = 1 Then GoTo *CompScrew8
939     Mov PInitialPosition
940     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
941     MScrewErrorCord% = MScrewErrorCord% + 8
942     If MRtn = 0 Then
943         fErrorProcess(11,MScrewErrorCord%,52,0)
944     ElseIf MRtn = -1 Then
945         fErrorProcess(11,94,95,0)
946     EndIf
947     If M_20# = MNext% Then M_20# = MClear%
948     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
949     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
950     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_8
951     *CompScrew8
952 '
953     'プログラム原点
954     'Mov PInitialPosition        ' 原点回避
955     MRtn = FnCtlValue2(2)       '組立ＯＫ＋１  2022/04/28 渡辺
956     Mov PTicketRead_1           ' チケットリード位置
957     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
958     InitialState()              ' 初期状態にする
959     M_20# = MAssyOK%              ' 正常終了処理
960     GoTo *fnAssyStart_FEndPosi
961 '
962 *ASSY_ERROR_END
963     fnInitialZone()   ' 初期位置に移動
964 *AssyEnd
965     InitialState()  ' 初期状態にする
966 *fnAssyStart_FEndPosi
967     Exit Function
968 FEnd
969 '
970 '■fnPiasCheck
971 ''' <summary>
972 ''' PIASチケット読込み
973 ''' </summary>
974 ''' <returns>   0 : NG
975 '''             1 : OK(読込み完了)
976 ''' </returns>
977 ''' <remarks>
978 ''' Date   : 2021/07/07 : M.Hayakawa
979 ''' </remarks>'
980 ''' <Update>
981 ''' Date   : 2022/01/11 : 中村
982 ''' </Update>
983 Function M% fnPiasCheck
984     fnPiasCheck = 0
985     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
986     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功
987 '
988 *RETRY_PIAS
989     M_20# = MClear%
990     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
991     '
992     '【IDチケット読み込み】
993     PInspPosition(1) = PTicketRead  'IDチケット読取位置
994     MInspGroup%(1) = 1              '検査G番号
995     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
996 '
997     'エラーの場合
998     If MRtn <> 1 Then
999         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1000         If MRtn <> 1 Then
1001             'D720 -> D1300 コピー要求
1002             M_Out(12565) = 1
1003             Dly 0.5
1004             M_Out(12565) = 0
1005             'エラー処理記述
1006             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1007             'GOT KEY入力待ち
1008             MKeyNumber = fnKEY_WAIT()
1009             '
1010             Select MKeyNumber
1011                 Case MNext%         '次へを選択した場合
1012                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1013                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1014                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1015                     Break
1016                 Case MAbout%        '停止を選択した場合
1017                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1018                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1019                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1020                     Break
1021                 Case MNgProcess%    'NGを選択した場合
1022                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1023                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1024                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1025                     Break
1026                 Case MContinue%     '継続を選択した場合
1027                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1028                     M_20# = MContinue%
1029                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1030                     Break
1031             End Select
1032         EndIf
1033     EndIf
1034     If M_20# = MPass% Then GoTo *fnPiasCheck_End            'PIASチェック終了
1035     If M_20# = MAbout% Then GoTo *fnPiasCheck_End           'PIASチェック終了
1036     If M_20# = MNgProcess% Then GoTo *fnPiasCheck_End       'PIASチェック終了
1037     If M_20# = MContinue% Then GoTo *RETRY_PIAS             'PIASチェックリトライ
1038 '----------D720 -> D1300 コピー要求----------
1039     M_Out(12565) = 1
1040     Dly 0.5
1041     M_Out(12565) = 0
1042 '----------通信確認をする----------
1043     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1044     MRtn = 0                ' 初期化
1045     M_20# = MClear%         ' 初期化
1046     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1047     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1048 '    If MRtn <> 1 Then
1049 '        If M_20# = MContinue% Then
1050 '            GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1051 '        Else
1052 '            GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1053 '        EndIf
1054 '    EndIf
1055     If MRtn = 1 Then GoTo *PCComu_OK                '通信OK時ラベルへジャンプ
1056     If M_20# = MContinue% Then GoTo *RETRY_PIAS      'チケット読み直しからリトライ
1057     GoTo *fnPiasCheck_End                           'その他の場合PIASチェック終了
1058     *PCComu_OK
1059 '----------工程抜け確認----------
1060     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1061     MRtn = 0                ' 初期化
1062     M_20# = MClear%         ' 初期化
1063     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1064     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1065 '    If MRtn <> 1 Then
1066 '        If M_20# = MContinue% Then
1067 '            GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1068 '        Else
1069 '            GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1070 '        EndIf
1071 '    EndIf
1072     If MRtn = 1 Then GoTo *ProcessCheck_OK                '工程チェックOK時ラベルへジャンプ
1073     If M_20# = MContinue% Then GoTo *RETRY_PIAS      'チケット読み直しからリトライ
1074     GoTo *fnPiasCheck_End                           'その他の場合PIASチェック終了
1075     *ProcessCheck_OK
1076     '
1077     fnPiasCheck = 1
1078     *fnPiasCheck_End
1079     Exit Function
1080 FEnd
1081 '
1082 '■fnPCComuCheck
1083 ''' <summary>
1084 ''' PC-PLC通信チェック
1085 ''' </summary>
1086 ''' <returns>   0 : NG
1087 '''             1 : OK(読込み完了)
1088 ''' </returns>
1089 ''' <remarks>
1090 ''' Date   : 2021/07/07 : M.Hayakawa
1091 ''' </remarks>'
1092 Function M% fnPCComuCheck
1093     fnPCComuCheck = 0
1094     MJudge% = 0                                  '初期化
1095     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1096     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1097     '
1098     For MStaNo = 0 To 5
1099         '
1100         If M_In(MIN_PIAS_ComOK%) = 1 Then
1101             'PC通信OK(M400)
1102             MJudge% = MOK%
1103             MStaNo = 5
1104             Break
1105         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1106             'toRBT_通信確認time out
1107             MJudge% = MNG%
1108             MCommentD1001 = 15
1109             MCommentD1002 = 21
1110             MStaNo = 5
1111             Break
1112         Else
1113             'toRBT_通信確認time out
1114             MJudge% = MNG%
1115             MCommentD1001 = 14
1116             MCommentD1002 = 21
1117             Break
1118         EndIf
1119     Next MStaNo
1120     '
1121     '上記で返信フラグを受信してからPC通信確認OFF
1122     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1123     '
1124     'エラー画面
1125     If MJudge% <> MOK% Then
1126         M_20# = MClear%     '初期化
1127         'エラー処理記述
1128         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1129         'GOT KEY入力待ち
1130         MKeyNumber = fnKEY_WAIT()
1131         '
1132         If MKeyNumber = MAbout% Then            '停止を選択した場合
1133             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1134             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1135             Break
1136         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1137             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1138             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1139             Break
1140         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1141             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1142             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1143             Break
1144         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1145             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1146             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1147             Break
1148         EndIf
1149     Else
1150         'OKの場合
1151         fnPCComuCheck = 1
1152     EndIf
1153     Exit Function
1154 FEnd
1155 '
1156 '■fnProcessCheck
1157 ''' <summary>
1158 ''' 工程抜け確認
1159 ''' </summary>
1160 ''' <returns>    1：工程履歴OK     0：異常終了
1161 '''             -1：前工程履歴NG  -2：自工程履歴あり
1162 '''             -3：モデル仕向NG  -4：タイムアウト
1163 '''             -5：履歴処理エラー
1164 ''' </returns>
1165 ''' <remarks>
1166 ''' Date   : 2021/07/07 : M.Hayakawa
1167 ''' </remarks>'
1168 Function M% fnProcessCheck
1169     fnProcessCheck = 0
1170     MJudge% = MNG%      '一旦NGを初期化とする
1171 '----------工程抜け確認----------
1172     MCommentD1001 = 0   'コメント初期化
1173     For MStaNo = 0 To 5
1174         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1175         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1176         '
1177         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1178             MJudge% = MOK%
1179             fnAutoScreenComment(85)     ' AUTO画面
1180             MStaNo = 5
1181             Break
1182         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1183             MFlgLoop% = 0
1184             MJudge% = MNG%
1185             MCommentD1001 = 27
1186             MCommentD1002 = 22
1187             fnAutoScreenComment(94)     ' AUTO画面
1188             fnProcessCheck = -2         ' NGは-2を返す
1189             MStaNo = 5
1190             Break
1191         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1192            MJudge% = MNG%
1193             MCommentD1001 = 31
1194             MCommentD1002 = 22
1195             fnAutoScreenComment(83)     ' AUTO画面
1196             fnProcessCheck = -3         ' NGは-3を返す
1197             MStaNo = 5
1198             Break
1199         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1200             '履歴NGは直ぐに終了せず繰り返し確認を行う
1201             '前工程の書込みが終了していない可能性があるため
1202             MJudge% = MNG%
1203             MCommentD1001 = 32
1204             MCommentD1002 = 22
1205             fnAutoScreenComment(84)     ' AUTO画面
1206             fnProcessCheck = -1         ' NGは-1を返す
1207             Dly 1.0
1208             '工程抜け確認OFF
1209             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1210             Dly 1.0
1211            'MStaNo = 5
1212             Break
1213         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1214             MFlgLoop% = 0
1215             MJudge% = MNG%
1216             MCommentD1001 = 29
1217             MCommentD1002 = 22
1218             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1219             fnProcessCheck = -5         ' NGは-5を返す
1220             MStaNo = 5
1221             Break
1222         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1223             MJudge% = MNG%
1224             If MCommentD1001 = 32 Then
1225                 '何もしない
1226             Else
1227                 MCommentD1001 = 26
1228             EndIf
1229             MCommentD1002 = 22
1230             fnProcessCheck = -4         ' NGは-4を返す
1231             MStaNo = 5
1232             Break
1233         Else
1234             MJudge% = MNG%
1235             MCommentD1001 = 28
1236             MCommentD1002 = 22
1237         EndIf
1238     Next MStaNo
1239     '工程抜け確認OFF
1240     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1241     '通過履歴NG 工程抜けの場合
1242     If MJudge% = MPass% Then
1243         M_20# = MPass%
1244     EndIf
1245     '
1246     'エラー画面
1247     If MJudge% <> MOK% Then
1248         M_20# = MClear%     '初期化
1249         'エラー処理記述
1250         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1251         'GOT KEY入力待ち
1252         MKeyNumber = fnKEY_WAIT()
1253         '
1254         Select MKeyNumber
1255             Case MAbout%        '停止を選択した場合
1256                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1257                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1258                 Break
1259             Case MNext%         '次へを選択した場合
1260                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1261                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1262                 Break
1263             Case MContinue%     '継続を選択した場合
1264                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1265                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1266                 Break
1267             Case MNgProcess%    'NGを選択した場合
1268                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1269                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1270                 Break
1271         End Select
1272     Else
1273         fnProcessCheck = 1  ' OKは1を返す
1274     EndIf
1275     Exit Function
1276 FEnd
1277 '
1278 '■fnPiasWrite
1279 ''' <summary>
1280 ''' Pias 組立結果書込み要求
1281 ''' </summary>
1282 '''<param name="MFlg%">
1283 ''' MOK%(1) = 工程履歴にOKを書込む
1284 ''' MNG%(0) = 工程履歴にNGを書込む
1285 '''</param>
1286 '''<returns></returns>
1287 ''' <remarks>
1288 ''' Date   : 2021/07/07 : M.Hayakawa
1289 ''' </remarks>'
1290 Function M% fnPiasWrite(ByVal MFlg%)
1291       fnPiasWrite = 0
1292 *RETRY_PIASWRITE
1293     '
1294     '組立OK(MOK%)の場合　M306 ON
1295    '組立NG(MNG%)の場合　M307 ON
1296     If MFlg% = MOK% Then
1297         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1298     Else
1299         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1300     EndIf
1301     Dly 0.1                  '念のため
1302     '
1303     'Piasへ書込み開始 M305 -> ON
1304     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1305     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1306     '
1307     MJudge% = MNG%
1308     '
1309     For MStaNo = 0 To 5
1310         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1311             MJudge% = MOK%
1312             'MRet = fnAutoScreenComment(85)  'AUTO画面
1313             MStaNo = 5
1314             Break
1315         '
1316         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1317             MJudge% = MNG%
1318             'MRet = fnAutoScreenComment(85)  'AUTO画面
1319            MCommentD1001 = 34
1320            MCommentD1002 = 25
1321             MStaNo = 5
1322             Break
1323         '
1324         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1325             MJudge% = MNG%
1326             'MRet = fnAutoScreenComment(85)  'AUTO画面
1327            MCommentD1001 = 35
1328            MCommentD1002 = 25
1329             MStaNo = 5
1330             Break
1331         '
1332         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1333             MJudge% = MNG%
1334             'MRet = fnAutoScreenComment(85)  'AUTO画面
1335            MCommentD1001 = 36
1336            MCommentD1002 = 25
1337             MStaNo = 5
1338             Break
1339         '
1340         Else
1341             MJudge% = MNG%
1342            MCommentD1001 = 42
1343            MCommentD1002 = 25
1344         '
1345         EndIf
1346         '
1347     Next MStaNo
1348     '
1349     'Piasへ書込み開始 M305 -> OfF
1350     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1351     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1352     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1353     '
1354     '
1355     '通過履歴NG 工程抜けの場合
1356     If MJudge% = MPass% Then
1357         M_20# = MPass%
1358     EndIf
1359     '
1360    M_20# = MClear%     '初期化
1361     '
1362     'エラー画面
1363     If MJudge% < MOK% Then
1364     '
1365 '残しておくが現状では使用しないラベル
1366 *RETRY_ERR_WRITE
1367         M_20# = MClear%     '初期化
1368         'エラー処理記述
1369         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1370         'GOT KEY入力待ち
1371         MKeyNumber = fnKEY_WAIT()
1372         '
1373         If MKeyNumber = MAbout% Then   '停止を選択した場合
1374             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1375            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1376             Break
1377         '
1378         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1379             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1380             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1381             Break
1382         '
1383         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1384             M_20# = MPass%            'M_20# プログラム間共通外部変数
1385             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1386             Break
1387         '
1388         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1389             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1390            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1391             Break
1392         '
1393         EndIf
1394         '
1395 '        If M_20# = MClear% Then *RETRY_ERR_WRITE
1396         '
1397     EndIf
1398     '
1399     If M_20# = MContinue% Then *RETRY_PIASWRITE
1400     '
1401     fnPiasWrite = 1
1402     Exit Function
1403 FEnd
1404 '
1405 '■fnPCBNumberCheck
1406 ''' <summary>
1407 ''' Pias 基板番号照合要求
1408 ''' </summary>
1409 '''<returns>0（固定）</returns>
1410 ''' <remarks>
1411 ''' Date   : 2021/07/07 : M.Hayakawa
1412 ''' </remarks>'
1413 Function M% fnPCBNumberCheck
1414       fnPCBNumberCheck = 0
1415     '
1416 *RETRY_PCBCHECK
1417     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1418     'Piasへ基板照合開始 M310 -> ON
1419     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1420     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1421     '
1422     MJudge% = MNG%
1423     '
1424     For MStaNo = 0 To 5
1425         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1426             MJudge% = MOK%
1427             fnAutoScreenComment(96)  'AUTO画面
1428             MStaNo = 5
1429             Break
1430         '
1431         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1432             MJudge% = MNG%
1433             fnAutoScreenComment(97)  'AUTO画面
1434             MCommentD1001 = 37
1435             MCommentD1002 = 25
1436             MStaNo = 5
1437             Break
1438         '
1439         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1440             MJudge% = MNG%
1441             fnAutoScreenComment(98)  'AUTO画面
1442             MCommentD1001 = 38
1443             MCommentD1002 = 25
1444             MStaNo = 5
1445             Break
1446         '
1447         ElseIf M_In(11580) = 1 Then                         'time out
1448             MJudge% = MNG%
1449             fnAutoScreenComment(99)  'AUTO画面
1450             MCommentD1001 = 39
1451             MCommentD1002 = 25
1452             MStaNo = 5
1453             Break
1454         '
1455         Else
1456             MJudge% = MNG%
1457            MCommentD1001 = 41
1458            MCommentD1002 = 25
1459         '
1460         EndIf
1461         '
1462     Next MStaNo
1463     '
1464     'Piasへ基板照合開始 M310 -> OfF
1465     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1466     '
1467     '
1468     '通過履歴NG 工程抜けの場合
1469     If MJudge% = MPass% Then
1470         M_20# = MPass%
1471     EndIf
1472     '
1473    M_20# = MClear%     '初期化
1474     '
1475     'エラー画面
1476     If MJudge% < MOK% Then
1477     '
1478 '残しておくが現状では使用しないラベル
1479 *RETRY_ERR_PCBNUMBER
1480         M_20# = MClear%     '初期化
1481         'エラー処理記述
1482         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1483         'GOT KEY入力待ち
1484         MKeyNumber = fnKEY_WAIT()
1485         '
1486         If MKeyNumber = MAbout% Then   '停止を選択した場合
1487             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1488             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1489             Break
1490         '
1491         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1492             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1493             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1494         '
1495         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1496             M_20# = MPass%            'M_20# プログラム間共通外部変数
1497             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1498         '
1499         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1500             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1501             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1502             Break
1503         '
1504         EndIf
1505         '
1506 '        If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1507         '
1508     EndIf
1509     '
1510     If M_20# = MContinue% Then *RETRY_PCBCHECK
1511     Exit Function
1512 FEnd
1513 '
1514 '■ScrewTight
1515 ''' <summary>
1516 ''' ねじ締めを行う(Sタイト)
1517 ''' </summary>
1518 '''<param name="PScrewPos()">
1519 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
1520 '''             PScrewPos(2)    ：ねじ締め回避点
1521 '''             PScrewPos(10)   ：ねじ締め終了高さ
1522 '''<param name="MScrewType">ネジタイプ(mm/sec)
1523 '''             1:6mm Sタイト銀ネジ
1524 '''             2:8mm Pタイト
1525 '''             3:6mm Sタイト黒ネジ
1526 '''             4:13mm Sタイト
1527 '''             5:6mm Mネジ
1528 '''</param>
1529 '''<param name="MFeedSpd">送り速度(mm/sec)</param>
1530 '''<returns>整数
1531 '''         0=異常終了、1=正常終了、ねじ無し=-1
1532 '''</returns>
1533 ''' <remarks>
1534 ''' Date   : 2021/07/07 : M.Hayakawa
1535 ''' Update : 2021/09/28 : M.Hayakawa ネジタイプ、送り速度を引数に追加
1536 ''' Update : 2022/07/08 : 中村 上空で吸着確認するように変更
1537 ''' </remarks>'
1538 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   'ネジ締め個別設定
1539     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
1540     ScrewTight = 0
1541     MOKNGFlg = 0
1542     Ovrd 100
1543     Mov PScrewPosition(1)       ' パレット上ねじ締めS①の安全回避位置
1544     MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1545     If MRtn = 0 Then
1546        ScrewTight = -1
1547     EndIf
1548     If MRtn = 0 Then GoTo *ScrewEnd
1549 '
1550     Fine 0.05 , P
1551     Ovrd MOvrdA%
1552     ' 減速設定
1553     Accel 100, 10
1554     ' パレット上ねじ締め開始位置へ移動
1555     Mvs PScrewPosition(2)
1556     ' 加減速を元に戻す
1557     Accel
1558     ' 内部Ovrd設定
1559 '    Ovrd MOvrdA%
1560     Ovrd 100
1561     ' Spd設定
1562 '    Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
1563     Spd MFeedSpd
1564     ' 設定進み量5.0 × 操作パネルのオーバーライド係数 × プログラム内オーバーライド係数
1565     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
1566     Select MScrewType%
1567         Case 1
1568             ' Sタイト：プログラム1、バンク1に設定
1569             ProgramBankSet(1,1)
1570             Break
1571         Case 2
1572             ' Pタイト：プログラム1、バンク1に設定
1573             ProgramBankSet(3,1)
1574             Break
1575         Case 3
1576             ' Sタイト黒：プログラム1、バンク1に設定
1577             ProgramBankSet(1,1)
1578             Break
1579         Case 4
1580             ' Sタイト13mm：プログラム1、バンク1に設定
1581             ProgramBankSet(1,1)
1582             Break
1583         Case 5
1584             ' Mネジ：プログラム1、バンク1に設定
1585             ProgramBankSet(1,1)
1586             Break
1587         Case 6
1588             ' Sタイト：プログラム1、バンク4に設定
1589             ProgramBankSet(1,4)
1590             Break
1591         Default
1592             ' プログラム1、バンクなし設定
1593             ProgramBankSet(0,0)
1594             Break
1595     End Select
1596 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     'ドライバーON　CW
1597      'ドライバーON　CW
1598     M_Out(12241)=1
1599     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1600     Wait M_In(11584)=1          '完了/エラー検出 暫定コメント 10/6 M.H
1601     Fine 0 , P
1602     Dly 0.1
1603     Spd M_NSpd
1604     '
1605     If M_In(11256)=1 Then  'ねじトータルエラー検出時
1606         M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1607         Dly 0.1
1608        ' プログラム・バンク解除
1609         ProgramBankSet(0,0)
1610         'パレット上ねじ締め終了位置上空へ移動
1611         Mvs PScrewPosition(10),-80
1612         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1613         M_Out(12249)=1 Dly 0.3
1614         MOKNGFlg = -1
1615         ScrewTight = 0
1616     Else
1617          'ドライバーOFF　CW
1618         M_Out(12241)=0
1619 '        エラーがない場合はネジ締め終了位置で増し締め
1620 '        Select MScrewType%
1621 '            Case 1
1622 '                ' Sタイト：プログラム1、バンク3に設定
1623 '                ProgramBankSet(1,3)
1624 '                Break
1625 '            Case 2
1626 '                ' Pタイト：プログラム1、バンク3に設定
1627 '                ProgramBankSet(3,3)
1628 '                Break
1629 '            Case 3
1630 '                ' Sタイト黒：プログラム1、バンク3に設定
1631 '                ProgramBankSet(1,3)
1632 '                Break
1633 '            Case 4
1634 '                ' Sタイト13mm：プログラム1、バンク3に設定
1635 '                ProgramBankSet(1,3)
1636 '                Break
1637 '            Case 5
1638 '                ' Mネジ：プログラム1、バンク3に設定
1639 '                ProgramBankSet(1,3)
1640 '                Break
1641 '            Default
1642 '                ' プログラム1、バンクなし設定
1643 '                ProgramBankSet(0,0)
1644 '                Break
1645 '        End Select
1646 '         'ドライバーON　CW
1647 '        Mvs PScrewPosition(10)
1648 '        M_Out(12241)=1
1649 '        Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1650 '        Fine 0 , P
1651 '
1652          'ドライバーOFF　CW
1653         M_Out(12241)=0
1654        ' プログラム・バンク解除
1655         ProgramBankSet(0,0)
1656         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1657         M_Out(12249)=1 Dly 0.3
1658     '     ↓PScrewPos(2) → PScrewPosition(10)に変更 9/16 M.Hayakawa
1659         'パレット上ねじ締め終了位置上空へ移動
1660        Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1661         'Mvs PScrewPosition(10),-80
1662         ScrewTight = 1
1663     EndIf
1664 ' 暫定（暫定マスク　9/16 M.Hayakawa)
1665 '    Ovrd 10
1666 '    Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1667     Ovrd 100
1668     Exit Function
1669 *ScrewEnd
1670 FEnd
1671 '
1672 '■ScrewGet
1673 ''' <summary>
1674 ''' ねじ供給機からねじを得る
1675 ''' </summary>
1676 '''<param name="%">
1677 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
1678 '''         PScrewPos(2)    ：ねじ供給器回避点
1679 '''         PScrewPos(9)    ：ねじ供給器上空ネジ捨位置
1680 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1681 '''         PScrewPos(3)    ：Mねじポカヨケ位置
1682 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
1683 '''</param>
1684 '''<param name = FeederReadyNo%> 指定の供給機Ready </param>
1685 '''<param name = FeederScrewSensor%> 指定の誤供給防止センサー指定(0でセンサー無し)</param>
1686 '''<returns>整数
1687 '''         0=異常終了、1=正常終了、-1=ねじ供給NG、-2=ねじ誤供給NG、-3=吸着エラー
1688 '''</returns>
1689 ''' <remarks>
1690 ''' Date   : 2021/07/07 : M.Hayakawa
1691 ''' </remarks>
1692 '''<update>
1693 '''Date    : 2021/11/15 : 中村
1694 '''Date    : 2021/02/07 : 早川 念のため確認を削除
1695 '''</update>
1696 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
1697     fnAutoScreenComment(522)    '状態表示[ネジ供給待ち] 2022/05/09 渡辺
1698     ScrewGet = 0
1699     MScrewJudge% = 0
1700     'ねじ供給器初期動作エラーチェック
1701 ' ↓暫定削除
1702     'Mov PScrewPosition(2)   'ねじ供給機回避点へ移動
1703     For MCnt% = 0 To MFinCnt%
1704         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '指定ねじ供給機がReadyになっているか確認(5秒間)
1705         If MRtn = 0 Then
1706             M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
1707             ScrewGet = -1
1708             MScrewJudge% = 2
1709         EndIf
1710         Ovrd 100
1711         If FeederScrewSensor% <> 0 Then
1712             If M_In(FeederScrewSensor%) = 1 Then  '誤供給が検出されたら
1713                 'Ovrd 30
1714                 M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
1715                 'NGとしてここの関数から抜ける
1716                 ScrewGet = -2
1717                 MScrewJudge% = 3
1718             EndIf
1719         EndIf
1720         Ovrd 100
1721         Spd M_NSpd
1722         If MScrewJudge% = 0 Then
1723     '        ScrewGet = 0
1724             M_Out(Y63_Driver)=1         ' バンクセッティング　C2
1725             Dly 0.3
1726             MScrewCnt% = 0
1727             MFinCnt% = 2
1728             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
1729             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1730             '
1731             '
1732             'Ovrd 40 '2に変更 10/6 M.H '5に変更10/7中村
1733             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1734             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
1735             'Mvs PScrewPosition(10), 1.2
1736            Mvs PScrewPosition(10)       'Fan用ねじ吸着位置修正のため変更 2022-02-01AJI
1737             'ビット回転(シーケンス見直しにつき処理位置変更3/30中村)
1738             M_Out(Y60_Driver)=1
1739             'ビット回転安定状態監視開始
1740             M_Timer(4) = 0
1741             MloopFlg = 0
1742             MCrtTime& = 0
1743            'ビット回転安定まで待機
1744             While MloopFlg = 0
1745                 MCrtTime& = M_Timer(4)
1746                 If MCrtTime& >= 180 Then
1747                     MloopFlg = 1
1748                 EndIf
1749             WEnd
1750             '
1751            M_Out(Y68_VV1)=1 Dly 0.3     ' ねじ吸着　ON'Dly 0.3追加(8/27中村)
1752             'ビット回転(シーケンス見直しにつき処理位置変更3/30中村)
1753 '            M_Out(Y60_Driver)=1
1754 '            Dly 0.2
1755             '吸着位置にて吸着確認
1756             MRtn = 0
1757             MRtn = frInCheck(11264, 1, MSETTIMEOUT01&)      'チェックはするがエラー判定はしない
1758             '
1759             JOvrd M_NJovrd
1760             Spd M_NSpd
1761             'ネジ吸着確認位置移動
1762             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
1763             Mvs PScrewPosition(10), -30  ' ネジ吸着確認位置
1764            'Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1765             'ビット回転停止
1766             M_Out(Y60_Driver)=0
1767             '
1768 '            If MRtn = 1 Then       '最初の閾値チェックを行わない
1769                 '1秒間ネジ吸着確認 始めの閾値
1770                 MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1771 '            EndIf
1772             'MRtn = 0'強制エラー
1773             '吸着エラーの場合
1774             'ネジをねじ太郎に戻す
1775             If MRtn = 0 Then
1776                 Ovrd 30      '2から5に変更'5から30に変更(3/30中村)
1777                 'ビット回転停止
1778                 M_Out(Y60_Driver)=0
1779                 'ネジ供給機上空
1780                 Mvs PScrewPosition(1)
1781                 '更に上空
1782                 Mov PScrewPosition(1), -140
1783                 'ネジ捨て位置
1784                 If FeederReadyNo% = 11260 Then     '供給機別に吸着エラー数をカウント　2022/05/19 渡辺
1785                     MRtn = FnCtlValue2(3)          '供給機２吸着エラー数＋１
1786                 Else
1787                     MRtn = FnCtlValue2(4)          '供給機１吸着エラー数＋１  2022/04/28 渡辺
1788                 EndIf
1789                 Mov PScrewPosition(9)
1790                 MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1791                 '吸着OFF
1792                 M_Out(12249)=1 Dly 0.3 'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1793                 Dly 0.2
1794                 '破壊ON
1795                 M_Out(Y6B_VB1)=1 '真空破壊ON
1796                 'ビット回転
1797                 M_Out(Y61_Driver)=1
1798                 Dly 0.5
1799                 '                '
1800                 Ovrd 100
1801                 JOvrd M_NJovrd
1802                 Spd M_NSpd
1803                 'ドライバーを上下させねじを振り落とす
1804                 Mov PScrewPosition(9), 10
1805                 Mov PScrewPosition(9)
1806                 Dly 0.1
1807                 Mov PScrewPosition(9), 10
1808                 Mov PScrewPosition(9)
1809                 '
1810                 'ネジ落ち待ち
1811                 Wait M_In(11265) = 0
1812                 'ビット回転停止
1813                 M_Out(Y61_Driver)=0
1814                 Dly 0.1
1815                 '破壊OFF
1816                 M_Out(Y6B_VB1)=0 '真空破壊OFF
1817                 'ねじ落ちたとして、移動更に上空
1818                 Mov PScrewPosition(1), -140
1819                 Ovrd 100
1820                 Spd M_NSpd
1821                 'ネジ供給機上空
1822                 Mvs PScrewPosition(1)
1823 '                '
1824                 ScrewGet = -3
1825                 If MCnt% = MFinCnt% Then
1826                     MScrewJudge% = 4
1827                     Mov PScrewPosition(2)
1828                     Break
1829                 EndIf
1830                 Break
1831 '                '
1832             Else
1833                 MCnt% = MFinCnt%
1834                 ScrewGet = 1
1835             EndIf
1836         Else
1837             MCnt% =MFinCnt%
1838         EndIf
1839     Next  MCnt%
1840         '
1841 '    If MScrewJudge% = 0 Then
1842 '        Ovrd 100
1843 '        Spd M_NSpd
1844 '        PScrewPosition(1)
1845 '        Mvs PScrewPosition(10), -30  ' ねじピックアップ位置 -30mm
1846 '        'Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1847 '        M_Out(Y60_Driver)=0     ' ビット回転停止
1848 '        M_Out(Y63_Driver)=0     ' バンクセッティング　C2
1849 '        'Mvs PScrewPosition(10), -30  ' ねじピックアップ位置 -30mm
1850 '        Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1851 '        'Mov PScrewPosition(2)
1852 '        'もう一度吸着確認　上空の最終閾値
1853 '        MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1854 '        If MRtn = 0 Then      '吸着エラーの場合
1855 '            MScrewJudge% = 4
1856 '            ScrewGet = -3
1857 '        ElseIf MRtn = 1 Then      '吸着OKの場合
1858 '            MScrewJudge% = 1
1859 '            ScrewGet = 1
1860 '        EndIf
1861 '        Break
1862 '    EndIf
1863     '
1864 '    If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
1865     If MScrewJudge% = 0 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
1866     '
1867     Select MScrewJudge%
1868 '        Case 0
1869 ''            fErrorProcess(11,162,163,0) '異常終了
1870 '            MCommentD1001 = 162
1871 '            MCommentD1002 = 96
1872 '            Break
1873         Case 2
1874 '            fErrorProcess(11,63,161,0) '供給NG
1875             MCommentD1001 = 63
1876             MCommentD1002 = 96
1877             Break
1878         Case 3
1879 '            fErrorProcess(11,160,164,0) '誤供給
1880             MCommentD1001 = 237
1881             MCommentD1002 = 96
1882             Break
1883         Case 4
1884 '            fErrorProcess(11,94,95,0) '吸着NG
1885             MCommentD1001 = 94
1886             MCommentD1002 = 95
1887             Break
1888     End Select
1889     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
1890     '
1891     Select M_20#
1892         Case MAbout%          '停止が押された場合
1893             Mov PScrewPosition(2)                  '初期位置に戻って停止処理
1894             Mov PInitialPosition
1895             Break
1896         Case MContinue%       'リトライが押されていた場合(関数を抜けた先で処理)
1897             Break
1898         Case MNext%           '継続が押された場合
1899             M_20# = MClear%     '初期化
1900             Break
1901         Case MNgProcess%      'NGが押された場合
1902             Mov PScrewPosition(2)   'PIASにNG書き込みを行い,初期位置に戻って行程終了
1903             Mov PInitialPosition
1904             Break
1905         End Select
1906 *End_ScrewGet
1907     Exit Function
1908 FEnd
1909 '
1910 '■ProgramBankSet
1911 ''' <summary>
1912 ''' ねじ締めを行う(Pタイト)
1913 ''' </summary>
1914 '''<param name="MProgramNo">プログラム番号</param>
1915 '''<param name="MBankNo">バンク番号</param>
1916 '''</returns>
1917 ''' <remarks>
1918 ''' Date   : 2021/10/05 : M.Hayakawa
1919 ''' </remarks>'
1920 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
1921 '
1922     MLocalPrgNo% = (MProgramNo% - 1) * 32
1923     MLocalBankNo% = MBankNo% * 4
1924 '
1925     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
1926         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
1927     Else
1928         MLocalOutNo% = 0
1929     EndIf
1930 '
1931     M_Out8(12240) = MLocalOutNo%
1932     Dly 0.1
1933     Exit Function
1934 FEnd
1935 '
1936 '■fnKEY_WAIT()
1937 ''' <summary>
1938 ''' GOTからのキー入力待ち
1939 ''' </summary>
1940 '''<returns>1：停止    2：次へ
1941 '''         3：継続    4：トルクチェック開始
1942 '''         5：NG
1943 '''         11：ロボット初期位置1    12：ロボット初期位置2
1944 '''         13：ロボット初期位置3    14：ロボット初期位置4
1945 '''</returns>
1946 ''' <remarks>
1947 ''' Date   : 2021/07/07 : M.Hayakawa
1948 ''' </remarks>'
1949 Function M% fnKEY_WAIT()
1950     fnKEY_WAIT = 0
1951     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
1952     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
1953     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
1954     '下記キー待ちの継続に反応させないため
1955     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
1956     Dly 0.2
1957     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
1958     MLocalLoopFlg=1
1959     While MLocalLoopFlg=1
1960         If M_In(11345) = 1 Then         '停止   M5345
1961             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
1962             fnKEY_WAIT = 1
1963             MLocalLoopFlg=-1
1964             Break
1965         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
1966             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
1967             fnKEY_WAIT = 2
1968             MLocalLoopFlg=-1
1969             Break
1970         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
1971             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
1972             fnKEY_WAIT = 3
1973             MLocalLoopFlg=-1
1974             Break
1975         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
1976             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
1977             fnKEY_WAIT = 4
1978             MLocalLoopFlg=-1
1979             Break
1980         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
1981             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
1982             fnKEY_WAIT = 5
1983             MLocalLoopFlg=-1
1984             Break
1985         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
1986             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
1987             fnKEY_WAIT = MRobotInit1%
1988             MLocalLoopFlg=-1
1989             Break
1990         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
1991             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
1992             fnKEY_WAIT = MRobotInit2%
1993             MLocalLoopFlg=-1
1994             Break
1995         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
1996             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
1997             fnKEY_WAIT = MRobotInit3%
1998             MLocalLoopFlg=-1
1999             Break
2000         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
2001             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
2002             fnKEY_WAIT = MRobotInit4%
2003             MLocalLoopFlg=-1
2004             Break
2005         Else
2006         EndIf
2007     WEnd
2008     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
2009     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
2010     Exit Function
2011 FEnd
2012 '
2013 '■ fnAUTO_CTL
2014 ''' <summary>
2015 ''' AUTOモードOFF、PLCからの開始待ち
2016 ''' </summary>
2017 ''' <remarks>
2018 ''' Date   : 2021/07/07 : M.Hayakawa
2019 ''' </remarks>
2020 Function M% fnAUTO_CTL
2021     fnAUTO_CTL = 0
2022     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2023     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
2024     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2025     '
2026     If M_Svo=0 Then             'サーボON確認
2027         Servo On
2028     EndIf
2029     Wait M_Svo=1
2030     Exit Function
2031 FEnd
2032 '
2033 '■ fnWindScreenOpen
2034 ''' <summary>
2035 ''' ウィンド画面の表示、非表示設定
2036 ''' </summary>
2037 '''<param name="%"></param>
2038 '''<param name="%"></param>
2039 '''<param name="%"></param>
2040 '''<param name="%"></param>
2041 ''' <remarks>
2042 ''' コメントD1001, D1002, D1003の設定
2043 ''' MWindReSet = 0     画面非表示
2044 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2045 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2046 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2047 ''' Date   : 2021/07/07 : M.Hayakawa
2048 ''' </remarks>
2049 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2050     If MCommentD1001 <> 0 Then              'コメント 0 は設定がないので確認
2051         M_Out16(12480) = MCommentD1001      'D1001 コメント
2052     EndIf
2053     '
2054     If MCommentD1002 <> 0 Then              'コメント 0 は設定がないので確認
2055         M_Out16(12496) = MCommentD1002      'D1002 コメント
2056     EndIf
2057     '
2058     If MCommentD1003 <> 0 Then              'コメント 0 は設定がないので確認
2059        M_Out16(12512) = MCommentD1003       'D1003 コメント
2060     EndIf
2061     '
2062     M_Out16(12448) = MScreenNo              '画面番号  M6448   10=エラー画面
2063     M_Out(12363) = 1 Dly 0.5                'ウィンド画面設定  M6362
2064     Exit Function
2065 FEnd
2066 '
2067 '■FnCtlValue2
2068 ''' <summary>
2069 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2070 ''' </summary>
2071 ''' <param name="MCtlNo%"></param>
2072 ''' <remarks>
2073 ''' Date : 2022/04/28 渡辺
2074 ''' </remarks>
2075 '''
2076 '''  1：投入数       ＋１
2077 '''  2：組立ＯＫ数   ＋１
2078 '''  3：供給機２吸着エラー数 ＋１　　組立NGから変更 2022/05/19 渡辺
2079 '''  4：供給機１吸着エラー数 ＋１
2080 ''' 99：読書開始信号 OFF
2081 '''
2082 Function M% FnCtlValue2(ByVal MCtlNo%)
2083     FnCtlValue2 = 1
2084     Select MCtlNo%
2085         Case 1        '投入数＋１
2086             M_Out(12569) = 0             '書込み開始信号OFF
2087             M_Out(12568) = 1             '読込み開始信号ON
2088             MInputQty = M_In16(11600)    '投入数受信
2089             MInputQty = MInputQty + 1    '投入数＋１
2090             M_Out16(12592) = MInputQty   '投入数送信
2091             M_Out(12569) = 1             '書込み開始信号ON
2092             Break
2093             '
2094         Case 2        '組立ＯＫ数＋１
2095             M_Out(12569) = 0             '書込み開始信号OFF
2096             M_Out(12568) = 1             '読込み開始信号ON
2097             MAssyOkQty = M_In16(11616)   '組立OK数受信
2098             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2099             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2100             M_Out(12569) = 1             '書込み開始信号ON
2101             Break
2102             '
2103         Case 3        '供給機２吸着エラー数＋１
2104             M_Out(12569) = 0                       '書込み開始信号OFF
2105             M_Out(12568) = 1                       '読込み開始信号ON
2106             MSuctionErrQty = M_In16(11632)         '供給機２吸着エラー数受信
2107             MSuctionErrQty = MSuctionErrQty + 1    '供給機２吸着エラー数＋１
2108             M_Out16(12624) = MSuctionErrQty        '供給機２吸着エラー数送信
2109             M_Out(12569) = 1                       '書込み開始信号ON
2110             Break
2111             '
2112         Case 4        '供給機１吸着エラー数＋１
2113             M_Out(12569) = 0                       '書込み開始信号OFF
2114             M_Out(12568) = 1                       '読込み開始信号ON
2115             MSuctionErrQty = M_In16(11648)         '供給機１吸着エラー数受信
2116             MSuctionErrQty = MSuctionErrQty + 1    '供給機１吸着エラー数＋１
2117             M_Out16(12640) = MSuctionErrQty        '供給機１吸着エラー数送信
2118             M_Out(12569) = 1                       '書込み開始信号ON
2119             Break
2120             '
2121         Case 99        '読書開始信号OFF
2122             M_Out(12568) = 0        '読込み開始信号OFF
2123             M_Out(12569) = 0        '書込み開始信号OFF
2124             Break
2125             '
2126     End Select
2127     Exit Function
2128 FEnd
2129 '
2130 '
2131 '■FnScreEroorCord
2132 ''' 電動ドライバーのエラーコードを含めたコメントを出す為のコメント番号の作成
2133 ''' 新規作成：2022/05/23 : 渡辺
2134 '''
2135 Function M% FnScreEroorCord()
2136     MScrewErrorCord% = 0
2137     If M_In(11252) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 1    '11252:E_driver Error Massage1 E1
2138     If M_In(11253) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 2    '11253:E_driver Error Massage1 E2
2139     If M_In(11254) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 4    '11254:E_driver Error Massage1 E3
2140     If M_In(11255) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 8    '11255:E_driver Error Massage1 E4
2141     If M_In(11258) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 16   '11258:E_driver Error Massage1 E5
2142     MScrewErrorCord% = MScrewErrorCord% * 10
2143     MScrewErrorCord% = MScrewErrorCord% + 500
2144     FnScreEroorCord = MScrewErrorCord%
2145     Exit Function
2146 FEnd
2147 '
2148 '
2149 'Insightによる画像処理検査実行（並列処理なし）
2150 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2151 '-------------------------------------------------------------------------------
2152 'Insightによる画像処理検査実行（並列処理なし）
2153 '   引数
2154 '       PInspPos()      ：検査位置
2155 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2156 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2157 '       MInspCnt%       ：検査位置数
2158 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2159 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2160 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2161 '   戻り値：整数
2162 '       0=異常終了、1=正常終了
2163 '
2164 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2165 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2166 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2167 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2168 '   20200410    :   検査グループ設定Retry追加
2169 '-------------------------------------------------------------------------------
2170     '----- 初期設定 -----
2171     Cnt 0                                                           '移動効率化解除(初期値=0)
2172     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2173 '    Cnt 1,0.1,0.1
2174     '変数宣言・初期化
2175     Def Inte MNum                                                   '検査番号(検査順1～)
2176     MNum% = 1                                                       '検査番号初期値設定
2177     Def Inte MEndFlg                                                '検査終了フラグ
2178     MEndFlg% = 0
2179     '
2180     '検査G番号設定要求・検査実行要求off
2181     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2182     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2183     'エラー番号クリア
2184     MInspErrNum = 0                                                 '検査実行エラー番号
2185     M_Out16(MOUT_InspErrNum) = MInspErrNum
2186     MInspNGStepNum = 0                                              '検査実行NGStep番号
2187     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2188     '
2189     'Insight Ready check?
2190     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2191         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2192         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2193         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2194         ISInspectionSingle = 0                                      '異常終了戻り値設定
2195         'Exit Function
2196     EndIf
2197     If MInspErrNum = 20 Then GoTo *ISInspectionSingle_End
2198     '
2199     '検査位置数確認
2200     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2201         MInspErrNum = 21                                            '検査データなし 21　引数<1
2202         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2203         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2204         ISInspectionSingle = 0                                      '異常終了戻り値設定
2205         'Exit Function
2206     EndIf
2207    If MInspErrNum = 21 Then GoTo *ISInspectionSingle_End
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
2228             Mov PInspPos( MNum% )                                       '移動
2229             fnAutoScreenComment(523)                                    '状態表示[画像処理検査中] 2022/05/09 渡辺
2230             Dly 0.2                                                     '移動完了後Delay 0.05>>0.2
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
2386 '
2387 *ISInspectionSingle_End
2388     Exit Function
2389 FEnd
2390 '
2391 '■fnAutoScreenComment
2392 ''' <summary>
2393 ''' メイン画面の動作状況表示
2394 ''' コメントD1005の設定
2395 ''' </summary>
2396 '''<param name="McommentD1005%">コメントID</param>
2397 ''' <remarks>
2398 ''' Date   : 2021/07/07 : M.Hayakawa
2399 ''' </remarks>
2400 Function fnAutoScreenComment(ByVal McommentD1005%)
2401     M_Out16(12576) = McommentD1005%
2402     Exit Function
2403 FEnd
2404 '
2405 '■fnRoboPosChk
2406 ''' <summary>
2407 ''' 最後に終了したロボットポジションの確認
2408 ''' </summary>
2409 '''<param name="MINNumber%">入力番号</param>
2410 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2411 '''<param name="MTimeCnt&">タイムアウト時間</param>
2412 ''' PLCに保続した番号を読込み、確認
2413 ''' MRBTOpeGroupNo = 5 が初期位置に設定
2414 '''<returns>整数 0:タイムアウト 1:OK</returns>
2415 ''' <remarks>
2416 ''' Date   : 2021/07/07 : M.Hayakawa
2417 ''' </remarks>
2418 Function M% fnRoboPosChk
2419     fnRoboPosChk = 0
2420     MRet = fnStepRead()
2421     '初期位置でないと判断した場合
2422     'ウィンド画面切換え
2423     If MRBTOpeGroupNo > 5 Then
2424         '下記キー待ちの継続に反応させないため
2425         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2426         Dly 0.2
2427         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2428         Dly 1.5
2429         '
2430         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
2431         '
2432         MLoopFlg% = 1
2433         While MLoopFlg% = 1
2434             '
2435             '
2436             MKeyNumber% = fnKEY_WAIT()
2437             Select MKeyNumber%
2438                 Case Is = MAbout%       '停止
2439                     M_20# = MAbout%
2440                     MLoopFlg% = -1
2441                     Break
2442                 Case Is = MNext%        '次へ
2443                     'MLoopFlg% = -1
2444                     Break
2445                 Case Is = MContinue%    '継続
2446                     M_20# = MContinue%
2447                     MLoopFlg% = -1
2448                     Break
2449                 Default
2450                     Break
2451             End Select
2452         WEnd
2453     EndIf
2454     '
2455     If M_20# = MContinue% Then                              '継続ボタンが押された場合
2456         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
2457         Ovrd 5                                   '低速オーバーライド値設定
2458         Select MRBTOpeGroupNo
2459             Case Is = 5                          '何もしない
2460                 Break
2461             Case Is = 10                         '初期位置へ戻す
2462                 'Mov PTEST001
2463                 Break
2464             Case Is = 15                         '初期位置へ戻す
2465                 'Mov PTEST002
2466                 Dly 0.5
2467                 'Mov PTEST001
2468                 Dly 0.5
2469                 Break
2470             Default
2471                 Break
2472         End Select
2473         '
2474         Ovrd M_NOvrd                            'システムの初期値を設定
2475         M_Out(12364) = 1                        'toPLC_データ保存ON
2476         MRBTOpeGroupNo = 5
2477         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
2478         Dly 1.0
2479         M_Out(12364) = 0                        'toPLC_データ保存OFF
2480         fnRoboPosChk = 1                        '初期位置動作実行
2481         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
2482     EndIf
2483     Exit Function
2484 FEnd
2485 '
2486 '■frInCheck
2487 ''' <summary>
2488 ''' センサーINチェック
2489 ''' </summary>
2490 '''<param name="MINNumber%">入力番号</param>
2491 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2492 '''<param name="MTimeCnt&">タイムアウト時間</param>
2493 '''<returns>整数 0:タイムアウト 1:OK</returns>
2494 ''' <remarks>
2495 ''' Date   : 2021/07/07 : M.Hayakawa
2496 ''' </remarks>
2497 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2498     M_Timer(4) = 0
2499     MloopFlg = 0
2500     While MloopFlg = 0
2501         MCrtTime& = M_Timer(4)
2502         If M_In(MINNumber%) = MCMPFLG% Then
2503             MloopFlg = 1
2504             frInCheck = 1
2505         ElseIf MCrtTime& > MTimeCnt& Then
2506             MloopFlg = 1
2507             frInCheck = 0
2508         EndIf
2509     WEnd
2510     Exit Function
2511 FEnd
2512 '-----------------------------------------------
2513 '
2514 'ねじ締め機通信確認
2515 '
2516 '-----------------------------------------------
2517 Function M% fScewTcomChk
2518     fScewTcomChk = 0
2519     '通信確認送信
2520     M_Out(MOUT_ScwT_ComChk%) = MOn%
2521     '通信確認受信待機
2522     Wait M_In(MIN_ScwT_comOK%) = MOn%
2523     '通信確認送信終了
2524     M_Out(MOUT_ScwT_ComChk%) = MOff%
2525     Exit Function
2526 FEnd
2527 '
2528 '
2529 '-----------------------------------------------
2530 '
2531 'ねじ締め開始送信
2532 '
2533 '-----------------------------------------------
2534 Function M% fScewTStart
2535     fScewTStart = 0
2536     'ねじ締め開始待機を受信
2537     Wait M_In(MIN_ScwT_STRec%) = MOn%
2538     Dly 0.1
2539     'ねじ締め開始受信を送信
2540     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msecパルス
2541     Exit Function
2542 FEnd
2543 '
2544 '
2545 '-----------------------------------------------
2546 '
2547 'ねじ締め完了受信
2548 '
2549 '-----------------------------------------------
2550 Function M% fScewTFinish
2551     fScewTFinish = 0
2552     'ねじ締め完了待機を受信
2553     Wait M_In(MIN_ScwT_Fin%) = MOn%
2554     Dly 0.1
2555     'ねじ締め完了受信を送信
2556     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msecパルス
2557     Exit Function
2558 FEnd
2559 '
2560 '
2561 '-----------------------------------------------
2562 '
2563 '条件xx停止受信
2564 '
2565 '-----------------------------------------------
2566 Function M% fScewTCaseStop(ByVal MCase%())
2567     fScewTCaseStop = 0
2568     '条件xx停止を受信
2569     Wait M_In(MCase%(1)) = MOn%
2570     Dly 0.1
2571     '条件xx停止受信を送信
2572     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msecパルス
2573     Exit Function
2574 FEnd
2575 '
2576 '-----------------------------------------------
2577 '
2578 '再開始受信
2579 '
2580 '-----------------------------------------------
2581 Function M% fScewTReStart()
2582     fScewTReStart = 0
2583     '再開始を受信
2584     Wait M_In(MIN_ScwT_ReST%) = MOn%
2585     Dly 0.1
2586     '再開始受信を送信
2587     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msecパルス
2588     Exit Function
2589 FEnd
2590 '
2591 '■fErrorProcess
2592 '<summary>
2593 'エラー処理
2594 '</summary>
2595 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
2596 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
2597 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
2598 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
2599 '<make>
2600 '2021/11/5 中村天哉
2601 '</make>
2602 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2603     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
2604     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
2605     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
2606     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
2607 *RETRY_ERR_PROCESS
2608      M_20# = MClear%     '初期化
2609 '        'エラー処理記述
2610         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2611 '        'GOT KEY入力待ち
2612         MKeyNumber = fnKEY_WAIT()
2613 '        '
2614         If MKeyNumber = MAbout% Then   '停止を選択した場合
2615             M_20# = MAbout%            'M_20# プログラム間共通外部変数
2616  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2617             Break
2618          '
2619         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
2620             M_20# = MContinue%            'M_20# プログラム間共通外部変数
2621  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2622         '
2623         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
2624             M_20# = MNext%            'M_20# プログラム間共通外部変数
2625  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2626          '
2627         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
2628             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
2629  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2630             Break
2631         '
2632         EndIf
2633         '
2634         '
2635         '
2636         If M_20# = MClear% Then *RETRY_ERR_PROCESS
2637         fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2638     Exit Function
2639 FEnd
2640 '
2641 '■fnInitialZone
2642 ''' <summary>
2643 ''' 現在位置から上空に待避し、初期位置に戻る
2644 ''' </summary>
2645 ''' <remarks>
2646 ''' Date : 2021/12/2 : M.Hayakawa
2647 ''' Update:2022/06/2 : M.Hayakawa 他工程の非常停止復帰に合わせて変更
2648 ''' </remarks>
2649 Function fnInitialZone()
2650     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中]
2651 '
2652     Ovrd 5
2653 ' 上空退避
2654     PActive = P_Curr
2655     Pmove = PActive
2656 '
2657     If PActive.X > 580 Then
2658         Pmove.Z =380        'パレット上に腕を伸ばしているときは500まで上げられない為、例外処置
2659     Else
2660         Pmove.Z =500        '上記以外はZ:500まで持ち上げ
2661     EndIf
2662 '
2663     Mvs Pmove
2664     Mov PInitialPosition
2665 ' ロックを開放
2666     InitialState()
2667 ' 一旦停止
2668     fErrorProcess(20,70,256,0)
2669     Exit Function
2670 FEnd
2671 '
2672 '■InitialState
2673 ''' <summary>
2674 ''' ハンド、治具を初期位置にする
2675 ''' </summary>
2676 ''' <returns>   0 : OK
2677 '''             1 : NG
2678 ''' </returns>
2679 ''' <remarks>
2680 ''' Date : 2021/12/2 : M.Hayakawa
2681 ''' </remarks>
2682 Function M% InitialState()
2683     InitialState = 0
2684     '製品位置決め解除
2685     M_Out(12261)=1 Dly 0.3      'FANクランプ出端パルス出力
2686     'Wait M_In(11271)=1          'FANクランプ出端検出(修正につきコメントアウト(8/26中村))
2687     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'FANクランプ出端検出(8/26中村)
2688     If MRtn = 0 Then
2689         fErrorProcess(11,234,284,0)
2690         Select M_20#
2691             Case MAbout%            '停止か
2692                 InitialState = 1
2693                 Break
2694             Case MNgProcess%        'NGが押された場合
2695                 InitialState = 0
2696                 Break
2697             Case MContinue%
2698                 M_20# = MClear%
2699                 InitialState = 0
2700                 Break
2701             Case MNext%
2702                 M_20# = MClear%
2703                 InitialState = 0
2704                 Break
2705         End Select
2706     EndIf
2707     '
2708     M_Out(12259)=1 Dly 0.3      'プッシュCY用SV戻端パルス出力
2709     'Wait M_In(11269)=1          'プッシュ戻端検出(修正につきコメントアウト(8/26中村))
2710     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)    'プッシュ戻端検出
2711     If MRtn = 0 Then
2712         fErrorProcess(11,234,284,0)
2713         Select M_20#
2714             Case MAbout%            '停止か
2715                 InitialState = 1
2716                 Break
2717             Case MNgProcess%        'NGが押された場合
2718                 InitialState = 1
2719                 Break
2720             Case MContinue%
2721                 M_20# = MClear%
2722                 InitialState = 0
2723                 Break
2724             Case MNext%
2725                 M_20# = MClear%
2726                 InitialState = 0
2727                 Break
2728         End Select
2729     EndIf
2730     '
2731     M_Out(12257)=1 Dly 0.3      '位置決めCY用SV戻端パルス出力
2732     'Wait M_In(11267)=1          '位置決め戻端検出(修正につきコメントアウト(8/26中村))
2733     MRtn = frInCheck(11267,1,MSETTIMEOUT05&)   '位置決め戻端検出(8/26中村)
2734     If MRtn = 0 Then
2735         fErrorProcess(11,234,284,0)
2736         Select M_20#
2737             Case MAbout%            '停止か
2738                 InitialState = 1
2739                 Break
2740             Case MNgProcess%        'NGが押された場合
2741                 InitialState = 1
2742                 Break
2743             Case MContinue%
2744                 M_20# = MClear%
2745                 InitialState = 0
2746                 Break
2747             Case MNext%
2748                 M_20# = MClear%
2749                 InitialState = 0
2750                 Break
2751         End Select
2752     EndIf
2753     Exit Function
2754 FEnd
2755 '
2756 '■fnTorqueCheck
2757 ''' <summary>
2758 ''' トルクチェック動作用のメイン
2759 ''' </summary>
2760 ''' <remarks>
2761 ''' Date   : 2021/12/21 : H.AJI
2762 ''' </remarks>'
2763 Function M% fnTorqueCheck
2764     'トルクチェック中送信  搬送系停止
2765     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
2766     '
2767     fnTorqueCheck = 0
2768     Ovrd 20
2769     Mov PInitialPosition              '初期位置移動
2770     Ovrd 100
2771     '下記キー待ちの継続に反応させないため
2772     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2773     Dly 0.2
2774     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2775     '
2776     'M6340  トルクチェック受信
2777     M_Out(12340) = 1 Dly 1.0                'トルクチェック受信 M6340
2778     Dly 1.0
2779     M_Out(12340) = 0
2780     '
2781     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
2782     '
2783     MLoopFlg = 1
2784     While MLoopFlg = 1
2785         '
2786         Mov PInitialPosition              '初期位置移動
2787         '
2788         MKeyNumber = fnKEY_WAIT()
2789         Select MKeyNumber
2790             Case Is = 1           '停止
2791                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
2792                 Dly 1.0
2793                 M_Out(12343) = 0
2794                 Ovrd 20
2795                 Mov PTicketRead_1
2796                 Ovrd 100
2797                 M_20# = 1
2798                 MLoopFlg = -1
2799                 Break
2800             Case Is = 2           '次へ
2801                 Break
2802             Case Is = 3           '継続
2803                 Break
2804             Case Is = 4           'トルクチェック開始
2805                 M_Out(12545) = 1    ' toPLC_PCトルクチェック1要求受信(M315)
2806                 M_Out(12342) = 1 Dly 1.0    'トルクチェック開始要求受信 M6342
2807                 fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
2808                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
2809                 MRet = fnMoveTorquePosi()
2810                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
2811                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2812                 Break
2813             Default
2814                 Break
2815         End Select
2816     WEnd
2817     '
2818     'トルクチェック中停止送信
2819     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
2820     '
2821     'ロボットの位置を元に戻す
2822     '
2823     Exit Function
2824  FEnd
2825  '
2826 '
2827 '
2828 '---------------------------
2829 '
2830 '    メイン画面の表示、非表示設定
2831 '         コメントD1001, D1002, D1003の設定
2832 '           MWindReSet = 0     画面非表示
2833 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
2834 '           MWindErrScr = 10    エラー画面 D1001, D1002
2835 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2836 '
2837 '---------------------------
2838 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2839     fnMainScreenOpen = 0
2840     '
2841    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2842         M_Out16(12480) = MCommentD1001            'D1001 コメント
2843     EndIf
2844     '
2845     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2846         M_Out16(12496) = MCommentD1002            'D1002 コメント
2847     EndIf
2848     '
2849     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2850         M_Out16(12512) = MCommentD1003            'D1003 コメント
2851     EndIf
2852     '
2853     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2854     M_Out(12362) = 1                         'ウィンド画面設定  M6362
2855     Dly 0.5
2856     M_Out(12362) = 0                         'ウィンド画面設定
2857     Exit Function
2858 FEnd
2859 '
2860 '■Main
2861 ''' <summary>
2862 ''' トルクチェック実動作
2863 ''' </summary>
2864 ''' <remarks>
2865 ''' Date   : 2021/12/21 : H.AJI
2866 ''' </remarks>'
2867 Function M% fnMoveTorquePosi
2868      fnMoveTorquePosi = 0
2869      Ovrd 50
2870      Mov PTorqueCheck_1 'トルクチェックメーター上空へ移動
2871     '
2872     Spd M_NSpd
2873 '-------------      ドライバーRST
2874     M_Out(12240)=0     'ドライバーOFF CCW
2875     M_Out(12241)=0     'ドライバーOFF CW
2876     M_Out(12242)=1     'ドライバー解除 C1
2877     M_Out(12243)=1     'ドライバー解除 C2
2878     M_Out(12245)=0     'プログラム解除 F1/プログラム2
2879 '---------------------------------------
2880 '[P-11]
2881 '--------------------------------------------------------------   【トルクチェック 0.4N - P11】
2882     Mov PTorqueCheck, -50                     ' トルク-1　置き位置上空 50mm へ移動
2883     Dly 0.1
2884 '-----------------------
2885    'Cnt 0                           'Cnt動作-2　終了
2886 '-----------------------
2887     Mov PTorqueCheck , -5                      'トルク-1　置き位置上空 5mm へ移動
2888     Dly 0.2
2889 '-----------------------
2890     ProgramBankSet(1,3)
2891     M_Out(12241)=0                   'ドライバーOFF  CW
2892     'Dly 0.1
2893 '--------------------------------
2894     Ovrd 40
2895    'Dly 0.1
2896 '--------------------------------  ネジ締め速度設定
2897     Spd 14                            'ライド 100-40 100% :Spd 12
2898     Dly 0.1
2899 '--------------------------------
2900 '--------------------------------
2901 '---------------------------------【ねじ締め動作】
2902 '
2903     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '移動中エラー検出
2904    Mvs PTorqueCheck               'トルクチェック位置へ移動
2905     Dly 0.3                          '動作安定待ち
2906    M_Out(12241)=1                   'ドライバーON  CW
2907 '
2908     Wait M_In(11584)=1                '完了/エラー検出
2909     Dly 0.1
2910     Spd M_NSpd
2911    'Ovrd 20
2912     If M_In(11256)=1 Then *LBL1       'ネジトータルエラー検出
2913     Wait M_In(11257)=1                'ネジ完了SC
2914 '---------------------------------
2915     Dly 0.1
2916     M_Out(12241)=0                    'ドライバーOFF CW
2917     Dly 0.1
2918     M_Out(12242)=0                    'ドライバー解除 C1
2919     Dly 0.1
2920     M_Out(12243)=0                    'ドライバー解除 C2 (バンク3)
2921     Dly 0.1
2922     M_Out(12245)=0                    'プログラム2解除 F1
2923 '--------------------------------------------------------------   【トルクチェック 0.4N - P11ここまで】
2924 '
2925     Mvs PTorqueCheck,-60                       'あえてmov から変更
2926     Dly 0.1
2927 '--------------------------------------------------------------
2928    'Ovrd 80
2929 '--------------------------------------------------------------
2930 '---------------------------------------
2931 '---------------------------------------
2932 '---------------------------------------エラー離脱処理
2933    *LBL1
2934    Fsc Off            '力覚センサ　Off   *STEP1は不要
2935    Mvs ,-100
2936    M_Out(12241)=0     'ドライバーOFF CW
2937    Dly 0.1
2938    M_Out(12242)=0     'ドライバー解除 C1
2939    Dly 0.1
2940    M_Out(12243)=0     'ドライバー解除 C2 (バンク3)
2941    Dly 0.1
2942    M_Out(12245)=0     'プログラム解除 F1
2943 '---------------------------------------
2944 '---------------------------------------
2945 '-------------
2946    'Mov PInitPos19049
2947    Dly 0.1
2948 '
2949 '
2950     Exit Function
2951 FEnd
2952 '
2953 '■Main
2954 ''' <summary>
2955 ''' 組立動作用のメイン
2956 ''' </summary>
2957 ''' <remarks>
2958 ''' Date   : 2021/07/07 : M.Hayakawa
2959 ''' </remarks>'
2960 Function Main
2961     MopeNo = M_21#         '外部変数にて動作番号代入
2962     '
2963     If M_Svo=0 Then
2964         Servo On
2965     EndIf
2966     Wait M_Svo=1
2967 '組立スタート日付時刻要求パルスON
2968     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
2969 'パトライト操作
2970     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
2971     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
2972     '
2973     M_20# = 0                                   'KEY入力初期化
2974     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
2975     MRet% = 0
2976 '復帰動作　実行・未実行判別      2022/03/22 渡辺 作成
2977     PActive = P_Curr                    '現在位置を取得
2978     MRecoveryPass% = 0
2979     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
2980         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
2981             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
2982             MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
2983         EndIf
2984     EndIf
2985     EndIf
2986     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
2987         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
2988             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
2989                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
2990             EndIf
2991         EndIf
2992     EndIf
2993     If MRecoveryPass% = 0 Then
2994         fnInitialZone()        '復帰動作パスフラグが立っていない時は復帰動作を実行
2995     EndIf
2996 '
2997     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
2998         M_Out(12364) = 1            'toPLC_データ保存ON
2999 'トルクチェック
3000         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3001             MRet% = fnTorqueCheck()
3002             Break
3003         Else
3004 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
3005 '                MRtn = InspInit()               '画像処理初期化処理
3006 '            EndIf
3007             '
3008            M_20# = MClear%                    '初期化
3009 '組立開始
3010             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3011                 MRet% = fnAssyStart()
3012             Else
3013                 M_20# = MPass%
3014             EndIf
3015 '組立終了日付時刻
3016             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
3017             Wait M_In(11572) = 1            '日付取得完了
3018             Dly 0.1
3019             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
3020             '  KEY入力が何もない場合 OKと判断
3021             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
3022 ' 後工程へフラグ出力
3023             If M_20# <> MAbout% Then
3024                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
3025             ElseIf M_20# = MPass% Then
3026                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
3027             EndIf
3028 'About(停止)以外はOKを出力（パレット降下）
3029             If M_20# <> MAbout% Then
3030                 M_Out(12339) = 1 Dly 0.5    'M6339  toPLC_RBT完了パルス出力
3031             EndIf
3032             M_Out(12346) = 0                ' M6346 toPLC_組立開始受信 OFF
3033 'PIASに組立完了書込み
3034             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3035                 If M_20# = MPass% Then
3036                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3037                 Else
3038                     'KEY入力がNGの場合
3039                     If M_20# = MNgProcess% Then
3040 '                        M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3041                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3042                         MRet% = fnPiasWrite(MNG%)
3043                        nAssyNgQty = nAssyNgQty + 1
3044                     EndIf
3045                     '
3046                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/17中村)
3047                     If M_20# = MAssyOK% Then
3048                             '-----------------------
3049                             'D732 -> D2600 コピー要求
3050                             M_Out(12566) = 1
3051 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3052                             M_Out(12566) = 0
3053                             '
3054                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3055                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3056                             '基板番号照合(PPは未使用）
3057 '                            MRet% = fnPCBNumberCheck()
3058                         Else
3059                             MRet% = 1
3060                         EndIf
3061                         '
3062                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3063                             If M_20# <> MAbout% Then
3064                                 '工程履歴OK書き込み
3065                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3066                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3067                                 MRet% = fnPiasWrite(MOK%)
3068                                 nAssyOkQty = 0
3069                                 nAssyOkQty = nAssyOkQty + 1
3070                             Else
3071                                 nAssyOkQty = nAssyOkQty + 1
3072                             EndIf
3073                         EndIf
3074                     EndIf
3075 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3076 '                    MRet% = fnPiasWrite(MOK%)
3077                 EndIf
3078             Else
3079                 nAssyOkQty = nAssyOkQty + 1
3080             EndIf
3081             '
3082             '組立終了日付時刻解除
3083             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3084             '投入数、組立OK数、組立NG数書込み
3085 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3086             '
3087 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3088 '                '画像処理終了処理
3089 '                MRtn = InspQuit()
3090 '            EndIf
3091         EndIf
3092         M_Out(12364) = 0                          'toPLC_データ保存OFF
3093     EndIf
3094 'パトライト操作
3095     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3096     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3097 'GOT表示
3098     fnAutoScreenComment(93)  'AUTO画面 工程完了
3099 '    M_Out(12339) = 1 Dly 0.5        ' M6339 toPLC_RBT完了パルスON
3100 '    M_Out(12346) = 0        'M6346  toPLC_AssY開始受信 OFF
3101 '
3102 FEnd
3103 End
3104 '
3105 '
3106 'おまじないコメント
3107 '絶対削除するな
3108 '
3109 '
3110 '
3111 '
PInspPosition(1)=(+313.20,-30.00,+430.00,+180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PInspPosition(2)=(+348.35,+127.39,+410.00,+180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PInspPosition(3)=(+538.88,+54.12,+420.00,+180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PInspPosition(4)=(+520.00,-46.00,+397.00,-180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
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
PTemp=(+602.00,-150.75,+450.00,-180.00,-0.02,+90.00,+0.00,+0.00)(7,0)
PScrewPosTemp(1)=(+349.00,+91.40,+370.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPosTemp(2)=(+349.00,+91.40,+313.84,+180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPosTemp(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(10)=(+349.00,+91.40,+303.84,+180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PGetScrewPosTemp(1)=(+233.44,+389.39,+380.00,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PGetScrewPosTemp(2)=(+166.05,+146.93,+400.00,-180.00,+0.00,+128.05,+0.00,+0.00)(7,0)
PGetScrewPosTemp(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(9)=(+127.52,+411.68,+432.42,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PGetScrewPosTemp(10)=(+233.44,+389.39,+338.70,-180.00,+0.00,+179.99,+0.00,+0.00)(7,0)
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
PActive=(+602.00,-150.75,+450.00,-180.00,-0.02,+90.00,+0.00,+0.00)(7,0)
Pmove=(+300.00,+0.00,+500.00,+180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
PCalcGetMainScrew=(+0.00,+0.00,-1.20,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PCalcGetFanScrew=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGyroPcbRead=(+315.06,-65.41,+419.00,+160.00,+0.00,+90.00)(7,0)
PGyroPcbRead_1=(+329.62,-57.71,+450.00,-180.00,+0.00,+90.00)(7,0)
PInitialPosition=(+300.00,+0.00,+450.00,-180.00,+0.00,-180.00)(7,0)
PMainPcbRead=(+309.58,-174.17,+408.00,-180.00,+0.00,+180.00)(7,0)
PMainPcbRead_1=(+309.58,-174.17,+450.00,-180.00,+0.00,+180.00)(7,0)
PParts1Check=(+313.20,-30.00,+430.00,+180.00,+0.00,-180.00)(7,0)
PParts1Check_1=(+313.20,-30.00,+480.00,-180.00,+0.00,+180.00)(7,0)
PParts2Check=(+348.35,+127.39,+410.00,+180.00,+0.00,-180.00)(7,0)
PParts2Check_1=(+348.35,+127.39,+450.00,+180.00,+0.00,+180.00)(7,0)
PParts3Check=(+538.88,+54.12,+420.00,+180.00,+0.00,-180.00)(7,0)
PParts3Check_1=(+538.88,+54.12,+460.00,-180.00,+0.00,+180.00)(7,0)
PParts4Check=(+520.00,-46.00,+397.00,-180.00,+0.00,+180.00)(7,0)
PParts4Check_1=(+520.00,-46.00,+460.00,-180.00,+0.00,-180.00)(7,0)
PScrewFan1=(+317.47,+123.55,+303.88,+180.00,+0.00,+90.00)(7,0)
PScrewFan1_0=(+317.47,+123.55,+313.88,+180.00,+0.00,+90.00)(7,0)
PScrewFan1_1=(+317.47,+123.55,+370.00,-180.00,+0.00,+90.00)(7,0)
PScrewFan2=(+349.00,+91.40,+303.84,+180.00,+0.00,+90.00)(7,0)
PScrewFan2_0=(+349.00,+91.40,+313.84,+180.00,+0.00,+90.00)(7,0)
PScrewFan2_1=(+349.00,+91.40,+370.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain1=(+305.10,-26.35,+304.11,-180.00,+0.00,+90.00)(7,0)
PScrewMain1_0=(+305.10,-26.35,+310.11,-180.00,+0.00,+90.00)(7,0)
PScrewMain1_1=(+305.10,-26.35,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain2=(+305.00,-175.00,+304.33,-180.00,+0.00,+90.00)(7,0)
PScrewMain2_0=(+305.00,-175.00,+310.33,+180.00,+0.00,+90.00)(7,0)
PScrewMain2_1=(+305.00,-175.00,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain3=(+366.84,-180.48,+304.36,-180.00,+0.00,+90.00)(7,0)
PScrewMain3_0=(+366.84,-180.48,+310.36,-180.00,+0.00,+90.00)(7,0)
PScrewMain3_1=(+366.84,-180.48,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain4=(+379.32,+24.31,+305.67,-180.00,+0.00,+60.00)(7,0)
PScrewMain4_0=(+379.32,+24.31,+311.67,-180.00,+0.00,+60.00)(7,0)
PScrewMain4_1=(+379.32,+24.31,+380.00,-180.00,+0.00,+60.00)(7,0)
PScrewMain5=(+385.18,-105.04,+323.66,-180.00,+0.00,+90.00)(7,0)
PScrewMain5_0=(+385.18,-105.04,+335.06,-180.00,+0.00,+90.00)(7,0)
PScrewMain5_1=(+385.18,-105.04,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain6=(+325.37,-78.92,+323.77,+180.00,+0.00,+90.00)(7,0)
PScrewMain6_0=(+325.37,-78.92,+334.17,-180.00,+0.00,+90.00)(7,0)
PScrewMain6_1=(+325.37,-78.92,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewSupplyFan=(+233.52,+389.28,+338.64,-180.00,+0.00,+179.99)(7,0)
PScrewSupplyFan_1=(+233.52,+389.28,+380.00,-180.00,+0.00,-180.00)(7,0)
PScrewSupplyFan_2=(+166.05,+146.93,+400.00,-180.00,+0.00,+128.05)(7,0)
PScrewSupplyFan_9=(+127.52,+411.68,+432.42,-180.00,+0.00,-180.00)(7,0)
PScrewSupplyMain=(+103.30,+195.32,+338.34,-180.00,+0.00,-180.00)(7,0)
PScrewSupplyMain_1=(+103.30,+195.32,+380.00,-180.00,+0.00,+180.00)(7,0)
PScrewSupplyMain_2=(+166.05,+146.93,+447.34,-180.00,+0.00,+128.05)(7,0)
PScrewSupplyMain_9=(-3.19,+216.64,+432.44,+180.00,+0.00,-180.00)(7,0)
PTicketRead=(+602.00,-150.75,+373.00,+180.00,-0.02,+90.00)(7,0)
PTicketRead_1=(+602.00,-150.75,+450.00,+180.00,-0.02,+90.00)(7,0)
PTorqueCheck=(+143.66,-242.00,+340.00,-180.00,-0.01,+90.00)(7,0)
PTorqueCheck_1=(+143.66,-242.00,+360.00,-180.00,-0.01,+90.00)(7,0)
