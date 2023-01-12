1 ' ===================================
2 '
3 '  210543200 STEP5 Assy6プログラムCD用
4 '
5 ' 作成者：自動化T
6 ' 作成日：2022.07.16
7 ' Ver 00 2021.07.16
8 '
9 ' Ver 00 2022.07.16 FA3 DVD用をベースにFA2と同様の変更
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
157 MSETTIMEOUT08& = 8000&                 ' 8秒設定
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
387 '    If M_22# = MIrregular% Then GoTo *IRREGULAR     'Assy完了後DVDメカを把持している場合
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
419 '組み立て開始(仮組み9/10中村)
420 'プログラム原点
421 Ovrd 100
422 'ハンドにDVDメカ,ブラケットが無いか
423 '
424 *INITIAL_CHECK
425 '
426 If M_In(11264) = 0 And M_In(11267) = 0 And M_In(11270) = 0 Then GoTo *CompInitial1  'DVDメカ,ブラケットが無いか
427 fErrorProcess(11,253,287,0)                 '284→287に変更6/2中村
428 If M_20# = MNext% Then M_20# = MClear%
429 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
430 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
431 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
432 *CompInitial1
433 '
434 'ハンドをイニシャルに戻す
435 If M_In(11266) = 1 Then     'DVDチャック閉検出
436     M_Out(12256) = 0        'DVDチャック閉OFF
437     M_Out(12257) = 1        'DVDチャック開ON
438     Break
439 EndIf
440 If M_In(11269) = 1 Then     'Fシリンダー出検出
441     M_Out(12258) = 0        'Fシリンダー出OFF
442     M_Out(12259) = 1        'Fシリンダー戻ON
443     Break
444 EndIf
445 If M_In(11272) = 1 Then     'Rシリンダー出検出
446     M_Out(12260) = 0        'Rシリンダー出OFF
447     M_Out(12261) = 1        'Rシリンダー戻ON
448     Break
449 EndIf
450 '
451 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    'DVDチャック開検出
452 If MRtn = 1 Then GoTo *CompInitial2
453 fErrorProcess(11,270,284,0)
454 If M_20# = MNext% Then M_20# = MClear%
455 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
456 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
457 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
458 *CompInitial2
459 '
460 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)    'Fシリンダー戻検出
461 If MRtn = 1 Then GoTo *CompInitial3
462 fErrorProcess(11,278,284,0)
463 If M_20# = MNext% Then M_20# = MClear%
464 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
465 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
466 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
467 *CompInitial3
468 '
469 MRtn = frInCheck(11271,1,MSETTIMEOUT05&)    'Rシリンダー戻検出
470 If MRtn = 1 Then GoTo *CompInitial4
471 fErrorProcess(11,276,284,0)
472 If M_20# = MNext% Then M_20# = MClear%
473 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
474 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
475 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
476 *CompInitial4
477 '
478 ' 2022/04/11 安全方向へ条件追加 渡辺
479 ' PInitialPosition 在席 MStandby=1
480 ' PMechaOnJigGet_3 在席 MStandby=2
481 '
482 MStandby = 0    '待機位置フラグを初期化
483 PTemp = P_Curr
484 If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
485     If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
486         If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
487             MStandby = 1
488         EndIf
489     EndIf
490 EndIf
491 If (PTemp.X <= PMechaOnJigGet_3.X + 1.0) And (PTemp.X >= PMechaOnJigGet_3.X - 1.0) Then
492     If ((PTemp.Y <= PMechaOnJigGet_3.Y + 1.0) And (PTemp.Y >= PMechaOnJigGet_3.Y - 1.0)) Then
493         If ((PTemp.Z <= PMechaOnJigGet_3.Z + 1.0) And (PTemp.Z >= PMechaOnJigGet_3.Z - 1.0)) Then
494             MStandby = 2
495         EndIf
496     EndIf
497 EndIf
498 If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
499     If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
500         If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
501             MStandby = 3
502         EndIf
503     EndIf
504 EndIf
505 If MStandby <> 0 Then GoTo *PositionOK
506 fErrorProcess(11,230,281,0)          '初期位置にいない時はエラーにする
507 If M_20# = MNext% Then GoTo *ASSY_ERROR_END
508 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
509 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
510 If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
511 *PositionOK
512 '
513 '
514 'DVD無しPASSプログラムから通常プログラムに切替えた時の対策 2022.05.13 渡辺
515 PTemp = P_Curr
516 If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
517     If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
518         If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
519             Ovrd 50
520             Mov PInitialPosition
521             Ovrd 100
522         EndIf
523     EndIf
524 EndIf
525 '
526 ' CDにつき以降の組立動作削除 2022/07/16 M.H
527 MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
528 MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
529 M_20# = MAssyOK%
530 '
531 *ASSY_ERROR_END
532 *AssyEnd
533 *fnAssyStart_FEndPosi
534 FEnd
535 '
536 '■fnPiasCheck
537 ''' <summary>
538 ''' PIASチケット読込み
539 ''' </summary>
540 ''' <returns>   0 : NG
541 '''             1 : OK(読込み完了)
542 ''' </returns>
543 ''' <remarks>
544 ''' Date   : 2021/07/07 : M.Hayakawa
545 ''' </remarks>'
546 Function M% fnPiasCheck
547     fnPiasCheck = 0
548     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
549     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
550 '
551 *RETRY_PIAS
552     M_20# = MClear%
553     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
554     '
555     '【IDチケット読み込み】
556     PInspPosition(1) = PTicketRead  'IDチケット読取位置
557     MInspGroup%(1) = 1              '検査G番号
558     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
559 '
560     'エラーの場合
561     If MRtn <> 1 Then
562         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
563         If MRtn <> 1 Then
564             'D720 -> D1300 コピー要求
565             M_Out(12565) = 1
566             Dly 0.5
567             M_Out(12565) = 0
568             'エラー処理記述
569             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
570             'GOT KEY入力待ち
571             MKeyNumber = fnKEY_WAIT()
572         '
573             Select MKeyNumber
574                 Case MNext%         '次へを選択した場合
575                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
576                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
577 '                    GoTo *fnPiasCheck_End                   'PIASチェック終了(関数外部で分岐するよう変更3/26中村)
578                     Break
579                 Case MAbout%        '停止を選択した場合
580                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
581                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
582 '                    GoTo *fnPiasCheck_End                   'PIASチェック終了(関数外部で分岐するよう変更3/26中村)
583                     Break
584                 Case MNgProcess%    'NGを選択した場合
585                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
586                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
587 '                    GoTo *fnPiasCheck_End                   'PIASチェック終了(関数外部で分岐するよう変更3/26中村)
588                     Break
589                 Case MContinue%     '継続を選択した場合
590                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
591                     M_20# = MContinue%
592 '                    GoTo *RETRY_PIAS                        'PIASチェックリトライ(関数外部で分岐するよう変更3/26中村)
593                     Break
594             End Select
595         EndIf
596     EndIf
597     If M_20# <> MClear% Then GoTo *fnPiasCheck_End
598 '
599 '----------D720 -> D1300 コピー要求----------
600     M_Out(12565) = 1
601     Dly 0.5
602     M_Out(12565) = 0
603 '----------通信確認をする----------
604     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
605     MRtn = 0                ' 初期化
606     M_20# = MClear%         ' 初期化
607     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
608     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）(関数外部で分岐するように変更3/26中村)
609     If MRtn <> 1 Then GoTo *fnPiasCheck_End
610 '        If M_20# = MContinue% Then
611 '            GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
612 '        Else
613 '            GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
614 '        EndIf
615 '    EndIf
616 '----------工程抜け確認----------
617     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
618     MRtn = 0                ' 初期化
619     M_20# = MClear%         ' 初期化
620     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
621     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）(関数外部で分岐するように変更3/26中村)
622     If MRtn <> 1 Then GoTo *fnPiasCheck_End
623 '        If M_20# = MContinue% Then
624 '            GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
625 '        Else
626 '            GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
627 '        EndIf
628 '    EndIf
629     '
630     fnPiasCheck = 1
631     *fnPiasCheck_End
632 FEnd
633 '
634 '■fnPCComuCheck
635 ''' <summary>
636 ''' PC-PLC通信チェック
637 ''' </summary>
638 ''' <returns>   0 : NG
639 '''             1 : OK(読込み完了)
640 ''' </returns>
641 ''' <remarks>
642 ''' Date   : 2021/07/07 : M.Hayakawa
643 ''' </remarks>'
644 Function M% fnPCComuCheck
645     fnPCComuCheck = 0
646     MJudge% = 0                                  '初期化
647     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
648     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
649     '
650     For MStaNo = 0 To 5
651         '
652         If M_In(MIN_PIAS_ComOK%) = 1 Then
653             'PC通信OK(M400)
654             MJudge% = MOK%
655             MStaNo = 5
656             Break
657         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
658             'toRBT_通信確認time out
659             MJudge% = MNG%
660             MCommentD1001 = 15
661             MCommentD1002 = 21
662             MStaNo = 5
663             Break
664         Else
665             'toRBT_通信確認time out
666             MJudge% = MNG%
667             MCommentD1001 = 14
668             MCommentD1002 = 21
669             Break
670         EndIf
671     Next MStaNo
672     '
673     '上記で返信フラグを受信してからPC通信確認OFF
674     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
675     '
676     'エラー画面
677     If MJudge% <> MOK% Then
678         M_20# = MClear%     '初期化
679         'エラー処理記述
680         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
681         'GOT KEY入力待ち
682         MKeyNumber = fnKEY_WAIT()
683         '
684         If MKeyNumber = MAbout% Then            '停止を選択した場合
685             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
686             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
687             Break
688         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
689             M_20# = MNext%                      'M_20# プログラム間共通外部変数
690             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
691             Break
692         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
693             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
694             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
695             Break
696         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
697             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
698             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
699             Break
700         EndIf
701     Else
702         'OKの場合
703         fnPCComuCheck = 1
704     EndIf
705 FEnd
706 '
707 '■fnProcessCheck
708 ''' <summary>
709 ''' 工程抜け確認
710 ''' </summary>
711 ''' <returns>    1：工程履歴OK     0：異常終了
712 '''             -1：前工程履歴NG  -2：自工程履歴あり
713 '''             -3：モデル仕向NG  -4：タイムアウト
714 '''             -5：履歴処理エラー
715 ''' </returns>
716 ''' <remarks>
717 ''' Date   : 2021/07/07 : M.Hayakawa
718 ''' </remarks>'
719 Function M% fnProcessCheck
720     fnProcessCheck = 0
721     MJudge% = MNG%      '一旦NGを初期化とする
722 '----------工程抜け確認----------
723     MCommentD1001 = 0   'コメント初期化
724     For MStaNo = 0 To 5
725         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
726         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
727         '
728         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
729             MJudge% = MOK%
730             fnAutoScreenComment(85)     ' AUTO画面
731             MStaNo = 5
732             Break
733         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
734             MFlgLoop% = 0
735             MJudge% = MNG%
736             MCommentD1001 = 27
737             MCommentD1002 = 22
738             fnAutoScreenComment(94)     ' AUTO画面
739             fnProcessCheck = -2         ' NGは-2を返す
740             MStaNo = 5
741             Break
742         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
743            MJudge% = MNG%
744             MCommentD1001 = 31
745             MCommentD1002 = 22
746             fnAutoScreenComment(83)     ' AUTO画面
747             fnProcessCheck = -3         ' NGは-3を返す
748             MStaNo = 5
749             Break
750         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
751             '履歴NGは直ぐに終了せず繰り返し確認を行う
752             '前工程の書込みが終了していない可能性があるため
753             MJudge% = MNG%
754             MCommentD1001 = 32
755             MCommentD1002 = 22
756             fnAutoScreenComment(84)     ' AUTO画面
757             fnProcessCheck = -1         ' NGは-1を返す
758             Dly 1.0
759             '工程抜け確認OFF
760             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
761             Dly 1.0
762            'MStaNo = 5
763             Break
764         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
765             MFlgLoop% = 0
766             MJudge% = MNG%
767             MCommentD1001 = 29
768             MCommentD1002 = 22
769             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
770             fnProcessCheck = -5         ' NGは-5を返す
771             MStaNo = 5
772             Break
773         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
774             MJudge% = MNG%
775             If MCommentD1001 = 32 Then
776                 '何もしない
777             Else
778                 MCommentD1001 = 26
779             EndIf
780             MCommentD1002 = 22
781             fnProcessCheck = -4         ' NGは-4を返す
782             MStaNo = 5
783             Break
784         Else
785             MJudge% = MNG%
786             MCommentD1001 = 28
787             MCommentD1002 = 22
788         EndIf
789     Next MStaNo
790     '工程抜け確認OFF
791     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
792     '通過履歴NG 工程抜けの場合
793     If MJudge% = MPass% Then
794         M_20# = MPass%
795     EndIf
796     '
797     'エラー画面
798     If MJudge% <> MOK% Then
799         M_20# = MClear%     '初期化
800         'エラー処理記述
801         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
802         'GOT KEY入力待ち
803         MKeyNumber = fnKEY_WAIT()
804         '
805         Select MKeyNumber
806             Case MAbout%        '停止を選択した場合
807                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
808                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
809                 Break
810             Case MNext%         '次へを選択した場合
811                 M_20# = MPass%          'M_20# プログラム間共通外部変数
812                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
813                 Break
814             Case MContinue%     '継続を選択した場合
815                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
816                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
817                 Break
818             Case MNgProcess%    'NGを選択した場合
819                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
820                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
821                 Break
822         End Select
823     Else
824         fnProcessCheck = 1  ' OKは1を返す
825     EndIf
826 FEnd
827 '
828 '■fnPiasWrite
829 ''' <summary>
830 ''' Pias 組立結果書込み要求
831 ''' </summary>
832 '''<param name="MFlg%">
833 '''                 MOK%(1) = 工程履歴にOKを書込む
834 '''                 MNG%(0) = 工程履歴にNGを書込む
835 '''</param>
836 '''<returns></returns>
837 ''' <remarks>
838 ''' Date   : 2021/07/07 : M.Hayakawa
839 ''' </remarks>'
840 Function M% fnPiasWrite(ByVal MFlg%)
841       fnPiasWrite = 0
842 *RETRY_PIASWRITE
843     '
844     '組立OK(MOK%)の場合　M306 ON
845    '組立NG(MNG%)の場合　M307 ON
846     If MFlg% = MOK% Then
847         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
848     Else
849         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
850     EndIf
851     Dly 0.1                  '念のため
852     '
853     'Piasへ書込み開始 M305 -> ON
854     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
855     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
856     '
857     MJudge% = MNG%
858     '
859     For MStaNo = 0 To 5
860         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
861             MJudge% = MOK%
862             'MRet = fnAutoScreenComment(85)  'AUTO画面
863             MStaNo = 5
864             Break
865         '
866         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
867             MJudge% = MNG%
868             'MRet = fnAutoScreenComment(85)  'AUTO画面
869            MCommentD1001 = 34
870            MCommentD1002 = 25
871             MStaNo = 5
872             Break
873         '
874         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
875             MJudge% = MNG%
876             'MRet = fnAutoScreenComment(85)  'AUTO画面
877            MCommentD1001 = 35
878            MCommentD1002 = 25
879             MStaNo = 5
880             Break
881         '
882         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
883             MJudge% = MNG%
884             'MRet = fnAutoScreenComment(85)  'AUTO画面
885            MCommentD1001 = 36
886            MCommentD1002 = 25
887             MStaNo = 5
888             Break
889         '
890         Else
891             MJudge% = MNG%
892            MCommentD1001 = 42
893            MCommentD1002 = 25
894         '
895         EndIf
896         '
897     Next MStaNo
898     '
899     'Piasへ書込み開始 M305 -> OfF
900     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
901     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
902     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
903     '
904     '
905     '通過履歴NG 工程抜けの場合
906     If MJudge% = MPass% Then
907         M_20# = MPass%
908     EndIf
909     '
910    M_20# = MClear%     '初期化
911     '
912     'エラー画面
913     If MJudge% < MOK% Then
914     '
915 '残しておくが現状では使用しないラベル
916 *RETRY_ERR_WRITE
917         M_20# = MClear%     '初期化
918         'エラー処理記述
919         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
920         'GOT KEY入力待ち
921         MKeyNumber = fnKEY_WAIT()
922         '
923         If MKeyNumber = MAbout% Then   '停止を選択した場合
924             M_20# = MAbout%            'M_20# プログラム間共通外部変数
925            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
926             Break
927         '
928         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
929             M_20# = MContinue%            'M_20# プログラム間共通外部変数
930             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
931         '
932         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
933             M_20# = MPass%            'M_20# プログラム間共通外部変数
934             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
935         '
936         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
937             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
938            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
939             Break
940         '
941         EndIf
942         '
943         If M_20# = MClear% Then *RETRY_ERR_WRITE
944         '
945     EndIf
946     '
947     If M_20# = MContinue% Then *RETRY_PIASWRITE
948     '
949     fnPiasWrite = 1
950     '
951 FEnd
952 '
953 '■fnPCBNumberCheck 基板番号照合のない工程につき削除 2022/07/16
954 '
955 '■ScrewTight_S2 ねじ締めなし工程につき削除 2022/07/16 M.H
956 '
957 '■ScrewGet_S3 ねじ締めなし工程につき削除 2022/07/16 M.H
958 '
959 '■fnKEY_WAIT()
960 ''' <summary>
961 ''' GOTからのキー入力待ち
962 ''' </summary>
963 '''<returns>1：停止    2：次へ
964 '''         3：継続    4：トルクチェック開始
965 '''         5：NG
966 '''         11：ロボット初期位置1    12：ロボット初期位置2
967 '''         13：ロボット初期位置3    14：ロボット初期位置4
968 '''</returns>
969 ''' <remarks>
970 ''' Date   : 2021/07/07 : M.Hayakawa
971 ''' </remarks>'
972 Function M% fnKEY_WAIT()
973     fnKEY_WAIT = 0
974     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
975     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
976     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
977     '下記キー待ちの継続に反応させないため
978     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
979     Dly 0.2
980     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
981     MLocalLoopFlg=1
982     While MLocalLoopFlg=1
983         If M_In(11345) = 1 Then         '停止   M5345
984             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
985             fnKEY_WAIT = 1
986             MLocalLoopFlg=-1
987             Break
988         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
989             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
990             fnKEY_WAIT = 2
991             MLocalLoopFlg=-1
992             Break
993         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
994             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
995             fnKEY_WAIT = 3
996             MLocalLoopFlg=-1
997             Break
998         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
999             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
1000             fnKEY_WAIT = 4
1001             MLocalLoopFlg=-1
1002             Break
1003         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
1004             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
1005             fnKEY_WAIT = 5
1006             MLocalLoopFlg=-1
1007             Break
1008             '
1009         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
1010             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
1011             fnKEY_WAIT = MRobotInit1%
1012             MLocalLoopFlg=-1
1013             Break
1014             '
1015         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
1016             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
1017             fnKEY_WAIT = MRobotInit2%
1018             MLocalLoopFlg=-1
1019             Break
1020             '
1021         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
1022             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
1023             fnKEY_WAIT = MRobotInit3%
1024             MLocalLoopFlg=-1
1025             Break
1026             '
1027         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
1028             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
1029             fnKEY_WAIT = MRobotInit4%
1030             MLocalLoopFlg=-1
1031             Break
1032             '
1033         Else
1034         EndIf
1035     WEnd
1036     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
1037     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
1038 FEnd
1039 '
1040 '■ fnAUTO_CTL
1041 ''' <summary>
1042 ''' AUTOモードOFF、PLCからの開始待ち
1043 ''' </summary>
1044 ''' <remarks>
1045 ''' Date   : 2021/07/07 : M.Hayakawa
1046 ''' </remarks>
1047 Function M% fnAUTO_CTL
1048     fnAUTO_CTL = 0
1049     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
1050     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
1051     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
1052     '
1053     If M_Svo=0 Then             'サーボON確認
1054         Servo On
1055     EndIf
1056     Wait M_Svo=1
1057 FEnd
1058 '
1059 '■ fnWindScreenOpen
1060 ''' <summary>
1061 ''' ウィンド画面の表示、非表示設定
1062 ''' </summary>
1063 '''<param name="%"></param>
1064 '''<param name="%"></param>
1065 '''<param name="%"></param>
1066 '''<param name="%"></param>
1067 ''' <remarks>
1068 ''' コメントD1001, D1002, D1003の設定
1069 ''' MWindReSet = 0     画面非表示
1070 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
1071 ''' MWindErrScr = 10    エラー画面 D1001, D1002
1072 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
1073 ''' Date   : 2021/07/07 : M.Hayakawa
1074 ''' </remarks>
1075 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
1076     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
1077         M_Out16(12480) = MCommentD1001            'D1001 コメント
1078     EndIf
1079     '
1080     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
1081         M_Out16(12496) = MCommentD1002            'D1002 コメント
1082     EndIf
1083     '
1084     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
1085        M_Out16(12512) = MCommentD1003            'D1003 コメント
1086     EndIf
1087     '
1088     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
1089     M_Out(12363) = 1                         'ウィンド画面設定  M6362
1090     Dly 0.5
1091     M_Out(12363) = 0                         'ウィンド画面設定
1092 FEnd
1093 '
1094 '■FnCtlValue2
1095 ''' <summary>
1096 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
1097 ''' </summary>
1098 ''' <param name="MCtlNo%"></param>
1099 ''' <remarks>
1100 ''' Date : 2022/04/28 渡辺
1101 ''' </remarks>
1102 '''
1103 '''  1：投入数       ＋１
1104 '''  2：組立ＯＫ数   ＋１
1105 '''  3：組立ＮＧ数   ＋１ (未使用)
1106 '''  4：吸着エラー数 ＋１
1107 ''' 99：読書開始信号 OFF
1108 '''
1109 Function M% FnCtlValue2(ByVal MCtlNo%)
1110     FnCtlValue2 = 1
1111     Select MCtlNo%
1112         Case 1        '投入数＋１
1113             M_Out(12569) = 0             '書込み開始信号OFF
1114             M_Out(12568) = 1             '読込み開始信号ON
1115             MInputQty = M_In16(11600)    '投入数受信
1116             MInputQty = MInputQty + 1    '投入数＋１
1117             M_Out16(12592) = MInputQty   '投入数送信
1118             M_Out(12569) = 1             '書込み開始信号ON
1119             Break
1120             '
1121         Case 2        '組立ＯＫ数＋１
1122             M_Out(12569) = 0             '書込み開始信号OFF
1123             M_Out(12568) = 1             '読込み開始信号ON
1124             MAssyOkQty = M_In16(11616)   '組立OK数受信
1125             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
1126             M_Out16(12608) = MAssyOkQty  '組立OK数送信
1127             M_Out(12569) = 1             '書込み開始信号ON
1128             Break
1129             '
1130         Case 4        '吸着エラー数＋１
1131             M_Out(12569) = 0                       '書込み開始信号OFF
1132             M_Out(12568) = 1                       '読込み開始信号ON
1133             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
1134             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
1135             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
1136             M_Out(12569) = 1                       '書込み開始信号ON
1137             Break
1138             '
1139         Case 99        '読書開始信号OFF
1140             M_Out(12568) = 0        '読込み開始信号OFF
1141             M_Out(12569) = 0        '書込み開始信号OFF
1142             Break
1143             '
1144     End Select
1145     Exit Function
1146 FEnd
1147 '
1148 'Insightによる画像処理検査実行（並列処理なし）
1149 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
1150 '-------------------------------------------------------------------------------
1151 'Insightによる画像処理検査実行（並列処理なし）
1152 '   引数
1153 '       PInspPos()      ：検査位置
1154 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
1155 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
1156 '       MInspCnt%       ：検査位置数
1157 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
1158 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
1159 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
1160 '   戻り値：整数
1161 '       0=異常終了、1=正常終了
1162 '
1163 '   MInspErrNum     ：異常終了時にエラー番号が設定される
1164 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
1165 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
1166 '   20190820    :   引数 MZAxis%,MNgContinue 追加
1167 '   20200410    :   検査グループ設定Retry追加
1168 '-------------------------------------------------------------------------------
1169     '----- 初期設定 -----
1170     Cnt 0                                                           '移動効率化解除(初期値=0)
1171     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
1172 '    Cnt 1,0.1,0.1
1173     '変数宣言・初期化
1174     Def Inte MNum                                                   '検査番号(検査順1～)
1175     MNum% = 1                                                       '検査番号初期値設定
1176     Def Inte MEndFlg                                                '検査終了フラグ
1177     MEndFlg% = 0
1178     '
1179     '検査G番号設定要求・検査実行要求off
1180     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
1181     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
1182     'エラー番号クリア
1183     MInspErrNum = 0                                                 '検査実行エラー番号
1184     M_Out16(MOUT_InspErrNum) = MInspErrNum
1185     MInspNGStepNum = 0                                              '検査実行NGStep番号
1186     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
1187     '
1188     'Insight Ready check?
1189     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
1190         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
1191         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
1192         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
1193         ISInspectionSingle = 0                                      '異常終了戻り値設定
1194         Exit Function
1195     EndIf
1196     '
1197     '検査位置数確認
1198     If MInspCnt% < 1 Or 30 < MInspCnt% Then
1199         MInspErrNum = 21                                            '検査データなし 21　引数<1
1200         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
1201         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
1202         ISInspectionSingle = 0                                      '異常終了戻り値設定
1203         Exit Function
1204     EndIf
1205     '
1206     '
1207     '
1208     '----- メイン処理 -----
1209     '設定された検査位置数分の検査実行
1210     While( MEndFlg% = 0 )
1211         '----- 検査グループ番号設定Retry追加 20200410
1212         MSetGrNumRetryExitFlg = 0
1213         MSetGrNumRetryCnt = 2                                           'Retry回数設定
1214         While( MSetGrNumRetryExitFlg = 0 )
1215         '----- 検査グループ番号設定Retry追加ここまで 20200410
1216             '
1217             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
1218             '
1219             '----- 検査グループ番号設定 -----
1220             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
1221             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
1222             '
1223             '検査位置へ移動・移動完了待ち
1224             Mvs PInspPos( MNum% )                                       '移動
1225             Dly 0.05                                                    '移動完了後Delay
1226             '
1227             '検査グループ番号設定終了確認
1228             M_Timer(1) = 0
1229             MExitFlg = 0
1230             While( MExitFlg = 0 )
1231                 '検査G設定正常終了?
1232                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
1233                     MExitFlg = 1
1234                 '
1235                 '検査G設定異常終了?
1236                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
1237                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
1238                     If MInspErrNum = 0 Then                             '1回目のエラー?
1239                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
1240                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
1241                     EndIf
1242                     MExitFlg = 1
1243                 '
1244                 'timeoutチェック
1245                 ElseIf 1000 < M_Timer(1) Then
1246                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
1247                     If MInspErrNum = 0 Then                             '1回目のエラー?
1248                         MInspErrNum = 12                                'timeout エラー番号=12
1249                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
1250                     EndIf
1251                     MExitFlg = 1
1252                 EndIf
1253             WEnd
1254             '
1255             '検査G番号設定要求off
1256             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
1257             '
1258             '----- 検査グループ設定Retry追加 20200410
1259             'NGなければ抜ける
1260             If MCurrentStepErr = 0 Then
1261                 MSetGrNumRetryExitFlg = 1
1262             Else
1263                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
1264                 If MSetGrNumRetryCnt = 0 Then
1265                     MSetGrNumRetryExitFlg = 1
1266                 Else
1267                     'Retryへ　その前にDelay
1268                     Dly 0.5
1269                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
1270                 EndIf
1271             EndIf
1272             '----- 検査グループ設定Retry追加ここまで 20200410
1273             '
1274         WEnd
1275         '
1276         '
1277         '
1278         '----- 検査実行 -----
1279         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
1280             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
1281                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
1282                 MInspRetryExitFlg = 0
1283                 MRetryCnt = 2                                        'Retry回数設定
1284                 While( MInspRetryExitFlg = 0 )
1285                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
1286                     '
1287                     '検査完了確認
1288                     MRetryCnt = MRetryCnt - 1
1289                     M_Timer(1) = 0
1290                     MExitFlg = 0
1291                     While( MExitFlg = 0 )
1292                     '検査完了待ち
1293                         '検査OK終了?
1294                         If M_In( MIN_IS_InspOK% ) = 1  Then
1295                             MJudgeOKFlg = 1                         '検査OKフラグON
1296                             MExitFlg = 1
1297                         '
1298                         '検査NG終了?
1299                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
1300                             If MInspErrNum = 0 Then                 '1回目のエラー?
1301                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
1302                                     MInspErrNum = 32                    '検査NG エラー番号=32
1303                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
1304                                 EndIf
1305                             EndIf
1306                             MExitFlg = 1
1307                         '
1308                         '検査異常終了(IS timeout)?
1309                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
1310                             If MInspErrNum = 0 Then                 '1回目のエラー?
1311                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
1312                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
1313                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
1314                                 EndIf
1315                             EndIf
1316                             MExitFlg = 1
1317                         '
1318                         'timeoutチェック
1319                         ElseIf 3000 < M_Timer(1) Then
1320                             If MInspErrNum = 0 Then                 '1回目のエラー?
1321                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
1322                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
1323                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
1324                                 EndIf
1325                             EndIf
1326                             MExitFlg = 1
1327                         EndIf
1328                     WEnd
1329                     '
1330                     '検査開始要求off
1331                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
1332                     '
1333                     'OKなら抜ける
1334                     If MJudgeOKFlg = 1 Then
1335                         MInspRetryExitFlg = 1
1336                     Else
1337                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
1338                         If MRetryCnt = 0 Then
1339                             MInspRetryExitFlg = 1
1340                         Else
1341                             'Retryへ　その前にDelay
1342                             Dly 0.3
1343                         EndIf
1344                     EndIf
1345                     '
1346                 WEnd
1347             EndIf
1348         EndIf
1349         '
1350         '
1351         '
1352         MNum% = MNum% + 1                                           '検査Step+1
1353         '検査終了確認　検査終了フラグセット
1354         If (MInspCnt% < MNum% ) Then
1355             MEndFlg% = 1                                            '検査終了フラグセット
1356         EndIf
1357         'NG発生時続行時処理
1358         If MInspErrNum <> 0 Then                                    'NGあり?
1359             If MNgContinue% <> 1 Then                               'NG続行?
1360                 MEndFlg% = 1                                        '検査終了フラグセット
1361             EndIf
1362         EndIf
1363     WEnd
1364     '
1365     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
1366     If 0 < MZAxis% Then
1367         PCurrentPos = P_Curr                                        '現在位置取得
1368         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
1369         Mvs PCurrentPos                                             '現在位置上空へ移動
1370     EndIf
1371     '
1372     '戻り値設定
1373     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      'カメラ検査強制OK(M_In(11372)=1)追加(12/21中村)
1374         ISInspectionSingle = 1                                      '正常終了戻り値設定
1375     Else
1376         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
1377         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
1378         ISInspectionSingle = 0                                      '異常終了戻り値設定
1379     EndIf
1380     Fine 0 , P
1381     '
1382 FEnd
1383 '
1384 ' ■ISInspection 未使用につき削除 2022/07/16 M.H
1385 '
1386 '■InitialZoneB
1387 ''' <summary>
1388 ''' 非常停止後の復帰動作
1389 ''' 1)上空退避　Z方向上に移動
1390 ''' 2)J1軸以外を退避ポジションへ移動
1391 ''' 3)J1軸のみを退避ポジションへ移動
1392 ''' 4)イニシャルポジションへ移動
1393 ''' </summary>
1394 ''' <remarks>
1395 ''' Date : 2022/04/11 : N.Watanabe
1396 ''' </remarks>
1397 Function V fnInitialZoneB()
1398     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中] 2022/04/26 渡辺
1399 '
1400 'パラメータ
1401     Ovrd 5
1402 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
1403 '    Cmp Pos, &B100011
1404 '
1405 '復帰動作開始
1406 '
1407 '置き台と両掴みの場所は、チャックを解放する
1408 *RecoveryChuckOpen
1409     PActive = P_Curr          '現在位置を取得
1410     MRecoveryChuckOpen = 0    'チャック解放フラグ 初期化
1411 'PMechaOnRoboSet(DVDメカ置き場)は、チャック解放
1412     If (PActive.X <= PMechaOnRoboSet.X + 1.0) And (PActive.X >= PMechaOnRoboSet.X -1.0) Then
1413         If (PActive.Y <= PMechaOnRoboSet.Y + 1.0) And (PActive.Y >= PMechaOnRoboSet.Y -1.0) Then
1414             If (PActive.Z <= PMechaOnRoboSet.Z + 1.0) And (PActive.Z >= PMechaOnRoboSet.Z -1.0) Then
1415                 MRecoveryChuckOpen = 1
1416             EndIf
1417         EndIf
1418     EndIf
1419 'PMechaOnRoboGet(DVDメカ受け取り位置)は、チャック解放
1420     If (PActive.X <= PMechaOnRoboGet.X + 1.0) And (PActive.X >= PMechaOnRoboGet.X -1.0) Then
1421         If (PActive.Y <= PMechaOnRoboGet.Y + 1.0) And (PActive.Y >= PMechaOnRoboGet.Y -1.0) Then
1422             If (PActive.Z <= PMechaOnRoboGet.Z + 1.0) And (PActive.Z >= PMechaOnRoboGet.Z -1.0) Then
1423                 MRecoveryChuckOpen = 1
1424             EndIf
1425         EndIf
1426     EndIf
1427     If MRecoveryChuckOpen = 1 Then
1428         M_Out(12256) = 0        'DVDチャック閉OFF
1429         M_Out(12257) = 1        'DVDチャック開ON
1430         M_20# = 0               'KEY入力初期化
1431         MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1432         If MRtn = 0 Then
1433             fErrorProcess(11,270,284,0)
1434             If M_20# = MNext% Then M_20# = MClear%
1435             If M_20# = MAbout% Then GoTo *RecoveryEnd
1436             If M_20# = MNgProcess% Then GoTo *RecoveryEnd
1437             If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
1438         Else
1439             M_Out(12257) = 0        'DVDチャック開OFF
1440         EndIf
1441     EndIf
1442 '
1443 '特殊回避　直接、上空退避が出来ない所の対処
1444 '
1445 'PMechaOnRoboSet(Get)～PMechaOnRoboSet(Get)_1のエリアにいるときは、PMechaOnRoboSet_1へ
1446 '・PMechaOnRoboSet
1447 '・PMechaOnRoboSet_1
1448 '・PMechaOnRoboGet
1449 '・PMechaOnRoboGet_1
1450 '上記４点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
1451     PActive = P_Curr                    '現在位置を取得
1452     If (PActive.X >= -150) And (PActive.X <= -60) Then
1453         If (PActive.Y >= 540) And (PActive.Y <= 570) Then
1454             If (PActive.Z >= 290) And (PActive.Z <= 320) Then
1455                 Mvs PMechaOnRoboSet_1
1456                 Dly 1.0
1457             EndIf
1458         EndIf
1459     EndIf
1460 '
1461 'PMechaOnRoboSet(Get)_1～PMechaOnRoboSet(Get)_2のエリアにいるときは、PMechaOnRoboSet_2へ
1462 '・PMechaOnRoboSet_1
1463 '・PMechaOnRoboSet_2
1464 '・PMechaOnRoboGet_1
1465 '・PMechaOnRoboGet_2
1466 '上記４点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
1467 '    PActive = P_Curr                    '現在位置を取得
1468 '    If (PActive.X >= -90) And (PActive.X <= 50) Then
1469 '        If (PActive.Y >= 300) And (PActive.Y <= 570) Then
1470 '            If (PActive.Z >= 290) And (PActive.Z <= 430) Then
1471 '                Mvs PMechaOnRoboSet_2
1472 '                Dly 1.0
1473 '            EndIf
1474 '        EndIf
1475 '    EndIf
1476 '
1477 '上空退避
1478     PActive = P_Curr
1479     Pmove = PActive
1480     Pmove.Z = 500           '上空退避する一律の高さ
1481      If PActive.X < -400 Then
1482         Pmove.Z =290        '金具(F)受け取り位置上に腕を伸ばしているときは500まで上げられない為、例外処置
1483     EndIf
1484     If PActive.X > 400 Then
1485         Pmove.Z =400        'パレット上に腕を伸ばしているときは500まで上げられない為、例外処置
1486     EndIf
1487     If PActive.Z < Pmove.Z Then
1488         Mvs Pmove
1489     EndIf
1490     Dly 1.0
1491 'J1軸以外を退避ポジションへ移動
1492     JActive = J_Curr
1493     Jmove = JTaihi
1494     Jmove.J1 = JActive.J1        'J1軸は現在値を使用し、JTaihiのポーズを取る
1495     Jmove.J6 = JActive.J6        'J6軸は現在値を使用し、JTaihiのポーズを取る
1496     Mov Jmove
1497     Dly 1.0
1498 'J1軸のみを退避ポジションへ移動
1499     Mov JTaihi
1500     Dly 1.0
1501 'イニシャルポジションへ移動
1502     Mov PInitialPosition
1503     Cmp Off
1504     Ovrd 100
1505 ' ねじロボを初期位置に戻すために強制的に自動運転開始         '2022/04/20 ファンクションの外へ移動 渡辺
1506 '    If M_In(11856) = 0 Then                 ' 停止中のみ
1507 '        M_Out(12834) = 1                    ' 自動運転開始ON 12834   M6834
1508 '        MRet = frInCheck(11842, 1, 30000&)  ' 自動運転開始受信待ち    11842   M5842
1509 '        If MRet = 0 Then
1510 '        Else
1511 '            M_Out(12834) = 0    ' 自動運転開始OFF 12834   M6834
1512 '        EndIf
1513 '    EndIf
1514     fErrorProcess(11,253,281,0)
1515     Exit Function
1516 *RecoveryEnd
1517 FEnd
1518 '
1519 '
1520 '■fnAutoScreenComment
1521 ''' <summary>
1522 ''' メイン画面の動作状況表示
1523 ''' コメントD1005の設定
1524 ''' </summary>
1525 '''<param name="McommentD1005%">コメントID</param>
1526 ''' <remarks>
1527 ''' Date   : 2021/07/07 : M.Hayakawa
1528 ''' </remarks>
1529 Function fnAutoScreenComment(ByVal McommentD1005%)
1530     M_Out16(12576) = McommentD1005%
1531 FEnd
1532 '
1533 '■fnRoboPosChk
1534 ''' <summary>
1535 ''' 最後に終了したロボットポジションの確認
1536 ''' </summary>
1537 '''<param name="MINNumber%">入力番号</param>
1538 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
1539 '''<param name="MTimeCnt&">タイムアウト時間</param>
1540 ''' PLCに保続した番号を読込み、確認
1541 ''' MRBTOpeGroupNo = 5 が初期位置に設定
1542 '''<returns>整数 0:タイムアウト 1:OK</returns>
1543 ''' <remarks>
1544 ''' Date   : 2021/07/07 : M.Hayakawa
1545 ''' </remarks>
1546 Function M% fnRoboPosChk
1547     fnRoboPosChk = 0
1548     MRet = fnStepRead()
1549     '初期位置でないと判断した場合
1550     'ウィンド画面切換え
1551     If MRBTOpeGroupNo > 5 Then
1552         '下記キー待ちの継続に反応させないため
1553         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
1554         Dly 0.2
1555         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
1556         Dly 1.5
1557         '
1558         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
1559         '
1560         MLoopFlg% = 1
1561         While MLoopFlg% = 1
1562             '
1563             '
1564             MKeyNumber% = fnKEY_WAIT()
1565             Select MKeyNumber%
1566                 Case Is = MAbout%       '停止
1567                     M_20# = MAbout%
1568                     MLoopFlg% = -1
1569                     Break
1570                 Case Is = MNext%        '次へ
1571                     'MLoopFlg% = -1
1572                     Break
1573                 Case Is = MContinue%    '継続
1574                     M_20# = MContinue%
1575                     MLoopFlg% = -1
1576                     Break
1577                 Default
1578                     Break
1579             End Select
1580         WEnd
1581     EndIf
1582     '
1583     If M_20# = MContinue% Then                              '継続ボタンが押された場合
1584         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
1585         Ovrd 5                                   '低速オーバーライド値設定
1586         Select MRBTOpeGroupNo
1587             Case Is = 5                          '何もしない
1588                 Break
1589             Case Is = 10                         '初期位置へ戻す
1590                 'Mov PTEST001
1591                 Break
1592             Case Is = 15                         '初期位置へ戻す
1593                 'Mov PTEST002
1594                 Dly 0.5
1595                 'Mov PTEST001
1596                 Dly 0.5
1597                 Break
1598             Default
1599                 Break
1600         End Select
1601         '
1602         Ovrd M_NOvrd                            'システムの初期値を設定
1603         M_Out(12364) = 1                        'toPLC_データ保存ON
1604         MRBTOpeGroupNo = 5
1605         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
1606         Dly 1.0
1607         M_Out(12364) = 0                        'toPLC_データ保存OFF
1608         fnRoboPosChk = 1                        '初期位置動作実行
1609         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
1610     EndIf
1611     Exit Function
1612 FEnd
1613 '
1614 '■frInCheck
1615 ''' <summary>
1616 ''' センサーINチェック
1617 ''' </summary>
1618 '''<param name="MINNumber%">入力番号</param>
1619 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
1620 '''<param name="MTimeCnt&">タイムアウト時間</param>
1621 '''<returns>整数 0:タイムアウト 1:OK</returns>
1622 ''' <remarks>
1623 ''' Date   : 2021/07/07 : M.Hayakawa
1624 ''' </remarks>
1625 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
1626     M_Timer(4) = 0
1627     MloopFlg = 0
1628     While MloopFlg = 0
1629         MCrtTime& = M_Timer(4)
1630         If M_In(MINNumber%) = MCMPFLG% Then
1631             MloopFlg = 1
1632             frInCheck = 1
1633         ElseIf MCrtTime& > MTimeCnt& Then
1634             MloopFlg = 1
1635             frInCheck = 0
1636         EndIf
1637     WEnd
1638 FEnd
1639 '-----------------------------------------------
1640 '
1641 'ねじ締め機通信確認
1642 '
1643 '-----------------------------------------------
1644 Function M% fScewTcomChk
1645     fScewTcomChk = 0
1646     '通信確認送信
1647     M_Out(MOUT_ScwT_ComChk%) = MOn%
1648     '通信確認受信待機
1649     Wait M_In(MIN_ScwT_comOK%) = MOn%
1650     '通信確認送信終了
1651     M_Out(MOUT_ScwT_ComChk%) = MOff%
1652  '
1653 FEnd
1654 '
1655 '
1656 '-----------------------------------------------
1657 '
1658 'ねじ締め開始送信
1659 '
1660 '-----------------------------------------------
1661 Function M% fScewTStart
1662     fScewTStart = 0
1663     'ねじ締め開始待機を受信
1664     Wait M_In(MIN_ScwT_STRec%) = MOn%
1665     Dly 0.1
1666     'ねじ締め開始受信を送信
1667     M_Out(MOUT_ScwT_ST%) = MOn%
1668     Dly 0.5
1669     'Wait M_In(MTEST_KEY%) = MOn%
1670     'ねじ締め開始送信終了
1671     M_Out(MOUT_ScwT_ST%) = MOff%
1672     '
1673 FEnd
1674 '
1675 '
1676 '-----------------------------------------------
1677 '
1678 'ねじ締め完了受信
1679 '
1680 '-----------------------------------------------
1681 Function M% fScewTFinish
1682     fScewTFinish = 0
1683     'ねじ締め完了待機を受信
1684     Wait M_In(MIN_ScwT_Fin%) = MOn%
1685     Dly 0.1
1686     'ねじ締め完了受信を送信
1687     M_Out(MOUT_ScwT_FinOK%) = MOn%
1688     Dly 0.5                          'とりあえず保持時間0.5msec
1689     'ねじ締め開始送信終了
1690     M_Out(MOUT_ScwT_FinOK%) = MOff%
1691     'Wait M_In(MTEST_KEY%) = MOn%
1692     '
1693 FEnd
1694 '
1695 '
1696 '-----------------------------------------------
1697 '
1698 '条件xx停止受信
1699 '
1700 '-----------------------------------------------
1701 Function M% fScewTCaseStop(ByVal MCase%())
1702     fScewTCaseStop = 0
1703     '条件xx停止を受信
1704     Wait M_In(MCase%(1)) = MOn%
1705     Dly 0.1
1706     '条件xx停止受信を送信
1707     M_Out(MCase%(2)) = MOn%
1708     Dly 0.5                          'とりあえず保持時間0.5msec
1709     'ねじ締め開始送信終了
1710     M_Out(MCase%(2)) = MOff%
1711     '
1712 FEnd
1713 '
1714 '
1715 '■fScrewTighenRoboCheck
1716 '<summary>
1717 'ねじロボ監視
1718 '</summary>
1719 '<param name = "MStopNum%"> 停止番号</param>
1720 '<returns>整数 0:ねじロボ異常終了 1:OK </returns>
1721 '<make>
1722 '2021/12/2 中村天哉
1723 '</make>
1724 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
1725     fnAutoScreenComment(503)    '状態表示[ねじロボ動作終了待ち] 2022/04/26 渡辺
1726     fScrewTighenRoboCheck = 1
1727     MScrewTighenRoboFlg% = 1    'フラグの初期化
1728     MCheck% = 0
1729     While MScrewTighenRoboFlg% = 1
1730         MCheck% = M_In16(11904)
1731         If M_In(MStopNum%) = 1 Then '停止位置まで来たら
1732             MScrewTighenRoboFlg% = 0 '関数を抜ける
1733             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1734         EndIf
1735         If MCheck% <> 0 Then
1736             fScrewTighenRoboError(MCheck%)
1737             Select M_20#
1738                 Case MAbout%            '停止が押された場合
1739                     M_Out(12869) = 1 Dly 1.0
1740                     MScrewTighenRoboFlg% = 0
1741                     fScrewTighenRoboCheck = 0   '異常終了
1742                     Break
1743                 Case MNgProcess%        'NGが押された場合
1744                     M_Out(12873) = 1 Dly 1.0
1745                     MScrewTighenRoboFlg% = 0
1746                     fScrewTighenRoboCheck = 0   '異常終了
1747                     Break
1748                 Case MContinue%             'リトライが押された場合
1749                     M_20# = MClear%         'M_20#初期化
1750                     M_Out(12871) = 1 Dly 1.0
1751                     Break
1752                 Case MNext%                 '次へが押された場合
1753                     M_20# = MClear%         'M_20#初期化
1754                     M_Out(12874) = 1 Dly 1.0
1755                     Break
1756             End Select
1757             Dly 0.5
1758         EndIf
1759     WEnd
1760 FEnd
1761 '■fScrewTighenRoboError
1762 '<summary>
1763 'ねじロボエラー処理
1764 '</summary>
1765 '<param name = "ErrorCode%"> エラー番号</param>
1766 '<make>
1767 '2021/12/2 中村天哉
1768 '</make>
1769 Function fScrewTighenRoboError(ErrorCode%)
1770     MCommentD1001 = ErrorCode% + 300
1771     fErrorProcess(11,MCommentD1001,0,0)
1772 FEnd
1773 '
1774 '■fErrorProcess
1775 '<summary>
1776 'エラー処理
1777 '</summary>
1778 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
1779 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
1780 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
1781 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
1782 '<make>
1783 '2021/11/5 中村天哉
1784 '</make>
1785 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
1786     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
1787     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
1788     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
1789     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
1790     MKeyNum% = 0
1791 *RETRY_ERR_PROCESS
1792      M_20# = MClear%     '初期化
1793 '        'エラー処理記述
1794         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
1795 '        'GOT KEY入力待ち
1796         MKeyNum% = fnKEY_WAIT()
1797 '        '
1798         If MKeyNum% = MAbout% Then   '停止を選択した場合
1799             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1800             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1801             Break
1802          '
1803         ElseIf MKeyNum% = MContinue% Then   '継続を選択した場合
1804             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1805             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1806         '
1807         ElseIf MKeyNum% = MNext% Then   '次へを選択した場合
1808             M_20# = MNext%            'M_20# プログラム間共通外部変数
1809             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1810          '
1811         ElseIf MKeyNum% = MNgProcess% Then   '停止を選択した場合
1812             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1813             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1814             Break
1815         '
1816         EndIf
1817         '
1818         If M_20# = MClear% Then *RETRY_ERR_PROCESS
1819 FEnd
1820 '
1821 '■fnTorqueCheck
1822 ''' <summary>
1823 ''' トルクチェック動作用のメイン
1824 ''' </summary>
1825 ''' <remarks>
1826 ''' Date   : 2021/12/21 : H.AJI
1827 ''' </remarks>'
1828 Function M% fnTorqueCheck
1829     'トルクチェック中送信  搬送系停止
1830     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
1831     '
1832     fnTorqueCheck = 0
1833     Ovrd 20
1834     Mov PInitialPosition              '初期位置移動
1835     Accel 100 , 20
1836     Mvs PHandChange                   'ハンド交換位置
1837     Accel 100 , 100
1838     Ovrd 100
1839     '下記キー待ちの継続に反応させないため
1840     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
1841     Dly 0.2
1842     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
1843     '
1844     'M6340  トルクチェック受信
1845     'Dly 5.0
1846     M_Out(12340) = 1          'トルクチェック受信 M6340
1847     Dly 1.0
1848     M_Out(12340) = 0
1849     '
1850     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
1851     M_Out(12835) = 1                         'ねじロボトルクチェック画面切替
1852    Wait M_In(11843) = 1                         'ねじロボトルクチェック画面切替
1853     M_Out(12835) = 0                         'ねじロボトルクチェック画面切替完了
1854     '
1855     '
1856     MLoopFlg = 1
1857     While MLoopFlg = 1
1858         '
1859 '        Mov PInitialPosition              '初期位置移動
1860         '
1861         MKeyNumber = fnKEY_WAIT()
1862         Select MKeyNumber
1863             Case Is = 1           '停止
1864                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
1865                 Dly 1.0
1866                 M_Out(12343) = 0
1867                 Ovrd 20
1868                 'Mov PTicketRead_1
1869                 M_Out(12840) = 1          'トルクチェック終了
1870                 Wait M_In(11859) = 1      'ねじロボからの終了
1871                 M_Out(12840) = 0          'トルクチェック終了
1872                 Ovrd 100
1873                 M_20# = 1
1874                 MLoopFlg = -1
1875                 Break
1876             Case Is = 2           '次へ
1877                 Break
1878             Case Is = 3           '継続
1879                 Break
1880             Case Is = 4           'トルクチェック開始
1881                 M_Out(12342) = 1          'トルクチェック開始要求受信 M6342
1882                 Dly 1.0
1883                 M_Out(12342) = 0
1884                 If M_In(11862) = 1 Then             'トルクチェッカー確認
1885                     fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
1886                     MRet = fnScrewMTorque()           'ねじロボ用トルクチェック
1887                 EndIf
1888                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
1889                 'MRet = fnMoveTorquePosi()
1890                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
1891                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1892                 Break
1893             Default
1894                 Break
1895         End Select
1896     WEnd
1897     '
1898     'トルクチェック中停止送信
1899     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
1900     '
1901     'ロボットの位置を元に戻す
1902     Mvs PInitialPosition            'イニシャルポジション
1903     '
1904     '
1905  FEnd
1906  '
1907 '
1908 '
1909 '---------------------------
1910 '
1911 '    メイン画面の表示、非表示設定
1912 '         コメントD1001, D1002, D1003の設定
1913 '           MWindReSet = 0     画面非表示
1914 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
1915 '           MWindErrScr = 10    エラー画面 D1001, D1002
1916 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
1917 '
1918 '---------------------------
1919 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
1920     fnMainScreenOpen = 0
1921     '
1922    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
1923         M_Out16(12480) = MCommentD1001            'D1001 コメント
1924     EndIf
1925     '
1926     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
1927         M_Out16(12496) = MCommentD1002            'D1002 コメント
1928     EndIf
1929     '
1930     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
1931         M_Out16(12512) = MCommentD1003            'D1003 コメント
1932     EndIf
1933     '
1934     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
1935     M_Out(12362) = 1                         'ウィンド画面設定  M6362
1936     Dly 0.5
1937     M_Out(12362) = 0                         'ウィンド画面設定
1938 FEnd
1939 '
1940 '■Main
1941 ''' <summary>
1942 ''' トルクチェック実動作
1943 ''' </summary>
1944 ''' <remarks>
1945 ''' Date   : 2021/12/21 : H.AJI
1946 ''' </remarks>'
1947 Function M% fnScrewMTorque
1948     fnScrewMTorque = 0
1949     M_Out(12838) = 1                         'トルクチェック開始1
1950     Wait M_In(11857) = 1                     '受信完了
1951     M_Out(12838) = 0                         'トルクチェック開始1
1952     Dly 2.0
1953 FEnd
1954 '
1955 '■Main
1956 ''' <summary>
1957 ''' トルクチェック実動作
1958 ''' </summary>
1959 ''' <remarks>
1960 ''' Date   : 2021/12/21 : H.AJI
1961 ''' </remarks>'
1962 Function M% fnMoveTorquePosi
1963      fnMoveTorquePosi = 0
1964      Ovrd 50
1965     'Mov PTorquePosi000 'トルクチェック回避位置へ移動
1966      Mov PTorqueCheck_1 'トルクチェックメーター上空へ移動
1967     'Mov PTorquePosi020 'トルクチェックビットジョイント上空
1968     '
1969     '
1970      '以下は阿部さんが作成したトルクチェックプラグラム
1971     '
1972     Spd M_NSpd
1973 '-------------      ドライバーRST
1974     M_Out(12240)=0     'ドライバーOFF CCW
1975     M_Out(12241)=0     'ドライバーOFF CW
1976     M_Out(12242)=0     'ドライバー解除 C1
1977     M_Out(12243)=0     'ドライバー解除 C2
1978     M_Out(12245)=0     'プログラム解除 F1/プログラム2
1979 '---------------------------------------
1980 '---------------------------------------
1981     Fsc Off            '力覚センサ　Off  STEP1は不要
1982 '--------------------------------------------------------------
1983 '--------------------------------------------------------------
1984 '[P-11]
1985 '--------------------------------------------------------------   【トルクチェック 0.4N - P11】
1986     Mov PTorqueCheck, -50                     ' トルク-1　置き位置上空 50mm へ移動
1987    'Mov PTorquePosi020, -10                    ' トルク-1　置き位置上空 10mm へ移動
1988     Dly 0.1
1989 '-----------------------
1990    'Cnt 0                           'Cnt動作-2　終了
1991 '-----------------------
1992     Mov PTorqueCheck , -5                      'トルク-1　置き位置上空 5mm へ移動
1993     Dly 0.2
1994 '-----------------------
1995     M_Out(12242)=1                   'ドライバーセット C1
1996     Dly 0.1
1997     M_Out(12243)=1                   'ドライバーセット C2 (バンク3)
1998     Dly 0.1
1999     M_Out(12245)=1                   'プログラム2セット F1  Mネジ
2000     Dly 0.1
2001     'M_Out(12241)=1                   'ドライバーON  CW
2002    M_Out(12241)=0                   'ドライバーOFF  CW
2003     'Dly 0.1
2004 '--------------------------------
2005     Ovrd 40
2006    'Dly 0.1
2007 '--------------------------------  ネジ締め速度設定
2008     Spd 14                            'ライド 100-40 100% :Spd 12
2009     Dly 0.1
2010 '--------------------------------
2011 '--------------------------------
2012 '---------------------------------【ねじ締め動作】
2013 '
2014     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '移動中エラー検出
2015    Mvs PTorqueCheck               'トルクチェック位置へ移動
2016     Dly 0.3                          '動作安定待ち
2017    M_Out(12241)=1                   'ドライバーON  CW
2018 '
2019     Wait M_In(11584)=1                '完了/エラー検出
2020     Dly 0.1
2021     Spd M_NSpd
2022    'Ovrd 20
2023     If M_In(11256)=1 Then *LBL1       'ネジトータルエラー検出
2024     Wait M_In(11257)=1                'ネジ完了SC
2025 '---------------------------------
2026     Dly 0.1
2027     M_Out(12241)=0                    'ドライバーOFF CW
2028     Dly 0.1
2029     M_Out(12242)=0                    'ドライバー解除 C1
2030     Dly 0.1
2031     M_Out(12243)=0                    'ドライバー解除 C2 (バンク3)
2032     Dly 0.1
2033     M_Out(12245)=0                    'プログラム2解除 F1
2034 '--------------------------------------------------------------   【トルクチェック 0.4N - P11ここまで】
2035 '
2036     Mvs PTorqueCheck,-60                       'あえてmov から変更
2037     Dly 0.1
2038 '--------------------------------------------------------------
2039    'Ovrd 80
2040 '--------------------------------------------------------------
2041 '---------------------------------------
2042 '---------------------------------------
2043 '---------------------------------------エラー離脱処理
2044    *LBL1
2045    Fsc Off            '力覚センサ　Off   *STEP1は不要
2046    Mvs ,-100
2047    M_Out(12241)=0     'ドライバーOFF CW
2048    Dly 0.1
2049    M_Out(12242)=0     'ドライバー解除 C1
2050    Dly 0.1
2051    M_Out(12243)=0     'ドライバー解除 C2 (バンク3)
2052    Dly 0.1
2053    M_Out(12245)=0     'プログラム解除 F1
2054 '---------------------------------------
2055 '---------------------------------------
2056 '-------------
2057    'Mov PInitPos19049
2058    Dly 0.1
2059 '
2060 '
2061 '
2062 FEnd
2063 '
2064 ''■Main
2065 ''' <summary>
2066 ''' 組立動作用のメイン
2067 ''' </summary>
2068 ''' <remarks>
2069 ''' Date   : 2021/07/07 : M.Hayakawa
2070 ''' </remarks>'
2071 Function Main
2072     MopeNo = M_21#         '外部変数にて動作番号代入
2073     '
2074     If M_Svo=0 Then
2075         Servo On
2076     EndIf
2077     Wait M_Svo=1
2078 '組立スタート日付時刻要求パルスON (別スロットの8から要求に変更）
2079 '    M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
2080 'パトライト操作
2081     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
2082     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
2083     '
2084     M_20# = 0                                   'KEY入力初期化
2085 '    M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化(位置移動1/18中村)
2086     MRet% = 0
2087 ' 初期位置の確認と移動(現状維持のままパレットのみ動作のため削除)
2088 '
2089 '復帰動作　実行・未実行判別      2022/03/22 渡辺 作成
2090     PActive = P_Curr                    '現在位置を取得
2091     MRecoveryPass% = 0
2092     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
2093         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
2094             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
2095                 MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
2096             EndIf
2097         EndIf
2098     EndIf
2099     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
2100         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
2101             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
2102                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
2103             EndIf
2104         EndIf
2105     EndIf
2106     If (PActive.X <= PMechaOnJigGet_3.X + 1.0) And (PActive.X >= PMechaOnJigGet_3.X -1.0) Then
2107         If (PActive.Y <= PMechaOnJigGet_3.Y + 1.0) And (PActive.Y >= PMechaOnJigGet_3.Y -1.0) Then
2108             If (PActive.Z <= PMechaOnJigGet_3.Z + 1.0) And (PActive.Z >= PMechaOnJigGet_3.Z -1.0) Then
2109                 MRecoveryPass% = 1       'DVD取り位置上空位置は復帰動作パス
2110             EndIf
2111         EndIf
2112     EndIf
2113     If MRecoveryPass% = 0 Then
2114        fnInitialZoneB()        '復帰動作パスフラグが立っていない時は復帰動作を実行
2115     EndIf
2116 ' ねじロボを初期位置に戻すために強制的に自動運転開始(現状維持のままパレットのみ動作のため削除)
2117 '
2118     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
2119         M_Out(12364) = 1            'toPLC_データ保存ON
2120 'トルクチェック
2121         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
2122             MRet% = fnTorqueCheck()
2123             Break
2124         Else
2125 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認'12/21コメントアウト(中村)
2126 '                MRtn = InspInit()               '画像処理初期化処理
2127 '            EndIf
2128             '
2129            M_20# = MClear%                    '初期化
2130 '組立開始
2131             If M_In(MIN_ASSY_CANCEL%) = 0 Then
2132                 fnAssyStart()
2133             Else
2134                 M_20# = MPass%
2135             EndIf
2136             M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化(位置移動1/18中村)
2137 '組立終了日付時刻
2138             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
2139             Wait M_In(11572) = 1            '日付取得完了
2140             Dly 0.1
2141             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
2142 'リフターユニットへのOUT
2143             '  KEY入力が何もない場合 OKと判断
2144             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
2145             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
2146 'OK/NGフラグ出力
2147             If M_20# = MAssyOK% Or M_22# = MIrregular% Then
2148                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
2149             ElseIf M_20# = MPass% Then
2150                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
2151             EndIf
2152 'PIASに組立完了書込み  CDにつき削除 2022/07/16 M.H
2153             '
2154             '組立終了日付時刻解除
2155             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
2156             '投入数、組立OK数、組立NG数書込み
2157 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
2158             '
2159 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認'コメントアウト12/21(中村)
2160 '                '画像処理終了処理
2161 '                MRtn = InspQuit()
2162 '            EndIf
2163         EndIf
2164         M_Out(12364) = 0                          'toPLC_データ保存OFF
2165     EndIf
2166 'パトライト操作
2167     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
2168     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
2169 'GOT表示
2170     fnAutoScreenComment(93)  'AUTO画面 工程完了
2171 FEnd
2172 End
2173 '
2174 '
2175 'おまじないコメント
2176 '絶対削除するな
2177 '
2178 '
2179 '
2180 '
2181 '
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
PTemp=(+303.63,-1.43,+467.78,-0.01,+90.00,+0.00,+0.00,+0.00)(6,0)
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
PActive=(+303.63,-1.43,+467.78,-0.01,+90.00,+0.00)(6,0)
Pmove=(+244.33,+180.27,+500.00,+23.65,+90.00,+60.35)(6,0)
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
PBracketFSet=(-197.56,+547.22,+455.08,-179.60,-0.16,-89.30)(7,0)
PBracketFSet_1=(-197.56,+547.22,+470.00,-179.60,-0.16,-89.30)(7,0)
PBracketFSet_2=(-197.56,+547.22,+540.00,-179.60,-0.16,-89.30)(7,0)
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
JActive=(+36.31,-16.13,+140.49,-0.69,-34.36,+0.57)
Jmove=(+36.31,-15.80,+124.16,+0.00,+71.59,+0.57,+0.00,+0.00)
JTaihi=(+0.00,-15.80,+124.16,+0.00,+71.59,+0.00,+0.00,+0.00)
