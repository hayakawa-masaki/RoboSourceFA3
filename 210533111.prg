1 ' ===================================
2 '
3 '  21053001 STEP5 Assy5プログラム
4 '
5 ' 作成者：M.Hayakawa
6 ' 作成日：2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1から流用
8 ' ===================================
9 '===== <Insight定数> =====
10 '===== <Insight変数定義> =====
11 Dim PInspPosition(30)               '画像処理Function引渡し用位置変数
12 Dim MInspGroup%(30)                 '画像処理Function引渡し用変数
13 Def Inte MIN_IS_Ready               '【入力IO】Insight準備OK
14 Def Inte MIN_IS_JobLoadOK           '【入力IO】Insightジョブロード正常終了
15 Def Inte MIN_IS_JobLoadNG           '【入力IO】Insightジョブロード異常終了
16 Def Inte MIN_IS_InspGSetOK          '【入力IO】Insight検査グループ番号設定正常終了
17 Def Inte MIN_IS_InspGSetNG          '【入力IO】Insight検査グループ番号設定異常終了
18 Def Inte MIN_IS_InspOK              '【入力IO】Insight検査OK
19 Def Inte MIN_IS_InspNG              '【入力IO】Insight検査NG
20 Def Inte MIN_IS_InspErr             '【入力IO】Insight検査異常終了
21 Def Inte MIN_IS_InspCapDone         '【入力IO】Insight検査画像取込完了
22 '
23 Def Inte MIN_IS_ErrNum              '【入力IO】Insight処理エラー番号取得開始アドレス(16bit)
24 'Output Signal
25 Def Inte MOUT_IS_JobLoadReq         '【出力IO】Insight JOBロード要求
26 Def Inte MOUT_IS_InspGSetReq        '【出力IO】Insight 検査グループ番号設定要求
27 Def Inte MOUT_IS_Insp               '【出力IO】Insight 検査実行要求
28 '
29 Def Inte MOUT_IS_JobNum             '【出力IO】Insight JOB番号設定開始アドレス(16bit)
30 Def Inte MOUT_IS_InspGNum           '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
31 '
32 Def Inte MOUT_InspErrNum            '【出力IO】検査実行エラー番号開始アドレス(16bit)
33 Def Inte MOUT_InspNGStepNum         '【出力IO】検査実行NGStep番号開始アドレス(16bit)
34 '作業用変数
35 Def Inte MInspErrNum                '検査実行エラー番号
36 Def Inte MInspNGStepNum             '検査実行NGStep番号
37 Def Inte MRtn                       'Function戻り値取得用
38 Def Inte MRtn2                      'Function戻り値取得用
39 Def Inte MRet3                      'Function戻り値取得用
40 Def Inte MGRtn                      'Function戻り値取得用 ネジ供給機
41 Def Inte MInspErrNumSub             '検査実行エラー番号sub　20190820追加
42 Def Inte MovrdA                     'ネジ締めOvrd 可変用
43 Def Float MSpdA                     'ネジ締めSpd　可変用
44 Def Pos PTemp                       'ネジ締め上空位置計算用
45 '===== <Insight変数設定> =====
46 MIN_IS_Ready%        =   11380      '【入力IO】Insight準備OK
47 MIN_IS_JobLoadOK%    =   11381      '【入力IO】Insightジョブロード正常終了
48 MIN_IS_JobLoadNG%    =   11382      '【入力IO】Insightジョブロード異常終了
49 MIN_IS_InspGSetOK%   =   11383      '【入力IO】Insight検査グループ番号設定正常終了
50 MIN_IS_InspGSetNG%   =   11384      '【入力IO】Insight検査グループ番号設定異常終了
51 MIN_IS_InspOK%       =   11385      '【入力IO】Insight検査OK
52 MIN_IS_InspNG%       =   11386      '【入力IO】Insight検査NG
53 MIN_IS_InspErr%      =   11387      '【入力IO】Insight検査異常終了
54 MIN_IS_InspCapDone%  =   11388      '【入力IO】Insight検査画像取込完了
55 MIN_IS_ErrNum%       =   11408      '【入力IO】Insight処理エラー番号開始アドレス(16bit)
56 'Output Signal
57 MOUT_IS_JobLoadReq%  =   12370      '【出力IO】Insight JOBロード要求
58 MOUT_IS_InspGSetReq% =   12371      '【出力IO】Insight 検査グループ番号設定要求
59 MOUT_IS_Insp%        =   12372      '【出力IO】Insight 検査実行要求
60 MOUT_IS_JobNum%      =   12384      '【出力IO】Insight JOB番号設定開始アドレス(16bit)
61 MOUT_IS_InspGNum%    =   12400      '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
62 MOUT_InspErrNum%     =   12416      '【出力IO】検査実行エラー番号開始アドレス(16bit)
63 MOUT_InspNGStepNum%  =   12432      '【出力IO】検査実行NGStep番号開始アドレス(16bit)
64 '===== <電ドラ変数定義> =====
65 X20_Driver=11248                    '電ドラステイタス1　Driver Status 1
66 X21_Driver=11249 '電ドラステイタス2  Driver Status 2
67 X22_Driver=11250 '電ドラステイタス3  Driver Status 3
68 X23_Driver=11251 '電ドラステイタス4  Driver Status 4
69 X24_Driver=11252 '電ドラエラーメッセージ1 Driver Error E1
70 X25_Driver=11253 '電ドラエラーメッセージ2 Driver Error E2
71 X26_Driver=11254 '電ドラエラーメッセージ3 Driver Error E3
72 X27_Driver=11255 '電ドラエラーメッセージ4 Driver Error E4
73 X28_Driver=11256 '電ドラトータルエラーシグナル Total Error
74 X29_Driver=11257 '電ドラ終了シグナル Comlete signal
75 X2A_Driver=11258 '電ドラエラーメッセージ5 Driver Error E5
76 '11584   'toRBトルクドライバ-COMP_ERR送信
77 Y60_Driver=12240 '電ドラ半時計回り CCW
78 Y61_Driver=12241 '電ドラ時計回り CW
79 Y62_Driver=12242 'バンクセッティング BANK C1
80 Y63_Driver=12243 'バンクセッティング BANK C2
81 Y64_Driver=12244 'バンクセッティング BANK C3
82 Y65_Driver=12245 'プログラムセッティング PRG SET F1
83 Y66_Driver=12246 'プログラムセッティング PRG SET F2
84 Y67_Driver=12247 'プログラムセッティング PRG SET F3
85 X34_ScrewReady1=11259 'ねじっこ1　Read
86 '===== <電ドラ定数> =====
87 Dim PScrewPos(10)       'ネジ締め用Function引数変数
88 Dim PGetScrewPos(10)    'ねじ供給機からねじを得るFunction引数変数
89 Dim PEscapePosi(10)
90 MLoopCnt% = 0'
91 '===== <ロボット定数> =====
92 '===== <ロボット変数定義> =====
93 MRBTOpeGroupNo = 0      'ロボット動作番号初期化
94 MCommentD1001 = 0
95 MCommentD1002 = 0
96 MCommentD1003 = 0
97 MScreenNo = 0
98 '
99 MCommentTSU = 0
100 MCommentTSD = 0
101 'ウィンド画面番号設定
102 MWindReSet = 0
103 MWindInfoScr = 5
104 MWindErrScr = 10
105 MWindErrScr2 = 11
106 MWindErrScr3 = 13
107 MWindErrScr17 = 17
108 MWindErrScr18 = 18
109 MWindCmmnScr = 20
110 MWindJigRelase19049 = 60
111 MWindJigRelase19050 = 61
112 MWindJigRelase19051 = 62
113 '
114 MClear% = 0        'KEY_のクリア
115 MAbout% = 1        'KEY_停止
116 MNext% = 2         'KEY_次のステップへ移行
117 MContinue% = 3     'KEY_継続 再度同じ動作を行う
118 '
119 Def Inte MNgProcess
120 MNgProcess% = 5      'KEY_NG
121 '
122 MAssyOK% = 6       '組立完了
123 MPass% = 7         '工程パス
124 MPiasNG% = 8       'Pias確認時履歴NG
125 '
126 '初期化用KEY番号   '
127 MRobotInit1% = 11  '初期位置用
128 MRobotInit2% = 12  '初期位置用
129 MRobotInit3% = 13  '初期位置用
130 MRobotInit4% = 14  '初期位置用
131 '
132 MIN_INIT1REQUEST% = 11568 'toRBT_ロボット初期位置1要求
133 MIN_INIT2REQUEST% = 11569 'toRBT_ロボット初期位置2要求
134 MIN_INIT3REQUEST% = 11570 'toRBT_ロボット初期位置3要求
135 MIN_INIT4REQUEST% = 11571 'toRBT_ロボット初期位置4要求
136 '
137 MOUT_INIT1RECIVE% = 12560 'toPLC_ロボット初期位置1受信
138 MOUT_INIT2RECIVE% = 12561 'toPLC_ロボット初期位置2受信
139 MOUT_INIT3RECIVE% = 12562 'toPLC_ロボット初期位置3受信
140 MOUT_INIT4RECIVE% = 12563 'toPLC_ロボット初期位置4受信
141 '
142 MOK% = 1               '各判定用
143 MNG% = 0               '各判定用
144 MTIMEOUT% = -1         '各判定用
145 MJudge% = 0            '判定情報格納用
146 '
147 MRECIVETIME& = 0
148 MSETTIMEOUT10& = 10000&                '10秒設定
149 MSETTIMEOUT03& = 3000&                 '3秒設定
150 MSETTIMEOUT01& = 1000&                 '1秒設定
151 MSETTIMEOUT05& = 5000&                 '5秒設定
152 MSETTIMEOUT009& = 900&                 '0.9秒設定
153 MSETTIMEOUT008& = 800&                 '0.8秒設定
154 MSETTIMEOUT007& = 700&                 '0.7秒設定
155 MSETTIMEOUT006& = 600&                 '0.6秒設定
156 MSETTIMEOUT005& = 500&                 '0.5秒設定
157 MSETTIMEOUT004& = 400&                 '0.4秒設定
158 MSETTIMEOUT003& = 300&                 '0.3秒設定
159 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
160 MIN_PIAS_ComOK% = 11552                'PC通信OK
161 MIN_PIAS_ComTimeOut% = 11576           'PC通信確認タイムアウト
162 MIN_PIAS_ComNG% = 11553                'PC通信NG
163 MOUT_PIAS_ComCheck% = 12544            'PC通信確認要求
164 MOUT_PIAS_Missing_Process% = 12546     '工程抜け確認要求
165 MIN_PIAS_ModelTypeNG% = 11554          'モデル仕向NG
166 MIN_PIAS_ProcessHistryNG% = 11555      '前工程履歴NG
167 MIN_PIAS_ProcessHistryOK% = 11556      '前工程履歴OK
168 MIN_PIAS_ProcessHistryErr% = 11557     '工程履歴処理エラー
169 MIN_PIAS_MyProcessComp% = 11573        '自工程履歴あり
170 MIN_PIAS_ProcessHistryTimeOut% = 11578 '工程履歴タイムアウト
171 MOUT_OKNG% = 12549                     'PLC OUT でOK=1, NG=0 出力
172 '
173 MOUT_PiasPCBNumberCheck = 12557        '基板番号照合
174 MIN_PiasPCBNumberOK% = 11566          '基板番号OK
175 MIN_PiasPCBNumberNG% = 11565          '基板番号NG
176 MIN_PiasPCBNumberErr% = 11567         '基板番号処理エラー
177 '
178 MOUT_PiasAssyResultOK% = 12549    '組立OK
179 MOUT_PiasAssyResultNG% = 12550    '組立NG
180 MOUT_PiasAssyResultWr% = 12548    '工程履歴書き込み
181 '
182 MIN_PiasProcessNG% = 11559        '工程履歴処理NG
183 MIN_PiasProcessOtherErr% = 11560  '工程履歴処理エラー(なんかのトラブル)
184 MIN_PiasProcessOK% = 11558        '工程履歴処理OK
185 '
186 MIN_Insight_Use% = 11369               '画像確認ON
187 MIN_TorqueCheck% = 11348               'トルクチェック
188 '
189 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT操作権
190 MOUT_RED_LIGHT% = 12356            'PATLIGHT 赤 点灯
191 MOUT_RED_FLASH% = 12357            'PATLIGHT 赤 点滅
192 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT 黄 点灯
193 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT 黄 点滅
194 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT 青 点灯
195 MOUT_GREEN_FLASH% = 12361          'PATLIGHT 青 点滅
196 '
197 MOUT_ST_DATETIME% = 12551          '組立開始日付時刻
198 MOUT_ED_DATETIME% = 12552          '組立終了日付時刻
199 '
200 MOUT_TORQUE_CHECK% = 12367         'PLCへトルクチェック中を送信
201 '
202 MIN_ASSY_CANCEL% = 11366           '組立を行うかのフラグ
203 '
204 MLoopFlg% = 0                      'KEY入力後のOK or NG内容
205 MopeNo% = 0
206 MRtn% = 0
207 MRet = 0
208 MRet3% = 0
209 '
210 Def Inte MInputQty          '投入数 演算変数
211 Def Inte MAssyOkQty         '組立ＯＫ数 演算変数
212 Def Inte MAssyNgQty         '組立ＮＧ数 演算変数(未使用)
213 Def Inte MSuctionErrQty     '吸着エラー数 2022/04/27 渡辺
214 Def Inte nAssyOkQty         '未使用
215 Def Inte MScrewNo
216 Def Inte MReTry
217 '===== <IO変数定義> =====
218 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
219 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
220 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
221 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
222 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
223 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
224 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
225 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
226 '
227 Def Inte Y6A_VV1            ' アーム先端　ネジ吸着バルブ
228 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
229 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
230 '
231 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
232 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
233 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
234 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
235 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
236 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
237 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
238 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
239 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
240 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
241 '
242 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
243 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
244 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
245 '
246 Def Inte MOUT_LED1          ' 画像処理用LED照明
247 '
248 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
249 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
250 '
251 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
252 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
253 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
254 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
255 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
256 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
257 '
258 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
259 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
260 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
261 '
262 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
263 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
264 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
265 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
266 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
267 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
268 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
269 Y6A_VV1%    =  12250    ' アーム先端　ネジ吸着バルブ
270 Y6B_VB1%    =  12251    'アーム先端　吸着破壊バルブ
271 MOUT_VB1%   =  12251    ' アーム先端　ネジ吸着破壊バルブ
272 '
273 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
274 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
275 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
276 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
277 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
278 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
279 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
280 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
281 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
282 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
283 '
284 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
285 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
286 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
287 '
288 MOUT_LED1%  =  12239    ' 画像処理用LED照明
289 '
290 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
291 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
292 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
293 '
294 '共通
295 Def Inte MTEST_KEY                      'デバックテスト用
296 Def Inte MOn                            '出力=1
297 Def Inte MOff                           '出力=0
298 '
299 'ねじ締め装置_出力アドレス
300 Def Inte MOUT_ScwT_ComChk               '通信確認
301 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
302 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
303 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
304 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
305 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
306 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
307 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
308 'ねじ締め装置_入力アドレス
309 Def Inte MIN_ScwT_comOK                 '通信確認返信
310 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
311 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
312 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
313 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
314 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
315 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
316 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
317 '
318 Dim MScwT_Case1%(2)               '条件1停止変数
319 Dim MScwT_Case2%(2)               '条件2停止変数
320 Dim MScwT_Case3%(2)               '条件3停止変数
321 Dim MScwT_Case4%(2)               '条件4停止変数
322 Dim MScwT_Case5%(2)               '条件5停止変数
323 '
324 '共通
325 MTEST_KEY% = 11359                       'デバッグ用テストKEY
326 MOn% = 1                                 '出力 = 1
327 MOff% = 0                                '出力 = 0
328 '
329 'ねじ締め機_アドレス設定
330 MOUT_ScwT_ComChk% = 12832               '通信確認送信
331 MOUT_ScwT_ST% = 12865                   'ねじ締め開始を送信
332 MOUT_ScwT_ReSTOK% = 12866               '再開始受信を送信
333 MOUT_ScwT_FinOK% = 12868                'ねじ締め完了受信を送信
334 MOUT_ScwT_Case1OK% = 12874              '条件1停止受信を送信
335 MOUT_ScwT_Case2OK% = 12875              '条件2停止受信を送信
336 MOUT_ScwT_Case3OK% = 12876              '条件3停止受信を送信
337 MOUT_ScwT_Case4OK% = 12877              '条件4停止受信を送信
338 MOUT_ScwT_Case5OK% = 12878              '条件5停止受信を送信
339 '
340 MIN_ScwT_comOK% = 11840                 'ねじ締め装置から返信
341 MIN_ScwT_STRec% = 11873                 'ねじ締め開始を受信
342 MIN_ScwT_ReST% = 11874                  '再開始を受信
343 MIN_ScwT_Fin% = 11876                   'ねじ締め完了を受信
344 MIN_ScwT_Case1% = 11882                 '条件1停止待機を受信
345 MIN_ScwT_Case2% = 11883                 '条件2停止待機を受信
346 MIN_ScwT_Case3% = 11884                 '条件3停止待機を受信
347 MIN_ScwT_Case4% = 11885                 '条件4停止待機を受信
348 MIN_ScwT_Case5% = 11886                 '条件5停止待機を受信
349 '
350 MScwT_Case1%(1) = MIN_ScwT_Case1%
351 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
352 MScwT_Case2%(1) = MIN_ScwT_Case2%
353 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
354 MScwT_Case3%(1) = MIN_ScwT_Case3%
355 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
356 MScwT_Case4%(1) = MIN_ScwT_Case4%
357 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
358 MScwT_Case5%(1) = MIN_ScwT_Case5%
359 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
360 '
361 '設定 InitialZoneBで使用する変数
362 Def Pos PActive       '直交座標系 位置変数 現在位置
363 Def Pos Pmove         '直交座標系 位置変数 移動先
364 Def Jnt JActive       '関節座標系 位置変数 現在位置
365 Def Jnt Jmove         '関節座標系 位置変数 移動先
366 Def Jnt JTaihi        '関節座標系 位置変数 退避ポジション ティーチングで設定
367 Def Inte MRecoveryPass      '復帰動作パスフラグ　1=復帰動作をパス　0=復帰動作を実行
368 Def Inte MJ6          'J6軸の値を比較する為の変数
369 Def Inte MStandby              '待機位置確認フラグ
370 Def Inte MRecoveryChuckOpen    'チャック解放フラグ（復帰動作前）両掴み対策
371 '★注意★初期位置を追加変更した時には、変更が必要！
372 '
373 'Functionエラー対策
374 Def Inte MCtlNo
375 '
376 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
377 Function M% fnAssyStart
378     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
379 '   BaseUnit6通信確認
380     *RE_COM_CHECK
381     MRtn = 1    '初期化
382     If M_In(11920) = 0 Then     'BaseUnit6が拾得中のフラグを立てていない
383         If M_In(11930) = 0 And M_In(11931) = 0 Then   '通信にて回転角不明
384             Dly 2.3                                   '回転待ち
385             If M_In(11930) = 0 And M_In(11931) = 0 Then  'もう一度確認
386                 MRtn = 0                              '通信にて異常
387                 Break
388             EndIf
389             Break
390         EndIf
391         Break
392     EndIf
393     If MRtn = 1 Then GoTo *BU6Com_OK    '通信OKならラベルジャンプ
394     fErrorProcess(11,298,284,0)           '0,284→298,287に変更6/7中村
395     If M_20# = MNext% Then M_20# = MClear%
396     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
397     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
398     If M_20# = MContinue% Then GoTo *RE_COM_CHECK
399     *BU6Com_OK
400 '
401 '
402 ' PIASチケット読込み工程抜け確認
403     M_20# = MClear%                       '初期化
404 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
405 '        MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
406 '        '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
407 '        '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
408 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' メニューへ戻る
409 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NGを工程履歴に書込み次の工程へ
410 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NGを工程履歴に書込み次の工程へ
411 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' 履歴NG, 工程抜け
412 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK以外は組立終了
413 '    EndIf
414 '    '
415 '    '座標移動
416 '    '
417 '    '条件xx停止
418 '    fScewTCaseStop(MScwT_Case5%)
419 '    '
420 '    'ベースユニットKEY
421 '    Wait M_In(MTEST_KEY%) = MOn%
422 '    '
423 '    '再開始
424 '    fScewTReStart()
425 '    '
426 '    '座標移動
427 '    '
428 '    'ねじ締め完了
429 '    Mret% = fScewTFinish()
430 ' ネジ締めテスト終了
431 ' PIASテスト -----------
432 '    MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
433 '    MRet% = fnPiasWrite(MNG%)
434  '   MRet% = fnPCBNumberCheck()
435 ' PIASテスト終了 -------
436 '
437     '組立開始(9/6追加(中村))
438     'プログラム原点
439     Ovrd 100
440     ' ハンド状態初期化(10/29追加M.H)(2/11修正(中村))
441     Cmp Off                     'コンプライアンスモード終了
442     ColChk On                   '衝突検知ON
443     If M_In(11266) Then
444         M_Out(12256) = 0
445         M_Out(12257) = 1
446     EndIf
447     If M_In(11269) Then
448         M_Out(12258) = 0
449         M_Out(12259) = 1
450     EndIf
451     If M_In(11271) Then
452         M_Out(12260) = 0
453         M_Out(12261) = 1
454     EndIf
455     *WAIT_HAND_INI
456     If M_In(11265) = 1 And M_In(11268) = 1 And M_In(11270) = 1 Then GoTo *CompHandIni Else GoTo *WAIT_HAND_INI
457     *CompHandIni
458     M_Out(12257) = 0
459     M_Out(12259) = 0
460     M_Out(12261) = 0
461 '
462 '
463 'Dly 5                               'デバッグ用(22/09/30中村)
464     ' ねじ締め機テスト用 ----------
465     Mret% = fScrewTcomChk()
466     If Mret% = -1 Then GoTo *ASSY_ERROR_END
467     ' ねじ締め機通信開始
468 '    fScrewTStart()           '暫定コメントアウト解除(10/19中村)
469     'チケットIDを読む
470     PTemp = P_Curr
471     MRtn = 0
472 '    If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
473 '        If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
474 '            If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
475 '                MRtn = 1
476 '            EndIf
477 '        EndIf
478 '    EndIf
479 '    If MRtn = 1 Then
480 '        Mov PTicketRead
481 '    Else
482 '        Cnt 1 , 10 , 10
483 '        Mov PInitialPosition
484 '        Mov PTicketRead_1           'チケットID読み取り回避点
485 '        Cnt 0
486 '        Mvs PTicketRead             'ID読み位置
487 '    EndIf
488 '
489 ' 2022/04/12 安全方向へ条件変更 渡辺
490 ' PInitialPosition 在席 MStandby=2
491 ' PTicketRead_1 在席 MStandby=1
492 '
493     MStandby = 0    '待機位置フラグを初期化
494     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
495         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
496             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
497                 MStandby = 2
498             EndIf
499         EndIf
500     EndIf
501     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
502         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
503             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
504                 MStandby = 1
505             EndIf
506         EndIf
507     EndIf
508     If MStandby = 2 Then
509         Mov PTicketRead_1           'チケットID読み取り回避点
510         Cnt 0
511     EndIf
512     If MStandby <> 0 Then GoTo *PositionOK
513     fErrorProcess(11,230,281,0)            '初期位置にいない時はエラーにする
514     If M_20# = MNext% Then GoTo *ASSY_ERROR_END
515     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
516     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
517     If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
518     *PositionOK
519 '
520     Mvs PTicketRead             'ID読み位置
521 '
522     M_Out(12259) = 0            'DVDメカチャック開OfF
523     M_Out(12258) = 1            'DVDメカチャック閉ON
524 '
525     '
526     MRtn = 1        'MRtn初期化
527 *RE_TICKET_READ
528 '    MRtn = fnPiasCheck()               'ID読み取り
529 'PInspPosition(1) = PTicketRead  'IDチケット読取位置
530 'MInspGroup%(1) = 1              '検査G番号
531 'MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
532 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
533     MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
534     '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
535     '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
536 EndIf
537 If MRtn = 1 Then GoTo *CompRead
538     'エラー時製品位置決めを解除
539 *RE_ERR_REL_1
540 If M_20# = MContinue% Then M_20# = MRtn
541 M_Out(12263) = 1 Dly 0.5    '製品位置決めを解除
542 MRtn = frInCheck(11274,1,MSETTIMEOUT05&)
543 '
544 If MRtn = 1 Then GoTo *CompErrorRelease
545 MRtn = M_20#        'M_20#一時避難
546 M_20# = MClear%
547 fErrorProcess(11,234,284,0)     '位置決め戻端エラー
548 If M_20# = MContinue% Then GoTo *RE_ERR_REL_1
549 If M_20# = MNext% Then M_20# = MRtn
550 If M_20# = MNgProcess% Then M_20# = MAbout%
551 *CompErrorRelease
552 '
553 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
554 If M_20# = MNext% Then M_20# = MPass%
555 Mvs PTicketRead_1                         '22/04/07 追加 渡辺
556 GoTo *ASSY_ERROR_END
557 *CompRead
558 '
559 '    パレットから製品を取る
560     fScrewTStart()           'ネジ締め開始
561 '
562     Mov PProductOnPltGet_2      '本体受け取り上空回避点
563     M_Out(12256) = 0            '本体チャック閉OFF(処理位置変更2/11中村)
564     M_Out(12257) = 1            '本体チャック開ON
565 '
566     *RE_POSITIONING
567     '
568     M_Out(12262) = 1 Dly 0.5 '本体位置決め出端ON
569 '    Wait M_In(11273) = 1     '本体位置決め出端検出
570     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)   '本体位置決め出端検出
571     If MRtn = 1 Then GoTo *CompPositioning
572     M_Out(12263) = 1 Dly 0.5
573     fErrorProcess(11,231,282,0)
574     If M_20# = MNext% Then M_20# = MClear%
575     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
576     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
577     If M_20# = MContinue% Then GoTo *RE_POSITIONING
578     M_Out(12262) = 1 Dly 0.5
579     *CompPositioning
580     '
581 '    Wait M_In(11888) = 1        'ねじロボ2停止1受信
582     MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
583     *RE_ERR_REL_2
584     If M_20# = MContinue% Then M_20# = MRtn2
585     If MRtn = 0 Then
586         MRtn2 = 1       'MRtn2初期化
587         M_Out(12263) = 1 Dly 0.5    '製品位置決めを解除
588         Mov PInitialPosition  '"イニシャルに戻る動き"
589         MRtn2 = frInCheck(11274,1,MSETTIMEOUT05&)
590         If MRtn2 = 0 Then
591             MRtn2 = M_20#                   '位置決め戻端エラーならM_20#内部を一時避難
592             M_20# = MClear%                 'M_20#初期化
593             fErrorProcess(11,234,284,0)     '位置決め戻端エラー
594             If M_20# = MNext% Then M_20# = MRtn2        'M_20#に避難した値を代入
595                 '位置決めエラーが発生して工程を抜ける場合停止処理を行う
596             If M_20# = MNgProcess% Then M_20# = MAbout%
597             Break
598         EndIf
599         Break
600             EndIf
601     If M_20# = MContinue% Then GoTo *RE_ERR_REL_2
602     If MRtn = 0 Then GoTo *ASSY_ERROR_END
603     '
604 '
605 '    Mov PProductOnPltGet_1      '本体受け取り上空(処理位置変更2/11中村)
606     '
607     *RE_PLT_GET_1
608     '
609     M_Out(12256) = 0            '本体チャック閉OFF(処理位置変更2/11中村)
610     M_Out(12257) = 1            '本体チャック開ON
611     '
612 '    Wait M_In(11265) = 1        '本体チャック開検出
613     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
614     If MRtn = 1 Then GoTo *CompPltGet1
615     fErrorProcess(11,244,284,0)
616     If M_20# = MNext% Then M_20# = MClear%
617     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
618     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
619     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
620     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
621     *CompPltGet1
622     Mov PProductOnPltGet_1      '本体受け取り上空(処理位置変更2/11中村)
623     '
624     Ovrd 25
625 '    Fine 0.05 , P
626     Mvs PProductOnPltGet        '本体受け取り位置
627     Dly 0.1
628     M_Out(12257) = 0            '本体チャック開OFF
629     M_Out(12256) = 1            '本体チャック閉ON
630 '    Fine 0 , P
631     '
632     M_Out(12263) = 1 Dly 0.5                    '本体位置決め戻端ON
633 '    Wait M_In(11274) = 1     '本体位置決め戻端検出
634     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)    '本体位置決め戻端検出
635     If MRtn = 1 Then GoTo *CompPltGet2
636     M_Out(12256) = 0                            '本体チャック閉OFF
637     M_Out(12257) = 1                            '本体チャック開ON
638     Dly 2.0
639     Mvs PProductOnPltGet_1
640     Mov PProductOnPltGet_2
641     fErrorProcess(11,234,284,0)
642     If M_20# = MNext% Then M_20# = MClear%
643     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
644     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
645     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
646     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
647     Mov PProductOnPltGet_1
648     Mvs PProductOnPltGet
649     M_Out(12257) = 0                            '本体チャック開OFF
650     M_Out(12256) = 1                            '本体チャック閉ON
651     Dly 2.0
652     *CompPltGet2
653     '
654 '    Wait M_In(11264) = 1        '本体検出
655     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '本体検出
656     If MRtn = 1 Then GoTo *CompPltGet3
657     M_Out(12256) = 0            '本体チャック閉OFF
658     M_Out(12257) = 1            '本体チャック開ON
659     Dly 2.0
660     Mvs PProductOnPltGet_1
661     Mov PProductOnPltGet_2
662     fErrorProcess(11,252,284,0)
663     If M_20# = MNext% Then M_20# = MClear%
664     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
665     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
666     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
667     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
668     Mov PProductOnPltGet_1
669     Mvs PProductOnPltGet
670     M_Out(12257) = 0            '本体チャック開OFF
671     M_Out(12256) = 1            '本体チャック閉ON
672     Dly 2.0
673     *CompPltGet3
674     '
675 '    Wait M_In(11266) = 1        '本体チャック閉検出
676     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
677     If MRtn = 1 Then GoTo *CompPltGet4
678     M_Out(12256) = 0            '本体チャック閉OFF
679     M_Out(12257) = 1            '本体チャック開ON
680     Dly 2.0
681     Mvs PProductOnPltGet_1
682     Mov PProductOnPltGet_2
683     Dly 0.1
684     M_Out(12257) = 0            '本体チャック開OFF
685     M_Out(12256) = 1            '本体チャック閉ON
686     Dly 3.0
687     fErrorProcess(11,245,284,0)
688     If M_20# = MNext% Then M_20# = MClear%
689     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
690     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
691     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
692     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
693     M_Out(12256) = 0            '本体チャック閉OFF
694     M_Out(12257) = 1            '本体チャック開ON
695     Dly 2.0
696     Mov PProductOnPltGet_1
697     Mvs PProductOnPltGet
698     M_Out(12257) = 0            '本体チャック開OFF
699     M_Out(12256) = 1            '本体チャック閉ON
700     Dly 2.0
701     *CompPltGet4
702     '
703     Dly 0.5                     '念のためディレイ
704     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
705     Mvs PProductOnPltGet_1      '本体受け取り上空
706     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
707     Ovrd 100
708     Mov PProductOnPltGet_2      '本体受け取り上空回避点
709     '
710     '製品をねじロボ2に置く
711     Ovrd 50                     '搬送時ヒートシンク外れ対応100→50(5/12中村)
712     Mov PProductOnRoboSet_3     '経路
713     '
714     *RE_ROBO_SET_1
715     '
716     M_Out(12259) = 0            'DVDメカチャック開OfF
717     M_Out(12258) = 1            'DVDメカチャック閉ON
718 '    Wait M_In(11269) = 1        'DVDメカチャック閉検出
719     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
720     If MRtn = 1 Then GoTo *CompRoboSet1
721     fErrorProcess(11,269,284,0)
722     If M_20# = MNext% Then M_20# = MClear%
723     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
724     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
725     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_1
726     *CompRoboSet1
727 '
728     Mov PProductOnRoboSet_2     'ねじロボ製品置き上空回避点
729 '    Mvs PProductOnRoboSet_4     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)
730     Ovrd 10                     'オーバーライド変更(22/12/09中村)
731     Mvs PProductOnRoboSet_1     'ねじロボ製品置き上空
732     Dly 0.1                     '念のためDly(22/12/09中村)
733     Mvs PProductOnRoboSet       'ねじロボ製品置き位置
734     M_Out(12866) = 1 Dly 0.3    'ねじロボ2動作再開(停止1～停止2)
735 '    Wait M_In(11889) = 1        'ねじロボ2停止2受信
736     MScrewRoboNgFlg% = 0
737     MRtn = fScrewTighenRoboCheck(11889)    '停止状態を受信する
738     If MRtn = 0 Then
739         MScrewRoboNgFlg% = 1
740     EndIf
741 '
742     *RE_ROBO_SET_2
743 '
744     M_Out(12256) = 0            '本体チャック閉OFF
745     M_Out(12257) = 1            '本体チャック開ON
746 '    Wait M_In(11265)            '本体チャック開検出
747     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
748     If MRtn = 1 Then GoTo *CompRoboSet2
749     fErrorProcess(11,244,284,0)
750     If M_20# = MNext% Then M_20# = MClear%
751     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
752     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
753     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_2
754     *CompRoboSet2
755     '
756     Mvs PProductOnRoboSet_1     'ねじロボ製品置き上空
757     Mvs PProductOnRoboSet_2     'ねじロボ製品置き上空回避点
758 '    Mvs PProductOnRoboSet_4     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)
759     Ovrd 100
760     Mov PProductOnRoboSet_3     '経路
761     '
762     If MScrewRoboNgFlg% = 1 Then Mov PInitialPosition
763     If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END
764 '
765 '
766 '
767     '
768     '背面板を取る(コンプライアンスモード実装11/8中村)
769     Mov PPlateBackGet_2         '背面板受け取り上空回避点
770     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止2～停止3)
771 '    MRtn = fScrewTighenRoboCheck(11890)    '停止状態を受信する'12/20受信位置の変更(中村)
772 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
773     Mov PPlateBackGet_1         '背面板受け取り上空
774     '
775     *RE_PLATE_GET
776     '
777     Fine 0.05 , P
778     Ovrd 25
779     Mvs PPlateBackGet           '背面板受け取り位置
780     M_Out(12257) = 0            '本体チャック開OFF
781     M_Out(12256) = 1            '本体チャック閉ON
782     '
783 '    Wait M_In(11266) = 1        '本体チャック閉検出
784     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
785     If MRtn = 1 Then GoTo *CompPlateGet_1
786     M_Out(12256) = 0            '本体チャック閉OFF
787     M_Out(12257) = 1            '本体チャック開ON
788     Mvs PPlateBackGet_1
789     fErrorProcess(11,245,293,0) '284→293に変更6/7中村
790     If M_20# = MNext% Then M_20# = MClear%
791     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
792     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
793     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
794     Mvs PPlateBackGet           '背面板受け取り位置
795     M_Out(12257) = 0            '本体チャック開OFF
796     M_Out(12256) = 1            '本体チャック閉ON
797     *CompPlateGet_1
798     Fine 0 , P
799     '
800     Ovrd 10
801     Accel 10 , 10               '25,100→10,10(22/12/09中村)
802     Dly 0.7                     'ディレイ時間調節中(把持力確保)
803 '    CmpG 0.7,0.7,,,,,,       'X,Y軸ゲインを0.7に変更
804 '    ColChk Off                  '衝突検知OFF
805 '    Cmp Pos , &B11          'X,Y軸コンプライアンスモード開始
806     Cnt 1 , 10 , 10
807     Mvs PPlateBackGet_1         '背面板受け取り上空
808 '    Cmp Off                     'コンプライアンスモード終了
809 '    ColChk On                   '衝突検知ON
810     Ovrd 25                     'オーバーライド変更50→25(22/12/09中村)
811     Mov PPlateBackGet_2         '背面板受け取り上空回避点
812     Ovrd 100
813     Accel 100 , 100
814     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '背面パネルチェック
815     If MRtn = 1 Then GoTo *CompPlateGet_2
816     Cnt 0
817     fErrorProcess(11,267,293,0)                     '284→293に変更6/7中村
818     If M_20# = MNext% Then M_20# = MClear%
819     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
820     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
821     If M_20# = MContinue% Then
822         Mov PPlateBackGet_1
823         Dly 0.3
824         M_Out(12256) = 0            '本体チャック閉OFF
825         M_Out(12257) = 1            '本体チャック開ON
826         Dly 2.0
827     EndIf
828     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
829     *CompPlateGet_2    '
830     '背面板を置く
831 '    Wait M_In(11889) = 1        'ねじロボ2停止2受信
832     ColChk Off
833     Mov PPlateBackSet_12        '背面板置き上空
834     Cnt 0
835     '
836     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        'もう一度背面パネルチェック
837     If MRtn = 1 Then GoTo *CompPlateGet_3
838     fErrorProcess(11,267,293,0)                     '284→293に変更6/7中村
839     If M_20# = MNext% Then M_20# = MClear%
840     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
841     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
842     If M_20# = MContinue% Then
843         Mov PPlateBackGet_2
844         Mov PPlateBackGet_1
845         M_Out(12256) = 0            '本体チャック閉OFF
846         M_Out(12257) = 1            '本体チャック開ON
847         Dly 2.0
848     EndIf
849     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
850     *CompPlateGet_3
851     '
852     ' 部品供給要求送信
853     M_Out(12787) = 1
854 '
855     MRtn = fScrewTighenRoboCheck(11890)    '停止状態を受信する'12/20位置の変更(中村)
856     If MRtn = 0 Then Mov PInitialPosition    '"イニシャルに戻る動き"
857     If MRtn = 0 Then GoTo *ASSY_ERROR_END
858 '
859 '    Mov PPlateBackSet_11        '経路1
860     Ovrd 50
861 '    Mov PPlateBackSet_10        '経路2
862 '    Mov PPlateBackSet_9         '経路3
863 '    Mov PPlateBackSet_8         '経路4
864 '    Mov PPlateBackSet_7         '経路5
865     Mov PPlateBackSet_6         '経路6
866     Ovrd 25
867     Mvs PPlateBackSet_5         '経路7
868     Mvs PPlateBackSet_4         '経路8
869     Mov PPlateBackSet_3         '経路9
870     Mov PPlateBackSet_2         '経路10
871     Mov PPlateBackSet_1         '経路11
872     Mov PPlateBackSet           '背面板置き位置
873     *RE_PLATE_SET
874     M_Out(12256) = 0            '本体チャック閉OFF
875     M_Out(12257) = 1            '本体チャック開ON
876     '
877 '    Wait M_In(11265)            '本体チャック開検出
878     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
879     If MRtn = 1 Then GoTo *CompPlateSet
880     fErrorProcess(11,244,284,0)
881     If M_20# = MNext% Then M_20# = MClear%
882     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
883     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
884     If M_20# = MContinue% Then GoTo *RE_PLATE_SET
885     *CompPlateSet
886     '
887 '-----暫定押し-------------------------------------(22/12/09中村)ここから
888 *RE_BUCK_PUSH
889     Mvs PPlateBackPush_2
890 '
891     M_Out(12257) = 0            '本体チャック開OFF
892     M_Out(12256) = 1            '本体チャック閉ON
893 '
894     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
895 '
896     If MRtn = 1 Then GoTo *CompBuckPushSetting  '正常なら押し動作へ
897 '
898     fErrorProcess(11,245,287,0) '閉端センサーNG時エラー表示
899         If M_20# = MNext% Then M_20# = MClear%              '次へが押されたら値を初期化
900         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '停止が押されたらエラーエンドへ
901         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END    'NGが押されたらエラーエンドへ
902         If M_20# = MContinue% Then GoTo *RE_BUCK_PUSH       'リトライが押されたらもう一度閉じる
903 '
904 *CompBuckPushSetting
905 '
906     Mvs PPlateBackPush_1
907     Ovrd 10
908     Mvs PPlateBackPush
909     Dly 0.1
910     Ovrd 25
911     Mvs PPlateBackPush_1
912 *RE_CHUCK_OPEN
913     M_20# = MClear%
914     M_Out(12256) = 0            '本体チャック閉OFF
915     M_Out(12257) = 1            '本体チャック開ON
916     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
917     If MRtn = 1 Then GoTo *CompChuckOpenForBackPush
918     fErrorProcess(11,244,284,0)
919         If M_20# = MNext% Then M_20# = MClear%              '次へが押されたら値を初期化
920         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '停止が押されたらエラーエンドへ
921         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END    'NGが押されたらエラーエンドへ
922         If M_20# = MContinue% Then GoTo *RE_CHUCK_OPEN       'リトライが押されたらもう一度閉じる
923 *CompChuckOpenForBackPush
924 '-----暫定押し-------------------------------------(22/12/09中村)ここまで
925     ColChk On
926     Mov PPlateBackSet_12        '背面板置き上空
927     '
928     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～停止4)
929     Ovrd 100
930     '
931 ''    ' 部品供給要求送信(処理位置変更2/27中村)
932 '    M_Out(12787) = 1
933     'ねじロボ製品クランプ固定待ち
934 '    Wait M_In(11891) = 1        'ねじロボ2停止4受信
935     MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
936     If MRtn = 0 Then Mov PInitialPosition    '"イニシャルに戻る動き"
937     If MRtn = 0 Then GoTo *ASSY_ERROR_END
938     '
939     '置き位置画像検査
940 '    Mov PPlateBackCheck_3       '画像検査位置上空
941 '    Mov PPlateBackCheck_2       '通過点
942     If M_In(11369) = 0 Then GoTo *CompCheck
943     Mvs PPlateBackCheck         '確認位置
944     '
945 *RE_CHECK
946     PInspPosition(1) = PPlateBackCheck
947     MInspGroup%(1) = 2
948     MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,1,-1,1)
949     If M_In(11374) = 0 Then MRtn = 1
950     If MRtn = 1 Then GoTo *CompCheck
951     fErrorProcess(11,43,23,0)
952     If M_20# = MNext% Then M_20# = MClear%
953     If M_20# = MAbout% Or M_20# = MNgProcess% Then
954         Mov PPlateBackSet_12
955         Mov PInitialPosition
956     EndIf
957     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
958     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
959     If M_20# = MContinue% Then GoTo *RE_CHECK
960 *CompCheck
961 '
962     Mov PPlateBackSet_12        '背面板置き上空
963 ''    ' 部品供給要求送信
964 '    M_Out(12787) = 1
965 '    Mov PPlateBackCheck_2       '通過点
966 '    Mov PPlateBackCheck_3       '画像検査位置上空
967     '
968     'ねじロボ引き込み待ち
969     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～完了)
970 '
971     M_Out(12258) = 0            'DVDメカチャック閉OFF
972     M_Out(12259) = 1            'DVDメカチャック開ON
973     '
974     'DVDメカを取る
975     Mvs PMechaGet_3             '経路1
976     Mvs PMechaGet_2             'DVDメカ受け取り上空回避点
977 '    Wait M_In(11272)            '部品供給機Ready
978 '    MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '部品供給機Ready
979 '    If MRtn = 0 Then
980 '        fErrorProcess()         'エラー処理
981 '    EndIf
982 '
983   ' 部品供給要求送信
984     fnAutoScreenComment(513)    '状態表示[部品供給待ち] 2022/04/26 渡辺
985 '    M_Out(12787) = 1
986     '    ' 部品供給完了待ち(処理変更2/27中村)
987 *RE_FEEDER_READY
988 '    Wait M_In(11810) = 1
989 MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '供給待ち
990 If MRtn = 1 Then GoTo *CompFeederReady
991 '   ' 部品供給要求終了
992 M_Out(12787) = 0
993 fErrorProcess(11,289,290,0)                '284→290に変更6/7中村
994 If M_20# = MNext% Then M_20# = MClear%
995 If M_20# = MAbout% Or M_20# = MNgProcess% Then
996     Mov PMechaGet_2
997     Mov PMechaGet_3
998     Mov PMechaGet_4
999     Mov PInitialPosition
1000 EndIf
1001 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1002 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1003     ' 部品供給要求
1004 M_Out(12787) = 1
1005 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
1006 *CompFeederReady
1007 '    ' 部品供給要求終了
1008     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1009     M_Out(12787) = 0
1010 '
1011     Mvs PMechaGet_1             'DVDメカ受け取り上空
1012     '
1013     *RE_MECHA_GET_1
1014     '
1015     M_Out(12258) = 0            'DVDメカチャック閉OFF
1016     M_Out(12259) = 1            'DVDメカチャック開ON
1017     '
1018 '    Wait M_In(11268) = 1        'DVDメカチャック開検出
1019     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1020     If MRtn = 1 Then GoTo *CompMechaGet1
1021     Mvs PMechaGet_2
1022     Mvs PMechaGet_3
1023     Mov PMechaGet_4
1024     fErrorProcess(11,270,284,0)
1025     If M_20# = MNext% Then M_20# = MClear%
1026     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1027     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1028     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1029     Mov PMechaGet_3
1030     Mvs PMechaGet_2
1031     Mvs PMechaGet_1
1032     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_1
1033     *CompMechaGet1
1034     '
1035     M_Out(12261) = 0            'DVDメカシリンダー戻OFF
1036     M_Out(12260) = 1            'DVDメカシリンダー出ON
1037 '    Wait M_In(11271) = 1        'DVDメカシリンダー出検出
1038     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVDメカシリンダー出検出
1039     If MRtn = 1 Then GoTo *CompMechaGet2
1040     Mvs PMechaGet_2
1041     Mvs PMechaGet_3
1042     Mov PMechaGet_4
1043     fErrorProcess(11,271,284,0)
1044     If M_20# = MNext% Then M_20# = MClear%
1045     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1046     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1047     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1048     Mov PMechaGet_3
1049     Mvs PMechaGet_2
1050     Mvs PMechaGet_1
1051     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_1
1052     *CompMechaGet2
1053     '
1054     Ovrd 25
1055     Mvs PMechaGet               'DVDメカ受け取り位置
1056     Dly 0.1                     '位置出し　(22/12/09中村)
1057 '
1058     MRtn = 0
1059     MRtn2 = 0
1060     *RE_MECHA_GET_2
1061     If M_20# = MContinue% Then M_20# = MClear%
1062     M_Out(12259) = 0            'DVDメカチャック開OfF
1063     M_Out(12258) = 1            'DVDメカチャック閉ON
1064     '
1065 '    Wait M_In(11269) = 1        'DVDメカチャック閉検出
1066     If MRtn = 1 Then Dly 1.0
1067     If MRtn = 1 Then GoTo *CompMechaGet3
1068     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
1069     If M_20# = MNext% Then GoTo *CompMechaGet3
1070     If MRtn = 1 Then GoTo *CompMechaGet3
1071     M_Out(12258) = 0            'DVDメカチャック閉OfF
1072     M_Out(12259) = 1            'DVDメカチャック開ON
1073     Dly 2.0
1074     Mvs PMechaGet_1
1075     Mvs PMechaGet_2
1076     M_Out(12259) = 0            'DVDメカチャック開OfF
1077     M_Out(12258) = 1            'DVDメカチャック閉ON
1078     Mvs PMechaGet_3
1079     Mov PMechaGet_4
1080     fErrorProcess(11,269,284,0)
1081     If M_20# = MNext% Then
1082         M_20# = MClear%
1083         MRtn = 1
1084     EndIf
1085     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1086     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1087     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1088     M_Out(12258) = 0            'DVDメカチャック閉OfF
1089     M_Out(12259) = 1            'DVDメカチャック開ON
1090     Mov PMechaGet_3
1091     Mvs PMechaGet_2
1092     Mvs PMechaGet_1
1093     Mvs PMechaGet
1094     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1095     *CompMechaGet3
1096     M_20# = MClear%
1097     '
1098 '    Wait M_In(11267) = 1        'DVDメカ検出
1099     If MRtn2 = 1 Then GoTo *CompMechaGet4
1100     MRtn2 = frInCheck(11267,1,MSETTIMEOUT05&)   'DVDメカ検出
1101     If MRtn2 = 1 Then GoTo *CompMechaGet4
1102     M_Out(12258) = 0            'DVDメカチャック閉OfF
1103     M_Out(12259) = 1            'DVDメカチャック開ON
1104     Dly 2.0
1105     Mvs PMechaGet_1
1106     Mvs PMechaGet_2
1107     M_Out(12259) = 0            'DVDメカチャック開OfF
1108     M_Out(12258) = 1            'DVDメカチャック閉ON
1109     Mvs PMechaGet_3
1110     Mov PMechaGet_4
1111     fErrorProcess(11,273,284,0)
1112     If M_20# = MNext% Then
1113         M_20# = MClear%
1114         MRtn2 = 1
1115     EndIf
1116     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1117     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1118     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1119     M_Out(12258) = 0            'DVDメカチャック閉OfF
1120     M_Out(12259) = 1            'DVDメカチャック開ON
1121     Mov PMechaGet_3
1122     Mvs PMechaGet_2
1123     Mvs PMechaGet_1
1124     Mvs PMechaGet
1125     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1126     *CompMechaGet4
1127     M_20# = MClear%
1128     Dly 0.5
1129     '
1130     Mvs PMechaGet_1             'DVDメカ受け取り上空
1131 '    *RE_MECHA_GET_3
1132     M_Out(12260) = 0            'DVDメカシリンダー出OFF
1133     M_Out(12261) = 1            'DVDメカシリンダー戻ON
1134 '    Wait M_In(11270) = 1        'DVDメカシリンダー戻検出
1135     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVDメカシリンダー戻検出
1136 '    If MRtn = 1 Then GoTo *CompMechaGet5       '処理位置変更2/11中村
1137 '    fErrorProcess(11,272,284,0)
1138 '    If M_20# = MNext% Then M_20# = MClear%
1139 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1140 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1141 '    If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1142 '    *CompMechaGet5
1143     '
1144     If MRtn = 1 Then Ovrd 50    '念のためオーバーライド変更100→50(22/12/09中村)
1145     Mvs PMechaGet_2             'DVDメカ受け取り上空回避点
1146 '    ' 部品供給要求終了
1147     M_Out(12787) = 0
1148 '    ' 部品取得完了送信(パルス)
1149     M_Out(12800) = 1 Dly 0.5
1150     Mvs PMechaGet_3             '経路1
1151     Mov PMechaGet_4             '経路2
1152 '
1153     *RE_MECHA_GET_3
1154     M_Out(12260) = 0            'DVDメカシリンダー出OFF
1155     M_Out(12261) = 1            'DVDメカシリンダー戻ON
1156     If MRtn = 1 Then GoTo *CompMechaGet5
1157     fErrorProcess(11,272,284,0)
1158     If M_20# = MNext% Then M_20# = MClear%
1159     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1160     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1161     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1162     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1163     *CompMechaGet5
1164     '
1165     'DVDメカを仮置き台へ置く
1166 '    Wait M_In(11920) = 0             'BaseUnit6側仮置き台フラグ確認(処理位置変更2/11中村)
1167 '
1168 '   仮置き台が回転中か確認
1169     *Loop_CW_CCW_1
1170     If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_1 Else GoTo *Loop_CW_CCW_1
1171     *Next_CW_CCW_1
1172     If M_In(11920) = 0 Then GoTo *OK_FLG_1 Else GoTo *Loop_CW_CCW_1           'BaseUnit6側仮置き台フラグ確認
1173     *OK_FLG_1
1174 '
1175     M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1176     '
1177     MRtn = 1
1178     'DVDメカが仮置き台に置かれていないかの確認(追加ここから10/1中村)
1179     If M_In(11931) = 1 Then          '回転方向の確認(CW方向)
1180         If M_In(11928) = 0 Then      'BaseUnit5側にDVDメカが置かれていた場合
1181             M_Out(12912) = 0         '仮置き台フラグ回収(回転可能状態にするため)
1182             Wait M_In(11930) = 1     '仮置き台回転待ち
1183             'If M_In(11929) = 0 Then  '回転後にまだBaseUnit5側に置かれている場合
1184                 'エラー処理(BaseUnit6側にて正常な動作がされていない)
1185             'Endif
1186         EndIf
1187     ElseIf M_In(11930) = 1 Then      '回転方向の確認(CCW方向)
1188         If M_In(11929) = 0 Then      'BaseUnit5側にDVDメカが置かれていた場合
1189             M_Out(12912) = 0         '仮置き台フラグ回収(回転可能状態にするため)
1190             Wait M_In(11931) = 1     '仮置き台回転待ち
1191             MRtn = 0
1192         EndIf
1193     Else
1194         MRtn = 0'エラー処理(仮置き台が正常な動作をしていない)
1195     EndIf
1196     If MRtn = 0 Then GoTo *Loop_CW_CCW_1
1197 '
1198 *Loop_CW_CCW_S
1199     fnAutoScreenComment(530)    '状態表示[工程６の動作終了待ち] 2022/04/26 渡辺
1200 'Ver 0.4 追加 -----------------------
1201 '自工程が12912 = 1 を出力した後、再度 工程6側の動作中監視を行う
1202     MRtn = 0
1203     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec 工程6のフラグON監視
1204     If MRtn = 1 Then M_Out(12912) = 0                  '仮置き台フラグ回収 工程6優先のため12912=0を出力
1205     If MRtn = 1 Then Dly 0.7
1206     If MRtn = 1 Then GoTo *Loop_CW_CCW_S        '工程6の動作中フラグがONしていたら再度ループ
1207     M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1208 'Ver 0.4 ここまで -------------------
1209 '
1210     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1211     Mov PMechaSet_3             'DVDメカ仮置き回避点1(処理位置変更2/27中村)
1212 '
1213 *Loop_CW_CCW_2
1214 'Ver 0.4 追加 -----------------------
1215     '自工程が12912 = 1 を出力した後、再度 工程6側の動作中監視を行う
1216     MRtn = 0
1217     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec 工程6のフラグON監視
1218     If MRtn = 1 Then M_Out(12912) = 0                  '仮置き台フラグ回収 工程6優先のため12912=0を出力
1219     If MRtn = 1 Then Dly 0.7
1220     If MRtn = 1 Then GoTo *Loop_CW_CCW_2        '工程6の動作中フラグがONしていたら再度ループ
1221     M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1222 'Ver 0.4 ここまで -------------------
1223 '
1224     Mov PMechaSet_2             'DVDメカ仮置き回避点2(処理位置変更2/27中村)
1225 '
1226 '    *Loop_CW_CCW_2  '仮置き台の状態をもう一度確認する(コメントアウト2/27中村)
1227 '    If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_2 Else GoTo *Loop_CW_CCW_2
1228 '    *Next_CW_CCW_2
1229 '    If M_In(11920) = 0 Then GoTo *OK_FLG_2 Else GoTo *Loop_CW_CCW_2           'BaseUnit6側仮置き台フラグ確認
1230 '    *OK_FLG_2
1231 '
1232 '    M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1233 '
1234     '
1235     *RE_MECHA_SET_1
1236     If M_20# = MContinue% Then M_20# = MClear%
1237     Ovrd 25
1238     M_Out(12261) = 0            'DVDメカシリンダー戻OFF
1239     M_Out(12260) = 1            'DVDメカシリンダー出ON
1240 '    Wait M_In(11271) = 1        'DVDメカシリンダー出検出
1241     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVDメカシリンダー出検出
1242     If MRtn = 1 Then GoTo *CompMechaSet1
1243     Mov PMechaSet_3
1244     Mov PMechaGet_4
1245     fErrorProcess(11,271,284,0)
1246     If M_20# = MNext% Then M_20# = MClear%
1247     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1248     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1249     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1250     Mov PMechaSet_3
1251     Mov PMechaSet_2
1252     Ovrd 100
1253     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_1
1254     *CompMechaSet1
1255     '
1256     *RE_MECHA_SET_12
1257     Fine 0.05 , P
1258 '    Wait M_In(11920) = 0        'BaseUnit6側仮置き台フラグ確認
1259 '    M_Out(12912) = 1            '仮置き台フラグ立て
1260     If M_In(11931) = 1 Then     '回転方向の確認(CW方向)(追加ここまで10/1中村)
1261         Mov PMechaSet1_1        'DVDメカ仮置き上空1
1262         Ovrd 10
1263         Mvs PMechaSet1          'DVDメカ仮置き位置1
1264         Dly 0.1
1265         M_Out(12258) = 0        'DVDメカチャック閉OFF
1266         M_Out(12259) = 1        'DVDメカチャック開ON
1267 '        Wait M_In(11268) = 1    'DVDメカチャック開検出
1268         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1269         Mvs PMechaSet1_1        'DVDメカ仮置き上空1
1270     ElseIf M_In(11930) = 1 Then '回転方向の確認(CCW方向)(追加ここから10/1中村)
1271         Mov PMechaSet2_1        'DVDメカ仮置き上空
1272         Ovrd 10
1273         Mvs PMechaSet2          'DVDメカ仮置き位置2
1274         Dly 0.1
1275         M_Out(12258) = 0        'DVDメカチャック閉OFF
1276         M_Out(12259) = 1        'DVDメカチャック開ON
1277 '        Wait M_In(11268) = 1    'DVDメカチャック開検出
1278         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1279         Mvs PMechaSet2_1        'DVDメカ仮置き上空2
1280     'Else
1281         'エラー文(仮置き台が正常な位置に無い)
1282     EndIf                       '追加ここまで10/1中村
1283     Fine 0 , P
1284     '
1285     If MRtn = 1 Then GoTo *CompMechaSet2
1286     fErrorProcess(11,270,284,0)
1287     If M_20# = MNext% Then M_20# = MClear%
1288     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1289     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1290     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_12
1291     *CompMechaSet2
1292     '
1293     Ovrd 100
1294     *RE_MECHA_SET_2
1295     M_Out(12260) = 0            'DVDメカシリンダー出OFF
1296     M_Out(12261) = 1            'DVDメカシリンダー戻ON
1297 '    Wait M_In(11270) = 1        'DVDメカシリンダー戻検出
1298     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVDメカシリンダー戻検出
1299     If MRtn = 1 Then GoTo *CompMechaSet3
1300     fErrorProcess(11,272,284,0)
1301     If M_20# = MNext% Then M_20# = MClear%
1302     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1303     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1304     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_2
1305     *CompMechaSet3
1306     '
1307     Mov PMechaSet_2             'DVDメカ仮置き回避点2
1308     M_Out(12912) = 0                  '仮置き台フラグ回収(追加10/1中村)
1309     '
1310     'ねじロボ2の製品を取る
1311     Mov PProductOnRoboGet_4     '経路3から4へ
1312     M_Out(12259) = 0            'DVDメカチャック開OFF
1313     M_Out(12258) = 1            'DVDメカチャック閉ON
1314 '    Wait M_In(11876) = 1        'ねじロボ2完了待機を受信
1315 MRtn = fScrewTighenRoboCheck(11876)    '停止状態を受信する
1316 If MRtn = 0 Then Mov PInitialPosition   '"イニシャルに戻る動き"
1317 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1318 '
1319     *RE_ROBO_GET_1
1320 '
1321     M_Out(12259) = 0            'DVDメカチャック開OFF
1322     M_Out(12258) = 1            'DVDメカチャック閉ON
1323     If M_20# = MContinue% Then Dly 0.5
1324 '
1325 '    Wait M_In(11269) = 1        'DVDメカチャック閉検出
1326     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
1327     If MRtn = 1 Then GoTo *CompRoboGet1
1328     fErrorProcess(11,269,284,0)
1329     If M_20# = MNext% Then M_20# = MClear%
1330     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1331     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1332     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1333     If M_20# = MContinue% Then GoTo *RE_ROBO_GET_1
1334     *CompRoboGet1
1335     '
1336     Ovrd 50
1337     Mov PProductOnRoboGet_3     'ねじロボ製品取り上空回避点2から3へ
1338     Ovrd 20
1339     Mvs PProductOnRoboGet_2     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)4から2へ
1340     Mvs PProductOnRoboGet_1     'ねじロボ製品取り上空
1341     Ovrd 10
1342     Mvs PProductOnRoboGet       'ねじロボ製品取り位置
1343     Dly 0.3
1344 '
1345     *RE_ROBO_GET_2
1346 '
1347     M_Out(12257) = 0            '本体チャック開OFF
1348     M_Out(12256) = 1            '本体チャック閉ON
1349 '
1350 '    Wait M_In(11266) = 1        '本体チャック閉検出
1351     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
1352     If MRtn = 1 Or M_20# = MNext% Then GoTo *CompRoboGet2
1353     M_Out(12256) = 0            '本体チャック閉OFF
1354     M_Out(12257) = 1            '本体チャック開ON
1355     Dly 2.0
1356     Mvs PProductOnRoboGet_1
1357     Mvs PProductOnRoboGet_2
1358     Mov PProductOnRoboGet_3
1359     Mov PProductOnRoboGet_4
1360     Mov PInitialPosition
1361     M_Out(12257) = 0            '本体チャック開OFF
1362     M_Out(12256) = 1            '本体チャック閉ON
1363     Dly 1.0
1364     fErrorProcess(11,245,284,0)
1365     If M_20# = MNext% Then MRtn = 1
1366     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1367     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1368     M_Out(12256) = 0            '本体チャック閉OFF
1369     M_Out(12257) = 1            '本体チャック開ON
1370     Dly 2.0
1371     Mov PProductOnRoboGet_4
1372     Mov PProductOnRoboGet_3
1373     Mov PProductOnRoboGet_2
1374     Mvs PProductOnRoboGet_1
1375     Mvs PProductOnRoboGet
1376     If M_20# = MContinue% Or M_20# = MNext% Then GoTo *RE_ROBO_GET_2
1377     *CompRoboGet2
1378     M_20# = MClear%
1379     '
1380     Accel 30 , 100
1381     Dly 0.3
1382     Mvs PProductOnRoboGet_1     'ねじロボ製品取り上空
1383     Ovrd 25
1384     Mvs PProductOnRoboGet_2     'ねじロボ製品取り上空回避点(9/27暫定コメントアウト)12/15コメント解除
1385     Mvs PProductOnRoboGet_3     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)4から3へ
1386     Ovrd 50                     'オーバーライド変更(22/12/09中村)
1387     Mov PProductOnRoboGet_4     '経路3から4へ
1388     Accel 50 , 50               '加速度変更(22/12/09中村)
1389 '
1390     M_Out(12868) = 1 Dly 0.5    'ねじロボ2動作完了を送信
1391 '    *RE_ROBO_GET_3                                     '処理位置変更2/11中村
1392 '    M_Out(12258) = 0            'DVDメカチャック閉OFF
1393 '    M_Out(12259) = 1            'DVDメカチャック開ON
1394 '    Wait M_In(11268) = 1        'DVDメカチャック開検出
1395 '    MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1396 '    If MRtn = 1 Then GoTo *CompRoboGet3
1397 '    fErrorProcess(11,270,284,0)
1398 '    If M_20# = MNext% Then M_20# = MClear%
1399 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1400 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1401 '    If M_20# = MContinue% Then GoTo *RE_ROBO_GET_3
1402 '    *CompRoboGet3
1403     '
1404     'パレットへ製品を置く
1405     Mov PProductOnPltSet_2      '本体置き位置上空回避点
1406     Accel 100 , 100             '加速度変更(22/12/09中村)
1407     Mov PProductOnPltSet_1      '本体置き位置上空
1408     Ovrd 10
1409     Mvs PProductOnPltSet        '本体置き位置
1410     Dly 0.3
1411 '
1412     *RE_PLT_SET
1413 '
1414     M_Out(12256) = 0            '本体チャック閉OFF
1415     M_Out(12257) = 1            '本体チャック開ON
1416 '
1417 '    Wait M_In(11265) = 1        '本体チャック開検出
1418     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
1419     If MRtn = 1 Then GoTo *CompPltSet
1420     fErrorProcess(11,244,284,0)
1421     If M_20# = MNext% Then M_20# = MClear%
1422     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1423         Mvs PProductOnPltSet_1
1424         Mov PProductOnPltSet_2
1425         Mov PInitialPosition
1426     EndIf
1427     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1428     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1429     If M_20# = MContinue% Then GoTo *RE_PLT_SET
1430     *CompPltSet
1431 '
1432     Mvs PProductOnPltSet_1      '本体置き位置上空
1433     Ovrd 100
1434     Mov PProductOnPltSet_2      '本体置き位置上空回避点
1435 '    Mov PInitialPosition        'イニシャルポジション
1436     MRtn = FnCtlValue2(2)       '組立ＯＫ＋１  2022/04/28 渡辺
1437     Mov PTicketRead_1           'チケットID読み取り回避点
1438     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
1439     *RE_FIN_INI
1440     M_Out(12258) = 0            'DVDメカチャック閉OFF
1441     M_Out(12259) = 1            'DVDメカチャック開ON
1442 '    Wait M_In(11268) = 1        'DVDメカチャック開検出
1443     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1444     If MRtn = 1 Then GoTo *CompFinIni
1445     fErrorProcess(11,270,284,0)         'リトライとNG時にリトライをかける
1446     If M_20# = MNext% Then M_20# = MClear%
1447     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1448     If M_20# = MNgProcess% Then GoTo *RE_FIN_INI
1449     If M_20# = MContinue% Then GoTo *RE_FIN_INI
1450     *CompFinIni
1451     '
1452     'チケットID書き込み
1453     M_20# = MAssyOK%
1454     *ASSY_ERROR_END
1455     *AssyEnd
1456     *fnAssyStart_FEndPosi
1457 FEnd
1458 '
1459 '■fnPiasCheck
1460 ''' <summary>
1461 ''' PIASチケット読込み
1462 ''' </summary>
1463 ''' <returns>   0 : NG
1464 '''             1 : OK(読込み完了)
1465 ''' </returns>
1466 ''' <remarks>
1467 ''' Date   : 2021/07/07 : M.Hayakawa
1468 ''' </remarks>'
1469 Function M% fnPiasCheck
1470     fnPiasCheck = 0
1471     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1472     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1473 '
1474 *RETRY_PIAS
1475     M_20# = MClear%
1476     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1477     '
1478     '【IDチケット読み込み】
1479     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1480     MInspGroup%(1) = 1              '検査G番号
1481     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1482 '
1483     'エラーの場合
1484     If MRtn <> 1 Then
1485         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1486         If MRtn <> 1 Then
1487             'D720 -> D1300 コピー要求
1488             M_Out(12565) = 1
1489             Dly 0.5
1490             M_Out(12565) = 0
1491             'エラー処理記述
1492             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1493             'GOT KEY入力待ち
1494             MKeyNumber = fnKEY_WAIT()
1495             '
1496             Select MKeyNumber
1497                 Case MNext%         '次へを選択した場合
1498                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1499                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1500                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1501                     Break
1502                 Case MAbout%        '停止を選択した場合
1503                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1504                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1505                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1506                     Break
1507                 Case MNgProcess%    'NGを選択した場合
1508                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1509                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1510                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1511                     Break
1512                 Case MContinue%     '継続を選択した場合
1513                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1514                     M_20# = MContinue%
1515                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1516                     Break
1517             End Select
1518         EndIf
1519     EndIf
1520 '----------D720 -> D1300 コピー要求----------
1521     M_Out(12565) = 1
1522     Dly 0.5
1523     M_Out(12565) = 0
1524 '----------通信確認をする----------
1525     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1526     MRtn = 0                ' 初期化
1527     M_20# = MClear%         ' 初期化
1528     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1529     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1530     If MRtn <> 1 Then
1531         If M_20# = MContinue% Then
1532             GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1533         Else
1534             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1535         EndIf
1536     EndIf
1537 '----------工程抜け確認----------
1538     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1539     MRtn = 0                ' 初期化
1540     M_20# = MClear%         ' 初期化
1541     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1542     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1543     If MRtn <> 1 Then
1544         If M_20# = MContinue% Then
1545             GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1546         Else
1547             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1548         EndIf
1549     EndIf
1550     '
1551     fnPiasCheck = 1
1552     *fnPiasCheck_End
1553 FEnd
1554 '
1555 '■fnPCComuCheck
1556 ''' <summary>
1557 ''' PC-PLC通信チェック
1558 ''' </summary>
1559 ''' <returns>   0 : NG
1560 '''             1 : OK(読込み完了)
1561 ''' </returns>
1562 ''' <remarks>
1563 ''' Date   : 2021/07/07 : M.Hayakawa
1564 ''' </remarks>'
1565 Function M% fnPCComuCheck
1566     fnPCComuCheck = 0
1567     MJudge% = 0                                  '初期化
1568     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1569     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1570     '
1571     For MStaNo = 0 To 5
1572         '
1573         If M_In(MIN_PIAS_ComOK%) = 1 Then
1574             'PC通信OK(M400)
1575             MJudge% = MOK%
1576             MStaNo = 5
1577             Break
1578         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1579             'toRBT_通信確認time out
1580             MJudge% = MNG%
1581             MCommentD1001 = 15
1582             MCommentD1002 = 21
1583             MStaNo = 5
1584             Break
1585         Else
1586             'toRBT_通信確認time out
1587             MJudge% = MNG%
1588             MCommentD1001 = 14
1589             MCommentD1002 = 21
1590             Break
1591         EndIf
1592     Next MStaNo
1593     '
1594     '上記で返信フラグを受信してからPC通信確認OFF
1595     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1596     '
1597     'エラー画面
1598     If MJudge% <> MOK% Then
1599         M_20# = MClear%     '初期化
1600         'エラー処理記述
1601         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1602         'GOT KEY入力待ち
1603         MKeyNumber = fnKEY_WAIT()
1604         '
1605         If MKeyNumber = MAbout% Then            '停止を選択した場合
1606             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1607             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1608             Break
1609         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1610             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1611             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1612             Break
1613         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1614             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1615             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1616             Break
1617         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1618             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1619             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1620             Break
1621         EndIf
1622     Else
1623         'OKの場合
1624         fnPCComuCheck = 1
1625     EndIf
1626 FEnd
1627 '
1628 '■fnProcessCheck
1629 ''' <summary>
1630 ''' 工程抜け確認
1631 ''' </summary>
1632 ''' <returns>    1：工程履歴OK     0：異常終了
1633 '''             -1：前工程履歴NG  -2：自工程履歴あり
1634 '''             -3：モデル仕向NG  -4：タイムアウト
1635 '''             -5：履歴処理エラー
1636 ''' </returns>
1637 ''' <remarks>
1638 ''' Date   : 2021/07/07 : M.Hayakawa
1639 ''' </remarks>'
1640 Function M% fnProcessCheck
1641     fnProcessCheck = 0
1642     MJudge% = MNG%      '一旦NGを初期化とする
1643 '----------工程抜け確認----------
1644     MCommentD1001 = 0   'コメント初期化
1645     For MStaNo = 0 To 5
1646         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1647         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1648         '
1649         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1650             MJudge% = MOK%
1651             fnAutoScreenComment(85)     ' AUTO画面
1652             MStaNo = 5
1653             Break
1654         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1655             MFlgLoop% = 0
1656             MJudge% = MNG%
1657             MCommentD1001 = 27
1658             MCommentD1002 = 22
1659             fnAutoScreenComment(94)     ' AUTO画面
1660             fnProcessCheck = -2         ' NGは-2を返す
1661             MStaNo = 5
1662             Break
1663         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1664            MJudge% = MNG%
1665             MCommentD1001 = 31
1666             MCommentD1002 = 22
1667             fnAutoScreenComment(83)     ' AUTO画面
1668             fnProcessCheck = -3         ' NGは-3を返す
1669             MStaNo = 5
1670             Break
1671         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1672             '履歴NGは直ぐに終了せず繰り返し確認を行う
1673             '前工程の書込みが終了していない可能性があるため
1674             MJudge% = MNG%
1675             MCommentD1001 = 32
1676             MCommentD1002 = 22
1677             fnAutoScreenComment(84)     ' AUTO画面
1678             fnProcessCheck = -1         ' NGは-1を返す
1679             Dly 1.0
1680             '工程抜け確認OFF
1681             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1682             Dly 1.0
1683            'MStaNo = 5
1684             Break
1685         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1686             MFlgLoop% = 0
1687             MJudge% = MNG%
1688             MCommentD1001 = 29
1689             MCommentD1002 = 22
1690             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1691             fnProcessCheck = -5         ' NGは-5を返す
1692             MStaNo = 5
1693             Break
1694         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1695             MJudge% = MNG%
1696             If MCommentD1001 = 32 Then
1697                 '何もしない
1698             Else
1699                 MCommentD1001 = 26
1700             EndIf
1701             MCommentD1002 = 22
1702             fnProcessCheck = -4         ' NGは-4を返す
1703             MStaNo = 5
1704             Break
1705         Else
1706             MJudge% = MNG%
1707             MCommentD1001 = 28
1708             MCommentD1002 = 22
1709         EndIf
1710     Next MStaNo
1711     '工程抜け確認OFF
1712     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1713     '通過履歴NG 工程抜けの場合
1714     If MJudge% = MPass% Then
1715         M_20# = MPass%
1716     EndIf
1717     '
1718     'エラー画面
1719     If MJudge% <> MOK% Then
1720         M_20# = MClear%     '初期化
1721         'エラー処理記述
1722         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1723         'GOT KEY入力待ち
1724         MKeyNumber = fnKEY_WAIT()
1725         '
1726         Select MKeyNumber
1727             Case MAbout%        '停止を選択した場合
1728                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1729                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1730                 Break
1731             Case MNext%         '次へを選択した場合
1732                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1733                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1734                 Break
1735             Case MContinue%     '継続を選択した場合
1736                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1737                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1738                 Break
1739             Case MNgProcess%    'NGを選択した場合
1740                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1741                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1742                 Break
1743         End Select
1744     Else
1745         fnProcessCheck = 1  ' OKは1を返す
1746     EndIf
1747 FEnd
1748 '
1749 '■fnPiasWrite
1750 ''' <summary>
1751 ''' Pias 組立結果書込み要求
1752 ''' </summary>
1753 '''<param name="MFlg%">
1754 '''                 MOK%(1) = 工程履歴にOKを書込む
1755 '''                 MNG%(0) = 工程履歴にNGを書込む
1756 '''</param>
1757 '''<returns></returns>
1758 ''' <remarks>
1759 ''' Date   : 2021/07/07 : M.Hayakawa
1760 ''' </remarks>'
1761 Function M% fnPiasWrite(ByVal MFlg%)
1762       fnPiasWrite = 0
1763 *RETRY_PIASWRITE
1764     '
1765     '組立OK(MOK%)の場合　M306 ON
1766    '組立NG(MNG%)の場合　M307 ON
1767     If MFlg% = MOK% Then
1768         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1769     Else
1770         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1771     EndIf
1772     Dly 0.1                  '念のため
1773     '
1774     'Piasへ書込み開始 M305 -> ON
1775     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1776     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1777     '
1778     MJudge% = MNG%
1779     '
1780     For MStaNo = 0 To 5
1781         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1782             MJudge% = MOK%
1783             'MRet = fnAutoScreenComment(85)  'AUTO画面
1784             MStaNo = 5
1785             Break
1786         '
1787         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1788             MJudge% = MNG%
1789             'MRet = fnAutoScreenComment(85)  'AUTO画面
1790            MCommentD1001 = 34
1791            MCommentD1002 = 25
1792             MStaNo = 5
1793             Break
1794         '
1795         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1796             MJudge% = MNG%
1797             'MRet = fnAutoScreenComment(85)  'AUTO画面
1798            MCommentD1001 = 35
1799            MCommentD1002 = 25
1800             MStaNo = 5
1801             Break
1802         '
1803         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1804             MJudge% = MNG%
1805             'MRet = fnAutoScreenComment(85)  'AUTO画面
1806            MCommentD1001 = 36
1807            MCommentD1002 = 25
1808             MStaNo = 5
1809             Break
1810         '
1811         Else
1812             MJudge% = MNG%
1813            MCommentD1001 = 42
1814            MCommentD1002 = 25
1815         '
1816         EndIf
1817         '
1818     Next MStaNo
1819     '
1820     'Piasへ書込み開始 M305 -> OfF
1821     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1822     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1823     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1824     '
1825     '
1826     '通過履歴NG 工程抜けの場合
1827     If MJudge% = MPass% Then
1828         M_20# = MPass%
1829     EndIf
1830     '
1831    M_20# = MClear%     '初期化
1832     '
1833     'エラー画面
1834     If MJudge% < MOK% Then
1835     '
1836 '残しておくが現状では使用しないラベル
1837 *RETRY_ERR_WRITE
1838         M_20# = MClear%     '初期化
1839         'エラー処理記述
1840         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1841         'GOT KEY入力待ち
1842         MKeyNumber = fnKEY_WAIT()
1843         '
1844         If MKeyNumber = MAbout% Then   '停止を選択した場合
1845             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1846            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1847             Break
1848         '
1849         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1850             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1851             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1852         '
1853         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1854             M_20# = MPass%            'M_20# プログラム間共通外部変数
1855             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1856         '
1857         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1858             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1859            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1860             Break
1861         '
1862         EndIf
1863         '
1864         If M_20# = MClear% Then *RETRY_ERR_WRITE
1865         '
1866     EndIf
1867     '
1868     If M_20# = MContinue% Then *RETRY_PIASWRITE
1869     '
1870     fnPiasWrite = 1
1871     '
1872 FEnd
1873 '
1874 '■fnPCBNumberCheck
1875 ''' <summary>
1876 ''' Pias 基板番号照合要求
1877 ''' </summary>
1878 '''<param name="%"></param>
1879 '''<param name="%"></param>
1880 '''<returns></returns>
1881 ''' <remarks>
1882 ''' Date   : 2021/07/07 : M.Hayakawa
1883 ''' </remarks>'
1884 Function M% fnPCBNumberCheck
1885       fnPCBNumberCheck = 0
1886     '
1887 *RETRY_PCBCHECK
1888     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1889     'Piasへ基板照合開始 M310 -> ON
1890     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1891     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1892     '
1893     MJudge% = MNG%
1894     '
1895     For MStaNo = 0 To 5
1896         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1897             MJudge% = MOK%
1898             fnAutoScreenComment(96)  'AUTO画面
1899             MStaNo = 5
1900             Break
1901         '
1902         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1903             MJudge% = MNG%
1904             fnAutoScreenComment(97)  'AUTO画面
1905             MCommentD1001 = 37
1906             MCommentD1002 = 25
1907             MStaNo = 5
1908             Break
1909         '
1910         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1911             MJudge% = MNG%
1912             fnAutoScreenComment(98)  'AUTO画面
1913             MCommentD1001 = 38
1914             MCommentD1002 = 25
1915             MStaNo = 5
1916             Break
1917         '
1918         ElseIf M_In(11580) = 1 Then                         'time out
1919             MJudge% = MNG%
1920             fnAutoScreenComment(99)  'AUTO画面
1921             MCommentD1001 = 39
1922             MCommentD1002 = 25
1923             MStaNo = 5
1924             Break
1925         '
1926         Else
1927             MJudge% = MNG%
1928            MCommentD1001 = 41
1929            MCommentD1002 = 25
1930         '
1931         EndIf
1932         '
1933     Next MStaNo
1934     '
1935     'Piasへ基板照合開始 M310 -> OfF
1936     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1937     '
1938     '
1939     '通過履歴NG 工程抜けの場合
1940     If MJudge% = MPass% Then
1941         M_20# = MPass%
1942     EndIf
1943     '
1944    M_20# = MClear%     '初期化
1945     '
1946     'エラー画面
1947     If MJudge% < MOK% Then
1948     '
1949 '残しておくが現状では使用しないラベル
1950 *RETRY_ERR_PCBNUMBER
1951         M_20# = MClear%     '初期化
1952         'エラー処理記述
1953         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1954         'GOT KEY入力待ち
1955         MKeyNumber = fnKEY_WAIT()
1956         '
1957         If MKeyNumber = MAbout% Then   '停止を選択した場合
1958             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1959             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1960             Break
1961         '
1962         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1963             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1964             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1965         '
1966         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1967             M_20# = MPass%            'M_20# プログラム間共通外部変数
1968             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1969         '
1970         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1971             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1972             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1973             Break
1974         '
1975         EndIf
1976         '
1977         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1978         '
1979     EndIf
1980     '
1981     If M_20# = MContinue% Then *RETRY_PCBCHECK
1982 FEnd
1983 '
1984 '■ScrewTight_S2
1985 ''' <summary>
1986 ''' ねじ締めを行う
1987 ''' </summary>
1988 '''<param name="PScrewPos()">
1989 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
1990 '''             PScrewPos(2)    ：ねじ締め回避点
1991 '''             PScrewPos(10)   ：ねじ締め終了高さ
1992 '''</param>
1993 '''<returns>整数
1994 '''         0=異常終了、1=正常終了
1995 '''</returns>
1996 ''' <remarks>
1997 ''' Date   : 2021/07/07 : M.Hayakawa
1998 ''' </remarks>'
1999 Function M% ScrewTight_S2(ByVal PScrewPosition())   'ネジ締め個別設定
2000     ScrewTight_S2 = 0
2001     MOKNGFlg = 0
2002     Ovrd 100
2003     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
2004     ' 暫定
2005     Ovrd 5
2006     Mvs PScrewPosition(10),-10    ' パレット上ねじ締めS①の上空へ移動
2007 '    Ovrd MOvrdA
2008     '暫定マスク
2009 '    M_Out(Y62_Driver)=1     ' バンクセッティング　C1
2010 '    Dly 0.1
2011 '    M_Out(Y61_Driver)=1     'ドライバーON　CW
2012 '    'Spd 8.3 '外部ライド100  内部ライド60   'ライド100-40　100%：Spd　15　'ねじ締め速度設定
2013 '    Spd MSpdA               'ネジ締め時Spd個別設定
2014     ' 暫定移動のみ
2015     Mvs PScrewPosition(10)
2016 '    '
2017 '    Dly 0.1
2018 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
2019 '    Wait M_In(11584)=1          '完了/エラー検出
2020 '    Dly 0.1
2021 '    Spd M_NSpd
2022 '    '
2023 '    If M_In(X28_Driver)=1 Then  'ねじトータルエラー検出時
2024 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
2025 '        Dly 0.1
2026 '        M_Out(Y62_Driver)=0     'バンクセッティング解除　C1
2027 '        Dly 0.1
2028 '        M_Out(Y63_Driver)=0     'バンクセッティング解除　C1
2029 '        Dly 0.1
2030 '        M_Out(Y65_Driver)=0     'プログラム解除　F1
2031 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
2032 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
2033 '        MOKNGFlg = -1
2034 '        ScrewTight_S2 = 0
2035 '    Else
2036 '        Wait M_In(X29_Driver)=1 ' 正常完了時
2037 '        Dly 0.1
2038 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
2039 '        Dly 0.1
2040 '        M_Out(Y62_Driver)=0     'バンクセッティング解除
2041 '        Dly 0.1
2042 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
2043 '        Dly 0.1
2044 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
2045 '        ScrewTight_S2 = 1
2046 '    EndIf
2047 ' 暫定
2048     Ovrd 10
2049     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
2050     Ovrd 100
2051 FEnd
2052 '
2053 '■ScrewGet_S3
2054 ''' <summary>
2055 ''' ねじ供給機からねじを得る
2056 ''' </summary>
2057 '''<param name="%"></param>
2058 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
2059 '''         PScrewPos(2)    ：ねじ供給器回避点
2060 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2061 '''         PScrewPos(3)    ：Mねじポカヨケ位置
2062 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
2063 '''<returns>整数
2064 '''         0=異常終了、1=正常終了、-1=MネジセンサーNG、-2=MネジセンサーON、-3=吸着エラー
2065 '''</returns>
2066 ''' <remarks>
2067 ''' Date   : 2021/07/07 : M.Hayakawa
2068 ''' </remarks>'
2069 Function M% ScrewGet_S3(ByVal PScrewPosition())
2070     ScrewGet_S3 = 0
2071     MMScrewJudge% = 0
2072     'ねじ供給器初期動作エラーチェック
2073 ' ↓暫定削除
2074 '    Wait M_In(X34_ScrewReady1)=1 'ねじ供給器SがReadyになるまで待つ　←　何秒か待ってReadyにならなければ抜けるプログラムが必要？
2075 '    Ovrd 100
2076 '    If M_In(X33_SS2)=0 Then  'Mねじ検出センサがOFF（故障）していた場合
2077 '        Ovrd 30
2078 '        Mvs,-80             'その場所から80mm上空へ移動
2079 '        Mov PInitPos19049   '19049初期位置へ移動
2080 '        M_Out(Y6A_VV1)=0    'ねじ吸着 Off
2081 '        'NGとしてここの関数から抜ける
2082 '        ScrewGet_S3 = -1
2083 '        MMScrewJudge% = 1
2084 '        MCommentD1001 = 61
2085 '    EndIf
2086 '    If ScrewGet_S3 = 0 Then
2087 '        'Sタイト用ねじ供給機にMねじが混入していないか監視
2088 '        MMScrewJudge% = 0 'MMScrewJudgeを初期化する
2089 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
2090 '        If MRtn = 0 Then
2091 '            Ovrd 30
2092 '            Mvs,-80            'その場所から50mm上空へ移動
2093 '            Mov PInitPos19049  '19049初期位置へ移動
2094 '            MMScrewJudge% = 2
2095 '            MRtn = All_CLamp_Release()'全てのクランプ解除へ分岐
2096 '            MCnt% = 2   '2を設定
2097 '            MCommentD1001 = 62
2098 '        EndIf
2099 '        If MMScrewJudge% = 2 Then
2100 '            ScrewGet_S3 = -2
2101 '        EndIf
2102 '    EndIf
2103 '    'Mネジ判定がONの場合 NGとして関数を抜ける
2104 '    If MMScrewJudge% = 2 Then
2105 '        ScrewGet_S3 = -2
2106 '    EndIf
2107     'Sネジ用ねじ太郎のMネジ混入確認用ここまで
2108     Ovrd 100
2109     Spd M_NSpd
2110     If MMScrewJudge% = 0 Then
2111         ScrewGet_S3 = 0
2112         M_Out(Y63_Driver)=1         ' バンクセッティング　C2
2113         MScrewCnt% = 0
2114         MFinCnt% = 2
2115 '        For MCnt% = 0 To MFinCnt%
2116             Mov PScrewPosition(2)        ' ねじ供給機回避点
2117             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
2118             Ovrd 80
2119             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2120             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
2121             Mvs PScrewPosition(10), 1.2
2122             M_Out(Y6A_VV1)=1        ' ねじ吸着　ON
2123             'ビット回転
2124             M_Out(Y60_Driver)=1
2125             Dly 0.2
2126             '
2127             Ovrd 100
2128             JOvrd M_NJovrd
2129             Spd M_NSpd
2130             'ネジ吸着確認位置移動
2131             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
2132             Mvs PScrewPosition(10), -15  ' ネジ吸着確認位置
2133             'ビット回転停止
2134             'M_Out(Y60_Driver)=0
2135             '
2136             '1秒間ネジ吸着確認
2137 ' 以下暫定削除
2138 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2139 '            'MRtn = 0'強制エラー
2140 '            '吸着エラーの場合
2141 '            'ネジをねじ太郎に戻す
2142 '            If MRtn = 0 Then
2143 '                Ovrd 30
2144 '                'ビット回転停止
2145 '                M_Out(Y60_Driver)=0
2146 '                'ネジ供給機上空
2147 '                Mvs PScrewPos(1)
2148 '                '更に上空
2149 '                Mov PScrewPos(1), -75
2150 '                'ネジ捨て位置
2151 '                Mov PScrewFeedS021
2152 '                '吸着OFF
2153 '                M_Out(Y6A_VV1)=0 'ねじ吸着　OFF
2154 '                Dly 0.2
2155 '                '破壊ON
2156 '                M_Out(Y6B_VB1)=1 '真空破壊ON
2157 '                'ビット回転
2158 '                M_Out(Y61_Driver)=1
2159 '                Dly 0.5
2160 '                '
2161 '                Ovrd 100
2162 '                JOvrd M_NJovrd
2163 '                Spd M_NSpd
2164 '                'ドライバーを上下させねじを振り落とす
2165 '                Mov PScrewFeedS021, 10
2166 '                Mov PScrewFeedS021
2167 '                Dly 0.1
2168 '                Mov PScrewFeedS021, 10
2169 '                Mov PScrewFeedS021
2170 '                '
2171 '                'ネジ落ち待ち
2172 '                'ビット回転停止
2173 '                M_Out(Y61_Driver)=0
2174 '                Dly 0.1
2175 '                '破壊OFF
2176 '                M_Out(Y6B_VB1)=0 '真空破壊OFF
2177 '                '
2178 '                '
2179 '                'ねじ落ちたとして、移動更に上空
2180 '                Mov PScrewPos(1), -75
2181 '                Ovrd 100
2182 '                Spd M_NSpd
2183 '                'ネジ供給機上空
2184 '                Mvs PScrewPos(1)
2185 '                '
2186 '                ScrewGet_S3 = -3
2187 '                Break
2188 '                '
2189 '            Else
2190 '                MCnt% = MFinCnt%
2191 '                ScrewGet_S3 = 0
2192 '            EndIf
2193 '        Next  MCnt%
2194         '
2195         Ovrd 100
2196         Spd M_NSpd
2197         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2198         M_Out(Y60_Driver)=0     ' ビット回転停止
2199         M_Out(Y63_Driver)=0     ' バンクセッティング　C2
2200         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2201         'もう一度吸着確認
2202 ' 以下暫定削除
2203 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2204 '        If MRtn = 0 Then      '吸着エラーの場合
2205 '            MCommentD1001 = 94
2206 '            MCommentD1002 = 95
2207 '            ScrewGet_S3 = -3
2208 '        EndIf
2209 '        If MRtn = 1 Then      '吸着OKの場合
2210 '            ScrewGet_S3 = 1
2211 '        EndIf
2212 '        Break
2213     Else
2214         'Mネジ
2215         If MMScrewJudge% = 2 Then
2216             ScrewGet_S3 = -2
2217         EndIf
2218     EndIf
2219 FEnd
2220 '
2221 '■fnKEY_WAIT()
2222 ''' <summary>
2223 ''' GOTからのキー入力待ち
2224 ''' </summary>
2225 '''<returns>1：停止    2：次へ
2226 '''         3：継続    4：トルクチェック開始
2227 '''         5：NG
2228 '''         11：ロボット初期位置1    12：ロボット初期位置2
2229 '''         13：ロボット初期位置3    14：ロボット初期位置4
2230 '''</returns>
2231 ''' <remarks>
2232 ''' Date   : 2021/07/07 : M.Hayakawa
2233 ''' </remarks>'
2234 Function M% fnKEY_WAIT()
2235     fnKEY_WAIT = 0
2236     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
2237     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
2238     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
2239     '下記キー待ちの継続に反応させないため
2240     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
2241     Dly 0.2
2242     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
2243     MLocalLoopFlg=1
2244     While MLocalLoopFlg=1
2245         If M_In(11345) = 1 Then         '停止   M5345
2246             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
2247             fnKEY_WAIT = 1
2248             MLocalLoopFlg=-1
2249             Break
2250         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
2251             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
2252             fnKEY_WAIT = 2
2253             MLocalLoopFlg=-1
2254             Break
2255         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
2256             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
2257             fnKEY_WAIT = 3
2258             MLocalLoopFlg=-1
2259             Break
2260         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
2261             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
2262             fnKEY_WAIT = 4
2263             MLocalLoopFlg=-1
2264             Break
2265         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
2266             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
2267             fnKEY_WAIT = 5
2268             MLocalLoopFlg=-1
2269             Break
2270             '
2271         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
2272             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
2273             fnKEY_WAIT = MRobotInit1%
2274             MLocalLoopFlg=-1
2275             Break
2276             '
2277         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
2278             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
2279             fnKEY_WAIT = MRobotInit2%
2280             MLocalLoopFlg=-1
2281             Break
2282             '
2283         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
2284             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
2285             fnKEY_WAIT = MRobotInit3%
2286             MLocalLoopFlg=-1
2287             Break
2288             '
2289         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
2290             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
2291             fnKEY_WAIT = MRobotInit4%
2292             MLocalLoopFlg=-1
2293             Break
2294             '
2295         Else
2296         EndIf
2297     WEnd
2298     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
2299     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
2300 FEnd
2301 '
2302 '■ fnAUTO_CTL
2303 ''' <summary>
2304 ''' AUTOモードOFF、PLCからの開始待ち
2305 ''' </summary>
2306 ''' <remarks>
2307 ''' Date   : 2021/07/07 : M.Hayakawa
2308 ''' </remarks>
2309 Function M% fnAUTO_CTL
2310     fnAUTO_CTL = 0
2311     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2312     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
2313     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2314     '
2315     If M_Svo=0 Then             'サーボON確認
2316         Servo On
2317     EndIf
2318     Wait M_Svo=1
2319 FEnd
2320 '
2321 '■ fnWindScreenOpen
2322 ''' <summary>
2323 ''' ウィンド画面の表示、非表示設定
2324 ''' </summary>
2325 '''<param name="%"></param>
2326 '''<param name="%"></param>
2327 '''<param name="%"></param>
2328 '''<param name="%"></param>
2329 ''' <remarks>
2330 ''' コメントD1001, D1002, D1003の設定
2331 ''' MWindReSet = 0     画面非表示
2332 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2333 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2334 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2335 ''' Date   : 2021/07/07 : M.Hayakawa
2336 ''' </remarks>
2337 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2338     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2339         M_Out16(12480) = MCommentD1001            'D1001 コメント
2340     EndIf
2341     '
2342     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2343         M_Out16(12496) = MCommentD1002            'D1002 コメント
2344     EndIf
2345     '
2346     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2347        M_Out16(12512) = MCommentD1003            'D1003 コメント
2348     EndIf
2349     '
2350     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2351     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2352     Dly 0.5
2353     M_Out(12363) = 0                         'ウィンド画面設定
2354 FEnd
2355 '
2356 '■FnCtlValue2
2357 ''' <summary>
2358 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2359 ''' </summary>
2360 ''' <param name="MCtlNo%"></param>
2361 ''' <remarks>
2362 ''' Date : 2022/04/28 渡辺
2363 ''' </remarks>
2364 '''
2365 '''  1：投入数       ＋１
2366 '''  2：組立ＯＫ数   ＋１
2367 '''  3：組立ＮＧ数   ＋１ (未使用)
2368 '''  4：吸着エラー数 ＋１
2369 ''' 99：読書開始信号 OFF
2370 '''
2371 Function M% FnCtlValue2(ByVal MCtlNo%)
2372     FnCtlValue2 = 1
2373     Select MCtlNo%
2374         Case 1        '投入数＋１
2375             M_Out(12569) = 0             '書込み開始信号OFF
2376             M_Out(12568) = 1             '読込み開始信号ON
2377             MInputQty = M_In16(11600)    '投入数受信
2378             MInputQty = MInputQty + 1    '投入数＋１
2379             M_Out16(12592) = MInputQty   '投入数送信
2380             M_Out(12569) = 1             '書込み開始信号ON
2381             Break
2382             '
2383         Case 2        '組立ＯＫ数＋１
2384             M_Out(12569) = 0             '書込み開始信号OFF
2385             M_Out(12568) = 1             '読込み開始信号ON
2386             MAssyOkQty = M_In16(11616)   '組立OK数受信
2387             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2388             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2389             M_Out(12569) = 1             '書込み開始信号ON
2390             Break
2391             '
2392         Case 4        '吸着エラー数＋１
2393             M_Out(12569) = 0                       '書込み開始信号OFF
2394             M_Out(12568) = 1                       '読込み開始信号ON
2395             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
2396             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
2397             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
2398             M_Out(12569) = 1                       '書込み開始信号ON
2399             Break
2400             '
2401         Case 99        '読書開始信号OFF
2402             M_Out(12568) = 0        '読込み開始信号OFF
2403             M_Out(12569) = 0        '書込み開始信号OFF
2404             Break
2405             '
2406     End Select
2407     Exit Function
2408 FEnd
2409 'Insightによる画像処理検査実行（並列処理なし）
2410 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2411 '-------------------------------------------------------------------------------
2412 'Insightによる画像処理検査実行（並列処理なし）
2413 '   引数
2414 '       PInspPos()      ：検査位置
2415 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2416 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2417 '       MInspCnt%       ：検査位置数
2418 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2419 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2420 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2421 '   戻り値：整数
2422 '       0=異常終了、1=正常終了
2423 '
2424 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2425 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2426 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2427 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2428 '   20200410    :   検査グループ設定Retry追加
2429 '-------------------------------------------------------------------------------
2430     '----- 初期設定 -----
2431     Cnt 0                                                           '移動効率化解除(初期値=0)
2432     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2433 '    Cnt 1,0.1,0.1
2434     '変数宣言・初期化
2435     Def Inte MNum                                                   '検査番号(検査順1～)
2436     MNum% = 1                                                       '検査番号初期値設定
2437     Def Inte MEndFlg                                                '検査終了フラグ
2438     MEndFlg% = 0
2439     '
2440     '検査G番号設定要求・検査実行要求off
2441     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2442     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2443     'エラー番号クリア
2444     MInspErrNum = 0                                                 '検査実行エラー番号
2445     M_Out16(MOUT_InspErrNum) = MInspErrNum
2446     MInspNGStepNum = 0                                              '検査実行NGStep番号
2447     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2448     '
2449     'Insight Ready check?
2450     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2451         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2452         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2453         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2454         ISInspectionSingle = 0                                      '異常終了戻り値設定
2455         Exit Function
2456     EndIf
2457     '
2458     '検査位置数確認
2459     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2460         MInspErrNum = 21                                            '検査データなし 21　引数<1
2461         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2462         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2463         ISInspectionSingle = 0                                      '異常終了戻り値設定
2464         Exit Function
2465     EndIf
2466     '
2467     '
2468     '
2469     '----- メイン処理 -----
2470     '設定された検査位置数分の検査実行
2471     While( MEndFlg% = 0 )
2472         '----- 検査グループ番号設定Retry追加 20200410
2473         MSetGrNumRetryExitFlg = 0
2474         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2475         While( MSetGrNumRetryExitFlg = 0 )
2476         '----- 検査グループ番号設定Retry追加ここまで 20200410
2477             '
2478             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2479             '
2480             '----- 検査グループ番号設定 -----
2481             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2482             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2483             '
2484             '検査位置へ移動・移動完了待ち
2485             Mvs PInspPos( MNum% )                                       '移動
2486             Dly 0.05                                                    '移動完了後Delay
2487             '
2488             '検査グループ番号設定終了確認
2489             M_Timer(1) = 0
2490             MExitFlg = 0
2491             While( MExitFlg = 0 )
2492                 '検査G設定正常終了?
2493                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2494                     MExitFlg = 1
2495                 '
2496                 '検査G設定異常終了?
2497                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2498                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2499                     If MInspErrNum = 0 Then                             '1回目のエラー?
2500                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2501                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2502                     EndIf
2503                     MExitFlg = 1
2504                 '
2505                 'timeoutチェック
2506                 ElseIf 1000 < M_Timer(1) Then
2507                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2508                     If MInspErrNum = 0 Then                             '1回目のエラー?
2509                         MInspErrNum = 12                                'timeout エラー番号=12
2510                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2511                     EndIf
2512                     MExitFlg = 1
2513                 EndIf
2514             WEnd
2515             '
2516             '検査G番号設定要求off
2517             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2518             '
2519             '----- 検査グループ設定Retry追加 20200410
2520             'NGなければ抜ける
2521             If MCurrentStepErr = 0 Then
2522                 MSetGrNumRetryExitFlg = 1
2523             Else
2524                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2525                 If MSetGrNumRetryCnt = 0 Then
2526                     MSetGrNumRetryExitFlg = 1
2527                 Else
2528                     'Retryへ　その前にDelay
2529                     Dly 0.5
2530                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2531                 EndIf
2532             EndIf
2533             '----- 検査グループ設定Retry追加ここまで 20200410
2534             '
2535         WEnd
2536         '
2537         '
2538         '
2539         '----- 検査実行 -----
2540         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2541             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2542                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2543                 MInspRetryExitFlg = 0
2544                 MRetryCnt = 2                                        'Retry回数設定
2545                 While( MInspRetryExitFlg = 0 )
2546                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2547                     '
2548                     '検査完了確認
2549                     MRetryCnt = MRetryCnt - 1
2550                     M_Timer(1) = 0
2551                     MExitFlg = 0
2552                     While( MExitFlg = 0 )
2553                     '検査完了待ち
2554                         '検査OK終了?
2555                         If M_In( MIN_IS_InspOK% ) = 1  Then
2556                             MJudgeOKFlg = 1                         '検査OKフラグON
2557                             MExitFlg = 1
2558                         '
2559                         '検査NG終了?
2560                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2561                             If MInspErrNum = 0 Then                 '1回目のエラー?
2562                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2563                                     MInspErrNum = 32                    '検査NG エラー番号=32
2564                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2565                                 EndIf
2566                             EndIf
2567                             MExitFlg = 1
2568                         '
2569                         '検査異常終了(IS timeout)?
2570                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2571                             If MInspErrNum = 0 Then                 '1回目のエラー?
2572                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2573                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2574                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2575                                 EndIf
2576                             EndIf
2577                             MExitFlg = 1
2578                         '
2579                         'timeoutチェック
2580                         ElseIf 3000 < M_Timer(1) Then
2581                             If MInspErrNum = 0 Then                 '1回目のエラー?
2582                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2583                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2584                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2585                                 EndIf
2586                             EndIf
2587                             MExitFlg = 1
2588                         EndIf
2589                     WEnd
2590                     '
2591                     '検査開始要求off
2592                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2593                     '
2594                     'OKなら抜ける
2595                     If MJudgeOKFlg = 1 Then
2596                         MInspRetryExitFlg = 1
2597                     Else
2598                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2599                         If MRetryCnt = 0 Then
2600                             MInspRetryExitFlg = 1
2601                         Else
2602                             'Retryへ　その前にDelay
2603                             Dly 0.3
2604                         EndIf
2605                     EndIf
2606                     '
2607                 WEnd
2608             EndIf
2609         EndIf
2610         '
2611         '
2612         '
2613         MNum% = MNum% + 1                                           '検査Step+1
2614         '検査終了確認　検査終了フラグセット
2615         If (MInspCnt% < MNum% ) Then
2616             MEndFlg% = 1                                            '検査終了フラグセット
2617         EndIf
2618         'NG発生時続行時処理
2619         If MInspErrNum <> 0 Then                                    'NGあり?
2620             If MNgContinue% <> 1 Then                               'NG続行?
2621                 MEndFlg% = 1                                        '検査終了フラグセット
2622             EndIf
2623         EndIf
2624     WEnd
2625     '
2626     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2627     If 0 < MZAxis% Then
2628         PCurrentPos = P_Curr                                        '現在位置取得
2629         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2630         Mvs PCurrentPos                                             '現在位置上空へ移動
2631     EndIf
2632     '
2633     '戻り値設定
2634     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      'カメラ検査強制OK(M_In(11372)=1)追加(12/21中村)
2635         ISInspectionSingle = 1                                      '正常終了戻り値設定
2636     Else
2637         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2638         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2639         ISInspectionSingle = 0                                      '異常終了戻り値設定
2640     EndIf
2641     '
2642     Fine 0,P    'Fine切り(22/12/09中村)
2643 FEnd
2644 '
2645 ' ■ISInspection
2646 ''' <summary>
2647 ''' Insightによる画像処理検査実行
2648 ''' </summary>
2649 '''<param name="PInspPos()">検査位置</param>
2650 '''<param name="MInspGrNum%()">検査位置での検査グループ番号（=0：画像検査未実施）</param>
2651 '''             PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2652 '''<param name="MInspCnt%">検査位置数</param>
2653 '''<param name="MZAxis%">終了時のZ軸退避座標（-1:無効）</param>
2654 '''             終了時にZ軸をMZAxisで設定された位置まで上昇させる
2655 '''<param name="MNgContinue%">=1で検査エラー・NG発生時に全Stepの検査を行う</param>
2656 '''<returns>    整数 0=異常終了、1=正常終了</returns>
2657 '''         MInspErrNum     ：異常終了時にエラー番号が設定される
2658 '''         MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される"
2659 ''' <remarks>
2660 ''' Date   : 2021/07/07 : M.Hayakawa
2661 ''' </remarks>
2662 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2663 '    '画像使用確認 0<- 画像確認無しの場合
2664 '    If M_In(11369) = 0 Then            'toRBT_使用確認
2665 '        ISInspection = 1                                        '正常終了戻り値設定
2666 '    EndIf
2667 ''
2668 '    Cnt 0                                                       '移動効率化解除(初期値=0)
2669 '    Fine 0.05,P                                                 '位置決め完了条件設置　0.05mm
2670 '    MNum% = 1                                                   '検査番号初期値設定
2671 '    Def Inte MEndFlg                                            '検査終了フラグ
2672 '    MEndFlg% = 0
2673 '    '
2674 '    'エラー番号クリア
2675 '    MInspErrNumSub = 0                                          '検査実行エラー番号sub
2676 '    MInspErrNum = 0                                             '検査実行エラー番号
2677 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2678 '    MInspNGStepNum = 0                                          '検査実行NGStep番号
2679 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2680 '    '
2681 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready offなら終了
2682 '        MInspErrNum = 20                                        '検査実行エラー番号 20 Insight offline
2683 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2684 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2685 '        ISInspection = 0                                        '異常終了戻り値設定
2686 ''
2687 '    EndIf
2688 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2689 '    '
2690 '    '検査位置数確認
2691 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2692 '        MInspErrNum = 21                                        '検査データなし 21　引数<1
2693 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2694 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2695 '        ISInspection = 0                                        '異常終了戻り値設定
2696 ''
2697 '    EndIf
2698 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2699 '    '
2700 '    '設定された検査位置数分の検査実行
2701 '    While( MEndFlg% = 0 )
2702 '        '検査終了確認　検査終了フラグセット
2703 '        If (MInspCnt% < MNum% ) Then
2704 '            MEndFlg% = 1                                        '検査終了フラグセット
2705 '        EndIf
2706 '        '
2707 '        'タスク　検査G番号設定・検査完了確認処理開始　INSPTAST1
2708 '        If MEndFlg% = 0 Then
2709 '            M_01# = MInspGrNum%(MNum%)                          '検査G番号引渡し
2710 '        EndIf
2711 '        M_02# = MEndFlg%                                        '検査終了フラグ引渡し
2712 '        M_05# = MNum%                                           '検査番号(検査順1～)
2713 '        'タスク　検査G設定フラグ引渡し
2714 '        If MEndFlg% = 0 Then
2715 '            If 0 < MInspGrNum%(MNum%) Then
2716 '                M_03# = 1
2717 '            Else
2718 '                M_03# = 0
2719 '            EndIf
2720 '        Else
2721 '            M_03# = 0
2722 '        EndIf
2723 '        'タスク　検査結果確認フラグ引渡し
2724 '        If 1 < MNum% Then
2725 '            If 0 < MInspGrNum%(MNum%-1) Then
2726 '                M_04# = 1
2727 '            Else
2728 '                M_04# = 0
2729 '            EndIf
2730 '        Else
2731 '            M_04# = 0
2732 '        EndIf
2733 '        '
2734 '        'タスク処理開始
2735 '        M_00# = 1                                               'TASK処理開始
2736 '        'タスク処理開始確認
2737 '        M_Timer(1) = 0
2738 '        MExitFlg = 0
2739 '        While( MExitFlg = 0 )
2740 '            '処理開始完了確認
2741 '            If M_00# = 0 And M_10# = 8 Then
2742 '                MExitFlg = 1
2743 '            EndIf
2744 '            'timeoutチェック
2745 '            If 2000 < M_Timer(1) Then
2746 '                If MNgContinue% = 1 Then                        'NG続行?
2747 '                    MInspErrNumSub = 36                         'エラー番号設定36
2748 '                Else
2749 '                    MInspErrNum = 36                            'エラー番号設定36
2750 '                EndIf
2751 '                MExitFlg = 1
2752 '            EndIf
2753 '        WEnd
2754 '        '
2755 '        '検査位置へ移動・移動完了待ち
2756 '        If 0 = MInspErrNum Then
2757 '            If MEndFlg% = 0 Then
2758 '                Mvs PInspPos( MNum% )                           '移動
2759 '            EndIf
2760 '        EndIf
2761 '        '
2762 '        'タスク　検査G番号設定・検査完了確認処理終了待ち　INSPTAST1
2763 '        If 0 = MInspErrNum Then
2764 '            M_Timer(1) = 0
2765 '            MExitFlg = 0
2766 '            While( MExitFlg = 0 )
2767 '                '処理完了待ち（正常終了）
2768 '                If M_10# = 1 Then
2769 '                    MExitFlg = 1
2770 '                EndIf
2771 '                '処理完了待ち（異常終了）
2772 '                If M_10# = 0 Then
2773 '                    If MNgContinue% = 1 Then                    'NG続行?
2774 '                        MInspErrNumSub = M_12#                  'エラー番号設定　M12
2775 '                    Else
2776 '                        MInspErrNum = M_12#                     'エラー番号設定　M12
2777 '                    EndIf
2778 '                    MExitFlg = 1
2779 '                EndIf
2780 '                'timeoutチェック
2781 '                If 5000 < M_Timer(1) Then
2782 '                    If MNgContinue% = 1 Then                    'NG続行?
2783 '                        MInspErrNumSub = 31                     'エラー番号設定31
2784 '                    Else
2785 '                        MInspErrNum = 31                        'エラー番号設定31
2786 '                    EndIf
2787 '                    MExitFlg = 1
2788 '                EndIf
2789 '            WEnd
2790 '        EndIf
2791 '        '
2792 '        '検査結果確認
2793 '        If 0 = MInspErrNum Then
2794 '            If 1 < MNum% Then
2795 '                If 0 < MInspGrNum%(MNum%-1) Then                '検査あり?
2796 '                    If M_11# = 2 Then                           '検査NG?
2797 '                        If MNgContinue% = 1 Then                'NG続行?
2798 '                            If MInspNGStepNum = 0 Then          'NG未発生?
2799 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2800 '                            EndIf
2801 '                            MInspErrNumSub = 32                 'エラー番号設定 32:検査NG
2802 '                        Else
2803 ''                            MInspNGStepNum = MNum% - 1          '検査実行NGStep番号設定
2804 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2805 '                            MInspErrNum = 32                    'エラー番号設定 32:検査NG
2806 '                        EndIf
2807 '                   EndIf
2808 '                EndIf
2809 '            EndIf
2810 '        EndIf
2811 '        '
2812 '        'エラーなら検査中断終了するのでLoopから抜けるため終了フラグセット
2813 '        If 0 <> MInspErrNum Then
2814 '            MEndFlg% = 1
2815 '        EndIf
2816 '        '
2817 '        '検査実行、取込完了待ち
2818 '        If 0 = MInspErrNum Then
2819 '            If MEndFlg% = 0 Then
2820 '                If 0 < MInspGrNum%(MNum%) Then                  '検査あり?
2821 '                    M_Out(MOUT_IS_Insp%) = 1                    '検査実行要求on
2822 '                    '取込完了確認
2823 '                    M_Timer(1) = 0
2824 '                    MExitFlg = 0
2825 '                    While( MExitFlg = 0 )
2826 '                        '処理完了待ち
2827 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2828 '                            MExitFlg = 1
2829 '                        EndIf
2830 '                        'timeoutチェック
2831 '                        If 2000 < M_Timer(1) Then
2832 '                            If MNgContinue% = 1 Then            'NG続行?
2833 '                                MInspErrNumSub = 33             'エラー番号設定33
2834 '                            Else
2835 '                                MInspErrNum = 33                'エラー番号設定33
2836 '                            EndIf
2837 '                            MExitFlg = 1
2838 '                        EndIf
2839 '                    WEnd
2840 '                EndIf
2841 '                '
2842 '            EndIf
2843 '        EndIf
2844 '        MNum% = MNum% + 1
2845 '    WEnd
2846 '    '
2847 '    '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2848 '    If 0 < MZAxis% Then
2849 '        PCurrentPos = P_Curr                                    '現在位置取得
2850 '        PCurrentPos.Z = MZAxis%                                 'Z軸を設定
2851 '        Mvs PCurrentPos                                         '現在位置上空へ移動
2852 '    EndIf
2853 '    '
2854 '    'NG続行時処理
2855 '    If MNgContinue% = 1 Then                                    'NG続行?
2856 '        MInspErrNum = MInspErrNumSub                            'エラー番号設定
2857 '    EndIf
2858 '    '
2859 '    '戻り値設定
2860 '    If MInspErrNum = 0 Then
2861 '        ISInspection = 1                                        '正常終了戻り値設定
2862 '    Else
2863 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2864 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2865 '        ISInspection = 0                                        '異常終了戻り値設定
2866 '    EndIf
2867 '    '
2868 '*ISInspection_End
2869 'FEnd
2870 '
2871 '■InitialZoneB
2872 ''' <summary>
2873 ''' 非常停止後の復帰動作
2874 ''' 1)上空退避　Z方向上に移動
2875 ''' 2)J1軸以外を退避ポジションへ移動
2876 ''' 3)J1軸のみを退避ポジションへ移動
2877 ''' 4)イニシャルポジションへ移動
2878 ''' </summary>
2879 ''' <remarks>
2880 ''' Date : 2022/04/08 : N.Watanabe
2881 ''' </remarks>
2882 Function V fnInitialZoneB()
2883     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中] 2022/04/26 渡辺
2884 '
2885 'パラメータ
2886     Ovrd 5
2887 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2888 '    Cmp Pos, &B100011
2889 '
2890 '復帰動作開始
2891 '
2892 '置き台と両掴みの場所は、チャックを解放する
2893 *RecoveryChuckOpen
2894     PActive = P_Curr          '現在位置を取得
2895     MRecoveryChuckOpen = 0    'チャック解放フラグ 初期化
2896 'PProductOnRoboSet(ねじロボ製品置き位置)は、チャック解放
2897     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2898         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2899             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2900                 MRecoveryChuckOpen = 1
2901             EndIf
2902         EndIf
2903     EndIf
2904 'PProductOnRoboGet(ねじロボ製品取り位置)は、チャック解放
2905     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2906         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2907             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2908                 MRecoveryChuckOpen = 1
2909             EndIf
2910         EndIf
2911     EndIf
2912 '
2913 '    If MRecoveryChuckOpen = 1 Then
2914 '        M_Out(12256) = 0        '本体チャック閉OFF
2915 '        M_Out(12257) = 1        '本体チャック開ON
2916 '        M_20# = 0               'KEY入力初期化
2917 '        MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
2918 '        If MRtn = 0 Then
2919 '            fErrorProcess(11,244,284,0)
2920 '            If M_20# = MNext% Then M_20# = MClear%
2921 '            If M_20# = MAbout% Then GoTo *RecoveryEnd
2922 '            If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2923 '            If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2924 '        Else
2925 '            M_Out(12257) = 0        '本体チャック開OFF
2926 '        EndIf
2927 '    EndIf
2928 '
2929     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2930     M_Out(12256) = 0                           '本体チャック閉OFF
2931     M_Out(12257) = 1                           '本体チャック開ON
2932 '
2933     M_20# = 0                                  'KEY入力初期化
2934     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
2935     If MRtn = 1 Then M_Out(12257) = 0          '本体チャック開OFF
2936     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2937 '
2938     fErrorProcess(11,244,284,0)
2939     If M_20# = MNext% Then M_20# = MClear%
2940     If M_20# = MAbout% Then GoTo *RecoveryEnd
2941     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2942     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2943 '
2944     *RecoveryChuckOpenEnd
2945 '
2946 '背面板回避
2947 'PPlateBackSet～PPlateBackSet_6のエリアにいるときは、本体チャック開く
2948 '・PPlateBackSet_6         '経路6
2949 '・PPlateBackSet_5         '経路7
2950 '・PPlateBackSet_4         '経路8
2951 '・PPlateBackSet_3         '経路9
2952 '・PPlateBackSet_2         '経路10
2953 '・PPlateBackSet_1         '経路11
2954 '・PPlateBackSet           '背面板置き位置
2955 '上記７点のＸ座標・Ｙ座標・Ｚ座標とJ6軸が下記If文の範囲に入っている事を確認する事
2956     PActive = P_Curr                    '現在位置を取得
2957     JActive = J_Curr                    '現在位置を取得
2958     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
2959     If (PActive.X >= -35) And (PActive.X <= -5) Then
2960         If (PActive.Y >= 340) And (PActive.Y <= 515) Then
2961             If (PActive.Z >= 470) And (PActive.Z <= 560) Then
2962                 If (MJ6 >= 160) And (MJ6 <= 200) Then
2963                     M_Out(12256) = 0            '本体チャック閉OFF
2964                     M_Out(12257) = 1            '本体チャック開ON
2965                 Dly 1.0
2966                 EndIf
2967             EndIf
2968         EndIf
2969     EndIf
2970 '
2971 '
2972 '特殊回避　直接、上空退避が出来ない所の対処
2973 '
2974     Ovrd 1
2975 'PProductOnRoboSet(Get)～PProductOnRoboSet(Get)_2のエリアにいるときは、PProductOnRoboSet_2へ
2976 '・PProductOnRoboSet
2977 '・PProductOnRoboSet_1
2978 '・PProductOnRoboSet_2
2979 '・PProductOnRoboGet
2980 '・PProductOnRoboGet_1
2981 '・PProductOnRoboGet_2
2982 '上記６点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
2983     PActive = P_Curr                    '現在位置を取得
2984     JActive = J_Curr                    '現在位置を取得
2985     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
2986     If (PActive.X >= -40) And (PActive.X <= 0) Then
2987         If (PActive.Y >= 380) And (PActive.Y <= 420) Then
2988             If (PActive.Z >= 300) And (PActive.Z <= 450) Then
2989                 If (MJ6 >= -20) And (MJ6 <= 20) Then
2990                     Mvs PProductOnRoboSet_1
2991                     Dly 1.0
2992                     Mvs PProductOnRoboSet_2
2993                     Dly 1.0
2994                     Mov PProductOnRoboSet_3
2995                     Dly 1.0
2996                 EndIf
2997             EndIf
2998         EndIf
2999     EndIf
3000 '
3001 'PProductOnRoboSet(Get)_2～PProductOnRoboSet(Get)_3のエリアにいるときは、PProductOnRoboSet_3へ
3002 '・PProductOnRoboSet_2
3003 '・PProductOnRoboSet_3
3004 '・PProductOnRoboGet_2
3005 '・PProductOnRoboGet_3
3006 '上記４点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
3007     PActive = P_Curr                    '現在位置を取得
3008     JActive = J_Curr                    '現在位置を取得
3009     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
3010     If (PActive.X >= -40) And (PActive.X <= 0) Then
3011         If (PActive.Y >= 220) And (PActive.Y <= 420) Then
3012             If (PActive.Z >= 400) And (PActive.Z <= 570) Then
3013                 If (MJ6 >= -20) And (MJ6 <= 20) Then
3014                     Mvs PProductOnRoboSet_3
3015                     Dly 1.0
3016                 EndIf
3017             EndIf
3018         EndIf
3019     EndIf
3020 '
3021     Ovrd 5
3022 '
3023 '上空退避
3024     PActive = P_Curr
3025     Pmove = PActive
3026     Pmove.Z = 640           '上空退避する一律の高さ
3027     If PActive.X > 550 Then
3028         Pmove.Z =550        'パレット上に腕を伸ばしているときは640まで上げられない為、例外処置
3029     EndIf
3030     If PActive.Z < Pmove.Z Then
3031         Mvs Pmove
3032     EndIf
3033     Dly 1.0
3034 'J1軸以外を退避ポジションへ移動
3035     JActive = J_Curr
3036     Jmove = JTaihi
3037     Jmove.J1 = JActive.J1        'J1軸は現在値を使用し、JTaihiのポーズを取る
3038     Jmove.J6 = JActive.J6        'J6軸は現在値を使用し、JTaihiのポーズを取る
3039     Mov Jmove
3040     Dly 1.0
3041 'J1軸のみを退避ポジションへ移動
3042     Mov JTaihi
3043     Dly 1.0
3044 'イニシャルポジションへ移動
3045     Mov PInitialPosition
3046     Cmp Off
3047     Ovrd 100
3048 ' ねじロボを初期位置に戻すために強制的に自動運転開始
3049     If M_In(11856) = 0 Then                 ' 停止中のみ
3050         fnAutoScreenComment(501)            ' 状態表示[ネジ締め機自動運転開始中] 2022/04/25 渡辺
3051         M_Out(12834) = 1                    ' 自動運転開始ON 12834   M6834
3052         MRet = frInCheck(11842, 1, 30000&)  ' 自動運転開始受信待ち    11842   M5842
3053         If MRet = 0 Then
3054         Else
3055             M_Out(12834) = 0    ' 自動運転開始OFF 12834   M6834
3056         EndIf
3057     EndIf
3058     M_Out(12262) = 0            '位置決め出OFF
3059     M_Out(12263) = 1            '位置決め戻ON
3060     fErrorProcess(11,253,281,0)
3061 *RecoveryEnd
3062     Exit Function
3063 FEnd
3064 '
3065 '
3066 '■fnAutoScreenComment
3067 ''' <summary>
3068 ''' メイン画面の動作状況表示
3069 ''' コメントD1005の設定
3070 ''' </summary>
3071 '''<param name="McommentD1005%">コメントID</param>
3072 ''' <remarks>
3073 ''' Date   : 2021/07/07 : M.Hayakawa
3074 ''' </remarks>
3075 Function fnAutoScreenComment(ByVal McommentD1005%)
3076     M_Out16(12576) = McommentD1005%
3077 FEnd
3078 '
3079 '■fnRoboPosChk
3080 ''' <summary>
3081 ''' 最後に終了したロボットポジションの確認
3082 ''' </summary>
3083 '''<param name="MINNumber%">入力番号</param>
3084 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
3085 '''<param name="MTimeCnt&">タイムアウト時間</param>
3086 ''' PLCに保続した番号を読込み、確認
3087 ''' MRBTOpeGroupNo = 5 が初期位置に設定
3088 '''<returns>整数 0:タイムアウト 1:OK</returns>
3089 ''' <remarks>
3090 ''' Date   : 2021/07/07 : M.Hayakawa
3091 ''' </remarks>
3092 Function M% fnRoboPosChk
3093     fnRoboPosChk = 0
3094     MRet = fnStepRead()
3095     '初期位置でないと判断した場合
3096     'ウィンド画面切換え
3097     If MRBTOpeGroupNo > 5 Then
3098         '下記キー待ちの継続に反応させないため
3099         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3100         Dly 0.2
3101         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3102         Dly 1.5
3103         '
3104         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
3105         '
3106         MLoopFlg% = 1
3107         While MLoopFlg% = 1
3108             '
3109             '
3110             MKeyNumber% = fnKEY_WAIT()
3111             Select MKeyNumber%
3112                 Case Is = MAbout%       '停止
3113                     M_20# = MAbout%
3114                     MLoopFlg% = -1
3115                     Break
3116                 Case Is = MNext%        '次へ
3117                     'MLoopFlg% = -1
3118                     Break
3119                 Case Is = MContinue%    '継続
3120                     M_20# = MContinue%
3121                     MLoopFlg% = -1
3122                     Break
3123                 Default
3124                     Break
3125             End Select
3126         WEnd
3127     EndIf
3128     '
3129     If M_20# = MContinue% Then                              '継続ボタンが押された場合
3130         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
3131         Ovrd 5                                   '低速オーバーライド値設定
3132         Select MRBTOpeGroupNo
3133             Case Is = 5                          '何もしない
3134                 Break
3135             Case Is = 10                         '初期位置へ戻す
3136                 'Mov PTEST001
3137                 Break
3138             Case Is = 15                         '初期位置へ戻す
3139                 'Mov PTEST002
3140                 Dly 0.5
3141                 'Mov PTEST001
3142                 Dly 0.5
3143                 Break
3144             Default
3145                 Break
3146         End Select
3147         '
3148         Ovrd M_NOvrd                            'システムの初期値を設定
3149         M_Out(12364) = 1                        'toPLC_データ保存ON
3150         MRBTOpeGroupNo = 5
3151         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
3152         Dly 1.0
3153         M_Out(12364) = 0                        'toPLC_データ保存OFF
3154         fnRoboPosChk = 1                        '初期位置動作実行
3155         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
3156     EndIf
3157     Exit Function
3158 FEnd
3159 '
3160 '■frInCheck
3161 ''' <summary>
3162 ''' センサーINチェック
3163 ''' </summary>
3164 '''<param name="MINNumber%">入力番号</param>
3165 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
3166 '''<param name="MTimeCnt&">タイムアウト時間</param>
3167 '''<returns>整数 0:タイムアウト 1:OK</returns>
3168 ''' <remarks>
3169 ''' Date   : 2021/07/07 : M.Hayakawa
3170 ''' </remarks>
3171 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
3172     M_Timer(4) = 0
3173     MloopFlg = 0
3174     While MloopFlg = 0
3175         MCrtTime& = M_Timer(4)
3176         If M_In(MINNumber%) = MCMPFLG% Then
3177             MloopFlg = 1
3178             frInCheck = 1
3179         ElseIf MCrtTime& > MTimeCnt& Then
3180             MloopFlg = 1
3181             frInCheck = 0
3182         EndIf
3183     WEnd
3184 FEnd
3185 '-----------------------------------------------
3186 '
3187 'ねじ締め機通信確認
3188 '
3189 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3190 'fScrewTcomChk = 0　：正常終了
3191 '          　 　 -1 ：異常終了
3192 '-----------------------------------------------
3193 Function M% fScrewTcomChk
3194 *ReCheckScewTcomChk
3195     fScrewTcomChk = 0
3196     '通信確認送信
3197     M_Out(MOUT_ScwT_ComChk%) = MOn%
3198     '通信確認受信待機
3199 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
3200     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
3201     '通信確認送信終了
3202     M_Out(MOUT_ScwT_ComChk%) = MOff%
3203     If MRtn = 0 Then
3204         fScrewTcomChk = -1
3205     EndIf
3206     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
3207  '
3208 FEnd
3209 '
3210 '
3211 '-----------------------------------------------
3212 '
3213 'ねじ締め開始送信
3214 '
3215 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3216 'fScrewTStart = 0　：正常終了
3217 '           　　-1 ：異常終了
3218 '-----------------------------------------------
3219 Function M% fScrewTStart
3220     fScrewTStart = 0
3221     nRet% = 0
3222     'ねじ締め開始待機を受信
3223 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
3224     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
3225     If MRtn = 0 Then nRet% = -1
3226     If MRtn = 0 Then GoTo *ScrewStartERROR      '開始できなかった場合ジャンプ
3227     Dly 0.1
3228     'ねじ締め開始受信を送信
3229     M_Out(MOUT_ScwT_ST%) = MOn%
3230     Dly 0.5
3231     'Wait M_In(MTEST_KEY%) = MOn%
3232     'ねじ締め開始送信終了
3233     M_Out(MOUT_ScwT_ST%) = MOff%
3234     '
3235 *ScrewStartERROR
3236     fScrewTStart = nRet%
3237 FEnd
3238 '
3239 '
3240 '
3241 '-----------------------------------------------
3242 '
3243 'ねじ締め完了受信
3244 '
3245 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3246 'fScewTFinish = 0　：正常終了
3247 '          　 　-1 ：異常終了
3248 '-----------------------------------------------
3249 Function M% fScewTFinish
3250 *ReCheckScewTFinish
3251     fScewTFinish = 0
3252     'ねじ締め完了待機を受信
3253 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3254     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3255     If MRtn = 0 Then
3256         fScewTFinish = -1
3257     EndIf
3258     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3259     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3260     Dly 0.1
3261     'ねじ締め完了受信を送信
3262     M_Out(MOUT_ScwT_FinOK%) = MOn%
3263     Dly 0.5                          'とりあえず保持時間0.5msec
3264     'ねじ締め開始送信終了
3265     M_Out(MOUT_ScwT_FinOK%) = MOff%
3266     'Wait M_In(MTEST_KEY%) = MOn%
3267     '
3268 *ScewTFinish_ErrEnd
3269 FEnd
3270 '
3271 '
3272 '-----------------------------------------------
3273 '
3274 '条件xx停止受信
3275 '
3276 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3277 'fScewTCaseStop = 0　：正常終了
3278 '          　   　-1 ：異常終了
3279 '-----------------------------------------------
3280 Function M% fScewTCaseStop(ByVal MCase%())
3281 *ReCheckScewTCaseStop
3282     fScewTCaseStop = 0
3283     '条件xx停止を受信
3284     Wait M_In(MCase%(1)) = MOn%
3285     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3286     If MRtn = 0 Then
3287         fScewTCaseStop = -1
3288     EndIf
3289     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3290     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3291     Dly 0.1
3292     '条件xx停止受信を送信
3293     M_Out(MCase%(2)) = MOn%
3294     Dly 0.5                          'とりあえず保持時間0.5msec
3295     'ねじ締め開始送信終了
3296     M_Out(MCase%(2)) = MOff%
3297 *ScewTCaseStop_ErrEnd
3298     '
3299 FEnd
3300 '
3301 '■fScrewTighenRoboCheck
3302 '<summary>
3303 'ねじロボ監視
3304 '</summary>
3305 '<param name = "MStopNum%"> 停止番号</param>
3306 '<returns>整数 0:ねじロボ異常終了 1:OK </returns>
3307 '<make>
3308 '2021/12/2 中村天哉
3309 '</make>
3310 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3311     fnAutoScreenComment(503)    '状態表示[ねじロボ動作終了待ち] 2022/04/26 渡辺
3312     fScrewTighenRoboCheck = 1
3313     MScrewTighenRoboFlg% = 1    'フラグの初期化
3314     MCheck% = 0
3315     While MScrewTighenRoboFlg% = 1
3316         MCheck% = M_In16(11904)
3317         If M_In(MStopNum%) = 1 Then '停止位置まで来たら
3318             MScrewTighenRoboFlg% = 0 '関数を抜ける
3319             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
3320         EndIf
3321         If MCheck% <> 0 Then
3322             fScrewTighenRoboError(MCheck%)
3323             Select M_20#
3324                 Case MAbout%            '停止が押された場合
3325                     M_Out(12869) = 1 Dly 1.0
3326                     MScrewTighenRoboFlg% = 0
3327                     fScrewTighenRoboCheck = 0   '異常終了
3328                     Break
3329                 Case MNgProcess%        'NGが押された場合
3330                     M_Out(12873) = 1 Dly 1.0
3331                     MScrewTighenRoboFlg% = 0
3332                     fScrewTighenRoboCheck = 0   '異常終了
3333                     Break
3334                 Case MContinue%             'リトライが押された場合
3335                     M_20# = MClear%         'M_20#初期化
3336                     M_Out(12871) = 1 Dly 1.0
3337                     Break
3338                 Case MNext%                 '次へが押された場合
3339                     M_20# = MClear%         'M_20#初期化
3340                     M_Out(12874) = 1 Dly 1.0
3341                     Break
3342             End Select
3343             Dly 0.5
3344         EndIf
3345     WEnd
3346 FEnd
3347 '
3348 '■fScrewTighenRoboError
3349 '<summary>
3350 'ねじロボエラー処理
3351 '</summary>
3352 '<param name = "ErrorCode%"> エラー番号</param>
3353 '<make>
3354 '2021/12/2 中村天哉
3355 '</make>
3356 Function fScrewTighenRoboError(ByVal MErrorCode%)
3357     MErrorScreenCode% = 0
3358     MErrorScreenCode% = MErrorCode% + 300
3359     fErrorProcess(11,MErrorScreenCode%,0,0)
3360 FEnd
3361 '
3362 '■fErrorProcess
3363 '<summary>
3364 'エラー処理
3365 '</summary>
3366 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
3367 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
3368 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
3369 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
3370 '<make>
3371 '2021/11/5 中村天哉
3372 '</make>
3373 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3374     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
3375     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
3376     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
3377     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
3378 *RETRY_ERR_PROCESS
3379      M_20# = MClear%     '初期化
3380 '        'エラー処理記述
3381         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3382 '        'GOT KEY入力待ち
3383         MKeyNumber = fnKEY_WAIT()
3384 '        '
3385         If MKeyNumber = MAbout% Then   '停止を選択した場合
3386             M_20# = MAbout%            'M_20# プログラム間共通外部変数
3387             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3388             Break
3389          '
3390         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
3391             M_20# = MContinue%            'M_20# プログラム間共通外部変数
3392             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3393         '
3394         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
3395             M_20# = MNext%            'M_20# プログラム間共通外部変数
3396             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3397          '
3398         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
3399             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
3400             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3401             Break
3402         '
3403         EndIf
3404         '
3405         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3406 FEnd
3407 '
3408 '■fnTorqueCheck
3409 ''' <summary>
3410 ''' トルクチェック動作用のメイン
3411 ''' </summary>
3412 ''' <remarks>
3413 ''' Date   : 2021/12/21 : H.AJI
3414 ''' </remarks>'
3415 Function M% fnTorqueCheck
3416     'トルクチェック中送信  搬送系停止
3417     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
3418     '
3419     fnTorqueCheck = 0
3420     Ovrd 20
3421     Mov PInitialPosition              '初期位置移動
3422     Ovrd 100
3423     '下記キー待ちの継続に反応させないため
3424     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3425     Dly 0.2
3426     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3427     '
3428     'M6340  トルクチェック受信
3429     'Dly 5.0
3430     M_Out(12340) = 1          'トルクチェック受信 M6340
3431     Dly 1.0
3432     M_Out(12340) = 0
3433     '
3434     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
3435     M_Out(12835) = 1                         'ねじロボトルクチェック画面切替
3436    Wait M_In(11843) = 1                         'ねじロボトルクチェック画面切替
3437     M_Out(12835) = 0                         'ねじロボトルクチェック画面切替完了
3438     '
3439     '
3440     MLoopFlg = 1
3441     While MLoopFlg = 1
3442         '
3443         Mov PInitialPosition              '初期位置移動
3444         '
3445         MKeyNumber = fnKEY_WAIT()
3446         Select MKeyNumber
3447             Case Is = 1           '停止
3448                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
3449                 Dly 1.0
3450                 M_Out(12343) = 0
3451                 Ovrd 20
3452                 'Mov PTicketRead_1
3453                 M_Out(12840) = 1          'トルクチェック終了
3454                 Wait M_In(11859) = 1      'ねじロボからの終了
3455                 M_Out(12840) = 0          'トルクチェック終了
3456                 Ovrd 100
3457                 M_20# = 1
3458                 MLoopFlg = -1
3459                 Break
3460             Case Is = 2           '次へ
3461                 Break
3462             Case Is = 3           '継続
3463                 Break
3464             Case Is = 4           'トルクチェック開始
3465                 M_Out(12342) = 1          'トルクチェック開始要求受信 M6342
3466                 Dly 1.0
3467                 M_Out(12342) = 0
3468                 If M_In(11862) = 1 Then             'トルクチェッカー確認
3469                     fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
3470                     MRet = fnScrewMTorque()           'ねじロボ用トルクチェック
3471                 EndIf
3472                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
3473                 'MRet = fnMoveTorquePosi()
3474                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
3475                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3476                 Break
3477             Default
3478                 Break
3479         End Select
3480     WEnd
3481     '
3482     'トルクチェック中停止送信
3483     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
3484     '
3485     'ロボットの位置を元に戻す
3486     '
3487     '
3488  FEnd
3489  '
3490 '
3491 '
3492 '---------------------------
3493 '
3494 '    メイン画面の表示、非表示設定
3495 '         コメントD1001, D1002, D1003の設定
3496 '           MWindReSet = 0     画面非表示
3497 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
3498 '           MWindErrScr = 10    エラー画面 D1001, D1002
3499 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
3500 '
3501 '---------------------------
3502 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3503     fnMainScreenOpen = 0
3504     '
3505    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
3506         M_Out16(12480) = MCommentD1001            'D1001 コメント
3507     EndIf
3508     '
3509     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
3510         M_Out16(12496) = MCommentD1002            'D1002 コメント
3511     EndIf
3512     '
3513     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
3514         M_Out16(12512) = MCommentD1003            'D1003 コメント
3515     EndIf
3516     '
3517     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
3518     M_Out(12362) = 1                         'ウィンド画面設定  M6362
3519     Dly 0.5
3520     M_Out(12362) = 0                         'ウィンド画面設定
3521 FEnd
3522 '
3523 '■Main
3524 ''' <summary>
3525 ''' トルクチェック実動作
3526 ''' </summary>
3527 ''' <remarks>
3528 ''' Date   : 2021/12/21 : H.AJI
3529 ''' </remarks>'
3530 Function M% fnScrewMTorque
3531     fnScrewMTorque = 0
3532     M_Out(12838) = 1                         'トルクチェック開始1
3533     Wait M_In(11857) = 1                     '受信完了
3534     M_Out(12838) = 0                         'トルクチェック開始1
3535     Dly 2.0
3536 FEnd
3537 '
3538 '■Main
3539 ''' <summary>
3540 ''' トルクチェック実動作
3541 ''' </summary>
3542 ''' <remarks>
3543 ''' Date   : 2021/12/21 : H.AJI
3544 ''' </remarks>'
3545 Function M% fnMoveTorquePosi
3546      fnMoveTorquePosi = 0
3547      Ovrd 50
3548     'Mov PTorquePosi000 'トルクチェック回避位置へ移動
3549      Mov PTorqueCheck_1 'トルクチェックメーター上空へ移動
3550     'Mov PTorquePosi020 'トルクチェックビットジョイント上空
3551     '
3552     '
3553      '以下は阿部さんが作成したトルクチェックプラグラム
3554     '
3555     Spd M_NSpd
3556 '-------------      ドライバーRST
3557     M_Out(12240)=0     'ドライバーOFF CCW
3558     M_Out(12241)=0     'ドライバーOFF CW
3559     M_Out(12242)=0     'ドライバー解除 C1
3560     M_Out(12243)=0     'ドライバー解除 C2
3561     M_Out(12245)=0     'プログラム解除 F1/プログラム2
3562 '---------------------------------------
3563 '---------------------------------------
3564     Fsc Off            '力覚センサ　Off  STEP1は不要
3565 '--------------------------------------------------------------
3566 '--------------------------------------------------------------
3567 '[P-11]
3568 '--------------------------------------------------------------   【トルクチェック 0.4N - P11】
3569     Mov PTorqueCheck, -50                     ' トルク-1　置き位置上空 50mm へ移動
3570    'Mov PTorquePosi020, -10                    ' トルク-1　置き位置上空 10mm へ移動
3571     Dly 0.1
3572 '-----------------------
3573    'Cnt 0                           'Cnt動作-2　終了
3574 '-----------------------
3575     Mov PTorqueCheck , -5                      'トルク-1　置き位置上空 5mm へ移動
3576     Dly 0.2
3577 '-----------------------
3578     M_Out(12242)=1                   'ドライバーセット C1
3579     Dly 0.1
3580     M_Out(12243)=1                   'ドライバーセット C2 (バンク3)
3581     Dly 0.1
3582     M_Out(12245)=1                   'プログラム2セット F1  Mネジ
3583     Dly 0.1
3584     'M_Out(12241)=1                   'ドライバーON  CW
3585    M_Out(12241)=0                   'ドライバーOFF  CW
3586     'Dly 0.1
3587 '--------------------------------
3588     Ovrd 40
3589    'Dly 0.1
3590 '--------------------------------  ネジ締め速度設定
3591     Spd 14                            'ライド 100-40 100% :Spd 12
3592     Dly 0.1
3593 '--------------------------------
3594 '--------------------------------
3595 '---------------------------------【ねじ締め動作】
3596 '
3597     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '移動中エラー検出
3598    Mvs PTorqueCheck               'トルクチェック位置へ移動
3599     Dly 0.3                          '動作安定待ち
3600    M_Out(12241)=1                   'ドライバーON  CW
3601 '
3602     Wait M_In(11584)=1                '完了/エラー検出
3603     Dly 0.1
3604     Spd M_NSpd
3605    'Ovrd 20
3606     If M_In(11256)=1 Then *LBL1       'ネジトータルエラー検出
3607     Wait M_In(11257)=1                'ネジ完了SC
3608 '---------------------------------
3609     Dly 0.1
3610     M_Out(12241)=0                    'ドライバーOFF CW
3611     Dly 0.1
3612     M_Out(12242)=0                    'ドライバー解除 C1
3613     Dly 0.1
3614     M_Out(12243)=0                    'ドライバー解除 C2 (バンク3)
3615     Dly 0.1
3616     M_Out(12245)=0                    'プログラム2解除 F1
3617 '--------------------------------------------------------------   【トルクチェック 0.4N - P11ここまで】
3618 '
3619     Mvs PTorqueCheck,-60                       'あえてmov から変更
3620     Dly 0.1
3621 '--------------------------------------------------------------
3622    'Ovrd 80
3623 '--------------------------------------------------------------
3624 '---------------------------------------
3625 '---------------------------------------
3626 '---------------------------------------エラー離脱処理
3627    *LBL1
3628    Fsc Off            '力覚センサ　Off   *STEP1は不要
3629    Mvs ,-100
3630    M_Out(12241)=0     'ドライバーOFF CW
3631    Dly 0.1
3632    M_Out(12242)=0     'ドライバー解除 C1
3633    Dly 0.1
3634    M_Out(12243)=0     'ドライバー解除 C2 (バンク3)
3635    Dly 0.1
3636    M_Out(12245)=0     'プログラム解除 F1
3637 '---------------------------------------
3638 '---------------------------------------
3639 '-------------
3640    'Mov PInitPos19049
3641    Dly 0.1
3642 '
3643 '
3644 '
3645 '
3646 FEnd
3647 '
3648 '
3649 '----------------------------------------------------------------
3650 'fTimeOutJudge
3651 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3652 '引数
3653 'Address% = 監視アドレス番号
3654 'JudgeFlg% = 対象アドレスの正常終了時の値
3655 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3656 '戻り値 = 0 エラー
3657 '         1 正常終了
3658 '         2 リトライ
3659 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3660 '作成日
3661 '2022/9/20 中村
3662 '----------------------------------------------------------------
3663 '
3664 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3665     fTimeOutJudge = 0
3666     MJudge% = 1
3667     MRtn = 0
3668     M_20# = MClear%
3669     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3670 *TimeOutLoop
3671     If MRtn = 1 Then GoTo *TimeOut
3672         fErrorProcess(11,202,203,0)
3673         If M_20# = MNext% Then GoTo *TimeOutLoop
3674         If M_20# = MContinue% Then MJudge% = 2
3675         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3676 *TimeOut
3677     fTimeOutJudge = MJudge%
3678 '
3679 *JUDGE_ERROR_END
3680 FEnd
3681 '■Main
3682 ''' <summary>
3683 ''' 組立動作用のメイン
3684 ''' </summary>
3685 ''' <remarks>
3686 ''' Date   : 2021/07/07 : M.Hayakawa
3687 ''' </remarks>'
3688 Function Main
3689     MopeNo = M_21#         '外部変数にて動作番号代入
3690     '
3691     If M_Svo=0 Then
3692         Servo On
3693     EndIf
3694     Wait M_Svo=1
3695 '組立スタート日付時刻要求パルスON
3696     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3697 'パトライト操作
3698     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
3699     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
3700     '
3701     M_20# = 0                                   'KEY入力初期化
3702     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
3703     MRet% = 0
3704 '初期位置の確認と移動
3705 '
3706 '復帰動作　実行・未実行判別      2022/04/08 渡辺 作成
3707     PActive = P_Curr                    '現在位置を取得
3708     MRecoveryPass% = 0
3709     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3710         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3711             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3712                 MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
3713             EndIf
3714         EndIf
3715     EndIf
3716     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3717         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3718             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3719                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
3720             EndIf
3721         EndIf
3722     EndIf
3723     If MRecoveryPass% = 0 Then
3724        fnInitialZoneB()        '復帰動作パスフラグが立っていない時は復帰動作を実行
3725     EndIf
3726 '
3727 '
3728 '    MRet% = fnRoboPosChk()
3729 '    If MRet% = 1 Then                           '初期位置の動作を行った場合    '2022/04/26 コメントアウト 渡辺
3730 '        fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  'ウィンド画面
3731 '        MKeyNumber% = fnKEY_WAIT()
3732 '        Select MKeyNumber%
3733 '            Case Is = MAbout%       '停止
3734 '                M_20# = MAbout%
3735 '                MLoopFlg% = -1
3736 '                Break
3737 '            Case Is = MNext%        '次へ
3738 '                'MLoopFlg = -1
3739 '                Break
3740 '            Case Is = MContinue%    '継続
3741 '                M_20# = MContinue%
3742 '                MLoopFlg% = -1
3743 '                Break
3744 '            Default
3745 '                Break
3746 '        End Select
3747 '    EndIf
3748     '
3749     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
3750         M_Out(12364) = 1            'toPLC_データ保存ON
3751 'トルクチェック
3752         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3753             MRet% = fnTorqueCheck()
3754             Break
3755         Else
3756 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
3757 '                MRtn = InspInit()               '画像処理初期化処理
3758 '            EndIf
3759             '
3760            M_20# = MClear%                    '初期化
3761 '組立開始
3762             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3763                 fnAssyStart()
3764             Else
3765                 M_20# = MPass%
3766             EndIf
3767 '組立終了日付時刻
3768             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
3769             Wait M_In(11572) = 1            '日付取得完了
3770             Dly 0.1
3771             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
3772 'リフターユニットへのOUT
3773             '  KEY入力が何もない場合 OKと判断
3774             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
3775             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
3776 'OK/NGフラグ出力
3777             If M_20# <= 0 Then
3778                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
3779             ElseIf M_20# = MPass% Then
3780                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
3781             EndIf
3782 'PIASに組立完了書込み
3783             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3784                 If M_20# = MPass% Then
3785                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3786                 Else
3787                     'KEY入力がNGの場合
3788                     If M_20# = MNgProcess% Then
3789                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3790                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3791                         MRet% = fnPiasWrite(MNG%)
3792                        nAssyNgQty = nAssyNgQty + 1
3793                     EndIf
3794                     '
3795                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/17中村)
3796                     If M_20# = MAssyOK% Then
3797                             '-----------------------
3798                             'D732 -> D2600 コピー要求
3799                             M_Out(12566) = 1
3800 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3801                             M_Out(12566) = 0
3802                             '
3803                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3804                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3805                             '基板番号照合(PPは未使用）
3806 '                            MRet% = fnPCBNumberCheck()
3807                         Else
3808                             MRet% = 1
3809                         EndIf
3810                         '
3811                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3812                             If M_20# <> MAbout% Then
3813                                 '工程履歴OK書き込み
3814                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3815                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3816                                 MRet% = fnPiasWrite(MOK%)
3817                                 nAssyOkQty = 0
3818                                 nAssyOkQty = nAssyOkQty + 1
3819                             Else
3820                                 nAssyOkQty = nAssyOkQty + 1
3821                             EndIf
3822                         EndIf
3823                     EndIf
3824 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3825 '                    MRet% = fnPiasWrite(MOK%)
3826                 EndIf
3827             Else
3828                 nAssyOkQty = nAssyOkQty + 1
3829             EndIf
3830             '
3831             '組立終了日付時刻解除
3832             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3833             '投入数、組立OK数、組立NG数書込み
3834 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3835             '
3836 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3837 '                '画像処理終了処理
3838 '                MRtn = InspQuit()
3839 '            EndIf
3840         EndIf
3841         M_Out(12364) = 0                          'toPLC_データ保存OFF
3842     EndIf
3843 'パトライト操作
3844     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3845     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3846 'GOT表示
3847     fnAutoScreenComment(93)  'AUTO画面 工程完了
3848 FEnd
3849 End
3850 '
3851 'おまじないコメント
3852 '絶対削除するな
3853 '
3854 '
3855 '
3856 '
3857 '
3858 '
3859 '
3860 '
3861 '
PInspPosition(1)=(-74.49,+309.56,+598.75,+180.00,-54.15,+90.00,+0.00,+0.00)(7,1048576)
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
PTemp=(+602.00,-150.00,+550.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
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
PActive=(+602.00,-150.00,+550.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
Pmove=(+467.95,+105.02,+640.00,+179.82,+0.33,+179.27,+0.00,+0.00)(7,0)
PInitialPosition=(+340.00,+0.00,+580.00,-180.00,+0.00,+180.00)(7,0)
PMechaGet=(-415.66,-8.74,+299.32,+179.45,-2.22,+176.86)(7,1048577)
PMechaGet_1=(-415.66,-8.74,+409.96,+179.45,-2.22,+176.86)(7,1048577)
PMechaGet_2=(-189.84,-0.01,+629.06,-180.00,+0.00,-179.99)(7,1)
PMechaGet_3=(+0.01,+189.84,+629.07,-180.00,+0.00,+90.00)(7,0)
PMechaGet_4=(+327.50,+0.02,+596.24,-179.99,+0.00,+103.50)(7,0)
PMechaGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaSet1=(+159.80,-334.91,+320.00,+122.02,+86.64,+31.39)(6,0)
PMechaSet1_1=(+159.80,-334.91,+340.00,+122.02,+86.64,+31.39)(6,0)
PMechaSet2=(+160.19,-334.97,+320.18,+122.02,+86.64,+31.36)(6,0)
PMechaSet2_1=(+160.19,-334.97,+339.97,+122.02,+86.64,+31.36)(6,0)
PMechaSet_2=(+162.58,-305.37,+557.38,+179.47,+90.00,+89.47)(6,0)
PMechaSet_3=(+114.45,-288.22,+565.58,+180.00,+0.00,+112.11)(7,0)
PMechaSet_4=(+310.11,-0.04,+565.56,+180.00,+0.00,-179.55)(7,0)
PMechaSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck=(-74.49,+309.56,+598.75,+180.00,-54.15,+90.00)(7,1048576)
PPlateBackCheck_2=(-59.41,+338.49,+632.86,+170.41,-66.66,+89.13)(7,1048576)
PPlateBackCheck_3=(-17.86,+286.22,+630.90,-179.69,-0.41,+90.87)(7,1048576)
PPlateBackCheck_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet=(+467.96,+104.98,+401.49,+179.92,+0.33,+179.27)(7,0)
PPlateBackGet_1=(+467.96,+104.98,+430.00,+179.92,+0.33,+179.27)(7,0)
PPlateBackGet_2=(+467.96,+104.98,+560.00,+179.82,+0.33,+179.27)(7,0)
PPlateBackGet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackPush=(-17.11,+414.76,+540.50,-180.00,+0.00,+88.64)(7,1048576)
PPlateBackPush_1=(-17.11,+414.76,+540.50,-180.00,+0.00,+88.64)(7,1048576)
PPlateBackPush_2=(-17.11,+402.30,+542.92,-179.41,-2.61,+88.95)(7,1048576)
PPlateBackSet=(-17.11,+487.50,+542.92,-179.41,-2.61,+88.95)(7,1048576)
PPlateBackSet_1=(-17.02,+432.03,+532.24,-179.18,-17.77,+89.04)(7,1048576)
PPlateBackSet_10=(-18.77,+363.66,+487.69,-179.91,-41.20,+90.53)(7,1048576)
PPlateBackSet_11=(-18.77,+362.28,+493.38,+179.84,-39.34,+90.58)(7,1048576)
PPlateBackSet_12=(-17.86,+286.22,+630.90,-179.69,-0.41,+90.87)(7,1048576)
PPlateBackSet_2=(-17.65,+400.77,+518.67,-179.12,-27.43,+88.88)(7,1048576)
PPlateBackSet_3=(-18.04,+373.86,+498.91,-179.02,-37.53,+88.69)(7,1048576)
PPlateBackSet_4=(-18.09,+364.27,+491.14,-178.97,-41.13,+88.61)(7,1048576)
PPlateBackSet_5=(-18.67,+359.99,+487.41,-178.97,-41.13,+88.61)(7,1048576)
PPlateBackSet_6=(-18.67,+359.99,+490.17,-178.97,-41.13,+88.61)(7,1048576)
PPlateBackSet_7=(-18.83,+378.77,+503.51,+179.34,-35.63,+90.89)(7,1048576)
PPlateBackSet_8=(-18.81,+374.94,+499.62,+179.31,-36.77,+90.90)(7,1048576)
PPlateBackSet_9=(-18.81,+372.36,+496.87,+179.85,-37.92,+90.58)(7,1048576)
PProductOnPltGet=(+478.76,-97.71,+371.78,+179.90,-0.17,+179.39)(7,0)
PProductOnPltGet_1=(+478.76,-97.71,+410.00,+179.90,-0.17,-179.10)(7,0)
PProductOnPltGet_2=(+478.76,-97.71,+500.00,+179.90,-0.17,-179.10)(7,0)
PProductOnPltGet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet=(+475.92,-96.64,+372.22,+179.83,+0.19,+178.68)(7,0)
PProductOnPltSet_1=(+475.92,-96.64,+410.00,+179.83,+0.19,+178.68)(7,0)
PProductOnPltSet_2=(+475.92,-96.64,+500.00,+179.83,+0.19,+178.68)(7,0)
PProductOnPltSet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboGet=(-19.74,+404.10,+319.10,+85.48,+89.14,+175.44)(6,0)
PProductOnRoboGet_1=(-19.74,+404.10,+395.77,+85.48,+89.14,+175.44)(6,0)
PProductOnRoboGet_2=(-19.74,+396.28,+425.48,-105.62,+89.69,-15.62)(6,0)
PProductOnRoboGet_3=(-19.74,+378.13,+425.48,-105.62,+89.69,-15.62)(6,0)
PProductOnRoboGet_4=(-19.74,+300.00,+550.00,+175.04,+89.99,-94.95)(6,0)
PProductOnRoboGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboSet=(-18.93,+403.63,+319.61,+63.21,+89.37,+153.36)(6,0)
PProductOnRoboSet_1=(-18.93,+403.63,+395.81,+63.21,+89.37,+153.36)(6,0)
PProductOnRoboSet_2=(-18.93,+396.29,+419.81,+84.41,+89.14,+174.55)(6,0)
PProductOnRoboSet_3=(-18.91,+236.65,+555.85,-94.47,+89.06,-4.46)(6,0)
PProductOnRoboSet_4=(-18.86,+404.69,+360.00,-102.74,+89.08,-12.36)(6,0)
PProductOnRoboSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPushTilt=(-213.93,+562.96,+465.17,+179.97,-0.02,+4.01)(7,0)
PPushTilt_1=(-213.93,+562.97,+480.78,+179.96,-0.02,+4.01)(7,0)
PPushTilt_2=(-213.93,+562.96,+620.00,+179.97,-0.02,+4.01)(7,0)
PPushTilt_3=(+0.02,+340.00,+610.00,-180.00,-0.01,-91.91)(7,0)
PTicketRead=(+602.00,-150.00,+500.00,+180.00,+0.00,+90.00)(7,0)
PTicketRead_1=(+602.00,-150.00,+550.00,+180.00,+0.00,+90.00)(7,0)
PTicketRead_2=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
JActive=(+12.63,+18.11,+79.30,+0.10,+82.23,+13.34,+0.00,+0.00)
Jmove=(+12.63,-46.87,+111.64,+0.00,+80.58,+13.34,+0.00,+0.00)
JTaihi=(+0.00,-46.87,+111.64,+0.00,+80.58,+0.00)
