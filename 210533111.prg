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
833     Mov PPlateBackSet_13        '背面板置き上空
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
859     Mov PPlateBackSet_12        '爪入れ前回避点
860     Cnt 0
861     Ovrd 25
862     Accel 25 , 25
863     Mvs PPlateBackSet_11        '爪入れ込み前
864     Mvs PPlateBackSet_10        '爪入れ込み1
865     Mvs PPlateBackSet_9         '爪入れ込み2
866 '    CmpG 1.0,1.0,1.0,1.0,1.0,1.0,,
867 '    Cmp Pos, &B001000
868     Cnt 1           '0.1→0.2mm近傍に変更(221219中村)
869     Mov PPlateBackSet_8         '経路1
870     Mov PPlateBackSet_7         '経路2
871     Mov PPlateBackSet_6         '経路3
872     Mov PPlateBackSet_5         '経路4
873     Mov PPlateBackSet_4         '経路5
874     Mov PPlateBackSet_3         '経路6-
875     Mov PPlateBackSet_2         '経路7
876     Mov PPlateBackSet_1         '経路8
877     Mov PPlateBackSet           '背面板離し位置
878 '    Cmp Off
879     Accel 100 , 100
880     Cnt 0
881     Dly 0.1
882     *RE_PLATE_SET
883     M_Out(12256) = 0            '本体チャック閉OFF
884     M_Out(12257) = 1            '本体チャック開ON
885     '
886 '    Wait M_In(11265)            '本体チャック開検出
887     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
888     If MRtn = 1 Then GoTo *CompPlateSet
889     fErrorProcess(11,244,284,0)
890     If M_20# = MNext% Then M_20# = MClear%
891     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
892     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
893     If M_20# = MContinue% Then GoTo *RE_PLATE_SET
894     *CompPlateSet
895     '
896 '-----暫定押し-------------------------------------(22/12/14中村)ここから
897 *RE_BUCK_PUSH
898     M_20# = MClear%
899     Mov PPlateBackPush_2
900 '
901     M_Out(12257) = 0            '本体チャック開OFF
902     M_Out(12256) = 1            '本体チャック閉ON
903 '
904     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
905 '
906     If MRtn = 1 Then GoTo *CompBuckPushSetting  '正常なら押し動作へ
907 '
908     fErrorProcess(11,245,287,0) '閉端センサーNG時エラー表示
909         If M_20# = MNext% Then M_20# = MClear%              '次へが押されたら値を初期化
910         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '停止が押されたらエラーエンドへ
911         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END    'NGが押されたらエラーエンドへ
912         If M_20# = MContinue% Then GoTo *RE_BUCK_PUSH       'リトライが押されたらもう一度閉じる
913 '
914 *CompBuckPushSetting
915 '
916     Mvs PPlateBackPush_1
917     Ovrd 10
918     Mvs PPlateBackPush
919 '    Dly 0.1     'クランプをするので削除(221219中村)
920 '背面クランプここから(12/15)
921     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～停止4)
922     MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
923         If MRtn = 0 Then
924             Mvs PPlateBackPush_1
925             Mov PPlateBackSet_13
926             Mov PInitialPosition    '"イニシャルに戻る動き"
927         EndIf
928         If MRtn = 0 Then GoTo *ASSY_ERROR_END
929 '背面クランプここまで(12/15)
930     Ovrd 50                     '20→50に変更(221219中村)
931     Mvs PPlateBackPush_1
932 *RE_CHUCK_OPEN
933     M_20# = MClear%
934     M_Out(12256) = 0            '本体チャック閉OFF
935     M_Out(12257) = 1            '本体チャック開ON
936     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
937     If MRtn = 1 Then GoTo *CompChuckOpenForBackPush
938     fErrorProcess(11,244,284,0)
939         If M_20# = MNext% Then M_20# = MClear%              '次へが押されたら値を初期化
940         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '停止が押されたらエラーエンドへ
941         If M_20# = MNgProcess% Then GoTo *RE_CHUCK_OPEN    'NGが押されたらエラーエンドへ
942         If M_20# = MContinue% Then GoTo *RE_CHUCK_OPEN       'リトライが押されたらもう一度閉じる
943 *CompChuckOpenForBackPush
944 '-----暫定押し-------------------------------------(22/12/14中村)ここまで
945 '
946     ColChk On
947     Mov PPlateBackSet_13        '背面板置き上空
948 '    M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～停止4)
949     Ovrd 100
950     'ねじロボ製品クランプ固定待ち(コメントアウト221215中村)
951 '    Wait M_In(11891) = 1        'ねじロボ2停止4受信
952 'MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
953 'If MRtn = 0 Then Mov PInitialPosition    '"イニシャルに戻る動き"
954 'If MRtn = 0 Then GoTo *ASSY_ERROR_END
955     '
956     'ねじロボ引き込み待ち
957 '    M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～完了)   '
958     '置き位置画像検査
959 '    Mov PPlateBackCheck_3       '画像検査位置上空
960 '    Mov PPlateBackCheck_2       '通過点
961     If M_In(11369) = 0 Then GoTo *CompCheck
962     Mvs PPlateBackCheck         '確認位置
963     '
964 *RE_CHECK
965     PInspPosition(1) = PPlateBackCheck
966     MInspGroup%(1) = 2
967     MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,1,-1,1)
968     If M_In(11374) = 0 Then MRtn = 1
969     If MRtn = 1 Then GoTo *CompCheck
970     fErrorProcess(11,43,23,0)
971     If M_20# = MNext% Then M_20# = MClear%
972     If M_20# = MAbout% Or M_20# = MNgProcess% Then
973         Mov PPlateBackSet_12
974         Mov PInitialPosition
975     EndIf
976     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
977     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
978     If M_20# = MContinue% Then GoTo *RE_CHECK
979 *CompCheck
980 '
981     Mov PPlateBackSet_13        '背面板置き上空
982 ''    ' 部品供給要求送信
983 '    M_Out(12787) = 1
984 '    Mov PPlateBackCheck_2       '通過点
985 '    Mov PPlateBackCheck_3       '画像検査位置上空
986     '
987     'ねじロボ引き込み待ち
988     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～完了)
989 '
990     M_Out(12258) = 0            'DVDメカチャック閉OFF
991     M_Out(12259) = 1            'DVDメカチャック開ON
992     '
993     'DVDメカを取る
994     Mvs PMechaGet_3             '経路1
995     Mvs PMechaGet_2             'DVDメカ受け取り上空回避点
996 '    Wait M_In(11272)            '部品供給機Ready
997 '    MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '部品供給機Ready
998 '    If MRtn = 0 Then
999 '        fErrorProcess()         'エラー処理
1000 '    EndIf
1001 '
1002   ' 部品供給要求送信
1003     fnAutoScreenComment(513)    '状態表示[部品供給待ち] 2022/04/26 渡辺
1004 '    M_Out(12787) = 1
1005     '    ' 部品供給完了待ち(処理変更2/27中村)
1006 *RE_FEEDER_READY
1007 '    Wait M_In(11810) = 1
1008 MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '供給待ち
1009 If MRtn = 1 Then GoTo *CompFeederReady
1010 '   ' 部品供給要求終了
1011 M_Out(12787) = 0
1012 fErrorProcess(11,289,290,0)                '284→290に変更6/7中村
1013 If M_20# = MNext% Then M_20# = MClear%
1014 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1015     Mov PMechaGet_2
1016     Mov PMechaGet_3
1017     Mov PMechaGet_4
1018     Mov PInitialPosition
1019 EndIf
1020 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1021 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1022     ' 部品供給要求
1023 M_Out(12787) = 1
1024 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
1025 *CompFeederReady
1026 '    ' 部品供給要求終了
1027     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1028     M_Out(12787) = 0
1029 '
1030     Mvs PMechaGet_1             'DVDメカ受け取り上空
1031     '
1032     *RE_MECHA_GET_1
1033     '
1034     M_Out(12258) = 0            'DVDメカチャック閉OFF
1035     M_Out(12259) = 1            'DVDメカチャック開ON
1036     '
1037 '    Wait M_In(11268) = 1        'DVDメカチャック開検出
1038     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1039     If MRtn = 1 Then GoTo *CompMechaGet1
1040     Mvs PMechaGet_2
1041     Mvs PMechaGet_3
1042     Mov PMechaGet_4
1043     fErrorProcess(11,270,284,0)
1044     If M_20# = MNext% Then M_20# = MClear%
1045     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1046     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1047     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1048     Mov PMechaGet_3
1049     Mvs PMechaGet_2
1050     Mvs PMechaGet_1
1051     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_1
1052     *CompMechaGet1
1053     '
1054     M_Out(12261) = 0            'DVDメカシリンダー戻OFF
1055     M_Out(12260) = 1            'DVDメカシリンダー出ON
1056 '    Wait M_In(11271) = 1        'DVDメカシリンダー出検出
1057     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVDメカシリンダー出検出
1058     If MRtn = 1 Then GoTo *CompMechaGet2
1059     Mvs PMechaGet_2
1060     Mvs PMechaGet_3
1061     Mov PMechaGet_4
1062     fErrorProcess(11,271,284,0)
1063     If M_20# = MNext% Then M_20# = MClear%
1064     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1065     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1066     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1067     Mov PMechaGet_3
1068     Mvs PMechaGet_2
1069     Mvs PMechaGet_1
1070     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_1
1071     *CompMechaGet2
1072     '
1073     Ovrd 25
1074     Mvs PMechaGet               'DVDメカ受け取り位置
1075     Dly 0.1                     '位置出し　(22/12/09中村)
1076 '
1077     MRtn = 0
1078     MRtn2 = 0
1079     *RE_MECHA_GET_2
1080     If M_20# = MContinue% Then M_20# = MClear%
1081     M_Out(12259) = 0            'DVDメカチャック開OfF
1082     M_Out(12258) = 1            'DVDメカチャック閉ON
1083     '
1084 '    Wait M_In(11269) = 1        'DVDメカチャック閉検出
1085     If MRtn = 1 Then Dly 1.0
1086     If MRtn = 1 Then GoTo *CompMechaGet3
1087     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
1088     If M_20# = MNext% Then GoTo *CompMechaGet3
1089     If MRtn = 1 Then GoTo *CompMechaGet3
1090     M_Out(12258) = 0            'DVDメカチャック閉OfF
1091     M_Out(12259) = 1            'DVDメカチャック開ON
1092     Dly 2.0
1093     Mvs PMechaGet_1
1094     Mvs PMechaGet_2
1095     M_Out(12259) = 0            'DVDメカチャック開OfF
1096     M_Out(12258) = 1            'DVDメカチャック閉ON
1097     Mvs PMechaGet_3
1098     Mov PMechaGet_4
1099     fErrorProcess(11,269,284,0)
1100     If M_20# = MNext% Then
1101         M_20# = MClear%
1102         MRtn = 1
1103     EndIf
1104     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1105     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1106     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1107     M_Out(12258) = 0            'DVDメカチャック閉OfF
1108     M_Out(12259) = 1            'DVDメカチャック開ON
1109     Mov PMechaGet_3
1110     Mvs PMechaGet_2
1111     Mvs PMechaGet_1
1112     Mvs PMechaGet
1113     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1114     *CompMechaGet3
1115     M_20# = MClear%
1116     '
1117 '    Wait M_In(11267) = 1        'DVDメカ検出
1118     If MRtn2 = 1 Then GoTo *CompMechaGet4
1119     MRtn2 = frInCheck(11267,1,MSETTIMEOUT05&)   'DVDメカ検出
1120     If MRtn2 = 1 Then GoTo *CompMechaGet4
1121     M_Out(12258) = 0            'DVDメカチャック閉OfF
1122     M_Out(12259) = 1            'DVDメカチャック開ON
1123     Dly 2.0
1124     Mvs PMechaGet_1
1125     Mvs PMechaGet_2
1126     M_Out(12259) = 0            'DVDメカチャック開OfF
1127     M_Out(12258) = 1            'DVDメカチャック閉ON
1128     Mvs PMechaGet_3
1129     Mov PMechaGet_4
1130     fErrorProcess(11,273,284,0)
1131     If M_20# = MNext% Then
1132         M_20# = MClear%
1133         MRtn2 = 1
1134     EndIf
1135     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1136     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1137     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1138     M_Out(12258) = 0            'DVDメカチャック閉OfF
1139     M_Out(12259) = 1            'DVDメカチャック開ON
1140     Mov PMechaGet_3
1141     Mvs PMechaGet_2
1142     Mvs PMechaGet_1
1143     Mvs PMechaGet
1144     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1145     *CompMechaGet4
1146     M_20# = MClear%
1147     Dly 0.5
1148     '
1149     Mvs PMechaGet_1             'DVDメカ受け取り上空
1150 '    *RE_MECHA_GET_3
1151     M_Out(12260) = 0            'DVDメカシリンダー出OFF
1152     M_Out(12261) = 1            'DVDメカシリンダー戻ON
1153 '    Wait M_In(11270) = 1        'DVDメカシリンダー戻検出
1154     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVDメカシリンダー戻検出
1155 '    If MRtn = 1 Then GoTo *CompMechaGet5       '処理位置変更2/11中村
1156 '    fErrorProcess(11,272,284,0)
1157 '    If M_20# = MNext% Then M_20# = MClear%
1158 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1159 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1160 '    If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1161 '    *CompMechaGet5
1162     '
1163     If MRtn = 1 Then Ovrd 50    '念のためオーバーライド変更100→50(22/12/09中村)
1164     Mvs PMechaGet_2             'DVDメカ受け取り上空回避点
1165 '    ' 部品供給要求終了
1166     M_Out(12787) = 0
1167 '    ' 部品取得完了送信(パルス)
1168     M_Out(12800) = 1 Dly 0.5
1169     Mvs PMechaGet_3             '経路1
1170     Mov PMechaGet_4             '経路2
1171 '
1172     *RE_MECHA_GET_3
1173     M_Out(12260) = 0            'DVDメカシリンダー出OFF
1174     M_Out(12261) = 1            'DVDメカシリンダー戻ON
1175     If MRtn = 1 Then GoTo *CompMechaGet5
1176     fErrorProcess(11,272,284,0)
1177     If M_20# = MNext% Then M_20# = MClear%
1178     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1179     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1180     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1181     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1182     *CompMechaGet5
1183     '
1184     'DVDメカを仮置き台へ置く
1185 '    Wait M_In(11920) = 0             'BaseUnit6側仮置き台フラグ確認(処理位置変更2/11中村)
1186 '
1187 '   仮置き台が回転中か確認
1188     *Loop_CW_CCW_1
1189     If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_1 Else GoTo *Loop_CW_CCW_1
1190     *Next_CW_CCW_1
1191     If M_In(11920) = 0 Then GoTo *OK_FLG_1 Else GoTo *Loop_CW_CCW_1           'BaseUnit6側仮置き台フラグ確認
1192     *OK_FLG_1
1193 '
1194     M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1195     '
1196     MRtn = 1
1197     'DVDメカが仮置き台に置かれていないかの確認(追加ここから10/1中村)
1198     If M_In(11931) = 1 Then          '回転方向の確認(CW方向)
1199         If M_In(11928) = 0 Then      'BaseUnit5側にDVDメカが置かれていた場合
1200             M_Out(12912) = 0         '仮置き台フラグ回収(回転可能状態にするため)
1201             Wait M_In(11930) = 1     '仮置き台回転待ち
1202             'If M_In(11929) = 0 Then  '回転後にまだBaseUnit5側に置かれている場合
1203                 'エラー処理(BaseUnit6側にて正常な動作がされていない)
1204             'Endif
1205         EndIf
1206     ElseIf M_In(11930) = 1 Then      '回転方向の確認(CCW方向)
1207         If M_In(11929) = 0 Then      'BaseUnit5側にDVDメカが置かれていた場合
1208             M_Out(12912) = 0         '仮置き台フラグ回収(回転可能状態にするため)
1209             Wait M_In(11931) = 1     '仮置き台回転待ち
1210             MRtn = 0
1211         EndIf
1212     Else
1213         MRtn = 0'エラー処理(仮置き台が正常な動作をしていない)
1214     EndIf
1215     If MRtn = 0 Then GoTo *Loop_CW_CCW_1
1216 '
1217 *Loop_CW_CCW_S
1218     fnAutoScreenComment(530)    '状態表示[工程６の動作終了待ち] 2022/04/26 渡辺
1219 'Ver 0.4 追加 -----------------------
1220 '自工程が12912 = 1 を出力した後、再度 工程6側の動作中監視を行う
1221     MRtn = 0
1222     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec 工程6のフラグON監視
1223     If MRtn = 1 Then M_Out(12912) = 0                  '仮置き台フラグ回収 工程6優先のため12912=0を出力
1224     If MRtn = 1 Then Dly 0.7
1225     If MRtn = 1 Then GoTo *Loop_CW_CCW_S        '工程6の動作中フラグがONしていたら再度ループ
1226     M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1227 'Ver 0.4 ここまで -------------------
1228 '
1229     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1230     Mov PMechaSet_3             'DVDメカ仮置き回避点1(処理位置変更2/27中村)
1231 '
1232 *Loop_CW_CCW_2
1233 'Ver 0.4 追加 -----------------------
1234     '自工程が12912 = 1 を出力した後、再度 工程6側の動作中監視を行う
1235     MRtn = 0
1236     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec 工程6のフラグON監視
1237     If MRtn = 1 Then M_Out(12912) = 0                  '仮置き台フラグ回収 工程6優先のため12912=0を出力
1238     If MRtn = 1 Then Dly 0.7
1239     If MRtn = 1 Then GoTo *Loop_CW_CCW_2        '工程6の動作中フラグがONしていたら再度ループ
1240     M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1241 'Ver 0.4 ここまで -------------------
1242 '
1243     Mov PMechaSet_2             'DVDメカ仮置き回避点2(処理位置変更2/27中村)
1244 '
1245 '    *Loop_CW_CCW_2  '仮置き台の状態をもう一度確認する(コメントアウト2/27中村)
1246 '    If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_2 Else GoTo *Loop_CW_CCW_2
1247 '    *Next_CW_CCW_2
1248 '    If M_In(11920) = 0 Then GoTo *OK_FLG_2 Else GoTo *Loop_CW_CCW_2           'BaseUnit6側仮置き台フラグ確認
1249 '    *OK_FLG_2
1250 '
1251 '    M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1252 '
1253     '
1254     *RE_MECHA_SET_1
1255     If M_20# = MContinue% Then M_20# = MClear%
1256     Ovrd 25
1257     M_Out(12261) = 0            'DVDメカシリンダー戻OFF
1258     M_Out(12260) = 1            'DVDメカシリンダー出ON
1259 '    Wait M_In(11271) = 1        'DVDメカシリンダー出検出
1260     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVDメカシリンダー出検出
1261     If MRtn = 1 Then GoTo *CompMechaSet1
1262     Mov PMechaSet_3
1263     Mov PMechaGet_4
1264     fErrorProcess(11,271,284,0)
1265     If M_20# = MNext% Then M_20# = MClear%
1266     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1267     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1268     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1269     Mov PMechaSet_3
1270     Mov PMechaSet_2
1271     Ovrd 100
1272     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_1
1273     *CompMechaSet1
1274     '
1275     *RE_MECHA_SET_12
1276     Fine 0.05 , P
1277 '    Wait M_In(11920) = 0        'BaseUnit6側仮置き台フラグ確認
1278 '    M_Out(12912) = 1            '仮置き台フラグ立て
1279     If M_In(11931) = 1 Then     '回転方向の確認(CW方向)(追加ここまで10/1中村)
1280         Mov PMechaSet1_1        'DVDメカ仮置き上空1
1281         Ovrd 10
1282         Mvs PMechaSet1          'DVDメカ仮置き位置1
1283         Dly 0.1
1284         M_Out(12258) = 0        'DVDメカチャック閉OFF
1285         M_Out(12259) = 1        'DVDメカチャック開ON
1286 '        Wait M_In(11268) = 1    'DVDメカチャック開検出
1287         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1288         Mvs PMechaSet1_1        'DVDメカ仮置き上空1
1289     ElseIf M_In(11930) = 1 Then '回転方向の確認(CCW方向)(追加ここから10/1中村)
1290         Mov PMechaSet2_1        'DVDメカ仮置き上空
1291         Ovrd 10
1292         Mvs PMechaSet2          'DVDメカ仮置き位置2
1293         Dly 0.1
1294         M_Out(12258) = 0        'DVDメカチャック閉OFF
1295         M_Out(12259) = 1        'DVDメカチャック開ON
1296 '        Wait M_In(11268) = 1    'DVDメカチャック開検出
1297         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1298         Mvs PMechaSet2_1        'DVDメカ仮置き上空2
1299     'Else
1300         'エラー文(仮置き台が正常な位置に無い)
1301     EndIf                       '追加ここまで10/1中村
1302     Fine 0 , P
1303     '
1304     If MRtn = 1 Then GoTo *CompMechaSet2
1305     fErrorProcess(11,270,284,0)
1306     If M_20# = MNext% Then M_20# = MClear%
1307     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1308     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1309     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_12
1310     *CompMechaSet2
1311     '
1312     Ovrd 100
1313     *RE_MECHA_SET_2
1314     M_Out(12260) = 0            'DVDメカシリンダー出OFF
1315     M_Out(12261) = 1            'DVDメカシリンダー戻ON
1316 '    Wait M_In(11270) = 1        'DVDメカシリンダー戻検出
1317     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVDメカシリンダー戻検出
1318     If MRtn = 1 Then GoTo *CompMechaSet3
1319     fErrorProcess(11,272,284,0)
1320     If M_20# = MNext% Then M_20# = MClear%
1321     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1322     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1323     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_2
1324     *CompMechaSet3
1325     '
1326     Mov PMechaSet_2             'DVDメカ仮置き回避点2
1327     M_Out(12912) = 0                  '仮置き台フラグ回収(追加10/1中村)
1328     '
1329     'ねじロボ2の製品を取る
1330     Mov PProductOnRoboGet_4     '経路3から4へ
1331     M_Out(12259) = 0            'DVDメカチャック開OFF
1332     M_Out(12258) = 1            'DVDメカチャック閉ON
1333 '    Wait M_In(11876) = 1        'ねじロボ2完了待機を受信
1334 MRtn = fScrewTighenRoboCheck(11876)    '停止状態を受信する
1335 If MRtn = 0 Then Mov PInitialPosition   '"イニシャルに戻る動き"
1336 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1337 '
1338     *RE_ROBO_GET_1
1339 '
1340     M_Out(12259) = 0            'DVDメカチャック開OFF
1341     M_Out(12258) = 1            'DVDメカチャック閉ON
1342     If M_20# = MContinue% Then Dly 0.5
1343 '
1344 '    Wait M_In(11269) = 1        'DVDメカチャック閉検出
1345     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
1346     If MRtn = 1 Then GoTo *CompRoboGet1
1347     fErrorProcess(11,269,284,0)
1348     If M_20# = MNext% Then M_20# = MClear%
1349     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1350     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1351     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1352     If M_20# = MContinue% Then GoTo *RE_ROBO_GET_1
1353     *CompRoboGet1
1354     '
1355     Ovrd 50
1356     Mov PProductOnRoboGet_3     'ねじロボ製品取り上空回避点2から3へ
1357     Ovrd 20
1358     Mvs PProductOnRoboGet_2     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)4から2へ
1359     Mvs PProductOnRoboGet_1     'ねじロボ製品取り上空
1360     Ovrd 10
1361     Mvs PProductOnRoboGet       'ねじロボ製品取り位置
1362     Dly 0.3
1363 '
1364     *RE_ROBO_GET_2
1365 '
1366     M_Out(12257) = 0            '本体チャック開OFF
1367     M_Out(12256) = 1            '本体チャック閉ON
1368 '
1369 '    Wait M_In(11266) = 1        '本体チャック閉検出
1370     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
1371     If MRtn = 1 Or M_20# = MNext% Then GoTo *CompRoboGet2
1372     M_Out(12256) = 0            '本体チャック閉OFF
1373     M_Out(12257) = 1            '本体チャック開ON
1374     Dly 2.0
1375     Mvs PProductOnRoboGet_1
1376     Mvs PProductOnRoboGet_2
1377     Mov PProductOnRoboGet_3
1378     Mov PProductOnRoboGet_4
1379     Mov PInitialPosition
1380     M_Out(12257) = 0            '本体チャック開OFF
1381     M_Out(12256) = 1            '本体チャック閉ON
1382     Dly 1.0
1383     fErrorProcess(11,245,284,0)
1384     If M_20# = MNext% Then MRtn = 1
1385     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1386     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1387     M_Out(12256) = 0            '本体チャック閉OFF
1388     M_Out(12257) = 1            '本体チャック開ON
1389     Dly 2.0
1390     Mov PProductOnRoboGet_4
1391     Mov PProductOnRoboGet_3
1392     Mov PProductOnRoboGet_2
1393     Mvs PProductOnRoboGet_1
1394     Mvs PProductOnRoboGet
1395     If M_20# = MContinue% Or M_20# = MNext% Then GoTo *RE_ROBO_GET_2
1396     *CompRoboGet2
1397     M_20# = MClear%
1398     '
1399     Accel 30 , 100
1400     Dly 0.3
1401     Mvs PProductOnRoboGet_1     'ねじロボ製品取り上空
1402     Ovrd 25
1403     Mvs PProductOnRoboGet_2     'ねじロボ製品取り上空回避点(9/27暫定コメントアウト)12/15コメント解除
1404     Mvs PProductOnRoboGet_3     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)4から3へ
1405     Ovrd 50                     'オーバーライド変更(22/12/09中村)
1406     Mov PProductOnRoboGet_4     '経路3から4へ
1407     Accel 50 , 50               '加速度変更(22/12/09中村)
1408 '
1409     M_Out(12868) = 1 Dly 0.5    'ねじロボ2動作完了を送信
1410 '    *RE_ROBO_GET_3                                     '処理位置変更2/11中村
1411 '    M_Out(12258) = 0            'DVDメカチャック閉OFF
1412 '    M_Out(12259) = 1            'DVDメカチャック開ON
1413 '    Wait M_In(11268) = 1        'DVDメカチャック開検出
1414 '    MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1415 '    If MRtn = 1 Then GoTo *CompRoboGet3
1416 '    fErrorProcess(11,270,284,0)
1417 '    If M_20# = MNext% Then M_20# = MClear%
1418 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1419 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1420 '    If M_20# = MContinue% Then GoTo *RE_ROBO_GET_3
1421 '    *CompRoboGet3
1422     '
1423     'パレットへ製品を置く
1424     Mov PProductOnPltSet_2      '本体置き位置上空回避点
1425     Accel 100 , 100             '加速度変更(22/12/09中村)
1426     Mov PProductOnPltSet_1      '本体置き位置上空
1427     Ovrd 10
1428     Mvs PProductOnPltSet        '本体置き位置
1429     Dly 0.3
1430 '
1431     *RE_PLT_SET
1432 '
1433     M_Out(12256) = 0            '本体チャック閉OFF
1434     M_Out(12257) = 1            '本体チャック開ON
1435 '
1436 '    Wait M_In(11265) = 1        '本体チャック開検出
1437     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
1438     If MRtn = 1 Then GoTo *CompPltSet
1439     fErrorProcess(11,244,284,0)
1440     If M_20# = MNext% Then M_20# = MClear%
1441     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1442         Mvs PProductOnPltSet_1
1443         Mov PProductOnPltSet_2
1444         Mov PInitialPosition
1445     EndIf
1446     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1447     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1448     If M_20# = MContinue% Then GoTo *RE_PLT_SET
1449     *CompPltSet
1450 '
1451     Mvs PProductOnPltSet_1      '本体置き位置上空
1452     Ovrd 100
1453     Mov PProductOnPltSet_2      '本体置き位置上空回避点
1454 '    Mov PInitialPosition        'イニシャルポジション
1455     MRtn = FnCtlValue2(2)       '組立ＯＫ＋１  2022/04/28 渡辺
1456     Mov PTicketRead_1           'チケットID読み取り回避点
1457     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
1458     *RE_FIN_INI
1459     M_Out(12258) = 0            'DVDメカチャック閉OFF
1460     M_Out(12259) = 1            'DVDメカチャック開ON
1461 '    Wait M_In(11268) = 1        'DVDメカチャック開検出
1462     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1463     If MRtn = 1 Then GoTo *CompFinIni
1464     fErrorProcess(11,270,284,0)         'リトライとNG時にリトライをかける
1465     If M_20# = MNext% Then M_20# = MClear%
1466     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1467     If M_20# = MNgProcess% Then GoTo *RE_FIN_INI
1468     If M_20# = MContinue% Then GoTo *RE_FIN_INI
1469     *CompFinIni
1470     '
1471     'チケットID書き込み
1472     M_20# = MAssyOK%
1473     *ASSY_ERROR_END
1474     *AssyEnd
1475     *fnAssyStart_FEndPosi
1476 FEnd
1477 '
1478 '■fnPiasCheck
1479 ''' <summary>
1480 ''' PIASチケット読込み
1481 ''' </summary>
1482 ''' <returns>   0 : NG
1483 '''             1 : OK(読込み完了)
1484 ''' </returns>
1485 ''' <remarks>
1486 ''' Date   : 2021/07/07 : M.Hayakawa
1487 ''' </remarks>'
1488 Function M% fnPiasCheck
1489     fnPiasCheck = 0
1490     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1491     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1492 '
1493 *RETRY_PIAS
1494     M_20# = MClear%
1495     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1496     '
1497     '【IDチケット読み込み】
1498     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1499     MInspGroup%(1) = 1              '検査G番号
1500     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1501 '
1502     'エラーの場合
1503     If MRtn <> 1 Then
1504         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1505         If MRtn <> 1 Then
1506             'D720 -> D1300 コピー要求
1507             M_Out(12565) = 1
1508             Dly 0.5
1509             M_Out(12565) = 0
1510             'エラー処理記述
1511             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1512             'GOT KEY入力待ち
1513             MKeyNumber = fnKEY_WAIT()
1514             '
1515             Select MKeyNumber
1516                 Case MNext%         '次へを選択した場合
1517                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1518                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1519                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1520                     Break
1521                 Case MAbout%        '停止を選択した場合
1522                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1523                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1524                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1525                     Break
1526                 Case MNgProcess%    'NGを選択した場合
1527                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1528                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1529                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1530                     Break
1531                 Case MContinue%     '継続を選択した場合
1532                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1533                     M_20# = MContinue%
1534                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1535                     Break
1536             End Select
1537         EndIf
1538     EndIf
1539 '----------D720 -> D1300 コピー要求----------
1540     M_Out(12565) = 1
1541     Dly 0.5
1542     M_Out(12565) = 0
1543 '----------通信確認をする----------
1544     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1545     MRtn = 0                ' 初期化
1546     M_20# = MClear%         ' 初期化
1547     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1548     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1549     If MRtn <> 1 Then
1550         If M_20# = MContinue% Then
1551             GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1552         Else
1553             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1554         EndIf
1555     EndIf
1556 '----------工程抜け確認----------
1557     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1558     MRtn = 0                ' 初期化
1559     M_20# = MClear%         ' 初期化
1560     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1561     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1562     If MRtn <> 1 Then
1563         If M_20# = MContinue% Then
1564             GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1565         Else
1566             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1567         EndIf
1568     EndIf
1569     '
1570     fnPiasCheck = 1
1571     *fnPiasCheck_End
1572 FEnd
1573 '
1574 '■fnPCComuCheck
1575 ''' <summary>
1576 ''' PC-PLC通信チェック
1577 ''' </summary>
1578 ''' <returns>   0 : NG
1579 '''             1 : OK(読込み完了)
1580 ''' </returns>
1581 ''' <remarks>
1582 ''' Date   : 2021/07/07 : M.Hayakawa
1583 ''' </remarks>'
1584 Function M% fnPCComuCheck
1585     fnPCComuCheck = 0
1586     MJudge% = 0                                  '初期化
1587     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1588     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1589     '
1590     For MStaNo = 0 To 5
1591         '
1592         If M_In(MIN_PIAS_ComOK%) = 1 Then
1593             'PC通信OK(M400)
1594             MJudge% = MOK%
1595             MStaNo = 5
1596             Break
1597         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1598             'toRBT_通信確認time out
1599             MJudge% = MNG%
1600             MCommentD1001 = 15
1601             MCommentD1002 = 21
1602             MStaNo = 5
1603             Break
1604         Else
1605             'toRBT_通信確認time out
1606             MJudge% = MNG%
1607             MCommentD1001 = 14
1608             MCommentD1002 = 21
1609             Break
1610         EndIf
1611     Next MStaNo
1612     '
1613     '上記で返信フラグを受信してからPC通信確認OFF
1614     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1615     '
1616     'エラー画面
1617     If MJudge% <> MOK% Then
1618         M_20# = MClear%     '初期化
1619         'エラー処理記述
1620         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1621         'GOT KEY入力待ち
1622         MKeyNumber = fnKEY_WAIT()
1623         '
1624         If MKeyNumber = MAbout% Then            '停止を選択した場合
1625             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1626             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1627             Break
1628         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1629             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1630             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1631             Break
1632         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1633             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1634             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1635             Break
1636         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1637             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1638             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1639             Break
1640         EndIf
1641     Else
1642         'OKの場合
1643         fnPCComuCheck = 1
1644     EndIf
1645 FEnd
1646 '
1647 '■fnProcessCheck
1648 ''' <summary>
1649 ''' 工程抜け確認
1650 ''' </summary>
1651 ''' <returns>    1：工程履歴OK     0：異常終了
1652 '''             -1：前工程履歴NG  -2：自工程履歴あり
1653 '''             -3：モデル仕向NG  -4：タイムアウト
1654 '''             -5：履歴処理エラー
1655 ''' </returns>
1656 ''' <remarks>
1657 ''' Date   : 2021/07/07 : M.Hayakawa
1658 ''' </remarks>'
1659 Function M% fnProcessCheck
1660     fnProcessCheck = 0
1661     MJudge% = MNG%      '一旦NGを初期化とする
1662 '----------工程抜け確認----------
1663     MCommentD1001 = 0   'コメント初期化
1664     For MStaNo = 0 To 5
1665         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1666         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1667         '
1668         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1669             MJudge% = MOK%
1670             fnAutoScreenComment(85)     ' AUTO画面
1671             MStaNo = 5
1672             Break
1673         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1674             MFlgLoop% = 0
1675             MJudge% = MNG%
1676             MCommentD1001 = 27
1677             MCommentD1002 = 22
1678             fnAutoScreenComment(94)     ' AUTO画面
1679             fnProcessCheck = -2         ' NGは-2を返す
1680             MStaNo = 5
1681             Break
1682         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1683            MJudge% = MNG%
1684             MCommentD1001 = 31
1685             MCommentD1002 = 22
1686             fnAutoScreenComment(83)     ' AUTO画面
1687             fnProcessCheck = -3         ' NGは-3を返す
1688             MStaNo = 5
1689             Break
1690         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1691             '履歴NGは直ぐに終了せず繰り返し確認を行う
1692             '前工程の書込みが終了していない可能性があるため
1693             MJudge% = MNG%
1694             MCommentD1001 = 32
1695             MCommentD1002 = 22
1696             fnAutoScreenComment(84)     ' AUTO画面
1697             fnProcessCheck = -1         ' NGは-1を返す
1698             Dly 1.0
1699             '工程抜け確認OFF
1700             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1701             Dly 1.0
1702            'MStaNo = 5
1703             Break
1704         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1705             MFlgLoop% = 0
1706             MJudge% = MNG%
1707             MCommentD1001 = 29
1708             MCommentD1002 = 22
1709             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1710             fnProcessCheck = -5         ' NGは-5を返す
1711             MStaNo = 5
1712             Break
1713         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1714             MJudge% = MNG%
1715             If MCommentD1001 = 32 Then
1716                 '何もしない
1717             Else
1718                 MCommentD1001 = 26
1719             EndIf
1720             MCommentD1002 = 22
1721             fnProcessCheck = -4         ' NGは-4を返す
1722             MStaNo = 5
1723             Break
1724         Else
1725             MJudge% = MNG%
1726             MCommentD1001 = 28
1727             MCommentD1002 = 22
1728         EndIf
1729     Next MStaNo
1730     '工程抜け確認OFF
1731     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1732     '通過履歴NG 工程抜けの場合
1733     If MJudge% = MPass% Then
1734         M_20# = MPass%
1735     EndIf
1736     '
1737     'エラー画面
1738     If MJudge% <> MOK% Then
1739         M_20# = MClear%     '初期化
1740         'エラー処理記述
1741         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1742         'GOT KEY入力待ち
1743         MKeyNumber = fnKEY_WAIT()
1744         '
1745         Select MKeyNumber
1746             Case MAbout%        '停止を選択した場合
1747                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1748                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1749                 Break
1750             Case MNext%         '次へを選択した場合
1751                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1752                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1753                 Break
1754             Case MContinue%     '継続を選択した場合
1755                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1756                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1757                 Break
1758             Case MNgProcess%    'NGを選択した場合
1759                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1760                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1761                 Break
1762         End Select
1763     Else
1764         fnProcessCheck = 1  ' OKは1を返す
1765     EndIf
1766 FEnd
1767 '
1768 '■fnPiasWrite
1769 ''' <summary>
1770 ''' Pias 組立結果書込み要求
1771 ''' </summary>
1772 '''<param name="MFlg%">
1773 '''                 MOK%(1) = 工程履歴にOKを書込む
1774 '''                 MNG%(0) = 工程履歴にNGを書込む
1775 '''</param>
1776 '''<returns></returns>
1777 ''' <remarks>
1778 ''' Date   : 2021/07/07 : M.Hayakawa
1779 ''' </remarks>'
1780 Function M% fnPiasWrite(ByVal MFlg%)
1781       fnPiasWrite = 0
1782 *RETRY_PIASWRITE
1783     '
1784     '組立OK(MOK%)の場合　M306 ON
1785    '組立NG(MNG%)の場合　M307 ON
1786     If MFlg% = MOK% Then
1787         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1788     Else
1789         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1790     EndIf
1791     Dly 0.1                  '念のため
1792     '
1793     'Piasへ書込み開始 M305 -> ON
1794     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1795     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1796     '
1797     MJudge% = MNG%
1798     '
1799     For MStaNo = 0 To 5
1800         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1801             MJudge% = MOK%
1802             'MRet = fnAutoScreenComment(85)  'AUTO画面
1803             MStaNo = 5
1804             Break
1805         '
1806         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1807             MJudge% = MNG%
1808             'MRet = fnAutoScreenComment(85)  'AUTO画面
1809            MCommentD1001 = 34
1810            MCommentD1002 = 25
1811             MStaNo = 5
1812             Break
1813         '
1814         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1815             MJudge% = MNG%
1816             'MRet = fnAutoScreenComment(85)  'AUTO画面
1817            MCommentD1001 = 35
1818            MCommentD1002 = 25
1819             MStaNo = 5
1820             Break
1821         '
1822         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1823             MJudge% = MNG%
1824             'MRet = fnAutoScreenComment(85)  'AUTO画面
1825            MCommentD1001 = 36
1826            MCommentD1002 = 25
1827             MStaNo = 5
1828             Break
1829         '
1830         Else
1831             MJudge% = MNG%
1832            MCommentD1001 = 42
1833            MCommentD1002 = 25
1834         '
1835         EndIf
1836         '
1837     Next MStaNo
1838     '
1839     'Piasへ書込み開始 M305 -> OfF
1840     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1841     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1842     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1843     '
1844     '
1845     '通過履歴NG 工程抜けの場合
1846     If MJudge% = MPass% Then
1847         M_20# = MPass%
1848     EndIf
1849     '
1850    M_20# = MClear%     '初期化
1851     '
1852     'エラー画面
1853     If MJudge% < MOK% Then
1854     '
1855 '残しておくが現状では使用しないラベル
1856 *RETRY_ERR_WRITE
1857         M_20# = MClear%     '初期化
1858         'エラー処理記述
1859         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1860         'GOT KEY入力待ち
1861         MKeyNumber = fnKEY_WAIT()
1862         '
1863         If MKeyNumber = MAbout% Then   '停止を選択した場合
1864             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1865            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1866             Break
1867         '
1868         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1869             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1870             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1871         '
1872         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1873             M_20# = MPass%            'M_20# プログラム間共通外部変数
1874             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1875         '
1876         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1877             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1878            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1879             Break
1880         '
1881         EndIf
1882         '
1883         If M_20# = MClear% Then *RETRY_ERR_WRITE
1884         '
1885     EndIf
1886     '
1887     If M_20# = MContinue% Then *RETRY_PIASWRITE
1888     '
1889     fnPiasWrite = 1
1890     '
1891 FEnd
1892 '
1893 '■fnPCBNumberCheck
1894 ''' <summary>
1895 ''' Pias 基板番号照合要求
1896 ''' </summary>
1897 '''<param name="%"></param>
1898 '''<param name="%"></param>
1899 '''<returns></returns>
1900 ''' <remarks>
1901 ''' Date   : 2021/07/07 : M.Hayakawa
1902 ''' </remarks>'
1903 Function M% fnPCBNumberCheck
1904       fnPCBNumberCheck = 0
1905     '
1906 *RETRY_PCBCHECK
1907     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1908     'Piasへ基板照合開始 M310 -> ON
1909     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1910     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1911     '
1912     MJudge% = MNG%
1913     '
1914     For MStaNo = 0 To 5
1915         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1916             MJudge% = MOK%
1917             fnAutoScreenComment(96)  'AUTO画面
1918             MStaNo = 5
1919             Break
1920         '
1921         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1922             MJudge% = MNG%
1923             fnAutoScreenComment(97)  'AUTO画面
1924             MCommentD1001 = 37
1925             MCommentD1002 = 25
1926             MStaNo = 5
1927             Break
1928         '
1929         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1930             MJudge% = MNG%
1931             fnAutoScreenComment(98)  'AUTO画面
1932             MCommentD1001 = 38
1933             MCommentD1002 = 25
1934             MStaNo = 5
1935             Break
1936         '
1937         ElseIf M_In(11580) = 1 Then                         'time out
1938             MJudge% = MNG%
1939             fnAutoScreenComment(99)  'AUTO画面
1940             MCommentD1001 = 39
1941             MCommentD1002 = 25
1942             MStaNo = 5
1943             Break
1944         '
1945         Else
1946             MJudge% = MNG%
1947            MCommentD1001 = 41
1948            MCommentD1002 = 25
1949         '
1950         EndIf
1951         '
1952     Next MStaNo
1953     '
1954     'Piasへ基板照合開始 M310 -> OfF
1955     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1956     '
1957     '
1958     '通過履歴NG 工程抜けの場合
1959     If MJudge% = MPass% Then
1960         M_20# = MPass%
1961     EndIf
1962     '
1963    M_20# = MClear%     '初期化
1964     '
1965     'エラー画面
1966     If MJudge% < MOK% Then
1967     '
1968 '残しておくが現状では使用しないラベル
1969 *RETRY_ERR_PCBNUMBER
1970         M_20# = MClear%     '初期化
1971         'エラー処理記述
1972         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1973         'GOT KEY入力待ち
1974         MKeyNumber = fnKEY_WAIT()
1975         '
1976         If MKeyNumber = MAbout% Then   '停止を選択した場合
1977             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1978             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1979             Break
1980         '
1981         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1982             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1983             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1984         '
1985         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1986             M_20# = MPass%            'M_20# プログラム間共通外部変数
1987             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1988         '
1989         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1990             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1991             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1992             Break
1993         '
1994         EndIf
1995         '
1996         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1997         '
1998     EndIf
1999     '
2000     If M_20# = MContinue% Then *RETRY_PCBCHECK
2001 FEnd
2002 '
2003 '■ScrewTight_S2
2004 ''' <summary>
2005 ''' ねじ締めを行う
2006 ''' </summary>
2007 '''<param name="PScrewPos()">
2008 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
2009 '''             PScrewPos(2)    ：ねじ締め回避点
2010 '''             PScrewPos(10)   ：ねじ締め終了高さ
2011 '''</param>
2012 '''<returns>整数
2013 '''         0=異常終了、1=正常終了
2014 '''</returns>
2015 ''' <remarks>
2016 ''' Date   : 2021/07/07 : M.Hayakawa
2017 ''' </remarks>'
2018 Function M% ScrewTight_S2(ByVal PScrewPosition())   'ネジ締め個別設定
2019     ScrewTight_S2 = 0
2020     MOKNGFlg = 0
2021     Ovrd 100
2022     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
2023     ' 暫定
2024     Ovrd 5
2025     Mvs PScrewPosition(10),-10    ' パレット上ねじ締めS①の上空へ移動
2026 '    Ovrd MOvrdA
2027     '暫定マスク
2028 '    M_Out(Y62_Driver)=1     ' バンクセッティング　C1
2029 '    Dly 0.1
2030 '    M_Out(Y61_Driver)=1     'ドライバーON　CW
2031 '    'Spd 8.3 '外部ライド100  内部ライド60   'ライド100-40　100%：Spd　15　'ねじ締め速度設定
2032 '    Spd MSpdA               'ネジ締め時Spd個別設定
2033     ' 暫定移動のみ
2034     Mvs PScrewPosition(10)
2035 '    '
2036 '    Dly 0.1
2037 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
2038 '    Wait M_In(11584)=1          '完了/エラー検出
2039 '    Dly 0.1
2040 '    Spd M_NSpd
2041 '    '
2042 '    If M_In(X28_Driver)=1 Then  'ねじトータルエラー検出時
2043 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
2044 '        Dly 0.1
2045 '        M_Out(Y62_Driver)=0     'バンクセッティング解除　C1
2046 '        Dly 0.1
2047 '        M_Out(Y63_Driver)=0     'バンクセッティング解除　C1
2048 '        Dly 0.1
2049 '        M_Out(Y65_Driver)=0     'プログラム解除　F1
2050 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
2051 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
2052 '        MOKNGFlg = -1
2053 '        ScrewTight_S2 = 0
2054 '    Else
2055 '        Wait M_In(X29_Driver)=1 ' 正常完了時
2056 '        Dly 0.1
2057 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
2058 '        Dly 0.1
2059 '        M_Out(Y62_Driver)=0     'バンクセッティング解除
2060 '        Dly 0.1
2061 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
2062 '        Dly 0.1
2063 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
2064 '        ScrewTight_S2 = 1
2065 '    EndIf
2066 ' 暫定
2067     Ovrd 10
2068     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
2069     Ovrd 100
2070 FEnd
2071 '
2072 '■ScrewGet_S3
2073 ''' <summary>
2074 ''' ねじ供給機からねじを得る
2075 ''' </summary>
2076 '''<param name="%"></param>
2077 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
2078 '''         PScrewPos(2)    ：ねじ供給器回避点
2079 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2080 '''         PScrewPos(3)    ：Mねじポカヨケ位置
2081 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
2082 '''<returns>整数
2083 '''         0=異常終了、1=正常終了、-1=MネジセンサーNG、-2=MネジセンサーON、-3=吸着エラー
2084 '''</returns>
2085 ''' <remarks>
2086 ''' Date   : 2021/07/07 : M.Hayakawa
2087 ''' </remarks>'
2088 Function M% ScrewGet_S3(ByVal PScrewPosition())
2089     ScrewGet_S3 = 0
2090     MMScrewJudge% = 0
2091     'ねじ供給器初期動作エラーチェック
2092 ' ↓暫定削除
2093 '    Wait M_In(X34_ScrewReady1)=1 'ねじ供給器SがReadyになるまで待つ　←　何秒か待ってReadyにならなければ抜けるプログラムが必要？
2094 '    Ovrd 100
2095 '    If M_In(X33_SS2)=0 Then  'Mねじ検出センサがOFF（故障）していた場合
2096 '        Ovrd 30
2097 '        Mvs,-80             'その場所から80mm上空へ移動
2098 '        Mov PInitPos19049   '19049初期位置へ移動
2099 '        M_Out(Y6A_VV1)=0    'ねじ吸着 Off
2100 '        'NGとしてここの関数から抜ける
2101 '        ScrewGet_S3 = -1
2102 '        MMScrewJudge% = 1
2103 '        MCommentD1001 = 61
2104 '    EndIf
2105 '    If ScrewGet_S3 = 0 Then
2106 '        'Sタイト用ねじ供給機にMねじが混入していないか監視
2107 '        MMScrewJudge% = 0 'MMScrewJudgeを初期化する
2108 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
2109 '        If MRtn = 0 Then
2110 '            Ovrd 30
2111 '            Mvs,-80            'その場所から50mm上空へ移動
2112 '            Mov PInitPos19049  '19049初期位置へ移動
2113 '            MMScrewJudge% = 2
2114 '            MRtn = All_CLamp_Release()'全てのクランプ解除へ分岐
2115 '            MCnt% = 2   '2を設定
2116 '            MCommentD1001 = 62
2117 '        EndIf
2118 '        If MMScrewJudge% = 2 Then
2119 '            ScrewGet_S3 = -2
2120 '        EndIf
2121 '    EndIf
2122 '    'Mネジ判定がONの場合 NGとして関数を抜ける
2123 '    If MMScrewJudge% = 2 Then
2124 '        ScrewGet_S3 = -2
2125 '    EndIf
2126     'Sネジ用ねじ太郎のMネジ混入確認用ここまで
2127     Ovrd 100
2128     Spd M_NSpd
2129     If MMScrewJudge% = 0 Then
2130         ScrewGet_S3 = 0
2131         M_Out(Y63_Driver)=1         ' バンクセッティング　C2
2132         MScrewCnt% = 0
2133         MFinCnt% = 2
2134 '        For MCnt% = 0 To MFinCnt%
2135             Mov PScrewPosition(2)        ' ねじ供給機回避点
2136             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
2137             Ovrd 80
2138             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2139             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
2140             Mvs PScrewPosition(10), 1.2
2141             M_Out(Y6A_VV1)=1        ' ねじ吸着　ON
2142             'ビット回転
2143             M_Out(Y60_Driver)=1
2144             Dly 0.2
2145             '
2146             Ovrd 100
2147             JOvrd M_NJovrd
2148             Spd M_NSpd
2149             'ネジ吸着確認位置移動
2150             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
2151             Mvs PScrewPosition(10), -15  ' ネジ吸着確認位置
2152             'ビット回転停止
2153             'M_Out(Y60_Driver)=0
2154             '
2155             '1秒間ネジ吸着確認
2156 ' 以下暫定削除
2157 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2158 '            'MRtn = 0'強制エラー
2159 '            '吸着エラーの場合
2160 '            'ネジをねじ太郎に戻す
2161 '            If MRtn = 0 Then
2162 '                Ovrd 30
2163 '                'ビット回転停止
2164 '                M_Out(Y60_Driver)=0
2165 '                'ネジ供給機上空
2166 '                Mvs PScrewPos(1)
2167 '                '更に上空
2168 '                Mov PScrewPos(1), -75
2169 '                'ネジ捨て位置
2170 '                Mov PScrewFeedS021
2171 '                '吸着OFF
2172 '                M_Out(Y6A_VV1)=0 'ねじ吸着　OFF
2173 '                Dly 0.2
2174 '                '破壊ON
2175 '                M_Out(Y6B_VB1)=1 '真空破壊ON
2176 '                'ビット回転
2177 '                M_Out(Y61_Driver)=1
2178 '                Dly 0.5
2179 '                '
2180 '                Ovrd 100
2181 '                JOvrd M_NJovrd
2182 '                Spd M_NSpd
2183 '                'ドライバーを上下させねじを振り落とす
2184 '                Mov PScrewFeedS021, 10
2185 '                Mov PScrewFeedS021
2186 '                Dly 0.1
2187 '                Mov PScrewFeedS021, 10
2188 '                Mov PScrewFeedS021
2189 '                '
2190 '                'ネジ落ち待ち
2191 '                'ビット回転停止
2192 '                M_Out(Y61_Driver)=0
2193 '                Dly 0.1
2194 '                '破壊OFF
2195 '                M_Out(Y6B_VB1)=0 '真空破壊OFF
2196 '                '
2197 '                '
2198 '                'ねじ落ちたとして、移動更に上空
2199 '                Mov PScrewPos(1), -75
2200 '                Ovrd 100
2201 '                Spd M_NSpd
2202 '                'ネジ供給機上空
2203 '                Mvs PScrewPos(1)
2204 '                '
2205 '                ScrewGet_S3 = -3
2206 '                Break
2207 '                '
2208 '            Else
2209 '                MCnt% = MFinCnt%
2210 '                ScrewGet_S3 = 0
2211 '            EndIf
2212 '        Next  MCnt%
2213         '
2214         Ovrd 100
2215         Spd M_NSpd
2216         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2217         M_Out(Y60_Driver)=0     ' ビット回転停止
2218         M_Out(Y63_Driver)=0     ' バンクセッティング　C2
2219         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2220         'もう一度吸着確認
2221 ' 以下暫定削除
2222 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2223 '        If MRtn = 0 Then      '吸着エラーの場合
2224 '            MCommentD1001 = 94
2225 '            MCommentD1002 = 95
2226 '            ScrewGet_S3 = -3
2227 '        EndIf
2228 '        If MRtn = 1 Then      '吸着OKの場合
2229 '            ScrewGet_S3 = 1
2230 '        EndIf
2231 '        Break
2232     Else
2233         'Mネジ
2234         If MMScrewJudge% = 2 Then
2235             ScrewGet_S3 = -2
2236         EndIf
2237     EndIf
2238 FEnd
2239 '
2240 '■fnKEY_WAIT()
2241 ''' <summary>
2242 ''' GOTからのキー入力待ち
2243 ''' </summary>
2244 '''<returns>1：停止    2：次へ
2245 '''         3：継続    4：トルクチェック開始
2246 '''         5：NG
2247 '''         11：ロボット初期位置1    12：ロボット初期位置2
2248 '''         13：ロボット初期位置3    14：ロボット初期位置4
2249 '''</returns>
2250 ''' <remarks>
2251 ''' Date   : 2021/07/07 : M.Hayakawa
2252 ''' </remarks>'
2253 Function M% fnKEY_WAIT()
2254     fnKEY_WAIT = 0
2255     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
2256     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
2257     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
2258     '下記キー待ちの継続に反応させないため
2259     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
2260     Dly 0.2
2261     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
2262     MLocalLoopFlg=1
2263     While MLocalLoopFlg=1
2264         If M_In(11345) = 1 Then         '停止   M5345
2265             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
2266             fnKEY_WAIT = 1
2267             MLocalLoopFlg=-1
2268             Break
2269         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
2270             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
2271             fnKEY_WAIT = 2
2272             MLocalLoopFlg=-1
2273             Break
2274         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
2275             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
2276             fnKEY_WAIT = 3
2277             MLocalLoopFlg=-1
2278             Break
2279         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
2280             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
2281             fnKEY_WAIT = 4
2282             MLocalLoopFlg=-1
2283             Break
2284         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
2285             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
2286             fnKEY_WAIT = 5
2287             MLocalLoopFlg=-1
2288             Break
2289             '
2290         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
2291             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
2292             fnKEY_WAIT = MRobotInit1%
2293             MLocalLoopFlg=-1
2294             Break
2295             '
2296         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
2297             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
2298             fnKEY_WAIT = MRobotInit2%
2299             MLocalLoopFlg=-1
2300             Break
2301             '
2302         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
2303             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
2304             fnKEY_WAIT = MRobotInit3%
2305             MLocalLoopFlg=-1
2306             Break
2307             '
2308         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
2309             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
2310             fnKEY_WAIT = MRobotInit4%
2311             MLocalLoopFlg=-1
2312             Break
2313             '
2314         Else
2315         EndIf
2316     WEnd
2317     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
2318     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
2319 FEnd
2320 '
2321 '■ fnAUTO_CTL
2322 ''' <summary>
2323 ''' AUTOモードOFF、PLCからの開始待ち
2324 ''' </summary>
2325 ''' <remarks>
2326 ''' Date   : 2021/07/07 : M.Hayakawa
2327 ''' </remarks>
2328 Function M% fnAUTO_CTL
2329     fnAUTO_CTL = 0
2330     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2331     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
2332     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2333     '
2334     If M_Svo=0 Then             'サーボON確認
2335         Servo On
2336     EndIf
2337     Wait M_Svo=1
2338 FEnd
2339 '
2340 '■ fnWindScreenOpen
2341 ''' <summary>
2342 ''' ウィンド画面の表示、非表示設定
2343 ''' </summary>
2344 '''<param name="%"></param>
2345 '''<param name="%"></param>
2346 '''<param name="%"></param>
2347 '''<param name="%"></param>
2348 ''' <remarks>
2349 ''' コメントD1001, D1002, D1003の設定
2350 ''' MWindReSet = 0     画面非表示
2351 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2352 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2353 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2354 ''' Date   : 2021/07/07 : M.Hayakawa
2355 ''' </remarks>
2356 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2357     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2358         M_Out16(12480) = MCommentD1001            'D1001 コメント
2359     EndIf
2360     '
2361     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2362         M_Out16(12496) = MCommentD1002            'D1002 コメント
2363     EndIf
2364     '
2365     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2366        M_Out16(12512) = MCommentD1003            'D1003 コメント
2367     EndIf
2368     '
2369     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2370     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2371     Dly 0.5
2372     M_Out(12363) = 0                         'ウィンド画面設定
2373 FEnd
2374 '
2375 '■FnCtlValue2
2376 ''' <summary>
2377 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2378 ''' </summary>
2379 ''' <param name="MCtlNo%"></param>
2380 ''' <remarks>
2381 ''' Date : 2022/04/28 渡辺
2382 ''' </remarks>
2383 '''
2384 '''  1：投入数       ＋１
2385 '''  2：組立ＯＫ数   ＋１
2386 '''  3：組立ＮＧ数   ＋１ (未使用)
2387 '''  4：吸着エラー数 ＋１
2388 ''' 99：読書開始信号 OFF
2389 '''
2390 Function M% FnCtlValue2(ByVal MCtlNo%)
2391     FnCtlValue2 = 1
2392     Select MCtlNo%
2393         Case 1        '投入数＋１
2394             M_Out(12569) = 0             '書込み開始信号OFF
2395             M_Out(12568) = 1             '読込み開始信号ON
2396             MInputQty = M_In16(11600)    '投入数受信
2397             MInputQty = MInputQty + 1    '投入数＋１
2398             M_Out16(12592) = MInputQty   '投入数送信
2399             M_Out(12569) = 1             '書込み開始信号ON
2400             Break
2401             '
2402         Case 2        '組立ＯＫ数＋１
2403             M_Out(12569) = 0             '書込み開始信号OFF
2404             M_Out(12568) = 1             '読込み開始信号ON
2405             MAssyOkQty = M_In16(11616)   '組立OK数受信
2406             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2407             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2408             M_Out(12569) = 1             '書込み開始信号ON
2409             Break
2410             '
2411         Case 4        '吸着エラー数＋１
2412             M_Out(12569) = 0                       '書込み開始信号OFF
2413             M_Out(12568) = 1                       '読込み開始信号ON
2414             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
2415             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
2416             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
2417             M_Out(12569) = 1                       '書込み開始信号ON
2418             Break
2419             '
2420         Case 99        '読書開始信号OFF
2421             M_Out(12568) = 0        '読込み開始信号OFF
2422             M_Out(12569) = 0        '書込み開始信号OFF
2423             Break
2424             '
2425     End Select
2426     Exit Function
2427 FEnd
2428 'Insightによる画像処理検査実行（並列処理なし）
2429 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2430 '-------------------------------------------------------------------------------
2431 'Insightによる画像処理検査実行（並列処理なし）
2432 '   引数
2433 '       PInspPos()      ：検査位置
2434 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2435 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2436 '       MInspCnt%       ：検査位置数
2437 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2438 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2439 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2440 '   戻り値：整数
2441 '       0=異常終了、1=正常終了
2442 '
2443 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2444 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2445 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2446 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2447 '   20200410    :   検査グループ設定Retry追加
2448 '-------------------------------------------------------------------------------
2449     '----- 初期設定 -----
2450     Cnt 0                                                           '移動効率化解除(初期値=0)
2451     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2452 '    Cnt 1,0.1,0.1
2453     '変数宣言・初期化
2454     Def Inte MNum                                                   '検査番号(検査順1～)
2455     MNum% = 1                                                       '検査番号初期値設定
2456     Def Inte MEndFlg                                                '検査終了フラグ
2457     MEndFlg% = 0
2458     '
2459     '検査G番号設定要求・検査実行要求off
2460     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2461     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2462     'エラー番号クリア
2463     MInspErrNum = 0                                                 '検査実行エラー番号
2464     M_Out16(MOUT_InspErrNum) = MInspErrNum
2465     MInspNGStepNum = 0                                              '検査実行NGStep番号
2466     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2467     '
2468     'Insight Ready check?
2469     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2470         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2471         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2472         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2473         ISInspectionSingle = 0                                      '異常終了戻り値設定
2474         Exit Function
2475     EndIf
2476     '
2477     '検査位置数確認
2478     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2479         MInspErrNum = 21                                            '検査データなし 21　引数<1
2480         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2481         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2482         ISInspectionSingle = 0                                      '異常終了戻り値設定
2483         Exit Function
2484     EndIf
2485     '
2486     '
2487     '
2488     '----- メイン処理 -----
2489     '設定された検査位置数分の検査実行
2490     While( MEndFlg% = 0 )
2491         '----- 検査グループ番号設定Retry追加 20200410
2492         MSetGrNumRetryExitFlg = 0
2493         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2494         While( MSetGrNumRetryExitFlg = 0 )
2495         '----- 検査グループ番号設定Retry追加ここまで 20200410
2496             '
2497             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2498             '
2499             '----- 検査グループ番号設定 -----
2500             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2501             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2502             '
2503             '検査位置へ移動・移動完了待ち
2504             Mvs PInspPos( MNum% )                                       '移動
2505             Dly 0.05                                                    '移動完了後Delay
2506             '
2507             '検査グループ番号設定終了確認
2508             M_Timer(1) = 0
2509             MExitFlg = 0
2510             While( MExitFlg = 0 )
2511                 '検査G設定正常終了?
2512                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2513                     MExitFlg = 1
2514                 '
2515                 '検査G設定異常終了?
2516                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2517                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2518                     If MInspErrNum = 0 Then                             '1回目のエラー?
2519                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2520                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2521                     EndIf
2522                     MExitFlg = 1
2523                 '
2524                 'timeoutチェック
2525                 ElseIf 1000 < M_Timer(1) Then
2526                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2527                     If MInspErrNum = 0 Then                             '1回目のエラー?
2528                         MInspErrNum = 12                                'timeout エラー番号=12
2529                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2530                     EndIf
2531                     MExitFlg = 1
2532                 EndIf
2533             WEnd
2534             '
2535             '検査G番号設定要求off
2536             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2537             '
2538             '----- 検査グループ設定Retry追加 20200410
2539             'NGなければ抜ける
2540             If MCurrentStepErr = 0 Then
2541                 MSetGrNumRetryExitFlg = 1
2542             Else
2543                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2544                 If MSetGrNumRetryCnt = 0 Then
2545                     MSetGrNumRetryExitFlg = 1
2546                 Else
2547                     'Retryへ　その前にDelay
2548                     Dly 0.5
2549                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2550                 EndIf
2551             EndIf
2552             '----- 検査グループ設定Retry追加ここまで 20200410
2553             '
2554         WEnd
2555         '
2556         '
2557         '
2558         '----- 検査実行 -----
2559         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2560             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2561                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2562                 MInspRetryExitFlg = 0
2563                 MRetryCnt = 2                                        'Retry回数設定
2564                 While( MInspRetryExitFlg = 0 )
2565                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2566                     '
2567                     '検査完了確認
2568                     MRetryCnt = MRetryCnt - 1
2569                     M_Timer(1) = 0
2570                     MExitFlg = 0
2571                     While( MExitFlg = 0 )
2572                     '検査完了待ち
2573                         '検査OK終了?
2574                         If M_In( MIN_IS_InspOK% ) = 1  Then
2575                             MJudgeOKFlg = 1                         '検査OKフラグON
2576                             MExitFlg = 1
2577                         '
2578                         '検査NG終了?
2579                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2580                             If MInspErrNum = 0 Then                 '1回目のエラー?
2581                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2582                                     MInspErrNum = 32                    '検査NG エラー番号=32
2583                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2584                                 EndIf
2585                             EndIf
2586                             MExitFlg = 1
2587                         '
2588                         '検査異常終了(IS timeout)?
2589                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2590                             If MInspErrNum = 0 Then                 '1回目のエラー?
2591                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2592                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2593                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2594                                 EndIf
2595                             EndIf
2596                             MExitFlg = 1
2597                         '
2598                         'timeoutチェック
2599                         ElseIf 3000 < M_Timer(1) Then
2600                             If MInspErrNum = 0 Then                 '1回目のエラー?
2601                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2602                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2603                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2604                                 EndIf
2605                             EndIf
2606                             MExitFlg = 1
2607                         EndIf
2608                     WEnd
2609                     '
2610                     '検査開始要求off
2611                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2612                     '
2613                     'OKなら抜ける
2614                     If MJudgeOKFlg = 1 Then
2615                         MInspRetryExitFlg = 1
2616                     Else
2617                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2618                         If MRetryCnt = 0 Then
2619                             MInspRetryExitFlg = 1
2620                         Else
2621                             'Retryへ　その前にDelay
2622                             Dly 0.3
2623                         EndIf
2624                     EndIf
2625                     '
2626                 WEnd
2627             EndIf
2628         EndIf
2629         '
2630         '
2631         '
2632         MNum% = MNum% + 1                                           '検査Step+1
2633         '検査終了確認　検査終了フラグセット
2634         If (MInspCnt% < MNum% ) Then
2635             MEndFlg% = 1                                            '検査終了フラグセット
2636         EndIf
2637         'NG発生時続行時処理
2638         If MInspErrNum <> 0 Then                                    'NGあり?
2639             If MNgContinue% <> 1 Then                               'NG続行?
2640                 MEndFlg% = 1                                        '検査終了フラグセット
2641             EndIf
2642         EndIf
2643     WEnd
2644     '
2645     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2646     If 0 < MZAxis% Then
2647         PCurrentPos = P_Curr                                        '現在位置取得
2648         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2649         Mvs PCurrentPos                                             '現在位置上空へ移動
2650     EndIf
2651     '
2652     '戻り値設定
2653     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      'カメラ検査強制OK(M_In(11372)=1)追加(12/21中村)
2654         ISInspectionSingle = 1                                      '正常終了戻り値設定
2655     Else
2656         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2657         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2658         ISInspectionSingle = 0                                      '異常終了戻り値設定
2659     EndIf
2660     '
2661     Fine 0,P    'Fine切り(22/12/09中村)
2662 FEnd
2663 '
2664 ' ■ISInspection
2665 ''' <summary>
2666 ''' Insightによる画像処理検査実行
2667 ''' </summary>
2668 '''<param name="PInspPos()">検査位置</param>
2669 '''<param name="MInspGrNum%()">検査位置での検査グループ番号（=0：画像検査未実施）</param>
2670 '''             PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2671 '''<param name="MInspCnt%">検査位置数</param>
2672 '''<param name="MZAxis%">終了時のZ軸退避座標（-1:無効）</param>
2673 '''             終了時にZ軸をMZAxisで設定された位置まで上昇させる
2674 '''<param name="MNgContinue%">=1で検査エラー・NG発生時に全Stepの検査を行う</param>
2675 '''<returns>    整数 0=異常終了、1=正常終了</returns>
2676 '''         MInspErrNum     ：異常終了時にエラー番号が設定される
2677 '''         MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される"
2678 ''' <remarks>
2679 ''' Date   : 2021/07/07 : M.Hayakawa
2680 ''' </remarks>
2681 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2682 '    '画像使用確認 0<- 画像確認無しの場合
2683 '    If M_In(11369) = 0 Then            'toRBT_使用確認
2684 '        ISInspection = 1                                        '正常終了戻り値設定
2685 '    EndIf
2686 ''
2687 '    Cnt 0                                                       '移動効率化解除(初期値=0)
2688 '    Fine 0.05,P                                                 '位置決め完了条件設置　0.05mm
2689 '    MNum% = 1                                                   '検査番号初期値設定
2690 '    Def Inte MEndFlg                                            '検査終了フラグ
2691 '    MEndFlg% = 0
2692 '    '
2693 '    'エラー番号クリア
2694 '    MInspErrNumSub = 0                                          '検査実行エラー番号sub
2695 '    MInspErrNum = 0                                             '検査実行エラー番号
2696 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2697 '    MInspNGStepNum = 0                                          '検査実行NGStep番号
2698 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2699 '    '
2700 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready offなら終了
2701 '        MInspErrNum = 20                                        '検査実行エラー番号 20 Insight offline
2702 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2703 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2704 '        ISInspection = 0                                        '異常終了戻り値設定
2705 ''
2706 '    EndIf
2707 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2708 '    '
2709 '    '検査位置数確認
2710 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2711 '        MInspErrNum = 21                                        '検査データなし 21　引数<1
2712 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2713 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2714 '        ISInspection = 0                                        '異常終了戻り値設定
2715 ''
2716 '    EndIf
2717 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2718 '    '
2719 '    '設定された検査位置数分の検査実行
2720 '    While( MEndFlg% = 0 )
2721 '        '検査終了確認　検査終了フラグセット
2722 '        If (MInspCnt% < MNum% ) Then
2723 '            MEndFlg% = 1                                        '検査終了フラグセット
2724 '        EndIf
2725 '        '
2726 '        'タスク　検査G番号設定・検査完了確認処理開始　INSPTAST1
2727 '        If MEndFlg% = 0 Then
2728 '            M_01# = MInspGrNum%(MNum%)                          '検査G番号引渡し
2729 '        EndIf
2730 '        M_02# = MEndFlg%                                        '検査終了フラグ引渡し
2731 '        M_05# = MNum%                                           '検査番号(検査順1～)
2732 '        'タスク　検査G設定フラグ引渡し
2733 '        If MEndFlg% = 0 Then
2734 '            If 0 < MInspGrNum%(MNum%) Then
2735 '                M_03# = 1
2736 '            Else
2737 '                M_03# = 0
2738 '            EndIf
2739 '        Else
2740 '            M_03# = 0
2741 '        EndIf
2742 '        'タスク　検査結果確認フラグ引渡し
2743 '        If 1 < MNum% Then
2744 '            If 0 < MInspGrNum%(MNum%-1) Then
2745 '                M_04# = 1
2746 '            Else
2747 '                M_04# = 0
2748 '            EndIf
2749 '        Else
2750 '            M_04# = 0
2751 '        EndIf
2752 '        '
2753 '        'タスク処理開始
2754 '        M_00# = 1                                               'TASK処理開始
2755 '        'タスク処理開始確認
2756 '        M_Timer(1) = 0
2757 '        MExitFlg = 0
2758 '        While( MExitFlg = 0 )
2759 '            '処理開始完了確認
2760 '            If M_00# = 0 And M_10# = 8 Then
2761 '                MExitFlg = 1
2762 '            EndIf
2763 '            'timeoutチェック
2764 '            If 2000 < M_Timer(1) Then
2765 '                If MNgContinue% = 1 Then                        'NG続行?
2766 '                    MInspErrNumSub = 36                         'エラー番号設定36
2767 '                Else
2768 '                    MInspErrNum = 36                            'エラー番号設定36
2769 '                EndIf
2770 '                MExitFlg = 1
2771 '            EndIf
2772 '        WEnd
2773 '        '
2774 '        '検査位置へ移動・移動完了待ち
2775 '        If 0 = MInspErrNum Then
2776 '            If MEndFlg% = 0 Then
2777 '                Mvs PInspPos( MNum% )                           '移動
2778 '            EndIf
2779 '        EndIf
2780 '        '
2781 '        'タスク　検査G番号設定・検査完了確認処理終了待ち　INSPTAST1
2782 '        If 0 = MInspErrNum Then
2783 '            M_Timer(1) = 0
2784 '            MExitFlg = 0
2785 '            While( MExitFlg = 0 )
2786 '                '処理完了待ち（正常終了）
2787 '                If M_10# = 1 Then
2788 '                    MExitFlg = 1
2789 '                EndIf
2790 '                '処理完了待ち（異常終了）
2791 '                If M_10# = 0 Then
2792 '                    If MNgContinue% = 1 Then                    'NG続行?
2793 '                        MInspErrNumSub = M_12#                  'エラー番号設定　M12
2794 '                    Else
2795 '                        MInspErrNum = M_12#                     'エラー番号設定　M12
2796 '                    EndIf
2797 '                    MExitFlg = 1
2798 '                EndIf
2799 '                'timeoutチェック
2800 '                If 5000 < M_Timer(1) Then
2801 '                    If MNgContinue% = 1 Then                    'NG続行?
2802 '                        MInspErrNumSub = 31                     'エラー番号設定31
2803 '                    Else
2804 '                        MInspErrNum = 31                        'エラー番号設定31
2805 '                    EndIf
2806 '                    MExitFlg = 1
2807 '                EndIf
2808 '            WEnd
2809 '        EndIf
2810 '        '
2811 '        '検査結果確認
2812 '        If 0 = MInspErrNum Then
2813 '            If 1 < MNum% Then
2814 '                If 0 < MInspGrNum%(MNum%-1) Then                '検査あり?
2815 '                    If M_11# = 2 Then                           '検査NG?
2816 '                        If MNgContinue% = 1 Then                'NG続行?
2817 '                            If MInspNGStepNum = 0 Then          'NG未発生?
2818 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2819 '                            EndIf
2820 '                            MInspErrNumSub = 32                 'エラー番号設定 32:検査NG
2821 '                        Else
2822 ''                            MInspNGStepNum = MNum% - 1          '検査実行NGStep番号設定
2823 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2824 '                            MInspErrNum = 32                    'エラー番号設定 32:検査NG
2825 '                        EndIf
2826 '                   EndIf
2827 '                EndIf
2828 '            EndIf
2829 '        EndIf
2830 '        '
2831 '        'エラーなら検査中断終了するのでLoopから抜けるため終了フラグセット
2832 '        If 0 <> MInspErrNum Then
2833 '            MEndFlg% = 1
2834 '        EndIf
2835 '        '
2836 '        '検査実行、取込完了待ち
2837 '        If 0 = MInspErrNum Then
2838 '            If MEndFlg% = 0 Then
2839 '                If 0 < MInspGrNum%(MNum%) Then                  '検査あり?
2840 '                    M_Out(MOUT_IS_Insp%) = 1                    '検査実行要求on
2841 '                    '取込完了確認
2842 '                    M_Timer(1) = 0
2843 '                    MExitFlg = 0
2844 '                    While( MExitFlg = 0 )
2845 '                        '処理完了待ち
2846 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2847 '                            MExitFlg = 1
2848 '                        EndIf
2849 '                        'timeoutチェック
2850 '                        If 2000 < M_Timer(1) Then
2851 '                            If MNgContinue% = 1 Then            'NG続行?
2852 '                                MInspErrNumSub = 33             'エラー番号設定33
2853 '                            Else
2854 '                                MInspErrNum = 33                'エラー番号設定33
2855 '                            EndIf
2856 '                            MExitFlg = 1
2857 '                        EndIf
2858 '                    WEnd
2859 '                EndIf
2860 '                '
2861 '            EndIf
2862 '        EndIf
2863 '        MNum% = MNum% + 1
2864 '    WEnd
2865 '    '
2866 '    '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2867 '    If 0 < MZAxis% Then
2868 '        PCurrentPos = P_Curr                                    '現在位置取得
2869 '        PCurrentPos.Z = MZAxis%                                 'Z軸を設定
2870 '        Mvs PCurrentPos                                         '現在位置上空へ移動
2871 '    EndIf
2872 '    '
2873 '    'NG続行時処理
2874 '    If MNgContinue% = 1 Then                                    'NG続行?
2875 '        MInspErrNum = MInspErrNumSub                            'エラー番号設定
2876 '    EndIf
2877 '    '
2878 '    '戻り値設定
2879 '    If MInspErrNum = 0 Then
2880 '        ISInspection = 1                                        '正常終了戻り値設定
2881 '    Else
2882 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2883 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2884 '        ISInspection = 0                                        '異常終了戻り値設定
2885 '    EndIf
2886 '    '
2887 '*ISInspection_End
2888 'FEnd
2889 '
2890 '■InitialZoneB
2891 ''' <summary>
2892 ''' 非常停止後の復帰動作
2893 ''' 1)上空退避　Z方向上に移動
2894 ''' 2)J1軸以外を退避ポジションへ移動
2895 ''' 3)J1軸のみを退避ポジションへ移動
2896 ''' 4)イニシャルポジションへ移動
2897 ''' </summary>
2898 ''' <remarks>
2899 ''' Date : 2022/04/08 : N.Watanabe
2900 ''' </remarks>
2901 Function V fnInitialZoneB()
2902     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中] 2022/04/26 渡辺
2903 '
2904 'パラメータ
2905     Ovrd 5
2906 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2907 '    Cmp Pos, &B100011
2908 '
2909 '復帰動作開始
2910 '
2911 '置き台と両掴みの場所は、チャックを解放する
2912 *RecoveryChuckOpen
2913     PActive = P_Curr          '現在位置を取得
2914     MRecoveryChuckOpen = 0    'チャック解放フラグ 初期化
2915 'PProductOnRoboSet(ねじロボ製品置き位置)は、チャック解放
2916     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2917         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2918             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2919                 MRecoveryChuckOpen = 1
2920             EndIf
2921         EndIf
2922     EndIf
2923 'PProductOnRoboGet(ねじロボ製品取り位置)は、チャック解放
2924     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2925         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2926             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2927                 MRecoveryChuckOpen = 1
2928             EndIf
2929         EndIf
2930     EndIf
2931 '
2932 '    If MRecoveryChuckOpen = 1 Then
2933 '        M_Out(12256) = 0        '本体チャック閉OFF
2934 '        M_Out(12257) = 1        '本体チャック開ON
2935 '        M_20# = 0               'KEY入力初期化
2936 '        MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
2937 '        If MRtn = 0 Then
2938 '            fErrorProcess(11,244,284,0)
2939 '            If M_20# = MNext% Then M_20# = MClear%
2940 '            If M_20# = MAbout% Then GoTo *RecoveryEnd
2941 '            If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2942 '            If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2943 '        Else
2944 '            M_Out(12257) = 0        '本体チャック開OFF
2945 '        EndIf
2946 '    EndIf
2947 '
2948     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2949     M_Out(12256) = 0                           '本体チャック閉OFF
2950     M_Out(12257) = 1                           '本体チャック開ON
2951 '
2952     M_20# = 0                                  'KEY入力初期化
2953     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
2954     If MRtn = 1 Then M_Out(12257) = 0          '本体チャック開OFF
2955     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2956 '
2957     fErrorProcess(11,244,284,0)
2958     If M_20# = MNext% Then M_20# = MClear%
2959     If M_20# = MAbout% Then GoTo *RecoveryEnd
2960     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2961     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2962 '
2963     *RecoveryChuckOpenEnd
2964 '
2965 '背面板回避
2966 'PPlateBackSet～PPlateBackSet_6のエリアにいるときは、本体チャック開く
2967 '・PPlateBackSet_6         '経路6
2968 '・PPlateBackSet_5         '経路7
2969 '・PPlateBackSet_4         '経路8
2970 '・PPlateBackSet_3         '経路9
2971 '・PPlateBackSet_2         '経路10
2972 '・PPlateBackSet_1         '経路11
2973 '・PPlateBackSet           '背面板置き位置
2974 '上記７点のＸ座標・Ｙ座標・Ｚ座標とJ6軸が下記If文の範囲に入っている事を確認する事
2975     PActive = P_Curr                    '現在位置を取得
2976     JActive = J_Curr                    '現在位置を取得
2977     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
2978     If (PActive.X >= -35) And (PActive.X <= -5) Then
2979         If (PActive.Y >= 340) And (PActive.Y <= 515) Then
2980             If (PActive.Z >= 470) And (PActive.Z <= 560) Then
2981                 If (MJ6 >= 160) And (MJ6 <= 200) Then
2982                     M_Out(12256) = 0            '本体チャック閉OFF
2983                     M_Out(12257) = 1            '本体チャック開ON
2984                 Dly 1.0
2985                 EndIf
2986             EndIf
2987         EndIf
2988     EndIf
2989 '
2990 '
2991 '特殊回避　直接、上空退避が出来ない所の対処
2992 '
2993     Ovrd 1
2994 'PProductOnRoboSet(Get)～PProductOnRoboSet(Get)_2のエリアにいるときは、PProductOnRoboSet_2へ
2995 '・PProductOnRoboSet
2996 '・PProductOnRoboSet_1
2997 '・PProductOnRoboSet_2
2998 '・PProductOnRoboGet
2999 '・PProductOnRoboGet_1
3000 '・PProductOnRoboGet_2
3001 '上記６点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
3002     PActive = P_Curr                    '現在位置を取得
3003     JActive = J_Curr                    '現在位置を取得
3004     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
3005     If (PActive.X >= -40) And (PActive.X <= 0) Then
3006         If (PActive.Y >= 380) And (PActive.Y <= 420) Then
3007             If (PActive.Z >= 300) And (PActive.Z <= 450) Then
3008                 If (MJ6 >= -20) And (MJ6 <= 20) Then
3009                     Mvs PProductOnRoboSet_1
3010                     Dly 1.0
3011                     Mvs PProductOnRoboSet_2
3012                     Dly 1.0
3013                     Mov PProductOnRoboSet_3
3014                     Dly 1.0
3015                 EndIf
3016             EndIf
3017         EndIf
3018     EndIf
3019 '
3020 'PProductOnRoboSet(Get)_2～PProductOnRoboSet(Get)_3のエリアにいるときは、PProductOnRoboSet_3へ
3021 '・PProductOnRoboSet_2
3022 '・PProductOnRoboSet_3
3023 '・PProductOnRoboGet_2
3024 '・PProductOnRoboGet_3
3025 '上記４点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
3026     PActive = P_Curr                    '現在位置を取得
3027     JActive = J_Curr                    '現在位置を取得
3028     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
3029     If (PActive.X >= -40) And (PActive.X <= 0) Then
3030         If (PActive.Y >= 220) And (PActive.Y <= 420) Then
3031             If (PActive.Z >= 400) And (PActive.Z <= 570) Then
3032                 If (MJ6 >= -20) And (MJ6 <= 20) Then
3033                     Mvs PProductOnRoboSet_3
3034                     Dly 1.0
3035                 EndIf
3036             EndIf
3037         EndIf
3038     EndIf
3039 '
3040     Ovrd 5
3041 '
3042 '上空退避
3043     PActive = P_Curr
3044     Pmove = PActive
3045     Pmove.Z = 640           '上空退避する一律の高さ
3046     If PActive.X > 550 Then
3047         Pmove.Z =550        'パレット上に腕を伸ばしているときは640まで上げられない為、例外処置
3048     EndIf
3049     If PActive.Z < Pmove.Z Then
3050         Mvs Pmove
3051     EndIf
3052     Dly 1.0
3053 'J1軸以外を退避ポジションへ移動
3054     JActive = J_Curr
3055     Jmove = JTaihi
3056     Jmove.J1 = JActive.J1        'J1軸は現在値を使用し、JTaihiのポーズを取る
3057     Jmove.J6 = JActive.J6        'J6軸は現在値を使用し、JTaihiのポーズを取る
3058     Mov Jmove
3059     Dly 1.0
3060 'J1軸のみを退避ポジションへ移動
3061     Mov JTaihi
3062     Dly 1.0
3063 'イニシャルポジションへ移動
3064     Mov PInitialPosition
3065     Cmp Off
3066     Ovrd 100
3067 ' ねじロボを初期位置に戻すために強制的に自動運転開始
3068     If M_In(11856) = 0 Then                 ' 停止中のみ
3069         fnAutoScreenComment(501)            ' 状態表示[ネジ締め機自動運転開始中] 2022/04/25 渡辺
3070         M_Out(12834) = 1                    ' 自動運転開始ON 12834   M6834
3071         MRet = frInCheck(11842, 1, 30000&)  ' 自動運転開始受信待ち    11842   M5842
3072         If MRet = 0 Then
3073         Else
3074             M_Out(12834) = 0    ' 自動運転開始OFF 12834   M6834
3075         EndIf
3076     EndIf
3077     M_Out(12262) = 0            '位置決め出OFF
3078     M_Out(12263) = 1            '位置決め戻ON
3079     fErrorProcess(11,253,281,0)
3080 *RecoveryEnd
3081     Exit Function
3082 FEnd
3083 '
3084 '
3085 '■fnAutoScreenComment
3086 ''' <summary>
3087 ''' メイン画面の動作状況表示
3088 ''' コメントD1005の設定
3089 ''' </summary>
3090 '''<param name="McommentD1005%">コメントID</param>
3091 ''' <remarks>
3092 ''' Date   : 2021/07/07 : M.Hayakawa
3093 ''' </remarks>
3094 Function fnAutoScreenComment(ByVal McommentD1005%)
3095     M_Out16(12576) = McommentD1005%
3096 FEnd
3097 '
3098 '■fnRoboPosChk
3099 ''' <summary>
3100 ''' 最後に終了したロボットポジションの確認
3101 ''' </summary>
3102 '''<param name="MINNumber%">入力番号</param>
3103 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
3104 '''<param name="MTimeCnt&">タイムアウト時間</param>
3105 ''' PLCに保続した番号を読込み、確認
3106 ''' MRBTOpeGroupNo = 5 が初期位置に設定
3107 '''<returns>整数 0:タイムアウト 1:OK</returns>
3108 ''' <remarks>
3109 ''' Date   : 2021/07/07 : M.Hayakawa
3110 ''' </remarks>
3111 Function M% fnRoboPosChk
3112     fnRoboPosChk = 0
3113     MRet = fnStepRead()
3114     '初期位置でないと判断した場合
3115     'ウィンド画面切換え
3116     If MRBTOpeGroupNo > 5 Then
3117         '下記キー待ちの継続に反応させないため
3118         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3119         Dly 0.2
3120         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3121         Dly 1.5
3122         '
3123         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
3124         '
3125         MLoopFlg% = 1
3126         While MLoopFlg% = 1
3127             '
3128             '
3129             MKeyNumber% = fnKEY_WAIT()
3130             Select MKeyNumber%
3131                 Case Is = MAbout%       '停止
3132                     M_20# = MAbout%
3133                     MLoopFlg% = -1
3134                     Break
3135                 Case Is = MNext%        '次へ
3136                     'MLoopFlg% = -1
3137                     Break
3138                 Case Is = MContinue%    '継続
3139                     M_20# = MContinue%
3140                     MLoopFlg% = -1
3141                     Break
3142                 Default
3143                     Break
3144             End Select
3145         WEnd
3146     EndIf
3147     '
3148     If M_20# = MContinue% Then                              '継続ボタンが押された場合
3149         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
3150         Ovrd 5                                   '低速オーバーライド値設定
3151         Select MRBTOpeGroupNo
3152             Case Is = 5                          '何もしない
3153                 Break
3154             Case Is = 10                         '初期位置へ戻す
3155                 'Mov PTEST001
3156                 Break
3157             Case Is = 15                         '初期位置へ戻す
3158                 'Mov PTEST002
3159                 Dly 0.5
3160                 'Mov PTEST001
3161                 Dly 0.5
3162                 Break
3163             Default
3164                 Break
3165         End Select
3166         '
3167         Ovrd M_NOvrd                            'システムの初期値を設定
3168         M_Out(12364) = 1                        'toPLC_データ保存ON
3169         MRBTOpeGroupNo = 5
3170         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
3171         Dly 1.0
3172         M_Out(12364) = 0                        'toPLC_データ保存OFF
3173         fnRoboPosChk = 1                        '初期位置動作実行
3174         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
3175     EndIf
3176     Exit Function
3177 FEnd
3178 '
3179 '■frInCheck
3180 ''' <summary>
3181 ''' センサーINチェック
3182 ''' </summary>
3183 '''<param name="MINNumber%">入力番号</param>
3184 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
3185 '''<param name="MTimeCnt&">タイムアウト時間</param>
3186 '''<returns>整数 0:タイムアウト 1:OK</returns>
3187 ''' <remarks>
3188 ''' Date   : 2021/07/07 : M.Hayakawa
3189 ''' </remarks>
3190 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
3191     M_Timer(4) = 0
3192     MloopFlg = 0
3193     While MloopFlg = 0
3194         MCrtTime& = M_Timer(4)
3195         If M_In(MINNumber%) = MCMPFLG% Then
3196             MloopFlg = 1
3197             frInCheck = 1
3198         ElseIf MCrtTime& > MTimeCnt& Then
3199             MloopFlg = 1
3200             frInCheck = 0
3201         EndIf
3202     WEnd
3203 FEnd
3204 '-----------------------------------------------
3205 '
3206 'ねじ締め機通信確認
3207 '
3208 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3209 'fScrewTcomChk = 0　：正常終了
3210 '          　 　 -1 ：異常終了
3211 '-----------------------------------------------
3212 Function M% fScrewTcomChk
3213 *ReCheckScewTcomChk
3214     fScrewTcomChk = 0
3215     '通信確認送信
3216     M_Out(MOUT_ScwT_ComChk%) = MOn%
3217     '通信確認受信待機
3218 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
3219     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
3220     '通信確認送信終了
3221     M_Out(MOUT_ScwT_ComChk%) = MOff%
3222     If MRtn = 0 Then
3223         fScrewTcomChk = -1
3224     EndIf
3225     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
3226  '
3227 FEnd
3228 '
3229 '
3230 '-----------------------------------------------
3231 '
3232 'ねじ締め開始送信
3233 '
3234 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3235 'fScrewTStart = 0　：正常終了
3236 '           　　-1 ：異常終了
3237 '-----------------------------------------------
3238 Function M% fScrewTStart
3239     fScrewTStart = 0
3240     nRet% = 0
3241     'ねじ締め開始待機を受信
3242 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
3243     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
3244     If MRtn = 0 Then nRet% = -1
3245     If MRtn = 0 Then GoTo *ScrewStartERROR      '開始できなかった場合ジャンプ
3246     Dly 0.1
3247     'ねじ締め開始受信を送信
3248     M_Out(MOUT_ScwT_ST%) = MOn%
3249     Dly 0.5
3250     'Wait M_In(MTEST_KEY%) = MOn%
3251     'ねじ締め開始送信終了
3252     M_Out(MOUT_ScwT_ST%) = MOff%
3253     '
3254 *ScrewStartERROR
3255     fScrewTStart = nRet%
3256 FEnd
3257 '
3258 '
3259 '
3260 '-----------------------------------------------
3261 '
3262 'ねじ締め完了受信
3263 '
3264 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3265 'fScewTFinish = 0　：正常終了
3266 '          　 　-1 ：異常終了
3267 '-----------------------------------------------
3268 Function M% fScewTFinish
3269 *ReCheckScewTFinish
3270     fScewTFinish = 0
3271     'ねじ締め完了待機を受信
3272 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3273     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3274     If MRtn = 0 Then
3275         fScewTFinish = -1
3276     EndIf
3277     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3278     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3279     Dly 0.1
3280     'ねじ締め完了受信を送信
3281     M_Out(MOUT_ScwT_FinOK%) = MOn%
3282     Dly 0.5                          'とりあえず保持時間0.5msec
3283     'ねじ締め開始送信終了
3284     M_Out(MOUT_ScwT_FinOK%) = MOff%
3285     'Wait M_In(MTEST_KEY%) = MOn%
3286     '
3287 *ScewTFinish_ErrEnd
3288 FEnd
3289 '
3290 '
3291 '-----------------------------------------------
3292 '
3293 '条件xx停止受信
3294 '
3295 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3296 'fScewTCaseStop = 0　：正常終了
3297 '          　   　-1 ：異常終了
3298 '-----------------------------------------------
3299 Function M% fScewTCaseStop(ByVal MCase%())
3300 *ReCheckScewTCaseStop
3301     fScewTCaseStop = 0
3302     '条件xx停止を受信
3303     Wait M_In(MCase%(1)) = MOn%
3304     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3305     If MRtn = 0 Then
3306         fScewTCaseStop = -1
3307     EndIf
3308     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3309     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3310     Dly 0.1
3311     '条件xx停止受信を送信
3312     M_Out(MCase%(2)) = MOn%
3313     Dly 0.5                          'とりあえず保持時間0.5msec
3314     'ねじ締め開始送信終了
3315     M_Out(MCase%(2)) = MOff%
3316 *ScewTCaseStop_ErrEnd
3317     '
3318 FEnd
3319 '
3320 '■fScrewTighenRoboCheck
3321 '<summary>
3322 'ねじロボ監視
3323 '</summary>
3324 '<param name = "MStopNum%"> 停止番号</param>
3325 '<returns>整数 0:ねじロボ異常終了 1:OK </returns>
3326 '<make>
3327 '2021/12/2 中村天哉
3328 '</make>
3329 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3330     fnAutoScreenComment(503)    '状態表示[ねじロボ動作終了待ち] 2022/04/26 渡辺
3331     fScrewTighenRoboCheck = 1
3332     MScrewTighenRoboFlg% = 1    'フラグの初期化
3333     MCheck% = 0
3334     While MScrewTighenRoboFlg% = 1
3335         MCheck% = M_In16(11904)
3336         If M_In(MStopNum%) = 1 Then '停止位置まで来たら
3337             MScrewTighenRoboFlg% = 0 '関数を抜ける
3338             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
3339         EndIf
3340         If MCheck% <> 0 Then
3341             fScrewTighenRoboError(MCheck%)
3342             Select M_20#
3343                 Case MAbout%            '停止が押された場合
3344                     M_Out(12869) = 1 Dly 1.0
3345                     MScrewTighenRoboFlg% = 0
3346                     fScrewTighenRoboCheck = 0   '異常終了
3347                     Break
3348                 Case MNgProcess%        'NGが押された場合
3349                     M_Out(12873) = 1 Dly 1.0
3350                     MScrewTighenRoboFlg% = 0
3351                     fScrewTighenRoboCheck = 0   '異常終了
3352                     Break
3353                 Case MContinue%             'リトライが押された場合
3354                     M_20# = MClear%         'M_20#初期化
3355                     M_Out(12871) = 1 Dly 1.0
3356                     Break
3357                 Case MNext%                 '次へが押された場合
3358                     M_20# = MClear%         'M_20#初期化
3359                     M_Out(12874) = 1 Dly 1.0
3360                     Break
3361             End Select
3362             Dly 0.5
3363         EndIf
3364     WEnd
3365 FEnd
3366 '
3367 '■fScrewTighenRoboError
3368 '<summary>
3369 'ねじロボエラー処理
3370 '</summary>
3371 '<param name = "ErrorCode%"> エラー番号</param>
3372 '<make>
3373 '2021/12/2 中村天哉
3374 '</make>
3375 Function fScrewTighenRoboError(ByVal MErrorCode%)
3376     MErrorScreenCode% = 0
3377     MErrorScreenCode% = MErrorCode% + 300
3378     fErrorProcess(11,MErrorScreenCode%,0,0)
3379 FEnd
3380 '
3381 '■fErrorProcess
3382 '<summary>
3383 'エラー処理
3384 '</summary>
3385 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
3386 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
3387 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
3388 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
3389 '<make>
3390 '2021/11/5 中村天哉
3391 '</make>
3392 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3393     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
3394     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
3395     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
3396     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
3397 *RETRY_ERR_PROCESS
3398      M_20# = MClear%     '初期化
3399 '        'エラー処理記述
3400         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3401 '        'GOT KEY入力待ち
3402         MKeyNumber = fnKEY_WAIT()
3403 '        '
3404         If MKeyNumber = MAbout% Then   '停止を選択した場合
3405             M_20# = MAbout%            'M_20# プログラム間共通外部変数
3406             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3407             Break
3408          '
3409         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
3410             M_20# = MContinue%            'M_20# プログラム間共通外部変数
3411             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3412         '
3413         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
3414             M_20# = MNext%            'M_20# プログラム間共通外部変数
3415             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3416          '
3417         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
3418             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
3419             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3420             Break
3421         '
3422         EndIf
3423         '
3424         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3425 FEnd
3426 '
3427 '■fnTorqueCheck
3428 ''' <summary>
3429 ''' トルクチェック動作用のメイン
3430 ''' </summary>
3431 ''' <remarks>
3432 ''' Date   : 2021/12/21 : H.AJI
3433 ''' </remarks>'
3434 Function M% fnTorqueCheck
3435     'トルクチェック中送信  搬送系停止
3436     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
3437     '
3438     fnTorqueCheck = 0
3439     Ovrd 20
3440     Mov PInitialPosition              '初期位置移動
3441     Ovrd 100
3442     '下記キー待ちの継続に反応させないため
3443     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3444     Dly 0.2
3445     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3446     '
3447     'M6340  トルクチェック受信
3448     'Dly 5.0
3449     M_Out(12340) = 1          'トルクチェック受信 M6340
3450     Dly 1.0
3451     M_Out(12340) = 0
3452     '
3453     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
3454     M_Out(12835) = 1                         'ねじロボトルクチェック画面切替
3455    Wait M_In(11843) = 1                         'ねじロボトルクチェック画面切替
3456     M_Out(12835) = 0                         'ねじロボトルクチェック画面切替完了
3457     '
3458     '
3459     MLoopFlg = 1
3460     While MLoopFlg = 1
3461         '
3462         Mov PInitialPosition              '初期位置移動
3463         '
3464         MKeyNumber = fnKEY_WAIT()
3465         Select MKeyNumber
3466             Case Is = 1           '停止
3467                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
3468                 Dly 1.0
3469                 M_Out(12343) = 0
3470                 Ovrd 20
3471                 'Mov PTicketRead_1
3472                 M_Out(12840) = 1          'トルクチェック終了
3473                 Wait M_In(11859) = 1      'ねじロボからの終了
3474                 M_Out(12840) = 0          'トルクチェック終了
3475                 Ovrd 100
3476                 M_20# = 1
3477                 MLoopFlg = -1
3478                 Break
3479             Case Is = 2           '次へ
3480                 Break
3481             Case Is = 3           '継続
3482                 Break
3483             Case Is = 4           'トルクチェック開始
3484                 M_Out(12342) = 1          'トルクチェック開始要求受信 M6342
3485                 Dly 1.0
3486                 M_Out(12342) = 0
3487                 If M_In(11862) = 1 Then             'トルクチェッカー確認
3488                     fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
3489                     MRet = fnScrewMTorque()           'ねじロボ用トルクチェック
3490                 EndIf
3491                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
3492                 'MRet = fnMoveTorquePosi()
3493                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
3494                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3495                 Break
3496             Default
3497                 Break
3498         End Select
3499     WEnd
3500     '
3501     'トルクチェック中停止送信
3502     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
3503     '
3504     'ロボットの位置を元に戻す
3505     '
3506     '
3507  FEnd
3508  '
3509 '
3510 '
3511 '---------------------------
3512 '
3513 '    メイン画面の表示、非表示設定
3514 '         コメントD1001, D1002, D1003の設定
3515 '           MWindReSet = 0     画面非表示
3516 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
3517 '           MWindErrScr = 10    エラー画面 D1001, D1002
3518 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
3519 '
3520 '---------------------------
3521 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3522     fnMainScreenOpen = 0
3523     '
3524    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
3525         M_Out16(12480) = MCommentD1001            'D1001 コメント
3526     EndIf
3527     '
3528     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
3529         M_Out16(12496) = MCommentD1002            'D1002 コメント
3530     EndIf
3531     '
3532     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
3533         M_Out16(12512) = MCommentD1003            'D1003 コメント
3534     EndIf
3535     '
3536     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
3537     M_Out(12362) = 1                         'ウィンド画面設定  M6362
3538     Dly 0.5
3539     M_Out(12362) = 0                         'ウィンド画面設定
3540 FEnd
3541 '
3542 '■Main
3543 ''' <summary>
3544 ''' トルクチェック実動作
3545 ''' </summary>
3546 ''' <remarks>
3547 ''' Date   : 2021/12/21 : H.AJI
3548 ''' </remarks>'
3549 Function M% fnScrewMTorque
3550     fnScrewMTorque = 0
3551     M_Out(12838) = 1                         'トルクチェック開始1
3552     Wait M_In(11857) = 1                     '受信完了
3553     M_Out(12838) = 0                         'トルクチェック開始1
3554     Dly 2.0
3555 FEnd
3556 '
3557 '■Main
3558 ''' <summary>
3559 ''' トルクチェック実動作
3560 ''' </summary>
3561 ''' <remarks>
3562 ''' Date   : 2021/12/21 : H.AJI
3563 ''' </remarks>'
3564 Function M% fnMoveTorquePosi
3565      fnMoveTorquePosi = 0
3566      Ovrd 50
3567     'Mov PTorquePosi000 'トルクチェック回避位置へ移動
3568      Mov PTorqueCheck_1 'トルクチェックメーター上空へ移動
3569     'Mov PTorquePosi020 'トルクチェックビットジョイント上空
3570     '
3571     '
3572      '以下は阿部さんが作成したトルクチェックプラグラム
3573     '
3574     Spd M_NSpd
3575 '-------------      ドライバーRST
3576     M_Out(12240)=0     'ドライバーOFF CCW
3577     M_Out(12241)=0     'ドライバーOFF CW
3578     M_Out(12242)=0     'ドライバー解除 C1
3579     M_Out(12243)=0     'ドライバー解除 C2
3580     M_Out(12245)=0     'プログラム解除 F1/プログラム2
3581 '---------------------------------------
3582 '---------------------------------------
3583     Fsc Off            '力覚センサ　Off  STEP1は不要
3584 '--------------------------------------------------------------
3585 '--------------------------------------------------------------
3586 '[P-11]
3587 '--------------------------------------------------------------   【トルクチェック 0.4N - P11】
3588     Mov PTorqueCheck, -50                     ' トルク-1　置き位置上空 50mm へ移動
3589    'Mov PTorquePosi020, -10                    ' トルク-1　置き位置上空 10mm へ移動
3590     Dly 0.1
3591 '-----------------------
3592    'Cnt 0                           'Cnt動作-2　終了
3593 '-----------------------
3594     Mov PTorqueCheck , -5                      'トルク-1　置き位置上空 5mm へ移動
3595     Dly 0.2
3596 '-----------------------
3597     M_Out(12242)=1                   'ドライバーセット C1
3598     Dly 0.1
3599     M_Out(12243)=1                   'ドライバーセット C2 (バンク3)
3600     Dly 0.1
3601     M_Out(12245)=1                   'プログラム2セット F1  Mネジ
3602     Dly 0.1
3603     'M_Out(12241)=1                   'ドライバーON  CW
3604    M_Out(12241)=0                   'ドライバーOFF  CW
3605     'Dly 0.1
3606 '--------------------------------
3607     Ovrd 40
3608    'Dly 0.1
3609 '--------------------------------  ネジ締め速度設定
3610     Spd 14                            'ライド 100-40 100% :Spd 12
3611     Dly 0.1
3612 '--------------------------------
3613 '--------------------------------
3614 '---------------------------------【ねじ締め動作】
3615 '
3616     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '移動中エラー検出
3617    Mvs PTorqueCheck               'トルクチェック位置へ移動
3618     Dly 0.3                          '動作安定待ち
3619    M_Out(12241)=1                   'ドライバーON  CW
3620 '
3621     Wait M_In(11584)=1                '完了/エラー検出
3622     Dly 0.1
3623     Spd M_NSpd
3624    'Ovrd 20
3625     If M_In(11256)=1 Then *LBL1       'ネジトータルエラー検出
3626     Wait M_In(11257)=1                'ネジ完了SC
3627 '---------------------------------
3628     Dly 0.1
3629     M_Out(12241)=0                    'ドライバーOFF CW
3630     Dly 0.1
3631     M_Out(12242)=0                    'ドライバー解除 C1
3632     Dly 0.1
3633     M_Out(12243)=0                    'ドライバー解除 C2 (バンク3)
3634     Dly 0.1
3635     M_Out(12245)=0                    'プログラム2解除 F1
3636 '--------------------------------------------------------------   【トルクチェック 0.4N - P11ここまで】
3637 '
3638     Mvs PTorqueCheck,-60                       'あえてmov から変更
3639     Dly 0.1
3640 '--------------------------------------------------------------
3641    'Ovrd 80
3642 '--------------------------------------------------------------
3643 '---------------------------------------
3644 '---------------------------------------
3645 '---------------------------------------エラー離脱処理
3646    *LBL1
3647    Fsc Off            '力覚センサ　Off   *STEP1は不要
3648    Mvs ,-100
3649    M_Out(12241)=0     'ドライバーOFF CW
3650    Dly 0.1
3651    M_Out(12242)=0     'ドライバー解除 C1
3652    Dly 0.1
3653    M_Out(12243)=0     'ドライバー解除 C2 (バンク3)
3654    Dly 0.1
3655    M_Out(12245)=0     'プログラム解除 F1
3656 '---------------------------------------
3657 '---------------------------------------
3658 '-------------
3659    'Mov PInitPos19049
3660    Dly 0.1
3661 '
3662 '
3663 '
3664 '
3665 FEnd
3666 '
3667 '
3668 '----------------------------------------------------------------
3669 'fTimeOutJudge
3670 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3671 '引数
3672 'Address% = 監視アドレス番号
3673 'JudgeFlg% = 対象アドレスの正常終了時の値
3674 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3675 '戻り値 = 0 エラー
3676 '         1 正常終了
3677 '         2 リトライ
3678 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3679 '作成日
3680 '2022/9/20 中村
3681 '----------------------------------------------------------------
3682 '
3683 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3684     fTimeOutJudge = 0
3685     MJudge% = 1
3686     MRtn = 0
3687     M_20# = MClear%
3688     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3689 *TimeOutLoop
3690     If MRtn = 1 Then GoTo *TimeOut
3691         fErrorProcess(11,202,203,0)
3692         If M_20# = MNext% Then GoTo *TimeOutLoop
3693         If M_20# = MContinue% Then MJudge% = 2
3694         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3695 *TimeOut
3696     fTimeOutJudge = MJudge%
3697 '
3698 *JUDGE_ERROR_END
3699 FEnd
3700 '■Main
3701 ''' <summary>
3702 ''' 組立動作用のメイン
3703 ''' </summary>
3704 ''' <remarks>
3705 ''' Date   : 2021/07/07 : M.Hayakawa
3706 ''' </remarks>'
3707 Function Main
3708     MopeNo = M_21#         '外部変数にて動作番号代入
3709     '
3710     If M_Svo=0 Then
3711         Servo On
3712     EndIf
3713     Wait M_Svo=1
3714 '組立スタート日付時刻要求パルスON
3715     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3716 'パトライト操作
3717     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
3718     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
3719     '
3720     M_20# = 0                                   'KEY入力初期化
3721     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
3722     MRet% = 0
3723 '初期位置の確認と移動
3724 '
3725 '復帰動作　実行・未実行判別      2022/04/08 渡辺 作成
3726     PActive = P_Curr                    '現在位置を取得
3727     MRecoveryPass% = 0
3728     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3729         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3730             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3731                 MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
3732             EndIf
3733         EndIf
3734     EndIf
3735     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3736         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3737             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3738                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
3739             EndIf
3740         EndIf
3741     EndIf
3742     If MRecoveryPass% = 0 Then
3743        fnInitialZoneB()        '復帰動作パスフラグが立っていない時は復帰動作を実行
3744     EndIf
3745 '
3746 '
3747 '    MRet% = fnRoboPosChk()
3748 '    If MRet% = 1 Then                           '初期位置の動作を行った場合    '2022/04/26 コメントアウト 渡辺
3749 '        fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  'ウィンド画面
3750 '        MKeyNumber% = fnKEY_WAIT()
3751 '        Select MKeyNumber%
3752 '            Case Is = MAbout%       '停止
3753 '                M_20# = MAbout%
3754 '                MLoopFlg% = -1
3755 '                Break
3756 '            Case Is = MNext%        '次へ
3757 '                'MLoopFlg = -1
3758 '                Break
3759 '            Case Is = MContinue%    '継続
3760 '                M_20# = MContinue%
3761 '                MLoopFlg% = -1
3762 '                Break
3763 '            Default
3764 '                Break
3765 '        End Select
3766 '    EndIf
3767     '
3768     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
3769         M_Out(12364) = 1            'toPLC_データ保存ON
3770 'トルクチェック
3771         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3772             MRet% = fnTorqueCheck()
3773             Break
3774         Else
3775 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
3776 '                MRtn = InspInit()               '画像処理初期化処理
3777 '            EndIf
3778             '
3779            M_20# = MClear%                    '初期化
3780 '組立開始
3781             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3782                 fnAssyStart()
3783             Else
3784                 M_20# = MPass%
3785             EndIf
3786 '組立終了日付時刻
3787             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
3788             Wait M_In(11572) = 1            '日付取得完了
3789             Dly 0.1
3790             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
3791 'リフターユニットへのOUT
3792             '  KEY入力が何もない場合 OKと判断
3793             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
3794             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
3795 'OK/NGフラグ出力
3796             If M_20# <= 0 Then
3797                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
3798             ElseIf M_20# = MPass% Then
3799                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
3800             EndIf
3801 'PIASに組立完了書込み
3802             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3803                 If M_20# = MPass% Then
3804                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3805                 Else
3806                     'KEY入力がNGの場合
3807                     If M_20# = MNgProcess% Then
3808                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3809                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3810                         MRet% = fnPiasWrite(MNG%)
3811                        nAssyNgQty = nAssyNgQty + 1
3812                     EndIf
3813                     '
3814                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/17中村)
3815                     If M_20# = MAssyOK% Then
3816                             '-----------------------
3817                             'D732 -> D2600 コピー要求
3818                             M_Out(12566) = 1
3819 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3820                             M_Out(12566) = 0
3821                             '
3822                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3823                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3824                             '基板番号照合(PPは未使用）
3825 '                            MRet% = fnPCBNumberCheck()
3826                         Else
3827                             MRet% = 1
3828                         EndIf
3829                         '
3830                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3831                             If M_20# <> MAbout% Then
3832                                 '工程履歴OK書き込み
3833                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3834                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3835                                 MRet% = fnPiasWrite(MOK%)
3836                                 nAssyOkQty = 0
3837                                 nAssyOkQty = nAssyOkQty + 1
3838                             Else
3839                                 nAssyOkQty = nAssyOkQty + 1
3840                             EndIf
3841                         EndIf
3842                     EndIf
3843 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3844 '                    MRet% = fnPiasWrite(MOK%)
3845                 EndIf
3846             Else
3847                 nAssyOkQty = nAssyOkQty + 1
3848             EndIf
3849             '
3850             '組立終了日付時刻解除
3851             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3852             '投入数、組立OK数、組立NG数書込み
3853 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3854             '
3855 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3856 '                '画像処理終了処理
3857 '                MRtn = InspQuit()
3858 '            EndIf
3859         EndIf
3860         M_Out(12364) = 0                          'toPLC_データ保存OFF
3861     EndIf
3862 'パトライト操作
3863     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3864     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3865 'GOT表示
3866     fnAutoScreenComment(93)  'AUTO画面 工程完了
3867 FEnd
3868 End
3869 '
3870 'おまじないコメント
3871 '絶対削除するな
3872 '
3873 '
3874 '
3875 '
3876 '
3877 '
3878 '
3879 '
3880 '
JActive=(-11.550,19.840,77.070,0.070,83.290,-10.420,0.000,0.000)
Jmove=(-11.550,-46.870,111.640,0.000,80.580,-10.420,0.000,0.000)
JTaihi=(0.000,-46.870,111.640,0.000,80.580,0.000)
PActive=(340.000,0.000,580.000,180.000,0.000,-180.000,0.000,0.000)(7,0)
PInitialPosition=(340.000,0.000,580.000,-180.000,0.000,180.000)(7,0)
PMechaGet=(-415.660,-8.740,299.320,179.450,-2.220,176.860)(7,1048577)
PMechaGet_1=(-415.660,-8.740,409.960,179.450,-2.220,176.860)(7,1048577)
PMechaGet_2=(-189.840,-0.010,629.060,-180.000,0.000,-179.990)(7,1)
PMechaGet_3=(0.010,189.840,629.070,-180.000,0.000,90.000)(7,0)
PMechaGet_4=(327.500,0.020,596.240,-179.990,0.000,103.500)(7,0)
PMechaGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaSet1=(159.800,-334.910,320.000,122.020,86.640,31.390)(6,0)
PMechaSet1_1=(159.800,-334.910,340.000,122.020,86.640,31.390)(6,0)
PMechaSet2=(160.190,-334.970,320.180,122.020,86.640,31.360)(6,0)
PMechaSet2_1=(160.190,-334.970,339.970,122.020,86.640,31.360)(6,0)
PMechaSet_2=(162.580,-305.370,557.380,179.470,90.000,89.470)(6,0)
PMechaSet_3=(114.450,-288.220,565.580,180.000,0.000,112.110)(7,0)
PMechaSet_4=(310.110,-0.040,565.560,180.000,0.000,-179.550)(7,0)
PMechaSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
Pmove=(478.560,-97.670,640.000,179.900,-0.170,178.870,0.000,0.000)(7,0)
PPlateBackCheck=(-74.490,309.560,598.750,180.000,-54.150,90.000)(7,1048576)
PPlateBackCheck_2=(-59.410,338.490,632.860,170.410,-66.660,89.130)(7,1048576)
PPlateBackCheck_3=(-17.860,286.220,630.900,-179.690,-0.410,90.870)(7,1048576)
PPlateBackCheck_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackCheck_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackCheck_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet=(469.500,105.410,400.260,179.920,-0.070,179.170)(7,0)
PPlateBackGet_1=(469.490,105.410,430.000,179.920,-0.070,179.170)(7,0)
PPlateBackGet_2=(469.490,105.410,560.000,179.920,-0.070,179.170)(7,0)
PPlateBackGet_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackPush=(-19.270,414.740,537.150,179.810,0.000,88.900)(7,1048576)
PPlateBackPush_1=(-19.270,405.000,537.150,179.810,0.000,88.900)(7,1048576)
PPlateBackPush_2=(-19.270,405.000,546.600,179.810,0.000,88.470)(7,1048576)
PPlateBackSet=(-19.270,455.480,538.130,-179.890,-11.000,88.450)(7,1048576)
PPlateBackSet_1=(-19.270,449.080,536.520,-179.830,-13.000,88.440)(7,1048576)
PPlateBackSet_10=(-19.010,351.550,478.290,-178.900,-45.000,88.440)(7,1048576)
PPlateBackSet_11=(-19.010,348.410,478.290,-178.900,-45.000,88.440)(7,1048576)
PPlateBackSet_12=(-19.010,346.190,488.960,-178.900,-45.000,88.440)(7,1048576)
PPlateBackSet_13=(-17.860,286.220,630.900,-179.690,-0.410,90.870)(7,1048576)
PPlateBackSet_2=(-19.270,435.190,532.920,-179.720,-17.000,88.440)(7,1048576)
PPlateBackSet_3=(-18.970,422.190,528.410,-179.610,-21.000,88.440)(7,1048576)
PPlateBackSet_4=(-18.970,408.850,522.580,-179.490,-25.000,88.440)(7,1048576)
PPlateBackSet_5=(-18.970,396.670,516.380,-179.390,-29.000,88.440)(7,1048576)
PPlateBackSet_6=(-18.970,385.090,508.310,-179.280,-33.000,88.440)(7,1048576)
PPlateBackSet_7=(-18.970,373.630,499.600,-179.170,-37.000,88.440)(7,1048576)
PPlateBackSet_8=(-18.970,362.940,490.670,-179.040,-41.000,88.440)(7,1048576)
PPlateBackSet_9=(-18.970,353.140,480.980,-178.900,-45.000,88.440)(7,1048576)
PProductOnPltGet=(478.760,-97.710,372.280,179.900,-0.170,179.390)(7,0)
PProductOnPltGet_1=(478.760,-97.710,410.000,179.900,-0.170,-179.100)(7,0)
PProductOnPltGet_2=(478.760,-97.710,500.000,179.900,-0.170,-179.100)(7,0)
PProductOnPltGet_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltGet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet=(477.080,-96.540,372.590,179.900,-0.170,178.770)(7,0)
PProductOnPltSet_1=(477.080,-96.540,410.000,179.900,-0.170,178.770)(7,0)
PProductOnPltSet_2=(477.080,-96.540,500.000,179.900,-0.170,178.770)(7,0)
PProductOnPltSet_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboGet=(-18.540,403.100,319.090,87.030,88.570,177.170)(6,0)
PProductOnRoboGet_1=(-18.540,403.100,410.000,87.030,88.570,177.170)(6,0)
PProductOnRoboGet_2=(-18.540,396.290,419.810,84.410,89.140,174.550)(6,0)
PProductOnRoboGet_3=(-19.740,378.130,425.480,-105.620,89.690,-15.620)(6,0)
PProductOnRoboGet_4=(-19.740,300.000,550.000,175.040,89.990,-94.950)(6,0)
PProductOnRoboGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboSet=(-18.540,403.200,319.090,87.030,88.570,177.170)(6,0)
PProductOnRoboSet_1=(-18.540,403.200,410.000,87.030,88.570,177.170)(6,0)
PProductOnRoboSet_2=(-18.540,396.290,419.810,84.410,89.140,174.550)(6,0)
PProductOnRoboSet_3=(-18.910,236.650,555.850,-94.470,89.060,-4.460)(6,0)
PProductOnRoboSet_4=(-18.860,404.690,360.000,-102.740,89.080,-12.360)(6,0)
PProductOnRoboSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPushTilt=(-213.930,562.960,465.170,179.970,-0.020,4.010)(7,0)
PPushTilt_1=(-213.930,562.970,480.780,179.960,-0.020,4.010)(7,0)
PPushTilt_2=(-213.930,562.960,620.000,179.970,-0.020,4.010)(7,0)
PPushTilt_3=(0.020,340.000,610.000,-180.000,-0.010,-91.910)(7,0)
PTemp=(340.000,0.000,580.000,180.000,0.000,-180.000,0.000,0.000)(7,0)
PTicketRead=(602.000,-150.000,500.000,180.000,0.000,90.000)(7,0)
PTicketRead_1=(602.000,-150.000,550.000,180.000,0.000,90.000)(7,0)
PTicketRead_2=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PEscapePosi(1)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(1)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(1)=(-74.490,309.560,598.750,180.000,-54.150,90.000,0.000,0.000)(7,1048576)
PInspPosition(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(11)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(12)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(13)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(14)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(15)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(16)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(17)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(18)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(19)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(20)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(21)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(22)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(23)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(24)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(25)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(26)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(27)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(28)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(29)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(30)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(1)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
