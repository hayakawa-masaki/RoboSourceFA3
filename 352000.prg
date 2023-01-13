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
214 Def Inte MScrewNo
215 Def Inte MReTry
216 '===== <IO変数定義> =====
217 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
218 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
219 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
220 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
221 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
222 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
223 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
224 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
225 '
226 Def Inte Y6A_VV1            ' アーム先端　ネジ吸着バルブ
227 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
228 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
229 '
230 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
231 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
232 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
233 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
234 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
235 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
236 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
237 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
238 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
239 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
240 '
241 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
242 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
243 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
244 '
245 Def Inte MOUT_LED1          ' 画像処理用LED照明
246 '
247 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
248 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
249 '
250 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
251 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
252 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
253 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
254 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
255 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
256 '
257 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
258 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
259 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
260 '
261 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
262 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
263 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
264 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
265 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
266 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
267 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
268 Y6A_VV1%    =  12250    ' アーム先端　ネジ吸着バルブ
269 Y6B_VB1%    =  12251    'アーム先端　吸着破壊バルブ
270 MOUT_VB1%   =  12251    ' アーム先端　ネジ吸着破壊バルブ
271 '
272 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
273 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
274 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
275 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
276 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
277 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
278 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
279 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
280 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
281 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
282 '
283 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
284 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
285 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
286 '
287 MOUT_LED1%  =  12239    ' 画像処理用LED照明
288 '
289 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
290 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
291 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
292 '
293 '共通
294 Def Inte MTEST_KEY                      'デバックテスト用
295 Def Inte MOn                            '出力=1
296 Def Inte MOff                           '出力=0
297 '
298 'ねじ締め装置_出力アドレス
299 Def Inte MOUT_ScwT_ComChk               '通信確認
300 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
301 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
302 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
303 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
304 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
305 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
306 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
307 'ねじ締め装置_入力アドレス
308 Def Inte MIN_ScwT_comOK                 '通信確認返信
309 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
310 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
311 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
312 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
313 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
314 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
315 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
316 '
317 Dim MScwT_Case1%(2)               '条件1停止変数
318 Dim MScwT_Case2%(2)               '条件2停止変数
319 Dim MScwT_Case3%(2)               '条件3停止変数
320 Dim MScwT_Case4%(2)               '条件4停止変数
321 Dim MScwT_Case5%(2)               '条件5停止変数
322 '
323 '共通
324 MTEST_KEY% = 11359                       'デバッグ用テストKEY
325 MOn% = 1                                 '出力 = 1
326 MOff% = 0                                '出力 = 0
327 '
328 'ねじ締め機_アドレス設定
329 MOUT_ScwT_ComChk% = 12832               '通信確認送信
330 MOUT_ScwT_ST% = 12865                   'ねじ締め開始を送信
331 MOUT_ScwT_ReSTOK% = 12866               '再開始受信を送信
332 MOUT_ScwT_FinOK% = 12868                'ねじ締め完了受信を送信
333 MOUT_ScwT_Case1OK% = 12874              '条件1停止受信を送信
334 MOUT_ScwT_Case2OK% = 12875              '条件2停止受信を送信
335 MOUT_ScwT_Case3OK% = 12876              '条件3停止受信を送信
336 MOUT_ScwT_Case4OK% = 12877              '条件4停止受信を送信
337 MOUT_ScwT_Case5OK% = 12878              '条件5停止受信を送信
338 '
339 MIN_ScwT_comOK% = 11840                 'ねじ締め装置から返信
340 MIN_ScwT_STRec% = 11873                 'ねじ締め開始を受信
341 MIN_ScwT_ReST% = 11874                  '再開始を受信
342 MIN_ScwT_Fin% = 11876                   'ねじ締め完了を受信
343 MIN_ScwT_Case1% = 11882                 '条件1停止待機を受信
344 MIN_ScwT_Case2% = 11883                 '条件2停止待機を受信
345 MIN_ScwT_Case3% = 11884                 '条件3停止待機を受信
346 MIN_ScwT_Case4% = 11885                 '条件4停止待機を受信
347 MIN_ScwT_Case5% = 11886                 '条件5停止待機を受信
348 '
349 MScwT_Case1%(1) = MIN_ScwT_Case1%
350 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
351 MScwT_Case2%(1) = MIN_ScwT_Case2%
352 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
353 MScwT_Case3%(1) = MIN_ScwT_Case3%
354 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
355 MScwT_Case4%(1) = MIN_ScwT_Case4%
356 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
357 MScwT_Case5%(1) = MIN_ScwT_Case5%
358 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
359 '
360 '設定 InitialZoneBで使用する変数
361 Def Pos PActive       '直交座標系 位置変数 現在位置
362 Def Pos Pmove         '直交座標系 位置変数 移動先
363 Def Jnt JActive       '関節座標系 位置変数 現在位置
364 Def Jnt Jmove         '関節座標系 位置変数 移動先
365 Def Jnt JTaihi        '関節座標系 位置変数 退避ポジション ティーチングで設定
366 Def Inte MRecoveryPass      '復帰動作パスフラグ　1=復帰動作をパス　0=復帰動作を実行
367 Def Inte MJ6          'J6軸の値を比較する為の変数
368 Def Inte MStandby              '待機位置確認フラグ
369 Def Inte MRecoveryChuckOpen    'チャック解放フラグ（復帰動作前）両掴み対策
370 '★注意★初期位置を変更した時には、変更が必要！
371 '
372 '
373 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
374 Function M% fnAssyStart
375     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
376 '   BaseUnit6通信確認 CDに伴い削除 2022/07/27 M.H
377 '
378 '
379 ' PIASチケット読込み工程抜け確認
380     M_20# = MClear%                       '初期化
381 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
382 '        MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
383 '        '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
384 '        '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
385 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' メニューへ戻る
386 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NGを工程履歴に書込み次の工程へ
387 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NGを工程履歴に書込み次の工程へ
388 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' 履歴NG, 工程抜け
389 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK以外は組立終了
390 '    EndIf
391 '    '
392 '    '座標移動
393 '    '
394 '    '条件xx停止
395 '    fScewTCaseStop(MScwT_Case5%)
396 '    '
397 '    'ベースユニットKEY
398 '    Wait M_In(MTEST_KEY%) = MOn%
399 '    '
400 '    '再開始
401 '    fScewTReStart()
402 '    '
403 '    '座標移動
404 '    '
405 '    'ねじ締め完了
406 '    Mret% = fScewTFinish()
407 ' ネジ締めテスト終了
408 ' PIASテスト -----------
409 '    MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
410 '    MRet% = fnPiasWrite(MNG%)
411  '   MRet% = fnPCBNumberCheck()
412 ' PIASテスト終了 -------
413 '
414     '組立開始(9/6追加(中村))
415     'プログラム原点
416     Ovrd 100
417     ' ハンド状態初期化(10/29追加M.H)(2/11修正(中村))
418     Cmp Off                     'コンプライアンスモード終了
419     ColChk On                   '衝突検知ON
420     If M_In(11266) Then
421         M_Out(12256) = 0
422         M_Out(12257) = 1
423     EndIf
424     If M_In(11269) Then
425         M_Out(12258) = 0
426         M_Out(12259) = 1
427     EndIf
428     If M_In(11271) Then
429         M_Out(12260) = 0
430         M_Out(12261) = 1
431     EndIf
432     *WAIT_HAND_INI
433     If M_In(11265) = 1 And M_In(11268) = 1 And M_In(11270) = 1 Then GoTo *CompHandIni Else GoTo *WAIT_HAND_INI
434     *CompHandIni
435     M_Out(12257) = 0
436     M_Out(12259) = 0
437     M_Out(12261) = 0
438 '
439 '
440 'Dly 5                           'デバッグ用(22/09/30中村)
441     ' ねじ締め機テスト用 ----------
442     Mret% = fScrewTcomChk()
443     If Mret% = -1 Then GoTo *ASSY_ERROR_END
444     'チケットIDを読む
445     M_Out(12262) = 1 Dly 0.5 '本体位置決め出端ON
446     PTemp = P_Curr
447     MRtn = 0
448 '    If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
449 '        If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
450 '            If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
451 '                MRtn = 1
452 '            EndIf
453 '        EndIf
454 '    EndIf
455 '    If MRtn = 1 Then
456 '        Mov PTicketRead
457 '    Else
458 '        Cnt 1 , 10 , 10
459 '        Mov PInitialPosition
460 '        Mov PTicketRead_1           'チケットID読み取り回避点
461 '        Cnt 0
462 '        Mvs PTicketRead             'ID読み位置
463 '    EndIf
464 '
465 ' 2022/04/12 安全方向へ条件変更 渡辺
466 ' PInitialPosition 在席 MStandby=2
467 ' PTicketRead_1 在席 MStandby=1
468 '
469     MStandby = 0    '待機位置フラグを初期化
470     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
471         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
472             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
473                 MStandby = 2
474             EndIf
475         EndIf
476     EndIf
477     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
478         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
479             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
480                 MStandby = 1
481             EndIf
482         EndIf
483     EndIf
484     If MStandby = 2 Then
485         Mov PTicketRead_1           'チケットID読み取り回避点
486         Cnt 0
487     EndIf
488     If MStandby <> 0 Then GoTo *PositionOK
489     fErrorProcess(11,230,281,0)            '初期位置にいない時はエラーにする
490     If M_20# = MNext% Then GoTo *ASSY_ERROR_END
491     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
492     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
493     If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
494     *PositionOK
495 '
496     Mvs PTicketRead             'ID読み位置
497 ' CDに伴い削除 2022/07/27 M.H
498 '    M_Out(12259) = 0            'DVDメカチャック開OfF
499 '    M_Out(12258) = 1            'DVDメカチャック閉ON
500 '
501     '
502     MRtn = 1        'MRtn初期化
503 *RE_TICKET_READ
504 '    MRtn = fnPiasCheck()               'ID読み取り
505 'PInspPosition(1) = PTicketRead  'IDチケット読取位置
506 'MInspGroup%(1) = 1              '検査G番号
507 'MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
508 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
509     MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
510     '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
511     '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
512 EndIf
513 If MRtn = 1 Then GoTo *CompRead
514 '
515     'エラー時製品位置決めを解除
516 *RE_ERR_REL_1
517 If M_20# = MContinue% Then M_20# = MRtn
518 M_Out(12263) = 1 Dly 0.5    '製品位置決めを解除
519 MRtn = frInCheck(11274,1,MSETTIMEOUT05&)
520 '
521 If MRtn = 1 Then GoTo *CompErrorRelease
522 MRtn = M_20#        'M_20#一時避難
523 M_20# = MClear%
524 fErrorProcess(11,234,284,0)     '位置決め戻端エラー
525 If M_20# = MContinue% Then GoTo *RE_ERR_REL_1
526 If M_20# = MNext% Then M_20# = MRtn
527 If M_20# = MNgProcess% Then M_20# = MAbout%
528 *CompErrorRelease
529 '
530 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
531 If M_20# = MNext% Then M_20# = MPass%
532 Mvs PTicketRead_1                         '22/04/07 追加 渡辺
533 GoTo *ASSY_ERROR_END
534 *CompRead
535     fScrewTStart()           '処理位置変更2/27中村)
536 '
537     'パレットから製品を取る
538     '
539     *RE_POSITIONING
540     '
541     M_Out(12262) = 1 Dly 0.5 '本体位置決め出端ON
542 '    Wait M_In(11273) = 1     '本体位置決め出端検出
543     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)   '本体位置決め出端検出
544     If MRtn = 1 Then GoTo *CompPositioning
545     fErrorProcess(11,231,282,0)
546     If M_20# = MNext% Then M_20# = MClear%
547     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
548     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
549     If M_20# = MContinue% Then GoTo *RE_POSITIONING
550     *CompPositioning
551 '
552     Mov PProductOnPltGet_2      '本体受け取り上空回避点
553 '
554 ''    Wait M_In(11888) = 1        'ねじロボ2停止1受信(処理位置変更3/14中村)
555 '    MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
556 '    *RE_ERR_REL_2
557 '    If M_20# = MContinue% Then M_20# = MRtn2
558 '    If MRtn = 0 Then
559 '        MRtn2 = 1       'MRtn2初期化
560 '        M_Out(12263) = 1 Dly 0.5    '製品位置決めを解除
561 '        Mov PInitialPosition  '"イニシャルに戻る動き"
562 '        MRtn2 = frInCheck(11274,1,MSETTIMEOUT05&)
563 '        If MRtn2 = 0 Then
564 '            MRtn2 = M_20#                   '位置決め戻端エラーならM_20#内部を一時避難
565 '            M_20# = MClear%                 'M_20#初期化
566 '            fErrorProcess(11,234,284,0)     '位置決め戻端エラー
567 '            If M_20# = MNext% Then M_20# = MRtn2        'M_20#に避難した値を代入
568 '                '位置決めエラーが発生して工程を抜ける場合停止処理を行う
569 '            If M_20# = MNgProcess% Then M_20# = MAbout%
570 '            Break
571 '        EndIf
572 '        Break
573 '            EndIf
574 '    If M_20# = MContinue% Then GoTo *RE_ERR_REL_2
575 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
576 '
577 '    Mov PProductOnPltGet_1      '本体受け取り上空
578     '
579     *RE_PLT_GET_1
580     '
581     M_Out(12256) = 0            '本体チャック閉OFF
582     M_Out(12257) = 1            '本体チャック開ON
583     '
584 '    Wait M_In(11265) = 1        '本体チャック開検出
585     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
586     If MRtn = 1 Then GoTo *CompPltGet1
587     fErrorProcess(11,244,284,0)
588     If M_20# = MNext% Then M_20# = MClear%
589     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
590     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
591     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
592     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
593     *CompPltGet1
594     '
595     Mov PProductOnPltGet_1      '本体受け取り上空
596     '
597     Ovrd 25
598 '    Fine 0.05 , P
599     Mvs PProductOnPltGet        '本体受け取り位置
600     Dly 0.1
601     M_Out(12257) = 0            '本体チャック開OFF
602     M_Out(12256) = 1            '本体チャック閉ON
603 '    Fine 0 , P
604     '
605     M_Out(12263) = 1 Dly 0.5                    '本体位置決め戻端ON
606 '    Wait M_In(11274) = 1     '本体位置決め戻端検出
607     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)    '本体位置決め戻端検出
608     If MRtn = 1 Then GoTo *CompPltGet2
609     M_Out(12256) = 0                            '本体チャック閉OFF
610     M_Out(12257) = 1                            '本体チャック開ON
611     Dly 2.0
612     Mvs PProductOnPltGet_1
613     Mov PProductOnPltGet_2
614     fErrorProcess(11,234,284,0)
615     If M_20# = MNext% Then M_20# = MClear%
616     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
617     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
618     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
619     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
620     Mov PProductOnPltGet_1
621     Mvs PProductOnPltGet
622     M_Out(12257) = 0                            '本体チャック開OFF
623     M_Out(12256) = 1                            '本体チャック閉ON
624     Dly 2.0
625     *CompPltGet2
626     '
627 '    Wait M_In(11264) = 1        '本体検出
628     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '本体検出
629     If MRtn = 1 Then GoTo *CompPltGet3
630     M_Out(12256) = 0            '本体チャック閉OFF
631     M_Out(12257) = 1            '本体チャック開ON
632     Dly 2.0
633     Mvs PProductOnPltGet_1
634     Mov PProductOnPltGet_2
635     fErrorProcess(11,252,284,0)
636     If M_20# = MNext% Then M_20# = MClear%
637     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
638     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
639     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
640     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
641     Mov PProductOnPltGet_1
642     Mvs PProductOnPltGet
643     M_Out(12257) = 0            '本体チャック開OFF
644     M_Out(12256) = 1            '本体チャック閉ON
645     Dly 2.0
646     *CompPltGet3
647     '
648 '    Wait M_In(11266) = 1        '本体チャック閉検出
649     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
650     If MRtn = 1 Then GoTo *CompPltGet4
651     M_Out(12256) = 0            '本体チャック閉OFF
652     M_Out(12257) = 1            '本体チャック開ON
653     Dly 2.0
654     Mvs PProductOnPltGet_1
655     Mov PProductOnPltGet_2
656     Dly 0.1
657     M_Out(12257) = 0            '本体チャック開OFF
658     M_Out(12256) = 1            '本体チャック閉ON
659     Dly 3.0
660     fErrorProcess(11,245,284,0)
661     If M_20# = MNext% Then M_20# = MClear%
662     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
663     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
664     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
665     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
666     M_Out(12256) = 0            '本体チャック閉OFF
667     M_Out(12257) = 1            '本体チャック開ON
668     Dly 2.0
669     Mov PProductOnPltGet_1
670     Mvs PProductOnPltGet
671     M_Out(12257) = 0            '本体チャック開OFF
672     M_Out(12256) = 1            '本体チャック閉ON
673     Dly 2.0
674     *CompPltGet4
675     '
676     Dly 0.2                     '念のためディレイ
677     Cnt 1 , 10 , 10
678     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
679     Mvs PProductOnPltGet_1      '本体受け取り上空
680     MRtn = FnCtlValue2(99)       '読書開始信号OFF  2022/04/28 渡辺
681     Ovrd 100
682     Mov PProductOnPltGet_2      '本体受け取り上空回避点
683     '
684     '製品をねじロボ2に置く
685     Mov PProductOnRoboSet_3     '経路
686     Cnt 0
687 '    Wait M_In(11888) = 1        'ねじロボ2停止1受信(処理位置,処理変更3/1中村)
688     MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
689     *RE_ERR_REL_2
690     If MRtn = 0 Then
691         Cnt 0
692         Mov PProductOnPltSet_2
693         Mov PProductOnPltSet_1
694         Mvs PProductOnPltSet
695         M_Out(12256) = 0        '本体チャック閉OFF
696         M_Out(12257) = 1        '本体チャック開ON
697         Dly 2.0
698         Mvs PProductOnPltSet_1
699         Mvs PProductOnPltSet_2
700         Mov PInitialPosition
701     EndIf
702     If MRtn = 0 Then GoTo *ASSY_ERROR_END
703     '
704     *RE_ROBO_SET_1
705     '
706     M_Out(12259) = 0            'DVDメカチャック開OfF
707     M_Out(12258) = 1            'DVDメカチャック閉ON
708 '    Wait M_In(11269) = 1        'DVDメカチャック閉検出
709     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
710     If MRtn = 1 Then GoTo *CompRoboSet1
711     fErrorProcess(11,269,284,0)
712     If M_20# = MNext% Then M_20# = MClear%
713     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
714     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
715     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_1
716     *CompRoboSet1
717 '
718     Mvs PProductOnRoboSet_2     'ねじロボ製品置き上空回避点
719 '    Mvs PProductOnRoboSet_4     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)
720     Ovrd 25
721     Mvs PProductOnRoboSet_1     'ねじロボ製品置き上空
722     Mvs PProductOnRoboSet       'ねじロボ製品置き位置
723     M_Out(12866) = 1 Dly 0.3    'ねじロボ2動作再開(停止1～停止2)
724 '    Wait M_In(11889) = 1        'ねじロボ2停止2受信
725     MScrewRoboNgFlg% = 0
726     MRtn = fScrewTighenRoboCheck(11889)    '停止状態を受信する
727     If MRtn = 0 Then
728         MScrewRoboNgFlg% = 1
729     EndIf
730 '
731     *RE_ROBO_SET_2
732 '
733     M_Out(12256) = 0            '本体チャック閉OFF
734     M_Out(12257) = 1            '本体チャック開ON
735 '    Wait M_In(11265)            '本体チャック開検出
736     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
737     If MRtn = 1 Then GoTo *CompRoboSet2
738     fErrorProcess(11,244,284,0)
739     If M_20# = MNext% Then M_20# = MClear%
740     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
741     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
742     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_2
743     *CompRoboSet2
744     '
745     Mvs PProductOnRoboSet_1     'ねじロボ製品置き上空
746     Mvs PProductOnRoboSet_2     'ねじロボ製品置き上空回避点
747 '    Mvs PProductOnRoboSet_4     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)
748     Ovrd 100
749     Cnt 1 , 10 , 10
750     Mov PProductOnRoboSet_3     '経路
751     '
752     If MScrewRoboNgFlg% = 1 Then Mov PInitialPosition
753     If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END
754 '
755 '
756 '
757     '
758     'チルトスライダーを押す
759     Cnt 1 , 10
760     Mov PPushTilt_3             'チルトスライダー押し向き変え回避点
761     Cnt 0
762     Mov PPushTilt_2             'チルトスライダー押し回避点
763     Ovrd 30
764     Mvs PPushTilt_1             'チルトスライダー押し上空
765     Spd 1000
766     Ovrd 5
767     Mvs PPushTilt               'チルトスライダー押し
768     Spd M_NSpd
769     Ovrd 30
770     Cnt 1 , 1 , 1
771     Mvs PPushTilt_1             'チルトスライダー押し上空
772     Cnt 1 , 1 , 10
773     Ovrd 100
774     Mov PPushTilt_2             'チルトスライダー押し回避点
775     '
776     '背面板を取る(コンプライアンスモード実装11/8中村)
777     Mov PPlateBackGet_2         '背面板受け取り上空回避点
778 '    Cnt 1 , 10'暫定
779     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止2～停止3)
780 '    MRtn = fScrewTighenRoboCheck(11890)    '停止状態を受信する'12/20位置の変更(中村)
781 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
782 '    Mov PPlateBackGet_1         '背面板受け取り上空'暫定
783     Cnt 0
784     '
785     *RE_PLATE_GET
786     '
787     Fine 0.05 , P               'ファイン命令ON
788     Ovrd 25
789     Mvs PPlateBackGet           '背面板受け取り位置
790 '    Dly 0.2                     '一時コメントアウト
791     M_Out(12257) = 0            '本体チャック開OFF
792     M_Out(12256) = 1            '本体チャック閉ON
793     '
794 '    Wait M_In(11266) = 1        '本体チャック閉検出
795     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
796     If MRtn = 1 Then GoTo *CompPlateGet_1
797     M_Out(12256) = 0            '本体チャック閉OFF
798     M_Out(12257) = 1            '本体チャック開ON
799     Mvs PPlateBackGet_1
800     fErrorProcess(11,245,293,0) '284→293に変更6/2中村
801     If M_20# = MNext% Then M_20# = MClear%
802     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
803     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
804     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
805     Mvs PPlateBackGet           '背面板受け取り位置
806     M_Out(12257) = 0            '本体チャック開OFF
807     M_Out(12256) = 1            '本体チャック閉ON
808     *CompPlateGet_1
809     Fine 0 , P                  'ファイン命令OFF
810     '
811     Ovrd 5
812     Accel 25 , 100
813     Dly 0.7                     'ディレイ時間調節中(把持力確保)
814 '    CmpG 0.7,0.7,,,,,,       'X,Y軸ゲインを0.7に変更
815 '    ColChk Off                  '衝突検知OFF
816 '    Cmp Pos , &B11          'X,Y軸コンプライアンスモード開始
817     Mov PPlateBackGet_1         '背面板受け取り上空
818     Cnt 1 , 10 , 10
819 '    Cmp Off                     'コンプライアンスモード終了
820 '    ColChk On                   '衝突検知ON
821     Ovrd 50
822     Mov PPlateBackGet_2         '背面板受け取り上空回避点
823     Ovrd 100
824     Accel 100 , 100
825     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '背面パネルチェック
826     If MRtn = 1 Then GoTo *CompPlateGet_2
827     Cnt 0
828     fErrorProcess(11,267,293,0)                     '284→293に変更6/2中村
829     If M_20# = MNext% Then M_20# = MClear%
830     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
831     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
832     If M_20# = MContinue% Then
833         Mov PPlateBackGet_1
834         Dly 0.3
835         M_Out(12256) = 0            '本体チャック閉OFF
836         M_Out(12257) = 1            '本体チャック開ON
837         Dly 2.0
838     EndIf
839     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
840     *CompPlateGet_2    '
841     '背面板を置く
842 '    Wait M_In(11889) = 1        'ねじロボ2停止2受信
843     ColChk Off
844     Mov PPlateBackSet_12        '背面板置き上空
845     Cnt 1 , 10
846 '
847     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        'もう一度背面パネルチェック
848     If MRtn = 1 Then GoTo *CompPlateGet_3
849     fErrorProcess(11,267,293,0)                     '284→293に変更6/2中村
850     If M_20# = MNext% Then M_20# = MClear%
851     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
852     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
853     If M_20# = MContinue% Then
854         Mov PPlateBackGet_2
855         Mov PPlateBackGet_1
856         M_Out(12256) = 0            '本体チャック閉OFF
857         M_Out(12257) = 1            '本体チャック開ON
858         Dly 2.0
859     EndIf
860     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
861     *CompPlateGet_3
862 '
863 ' CDに伴い削除 2022/07/27 M.H
864 '    ' 部品供給要求送信
865 '    M_Out(12787) = 1
866 '
867     MRtn = fScrewTighenRoboCheck(11890)    '停止状態を受信する'12/20位置の変更(中村)
868 '    If MRtn = 0 Then Mov PInitialPosition    '"イニシャルに戻る動き"
869     If MRtn = 0 Then GoTo *ASSY_ERROR_END
870 '
871 '    Mov PPlateBackSet_11        '経路1
872     Ovrd 50
873 '    Mov PPlateBackSet_10        '経路2
874 '    Mov PPlateBackSet_9         '経路3
875 '    Mov PPlateBackSet_8         '経路4
876 '    Mov PPlateBackSet_7         '経路5
877     Mov PPlateBackSet_6         '経路6
878     Cnt 0
879     Ovrd 25
880     Mvs PPlateBackSet_5         '経路7
881     Mvs PPlateBackSet_4         '経路8
882     Mov PPlateBackSet_3         '経路9
883     Mov PPlateBackSet_2         '経路10
884     Mov PPlateBackSet_1         '経路11
885     Mov PPlateBackSet           '背面板置き位置
886     *RE_PLATE_SET
887     M_Out(12256) = 0            '本体チャック閉OFF
888     M_Out(12257) = 1            '本体チャック開ON
889     '
890 '    Wait M_In(11265)            '本体チャック開検出
891     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
892     If MRtn = 1 Then GoTo *CompPlateSet
893     fErrorProcess(11,244,284,0)
894     If M_20# = MNext% Then M_20# = MClear%
895     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
896     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
897     If M_20# = MContinue% Then GoTo *RE_PLATE_SET
898     *CompPlateSet
899     '
900     ColChk On
901     Mov PPlateBackSet_12        '背面板置き上空
902     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～停止4)
903     Ovrd 100
904     '
905 ''    ' 部品供給要求送信(処理位置変更2/27中村)
906 '    M_Out(12787) = 1
907     'ねじロボ製品クランプ固定待ち
908 '    Wait M_In(11891) = 1        'ねじロボ2停止4受信
909 MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
910 'If MRtn = 0 Then Mov PInitialPosition    '"イニシャルに戻る動き"
911 If MRtn = 0 Then GoTo *ASSY_ERROR_END
912     '置き位置画像検査
913 '    Mov PPlateBackCheck_3       '画像検査位置上空
914 '    Mov PPlateBackCheck_2       '通過点
915 '    Mvs PPlateBackCheck         '確認位置
916     '
917     'PInspPosition(2) = PPlateBackCheck
918     'MInspGroup(2) = 2
919     'MRtn = ISInspection(PInspPosition,MInspGroup,2,-1,1)
920     'If MRtn <> 1 Then
921     '   'エラー処理
922     'EndIf
923     '
924 ''    ' 部品供給要求送信
925 '    M_Out(12787) = 1    '処理位置変更
926 '    Mov PPlateBackCheck_3       '画像検査位置上空
927     '
928     'ねじロボ引き込み待ち
929     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～完了)
930     '
931     'DVDメカを取る CDに伴い削除 2022/07/27 M.H
932 *Loop_CW_CCW_S
933     *RE_MECHA_SET_1
934 MRtn = fScrewTighenRoboCheck(11876)    '停止状態を受信する
935 'If MRtn = 0 Then Mov PInitialPosition   '"イニシャルに戻る動き"
936 If MRtn = 0 Then GoTo *ASSY_ERROR_END
937 '
938     *CompRoboGet1
939     '
940 '    Ovrd 50
941     Mov PProductOnRoboGet_3     'ねじロボ製品取り上空回避点2から3へ
942 '    Ovrd 20
943     Mvs PProductOnRoboGet_2     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)4から2へ
944     Ovrd 20
945     Mvs PProductOnRoboGet_1     'ねじロボ製品取り上空
946     Ovrd 10
947     Mvs PProductOnRoboGet       'ねじロボ製品取り位置
948     Dly 0.2
949 '
950     *RE_ROBO_GET_2
951 '
952     M_Out(12257) = 0            '本体チャック開OFF
953     M_Out(12256) = 1            '本体チャック閉ON
954 '
955 '    Wait M_In(11266) = 1        '本体チャック閉検出
956     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
957     If MRtn = 1 Or M_20# = MNext% Then GoTo *CompRoboGet2
958     M_Out(12256) = 0            '本体チャック閉OFF
959     M_Out(12257) = 1            '本体チャック開ON
960     Dly 2.0
961     Mvs PProductOnRoboGet_1
962     Mvs PProductOnRoboGet_2
963     Mov PProductOnRoboGet_3
964     Mov PProductOnRoboGet_4
965     Mov PInitialPosition
966     M_Out(12257) = 0            '本体チャック開OFF
967     M_Out(12256) = 1            '本体チャック閉ON
968     Dly 1.0
969     fErrorProcess(11,245,284,0)
970     If M_20# = MNext% Then MRtn = 1
971     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
972     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
973     M_Out(12256) = 0            '本体チャック閉OFF
974     M_Out(12257) = 1            '本体チャック開ON
975     Dly 2.0
976     Mov PProductOnRoboGet_4
977     Mov PProductOnRoboGet_3
978     Mov PProductOnRoboGet_2
979     Mvs PProductOnRoboGet_1
980     Mvs PProductOnRoboGet
981     If M_20# = MContinue% Or M_20# = MNext% Then GoTo *RE_ROBO_GET_2
982     *CompRoboGet2
983     M_20# = MClear%
984     '
985     Dly 0.2
986     Mvs PProductOnRoboGet_1     'ねじロボ製品取り上空
987     Ovrd 50
988     Mvs PProductOnRoboGet_2     'ねじロボ製品取り上空回避点(9/27暫定コメントアウト)12/15コメント解除
989     Mvs PProductOnRoboGet_3     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)4から3へ
990     Ovrd 100
991     Mov PProductOnRoboGet_4     '経路3から4へ
992     Cnt 1 , 10 , 10
993 '
994     M_Out(12868) = 1 Dly 0.3    'ねじロボ2動作完了を送信
995     *RE_ROBO_GET_3
996 ' CDに伴い削除 2022/07/27 M.H
997 '    M_Out(12258) = 0            'DVDメカチャック閉OFF
998 '    M_Out(12259) = 1            'DVDメカチャック開ON
999 ''    Wait M_In(11268) = 1        'DVDメカチャック開検出
1000 '    MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1001 '    If MRtn = 1 Then GoTo *CompRoboGet3
1002 '    fErrorProcess(11,270,284,0)
1003 '    If M_20# = MNext% Then M_20# = MClear%
1004 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1005 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1006 '    If M_20# = MContinue% Then GoTo *RE_ROBO_GET_3
1007     *CompRoboGet3
1008     '
1009     'パレットへ製品を置く
1010     Mov PProductOnPltSet_2      '本体置き位置上空回避点
1011     Cnt 1 , 10
1012     Mov PProductOnPltSet_1      '本体置き位置上空
1013     Cnt 0
1014     Ovrd 10
1015     Mvs PProductOnPltSet        '本体置き位置
1016     Dly 0.5
1017 '
1018     *RE_PLT_SET
1019 '
1020     M_Out(12256) = 0            '本体チャック閉OFF
1021     M_Out(12257) = 1            '本体チャック開ON
1022 '
1023     Wait M_In(11265) = 1        '本体チャック開検出
1024 '    MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
1025     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
1026     If MRtn = 1 Then GoTo *CompPltSet
1027     fErrorProcess(11,244,284,0)
1028     If M_20# = MNext% Then M_20# = MClear%
1029     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1030         Mvs PProductOnPltSet_1
1031         Mov PProductOnPltSet_2
1032         Mov PInitialPosition
1033     EndIf
1034     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1035     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1036     If M_20# = MContinue% Then GoTo *RE_PLT_SET
1037     *CompPltSet
1038 '
1039     Mvs PProductOnPltSet_1      '本体置き位置上空
1040     Ovrd 100
1041     Mov PProductOnPltSet_2      '本体置き位置上空回避点
1042 '    Mov PInitialPosition        'イニシャルポジション
1043     MRtn = FnCtlValue2(2)          '組立ＯＫ＋１  2022/04/28 渡辺
1044     Mov PTicketRead_1           'チケットID読み取り回避点
1045     MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1046     '
1047     'チケットID書き込み
1048     M_20# = MAssyOK%
1049     *ASSY_ERROR_END
1050     *AssyEnd
1051     *fnAssyStart_FEndPosi
1052 FEnd
1053 '
1054 '■fnPiasCheck
1055 ''' <summary>
1056 ''' PIASチケット読込み
1057 ''' </summary>
1058 ''' <returns>   0 : NG
1059 '''             1 : OK(読込み完了)
1060 ''' </returns>
1061 ''' <remarks>
1062 ''' Date   : 2021/07/07 : M.Hayakawa
1063 ''' </remarks>'
1064 Function M% fnPiasCheck
1065     fnPiasCheck = 0
1066     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1067     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1068 '
1069 *RETRY_PIAS
1070     M_20# = MClear%
1071     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1072     '
1073     '【IDチケット読み込み】
1074     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1075     MInspGroup%(1) = 1              '検査G番号
1076     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1077 '
1078     'エラーの場合
1079     If MRtn <> 1 Then
1080         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1081         If MRtn <> 1 Then
1082             'D720 -> D1300 コピー要求
1083             M_Out(12565) = 1
1084             Dly 0.5
1085             M_Out(12565) = 0
1086             'エラー処理記述
1087             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1088             'GOT KEY入力待ち
1089             MKeyNumber = fnKEY_WAIT()
1090             '
1091             Select MKeyNumber
1092                 Case MNext%         '次へを選択した場合
1093                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1094                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1095                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1096                     Break
1097                 Case MAbout%        '停止を選択した場合
1098                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1099                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1100                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1101                     Break
1102                 Case MNgProcess%    'NGを選択した場合
1103                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1104                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1105                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1106                     Break
1107                 Case MContinue%     '継続を選択した場合
1108                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1109                     M_20# = MContinue%
1110                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1111                     Break
1112             End Select
1113         EndIf
1114     EndIf
1115 '----------D720 -> D1300 コピー要求----------
1116     M_Out(12565) = 1
1117     Dly 0.5
1118     M_Out(12565) = 0
1119 '----------通信確認をする----------
1120     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1121     MRtn = 0                ' 初期化
1122     M_20# = MClear%         ' 初期化
1123     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1124     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1125     If MRtn <> 1 Then
1126         If M_20# = MContinue% Then
1127             GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1128         Else
1129             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1130         EndIf
1131     EndIf
1132 '----------工程抜け確認----------
1133     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1134     MRtn = 0                ' 初期化
1135     M_20# = MClear%         ' 初期化
1136     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1137     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1138     If MRtn <> 1 Then
1139         If M_20# = MContinue% Then
1140             GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1141         Else
1142             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1143         EndIf
1144     EndIf
1145     '
1146     fnPiasCheck = 1
1147     *fnPiasCheck_End
1148 FEnd
1149 '
1150 '■fnPCComuCheck
1151 ''' <summary>
1152 ''' PC-PLC通信チェック
1153 ''' </summary>
1154 ''' <returns>   0 : NG
1155 '''             1 : OK(読込み完了)
1156 ''' </returns>
1157 ''' <remarks>
1158 ''' Date   : 2021/07/07 : M.Hayakawa
1159 ''' </remarks>'
1160 Function M% fnPCComuCheck
1161     fnPCComuCheck = 0
1162     MJudge% = 0                                  '初期化
1163     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1164     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1165     '
1166     For MStaNo = 0 To 5
1167         '
1168         If M_In(MIN_PIAS_ComOK%) = 1 Then
1169             'PC通信OK(M400)
1170             MJudge% = MOK%
1171             MStaNo = 5
1172             Break
1173         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1174             'toRBT_通信確認time out
1175             MJudge% = MNG%
1176             MCommentD1001 = 15
1177             MCommentD1002 = 21
1178             MStaNo = 5
1179             Break
1180         Else
1181             'toRBT_通信確認time out
1182             MJudge% = MNG%
1183             MCommentD1001 = 14
1184             MCommentD1002 = 21
1185             Break
1186         EndIf
1187     Next MStaNo
1188     '
1189     '上記で返信フラグを受信してからPC通信確認OFF
1190     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1191     '
1192     'エラー画面
1193     If MJudge% <> MOK% Then
1194         M_20# = MClear%     '初期化
1195         'エラー処理記述
1196         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1197         'GOT KEY入力待ち
1198         MKeyNumber = fnKEY_WAIT()
1199         '
1200         If MKeyNumber = MAbout% Then            '停止を選択した場合
1201             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1202             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1203             Break
1204         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1205             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1206             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1207             Break
1208         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1209             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1210             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1211             Break
1212         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1213             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1214             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1215             Break
1216         EndIf
1217     Else
1218         'OKの場合
1219         fnPCComuCheck = 1
1220     EndIf
1221 FEnd
1222 '
1223 '■fnProcessCheck
1224 ''' <summary>
1225 ''' 工程抜け確認
1226 ''' </summary>
1227 ''' <returns>    1：工程履歴OK     0：異常終了
1228 '''             -1：前工程履歴NG  -2：自工程履歴あり
1229 '''             -3：モデル仕向NG  -4：タイムアウト
1230 '''             -5：履歴処理エラー
1231 ''' </returns>
1232 ''' <remarks>
1233 ''' Date   : 2021/07/07 : M.Hayakawa
1234 ''' </remarks>'
1235 Function M% fnProcessCheck
1236     fnProcessCheck = 0
1237     MJudge% = MNG%      '一旦NGを初期化とする
1238 '----------工程抜け確認----------
1239     MCommentD1001 = 0   'コメント初期化
1240     For MStaNo = 0 To 5
1241         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1242         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1243         '
1244         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1245             MJudge% = MOK%
1246             fnAutoScreenComment(85)     ' AUTO画面
1247             MStaNo = 5
1248             Break
1249         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1250             MFlgLoop% = 0
1251             MJudge% = MNG%
1252             MCommentD1001 = 27
1253             MCommentD1002 = 22
1254             fnAutoScreenComment(94)     ' AUTO画面
1255             fnProcessCheck = -2         ' NGは-2を返す
1256             MStaNo = 5
1257             Break
1258         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1259            MJudge% = MNG%
1260             MCommentD1001 = 31
1261             MCommentD1002 = 22
1262             fnAutoScreenComment(83)     ' AUTO画面
1263             fnProcessCheck = -3         ' NGは-3を返す
1264             MStaNo = 5
1265             Break
1266         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1267             '履歴NGは直ぐに終了せず繰り返し確認を行う
1268             '前工程の書込みが終了していない可能性があるため
1269             MJudge% = MNG%
1270             MCommentD1001 = 32
1271             MCommentD1002 = 22
1272             fnAutoScreenComment(84)     ' AUTO画面
1273             fnProcessCheck = -1         ' NGは-1を返す
1274             Dly 1.0
1275             '工程抜け確認OFF
1276             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1277             Dly 1.0
1278            'MStaNo = 5
1279             Break
1280         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1281             MFlgLoop% = 0
1282             MJudge% = MNG%
1283             MCommentD1001 = 29
1284             MCommentD1002 = 22
1285             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1286             fnProcessCheck = -5         ' NGは-5を返す
1287             MStaNo = 5
1288             Break
1289         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1290             MJudge% = MNG%
1291             If MCommentD1001 = 32 Then
1292                 '何もしない
1293             Else
1294                 MCommentD1001 = 26
1295             EndIf
1296             MCommentD1002 = 22
1297             fnProcessCheck = -4         ' NGは-4を返す
1298             MStaNo = 5
1299             Break
1300         Else
1301             MJudge% = MNG%
1302             MCommentD1001 = 28
1303             MCommentD1002 = 22
1304         EndIf
1305     Next MStaNo
1306     '工程抜け確認OFF
1307     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1308     '通過履歴NG 工程抜けの場合
1309     If MJudge% = MPass% Then
1310         M_20# = MPass%
1311     EndIf
1312     '
1313     'エラー画面
1314     If MJudge% <> MOK% Then
1315         M_20# = MClear%     '初期化
1316         'エラー処理記述
1317         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1318         'GOT KEY入力待ち
1319         MKeyNumber = fnKEY_WAIT()
1320         '
1321         Select MKeyNumber
1322             Case MAbout%        '停止を選択した場合
1323                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1324                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1325                 Break
1326             Case MNext%         '次へを選択した場合
1327                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1328                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1329                 Break
1330             Case MContinue%     '継続を選択した場合
1331                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1332                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1333                 Break
1334             Case MNgProcess%    'NGを選択した場合
1335                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1336                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1337                 Break
1338         End Select
1339     Else
1340         fnProcessCheck = 1  ' OKは1を返す
1341     EndIf
1342 FEnd
1343 '
1344 '■fnPiasWrite
1345 ''' <summary>
1346 ''' Pias 組立結果書込み要求
1347 ''' </summary>
1348 '''<param name="MFlg%">
1349 '''                 MOK%(1) = 工程履歴にOKを書込む
1350 '''                 MNG%(0) = 工程履歴にNGを書込む
1351 '''</param>
1352 '''<returns></returns>
1353 ''' <remarks>
1354 ''' Date   : 2021/07/07 : M.Hayakawa
1355 ''' </remarks>'
1356 Function M% fnPiasWrite(ByVal MFlg%)
1357       fnPiasWrite = 0
1358 *RETRY_PIASWRITE
1359     '
1360     '組立OK(MOK%)の場合　M306 ON
1361    '組立NG(MNG%)の場合　M307 ON
1362     If MFlg% = MOK% Then
1363         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1364     Else
1365         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1366     EndIf
1367     Dly 0.1                  '念のため
1368     '
1369     'Piasへ書込み開始 M305 -> ON
1370     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1371     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1372     '
1373     MJudge% = MNG%
1374     '
1375     For MStaNo = 0 To 5
1376         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1377             MJudge% = MOK%
1378             'MRet = fnAutoScreenComment(85)  'AUTO画面
1379             MStaNo = 5
1380             Break
1381         '
1382         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1383             MJudge% = MNG%
1384             'MRet = fnAutoScreenComment(85)  'AUTO画面
1385            MCommentD1001 = 34
1386            MCommentD1002 = 25
1387             MStaNo = 5
1388             Break
1389         '
1390         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1391             MJudge% = MNG%
1392             'MRet = fnAutoScreenComment(85)  'AUTO画面
1393            MCommentD1001 = 35
1394            MCommentD1002 = 25
1395             MStaNo = 5
1396             Break
1397         '
1398         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1399             MJudge% = MNG%
1400             'MRet = fnAutoScreenComment(85)  'AUTO画面
1401            MCommentD1001 = 36
1402            MCommentD1002 = 25
1403             MStaNo = 5
1404             Break
1405         '
1406         Else
1407             MJudge% = MNG%
1408            MCommentD1001 = 42
1409            MCommentD1002 = 25
1410         '
1411         EndIf
1412         '
1413     Next MStaNo
1414     '
1415     'Piasへ書込み開始 M305 -> OfF
1416     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1417     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1418     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1419     '
1420     '
1421     '通過履歴NG 工程抜けの場合
1422     If MJudge% = MPass% Then
1423         M_20# = MPass%
1424     EndIf
1425     '
1426    M_20# = MClear%     '初期化
1427     '
1428     'エラー画面
1429     If MJudge% < MOK% Then
1430     '
1431 '残しておくが現状では使用しないラベル
1432 *RETRY_ERR_WRITE
1433         M_20# = MClear%     '初期化
1434         'エラー処理記述
1435         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1436         'GOT KEY入力待ち
1437         MKeyNumber = fnKEY_WAIT()
1438         '
1439         If MKeyNumber = MAbout% Then   '停止を選択した場合
1440             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1441            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1442             Break
1443         '
1444         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1445             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1446             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1447         '
1448         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1449             M_20# = MPass%            'M_20# プログラム間共通外部変数
1450             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1451         '
1452         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1453             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1454            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1455             Break
1456         '
1457         EndIf
1458         '
1459         If M_20# = MClear% Then *RETRY_ERR_WRITE
1460         '
1461     EndIf
1462     '
1463     If M_20# = MContinue% Then *RETRY_PIASWRITE
1464     '
1465     fnPiasWrite = 1
1466     '
1467 FEnd
1468 '
1469 '■fnPCBNumberCheck
1470 ''' <summary>
1471 ''' Pias 基板番号照合要求
1472 ''' </summary>
1473 '''<param name="%"></param>
1474 '''<param name="%"></param>
1475 '''<returns></returns>
1476 ''' <remarks>
1477 ''' Date   : 2021/07/07 : M.Hayakawa
1478 ''' </remarks>'
1479 Function M% fnPCBNumberCheck
1480       fnPCBNumberCheck = 0
1481     '
1482 *RETRY_PCBCHECK
1483     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1484     'Piasへ基板照合開始 M310 -> ON
1485     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1486     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1487     '
1488     MJudge% = MNG%
1489     '
1490     For MStaNo = 0 To 5
1491         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1492             MJudge% = MOK%
1493             fnAutoScreenComment(96)  'AUTO画面
1494             MStaNo = 5
1495             Break
1496         '
1497         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1498             MJudge% = MNG%
1499             fnAutoScreenComment(97)  'AUTO画面
1500             MCommentD1001 = 37
1501             MCommentD1002 = 25
1502             MStaNo = 5
1503             Break
1504         '
1505         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1506             MJudge% = MNG%
1507             fnAutoScreenComment(98)  'AUTO画面
1508             MCommentD1001 = 38
1509             MCommentD1002 = 25
1510             MStaNo = 5
1511             Break
1512         '
1513         ElseIf M_In(11580) = 1 Then                         'time out
1514             MJudge% = MNG%
1515             fnAutoScreenComment(99)  'AUTO画面
1516             MCommentD1001 = 39
1517             MCommentD1002 = 25
1518             MStaNo = 5
1519             Break
1520         '
1521         Else
1522             MJudge% = MNG%
1523            MCommentD1001 = 41
1524            MCommentD1002 = 25
1525         '
1526         EndIf
1527         '
1528     Next MStaNo
1529     '
1530     'Piasへ基板照合開始 M310 -> OfF
1531     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1532     '
1533     '
1534     '通過履歴NG 工程抜けの場合
1535     If MJudge% = MPass% Then
1536         M_20# = MPass%
1537     EndIf
1538     '
1539    M_20# = MClear%     '初期化
1540     '
1541     'エラー画面
1542     If MJudge% < MOK% Then
1543     '
1544 '残しておくが現状では使用しないラベル
1545 *RETRY_ERR_PCBNUMBER
1546         M_20# = MClear%     '初期化
1547         'エラー処理記述
1548         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1549         'GOT KEY入力待ち
1550         MKeyNumber = fnKEY_WAIT()
1551         '
1552         If MKeyNumber = MAbout% Then   '停止を選択した場合
1553             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1554             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1555             Break
1556         '
1557         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1558             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1559             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1560         '
1561         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1562             M_20# = MPass%            'M_20# プログラム間共通外部変数
1563             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1564         '
1565         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1566             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1567             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1568             Break
1569         '
1570         EndIf
1571         '
1572         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1573         '
1574     EndIf
1575     '
1576     If M_20# = MContinue% Then *RETRY_PCBCHECK
1577 FEnd
1578 '
1579 '■ScrewTight_S2
1580 ''' <summary>
1581 ''' ねじ締めを行う
1582 ''' </summary>
1583 '''<param name="PScrewPos()">
1584 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
1585 '''             PScrewPos(2)    ：ねじ締め回避点
1586 '''             PScrewPos(10)   ：ねじ締め終了高さ
1587 '''</param>
1588 '''<returns>整数
1589 '''         0=異常終了、1=正常終了
1590 '''</returns>
1591 ''' <remarks>
1592 ''' Date   : 2021/07/07 : M.Hayakawa
1593 ''' </remarks>'
1594 Function M% ScrewTight_S2(ByVal PScrewPosition())   'ネジ締め個別設定
1595     ScrewTight_S2 = 0
1596     MOKNGFlg = 0
1597     Ovrd 100
1598     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1599     ' 暫定
1600     Ovrd 5
1601     Mvs PScrewPosition(10),-10    ' パレット上ねじ締めS①の上空へ移動
1602 '    Ovrd MOvrdA
1603     '暫定マスク
1604 '    M_Out(Y62_Driver)=1     ' バンクセッティング　C1
1605 '    Dly 0.1
1606 '    M_Out(Y61_Driver)=1     'ドライバーON　CW
1607 '    'Spd 8.3 '外部ライド100  内部ライド60   'ライド100-40　100%：Spd　15　'ねじ締め速度設定
1608 '    Spd MSpdA               'ネジ締め時Spd個別設定
1609     ' 暫定移動のみ
1610     Mvs PScrewPosition(10)
1611 '    '
1612 '    Dly 0.1
1613 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1614 '    Wait M_In(11584)=1          '完了/エラー検出
1615 '    Dly 0.1
1616 '    Spd M_NSpd
1617 '    '
1618 '    If M_In(X28_Driver)=1 Then  'ねじトータルエラー検出時
1619 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1620 '        Dly 0.1
1621 '        M_Out(Y62_Driver)=0     'バンクセッティング解除　C1
1622 '        Dly 0.1
1623 '        M_Out(Y63_Driver)=0     'バンクセッティング解除　C1
1624 '        Dly 0.1
1625 '        M_Out(Y65_Driver)=0     'プログラム解除　F1
1626 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
1627 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
1628 '        MOKNGFlg = -1
1629 '        ScrewTight_S2 = 0
1630 '    Else
1631 '        Wait M_In(X29_Driver)=1 ' 正常完了時
1632 '        Dly 0.1
1633 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1634 '        Dly 0.1
1635 '        M_Out(Y62_Driver)=0     'バンクセッティング解除
1636 '        Dly 0.1
1637 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
1638 '        Dly 0.1
1639 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
1640 '        ScrewTight_S2 = 1
1641 '    EndIf
1642 ' 暫定
1643     Ovrd 10
1644     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1645     Ovrd 100
1646 FEnd
1647 '
1648 '■ScrewGet_S3
1649 ''' <summary>
1650 ''' ねじ供給機からねじを得る
1651 ''' </summary>
1652 '''<param name="%"></param>
1653 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
1654 '''         PScrewPos(2)    ：ねじ供給器回避点
1655 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1656 '''         PScrewPos(3)    ：Mねじポカヨケ位置
1657 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
1658 '''<returns>整数
1659 '''         0=異常終了、1=正常終了、-1=MネジセンサーNG、-2=MネジセンサーON、-3=吸着エラー
1660 '''</returns>
1661 ''' <remarks>
1662 ''' Date   : 2021/07/07 : M.Hayakawa
1663 ''' </remarks>'
1664 Function M% ScrewGet_S3(ByVal PScrewPosition())
1665     ScrewGet_S3 = 0
1666     MMScrewJudge% = 0
1667     'ねじ供給器初期動作エラーチェック
1668 ' ↓暫定削除
1669 '    Wait M_In(X34_ScrewReady1)=1 'ねじ供給器SがReadyになるまで待つ　←　何秒か待ってReadyにならなければ抜けるプログラムが必要？
1670 '    Ovrd 100
1671 '    If M_In(X33_SS2)=0 Then  'Mねじ検出センサがOFF（故障）していた場合
1672 '        Ovrd 30
1673 '        Mvs,-80             'その場所から80mm上空へ移動
1674 '        Mov PInitPos19049   '19049初期位置へ移動
1675 '        M_Out(Y6A_VV1)=0    'ねじ吸着 Off
1676 '        'NGとしてここの関数から抜ける
1677 '        ScrewGet_S3 = -1
1678 '        MMScrewJudge% = 1
1679 '        MCommentD1001 = 61
1680 '    EndIf
1681 '    If ScrewGet_S3 = 0 Then
1682 '        'Sタイト用ねじ供給機にMねじが混入していないか監視
1683 '        MMScrewJudge% = 0 'MMScrewJudgeを初期化する
1684 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
1685 '        If MRtn = 0 Then
1686 '            Ovrd 30
1687 '            Mvs,-80            'その場所から50mm上空へ移動
1688 '            Mov PInitPos19049  '19049初期位置へ移動
1689 '            MMScrewJudge% = 2
1690 '            MRtn = All_CLamp_Release()'全てのクランプ解除へ分岐
1691 '            MCnt% = 2   '2を設定
1692 '            MCommentD1001 = 62
1693 '        EndIf
1694 '        If MMScrewJudge% = 2 Then
1695 '            ScrewGet_S3 = -2
1696 '        EndIf
1697 '    EndIf
1698 '    'Mネジ判定がONの場合 NGとして関数を抜ける
1699 '    If MMScrewJudge% = 2 Then
1700 '        ScrewGet_S3 = -2
1701 '    EndIf
1702     'Sネジ用ねじ太郎のMネジ混入確認用ここまで
1703     Ovrd 100
1704     Spd M_NSpd
1705     If MMScrewJudge% = 0 Then
1706         ScrewGet_S3 = 0
1707         M_Out(Y63_Driver)=1         ' バンクセッティング　C2
1708         MScrewCnt% = 0
1709         MFinCnt% = 2
1710 '        For MCnt% = 0 To MFinCnt%
1711             Mov PScrewPosition(2)        ' ねじ供給機回避点
1712             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1713             Ovrd 80
1714             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1715             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
1716             Mvs PScrewPosition(10), 1.2
1717             M_Out(Y6A_VV1)=1        ' ねじ吸着　ON
1718             'ビット回転
1719             M_Out(Y60_Driver)=1
1720             Dly 0.2
1721             '
1722             Ovrd 100
1723             JOvrd M_NJovrd
1724             Spd M_NSpd
1725             'ネジ吸着確認位置移動
1726             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
1727             Mvs PScrewPosition(10), -15  ' ネジ吸着確認位置
1728             'ビット回転停止
1729             'M_Out(Y60_Driver)=0
1730             '
1731             '1秒間ネジ吸着確認
1732 ' 以下暫定削除
1733 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
1734 '            'MRtn = 0'強制エラー
1735 '            '吸着エラーの場合
1736 '            'ネジをねじ太郎に戻す
1737 '            If MRtn = 0 Then
1738 '                Ovrd 30
1739 '                'ビット回転停止
1740 '                M_Out(Y60_Driver)=0
1741 '                'ネジ供給機上空
1742 '                Mvs PScrewPos(1)
1743 '                '更に上空
1744 '                Mov PScrewPos(1), -75
1745 '                'ネジ捨て位置
1746 '                Mov PScrewFeedS021
1747 '                '吸着OFF
1748 '                M_Out(Y6A_VV1)=0 'ねじ吸着　OFF
1749 '                Dly 0.2
1750 '                '破壊ON
1751 '                M_Out(Y6B_VB1)=1 '真空破壊ON
1752 '                'ビット回転
1753 '                M_Out(Y61_Driver)=1
1754 '                Dly 0.5
1755 '                '
1756 '                Ovrd 100
1757 '                JOvrd M_NJovrd
1758 '                Spd M_NSpd
1759 '                'ドライバーを上下させねじを振り落とす
1760 '                Mov PScrewFeedS021, 10
1761 '                Mov PScrewFeedS021
1762 '                Dly 0.1
1763 '                Mov PScrewFeedS021, 10
1764 '                Mov PScrewFeedS021
1765 '                '
1766 '                'ネジ落ち待ち
1767 '                'ビット回転停止
1768 '                M_Out(Y61_Driver)=0
1769 '                Dly 0.1
1770 '                '破壊OFF
1771 '                M_Out(Y6B_VB1)=0 '真空破壊OFF
1772 '                '
1773 '                '
1774 '                'ねじ落ちたとして、移動更に上空
1775 '                Mov PScrewPos(1), -75
1776 '                Ovrd 100
1777 '                Spd M_NSpd
1778 '                'ネジ供給機上空
1779 '                Mvs PScrewPos(1)
1780 '                '
1781 '                ScrewGet_S3 = -3
1782 '                Break
1783 '                '
1784 '            Else
1785 '                MCnt% = MFinCnt%
1786 '                ScrewGet_S3 = 0
1787 '            EndIf
1788 '        Next  MCnt%
1789         '
1790         Ovrd 100
1791         Spd M_NSpd
1792         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
1793         M_Out(Y60_Driver)=0     ' ビット回転停止
1794         M_Out(Y63_Driver)=0     ' バンクセッティング　C2
1795         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
1796         'もう一度吸着確認
1797 ' 以下暫定削除
1798 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
1799 '        If MRtn = 0 Then      '吸着エラーの場合
1800 '            MCommentD1001 = 94
1801 '            MCommentD1002 = 95
1802 '            ScrewGet_S3 = -3
1803 '        EndIf
1804 '        If MRtn = 1 Then      '吸着OKの場合
1805 '            ScrewGet_S3 = 1
1806 '        EndIf
1807 '        Break
1808     Else
1809         'Mネジ
1810         If MMScrewJudge% = 2 Then
1811             ScrewGet_S3 = -2
1812         EndIf
1813     EndIf
1814 FEnd
1815 '
1816 '■fnKEY_WAIT()
1817 ''' <summary>
1818 ''' GOTからのキー入力待ち
1819 ''' </summary>
1820 '''<returns>1：停止    2：次へ
1821 '''         3：継続    4：トルクチェック開始
1822 '''         5：NG
1823 '''         11：ロボット初期位置1    12：ロボット初期位置2
1824 '''         13：ロボット初期位置3    14：ロボット初期位置4
1825 '''</returns>
1826 ''' <remarks>
1827 ''' Date   : 2021/07/07 : M.Hayakawa
1828 ''' </remarks>'
1829 Function M% fnKEY_WAIT()
1830     fnKEY_WAIT = 0
1831     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
1832     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
1833     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
1834     '下記キー待ちの継続に反応させないため
1835     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
1836     Dly 0.2
1837     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
1838     MLocalLoopFlg=1
1839     While MLocalLoopFlg=1
1840         If M_In(11345) = 1 Then         '停止   M5345
1841             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
1842             fnKEY_WAIT = 1
1843             MLocalLoopFlg=-1
1844             Break
1845         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
1846             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
1847             fnKEY_WAIT = 2
1848             MLocalLoopFlg=-1
1849             Break
1850         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
1851             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
1852             fnKEY_WAIT = 3
1853             MLocalLoopFlg=-1
1854             Break
1855         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
1856             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
1857             fnKEY_WAIT = 4
1858             MLocalLoopFlg=-1
1859             Break
1860         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
1861             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
1862             fnKEY_WAIT = 5
1863             MLocalLoopFlg=-1
1864             Break
1865             '
1866         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
1867             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
1868             fnKEY_WAIT = MRobotInit1%
1869             MLocalLoopFlg=-1
1870             Break
1871             '
1872         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
1873             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
1874             fnKEY_WAIT = MRobotInit2%
1875             MLocalLoopFlg=-1
1876             Break
1877             '
1878         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
1879             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
1880             fnKEY_WAIT = MRobotInit3%
1881             MLocalLoopFlg=-1
1882             Break
1883             '
1884         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
1885             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
1886             fnKEY_WAIT = MRobotInit4%
1887             MLocalLoopFlg=-1
1888             Break
1889             '
1890         Else
1891         EndIf
1892     WEnd
1893     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
1894     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
1895 FEnd
1896 '
1897 '■ fnAUTO_CTL
1898 ''' <summary>
1899 ''' AUTOモードOFF、PLCからの開始待ち
1900 ''' </summary>
1901 ''' <remarks>
1902 ''' Date   : 2021/07/07 : M.Hayakawa
1903 ''' </remarks>
1904 Function M% fnAUTO_CTL
1905     fnAUTO_CTL = 0
1906     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
1907     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
1908     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
1909     '
1910     If M_Svo=0 Then             'サーボON確認
1911         Servo On
1912     EndIf
1913     Wait M_Svo=1
1914 FEnd
1915 '
1916 '■ fnWindScreenOpen
1917 ''' <summary>
1918 ''' ウィンド画面の表示、非表示設定
1919 ''' </summary>
1920 '''<param name="%"></param>
1921 '''<param name="%"></param>
1922 '''<param name="%"></param>
1923 '''<param name="%"></param>
1924 ''' <remarks>
1925 ''' コメントD1001, D1002, D1003の設定
1926 ''' MWindReSet = 0     画面非表示
1927 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
1928 ''' MWindErrScr = 10    エラー画面 D1001, D1002
1929 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
1930 ''' Date   : 2021/07/07 : M.Hayakawa
1931 ''' </remarks>
1932 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
1933     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
1934         M_Out16(12480) = MCommentD1001            'D1001 コメント
1935     EndIf
1936     '
1937     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
1938         M_Out16(12496) = MCommentD1002            'D1002 コメント
1939     EndIf
1940     '
1941     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
1942        M_Out16(12512) = MCommentD1003            'D1003 コメント
1943     EndIf
1944     '
1945     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
1946     M_Out(12363) = 1                         'ウィンド画面設定  M6362
1947     Dly 0.5
1948     M_Out(12363) = 0                         'ウィンド画面設定
1949 FEnd
1950 '
1951 '■FnCtlValue2
1952 ''' <summary>
1953 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
1954 ''' </summary>
1955 ''' <param name="MCtlNo%"></param>
1956 ''' <remarks>
1957 ''' Date : 2022/04/28 渡辺
1958 ''' </remarks>
1959 '''
1960 '''  1：投入数       ＋１
1961 '''  2：組立ＯＫ数   ＋１
1962 '''  3：組立ＮＧ数   ＋１ (未使用)
1963 '''  4：吸着エラー数 ＋１
1964 ''' 99：読書開始信号 OFF
1965 '''
1966 Function M% FnCtlValue2(ByVal MCtlNo%)
1967     FnCtlValue2 = 1
1968     Select MCtlNo%
1969         Case 1        '投入数＋１
1970             M_Out(12569) = 0             '書込み開始信号OFF
1971             M_Out(12568) = 1             '読込み開始信号ON
1972             MInputQty = M_In16(11600)    '投入数受信
1973             MInputQty = MInputQty + 1    '投入数＋１
1974             M_Out16(12592) = MInputQty   '投入数送信
1975             M_Out(12569) = 1             '書込み開始信号ON
1976             Break
1977             '
1978         Case 2        '組立ＯＫ数＋１
1979             M_Out(12569) = 0             '書込み開始信号OFF
1980             M_Out(12568) = 1             '読込み開始信号ON
1981             MAssyOkQty = M_In16(11616)   '組立OK数受信
1982             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
1983             M_Out16(12608) = MAssyOkQty  '組立OK数送信
1984             M_Out(12569) = 1             '書込み開始信号ON
1985             Break
1986             '
1987         Case 4        '吸着エラー数＋１
1988             M_Out(12569) = 0                       '書込み開始信号OFF
1989             M_Out(12568) = 1                       '読込み開始信号ON
1990             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
1991             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
1992             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
1993             M_Out(12569) = 1                       '書込み開始信号ON
1994             Break
1995             '
1996         Case 99        '読書開始信号OFF
1997             M_Out(12568) = 0        '読込み開始信号OFF
1998             M_Out(12569) = 0        '書込み開始信号OFF
1999             Break
2000             '
2001     End Select
2002     Exit Function
2003 FEnd
2004 'Insightによる画像処理検査実行（並列処理なし）
2005 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2006 '-------------------------------------------------------------------------------
2007 'Insightによる画像処理検査実行（並列処理なし）
2008 '   引数
2009 '       PInspPos()      ：検査位置
2010 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2011 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2012 '       MInspCnt%       ：検査位置数
2013 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2014 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2015 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2016 '   戻り値：整数
2017 '       0=異常終了、1=正常終了
2018 '
2019 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2020 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2021 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2022 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2023 '   20200410    :   検査グループ設定Retry追加
2024 '-------------------------------------------------------------------------------
2025     '----- 初期設定 -----
2026     Cnt 0                                                           '移動効率化解除(初期値=0)
2027     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2028 '    Cnt 1,0.1,0.1
2029     '変数宣言・初期化
2030     Def Inte MNum                                                   '検査番号(検査順1～)
2031     MNum% = 1                                                       '検査番号初期値設定
2032     Def Inte MEndFlg                                                '検査終了フラグ
2033     MEndFlg% = 0
2034     '
2035     '検査G番号設定要求・検査実行要求off
2036     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2037     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2038     'エラー番号クリア
2039     MInspErrNum = 0                                                 '検査実行エラー番号
2040     M_Out16(MOUT_InspErrNum) = MInspErrNum
2041     MInspNGStepNum = 0                                              '検査実行NGStep番号
2042     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2043     '
2044     'Insight Ready check?
2045     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2046         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2047         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2048         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2049         ISInspectionSingle = 0                                      '異常終了戻り値設定
2050         Exit Function
2051     EndIf
2052     '
2053     '検査位置数確認
2054     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2055         MInspErrNum = 21                                            '検査データなし 21　引数<1
2056         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2057         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2058         ISInspectionSingle = 0                                      '異常終了戻り値設定
2059         Exit Function
2060     EndIf
2061     '
2062     '
2063     '
2064     '----- メイン処理 -----
2065     '設定された検査位置数分の検査実行
2066     While( MEndFlg% = 0 )
2067         '----- 検査グループ番号設定Retry追加 20200410
2068         MSetGrNumRetryExitFlg = 0
2069         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2070         While( MSetGrNumRetryExitFlg = 0 )
2071         '----- 検査グループ番号設定Retry追加ここまで 20200410
2072             '
2073             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2074             '
2075             '----- 検査グループ番号設定 -----
2076             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2077             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2078             '
2079             '検査位置へ移動・移動完了待ち
2080             Mvs PInspPos( MNum% )                                       '移動
2081             Dly 0.05                                                    '移動完了後Delay
2082             '
2083             '検査グループ番号設定終了確認
2084             M_Timer(1) = 0
2085             MExitFlg = 0
2086             While( MExitFlg = 0 )
2087                 '検査G設定正常終了?
2088                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2089                     MExitFlg = 1
2090                 '
2091                 '検査G設定異常終了?
2092                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2093                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2094                     If MInspErrNum = 0 Then                             '1回目のエラー?
2095                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2096                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2097                     EndIf
2098                     MExitFlg = 1
2099                 '
2100                 'timeoutチェック
2101                 ElseIf 1000 < M_Timer(1) Then
2102                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2103                     If MInspErrNum = 0 Then                             '1回目のエラー?
2104                         MInspErrNum = 12                                'timeout エラー番号=12
2105                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2106                     EndIf
2107                     MExitFlg = 1
2108                 EndIf
2109             WEnd
2110             '
2111             '検査G番号設定要求off
2112             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2113             '
2114             '----- 検査グループ設定Retry追加 20200410
2115             'NGなければ抜ける
2116             If MCurrentStepErr = 0 Then
2117                 MSetGrNumRetryExitFlg = 1
2118             Else
2119                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2120                 If MSetGrNumRetryCnt = 0 Then
2121                     MSetGrNumRetryExitFlg = 1
2122                 Else
2123                     'Retryへ　その前にDelay
2124                     Dly 0.5
2125                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2126                 EndIf
2127             EndIf
2128             '----- 検査グループ設定Retry追加ここまで 20200410
2129             '
2130         WEnd
2131         '
2132         '
2133         '
2134         '----- 検査実行 -----
2135         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2136             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2137                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2138                 MInspRetryExitFlg = 0
2139                 MRetryCnt = 2                                        'Retry回数設定
2140                 While( MInspRetryExitFlg = 0 )
2141                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2142                     '
2143                     '検査完了確認
2144                     MRetryCnt = MRetryCnt - 1
2145                     M_Timer(1) = 0
2146                     MExitFlg = 0
2147                     While( MExitFlg = 0 )
2148                     '検査完了待ち
2149                         '検査OK終了?
2150                         If M_In( MIN_IS_InspOK% ) = 1  Then
2151                             MJudgeOKFlg = 1                         '検査OKフラグON
2152                             MExitFlg = 1
2153                         '
2154                         '検査NG終了?
2155                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2156                             If MInspErrNum = 0 Then                 '1回目のエラー?
2157                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2158                                     MInspErrNum = 32                    '検査NG エラー番号=32
2159                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2160                                 EndIf
2161                             EndIf
2162                             MExitFlg = 1
2163                         '
2164                         '検査異常終了(IS timeout)?
2165                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2166                             If MInspErrNum = 0 Then                 '1回目のエラー?
2167                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2168                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2169                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2170                                 EndIf
2171                             EndIf
2172                             MExitFlg = 1
2173                         '
2174                         'timeoutチェック
2175                         ElseIf 3000 < M_Timer(1) Then
2176                             If MInspErrNum = 0 Then                 '1回目のエラー?
2177                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2178                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2179                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2180                                 EndIf
2181                             EndIf
2182                             MExitFlg = 1
2183                         EndIf
2184                     WEnd
2185                     '
2186                     '検査開始要求off
2187                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2188                     '
2189                     'OKなら抜ける
2190                     If MJudgeOKFlg = 1 Then
2191                         MInspRetryExitFlg = 1
2192                     Else
2193                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2194                         If MRetryCnt = 0 Then
2195                             MInspRetryExitFlg = 1
2196                         Else
2197                             'Retryへ　その前にDelay
2198                             Dly 0.3
2199                         EndIf
2200                     EndIf
2201                     '
2202                 WEnd
2203             EndIf
2204         EndIf
2205         '
2206         '
2207         '
2208         MNum% = MNum% + 1                                           '検査Step+1
2209         '検査終了確認　検査終了フラグセット
2210         If (MInspCnt% < MNum% ) Then
2211             MEndFlg% = 1                                            '検査終了フラグセット
2212         EndIf
2213         'NG発生時続行時処理
2214         If MInspErrNum <> 0 Then                                    'NGあり?
2215             If MNgContinue% <> 1 Then                               'NG続行?
2216                 MEndFlg% = 1                                        '検査終了フラグセット
2217             EndIf
2218         EndIf
2219     WEnd
2220     '
2221     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2222     If 0 < MZAxis% Then
2223         PCurrentPos = P_Curr                                        '現在位置取得
2224         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2225         Mvs PCurrentPos                                             '現在位置上空へ移動
2226     EndIf
2227     Fine 0 , P
2228     '
2229     '戻り値設定
2230     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      'カメラ検査強制OK(M_In(11372)=1)追加(12/21中村)
2231         ISInspectionSingle = 1                                      '正常終了戻り値設定
2232     Else
2233         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2234         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2235         ISInspectionSingle = 0                                      '異常終了戻り値設定
2236     EndIf
2237     '
2238 FEnd
2239 '
2240 ' ■ISInspection
2241 ''' <summary>
2242 ''' Insightによる画像処理検査実行
2243 ''' </summary>
2244 '''<param name="PInspPos()">検査位置</param>
2245 '''<param name="MInspGrNum%()">検査位置での検査グループ番号（=0：画像検査未実施）</param>
2246 '''             PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2247 '''<param name="MInspCnt%">検査位置数</param>
2248 '''<param name="MZAxis%">終了時のZ軸退避座標（-1:無効）</param>
2249 '''             終了時にZ軸をMZAxisで設定された位置まで上昇させる
2250 '''<param name="MNgContinue%">=1で検査エラー・NG発生時に全Stepの検査を行う</param>
2251 '''<returns>    整数 0=異常終了、1=正常終了</returns>
2252 '''         MInspErrNum     ：異常終了時にエラー番号が設定される
2253 '''         MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される"
2254 ''' <remarks>
2255 ''' Date   : 2021/07/07 : M.Hayakawa
2256 ''' </remarks>
2257 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2258 '    '画像使用確認 0<- 画像確認無しの場合
2259 '    If M_In(11369) = 0 Then            'toRBT_使用確認
2260 '        ISInspection = 1                                        '正常終了戻り値設定
2261 '    EndIf
2262 ''
2263 '    Cnt 0                                                       '移動効率化解除(初期値=0)
2264 '    Fine 0.05,P                                                 '位置決め完了条件設置　0.05mm
2265 '    MNum% = 1                                                   '検査番号初期値設定
2266 '    Def Inte MEndFlg                                            '検査終了フラグ
2267 '    MEndFlg% = 0
2268 '    '
2269 '    'エラー番号クリア
2270 '    MInspErrNumSub = 0                                          '検査実行エラー番号sub
2271 '    MInspErrNum = 0                                             '検査実行エラー番号
2272 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2273 '    MInspNGStepNum = 0                                          '検査実行NGStep番号
2274 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2275 '    '
2276 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready offなら終了
2277 '        MInspErrNum = 20                                        '検査実行エラー番号 20 Insight offline
2278 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2279 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2280 '        ISInspection = 0                                        '異常終了戻り値設定
2281 ''
2282 '    EndIf
2283 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2284 '    '
2285 '    '検査位置数確認
2286 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2287 '        MInspErrNum = 21                                        '検査データなし 21　引数<1
2288 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2289 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2290 '        ISInspection = 0                                        '異常終了戻り値設定
2291 ''
2292 '    EndIf
2293 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2294 '    '
2295 '    '設定された検査位置数分の検査実行
2296 '    While( MEndFlg% = 0 )
2297 '        '検査終了確認　検査終了フラグセット
2298 '        If (MInspCnt% < MNum% ) Then
2299 '            MEndFlg% = 1                                        '検査終了フラグセット
2300 '        EndIf
2301 '        '
2302 '        'タスク　検査G番号設定・検査完了確認処理開始　INSPTAST1
2303 '        If MEndFlg% = 0 Then
2304 '            M_01# = MInspGrNum%(MNum%)                          '検査G番号引渡し
2305 '        EndIf
2306 '        M_02# = MEndFlg%                                        '検査終了フラグ引渡し
2307 '        M_05# = MNum%                                           '検査番号(検査順1～)
2308 '        'タスク　検査G設定フラグ引渡し
2309 '        If MEndFlg% = 0 Then
2310 '            If 0 < MInspGrNum%(MNum%) Then
2311 '                M_03# = 1
2312 '            Else
2313 '                M_03# = 0
2314 '            EndIf
2315 '        Else
2316 '            M_03# = 0
2317 '        EndIf
2318 '        'タスク　検査結果確認フラグ引渡し
2319 '        If 1 < MNum% Then
2320 '            If 0 < MInspGrNum%(MNum%-1) Then
2321 '                M_04# = 1
2322 '            Else
2323 '                M_04# = 0
2324 '            EndIf
2325 '        Else
2326 '            M_04# = 0
2327 '        EndIf
2328 '        '
2329 '        'タスク処理開始
2330 '        M_00# = 1                                               'TASK処理開始
2331 '        'タスク処理開始確認
2332 '        M_Timer(1) = 0
2333 '        MExitFlg = 0
2334 '        While( MExitFlg = 0 )
2335 '            '処理開始完了確認
2336 '            If M_00# = 0 And M_10# = 8 Then
2337 '                MExitFlg = 1
2338 '            EndIf
2339 '            'timeoutチェック
2340 '            If 2000 < M_Timer(1) Then
2341 '                If MNgContinue% = 1 Then                        'NG続行?
2342 '                    MInspErrNumSub = 36                         'エラー番号設定36
2343 '                Else
2344 '                    MInspErrNum = 36                            'エラー番号設定36
2345 '                EndIf
2346 '                MExitFlg = 1
2347 '            EndIf
2348 '        WEnd
2349 '        '
2350 '        '検査位置へ移動・移動完了待ち
2351 '        If 0 = MInspErrNum Then
2352 '            If MEndFlg% = 0 Then
2353 '                Mvs PInspPos( MNum% )                           '移動
2354 '            EndIf
2355 '        EndIf
2356 '        '
2357 '        'タスク　検査G番号設定・検査完了確認処理終了待ち　INSPTAST1
2358 '        If 0 = MInspErrNum Then
2359 '            M_Timer(1) = 0
2360 '            MExitFlg = 0
2361 '            While( MExitFlg = 0 )
2362 '                '処理完了待ち（正常終了）
2363 '                If M_10# = 1 Then
2364 '                    MExitFlg = 1
2365 '                EndIf
2366 '                '処理完了待ち（異常終了）
2367 '                If M_10# = 0 Then
2368 '                    If MNgContinue% = 1 Then                    'NG続行?
2369 '                        MInspErrNumSub = M_12#                  'エラー番号設定　M12
2370 '                    Else
2371 '                        MInspErrNum = M_12#                     'エラー番号設定　M12
2372 '                    EndIf
2373 '                    MExitFlg = 1
2374 '                EndIf
2375 '                'timeoutチェック
2376 '                If 5000 < M_Timer(1) Then
2377 '                    If MNgContinue% = 1 Then                    'NG続行?
2378 '                        MInspErrNumSub = 31                     'エラー番号設定31
2379 '                    Else
2380 '                        MInspErrNum = 31                        'エラー番号設定31
2381 '                    EndIf
2382 '                    MExitFlg = 1
2383 '                EndIf
2384 '            WEnd
2385 '        EndIf
2386 '        '
2387 '        '検査結果確認
2388 '        If 0 = MInspErrNum Then
2389 '            If 1 < MNum% Then
2390 '                If 0 < MInspGrNum%(MNum%-1) Then                '検査あり?
2391 '                    If M_11# = 2 Then                           '検査NG?
2392 '                        If MNgContinue% = 1 Then                'NG続行?
2393 '                            If MInspNGStepNum = 0 Then          'NG未発生?
2394 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2395 '                            EndIf
2396 '                            MInspErrNumSub = 32                 'エラー番号設定 32:検査NG
2397 '                        Else
2398 ''                            MInspNGStepNum = MNum% - 1          '検査実行NGStep番号設定
2399 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2400 '                            MInspErrNum = 32                    'エラー番号設定 32:検査NG
2401 '                        EndIf
2402 '                   EndIf
2403 '                EndIf
2404 '            EndIf
2405 '        EndIf
2406 '        '
2407 '        'エラーなら検査中断終了するのでLoopから抜けるため終了フラグセット
2408 '        If 0 <> MInspErrNum Then
2409 '            MEndFlg% = 1
2410 '        EndIf
2411 '        '
2412 '        '検査実行、取込完了待ち
2413 '        If 0 = MInspErrNum Then
2414 '            If MEndFlg% = 0 Then
2415 '                If 0 < MInspGrNum%(MNum%) Then                  '検査あり?
2416 '                    M_Out(MOUT_IS_Insp%) = 1                    '検査実行要求on
2417 '                    '取込完了確認
2418 '                    M_Timer(1) = 0
2419 '                    MExitFlg = 0
2420 '                    While( MExitFlg = 0 )
2421 '                        '処理完了待ち
2422 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2423 '                            MExitFlg = 1
2424 '                        EndIf
2425 '                        'timeoutチェック
2426 '                        If 2000 < M_Timer(1) Then
2427 '                            If MNgContinue% = 1 Then            'NG続行?
2428 '                                MInspErrNumSub = 33             'エラー番号設定33
2429 '                            Else
2430 '                                MInspErrNum = 33                'エラー番号設定33
2431 '                            EndIf
2432 '                            MExitFlg = 1
2433 '                        EndIf
2434 '                    WEnd
2435 '                EndIf
2436 '                '
2437 '            EndIf
2438 '        EndIf
2439 '        MNum% = MNum% + 1
2440 '    WEnd
2441 '    '
2442 '    '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2443 '    If 0 < MZAxis% Then
2444 '        PCurrentPos = P_Curr                                    '現在位置取得
2445 '        PCurrentPos.Z = MZAxis%                                 'Z軸を設定
2446 '        Mvs PCurrentPos                                         '現在位置上空へ移動
2447 '    EndIf
2448 '    '
2449 '    'NG続行時処理
2450 '    If MNgContinue% = 1 Then                                    'NG続行?
2451 '        MInspErrNum = MInspErrNumSub                            'エラー番号設定
2452 '    EndIf
2453 '    '
2454 '    '戻り値設定
2455 '    If MInspErrNum = 0 Then
2456 '        ISInspection = 1                                        '正常終了戻り値設定
2457 '    Else
2458 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2459 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2460 '        ISInspection = 0                                        '異常終了戻り値設定
2461 '    EndIf
2462 '    '
2463 '*ISInspection_End
2464 'FEnd
2465 '
2466 '■InitialZoneB
2467 ''' <summary>
2468 ''' 非常停止後の復帰動作
2469 ''' 1)上空退避　Z方向上に移動
2470 ''' 2)J1軸以外を退避ポジションへ移動
2471 ''' 3)J1軸のみを退避ポジションへ移動
2472 ''' 4)イニシャルポジションへ移動
2473 ''' </summary>
2474 ''' <remarks>
2475 ''' Date : 2022/04/08 : N.Watanabe
2476 ''' </remarks>
2477 Function V fnInitialZoneB()
2478     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中] 2022/04/26 渡辺
2479 '
2480 'パラメータ
2481     Ovrd 5
2482 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2483 '    Cmp Pos, &B100011
2484 '
2485 '復帰動作開始
2486 '
2487 '置き台と両掴みの場所は、チャックを解放する
2488 *RecoveryChuckOpen
2489     PActive = P_Curr          '現在位置を取得
2490     MRecoveryChuckOpen = 0    'チャック解放フラグ 初期化
2491 'PProductOnRoboSet(ねじロボ製品置き位置)は、チャック解放
2492     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2493         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2494             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2495                 MRecoveryChuckOpen = 1
2496             EndIf
2497         EndIf
2498     EndIf
2499 'PProductOnRoboGet(ねじロボ製品取り位置)は、チャック解放
2500     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2501         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2502             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2503                 MRecoveryChuckOpen = 1
2504             EndIf
2505         EndIf
2506     EndIf
2507 '
2508     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2509     M_Out(12256) = 0                           '本体チャック閉OFF
2510     M_Out(12257) = 1                           '本体チャック開ON
2511 '
2512     M_20# = 0                                  'KEY入力初期化
2513     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
2514     If MRtn = 1 Then M_Out(12257) = 0          '本体チャック開OFF
2515     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2516 '
2517     fErrorProcess(11,244,284,0)
2518     If M_20# = MNext% Then M_20# = MClear%
2519     If M_20# = MAbout% Then GoTo *RecoveryEnd
2520     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2521     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2522 '
2523     *RecoveryChuckOpenEnd
2524 '
2525 '背面板回避
2526 'PPlateBackSet～PPlateBackSet_6のエリアにいるときは、本体チャック開く
2527 '・PPlateBackSet_6         '経路6
2528 '・PPlateBackSet_5         '経路7
2529 '・PPlateBackSet_4         '経路8
2530 '・PPlateBackSet_3         '経路9
2531 '・PPlateBackSet_2         '経路10
2532 '・PPlateBackSet_1         '経路11
2533 '・PPlateBackSet           '背面板置き位置
2534 '上記７点のＸ座標・Ｙ座標・Ｚ座標とJ6軸が下記If文の範囲に入っている事を確認する事
2535     PActive = P_Curr                    '現在位置を取得
2536     JActive = J_Curr                    '現在位置を取得
2537     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
2538     If (PActive.X >= -35) And (PActive.X <= -5) Then
2539         If (PActive.Y >= 340) And (PActive.Y <= 510) Then
2540             If (PActive.Z >= 470) And (PActive.Z <= 560) Then
2541                 If (MJ6 >= 160) And (MJ6 <= 200) Then
2542                     M_Out(12256) = 0            '本体チャック閉OFF
2543                     M_Out(12257) = 1            '本体チャック開ON
2544                 Dly 1.0
2545                 EndIf
2546             EndIf
2547         EndIf
2548     EndIf
2549 '
2550 '
2551 '特殊回避　直接、上空退避が出来ない所の対処
2552 '
2553     Ovrd 1
2554 'PProductOnRoboSet(Get)～PProductOnRoboSet(Get)_2のエリアにいるときは、PProductOnRoboSet_2へ
2555 '・PProductOnRoboSet
2556 '・PProductOnRoboSet_1
2557 '・PProductOnRoboSet_2
2558 '・PProductOnRoboGet
2559 '・PProductOnRoboGet_1
2560 '・PProductOnRoboGet_2
2561 '上記６点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
2562     PActive = P_Curr                    '現在位置を取得
2563     JActive = J_Curr                    '現在位置を取得
2564     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
2565     If (PActive.X >= -35) And (PActive.X <= 0) Then
2566         If (PActive.Y >= 350) And (PActive.Y <= 420) Then
2567             If (PActive.Z >= 300) And (PActive.Z <= 450) Then
2568                 If (MJ6 >= -20) And (MJ6 <= 20) Then
2569                     Mvs PProductOnRoboSet_1
2570                     Dly 1.0
2571                     Mvs PProductOnRoboSet_2
2572                     Dly 1.0
2573                     Mov PProductOnRoboSet_3
2574                     Dly 1.0
2575                 EndIf
2576             EndIf
2577         EndIf
2578     EndIf
2579 '
2580 'PProductOnRoboSet(Get)_2～PProductOnRoboSet(Get)_3のエリアにいるときは、PProductOnRoboSet_3へ
2581 '・PProductOnRoboSet_2
2582 '・PProductOnRoboSet_3
2583 '・PProductOnRoboGet_2
2584 '・PProductOnRoboGet_3
2585 '上記４点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
2586     PActive = P_Curr                    '現在位置を取得
2587     JActive = J_Curr                    '現在位置を取得
2588     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
2589     If (PActive.X >= -35) And (PActive.X <= 0) Then
2590         If (PActive.Y >= 280) And (PActive.Y <= 390) Then
2591             If (PActive.Z >= 410) And (PActive.Z <= 570) Then
2592                 If (MJ6 >= -20) And (MJ6 <= 20) Then
2593                     Mvs PProductOnRoboSet_3
2594                     Dly 1.0
2595                 EndIf
2596             EndIf
2597         EndIf
2598     EndIf
2599 '
2600     Ovrd 5
2601 '
2602 '上空退避
2603     PActive = P_Curr
2604     Pmove = PActive
2605     Pmove.Z = 640           '上空退避する一律の高さ
2606     If PActive.X > 550 Then
2607         Pmove.Z =550        'パレット上に腕を伸ばしているときは640まで上げられない為、例外処置
2608     EndIf
2609     If PActive.Z < Pmove.Z Then
2610         Mvs Pmove
2611     EndIf
2612     Dly 1.0
2613 'J1軸以外を退避ポジションへ移動
2614     JActive = J_Curr
2615     Jmove = JTaihi
2616     Jmove.J1 = JActive.J1        'J1軸は現在値を使用し、JTaihiのポーズを取る
2617     Jmove.J6 = JActive.J6        'J6軸は現在値を使用し、JTaihiのポーズを取る
2618     Mov Jmove
2619     Dly 1.0
2620 'J1軸のみを退避ポジションへ移動
2621     Mov JTaihi
2622     Dly 1.0
2623 'イニシャルポジションへ移動
2624     Mov PInitialPosition
2625     Cmp Off
2626     Ovrd 100
2627 ' ねじロボを初期位置に戻すために強制的に自動運転開始
2628     If M_In(11856) = 0 Then                 ' 停止中のみ
2629         fnAutoScreenComment(501)            ' 状態表示[ネジ締め機自動運転開始中] 2022/04/25 渡辺
2630         M_Out(12834) = 1                    ' 自動運転開始ON 12834   M6834
2631         MRet = frInCheck(11842, 1, 30000&)  ' 自動運転開始受信待ち    11842   M5842
2632         If MRet = 0 Then
2633         Else
2634             M_Out(12834) = 0    ' 自動運転開始OFF 12834   M6834
2635         EndIf
2636     EndIf
2637     M_Out(12262) = 0            '位置決め出OFF
2638     M_Out(12263) = 1            '位置決め戻ON
2639     fErrorProcess(11,253,281,0)
2640 *RecoveryEnd
2641     Exit Function
2642 FEnd
2643 '
2644 '
2645 '■fnAutoScreenComment
2646 ''' <summary>
2647 ''' メイン画面の動作状況表示
2648 ''' コメントD1005の設定
2649 ''' </summary>
2650 '''<param name="McommentD1005%">コメントID</param>
2651 ''' <remarks>
2652 ''' Date   : 2021/07/07 : M.Hayakawa
2653 ''' </remarks>
2654 Function fnAutoScreenComment(ByVal McommentD1005%)
2655     M_Out16(12576) = McommentD1005%
2656 FEnd
2657 '
2658 '■fnRoboPosChk
2659 ''' <summary>
2660 ''' 最後に終了したロボットポジションの確認
2661 ''' </summary>
2662 '''<param name="MINNumber%">入力番号</param>
2663 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2664 '''<param name="MTimeCnt&">タイムアウト時間</param>
2665 ''' PLCに保続した番号を読込み、確認
2666 ''' MRBTOpeGroupNo = 5 が初期位置に設定
2667 '''<returns>整数 0:タイムアウト 1:OK</returns>
2668 ''' <remarks>
2669 ''' Date   : 2021/07/07 : M.Hayakawa
2670 ''' </remarks>
2671 Function M% fnRoboPosChk
2672     fnRoboPosChk = 0
2673     MRet = fnStepRead()
2674     '初期位置でないと判断した場合
2675     'ウィンド画面切換え
2676     If MRBTOpeGroupNo > 5 Then
2677         '下記キー待ちの継続に反応させないため
2678         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2679         Dly 0.2
2680         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2681         Dly 1.5
2682         '
2683         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
2684         '
2685         MLoopFlg% = 1
2686         While MLoopFlg% = 1
2687             '
2688             '
2689             MKeyNumber% = fnKEY_WAIT()
2690             Select MKeyNumber%
2691                 Case Is = MAbout%       '停止
2692                     M_20# = MAbout%
2693                     MLoopFlg% = -1
2694                     Break
2695                 Case Is = MNext%        '次へ
2696                     'MLoopFlg% = -1
2697                     Break
2698                 Case Is = MContinue%    '継続
2699                     M_20# = MContinue%
2700                     MLoopFlg% = -1
2701                     Break
2702                 Default
2703                     Break
2704             End Select
2705         WEnd
2706     EndIf
2707     '
2708     If M_20# = MContinue% Then                              '継続ボタンが押された場合
2709         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
2710         Ovrd 5                                   '低速オーバーライド値設定
2711         Select MRBTOpeGroupNo
2712             Case Is = 5                          '何もしない
2713                 Break
2714             Case Is = 10                         '初期位置へ戻す
2715                 'Mov PTEST001
2716                 Break
2717             Case Is = 15                         '初期位置へ戻す
2718                 'Mov PTEST002
2719                 Dly 0.5
2720                 'Mov PTEST001
2721                 Dly 0.5
2722                 Break
2723             Default
2724                 Break
2725         End Select
2726         '
2727         Ovrd M_NOvrd                            'システムの初期値を設定
2728         M_Out(12364) = 1                        'toPLC_データ保存ON
2729         MRBTOpeGroupNo = 5
2730         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
2731         Dly 1.0
2732         M_Out(12364) = 0                        'toPLC_データ保存OFF
2733         fnRoboPosChk = 1                        '初期位置動作実行
2734         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
2735     EndIf
2736     Exit Function
2737 FEnd
2738 '
2739 '■frInCheck
2740 ''' <summary>
2741 ''' センサーINチェック
2742 ''' </summary>
2743 '''<param name="MINNumber%">入力番号</param>
2744 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2745 '''<param name="MTimeCnt&">タイムアウト時間</param>
2746 '''<returns>整数 0:タイムアウト 1:OK</returns>
2747 ''' <remarks>
2748 ''' Date   : 2021/07/07 : M.Hayakawa
2749 ''' </remarks>
2750 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2751     M_Timer(4) = 0
2752     MloopFlg = 0
2753     While MloopFlg = 0
2754         MCrtTime& = M_Timer(4)
2755         If M_In(MINNumber%) = MCMPFLG% Then
2756             MloopFlg = 1
2757             frInCheck = 1
2758         ElseIf MCrtTime& > MTimeCnt& Then
2759             MloopFlg = 1
2760             frInCheck = 0
2761         EndIf
2762     WEnd
2763 FEnd
2764 '-----------------------------------------------
2765 '
2766 'ねじ締め機通信確認
2767 '
2768 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2769 'fScrewTcomChk = 0　：正常終了
2770 '          　 　 -1 ：異常終了
2771 '-----------------------------------------------
2772 Function M% fScrewTcomChk
2773 *ReCheckScewTcomChk
2774     fScrewTcomChk = 0
2775     '通信確認送信
2776     M_Out(MOUT_ScwT_ComChk%) = MOn%
2777     '通信確認受信待機
2778 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
2779     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
2780     '通信確認送信終了
2781     M_Out(MOUT_ScwT_ComChk%) = MOff%
2782     If MRtn = 0 Then
2783         fScrewTcomChk = -1
2784     EndIf
2785     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
2786  '
2787 FEnd
2788 '
2789 '
2790 '-----------------------------------------------
2791 '
2792 'ねじ締め開始送信
2793 '
2794 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2795 'fScrewTStart = 0　：正常終了
2796 '           　　-1 ：異常終了
2797 '-----------------------------------------------
2798 Function M% fScrewTStart
2799     fScrewTStart = 0
2800     nRet% = 0
2801     'ねじ締め開始待機を受信
2802 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
2803     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
2804     If MRtn = 0 Then nRet% = -1
2805     If MRtn = 0 Then GoTo *ScrewStartERROR      '開始できなかった場合ジャンプ
2806     Dly 0.1
2807     'ねじ締め開始受信を送信
2808     M_Out(MOUT_ScwT_ST%) = MOn%
2809     Dly 0.5
2810     'Wait M_In(MTEST_KEY%) = MOn%
2811     'ねじ締め開始送信終了
2812     M_Out(MOUT_ScwT_ST%) = MOff%
2813     '
2814 *ScrewStartERROR
2815     fScrewTStart = nRet%
2816 FEnd
2817 '
2818 '
2819 '
2820 '-----------------------------------------------
2821 '
2822 'ねじ締め完了受信
2823 '
2824 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2825 'fScewTFinish = 0　：正常終了
2826 '          　 　-1 ：異常終了
2827 '-----------------------------------------------
2828 Function M% fScewTFinish
2829 *ReCheckScewTFinish
2830     fScewTFinish = 0
2831     'ねじ締め完了待機を受信
2832 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
2833     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
2834     If MRtn = 0 Then
2835         fScewTFinish = -1
2836     EndIf
2837     If MRtn = 2 Then GoTo *ReCheckScewTFinish
2838     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
2839     Dly 0.1
2840     'ねじ締め完了受信を送信
2841     M_Out(MOUT_ScwT_FinOK%) = MOn%
2842     Dly 0.5                          'とりあえず保持時間0.5msec
2843     'ねじ締め開始送信終了
2844     M_Out(MOUT_ScwT_FinOK%) = MOff%
2845     'Wait M_In(MTEST_KEY%) = MOn%
2846     '
2847 *ScewTFinish_ErrEnd
2848 FEnd
2849 '
2850 '
2851 '-----------------------------------------------
2852 '
2853 '条件xx停止受信
2854 '
2855 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2856 'fScewTCaseStop = 0　：正常終了
2857 '          　   　-1 ：異常終了
2858 '-----------------------------------------------
2859 Function M% fScewTCaseStop(ByVal MCase%())
2860 *ReCheckScewTCaseStop
2861     fScewTCaseStop = 0
2862     '条件xx停止を受信
2863     Wait M_In(MCase%(1)) = MOn%
2864     MRtn = fTimeOutJudge(MCase%(1),MOn%)
2865     If MRtn = 0 Then
2866         fScewTCaseStop = -1
2867     EndIf
2868     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
2869     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
2870     Dly 0.1
2871     '条件xx停止受信を送信
2872     M_Out(MCase%(2)) = MOn%
2873     Dly 0.5                          'とりあえず保持時間0.5msec
2874     'ねじ締め開始送信終了
2875     M_Out(MCase%(2)) = MOff%
2876 *ScewTCaseStop_ErrEnd
2877     '
2878 FEnd
2879 '
2880 '■fScrewTighenRoboCheck
2881 '<summary>
2882 'ねじロボ監視
2883 '</summary>
2884 '<param name = "MStopNum%"> 停止番号</param>
2885 '<returns>整数 0:ねじロボ異常終了 1:OK </returns>
2886 '<make>
2887 '2021/12/2 中村天哉
2888 '</make>
2889 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
2890     fnAutoScreenComment(503)    '状態表示[ねじロボ動作終了待ち] 2022/04/26 渡辺
2891     fScrewTighenRoboCheck = 1
2892     MScrewTighenRoboFlg% = 1    'フラグの初期化
2893     MCheck% = 0
2894     While MScrewTighenRoboFlg% = 1
2895         MCheck% = M_In16(11904)
2896         If M_In(MStopNum%) = 1 Then '停止位置まで来たら
2897             MScrewTighenRoboFlg% = 0 '関数を抜ける
2898             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
2899         EndIf
2900         If MCheck% <> 0 Then
2901             fScrewTighenRoboError(MCheck%)
2902             Select M_20#
2903                 Case MAbout%            '停止が押された場合
2904                     M_Out(12869) = 1 Dly 1.0
2905                     MScrewTighenRoboFlg% = 0
2906                     fScrewTighenRoboCheck = 0   '異常終了
2907                     Break
2908                 Case MNgProcess%        'NGが押された場合
2909                     M_Out(12873) = 1 Dly 1.0
2910                     MScrewTighenRoboFlg% = 0
2911                     fScrewTighenRoboCheck = 0   '異常終了
2912                     Break
2913                 Case MContinue%             'リトライが押された場合
2914                     M_20# = MClear%         'M_20#初期化
2915                     M_Out(12871) = 1 Dly 1.0
2916                     Break
2917                 Case MNext%                 '次へが押された場合
2918                     M_20# = MClear%         'M_20#初期化
2919                     M_Out(12874) = 1 Dly 1.0
2920                     Break
2921             End Select
2922             Dly 0.5
2923         EndIf
2924     WEnd
2925 FEnd
2926 '
2927 '■fScrewTighenRoboError
2928 '<summary>
2929 'ねじロボエラー処理
2930 '</summary>
2931 '<param name = "ErrorCode%"> エラー番号</param>
2932 '<make>
2933 '2021/12/2 中村天哉
2934 '</make>
2935 Function fScrewTighenRoboError(ByVal MErrorCode%)
2936     MErrorScreenCode% = 0
2937     MErrorScreenCode% = MErrorCode% + 300
2938     fErrorProcess(11,MErrorScreenCode%,0,0)
2939 FEnd
2940 '
2941 '■fErrorProcess
2942 '<summary>
2943 'エラー処理
2944 '</summary>
2945 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
2946 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
2947 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
2948 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
2949 '<make>
2950 '2021/11/5 中村天哉
2951 '</make>
2952 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2953     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
2954     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
2955     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
2956     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
2957 *RETRY_ERR_PROCESS
2958      M_20# = MClear%     '初期化
2959 '        'エラー処理記述
2960         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2961 '        'GOT KEY入力待ち
2962         MKeyNumber = fnKEY_WAIT()
2963 '        '
2964         If MKeyNumber = MAbout% Then   '停止を選択した場合
2965             M_20# = MAbout%            'M_20# プログラム間共通外部変数
2966             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2967             Break
2968          '
2969         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
2970             M_20# = MContinue%            'M_20# プログラム間共通外部変数
2971             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2972         '
2973         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
2974             M_20# = MNext%            'M_20# プログラム間共通外部変数
2975             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2976          '
2977         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
2978             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
2979             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2980             Break
2981         '
2982         EndIf
2983         '
2984         If M_20# = MClear% Then *RETRY_ERR_PROCESS
2985 FEnd
2986 '
2987 '■fnTorqueCheck
2988 ''' <summary>
2989 ''' トルクチェック動作用のメイン
2990 ''' </summary>
2991 ''' <remarks>
2992 ''' Date   : 2021/12/21 : H.AJI
2993 ''' </remarks>'
2994 Function M% fnTorqueCheck
2995     'トルクチェック中送信  搬送系停止
2996     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
2997     '
2998     fnTorqueCheck = 0
2999     Ovrd 20
3000     Mov PInitialPosition              '初期位置移動
3001     Ovrd 100
3002     '下記キー待ちの継続に反応させないため
3003     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3004     Dly 0.2
3005     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3006     '
3007     'M6340  トルクチェック受信
3008     'Dly 5.0
3009     M_Out(12340) = 1          'トルクチェック受信 M6340
3010     Dly 1.0
3011     M_Out(12340) = 0
3012     '
3013     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
3014     M_Out(12835) = 1                         'ねじロボトルクチェック画面切替
3015    Wait M_In(11843) = 1                         'ねじロボトルクチェック画面切替
3016     M_Out(12835) = 0                         'ねじロボトルクチェック画面切替完了
3017     '
3018     '
3019     MLoopFlg = 1
3020     While MLoopFlg = 1
3021         '
3022         Mov PInitialPosition              '初期位置移動
3023         '
3024         MKeyNumber = fnKEY_WAIT()
3025         Select MKeyNumber
3026             Case Is = 1           '停止
3027                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
3028                 Dly 1.0
3029                 M_Out(12343) = 0
3030                 Ovrd 20
3031                 'Mov PTicketRead_1
3032                 M_Out(12840) = 1          'トルクチェック終了
3033                 Wait M_In(11859) = 1      'ねじロボからの終了
3034                 M_Out(12840) = 0          'トルクチェック終了
3035                 Ovrd 100
3036                 M_20# = 1
3037                 MLoopFlg = -1
3038                 Break
3039             Case Is = 2           '次へ
3040                 Break
3041             Case Is = 3           '継続
3042                 Break
3043             Case Is = 4           'トルクチェック開始
3044                 M_Out(12342) = 1          'トルクチェック開始要求受信 M6342
3045                 Dly 1.0
3046                 M_Out(12342) = 0
3047                 If M_In(11862) = 1 Then             'トルクチェッカー確認
3048                     fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
3049                     MRet = fnScrewMTorque()           'ねじロボ用トルクチェック
3050                 EndIf
3051                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
3052                 'MRet = fnMoveTorquePosi()
3053                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
3054                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3055                 Break
3056             Default
3057                 Break
3058         End Select
3059     WEnd
3060     '
3061     'トルクチェック中停止送信
3062     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
3063     '
3064     'ロボットの位置を元に戻す
3065     '
3066     '
3067  FEnd
3068  '
3069 '
3070 '
3071 '---------------------------
3072 '
3073 '    メイン画面の表示、非表示設定
3074 '         コメントD1001, D1002, D1003の設定
3075 '           MWindReSet = 0     画面非表示
3076 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
3077 '           MWindErrScr = 10    エラー画面 D1001, D1002
3078 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
3079 '
3080 '---------------------------
3081 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3082     fnMainScreenOpen = 0
3083     '
3084    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
3085         M_Out16(12480) = MCommentD1001            'D1001 コメント
3086     EndIf
3087     '
3088     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
3089         M_Out16(12496) = MCommentD1002            'D1002 コメント
3090     EndIf
3091     '
3092     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
3093         M_Out16(12512) = MCommentD1003            'D1003 コメント
3094     EndIf
3095     '
3096     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
3097     M_Out(12362) = 1                         'ウィンド画面設定  M6362
3098     Dly 0.5
3099     M_Out(12362) = 0                         'ウィンド画面設定
3100 FEnd
3101 '
3102 '■Main
3103 ''' <summary>
3104 ''' トルクチェック実動作
3105 ''' </summary>
3106 ''' <remarks>
3107 ''' Date   : 2021/12/21 : H.AJI
3108 ''' </remarks>'
3109 Function M% fnScrewMTorque
3110     fnScrewMTorque = 0
3111     M_Out(12838) = 1                         'トルクチェック開始1
3112     Wait M_In(11857) = 1                     '受信完了
3113     M_Out(12838) = 0                         'トルクチェック開始1
3114     Dly 2.0
3115 FEnd
3116 '
3117 '
3118 '----------------------------------------------------------------
3119 'fTimeOutJudge
3120 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3121 '引数
3122 'Address% = 監視アドレス番号
3123 'JudgeFlg% = 対象アドレスの正常終了時の値
3124 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3125 '戻り値 = 0 エラー
3126 '         1 正常終了
3127 '         2 リトライ
3128 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3129 '作成日
3130 '2022/9/20 中村
3131 '----------------------------------------------------------------
3132 '
3133 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3134     fTimeOutJudge = 0
3135     MJudge% = 1
3136     MRtn = 0
3137     M_20# = MClear%
3138     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3139 *TimeOutLoop
3140     If MRtn = 1 Then GoTo *TimeOut
3141         fErrorProcess(11,202,203,0)
3142         If M_20# = MNext% Then GoTo *TimeOutLoop
3143         If M_20# = MContinue% Then MJudge% = 2
3144         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3145 *TimeOut
3146     fTimeOutJudge = MJudge%
3147 '
3148 *JUDGE_ERROR_END
3149 FEnd
3150 '■Main
3151 ''' <summary>
3152 ''' 組立動作用のメイン
3153 ''' </summary>
3154 ''' <remarks>
3155 ''' Date   : 2021/07/07 : M.Hayakawa
3156 ''' </remarks>'
3157 Function Main
3158     MopeNo = M_21#         '外部変数にて動作番号代入
3159     '
3160     If M_Svo=0 Then
3161         Servo On
3162     EndIf
3163     Wait M_Svo=1
3164 '組立スタート日付時刻要求パルスON
3165     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3166 'パトライト操作
3167     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
3168     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
3169     '
3170     M_20# = 0                                   'KEY入力初期化
3171     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
3172     MRet% = 0
3173 '初期位置の確認と移動
3174 '
3175 '復帰動作　実行・未実行判別      2022/04/08 渡辺 作成
3176     PActive = P_Curr                    '現在位置を取得
3177     MRecoveryPass% = 0
3178     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3179         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3180             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3181                 MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
3182             EndIf
3183         EndIf
3184     EndIf
3185     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3186         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3187             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3188                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
3189             EndIf
3190         EndIf
3191     EndIf
3192     If MRecoveryPass% = 0 Then
3193        fnInitialZoneB()        '復帰動作パスフラグが立っていない時は復帰動作を実行
3194     EndIf
3195 '
3196     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
3197         M_Out(12364) = 1            'toPLC_データ保存ON
3198 'トルクチェック
3199         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3200             MRet% = fnTorqueCheck()
3201             Break
3202         Else
3203 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
3204 '                MRtn = InspInit()               '画像処理初期化処理
3205 '            EndIf
3206 '
3207             M_20# = MClear%             '初期化
3208 '組立開始
3209             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3210                 fnAssyStart()
3211             Else
3212                 M_20# = MPass%
3213             EndIf
3214 '組立終了日付時刻
3215             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
3216             Wait M_In(11572) = 1            '日付取得完了
3217             Dly 0.1
3218             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
3219 'リフターユニットへのOUT
3220             '  KEY入力が何もない場合 OKと判断
3221             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
3222             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
3223 'OK/NGフラグ出力
3224             If M_20# <= 0 Then
3225                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
3226             ElseIf M_20# = MPass% Then
3227                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
3228             EndIf
3229 'PIASに組立完了書込み
3230             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3231                 If M_20# = MPass% Then
3232                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3233                 Else
3234                     'KEY入力がNGの場合
3235                     If M_20# = MNgProcess% Then
3236                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3237                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3238                         MRet% = fnPiasWrite(MNG%)
3239                        nAssyNgQty = nAssyNgQty + 1
3240                     EndIf
3241                     '
3242                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/17中村)
3243                     If M_20# = MAssyOK% Then
3244                             '-----------------------
3245                             'D732 -> D2600 コピー要求
3246                             M_Out(12566) = 1
3247 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3248                             M_Out(12566) = 0
3249                             '
3250                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3251                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3252                             '基板番号照合(PPは未使用）
3253 '                            MRet% = fnPCBNumberCheck()
3254                         Else
3255                             MRet% = 1
3256                         EndIf
3257                         '
3258                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3259                             If M_20# <> MAbout% Then
3260                                 '工程履歴OK書き込み
3261                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3262                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3263                                 MRet% = fnPiasWrite(MOK%)
3264                                 nAssyOkQty = 0
3265                                 nAssyOkQty = nAssyOkQty + 1
3266                             Else
3267                                 nAssyOkQty = nAssyOkQty + 1
3268                             EndIf
3269                         EndIf
3270                     EndIf
3271 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3272 '                    MRet% = fnPiasWrite(MOK%)
3273                 EndIf
3274             Else
3275                 nAssyOkQty = nAssyOkQty + 1
3276             EndIf
3277             '
3278             '組立終了日付時刻解除
3279             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3280             '投入数、組立OK数、組立NG数書込み
3281 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3282             '
3283 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3284 '                '画像処理終了処理
3285 '                MRtn = InspQuit()
3286 '            EndIf
3287         EndIf
3288         M_Out(12364) = 0                          'toPLC_データ保存OFF
3289     EndIf
3290 'パトライト操作
3291     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3292     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3293 'GOT表示
3294     fnAutoScreenComment(93)  'AUTO画面 工程完了
3295 FEnd
3296 End
3297 '
3298 'おまじないコメント
3299 '絶対削除するな
3300 '
3301 '
3302 '
3303 '
3304 '
3305 '
3306 '
PInspPosition(1)=(+602.00,-150.00,+500.00,+180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
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
Pmove=(-17.62,+286.22,+640.00,-179.82,-0.29,+90.49,+0.00,+0.00)(7,1048576)
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
PPlateBackCheck=(-90.21,+513.03,+577.72,-180.00,+0.00,-90.00)(7,0)
PPlateBackCheck_2=(+66.39,+429.86,+577.75,+180.00,+0.00,-90.00)(7,0)
PPlateBackCheck_3=(-18.78,+286.22,+630.88,+180.00,+0.00,-90.00)(7,0)
PPlateBackCheck_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet=(+477.48,+103.78,+401.13,+179.78,+0.01,+178.57)(7,0)
PPlateBackGet_1=(+477.48,+103.78,+430.00,+179.78,+0.01,+178.57)(7,0)
PPlateBackGet_2=(+477.48,+103.78,+560.00,+179.78,+0.01,+178.57)(7,0)
PPlateBackGet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackSet=(-18.00,+492.33,+543.82,-179.86,-0.33,+89.00)(7,1048576)
PPlateBackSet_1=(-18.00,+446.52,+535.30,-179.37,-13.97,+88.96)(7,1048576)
PPlateBackSet_10=(-18.77,+363.66,+487.69,-179.91,-41.20,+90.53)(7,1048576)
PPlateBackSet_11=(-18.77,+362.28,+493.38,+179.84,-39.34,+90.58)(7,1048576)
PPlateBackSet_12=(-17.62,+286.22,+630.90,-179.82,-0.29,+90.49)(7,1048576)
PPlateBackSet_2=(-18.00,+402.05,+517.82,-179.10,-27.37,+88.87)(7,1048576)
PPlateBackSet_3=(-18.00,+376.88,+500.39,-178.89,-36.13,+88.75)(7,1048576)
PPlateBackSet_4=(-18.83,+358.54,+485.67,-179.10,-42.67,+88.56)(7,1048576)
PPlateBackSet_5=(-18.90,+355.89,+483.22,-179.10,-42.67,+88.56)(7,1048576)
PPlateBackSet_6=(-18.90,+352.51,+487.90,-179.10,-42.67,+88.56)(7,1048576)
PPlateBackSet_7=(-18.83,+378.77,+503.51,+179.34,-35.63,+90.89)(7,1048576)
PPlateBackSet_8=(-18.81,+374.94,+499.62,+179.31,-36.77,+90.90)(7,1048576)
PPlateBackSet_9=(-18.81,+372.36,+496.87,+179.85,-37.92,+90.58)(7,1048576)
PProductOnPltGet=(+479.30,-99.28,+372.11,+179.79,-0.35,+178.62)(7,0)
PProductOnPltGet_1=(+479.30,-99.28,+410.00,+179.79,-0.35,+178.62)(7,0)
PProductOnPltGet_2=(+479.30,-99.28,+500.00,+179.79,-0.35,+178.62)(7,0)
PProductOnPltGet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet=(+478.90,-98.88,+372.11,+179.79,-0.35,+178.62)(7,0)
PProductOnPltSet_1=(+478.90,-98.88,+410.00,+179.79,-0.35,+178.62)(7,0)
PProductOnPltSet_2=(+478.90,-98.88,+500.00,+179.79,-0.35,+178.62)(7,0)
PProductOnPltSet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboGet=(-18.40,+403.83,+318.61,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboGet_1=(-18.40,+403.83,+425.20,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboGet_2=(-18.40,+370.97,+425.20,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboGet_3=(-18.40,+300.00,+550.00,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboGet_4=(-18.40,+300.00,+550.00,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboSet=(-18.40,+403.83,+318.61,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboSet_1=(-18.40,+403.83,+425.20,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboSet_2=(-18.40,+370.97,+425.20,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboSet_3=(-18.40,+300.00,+550.00,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboSet_4=(-18.86,+404.69,+360.00,-102.74,+89.08,-12.36)(6,0)
PProductOnRoboSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPushTilt=(-213.31,+564.75,+464.13,+179.96,-0.02,+1.51)(7,0)
PPushTilt_1=(-213.31,+564.75,+480.78,+179.96,-0.02,+1.51)(7,0)
PPushTilt_2=(-213.31,+564.75,+620.00,+179.96,-0.02,+1.51)(7,0)
PPushTilt_3=(+0.02,+340.00,+610.00,-180.00,-0.01,-91.91)(7,0)
PTicketRead=(+602.00,-150.00,+500.00,+180.00,+0.00,+90.00)(7,0)
PTicketRead_1=(+602.00,-150.00,+550.00,+180.00,+0.00,+90.00)(7,0)
PTicketRead_2=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
JActive=(+93.47,-14.83,+113.09,+0.17,+81.44,+182.96,+0.00,+0.00)
Jmove=(+93.47,-46.87,+111.64,+0.00,+80.58,+182.96,+0.00,+0.00)
JTaihi=(+0.00,-46.87,+111.64,+0.00,+80.58,+0.00,+0.00,+0.00)
