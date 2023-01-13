1 ' ===================================
2 '
3 '  2100301001 STEP5 Assy1プログラム
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
171 MOUT_OKNG% = 12226                     'PLC OUT でOK=1, NG=0 出力
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
210 Def Inte nInputQty
211 Def Inte nAssyOkQty
212 Def Inte nAssyNgQty
213 Def Inte MScrewNo
214 Def Inte MReTry
215 '===== <IO変数定義> =====
216 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
217 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
218 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
219 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
220 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
221 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
222 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
223 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
224 '
225 Def Inte Y68_VV1            ' アーム先端　ネジ吸着バルブ
226 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
227 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
228 '
229 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
230 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
231 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
232 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
233 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
234 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
235 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
236 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
237 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
238 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
239 '
240 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
241 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
242 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
243 '
244 Def Inte MOUT_LED1          ' 画像処理用LED照明
245 '
246 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
247 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
248 '
249 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
250 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
251 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
252 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
253 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
254 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
255 '
256 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
257 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
258 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
259 '
260 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
261 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
262 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
263 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
264 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
265 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
266 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
267 Y68_VV1%    =  12248    ' アーム先端　ネジ吸着バルブ '数値12250から12248へ変更(8/5中村)
268 Y6B_VB1%    =  12250    'アーム先端　吸着破壊バルブ  '数値12251から12250へ変更(8/5中村)
269 MOUT_VB1%   =  12250    ' アーム先端　ネジ吸着破壊バルブ  '数値12251から12250へ変更(8/5中村)
270 '
271 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
272 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
273 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
274 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
275 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
276 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
277 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
278 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
279 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
280 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
281 '
282 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
283 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
284 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
285 '
286 MOUT_LED1%  =  12239    ' 画像処理用LED照明
287 '
288 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
289 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
290 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
291 '
292 '共通
293 Def Inte MTEST_KEY                      'デバックテスト用
294 Def Inte MOn                            '出力=1
295 Def Inte MOff                           '出力=0
296 '
297 'ねじ締め装置_出力アドレス
298 Def Inte MOUT_ScwT_ComChk               '通信確認
299 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
300 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
301 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
302 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
303 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
304 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
305 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
306 'ねじ締め装置_入力アドレス
307 Def Inte MIN_ScwT_comOK                 '通信確認返信
308 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
309 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
310 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
311 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
312 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
313 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
314 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
315 '
316 Dim MScwT_Case1%(2)               '条件1停止変数
317 Dim MScwT_Case2%(2)               '条件2停止変数
318 Dim MScwT_Case3%(2)               '条件3停止変数
319 Dim MScwT_Case4%(2)               '条件4停止変数
320 Dim MScwT_Case5%(2)               '条件5停止変数
321 '
322 '共通
323 MTEST_KEY% = 11359                       'デバッグ用テストKEY
324 MOn% = 1                                 '出力 = 1
325 MOff% = 0                                '出力 = 0
326 '
327 'ねじ締め機_アドレス設定
328 MOUT_ScwT_ComChk% = 12816               '通信確認送信
329 MOUT_ScwT_ST% = 12849                   'ねじ締め開始を送信
330 MOUT_ScwT_ReSTOK% = 12850               '再開始受信を送信
331 MOUT_ScwT_FinOK% = 12852                'ねじ締め完了受信を送信
332 MOUT_ScwT_Case1OK% = 12858              '条件1停止受信を送信
333 MOUT_ScwT_Case2OK% = 12859              '条件2停止受信を送信
334 MOUT_ScwT_Case3OK% = 12860              '条件3停止受信を送信
335 MOUT_ScwT_Case4OK% = 12861              '条件4停止受信を送信
336 MOUT_ScwT_Case5OK% = 12862              '条件5停止受信を送信
337 '
338 MIN_ScwT_comOK% = 11824                 'ねじ締め装置から返信
339 MIN_ScwT_STRec% = 11857                 'ねじ締め開始を受信
340 MIN_ScwT_ReST% = 11858                  '再開始を受信
341 MIN_ScwT_Fin% = 11860                   'ねじ締め完了を受信
342 MIN_ScwT_Case1% = 11866                 '条件1停止待機を受信
343 MIN_ScwT_Case2% = 11867                 '条件2停止待機を受信
344 MIN_ScwT_Case3% = 11868                 '条件3停止待機を受信
345 MIN_ScwT_Case4% = 11869                 '条件4停止待機を受信
346 MIN_ScwT_Case5% = 11870                 '条件5停止待機を受信
347 '
348 MScwT_Case1%(1) = MIN_ScwT_Case1%
349 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
350 MScwT_Case2%(1) = MIN_ScwT_Case2%
351 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
352 MScwT_Case3%(1) = MIN_ScwT_Case3%
353 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
354 MScwT_Case4%(1) = MIN_ScwT_Case4%
355 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
356 MScwT_Case5%(1) = MIN_ScwT_Case5%
357 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
358 '
359 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
360 Function M% fnAssyStart
361 ' PIASチケット読込み工程抜け確認
362     M_20# = MClear%                       '初期化
363 *ASSY_ERROR_END
364 *AssyEnd
365 *fnAssyStart_FEndPosi
366 FEnd
367 '
368 '■fnPiasCheck
369 ''' <summary>
370 ''' PIASチケット読込み
371 ''' </summary>
372 ''' <returns>   0 : NG
373 '''             1 : OK(読込み完了)
374 ''' </returns>
375 ''' <remarks>
376 ''' Date   : 2021/07/07 : M.Hayakawa
377 ''' </remarks>'
378 Function M% fnPiasCheck
379     fnPiasCheck = 0
380     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
381     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
382 '
383 *RETRY_PIAS
384     M_20# = MClear%
385     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
386     '
387     '【IDチケット読み込み】
388     PInspPosition(1) = PTicketRead  'IDチケット読取位置
389     MInspGroup%(1) = 1              '検査G番号
390     MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
391 '
392     'エラーの場合
393     If MRtn <> 1 Then
394         'D720 -> D1300 コピー要求
395         M_Out(12565) = 1
396         Dly 0.5
397         M_Out(12565) = 0
398         'エラー処理記述
399         fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
400         'GOT KEY入力待ち
401         MKeyNumber = fnKEY_WAIT()
402         '
403         Select MKeyNumber
404             Case MNext%         '次へを選択した場合
405                 M_20# = MPass%                          'M_20# プログラム間共通外部変数
406                 fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
407                 GoTo *fnPiasCheck_End                   'PIASチェック終了
408                 Break
409             Case MAbout%        '停止を選択した場合
410                 M_20# = MAbout%                         'M_20# プログラム間共通外部変数
411                 fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
412                 GoTo *fnPiasCheck_End                   'PIASチェック終了
413                 Break
414             Case MNgProcess%    'NGを選択した場合
415                 M_20# = MAbout%                         'M_20# プログラム間共通外部変数
416                 fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
417                 GoTo *fnPiasCheck_End                   'PIASチェック終了
418                 Break
419             Case MContinue%     '継続を選択した場合
420                 fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
421                 M_20# = MContinue%
422                 GoTo *RETRY_PIAS                        'PIASチェックリトライ
423                 Break
424         End Select
425     EndIf
426 '----------D720 -> D1300 コピー要求----------
427     M_Out(12565) = 1
428     Dly 0.5
429     M_Out(12565) = 0
430 '----------通信確認をする----------
431     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
432     MRtn = 0                ' 初期化
433     M_20# = MClear%         ' 初期化
434     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
435     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
436     If MRtn <> 1 Then
437         If M_20# = MContinue% Then
438             GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
439         Else
440             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
441         EndIf
442     EndIf
443 '----------工程抜け確認----------
444     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
445     MRtn = 0                ' 初期化
446     M_20# = MClear%         ' 初期化
447     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
448     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
449     If MRtn <> 1 Then
450         If M_20# = MContinue% Then
451             GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
452         Else
453             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
454         EndIf
455     EndIf
456     '
457     fnPiasCheck = 1
458     *fnPiasCheck_End
459 FEnd
460 '
461 '■fnPCComuCheck
462 ''' <summary>
463 ''' PC-PLC通信チェック
464 ''' </summary>
465 ''' <returns>   0 : NG
466 '''             1 : OK(読込み完了)
467 ''' </returns>
468 ''' <remarks>
469 ''' Date   : 2021/07/07 : M.Hayakawa
470 ''' </remarks>'
471 Function M% fnPCComuCheck
472     fnPCComuCheck = 0
473     MJudge% = 0                                  '初期化
474     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
475     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
476     '
477     For MStaNo = 0 To 5
478         '
479         If M_In(MIN_PIAS_ComOK%) = 1 Then
480             'PC通信OK(M400)
481             MJudge% = MOK%
482             MStaNo = 5
483             Break
484         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
485             'toRBT_通信確認time out
486             MJudge% = MNG%
487             MCommentD1001 = 15
488             MCommentD1002 = 21
489             MStaNo = 5
490             Break
491         Else
492             'toRBT_通信確認time out
493             MJudge% = MNG%
494             MCommentD1001 = 14
495             MCommentD1002 = 21
496             Break
497         EndIf
498     Next MStaNo
499     '
500     '上記で返信フラグを受信してからPC通信確認OFF
501     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
502     '
503     'エラー画面
504     If MJudge% <> MOK% Then
505         M_20# = MClear%     '初期化
506         'エラー処理記述
507         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
508         'GOT KEY入力待ち
509         MKeyNumber = fnKEY_WAIT()
510         '
511         If MKeyNumber = MAbout% Then            '停止を選択した場合
512             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
513             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
514             Break
515         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
516             M_20# = MNext%                      'M_20# プログラム間共通外部変数
517             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
518             Break
519         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
520             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
521             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
522             Break
523         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
524             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
525             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
526             Break
527         EndIf
528     Else
529         'OKの場合
530         fnPCComuCheck = 1
531     EndIf
532 FEnd
533 '
534 '■fnProcessCheck
535 ''' <summary>
536 ''' 工程抜け確認
537 ''' </summary>
538 ''' <returns>    1：工程履歴OK     0：異常終了
539 '''             -1：前工程履歴NG  -2：自工程履歴あり
540 '''             -3：モデル仕向NG  -4：タイムアウト
541 '''             -5：履歴処理エラー
542 ''' </returns>
543 ''' <remarks>
544 ''' Date   : 2021/07/07 : M.Hayakawa
545 ''' </remarks>'
546 Function M% fnProcessCheck
547     fnProcessCheck = 0
548     MJudge% = MNG%      '一旦NGを初期化とする
549 '----------工程抜け確認----------
550     MCommentD1001 = 0   'コメント初期化
551     For MStaNo = 0 To 5
552         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
553         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
554         '
555         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
556             MJudge% = MOK%
557             fnAutoScreenComment(85)     ' AUTO画面
558             MStaNo = 5
559             Break
560         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
561             MFlgLoop% = 0
562             MJudge% = MNG%
563             MCommentD1001 = 27
564             MCommentD1002 = 22
565             fnAutoScreenComment(94)     ' AUTO画面
566             fnProcessCheck = -2         ' NGは-2を返す
567             MStaNo = 5
568             Break
569         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
570            MJudge% = MNG%
571             MCommentD1001 = 31
572             MCommentD1002 = 22
573             fnAutoScreenComment(83)     ' AUTO画面
574             fnProcessCheck = -3         ' NGは-3を返す
575             MStaNo = 5
576             Break
577         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
578             '履歴NGは直ぐに終了せず繰り返し確認を行う
579             '前工程の書込みが終了していない可能性があるため
580             MJudge% = MNG%
581             MCommentD1001 = 32
582             MCommentD1002 = 22
583             fnAutoScreenComment(84)     ' AUTO画面
584             fnProcessCheck = -1         ' NGは-1を返す
585             Dly 1.0
586             '工程抜け確認OFF
587             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
588             Dly 1.0
589            'MStaNo = 5
590             Break
591         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
592             MFlgLoop% = 0
593             MJudge% = MNG%
594             MCommentD1001 = 29
595             MCommentD1002 = 22
596             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
597             fnProcessCheck = -5         ' NGは-5を返す
598             MStaNo = 5
599             Break
600         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
601             MJudge% = MNG%
602             If MCommentD1001 = 32 Then
603                 '何もしない
604             Else
605                 MCommentD1001 = 26
606             EndIf
607             MCommentD1002 = 22
608             fnProcessCheck = -4         ' NGは-4を返す
609             MStaNo = 5
610             Break
611         Else
612             MJudge% = MNG%
613             MCommentD1001 = 28
614             MCommentD1002 = 22
615         EndIf
616     Next MStaNo
617     '工程抜け確認OFF
618     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
619     '通過履歴NG 工程抜けの場合
620     If MJudge% = MPass% Then
621         M_20# = MPass%
622     EndIf
623     '
624     'エラー画面
625     If MJudge% <> MOK% Then
626         M_20# = MClear%     '初期化
627         'エラー処理記述
628         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
629         'GOT KEY入力待ち
630         MKeyNumber = fnKEY_WAIT()
631         '
632         Select MKeyNumber
633             Case MAbout%        '停止を選択した場合
634                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
635                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
636                 Break
637             Case MNext%         '次へを選択した場合
638                 M_20# = MPass%          'M_20# プログラム間共通外部変数
639                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
640                 Break
641             Case MContinue%     '継続を選択した場合
642                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
643                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
644                 Break
645             Case MNgProcess%    'NGを選択した場合
646                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
647                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
648                 Break
649         End Select
650     Else
651         fnProcessCheck = 1  ' OKは1を返す
652     EndIf
653 FEnd
654 '
655 '■fnPiasWrite
656 ''' <summary>
657 ''' Pias 組立結果書込み要求
658 ''' </summary>
659 '''<param name="MFlg%">
660 '''                 MOK%(1) = 工程履歴にOKを書込む
661 '''                 MNG%(0) = 工程履歴にNGを書込む
662 '''</param>
663 '''<returns></returns>
664 ''' <remarks>
665 ''' Date   : 2021/07/07 : M.Hayakawa
666 ''' </remarks>'
667 Function M% fnPiasWrite(ByVal MFlg%)
668       fnPiasWrite = 0
669 *RETRY_PIASWRITE
670     '
671     '組立OK(MOK%)の場合　M306 ON
672    '組立NG(MNG%)の場合　M307 ON
673     If MFlg% = MOK% Then
674         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
675     Else
676         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
677     EndIf
678     Dly 0.1                  '念のため
679     '
680     'Piasへ書込み開始 M305 -> ON
681     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
682     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
683     '
684     MJudge% = MNG%
685     '
686     For MStaNo = 0 To 5
687         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
688             MJudge% = MOK%
689             'MRet = fnAutoScreenComment(85)  'AUTO画面
690             MStaNo = 5
691             Break
692         '
693         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
694             MJudge% = MNG%
695             'MRet = fnAutoScreenComment(85)  'AUTO画面
696            MCommentD1001 = 34
697            MCommentD1002 = 25
698             MStaNo = 5
699             Break
700         '
701         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
702             MJudge% = MNG%
703             'MRet = fnAutoScreenComment(85)  'AUTO画面
704            MCommentD1001 = 35
705            MCommentD1002 = 25
706             MStaNo = 5
707             Break
708         '
709         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
710             MJudge% = MNG%
711             'MRet = fnAutoScreenComment(85)  'AUTO画面
712            MCommentD1001 = 36
713            MCommentD1002 = 25
714             MStaNo = 5
715             Break
716         '
717         Else
718             MJudge% = MNG%
719            MCommentD1001 = 42
720            MCommentD1002 = 25
721         '
722         EndIf
723         '
724     Next MStaNo
725     '
726     'Piasへ書込み開始 M305 -> OfF
727     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
728     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
729     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
730     '
731     '
732     '通過履歴NG 工程抜けの場合
733     If MJudge% = MPass% Then
734         M_20# = MPass%
735     EndIf
736     '
737    M_20# = MClear%     '初期化
738     '
739     'エラー画面
740     If MJudge% < MOK% Then
741     '
742 '残しておくが現状では使用しないラベル
743 *RETRY_ERR_WRITE
744         M_20# = MClear%     '初期化
745         'エラー処理記述
746         MRet = fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
747         'GOT KEY入力待ち
748         MKeyNumber = fnKEY_WAIT()
749         '
750         If MKeyNumber = MAbout% Then   '停止を選択した場合
751             M_20# = MAbout%            'M_20# プログラム間共通外部変数
752            MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
753             Break
754         '
755         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
756             M_20# = MContinue%            'M_20# プログラム間共通外部変数
757             MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
758         '
759         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
760             M_20# = MPass%            'M_20# プログラム間共通外部変数
761             MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
762         '
763         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
764             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
765            MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
766             Break
767         '
768         EndIf
769         '
770         If M_20# = MClear% Then *RETRY_ERR_WRITE
771         '
772     EndIf
773     '
774     If M_20# = MContinue% Then *RETRY_PIASWRITE
775     '
776     fnPiasWrite = 1
777     '
778 FEnd
779 '
780 '■fnPCBNumberCheck
781 ''' <summary>
782 ''' Pias 基板番号照合要求
783 ''' </summary>
784 '''<param name="%"></param>
785 '''<param name="%"></param>
786 '''<returns></returns>
787 ''' <remarks>
788 ''' Date   : 2021/07/07 : M.Hayakawa
789 ''' </remarks>'
790 Function M% fnPCBNumberCheck
791       fnPCBNumberCheck = 0
792     '
793 *RETRY_PCBCHECK
794     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
795     'Piasへ基板照合開始 M310 -> ON
796     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
797     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
798     '
799     MJudge% = MNG%
800     '
801     For MStaNo = 0 To 5
802         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
803             MJudge% = MOK%
804             fnAutoScreenComment(96)  'AUTO画面
805             MStaNo = 5
806             Break
807         '
808         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
809             MJudge% = MNG%
810             fnAutoScreenComment(97)  'AUTO画面
811             MCommentD1001 = 37
812             MCommentD1002 = 25
813             MStaNo = 5
814             Break
815         '
816         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
817             MJudge% = MNG%
818             fnAutoScreenComment(98)  'AUTO画面
819             MCommentD1001 = 38
820             MCommentD1002 = 25
821             MStaNo = 5
822             Break
823         '
824         ElseIf M_In(11580) = 1 Then                         'time out
825             MJudge% = MNG%
826             fnAutoScreenComment(99)  'AUTO画面
827             MCommentD1001 = 39
828             MCommentD1002 = 25
829             MStaNo = 5
830             Break
831         '
832         Else
833             MJudge% = MNG%
834            MCommentD1001 = 41
835            MCommentD1002 = 25
836         '
837         EndIf
838         '
839     Next MStaNo
840     '
841     'Piasへ基板照合開始 M310 -> OfF
842     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
843     '
844     '
845     '通過履歴NG 工程抜けの場合
846     If MJudge% = MPass% Then
847         M_20# = MPass%
848     EndIf
849     '
850    M_20# = MClear%     '初期化
851     '
852     'エラー画面
853     If MJudge% < MOK% Then
854     '
855 '残しておくが現状では使用しないラベル
856 *RETRY_ERR_PCBNUMBER
857         M_20# = MClear%     '初期化
858         'エラー処理記述
859         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
860         'GOT KEY入力待ち
861         MKeyNumber = fnKEY_WAIT()
862         '
863         If MKeyNumber = MAbout% Then   '停止を選択した場合
864             M_20# = MAbout%            'M_20# プログラム間共通外部変数
865             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
866             Break
867         '
868         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
869             M_20# = MContinue%            'M_20# プログラム間共通外部変数
870             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
871         '
872         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
873             M_20# = MPass%            'M_20# プログラム間共通外部変数
874             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
875         '
876         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
877             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
878             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
879             Break
880         '
881         EndIf
882         '
883         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
884         '
885     EndIf
886     '
887     If M_20# = MContinue% Then *RETRY_PCBCHECK
888 FEnd
889 '
890 '■ScrewTight
891 ''' <summary>
892 ''' ねじ締めを行う(Sタイト)
893 ''' </summary>
894 '''<param name="PScrewPos()">
895 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
896 '''             PScrewPos(2)    ：ねじ締め回避点
897 '''             PScrewPos(10)   ：ねじ締め終了高さ
898 '''<param name="MScrewType">ネジタイプ(mm/sec)
899 '''             1:6mm Sタイト銀ネジ
900 '''             2:13mm Sタイト銀ネジ
901 '''             3:6mm Sタイト黒ネジ
902 '''             4:3mm Sタイト黒ネジ
903 '''             5:6mm Mネジ
904 '''</param>
905 '''<param name="MFeedSpd">送り速度(mm/sec)</param>
906 '''<returns>整数
907 '''         0=異常終了、1=正常終了
908 '''</returns>
909 ''' <remarks>
910 ''' Date   : 2021/07/07 : M.Hayakawa
911 ''' Update : 2021/09/28 : M.Hayakawa ネジタイプ、送り速度を引数に追加
912 ''' </remarks>'
913 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   'ネジ締め個別設定
914     ScrewTight = 0
915     MOKNGFlg = 0
916     Ovrd 100
917     Fine 0.05 , P
918     Mvs PScrewPosition(1)       ' パレット上ねじ締めS①の安全回避位置
919 '    Ovrd MOvrdA%               '10/7現在値Null
920     Ovrd 20                     '念のため減速
921     ' パレット上ねじ締め開始位置へ移動
922     Mvs PScrewPosition(2)
923     ' 内部Ovrd設定
924 '    Ovrd MOvrdA%
925     Ovrd 100
926     ' Spd設定
927 '    Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
928     Spd MFeedSpd * (100/M_OPovrd)
929     ' 設定進み量5.0 × 操作パネルのオーバーライド係数 × プログラム内オーバーライド係数
930     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
931     Select MScrewType%
932         Case 1
933             ' Sタイト：プログラム1、バンク1に設定
934             ProgramBankSet(1,1)
935             Break
936         Case 2
937             ' Sタイト13mm：プログラム2、バンク1に設定
938             ProgramBankSet(2,1)
939             Break
940         Case 3
941             ' Sタイト黒：プログラム3、バンク1に設定
942             ProgramBankSet(3,1)
943             Break
944         Case 4
945             ' Sタイト3mm黒：プログラム4、バンク1に設定
946             ProgramBankSet(4,1)
947             Break
948         Case 5
949             ' Mネジ：プログラム5、バンク1に設定
950             ProgramBankSet(5,1)
951             Break
952         Default
953             ' プログラム1、バンクなし設定
954             ProgramBankSet(0,0)
955             Break
956     End Select
957 '
958 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     'ドライバーON　CW
959      'ドライバーON　CW
960     M_Out(12241)=1
961     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
962     Wait M_In(11584)=1          '完了/エラー検出 暫定コメント 10/6 M.H
963     Dly 0.1
964     Spd M_NSpd
965     Fine 0 , P
966     '
967     If M_In(11256)=1 Then  'ねじトータルエラー検出時
968         M_Out(Y61_Driver)=0     'ドライバーOFF　CW
969         Dly 0.1
970        ' プログラム・バンク解除
971         ProgramBankSet(0,0)
972         'パレット上ねじ締め終了位置上空へ移動
973         Mvs PScrewPosition(10),-80
974         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
975         M_Out(12249)=1 Dly 0.3
976         MOKNGFlg = -1
977         ScrewTight = 0
978     Else
979          'ドライバーOFF　CW
980         M_Out(12241)=0
981 '        エラーがない場合はネジ締め終了位置で増し締め
982         Select MScrewType%
983             Case 1
984                 ' Sタイト：プログラム1、バンク3に設定
985                 ProgramBankSet(1,3)
986                 Break
987             Case 2
988                 ' Sタイト13mm：プログラム2、バンク3に設定
989                 ProgramBankSet(2,3)
990                 Break
991             Case 3
992                 ' Sタイト黒：プログラム1、バンク3に設定
993                 ProgramBankSet(3,3)
994                 Break
995             Case 4
996                 ' Sタイト13mm：プログラム1、バンク3に設定
997                 ProgramBankSet(4,3)
998                 Break
999             Case 5
1000                 ' Mネジ：プログラム1、バンク3に設定
1001                 ProgramBankSet(5,3)
1002                 Break
1003             Default
1004                 ' プログラム1、バンクなし設定
1005                 ProgramBankSet(0,0)
1006                 Break
1007         End Select
1008          'ドライバーON　CW
1009         Mvs PScrewPosition(10)
1010         M_Out(12241)=1
1011         Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1012 '
1013          'ドライバーOFF　CW
1014         M_Out(12241)=0
1015        ' プログラム・バンク解除
1016         ProgramBankSet(0,0)
1017         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1018         M_Out(12249)=1 Dly 0.3
1019     '     ↓PScrewPos(2) → PScrewPosition(10)に変更 9/16 M.Hayakawa
1020         'パレット上ねじ締め終了位置上空へ移動
1021         Mvs PScrewPosition(10),-80
1022         ScrewTight = 1
1023     EndIf
1024 ' 暫定（暫定マスク　9/16 M.Hayakawa)
1025 '    Ovrd 10
1026 '    Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1027     Ovrd 100
1028 FEnd
1029 '
1030 '■ScrewGet
1031 ''' <summary>
1032 ''' ねじ供給機からねじを得る
1033 ''' </summary>
1034 '''<param name="%">
1035 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
1036 '''         PScrewPos(2)    ：ねじ供給器回避点
1037 '''         PScrewPos(9)    ：ねじ供給器上空ネジ捨位置
1038 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1039 '''         PScrewPos(3)    ：Mねじポカヨケ位置
1040 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
1041 '''</param>
1042 '''<param name = FeederReadyNo%> 指定の供給機Ready </param>
1043 '''<param name = FeederScrewSensor%> 指定の誤供給防止センサー指定(0でセンサー無し)</param>
1044 '''<returns>整数
1045 '''         0=異常終了、1=正常終了、-1=ねじ供給NG、-2=ねじ誤供給NG、-3=吸着エラー
1046 '''</returns>
1047 ''' <remarks>
1048 ''' Date   : 2021/07/07 : M.Hayakawa
1049 ''' </remarks>
1050 '''<update>
1051 '''Date    : 2021/11/15 : 中村
1052 '''</update>
1053 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
1054     ScrewGet = 0
1055     MScrewJudge% = 0
1056     'ねじ供給器初期動作エラーチェック
1057 ' ↓暫定削除
1058     Mov PScrewPosition(2)   'ねじ供給機回避点へ移動
1059     For MCnt% = 0 To MFinCnt%
1060        'ねじ供給器初期動作エラーチェック
1061         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '指定ねじ供給機がReadyになっているか確認(5秒間)
1062         If MRtn = 0 Then
1063             'Ovrd 30
1064             M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
1065             ScrewGet = -1
1066             MScrewJudge% = 2
1067         EndIf
1068         Ovrd 100
1069         If FeederScrewSensor% <> 0 Then
1070             If M_In(FeederScrewSensor%) = 1 Then  '誤供給が検出されたら
1071                 'Ovrd 30
1072                 M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
1073                 'NGとしてここの関数から抜ける
1074                 ScrewGet = -2
1075                 MScrewJudge% = 3
1076             EndIf
1077         EndIf
1078         Ovrd 100
1079         Spd M_NSpd
1080         'ねじ供給開始
1081         If MScrewJudge% = 0 Then
1082     '        ScrewGet = 0
1083             M_Out(Y63_Driver)=1         ' バンクセッティング　C2
1084             MScrewCnt% = 0
1085             MFinCnt% = 2
1086             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1087             Ovrd 5 '2に変更 10/6 M.H '5に変更10/7中村
1088             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1089             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
1090             Mvs PScrewPosition(10), 1.2
1091             M_Out(Y68_VV1)=1 Dly 0.3     ' ねじ吸着　ON'Dly 0.3追加(8/27中村)
1092             'ビット回転
1093             M_Out(Y60_Driver)=1
1094             Dly 0.2
1095             '
1096             Ovrd 5 '2に変更 10/6 M.H '5に変更10/7中村
1097             JOvrd M_NJovrd
1098             Spd M_NSpd
1099             'ネジ吸着確認位置移動
1100             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
1101             Mvs PScrewPosition(10), -15  ' ネジ吸着確認位置
1102             'ビット回転停止
1103             M_Out(Y60_Driver)=0
1104             '
1105             '1秒間ネジ吸着確認
1106             MRtn = frInCheck(11268, 1, MSETTIMEOUT01&)
1107             'MRtn = 0'強制エラー
1108             '吸着エラーの場合
1109             'ネジをねじ太郎に戻す
1110             If MRtn = 0 Then
1111                 Ovrd 5      '2から5に変更
1112                 'ビット回転停止
1113                 M_Out(Y60_Driver)=0
1114                 'ネジ供給機上空
1115                 Mvs PScrewPosition(1)
1116                 '更に上空
1117                 Mov PScrewPosition(1), -140
1118                 'ネジ捨て位置
1119                 Mov PScrewPosition(9)
1120                 '吸着OFF
1121                 M_Out(12249)=1 Dly 0.3 'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1122                 Dly 0.2
1123                 '破壊ON
1124                 M_Out(Y6B_VB1)=1 '真空破壊ON
1125                 'ビット回転
1126                 M_Out(Y61_Driver)=1
1127                 Dly 0.5
1128                 '                '
1129                 Ovrd 100
1130                 JOvrd M_NJovrd
1131                 Spd M_NSpd
1132                 'ドライバーを上下させねじを振り落とす
1133                 Mov PScrewPosition(9), 10
1134                 Mov PScrewPosition(9)
1135                 Dly 0.1
1136                 Mov PScrewPosition(9), 10
1137                 Mov PScrewPosition(9)
1138                 '
1139                 'ネジ落ち待ち
1140                 Wait M_In(11268) = 0
1141                 'ビット回転停止
1142                 M_Out(Y61_Driver)=0
1143                 Dly 0.1
1144                 '破壊OFF
1145                 M_Out(Y6B_VB1)=0 '真空破壊OFF
1146                 'ねじ落ちたとして、移動更に上空
1147                 Mov PScrewPosition(1), -140
1148                 Ovrd 100
1149                 Spd M_NSpd
1150                 'ネジ供給機上空
1151                 Mvs PScrewPosition(1)
1152 '                '
1153                 ScrewGet = -3
1154                 Break
1155 '                '
1156             Else
1157                 MCnt% = MFinCnt%
1158                 ScrewGet = 0
1159             EndIf
1160         Else
1161             MCnt% =MFinCnt%
1162         EndIf
1163     Next  MCnt%
1164         '
1165     If MScrewJudge% = 0 Then
1166         Ovrd 100
1167         Spd M_NSpd
1168         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
1169         M_Out(Y60_Driver)=0     ' ビット回転停止
1170         M_Out(Y63_Driver)=0     ' バンクセッティング　C2
1171         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
1172         Mov PScrewPosition(2)
1173         'もう一度吸着確認
1174         MRtn = frInCheck(11268, 1, MSETTIMEOUT01&)
1175         If MRtn = 0 Then      '吸着エラーの場合
1176             MScrewJudge% = 4
1177             ScrewGet = -3
1178         ElseIf MRtn = 1 Then      '吸着OKの場合
1179             MScrewJudge% = 1
1180             ScrewGet = 1
1181         EndIf
1182         Break
1183     EndIf
1184     If MScrewJudge% <> 1 Then                 'エラー終了時
1185     Select MScrewJudge%
1186         Case 0
1187             fErrorProcess(11,0,0,0) '異常終了
1188             Break
1189         Case 2
1190             fErrorProcess(11,0,0,0) '供給NG
1191             Break
1192         Case 3
1193             fErrorProcess(11,0,0,0) '誤供給
1194             Break
1195         Case 4
1196             fErrorProcess(11,94,95,0) '吸着NG
1197             Break
1198     End Select
1199     '
1200     Select M_20#
1201         Case MAbout%          '停止が押された場合
1202 '            Mov PScrewPosition(2)                  '初期位置に戻って停止処理'経路に問題ありコメントアウト11/24中村
1203 '            Mov PInitialPosition
1204             Break
1205         Case MContinue%       'リトライが押されていた場合(関数を抜けた先で処理)
1206             Break
1207         Case MNext%           '継続が押された場合
1208             M_20# = MClear%     '初期化
1209             Break
1210         Case MNgProcess%      'NGが押された場合
1211 '            Mov PScrewPosition(2)   'PIASにNG書き込みを行い,初期位置に戻って行程終了'経路に問題ありコメントアウト11/24中村
1212 '            Mov PInitialPosition
1213             Break
1214         End Select
1215     EndIf
1216 FEnd
1217 '
1218 '■ProgramBankSet
1219 ''' <summary>
1220 ''' ねじ締めを行う(Pタイト)
1221 ''' </summary>
1222 '''<param name="MProgramNo">プログラム番号</param>
1223 '''<param name="MBankNo">バンク番号</param>
1224 '''</returns>
1225 ''' <remarks>
1226 ''' Date   : 2021/10/05 : M.Hayakawa
1227 ''' </remarks>'
1228 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
1229     Select MProgramNo%
1230         Case 1
1231             M_Out(Y65_Driver)=0     ' プログラムセッティング　F1
1232             Dly 0.1
1233             M_Out(Y66_Driver)=0     ' プログラムセッティング　F2
1234             Dly 0.1
1235             M_Out(Y67_Driver)=0     ' プログラムセッティング　F3
1236             Dly 0.1
1237             Break
1238         Case 2
1239             M_Out(Y65_Driver)=1     ' プログラムセッティング　F1
1240             Dly 0.1
1241             M_Out(Y66_Driver)=0     ' プログラムセッティング　F2
1242             Dly 0.1
1243             M_Out(Y67_Driver)=0     ' プログラムセッティング　F3
1244             Dly 0.1
1245             Break
1246         Case 3
1247             M_Out(Y65_Driver)=0     ' プログラムセッティング　F1
1248             Dly 0.1
1249             M_Out(Y66_Driver)=1     ' プログラムセッティング　F2
1250             Dly 0.1
1251             M_Out(Y67_Driver)=0     ' プログラムセッティング　F3
1252             Dly 0.1
1253             Break
1254          Case 4
1255             M_Out(Y65_Driver)=1     ' プログラムセッティング　F1
1256             Dly 0.1
1257             M_Out(Y66_Driver)=1     ' プログラムセッティング　F2
1258             Dly 0.1
1259             M_Out(Y67_Driver)=0     ' プログラムセッティング　F3
1260             Dly 0.1
1261             Break
1262         Case 5
1263             M_Out(Y65_Driver)=0     ' プログラムセッティング　F1
1264             Dly 0.1
1265             M_Out(Y66_Driver)=0     ' プログラムセッティング　F2
1266             Dly 0.1
1267             M_Out(Y67_Driver)=1     ' プログラムセッティング　F3
1268             Dly 0.1
1269             Break
1270         Case 6
1271             M_Out(Y65_Driver)=1     ' プログラムセッティング　F1
1272             Dly 0.1
1273             M_Out(Y66_Driver)=0     ' プログラムセッティング　F2
1274             Dly 0.1
1275             M_Out(Y67_Driver)=1     ' プログラムセッティング　F3
1276             Dly 0.1
1277             Break
1278         Case 7
1279             M_Out(Y65_Driver)=0     ' プログラムセッティング　F1
1280             Dly 0.1
1281             M_Out(Y66_Driver)=1     ' プログラムセッティング　F2
1282             Dly 0.1
1283             M_Out(Y67_Driver)=1     ' プログラムセッティング　F3
1284             Dly 0.1
1285             Break
1286         Case 8
1287             M_Out(Y65_Driver)=1     ' プログラムセッティング　F1
1288             Dly 0.1
1289             M_Out(Y66_Driver)=1     ' プログラムセッティング　F2
1290             Dly 0.1
1291             M_Out(Y67_Driver)=1     ' プログラムセッティング　F3
1292             Dly 0.1
1293             Break
1294         Default
1295             M_Out(Y65_Driver)=0     ' プログラムセッティング　F1
1296             Dly 0.1
1297             M_Out(Y66_Driver)=0     ' プログラムセッティング　F2
1298             Dly 0.1
1299             M_Out(Y67_Driver)=0     ' プログラムセッティング　F3
1300             Dly 0.1
1301             Break
1302     End Select
1303 '
1304     Select MBankNo%
1305         Case 1
1306             M_Out(Y62_Driver)=1     ' バンクセッティング　C1
1307             Dly 0.1
1308             M_Out(Y63_Driver)=0     ' バンクセッティング　C2
1309             Dly 0.1
1310             M_Out(Y64_Driver)=0     ' バンクセッティング　C3
1311             Dly 0.1
1312             Break
1313         Case 2
1314             M_Out(Y62_Driver)=0     ' バンクセッティング　C1
1315             Dly 0.1
1316             M_Out(Y63_Driver)=1     ' バンクセッティング　C2
1317             Dly 0.1
1318             M_Out(Y64_Driver)=0     ' バンクセッティング　C3
1319             Dly 0.1
1320             Break
1321         Case 3
1322             M_Out(Y62_Driver)=1     ' バンクセッティング　C1
1323             Dly 0.1
1324             M_Out(Y63_Driver)=1     ' バンクセッティング　C2
1325             Dly 0.1
1326             M_Out(Y64_Driver)=0     ' バンクセッティング　C3
1327             Dly 0.1
1328             Break
1329         Default
1330             M_Out(Y62_Driver)=0     ' バンクセッティング　C1
1331             Dly 0.1
1332             M_Out(Y63_Driver)=0     ' バンクセッティング　C2
1333             Dly 0.1
1334             M_Out(Y64_Driver)=0     ' バンクセッティング　C3
1335             Dly 0.1
1336             Break
1337     End Select
1338 FEnd
1339 '
1340 '■fnKEY_WAIT()
1341 ''' <summary>
1342 ''' GOTからのキー入力待ち
1343 ''' </summary>
1344 '''<returns>1：停止    2：次へ
1345 '''         3：継続    4：トルクチェック開始
1346 '''         5：NG
1347 '''         11：ロボット初期位置1    12：ロボット初期位置2
1348 '''         13：ロボット初期位置3    14：ロボット初期位置4
1349 '''</returns>
1350 ''' <remarks>
1351 ''' Date   : 2021/07/07 : M.Hayakawa
1352 ''' </remarks>'
1353 Function M% fnKEY_WAIT()
1354     fnKEY_WAIT = 0
1355     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
1356     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
1357     fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
1358     '下記キー待ちの継続に反応させないため
1359     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
1360     Dly 0.2
1361     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
1362     MLocalLoopFlg=1
1363     While MLocalLoopFlg=1
1364         If M_In(11345) = 1 Then         '停止   M5345
1365             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
1366             fnKEY_WAIT = 1
1367             MLocalLoopFlg=-1
1368             Break
1369         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
1370             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
1371             fnKEY_WAIT = 2
1372             MLocalLoopFlg=-1
1373             Break
1374         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
1375             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
1376             fnKEY_WAIT = 3
1377             MLocalLoopFlg=-1
1378             Break
1379         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
1380             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
1381             fnKEY_WAIT = 4
1382             MLocalLoopFlg=-1
1383             Break
1384         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
1385             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
1386             fnKEY_WAIT = 5
1387             MLocalLoopFlg=-1
1388             Break
1389             '
1390         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
1391             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
1392             fnKEY_WAIT = MRobotInit1%
1393             MLocalLoopFlg=-1
1394             Break
1395             '
1396         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
1397             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
1398             fnKEY_WAIT = MRobotInit2%
1399             MLocalLoopFlg=-1
1400             Break
1401             '
1402         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
1403             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
1404             fnKEY_WAIT = MRobotInit3%
1405             MLocalLoopFlg=-1
1406             Break
1407             '
1408         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
1409             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
1410             fnKEY_WAIT = MRobotInit4%
1411             MLocalLoopFlg=-1
1412             Break
1413             '
1414         Else
1415         EndIf
1416     WEnd
1417     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
1418     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
1419 FEnd
1420 '
1421 '■ fnAUTO_CTL
1422 ''' <summary>
1423 ''' AUTOモードOFF、PLCからの開始待ち
1424 ''' </summary>
1425 ''' <remarks>
1426 ''' Date   : 2021/07/07 : M.Hayakawa
1427 ''' </remarks>
1428 Function M% fnAUTO_CTL
1429     fnAUTO_CTL = 0
1430     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
1431     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
1432     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
1433     '
1434     If M_Svo=0 Then             'サーボON確認
1435         Servo On
1436     EndIf
1437     Wait M_Svo=1
1438 FEnd
1439 '
1440 '■ fnWindScreenOpen
1441 ''' <summary>
1442 ''' ウィンド画面の表示、非表示設定
1443 ''' </summary>
1444 '''<param name="%"></param>
1445 '''<param name="%"></param>
1446 '''<param name="%"></param>
1447 '''<param name="%"></param>
1448 ''' <remarks>
1449 ''' コメントD1001, D1002, D1003の設定
1450 ''' MWindReSet = 0     画面非表示
1451 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
1452 ''' MWindErrScr = 10    エラー画面 D1001, D1002
1453 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
1454 ''' Date   : 2021/07/07 : M.Hayakawa
1455 ''' </remarks>
1456 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
1457     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
1458         M_Out16(12480) = MCommentD1001            'D1001 コメント
1459     EndIf
1460     '
1461     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
1462         M_Out16(12496) = MCommentD1002            'D1002 コメント
1463     EndIf
1464     '
1465     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
1466        M_Out16(12512) = MCommentD1003            'D1003 コメント
1467     EndIf
1468     '
1469     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
1470     M_Out(12363) = 1                         'ウィンド画面設定  M6362
1471     Dly 0.5
1472     M_Out(12363) = 0                         'ウィンド画面設定
1473 FEnd
1474 '
1475 '■fnCtlValue
1476 ''' <summary>
1477 ''' 投入数、組立OK数、組立NG数　Read/Write
1478 ''' </summary>
1479 ''' <param name="MCtlNo%"></param>
1480 ''' <remarks>
1481 ''' Date   : 2021/07/07 : M.Hayakawa
1482 ''' </remarks>
1483 Function fnCtlValue(ByVal MCtlNo)
1484     Select MCtlNo
1485         Case 1        '読込み
1486             M_Out(12568) = 1        '
1487             M_30# = M_In16(11600)   '投入数受信
1488             M_31# = M_In16(11616)   '組立OK受信
1489             M_32# = M_In16(11632)   '組立NG受信
1490             M_33# = M_In16(11648)   '組立NG受信
1491             M_Out(12568) = 0
1492             Break
1493             '
1494         Case 2        '書込み
1495             M_Out(12569) = 1
1496             Dly 0.3
1497             M_Out16(12592) = M_30#  '投入数送信
1498             M_Out16(12608) = M_31#  '組立OK送信
1499             M_Out16(12624) = M_32#  '組立N送信
1500             M_Out16(12640) = M_33#  '組立N送信
1501             M_Out(12569) = 0
1502             Break
1503     End Select
1504 FEnd
1505 '
1506 ' ■ISInspection
1507 ''' <summary>
1508 ''' Insightによる画像処理検査実行
1509 ''' </summary>
1510 '''<param name="PInspPos()">検査位置</param>
1511 '''<param name="MInspGrNum%()">検査位置での検査グループ番号（=0：画像検査未実施）</param>
1512 '''             PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
1513 '''<param name="MInspCnt%">検査位置数</param>
1514 '''<param name="MZAxis%">終了時のZ軸退避座標（-1:無効）</param>
1515 '''             終了時にZ軸をMZAxisで設定された位置まで上昇させる
1516 '''<param name="MNgContinue%">=1で検査エラー・NG発生時に全Stepの検査を行う</param>
1517 '''<returns>    整数 0=異常終了、1=正常終了</returns>
1518 '''         MInspErrNum     ：異常終了時にエラー番号が設定される
1519 '''         MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される"
1520 ''' <remarks>
1521 ''' Date   : 2021/07/07 : M.Hayakawa
1522 ''' </remarks>
1523 Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
1524     '画像使用確認 0<- 画像確認無しの場合
1525     If M_In(11369) = 0 Then            'toRBT_使用確認
1526         ISInspection = 1                                        '正常終了戻り値設定
1527     EndIf
1528 '
1529     Cnt 0                                                       '移動効率化解除(初期値=0)
1530     Fine 0.05,P                                                 '位置決め完了条件設置　0.05mm
1531     MNum% = 1                                                   '検査番号初期値設定
1532     Def Inte MEndFlg                                            '検査終了フラグ
1533     MEndFlg% = 0
1534     '
1535     'エラー番号クリア
1536     MInspErrNumSub = 0                                          '検査実行エラー番号sub
1537     MInspErrNum = 0                                             '検査実行エラー番号
1538     M_Out16(MOUT_InspErrNum) = MInspErrNum
1539     MInspNGStepNum = 0                                          '検査実行NGStep番号
1540     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
1541     '
1542     If M_In(MIN_IS_Ready) = 0 Then                              'Ready offなら終了
1543         MInspErrNum = 20                                        '検査実行エラー番号 20 Insight offline
1544         M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
1545         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
1546         ISInspection = 0                                        '異常終了戻り値設定
1547 '
1548     EndIf
1549    If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
1550     '
1551     '検査位置数確認
1552     If MInspCnt% < 1 Or 30 < MInspCnt% Then
1553         MInspErrNum = 21                                        '検査データなし 21　引数<1
1554         M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
1555         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
1556         ISInspection = 0                                        '異常終了戻り値設定
1557 '
1558     EndIf
1559    If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
1560     '
1561     '設定された検査位置数分の検査実行
1562     While( MEndFlg% = 0 )
1563         '検査終了確認　検査終了フラグセット
1564         If (MInspCnt% < MNum% ) Then
1565             MEndFlg% = 1                                        '検査終了フラグセット
1566         EndIf
1567         '
1568         'タスク　検査G番号設定・検査完了確認処理開始　INSPTAST1
1569         If MEndFlg% = 0 Then
1570             M_01# = MInspGrNum%(MNum%)                          '検査G番号引渡し
1571         EndIf
1572         M_02# = MEndFlg%                                        '検査終了フラグ引渡し
1573         M_05# = MNum%                                           '検査番号(検査順1～)
1574         'タスク　検査G設定フラグ引渡し
1575         If MEndFlg% = 0 Then
1576             If 0 < MInspGrNum%(MNum%) Then
1577                 M_03# = 1
1578             Else
1579                 M_03# = 0
1580             EndIf
1581         Else
1582             M_03# = 0
1583         EndIf
1584         'タスク　検査結果確認フラグ引渡し
1585         If 1 < MNum% Then
1586             If 0 < MInspGrNum%(MNum%-1) Then
1587                 M_04# = 1
1588             Else
1589                 M_04# = 0
1590             EndIf
1591         Else
1592             M_04# = 0
1593         EndIf
1594         '
1595         'タスク処理開始
1596         M_00# = 1                                               'TASK処理開始
1597         'タスク処理開始確認
1598         M_Timer(1) = 0
1599         MExitFlg = 0
1600         While( MExitFlg = 0 )
1601             '処理開始完了確認
1602             If M_00# = 0 And M_10# = 8 Then
1603                 MExitFlg = 1
1604             EndIf
1605             'timeoutチェック
1606             If 2000 < M_Timer(1) Then
1607                 If MNgContinue% = 1 Then                        'NG続行?
1608                     MInspErrNumSub = 36                         'エラー番号設定36
1609                 Else
1610                     MInspErrNum = 36                            'エラー番号設定36
1611                 EndIf
1612                 MExitFlg = 1
1613             EndIf
1614         WEnd
1615         '
1616         '検査位置へ移動・移動完了待ち
1617         If 0 = MInspErrNum Then
1618             If MEndFlg% = 0 Then
1619                 Mvs PInspPos( MNum% )                           '移動
1620             EndIf
1621         EndIf
1622         '
1623         'タスク　検査G番号設定・検査完了確認処理終了待ち　INSPTAST1
1624         If 0 = MInspErrNum Then
1625             M_Timer(1) = 0
1626             MExitFlg = 0
1627             While( MExitFlg = 0 )
1628                 '処理完了待ち（正常終了）
1629                 If M_10# = 1 Then
1630                     MExitFlg = 1
1631                 EndIf
1632                 '処理完了待ち（異常終了）
1633                 If M_10# = 0 Then
1634                     If MNgContinue% = 1 Then                    'NG続行?
1635                         MInspErrNumSub = M_12#                  'エラー番号設定　M12
1636                     Else
1637                         MInspErrNum = M_12#                     'エラー番号設定　M12
1638                     EndIf
1639                     MExitFlg = 1
1640                 EndIf
1641                 'timeoutチェック
1642                 If 5000 < M_Timer(1) Then
1643                     If MNgContinue% = 1 Then                    'NG続行?
1644                         MInspErrNumSub = 31                     'エラー番号設定31
1645                     Else
1646                         MInspErrNum = 31                        'エラー番号設定31
1647                     EndIf
1648                     MExitFlg = 1
1649                 EndIf
1650             WEnd
1651         EndIf
1652         '
1653         '検査結果確認
1654         If 0 = MInspErrNum Then
1655             If 1 < MNum% Then
1656                 If 0 < MInspGrNum%(MNum%-1) Then                '検査あり?
1657                     If M_11# = 2 Then                           '検査NG?
1658                         If MNgContinue% = 1 Then                'NG続行?
1659                             If MInspNGStepNum = 0 Then          'NG未発生?
1660                                 MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
1661                             EndIf
1662                             MInspErrNumSub = 32                 'エラー番号設定 32:検査NG
1663                         Else
1664 '                            MInspNGStepNum = MNum% - 1          '検査実行NGStep番号設定
1665                             MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
1666                             MInspErrNum = 32                    'エラー番号設定 32:検査NG
1667                         EndIf
1668                    EndIf
1669                 EndIf
1670             EndIf
1671         EndIf
1672         '
1673         'エラーなら検査中断終了するのでLoopから抜けるため終了フラグセット
1674         If 0 <> MInspErrNum Then
1675             MEndFlg% = 1
1676         EndIf
1677         '
1678         '検査実行、取込完了待ち
1679         If 0 = MInspErrNum Then
1680             If MEndFlg% = 0 Then
1681                 If 0 < MInspGrNum%(MNum%) Then                  '検査あり?
1682                     M_Out(MOUT_IS_Insp%) = 1                    '検査実行要求on
1683                     '取込完了確認
1684                     M_Timer(1) = 0
1685                     MExitFlg = 0
1686                     While( MExitFlg = 0 )
1687                         '処理完了待ち
1688                         If M_In( MIN_IS_InspCapDone% ) = 1  Then
1689                             MExitFlg = 1
1690                         EndIf
1691                         'timeoutチェック
1692                         If 2000 < M_Timer(1) Then
1693                             If MNgContinue% = 1 Then            'NG続行?
1694                                 MInspErrNumSub = 33             'エラー番号設定33
1695                             Else
1696                                 MInspErrNum = 33                'エラー番号設定33
1697                             EndIf
1698                             MExitFlg = 1
1699                         EndIf
1700                     WEnd
1701                 EndIf
1702                 '
1703             EndIf
1704         EndIf
1705         MNum% = MNum% + 1
1706     WEnd
1707     '
1708     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
1709     If 0 < MZAxis% Then
1710         PCurrentPos = P_Curr                                    '現在位置取得
1711         PCurrentPos.Z = MZAxis%                                 'Z軸を設定
1712         Mvs PCurrentPos                                         '現在位置上空へ移動
1713     EndIf
1714     '
1715     'NG続行時処理
1716     If MNgContinue% = 1 Then                                    'NG続行?
1717         MInspErrNum = MInspErrNumSub                            'エラー番号設定
1718     EndIf
1719     '
1720     '戻り値設定
1721     If MInspErrNum = 0 Then
1722         ISInspection = 1                                        '正常終了戻り値設定
1723     Else
1724         M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
1725         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
1726         ISInspection = 0                                        '異常終了戻り値設定
1727     EndIf
1728     Fine 0 , P
1729     '
1730 *ISInspection_End
1731 FEnd
1732 '
1733 '■fnAutoScreenComment
1734 ''' <summary>
1735 ''' メイン画面の動作状況表示
1736 ''' コメントD1005の設定
1737 ''' </summary>
1738 '''<param name="McommentD1005%">コメントID</param>
1739 ''' <remarks>
1740 ''' Date   : 2021/07/07 : M.Hayakawa
1741 ''' </remarks>
1742 Function fnAutoScreenComment(ByVal McommentD1005%)
1743     M_Out16(12576) = McommentD1005%
1744 FEnd
1745 '
1746 '■fnRoboPosChk
1747 ''' <summary>
1748 ''' 最後に終了したロボットポジションの確認
1749 ''' </summary>
1750 '''<param name="MINNumber%">入力番号</param>
1751 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
1752 '''<param name="MTimeCnt&">タイムアウト時間</param>
1753 ''' PLCに保続した番号を読込み、確認
1754 ''' MRBTOpeGroupNo = 5 が初期位置に設定
1755 '''<returns>整数 0:タイムアウト 1:OK</returns>
1756 ''' <remarks>
1757 ''' Date   : 2021/07/07 : M.Hayakawa
1758 ''' </remarks>
1759 Function M% fnRoboPosChk
1760     fnRoboPosChk = 0
1761     MRet = fnStepRead()
1762     '初期位置でないと判断した場合
1763     'ウィンド画面切換え
1764     If MRBTOpeGroupNo > 5 Then
1765         '下記キー待ちの継続に反応させないため
1766         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
1767         Dly 0.2
1768         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
1769         Dly 1.5
1770         '
1771         MRet = fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
1772         '
1773         MLoopFlg% = 1
1774         While MLoopFlg% = 1
1775             '
1776             '
1777             MKeyNumber% = fnKEY_WAIT()
1778             Select MKeyNumber%
1779                 Case Is = MAbout%       '停止
1780                     M_20# = MAbout%
1781                     MLoopFlg% = -1
1782                     Break
1783                 Case Is = MNext%        '次へ
1784                     'MLoopFlg% = -1
1785                     Break
1786                 Case Is = MContinue%    '継続
1787                     M_20# = MContinue%
1788                     MLoopFlg% = -1
1789                     Break
1790                 Default
1791                     Break
1792             End Select
1793         WEnd
1794     EndIf
1795     '
1796     If M_20# = MContinue% Then                              '継続ボタンが押された場合
1797         MRet = fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
1798         Ovrd 5                                   '低速オーバーライド値設定
1799         Select MRBTOpeGroupNo
1800             Case Is = 5                          '何もしない
1801                 Break
1802             Case Is = 10                         '初期位置へ戻す
1803                 'Mov PTEST001
1804                 Break
1805             Case Is = 15                         '初期位置へ戻す
1806                 'Mov PTEST002
1807                 Dly 0.5
1808                 'Mov PTEST001
1809                 Dly 0.5
1810                 Break
1811             Default
1812                 Break
1813         End Select
1814         '
1815         Ovrd M_NOvrd                            'システムの初期値を設定
1816         M_Out(12364) = 1                        'toPLC_データ保存ON
1817         MRBTOpeGroupNo = 5
1818         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
1819         Dly 1.0
1820         M_Out(12364) = 0                        'toPLC_データ保存OFF
1821         fnRoboPosChk = 1                        '初期位置動作実行
1822         MRet = fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
1823     EndIf
1824     Exit Function
1825 FEnd
1826 '
1827 '■frInCheck
1828 ''' <summary>
1829 ''' センサーINチェック
1830 ''' </summary>
1831 '''<param name="MINNumber%">入力番号</param>
1832 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
1833 '''<param name="MTimeCnt&">タイムアウト時間</param>
1834 '''<returns>整数 0:タイムアウト 1:OK</returns>
1835 ''' <remarks>
1836 ''' Date   : 2021/07/07 : M.Hayakawa
1837 ''' </remarks>
1838 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
1839     M_Timer(4) = 0
1840     MloopFlg = 0
1841     While MloopFlg = 0
1842         MCrtTime& = M_Timer(4)
1843         If M_In(MINNumber%) = MCMPFLG% Then
1844             MloopFlg = 1
1845             frInCheck = 1
1846         ElseIf MCrtTime& > MTimeCnt& Then
1847             MloopFlg = 1
1848             frInCheck = 0
1849         EndIf
1850     WEnd
1851 FEnd
1852 '-----------------------------------------------
1853 '
1854 'ねじ締め機通信確認
1855 '
1856 '-----------------------------------------------
1857 Function M% fScewTcomChk
1858     fScewTcomChk = 0
1859     '通信確認送信
1860     M_Out(MOUT_ScwT_ComChk%) = MOn%
1861     '通信確認受信待機
1862     Wait M_In(MIN_ScwT_comOK%) = MOn%
1863     '通信確認送信終了
1864     M_Out(MOUT_ScwT_ComChk%) = MOff%
1865  '
1866 FEnd
1867 '
1868 '
1869 '-----------------------------------------------
1870 '
1871 'ねじ締め開始送信
1872 '
1873 '-----------------------------------------------
1874 Function M% fScewTStart
1875     fScewTStart = 0
1876     'ねじ締め開始待機を受信
1877     Wait M_In(MIN_ScwT_STRec%) = MOn%
1878     Dly 0.1
1879     'ねじ締め開始受信を送信
1880     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msecパルス
1881 FEnd
1882 '
1883 '
1884 '-----------------------------------------------
1885 '
1886 'ねじ締め完了受信
1887 '
1888 '-----------------------------------------------
1889 Function M% fScewTFinish
1890     fScewTFinish = 0
1891     'ねじ締め完了待機を受信
1892     Wait M_In(MIN_ScwT_Fin%) = MOn%
1893     Dly 0.1
1894     'ねじ締め完了受信を送信
1895     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msecパルス
1896 FEnd
1897 '
1898 '
1899 '-----------------------------------------------
1900 '
1901 '条件xx停止受信
1902 '
1903 '-----------------------------------------------
1904 Function M% fScewTCaseStop(ByVal MCase%())
1905     fScewTCaseStop = 0
1906     '条件xx停止を受信
1907     Wait M_In(MCase%(1)) = MOn%
1908     Dly 0.1
1909     '条件xx停止受信を送信
1910     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msecパルス
1911 FEnd
1912 '
1913 '-----------------------------------------------
1914 '
1915 '再開始受信
1916 '
1917 '-----------------------------------------------
1918 Function M% fScewTReStart()
1919     fScewTReStart = 0
1920     '再開始を受信
1921     Wait M_In(MIN_ScwT_ReST%) = MOn%
1922     Dly 0.1
1923     '再開始受信を送信
1924     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msecパルス
1925 FEnd
1926 '
1927 '■fErrorProcess
1928 '<summary>
1929 'エラー処理
1930 '</summary>
1931 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
1932 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
1933 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
1934 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
1935 '<make>
1936 '2021/11/5 中村天哉
1937 '</make>
1938 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
1939     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
1940     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
1941     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
1942     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
1943 *RETRY_ERR_PROCESS
1944      M_20# = MClear%     '初期化
1945 '        'エラー処理記述
1946         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
1947 '        'GOT KEY入力待ち
1948         MKeyNumber = fnKEY_WAIT()
1949 '        '
1950         If MKeyNumber = MAbout% Then   '停止を選択した場合
1951             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1952             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1953             Break
1954          '
1955         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1956             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1957             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1958         '
1959         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1960             M_20# = MNext%            'M_20# プログラム間共通外部変数
1961             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1962          '
1963         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1964             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1965             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1966             Break
1967         '
1968         EndIf
1969         '
1970         If M_20# = MClear% Then *RETRY_ERR_PROCESS
1971 FEnd
1972 '
1973 '■Main
1974 ''' <summary>
1975 ''' 組立動作用のメイン
1976 ''' </summary>
1977 ''' <remarks>
1978 ''' Date   : 2021/07/07 : M.Hayakawa
1979 ''' </remarks>'
1980 Function Main
1981     MopeNo = M_21#         '外部変数にて動作番号代入
1982     '
1983     If M_Svo=0 Then
1984         Servo On
1985     EndIf
1986     Wait M_Svo=1
1987 '組立スタート日付時刻要求パルスON
1988     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
1989 'パトライト操作
1990     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
1991     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
1992     '
1993     M_20# = 0                                   'KEY入力初期化
1994     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
1995     MRet% = 0
1996 '初期位置の確認と移動
1997 '    MRet% = fnRoboPosChk()
1998     If MRet% = 1 Then                           '初期位置の動作を行った場合
1999         MRet% = fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  'ウィンド画面
2000         MKeyNumber% = fnKEY_WAIT()
2001         Select MKeyNumber%
2002             Case Is = MAbout%       '停止
2003                 M_20# = MAbout%
2004                 MLoopFlg% = -1
2005                 Break
2006             Case Is = MNext%        '次へ
2007                 'MLoopFlg = -1
2008                 Break
2009             Case Is = MContinue%    '継続
2010                 M_20# = MContinue%
2011                 MLoopFlg% = -1
2012                 Break
2013             Default
2014                 Break
2015         End Select
2016     EndIf
2017     '
2018     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
2019         M_Out(12364) = 1            'toPLC_データ保存ON
2020 'トルクチェック
2021         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
2022             MRet% = fnTorqueCheck()
2023             Break
2024         Else
2025 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
2026 '                MRtn = InspInit()               '画像処理初期化処理
2027 '            EndIf
2028 '投入数、組立OK数、組立NG数Read/Write
2029             fnCtlValue(1)               '読込み
2030             nInputQty = nInputQty + 1   '投入数インクリメント
2031             fnCtlValue(2)               '書込み
2032             '
2033            M_20# = MClear%                    '初期化
2034 '組立開始
2035             If M_In(MIN_ASSY_CANCEL%) = 0 Then
2036 '                MRet% = fnAssyStart()
2037                 fnAssyStart()
2038             Else
2039                 M_20# = MPass%
2040             EndIf
2041 '組立終了日付時刻
2042             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
2043             Wait M_In(11572) = 1            '日付取得完了
2044             Dly 0.1
2045             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
2046 'リフターユニットへのOUT
2047             '  KEY入力が何もない場合 OKと判断
2048             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
2049             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
2050 'OK/NGフラグ出力
2051             If M_20# <= 0 Then
2052                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
2053             ElseIf M_20# = MPass% Then
2054                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
2055             EndIf
2056 'PIASに組立完了書込み
2057             If M_In(MIN_PIAS_Use%) = 1 Then             'PIAS_ON確認
2058                 If M_20# <> MPass% Then
2059                     'KEY入力がNGの場合
2060                     If M_20# = MNgProcess% Then
2061                         M_Out(MOUT_OKNG%) = 0           '後工程へNGフラグを出力(PLC OUT)
2062                         'MRet% = fnAutoScreenComment(90) 'AUTO画面 通過履歴NG書込み
2063                         'MRet% = fnPiasWrite(MNG%)
2064                        nAssyNgQty = nAssyNgQty + 1      ' 組立NG数インクリメント
2065                     EndIf
2066                     '
2067                     'KEY入力が何もない場合 OKと判断
2068                     If M_20# <= 0 Then
2069                             'D732 -> D2600 コピー要求
2070                             M_Out(12566) = 1
2071 '                            Wait M_In(11581) = 1        'PLCよりコピー完了信号
2072                             M_Out(12566) = 0
2073                             '
2074                         If M_In(11367) = 0 Then         '基板履歴書込みキャンセル=1 DEbug用
2075                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
2076                             '基板番号照合
2077                             MRet% = fnPCBNumberCheck()
2078                         Else
2079                             MRet% = 1
2080                         EndIf
2081                         '
2082                         If M_In(11368) = 0 Then         '工程履歴書込みキャンセル=1 DEbug用
2083                             If M_20# <> MAbout% Then
2084                                 '工程履歴OK書き込み
2085                                 M_Out(MOUT_OKNG%) = 1   '後工程へOKフラグを出力(PLC OUT)
2086                                 'MRet% = fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
2087                                 'MRet% = fnPiasWrite(MOK%)
2088                                 nAssyOkQty = nAssyOkQty + 1
2089                             Else
2090                                 nAssyOkQty = nAssyOkQty + 1
2091                             EndIf
2092                         EndIf
2093                     EndIf
2094                 Else
2095                     M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
2096                 EndIf
2097             Else
2098                 nAssyOkQty = nAssyOkQty + 1             ' 組立OK数インクリメント
2099             EndIf
2100             '
2101             '組立終了日付時刻解除
2102             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
2103             '投入数、組立OK数、組立NG数書込み
2104             fnCtlValue(2)                       '書込み
2105             '
2106 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
2107 '                '画像処理終了処理
2108 '                MRtn = InspQuit()
2109 '            EndIf
2110         EndIf
2111         M_Out(12364) = 0                          'toPLC_データ保存OFF
2112     EndIf
2113 'パトライト操作
2114     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
2115     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
2116 'GOT表示
2117     fnAutoScreenComment(93)  'AUTO画面 工程完了
2118 FEnd
2119 End
2120 '
2121 'おまじないコメント
2122 '絶対削除するな
2123 '
2124 '
PInspPosition(1)=(+601.00,-152.00,+375.00,+180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
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
PTemp=(+601.01,-151.96,+429.95,-179.99,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPos(1)=(-377.09,+108.11,+620.00,+180.00,+0.00,+0.00,+0.00,+0.00)(7,0)
PScrewPos(2)=(-377.09,+108.11,+577.47,+180.00,+0.00,+0.00,+0.00,+0.00)(7,0)
PScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(10)=(-377.10,+108.11,+567.73,+180.00,+0.00,+0.00,+0.00,+0.00)(7,0)
PGetScrewPos(1)=(-282.57,-243.43,+422.84,-179.99,+0.00,-58.70,+0.00,+0.00)(7,1)
PGetScrewPos(2)=(-147.31,-147.28,+610.00,-180.00,+0.00,-45.01,+0.00,+0.00)(7,1)
PGetScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(9)=(-235.40,-243.41,+334.56,-179.99,+0.00,-58.70,+0.00,+0.00)(7,1)
PGetScrewPos(10)=(-282.57,-243.44,+403.39,-179.99,+0.00,-58.70,+0.00,+0.00)(7,1)
PEscapePosi(1)=(-252.42,-263.31,+570.00,+179.96,-0.07,-43.81)(7,1)
PEscapePosi(2)=(-141.51,-152.87,+570.00,+179.96,-0.07,-43.81)(7,1)
PEscapePosi(3)=(-208.31,+0.23,+570.00,-180.00,+0.00,-90.00)(7,0)
PEscapePosi(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PEscapePosi(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PEscapePosi(6)=(-208.31,+0.01,+610.00,+180.00,+0.00,+0.00)(7,0)
PEscapePosi(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PEscapePosi(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PEscapePosi(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PEscapePosi(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PEscapePosition=(+247.61,-0.38,+580.00,-180.00,+0.00,-179.99)(7,0)
PEscapePosition_2=(-131.77,+177.64,+579.99,-180.00,+0.00,-53.32)(7,0)
PEscapePosition_3=(-178.95,+226.16,+579.83,-180.00,-0.02,-53.33)(7,0)
PEscapePosition_4=(-221.18,+0.03,+579.99,-180.00,+0.00,+0.10)(7,0)
PInitialPosition=(+300.00,+0.00,+440.00,-180.00,+0.00,-180.00)(7,0)
PProductOnJigGet=(-245.32,-0.38,+429.95,-49.33,+88.91,+130.56)(6,1)
PProductOnJigGet_1=(-245.33,-0.40,+460.00,-48.95,+88.90,+130.94)(6,1)
PProductOnJigGet_2=(-190.37,-0.40,+560.00,-48.95,+88.90,+130.94)(6,1)
PProductOnJigGet_3=(-133.01,+133.47,+580.00,-173.13,+89.99,-38.35)(6,0)
PProductOnJigGet_4=(-164.65,+0.00,+671.53,-166.66,+90.00,+13.34)(7,0)
PProductOnJigGet_5=(-224.30,+0.01,+604.77,+180.00,+0.00,+0.00)(7,0)
PProductOnJigGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnJigSet=(-245.32,-0.40,+430.31,-49.37,+88.91,+130.52)(6,1)
PProductOnJigSet_1=(-245.33,-0.40,+460.00,-48.95,+88.90,+130.94)(6,1)
PProductOnJigSet_2=(-190.37,-0.40,+580.00,-48.95,+88.90,+130.94)(6,1)
PProductOnJigSet_3=(-136.17,+133.06,+580.00,-175.95,+89.99,-39.47)(6,0)
PProductOnJigSet_4=(+163.76,+97.11,+580.00,-176.54,+89.98,-145.04)(6,0)
PProductOnPltGet=(+549.98,-99.42,+250.75,+180.00,+0.00,-179.03)(7,0)
PProductOnPltGet_1=(+549.98,-99.42,+290.00,+180.00,+0.00,-179.03)(7,0)
PProductOnPltGet_2=(+549.98,-99.42,+400.00,+180.00,+0.00,-179.03)(7,0)
PProductOnPltSet=(+548.99,-100.57,+251.32,+180.00,+0.00,-179.54)(7,0)
PProductOnPltSet_1=(+548.99,-100.57,+290.00,+180.00,+0.00,-179.54)(7,0)
PProductOnPltSet_2=(+548.99,-100.57,+400.00,+180.00,+0.00,-179.54)(7,0)
PProductOnPltSet_3=(+133.46,+133.02,+580.00,-173.90,+89.99,-129.11)(6,0)
PScrewHeatSink1=(-377.09,+81.39,+568.02,+180.00,+0.00,+0.00)(7,0)
PScrewHeatSink1_0=(-377.09,+81.39,+577.45,+180.00,+0.00,+0.00)(7,0)
PScrewHeatSink1_1=(-377.09,+81.39,+620.00,+180.00,+0.00,+0.00)(7,0)
PScrewHeatSink2=(-377.10,+108.11,+567.73,+180.00,+0.00,+0.00)(7,0)
PScrewHeatSink2_0=(-377.09,+108.11,+577.47,+180.00,+0.00,+0.00)(7,0)
PScrewHeatSink2_1=(-377.09,+108.11,+620.00,+180.00,+0.00,+0.00)(7,0)
PScrewPlateL=(-284.78,+112.54,+536.90,-180.00,+0.00,-90.00)(7,0)
PScrewPlateL1=(-317.11,+26.69,+536.90,+179.96,+0.00,-90.00)(7,0)
PScrewPlateL1_0=(-317.11,+26.71,+543.53,+179.96,+0.00,-90.00)(7,0)
PScrewPlateL1_1=(-317.11,+26.71,+570.00,+179.96,+0.00,-90.00)(7,0)
PScrewPlateL2=(-317.26,+101.98,+536.35,-180.00,+0.00,-90.00)(7,0)
PScrewPlateL2_0=(-317.26,+101.99,+543.71,-180.00,+0.00,-90.00)(7,0)
PScrewPlateL2_1=(-317.26,+101.99,+570.00,-180.00,+0.00,-90.00)(7,0)
PScrewPlateL_0=(-284.78,+112.54,+543.53,-180.00,+0.00,-90.00)(7,0)
PScrewPlateL_1=(-284.78,+112.54,+570.00,-180.00,+0.00,-90.00)(7,0)
PScrewPlateR=(-286.12,-114.33,+536.39,-180.00,+0.00,-90.00)(7,1)
PScrewPlateR1=(-317.31,-28.86,+536.39,-180.00,+0.00,-89.94)(7,1)
PScrewPlateR1_0=(-317.31,-28.86,+543.60,-180.00,+0.00,-89.94)(7,1)
PScrewPlateR1_1=(-317.31,-28.85,+569.98,-180.00,+0.00,-89.94)(7,1)
PScrewPlateR2=(-318.02,-104.08,+536.32,-179.99,+0.00,-89.94)(7,1)
PScrewPlateR2_0=(-318.03,-104.06,+543.56,-179.99,+0.00,-89.94)(7,1)
PScrewPlateR2_1=(-318.02,-104.08,+570.00,-179.99,+0.00,-89.94)(7,1)
PScrewPlateR_0=(-286.12,-114.33,+543.60,-180.00,+0.00,-90.00)(7,1)
PScrewPlateR_1=(-286.12,-114.33,+570.00,-180.00,+0.00,-90.00)(7,1)
PScrewSupplyHS=(-282.57,-243.44,+403.39,-179.99,+0.00,-58.70)(7,1)
PScrewSupplyHS_1=(-282.57,-243.43,+422.84,-179.99,+0.00,-58.70)(7,1)
PScrewSupplyHS_2=(-147.31,-147.28,+610.00,-180.00,+0.00,-45.01)(7,1)
PScrewSupplyHS_3=(-235.18,-0.02,+609.98,+180.00,+0.00,+0.00)(7,1)
PScrewSupplyHS_4=(-235.40,-243.41,+334.56,-179.99,+0.00,-58.70)(7,1)
PScrewSupplyPlate=(-136.35,+200.79,+452.16,-180.00,+0.01,+170.74)(7,0)
PScrewSupplyPlate_1=(-136.35,+200.79,+470.00,-180.00,+0.01,+170.74)(7,0)
PScrewSupplyPlate_2=(-136.35,+200.79,+570.00,-180.00,+0.01,+170.74)(7,0)
PScrewSupplyPlate_3=(-132.53,+160.72,+570.00,-179.99,+0.00,-140.43)(7,0)
PScrewSupplyPlate_4=(-208.31,+0.23,+610.00,-180.00,+0.00,-90.00)(7,0)
PScrewSupplyPlate_5=(-113.97,+113.99,+696.72,-0.32,+89.24,+134.68)(7,0)
PScrewSupplyPlate_6=(-161.19,+0.02,+696.72,-0.32,+89.24,+179.67)(7,0)
PScrewSupplyPlate_7=(-50.58,+239.35,+544.69,-180.00,+0.01,+170.74)(7,0)
PScrewSupplyPlatel_2=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PTicketRead=(+601.00,-152.00,+375.00,+180.00,+0.00,+90.00)(7,0)
PTicketRead_1=(+601.00,-152.00,+430.00,-180.00,+0.00,+90.00)(7,0)
